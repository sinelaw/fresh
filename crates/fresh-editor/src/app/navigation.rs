//! Cursor-jump primitives that guarantee viewport visibility.
//!
//! All "navigate the cursor to a byte offset" flows — search next/prev,
//! LSP go-to-definition, jump-to-line, diagnostic jumps, plugin
//! `scrollBufferToLine`, etc. — should funnel through this module instead
//! of mutating `cursors` and calling `ensure_cursor_visible` directly.
//!
//! The lower-level [`view::split::BufferViewState::ensure_cursor_visible`]
//! has several short-circuit paths (the `skip_ensure_visible` flag set by
//! prior scroll actions, the `top_view_line_offset > 0` early-return for
//! wrapped buffers, `skip_resize_sync`) that can leave a freshly-set cursor
//! stranded outside the viewport. That class of bug — "status bar updates
//! but the page never moves" — is why every navigation primitive here ends
//! with a *post-condition check*: if the cursor is still off-screen when
//! the call returns, we force a hard recenter (issue #1689).
//!
//! Use [`Editor::ensure_active_cursor_visible_for_navigation`] right after
//! any explicit cursor mutation that represents a user-visible jump. Use
//! [`Editor::jump_active_cursor_to`] when the call site can also delegate
//! the cursor mutation itself.
//!
//! Edits (typing, paste, indent, …) should keep using the existing
//! `ensure_cursor_visible` path — they want the "don't undo a deliberate
//! scroll" behavior of the skip flag.

use crate::model::buffer::LineNumber;

use super::Editor;

/// Whether the active cursor should be vertically recentered when a jump
/// causes the viewport to scroll, and whether the selection anchor should
/// be reset.
#[derive(Clone, Copy, Debug)]
pub struct JumpOptions {
    /// If `true`, drop the selection anchor (the jump becomes a plain move).
    /// Set to `false` to extend the selection from the previous anchor.
    pub clear_anchor: bool,
    /// If the jump caused the viewport to scroll *or* the post-condition
    /// safety net had to fire, recenter the cursor vertically. This is the
    /// behavior search/LSP/error navigation want — a cold landing spot
    /// should show context above and below.
    pub recenter_on_scroll: bool,
}

impl Default for JumpOptions {
    fn default() -> Self {
        Self {
            clear_anchor: true,
            recenter_on_scroll: true,
        }
    }
}

impl JumpOptions {
    /// Convenience: defaults for navigation jumps (clear anchor, recenter).
    pub fn navigation() -> Self {
        Self::default()
    }
}

impl Editor {
    /// Move the active cursor to `position` and guarantee that position is
    /// rendered in the active viewport.
    ///
    /// This is the canonical "jump the cursor somewhere" entry point. It
    /// performs a direct cursor mutation (no `MoveCursor` event, no undo
    /// entry, no `cursor_moved` plugin hook) and then funnels through
    /// [`Editor::ensure_active_cursor_visible_for_navigation`] for the
    /// visibility invariant.
    ///
    /// Callers that need a `MoveCursor` event (undo + plugin hooks) should
    /// build the event themselves and call
    /// [`Editor::ensure_active_cursor_visible_for_navigation`] afterwards.
    pub fn jump_active_cursor_to(&mut self, position: usize, opts: JumpOptions) {
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();
        if let Some(view_state) = self.split_view_states.get_mut(&active_split) {
            view_state.cursors.primary_mut().position = position;
            if opts.clear_anchor {
                view_state.cursors.primary_mut().anchor = None;
            }
            if let Some(state) = self.buffers.get_mut(&active_buffer) {
                if let Some(pos) = state.buffer.offset_to_position(position) {
                    state.primary_cursor_line_number = LineNumber::Absolute(pos.line);
                }
            }
        }
        self.ensure_active_cursor_visible_for_navigation(opts.recenter_on_scroll);
    }

    /// Guarantee the active cursor is visible in the active viewport.
    ///
    /// Call this immediately after any cursor mutation that represents a
    /// programmatic jump (search match, goto-definition, jump-to-line,
    /// next-error, plugin scroll-to-position). It:
    ///
    /// 1. Clears `skip_ensure_visible` so a stale prior scroll does not
    ///    suppress this one.
    /// 2. Calls the lower-level `ensure_cursor_visible`.
    /// 3. **Verifies** the cursor's line is now within the viewport's line
    ///    range. If it isn't (the lower-level routine short-circuited, or
    ///    `view_lines`-aware logic disagreed with byte-line math), forces a
    ///    hard recenter so the cursor lands roughly mid-viewport.
    /// 4. If the visible range moved at all and `recenter_on_scroll` is
    ///    set, recenters for context.
    ///
    /// Step 3 is the safety net that makes "cursor moves but viewport
    /// stalls" (#1689) impossible to reproduce regardless of what the
    /// lower-level scroll machinery decides to do.
    pub fn ensure_active_cursor_visible_for_navigation(&mut self, recenter_on_scroll: bool) {
        let active_split = self.split_manager.active_split();
        let active_buffer = self.active_buffer();

        let Some(view_state) = self.split_view_states.get_mut(&active_split) else {
            return;
        };
        let Some(state) = self.buffers.get_mut(&active_buffer) else {
            return;
        };

        // 1. Clear stale skip flag — a prior recenter (or scroll action) may
        // have set it, but this navigation step is *new user intent* and must
        // not be silently suppressed.
        view_state.viewport.clear_skip_ensure_visible();

        let cursor_pos = view_state.cursors.primary().position;
        let top_byte_before = view_state.viewport.top_byte;

        // 2. Best-effort scroll via the existing line-aware routine.
        view_state.ensure_cursor_visible(&mut state.buffer, &state.marker_list);

        let scrolled = view_state.viewport.top_byte != top_byte_before;

        // 3. Post-condition check — derive line numbers (cheap, exact for
        // non-large files; estimated for large files) and confirm the cursor
        // line lies within the viewport's line range. If it doesn't, the
        // lower-level routine bailed out for one of its skip-paths and we
        // must force a recenter.
        let cursor_visible = is_cursor_line_visible(view_state, &state.buffer, cursor_pos);

        let needs_recenter = !cursor_visible || (scrolled && recenter_on_scroll);
        if needs_recenter {
            let viewport_height = view_state.viewport.visible_line_count();
            let target_rows_from_top = viewport_height / 2;
            let mut iter = state.buffer.line_iterator(cursor_pos, 80);
            for _ in 0..target_rows_from_top {
                if iter.prev().is_none() {
                    break;
                }
            }
            view_state.viewport.top_byte = iter.current_position();
            view_state.viewport.top_view_line_offset = 0;
            // The next render-time `ensure_visible_in_layout` would otherwise
            // immediately undo this recenter to satisfy its own scroll-margin
            // invariants. Tell it to keep the position we just chose.
            view_state.viewport.set_skip_ensure_visible();
        }
    }
}

/// Approximate visibility check using line numbers. False negatives only —
/// if we say "not visible" when it actually is, the helper recenters
/// unnecessarily but still leaves the cursor on screen, which is
/// observably indistinguishable from the no-op case.
fn is_cursor_line_visible(
    view_state: &crate::view::split::BufferViewState,
    buffer: &crate::model::buffer::Buffer,
    cursor_pos: usize,
) -> bool {
    let viewport = &view_state.viewport;
    let top_line = buffer.get_line_number(viewport.top_byte);
    let cursor_line = buffer.get_line_number(cursor_pos);
    let viewport_height = viewport.visible_line_count();
    cursor_line >= top_line && cursor_line < top_line.saturating_add(viewport_height)
}

/// Reconcile a freshly-restored `(buf_state.viewport, buf_state.cursors)` pair
/// so the cursor is guaranteed visible.
///
/// Session/workspace restore re-applies the previously-saved viewport
/// `top_byte` (and `top_view_line_offset` in wrap mode) and the previously-
/// saved cursor position independently. If those two were *already* out of
/// sync at save time — for example because the cursor moved off-screen via a
/// prior bug or via plugin scroll-to-position — the restore re-creates an
/// off-screen cursor that arrow keys can't escape (the wrap-mode early
/// return in `viewport.rs::ensure_visible` kicks in for any cursor whose
/// byte position is `>= viewport.top_byte`, which is true for *all* cursors
/// below the viewport top — so naive Up/Down can never bring the viewport
/// back to the cursor).
///
/// Call this on each restored buffer's state right after writing the
/// scroll/cursor fields. If the cursor's line is already visible inside the
/// restored viewport this is a no-op — we keep the user's saved scroll
/// position for free. If not, recenter so the cursor lands mid-viewport
/// (#1689 follow-up).
pub(crate) fn reconcile_restored_buffer_view(
    buf_state: &mut crate::view::split::BufferViewState,
    buffer: &mut crate::model::buffer::Buffer,
) {
    let cursor_pos = buf_state.cursors.primary().position;
    if is_cursor_line_visible(buf_state, buffer, cursor_pos) {
        return;
    }
    let viewport_height = buf_state.viewport.visible_line_count();
    let target_rows_from_top = viewport_height / 2;
    let mut iter = buffer.line_iterator(cursor_pos, 80);
    for _ in 0..target_rows_from_top {
        if iter.prev().is_none() {
            break;
        }
    }
    buf_state.viewport.top_byte = iter.current_position();
    buf_state.viewport.top_view_line_offset = 0;
    // Restore code already calls set_skip_resize_sync; we don't need to also
    // pin against ensure_visible because the next render will see the cursor
    // is already inside the viewport range we just chose.
}
