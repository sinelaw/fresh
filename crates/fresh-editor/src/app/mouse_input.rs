//! Mouse input handling.
//!
//! This module contains all mouse event handling logic including:
//! - Click, double-click, and drag handling
//! - Scrollbar interaction
//! - Hover target computation
//! - Split separator dragging
//! - Text selection via mouse

use super::chrome::in_rect;
use super::*;
use crate::services::plugins::hooks::HookArgs;
use crate::view::popup_mouse::{popup_areas_to_layout_info, PopupHitTester};
use crate::view::prompt::PromptType;
use anyhow::Result as AnyhowResult;
use ratatui::layout::Rect;
use std::time::{Duration, Instant};

/// Columns one notch of a sideways wheel pans. Horizontal panning has
/// no line-oriented setting to follow — `mouse_wheel_scroll_lines`
/// counts lines — so it keeps the fixed step it always had.
const WHEEL_COLUMNS: i32 = 3;

/// How long one line of a smoothed wheel gesture is meant to take.
/// Roughly a frame at 60Hz, so a three-line notch walks across about
/// three frames — enough to read as a slide rather than a jump, and
/// short enough that the view never feels behind the wheel.
const SMOOTH_SCROLL_LINE: Duration = Duration::from_millis(16);

/// A wheel gesture still playing out.
///
/// One notch asks for several lines at once. Handing them over one at a
/// time makes the view slide instead of jumping — and gives the scroll
/// fade a row at a time to work with — but only while the frames to
/// show it in are actually arriving. So the walk is paced by the clock
/// rather than by frames: whatever the last frame did not get to is
/// still owed, and a caller that has been away long enough for all of
/// it delivers the remainder in one go. A terminal too slow to animate
/// therefore gets the plain jump it had before, rather than a scroll
/// that drags on behind the wheel.
pub(crate) struct PendingWheelScroll {
    /// Where the pointer was. The lines still owed are replayed through
    /// the same dispatch as the original event, so they land on
    /// whatever surface that event routed to.
    col: u16,
    row: u16,
    /// -1 for up, +1 for down.
    direction: i32,
    /// Lines still owed to that surface.
    remaining: u32,
    /// How many lines may stay owed before the walk starts handing over
    /// the surplus with the current frame. Two notches' worth, so the
    /// view never trails the wheel by more than a moment.
    max_backlog: u32,
    /// When the last line was handed over. Advanced by exactly the
    /// lines delivered, so the pace does not drift with frame times.
    last_step: Instant,
}

/// How many of `remaining` lines the clock has made due after
/// `elapsed`, at one line per `interval`: none until the first comes
/// due, and the whole remainder at once for a caller that has been away
/// long enough for all of them.
fn lines_due(elapsed: Duration, interval: Duration, remaining: u32) -> u32 {
    if interval.is_zero() {
        return remaining;
    }
    let due = elapsed.as_nanos() / interval.as_nanos();
    u32::try_from(due).unwrap_or(u32::MAX).min(remaining)
}

/// Lines to hand over this frame: the ones the clock has made due, plus
/// anything past `max_backlog` that would otherwise leave the view
/// trailing the wheel.
///
/// The backlog is what keeps a flick honest. Notches can arrive faster
/// than a line a frame, and a gesture must travel exactly as far as it
/// asks for — dropping the excess would make fast scrolling cover less
/// ground than slow scrolling. So nothing is ever dropped: past the
/// backlog the surplus rides along with this frame's line, and the walk
/// degrades toward the jump it replaced instead of falling behind.
fn lines_to_deliver(
    elapsed: Duration,
    interval: Duration,
    remaining: u32,
    max_backlog: u32,
) -> u32 {
    lines_due(elapsed, interval, remaining).max(remaining.saturating_sub(max_backlog))
}

/// Map a screen row on a suggestion list's scrollbar track to the prompt
/// scroll offset that puts the thumb's top on exactly that row.
///
/// Shared by the press and the drag-follow-up so the thumb tracks the cursor
/// identically in both. [`ScrollbarState::offset_for_thumb_top`] is the real
/// inverse of the thumb geometry the renderer draws — the ONE track mapping
/// (its off-by-a-row `click_to_offset` sibling is deleted).
///
/// Rows above/below the track clamp to its ends rather than being rejected,
/// so a fast drag doesn't drop the thumb.
pub(super) fn prompt_scrollbar_offset_for_row(
    total: usize,
    visible: usize,
    scroll_offset: usize,
    sb_rect: Rect,
    row: u16,
) -> usize {
    use crate::view::ui::scrollbar::ScrollbarState;
    let clamped_row = row.clamp(sb_rect.y, sb_rect.y + sb_rect.height.saturating_sub(1));
    let track_row = clamped_row.saturating_sub(sb_rect.y) as usize;
    ScrollbarState::new(total, visible, scroll_offset)
        .offset_for_thumb_top(sb_rect.height as usize, track_row)
}

impl Editor {
    /// Handle a mouse event.
    /// Returns true if a re-render is needed.
    ///
    /// Memo contract: unlike keys, mouse events bump the UI generation on
    /// EXIT, and only when the event changed something (`needs_render`). A
    /// quiet mouse-motion stream — the highest-frequency input the editor
    /// sees — then reuses one chrome tree across MANY events, while any
    /// event that mutates state (click opens a menu, drag moves a
    /// separator) invalidates for the next one. This gate is a coarse
    /// epoch, not the correctness story: `chrome_tree` additionally
    /// validates each hit against a fresh `overlay_stack` build, and its
    /// debug oracle cross-checks every hit against a full rebuild.
    pub fn handle_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        let result = self.handle_mouse_impl(mouse_event);
        if matches!(result, Ok(true)) {
            self.bump_ui_gen();
        }
        result
    }

    fn handle_mouse_impl(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        let (is_double_click, is_triple_click) = self.detect_multi_click(&mouse_event, col, row);

        // Modal mouse-capture, offered in RANK order over the derived
        // overlay stack: the first component whose modal surface is up
        // claims the whole mouse channel. Every capturing component
        // declares a layer from the same activity predicate its
        // capture gates on, so walking the owner-stamped stack visits
        // exactly the capturing candidates — and deletes the old
        // registry-order duplicate of the precedence (two hand-synced
        // encodings, comment-only sync). Rank IS the one source now,
        // for the keyboard walk and the capture band alike.
        {
            let stack = self.overlay_stack();
            let mut seen = std::collections::HashSet::new();
            for entry in &stack {
                // The hardcoded event-debug head has no owner; a
                // component contributing several layers is offered
                // the capture once, at its highest rank.
                let Some(owner) = entry.owner else { continue };
                if !seen.insert(owner) {
                    continue;
                }
                if let Some(result) = super::chrome::components()[owner].capture_mouse(
                    self,
                    mouse_event,
                    is_double_click,
                ) {
                    return result;
                }
            }
        }

        // Cancel the LSP-rename prompt on ANY mouse interaction that
        // reaches normal routing. RULING — a pre-WALK observer of the
        // non-modal channel: it fires on every event kind the capture
        // band above lets through (click, wheel, even bare motion)
        // wherever it lands, which no box on the walk can express (a
        // box fires only when hit, and only for gestures with arms),
        // then continues — the walk proceeds as if it weren't here.
        // Deliberately BELOW the capture band, unlike the keyboard's
        // transient-popup dismissal (which observes even under
        // modals): a capturing modal owns its events wholly, and a
        // click aimed at Settings must not reach through it to cancel
        // a rename prompt the user cannot see. The `prompt_type`
        // match is the observer's own gate, not surface routing.
        let mut needs_render = false;
        if let Some(ref prompt) = self.active_window_mut().prompt {
            if matches!(prompt.prompt_type, PromptType::LspRename { .. }) {
                self.cancel_prompt();
                needs_render = true;
            }
        }

        // Update mouse cursor position for software cursor rendering (used by GPM)
        // When GPM is active, we always need to re-render to update the cursor position
        let cursor_moved = self.active_window_mut().mouse_cursor_position != Some((col, row));
        self.active_window_mut().mouse_cursor_position = Some((col, row));
        if self.active_window_mut().gpm_active && cursor_moved {
            needs_render = true;
        }

        tracing::trace!(
            "handle_mouse: kind={:?}, col={}, row={}",
            mouse_event.kind,
            col,
            row
        );

        // Check if we should forward mouse events to the terminal
        // Forward if: in terminal mode, mouse is over terminal buffer, and terminal is in alternate screen mode
        //
        // ...unless a chrome drag is in progress (dock-border resize, split
        // separator, or file-explorer width). That drag owns the mouse until
        // release, so don't let an alternate-screen terminal swallow the
        // motion once the pointer crosses over it — *growing* the dock drags
        // the cursor rightward across a full-screen `btop`, and forwarding
        // there both stalls the resize and eats the mouse-up that ends it,
        // leaving the drag stuck. Shrinking happened to work only because the
        // pointer stays left of the terminal the whole time.
        let chrome_drag_active = super::chrome::pointer_grab(self).is_some();
        // An open native context menu (tab / "+" new-tab / file-explorer)
        // takes mouse precedence over terminal forwarding. These menus render
        // on top of — and frequently overlap — an alternate-screen terminal
        // that has captured the mouse (e.g. right-clicking a terminal's tab
        // opens the tab menu directly over the terminal's content). Without
        // this gate the terminal-forward path below would swallow clicks/moves
        // aimed at the menu, so menu items couldn't be selected (they'd inject
        // mouse escape codes into the PTY instead). Skipping forwarding lets
        // the event fall through to the normal pipeline, where
        // `handle_click_context_menus` (select / dismiss) and the hover
        // hit-test (highlight-follows-pointer) already handle it. The menu's
        // precedence itself lives in the chrome walk/capture ordering (its
        // boxes ride the top routable band); this fork only keeps the PTY
        // from swallowing the events first.
        let context_menu_open = self.active_window().context_menu_core().is_some();
        // DERIVED suppression for everything with opaque geometry: a
        // pointer-opaque chrome box over the cell (an info popup, the
        // suggestions dropdown, the theme-info popup) must take the
        // event in the walk — forwarding it would inject mouse codes
        // into the PTY *through* the popup. This replaces growing the
        // hand list one surface at a time; the context-menu check
        // above stays NAMED by ruling because its boxes are
        // deliberately not opaque (its close-guard backdrop owns
        // outside clicks), so opacity cannot express it.
        // ONE tree per event, built AFTER the pre-band observers above
        // (the LSP-rename cancel can close a prompt, which changes the
        // geometry) and shared by the forward gate and every dispatch
        // arm below — the per-event-freshness ruling holds (same
        // event, same state), and a mouse-move stream no longer pays
        // two collects + two hit_stack sorts per event.
        let tree = super::chrome::chrome_tree(self);
        let opaque_chrome_over_point =
            crate::widgets::layout_box::hit_stack(&tree, row as u32, col as u32)
                .into_iter()
                .any(|i| tree[i].lb.pointer_opaque);
        if !chrome_drag_active && !context_menu_open && !opaque_chrome_over_point {
            let forwarding = self.config.terminal.mouse_forwarding;
            if let Some(result) = self.active_window_mut().try_forward_mouse_to_terminal(
                col,
                row,
                mouse_event,
                forwarding,
            ) {
                return result;
            }
        }

        // Ctrl+Click on a file path printed in the live terminal opens it in
        // Fresh (jumping to any :line:col it encodes). Handled before normal
        // click routing so it doesn't disturb cursor/selection state.
        if let Some(result) = self.try_open_terminal_link(col, row, mouse_event) {
            return result;
        }

        match mouse_event.kind {
            MouseEventKind::Down(MouseButton::Left) => {
                // NOTE: the fold-toggle double/triple check lives in
                // `Splits::on_pointer`'s Double/Triple arm — inside the
                // walk, so a popup's opaque box or the overlay prompt's
                // swallow blocks it by construction (it used to sit
                // here pre-walk, hit-testing `split_areas` directly and
                // bypassing every guard the walk enforces).
                if is_triple_click {
                    // Triple click detected - select entire line
                    self.handle_mouse_triple_click(&tree, col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                if is_double_click {
                    // Double click detected - both clicks within time threshold AND at same position
                    self.handle_mouse_double_click(&tree, col, row)?;
                    needs_render = true;
                    return Ok(needs_render);
                }
                self.handle_mouse_click(&tree, col, row, mouse_event.modifiers)?;
                needs_render = true;
            }
            MouseEventKind::Drag(MouseButton::Left) => {
                self.handle_mouse_drag(col, row)?;
                needs_render = true;
            }
            MouseEventKind::Up(MouseButton::Left) => {
                // Release is GRAB-KEYED like the Drag arm: the derived
                // `pointer_grab` names which press-to-release routing is
                // ending, and its arm runs that grab's finalizer — no
                // more per-surface field-poke ladder that had to be kept
                // in sync with the grab roster by hand. Grabs without a
                // finalizer just fall to the blanket clear below.
                let grab = super::chrome::pointer_grab(self);
                match grab {
                    // End a dock-resize drag and persist the chosen
                    // width so it survives toggling the dock off/on.
                    Some(super::chrome::PointerGrab::DockResize) => {
                        self.dock_resizing = false;
                        if let Some(super::PanelPlacement::LeftDock { width_cols }) =
                            self.dock.as_ref().map(|f| f.placement)
                        {
                            self.dock_width = Some(width_cols);
                        }
                        return Ok(true);
                    }
                    // Complete a tab drop before the drag state clears.
                    Some(super::chrome::PointerGrab::TabDrag) => {
                        if let Some(drag_state) =
                            self.active_window_mut().mouse_state.dragging_tab.take()
                        {
                            if drag_state.is_dragging() {
                                if let Some(drop_zone) = drag_state.drop_zone {
                                    self.execute_tab_drop(
                                        drag_state.buffer_id,
                                        drag_state.source_split_id,
                                        drop_zone,
                                    );
                                }
                            }
                        }
                    }
                    _ => {}
                }

                // Blanket sweep: every remaining drag flag drops here,
                // so no grab can outlive its release even if its
                // finalizer above was skipped.
                self.release_widget_scrollbar();
                self.release_split_widget_scrollbar();
                self.widget_text_drag = None;
                self.clear_active_window_drag_state();

                // A finished split-separator drag changed the ratios:
                // reflow through the single layout funnel (after the
                // sweep, as before).
                if matches!(grab, Some(super::chrome::PointerGrab::SplitSeparator)) {
                    self.relayout();
                }

                needs_render = true;
            }
            MouseEventKind::Moved => {
                // Dispatch MouseMove hook to plugins (fire-and-forget, no blocking check)
                {
                    // Find content rect for the split under the mouse
                    let content_rect = self
                        .active_layout()
                        .split_areas
                        .iter()
                        .find(|(_, _, content_rect, _, _, _)| in_rect(col, row, *content_rect))
                        .map(|(_, _, rect, _, _, _)| *rect);

                    let (content_x, content_y) = content_rect.map(|r| (r.x, r.y)).unwrap_or((0, 0));

                    self.plugin_manager.read().unwrap().run_hook(
                        "mouse_move",
                        HookArgs::MouseMove {
                            column: col,
                            row,
                            content_x,
                            content_y,
                        },
                    );
                }

                // Only re-render if hover target actually changed
                // (preserve needs_render if already set, e.g., for GPM cursor updates)
                let hover_changed = self.update_hover_target(&tree, col, row);
                needs_render = needs_render || hover_changed;

                // Ctrl+hover over a resolvable path in the live terminal
                // underlines it to signal it's clickable. RULING: stays
                // beside (not inside) the `HoverTarget` walk, like its
                // click half stays pre-walk — the tracker is a
                // modifier-keyed regex probe over terminal-grid CONTENT,
                // not a surface-naming question; the walk names chrome,
                // content trackers own their reactions (the same seam as
                // `update_lsp_hover_state` below).
                let term_link_changed =
                    self.update_terminal_link_hover(col, row, mouse_event.modifiers);
                needs_render = needs_render || term_link_changed;

                // Track LSP hover state for mouse-triggered hover popups
                self.update_lsp_hover_state(col, row);

                // Bare icon buttons inside a panel (the dock's `×`) light up
                // under the pointer, the way the tab and file explorer `×`
                // do. Tracked off the same motion events as the dock's
                // scrollbar reveal (`Dock::on_hover_change`), and likewise
                // re-rendering only on the enter/leave transition.
                needs_render = self.update_widget_hover(col, row, None) || needs_render;
            }
            MouseEventKind::ScrollUp => {
                self.begin_wheel_scroll(&tree, col, row, mouse_event.modifiers, -1)?;
                needs_render = true;
            }
            MouseEventKind::ScrollDown => {
                self.begin_wheel_scroll(&tree, col, row, mouse_event.modifiers, 1)?;
                needs_render = true;
            }
            MouseEventKind::ScrollLeft => {
                // Native horizontal scroll left
                self.handle_horizontal_scroll(&tree, col, row, -WHEEL_COLUMNS)?;
                needs_render = true;
            }
            MouseEventKind::ScrollRight => {
                // Native horizontal scroll right
                self.handle_horizontal_scroll(&tree, col, row, WHEEL_COLUMNS)?;
                needs_render = true;
            }
            MouseEventKind::Down(MouseButton::Right) => {
                // One walk for every right-click flavor: the overlay
                // prompt's guard box swallows (mouse-modal), the theme
                // inspector's trigger claims Ctrl+Right-Click, and the
                // routable surfaces below take plain right-clicks.
                self.handle_right_click(&tree, col, row, mouse_event.modifiers)?;
                needs_render = true;
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

        self.active_window_mut().mouse_state.last_position = Some((col, row));
        Ok(needs_render)
    }

    /// Detect double/triple clicks and update click-tracking state.
    fn detect_multi_click(
        &mut self,
        mouse_event: &crossterm::event::MouseEvent,
        col: u16,
        row: u16,
    ) -> (bool, bool) {
        use crossterm::event::{MouseButton, MouseEventKind};
        if !matches!(mouse_event.kind, MouseEventKind::Down(MouseButton::Left)) {
            return (false, false);
        }
        let now = self.time_source.now();
        let threshold = std::time::Duration::from_millis(self.config.editor.double_click_time_ms);
        let is_consecutive = if let (Some(prev_time), Some(prev_pos)) = (
            self.active_window_mut().previous_click_time,
            self.active_window_mut().previous_click_position,
        ) {
            now.duration_since(prev_time) < threshold && prev_pos == (col, row)
        } else {
            false
        };
        if is_consecutive {
            self.active_window_mut().click_count += 1;
        } else {
            self.active_window_mut().click_count = 1;
        }
        self.active_window_mut().previous_click_time = Some(now);
        self.active_window_mut().previous_click_position = Some((col, row));
        let is_triple = self.active_window_mut().click_count >= 3;
        let is_double = self.active_window_mut().click_count == 2;
        if is_triple {
            self.active_window_mut().click_count = 0;
            self.active_window_mut().previous_click_time = None;
            self.active_window_mut().previous_click_position = None;
        }
        (is_double, is_triple)
    }

    /// Dispatch a vertical scroll event (ScrollUp/ScrollDown): Shift
    /// pans horizontally; otherwise the wheel scans the chrome tree
    /// top-down (`hit_stack`), offering each box to its owning
    /// component until one consumes — surfaces with no wheel handler
    /// decline, so the wheel keeps falling (scroll chaining) down to
    /// the `chrome:base` fallback.
    /// THE wheel dispatch engine — one walk for both axes. Build the
    /// per-event chrome tree, scan the boxes under the point top-down
    /// (`hit_stack`), offer the delta to each box's owning component
    /// (`on_wheel` / `on_hwheel` by axis) until one consumes.
    /// Deliberately NO opacity gate: wheel chains through declining
    /// surfaces (scroll chaining). Deliberately NO per-surface dedup
    /// either (unlike `dispatch_pointer`): chaining wants every box's
    /// own at-bound check, including a second box of the same surface.
    /// Adding a scroll surface never touches this — write a
    /// component, register it, contribute boxes.
    fn dispatch_wheel(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        horizontal: bool,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        for i in crate::widgets::layout_box::hit_stack(tree, row as u32, col as u32) {
            let b = &tree[i];
            let c = super::chrome::components()[b.owner];
            let disp = if horizontal {
                c.on_hwheel(self, &b.lb, col, row, delta)?
            } else {
                c.on_wheel(self, &b.lb, col, row, delta)?
            };
            match disp {
                super::chrome::Disposition::Consumed => return Ok(()),
                super::chrome::Disposition::PassAfter | super::chrome::Disposition::Pass => {}
            }
        }
        Ok(())
    }

    /// Take one notch of the wheel, `direction` being -1 for up and +1
    /// for down.
    ///
    /// A notch is worth `mouse_wheel_scroll_lines` lines. The first
    /// lands immediately, so the view answers the wheel on the same
    /// frame; the rest are owed and walked one at a time by
    /// [`Self::step_pending_wheel_scroll`], which is what makes a
    /// multi-line notch slide rather than jump.
    fn begin_wheel_scroll(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
        direction: i32,
    ) -> AnyhowResult<()> {
        // Shift turns the wheel horizontal (same engine, other axis).
        // That pans by columns, which the line-oriented setting has
        // nothing to say about and there is no line-by-line walk for.
        if modifiers.contains(crossterm::event::KeyModifiers::SHIFT) {
            self.flush_pending_wheel_scroll(tree)?;
            return self.dispatch_wheel(tree, true, col, row, direction * WHEEL_COLUMNS);
        }

        // A zero would make the wheel dead; the config's own clamp is
        // not load-bearing here, but a hand-edited file reaches this too.
        let lines = self.config.editor.mouse_wheel_scroll_lines.max(1) as u32;
        // Nothing to walk for a single-line notch, and a user who turned
        // motion off gets the jump.
        let walk = lines > 1 && self.config.editor.smooth_scroll && self.config.editor.animations;
        if !walk {
            self.flush_pending_wheel_scroll(tree)?;
            return self.dispatch_wheel(tree, false, col, row, direction * lines as i32);
        }

        // A flick sends notches faster than they can be walked, so the
        // lines the last one still owed carry over into this one — the
        // walk tracks a single running total rather than a queue of
        // notches. One aimed elsewhere, or the other way, cannot carry
        // over; its lines are handed to the surface they were routed to
        // instead, so a nudge of the mouse mid-scroll cannot swallow
        // distance. Either way nothing is dropped.
        let carried = match self.pending_wheel_scroll.take() {
            Some(pending)
                if pending.direction == direction && (pending.col, pending.row) == (col, row) =>
            {
                pending.remaining
            }
            Some(pending) => {
                self.deliver_owed(tree, &pending)?;
                0
            }
            None => 0,
        };
        self.dispatch_wheel(tree, false, col, row, direction)?;
        self.pending_wheel_scroll = Some(PendingWheelScroll {
            col,
            row,
            direction,
            remaining: carried + lines - 1,
            max_backlog: lines * 2,
            last_step: Instant::now(),
        });
        Ok(())
    }

    /// Hand a gesture the lines it still owes, all at once, to the
    /// surface its own notches were routed to.
    fn deliver_owed(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        pending: &PendingWheelScroll,
    ) -> AnyhowResult<()> {
        if pending.remaining == 0 {
            return Ok(());
        }
        let delta = pending.direction * pending.remaining as i32;
        self.dispatch_wheel(tree, false, pending.col, pending.row, delta)
    }

    /// End any playing-out gesture, delivering what it still owes rather
    /// than dropping it. A wheel turned sideways, or a walk switched off
    /// mid-gesture, must not cost the view the distance already asked
    /// for.
    fn flush_pending_wheel_scroll(
        &mut self,
        tree: &[super::chrome::ChromeBox],
    ) -> AnyhowResult<()> {
        let Some(pending) = self.pending_wheel_scroll.take() else {
            return Ok(());
        };
        self.deliver_owed(tree, &pending)
    }

    /// True while a wheel gesture still owes lines. The event loop keeps
    /// producing frames while it does — without them the walk would
    /// stall part-way through a notch.
    pub fn has_pending_wheel_scroll(&self) -> bool {
        self.pending_wheel_scroll.is_some()
    }

    /// When the next owed line comes due, for the loop's wait.
    pub fn pending_wheel_scroll_deadline(&self) -> Option<Instant> {
        self.pending_wheel_scroll
            .as_ref()
            .map(|pending| pending.last_step + SMOOTH_SCROLL_LINE)
    }

    /// Hand the surface under the pointer however many lines a
    /// playing-out wheel gesture owes it by now. Called once per frame,
    /// before layout, so the lines land in the frame about to be
    /// painted.
    pub(crate) fn step_pending_wheel_scroll(&mut self) {
        let Some(pending) = self.pending_wheel_scroll.as_mut() else {
            return;
        };
        let due = lines_to_deliver(
            pending.last_step.elapsed(),
            SMOOTH_SCROLL_LINE,
            pending.remaining,
            pending.max_backlog,
        );
        if due == 0 {
            return;
        }
        let (col, row, direction) = (pending.col, pending.row, pending.direction);
        pending.remaining -= due;
        // Advance by what was delivered rather than to now, so a late
        // frame does not push the rest of the walk back with it.
        pending.last_step += SMOOTH_SCROLL_LINE * due;
        if pending.remaining == 0 {
            self.pending_wheel_scroll = None;
        }

        let tree = super::chrome::chrome_tree(self);
        if let Err(err) = self.dispatch_wheel(&tree, false, col, row, direction * due as i32) {
            // The gesture is already cleared or decremented above, so a
            // failed replay drops those lines rather than retrying into
            // the same error every frame.
            tracing::warn!("smooth scroll step failed: {err}");
        }
    }

    /// Route a horizontal scroll (Shift+wheel, native ScrollLeft /
    /// ScrollRight) through the SAME engine as every other gesture —
    /// surfaces with a horizontal axis (split panes, tab strips)
    /// claim their boxes; everything else declines and the base
    /// drops it.
    pub(super) fn handle_horizontal_scroll(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<()> {
        self.dispatch_wheel(tree, true, col, row, delta)
    }

    /// Update the current hover target based on mouse position.
    /// Returns true if a re-render is needed. This is the generic
    /// engine only: walk the tree for the new target, diff, store,
    /// then offer the transition to every registered component
    /// (`on_hover_change`) — the per-surface hover REACTIONS (menu
    /// auto-switch/submenu machine, context-menu highlight, explorer
    /// tooltip) live with their components, not here.
    pub(super) fn update_hover_target(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
    ) -> bool {
        // Same cell, same TREE SEQUENCE → nothing below can produce a
        // different answer: `ui_tree_seq` advances only when `chrome_tree`
        // actually rebuilds, and a non-rebuild is a VALIDATED claim (gen
        // match + overlay-stack equality, see `chrome_tree`) that the
        // tree's inputs didn't change — so the walk and every hover
        // reaction already ran for this exact (col, row) against this
        // exact tree. Terminals emit Moved events far faster than the
        // cell grid changes, so this collapses the common motion burst to
        // one walk per cell. If a hover reaction mutates state, the next
        // event's tree query misses its memo, the seq advances, and this
        // key misses with it.
        if self.hover_cell_memo.get() == Some((self.ui_tree_seq.get(), col, row)) {
            return false;
        }
        let old_target = self.active_window_mut().mouse_state.hover_target.clone();
        let new_target = self.compute_hover_target(tree, col, row);
        let mut needs_render = old_target != new_target;
        self.active_window_mut().mouse_state.hover_target = new_target.clone();
        for c in super::chrome::components() {
            needs_render |=
                c.on_hover_change(self, old_target.as_ref(), new_target.as_ref(), col, row);
        }
        self.hover_cell_memo
            .set(Some((self.ui_tree_seq.get(), col, row)));
        needs_render
    }

    /// Update LSP hover state based on mouse position
    /// Tracks position for debounced hover requests
    ///
    /// Hover popup stays visible when:
    /// - Mouse is over the hover popup itself
    /// - Mouse is within the hovered symbol range
    ///
    /// Hover is dismissed when mouse leaves the editor area entirely.
    ///
    /// RULING — this pipeline stays OUTSIDE the `HoverTarget` walk: it
    /// is not a "name the surface under the pointer" question but a
    /// debounced request state machine over BUFFER content (symbol
    /// ranges, popup keep-alive, request dedup) whose transitions the
    /// walk's enter/leave diff cannot express. It composes with the
    /// walk the same way `update_terminal_link_hover` does: the walk
    /// names chrome, these trackers own editor-content reactions.
    /// Folding it in is recorded in the plan doc as part of the
    /// mounted-panel/hover unification arc, not chrome registration.
    fn update_lsp_hover_state(&mut self, col: u16, row: u16) {
        tracing::trace!(col, row, "update_lsp_hover_state: raw mouse position");

        // Suppress LSP hover when a popup is already visible (the theme
        // info popup or the status-bar LSP status popup — both hand
        // -listed because neither declares an overlay layer) to avoid
        // hover tooltips overlapping other popups. Same for any modal
        // overlay (Open File dialog, command palette, menu, native
        // context menus, …), all DERIVED from `modal_overlay_active`:
        // mouse positions over the overlay map to the buffer *behind*
        // it, so tracking them would fire hover requests for invisible
        // content and render the popup on top of the dialog
        // (sinelaw/fresh#2912). (An open context menu used to be a
        // third hand-listed check here; its ContextMenu layer already
        // makes `modal_overlay_active` true, so the check was a
        // redundant second encoding.)
        if self.active_window_mut().theme_info_popup.is_some()
            || self.is_lsp_status_popup_open()
            || self.modal_overlay_active()
        {
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        }

        // Check if mouse is over a transient popup - if so, keep hover active
        if self.is_mouse_over_transient_popup(col, row) {
            return;
        }

        // Find which split the mouse is over
        let split_info = self
            .active_layout()
            .split_areas
            .iter()
            .find(|(_, _, content_rect, _, _, _)| in_rect(col, row, *content_rect))
            .map(|(split_id, buffer_id, content_rect, _, _, _)| {
                (*split_id, *buffer_id, *content_rect)
            });

        let Some((split_id, buffer_id, content_rect)) = split_info else {
            // Mouse is not over editor content - clear hover state and dismiss popup
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
                self.dismiss_transient_popups();
            }
            return;
        };

        // Get cached mappings and gutter width for this split
        let cached_mappings = self
            .active_layout()
            .view_line_mappings
            .get(&split_id)
            .cloned();
        let gutter_width = self
            .buffers()
            .get(&buffer_id)
            .map(|s| s.margins.left_total_width() as u16)
            .unwrap_or(0);
        let fallback = self
            .buffers()
            .get(&buffer_id)
            .map(|s| s.buffer.len())
            .unwrap_or(0);

        // Get compose width for this split
        let compose_width = self
            .windows
            .get(&self.active_window)
            .and_then(|w| w.buffers.splits())
            .map(|(_, vs)| vs)
            .expect("active window must have a populated split layout")
            .get(&split_id)
            .and_then(|vs| vs.compose_width);

        // Convert screen position to buffer byte position
        let Some(byte_pos) = super::click_geometry::screen_to_buffer_position(
            col,
            row,
            content_rect,
            gutter_width,
            &cached_mappings,
            fallback,
            false, // Don't include gutter
            compose_width,
        ) else {
            // Mouse is in the gutter — stop tracking a pending request but keep
            // any existing popup visible. The popup is only dismissed when the
            // mouse leaves the editor area entirely (see docstring).
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
            }
            return;
        };

        // Check if mouse is past the end of line content - don't trigger hover for empty space
        let content_col = col.saturating_sub(content_rect.x);
        let text_col = content_col.saturating_sub(gutter_width) as usize;
        let visual_row = row.saturating_sub(content_rect.y) as usize;

        let line_info = cached_mappings
            .as_ref()
            .and_then(|mappings| mappings.get(visual_row))
            .map(|line_mapping| {
                (
                    line_mapping.visual_to_char.len(),
                    line_mapping.line_end_byte,
                )
            });

        let is_past_line_end_or_empty = line_info
            .map(|(line_len, _)| {
                // Empty lines (just newline) should not trigger hover
                if line_len <= 1 {
                    return true;
                }
                text_col >= line_len
            })
            // If mouse is below all mapped lines (no mapping), don't trigger hover
            .unwrap_or(true);

        tracing::trace!(
            col,
            row,
            content_col,
            text_col,
            visual_row,
            gutter_width,
            byte_pos,
            ?line_info,
            is_past_line_end_or_empty,
            "update_lsp_hover_state: position check"
        );

        if is_past_line_end_or_empty {
            tracing::trace!(
                "update_lsp_hover_state: mouse past line end or empty line, clearing hover"
            );
            // Mouse is past end of line content — stop tracking a pending
            // request but keep any existing popup visible. The popup is only
            // dismissed when the mouse leaves the editor area entirely
            // (see docstring).
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
            }
            return;
        }

        // Check if mouse is within the hovered symbol range - if so, keep hover active
        if let Some((start, end)) = self.active_window_mut().hover.symbol_range() {
            if byte_pos >= start && byte_pos < end {
                // Mouse is still over the hovered symbol - keep hover state
                return;
            }
        }

        // Check if we're still hovering the same position in the same buffer
        if let Some((old_pos, _, _, _, old_buf)) =
            self.active_window_mut().mouse_state.lsp_hover_state
        {
            if old_pos == byte_pos && old_buf == buffer_id {
                // Same position - keep existing state
                return;
            }
            // Position changed outside the hovered symbol range. Don't dismiss
            // the popup here: a new hover request will fire after the debounce
            // and replace the popup naturally if the mouse settles on another
            // symbol. Dismissing eagerly tore the popup down whenever the
            // mouse passed through whitespace between two words (issue #692).
        }

        // Start tracking new hover position (remembering which buffer the
        // pointer is over, so the request targets that buffer — not the
        // active one — see `lsp_hover_state`).
        self.active_window_mut().mouse_state.lsp_hover_state =
            Some((byte_pos, std::time::Instant::now(), col, row, buffer_id));
        self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
    }

    /// Check if mouse position is over a transient popup (hover, signature help)
    fn is_mouse_over_transient_popup(&self, col: u16, row: u16) -> bool {
        let layouts = popup_areas_to_layout_info(&self.active_chrome().popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_transient_popup(col, row)
    }

    /// Check if mouse position is over any popup (including non-transient ones like completion)
    pub(super) fn is_mouse_over_any_popup(&self, col: u16, row: u16) -> bool {
        // Editor-level popup overlays absorb every click within their outer
        // rect so the buffer below doesn't receive a stray cursor placement.
        for (_, popup_area, _, _, _) in &self.active_chrome().global_popup_areas {
            if in_rect(col, row, *popup_area) {
                return true;
            }
        }
        // The prompt's suggestions popup also absorbs clicks across its full
        // outer rect (border + items): clicking the chrome must not move the
        // buffer cursor below.
        if let Some(outer) = self.active_chrome().suggestions_outer_area {
            if in_rect(col, row, outer) {
                return true;
            }
        }
        let layouts = popup_areas_to_layout_info(&self.active_chrome().popup_areas);
        let hit_tester = PopupHitTester::new(&layouts, &self.active_state().popups);
        hit_tester.is_over_popup(col, row)
    }

    /// Check if mouse position is over the file browser popup
    pub(super) fn is_mouse_over_file_browser(&self, col: u16, row: u16) -> bool {
        self.active_window()
            .file_browser_layout
            .as_ref()
            .is_some_and(|layout| layout.contains(col, row))
    }

    // `split_at_position` lives on `impl Window` — call it via
    // `self.active_window().split_at_position(col, row)`.

    /// Compute what hover target is at the given position
    fn compute_hover_target(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
    ) -> Option<HoverTarget> {
        // The hover surfaces, as chrome boxes — the same geometric walk
        // as wheel/click/right-click/double-click, in query form: the
        // highest-z box whose handler names a target wins, and handlers
        // whose geometry is finer than their rectangle (context-menu
        // borders, tab-bar background) decline so the point falls
        // through to the boxes below. No per-surface dedup here
        // (unlike `dispatch_pointer`): the first `Some` ends the walk,
        // so re-offering a surface's second box is harmless and dedup
        // would only add state.
        for i in crate::widgets::layout_box::hit_stack(tree, row as u32, col as u32) {
            let b = &tree[i];
            if let Some(t) = super::chrome::components()[b.owner].hover(self, &b.lb, col, row) {
                return Some(t);
            }
            // Opacity gate: a declining opaque surface (a popup) stops
            // the scan — nothing beneath it is hoverable through it.
            if b.lb.pointer_opaque {
                return None;
            }
        }
        None
    }

    /// Handle mouse double click (down event)
    /// Double-click in editor area selects the word under the cursor:
    /// the suggestion-confirm (#1660), overlay swallow, popup
    /// block/dismiss guard, file-open dialog, explorer body, and the
    /// split word-select arm are all component arms in the engine's
    /// one scan — no post-walk special cases.
    pub(super) fn handle_mouse_double_click(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        self.dispatch_pointer(
            tree,
            super::chrome::PointerPress::Double,
            col,
            row,
            crossterm::event::KeyModifiers::empty(),
        )
    }

    /// Handle mouse triple click (down event)
    /// Triple-click in editor area selects the entire line under the
    /// cursor — same engine, same arms (the Splits line-select arm
    /// takes what the overlay/popup guards let through).
    pub(super) fn handle_mouse_triple_click(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
    ) -> AnyhowResult<()> {
        self.dispatch_pointer(
            tree,
            super::chrome::PointerPress::Triple,
            col,
            row,
            crossterm::event::KeyModifiers::empty(),
        )
    }

    /// True while a floating-overlay prompt (e.g. Live Grep / Universal
    /// Search) owns the screen. Such overlays are **mouse-modal**: their own
    /// targets (result list, scrollbar, and — once wired — toolbar controls)
    /// are handled, but every other click is swallowed so it never lands in
    /// the buffer below and moves its cursor. Bottom-anchored (non-overlay)
    /// prompts are unaffected.
    pub(super) fn overlay_prompt_active(&self) -> bool {
        self.active_window()
            .prompt
            .as_ref()
            .is_some_and(|p| p.overlay)
    }

    /// THE pointer dispatch engine — ONE walk for every press kind
    /// (left, right, double, triple). Build the per-event chrome
    /// tree, scan the boxes under the point top-down (`hit_stack`),
    /// offer the press to each box's owning component, and honor the
    /// dispositions: `Consumed` stops the walk, `PassAfter` acts then
    /// continues (guards), and a DECLINED opaque box absorbs the
    /// press (nothing routes through a popup). Multi-box surfaces
    /// (one box per popup / dropdown level) are dispatched once per
    /// surface — their handlers resolve by position over the whole
    /// collection; the dedup is keyed on (owner, kind) so it never
    /// couples two components through the flat kind-string namespace
    /// (a reused string in another component still dispatches).
    /// DEDUP POLICY per walk, deliberate: presses dedup (this walk);
    /// the wheel walk offers EVERY hit box (scroll chaining wants
    /// each surface's own bound check); the hover walk offers every
    /// box too (first Some wins, dedup would be a no-op). Adding a
    /// chrome surface never touches this engine: write a component,
    /// register it, contribute boxes.
    fn dispatch_pointer(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        press: super::chrome::PointerPress,
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        let mut seen = std::collections::HashSet::new();
        for i in crate::widgets::layout_box::hit_stack(tree, row as u32, col as u32) {
            let b = &tree[i];
            if !seen.insert((b.owner, b.lb.kind)) {
                continue;
            }
            let ev = super::chrome::ChromePointer {
                press,
                col,
                row,
                modifiers,
            };
            let disp = super::chrome::components()[b.owner].on_pointer(self, &b.lb, &ev)?;
            // Consumed stops; the PassAfter/Pass-vs-opacity contract is
            // `pointer_walk_step` (pure, unit-tested).
            if disp == super::chrome::Disposition::Consumed {
                return Ok(());
            }
            if super::chrome::pointer_walk_step(disp, b.lb.pointer_opaque)
                == super::chrome::PointerWalkStep::Stop
            {
                break;
            }
        }
        Ok(())
    }

    pub(super) fn handle_mouse_click(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        // (The centered modal's precedence over everything here is the
        // FloatingModal component's whole-channel capture — this path
        // is unreachable while it is up. Dock routing — column clicks,
        // the resize-border grab, blur-on-outside — is the Dock
        // component's boxes and arms in the engine's scan.)
        self.dispatch_pointer(tree, super::chrome::PointerPress::Left, col, row, modifiers)
    }

    /// Handle mouse drag event
    pub(super) fn handle_mouse_drag(&mut self, col: u16, row: u16) -> AnyhowResult<()> {
        use super::chrome::PointerGrab;
        // THE grab slot: the press-to-release owner derived from live
        // drag state (`chrome::pointer_grab`) routes every motion —
        // no re-hit-testing mid-drag (the btop-resize ruling), no
        // hand-ordered flag ladder. `pointer_grab`'s check order
        // preserves the old ladder's precedence.
        let Some(grab) = super::chrome::pointer_grab(self) else {
            return Ok(());
        };
        // Mouse-modal overlay: the only legitimate drags are the
        // overlay's own result-list scrollbar and the grabs the old
        // ladder ran ahead of the swallow (dock resize, widget text,
        // widget scrollbar). Anything else — text selection in the
        // buffer, a buffer scrollbar behind the overlay — is
        // swallowed so the buffer stays put.
        if self.overlay_prompt_active()
            && !matches!(
                grab,
                PointerGrab::PromptScrollbar
                    | PointerGrab::DockResize
                    | PointerGrab::WidgetText
                    | PointerGrab::WidgetScrollbar
            )
        {
            return Ok(());
        }
        match grab {
            // Dock resize drag: track the pointer column as the new dock
            // width (the right border follows the cursor), clamped so it
            // can't swallow the chrome.
            PointerGrab::DockResize => {
                self.handle_dock_resize_drag(col);
            }
            // Drag-to-select on a widget markdown/text document: armed by the
            // press that placed the caret; every Drag extends the selection to
            // the pointer.
            PointerGrab::WidgetText => {
                self.handle_widget_text_selection_drag(col, row);
            }
            // Floating-panel list scrollbar drag — the modal panel
            // owns the input channel while it's up.
            PointerGrab::WidgetScrollbar => {
                let _ = self.try_widget_scrollbar_drag(super::PanelSlot::Dock, row)
                    || self.try_widget_scrollbar_drag(super::PanelSlot::Floating, row)
                    // Buffer-mounted panels (review-diff sidebar, Search &
                    // Replace) keep their tracks on the editor.
                    || self.try_split_widget_scrollbar_drag(row);
            }
            // Vertical scrollbar drag: update scroll position.
            PointerGrab::VScrollbar => {
                self.handle_vscrollbar_drag(col, row)?;
            }
            // Horizontal scrollbar drag: update horizontal scroll position.
            PointerGrab::HScrollbar => {
                self.handle_hscrollbar_drag(col, row)?;
            }
            // Selecting text in an info popup: extend the selection.
            PointerGrab::PopupSelect => {
                self.handle_popup_select_drag(col, row);
            }
            // The floating-overlay prompt's scrollbar (issue #1796):
            // update its scroll_offset using the same math as the
            // click handler. Same shared-widget logic the
            // popup-scrollbar drag uses below.
            PointerGrab::PromptScrollbar => {
                self.handle_prompt_scrollbar_drag(row);
            }
            // A buffer popup's scrollbar: update its scroll position.
            PointerGrab::PopupScrollbar => {
                self.handle_popup_scrollbar_drag(row);
            }
            // Split-separator drag: update the split ratio.
            PointerGrab::SplitSeparator => {
                if let Some((split_id, direction)) =
                    self.active_window_mut().mouse_state.dragging_separator
                {
                    self.handle_separator_drag(col, row, split_id, direction)?;
                }
            }
            // File-explorer border drag: update its width.
            PointerGrab::ExplorerWidth => {
                self.handle_file_explorer_border_drag(col)?;
            }
            // A drag whose press landed on a live terminal grid: this is
            // selection intent (a bare click only focuses — see
            // `handle_editor_click`). Drop the split into read-only scrollback
            // and start a normal text-selection drag anchored at the press.
            PointerGrab::TerminalSelectPending => {
                if let Some((split_id, buffer_id, ocol, orow)) =
                    self.active_window().mouse_state.terminal_drag_pending
                {
                    self.begin_terminal_grid_selection(split_id, buffer_id, ocol, orow, col, row)?;
                }
            }
            // Text-selection drag: extend from the anchor.
            PointerGrab::TextSelection => {
                self.handle_text_selection_drag(col, row)?;
            }
            // Tab drag: update position and compute the drop zone.
            PointerGrab::TabDrag => {
                self.handle_tab_drag(col, row)?;
            }
        }

        Ok(())
    }

    /// Handle right-click event — same engine. Ordering rides z; the
    /// anywhere-clears (tab "+" menu, close-split confirm) are the
    /// Splits component's top-band PassAfter guard, the overlay
    /// prompt's swallow and the theme inspector's Ctrl+Right trigger
    /// are boxes above the routable surfaces.
    pub(super) fn handle_right_click(
        &mut self,
        tree: &[super::chrome::ChromeBox],
        col: u16,
        row: u16,
        modifiers: crossterm::event::KeyModifiers,
    ) -> AnyhowResult<()> {
        self.dispatch_pointer(
            tree,
            super::chrome::PointerPress::Right,
            col,
            row,
            modifiers,
        )
    }

    /// Clear all in-progress drag state on the active window's mouse state.
    /// The active text/popup selection is intentionally preserved — only the
    /// drag bookkeeping fields are reset.
    fn clear_active_window_drag_state(&mut self) {
        let ms = &mut self.active_window_mut().mouse_state;
        ms.dragging_scrollbar = None;
        ms.drag_start_row = None;
        ms.drag_start_top_byte = None;
        ms.dragging_horizontal_scrollbar = None;
        ms.drag_start_hcol = None;
        ms.drag_start_left_column = None;
        ms.dragging_separator = None;
        ms.drag_start_position = None;
        ms.drag_start_ratio = None;
        ms.dragging_file_explorer = false;
        ms.drag_start_explorer_width = None;
        ms.dragging_text_selection = false;
        ms.drag_selection_split = None;
        ms.drag_selection_anchor = None;
        ms.drag_selection_by_words = false;
        ms.drag_selection_word_end = None;
        ms.terminal_drag_pending = None;
        ms.dragging_popup_scrollbar = None;
        ms.dragging_prompt_scrollbar = false;
        ms.selecting_in_popup = None;
    }
}

#[cfg(test)]
mod smooth_scroll_tests {
    use super::{lines_due, lines_to_deliver, SMOOTH_SCROLL_LINE};
    use std::time::Duration;

    /// The walk is paced by the clock: a line comes due once its share
    /// of time has passed, and not before.
    #[test]
    fn lines_come_due_one_interval_at_a_time() {
        assert_eq!(lines_due(Duration::ZERO, SMOOTH_SCROLL_LINE, 4), 0);
        assert_eq!(
            lines_due(
                SMOOTH_SCROLL_LINE - Duration::from_millis(1),
                SMOOTH_SCROLL_LINE,
                4
            ),
            0
        );
        assert_eq!(lines_due(SMOOTH_SCROLL_LINE, SMOOTH_SCROLL_LINE, 4), 1);
        assert_eq!(lines_due(SMOOTH_SCROLL_LINE * 3, SMOOTH_SCROLL_LINE, 4), 3);
    }

    /// A frame rate too slow to show the walk collapses it back into a
    /// jump rather than letting the view lag behind the wheel: a caller
    /// away long enough for every remaining line gets all of them at
    /// once, and never more than it is owed.
    #[test]
    fn a_slow_frame_delivers_the_whole_remainder_at_once() {
        assert_eq!(
            lines_due(Duration::from_millis(250), SMOOTH_SCROLL_LINE, 4),
            4
        );
        assert_eq!(lines_due(Duration::from_secs(60), SMOOTH_SCROLL_LINE, 2), 2);
        // Nothing owed, nothing delivered, however long the wait.
        assert_eq!(lines_due(Duration::from_secs(60), SMOOTH_SCROLL_LINE, 0), 0);
    }

    /// A zero interval is a walk with no pacing at all — everything at
    /// once, rather than a division by zero.
    #[test]
    fn a_zero_interval_delivers_everything() {
        assert_eq!(lines_due(Duration::ZERO, Duration::ZERO, 7), 7);
    }

    /// Notches can arrive faster than a line a frame. Whatever the walk
    /// cannot pace out within the backlog rides along with this frame,
    /// so a flick covers exactly the ground it asked for — dropping the
    /// surplus instead would make fast scrolling travel less far than
    /// slow scrolling.
    #[test]
    fn a_backlog_past_the_limit_rides_along_with_this_frame() {
        // Within the backlog, only the clock decides.
        assert_eq!(
            lines_to_deliver(Duration::ZERO, SMOOTH_SCROLL_LINE, 6, 6),
            0
        );
        // Past it, the surplus comes too.
        assert_eq!(
            lines_to_deliver(Duration::ZERO, SMOOTH_SCROLL_LINE, 10, 6),
            4
        );
        // The clock still wins when it is further along.
        assert_eq!(
            lines_to_deliver(SMOOTH_SCROLL_LINE * 8, SMOOTH_SCROLL_LINE, 10, 6),
            8
        );
        // And never more than is owed.
        assert_eq!(
            lines_to_deliver(SMOOTH_SCROLL_LINE * 99, SMOOTH_SCROLL_LINE, 3, 6),
            3
        );
    }
}
