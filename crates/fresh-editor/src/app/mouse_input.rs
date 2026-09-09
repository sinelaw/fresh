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
use crate::view::prompt::PromptType;
use anyhow::Result as AnyhowResult;
use std::time::{Duration, Instant};

/// Columns one notch of a sideways wheel pans. Horizontal panning has no
/// line-oriented setting to follow — `mouse_wheel_scroll_lines` counts lines
/// — so it takes the same fixed step a pan keystroke does, which is what
/// [`PAN_COLUMNS`] is for.
///
/// [`PAN_COLUMNS`]: crate::widgets::render::PAN_COLUMNS
const WHEEL_COLUMNS: i32 = crate::widgets::render::PAN_COLUMNS;

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

impl Editor {
    /// Handle a mouse event.
    /// Returns true if a re-render is needed.
    pub fn handle_mouse(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        // As `handle_key`: routed over a tree laid out from the facts as they
        // stand. What it leaves stale is decided where the event is spent —
        // a fact says whether it changed anything routing reads
        // (`UiFact::is_pointer_transient`), and the legacy walk, which cannot,
        // marks the description stale for every press and release it takes.
        self.lay_out_shell_if_stale();
        self.handle_mouse_routed(mouse_event)
    }

    fn handle_mouse_routed(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        self.handle_mouse_impl(mouse_event)
    }

    fn handle_mouse_impl(
        &mut self,
        mouse_event: crossterm::event::MouseEvent,
    ) -> AnyhowResult<bool> {
        use crossterm::event::{MouseButton, MouseEventKind};

        let col = mouse_event.column;
        let row = mouse_event.row;

        let (is_double_click, is_triple_click) = self.detect_multi_click(&mouse_event, col, row);

        // The modal mouse-capture band is gone. It walked the overlay stack
        // in rank order and gave the whole mouse channel to the first modal
        // that was up — a second routing engine, ahead of the shell's, and
        // the reason `placed_surface_outranks_shell` had to exist. A modal is
        // a `Modality::Exclusive` layer in the tree now, so the tree answers
        // the same question in the same walk as everything else.
        //
        // The event it routes stays here rather than travelling as a fact: a
        // full-screen modal's interior hit-tests rectangles its own painter
        // recorded and tells a drag from a move, which a tree `Event`
        // deliberately cannot. See `view::shell::modal`.
        self.shell_pointer_event = Some((mouse_event, is_double_click));

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

        // Then the migration shell. It used to run *after* a capture band,
        // because a full-screen modal had to outrank anything in the tree and
        // running the shell first would have inverted that — and it consulted
        // `placed_surface_outranks_shell` for the same reason at the level of
        // individual surfaces, restating the `z` a migrated box used to carry
        // so a modal drawn over the file explorer still owned its own cells.
        //
        // Both are gone. The modals are `Modality::Exclusive` layers, and no
        // placed box above the shell's band is left — the split grid's sit at
        // 70 and 80, below it. So there is one walk, and it is this one; the
        // legacy walk below stays the floor.
        //
        // Whether the tree took the event is reported by `dispatch`, not
        // inferred from whether it had anything to say: a hover moves a
        // highlight without claiming, and a right-click outside a menu closes
        // it while staying available to open the next one.
        //
        // Which press of a run this is, as the editor's own multi-click
        // detector saw it. The tree carries it to its handlers on
        // `Event::clicks`; nothing in the library counts.
        let clicks = if is_triple_click {
            3
        } else if is_double_click {
            2
        } else {
            1
        };
        // A notch is worth `mouse_wheel_scroll_lines` on the vertical axis and
        // `WHEEL_COLUMNS` sideways — the same rule `begin_wheel_scroll` states
        // for the walk below, because a surface that moved into the tree must
        // not scroll at a different speed from the one beside it.
        let wheel_lines = self.config.editor.mouse_wheel_scroll_lines.max(1) as i32;
        // **A multi-line notch slides rather than jumping**, and the split has
        // to happen here, ahead of dispatch. The first line goes with this
        // event and the rest are owed, walked one at a time by
        // `step_pending_wheel_scroll`. It used to sit between the tree's
        // dispatch and the legacy walk — which meant it applied only to the
        // notches the tree *declined*, and once a surface's wheel became a node
        // that was none of that surface's.
        let wheel_lines = self.arm_wheel_walk(mouse_event, col, row, wheel_lines);
        if let Some(input) =
            crate::view::shell::input::mouse(mouse_event, clicks, wheel_lines, WHEEL_COLUMNS)
        {
            let d = self.shell_dispatch(input);
            if d.claimed {
                return Ok(true);
            }
            // Declined, but not necessarily inert: a hover restyles the
            // surface under the pointer and lets the event go on to the
            // trackers below, so the frame is stale even though the walk
            // continues. See `Dispatched`.
            needs_render = needs_render || d.changed;
        }
        // **The legacy walk cannot say what it changed**, so a press or a
        // release it takes leaves the description stale for routing. A
        // motion report is spent on trackers and grabs that change nothing
        // routing reads, and a drag along a divider must not cost a layout
        // per report.
        if !matches!(
            mouse_event.kind,
            MouseEventKind::Moved | MouseEventKind::Drag(_)
        ) {
            self.shell_description_stale = true;
        }
        // A live terminal's own mouse, and the Ctrl+Click that opens a path it
        // printed. Both belong to a pane's *content*, and the pane's content
        // is a node — so its handlers ask this first, before placing a caret
        // (`Editor::pane_content_takes_pointer`). This call is for the event
        // kinds that node does not claim: a motion or a release mid-drag, and
        // anything landing outside a pane. One rule, asked wherever the
        // pointer can reach the content.
        if let Some(result) = self.pane_content_takes_pointer(col, row, mouse_event) {
            return result;
        }

        match mouse_event.kind {
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
                // A tab drop was finalized here, keyed on its grab. The
                // tab's node holds the pointer for the drag now, so the
                // release comes back to it (`UiFact::PaneTabDrop`) and never
                // reaches this walk.

                // Blanket sweep: every remaining drag flag drops here,
                // so no grab can outlive its release even if its
                // finalizer above was skipped.
                self.widget_text_drag = None;
                self.clear_active_window_drag_state();

                // The separator's reflow was here, keyed on its grab. It is
                // the divider node's own release now — the grip keeps the
                // pointer it took, so its release never reaches this walk.

                needs_render = true;
            }
            MouseEventKind::Moved => {
                // Dispatch MouseMove hook to plugins (fire-and-forget, no blocking check)
                {
                    // Where the pane under the pointer starts, so a plugin can
                    // turn a screen cell into a content one. `pane_content_at`
                    // is the one answer to that; this used to scan
                    // `split_areas` for it.
                    let (content_x, content_y) = self
                        .pane_content_at(col, row)
                        .map(|(_, r)| (r.x, r.y))
                        .unwrap_or((0, 0));

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

                // The cell-keyed fan-out was here, for one reaction: the
                // dock's overlay scrollbar. Its column is a node and reports
                // its own Enter and Leave, so every motion event no longer has
                // to be offered to every component to find out.

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

                // Track LSP hover state for mouse-triggered hover popups.
                // Dismissing the popup is a repaint, and this is the only
                // party that knows it happened — see the docstring.
                needs_render = self.update_lsp_hover_state(col, row) || needs_render;
            }
            _ => {
                // Ignore other mouse events for now
            }
        }

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

    /// Split one wheel notch into the line that lands now and the lines the
    /// walk still owes, returning the first. `lines` is the notch's full worth.
    ///
    /// A notch is worth `mouse_wheel_scroll_lines`. The first lands with the
    /// event itself, so the view answers the wheel on the same frame; the rest
    /// are owed and walked one at a time by [`Self::step_pending_wheel_scroll`],
    /// which is what makes a multi-line notch slide rather than jump.
    fn arm_wheel_walk(
        &mut self,
        ev: crossterm::event::MouseEvent,
        col: u16,
        row: u16,
        lines: i32,
    ) -> i32 {
        use crossterm::event::{KeyModifiers, MouseEventKind};
        // Only a vertical notch walks. Anything else — a press, a motion, a
        // sideways wheel — leaves the gesture in progress alone; it plays out
        // on its own frames.
        let direction = match ev.kind {
            MouseEventKind::ScrollDown => 1,
            MouseEventKind::ScrollUp => -1,
            _ => return lines,
        };
        // Shift turns the wheel horizontal. That pans by columns, which the
        // line-oriented setting has nothing to say about and there is no
        // line-by-line walk for — so it ends any gesture in flight rather than
        // letting one keep playing under a sideways scroll.
        let sideways = ev.modifiers.contains(KeyModifiers::SHIFT);
        // Nothing to walk for a single-line notch, and a user who turned
        // motion off gets the jump.
        let walk = !sideways
            && lines > 1
            && self.config.editor.smooth_scroll
            && self.config.editor.animations;
        if !walk {
            self.flush_pending_wheel_scroll();
            return lines;
        }

        // A flick sends notches faster than they can be walked, so the lines
        // the last one still owed carry over into this one — the walk tracks a
        // single running total rather than a queue of notches. One aimed
        // elsewhere, or the other way, cannot carry over; its lines are handed
        // to the surface they were routed to instead, so a nudge of the mouse
        // mid-scroll cannot swallow distance. Either way nothing is dropped.
        let lines = lines as u32;
        let carried = match self.pending_wheel_scroll.take() {
            Some(pending)
                if pending.direction == direction && (pending.col, pending.row) == (col, row) =>
            {
                pending.remaining
            }
            Some(pending) => {
                self.deliver_owed(&pending);
                0
            }
            None => 0,
        };
        self.pending_wheel_scroll = Some(PendingWheelScroll {
            col,
            row,
            direction,
            remaining: carried + lines - 1,
            max_backlog: lines * 2,
            last_step: Instant::now(),
        });
        1
    }

    /// Hand a gesture the lines it still owes, all at once, to the
    /// surface its own notches were routed to.
    fn deliver_owed(&mut self, pending: &PendingWheelScroll) {
        if pending.remaining == 0 {
            return;
        }
        self.deliver_wheel(
            pending.col,
            pending.row,
            pending.direction,
            pending.remaining,
        );
    }

    /// Hand `lines` of owed wheel to whatever is under `(col, row)`, through
    /// the same route a real notch takes.
    ///
    /// **One route.** The walk replays into the tree, exactly as the notch it
    /// came from did, so the surface that took the first line takes the rest
    /// — rather than the walk having a delivery path of its own that could
    /// route somewhere else.
    fn deliver_wheel(&mut self, col: u16, row: u16, direction: i32, lines: u32) {
        use crossterm::event::{KeyModifiers, MouseEvent, MouseEventKind};
        let ev = MouseEvent {
            kind: match direction {
                d if d > 0 => MouseEventKind::ScrollDown,
                _ => MouseEventKind::ScrollUp,
            },
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        };
        if let Some(input) = crate::view::shell::input::mouse(ev, 1, lines as i32, WHEEL_COLUMNS) {
            self.shell_dispatch(input);
        }
    }

    /// End any playing-out gesture, delivering what it still owes rather
    /// than dropping it. A wheel turned sideways, or a walk switched off
    /// mid-gesture, must not cost the view the distance already asked
    /// for.
    fn flush_pending_wheel_scroll(&mut self) {
        let Some(pending) = self.pending_wheel_scroll.take() else {
            return;
        };
        self.deliver_owed(&pending);
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

        self.deliver_wheel(col, row, direction, due);
    }

    /// Update LSP hover state based on mouse position
    /// Tracks position for debounced hover requests
    ///
    /// Hover popup stays visible when:
    /// - Mouse is over the hover popup itself
    /// - Mouse is within the hovered symbol range
    ///
    /// Hover is dismissed when the pointer leaves the editor's content — and
    /// **the condition there is the popup, not the request**. Those are two
    /// different facts, and gating the dismissal on the second one is what
    /// stranded tooltips (F.7). `lsp_hover_state` is the debounce state
    /// machine — *which byte a request is pending for* — and the branches
    /// below for the gutter and for the space past a line's end deliberately
    /// clear it while keeping the popup up, because passing over a line
    /// number must not tear the card down. So a pointer that left the editor
    /// *through the gutter* arrived at the leave-the-editor branch with the
    /// state already `None`, that branch skipped its own dismissal, and the
    /// card sat there until a key or a click removed it. What the branch
    /// means is "a transient popup is up and the pointer has left", so that
    /// is what it now asks: `popups.is_visible()` with a `transient` popup on
    /// top — the same three facts `is_mouse_over_transient_popup` reads, and
    /// exactly the popup `dismiss_transient_popups` would take down.
    ///
    /// One transient popup on that stack is **not** this pipeline's: the file
    /// explorer's status tooltip, which `FileExplorer::on_hover_change` shows
    /// while the pointer rests on a status indicator. The sidebar is not pane
    /// content, so this branch runs on the very same motion event that put
    /// the tooltip up and would pop it before it was ever painted. Hovering
    /// that indicator is therefore held out by name — see
    /// `chrome_owns_transient_popup`.
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
    ///
    /// **Reports whether the frame is stale**, which is the half of
    /// `update_hover_target` that had no replacement. That walk returned "the
    /// target moved, redraw" and every tracker rode on it; the tree reports
    /// its own hover now, but a pointer over ground the tree does not describe
    /// — the `~` filler past the last line, the padding right of the text —
    /// crosses no element and produces neither a message nor a mutation. So
    /// the one transition here that changes pixels, dismissing the popup, has
    /// to say so itself. Nothing else does: clearing `lsp_hover_state` moves
    /// the request state machine, not the screen.
    fn update_lsp_hover_state(&mut self, col: u16, row: u16) -> bool {
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
                return true;
            }
            return false;
        }

        // Check if mouse is over a transient popup - if so, keep hover active
        if self.is_mouse_over_transient_popup(col, row) {
            return false;
        }

        // Which split the mouse is over, and the rectangle to project through.
        let split_info = self
            .pane_content_at(col, row)
            .and_then(|(pane, rect)| Some((pane, self.active_window().pane_buffer(pane)?, rect)));

        let Some((split_id, buffer_id, content_rect)) = split_info else {
            // Mouse is not over editor content. Two independent things happen
            // here, and the bug was treating them as one: the pending request
            // is dropped *if there is one*, and the popup comes down *if there
            // is one*. Neither implies the other — see the docstring.
            if self
                .active_window_mut()
                .mouse_state
                .lsp_hover_state
                .is_some()
            {
                self.active_window_mut().mouse_state.lsp_hover_state = None;
                self.active_window_mut().mouse_state.lsp_hover_request_sent = false;
            }
            if self.transient_popup_showing() && !self.chrome_owns_transient_popup() {
                self.dismiss_transient_popups();
                // Only this path repainted, so only this path is stale.
                return true;
            }
            return false;
        };

        // The rows the pane's last text pass drew, as its leaf keeps them,
        // and the gutter width for this split.
        let cached_mappings = self
            .active_window()
            .pane_view(split_id)
            .map(|v| v.rows.clone());
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
            cached_mappings.as_deref(),
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
            return false;
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
            return false;
        }

        // Check if mouse is within the hovered symbol range - if so, keep hover active
        if let Some((start, end)) = self.active_window_mut().hover.symbol_range() {
            if byte_pos >= start && byte_pos < end {
                // Mouse is still over the hovered symbol - keep hover state
                return false;
            }
        }

        // Check if we're still hovering the same position in the same buffer
        if let Some((old_pos, _, _, _, old_buf)) =
            self.active_window_mut().mouse_state.lsp_hover_state
        {
            if old_pos == byte_pos && old_buf == buffer_id {
                // Same position - keep existing state
                return false;
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
        false
    }

    /// Is the pointer over a transient popup (hover, signature help)?
    ///
    /// The LSP hover keep-alive's one question, asked directly. It used to go
    /// through `view::popup_mouse` — a `PopupHitTester` over a
    /// `Vec<PopupLayoutInfo>` built by converting the cached tuples into a
    /// struct with seven fields, of which this reads one. That module's other
    /// half (click, hover, drag dispatch) was replaced by the popups
    /// component and then by the tree; what was left was scaffolding around a
    /// rectangle test, with a doc comment naming a second caller that no
    /// longer exists.
    fn is_mouse_over_transient_popup(&self, col: u16, row: u16) -> bool {
        if !self.transient_popup_showing() {
            return false;
        }
        self.active_chrome()
            .popup_areas
            .iter()
            .any(|(_, outer, ..)| in_rect(col, row, *outer))
    }

    /// Is a transient popup (hover, signature help) actually on screen?
    ///
    /// The keep-alive's question minus the pointer, and the one the
    /// leave-the-editor dismissal wants: it names the popup
    /// `dismiss_transient_popups` would take down, so it is also the honest
    /// answer to "would dismissing change any pixels". Asking
    /// `lsp_hover_state.is_some()` instead — the gate that used to stand here
    /// — answers a different question entirely (is a debounced request
    /// pending), and the two come apart every time the pointer crosses the
    /// gutter.
    fn transient_popup_showing(&self) -> bool {
        let popups = &self.active_state().popups;
        popups.is_visible() && popups.top().is_some_and(|p| p.transient)
    }

    /// Is the transient popup on top one that a chrome surface under the
    /// pointer owns, rather than this pipeline?
    ///
    /// Only one is: the file explorer's git-status tooltip. It is pushed by
    /// `FileExplorer::on_hover_change` when the pointer enters a status
    /// indicator and popped by the same reaction when it leaves, so "the
    /// pointer is on a status indicator" and "that tooltip is up" are the
    /// same fact — no stale answer is possible. Hand-listed for the same
    /// reason the theme-info and LSP-status popups are hand-listed at the top
    /// of `update_lsp_hover_state`: a popup does not record who put it there.
    ///
    /// Without this, `update_lsp_hover_state` would dismiss the tooltip on
    /// the very motion event that created it — the shell's hover fact is
    /// applied first, then the same event falls through to the trackers, and
    /// the sidebar is not pane content, so the leave-the-editor branch runs
    /// and finds a transient popup on top.
    fn chrome_owns_transient_popup(&self) -> bool {
        matches!(
            self.hovered(),
            Some(crate::app::types::HoverTarget::FileExplorerStatusIndicator(
                _
            ))
        )
    }

    // `split_at_position` lives on `impl Window` — call it via
    // `self.active_window().split_at_position(col, row)`.

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

    /// Whether the pane's content under the pointer takes this event before
    /// anything the editor would do with it.
    ///
    /// Two things do. A **live terminal** that has asked for the mouse gets
    /// it; and a **Ctrl+Click on a path** the terminal printed opens it in
    /// Fresh, before normal click routing, so it does not disturb the cursor
    /// or the selection.
    ///
    /// Forwarding is suppressed in two cases, both of which would otherwise
    /// have their events swallowed by the PTY:
    ///
    /// * **A chrome drag is in progress** — a dock-border resize, a split
    ///   separator, the file-explorer width. That drag owns the mouse until
    ///   release, and an alternate-screen terminal must not take the motion
    ///   once the pointer crosses it. *Growing* the dock drags the cursor
    ///   rightward across a full-screen `btop`; forwarding there both stalls
    ///   the resize and eats the mouse-up that ends it, leaving the drag
    ///   stuck. Shrinking only ever worked because the pointer stays left of
    ///   the terminal.
    /// * **A native context menu is open** — the tab menu, the "+" menu, the
    ///   explorer's. They render over, and often overlap, an alternate-screen
    ///   terminal that has captured the mouse: right-clicking a terminal's tab
    ///   opens its menu directly over that terminal's content. The menu is a
    ///   `Modality::Inert` layer, so it does not claim a press aimed past it,
    ///   and this fork is what keeps the PTY from swallowing one before the
    ///   menu's own dismissal sees it.
    ///
    /// The opacity suppression that used to sit beside these is gone with the
    /// surfaces it derived from. It said a pointer-opaque chrome box over the
    /// cell — an info popup, the suggestions dropdown, the theme inspector —
    /// had to take the event, because forwarding it would inject mouse codes
    /// into the PTY *through* the popup. Every one of those is a node now, and
    /// the tree is offered the pointer first: a surface that claims stops the
    /// event there.
    ///
    /// `Some` means handled — nothing else should see the event.
    pub(super) fn pane_content_takes_pointer(
        &mut self,
        col: u16,
        row: u16,
        mouse_event: crossterm::event::MouseEvent,
    ) -> Option<AnyhowResult<bool>> {
        let chrome_drag_active = super::chrome::pointer_grab(self).is_some();
        let context_menu_open = self.active_window().context_menu_core().is_some();
        if !chrome_drag_active && !context_menu_open {
            let forwarding = self.config.terminal.mouse_forwarding;
            // Which terminal, and where its grid is: a question about the
            // shell's tree, so it is asked on this side and handed down.
            if let Some(at) = self.terminal_pane_at(col, row) {
                if let Some(result) = self.active_window_mut().try_forward_mouse_to_terminal(
                    col,
                    row,
                    at,
                    mouse_event,
                    forwarding,
                ) {
                    return Some(result);
                }
            }
        }
        self.try_open_terminal_link(col, row, mouse_event)
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
        // Mouse-modal overlay: the only legitimate drags are the grabs the
        // old ladder ran ahead of the swallow (dock resize, widget text,
        // widget scrollbar). Anything else — text selection in the buffer, a
        // buffer scrollbar behind the overlay — is swallowed so the buffer
        // stays put.
        //
        // The overlay's own result-list scrollbar used to be on this list.
        // It is not a grab any more: the list is a `fresh-ui` viewport and
        // `hit.rs` owns its thumb, capturing the pointer itself for the
        // duration of the drag, so nothing reaches this walk to be let
        // through.
        if self.overlay_prompt_active() && !matches!(grab, PointerGrab::WidgetText) {
            return Ok(());
        }
        #[allow(clippy::single_match, clippy::match_single_binding)]
        match grab {
            // Drag-to-select on a widget markdown/text document: armed by the
            // press that placed the caret; every Drag extends the selection to
            // the pointer.
            PointerGrab::WidgetText => {
                self.handle_widget_text_selection_drag(col, row);
            } // A panel's list scrollbar was an arm here — the dock's and the
              // modal's, then a buffer-mounted panel's. A described panel's
              // list is a viewport and `hit.rs` captures the pointer for its
              // own thumb, so the drag never reaches this walk.
              // The split separator's and the file explorer's width drags were
              // here, and so were both of a pane's scrollbars. All four are
              // nodes that capture the pointer, so their moves arrive as
              // `UiFact::GripDrag` / `UiFact::PaneScrollbarDrag` and never reach
              // this walk. The buffer's text selection and the live terminal
              // grid's selection intent were the last two: the pane's content
              // leaf captures the pointer on its press, so they arrive as
              // `UiFact::PaneContentDrag` (`Editor::drag_pane_content`).
        }

        Ok(())
    }

    /// Clear all in-progress drag state on the active window's mouse state.
    /// The active text/popup selection is intentionally preserved — only the
    /// drag bookkeeping fields are reset.
    pub(crate) fn clear_active_window_drag_state(&mut self) {
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
