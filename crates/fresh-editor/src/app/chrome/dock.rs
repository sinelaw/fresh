//! The left dock column (orchestrator sessions panel).

use super::{ChromeComponent, Editor};

pub(crate) struct Dock;

impl ChromeComponent for Dock {
    // **No `on_pointer_moved` either, and the reason is the same shape.**
    // The dock's overlay scrollbar reveals itself while the pointer is over
    // the column, and this arm answered that by testing every motion event's
    // cell against `scrollbar_hover_zones` — rectangles the painter's
    // scrollbar pass recorded on its way past, so the reveal only worked
    // while there was a painter to record them.
    //
    // The column is a node, and a node knows when the pointer crosses its
    // edge: `view::shell::dock::column` reports it as `UiFact::DockHover`,
    // for a painted interior exactly as for a described one, because the
    // gesture wraps the `Host` leaf and the description alike.
    // **No `on_layer_key`.** A focused dock's keys are the tree's now
    // (`view::shell::panel::keys_layer`): a `Modality::Focus` layer confines
    // the keyboard to the panel without swallowing the shortcuts it does not
    // bind, which is what let this arm return `false` and blur instead. The
    // `ToggleDockFocus` pre-resolution that had to run ahead of the panel's
    // own dispatch — so the toggle stays symmetric once you have dived in —
    // went with it, into the fact's applier.
    //
    // The layer below still exists: `get_key_context` and the PTY gate read
    // it. What is gone is the rank, which said an open prompt, popup, menu or
    // modal takes a key before this does — and which the frame now says by
    // declaring this layer first, under all four.

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // Owns the keyboard only while focused; a blurred dock stays
        // visible but lets the buffer underneath keep the keyboard
        // AND receive PTY routing (the dock lives beside the chrome,
        // not over it).
        if let Some(d) = ed.dock.as_ref() {
            out.push((
                super::layer_rank::DOCK,
                Layer {
                    kind: LayerKind::Dock,
                    owns_keyboard: d.focused,
                    key_context: Some(crate::input::keybindings::KeyContext::Dock),
                    blocks_terminal_input: d.focused,
                },
            ));
        }
        // A focused sidebar plugin section is the dock's case with a
        // different slot (`app::sidebar`): the same non-modal layer, the same
        // context for the keys its widgets decline.
        if ed.focused_sidebar_panel().is_some() {
            out.push((
                super::layer_rank::DOCK,
                Layer {
                    kind: LayerKind::Dock,
                    owns_keyboard: true,
                    key_context: Some(crate::input::keybindings::KeyContext::Dock),
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}

/// Behavior owned by this component — the drag half of the
/// width-resize grab; the press half arms it in `on_pointer`, and the
/// release finalizer in `handle_mouse`'s Up arm persists the width.
impl Editor {
    /// Dock resize drag (`PointerGrab::DockResize`, armed by the grip's own
    /// press — see `view::shell::dock`): track the pointer column as the new
    /// dock width (the right border follows the cursor), clamped so it
    /// can't swallow the chrome.
    pub(crate) fn handle_dock_resize_drag(&mut self, col: u16) {
        let max_cols = self.terminal_width.max(20).saturating_sub(20).max(10);
        let new_w = col.saturating_add(1).clamp(10, max_cols);
        let mut changed = false;
        if let Some(fwp) = self.dock.as_mut() {
            if let crate::app::PanelPlacement::LeftDock { width_cols } = &mut fwp.placement {
                changed = *width_cols != new_w;
                *width_cols = new_w;
            }
        }
        if changed {
            // Persist the live width *before* relaying out. `relayout`
            // fires the `resize` hook, and the orchestrator answers it
            // by re-issuing the dock's responsive `dock_width`, which
            // `handle_floating_panel_control` clamps against the
            // persisted `dock_width` override. Updating that override
            // here (not only on mouse-up) lets the user's dragged width
            // win the round-trip — otherwise the responsive re-issue
            // snaps the dock straight back and the drag does nothing.
            self.dock_width = Some(new_w);
            // The dock got wider/narrower: reflow the chrome (terminals,
            // viewports, panels) to the new dock width via the funnel.
            self.relayout();
        }
    }
}
