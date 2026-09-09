//! The left dock column (orchestrator sessions panel).

use super::Editor;

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
