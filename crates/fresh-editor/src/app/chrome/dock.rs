//! The left dock column (orchestrator sessions panel).

use crate::app::types::HoverTarget;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Dock;

impl ChromeComponent for Dock {
    // No boxes. The column, its width grip, its wheel and the blur observer
    // are nodes in the shell's tree — see `view::shell::dock`. What is left
    // here is what the tree cannot yet own: the panel's *content* is a
    // plugin's `WidgetSpec`, so its scrollbar-reveal hover reads zones the
    // plugin publishes, and its keys ride the layer walk.
    fn collect(&self, _ed: &Editor, _t: &mut ChromeTreeBuilder) {}

    fn on_hover_change(
        &self,
        ed: &mut Editor,
        _old: Option<&HoverTarget>,
        _new: Option<&HoverTarget>,
        col: u16,
        row: u16,
    ) -> bool {
        // The dock's overlay scrollbar follows the pointer: reveal it
        // while the mouse is over the sessions list, hide it otherwise.
        // Tracked off the actual motion events we receive (not gated on
        // `mouse_hover_enabled`, which only governs terminal-level mode
        // 1003 — and is off by default on Windows): if a Moved event
        // arrives, use it. Keyed on col/row, not the target diff.
        // Re-render only on the enter/leave transition (not every
        // motion) so it fades in/out without churn.
        let now_over = ed
            .dock
            .as_ref()
            .map(|d| {
                d.scrollbar_hover_zones.iter().any(|z| {
                    col >= z.x && col < z.x + z.width && row >= z.y && row < z.y + z.height
                })
            })
            .unwrap_or(false);
        if let Some(d) = ed.dock.as_mut() {
            if d.scrollbar_zone_hovered != now_over {
                d.scrollbar_zone_hovered = now_over;
                return true;
            }
        }
        false
    }

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        layer: &crate::app::overlay::Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<anyhow::Result<crate::input::handler::InputResult>> {
        // Only while this layer owns the keyboard (a FOCUSED dock). A
        // blurred dock's layer is still visited by the walk (it
        // carries the Dock `KeyContext` for `get_key_context`) but
        // never claims keys. Riding the walk at rank DOCK (810) —
        // instead of the old pre-band `on_key` grab — means an open
        // prompt (850), popup (840), menu (860) or modal (900s) now
        // takes the key FIRST, exactly as `get_key_context` always
        // resolved it: the grab's rank inversion (Esc aimed at a
        // prompt blurring the dock instead) is gone.
        if !layer.owns_keyboard {
            return None;
        }
        // A focused dock swallows keys in the dispatch below, so the
        // global focus-toggle (default Alt+O) would never be able to
        // hand focus back to the editor once you've dived in. Resolve
        // it ahead of the dock's own key handling, so the toggle is
        // symmetric (same key in and out). Only the blur-out
        // direction needs this — focusing a blurred dock is ordinary
        // keybinding resolution (the editor owns the keyboard then).
        let ctx = ed.get_key_context();
        let resolved = ed.keybindings.read().ok().map(|kb| kb.resolve(event, ctx));
        if matches!(
            resolved,
            Some(crate::input::keybindings::Action::ToggleDockFocus)
        ) {
            return Some(
                ed.handle_action(crate::input::keybindings::Action::ToggleDockFocus)
                    .map(|_| crate::input::handler::InputResult::Consumed),
            );
        }
        // The focused dock claims every other key its widget dispatch
        // consumes; anything it declines falls through the walk to the
        // editor base.
        if ed.dispatch_floating_widget_key(crate::app::PanelSlot::Dock, event.code, event.modifiers)
        {
            return Some(Ok(crate::input::handler::InputResult::Consumed));
        }
        None
    }

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
    }
}

/// Behavior owned by this component — the drag half of the
/// width-resize grab; the press half arms it in `on_pointer`, and the
/// release finalizer in `handle_mouse`'s Up arm persists the width.
impl Editor {
    /// Dock resize drag (`PointerGrab::DockResize`, armed by the press
    /// on `chrome:dock_border`): track the pointer column as the new
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
