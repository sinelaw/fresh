//! The left dock column (orchestrator sessions panel).

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Dock;

impl ChromeComponent for Dock {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(dock) = &ed.dock {
            if let crate::app::PanelPlacement::LeftDock { width_cols } = dock.placement {
                // The resize border (the column's rightmost cell
                // column) is pushed FIRST: it sits inside the column
                // rect, and within a band the earlier-pushed box wins,
                // so a press there is a resize handle, not a widget
                // hit.
                let mut border = LayoutBox::plain(
                    "chrome:dock_border",
                    0,
                    width_cols.saturating_sub(1) as u32,
                    1,
                    t.frame_height(),
                );
                border.z = 130;
                t.push(border);
                let mut column = LayoutBox::plain(
                    "chrome:dock_column",
                    0,
                    0,
                    width_cols as u32,
                    t.frame_height(),
                );
                column.z = 130;
                t.push(column);
                // Blur observer: any left-click OUTSIDE the dock column
                // while the dock is focused blurs it, then routing
                // continues (act-then-continue). Ranked above every
                // consuming surface so the blur precedes whatever the
                // click lands on — the pre-walk block's exact contract.
                t.full("chrome:dock_blur", 195);
            }
            // (No separate inner-rect box: no handler ever matched
            // "chrome:dock", and the kind-blind wheel arm is already
            // reachable through `chrome:dock_column`, which covers the
            // inner rect at the same z and ranks first in the band.)
        }
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &super::ChromePointer,
    ) -> anyhow::Result<super::Disposition> {
        use super::{Disposition, PointerPress};
        let width_cols = match ed.dock.as_ref().map(|f| f.placement) {
            Some(crate::app::PanelPlacement::LeftDock { width_cols }) => width_cols,
            _ => return Ok(Disposition::Pass),
        };
        match (ev.press, bx.kind) {
            // A press on the dock's right border starts the
            // width-resize GRAB (routed by the drag/up arms until
            // release; see `chrome::pointer_grab`).
            (PointerPress::Left, "chrome:dock_border") => {
                ed.dock_resizing = true;
                Ok(Disposition::Consumed)
            }
            // Clicks inside the column hit-test the dock's panel,
            // re-focusing it first if blurred: the un-blur must notify
            // the plugin via a `focus` widget_event so any mirror of
            // dock-focus state updates before the click's row-select
            // event fires its scheduling logic.
            (PointerPress::Left, "chrome:dock_column") => {
                if ed.dock.as_ref().is_some_and(|f| !f.focused) {
                    ed.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
                ed.handle_floating_widget_click(crate::app::PanelSlot::Dock, ev.col, ev.row);
                Ok(Disposition::Consumed)
            }
            (PointerPress::Left, "chrome:dock_blur") => {
                if ev.col >= width_cols && ed.dock.as_ref().is_some_and(|f| f.focused) {
                    ed.blur_floating_panel(crate::app::PanelSlot::Dock);
                    return Ok(Disposition::PassAfter);
                }
                Ok(Disposition::Pass)
            }
            // Right-click in the column → the plugin raises a
            // per-session context menu; mirror the left-click path
            // (re-focus first) and consume so it never falls through
            // to the editor or the file-explorer menu below.
            (PointerPress::Right, "chrome:dock_column") => {
                if ed.dock.as_ref().is_some_and(|f| !f.focused) {
                    ed.refocus_floating_panel(crate::app::PanelSlot::Dock);
                }
                ed.handle_floating_widget_context_click(
                    crate::app::PanelSlot::Dock,
                    ev.col,
                    ev.row,
                );
                Ok(Disposition::Consumed)
            }
            _ => Ok(Disposition::Pass),
        }
    }

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

    fn on_wheel(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        if ed.handle_floating_widget_panel_wheel(crate::app::PanelSlot::Dock, col, row, delta) {
            Ok(super::Disposition::Consumed)
        } else {
            Ok(super::Disposition::Pass)
        }
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
