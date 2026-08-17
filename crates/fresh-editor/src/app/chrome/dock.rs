//! The left dock column (orchestrator sessions panel).

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
            if let Some(inner) = dock.last_inner_rect {
                t.rect("chrome:dock", 130, inner);
            }
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

    fn on_key(
        &self,
        ed: &mut Editor,
        code: crossterm::event::KeyCode,
        modifiers: crossterm::event::KeyModifiers,
    ) -> Option<anyhow::Result<()>> {
        if !ed.dock.as_ref().is_some_and(|f| f.focused) {
            return None;
        }
        // A focused dock swallows keys in the dispatch below, so the
        // global focus-toggle (default Alt+O) would never be able to
        // hand focus back to the editor once you've dived in. Resolve
        // it ahead of the dock's own key handling, so the toggle is
        // symmetric (same key in and out). Only the blur-out
        // direction needs this — focusing a blurred dock is ordinary
        // keybinding resolution (the editor owns the keyboard then).
        let key_event = crossterm::event::KeyEvent::new(code, modifiers);
        let ctx = ed.get_key_context();
        let resolved = ed
            .keybindings
            .read()
            .ok()
            .map(|kb| kb.resolve(&key_event, ctx));
        if matches!(
            resolved,
            Some(crate::input::keybindings::Action::ToggleDockFocus)
        ) {
            return Some(
                ed.handle_action(crate::input::keybindings::Action::ToggleDockFocus)
                    .map(|_| ()),
            );
        }
        // The focused dock claims every other key (registered after
        // FloatingModal — one precedence source, matching the
        // `layers()` ranks).
        if ed.dispatch_floating_widget_key(crate::app::PanelSlot::Dock, code, modifiers) {
            return Some(Ok(()));
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
