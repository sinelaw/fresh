//! The base surface: splits / tab strip fallback (right-click's tab
//! context menu, double-click's word select, the wheel's
//! `wheel_surface_at` resolution).

use crate::app::types::TabContextMenu;
use crate::view::ui::tabs::TabHit;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct Base;

impl ChromeComponent for Base {
    fn collect(&self, _ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:base", 0);
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press != PointerPress::Right {
            return Ok(Disposition::Pass);
        }
        // Right-click on a tab raises its context menu; anywhere else
        // on the base surface clears it. Context menus only make sense
        // for buffer tabs; groups are plugin-managed.
        let tab_hit =
            ed.active_layout().tab_layouts.iter().find_map(
                |(split_id, tab_layout)| match tab_layout.hit_test(ev.col, ev.row) {
                    Some(TabHit::TabName(target) | TabHit::CloseButton(target)) => {
                        target.as_buffer().map(|bid| (*split_id, bid))
                    }
                    _ => None,
                },
            );
        if let Some((split_id, buffer_id)) = tab_hit {
            ed.active_window_mut().tab_context_menu =
                Some(TabContextMenu::new(buffer_id, split_id, ev.col, ev.row + 1));
        } else {
            ed.active_window_mut().tab_context_menu = None;
        }
        Ok(Disposition::Consumed)
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        use crate::app::scrollbar_input::WheelSurface;
        match ed.active_window().wheel_surface_at(col, row) {
            None => {}
            Some(surface) => {
                // Only a wheel over a pane changes that terminal's
                // live/scrollback state; panning the tab strip or the
                // explorer leaves a live terminal streaming.
                if let WheelSurface::Split(split_id, buffer_id) = surface {
                    if ed.active_window().focused_terminal_live() {
                        ed.enter_terminal_scrollback();
                    } else {
                        ed.active_window_mut()
                            .set_split_terminal_drag_scrollback(split_id, buffer_id, false);
                    }
                }
                ed.dismiss_transient_popups();
                ed.active_window_mut()
                    .handle_mouse_scroll(col, row, delta)?;
            }
        }
        Ok(super::Disposition::Consumed)
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        use crate::input::keybindings::KeyContext;
        // The editor content is the keyboard owner of last resort.
        let base_context = if ed.active_window().is_composite_buffer(ed.active_buffer()) {
            KeyContext::CompositeBuffer
        } else {
            ed.active_window().key_context.clone()
        };
        out.push((
            super::layer_rank::EDITOR_BASE,
            Layer {
                kind: LayerKind::Editor,
                owns_keyboard: true,
                key_context: Some(base_context),
                blocks_terminal_input: false,
            },
        ));
    }
}
