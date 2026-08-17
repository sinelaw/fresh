//! The base surface: the tab-strip right-click fallback and the
//! wheel's drop floor (scroll surfaces each own their boxes in the
//! Splits / FileExplorer components; what falls to the base has
//! nothing scrollable under the pointer).

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
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        // The wheel's floor: chrome that owns no scrollable content —
        // the menu bar, the status bar, separators, empty frame —
        // DROPS the wheel rather than handing it to the focused pane
        // (sinelaw/fresh#2969). The scrollable surfaces (splits, tab
        // strips, the explorer) each claim their own boxes above; what
        // reaches the base has nothing under the pointer to move.
        Ok(super::Disposition::Consumed)
    }

    fn on_hwheel(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> anyhow::Result<super::Disposition> {
        // Same drop ruling as the vertical wheel.
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
