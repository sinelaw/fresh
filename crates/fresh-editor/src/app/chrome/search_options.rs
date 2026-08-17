//! The search-prompt option checkboxes band.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct SearchOptions;

impl ChromeComponent for SearchOptions {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(l) = ed.search_options_layout_now() {
            let spans = [l.case_sensitive, l.whole_word, l.regex, l.confirm_each];
            let start = spans.iter().flatten().map(|(s, _)| *s).min();
            let end = spans.iter().flatten().map(|(_, e)| *e).max();
            if let (Some(start), Some(end)) = (start, end) {
                let mut b = LayoutBox::plain(
                    "chrome:search_options",
                    l.row as u32,
                    start as u32,
                    end.saturating_sub(start) as u32,
                    1,
                );
                b.z = 30;
                t.push(b);
            }
        }
    }

    fn hover(&self, ed: &mut Editor, _bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if let Some(layout) = ed.search_options_layout_now() {
            use crate::view::ui::status_bar::SearchOptionsHover;
            if let Some(hover) = layout.checkbox_at(col, row) {
                return Some(match hover {
                    SearchOptionsHover::CaseSensitive => HoverTarget::SearchOptionCaseSensitive,
                    SearchOptionsHover::WholeWord => HoverTarget::SearchOptionWholeWord,
                    SearchOptionsHover::Regex => HoverTarget::SearchOptionRegex,
                    SearchOptionsHover::ConfirmEach => HoverTarget::SearchOptionConfirmEach,
                    SearchOptionsHover::None => return None,
                });
            }
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        _bx: &LayoutBox,
        ev: &super::ChromePointer,
    ) -> anyhow::Result<super::Disposition> {
        use super::{Disposition, PointerPress};
        if ev.press != PointerPress::Left {
            return Ok(Disposition::Pass);
        }
        if let Some(r) = ed.handle_click_search_options(ev.col, ev.row) {
            r?;
            return Ok(Disposition::Consumed);
        }
        Ok(Disposition::Pass)
    }
}
