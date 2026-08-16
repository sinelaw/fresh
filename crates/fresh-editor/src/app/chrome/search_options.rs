//! The search-prompt option checkboxes band.

use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct SearchOptions;

impl ChromeComponent for SearchOptions {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some(l) = &ed.active_chrome().search_options_layout {
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
                b.z = 3;
                t.boxes.push(b);
            }
        }
    }
}
