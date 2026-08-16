//! The status bar row.

use crate::widgets::LayoutBox;

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct StatusBar;

impl ChromeComponent for StatusBar {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        if let Some((status_row, status_x, status_width)) = ed.active_chrome().status_bar.area {
            let mut b = LayoutBox::plain(
                "chrome:status_bar",
                status_row as u32,
                status_x as u32,
                status_width as u32,
                1,
            );
            b.z = 4;
            t.boxes.push(b);
        }
    }
}
