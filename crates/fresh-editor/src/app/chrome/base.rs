//! The base surface: splits / tab strip fallback (right-click's tab
//! context menu, double-click's word select, the wheel's
//! `wheel_surface_at` resolution).

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Base;

impl ChromeComponent for Base {
    fn collect(&self, _ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:base", 0);
    }
}
