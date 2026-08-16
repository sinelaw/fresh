//! Info/message popups: transient-dismiss guard, per-popup rects and
//! scrollbar tracks, and the absorb/dismiss guards.

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Popups;

impl ChromeComponent for Popups {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:transient_guard", 17);
        // Each popup's scrollbar track at its painted rect.
        for area in &ed.active_chrome().popup_areas {
            if let Some(r) = area.5 {
                t.rect("chrome:popup_scrollbar", 17, r);
            }
        }
        // Popups are rect-bounded (a wheel or click outside every popup
        // rect falls through); the absorb/dismiss guards below stay
        // full-frame.
        for (_, popup_rect, ..) in &ed.active_chrome().global_popup_areas {
            t.rect("chrome:popups", 15, *popup_rect);
        }
        for area in &ed.active_chrome().popup_areas {
            t.rect("chrome:popups", 15, area.1);
        }
        t.full("chrome:popup_absorb", 14);
        // Block-or-dismiss guard for transient popups (double-click).
        t.full("chrome:popup_guard", 14);
    }
}
