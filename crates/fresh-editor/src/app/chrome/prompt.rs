//! The prompt: suggestion list (bottom dropdown and floating-overlay
//! forms), its scrollbar, the overlay preview pane, the wheel's
//! position-blind suggestion capture, and the overlay-prompt modal
//! scrim.

use super::{ChromeComponent, ChromeTreeBuilder, Editor};

pub(crate) struct Prompt;

impl ChromeComponent for Prompt {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        // The suggestions box spans the OUTER rect (click targets the
        // scrollbar border too); handlers with inner-rect geometry
        // (hover) re-check and decline.
        if let Some(outer) = ed.active_chrome().suggestions_outer_area {
            t.rect("chrome:suggestions", 17, outer);
        } else if let Some((inner_rect, _, _, _)) = &ed.active_chrome().suggestions_area {
            t.rect("chrome:suggestions", 17, *inner_rect);
        }
        // The suggestion list's scrollbar track at its painted rect
        // (shared by the floating-overlay prompt and the
        // bottom-anchored dropdown). No box when none was painted.
        if let Some(r) = ed.active_chrome().suggestions_scrollbar_rect {
            t.rect("chrome:prompt_scrollbar", 17, r);
        }
        if ed.overlay_prompt_active() {
            if let Some(r) = ed.active_chrome().prompt_preview_area {
                t.rect("chrome:prompt_preview", 17, r);
            }
        }
        // DELIBERATE full-frame capture, not a geometry proxy: while a
        // prompt with suggestions is open, the wheel scrolls that list
        // wherever the pointer sits (position-blind capture for the
        // bottom-anchored dropdown — see WHEEL_ORDER's doc). Other
        // gestures have no handler for it and skip it.
        t.full("chrome:prompt_suggestions", 17);
        // The floating-overlay prompt as a mouse-modal surface (its own
        // result rows resolved above via the suggestions box).
        t.full("chrome:overlay_prompt_modal", 16);
    }
}
