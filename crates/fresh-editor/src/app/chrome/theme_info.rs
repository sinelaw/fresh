//! The theme-inspector popup (Ctrl+Right-Click debug instrument):
//! the full-frame inspect trigger, the popup surface (button click /
//! swallow), hover highlight, outside-dismiss, and key dismissal.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

pub(crate) struct ThemeInfo;

impl ChromeComponent for ThemeInfo {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        // Inspector trigger: full-frame, above every routable surface —
        // Ctrl+Right-Click inspects the cell under ANY chrome (that is
        // the tool's point) — but below the overlay prompt's
        // right-click guard (z195), which swallows both plain and
        // Ctrl+ right-clicks while the overlay is up. Pushed before
        // the popup box so a Ctrl+Right-Click inside the open popup
        // re-inspects at the new position (same band; document order
        // breaks the tie, and the trigger passes everything else).
        t.full("chrome:theme_inspect", 190);
        if ed.active_window().theme_info_popup.is_some() {
            if let Some((rect, _)) = ed.theme_info_popup_rect() {
                // OPAQUE: clicks and hover inside the popup that its
                // arms decline die here instead of falling through to
                // the surfaces beneath.
                let mut b = LayoutBox::plain(
                    "chrome:theme_info",
                    rect.y as u32,
                    rect.x as u32,
                    rect.width as u32,
                    rect.height as u32,
                );
                b.z = 190;
                b.pointer_opaque = true;
                t.push(b);
            }
            // Outside-dismiss: act-then-continue (the click that
            // dismisses the inspector still routes to what it hit).
            t.full("chrome:theme_info_guard", 185);
        }
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:theme_info" {
            return None;
        }
        // Highlight the "Open in Theme Editor" button row (absent on
        // the keyless message variant). Paint derives the highlight
        // from this target — no per-move state mutation.
        let (rect, offset) = ed.theme_info_popup_rect()?;
        let offset = offset?;
        (row == rect.y + offset && col >= rect.x && col < rect.x + rect.width)
            .then_some(HoverTarget::ThemeInfoButton)
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        match (bx.kind, ev.press) {
            // Ctrl+Right-Click anywhere → inspect the cell under the
            // pointer. Plain right-clicks pass to the surfaces below.
            ("chrome:theme_inspect", PointerPress::Right) => {
                if ev
                    .modifiers
                    .contains(crossterm::event::KeyModifiers::CONTROL)
                {
                    ed.show_theme_info_popup(ev.col, ev.row)?;
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            ("chrome:theme_info", PointerPress::Left) => {
                let Some((rect, button_row_offset)) = ed.theme_info_popup_rect() else {
                    return Ok(Disposition::Pass);
                };
                if let Some(offset) = button_row_offset {
                    if ev.row == rect.y + offset {
                        let key =
                            ed.active_window().theme_info_popup.as_ref().and_then(|p| {
                                p.info.fg_key.clone().or_else(|| p.info.bg_key.clone())
                            });
                        ed.active_window_mut().theme_info_popup = None;
                        if let Some(key) = key {
                            ed.fire_theme_inspect_hook(key);
                        }
                        return Ok(Disposition::Consumed);
                    }
                }
                // Inside the popup but not on the button: swallow.
                Ok(Disposition::Consumed)
            }
            // Double/triple-click on the popup: block (belt over
            // the opacity gate's suspenders).
            ("chrome:theme_info", PointerPress::Double | PointerPress::Triple) => {
                Ok(Disposition::Consumed)
            }
            ("chrome:theme_info_guard", PointerPress::Left) => {
                ed.active_window_mut().theme_info_popup = None;
                Ok(Disposition::PassAfter)
            }
            _ => Ok(Disposition::Pass),
        }
    }

    fn on_key(
        &self,
        ed: &mut Editor,
        _code: crossterm::event::KeyCode,
        _modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        // Any key dismisses the inspector and keeps routing — the
        // keyboard analogue of a PassAfter guard, not a grab.
        if ed.active_window().theme_info_popup.is_some() {
            ed.active_window_mut().theme_info_popup = None;
        }
        None
    }
}
