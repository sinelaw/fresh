//! Info/message popups: transient-dismiss guard, per-popup OPAQUE
//! rects (absorb as a tree property) and scrollbar tracks, and the
//! double-click block guard.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

pub(crate) struct Popups;

impl ChromeComponent for Popups {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        t.full("chrome:transient_guard", 175);
        // Each popup's scrollbar track at its painted rect.
        for area in &ed.active_chrome().popup_areas {
            if let Some(r) = area.5 {
                t.rect("chrome:popup_scrollbar", 170, r);
            }
        }
        // Popups are rect-bounded, OPAQUE surfaces: a pointer event
        // inside a popup that its handlers decline dies at the popup
        // box (the scan's opacity gate) instead of falling to content
        // beneath — absorb is a tree property, not a guard box.
        let opaque_popup = |t: &mut ChromeTreeBuilder, r: ratatui::layout::Rect| {
            let mut b = LayoutBox::plain(
                "chrome:popups",
                r.y as u32,
                r.x as u32,
                r.width as u32,
                r.height as u32,
            );
            b.z = 150;
            b.pointer_opaque = true;
            t.push(b);
        };
        for (_, popup_rect, ..) in &ed.active_chrome().global_popup_areas {
            opaque_popup(t, *popup_rect);
        }
        for area in &ed.active_chrome().popup_areas {
            opaque_popup(t, area.1);
        }
        // Block-or-dismiss guard for transient popups (double-click's
        // post-walk split scan runs unconditionally after the walk, so
        // the block half must CONSUME — opacity alone can't stop it).
        t.full("chrome:popup_guard", 140);
    }

    fn hover(&self, ed: &Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:popups" {
            return None;
        }
        // Check popups from top to bottom (reverse order since the
        // last popup is on top).
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            ed.active_chrome().popup_areas.iter().rev()
        {
            if in_rect(col, row, *inner_rect) && *num_items > 0 {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;
                if item_idx < *num_items {
                    return Some(HoverTarget::PopupListItem(*popup_idx, item_idx));
                }
            }
        }
        None
    }

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press == PointerPress::Left {
            return match bx.kind {
                // Outside every popup: dismiss transients, keep
                // routing (act-then-continue guard).
                "chrome:transient_guard" => {
                    if !ed.is_mouse_over_any_popup(ev.col, ev.row) {
                        ed.dismiss_transient_popups();
                        return Ok(Disposition::PassAfter);
                    }
                    Ok(Disposition::Pass)
                }
                "chrome:popup_scrollbar" => {
                    if let Some(r) = ed.handle_click_popup_scrollbar(ev.col, ev.row) {
                        r?;
                        return Ok(Disposition::Consumed);
                    }
                    Ok(Disposition::Pass)
                }
                "chrome:popups" => {
                    if let Some(r) = ed
                        .handle_click_global_popups(ev.col, ev.row)
                        .or_else(|| ed.handle_click_buffer_popups(ev.col, ev.row))
                    {
                        r?;
                        return Ok(Disposition::Consumed);
                    }
                    Ok(Disposition::Pass)
                }
                _ => Ok(Disposition::Pass),
            };
        }
        if ev.press != PointerPress::Double {
            return Ok(Disposition::Pass);
        }
        match bx.kind {
            // Double-click inside a popup: BLOCK, as a consume — the
            // double-click walk's post-walk split scan runs
            // unconditionally after the loop, so the opaque box must
            // consume rather than rely on the opacity gate (which
            // would also skip the guard below it in the same scan).
            "chrome:popups" => Ok(Disposition::Consumed),
            // Outside every popup: dismiss transients and keep
            // routing (act-then-continue guard).
            "chrome:popup_guard" => {
                if ed.is_mouse_over_any_popup(ev.col, ev.row) {
                    // Defensive: rect sources for the boxes and this
                    // check could drift; blocking stays correct.
                    return Ok(Disposition::Consumed);
                }
                ed.dismiss_transient_popups();
                Ok(Disposition::PassAfter)
            }
            _ => Ok(Disposition::Pass),
        }
    }

    fn on_wheel(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        col: u16,
        row: u16,
        delta: i32,
    ) -> AnyhowResult<Disposition> {
        if bx.kind != "chrome:popups" {
            return Ok(Disposition::Pass);
        }
        // File browser popup scrolls its list; every other popup
        // scrolls through the popup stack. Both consume. (The file
        // browser check rides here too because historically the two
        // surfaces shared one arm — the file_browser component runs
        // the same logic for its own box.)
        if ed.is_file_open_active()
            && ed.is_mouse_over_file_browser(col, row)
            && ed.handle_file_open_scroll(delta)
        {
            return Ok(Disposition::Consumed);
        }
        if !ed.is_mouse_over_any_popup(col, row) {
            return Ok(Disposition::Pass);
        }
        ed.scroll_popup(delta);
        Ok(Disposition::Consumed)
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        // A non-trust popup is *present* whenever visible, but only
        // *owns* the keyboard while capturing; a merely-visible
        // unfocused popup falls through. Either way a visible popup
        // blocks PTY routing — it covers the active buffer. While the
        // workspace-trust prompt tops the global stack, its dedicated
        // layer (the modals component) takes this one's place.
        if !ed.workspace_trust_on_top()
            && (ed.global_popups.is_visible() || ed.active_state().popups.is_visible())
        {
            out.push((
                super::layer_rank::POPUP,
                Layer {
                    kind: LayerKind::Popup,
                    owns_keyboard: ed.popups_capture_keys(),
                    key_context: Some(crate::input::keybindings::KeyContext::Popup),
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}
