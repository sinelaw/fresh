//! Info/message popups: transient-dismiss guard, per-popup OPAQUE
//! rects (absorb as a tree property) and scrollbar tracks, and the
//! double-click block guard.

use crate::app::types::HoverTarget;
use crate::input::keybindings::Action;
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
        // Block-or-dismiss guard for transient popups on double/
        // triple-click: outside every popup, dismiss and keep routing.
        t.full("chrome:popup_guard", 140);
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
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
        if !matches!(ev.press, PointerPress::Double | PointerPress::Triple) {
            return Ok(Disposition::Pass);
        }
        match bx.kind {
            // Double/triple-click inside a popup: BLOCK, as a consume
            // (belt over the opacity gate's suspenders — the split
            // select arms live in the walk now, so opacity alone
            // would also stop them, but an explicit block keeps the
            // guard's dismiss half unambiguous).
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
        // The popup stack scrolls under the wheel. (The file browser's
        // wheel lives on ITS box — the z bands disambiguate the two
        // surfaces now, so the historically shared arm is un-shared.)
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

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    pub(super) fn handle_click_popup_scrollbar(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        // Collect all needed data before mutating self.
        let scrollbar_info: Option<(usize, i32)> =
            self.active_chrome().popup_areas.iter().rev().find_map(
                |(popup_idx, _popup_rect, inner_rect, _scroll, _n, scrollbar_rect, total_lines)| {
                    let sb_rect = scrollbar_rect.as_ref()?;
                    if col >= sb_rect.x
                        && col < sb_rect.x + sb_rect.width
                        && row >= sb_rect.y
                        && row < sb_rect.y + sb_rect.height
                    {
                        let relative_row = (row - sb_rect.y) as usize;
                        let track_height = sb_rect.height as usize;
                        let visible_lines = inner_rect.height as usize;
                        if track_height > 0 && *total_lines > visible_lines {
                            let max_scroll = total_lines.saturating_sub(visible_lines);
                            let target = if track_height > 1 {
                                (relative_row * max_scroll) / (track_height.saturating_sub(1))
                            } else {
                                0
                            };
                            Some((*popup_idx, target as i32))
                        } else {
                            Some((*popup_idx, 0))
                        }
                    } else {
                        None
                    }
                },
            );
        let (popup_idx, target_scroll) = scrollbar_info?;
        self.active_window_mut()
            .mouse_state
            .dragging_popup_scrollbar = Some(popup_idx);
        self.active_window_mut().mouse_state.drag_start_row = Some(row);
        let current_scroll = self
            .active_state()
            .popups
            .get(popup_idx)
            .map(|p| p.scroll_offset)
            .unwrap_or(0);
        self.active_window_mut().mouse_state.drag_start_popup_scroll = Some(current_scroll);
        let state = self.active_state_mut();
        if let Some(popup) = state.popups.get_mut(popup_idx) {
            popup.scroll_by(target_scroll - current_scroll as i32);
        }
        Some(Ok(()))
    }

    pub(super) fn handle_click_global_popups(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        for (popup_idx, popup_rect, inner_rect, scroll_offset, num_items) in self
            .active_chrome()
            .global_popup_areas
            .clone()
            .into_iter()
            .rev()
        {
            if popup_rect.width >= 5 {
                let cb_x = popup_rect.x + popup_rect.width - 4;
                if row == popup_rect.y && col >= cb_x && col < cb_x + 3 {
                    return Some(self.handle_action(Action::PopupCancel));
                }
            }
            if in_rect(col, row, inner_rect) && num_items > 0 {
                let relative_row = (row - inner_rect.y) as usize;
                let item_idx = scroll_offset + relative_row;
                if item_idx < num_items {
                    if let Some(popup) = self.global_popups.get_mut(popup_idx) {
                        if let crate::view::popup::PopupContent::List { items: _, selected } =
                            &mut popup.content
                        {
                            *selected = item_idx;
                        }
                    }
                    return Some(self.handle_action(Action::PopupConfirm));
                }
            }
        }
        None
    }

    pub(super) fn handle_click_buffer_popups(
        &mut self,
        col: u16,
        row: u16,
    ) -> Option<AnyhowResult<()>> {
        // Check close-button overlay ("[×]") on each popup.
        let close_hit = self.active_chrome().popup_areas.iter().rev().find_map(
            |(_idx, popup_rect, _inner, _scroll, _n, _sb, _tl)| {
                if popup_rect.width < 5 {
                    return None;
                }
                let cb_x = popup_rect.x + popup_rect.width - 4;
                if row == popup_rect.y && col >= cb_x && col < cb_x + 3 {
                    Some(())
                } else {
                    None
                }
            },
        );
        if close_hit.is_some() {
            return Some(self.handle_action(Action::PopupCancel));
        }

        // Content area clicks — clone to allow &mut self calls inside the loop.
        let popup_areas = self.active_chrome().popup_areas.clone();
        for (popup_idx, _popup_rect, inner_rect, scroll_offset, num_items, _, _) in
            popup_areas.iter().rev()
        {
            if !in_rect(col, row, *inner_rect) {
                continue;
            }
            let relative_col = (col - inner_rect.x) as usize;
            let relative_row = (row - inner_rect.y) as usize;

            let link_url = {
                let state = self.active_state();
                state
                    .popups
                    .top()
                    .and_then(|p| p.link_at_position(relative_col, relative_row))
            };
            if let Some(url) = link_url {
                #[cfg(feature = "runtime")]
                if let Err(e) = open::that(&url) {
                    self.set_status_message(format!("Failed to open URL: {}", e));
                } else {
                    self.set_status_message(format!("Opening: {}", url));
                }
                return Some(Ok(()));
            }

            if *num_items > 0 {
                let item_idx = scroll_offset + relative_row;
                if item_idx < *num_items {
                    let state = self.active_state_mut();
                    if let Some(popup) = state.popups.top_mut() {
                        if let crate::view::popup::PopupContent::List { items: _, selected } =
                            &mut popup.content
                        {
                            *selected = item_idx;
                        }
                    }
                    return Some(self.handle_action(Action::PopupConfirm));
                }
            }

            let is_text_popup = {
                let state = self.active_state();
                state.popups.top().is_some_and(|p| {
                    matches!(
                        p.content,
                        crate::view::popup::PopupContent::Text(_)
                            | crate::view::popup::PopupContent::Markdown(_)
                    )
                })
            };
            if is_text_popup {
                let line = scroll_offset + relative_row;
                let popup_idx_copy = *popup_idx;
                let state = self.active_state_mut();
                if let Some(popup) = state.popups.top_mut() {
                    popup.start_selection(line, relative_col);
                }
                self.active_window_mut().mouse_state.selecting_in_popup = Some(popup_idx_copy);
                return Some(Ok(()));
            }
        }
        None
    }
}
