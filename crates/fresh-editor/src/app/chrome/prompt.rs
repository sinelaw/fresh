//! The prompt: suggestion list (bottom dropdown and floating-overlay
//! forms), its scrollbar, the overlay preview pane, the wheel's
//! position-blind suggestion capture, and the overlay-prompt modal
//! scrim.

use crate::app::types::HoverTarget;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress};

fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

pub(crate) struct Prompt;

impl ChromeComponent for Prompt {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        // The suggestion list's scrollbar track at its painted rect
        // (shared by the floating-overlay prompt and the
        // bottom-anchored dropdown). No box when none was painted.
        // Pushed BEFORE the opaque suggestions box: within a band the
        // earlier-pushed box is consulted first, and a specific target
        // must resolve before the opaque surface absorbs the click.
        if let Some(r) = ed.active_chrome().suggestions_scrollbar_rect {
            t.rect("chrome:prompt_scrollbar", 170, r);
        }
        // The suggestions box spans the OUTER rect (click targets the
        // scrollbar border too); handlers with inner-rect geometry
        // (hover) re-check and decline. OPAQUE: a click on the popup's
        // chrome (border cells) that no handler claims is absorbed by
        // the scan's opacity gate — it must not fall through and move
        // the buffer cursor beneath (the deleted popup_absorb guard
        // covered this rect; the box's own opacity replaces it).
        let opaque_rect = |t: &mut ChromeTreeBuilder, r: ratatui::layout::Rect| {
            let mut b = crate::widgets::LayoutBox::plain(
                "chrome:suggestions",
                r.y as u32,
                r.x as u32,
                r.width as u32,
                r.height as u32,
            );
            b.z = 170;
            b.pointer_opaque = true;
            t.push(b);
        };
        if let Some(outer) = ed.active_chrome().suggestions_outer_area {
            opaque_rect(t, outer);
        } else if let Some((inner_rect, _, _, _)) = &ed.active_chrome().suggestions_area {
            opaque_rect(t, *inner_rect);
        }
        if ed.overlay_prompt_active() {
            if let Some(r) = ed.active_chrome().prompt_preview_area {
                t.rect("chrome:prompt_preview", 170, r);
            }
        }
        // The floating-overlay prompt as a mouse-modal surface for the
        // wheel and double-click (its own result rows resolved above
        // via the suggestions box). Sits ABOVE the suggestion capture:
        // while the overlay is up, its own scroll model wins.
        t.full("chrome:overlay_prompt_modal", 160);
        // DELIBERATE full-frame capture, not a geometry proxy: while a
        // prompt with suggestions is open, the wheel scrolls that list
        // wherever the pointer sits (position-blind capture for the
        // bottom-anchored dropdown). Other gestures have no handler
        // for it and fall through.
        t.full("chrome:prompt_suggestions", 155);
        // The overlay prompt's CLICK scrim rides low — just above the
        // editor content band — so chrome controls that peek out from
        // under the overlay (tabs, scrollbars, status bar) still take
        // their clicks; anything that reaches the scrim is swallowed
        // so it can't move the buffer cursor. The wheel/double-click
        // modal above and this click scrim are the same surface's two
        // per-gesture bands, encoded as two thin boxes instead of two
        // hand-ordered arrays.
        t.full("chrome:overlay_prompt_scrim", 15);
    }

    fn hover(&self, ed: &mut Editor, bx: &LayoutBox, col: u16, row: u16) -> Option<HoverTarget> {
        if bx.kind != "chrome:suggestions" {
            return None;
        }
        // Command palette / autocomplete list.
        let (inner_rect, start_idx, _visible_count, total_count) =
            ed.active_chrome().suggestions_area.as_ref()?;
        if in_rect(col, row, *inner_rect) {
            let relative_row = (row - inner_rect.y) as usize;
            let item_idx = start_idx + relative_row;
            if item_idx < *total_count {
                return Some(HoverTarget::SuggestionItem(item_idx));
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
                "chrome:suggestions" => {
                    if let Some(r) = ed.handle_click_suggestions(ev.col, ev.row) {
                        r?;
                        return Ok(Disposition::Consumed);
                    }
                    Ok(Disposition::Pass)
                }
                "chrome:prompt_scrollbar" => {
                    if let Some(r) = ed.handle_click_prompt_scrollbar(ev.col, ev.row) {
                        r?;
                        return Ok(Disposition::Consumed);
                    }
                    Ok(Disposition::Pass)
                }
                // A floating-overlay prompt is mouse-modal: its own
                // targets (result list, scrollbar) were handled above.
                // A click on a toolbar control toggles it through the
                // host (which emits a widget_event); anything else —
                // the input row, separator, preview pane, empty space,
                // or a click outside the frame — is swallowed here so
                // it never reaches the buffer and moves its cursor.
                "chrome:overlay_prompt_scrim" => {
                    if !ed.overlay_prompt_active() {
                        return Ok(Disposition::Pass);
                    }
                    // Hit-test the toolbar's box tree (screen click →
                    // toolbar-local row/col), innermost box first — the
                    // same walk panel clicks use. The deepest keyed
                    // focusable box under the pointer is the control.
                    let hit = ed
                        .active_chrome()
                        .prompt_toolbar_origin
                        .and_then(|(ox, oy)| {
                            let (lrow, lcol) = (ev.row.checked_sub(oy)?, ev.col.checked_sub(ox)?);
                            let boxes = &ed.active_chrome().prompt_toolbar_boxes;
                            crate::widgets::layout_box::hit_path(boxes, lrow as u32, lcol as u32)
                                .into_iter()
                                .rev()
                                .filter(|&i| boxes[i].focusable)
                                .find_map(|i| boxes[i].key.clone())
                        });
                    if let Some(widget_key) = hit {
                        // Move keyboard focus to the clicked control so
                        // Tab continues from here, then flip it through
                        // the host (which emits a widget_event).
                        if let Some(p) = ed.active_window_mut().prompt.as_mut() {
                            p.toolbar_focus = Some(widget_key.clone());
                        }
                        ed.toggle_overlay_toolbar_widget(&widget_key);
                    }
                    Ok(Disposition::Consumed)
                }
                _ => Ok(Disposition::Pass),
            };
        }
        if ev.press != PointerPress::Double {
            return Ok(Disposition::Pass);
        }
        match bx.kind {
            // Double-click on a suggestion row confirms it (#1660).
            "chrome:suggestions" => {
                if let Some(r) = ed.handle_click_suggestions_confirm(ev.col, ev.row) {
                    r?;
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
            }
            // Mouse-modal: swallow anything that wasn't a result row so
            // it can't word-select in the buffer below.
            "chrome:overlay_prompt_modal" => {
                if ed.overlay_prompt_active() {
                    return Ok(Disposition::Consumed);
                }
                Ok(Disposition::Pass)
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
        match bx.kind {
            "chrome:prompt_preview" | "chrome:overlay_prompt_modal" => {
                if !ed.overlay_prompt_active() {
                    return Ok(Disposition::Pass);
                }
                if ed.handle_overlay_prompt_scroll(col, row, delta) {
                    Ok(Disposition::Consumed)
                } else {
                    Ok(Disposition::Pass)
                }
            }
            "chrome:prompt_suggestions" => {
                if ed.handle_prompt_scroll(delta) {
                    Ok(Disposition::Consumed)
                } else {
                    Ok(Disposition::Pass)
                }
            }
            _ => Ok(Disposition::Pass),
        }
    }

    fn layers(&self, ed: &Editor, out: &mut Vec<(u16, crate::app::overlay::Layer)>) {
        use crate::app::overlay::{Layer, LayerKind};
        use crate::input::keybindings::KeyContext;
        if ed.is_prompting() {
            // Find/replace prompts resolve in the narrower
            // `SearchPrompt` context, which owns the match-mode
            // toggles and otherwise falls through to `Prompt` — so the
            // toggle keys (Alt+W etc.) never fire outside a search.
            let key_context = if ed.active_prompt_has_search_options() {
                KeyContext::SearchPrompt
            } else {
                KeyContext::Prompt
            };
            out.push((
                super::layer_rank::PROMPT,
                Layer {
                    kind: LayerKind::Prompt,
                    owns_keyboard: true,
                    key_context: Some(key_context),
                    blocks_terminal_input: true,
                },
            ));
        }
    }
}
