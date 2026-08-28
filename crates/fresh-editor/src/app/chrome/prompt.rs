//! The prompt: suggestion list (bottom dropdown and floating-overlay
//! forms), its scrollbar, the overlay preview pane, the wheel's
//! position-blind suggestion capture, and the overlay-prompt modal
//! scrim.

use crate::input::keybindings::Action;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

use super::{
    in_rect, ChromeComponent, ChromePointer, ChromeTreeBuilder, Disposition, Editor, PointerPress,
};

pub(crate) struct Prompt;

impl ChromeComponent for Prompt {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder) {
        // No box for the suggestion list. Both prompt forms grow it from the
        // shell tree now: the rows claim their own click and their own
        // double-click, `hit.rs` owns the scrollbar's jump and its drag, the
        // viewport takes the wheel, and the popup absorbs a press on its own
        // chrome. What is left here is the overlay *card* — its preview pane,
        // and the guards that stop a gesture reaching the buffer while it is
        // up.
        if ed.overlay_prompt_active() {
            if let Some(r) = ed.active_chrome().prompt_preview_area {
                t.rect("chrome:prompt_preview", 170, r);
            }
            // Mouse-modal overlay swallows EVERY right-click — plain
            // and Ctrl+ alike — so neither the tab context menu nor
            // the theme inspector (trigger at z190) fires, and the
            // buffer below is untouched. Right-click-only band; other
            // gestures have no arm here and fall through.
            t.full("chrome:overlay_rclick_guard", 195);
        }
        // The floating-overlay prompt as a mouse-modal surface for the
        // wheel and double-click (its own result rows resolved above
        // via the suggestions box). Sits ABOVE the suggestion capture:
        // while the overlay is up, its own scroll model wins.
        t.full("chrome:overlay_prompt_modal", 160);
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

    fn on_pointer(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        if ev.press == PointerPress::Left {
            return match bx.kind {
                // A floating-overlay prompt is mouse-modal. Its result list
                // takes its own clicks in the tree, before any of this runs.
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
        if ev.press == PointerPress::Right {
            // Mouse-modal overlay: swallow every right-click flavor
            // (plain AND Ctrl+) so neither the tab context menu nor
            // the theme inspector fires while the overlay is up.
            if bx.kind == "chrome:overlay_rclick_guard" && ed.overlay_prompt_active() {
                return Ok(Disposition::Consumed);
            }
            return Ok(Disposition::Pass);
        }
        if !matches!(ev.press, PointerPress::Double | PointerPress::Triple) {
            return Ok(Disposition::Pass);
        }
        if ev.press == PointerPress::Triple {
            // Mouse-modal overlay: a triple-click must never
            // line-select in the buffer below (no triple semantics of
            // its own — plain swallow).
            if bx.kind == "chrome:overlay_prompt_modal" && ed.overlay_prompt_active() {
                return Ok(Disposition::Consumed);
            }
            return Ok(Disposition::Pass);
        }
        match bx.kind {
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
            _ => Ok(Disposition::Pass),
        }
    }

    fn on_hwheel(
        &self,
        ed: &mut Editor,
        bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> AnyhowResult<Disposition> {
        // The horizontal axis mirrors the vertical MODAL claims: none
        // of these surfaces has horizontal content, so wherever the
        // vertical wheel would be consumed, the horizontal delta is
        // ABSORBED — Shift+wheel over the overlay prompt or the
        // suggestions dropdown must not pan the buffer hidden beneath
        // (the wheel walk deliberately skips the opacity gate for
        // scroll chaining, so without these arms the delta fell all
        // the way to the split's h-scroll).
        match bx.kind {
            "chrome:prompt_preview" | "chrome:overlay_prompt_modal" => {
                if ed.overlay_prompt_active() {
                    Ok(Disposition::Consumed)
                } else {
                    Ok(Disposition::Pass)
                }
            }
            "chrome:prompt_suggestions" | "chrome:suggestions" | "chrome:prompt_scrollbar" => {
                let suggestions_visible = ed
                    .active_window()
                    .prompt
                    .as_ref()
                    .is_some_and(|p| !p.suggestions.is_empty());
                if suggestions_visible {
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

    fn on_layer_key(
        &self,
        ed: &mut Editor,
        _layer: &crate::app::overlay::Layer,
        event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
        ed.dispatch_prompt_key(event).map(Ok)
    }
}

/// Behavior owned by this component (moved from mouse_input.rs —
/// the handlers its arms dispatch to).
impl Editor {
    /// Keyboard for the prompt layer (moved verbatim from
    /// `dispatch_modal_input`'s prompt block — offered by the layer
    /// walk while a prompt is up). Rungs in order: file-browser
    /// prompts, the query-replace confirm prompt, the overlay
    /// toolbar focus ring, then the prompt's own handler. `None` =
    /// the prompt ignored the key, so the walk falls through to the
    /// layers below (and ultimately normal keybinding resolution,
    /// which resolves in the Prompt context — that's how the file
    /// browser's Alt+letter toggles and Ctrl+P reach their bindings).
    pub(super) fn dispatch_prompt_key(
        &mut self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::handler::InputResult> {
        use crate::input::handler::{InputContext, InputHandler, InputResult};
        let mut ctx = InputContext::new();

        // File browser prompts use FileBrowserInputHandler. Keys it
        // ignores (Alt+letter) fall through to regular keybinding
        // resolution, which resolves them in the Prompt context —
        // context-specific bindings outrank global ones there, so the
        // browser's Alt toggles (encoding, hidden files) win over e.g.
        // the Alt+E menu mnemonic without any special-casing here.
        if self.is_file_open_active() {
            let active_window_id = self.active_window;
            let __win = self
                .windows
                .get_mut(&active_window_id)
                .expect("active window present");
            if let (Some(ref mut file_state), Some(ref mut prompt)) =
                (&mut __win.file_open_state, &mut __win.prompt)
            {
                let mut handler = crate::view::file_browser_input::FileBrowserInputHandler::new(
                    file_state, prompt,
                );
                let result = handler.dispatch_input(event, &mut ctx);
                if result != InputResult::Ignored {
                    self.process_deferred_actions(ctx);
                    return Some(result);
                }
                // Deliberately dropped: an Ignored file-browser pass
                // must not leak its context into the rungs below.
                ctx = InputContext::new();
            }
        }

        // QueryReplaceConfirm prompts use QueryReplaceConfirmInputHandler.
        // Returned even when `Ignored` — the confirm prompt consumes
        // every key (the old block's unconditional `return Some(result)`),
        // and `Some(Ignored)` stops the walk just as it stopped
        // `dispatch_modal_input`.
        let is_query_replace_confirm =
            self.active_window().prompt.as_ref().is_some_and(|p| {
                p.prompt_type == crate::view::prompt::PromptType::QueryReplaceConfirm
            });
        if is_query_replace_confirm {
            let mut handler =
                crate::view::query_replace_input::QueryReplaceConfirmInputHandler::new();
            let result = handler.dispatch_input(event, &mut ctx);
            self.process_deferred_actions(ctx);
            return Some(result);
        }

        // Universal Search overlay focus ring: Tab/Shift+Tab move focus
        // between the query input and the scope toggles; Space/Enter
        // activate the focused toggle. Intercepted before the prompt's own
        // input handling so Tab doesn't fall through to other behaviour.
        if let Some(result) = self.handle_overlay_toolbar_key(event) {
            return Some(result);
        }

        // A `prompt`-context binding outranks the prompt widget's own
        // hardcoded key handling. `Prompt`'s `InputHandler` owns a handful of
        // Ctrl keys outright (Ctrl+A select-all, Ctrl+Y redo-input, …) and runs
        // ahead of keybinding resolution, so no keymap and no user config could
        // reach those keys — an Emacs user cannot get `C-a` to mean
        // beginning-of-line in the minibuffer, however they bind it.
        //
        // Narrowly scoped on purpose: only a resolved `prompt_*` action is
        // taken here, so a binding that means something else entirely (the file
        // browser's Alt toggles, Ctrl+P for quick-open) keeps its existing
        // route through the rungs below. The actions themselves re-enter this
        // same handler with their canonical key, so nothing here recurses.
        if let Some(action) = self.prompt_action_binding(event) {
            return Some(
                self.handle_action(action)
                    .map(|_| InputResult::Consumed)
                    .unwrap_or(InputResult::Consumed),
            );
        }

        if let Some(ref mut prompt) = self.active_window_mut().prompt {
            let result = prompt.dispatch_input(event, &mut ctx);
            // Only return and process deferred actions if the prompt
            // handled the input. If Ignored, fall through (proven safe:
            // every Ignored return in the prompt handler is immediate —
            // no deferred actions are queued on those paths, so the
            // dropped ctx is empty).
            if result != InputResult::Ignored {
                self.process_deferred_actions(ctx);
                return Some(result);
            }
        }
        None
    }

    /// The `prompt_*` action this key resolves to in the prompt's own key
    /// context, if any.
    ///
    /// Only the prompt's navigation and editing actions qualify — the set with
    /// a dispatch arm that delegates back to the prompt widget. Anything else
    /// the key might resolve to is left to the normal walk, so this can only
    /// ever change which of the *prompt's own* operations a key performs.
    fn prompt_action_binding(
        &self,
        event: &crossterm::event::KeyEvent,
    ) -> Option<crate::input::keybindings::Action> {
        use crate::input::keybindings::Action;
        let context = self.get_key_context();
        if !matches!(
            context,
            crate::input::keybindings::KeyContext::Prompt
                | crate::input::keybindings::KeyContext::SearchPrompt
        ) {
            return None;
        }
        let action = self.keybindings.read().ok()?.resolve(event, context);
        matches!(
            action,
            Action::PromptCancel
                | Action::PromptBackspace
                | Action::PromptDelete
                | Action::PromptMoveLeft
                | Action::PromptMoveRight
                | Action::PromptMoveStart
                | Action::PromptMoveEnd
                | Action::PromptSelectPrev
                | Action::PromptSelectNext
                | Action::PromptPageUp
                | Action::PromptPageDown
                | Action::PromptAcceptSuggestion
                | Action::PromptMoveWordLeft
                | Action::PromptMoveWordRight
                | Action::PromptDeleteWordBackward
                | Action::PromptDeleteWordForward
                | Action::PromptDeleteToLineEnd
                | Action::PromptSelectAll
                | Action::PromptMoveLeftSelecting
                | Action::PromptMoveRightSelecting
                | Action::PromptMoveHomeSelecting
                | Action::PromptMoveEndSelecting
                | Action::PromptSelectWordLeft
                | Action::PromptSelectWordRight
                | Action::PromptCopy
                | Action::PromptCut
                | Action::PromptPaste
                | Action::PromptConfirm
        )
        .then_some(action)
    }

    /// The same, for a row that already knows which one it is.
    ///
    /// Split out of the coordinate form the way `activate_menu_item` was: a
    /// list row that answers its own click has an index, and asking it to
    /// report a screen position so the editor can hit-test its way back to
    /// that index is the round trip the migration removes.
    pub(crate) fn select_suggestion(&mut self, item_idx: usize) -> Option<AnyhowResult<()>> {
        let prompt = self.active_window_mut().prompt.as_mut()?;
        prompt.selected_suggestion = Some(item_idx);
        let confirms = prompt.prompt_type.click_confirms();
        if !confirms {
            // Mirror keyboard navigation / scroll: sync the input
            // to the selected suggestion so the prompt reflects
            // what Enter would commit.
            if let Some(suggestion) = prompt.suggestions.get(item_idx) {
                prompt.set_input_plain(suggestion.get_value().to_string());
            }
        }
        if confirms {
            return Some(self.handle_action(Action::PromptConfirm));
        }
        Some(Ok(()))
    }

    /// The same, by index. `click_confirms` is not consulted: a double-click
    /// is the mouse-only commit path for the prompts that preview on a single
    /// click, which is the whole reason this variant exists.
    pub(crate) fn confirm_suggestion(&mut self, item_idx: usize) -> Option<AnyhowResult<()>> {
        let prompt = self.active_window_mut().prompt.as_mut()?;
        prompt.selected_suggestion = Some(item_idx);
        if let Some(suggestion) = prompt.suggestions.get(item_idx) {
            prompt.set_input_plain(suggestion.get_value().to_string());
        }
        Some(self.handle_action(Action::PromptConfirm))
    }

    /// Route a wheel event inside the floating-overlay prompt (Live Grep).
    ///
    /// The overlay is mouse-modal, so it always consumes the wheel (returns
    /// true) when active — the event must never leak to the buffer below.
    /// * Over the preview pane → scroll the preview.
    /// * Anywhere else (result list, input, toolbar, frame) → scroll the
    ///   result list *without* moving the selection.
    ///
    /// Bottom-anchored prompts have no arm here at all: their list is a
    /// `fresh-ui` viewport and takes the wheel itself when the pointer is
    /// over it.
    pub(super) fn handle_overlay_prompt_scroll(&mut self, col: u16, row: u16, delta: i32) -> bool {
        if !self.overlay_prompt_active() {
            return false;
        }
        let preview_area = self.active_chrome().prompt_preview_area;
        let results_visible = self
            .active_chrome()
            .prompt_results_area
            .map(|r| r.height as usize)
            .unwrap_or(0);
        if let Some(preview) = preview_area {
            if in_rect(col, row, preview) {
                self.active_window_mut()
                    .scroll_overlay_preview_by_lines(delta);
                return true;
            }
        }
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.scroll_results(delta, results_visible);
        }
        true
    }
}
