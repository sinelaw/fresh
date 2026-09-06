//! The prompt: **the keyboard, and the handlers the tree's messages land in.**
//!
//! Everything positional has gone into the shell tree — the suggestion list in
//! both its forms, its scrollbar, the overlay card's modal scrim, its preview
//! pane and its toolbar — and so has who owns the keyboard while a prompt is
//! up (`view::shell::prompt::keys_layer`, whose sink names the prompt's key
//! section). What is left is `dispatch_prompt_key` and the few `Editor`
//! methods a `UiFact` calls into.

use crate::input::keybindings::Action;
use anyhow::Result as AnyhowResult;

use super::Editor;

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
    pub(crate) fn dispatch_prompt_key(
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
        &mut self,
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
}
