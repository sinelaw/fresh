//! Input handling for the Prompt (minibuffer).
//!
//! Implements the InputHandler trait for Prompt, handling text editing,
//! cursor movement, and suggestion navigation.

use super::prompt::Prompt;
use crate::input::handler::{DeferredAction, InputContext, InputHandler, InputResult};
use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};

impl InputHandler for Prompt {
    fn handle_key_event(&mut self, event: &KeyEvent, ctx: &mut InputContext) -> InputResult {
        let ctrl = event.modifiers.contains(KeyModifiers::CONTROL);
        let alt = event.modifiers.contains(KeyModifiers::ALT);
        let shift = event.modifiers.contains(KeyModifiers::SHIFT);

        match event.code {
            // Confirmation and cancellation
            KeyCode::Enter => {
                ctx.defer(DeferredAction::ConfirmPrompt);
                InputResult::Consumed
            }
            KeyCode::Esc => {
                ctx.defer(DeferredAction::ClosePrompt);
                InputResult::Consumed
            }

            // Alt+key combinations should pass through to keybindings
            KeyCode::Char(_) if alt => InputResult::Ignored,

            // Character input (no modifiers or just shift)
            KeyCode::Char(c) if !ctrl => {
                // Delete any selection before inserting
                if self.has_selection() {
                    self.delete_selection();
                }
                if shift {
                    self.insert_char(c.to_ascii_uppercase());
                } else {
                    self.insert_char(c);
                }
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            KeyCode::Char(c) if ctrl => self.handle_ctrl_key(c, ctx),

            // Word motion stays prompt policy (buffer-style
            // next-word-start / select-to-word-end — deliberately
            // different from the engine's word hops).
            KeyCode::Left if ctrl && shift => {
                self.move_word_left_selecting();
                InputResult::Consumed
            }
            KeyCode::Left if ctrl => {
                self.move_word_left();
                InputResult::Consumed
            }
            KeyCode::Right if ctrl && shift => {
                self.move_word_right_selecting();
                InputResult::Consumed
            }
            KeyCode::Right if ctrl => {
                self.move_word_right();
                InputResult::Consumed
            }

            // Every remaining editing key rides the shared text-key
            // table (one `key → op` mapping with the Settings fields
            // and widget Text): Backspace/Delete (word variants via
            // Ctrl), and plain/Shift arrows and Home/End.
            KeyCode::Backspace | KeyCode::Delete => {
                self.handle_text_key(event);
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            KeyCode::Left | KeyCode::Right | KeyCode::Home | KeyCode::End => {
                self.handle_text_key(event);
                InputResult::Consumed
            }

            // Suggestion navigation
            // TODO: Refactor to use callbacks - the prompt creator (e.g. SelectTheme, SelectLocale)
            // should be able to register a callback for selection changes instead of having
            // hardcoded prompt type checks here. This would make the suggestion UI more flexible
            // and allow custom handling for any prompt type without modifying this code.
            KeyCode::Up => {
                if !self.suggestions.is_empty() {
                    // Don't wrap around - stay at 0 if already at the beginning
                    if let Some(selected) = self.selected_suggestion() {
                        let new_selected = if selected == 0 { 0 } else { selected - 1 };
                        self.select_suggestion(Some(new_selected));
                        // For non-plugin prompts (except QuickOpen), or plugin prompts
                        // with sync_input_on_navigate, update input to match selected suggestion
                        let should_sync = self.sync_input_on_navigate
                            || !matches!(
                                self.prompt_type,
                                crate::view::prompt::PromptType::Plugin { .. }
                                    | crate::view::prompt::PromptType::QuickOpen
                                    | crate::view::prompt::PromptType::LiveGrep
                            );
                        if should_sync {
                            if let Some(suggestion) = self.suggestions.get(new_selected) {
                                self.set_input_selected(suggestion.get_value().to_string());
                            }
                        }
                        // For theme selection, trigger live preview
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::SelectTheme { .. }
                        ) {
                            ctx.defer(DeferredAction::PreviewThemeFromPrompt);
                        }
                        // For plugin prompts, notify about selection change (for live preview)
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::Plugin { .. }
                        ) {
                            ctx.defer(DeferredAction::PromptSelectionChanged {
                                selected_index: new_selected,
                            });
                        }
                    }
                } else {
                    // No suggestions - use history
                    ctx.defer(DeferredAction::PromptHistoryPrev);
                }
                InputResult::Consumed
            }
            KeyCode::Down => {
                if !self.suggestions.is_empty() {
                    // Don't wrap around - stay at end if already at the last item
                    if let Some(selected) = self.selected_suggestion() {
                        let new_selected = (selected + 1).min(self.suggestions.len() - 1);
                        self.select_suggestion(Some(new_selected));
                        // For non-plugin prompts (except QuickOpen), or plugin prompts
                        // with sync_input_on_navigate, update input to match selected suggestion
                        let should_sync = self.sync_input_on_navigate
                            || !matches!(
                                self.prompt_type,
                                crate::view::prompt::PromptType::Plugin { .. }
                                    | crate::view::prompt::PromptType::QuickOpen
                                    | crate::view::prompt::PromptType::LiveGrep
                            );
                        if should_sync {
                            if let Some(suggestion) = self.suggestions.get(new_selected) {
                                self.set_input_selected(suggestion.get_value().to_string());
                            }
                        }
                        // For theme selection, trigger live preview
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::SelectTheme { .. }
                        ) {
                            ctx.defer(DeferredAction::PreviewThemeFromPrompt);
                        }
                        // For plugin prompts, notify about selection change (for live preview)
                        if matches!(
                            self.prompt_type,
                            crate::view::prompt::PromptType::Plugin { .. }
                        ) {
                            ctx.defer(DeferredAction::PromptSelectionChanged {
                                selected_index: new_selected,
                            });
                        }
                    }
                } else {
                    // No suggestions - use history
                    ctx.defer(DeferredAction::PromptHistoryNext);
                }
                InputResult::Consumed
            }
            KeyCode::PageUp => {
                self.page_suggestions(-1);
                InputResult::Consumed
            }
            KeyCode::PageDown => {
                self.page_suggestions(1);
                InputResult::Consumed
            }

            // Tab accepts suggestion
            KeyCode::Tab => {
                // In a floating-overlay prompt (Live Grep) the
                // suggestion's `value` field is opaque (the Finder
                // library uses indices like "0", "1", …) so accepting
                // it would clobber the search query with garbage.
                // Arrow keys already move the selection, so Tab is
                // simply a no-op in overlay mode.
                if self.overlay {
                    return InputResult::Consumed;
                }
                // Quick Open's file finder completes to the longest common
                // prefix of the matching paths, like shell completion.
                if let Some(completed) = self.quick_open_file_completion() {
                    if completed != self.input_str() {
                        self.set_input(completed);
                    }
                } else if let Some(selected) = self.selected_suggestion() {
                    if let Some(suggestion) = self.suggestions.get(selected) {
                        if !suggestion.disabled {
                            let value = suggestion.get_value().to_string();
                            // For QuickOpen mode, preserve the prefix character
                            let accepted = if matches!(
                                self.prompt_type,
                                crate::view::prompt::PromptType::QuickOpen
                            ) {
                                match self
                                    .input_str()
                                    .chars()
                                    .next()
                                    .filter(|c| *c == '>' || *c == '#' || *c == ':')
                                {
                                    Some(p) => format!("{}{}", p, value),
                                    None => value,
                                }
                            } else {
                                value
                            };
                            self.set_input(accepted);
                        }
                    }
                }
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }

            // The search bar is a bar, not a modal: function keys fall
            // through to keybinding resolution instead of dying in the modal
            // catch-all below, which is what keeps F3 / Shift+F3 (find
            // next/previous) live while it is open (issue #2111). Other
            // prompts stay fully modal.
            KeyCode::F(_) if self.prompt_type.has_search_options() => InputResult::Ignored,

            _ => InputResult::Consumed, // Modal - consume all unhandled keys
        }
    }

    fn is_modal(&self) -> bool {
        true
    }
}

impl Prompt {
    /// Tab completion for Quick Open's file finder (no `>`/`#`/`:` prefix).
    ///
    /// Extends the typed path to the longest common prefix of the suggested
    /// paths that start with it, stopping just past the next path separator
    /// (one directory level per Tab, like a shell), and keeps any
    /// `:line:col` suffix. When those paths share nothing beyond the typed
    /// path, the input comes back unchanged. Returns `None` when this does
    /// not apply (other prompts, other providers, a selection moved off the
    /// first row, an empty path, or only fuzzy matches), so the caller falls
    /// back to accepting the selection.
    fn quick_open_file_completion(&self) -> Option<String> {
        if !matches!(self.prompt_type, crate::view::prompt::PromptType::QuickOpen) {
            return None;
        }
        let input = self.input_str();
        if input.starts_with(['>', '#', ':']) {
            return None;
        }
        // The selection resets to the first row on every keystroke, so a
        // selection anywhere else means the user arrowed to a row: let Tab
        // accept that row, as it always has.
        if self.selected_suggestion().is_some_and(|i| i != 0) {
            return None;
        }
        let (query, line, column) = crate::input::quick_open::parse_path_line_col(input);
        if query.is_empty() {
            return None;
        }

        let mut candidates = self
            .suggestions
            .iter()
            .filter(|s| !s.disabled)
            .map(|s| s.get_value())
            .filter(|path| path.starts_with(query.as_str()));
        let first = candidates.next()?;

        // Count the shared chars rather than comparing bytes, then snap to a
        // grapheme boundary so a combining mark is never split from its base.
        let shared_chars = candidates.fold(first.chars().count(), |shared, path| {
            first
                .chars()
                .zip(path.chars())
                .take(shared)
                .take_while(|(a, b)| a == b)
                .count()
        });
        let shared_bytes = first
            .char_indices()
            .nth(shared_chars)
            .map_or(first.len(), |(i, _)| i);
        let mut end = crate::primitives::grapheme::snap_to_grapheme_boundary(first, shared_bytes);
        // Complete one directory level per Tab: stop just past the first
        // path separator after the typed path.
        if let Some(sep) = first
            .get(query.len()..end)
            .and_then(|rest| rest.find(['/', '\\']))
        {
            end = query.len() + sep + 1;
        }
        let completed = first.get(..end)?;

        if completed.chars().count() <= query.chars().count() {
            return Some(input.to_string());
        }
        Some(match (line, column) {
            (Some(line), Some(column)) => format!("{completed}:{line}:{column}"),
            (Some(line), None) => format!("{completed}:{line}"),
            _ => completed.to_string(),
        })
    }

    /// Move the selection `pages` pages, by the window the suggestion list
    /// was last laid out with. A list that has not been laid out has no
    /// page, and nothing moves.
    fn page_suggestions(&mut self, pages: i32) {
        let Some(selected) = self.selected_suggestion() else {
            return;
        };
        if let Some(to) = self.pager.target(selected, pages, self.suggestions.len()) {
            self.select_suggestion(Some(to));
        }
    }

    fn handle_ctrl_key(&mut self, c: char, ctx: &mut InputContext) -> InputResult {
        match c {
            'a' => {
                // Select all: caret to end, anchor at start.
                let text = self.input_str().to_string();
                self.set_input_selected(text);
                InputResult::Consumed
            }
            'c' => {
                // Copy - defer to Editor for clipboard access
                ctx.defer(DeferredAction::ExecuteAction(
                    crate::input::keybindings::Action::PromptCopy,
                ));
                InputResult::Consumed
            }
            'x' => {
                // Cut - defer to Editor for clipboard access
                ctx.defer(DeferredAction::ExecuteAction(
                    crate::input::keybindings::Action::PromptCut,
                ));
                InputResult::Consumed
            }
            'v' => {
                // Paste - defer to Editor for clipboard access
                ctx.defer(DeferredAction::ExecuteAction(
                    crate::input::keybindings::Action::PromptPaste,
                ));
                InputResult::Consumed
            }
            'k' => {
                // Delete to end of line
                self.delete_to_end();
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            'u' => {
                // Delete to start of line (standard readline kill).
                // Without this, Ctrl+U did nothing and the only way to
                // clear the command-palette line was repeated Backspace
                // (issue #2143).
                self.clear_selection();
                self.delete_to_start();
                ctx.defer(DeferredAction::UpdatePromptSuggestions);
                InputResult::Consumed
            }
            'z' => {
                // Undo the last input edit. Operates on the prompt's own
                // history so undo edits the query box, not the underlying
                // (modal-inaccessible) buffer. Consumed unconditionally so
                // it never falls through to the global buffer undo.
                if self.undo_input() {
                    ctx.defer(DeferredAction::UpdatePromptSuggestions);
                }
                InputResult::Consumed
            }
            'y' => {
                // Redo the last undone input edit.
                if self.redo_input() {
                    ctx.defer(DeferredAction::UpdatePromptSuggestions);
                }
                InputResult::Consumed
            }
            // Pass through other Ctrl+key combinations to global keybindings (e.g., Ctrl+P to toggle Quick Open)
            _ => InputResult::Ignored,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::input::commands::Suggestion;
    use crate::view::prompt::PromptType;

    fn key(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::NONE)
    }

    fn key_with_ctrl(c: char) -> KeyEvent {
        KeyEvent::new(KeyCode::Char(c), KeyModifiers::CONTROL)
    }

    fn key_with_shift(code: KeyCode) -> KeyEvent {
        KeyEvent::new(code, KeyModifiers::SHIFT)
    }

    #[test]
    fn test_prompt_character_input() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(
            &KeyEvent::new(KeyCode::Char('h'), KeyModifiers::NONE),
            &mut ctx,
        );
        prompt.handle_key_event(
            &KeyEvent::new(KeyCode::Char('i'), KeyModifiers::NONE),
            &mut ctx,
        );

        assert_eq!(prompt.input_str(), "hi");
        assert_eq!(prompt.cursor_byte(), 2);
    }

    #[test]
    fn test_prompt_backspace() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello".to_string());
        prompt.set_cursor_byte(5);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(&key(KeyCode::Backspace), &mut ctx);
        assert_eq!(prompt.input_str(), "hell");
        assert_eq!(prompt.cursor_byte(), 4);
    }

    #[test]
    fn test_prompt_cursor_movement() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello".to_string());
        prompt.set_cursor_byte(5);
        let mut ctx = InputContext::new();

        // Move to start
        prompt.handle_key_event(&key(KeyCode::Home), &mut ctx);
        assert_eq!(prompt.cursor_byte(), 0);

        // Move to end
        prompt.handle_key_event(&key(KeyCode::End), &mut ctx);
        assert_eq!(prompt.cursor_byte(), 5);

        // Move left
        prompt.handle_key_event(&key(KeyCode::Left), &mut ctx);
        assert_eq!(prompt.cursor_byte(), 4);

        // Move right
        prompt.handle_key_event(&key(KeyCode::Right), &mut ctx);
        assert_eq!(prompt.cursor_byte(), 5);
    }

    #[test]
    fn test_prompt_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world".to_string());
        prompt.set_cursor_byte(0);
        let mut ctx = InputContext::new();

        // Select with Shift+Right
        prompt.handle_key_event(&key_with_shift(KeyCode::Right), &mut ctx);
        prompt.handle_key_event(&key_with_shift(KeyCode::Right), &mut ctx);
        assert!(prompt.has_selection());
        assert_eq!(prompt.selected_text(), Some("he".to_string()));

        // Select all with Ctrl+A
        prompt.handle_key_event(&key_with_ctrl('a'), &mut ctx);
        assert_eq!(prompt.selected_text(), Some("hello world".to_string()));
    }

    #[test]
    fn test_prompt_enter_confirms() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(&key(KeyCode::Enter), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ConfirmPrompt)));
    }

    #[test]
    fn test_prompt_escape_cancels() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        prompt.handle_key_event(&key(KeyCode::Esc), &mut ctx);
        assert!(ctx
            .deferred_actions
            .iter()
            .any(|a| matches!(a, DeferredAction::ClosePrompt)));
    }

    #[test]
    fn test_prompt_is_modal() {
        let prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        assert!(prompt.is_modal());
    }

    #[test]
    fn test_prompt_ctrl_p_returns_ignored() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        // Ctrl+P should return Ignored so it can be handled by global keybindings
        let result = prompt.handle_key_event(&key_with_ctrl('p'), &mut ctx);
        assert_eq!(result, InputResult::Ignored, "Ctrl+P should return Ignored");
    }

    #[test]
    fn test_prompt_ctrl_p_dispatch_returns_ignored() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        let mut ctx = InputContext::new();

        // dispatch_input should also return Ignored for Ctrl+P (not Consumed by modal behavior)
        let result = prompt.dispatch_input(&key_with_ctrl('p'), &mut ctx);
        assert_eq!(
            result,
            InputResult::Ignored,
            "dispatch_input should return Ignored for Ctrl+P"
        );
    }

    /// A prompt of `prompt_type` holding `input`, with one suggestion per
    /// path and the first row highlighted, as after a keystroke.
    fn prompt_with_paths(prompt_type: PromptType, input: &str, paths: &[&str]) -> Prompt {
        let mut prompt = Prompt::new("Test: ".to_string(), prompt_type);
        prompt.set_input_plain(input.to_string());
        prompt.set_suggestions(
            paths
                .iter()
                .map(|p| {
                    Suggestion::new(format!("file:{p}"), p.to_string()).with_value(p.to_string())
                })
                .collect(),
        );
        prompt.select_suggestion(Some(0));
        prompt
    }

    /// Press Tab and return the resulting input.
    fn press_tab(prompt: &mut Prompt) -> String {
        let mut ctx = InputContext::new();
        prompt.handle_key_event(&key(KeyCode::Tab), &mut ctx);
        prompt.input_str().to_string()
    }

    /// Press Tab in Quick Open's file finder and return the resulting input.
    fn quick_open_tab(input: &str, paths: &[&str]) -> String {
        press_tab(&mut prompt_with_paths(PromptType::QuickOpen, input, paths))
    }

    const CRATES: &[&str] = &["crates/fresh-editor/src/main.rs", "crates/fresh-gui/lib.rs"];

    #[test]
    fn test_quick_open_tab_stops_after_directory() {
        assert_eq!(quick_open_tab("crat", CRATES), "crates/");
    }

    #[test]
    fn test_quick_open_tab_stops_mid_name_on_shared_prefix() {
        assert_eq!(quick_open_tab("crates/fre", CRATES), "crates/fresh-");
    }

    #[test]
    fn test_quick_open_tab_stops_where_names_diverge() {
        let paths = &[
            "crates/fresh-editor/src/main.rs",
            "crates/fresh-editor-core/lib.rs",
        ];
        assert_eq!(
            quick_open_tab("crates/fresh-ed", paths),
            "crates/fresh-editor"
        );
    }

    #[test]
    fn test_quick_open_tab_single_match_completes_directory() {
        let paths = &["crates/fresh-gui/lib.rs"];
        assert_eq!(quick_open_tab("crates/fresh-g", paths), "crates/fresh-gui/");
    }

    #[test]
    fn test_quick_open_tab_completes_file_name_in_full() {
        assert_eq!(quick_open_tab("not", &["notes.txt"]), "notes.txt");
    }

    #[test]
    fn test_quick_open_tab_without_shared_progress_leaves_input() {
        assert_eq!(quick_open_tab("crates/fresh-", CRATES), "crates/fresh-");
    }

    #[test]
    fn test_quick_open_tab_keeps_line_and_column() {
        assert_eq!(quick_open_tab("crat:42", CRATES), "crates/:42");
        assert_eq!(quick_open_tab("crat:42:5", CRATES), "crates/:42:5");
    }

    #[test]
    fn test_quick_open_tab_follows_backslash_separators() {
        let paths = &["src\\app\\main.rs", "src\\app\\lib.rs"];
        assert_eq!(quick_open_tab("src\\a", paths), "src\\app\\");
    }

    #[test]
    fn test_quick_open_tab_non_ascii_paths() {
        let paths = &["src/über_a.rs", "src/über_b.rs"];
        assert_eq!(quick_open_tab("src/ü", paths), "src/über_");
    }

    #[test]
    fn test_quick_open_tab_never_splits_a_grapheme() {
        // The paths share the chars "x/cafe" but diverge on the combining
        // accent, so the shared "e" belongs to two different graphemes.
        let paths = &["x/cafe\u{301}.rs", "x/cafe\u{300}.rs"];
        assert_eq!(quick_open_tab("x/caf", paths), "x/caf");
    }

    #[test]
    fn test_quick_open_tab_ignores_disabled_rows() {
        let mut prompt = prompt_with_paths(PromptType::QuickOpen, "crates/", &[]);
        prompt.set_suggestions(vec![
            Suggestion::new("file:a", "crates/fresh-editor/main.rs".to_string())
                .with_value("crates/fresh-editor/main.rs".to_string()),
            Suggestion::disabled("status:scanning", "crates/other".to_string()),
        ]);
        prompt.select_suggestion(Some(0));
        assert_eq!(press_tab(&mut prompt), "crates/fresh-editor/");
    }

    #[test]
    fn test_quick_open_tab_fuzzy_only_accepts_selection() {
        assert_eq!(
            quick_open_tab("main", CRATES),
            "crates/fresh-editor/src/main.rs"
        );
    }

    #[test]
    fn test_quick_open_tab_empty_input_accepts_selection() {
        assert_eq!(
            quick_open_tab("", CRATES),
            "crates/fresh-editor/src/main.rs"
        );
    }

    #[test]
    fn test_quick_open_tab_after_arrowing_accepts_that_row() {
        let mut prompt = prompt_with_paths(PromptType::QuickOpen, "crates/fre", CRATES);
        let mut ctx = InputContext::new();
        prompt.handle_key_event(&key(KeyCode::Down), &mut ctx);
        assert_eq!(press_tab(&mut prompt), "crates/fresh-gui/lib.rs");
    }

    #[test]
    fn test_quick_open_tab_after_arrowing_back_to_first_row_completes_prefix() {
        let mut prompt = prompt_with_paths(PromptType::QuickOpen, "crates/fre", CRATES);
        let mut ctx = InputContext::new();
        prompt.handle_key_event(&key(KeyCode::Down), &mut ctx);
        prompt.handle_key_event(&key(KeyCode::Up), &mut ctx);
        assert_eq!(press_tab(&mut prompt), "crates/fresh-");
    }

    #[test]
    fn test_quick_open_tab_other_modes_accept_selection_with_prefix() {
        let mut commands = prompt_with_paths(PromptType::QuickOpen, ">sav", &["Save File"]);
        assert_eq!(press_tab(&mut commands), ">Save File");

        let mut buffers = prompt_with_paths(PromptType::QuickOpen, "#ma", &["main.rs"]);
        assert_eq!(press_tab(&mut buffers), "#main.rs");
    }

    #[test]
    fn test_tab_in_other_prompts_accepts_selection() {
        let mut prompt = prompt_with_paths(PromptType::Search, "crat", CRATES);
        assert_eq!(press_tab(&mut prompt), "crates/fresh-editor/src/main.rs");
    }
}
