//! Prompt/minibuffer system for user input

use crate::input::commands::Suggestion;
use crate::primitives::word_navigation::{
    find_word_end_bytes, find_word_start_bytes, is_word_char,
};

/// Type of prompt - determines what action to take when user confirms
#[derive(Debug, Clone, PartialEq)]
pub enum PromptType {
    /// Open a file
    OpenFile,
    /// Open a file with a specific encoding (used when detect_encoding is disabled)
    /// Contains the path to open after encoding selection
    OpenFileWithEncoding { path: std::path::PathBuf },
    /// Reload current file with a different encoding
    /// Requires the buffer to have no unsaved modifications
    ReloadWithEncoding,
    /// Switch to a different project folder (change working directory)
    SwitchProject,
    /// Save current buffer to a new file
    SaveFileAs,
    /// Search for text in buffer
    Search,
    /// Search for text in buffer (for replace operation - will prompt for replacement after)
    ReplaceSearch,
    /// Replace text in buffer
    Replace { search: String },
    /// Search for text in buffer (for query-replace - will prompt for replacement after)
    QueryReplaceSearch,
    /// Query replace text in buffer - prompt for replacement text
    QueryReplace { search: String },
    /// Query replace confirmation prompt (y/n/!/q for each match)
    QueryReplaceConfirm,
    /// Quick Open - unified prompt with prefix-based provider routing
    /// Supports file finding (default), commands (>), buffers (#), goto line (:)
    QuickOpen,
    /// Live Grep — project-wide search rendered as a centred floating
    /// overlay (issue #1796). Unlike `Plugin { custom_type }`, this
    /// variant gets first-class layout handling: the renderer draws the
    /// prompt and its suggestion list inside a `PopupPosition::CenteredOverlay`
    /// frame instead of on the bottom minibuffer row, leaving the
    /// underlying split tree untouched.
    LiveGrep,
    /// Go to a specific line number
    GotoLine,
    /// Go to a specific byte offset (large file without line index scan)
    GotoByteOffset,
    /// Confirm whether to scan a large file for exact line numbers before Go To Line
    GotoLineScanConfirm,
    /// Choose an ANSI background file
    SetBackgroundFile,
    /// Set background blend ratio (0-1)
    SetBackgroundBlend,
    /// Plugin-controlled prompt with custom type identifier
    /// The string identifier is used to filter hooks in plugin code
    Plugin { custom_type: String },
    /// LSP Rename operation
    /// Stores the original text, start/end positions in buffer, and overlay handle
    LspRename {
        original_text: String,
        start_pos: usize,
        end_pos: usize,
        overlay_handle: crate::view::overlay::OverlayHandle,
    },
    /// Record a macro - prompts for register (0-9)
    RecordMacro,
    /// Play a macro - prompts for register (0-9)
    PlayMacro,
    /// Save a recorded macro to init.ts - prompts for register (0-9)
    SaveMacroToInit,
    /// Promote a recorded macro to an editable init.ts command - prompts for register
    PromoteMacro,
    /// Set a bookmark - prompts for register (0-9)
    SetBookmark,
    /// Jump to a bookmark - prompts for register (0-9)
    JumpToBookmark,
    /// Set page width (empty clears to viewport)
    SetPageWidth,
    /// Add a vertical ruler at a column position
    AddRuler,
    /// Remove a vertical ruler (select from list)
    RemoveRuler,
    /// Set tab size for current buffer
    SetTabSize,
    /// Set line ending format for current buffer
    SetLineEnding,
    /// Set text encoding format for current buffer
    SetEncoding,
    /// Set language/syntax highlighting for current buffer
    SetLanguage,
    /// Stop a running LSP server (select from list)
    StopLspServer,
    /// Restart LSP server(s) (select from list)
    RestartLspServer,
    /// Select a theme (select from list)
    /// Stores the original theme name for restoration on cancel
    SelectTheme { original_theme: String },
    /// Select a keybinding map (select from list)
    SelectKeybindingMap,
    /// Select a cursor style (select from list)
    SelectCursorStyle,
    /// Select a UI locale/language (select from list)
    SelectLocale,
    /// Select a theme for copy with formatting
    CopyWithFormattingTheme,
    /// Confirm reverting a modified file
    ConfirmRevert,
    /// Confirm saving over a file that changed on disk
    ConfirmSaveConflict,
    /// Confirm saving with sudo after permission denied
    ConfirmSudoSave {
        info: crate::model::buffer::SudoSaveRequired,
    },
    /// Confirm overwriting an existing file during SaveAs
    ConfirmOverwriteFile { path: std::path::PathBuf },
    /// Confirm creating parent directories for a save target
    ConfirmCreateDirectory { path: std::path::PathBuf },
    /// Confirm closing a modified buffer (save/discard/cancel)
    /// Stores buffer_id to close after user confirms
    ConfirmCloseBuffer {
        buffer_id: crate::model::event::BufferId,
    },
    /// Confirm quitting with modified buffers
    ConfirmQuitWithModified,
    /// Confirm quitting on a clean session (opt-in via `editor.confirm_quit`).
    /// Issued only when no buffer is modified; otherwise
    /// `ConfirmQuitWithModified` runs instead.
    ConfirmQuit,
    /// File Explorer rename operation
    /// Stores the original path and name for the file/directory being renamed
    FileExplorerRename {
        original_path: std::path::PathBuf,
        original_name: String,
        /// True if this rename is for a newly created file (should switch focus to editor after)
        /// False if renaming an existing file (should keep focus in file explorer)
        is_new_file: bool,
    },
    /// Confirm deleting a file or directory in the file explorer
    ConfirmDeleteFile {
        path: std::path::PathBuf,
        is_dir: bool,
    },
    /// Confirm overwriting, renaming, or cancelling a paste conflict
    ConfirmPasteConflict {
        src: std::path::PathBuf,
        dst: std::path::PathBuf,
        is_cut: bool,
    },
    /// Rename destination when pasting (user chose 'r' in conflict prompt)
    FileExplorerPasteRename {
        src: std::path::PathBuf,
        dst_dir: std::path::PathBuf,
        is_cut: bool,
    },
    /// Confirm deleting multiple items from the file explorer
    ConfirmMultiDelete { paths: Vec<std::path::PathBuf> },
    /// Per-conflict prompt for multi-file paste.
    /// `pending[0]` is the conflict currently being shown.
    /// User choices: (o)verwrite this, (O) all, (s)kip this, (S) all, (c)ancel.
    ConfirmMultiPasteConflict {
        safe: Vec<(std::path::PathBuf, std::path::PathBuf)>,
        confirmed: Vec<(std::path::PathBuf, std::path::PathBuf)>,
        pending: Vec<(std::path::PathBuf, std::path::PathBuf)>,
        is_cut: bool,
    },
    /// Confirm loading a large file with non-resynchronizable encoding
    /// (like GB18030, GBK, Shift-JIS, EUC-KR) that requires full file loading
    ConfirmLargeFileEncoding { path: std::path::PathBuf },
    /// Switch to a tab by name (from the current split's open buffers)
    SwitchToTab,
    /// Run shell command on buffer/selection
    /// If replace is true, replace the input with the output
    /// If replace is false, output goes to a new buffer
    ShellCommand { replace: bool },
    /// Async prompt from plugin (for editor.prompt() API)
    /// The result is returned via callback resolution
    AsyncPrompt,
}

impl PromptType {
    /// Whether a mouse click on a suggestion should immediately confirm.
    ///
    /// Defaults to `true` (matches command palette / file finder UX). Returns
    /// `false` for prompts that pick from a small fixed list and trigger an
    /// expensive or destructive action — there, click should preview the
    /// selection and Enter should commit (issue #1660).
    pub fn click_confirms(&self) -> bool {
        !matches!(self, PromptType::ReloadWithEncoding)
    }

    /// Whether this prompt is one of the search/replace prompts that exposes
    /// the match-mode toggles (case sensitive / whole word / regex).
    ///
    /// This is the single source of truth for "are search options in scope":
    /// it gates both the rendering of the search-options bar and the
    /// `ToggleSearch*` actions, so the toggle keys are inert in unrelated
    /// prompts like the (s)ave/(d)iscard/(C)ancel close confirmation
    /// (otherwise Alt+W there would silently flip whole-word match mode —
    /// see issue with Alt+W leaking into the close-buffer prompt).
    pub fn has_search_options(&self) -> bool {
        matches!(
            self,
            PromptType::Search
                | PromptType::ReplaceSearch
                | PromptType::Replace { .. }
                | PromptType::QueryReplaceSearch
                | PromptType::QueryReplace { .. }
        )
    }
}

/// Prompt state for the minibuffer
#[derive(Debug, Clone)]
pub struct Prompt {
    /// The prompt message (e.g., "Find file: ")
    pub message: String,
    /// The query's text engine — the same single-field `TextEdit`
    /// every text surface uses. Text, caret, and selection all live
    /// here; access goes through the accessors below.
    edit: crate::primitives::text_edit::TextEdit,
    /// What to do when user confirms
    pub prompt_type: PromptType,
    /// Autocomplete suggestions (filtered)
    pub suggestions: Vec<Suggestion>,
    /// Original unfiltered suggestions (for prompts that filter client-side like SwitchToTab)
    pub original_suggestions: Option<Vec<Suggestion>>,
    /// Currently selected suggestion index
    pub selected_suggestion: Option<usize>,
    /// Index of the first suggestion shown in the popup viewport.
    /// Updated minimally by the renderer to keep `selected_suggestion`
    /// visible — selection changes inside the viewport never scroll
    /// (issue #1660).
    pub scroll_offset: usize,
    /// When true, the user has scrolled the result list with the mouse wheel,
    /// so the renderer must NOT pull `scroll_offset` back to keep the
    /// selection in view (issue #2119). Reset whenever the selection moves by
    /// keyboard or the suggestion list is rebuilt, so normal navigation
    /// re-engages the keep-selection-visible behaviour.
    pub manual_scroll: bool,
    /// Tracks the input value when suggestions were last set by a plugin.
    /// Used to skip Rust-side filtering when plugin has already filtered for this input.
    pub suggestions_set_for_input: Option<String>,
    /// When true, navigating suggestions updates the input text (selected) to match.
    /// Used by plugin prompts that want picker-like behavior (e.g. compose width).
    pub sync_input_on_navigate: bool,
    /// When true, the renderer draws the prompt inside a centred
    /// floating overlay (PopupPosition::CenteredOverlay) instead of
    /// the bottom minibuffer row. Set by the live-grep plugin via the
    /// `floatingOverlay` flag on `editor.startPrompt(...)`. The flag
    /// is rendering-only — confirm/cancel/hooks behave identically to
    /// a non-overlay prompt of the same `prompt_type`.
    pub overlay: bool,
    /// Title shown in the overlay's frame header as styled
    /// segments. An empty vec falls back to the `prompt_type`-
    /// specific default. Plugin-controlled via
    /// `editor.setPromptTitle(segments)`. Has no effect on
    /// non-overlay prompts.
    pub title: Vec<fresh_core::api::StyledText>,
    /// Optional footer chrome shown along the bottom of the
    /// floating overlay's results pane (above the frame border).
    /// Plugin-controlled via `editor.setPromptFooter(segments)`.
    /// Orchestrator uses this for hotkey-hint rows
    /// (e.g. " [n] new   [d] dive   [k] kill   [Esc] close").
    /// Empty by default; has no effect on non-overlay prompts.
    /// Implements the chrome-region piece of Primitive #2 in
    /// docs/internal/orchestrator-sessions-design.md (the
    /// session_preview delegate region was already provided by
    /// Primitive #1 — `editor.previewWindowInRect`).
    pub footer: Vec<fresh_core::api::StyledText>,
    /// Optional toolbar for the overlay's header band, as real widgets
    /// (`Toggle`/`Button` in a `Row`/`Col`). When `Some`, it is rendered via
    /// the widget engine *in place of* the styled-text `title`, so the
    /// controls are themed and clickable. Plugin-controlled via
    /// `editor.setPromptToolbar(spec)`. No effect on non-overlay prompts.
    pub toolbar_widget: Option<fresh_core::api::WidgetSpec>,
    /// Overlay focus ring position: `None` = the query input is focused
    /// (typing edits the query, the caret shows there); `Some(key)` = that
    /// toolbar control is focused (Space/Enter toggles it, it renders
    /// highlighted). Tab/Shift+Tab cycle input → toggles → input.
    pub toolbar_focus: Option<String>,
    /// Short status shown right-aligned on the input row, just left of the
    /// `selected / total` count (e.g. "Searching…", "No matches"). Plugin-
    /// controlled via `editor.setPromptStatus(text)`; overlay-only.
    pub status: String,
}

/// Maximum number of suggestion rows a bottom-anchored dropdown shows at once.
/// The palette is a transient overlay drawn over the document, so it stays
/// deliberately short rather than growing with the terminal — on an 80x24
/// terminal a height-sized dropdown would cover all but one line of the
/// buffer, and on a split layout it would hide the other pane entirely.
/// Overflow is communicated by the scrollbar the renderer draws over the
/// popup's right border (issues #623 / #1593) instead of by more rows.
///
/// Renderers with their own geometry (the floating Live Grep overlay, which
/// can be 30+ rows tall) pass their actual height to
/// [`Prompt::ensure_selected_visible_within`] rather than using this cap.
pub const MAX_VISIBLE_SUGGESTIONS: usize = 10;

impl Prompt {
    /// Create a new prompt
    pub fn new(message: String, prompt_type: PromptType) -> Self {
        Self {
            message,
            edit: crate::primitives::text_edit::TextEdit::single_line(),
            prompt_type,
            suggestions: Vec::new(),
            original_suggestions: None,
            selected_suggestion: None,
            scroll_offset: 0,
            manual_scroll: false,
            suggestions_set_for_input: None,
            sync_input_on_navigate: false,
            overlay: false,
            title: Vec::new(),
            footer: Vec::new(),
            toolbar_widget: None,
            toolbar_focus: None,
            status: String::new(),
        }
    }

    /// Create a new prompt with suggestions
    ///
    /// The suggestions are stored both as the current filtered list and as the original
    /// unfiltered list (for prompts that filter client-side like SwitchToTab).
    pub fn with_suggestions(
        message: String,
        prompt_type: PromptType,
        suggestions: Vec<Suggestion>,
    ) -> Self {
        let selected_suggestion = if suggestions.is_empty() {
            None
        } else {
            Some(0)
        };
        Self {
            message,
            edit: crate::primitives::text_edit::TextEdit::single_line(),
            prompt_type,
            original_suggestions: Some(suggestions.clone()),
            suggestions,
            selected_suggestion,
            scroll_offset: 0,
            manual_scroll: false,
            suggestions_set_for_input: None,
            sync_input_on_navigate: false,
            overlay: false,
            title: Vec::new(),
            footer: Vec::new(),
            toolbar_widget: None,
            toolbar_focus: None,
            status: String::new(),
        }
    }

    /// Create a new prompt with initial text, cursor at end, ready for
    /// incremental editing (no selection). Use for rename-style flows where
    /// the user typically keeps most of the prefilled name and only
    /// appends or tweaks a suffix.
    pub fn with_initial_text_for_edit(
        message: String,
        prompt_type: PromptType,
        initial_text: String,
    ) -> Self {
        Self::with_initial_text_inner(message, prompt_type, initial_text, false)
    }

    /// Create a new prompt with initial text (selected so typing replaces it)
    pub fn with_initial_text(
        message: String,
        prompt_type: PromptType,
        initial_text: String,
    ) -> Self {
        Self::with_initial_text_inner(message, prompt_type, initial_text, true)
    }

    fn with_initial_text_inner(
        message: String,
        prompt_type: PromptType,
        initial_text: String,
        select_all: bool,
    ) -> Self {
        let mut edit = crate::primitives::text_edit::TextEdit::single_line_with_text(&initial_text);
        if select_all && !initial_text.is_empty() {
            edit.select_all();
        } else {
            edit.move_end();
        }
        Self {
            message,
            edit,
            prompt_type,
            suggestions: Vec::new(),
            original_suggestions: None,
            selected_suggestion: None,
            scroll_offset: 0,
            manual_scroll: false,
            suggestions_set_for_input: None,
            sync_input_on_navigate: false,
            overlay: false,
            title: Vec::new(),
            footer: Vec::new(),
            toolbar_widget: None,
            toolbar_focus: None,
            status: String::new(),
        }
    }

    /// Move cursor left (to previous grapheme cluster boundary)
    ///
    /// Uses grapheme cluster boundaries for proper handling of combining characters
    /// like Thai diacritics, emoji with modifiers, etc.
    pub fn cursor_left(&mut self) {
        self.edit.move_left();
    }

    /// Move cursor right (to next grapheme cluster boundary)
    ///
    /// Uses grapheme cluster boundaries for proper handling of combining characters
    /// like Thai diacritics, emoji with modifiers, etc.
    pub fn cursor_right(&mut self) {
        self.edit.move_right();
    }

    /// Run one editing operation through the shared single-field
    /// engine ([`TextEdit`](crate::primitives::text_edit::TextEdit)) —
    /// the prompt's `(input, cursor_pos, selection_anchor)` sync in,
    /// the op runs, and the post-state syncs back. This is what
    /// deleted the prompt's private editing implementations: boundary
    /// walks, drains, and selection bookkeeping exist once, in the
    /// engine every text field uses. (The prompt keeps its own word
    /// *motion* policy below — it deliberately mimics the buffer's
    /// next-word-start / select-to-word-end behavior, which differs
    /// from the engine's word hops.)
    fn apply_edit(&mut self, op: impl FnOnce(&mut crate::primitives::text_edit::TextEdit)) {
        op(&mut self.edit);
    }

    /// Route one editing key through the shared text-key table — the
    /// same `apply_text_key` mapping the Settings fields and widget
    /// Text use — via [`Self::apply_edit`]. Returns true when the
    /// table handled the key. Deliberately NOT routed here: Ctrl+
    /// word-motion (the prompt keeps buffer-style next-word-start /
    /// select-to-word-end policy), printable chars (the input ladder
    /// keeps its shift-uppercase compensation and suggestion-refresh
    /// policy), and every chrome key the table ignores by contract.
    pub(crate) fn handle_text_key(&mut self, event: &crossterm::event::KeyEvent) -> bool {
        use crossterm::event::{KeyCode, KeyModifiers};
        let ctrl = event.modifiers.contains(KeyModifiers::CONTROL);
        let covered = matches!(event.code, KeyCode::Backspace | KeyCode::Delete)
            || (!ctrl
                && matches!(
                    event.code,
                    KeyCode::Left | KeyCode::Right | KeyCode::Home | KeyCode::End
                ));
        if !covered {
            return false;
        }
        if matches!(event.code, KeyCode::Backspace | KeyCode::Delete) {}
        self.apply_edit(|e| {
            crate::primitives::text_key::apply_text_key(
                e,
                event,
                crate::primitives::text_key::TextKeyContext::multiline(false),
            );
        });
        true
    }

    /// Undo the last input edit (engine history — the prompt keeps no
    /// stacks of its own). Consumed by Ctrl+Z so undo edits the query
    /// box rather than the underlying (modal-inaccessible) buffer.
    pub fn undo_input(&mut self) -> bool {
        self.edit.undo()
    }

    /// Redo the last undone input edit. Returns true if the input changed.
    pub fn redo_input(&mut self) -> bool {
        self.edit.redo()
    }

    /// Insert a character at the cursor position
    pub fn insert_char(&mut self, ch: char) {
        self.apply_edit(|e| e.insert_char(ch));
    }

    /// Delete one code point before cursor (backspace)
    ///
    /// Deletes one Unicode code point at a time, allowing layer-by-layer deletion
    /// of combining characters. For Thai text, this means you can delete just the
    /// tone mark without removing the base consonant.
    pub fn backspace(&mut self) {
        if self.cursor_byte() > 0 || self.has_selection() {
            self.apply_edit(|e| e.backspace());
        }
    }

    /// Delete grapheme cluster at cursor (delete key)
    ///
    /// Deletes the entire grapheme cluster, handling combining characters properly.
    pub fn delete(&mut self) {
        if self.cursor_byte() < self.input_str().len() || self.has_selection() {
            self.apply_edit(|e| e.delete());
        }
    }

    /// Move to start of input
    pub fn move_to_start(&mut self) {
        self.edit.move_home();
    }

    /// Move to end of input
    pub fn move_to_end(&mut self) {
        self.edit.move_end();
    }

    /// Set the input text and cursor position
    ///
    /// Used for history navigation - replaces the entire input with a new value
    /// and moves cursor to the end.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Search: ".to_string(), PromptType::Search);
    /// prompt.set_input_plain("current".to_string());
    /// prompt.set_cursor_byte(7);
    ///
    /// prompt.set_input("from history".to_string());
    /// assert_eq!(prompt.input_str(), "from history");
    /// assert_eq!(prompt.cursor_byte(), 12); // At end
    /// ```
    pub fn set_input(&mut self, text: String) {
        // A wholesale replacement (history navigation, Tab-accept) is
        // an undoable step; `set_value` itself never checkpoints.
        self.edit.push_undo_checkpoint();
        self.edit.set_value(&text);
        self.edit.move_end();
    }

    /// Select next suggestion
    pub fn select_next_suggestion(&mut self) {
        if !self.suggestions.is_empty() {
            // Keyboard navigation re-engages keep-selection-visible scrolling.
            self.manual_scroll = false;
            self.selected_suggestion = Some(match self.selected_suggestion {
                Some(idx) if idx + 1 < self.suggestions.len() => idx + 1,
                Some(_) => 0, // Wrap to start
                None => 0,
            });
        }
    }

    /// Select previous suggestion
    pub fn select_prev_suggestion(&mut self) {
        if !self.suggestions.is_empty() {
            self.manual_scroll = false;
            self.selected_suggestion = Some(match self.selected_suggestion {
                Some(0) => self.suggestions.len() - 1, // Wrap to end
                Some(idx) => idx - 1,
                None => 0,
            });
        }
    }

    /// Scroll the result list by `delta` rows without moving the selection
    /// (mouse wheel over the Live Grep overlay results pane, issue #2119).
    /// `visible` is the number of result rows currently on screen, used to
    /// clamp the offset so it can't scroll past the end of the list.
    pub fn scroll_results(&mut self, delta: i32, visible: usize) {
        let total = self.suggestions.len();
        if total == 0 {
            return;
        }
        let max_offset = total.saturating_sub(visible.max(1));
        let next = (self.scroll_offset as i32 + delta).clamp(0, max_offset as i32) as usize;
        if next != self.scroll_offset {
            self.scroll_offset = next;
        }
        // Latch manual scroll even when clamped at an edge, so a follow-up
        // render doesn't immediately yank the offset back to the selection.
        self.manual_scroll = true;
    }

    /// Get the currently selected suggestion value
    pub fn selected_value(&self) -> Option<String> {
        self.selected_suggestion
            .and_then(|idx| self.suggestions.get(idx))
            .map(|s| s.get_value().to_string())
    }

    /// Get the final input (use selected suggestion if available, otherwise raw input)
    /// The query text. Accessor for the §4.5 state collapse: external
    /// readers go through this (not the field) so the storage can
    /// become an embedded `TextEdit` without touching them again.
    pub fn input_str(&self) -> &str {
        self.edit.current_line()
    }

    /// The caret's byte offset in the query text. Accessor twin of
    /// [`Self::input_str`].
    pub fn cursor_byte(&self) -> usize {
        self.edit.flat_cursor_byte()
    }

    /// Replace the query with `text`, caret at end, whole text
    /// selected (anchor at 0) — the suggestion-navigation sync shape:
    /// the synced value sits selected so typing replaces it.
    pub fn set_input_selected(&mut self, text: String) {
        self.edit.set_value(&text);
        self.edit.select_all();
    }

    /// Replace the query with `text`, caret at end, no selection and
    /// no undo snapshot — the programmatic sync shape (click-on-
    /// suggestion, encoding pre-fill, dialog resets).
    pub fn set_input_plain(&mut self, text: String) {
        self.edit.set_value(&text);
        self.edit.move_end();
    }

    /// Place the caret at `byte` (clamped into the text). Accessor
    /// twin of [`Self::cursor_byte`] for callers that position the
    /// caret directly (mouse placement, tests).
    pub fn set_cursor_byte(&mut self, byte: usize) {
        self.edit.set_cursor_from_flat(byte);
    }

    /// Select the byte range `anchor..cursor` (caret lands at
    /// `cursor`). Used by flows that programmatically select a
    /// sub-range (and by tests).
    pub fn select_range(&mut self, anchor: usize, cursor: usize) {
        self.edit.set_cursor_from_flat(cursor);
        if anchor != self.edit.flat_cursor_byte() {
            let a = anchor.min(self.input_str().len());
            self.edit.selection_anchor = Some((0, a));
        }
    }

    pub fn get_final_input(&self) -> String {
        self.selected_value().unwrap_or_else(|| self.edit.value())
    }

    /// Apply fuzzy filtering to suggestions based on current input
    ///
    /// If `match_description` is true, also matches against suggestion descriptions.
    /// Updates `suggestions` with filtered and sorted results.
    pub fn filter_suggestions(&mut self, match_description: bool) {
        use crate::input::fuzzy::{fuzzy_match, FuzzyMatch};

        // Skip filtering if the plugin has already set suggestions for this exact input.
        // This handles the race condition where run_hook("prompt_changed") is async:
        // the plugin may have already responded with filtered results via setPromptSuggestions.
        if let Some(ref set_for_input) = self.suggestions_set_for_input {
            if set_for_input.as_str() == self.input_str() {
                return;
            }
        }
        // Input has diverged from whatever the plugin pre-filtered
        // for — invalidate the marker so a later return to that
        // same input doesn't reuse a now-stale list.
        self.suggestions_set_for_input = None;

        let Some(original) = &self.original_suggestions else {
            return;
        };

        let input = self.input_str().to_string();
        let input = &input;
        let mut filtered: Vec<(crate::input::commands::Suggestion, i32)> = original
            .iter()
            .filter_map(|s| {
                let text_result = fuzzy_match(input, &s.text);
                let desc_result = if match_description {
                    s.description
                        .as_ref()
                        .map(|d| fuzzy_match(input, d))
                        .unwrap_or_else(FuzzyMatch::no_match)
                } else {
                    FuzzyMatch::no_match()
                };
                if text_result.matched || desc_result.matched {
                    Some((s.clone(), text_result.score.max(desc_result.score)))
                } else {
                    None
                }
            })
            .collect();

        filtered.sort_by_key(|b| std::cmp::Reverse(b.1));
        self.suggestions = filtered.into_iter().map(|(s, _)| s).collect();
        self.selected_suggestion = if self.suggestions.is_empty() {
            None
        } else {
            Some(0)
        };
        self.scroll_offset = 0;
        self.manual_scroll = false;
    }

    /// Adjust `scroll_offset` so that `selected_suggestion` is inside a
    /// viewport of `visible_count` rows, scrolling the minimum amount
    /// required. A selection that's already on-screen leaves the viewport
    /// untouched — this is what stops a click on a near-bottom item from
    /// snapping the list upward and recentering under the cursor (issue
    /// #1660). Callers pass the actual rendered height of their list
    /// (bottom-anchored popup and floating Live Grep overlay alike), so
    /// the scroll only moves when the selection genuinely leaves the
    /// visible window.
    pub fn ensure_selected_visible_within(&mut self, visible_count: usize) {
        let total = self.suggestions.len();
        let visible = total.min(visible_count.max(1));
        let max_offset = total.saturating_sub(visible);
        if visible == 0 {
            self.scroll_offset = 0;
            return;
        }
        if let Some(selected) = self.selected_suggestion {
            if selected < self.scroll_offset {
                self.scroll_offset = selected;
            } else if selected >= self.scroll_offset + visible {
                self.scroll_offset = selected + 1 - visible;
            }
        }
        if self.scroll_offset > max_offset {
            self.scroll_offset = max_offset;
        }
    }

    // ========================================================================
    // Advanced editing operations (word-based, clipboard)
    // ========================================================================
    //
    // MOTIVATION:
    // These methods provide advanced editing capabilities in prompts that
    // users expect from normal text editing:
    // - Word-based deletion (Ctrl+Backspace/Delete)
    // - Copy/paste/cut operations
    //
    // This enables consistent editing experience across both buffer editing
    // and prompt input (command palette, file picker, search, etc.).

    /// Delete from cursor to end of word (Ctrl+Delete).
    ///
    /// Deletes from the current cursor position to the end of the current word.
    /// If the cursor is at a non-word character, skips to the next word and
    /// deletes to its end.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.set_input_plain("hello world".to_string());
    /// prompt.set_cursor_byte(0); // At start of "hello"
    /// prompt.delete_word_forward();
    /// assert_eq!(prompt.input_str(), " world");
    /// assert_eq!(prompt.cursor_byte(), 0);
    /// ```
    pub fn delete_word_forward(&mut self) {
        if find_word_end_bytes(self.input_str().as_bytes(), self.cursor_byte()) > self.cursor_byte()
        {
            self.apply_edit(|e| e.delete_word_forward());
        }
    }

    /// Delete from start of word to cursor (Ctrl+Backspace).
    ///
    /// Deletes from the start of the current word to the cursor position.
    /// If the cursor is after a non-word character, deletes the previous word.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.set_input_plain("hello world".to_string());
    /// prompt.set_cursor_byte(5); // After "hello"
    /// prompt.delete_word_backward();
    /// assert_eq!(prompt.input_str(), " world");
    /// assert_eq!(prompt.cursor_byte(), 0);
    /// ```
    pub fn delete_word_backward(&mut self) {
        if find_word_start_bytes(self.input_str().as_bytes(), self.cursor_byte())
            < self.cursor_byte()
        {
            self.apply_edit(|e| e.delete_word_backward());
        }
    }

    /// Delete from cursor to end of line (Ctrl+K).
    ///
    /// Deletes all text from the cursor position to the end of the input.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.set_input_plain("hello world".to_string());
    /// prompt.set_cursor_byte(5); // After "hello"
    /// prompt.delete_to_end();
    /// assert_eq!(prompt.input_str(), "hello");
    /// assert_eq!(prompt.cursor_byte(), 5);
    /// ```
    pub fn delete_to_end(&mut self) {
        if self.cursor_byte() < self.input_str().len() {
            self.apply_edit(|e| e.delete_to_end());
        }
    }

    /// Delete from the cursor back to the start of the line (Ctrl+U).
    ///
    /// Mirrors the standard readline kill-to-start behavior so the
    /// command palette can be cleared without holding Backspace.
    pub fn delete_to_start(&mut self) {
        if self.cursor_byte() > 0 {
            self.apply_edit(|e| e.delete_to_start());
        }
    }

    /// Get the current input text (for copy operation).
    ///
    /// Returns a copy of the entire input. In future, this could be extended
    /// to support selection ranges for copying only selected text.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Search: ".to_string(), PromptType::Search);
    /// prompt.set_input_plain("test query".to_string());
    /// assert_eq!(prompt.get_text(), "test query");
    /// ```
    pub fn get_text(&self) -> String {
        self.edit.value()
    }

    /// Clear the input (used for cut operation).
    ///
    /// Removes all text from the input and resets cursor to start.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
    /// prompt.set_input_plain("some text".to_string());
    /// prompt.set_cursor_byte(9);
    /// prompt.clear();
    /// assert_eq!(prompt.input_str(), "");
    /// assert_eq!(prompt.cursor_byte(), 0);
    /// ```
    pub fn clear(&mut self) {
        self.edit.clear();
        // Also clear selection when clearing input
        self.selected_suggestion = None;
    }

    /// Insert text at cursor position (used for paste operation).
    ///
    /// Inserts the given text at the current cursor position and moves
    /// the cursor to the end of the inserted text.
    ///
    /// # Example
    /// ```
    /// # use fresh::prompt::{Prompt, PromptType};
    /// let mut prompt = Prompt::new("Command: ".to_string(), PromptType::QuickOpen);
    /// prompt.set_input_plain("save".to_string());
    /// prompt.set_cursor_byte(4);
    /// prompt.insert_str(" file");
    /// assert_eq!(prompt.input_str(), "save file");
    /// assert_eq!(prompt.cursor_byte(), 9);
    /// ```
    pub fn insert_str(&mut self, text: &str) {
        // Replaces any active selection (engine behavior).
        self.apply_edit(|e| e.insert_str(text));
    }

    // ========================================================================
    // Selection support
    // ========================================================================

    /// Check if there's an active selection
    pub fn has_selection(&self) -> bool {
        self.edit.has_selection()
    }

    /// Get the selection range (start, end) where start <= end
    pub fn selection_range(&self) -> Option<(usize, usize)> {
        self.edit.selection_flat_range()
    }

    /// Get the selected text
    pub fn selected_text(&self) -> Option<String> {
        self.edit.selected_text()
    }

    /// Delete the current selection and return the deleted text
    pub fn delete_selection(&mut self) -> Option<String> {
        if self.selection_range().is_some() {
            let mut deleted = None;
            self.apply_edit(|e| deleted = e.delete_selection());
            deleted
        } else {
            None
        }
    }

    /// Clear selection without deleting text
    pub fn clear_selection(&mut self) {
        self.edit.clear_selection();
    }

    /// Move cursor left with selection (by grapheme cluster)
    pub fn move_left_selecting(&mut self) {
        self.apply_edit(|e| e.move_left_selecting());
    }

    /// Move cursor right with selection (by grapheme cluster)
    pub fn move_right_selecting(&mut self) {
        self.apply_edit(|e| e.move_right_selecting());
    }

    /// Move to start of input with selection
    pub fn move_home_selecting(&mut self) {
        self.apply_edit(|e| e.move_home_selecting());
    }

    /// Move to end of input with selection
    pub fn move_end_selecting(&mut self) {
        self.apply_edit(|e| e.move_end_selecting());
    }

    /// Move to start of previous word with selection
    /// Mimics Buffer's find_word_start_left behavior
    pub fn move_word_left_selecting(&mut self) {
        let anchor = self
            .edit
            .selection_anchor
            .map(|(_, col)| col)
            .unwrap_or_else(|| self.edit.flat_cursor_byte());
        let value = self.edit.value();
        let bytes = value.as_bytes();
        let cursor = self.edit.flat_cursor_byte();
        if cursor == 0 {
            self.edit.selection_anchor = Some((0, anchor));
            return;
        }

        let mut new_pos = cursor.saturating_sub(1);

        // Skip non-word characters (spaces) backwards
        while new_pos > 0 && !is_word_char(bytes[new_pos]) {
            new_pos = new_pos.saturating_sub(1);
        }

        // Find start of word
        while new_pos > 0 && is_word_char(bytes[new_pos.saturating_sub(1)]) {
            new_pos = new_pos.saturating_sub(1);
        }

        // `set_cursor_from_flat` clears the anchor; re-attach it after.
        self.edit.set_cursor_from_flat(new_pos);
        self.edit.selection_anchor = Some((0, anchor));
    }

    /// Move to end of next word with selection
    /// For selection, we want to select whole words, so move to word END, not word START
    pub fn move_word_right_selecting(&mut self) {
        let anchor = self
            .edit
            .selection_anchor
            .map(|(_, col)| col)
            .unwrap_or_else(|| self.edit.flat_cursor_byte());
        // Use find_word_end_bytes which moves to the END of words
        let value = self.edit.value();
        let bytes = value.as_bytes();
        let cursor = self.edit.flat_cursor_byte();
        let mut new_pos = find_word_end_bytes(bytes, cursor);

        // If we didn't move (already at word end), move forward to next word end
        if new_pos == cursor && new_pos < bytes.len() {
            new_pos = (new_pos + 1).min(bytes.len());
            new_pos = find_word_end_bytes(bytes, new_pos);
        }

        // `set_cursor_from_flat` clears the anchor; re-attach it after.
        self.edit.set_cursor_from_flat(new_pos);
        self.edit.selection_anchor = Some((0, anchor));
    }

    /// Move to start of previous word (without selection)
    /// Mimics Buffer's find_word_start_left behavior
    pub fn move_word_left(&mut self) {
        self.clear_selection();

        let value = self.edit.value();
        let bytes = value.as_bytes();
        let cursor = self.edit.flat_cursor_byte();
        if cursor == 0 {
            return;
        }

        let mut new_pos = cursor.saturating_sub(1);

        // Skip non-word characters (spaces) backwards
        while new_pos > 0 && !is_word_char(bytes[new_pos]) {
            new_pos = new_pos.saturating_sub(1);
        }

        // Find start of word
        while new_pos > 0 && is_word_char(bytes[new_pos.saturating_sub(1)]) {
            new_pos = new_pos.saturating_sub(1);
        }

        self.edit.set_cursor_from_flat(new_pos);
    }

    /// Move to start of next word (without selection)
    /// Mimics Buffer's find_word_start_right behavior
    pub fn move_word_right(&mut self) {
        self.clear_selection();

        let value = self.edit.value();
        let bytes = value.as_bytes();
        let cursor = self.edit.flat_cursor_byte();
        if cursor >= bytes.len() {
            return;
        }

        let mut new_pos = cursor;

        // Skip current word
        while new_pos < bytes.len() && is_word_char(bytes[new_pos]) {
            new_pos += 1;
        }

        // Skip non-word characters (spaces)
        while new_pos < bytes.len() && !is_word_char(bytes[new_pos]) {
            new_pos += 1;
        }

        self.edit.set_cursor_from_flat(new_pos);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_delete_word_forward_basic() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world test".to_string());
        prompt.set_cursor_byte(0);

        prompt.delete_word_forward();
        assert_eq!(prompt.input_str(), " world test");
        assert_eq!(prompt.cursor_byte(), 0);
    }

    #[test]
    fn test_delete_word_forward_middle() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world test".to_string());
        prompt.set_cursor_byte(3); // Middle of "hello"

        prompt.delete_word_forward();
        assert_eq!(prompt.input_str(), "hel world test");
        assert_eq!(prompt.cursor_byte(), 3);
    }

    #[test]
    fn test_delete_word_forward_at_space() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world".to_string());
        prompt.set_cursor_byte(5); // At space after "hello"

        prompt.delete_word_forward();
        assert_eq!(prompt.input_str(), "hello");
        assert_eq!(prompt.cursor_byte(), 5);
    }

    #[test]
    fn test_delete_word_backward_basic() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world test".to_string());
        prompt.set_cursor_byte(5); // After "hello"

        prompt.delete_word_backward();
        assert_eq!(prompt.input_str(), " world test");
        assert_eq!(prompt.cursor_byte(), 0);
    }

    #[test]
    fn test_delete_word_backward_middle() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world test".to_string());
        prompt.set_cursor_byte(8); // Middle of "world"

        prompt.delete_word_backward();
        assert_eq!(prompt.input_str(), "hello rld test");
        assert_eq!(prompt.cursor_byte(), 6);
    }

    #[test]
    fn test_delete_word_backward_at_end() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world".to_string());
        prompt.set_cursor_byte(11); // At end

        prompt.delete_word_backward();
        assert_eq!(prompt.input_str(), "hello ");
        assert_eq!(prompt.cursor_byte(), 6);
    }

    #[test]
    fn test_delete_word_with_special_chars() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("save-file-as".to_string());
        prompt.set_cursor_byte(12); // At end

        // Delete "as"
        prompt.delete_word_backward();
        assert_eq!(prompt.input_str(), "save-file-");
        assert_eq!(prompt.cursor_byte(), 10);

        // Delete "file"
        prompt.delete_word_backward();
        assert_eq!(prompt.input_str(), "save-");
        assert_eq!(prompt.cursor_byte(), 5);
    }

    #[test]
    fn test_get_text() {
        let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
        prompt.set_input_plain("test content".to_string());

        assert_eq!(prompt.get_text(), "test content");
    }

    #[test]
    fn test_clear() {
        let mut prompt = Prompt::new("Find: ".to_string(), PromptType::OpenFile);
        prompt.set_input_plain("some text".to_string());
        prompt.set_cursor_byte(5);
        prompt.selected_suggestion = Some(0);

        prompt.clear();

        assert_eq!(prompt.input_str(), "");
        assert_eq!(prompt.cursor_byte(), 0);
        assert_eq!(prompt.selected_suggestion, None);
    }

    #[test]
    fn test_delete_forward_basic() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello".to_string());
        prompt.set_cursor_byte(1); // After 'h'

        // Simulate delete key (remove 'e')
        prompt.delete();

        assert_eq!(prompt.input_str(), "hllo");
        assert_eq!(prompt.cursor_byte(), 1);
    }

    #[test]
    fn test_delete_at_end() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello".to_string());
        prompt.set_cursor_byte(5); // At end

        // Delete at end should do nothing
        if prompt.cursor_byte() < prompt.input_str().len() {
            prompt.delete();
        }

        assert_eq!(prompt.input_str(), "hello");
        assert_eq!(prompt.cursor_byte(), 5);
    }

    #[test]
    fn test_insert_str_at_start() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("world".to_string());
        prompt.set_cursor_byte(0);

        prompt.insert_str("hello ");
        assert_eq!(prompt.input_str(), "hello world");
        assert_eq!(prompt.cursor_byte(), 6);
    }

    #[test]
    fn test_insert_str_at_middle() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("helloworld".to_string());
        prompt.set_cursor_byte(5);

        prompt.insert_str(" ");
        assert_eq!(prompt.input_str(), "hello world");
        assert_eq!(prompt.cursor_byte(), 6);
    }

    #[test]
    fn test_insert_str_at_end() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello".to_string());
        prompt.set_cursor_byte(5);

        prompt.insert_str(" world");
        assert_eq!(prompt.input_str(), "hello world");
        assert_eq!(prompt.cursor_byte(), 11);
    }

    #[test]
    fn test_delete_word_forward_empty() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("".to_string());
        prompt.set_cursor_byte(0);

        prompt.delete_word_forward();
        assert_eq!(prompt.input_str(), "");
        assert_eq!(prompt.cursor_byte(), 0);
    }

    #[test]
    fn test_delete_word_backward_empty() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("".to_string());
        prompt.set_cursor_byte(0);

        prompt.delete_word_backward();
        assert_eq!(prompt.input_str(), "");
        assert_eq!(prompt.cursor_byte(), 0);
    }

    #[test]
    fn test_delete_word_forward_only_spaces() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("   ".to_string());
        prompt.set_cursor_byte(0);

        prompt.delete_word_forward();
        assert_eq!(prompt.input_str(), "");
        assert_eq!(prompt.cursor_byte(), 0);
    }

    #[test]
    fn test_multiple_word_deletions() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("one two three four".to_string());
        prompt.set_cursor_byte(18);

        prompt.delete_word_backward(); // Delete "four"
        assert_eq!(prompt.input_str(), "one two three ");

        prompt.delete_word_backward(); // Delete "three"
        assert_eq!(prompt.input_str(), "one two ");

        prompt.delete_word_backward(); // Delete "two"
        assert_eq!(prompt.input_str(), "one ");
    }

    // Tests for selection functionality
    #[test]
    fn test_selection_with_shift_arrows() {
        let mut prompt = Prompt::new("Command: ".to_string(), PromptType::QuickOpen);
        prompt.set_input_plain("hello world".to_string());
        prompt.set_cursor_byte(5); // After "hello"

        // No selection initially
        assert!(!prompt.has_selection());
        assert_eq!(prompt.selected_text(), None);

        // Move right selecting - should select " "
        prompt.move_right_selecting();
        assert!(prompt.has_selection());
        assert_eq!(prompt.selection_range(), Some((5, 6)));
        assert_eq!(prompt.selected_text(), Some(" ".to_string()));

        // Move right selecting again - should select " w"
        prompt.move_right_selecting();
        assert_eq!(prompt.selection_range(), Some((5, 7)));
        assert_eq!(prompt.selected_text(), Some(" w".to_string()));

        // Move left selecting - should shrink to " "
        prompt.move_left_selecting();
        assert_eq!(prompt.selection_range(), Some((5, 6)));
        assert_eq!(prompt.selected_text(), Some(" ".to_string()));
    }

    #[test]
    fn test_selection_backward() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("abcdef".to_string());
        prompt.set_cursor_byte(4); // After "abcd"

        // Select backward
        prompt.move_left_selecting();
        prompt.move_left_selecting();
        assert!(prompt.has_selection());
        assert_eq!(prompt.selection_range(), Some((2, 4)));
        assert_eq!(prompt.selected_text(), Some("cd".to_string()));
    }

    #[test]
    fn test_selection_with_home_end() {
        let mut prompt = Prompt::new("Prompt: ".to_string(), PromptType::QuickOpen);
        prompt.set_input_plain("select this text".to_string());
        prompt.set_cursor_byte(7); // After "select "

        // Select to end
        prompt.move_end_selecting();
        assert_eq!(prompt.selection_range(), Some((7, 16)));
        assert_eq!(prompt.selected_text(), Some("this text".to_string()));

        // Clear and select from current position to home
        prompt.clear_selection();
        prompt.move_home_selecting();
        assert_eq!(prompt.selection_range(), Some((0, 16)));
        assert_eq!(prompt.selected_text(), Some("select this text".to_string()));
    }

    #[test]
    fn test_word_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("one two three".to_string());
        prompt.set_cursor_byte(4); // After "one "

        // Select word right
        prompt.move_word_right_selecting();
        assert_eq!(prompt.selection_range(), Some((4, 7)));
        assert_eq!(prompt.selected_text(), Some("two".to_string()));

        // Select another word
        prompt.move_word_right_selecting();
        assert_eq!(prompt.selection_range(), Some((4, 13)));
        assert_eq!(prompt.selected_text(), Some("two three".to_string()));
    }

    #[test]
    fn test_word_selection_backward() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("one two three".to_string());
        prompt.set_cursor_byte(13); // At end

        // Select word left - moves to start of "three"
        prompt.move_word_left_selecting();
        assert_eq!(prompt.selection_range(), Some((8, 13)));
        assert_eq!(prompt.selected_text(), Some("three".to_string()));

        // Note: Currently, calling move_word_left_selecting again when already
        // at a word boundary doesn't move further back. This matches the behavior
        // of find_word_start_bytes which finds the start of the current word.
        // For multi-word backward selection, move cursor backward first, then select.
    }

    #[test]
    fn test_delete_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world".to_string());
        prompt.set_cursor_byte(5);

        // Select " world"
        prompt.move_end_selecting();
        assert_eq!(prompt.selected_text(), Some(" world".to_string()));

        // Delete selection
        let deleted = prompt.delete_selection();
        assert_eq!(deleted, Some(" world".to_string()));
        assert_eq!(prompt.input_str(), "hello");
        assert_eq!(prompt.cursor_byte(), 5);
        assert!(!prompt.has_selection());
    }

    #[test]
    fn test_insert_deletes_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello world".to_string());
        prompt.set_cursor_byte(0);

        // Select "hello"
        for _ in 0..5 {
            prompt.move_right_selecting();
        }
        assert_eq!(prompt.selected_text(), Some("hello".to_string()));

        // Insert text - should delete selection first
        prompt.insert_str("goodbye");
        assert_eq!(prompt.input_str(), "goodbye world");
        assert_eq!(prompt.cursor_byte(), 7);
        assert!(!prompt.has_selection());
    }

    #[test]
    fn test_clear_selection() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("test".to_string());
        prompt.set_cursor_byte(0);

        // Create selection
        prompt.move_end_selecting();
        assert!(prompt.has_selection());

        // Clear selection
        prompt.clear_selection();
        assert!(!prompt.has_selection());
        assert_eq!(prompt.cursor_byte(), 4); // Cursor should remain at end
        assert_eq!(prompt.input_str(), "test"); // Input unchanged
    }

    #[test]
    fn test_selection_edge_cases() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("abc".to_string());
        prompt.set_cursor_byte(3);

        // Select beyond end should stop at end (no movement, no selection)
        prompt.move_right_selecting();
        assert_eq!(prompt.cursor_byte(), 3);
        // Since cursor didn't move, anchor equals cursor, so no selection
        assert_eq!(prompt.selection_range(), None);
        assert_eq!(prompt.selected_text(), None);

        // Delete non-existent selection should return None
        assert_eq!(prompt.delete_selection(), None);
        assert_eq!(prompt.input_str(), "abc");
    }

    #[test]
    fn test_selection_with_unicode() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("hello 世界 world".to_string());
        prompt.set_cursor_byte(6); // After "hello "

        // Select the Chinese characters
        for _ in 0..2 {
            prompt.move_right_selecting();
        }

        let selected = prompt.selected_text().unwrap();
        assert_eq!(selected, "世界");

        // Delete should work correctly
        prompt.delete_selection();
        assert_eq!(prompt.input_str(), "hello  world");
    }

    // BUG REPRODUCTION TESTS

    /// Test that Ctrl+Shift+Left continues past first word boundary (was bug #2)
    #[test]
    fn test_word_selection_continues_across_words() {
        let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
        prompt.set_input_plain("one two three".to_string());
        prompt.set_cursor_byte(13); // At end

        // First Ctrl+Shift+Left - selects "three"
        prompt.move_word_left_selecting();
        assert_eq!(prompt.selection_range(), Some((8, 13)));
        assert_eq!(prompt.selected_text(), Some("three".to_string()));

        // Second Ctrl+Shift+Left - should extend to "two three"
        // Now correctly moves back one more word when already at word boundary
        prompt.move_word_left_selecting();

        // Selection should extend to include "two three"
        assert_eq!(prompt.selection_range(), Some((4, 13)));
        assert_eq!(prompt.selected_text(), Some("two three".to_string()));
    }

    // Property-based tests for Prompt operations
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        proptest! {
            /// Property: delete_word_backward should never increase input length
            #[test]
            fn prop_delete_word_backward_shrinks(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input.clone());
                prompt.set_cursor_byte(cursor_pos.min(input.len()));

                let original_len = prompt.input_str().len();
                prompt.delete_word_backward();

                prop_assert!(prompt.input_str().len() <= original_len);
            }

            /// Property: delete_word_forward should never increase input length
            #[test]
            fn prop_delete_word_forward_shrinks(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input.clone());
                prompt.set_cursor_byte(cursor_pos.min(input.len()));

                let original_len = prompt.input_str().len();
                prompt.delete_word_forward();

                prop_assert!(prompt.input_str().len() <= original_len);
            }

            /// Property: delete_word_backward should not move cursor past input start
            #[test]
            fn prop_delete_word_backward_cursor_valid(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input.clone());
                prompt.set_cursor_byte(cursor_pos.min(input.len()));

                prompt.delete_word_backward();

                prop_assert!(prompt.cursor_byte() <= prompt.input_str().len());
            }

            /// Property: delete_word_forward should keep cursor in valid range
            #[test]
            fn prop_delete_word_forward_cursor_valid(
                input in "[a-zA-Z0-9_ ]{0,50}",
                cursor_pos in 0usize..50
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input.clone());
                prompt.set_cursor_byte(cursor_pos.min(input.len()));

                prompt.delete_word_forward();

                prop_assert!(prompt.cursor_byte() <= prompt.input_str().len());
            }

            /// Property: insert_str should increase length by inserted text length
            #[test]
            fn prop_insert_str_length(
                input in "[a-zA-Z0-9_ ]{0,30}",
                insert in "[a-zA-Z0-9_ ]{0,20}",
                cursor_pos in 0usize..30
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input.clone());
                prompt.set_cursor_byte(cursor_pos.min(input.len()));

                let original_len = prompt.input_str().len();
                prompt.insert_str(&insert);

                prop_assert_eq!(prompt.input_str().len(), original_len + insert.len());
            }

            /// Property: insert_str should move cursor by inserted text length
            #[test]
            fn prop_insert_str_cursor(
                input in "[a-zA-Z0-9_ ]{0,30}",
                insert in "[a-zA-Z0-9_ ]{0,20}",
                cursor_pos in 0usize..30
            ) {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input.clone());
                let original_pos = cursor_pos.min(input.len());
                prompt.set_cursor_byte(original_pos);

                prompt.insert_str(&insert);

                prop_assert_eq!(prompt.cursor_byte(), original_pos + insert.len());
            }

            /// Property: clear should always result in empty string and zero cursor
            #[test]
            fn prop_clear_resets(input in "[a-zA-Z0-9_ ]{0,50}") {
                let mut prompt = Prompt::new("Test: ".to_string(), PromptType::Search);
                prompt.set_input_plain(input);
                prompt.set_cursor_byte(prompt.input_str().len());

                prompt.clear();

                prop_assert_eq!(prompt.input_str(), "");
                prop_assert_eq!(prompt.cursor_byte(), 0);
            }
        }
    }
}
