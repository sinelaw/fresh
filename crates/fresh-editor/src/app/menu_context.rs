//! Menu context computation.
//!
//! This module provides methods to compute menu context values that determine
//! when menu items and commands should be enabled or disabled. Each context
//! value has a dedicated method that encapsulates the logic for checking
//! whether that feature is available.

use super::Editor;
use crate::view::ui::context_keys;

impl Editor {
    /// Update all menu context values based on current editor state.
    /// This should be called before rendering the menu bar.
    pub fn update_menu_context(&mut self) {
        // Simple state lookups
        let line_numbers = self.is_line_numbers_visible();
        let line_wrap = self.is_line_wrap_enabled();
        let compose_mode = self.is_compose_mode();
        let file_explorer_visible = self.file_explorer_visible;
        let file_explorer_focused = self.is_file_explorer_focused();
        let mouse_capture = self.mouse_enabled;
        let mouse_hover = self.config.editor.mouse_hover_enabled;
        let inlay_hints = self.config.editor.enable_inlay_hints;
        let has_selection = self.has_active_selection();
        let menu_bar = self.menu_bar_visible;
        let vertical_scrollbar = self.config.editor.show_vertical_scrollbar;
        let horizontal_scrollbar = self.config.editor.show_horizontal_scrollbar;

        // File explorer state
        let show_hidden = self.is_file_explorer_showing_hidden();
        let show_gitignored = self.is_file_explorer_showing_gitignored();

        // Language-dependent context values
        let lsp_available = self.is_lsp_available();
        let formatter_available = self.is_formatter_available();

        // Session mode (for detach command availability)
        let session_mode = self.session_mode;

        // Apply all context values
        self.menu_state
            .context
            .set(context_keys::LINE_NUMBERS, line_numbers)
            .set(context_keys::LINE_WRAP, line_wrap)
            .set(context_keys::COMPOSE_MODE, compose_mode)
            .set(context_keys::FILE_EXPLORER, file_explorer_visible)
            .set(context_keys::FILE_EXPLORER_FOCUSED, file_explorer_focused)
            .set(context_keys::MOUSE_CAPTURE, mouse_capture)
            .set(context_keys::MOUSE_HOVER, mouse_hover)
            .set(context_keys::INLAY_HINTS, inlay_hints)
            .set(context_keys::LSP_AVAILABLE, lsp_available)
            .set(context_keys::FILE_EXPLORER_SHOW_HIDDEN, show_hidden)
            .set(context_keys::FILE_EXPLORER_SHOW_GITIGNORED, show_gitignored)
            .set(context_keys::HAS_SELECTION, has_selection)
            .set(context_keys::MENU_BAR, menu_bar)
            .set(context_keys::FORMATTER_AVAILABLE, formatter_available)
            .set(context_keys::SESSION_MODE, session_mode)
            .set(context_keys::VERTICAL_SCROLLBAR, vertical_scrollbar)
            .set(context_keys::HORIZONTAL_SCROLLBAR, horizontal_scrollbar);
    }

    /// Check if line numbers are visible in the active split.
    fn is_line_numbers_visible(&self) -> bool {
        let active_split = self.split_manager.active_split();
        self.split_view_states
            .get(&active_split)
            .map(|vs| vs.show_line_numbers)
            .unwrap_or(true)
    }

    /// Check if line wrap is enabled in the active split.
    fn is_line_wrap_enabled(&self) -> bool {
        let active_split = self.split_manager.active_split();
        self.split_view_states
            .get(&active_split)
            .map(|vs| vs.viewport.line_wrap_enabled)
            .unwrap_or(false)
    }

    /// Check if compose mode is active in the current buffer.
    fn is_compose_mode(&self) -> bool {
        let active_split = self.split_manager.active_split();
        self.split_view_states
            .get(&active_split)
            .map(|vs| vs.view_mode == crate::state::ViewMode::Compose)
            .unwrap_or(false)
    }

    /// Check if the file explorer is currently focused.
    fn is_file_explorer_focused(&self) -> bool {
        self.key_context == crate::input::keybindings::KeyContext::FileExplorer
    }

    /// Check if the file explorer is showing hidden files.
    fn is_file_explorer_showing_hidden(&self) -> bool {
        self.file_explorer
            .as_ref()
            .map(|fe| fe.ignore_patterns().show_hidden())
            .unwrap_or(false)
    }

    /// Check if the file explorer is showing gitignored files.
    fn is_file_explorer_showing_gitignored(&self) -> bool {
        self.file_explorer
            .as_ref()
            .map(|fe| fe.ignore_patterns().show_gitignored())
            .unwrap_or(false)
    }

    /// Check if an LSP server is available and ready for the current buffer's language.
    fn is_lsp_available(&self) -> bool {
        let buffer_id = self.active_buffer();

        // Check if LSP is enabled for this buffer
        if let Some(metadata) = self.buffer_metadata.get(&buffer_id) {
            if !metadata.lsp_enabled {
                return false;
            }
        } else {
            return false;
        }

        // Use buffer's stored language
        self.buffers
            .get(&buffer_id)
            .and_then(|state| {
                self.lsp
                    .as_ref()
                    .map(|lsp| lsp.is_server_ready(&state.language))
            })
            .unwrap_or(false)
    }

    /// Check if a formatter is configured for the current buffer's language.
    fn is_formatter_available(&self) -> bool {
        let buffer_id = self.active_buffer();

        // Use buffer's stored language
        self.buffers
            .get(&buffer_id)
            .and_then(|state| {
                self.config
                    .languages
                    .get(&state.language)
                    .and_then(|lc| lc.formatter.as_ref())
                    .map(|_| true)
            })
            .unwrap_or(false)
    }
}
