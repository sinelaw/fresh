//! Menu context computation.
//!
//! This module provides methods to compute menu context values that determine
//! when menu items and commands should be enabled or disabled. Each context
//! value has a dedicated method that encapsulates the logic for checking
//! whether that feature is available.

use super::Editor;
use crate::view::ui::context_keys;

impl Editor {
    /// Return a clone of the current menu context (boolean state flags).
    ///
    /// This is used by the GUI layer to sync native menu item states
    /// (enabled/disabled, checkmarks) without knowing about the editor's
    /// internal state.
    pub fn menu_context(&self) -> crate::view::ui::MenuContext {
        self.menu_state.context.clone()
    }

    /// Return the fully-expanded menu definitions (with `DynamicSubmenu`
    /// items resolved to `Submenu`).  Used by the GUI layer to build
    /// platform-native menus.
    pub fn expanded_menu_definitions(&self) -> Vec<fresh_core::menu::Menu> {
        use crate::config::{MenuConfig, MenuExt};

        let mut menus = MenuConfig::translated_menus();
        let themes_dir = self.menu_state.themes_dir.clone();
        for menu in &mut menus {
            menu.expand_dynamic_items(&themes_dir);
        }
        menus
    }

    /// Update all menu context values based on current editor state.
    /// This should be called before rendering the menu bar.
    pub fn update_menu_context(&mut self) {
        // Simple state lookups
        let line_numbers = self.is_line_numbers_visible();
        let line_wrap = self.is_line_wrap_enabled();
        let page_view = self.is_page_view();
        let file_explorer_visible = self.file_explorer_visible;
        let file_explorer_focused = self.is_file_explorer_focused();
        let mouse_capture = self.mouse_enabled;
        let mouse_hover = self.config.editor.mouse_hover_enabled;
        let inlay_hints = self.config.editor.enable_inlay_hints;
        // True for any real buffer; false when the active buffer is the
        // synthesized placeholder kept alive after a last-buffer close with
        // `auto_create_empty_buffer_on_last_buffer_close` disabled.
        let has_buffer = !self
            .buffer_metadata
            .get(&self.active_buffer())
            .map(|m| m.synthetic_placeholder)
            .unwrap_or(false);
        let has_selection = has_buffer && self.has_active_selection();
        let can_copy = has_selection
            || file_explorer_focused
            || self
                .file_explorer
                .as_ref()
                .map(|fe| fe.get_selected().is_some())
                .unwrap_or(false);
        // Paste is available in the explorer only when a file is in the clipboard,
        // or in the editor only when no file is in the clipboard. There's no
        // buffer to paste into in placeholder mode, so suppress it there.
        let can_paste = if file_explorer_focused {
            self.file_explorer_clipboard.is_some()
        } else {
            has_buffer && self.file_explorer_clipboard.is_none()
        };
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

        // Scroll sync state
        let scroll_sync = self.same_buffer_scroll_sync;
        let has_same_buffer_splits = self.has_same_buffer_splits();

        // Keybinding map state
        let active_keymap: &str = &self.config.active_keybinding_map;

        // Apply all context values
        self.menu_state
            .context
            .set(context_keys::HAS_BUFFER, has_buffer)
            .set(context_keys::KEYMAP_DEFAULT, active_keymap == "default")
            .set(context_keys::KEYMAP_EMACS, active_keymap == "emacs")
            .set(context_keys::KEYMAP_VSCODE, active_keymap == "vscode")
            .set(context_keys::KEYMAP_MACOS_GUI, active_keymap == "macos-gui")
            .set(context_keys::LINE_NUMBERS, line_numbers)
            .set(context_keys::LINE_WRAP, line_wrap)
            .set(context_keys::PAGE_VIEW, page_view)
            // Keep backward-compatible key for existing keybindings/menus
            .set(context_keys::COMPOSE_MODE, page_view)
            .set(context_keys::FILE_EXPLORER, file_explorer_visible)
            .set(context_keys::FILE_EXPLORER_FOCUSED, file_explorer_focused)
            .set(context_keys::MOUSE_CAPTURE, mouse_capture)
            .set(context_keys::MOUSE_HOVER, mouse_hover)
            .set(context_keys::INLAY_HINTS, inlay_hints)
            .set(context_keys::LSP_AVAILABLE, lsp_available)
            .set(context_keys::FILE_EXPLORER_SHOW_HIDDEN, show_hidden)
            .set(context_keys::FILE_EXPLORER_SHOW_GITIGNORED, show_gitignored)
            .set(context_keys::HAS_SELECTION, has_selection)
            .set(context_keys::CAN_COPY, can_copy)
            .set(context_keys::CAN_PASTE, can_paste)
            .set(context_keys::MENU_BAR, menu_bar)
            .set(context_keys::FORMATTER_AVAILABLE, formatter_available)
            .set(context_keys::SESSION_MODE, session_mode)
            .set(context_keys::VERTICAL_SCROLLBAR, vertical_scrollbar)
            .set(context_keys::HORIZONTAL_SCROLLBAR, horizontal_scrollbar)
            .set(context_keys::SCROLL_SYNC, scroll_sync)
            .set(context_keys::HAS_SAME_BUFFER_SPLITS, has_same_buffer_splits);
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
    fn is_page_view(&self) -> bool {
        let active_split = self.split_manager.active_split();
        self.split_view_states
            .get(&active_split)
            .map(|vs| vs.view_mode == crate::state::ViewMode::PageView)
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

    /// Check if the active buffer is shown in more than one visible split.
    fn has_same_buffer_splits(&self) -> bool {
        let active_split = self.split_manager.active_split();
        let active_buf_id = self.split_manager.buffer_for_split(active_split);
        if let Some(buf_id) = active_buf_id {
            self.split_view_states.keys().any(|&s| {
                s != active_split && self.split_manager.buffer_for_split(s) == Some(buf_id)
            })
        } else {
            false
        }
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
