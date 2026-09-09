//! What the active buffer can actually be asked to do.
//!
//! Menu items and command-palette entries both need the same answer: is there
//! anything here to save, revert, or edit? They used to ask different, looser
//! questions — the menus asked only "is there a buffer at all" (`has_buffer`),
//! which is true for a terminal, for a plugin panel with no file behind it, and
//! for a buffer with nothing unsaved in it. So Save was offered when there was
//! nothing to write, and Cut/Paste when the target refuses edits.
//!
//! Computing it once here keeps the two surfaces from drifting apart, and keeps
//! each flag next to the reason it exists.

use super::Editor;

/// What the active buffer supports right now.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BufferCapabilities {
    /// A real, user-visible buffer — not the synthesized placeholder the close
    /// path keeps alive when `auto_create_empty_buffer_on_last_buffer_close`
    /// is off.
    pub has_buffer: bool,
    /// A buffer holding the user's own text: not a terminal (whose "file" is
    /// the scrollback transcript the terminal itself writes) and not a virtual
    /// plugin panel (theme editor, tour, git log — no file behind them).
    pub is_text_buffer: bool,
    /// `is_text_buffer` and the buffer accepts edits — the same
    /// `editing_disabled` flag the action handlers consult before mutating, so
    /// read-only buffers and panels stay out.
    pub editable: bool,
    /// The active buffer has unsaved changes. Without them Save has nothing to
    /// write.
    pub modified: bool,
    /// The active buffer is backed by a file, so it can be reverted or reloaded
    /// (a never-saved scratch buffer cannot).
    pub has_path: bool,
    /// Some buffer in the window has unsaved changes — what Save All acts on.
    pub any_modified: bool,
}

impl BufferCapabilities {
    /// Save writes the active buffer's own text: it needs one, with something
    /// unsaved in it.
    pub fn can_save(&self) -> bool {
        self.is_text_buffer && self.modified
    }

    /// Save As writes the text somewhere new, so unlike Save it doesn't need
    /// pending changes — but it still needs text that is the user's to write.
    pub fn can_save_as(&self) -> bool {
        self.is_text_buffer
    }

    /// Revert and "reload with encoding" re-read the file behind the buffer,
    /// so there has to be one.
    pub fn can_revert(&self) -> bool {
        self.is_text_buffer && self.has_path
    }
}

impl Editor {
    /// Capabilities of the buffer the user is looking at.
    pub fn buffer_capabilities(&self) -> BufferCapabilities {
        let buffer_id = self.active_buffer();
        let window = self.active_window();

        let meta = window.buffer_metadata.get(&buffer_id);
        let has_buffer = !meta.map(|m| m.synthetic_placeholder).unwrap_or(false);
        let is_virtual = meta.map(|m| m.is_virtual()).unwrap_or(false);
        let is_text_buffer = has_buffer && !is_virtual && !window.is_terminal_buffer(buffer_id);

        let state = window.buffers.get(&buffer_id);
        let modified = state.map(|s| s.buffer.is_modified()).unwrap_or(false);
        let has_path = state
            .map(|s| s.buffer.file_path().is_some())
            .unwrap_or(false);
        let editable = is_text_buffer && !window.is_editing_disabled();
        let any_modified = window.buffers.iter().any(|(_, s)| s.buffer.is_modified());

        BufferCapabilities {
            has_buffer,
            is_text_buffer,
            editable,
            modified,
            has_path,
            any_modified,
        }
    }
}
