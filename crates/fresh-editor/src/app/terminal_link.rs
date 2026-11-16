//! Ctrl+Click / Ctrl+hover to open file paths printed in the embedded terminal.
//!
//! When the focused terminal is in live mode, a Ctrl+Click on text that
//! resolves to a real file opens it in Fresh, jumping to any `:line:col` the
//! text encoded. Ctrl+hover underlines such a path to signal it's clickable.
//! The Ctrl gesture is reserved for links even while a full-screen or
//! mouse-reporting program (vim/less/htop, or anything that enabled DECSET
//! 1000/1002/1003) runs — it is withheld from the PTY rather than forwarded,
//! so links work everywhere, not just at a bare shell prompt.
//!
//! Relative paths are resolved against, in order: the terminal's working
//! directory as reported via OSC 7 (tracks `cd`), then Fresh's working
//! directory. Resolution and the existence check go through the editor's
//! [`FileSystem`] so it works transparently on remote (SSH) hosts.
//!
//! [`FileSystem`]: crate::model::filesystem::FileSystem

use crate::app::Editor;
use anyhow::Result as AnyhowResult;
use crossterm::event::{KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use fresh_editor_core::model::host_path::AsHostPath;
use std::path::{Path, PathBuf};

impl Editor {
    /// If this is a Ctrl+Left-click over a resolvable path in the live
    /// terminal, open the file and return `Some(Ok(true))` (event handled).
    /// Returns `None` to let normal mouse handling proceed.
    pub(crate) fn try_open_terminal_link(
        &mut self,
        col: u16,
        row: u16,
        mouse_event: MouseEvent,
    ) -> Option<AnyhowResult<bool>> {
        if !matches!(mouse_event.kind, MouseEventKind::Down(MouseButton::Left))
            || !mouse_event.modifiers.contains(KeyModifiers::CONTROL)
        {
            return None;
        }

        // Try the live grid first, then the scrollback buffer view (only one
        // of the two is showing for any given terminal).
        let (link, term_cwd) = self
            .terminal_pane_at(col, row)
            .and_then(|at| self.active_window().detect_terminal_link_at(col, row, at))
            .map(|(_, _, link, cwd)| (link, cwd))
            .or_else(|| {
                let at = self.pane_content_at(col, row)?;
                self.active_window()
                    .detect_terminal_scrollback_link_at(col, row, at)
                    .map(|(_, link, cwd)| (link, cwd))
            })?;
        let resolved = self.resolve_terminal_path(&link.path, term_cwd.as_deref())?;

        // Clear the hover highlight now that we're acting on it.
        self.active_window_mut().terminal_link_hover = None;

        Some(
            self.handle_open_file_at_location(resolved, link.line, link.column)
                .map(|()| true),
        )
    }

    /// Update the Ctrl+hover path-link highlight for the live terminal.
    ///
    /// Returns true if the highlighted span changed (a re-render is needed).
    /// The highlight is shown only while Ctrl is held over a path that
    /// resolves to a real file.
    pub(crate) fn update_terminal_link_hover(
        &mut self,
        col: u16,
        row: u16,
        modifiers: KeyModifiers,
    ) -> bool {
        let new_hover = if modifiers.contains(KeyModifiers::CONTROL) {
            self.compute_terminal_link_hover(col, row)
        } else {
            None
        };
        if self.active_window().terminal_link_hover != new_hover {
            self.active_window_mut().terminal_link_hover = new_hover;
            true
        } else {
            false
        }
    }

    /// Compute the hover highlight (buffer + grid row + column span) for a path
    /// link at the given screen position, if it resolves to a real file.
    fn compute_terminal_link_hover(
        &self,
        col: u16,
        row: u16,
    ) -> Option<crate::app::window::TerminalLinkHover> {
        let at = self.terminal_pane_at(col, row)?;
        let (buffer_id, term_row, link, term_cwd) =
            self.active_window().detect_terminal_link_at(col, row, at)?;
        // Only highlight paths that actually resolve — otherwise the underline
        // would promise a link that clicking can't honor.
        self.resolve_terminal_path(&link.path, term_cwd.as_deref())?;
        Some(crate::app::window::TerminalLinkHover {
            buffer_id,
            row: term_row,
            cols: link.range,
        })
    }

    /// Resolve a path printed by a terminal program to an existing file.
    ///
    /// Tries, in order: the path as-is if absolute (after `~` expansion), then
    /// joined against the terminal's OSC 7 cwd, then against Fresh's working
    /// directory. Returns the first candidate that exists and is a regular
    /// file. Directories and non-existent paths yield `None` (so the link is
    /// inert).
    fn resolve_terminal_path(&self, raw: &str, term_cwd: Option<&Path>) -> Option<PathBuf> {
        // The terminal runs on the host, so the path it printed is the host's
        // and every question about it belongs to the host's filesystem: where
        // `~` is, whether the path is already rooted, and how to join it onto
        // a base. Asking `std::path` asked the client.
        let fs = &self.authority().filesystem;
        let expanded = fs.expand_tilde(raw);

        let candidates: Vec<PathBuf> = if fs.at(&expanded).is_absolute() {
            vec![expanded.into_path_buf()]
        } else {
            let relative = expanded.to_wire_string();
            let mut v = Vec::new();
            if let Some(cwd) = term_cwd {
                v.push(fs.at(cwd.as_host()).join_rel(&relative).into_path_buf());
            }
            v.push(
                fs.at(self.working_dir().as_host())
                    .join_rel(&relative)
                    .into_path_buf(),
            );
            v
        };

        candidates
            .into_iter()
            .find(|p| fs.is_file(p.as_host()).unwrap_or(false))
    }
}
