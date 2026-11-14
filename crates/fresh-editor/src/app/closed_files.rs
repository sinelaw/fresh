//! Reopen Closed File, per window.
//!
//! Each window (workspace) remembers the files closed in it, most recent
//! first, so "Reopen Closed File" offers this workspace's files rather than
//! an editor-wide history. A file is recorded when its buffer is closed
//! (`Editor::close_buffer_internal`, which every close passes through) and
//! is listed only while it is not open again in the window. The list is
//! persisted with the rest of the workspace.

use std::path::{Path, PathBuf};

use fresh_i18n::t;

use super::Editor;
use crate::input::commands::Suggestion;
use crate::model::event::BufferId;
use crate::view::prompt::{Prompt, PromptType};

/// How many closed files a window remembers.
const MAX_CLOSED_FILES: usize = 50;

/// A window's recently closed files, most recent first, without duplicates.
#[derive(Debug, Default, Clone)]
pub(crate) struct ClosedFiles {
    paths: Vec<PathBuf>,
}

impl ClosedFiles {
    /// A list seeded from persisted paths (most recent first).
    pub(crate) fn from_paths(paths: impl IntoIterator<Item = PathBuf>) -> Self {
        let mut closed = Self::default();
        for path in paths {
            if closed.paths.len() == MAX_CLOSED_FILES {
                break;
            }
            if !closed.paths.contains(&path) {
                closed.paths.push(path);
            }
        }
        closed
    }

    /// Record `path` as the most recently closed file.
    pub(crate) fn push(&mut self, path: PathBuf) {
        self.paths.retain(|p| *p != path);
        self.paths.insert(0, path);
        self.paths.truncate(MAX_CLOSED_FILES);
    }

    /// Forget `path` (it no longer exists).
    pub(crate) fn remove(&mut self, path: &Path) {
        self.paths.retain(|p| p != path);
    }

    /// The paths, most recently closed first.
    pub(crate) fn paths(&self) -> &[PathBuf] {
        &self.paths
    }
}

impl Editor {
    /// Record a buffer of the active window that is about to be closed, if
    /// it is a file the user can reopen. Preview tabs, terminals, virtual
    /// and unnamed buffers are not recorded.
    pub(crate) fn record_closed_file(&mut self, id: BufferId) {
        let window = self.active_window_mut();
        if window.is_buffer_preview(id) || window.is_terminal_buffer(id) {
            return;
        }
        let Some(path) = window
            .buffer_metadata
            .get(&id)
            .filter(|meta| !meta.hidden_from_tabs)
            .and_then(|meta| meta.file_path())
            .filter(|path| !path.as_os_str().is_empty())
            .cloned()
        else {
            return;
        };
        window.closed_files.push(path);
    }

    /// Close a preview buffer that is being discarded (replaced by another
    /// preview, or dismissed). It was never really opened, so it does not go
    /// on the closed list — callers have already cleared the preview marker
    /// `record_closed_file` would otherwise recognise it by.
    pub(crate) fn close_discarded_preview(&mut self, id: BufferId) -> anyhow::Result<()> {
        let before = self.active_window().closed_files.clone();
        let result = self.close_buffer(id);
        self.active_window_mut().closed_files = before;
        result
    }

    /// Open the prompt listing the active window's closed files that are not
    /// open again.
    pub(crate) fn start_reopen_closed_file_prompt(&mut self) {
        let window = self.active_window();
        let is_open = |path: &PathBuf| {
            window
                .buffer_metadata
                .values()
                .any(|meta| meta.file_path() == Some(path))
        };
        let suggestions: Vec<Suggestion> = window
            .closed_files
            .paths()
            .iter()
            .filter(|path| !is_open(path))
            .map(|path| {
                let full = path.to_string_lossy().into_owned();
                let shown = path
                    .strip_prefix(&window.root)
                    .unwrap_or(path)
                    .to_string_lossy()
                    .into_owned();
                let mut suggestion = Suggestion::new(full.clone(), shown);
                suggestion.value = Some(full);
                suggestion
            })
            .collect();

        if suggestions.is_empty() {
            self.set_status_message(t!("status.no_closed_files").to_string());
            return;
        }

        self.set_prompt(Prompt::with_suggestions(
            t!("file.reopen_closed_prompt").to_string(),
            PromptType::ReopenClosedFile,
            suggestions,
        ));
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.select_suggestion(Some(0));
        }
    }

    /// Reopen the file picked in the Reopen Closed File prompt. A file that
    /// no longer exists is dropped from the list rather than opened as a new
    /// empty buffer.
    pub(crate) fn reopen_closed_file(&mut self, input: &str) {
        let path = PathBuf::from(input.trim());
        // Only what the list offered: typed text that matched nothing is not
        // a path to open.
        if !self.active_window().closed_files.paths().contains(&path) {
            return;
        }
        if !self.authority().filesystem.exists(&path) {
            self.active_window_mut().closed_files.remove(&path);
            self.set_status_message(
                t!(
                    "status.closed_file_missing",
                    path = path.display().to_string()
                )
                .to_string(),
            );
            return;
        }
        if let Err(e) = self.open_file(&path) {
            self.set_status_message(t!("file.error_opening", error = e.to_string()).to_string());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(s: &str) -> PathBuf {
        PathBuf::from(s)
    }

    #[test]
    fn push_moves_to_front_without_duplicates() {
        let mut closed = ClosedFiles::default();
        closed.push(p("/a"));
        closed.push(p("/b"));
        closed.push(p("/a"));
        assert_eq!(closed.paths(), &[p("/a"), p("/b")]);
    }

    #[test]
    fn list_is_capped_dropping_the_oldest() {
        let mut closed = ClosedFiles::default();
        for i in 0..MAX_CLOSED_FILES + 5 {
            closed.push(p(&format!("/f{i}")));
        }
        assert_eq!(closed.paths().len(), MAX_CLOSED_FILES);
        assert_eq!(closed.paths()[0], p(&format!("/f{}", MAX_CLOSED_FILES + 4)));
        assert!(!closed.paths().contains(&p("/f0")));
    }

    #[test]
    fn from_paths_dedups_keeps_order_and_caps() {
        let closed = ClosedFiles::from_paths([p("/a"), p("/b"), p("/a")]);
        assert_eq!(closed.paths(), &[p("/a"), p("/b")]);

        let many = (0..MAX_CLOSED_FILES + 5).map(|i| p(&format!("/f{i}")));
        assert_eq!(
            ClosedFiles::from_paths(many).paths().len(),
            MAX_CLOSED_FILES
        );
    }

    #[test]
    fn remove_forgets_path() {
        let mut closed = ClosedFiles::from_paths([p("/a"), p("/b")]);
        closed.remove(&p("/a"));
        assert_eq!(closed.paths(), &[p("/b")]);
    }
}
