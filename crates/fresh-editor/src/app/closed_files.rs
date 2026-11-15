//! Recent Files, per window.
//!
//! Each window (workspace) remembers the files the user closed in it, most
//! recent first, so "Recent Files" offers this workspace's files rather
//! than an editor-wide history. A file is recorded when the user closes its
//! tab (`Editor::close_buffer_internal` with `CloseCause::User`; closes the
//! editor makes on its own are not recorded) and is listed only while it is
//! not open again in the window. The list is persisted with the rest of the
//! workspace.
//!
//! Not covered yet: a launch that keeps no workspace (`fresh FILE`,
//! `--simple`, `--diff`) records into its window like any other, but nothing
//! persists it, so those files are gone after quitting. A possible follow-up:
//! hand each such file, on close and at quit, to the workspace whose root is
//! the nearest ancestor of the file, through a small side file that workspace
//! merges in (never by rewriting its workspace file, which a running
//! orchestrator owns); files under no workspace would be dropped.

use fresh_editor_core::model::host_path::AsHostPath;
use std::collections::HashSet;
use std::io;
use std::path::{Path, PathBuf};

use fresh_i18n::t;

use super::path_utils::ExplorerRoot;
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

    /// Forget `path` and everything under it (it was deleted).
    pub(crate) fn remove_under(&mut self, path: &Path) {
        self.paths.retain(|p| !p.starts_with(path));
    }

    /// The paths, most recently closed first.
    pub(crate) fn paths(&self) -> &[PathBuf] {
        &self.paths
    }

    /// The paths, most recently closed first, relative to `root` where they
    /// lie under it and absolute otherwise — how they are shown and saved.
    ///
    /// A recorded path is spelled the way its buffer resolved it, which need
    /// not be how the root is spelled: a root reached through a symlink,
    /// macOS's `/var` (really `/private/var`), Windows' `\\?\` prefix.
    /// `ExplorerRoot` matches the two either way.
    pub(crate) fn relative_to(&self, root: &Path) -> Vec<PathBuf> {
        let root_match = ExplorerRoot::new(root);
        self.paths
            .iter()
            .map(|path| {
                root_match
                    .admit(path)
                    .and_then(|under| under.strip_prefix(root).ok().map(Path::to_path_buf))
                    .filter(|relative| !relative.as_os_str().is_empty())
                    .unwrap_or_else(|| path.clone())
            })
            .collect()
    }
}

impl Editor {
    /// Record a buffer of the active window that the user is closing, if it
    /// is a file with a tab. Hidden, virtual (help, terminals, results) and
    /// unnamed buffers have nothing to reopen; a preview tab was only looked
    /// at.
    pub(crate) fn record_closed_file(&mut self, id: BufferId) {
        let window = self.active_window_mut();
        if window.is_buffer_preview(id) {
            return;
        }
        let Some(path) = window
            .tab_file_buffers()
            .find(|(buffer_id, _)| *buffer_id == id)
            .map(|(_, path)| path.to_path_buf())
        else {
            return;
        };
        window.closed_files.push(path);
    }

    /// Open the prompt listing the active window's closed files that are not
    /// open again.
    pub(crate) fn start_recent_files_prompt(&mut self) {
        let window = self.active_window();
        let open: HashSet<&Path> = window.tab_file_buffers().map(|(_, path)| path).collect();
        let closed = &window.closed_files;
        let suggestions: Vec<Suggestion> = closed
            .paths()
            .iter()
            .zip(closed.relative_to(&window.root))
            .filter(|(path, _)| !open.contains(path.as_path()))
            .map(|(path, shown)| {
                let full = path.to_string_lossy().into_owned();
                let shown = shown.to_string_lossy().into_owned();
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
            t!("file.recent_files_prompt").to_string(),
            PromptType::RecentFiles,
            suggestions,
        ));
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.select_suggestion(Some(0));
        }
    }

    /// Reopen the file picked in the Recent Files prompt. A file that
    /// is gone is dropped from the list rather than opened as a new empty
    /// buffer; one that can't be reached right now keeps its place.
    pub(crate) fn open_recent_file(&mut self, input: &str) {
        let path = PathBuf::from(input);
        // Only what the list offered: typed text that matched nothing is not
        // a path to open.
        if !self.active_window().closed_files.paths().contains(&path) {
            return;
        }
        let window = self.active_window();
        // A remote workspace not connected yet runs on a local placeholder,
        // which can't say anything about the remote file.
        let unconnected = window.authority_spec.is_remote()
            && window
                .authority()
                .filesystem
                .remote_connection_info()
                .is_none();
        let found = if unconnected {
            Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "the workspace is not connected",
            ))
        } else {
            window
                .authority()
                .filesystem
                .metadata(path.as_host())
                .map(|_| ())
        };
        match found {
            Ok(()) => {
                if let Err(e) = self.open_file(&path) {
                    self.set_status_message(
                        t!("file.error_opening", error = e.to_string()).to_string(),
                    );
                }
            }
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                self.active_window_mut().closed_files.remove(&path);
                self.set_status_message(
                    t!(
                        "status.closed_file_missing",
                        path = path.display().to_string()
                    )
                    .to_string(),
                );
            }
            Err(e) => {
                self.set_status_message(
                    t!("file.error_opening", error = e.to_string()).to_string(),
                );
            }
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
    fn remove_under_forgets_a_deleted_directory() {
        let mut closed = ClosedFiles::from_paths([p("/d/a"), p("/d/sub/b"), p("/dx"), p("/e")]);
        closed.remove_under(&p("/d"));
        assert_eq!(closed.paths(), &[p("/dx"), p("/e")]);
    }

    #[test]
    fn remove_forgets_path() {
        let mut closed = ClosedFiles::from_paths([p("/a"), p("/b")]);
        closed.remove(&p("/a"));
        assert_eq!(closed.paths(), &[p("/b")]);
    }
}
