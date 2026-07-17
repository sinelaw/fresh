//! Recent / pinned projects actions (issue #1895).
//!
//! Fresh implements a project switch as a *restart* against the new working
//! directory, so the only instrumentation point needed is
//! [`Editor::record_recent_project_open`], called once by the real run loops
//! after the editor boots against a directory. That single hook captures both
//! the initial launch and every subsequent switch.

use std::path::{Path, PathBuf};

use rust_i18n::t;

use super::Editor;
use crate::input::commands::Suggestion;
use crate::services::recent_projects::{now_secs, RecentProjects};
use crate::view::prompt::PromptType;

impl Editor {
    /// Absolute path of the data directory backing the recent-projects store.
    fn recent_projects_data_dir(&self) -> PathBuf {
        self.dir_context().data_dir.clone()
    }

    /// Record the current working directory as a recently opened project.
    ///
    /// Called from the real boot points (TUI `main` loop and the session
    /// server) after the editor is constructed against its working directory.
    /// Best-effort: a failure to persist must never disrupt startup, so errors
    /// are logged and swallowed.
    pub fn record_recent_project_open(&self) {
        let data_dir = self.recent_projects_data_dir();
        let project = self.working_dir().to_path_buf();

        let mut store = RecentProjects::load(&data_dir);
        store.record_open(&project, now_secs());
        if let Err(e) = store.save(&data_dir) {
            tracing::warn!("Failed to persist recent projects: {}", e);
        }
    }

    /// Open the "Open Recent Project" picker, listing pinned projects first
    /// (marked with a star) then recent ones, excluding the current project.
    pub(crate) fn start_open_recent_project_prompt(&mut self) {
        let data_dir = self.recent_projects_data_dir();
        let store = RecentProjects::load(&data_dir);
        let current = self.working_dir().to_path_buf();

        let suggestions: Vec<Suggestion> = store
            .ordered()
            .into_iter()
            .filter(|entry| entry.path != current)
            .map(|entry| {
                let path_str = entry.path.display().to_string();
                let name = display_name(&entry.path);
                // The full path lives in the description so the user can filter
                // on any path fragment (the picker matches description too).
                let description = if entry.pinned {
                    format!("★ {}", path_str)
                } else {
                    path_str.clone()
                };
                Suggestion {
                    text: name,
                    description: Some(description),
                    description_spans: None,
                    value: Some(path_str),
                    disabled: false,
                    keybinding: None,
                    source: None,
                }
            })
            .collect();

        if suggestions.is_empty() {
            self.set_status_message(t!("recent_projects.empty").to_string());
            return;
        }

        self.start_prompt_with_suggestions(
            t!("recent_projects.prompt").to_string(),
            PromptType::OpenRecentProject,
            suggestions,
        );
    }

    /// Confirm handler for the recent-project picker: switch to the selected
    /// project directory (reusing the Switch Project code path).
    pub(super) fn handle_open_recent_project(&mut self, input: &str) {
        let path = PathBuf::from(input);
        if path.is_dir() {
            self.change_working_dir(path);
        } else {
            self.set_status_message(
                t!("file.not_directory", path = path.display().to_string()).to_string(),
            );
        }
    }

    /// Toggle whether the current project is pinned, persisting the change.
    pub(crate) fn toggle_pin_current_project(&mut self) {
        let data_dir = self.recent_projects_data_dir();
        let project = self.working_dir().to_path_buf();

        let mut store = RecentProjects::load(&data_dir);
        let pinned = store.toggle_pinned(&project, now_secs());
        if let Err(e) = store.save(&data_dir) {
            tracing::warn!("Failed to persist recent projects: {}", e);
        }

        let path = project.display().to_string();
        let message = if pinned {
            t!("recent_projects.pinned_status", path = path)
        } else {
            t!("recent_projects.unpinned_status", path = path)
        };
        self.set_status_message(message.to_string());
    }
}

/// A short, human-friendly label for a project root — its final path component,
/// falling back to the full path for filesystem roots without one.
fn display_name(path: &Path) -> String {
    path.file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| path.display().to_string())
}
