//! "Open With…": open a file as text, in one of the plugin file openers
//! that handle its type (`registerFileOpener`), or in the system's default
//! app, and optionally remember the choice in the `file_openers` setting.
//!
//! Reached from the file explorer's and the tab bar's context menus, and
//! from the command palette for the active tab's file.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use fresh_i18n::t;

use super::file_open_orchestrators::{OpenKind, OpenerRequest};
use super::Editor;
use crate::input::commands::Suggestion;
use crate::view::prompt::PromptType;
use fresh_core::file_openers::{extension_key, TEXT_OPENER};

/// Opens a file in the system's default app. The editor's is
/// [`default_system_opener`]; tests substitute one that launches nothing.
pub type SystemOpener = Arc<dyn Fn(&Path) -> Result<(), String> + Send + Sync>;

/// `open::that`, where the editor is built with it.
pub fn default_system_opener() -> SystemOpener {
    Arc::new(|path: &Path| {
        #[cfg(feature = "runtime")]
        {
            open::that(path).map_err(|e| e.to_string())
        }
        #[cfg(not(feature = "runtime"))]
        {
            let _ = path;
            Err("not available in this build".to_string())
        }
    })
}

/// What a suggestion of the "Open With…" prompt does, as its `value`.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Choice {
    Open(Target),
    /// Open with the target, and remember it for the file's extension.
    Always(Target),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Target {
    Text,
    Plugin(String),
    System,
}

const SYSTEM: &str = "system";
const ALWAYS: &str = "always:";
const PLUGIN: &str = "opener:";

impl Target {
    fn encode(&self) -> String {
        match self {
            Self::Text => TEXT_OPENER.to_string(),
            Self::Plugin(id) => format!("{PLUGIN}{id}"),
            Self::System => SYSTEM.to_string(),
        }
    }

    fn decode(value: &str) -> Option<Self> {
        match value {
            TEXT_OPENER => Some(Self::Text),
            SYSTEM => Some(Self::System),
            _ => value
                .strip_prefix(PLUGIN)
                .map(|id| Self::Plugin(id.to_string())),
        }
    }
}

impl Choice {
    fn encode(&self) -> String {
        match self {
            Self::Open(target) => target.encode(),
            Self::Always(target) => format!("{ALWAYS}{}", target.encode()),
        }
    }

    fn decode(value: &str) -> Option<Self> {
        match value.strip_prefix(ALWAYS) {
            Some(rest) => Target::decode(rest).map(Self::Always),
            None => Target::decode(value).map(Self::Open),
        }
    }
}

impl Editor {
    /// Replace the function that opens files in the system's default app.
    #[doc(hidden)]
    pub fn set_system_opener(&mut self, opener: SystemOpener) {
        self.system_opener = opener;
    }

    /// The file `buffer_id` (in the active window) shows: its file, or the
    /// file a plugin's file opener shows in it.
    pub(crate) fn file_of_buffer(
        &self,
        buffer_id: crate::model::event::BufferId,
    ) -> Option<PathBuf> {
        let window = self.active_window();
        let meta = window.buffer_metadata.get(&buffer_id)?;
        if let Some(viewer) = &meta.viewer {
            return Some(viewer.path.clone());
        }
        meta.file_path()
            .filter(|p| !p.as_os_str().is_empty())
            .cloned()
    }

    /// "Open With…" for the file selected in the file explorer.
    pub(crate) fn file_explorer_open_with(&mut self) {
        let path = self.file_explorer().and_then(|explorer| {
            let id = explorer.get_selected()?;
            let node = explorer.tree().get_node(id)?;
            (!node.entry.is_dir()).then(|| node.entry.path.clone().into_path_buf())
        });
        match path {
            Some(path) => self.start_open_with_prompt(path),
            None => self.set_status_message(t!("open_with.no_file").to_string()),
        }
    }

    /// "Open With…" for the active tab's file (the palette command).
    pub(crate) fn open_with_active_file(&mut self) {
        match self.file_of_buffer(self.active_buffer()) {
            Some(path) => self.start_open_with_prompt(path),
            None => self.set_status_message(t!("open_with.no_file").to_string()),
        }
    }

    /// Offer the ways `path` can open: as text, with each plugin file opener
    /// that handles its type, and with the system's default app (for a file
    /// on this machine). Each comes with an "always" twin that also
    /// remembers the choice for the extension, except the system app, which
    /// opens outside the editor.
    pub(crate) fn start_open_with_prompt(&mut self, path: PathBuf) {
        let extension = extension_key(&path);
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.display().to_string());

        let mut targets: Vec<(Target, String, bool)> = Vec::new();
        let configured = extension
            .as_ref()
            .and_then(|ext| self.config.file_openers.get(ext).cloned());
        {
            let registry = self.plugin_manager.read().unwrap().file_openers();
            let registry = registry.read().unwrap();
            let default_id = registry
                .resolve(&path, configured.as_deref())
                .map(|o| o.id.clone());
            targets.push((
                Target::Text,
                t!("open_with.text_editor").to_string(),
                default_id.is_none(),
            ));
            let mut openers = registry.openers_for(&path);
            // The setting can pick an opener that does not list the type.
            if let Some(chosen) = default_id.as_deref().and_then(|id| registry.get(id)) {
                if !openers.iter().any(|o| o.id == chosen.id) {
                    openers.push(chosen);
                }
            }
            for opener in openers {
                targets.push((
                    Target::Plugin(opener.id.clone()),
                    opener.label.clone(),
                    default_id.as_deref() == Some(opener.id.as_str()),
                ));
            }
        }
        let local = self.active_window().host_key().is_none();
        let suggestion = |choice: Choice, text: String, is_default: bool| Suggestion {
            id: choice.encode(),
            text,
            description: is_default.then(|| t!("open_with.default").to_string()),
            description_spans: None,
            value: Some(choice.encode()),
            disabled: false,
            keybinding: None,
            source: None,
        };
        let mut suggestions: Vec<Suggestion> = targets
            .iter()
            .map(|(target, label, is_default)| {
                suggestion(Choice::Open(target.clone()), label.clone(), *is_default)
            })
            .collect();
        if local {
            suggestions.push(suggestion(
                Choice::Open(Target::System),
                t!("open_with.system_app").to_string(),
                false,
            ));
        }
        if let Some(ext) = &extension {
            for (target, label, is_default) in &targets {
                if *is_default {
                    continue;
                }
                suggestions.push(suggestion(
                    Choice::Always(target.clone()),
                    t!("open_with.always", ext = ext, opener = label).to_string(),
                    false,
                ));
            }
        }

        self.set_prompt(crate::view::prompt::Prompt::with_suggestions(
            t!("open_with.prompt", name = name).to_string(),
            PromptType::OpenWith { path },
            suggestions,
        ));
        if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
            prompt.select_suggestion(Some(0));
        }
    }

    /// The "Open With…" prompt's confirmed `value`.
    pub(crate) fn handle_open_with(&mut self, path: PathBuf, value: &str) {
        let Some(choice) = Choice::decode(value) else {
            return;
        };
        let target = match choice {
            Choice::Open(target) => target,
            Choice::Always(target) => {
                self.remember_file_opener(&path, &target);
                target
            }
        };
        match target {
            Target::System => self.open_in_system_app(&path),
            Target::Text => self.reopen_as(&path, OpenerRequest::Text),
            Target::Plugin(id) => self.reopen_as(&path, OpenerRequest::Plugin(id)),
        }
    }

    /// Open `path` as `request` asks. A tab already showing the file another
    /// way is closed first, so the file has one tab; one with unsaved edits
    /// is left alone, as closing it would lose them.
    fn reopen_as(&mut self, path: &Path, request: OpenerRequest) {
        let canonical = self.active_window().resolve_open_path(path).canonical_path;
        let window = self.active_window();
        let text_buffer = window
            .buffers
            .iter()
            .find(|(_, state)| state.buffer.file_path() == Some(canonical.as_path()))
            .map(|(id, state)| (*id, state.buffer.is_modified()));
        let viewer = window.viewer_of(&canonical).map(|id| {
            let opener_id = window
                .buffer_metadata
                .get(&id)
                .and_then(|m| m.viewer.as_ref())
                .map(|v| v.opener_id.clone());
            (id, opener_id)
        });
        let open_as_wanted = match &request {
            OpenerRequest::Text => text_buffer.is_some(),
            OpenerRequest::Plugin(id) => viewer
                .as_ref()
                .is_some_and(|(_, o)| o.as_deref() == Some(id)),
            OpenerRequest::Configured => false,
        };
        if !open_as_wanted {
            if let Some((id, modified)) = text_buffer {
                if modified {
                    let name = self
                        .active_window()
                        .buffer_metadata
                        .get(&id)
                        .map(|m| m.display_name.clone())
                        .unwrap_or_default();
                    self.set_status_message(t!("open_with.unsaved", name = name).to_string());
                    return;
                }
                if let Err(e) = self.close_buffer(id) {
                    tracing::warn!("Open With: closing {}: {e}", canonical.display());
                    return;
                }
            }
            if let Some((id, _)) = viewer {
                if let Err(e) = self.close_buffer(id) {
                    tracing::warn!("Open With: closing {}: {e}", canonical.display());
                    return;
                }
            }
        }
        if let Err(e) = self.open_file_as(path, OpenKind::Commit, &request) {
            self.set_status_message(t!("file.error_opening", error = e.to_string()).to_string());
        }
    }

    fn open_in_system_app(&mut self, path: &Path) {
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| path.display().to_string());
        let opener = Arc::clone(&self.system_opener);
        let message = match opener(path) {
            Ok(()) => t!("open_with.opened_in_system", name = name),
            Err(error) => t!("open_with.system_failed", name = name, error = error),
        };
        self.set_status_message(message.to_string());
    }

    /// Remember `target` for `path`'s extension in the `file_openers`
    /// setting: in the config in memory, and in whichever config file
    /// already sets it (the user's, by default).
    fn remember_file_opener(&mut self, path: &Path, target: &Target) {
        let Some(ext) = extension_key(path) else {
            return;
        };
        let value = match target {
            Target::Text => TEXT_OPENER.to_string(),
            Target::Plugin(id) => id.clone(),
            // Opens outside the editor: nothing an open in it can follow.
            Target::System => return,
        };
        self.config_mut()
            .file_openers
            .insert(ext.clone(), value.clone());
        self.sync_windows_config();
        // A JSON pointer escapes `~` and `/`; an extension holds no `/`.
        let key = ext.replace('~', "~0");
        self.persist_config_pointer(
            &format!("/file_openers/{key}"),
            serde_json::Value::String(value),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn choices_round_trip_through_suggestion_values() {
        for choice in [
            Choice::Open(Target::Text),
            Choice::Open(Target::System),
            Choice::Open(Target::Plugin("pdf:view".into())),
            Choice::Always(Target::Text),
            Choice::Always(Target::Plugin("image".into())),
        ] {
            assert_eq!(Choice::decode(&choice.encode()), Some(choice));
        }
        assert_eq!(Choice::decode("nonsense"), None);
    }
}
