//! How a file's tab shows it: as text, or in a plugin's viewer
//! (`openFileView`), with the file kept to one tab.
//!
//! A viewer is a read-only virtual buffer the plugin fills. Its metadata
//! names the file it shows (`BufferMetadata::viewing`), which is what makes
//! it the file's tab: opening the file finds it (`Window::tab_file_buffers`,
//! the already-open check in the open path), a session keeps it as the file,
//! and the explorer reveals the file for it. Being virtual, it is never saved,
//! reverted, recovered or sent to a language server as the file.

use std::path::{Path, PathBuf};

use fresh_core::api::{FileView, OpenFileViewResult, WindowPath};

use super::Editor;
use crate::app::types::{BufferMetadata, FileViewing};
use crate::model::event::BufferId;
use crate::view::split::TabTarget;

/// What a file's tab shows now.
enum Shown {
    Text { modified: bool },
    Viewer { viewer_id: String },
}

impl Editor {
    /// Show the file at `path` as `view` in its tab: in place of the tab it
    /// has, which closes, or in a new tab when it has none.
    pub(crate) fn open_file_view(
        &mut self,
        path: &WindowPath,
        view: FileView,
    ) -> OpenFileViewResult {
        if path.window != self.active_window_id() {
            return OpenFileViewResult::Failed {
                error: format!("window {} is not the active window", path.window.0),
            };
        }
        let file = self
            .active_window()
            .resolve_open_path(Path::new(&path.value))
            .canonical_path;
        // The file's one tab is in another workspace: that one has it.
        if let Some(elsewhere) = self.file_open_in_other_window(&file) {
            return OpenFileViewResult::Failed {
                error: elsewhere.to_string(),
            };
        }

        let old = self.file_tab(&file);
        match (&view, &old) {
            (FileView::Text, Some((id, Shown::Text { .. }))) => {
                return self.go_to_tab(*id);
            }
            (
                FileView::Viewer { viewer_id, .. },
                Some((id, Shown::Viewer { viewer_id: shown })),
            ) if viewer_id == shown => {
                return self.go_to_tab(*id);
            }
            (_, Some((_, Shown::Text { modified: true }))) => {
                return OpenFileViewResult::UnsavedChanges;
            }
            _ => {}
        }

        let new = match view {
            FileView::Text => self.open_file_as_text(&file, old.as_ref().map(|(id, _)| *id)),
            FileView::Viewer { viewer_id, mode } => {
                self.open_file_viewer(&file, viewer_id, mode.unwrap_or_default())
            }
        };
        let new = match new {
            Ok(id) => id,
            Err(error) => return OpenFileViewResult::Failed { error },
        };

        if let Some((old, _)) = old {
            self.active_window_mut().move_tab_into_place_of(new, old);
            // Neither has anything to lose: the text was unmodified, and a
            // viewer's content is the plugin's to make again.
            if let Err(e) = self.force_close_buffer(old) {
                tracing::warn!("openFileView: closing {old:?}: {e}");
            }
        }
        OpenFileViewResult::Opened { buffer_id: new }
    }

    /// The tab of `file` in the active window, and what it shows.
    fn file_tab(&self, file: &Path) -> Option<(BufferId, Shown)> {
        let window = self.active_window();
        let (id, _) = window.tab_file_buffers().find(|(_, f)| *f == file)?;
        let shown = match window
            .buffer_metadata
            .get(&id)
            .and_then(|meta| meta.viewing.as_ref())
        {
            Some(viewing) => Shown::Viewer {
                viewer_id: viewing.viewer_id.clone(),
            },
            None => Shown::Text {
                modified: window
                    .buffers
                    .get(&id)
                    .is_some_and(|state| state.buffer.is_modified()),
            },
        };
        Some((id, shown))
    }

    fn go_to_tab(&mut self, id: BufferId) -> OpenFileViewResult {
        self.set_active_buffer(id);
        OpenFileViewResult::Opened { buffer_id: id }
    }

    /// Open `file` as text, in a new tab. `viewer`, the viewer tab the file
    /// has, is set aside for the open, which would otherwise find it and go
    /// there; it is put back if the open fails.
    fn open_file_as_text(
        &mut self,
        file: &Path,
        viewer: Option<BufferId>,
    ) -> Result<BufferId, String> {
        let set_aside = viewer.and_then(|id| {
            let meta = self.active_window_mut().buffer_metadata.get_mut(&id)?;
            Some((id, meta.viewing.take()?))
        });
        let opened = self.open_file(file);
        if opened.is_err() {
            if let Some((id, viewing)) = set_aside {
                if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&id) {
                    meta.viewing = Some(viewing);
                }
            }
        }
        opened.map_err(|e| e.to_string())
    }

    /// Show `file` in a new tab of the plugin viewer `viewer_id`: a
    /// read-only buffer, without line numbers, that the plugin fills.
    fn open_file_viewer(
        &mut self,
        file: &Path,
        viewer_id: String,
        mode: String,
    ) -> Result<BufferId, String> {
        let window = self.active_window();
        let fs = std::sync::Arc::clone(&window.authority().filesystem);
        let is_file = fs
            .is_file(crate::model::host_path::AsHostPath::as_host(file))
            .unwrap_or(false);
        if !is_file {
            return Err(format!("{} is not a file", file.display()));
        }
        let name = BufferMetadata::display_name_for_path(fs.as_ref(), file, &window.root);
        let id = self
            .active_window_mut()
            .create_virtual_buffer(name, mode, true);
        self.configure_vbuf_display(id, false, true, true, None, true);
        let active_split = self.active_window().split_manager().active_split();
        if let Some(view_state) = self
            .active_window_mut()
            .split_view_states_mut()
            .get_mut(&active_split)
        {
            view_state.ensure_buffer_state(id).show_line_numbers = false;
        }
        if let Some(meta) = self.active_window_mut().buffer_metadata.get_mut(&id) {
            meta.viewing = Some(FileViewing {
                path: PathBuf::from(file),
                viewer_id,
            });
        }
        self.set_active_buffer(id);
        Ok(id)
    }
}

impl crate::app::window::Window {
    /// Put the tab of `new` where `old`'s tab is, in each split that has
    /// both: the file's view changes, its place in the tab bar does not.
    pub(crate) fn move_tab_into_place_of(&mut self, new: BufferId, old: BufferId) {
        for view_state in self.split_view_states_mut().values_mut() {
            let Some(at) = view_state
                .open_buffers
                .iter()
                .position(|tab| *tab == TabTarget::Buffer(old))
            else {
                continue;
            };
            // Only where `new` was opened: a split it is not in has no view
            // of it to show.
            let Some(from) = view_state
                .open_buffers
                .iter()
                .position(|tab| *tab == TabTarget::Buffer(new))
            else {
                continue;
            };
            view_state.open_buffers.remove(from);
            // Taking `new` out from before `old` moved `old` one to the left.
            let at = if from < at { at - 1 } else { at };
            view_state.open_buffers.insert(at, TabTarget::Buffer(new));
        }
    }
}
