//! What Universal Search looks in besides a workspace's files on disk: its
//! terminals' screens, its open buffers and — for a workspace listed but
//! not opened yet this session — its unsaved edits in the recovery store.
//! Backs the plugin API's `getTerminalScreens` (the active window) and
//! `getWorkspaceSearchSources` (any window), the latter for Live Grep's
//! "All workspaces" toggle.

use fresh_core::api::{RecoveredFile, TerminalScreen, WorkspaceBuffer, WorkspaceSearchSources};
use fresh_core::WindowId;

use super::window::Window;
use super::Editor;

/// A recovered file larger than this is left out of a search: its whole text
/// crosses to the plugin on every query.
const MAX_RECOVERED_SEARCH_BYTES: u64 = 4 * 1024 * 1024;

impl Window {
    /// The visible screen of each of this window's terminals, read from
    /// memory.
    pub(crate) fn terminal_screens(&self) -> Vec<TerminalScreen> {
        self.terminal_buffers
            .iter()
            .filter_map(|(buffer_id, info)| {
                let handle = self.terminal_manager.get(info.terminal_id)?;
                let state = handle.state.lock().ok()?;
                Some(TerminalScreen {
                    buffer_id: buffer_id.0 as u64,
                    lines: state.screen_lines(),
                    file: self
                        .terminal_backing_files
                        .get(&info.terminal_id)
                        .map(|p| p.to_string_lossy().into_owned())
                        .unwrap_or_default(),
                    name: self
                        .buffer_metadata
                        .get(buffer_id)
                        .map(|m| m.display_name.clone())
                        .unwrap_or_default(),
                })
            })
            .collect()
    }

    /// This window's file buffers that have a tab.
    fn workspace_buffers(&self) -> Vec<WorkspaceBuffer> {
        self.tab_file_buffers()
            .filter_map(|(id, path)| {
                Some(WorkspaceBuffer {
                    buffer_id: id.0 as u64,
                    path: path.to_string_lossy().into_owned(),
                    modified: self.buffers.get(&id)?.buffer.is_modified(),
                })
            })
            .collect()
    }
}

impl Editor {
    /// What window `id` has to search besides its files on disk, or `None`
    /// for an unknown window.
    pub(crate) fn workspace_search_sources(&self, id: WindowId) -> Option<WorkspaceSearchSources> {
        let window = self.windows.get(&id)?;
        let loaded = !self.materialize_pending.contains(&id);
        let terminals = window.terminal_screens();
        // The window's own directory first; a terminal started with another
        // cwd keeps its backing file under that cwd's directory instead.
        let mut terminal_dirs = vec![self.dir_context().terminal_dir_for(&window.root)];
        for screen in &terminals {
            let Some(dir) = std::path::Path::new(&screen.file).parent() else {
                continue;
            };
            if !screen.file.is_empty() && !terminal_dirs.iter().any(|d| d == dir) {
                terminal_dirs.push(dir.to_path_buf());
            }
        }
        Some(WorkspaceSearchSources {
            loaded,
            terminal_dirs: terminal_dirs
                .iter()
                .map(|d| d.to_string_lossy().into_owned())
                .collect(),
            terminals,
            buffers: window.workspace_buffers(),
            // An opened window's unsaved work is in its buffers: it claimed
            // its entries when it was opened.
            recovered: if loaded {
                Vec::new()
            } else {
                self.recovered_files_for(window)
            },
        })
    }

    /// The unsaved edits the recovery store keeps for files of `window`'s
    /// workspace, as opening it would bring them back: entries no running
    /// editor holds, on this machine, last edited in a window on its root.
    fn recovered_files_for(&self, window: &Window) -> Vec<RecoveredFile> {
        // A remote workspace's files are not on this machine.
        if window.host_key().is_some() {
            return Vec::new();
        }
        let root = window
            .root
            .canonicalize()
            .unwrap_or_else(|_| window.root.clone());
        let service = self.recovery_service.lock().unwrap();
        service
            .list_recoverable(None, Some(None))
            .into_iter()
            .filter(|entry| entry.metadata.root.as_deref() == Some(root.as_path()))
            .filter(|entry| entry.metadata.content_size <= MAX_RECOVERED_SEARCH_BYTES)
            .filter_map(|entry| {
                let path = entry.metadata.original_path.clone()?;
                match service.read_content(&entry) {
                    Ok(Some(content)) => Some(RecoveredFile {
                        path: path.to_string_lossy().into_owned(),
                        text: String::from_utf8_lossy(&content).into_owned(),
                    }),
                    Ok(None) => None,
                    Err(e) => {
                        tracing::debug!(
                            "Leaving recovery entry {} out of the search: {e}",
                            entry.id
                        );
                        None
                    }
                }
            })
            .collect()
    }
}
