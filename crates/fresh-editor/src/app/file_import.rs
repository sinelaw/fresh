//! Window-owned import queue for explorer drops and the manual import prompt.

use super::Editor;
use crate::input::keybindings::KeyContext;
use crate::model::filesystem::{DirEntry, StdFileSystem};
use crate::services::file_import::batch::{ImportDecision, ImportEvent, ImportWorker};
use crate::services::file_import::parse_paths;
use crate::view::confirm::{Choice, Confirm, Tone};
use crate::view::prompt::PromptType;
use fresh_i18n::t;
use std::collections::{BTreeSet, VecDeque};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc};
use std::time::SystemTime;

#[derive(Debug)]
pub(crate) struct FileImport {
    directory: PathBuf,
    current: Option<PathBuf>,
    job: Option<ImportWorker>,
    completed: usize,
    folders: usize,
    changed_directories: BTreeSet<PathBuf>,
    skipped: usize,
    cancelled: bool,
    last_destination: Option<PathBuf>,
    refresh: Option<mpsc::Receiver<ImportRefresh>>,
    refresh_cancel: Arc<AtomicBool>,
    error: Option<String>,
}

impl Drop for FileImport {
    fn drop(&mut self) {
        // Stop a multi-directory refresh when its owning window is closed.
        self.refresh_cancel.store(true, Ordering::Release);
    }
}

type ImportRefresh = io::Result<Option<(PathBuf, ImportDirectory)>>;

#[derive(Debug)]
struct ImportDirectory {
    entries: Vec<DirEntry>,
    modified: Option<SystemTime>,
    gitignore: Option<(Vec<u8>, Option<SystemTime>)>,
}

impl Editor {
    pub(crate) fn start_file_import(&mut self) {
        if self.active_window().file_import.is_some() {
            return;
        }
        let directory = self.file_import_directory();
        self.start_prompt(
            t!(
                "explorer.import_prompt",
                directory = directory.display().to_string()
            )
            .to_string(),
            PromptType::FileImportPaths { directory },
        );
    }

    fn file_import_directory(&self) -> PathBuf {
        self.file_explorer()
            .and_then(|explorer| explorer.get_selected_entry())
            .map(|entry| {
                if entry.is_dir() {
                    entry.path.clone()
                } else {
                    entry
                        .path
                        .parent()
                        .unwrap_or(self.working_dir())
                        .to_path_buf()
                }
            })
            .unwrap_or_else(|| self.working_dir().to_path_buf())
    }

    /// Terminal drops have no pointer coordinates. The focused explorer's
    /// selection supplies the destination, just as for its paste command.
    pub(crate) fn import_dropped_files(&mut self, input: &str) {
        self.confirm_file_import_paths(input, self.file_import_directory());
    }

    pub(crate) fn confirm_file_import_paths(&mut self, input: &str, directory: PathBuf) {
        if self.active_window().file_import.is_some() {
            return;
        }
        match parse_paths(input) {
            Ok(paths) => {
                let mut pending = VecDeque::new();
                for source in paths {
                    let Some(name) = source.file_name() else {
                        self.set_status_message(
                            t!(
                                "explorer.error_copying",
                                error = "source path has no filename"
                            )
                            .to_string(),
                        );
                        return;
                    };
                    let destination = directory.join(name);
                    pending.push_back((source, destination));
                }
                let job = match ImportWorker::start(
                    Arc::new(StdFileSystem),
                    self.authority().filesystem.clone(),
                    pending,
                ) {
                    Ok(job) => job,
                    Err(error) => {
                        self.set_status_message(
                            t!("explorer.error_copying", error = error.to_string()).to_string(),
                        );
                        return;
                    }
                };
                self.active_window_mut().file_import = Some(FileImport {
                    directory,
                    current: None,
                    job: Some(job),
                    completed: 0,
                    folders: 0,
                    changed_directories: BTreeSet::new(),
                    skipped: 0,
                    cancelled: false,
                    last_destination: None,
                    refresh: None,
                    refresh_cancel: Arc::new(AtomicBool::new(false)),
                    error: None,
                });
                self.show_file_import_progress();
            }
            Err(error) => self.set_status_message(
                t!("explorer.error_copying", error = error.to_string()).to_string(),
            ),
        }
    }

    fn show_file_import_progress(&mut self) {
        let confirm = Confirm::new(
            t!("cmd.explorer_import").into_owned(),
            self.file_import_progress(),
            vec![super::confirm_dialog::cancel()],
        );
        self.start_confirm_prompt(
            confirm.body.clone(),
            PromptType::FileImportProgress,
            confirm,
        );
    }

    fn file_import_progress(&self) -> String {
        let batch = self.active_window().file_import.as_ref().unwrap();
        let job = batch.job.as_ref().unwrap();
        let progress = job.progress();
        let name = progress
            .destination
            .file_name()
            .unwrap_or_default()
            .to_string_lossy();
        t!(
            "explorer.import_progress",
            name = name,
            bytes = progress.bytes,
            total = progress.total
        )
        .to_string()
    }

    /// Drain a bounded number of completions; transfers proceed independently
    /// of UI ticks until they need a conflict decision or reach backpressure.
    pub(crate) fn poll_file_import(&mut self) -> bool {
        if self
            .active_window()
            .file_import
            .as_ref()
            .is_some_and(|batch| batch.refresh.is_some())
        {
            return self.poll_file_import_refresh();
        }
        let mut changed = false;
        for _ in 0..16 {
            let Some(batch) = self.active_window().file_import.as_ref() else {
                return changed;
            };
            let Some(job) = &batch.job else {
                return changed;
            };
            let event = match job.events.try_recv() {
                Ok(event) => event,
                Err(mpsc::TryRecvError::Empty) => break,
                Err(mpsc::TryRecvError::Disconnected) => {
                    ImportEvent::Finished(Err(io::Error::other("file import worker stopped")))
                }
            };
            changed = true;
            match event {
                ImportEvent::Imported {
                    destination,
                    directory,
                    skipped,
                } => {
                    // Only directories already loaded in this window need a
                    // refresh. New/collapsed subtrees are read on expansion.
                    let refresh_paths: Vec<_> = [
                        destination.parent(),
                        directory.then_some(destination.as_path()),
                    ]
                    .into_iter()
                    .flatten()
                    .filter(|path| {
                        self.active_window()
                            .file_import
                            .as_ref()
                            .is_some_and(|batch| batch.directory == *path)
                            || self
                                .file_explorer()
                                .and_then(|e| e.tree().get_node_by_path(path))
                                .is_some_and(|node| node.is_expanded())
                    })
                    .map(Path::to_path_buf)
                    .collect();
                    let batch = self.active_window_mut().file_import.as_mut().unwrap();
                    batch.last_destination = Some(destination.clone());
                    batch.changed_directories.extend(refresh_paths);
                    batch.skipped += skipped;
                    if directory {
                        batch.folders += 1;
                    } else {
                        batch.completed += 1;
                    }
                    // Preserve the existing buffer/cursor/LSP reload path.
                    if !directory
                        && self.buffers().iter().any(|(id, state)| {
                            state.buffer.file_path() == Some(destination.as_path())
                                && !state.buffer.is_modified()
                                && self.active_window().buffer_auto_revert_enabled(*id)
                        })
                    {
                        self.file_mod_times_mut().remove(&destination);
                        self.handle_file_changed(&destination.to_string_lossy());
                    }
                }
                ImportEvent::Skipped => {
                    self.active_window_mut()
                        .file_import
                        .as_mut()
                        .unwrap()
                        .skipped += 1
                }
                ImportEvent::Conflict(destination) => {
                    let batch = self.active_window_mut().file_import.as_mut().unwrap();
                    if !batch.cancelled {
                        batch.current = Some(destination);
                        self.ask_file_import_conflict();
                        return true;
                    }
                }
                ImportEvent::Finished(result) => {
                    let cancelled = self.active_window().file_import.as_ref().unwrap().cancelled;
                    let error = result
                        .err()
                        .filter(|error| !(cancelled && error.kind() == io::ErrorKind::Interrupted))
                        .map(|error| error.to_string());
                    self.finish_file_import(error);
                    return true;
                }
            }
        }
        if self
            .active_window()
            .file_import
            .as_ref()
            .is_some_and(|batch| batch.job.is_some() && batch.current.is_none())
        {
            let body = self.file_import_progress();
            if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
                if matches!(prompt.prompt_type, PromptType::FileImportProgress) {
                    if let Some(confirm) = &mut prompt.confirm {
                        if confirm.body != body {
                            confirm.body = body;
                            changed = true;
                        }
                    }
                }
            }
        }
        changed
    }

    fn ask_file_import_conflict(&mut self) {
        let destination = &self
            .active_window()
            .file_import
            .as_ref()
            .unwrap()
            .current
            .as_ref()
            .unwrap();
        let name = super::file_explorer::truncate_name_for_prompt(
            &destination
                .file_name()
                .unwrap_or_default()
                .to_string_lossy(),
            40,
        );
        let confirm = Confirm::new(
            t!("dialog.title.name_conflict").into_owned(),
            t!("explorer.paste_conflict", name = name).into_owned(),
            vec![
                Choice::new(
                    t!("dialog.btn.overwrite").into_owned(),
                    "o",
                    Tone::Destructive,
                ),
                Choice::new(t!("dialog.btn.skip").into_owned(), "s", Tone::Safe),
                Choice::new(t!("dialog.btn.rename").into_owned(), "r", Tone::Safe),
                super::confirm_dialog::cancel(),
            ],
        );
        self.start_confirm_prompt(
            confirm.body.clone(),
            PromptType::FileImportConflict,
            confirm,
        );
    }

    pub(crate) fn confirm_file_import_conflict(&mut self, input: &str) {
        if self.active_window().file_import.is_none() {
            return;
        }
        match input {
            "o" => {
                let destination = self
                    .active_window()
                    .file_import
                    .as_ref()
                    .unwrap()
                    .current
                    .as_ref()
                    .unwrap();
                if self.buffers().iter().any(|(_, state)| {
                    state.buffer.file_path() == Some(destination.as_path())
                        && state.buffer.is_modified()
                }) {
                    self.active_window_mut().file_import.as_mut().unwrap().error =
                        Some("destination has unsaved changes in an open buffer".into());
                    self.cancel_file_import();
                } else {
                    self.resume_file_import(ImportDecision::Overwrite);
                }
            }
            "s" => self.resume_file_import(ImportDecision::Skip),
            "r" => {
                let name = self
                    .active_window()
                    .file_import
                    .as_ref()
                    .unwrap()
                    .current
                    .as_ref()
                    .unwrap()
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .into_owned();
                self.start_prompt_with_initial_text(
                    t!("explorer.paste_rename_prompt").to_string(),
                    PromptType::FileImportRename,
                    name,
                );
            }
            _ => self.cancel_file_import(),
        }
    }

    pub(crate) fn rename_file_import(&mut self, name: &str) {
        // One filename only, on both local and remote operating systems.
        if name.is_empty() || name == "." || name == ".." || name.contains(['/', '\\', ':', '\0']) {
            self.finish_file_import(Some("enter a single filename".into()));
            return;
        }
        self.resume_file_import(ImportDecision::Rename(name.to_owned()));
    }

    fn resume_file_import(&mut self, decision: ImportDecision) {
        let Some(batch) = self.active_window_mut().file_import.as_mut() else {
            return;
        };
        batch.current = None;
        if let Err(error) = batch.job.as_ref().unwrap().decide(decision) {
            self.finish_file_import(Some(error.to_string()));
        } else {
            self.show_file_import_progress();
        }
    }

    pub(crate) fn cancel_file_import(&mut self) {
        self.set_status_message(t!("explorer.paste_cancelled").to_string());
        if let Some(batch) = self.active_window_mut().file_import.as_mut() {
            batch.cancelled = true;
            if let Some(job) = &batch.job {
                job.cancel();
            } else {
                self.finish_file_import(None);
            }
        }
    }

    fn finish_file_import(&mut self, error: Option<String>) {
        let filesystem = Arc::clone(&self.authority().filesystem);
        let Some(batch) = self.active_window_mut().file_import.as_mut() else {
            return;
        };
        if batch.refresh.is_some() {
            return;
        }
        batch.job = None;
        if error.is_some() {
            batch.error = error;
        }
        if batch.last_destination.is_none() {
            self.complete_file_import();
            return;
        }
        let directories = std::mem::take(&mut batch.changed_directories);
        let cancel = Arc::clone(&batch.refresh_cancel);
        // At most one listing waits in the channel, rather than collecting all
        // directory snapshots before the editor can consume any of them.
        let (sender, receiver) = mpsc::sync_channel(1);
        batch.refresh = Some(receiver);
        let spawn = std::thread::Builder::new()
            .name("import-refresh".into())
            .spawn(move || {
                for directory in directories {
                    if cancel.load(Ordering::Acquire) {
                        return;
                    }
                    let result = (|| {
                        let modified = filesystem
                            .metadata(&directory)
                            .ok()
                            .and_then(|m| m.modified);
                        let mut entries = filesystem.read_dir(&directory)?;
                        for entry in &mut entries {
                            if entry.metadata.is_none() {
                                entry.metadata = filesystem.metadata(&entry.path).ok();
                            }
                        }
                        let gitignore = entries
                            .iter()
                            .find(|entry| entry.name == ".gitignore")
                            .and_then(|entry| {
                                filesystem.read_file(&entry.path).ok().map(|bytes| {
                                    (bytes, entry.metadata.as_ref().and_then(|m| m.modified))
                                })
                            });
                        Ok(Some((
                            directory,
                            ImportDirectory {
                                entries,
                                modified,
                                gitignore,
                            },
                        )))
                    })();
                    let failed = result.is_err();
                    if sender.send(result).is_err() || failed {
                        return;
                    }
                }
                drop(sender.send(Ok(None)));
            });
        if let Err(error) = spawn {
            self.record_import_refresh_error(error);
            self.complete_file_import();
        }
    }

    /// Suppress watcher-triggered re-listing of the same directory while the
    /// batch owns its refresh. Other directories continue to be watched.
    pub(super) fn file_import_refresh_pending(&self, path: &Path) -> bool {
        self.active_window()
            .file_import
            .as_ref()
            .is_some_and(|batch| path.starts_with(&batch.directory))
    }

    fn record_import_refresh_error(&mut self, error: io::Error) {
        let batch = self.active_window_mut().file_import.as_mut().unwrap();
        let message = format!("could not refresh {}: {error}", batch.directory.display());
        batch.error = Some(match batch.error.take() {
            Some(previous) => format!("{previous}; {message}"),
            None => message,
        });
    }

    fn poll_file_import_refresh(&mut self) -> bool {
        let batch = self.active_window().file_import.as_ref().unwrap();
        let result = match batch.refresh.as_ref().unwrap().try_recv() {
            Ok(result) => result,
            Err(mpsc::TryRecvError::Empty) => return false,
            Err(mpsc::TryRecvError::Disconnected) => {
                Err(io::Error::other("import refresh worker stopped"))
            }
        };
        let directory = batch.directory.clone();
        let destination = batch.last_destination.clone().unwrap();
        match result {
            Ok(Some((path, snapshot))) => {
                let window = self.active_window_mut();
                window.pending_dir_poll_rx = None;
                if let Some(modified) = snapshot.modified {
                    window.dir_mod_times.insert(path.clone(), modified);
                }
                if let Some(explorer) = window.file_explorer.as_mut() {
                    if let Some((bytes, modified)) = snapshot.gitignore {
                        explorer
                            .ignore_patterns_mut()
                            .load_gitignore_from_bytes(&path, &bytes, modified);
                    } else if !snapshot
                        .entries
                        .iter()
                        .any(|entry| entry.name == ".gitignore")
                    {
                        explorer.ignore_patterns_mut().remove_gitignore(&path);
                    }
                    // Recheck the owner: a folder may have been collapsed while
                    // its listing was in flight. Never reopen it from a snapshot.
                    if path == directory
                        || explorer
                            .tree()
                            .get_node_by_path(&path)
                            .is_some_and(|node| node.is_expanded())
                    {
                        explorer
                            .tree_mut()
                            .reconcile_directory(&path, snapshot.entries);
                    }
                }
                window.rebuild_file_explorer_decoration_cache();
                window.rebuild_file_explorer_slot_override_cache();
                return true;
            }
            Ok(None) => {}
            Err(error) => self.record_import_refresh_error(error),
        }
        if let Some(explorer) = self.active_window_mut().file_explorer.as_mut() {
            explorer.clear_multi_selection();
            explorer.navigate_to_path(&directory);
            // Prefer the imported file, or its nearest loaded ancestor. Do not
            // load a new subtree just to select its final leaf.
            for path in destination.ancestors() {
                if explorer.tree().get_node_by_path(path).is_some() {
                    explorer.navigate_to_path(path);
                    break;
                }
            }
        }
        // Plugins (including git decorations) rescan once per batch. Open
        // buffers/LSP keep their per-file reload above to respect dirty edits.
        self.notify_file_explorer_change(&directory);
        self.complete_file_import();
        true
    }

    fn complete_file_import(&mut self) {
        if let Some(batch) = self.active_window_mut().file_import.take() {
            if self.active_window().prompt.as_ref().is_some_and(|prompt| {
                matches!(
                    prompt.prompt_type,
                    PromptType::FileImportProgress
                        | PromptType::FileImportConflict
                        | PromptType::FileImportRename
                )
            }) {
                self.drop_prompt();
            }
            let summary = if batch.folders == 0 {
                t!(
                    "explorer.import_done",
                    count = batch.completed,
                    skipped = batch.skipped
                )
            } else {
                t!(
                    "explorer.import_done_with_folders",
                    count = batch.completed,
                    folders = batch.folders,
                    skipped = batch.skipped
                )
            }
            .to_string();
            let status = if let Some(error) = batch.error.as_ref() {
                format!(
                    "{summary} — {}",
                    t!("explorer.error_copying", error = error)
                )
            } else if batch.cancelled {
                format!("{summary} — {}", t!("explorer.paste_cancelled"))
            } else {
                summary
            };
            self.set_status_message(status);
            self.active_window_mut().key_context = if self.file_explorer_visible() {
                KeyContext::FileExplorer
            } else {
                KeyContext::Normal
            };
        }
    }
}
