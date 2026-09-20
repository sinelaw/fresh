//! Window-owned import queue for explorer drops and the manual import prompt.

use super::Editor;
use crate::input::keybindings::KeyContext;
use crate::model::filesystem::{DirEntry, StdFileSystem};
use crate::services::file_import::{import_entry, parse_paths, ImportOutcome};
use crate::view::confirm::{Choice, Confirm, Tone};
use crate::view::prompt::PromptType;
use fresh_i18n::t;
use std::collections::{BTreeSet, VecDeque};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{mpsc, Arc};
use std::time::SystemTime;

#[derive(Debug)]
pub(crate) struct FileImport {
    directory: PathBuf,
    pending: VecDeque<(PathBuf, PathBuf)>,
    current: Option<(PathBuf, PathBuf)>,
    job: Option<ImportJob>,
    completed: usize,
    folders: usize,
    changed_directories: BTreeSet<PathBuf>,
    skipped: usize,
    cancelled: bool,
    last_destination: Option<PathBuf>,
    refresh: Option<mpsc::Receiver<io::Result<ImportRefresh>>>,
    refresh_cancel: Arc<AtomicBool>,
    error: Option<String>,
}

impl Drop for FileImport {
    fn drop(&mut self) {
        // Stop a multi-directory refresh when its owning window is closed.
        self.refresh_cancel.store(true, Ordering::Release);
    }
}

type ImportRefresh = Vec<(PathBuf, ImportDirectory)>;

#[derive(Debug)]
struct ImportDirectory {
    entries: Vec<DirEntry>,
    modified: Option<SystemTime>,
    gitignore: Option<(Vec<u8>, Option<SystemTime>)>,
}

#[derive(Debug)]
struct ImportJob {
    cancel: Arc<AtomicBool>,
    bytes: Arc<AtomicU64>,
    total: Arc<AtomicU64>,
    result: mpsc::Receiver<io::Result<ImportOutcome>>,
}

impl Drop for ImportJob {
    fn drop(&mut self) {
        // Closing a window/editor stops the worker at the next chunk, too.
        self.cancel.store(true, Ordering::Release);
    }
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
                self.active_window_mut().file_import = Some(FileImport {
                    directory,
                    pending,
                    current: None,
                    job: None,
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
                self.advance_file_import();
            }
            Err(error) => self.set_status_message(
                t!("explorer.error_copying", error = error.to_string()).to_string(),
            ),
        }
    }

    fn advance_file_import(&mut self) {
        let Some(batch) = self.active_window_mut().file_import.as_mut() else {
            return;
        };
        if let Some(entry) = batch.pending.pop_front() {
            batch.current = Some(entry);
            self.run_file_import(false);
        } else {
            self.finish_file_import(None);
        }
    }

    fn run_file_import(&mut self, overwrite: bool) {
        let filesystem = Arc::clone(&self.authority().filesystem);
        let Some(batch) = self.active_window().file_import.as_ref() else {
            return;
        };
        let Some((source, destination)) = batch.current.clone() else {
            return;
        };
        if overwrite
            && self.buffers().iter().any(|(_, state)| {
                state.buffer.file_path() == Some(destination.as_path())
                    && state.buffer.is_modified()
            })
        {
            self.finish_file_import(Some(
                "destination has unsaved changes in an open buffer".into(),
            ));
            return;
        }
        let cancel = Arc::new(AtomicBool::new(false));
        let bytes = Arc::new(AtomicU64::new(0));
        let total = Arc::new(AtomicU64::new(0));
        let (sender, result) = mpsc::channel();
        let job = ImportJob {
            cancel: cancel.clone(),
            bytes: bytes.clone(),
            total: total.clone(),
            result,
        };
        let spawn = std::thread::Builder::new()
            .name("file-import".into())
            .spawn(move || {
                let result = import_entry(
                    &StdFileSystem,
                    filesystem.as_ref(),
                    &source,
                    &destination,
                    overwrite,
                    &cancel,
                    |copied, size| {
                        total.store(size, Ordering::Relaxed);
                        bytes.store(copied, Ordering::Relaxed);
                    },
                )
                .map_err(|error| {
                    io::Error::new(
                        error.kind(),
                        format!("{} → {}: {error}", source.display(), destination.display()),
                    )
                });
                // A closed window has dropped the receiver; cleanup already ran.
                drop(sender.send(result));
            });
        if let Err(error) = spawn {
            self.finish_file_import(Some(error.to_string()));
            return;
        }
        self.active_window_mut().file_import.as_mut().unwrap().job = Some(job);
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
        let name = batch
            .current
            .as_ref()
            .unwrap()
            .1
            .file_name()
            .unwrap_or_default()
            .to_string_lossy();
        t!(
            "explorer.import_progress",
            name = name,
            bytes = job.bytes.load(Ordering::Relaxed),
            total = job.total.load(Ordering::Relaxed)
        )
        .to_string()
    }

    /// Poll only the owning window. An inactive window retains its completion
    /// until selected; it cannot open a conflict prompt in another workspace.
    pub(crate) fn poll_file_import(&mut self) -> bool {
        if self
            .active_window()
            .file_import
            .as_ref()
            .is_some_and(|batch| batch.refresh.is_some())
        {
            return self.poll_file_import_refresh();
        }
        let Some(batch) = self.active_window_mut().file_import.as_mut() else {
            return false;
        };
        let Some(job) = &batch.job else {
            return false;
        };
        let result = match job.result.try_recv() {
            Ok(result) => result,
            Err(mpsc::TryRecvError::Empty) => {
                let body = self.file_import_progress();
                if let Some(prompt) = self.active_window_mut().prompt.as_mut() {
                    if matches!(prompt.prompt_type, PromptType::FileImportProgress) {
                        if let Some(confirm) = &mut prompt.confirm {
                            if confirm.body != body {
                                confirm.body = body;
                                return true;
                            }
                        }
                    }
                }
                return false;
            }
            Err(mpsc::TryRecvError::Disconnected) => {
                Err(io::Error::other("file import worker stopped"))
            }
        };
        batch.job = None;
        let (_, destination) = batch.current.clone().unwrap();
        let cancelled = batch.cancelled;
        match result {
            Ok(outcome) => {
                batch.last_destination = Some(destination.clone());
                if let Some(parent) = destination.parent() {
                    batch.changed_directories.insert(parent.to_path_buf());
                }
                match outcome {
                    ImportOutcome::File => batch.completed += 1,
                    ImportOutcome::Directory { children, skipped } => {
                        batch.folders += 1;
                        batch.skipped += skipped;
                        batch.changed_directories.insert(destination.clone());
                        for source in children.into_iter().rev() {
                            let target = destination.join(source.file_name().unwrap());
                            batch.pending.push_front((source, target));
                        }
                    }
                }
                // Reuse the reload path for cursors, LSP and plugin caches.
                // Remote mtimes can have only second precision.
                if self.buffers().iter().any(|(id, state)| {
                    state.buffer.file_path() == Some(destination.as_path())
                        && !state.buffer.is_modified()
                        && self.active_window().buffer_auto_revert_enabled(*id)
                }) {
                    self.file_mod_times_mut().remove(&destination);
                    self.handle_file_changed(&destination.to_string_lossy());
                }
                if cancelled {
                    self.finish_file_import(None);
                } else {
                    self.advance_file_import();
                }
            }
            Err(error) if error.kind() == io::ErrorKind::AlreadyExists && !cancelled => {
                self.ask_file_import_conflict()
            }
            Err(error) if error.kind() == io::ErrorKind::Interrupted && cancelled => {
                self.finish_file_import(None)
            }
            Err(error) => self.finish_file_import(Some(error.to_string())),
        }
        true
    }

    fn ask_file_import_conflict(&mut self) {
        let destination = &self
            .active_window()
            .file_import
            .as_ref()
            .unwrap()
            .current
            .as_ref()
            .unwrap()
            .1;
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
            "o" => self.run_file_import(true),
            "s" => {
                self.active_window_mut()
                    .file_import
                    .as_mut()
                    .unwrap()
                    .skipped += 1;
                self.advance_file_import();
            }
            "r" => {
                let name = self
                    .active_window()
                    .file_import
                    .as_ref()
                    .unwrap()
                    .current
                    .as_ref()
                    .unwrap()
                    .1
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
        if let Some(batch) = self.active_window_mut().file_import.as_mut() {
            let destination = &mut batch.current.as_mut().unwrap().1;
            *destination = destination.parent().unwrap().join(name);
            self.run_file_import(false);
        }
    }

    pub(crate) fn cancel_file_import(&mut self) {
        self.set_status_message(t!("explorer.paste_cancelled").to_string());
        if let Some(batch) = self.active_window_mut().file_import.as_mut() {
            batch.cancelled = true;
            if let Some(job) = &batch.job {
                job.cancel.store(true, Ordering::Release);
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
        batch.error = error;
        if batch.last_destination.is_none() {
            self.complete_file_import();
            return;
        }
        let directories = batch.changed_directories.clone();
        let cancel = Arc::clone(&batch.refresh_cancel);
        let (sender, receiver) = mpsc::channel();
        batch.refresh = Some(receiver);
        // One listing per affected directory at the end of the batch, including
        // partial success. Never block the editor on remote directory I/O.
        let spawn = std::thread::Builder::new()
            .name("import-refresh".into())
            .spawn(move || {
                let result = directories
                    .into_iter()
                    .map(|directory| {
                        if cancel.load(Ordering::Acquire) {
                            return Err(io::Error::new(
                                io::ErrorKind::Interrupted,
                                "import refresh cancelled",
                            ));
                        }
                        // Sample before listing: changes during/after it remain visible
                        // to the ordinary watcher instead of being acknowledged away.
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
                        Ok((
                            directory,
                            ImportDirectory {
                                entries,
                                modified,
                                gitignore,
                            },
                        ))
                    })
                    .collect();
                // The owning window may have been closed while the read was running.
                drop(sender.send(result));
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
            .is_some_and(|batch| {
                batch.directory == path
                    || batch.changed_directories.contains(path)
                    || batch.current.as_ref().is_some_and(|(_, destination)| {
                        destination == path || destination.parent() == Some(path)
                    })
            })
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
            Ok(snapshots) => {
                let window = self.active_window_mut();
                // Discard any older poll snapshot before installing these stamps.
                window.pending_dir_poll_rx = None;
                for (directory, snapshot) in snapshots {
                    if let Some(modified) = snapshot.modified {
                        window.dir_mod_times.insert(directory.clone(), modified);
                    }
                    if let Some(explorer) = window.file_explorer.as_mut() {
                        if let Some((bytes, modified)) = snapshot.gitignore {
                            explorer
                                .ignore_patterns_mut()
                                .load_gitignore_from_bytes(&directory, &bytes, modified);
                        } else if !snapshot
                            .entries
                            .iter()
                            .any(|entry| entry.name == ".gitignore")
                        {
                            explorer.ignore_patterns_mut().remove_gitignore(&directory);
                        }
                        explorer
                            .tree_mut()
                            .reconcile_directory(&directory, snapshot.entries);
                    }
                }
                if let Some(explorer) = window.file_explorer.as_mut() {
                    explorer.clear_multi_selection();
                    explorer.navigate_to_path(&directory);
                    explorer.navigate_to_path(&destination);
                }
                window.rebuild_file_explorer_decoration_cache();
                window.rebuild_file_explorer_slot_override_cache();
            }
            Err(error) => self.record_import_refresh_error(error),
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
