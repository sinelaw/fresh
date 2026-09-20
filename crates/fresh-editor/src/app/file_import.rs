//! Window-owned import queue and its explicit terminal-drop prompt.

use super::Editor;
use crate::input::keybindings::KeyContext;
use crate::model::filesystem::StdFileSystem;
use crate::services::file_import::{import_file, parse_paths};
use crate::view::confirm::{Choice, Confirm, Tone};
use crate::view::prompt::PromptType;
use fresh_i18n::t;
use std::collections::VecDeque;
use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{mpsc, Arc};

#[derive(Debug)]
pub(crate) struct FileImport {
    directory: PathBuf,
    pending: VecDeque<PathBuf>,
    current: Option<(PathBuf, PathBuf)>,
    job: Option<ImportJob>,
    completed: usize,
    skipped: usize,
    cancelled: bool,
}

#[derive(Debug)]
struct ImportJob {
    cancel: Arc<AtomicBool>,
    bytes: Arc<AtomicU64>,
    total: Arc<AtomicU64>,
    result: mpsc::Receiver<io::Result<()>>,
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
        let directory = self
            .file_explorer()
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
            .unwrap_or_else(|| self.working_dir().to_path_buf());
        self.start_prompt(
            t!(
                "explorer.import_prompt",
                directory = directory.display().to_string()
            )
            .to_string(),
            PromptType::FileImportPaths { directory },
        );
    }

    pub(crate) fn confirm_file_import_paths(&mut self, input: &str, directory: PathBuf) {
        match parse_paths(input) {
            Ok(paths) => {
                self.active_window_mut().file_import = Some(FileImport {
                    directory,
                    pending: paths.into(),
                    current: None,
                    job: None,
                    completed: 0,
                    skipped: 0,
                    cancelled: false,
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
        if let Some(source) = batch.pending.pop_front() {
            let Some(name) = source.file_name() else {
                self.finish_file_import(Some("source path has no filename".into()));
                return;
            };
            batch.current = Some((source.clone(), batch.directory.join(name)));
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
                let result = import_file(
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
        let (source, destination) = batch.current.clone().unwrap();
        let cancelled = batch.cancelled;
        match result {
            Ok(()) => {
                batch.completed += 1;
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
                self.refresh_tree_after_paste(&source, &destination, false);
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
            batch.current.as_mut().unwrap().1 = batch.directory.join(name);
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
            let summary = t!(
                "explorer.import_done",
                count = batch.completed,
                skipped = batch.skipped
            )
            .to_string();
            let status = if let Some(error) = error {
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
