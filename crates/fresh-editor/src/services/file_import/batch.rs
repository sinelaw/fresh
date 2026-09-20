//! One bounded worker per import batch. Only conflicts require a UI reply.
use super::{check_cancel, import_entry_with_buffer, ImportOutcome};
use crate::model::filesystem::FileSystem;
use std::collections::VecDeque;
use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{mpsc, Arc, Mutex};

#[derive(Debug)]
pub enum ImportEvent {
    Imported {
        destination: PathBuf,
        directory: bool,
        skipped: usize,
    },
    Skipped,
    Conflict(PathBuf),
    Finished(io::Result<()>),
}

#[derive(Debug)]
pub enum ImportDecision {
    Overwrite,
    Skip,
    Rename(String),
    Cancel,
}

#[derive(Debug, Default, Clone)]
pub struct ImportProgress {
    pub destination: PathBuf,
    pub bytes: u64,
    pub total: u64,
}

#[derive(Debug)]
pub struct ImportWorker {
    pub events: mpsc::Receiver<ImportEvent>,
    decisions: mpsc::Sender<ImportDecision>,
    cancel: Arc<AtomicBool>,
    progress: Arc<Mutex<ImportProgress>>,
}

impl ImportWorker {
    pub fn start(
        source_fs: Arc<dyn FileSystem>,
        destination_fs: Arc<dyn FileSystem>,
        mut pending: VecDeque<(PathBuf, PathBuf)>,
    ) -> io::Result<Self> {
        // Backpressure bounds both completed-path memory and the amount of work
        // that can get ahead of an inactive owning window.
        let (sender, events) = mpsc::sync_channel(16);
        let (decisions, replies) = mpsc::channel();
        let cancel = Arc::new(AtomicBool::new(false));
        let progress = Arc::new(Mutex::new(ImportProgress {
            destination: pending
                .front()
                .map(|(_, destination)| destination.clone())
                .unwrap_or_default(),
            ..ImportProgress::default()
        }));
        let worker = Self {
            events,
            decisions,
            cancel: cancel.clone(),
            progress: progress.clone(),
        };
        std::thread::Builder::new()
            .name("file-import".into())
            .spawn(move || {
                let mut buffer = Vec::new();
                let result = (|| {
                    while let Some((source, mut destination)) = pending.pop_front() {
                        let mut overwrite = false;
                        loop {
                            check_cancel(&cancel)?;
                            *progress.lock().unwrap() = ImportProgress {
                                destination: destination.clone(),
                                bytes: 0,
                                total: 0,
                            };
                            let result = import_entry_with_buffer(
                                source_fs.as_ref(),
                                destination_fs.as_ref(),
                                &source,
                                &destination,
                                overwrite,
                                &cancel,
                                |bytes, total| {
                                    let mut progress = progress.lock().unwrap();
                                    progress.bytes = bytes;
                                    progress.total = total;
                                },
                                &mut buffer,
                            );
                            match result {
                                Ok(outcome) => {
                                    let (directory, skipped) = match outcome {
                                        ImportOutcome::File => (false, 0),
                                        ImportOutcome::Directory { children, skipped } => {
                                            for child in children.into_iter().rev() {
                                                let target =
                                                    destination.join(child.file_name().unwrap());
                                                pending.push_front((child, target));
                                            }
                                            (true, skipped)
                                        }
                                    };
                                    sender
                                        .send(ImportEvent::Imported {
                                            destination,
                                            directory,
                                            skipped,
                                        })
                                        .map_err(|_| {
                                            io::Error::new(
                                                io::ErrorKind::Interrupted,
                                                "import window closed",
                                            )
                                        })?;
                                    break;
                                }
                                Err(error) if error.kind() == io::ErrorKind::AlreadyExists => {
                                    check_cancel(&cancel)?;
                                    sender
                                        .send(ImportEvent::Conflict(destination.clone()))
                                        .map_err(|_| {
                                            io::Error::new(
                                                io::ErrorKind::Interrupted,
                                                "import window closed",
                                            )
                                        })?;
                                    match replies.recv() {
                                        Ok(ImportDecision::Overwrite) => overwrite = true,
                                        Ok(ImportDecision::Rename(name)) => {
                                            if name.is_empty()
                                                || name == "."
                                                || name == ".."
                                                || name.contains(['/', '\\', ':', '\0'])
                                            {
                                                return Err(io::Error::new(
                                                    io::ErrorKind::InvalidInput,
                                                    "enter a single filename",
                                                ));
                                            }
                                            destination = destination.parent().unwrap().join(name);
                                            overwrite = false;
                                        }
                                        Ok(ImportDecision::Skip) => {
                                            sender.send(ImportEvent::Skipped).map_err(|_| {
                                                io::Error::new(
                                                    io::ErrorKind::Interrupted,
                                                    "import window closed",
                                                )
                                            })?;
                                            break;
                                        }
                                        Ok(ImportDecision::Cancel) | Err(_) => {
                                            return Err(io::Error::new(
                                                io::ErrorKind::Interrupted,
                                                "file import cancelled",
                                            ))
                                        }
                                    }
                                }
                                Err(error) => {
                                    return Err(io::Error::new(
                                        error.kind(),
                                        format!(
                                            "{} → {}: {error}",
                                            source.display(),
                                            destination.display()
                                        ),
                                    ))
                                }
                            }
                        }
                    }
                    Ok(())
                })();
                // Dropping the receiver releases a worker blocked on backpressure.
                drop(sender.send(ImportEvent::Finished(result)));
            })?;
        Ok(worker)
    }

    pub fn progress(&self) -> ImportProgress {
        self.progress.lock().unwrap().clone()
    }
    pub fn decide(&self, decision: ImportDecision) -> io::Result<()> {
        self.decisions
            .send(decision)
            .map_err(|_| io::Error::other("file import worker stopped"))
    }
    pub fn cancel(&self) {
        self.cancel.store(true, Ordering::Release);
        // Wake a worker waiting at a conflict. A finished worker needs no reply.
        drop(self.decisions.send(ImportDecision::Cancel));
    }
}

impl Drop for ImportWorker {
    fn drop(&mut self) {
        self.cancel();
    }
}
