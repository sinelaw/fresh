//! Off-loop plugin work: the capability-handle side of the plugin command
//! surface.
//!
//! Everything in this module runs on the tokio runtime, never on the editor
//! thread. That is enforced by construction rather than by discipline: the
//! module deliberately does not import `Editor`/`EditorApp`, so a handler
//! written here *cannot* reach editor state, and the only way to affect the
//! UI is to post an [`AsyncMessage`] back to the main loop. Adding a
//! blocking handler to the editor thread is therefore a visible choice —
//! either it lands here with a capability handle, or the frame-budget guard
//! in `plugin_dispatch` names it as the offender.
//!
//! The editor-thread half of each command is limited to bounded state
//! snapshotting (see `handle_grep_project`): collect what only the editor
//! thread can see, hand it to a handler here, return immediately.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use fresh_core::api::{GrepMatch, JsCallbackId, PluginAsyncMessage};
use fresh_core::BufferId;

use crate::model::buffer::HybridSearchPlan;
use crate::model::filesystem::{FileSearchCursor, FileSearchOptions, FileSystem};
use crate::services::async_bridge::AsyncMessage;

/// Capability handle for plugin work that must not run on the editor thread.
///
/// Holds exactly the authority a handler needs — a filesystem, a runtime to
/// run on, and the return path to the main loop. Notably absent: anything
/// that can read or mutate editor state.
pub(crate) struct OffLoop {
    pub filesystem: Arc<dyn FileSystem + Send + Sync>,
    pub runtime: Arc<tokio::runtime::Runtime>,
    pub sender: std::sync::mpsc::Sender<AsyncMessage>,
}

impl OffLoop {
    /// Settle a JS callback from off-loop code. Always call exactly once per
    /// request — a plugin `await`ing the promise hangs forever otherwise.
    fn settle(&self, callback_id: JsCallbackId, result: Result<String, String>) {
        // A send error means the editor is shutting down — nothing left to
        // settle into.
        if self
            .sender
            .send(AsyncMessage::Plugin(PluginAsyncMessage::OffLoopSettled {
                callback_id: callback_id.as_u64(),
                result,
            }))
            .is_err()
        {
            tracing::debug!("off-loop callback dropped: editor gone");
        }
    }
}

/// Editor-thread-collected inputs for a project grep. Everything here is a
/// snapshot: the piece tree is not `Send`, so dirty buffers are captured as
/// `HybridSearchPlan`s while still on the editor thread.
pub(crate) struct GrepProjectRequest {
    pub pattern: String,
    pub opts: FileSearchOptions,
    pub regex: regex::bytes::Regex,
    pub max_results: usize,
    pub root: PathBuf,
    pub ignored_dirs: &'static [&'static str],
    pub callback_id: JsCallbackId,
    /// Open, modified buffers keyed by path — searched through their plan so
    /// unsaved edits are visible to the grep.
    pub dirty_plans: HashMap<PathBuf, (BufferId, HybridSearchPlan)>,
    /// Open, clean buffers keyed by path — attributed to their buffer id so
    /// plugins can jump straight to the buffer, but read from disk.
    pub clean_buffers: HashMap<PathBuf, BufferId>,
    /// Flips when a newer grep from the same plugin supersedes this one, at
    /// which point the request settles as an error rather than reporting the
    /// partial results it had reached.
    pub cancel: Arc<AtomicBool>,
}

/// Walk `root` and grep every file, entirely off the editor thread.
///
/// Concurrency is capped so a plugin looping on `grepProject` cannot saturate
/// the runtime, and every stage re-checks `cancel` so a superseded request
/// stops doing work instead of running to completion.
pub(crate) fn grep_project(cap: OffLoop, req: GrepProjectRequest) {
    let runtime = Arc::clone(&cap.runtime);
    runtime.spawn(async move {
        let GrepProjectRequest {
            pattern,
            opts,
            regex,
            max_results,
            root,
            ignored_dirs,
            callback_id,
            mut dirty_plans,
            clean_buffers,
            cancel,
        } = req;
        let query_len = pattern.len();

        let (path_tx, mut path_rx) = tokio::sync::mpsc::channel::<PathBuf>(256);
        let walk_fs = Arc::clone(&cap.filesystem);
        let walk_cancel = Arc::clone(&cancel);
        tokio::task::spawn_blocking(move || {
            if let Err(e) = walk_fs.walk_files(&root, ignored_dirs, &walk_cancel, &mut |path, _| {
                path_tx.blocking_send(path.to_path_buf()).is_ok()
            }) {
                tracing::warn!("grepProject: walk_files failed: {}", e);
            }
        });

        // Bounded fan-out over files. `results` is only ever touched by this
        // task, so ordering stays deterministic per completion batch.
        let semaphore = Arc::new(tokio::sync::Semaphore::new(8));
        let mut joins: Vec<tokio::task::JoinHandle<Vec<GrepMatch>>> = Vec::new();
        let mut results: Vec<GrepMatch> = Vec::new();
        // Set when the walk stops because `max_results` was reached, which is a
        // complete answer — as opposed to `cancel` being flipped from outside.
        let mut enough_results = false;

        while let Some(file_path) = path_rx.recv().await {
            if cancel.load(Ordering::Relaxed) {
                break;
            }
            let Ok(permit) = Arc::clone(&semaphore).acquire_owned().await else {
                break;
            };
            let fs = Arc::clone(&cap.filesystem);
            let regex = regex.clone();
            let pattern = pattern.clone();
            let opts = opts.clone();
            let task_cancel = Arc::clone(&cancel);
            let dirty = dirty_plans.remove(&file_path);
            let buffer_id = clean_buffers.get(&file_path).map(|b| b.0).unwrap_or(0);
            joins.push(tokio::task::spawn_blocking(move || {
                let _permit = permit;
                if task_cancel.load(Ordering::Relaxed) {
                    return Vec::new();
                }
                search_one(
                    &*fs,
                    &file_path,
                    dirty,
                    buffer_id,
                    &pattern,
                    &opts,
                    &regex,
                    max_results,
                    query_len,
                )
            }));

            // Reap eagerly so a huge tree doesn't accumulate a join list
            // proportional to the file count, and so `max_results` short-
            // circuits the walk instead of only trimming at the end.
            if joins.len() >= 32 {
                drain_joins(&mut joins, &mut results, max_results).await;
                if results.len() >= max_results {
                    // Reusing `cancel` to stop the walk means the flag no
                    // longer distinguishes "superseded" from "we have all the
                    // matches we were asked for" — this remembers which.
                    enough_results = true;
                    cancel.store(true, Ordering::Relaxed);
                    break;
                }
            }
        }
        drain_joins(&mut joins, &mut results, max_results).await;

        // Dirty buffers whose files the walk never reached (e.g. an unsaved
        // buffer outside the workspace root) still deserve a search.
        for (file_path, (bid, plan)) in dirty_plans {
            if results.len() >= max_results || cancel.load(Ordering::Relaxed) {
                break;
            }
            let remaining = max_results - results.len();
            if let Ok(matches) = plan.execute(
                &*cap.filesystem,
                &pattern,
                &opts,
                &regex,
                remaining,
                query_len,
            ) {
                let file = file_path.to_string_lossy().to_string();
                results.extend(matches.into_iter().map(|m| GrepMatch {
                    file: file.clone(),
                    buffer_id: bid.0,
                    byte_offset: m.byte_offset,
                    length: m.length,
                    line: m.line,
                    column: m.column,
                    context: m.context,
                }));
            }
        }

        // A superseded request stopped mid-walk, so `results` is an arbitrary
        // prefix of the real answer. Returning it would be indistinguishable
        // from "the project contains exactly these matches" — the caller must
        // be told instead. It still settles either way, so the `await` never
        // dangles. Hitting `max_results` is not that case: the walk stopped
        // because the answer was complete.
        if cancel.load(Ordering::Relaxed) && !enough_results {
            cap.settle(
                callback_id,
                Err("grepProject superseded by a newer call from this plugin".to_string()),
            );
            return;
        }

        results.truncate(max_results);
        let json = serde_json::to_string(&results).unwrap_or_else(|_| "[]".to_string());
        cap.settle(callback_id, Ok(json));
    });
}

/// Collect finished per-file searches into `results`, capped at `max_results`.
async fn drain_joins(
    joins: &mut Vec<tokio::task::JoinHandle<Vec<GrepMatch>>>,
    results: &mut Vec<GrepMatch>,
    max_results: usize,
) {
    for join in joins.drain(..) {
        if let Ok(matches) = join.await {
            if results.len() < max_results {
                results.extend(matches);
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn search_one(
    fs: &dyn FileSystem,
    file_path: &std::path::Path,
    dirty: Option<(BufferId, HybridSearchPlan)>,
    clean_buffer_id: usize,
    pattern: &str,
    opts: &FileSearchOptions,
    regex: &regex::bytes::Regex,
    max_results: usize,
    query_len: usize,
) -> Vec<GrepMatch> {
    let file = file_path.to_string_lossy().to_string();
    if let Some((bid, plan)) = dirty {
        let Ok(matches) = plan.execute(fs, pattern, opts, regex, max_results, query_len) else {
            return Vec::new();
        };
        return matches
            .into_iter()
            .map(|m| GrepMatch {
                file: file.clone(),
                buffer_id: bid.0,
                byte_offset: m.byte_offset,
                length: m.length,
                line: m.line,
                column: m.column,
                context: m.context,
            })
            .collect();
    }

    let mut cursor = FileSearchCursor::new();
    let mut matches = Vec::new();
    while !cursor.done && matches.len() < max_results {
        match fs.search_file(file_path, pattern, opts, &mut cursor) {
            Ok(batch) => matches.extend(batch),
            Err(_) => break,
        }
    }
    matches.truncate(max_results);
    matches
        .into_iter()
        .map(|m| GrepMatch {
            file: file.clone(),
            buffer_id: clean_buffer_id,
            byte_offset: m.byte_offset,
            length: m.length,
            line: m.line,
            column: m.column,
            context: m.context,
        })
        .collect()
}

// ============================================================================
// Diff-baseline loading
// ============================================================================

/// Editor-thread-collected inputs for loading a diff baseline's content.
/// The spawner and store handle are owned `Arc`s so the load runs entirely
/// off-loop; the editor thread only allocated the id and inserted the
/// placeholder entry.
pub(crate) struct BaselineLoadRequest {
    pub baseline_id: u64,
    pub spec: crate::app::diff_baselines::BaselineSpec,
    pub spawner: Arc<dyn crate::services::remote::ProcessSpawner>,
    pub store: crate::app::diff_baselines::BaselineStore,
    pub callback_id: JsCallbackId,
    /// Registration resolves with the baseline id; a refresh resolves with
    /// null. A failed registration also removes the placeholder entry,
    /// while a failed refresh keeps the previous content serving.
    pub is_registration: bool,
}

/// Load a baseline's reference content (filesystem read or `git show` on
/// the window's authority), install it in the shared store, and settle the
/// plugin's promise. Runs on the tokio runtime.
pub(crate) fn load_diff_baseline(cap: OffLoop, req: BaselineLoadRequest) {
    use crate::app::diff_baselines::{BaselineContent, BaselineSpec};

    let runtime = Arc::clone(&cap.runtime);
    runtime.spawn(async move {
        let text: Result<String, String> = match &req.spec {
            // Saved baselines never load content; the editor thread
            // resolves them synchronously and never sends them here.
            BaselineSpec::Saved => Ok(String::new()),
            BaselineSpec::Disk { path } => {
                let path = path.clone();
                let fs = Arc::clone(&cap.filesystem);
                tokio::task::spawn_blocking(move || fs.read_file(&path))
                    .await
                    .map_err(|e| format!("baseline read task failed: {e}"))
                    .and_then(|r| {
                        r.map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
                            .map_err(|e| format!("failed to read baseline file: {e}"))
                    })
            }
            BaselineSpec::Git {
                cwd,
                file_path,
                git_ref,
            } => load_git_baseline(req.spawner.as_ref(), cwd, file_path, git_ref.as_deref()).await,
        };

        match text {
            Ok(text) => {
                let content = BaselineContent::new(text);
                let installed = match req.store.inner.lock() {
                    Ok(mut inner) => match inner.entries.get_mut(&req.baseline_id) {
                        Some(entry) => {
                            entry.content = Some(content);
                            entry.generation += 1;
                            true
                        }
                        // Released while the load was in flight.
                        None => false,
                    },
                    Err(_) => false,
                };
                if installed {
                    let json = if req.is_registration {
                        req.baseline_id.to_string()
                    } else {
                        "null".to_string()
                    };
                    cap.settle(req.callback_id, Ok(json));
                } else {
                    cap.settle(
                        req.callback_id,
                        Err("baseline released during load".to_string()),
                    );
                }
            }
            Err(e) => {
                if req.is_registration {
                    if let Ok(mut inner) = req.store.inner.lock() {
                        inner.entries.remove(&req.baseline_id);
                    }
                }
                cap.settle(req.callback_id, Err(e));
            }
        }
    });
}

/// Fetch a file's content at a git revision: resolve the repo-relative
/// path, then `git show`. `git_ref` of `None` means the index (stage 0).
async fn load_git_baseline(
    spawner: &dyn crate::services::remote::ProcessSpawner,
    cwd: &std::path::Path,
    file_path: &std::path::Path,
    git_ref: Option<&str>,
) -> Result<String, String> {
    let cwd_str = cwd.to_string_lossy().to_string();
    let file_str = file_path.to_string_lossy().to_string();

    let ls = spawner
        .spawn(
            "git".to_string(),
            vec![
                "ls-files".to_string(),
                "--full-name".to_string(),
                "--".to_string(),
                file_str,
            ],
            Some(cwd_str.clone()),
        )
        .await
        .map_err(|e| format!("git ls-files failed to spawn: {e}"))?;
    if ls.exit_code != 0 {
        return Err(format!("git ls-files failed: {}", ls.stderr.trim()));
    }
    let rel_path = ls
        .stdout
        .lines()
        .next()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .ok_or_else(|| "file is not tracked in git".to_string())?
        .to_string();

    let show_spec = match git_ref {
        Some(r) => format!("{r}:{rel_path}"),
        None => format!(":0:{rel_path}"),
    };
    let show = spawner
        .spawn(
            "git".to_string(),
            vec!["show".to_string(), show_spec],
            Some(cwd_str),
        )
        .await
        .map_err(|e| format!("git show failed to spawn: {e}"))?;
    if show.exit_code != 0 {
        return Err(format!("git show failed: {}", show.stderr.trim()));
    }
    Ok(show.stdout)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;

    fn opts() -> FileSearchOptions {
        FileSearchOptions {
            fixed_string: true,
            case_sensitive: true,
            whole_word: false,
            max_matches: 100,
        }
    }

    fn run_grep(root: PathBuf, cancel: Arc<AtomicBool>) -> Result<String, String> {
        let runtime = Arc::new(tokio::runtime::Runtime::new().unwrap());
        let (tx, rx) = std::sync::mpsc::channel();
        grep_project(
            OffLoop {
                filesystem: Arc::new(StdFileSystem),
                runtime,
                sender: tx,
            },
            GrepProjectRequest {
                pattern: "NEEDLE".to_string(),
                opts: opts(),
                regex: regex::bytes::Regex::new("NEEDLE").unwrap(),
                max_results: 100,
                root,
                ignored_dirs: &[],
                callback_id: JsCallbackId::new(1),
                dirty_plans: HashMap::new(),
                clean_buffers: HashMap::new(),
                cancel,
            },
        );
        match rx.recv().expect("the request must settle exactly once") {
            AsyncMessage::Plugin(PluginAsyncMessage::OffLoopSettled { result, .. }) => result,
            other => panic!("unexpected message: {other:?}"),
        }
    }

    /// A grep that runs to completion reports its matches.
    #[test]
    fn completed_grep_returns_matches() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.txt"), "NEEDLE here\n").unwrap();

        let result = run_grep(dir.path().to_path_buf(), Arc::new(AtomicBool::new(false)))
            .expect("an uncancelled grep should resolve");
        assert!(
            result.contains("a.txt"),
            "the match should be reported: {result}"
        );
    }

    /// A superseded grep stopped partway, so the matches it happened to collect
    /// are a prefix of the answer, not the answer. Reporting them as success
    /// would be indistinguishable from "this is all there is" — the caller has
    /// to be able to tell, so it settles as an error instead.
    #[test]
    fn superseded_grep_reports_an_error_rather_than_partial_matches() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("a.txt"), "NEEDLE here\n").unwrap();

        let err = run_grep(dir.path().to_path_buf(), Arc::new(AtomicBool::new(true)))
            .expect_err("a superseded grep must not resolve with partial results");
        assert!(
            err.contains("superseded"),
            "the error should say why: {err}"
        );
    }
}
