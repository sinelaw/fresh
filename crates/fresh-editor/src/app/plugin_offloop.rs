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
//! snapshotting (see `prepare_project_search`): collect what only the editor
//! thread can see, hand it to a handler here, return immediately.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

use fresh_core::api::{GrepMatch, JsCallbackId, PluginAsyncMessage, SearchHandleState};
use fresh_core::BufferId;
use tokio::sync::oneshot;

use super::open_buffer_search::grep_match;

use crate::model::filesystem::{FileSearchOptions, FileSystem};
use crate::services::async_bridge::AsyncMessage;
use crate::services::runtime::LiveRuntime;

/// Capability handle for plugin work that must not run on the editor thread.
///
/// Holds exactly the authority a handler needs — a filesystem and the return
/// path to the main loop. Notably absent: anything that can read or mutate
/// editor state, and — deliberately — the runtime.
///
/// The capability is moved *into* the spawned task, and an owning
/// [`LiveRuntime`] in here would therefore keep the runtime up for as long as
/// the task runs. Nothing cancels off-loop work at teardown (there is no
/// `Drop for Editor`, and nobody trips `grep_project_cancel`), so the editor's
/// runtime being dropped is the only thing that stops an in-flight
/// `grepProject`: keep it here and a project-wide grep would carry on against
/// a filesystem and an `async_bridge` nobody is reading, one request timeout
/// per file, holding a dropped runtime's threads alive.
/// The runtime is passed to the entry points below instead, which is all that
/// is needed to prove it is alive at the moment of spawning.
pub(crate) struct OffLoop {
    pub filesystem: Arc<dyn FileSystem + Send + Sync>,
    pub sender: std::sync::mpsc::Sender<AsyncMessage>,
    /// Set when nobody is waiting for the work any more. It is the machine
    /// handle's flag (`OpenMachine::cancel`), so closing the handle stops the
    /// work. Work with no handle gets a flag nothing ever sets.
    pub cancel: Arc<AtomicBool>,
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

/// One `walkTree` request.
pub(crate) struct WalkTreeRequest {
    pub root: PathBuf,
    pub skip_dirs: Vec<String>,
    pub include_hidden: bool,
    pub include_dirs: bool,
    pub max_depth: usize,
    pub max_entries: usize,
    pub callback_id: JsCallbackId,
}

/// Walk a subtree in one call and hand the entries back as JSON. Uses
/// `spawn_blocking` because `FileSystem` is synchronous and a remote one
/// blocks on its transport.
pub(crate) fn walk_tree(runtime: &LiveRuntime, cap: OffLoop, req: WalkTreeRequest) {
    runtime.spawn_blocking(move || {
        let WalkTreeRequest {
            root,
            skip_dirs,
            include_hidden,
            include_dirs,
            max_depth,
            max_entries,
            callback_id,
        } = req;

        let skip: Vec<&str> = skip_dirs.iter().map(String::as_str).collect();
        // One over the cap, so truncation is observed rather than inferred.
        let probe = max_entries.saturating_add(1);
        let opts = crate::model::filesystem::WalkOptions {
            skip_dirs: &skip,
            include_hidden,
            include_dirs,
            max_depth,
            max_entries: probe,
        };

        let cancel = Arc::clone(&cap.cancel);
        let mut entries: Vec<serde_json::Value> = Vec::new();
        let walked = cap.filesystem.walk(&root, &opts, &cancel, &mut |entry| {
            entries.push(serde_json::json!({
                "path": entry.path.to_string_lossy(),
                "rel": entry.rel,
                "kind": match entry.entry_type {
                    crate::model::filesystem::EntryType::Directory => "dir",
                    crate::model::filesystem::EntryType::Symlink => "symlink",
                    crate::model::filesystem::EntryType::File => "file",
                },
                "mtime": entry
                    .metadata
                    .modified
                    .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                    .map_or(0, |d| d.as_secs()),
                "size": entry.metadata.size,
            }));
            true
        });

        let result = match walked {
            Ok(()) => {
                let truncated = entries.len() > max_entries;
                entries.truncate(max_entries);
                serde_json::to_string(&serde_json::json!({
                    "entries": entries,
                    "truncated": truncated,
                }))
                .map_err(|err| format!("could not serialise walk: {err}"))
            }
            Err(err) => Err(format!("{}: {err}", root.display())),
        };
        cap.settle(callback_id, result);
    });
}

/// One `readFilePrefixes` request: many bounded reads in a single call.
pub(crate) struct ReadPrefixesRequest {
    /// `(path, max_bytes)` pairs.
    pub requests: Vec<(PathBuf, usize)>,
    pub callback_id: JsCallbackId,
}

/// Read the first `max_bytes` of each path in one call. A failure is reported
/// per path, so one unreadable file does not lose the batch.
pub(crate) fn read_file_prefixes(runtime: &LiveRuntime, cap: OffLoop, req: ReadPrefixesRequest) {
    runtime.spawn_blocking(move || {
        let ReadPrefixesRequest {
            requests,
            callback_id,
        } = req;

        // Checked between files: each one may be a round trip on a remote machine.
        let results: Vec<serde_json::Value> = requests
            .into_iter()
            .take_while(|_| !cap.cancel.load(Ordering::Relaxed))
            .map(|(path, max_bytes)| {
                // Never ask for more than the file holds: a ranged read may use
                // `read_exact`, which fails on a short file.
                let wanted = cap
                    .filesystem
                    .metadata(&path)
                    .map(|meta| max_bytes.min(meta.size as usize))
                    .unwrap_or(max_bytes);
                let read = if wanted == 0 {
                    Ok(Vec::new())
                } else {
                    cap.filesystem.read_range(&path, 0, wanted)
                };
                match read {
                    Ok(bytes) => serde_json::json!({
                        "path": path.to_string_lossy(),
                        "text": String::from_utf8_lossy(&bytes),
                    }),
                    Err(err) => serde_json::json!({
                        "path": path.to_string_lossy(),
                        "error": err.to_string(),
                    }),
                }
            })
            .collect();

        cap.settle(
            callback_id,
            serde_json::to_string(&results)
                .map_err(|err| format!("could not serialise reads: {err}")),
        );
    });
}

/// One `runOnTarget` request.
pub(crate) struct RunOnTargetRequest {
    pub program: String,
    pub args: Vec<String>,
    pub cwd: Option<String>,
    pub spawner: Arc<dyn crate::services::remote::ProcessSpawner>,
    pub callback_id: JsCallbackId,
}

/// Run a command through the authority's spawner, so a remote machine runs it
/// there. A non-zero exit is data, not an error.
pub(crate) fn run_on_target(runtime: &LiveRuntime, cap: OffLoop, req: RunOnTargetRequest) {
    runtime.spawn(async move {
        let RunOnTargetRequest {
            program,
            args,
            cwd,
            spawner,
            callback_id,
        } = req;

        let result = match spawner.spawn(program.clone(), args, cwd).await {
            Ok(out) => serde_json::to_string(&serde_json::json!({
                "code": out.exit_code,
                "stdout": out.stdout,
                "stderr": out.stderr,
            }))
            .map_err(|err| format!("could not serialise command result: {err}")),
            Err(err) => Err(format!("{program}: {err}")),
        };
        cap.settle(callback_id, result);
    });
}

/// One `machineEnv` request for a machine that has to be asked.
pub(crate) struct RemoteEnvRequest {
    pub names: Vec<String>,
    pub spawner: Arc<dyn crate::services::remote::ProcessSpawner>,
    pub callback_id: JsCallbackId,
}

/// Read environment variables from a remote machine with one `printenv`.
/// A machine without `printenv` reports nothing, the same as an unset name.
pub(crate) fn remote_env(runtime: &LiveRuntime, cap: OffLoop, req: RemoteEnvRequest) {
    runtime.spawn(async move {
        let RemoteEnvRequest {
            names,
            spawner,
            callback_id,
        } = req;

        let result = match spawner
            .spawn("printenv".to_string(), Vec::new(), None)
            .await
        {
            Ok(out) if out.exit_code == 0 => {
                Ok(serde_json::Value::Object(parse_printenv(&out.stdout, &names)).to_string())
            }
            // No `printenv`, or it failed: report nothing rather than guessing.
            Ok(_) | Err(_) => Ok(serde_json::Value::Object(serde_json::Map::new()).to_string()),
        };
        cap.settle(callback_id, result);
    });
}

/// Pick `names` out of `printenv` output. A line without `=` is a newline
/// inside the previous value: `printenv` writes values verbatim.
fn parse_printenv(stdout: &str, names: &[String]) -> serde_json::Map<String, serde_json::Value> {
    let wanted: std::collections::HashSet<&str> = names.iter().map(String::as_str).collect();
    let mut found = serde_json::Map::new();
    // The variable a continuation line belongs to, if we kept it.
    let mut open: Option<String> = None;
    for line in stdout.lines() {
        match line.split_once('=') {
            Some((name, value)) if wanted.contains(name) => {
                found.insert(
                    name.to_string(),
                    serde_json::Value::String(value.to_string()),
                );
                open = Some(name.to_string());
            }
            // Not asked about: its continuation lines are not ours either.
            Some(_) => open = None,
            None => {
                if let Some(name) = open.as_ref() {
                    if let Some(serde_json::Value::String(v)) = found.get_mut(name) {
                        v.push('\n');
                        v.push_str(line);
                    }
                }
            }
        }
    }
    found
}

/// Return whether a workspace-relative file path is included by a
/// comma-separated set of globs from the search panel (all of them when
/// there are none).
///
/// Patterns with a path separator match the whole relative path (`src/**`),
/// while patterns without one match the basename at any depth (`*.rs`).
/// Whitespace around comma-separated patterns is ignored.
pub(crate) fn search_file_glob_matches(file_glob: &str, relative_path: &str) -> bool {
    let relative_path = relative_path.trim_start_matches(['/', '\\']);
    let file_name = relative_path
        .rsplit(['/', '\\'])
        .next()
        .unwrap_or(relative_path);
    let mut has_pattern = false;

    for pattern in file_glob
        .split(',')
        .map(str::trim)
        .filter(|p| !p.is_empty())
    {
        has_pattern = true;
        let pattern = pattern.strip_prefix("./").unwrap_or(pattern);
        let matches = if crate::primitives::glob_match::is_path_pattern(pattern) {
            crate::primitives::glob_match::path_glob_matches(pattern, relative_path)
        } else {
            crate::primitives::glob_match::filename_glob_matches(pattern, file_name)
        };
        if matches {
            return true;
        }
    }

    !has_pattern
}

/// One project search: what the editor thread collected for it (see
/// `Editor::prepare_project_search`), for [`project_search`] to finish here.
pub(crate) struct ProjectSearch {
    pub pattern: String,
    pub opts: FileSearchOptions,
    pub max_results: usize,
    /// Comma-separated globs a file's relative path must match; empty for
    /// every file.
    pub file_glob: String,
    pub root: PathBuf,
    /// Runs `git ls-files` to list the project's files; the authority's, so a
    /// remote session lists the remote project.
    pub spawner: Arc<dyn crate::services::remote::ProcessSpawner>,
    /// `file_explorer.custom_ignore_patterns`, rooted at `root`.
    pub custom_ignore: crate::view::file_tree::ignore::CustomIgnore,
    /// A clean source file, searched ahead of the walk: the listing can miss
    /// files the user has open (outside the root, hidden, gitignored).
    pub source_file: Option<PathBuf>,
    /// Open, modified buffers, searched as they are on the editor thread
    /// (`OpenBufferSearch`); the walk skips their files. The source buffer's
    /// matches are taken first, so `max_results` can't cut them off; the
    /// others' at the end.
    pub open_paths: HashSet<PathBuf>,
    pub source_result: Option<oneshot::Receiver<Vec<GrepMatch>>>,
    pub open_results: Vec<oneshot::Receiver<Vec<GrepMatch>>>,
    /// Open, clean buffers by path — searched on disk, their matches
    /// attributed to the buffer.
    pub clean_buffers: HashMap<PathBuf, BufferId>,
}

/// Add `matches` to a search's pending results, up to `max_results` in all.
fn push_capped(
    handle: &SearchHandleState,
    match_count: &AtomicUsize,
    max_results: usize,
    mut matches: Vec<GrepMatch>,
) {
    // Claim room atomically: several files' searches push at once, and a
    // separate check and add would let each see room for its whole batch.
    let wanted = matches.len();
    let Ok(seen) = match_count.fetch_update(Ordering::Relaxed, Ordering::Relaxed, |seen| {
        let room = max_results.saturating_sub(seen);
        (room > 0).then(|| seen + wanted.min(room))
    }) else {
        return;
    };
    matches.truncate(max_results - seen);
    if matches.is_empty() {
        return;
    }
    let mut state = match handle.state.lock() {
        Ok(s) => s,
        Err(poisoned) => poisoned.into_inner(),
    };
    state.total_seen += matches.len();
    state.pending.extend(matches);
}

/// Search the project's files (see [`project_files`]) for `search`, entirely
/// off the editor thread, streaming matches into `handle` until
/// `max_results`, its cancel flag, or the end, then mark it done. The
/// returned task finishes with it.
///
/// [`project_files`]: crate::services::project_files
///
/// Concurrency is capped so a plugin looping on a search cannot saturate the
/// runtime, and every stage re-checks the cancel flag so a superseded search
/// stops doing work instead of running to completion.
pub(crate) fn project_search(
    runtime: &LiveRuntime,
    filesystem: Arc<dyn FileSystem + Send + Sync>,
    search: ProjectSearch,
    handle: Arc<SearchHandleState>,
) -> tokio::task::JoinHandle<()> {
    runtime.spawn(async move {
        let ProjectSearch {
            pattern,
            opts,
            max_results,
            file_glob,
            root,
            spawner,
            custom_ignore,
            source_file,
            open_paths,
            source_result,
            open_results,
            clean_buffers,
        } = search;
        let cancelled = |handle: &SearchHandleState| handle.cancel.load(Ordering::Relaxed);
        let match_count = Arc::new(AtomicUsize::new(0));
        if let Some(receiver) = source_result {
            let matches = receiver.await.unwrap_or_default();
            push_capped(&handle, &match_count, max_results, matches);
        }

        let (path_tx, mut path_rx) = tokio::sync::mpsc::channel::<PathBuf>(256);
        if let Some(path) = source_file.clone() {
            // The receive loop below is the only consumer and is still
            // alive, so a send error here just means the search was
            // cancelled — discarding the path is correct.
            path_tx.send(path).await.ok();
        }
        let walk_fs = Arc::clone(&filesystem);
        let walk_handle = Arc::clone(&handle);
        let walk_tx = path_tx.clone();
        tokio::task::spawn_blocking(move || {
            if let Err(e) = crate::services::project_files::for_each_file_blocking(
                &spawner,
                &*walk_fs,
                &root,
                &custom_ignore,
                &walk_handle.cancel,
                &mut |path, rel| {
                    if source_file.as_deref() == Some(path) || open_paths.contains(path) {
                        // Queued ahead of the walk, or searched as open.
                        true
                    } else if search_file_glob_matches(&file_glob, rel) {
                        walk_tx.blocking_send(path.to_path_buf()).is_ok()
                    } else {
                        true
                    }
                },
            ) {
                tracing::warn!("Project search: listing project files failed: {}", e);
            }
        });
        // Drop our retained sender so the receive loop terminates once the
        // walker's clone is also dropped.
        drop(path_tx);

        // Bounded fan-out over files.
        let semaphore = Arc::new(tokio::sync::Semaphore::new(8));
        let mut joins: Vec<tokio::task::JoinHandle<()>> = Vec::new();
        while let Some(file_path) = path_rx.recv().await {
            if cancelled(&handle) || match_count.load(Ordering::Relaxed) >= max_results {
                break;
            }
            let Ok(permit) = Arc::clone(&semaphore).acquire_owned().await else {
                break;
            };
            let fs = Arc::clone(&filesystem);
            let (pattern, opts) = (pattern.clone(), opts.clone());
            let (handle, match_count) = (Arc::clone(&handle), Arc::clone(&match_count));
            let buffer_id = clean_buffers.get(&file_path).map_or(0, |b| b.0);
            joins.push(tokio::task::spawn_blocking(move || {
                let _permit = permit;
                let file = file_path.to_string_lossy().into_owned();
                if cancelled(&handle) || match_count.load(Ordering::Relaxed) >= max_results {
                    return;
                }
                match fs.search_file(&file_path, &pattern, &opts) {
                    Ok(found) => {
                        let found = found.into_iter().map(|m| grep_match(&file, buffer_id, m));
                        push_capped(&handle, &match_count, max_results, found.collect());
                    }
                    Err(e) => tracing::debug!("search_file failed {:?}: {}", file_path, e),
                }
            }));
        }
        for join in joins {
            drop(join.await);
        }
        for receiver in open_results {
            let matches = receiver.await.unwrap_or_default();
            push_capped(&handle, &match_count, max_results, matches);
        }

        let mut state = match handle.state.lock() {
            Ok(s) => s,
            Err(poisoned) => poisoned.into_inner(),
        };
        if match_count.load(Ordering::Relaxed) >= max_results {
            state.truncated = true;
        }
        state.done = true;
    })
}

/// A [`project_search`] whose matches settle one JS callback when it is
/// done: `grepProject`. A search superseded before it finished (its cancel
/// flag set by a newer one from the same plugin) settles as an error rather
/// than with the arbitrary prefix of matches it had reached; one stopped by
/// `max_results` is complete.
pub(crate) fn grep_project(
    runtime: &LiveRuntime,
    cap: OffLoop,
    search: ProjectSearch,
    handle: Arc<SearchHandleState>,
    callback_id: JsCallbackId,
) {
    let max_results = search.max_results;
    let done = project_search(
        runtime,
        Arc::clone(&cap.filesystem),
        search,
        Arc::clone(&handle),
    );
    runtime.spawn(async move {
        // A panicked search still settles, with what it had.
        done.await.ok();
        if handle.cancel.load(Ordering::Relaxed) {
            cap.settle(
                callback_id,
                Err("grepProject superseded by a newer call from this plugin".to_string()),
            );
            return;
        }
        let mut results = match handle.state.lock() {
            Ok(mut s) => std::mem::take(&mut s.pending),
            Err(poisoned) => std::mem::take(&mut poisoned.into_inner().pending),
        };
        results.truncate(max_results);
        let json = serde_json::to_string(&results).unwrap_or_else(|_| "[]".to_string());
        cap.settle(callback_id, Ok(json));
    });
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
    /// The buffer's encoding, which the baseline's bytes are decoded with
    /// so a non-UTF-8 file compares equal to its unmodified buffer.
    pub encoding: crate::model::encoding::Encoding,
    /// Registration resolves with the baseline id; a refresh resolves with
    /// null. A failed registration also removes the placeholder entry,
    /// while a failed refresh keeps the previous content serving.
    pub is_registration: bool,
}

/// Load a baseline's reference content (filesystem read or `git show` on
/// the window's authority), install it in the shared store, and settle the
/// plugin's promise. Runs on the tokio runtime.
pub(crate) fn load_diff_baseline(runtime: &LiveRuntime, cap: OffLoop, req: BaselineLoadRequest) {
    use crate::app::diff_baselines::{BaselineContent, BaselineSpec};

    runtime.spawn(async move {
        let decode = |bytes: Vec<u8>| {
            String::from_utf8_lossy(&crate::model::encoding::convert_to_utf8(
                &bytes,
                req.encoding,
            ))
            .into_owned()
        };
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
                        r.map(decode)
                            .map_err(|e| format!("failed to read baseline file: {e}"))
                    })
            }
            BaselineSpec::Git {
                cwd,
                file_path,
                git_ref,
            } => load_git_baseline(req.spawner.as_ref(), cwd, file_path, git_ref.as_deref())
                .await
                .map(decode),
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
/// Returns the raw bytes; the caller decodes them with the buffer's
/// encoding.
async fn load_git_baseline(
    spawner: &dyn crate::services::remote::ProcessSpawner,
    cwd: &std::path::Path,
    file_path: &std::path::Path,
    git_ref: Option<&str>,
) -> Result<Vec<u8>, String> {
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
        .spawn_raw(
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
        run_grep_ignoring(root, cancel, &[])
    }

    /// A spawner with no `git`, so the grep walks the root rather than
    /// depending on whether the temp dir happens to sit in a repository.
    struct NoGit;

    #[async_trait::async_trait]
    impl crate::services::remote::ProcessSpawner for NoGit {
        async fn spawn_raw(
            &self,
            _command: String,
            _args: Vec<String>,
            _cwd: Option<String>,
        ) -> Result<crate::services::remote::RawSpawnResult, crate::services::remote::SpawnError>
        {
            Err(crate::services::remote::SpawnError::Process(
                "no git in test".to_string(),
            ))
        }
    }

    /// `run_grep` with `custom_ignore_patterns`.
    fn run_grep_ignoring(
        root: PathBuf,
        cancel: Arc<AtomicBool>,
        custom_ignore_patterns: &[&str],
    ) -> Result<String, String> {
        use crate::view::file_tree::ignore::CustomIgnore;

        let patterns: Vec<String> = custom_ignore_patterns
            .iter()
            .map(|p| p.to_string())
            .collect();
        let custom_ignore = CustomIgnore::new(&root, &patterns);
        let runtime = LiveRuntime::multi_thread("offloop-test", 2).unwrap();
        let (tx, rx) = std::sync::mpsc::channel();
        let handle = Arc::new(SearchHandleState::new());
        if cancel.load(Ordering::Relaxed) {
            handle.cancel.store(true, Ordering::Relaxed);
        }
        grep_project(
            &runtime,
            OffLoop {
                filesystem: Arc::new(StdFileSystem),
                sender: tx,
                cancel,
            },
            ProjectSearch {
                pattern: "NEEDLE".to_string(),
                opts: opts(),
                max_results: 100,
                file_glob: String::new(),
                root,
                spawner: Arc::new(NoGit),
                custom_ignore,
                source_file: None,
                open_paths: HashSet::new(),
                source_result: None,
                open_results: Vec::new(),
                clean_buffers: HashMap::new(),
            },
            handle,
            JsCallbackId::new(1),
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

    /// Files `custom_ignore_patterns` match, and directories a project walk
    /// never enters, are not searched.
    #[test]
    fn grep_leaves_out_ignored_files() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        std::fs::write(root.join("kept.txt"), "NEEDLE\n").unwrap();
        std::fs::write(root.join("skipped.log"), "NEEDLE\n").unwrap();
        for sub in ["generated", "node_modules"] {
            std::fs::create_dir(root.join(sub)).unwrap();
            std::fs::write(root.join(sub).join("inner.txt"), "NEEDLE\n").unwrap();
        }

        let result = run_grep_ignoring(
            root.to_path_buf(),
            Arc::new(AtomicBool::new(false)),
            &["*.log", "generated/"],
        )
        .expect("an uncancelled grep should resolve");
        let matches: Vec<GrepMatch> = serde_json::from_str(&result).unwrap();
        let files: Vec<&str> = matches.iter().map(|m| m.file.as_str()).collect();
        assert_eq!(files.len(), 1, "only kept.txt should match: {files:?}");
        assert!(files[0].ends_with("kept.txt"), "got {files:?}");
    }

    /// A baseline load settles, and the work it spawned holds no owning
    /// reference to the runtime it spawned onto.
    ///
    /// That second half is the load-bearing one: off-loop work is never
    /// cancelled explicitly — there is no `Drop for Editor` and nothing trips
    /// `grep_project_cancel` at teardown — so the editor's runtime being
    /// dropped is the only thing that stops an in-flight `grepProject`. A
    /// capability that owned its runtime would keep that grep running against
    /// an `async_bridge` nobody is reading, and keep the old runtime's threads
    /// alive across an editor rebuild.
    #[test]
    fn baseline_load_settles_and_leaves_the_runtime_unowned() {
        use crate::app::diff_baselines::{BaselineEntry, BaselineSpec, BaselineStore};
        use crate::services::env_provider::EnvProvider;
        use crate::services::remote::LocalProcessSpawner;
        use crate::services::workspace_trust::WorkspaceTrust;

        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("baseline.txt");
        std::fs::write(&path, "reference\n").unwrap();

        let runtime = LiveRuntime::multi_thread("offloop-test", 2).unwrap();
        let (tx, rx) = std::sync::mpsc::channel();
        let store = BaselineStore::default();
        store.inner.lock().unwrap().entries.insert(
            7,
            BaselineEntry {
                buffer_id: BufferId(1),
                window_id: fresh_core::WindowId(1),
                spec: BaselineSpec::Disk { path: path.clone() },
                generation: 0,
                content: None,
            },
        );

        load_diff_baseline(
            &runtime,
            OffLoop {
                filesystem: Arc::new(StdFileSystem),
                sender: tx,
                cancel: Arc::default(),
            },
            BaselineLoadRequest {
                baseline_id: 7,
                spec: BaselineSpec::Disk { path },
                spawner: Arc::new(LocalProcessSpawner::new(
                    Arc::new(EnvProvider::inactive()),
                    Arc::new(WorkspaceTrust::permissive()),
                )),
                store: store.clone(),
                callback_id: JsCallbackId::new(1),
                encoding: Default::default(),
                is_registration: true,
            },
        );

        assert_eq!(
            runtime.live_clones(),
            1,
            "off-loop work must not hold an owning reference to the runtime: a \
             task outliving the editor's own reference would keep the runtime \
             up and keep working into a bridge nobody is reading"
        );

        let result = match rx.recv().expect("the load must settle exactly once") {
            AsyncMessage::Plugin(PluginAsyncMessage::OffLoopSettled { result, .. }) => result,
            other => panic!("unexpected message: {other:?}"),
        };
        assert_eq!(
            result.expect("a readable baseline file should resolve"),
            "7",
            "registration resolves with the baseline id"
        );
        let inner = store.inner.lock().unwrap();
        let entry = inner
            .entries
            .get(&7)
            .expect("the entry must still be there");
        assert_eq!(
            entry.content.as_ref().map(|c| c.text.as_str()),
            Some("reference\n"),
            "the loaded content should be installed in the store"
        );
        drop(inner);
    }

    /// A disk baseline is decoded with the buffer's encoding, so a Shift-JIS
    /// file's baseline matches its (UTF-8) buffer text. Issue #3246.
    #[test]
    fn disk_baseline_is_decoded_with_the_buffer_encoding() {
        use crate::app::diff_baselines::{BaselineEntry, BaselineSpec, BaselineStore};
        use crate::model::encoding::Encoding;
        use crate::services::env_provider::EnvProvider;
        use crate::services::remote::LocalProcessSpawner;
        use crate::services::workspace_trust::WorkspaceTrust;

        let text = "// コンフィグウィンドウ：ベース\n";
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("baseline.cpp");
        std::fs::write(&path, encoding_rs::SHIFT_JIS.encode(text).0).unwrap();

        let runtime = LiveRuntime::multi_thread("offloop-test", 2).unwrap();
        let (tx, rx) = std::sync::mpsc::channel();
        let store = BaselineStore::default();
        store.inner.lock().unwrap().entries.insert(
            3,
            BaselineEntry {
                buffer_id: BufferId(1),
                window_id: fresh_core::WindowId(1),
                spec: BaselineSpec::Disk { path: path.clone() },
                generation: 0,
                content: None,
            },
        );
        load_diff_baseline(
            &runtime,
            OffLoop {
                filesystem: Arc::new(StdFileSystem),
                sender: tx,
                cancel: Arc::default(),
            },
            BaselineLoadRequest {
                baseline_id: 3,
                spec: BaselineSpec::Disk { path },
                spawner: Arc::new(LocalProcessSpawner::new(
                    Arc::new(EnvProvider::inactive()),
                    Arc::new(WorkspaceTrust::permissive()),
                )),
                store: store.clone(),
                callback_id: JsCallbackId::new(1),
                encoding: Encoding::ShiftJis,
                is_registration: true,
            },
        );
        match rx.recv().expect("the load must settle exactly once") {
            AsyncMessage::Plugin(PluginAsyncMessage::OffLoopSettled { result, .. }) => {
                result.expect("a readable baseline file should resolve");
            }
            other => panic!("unexpected message: {other:?}"),
        }
        let inner = store.inner.lock().unwrap();
        assert_eq!(
            inner.entries[&3].content.as_ref().map(|c| c.text.as_str()),
            Some(text)
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

    use super::parse_printenv;

    fn names(list: &[&str]) -> Vec<String> {
        list.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn only_the_names_asked_for_come_back() {
        let out = parse_printenv(
            "HOME=/root\nCODEX_HOME=/srv/codex\nPATH=/usr/bin\n",
            &names(&["CODEX_HOME", "HOME"]),
        );
        assert_eq!(out.len(), 2);
        assert_eq!(out["CODEX_HOME"], "/srv/codex");
        assert_eq!(out["HOME"], "/root");
    }

    #[test]
    fn a_name_that_is_not_set_is_absent_rather_than_empty() {
        let out = parse_printenv("HOME=/root\n", &names(&["CODEX_HOME"]));
        assert!(out.is_empty(), "an unset variable is not reported at all");
    }

    #[test]
    fn a_value_with_an_equals_sign_keeps_it() {
        let out = parse_printenv("FLAGS=-Dfoo=bar\n", &names(&["FLAGS"]));
        assert_eq!(out["FLAGS"], "-Dfoo=bar", "only the first `=` separates");
    }

    #[test]
    fn a_newline_inside_a_value_is_part_of_it() {
        let out = parse_printenv(
            "SCRIPT=line one\nline two\nHOME=/root\n",
            &names(&["SCRIPT", "HOME"]),
        );
        assert_eq!(out["SCRIPT"], "line one\nline two");
        assert_eq!(out["HOME"], "/root");
    }

    #[test]
    fn a_continuation_of_a_variable_we_skipped_is_not_attached_to_the_last_kept_one() {
        let out = parse_printenv("HOME=/root\nOTHER=first\nstray\n", &names(&["HOME"]));
        assert_eq!(out["HOME"], "/root", "the stray line belongs to OTHER");
    }
}
