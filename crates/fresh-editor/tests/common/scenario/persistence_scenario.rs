//! `PersistenceScenario` — filesystem + session/recovery state.
//!
//! Phase 6 lands as a *real-FS* runner: scenario fixtures land in
//! the harness's existing temp directory, the editor opens them
//! through its normal filesystem code, and FsState assertions read
//! the resulting contents back from disk.
//!
//! A future production refactor (the `VirtualFs` adapter trait
//! described in the design doc) would replace the temp-FS with an
//! in-memory backend — same scenario data, faster runs, no I/O
//! contention. The data shape here is deliberately the
//! VirtualFs-shaped one so the corpus is forward-compatible.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::context::{VirtualFile, VirtualFs};
use crate::common::scenario::failure::ScenarioFailure;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::observable::FsState;
use crate::common::scenario::property::BufferState;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct PersistenceScenario {
    pub description: String,
    /// Files to seed under the harness's temp directory at scenario
    /// start. Paths in `initial_fs` are interpreted as **relative**
    /// to the temp root; absolute paths are treated relative as
    /// well to keep scenarios portable across machines.
    pub initial_fs: VirtualFs,
    /// Path the editor opens at scenario start, relative to the
    /// temp root. Empty string ⇒ start with the harness's default
    /// empty unnamed buffer (used by Save-As scenarios that begin
    /// without an on-disk file).
    #[serde(default)]
    pub initial_open: String,
    pub events: Vec<InputEvent>,
    /// Optional buffer-text expectation. None ⇒ skip.
    #[serde(default)]
    pub expected_buffer: Option<BufferState>,
    /// Files we expect to find on disk at scenario end. Paths
    /// relative to the temp root. Files not listed are not asserted
    /// on (so a scenario that only cares about one file doesn't
    /// have to enumerate the whole tree).
    pub expected_fs: FsState,
}

pub fn check_persistence_scenario(s: PersistenceScenario) -> Result<(), ScenarioFailure> {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let temp_root: PathBuf = harness
        .temp_dir_path()
        .ok_or_else(|| ScenarioFailure::InputProjectionFailed {
            description: s.description.clone(),
            reason: "harness has no temp dir; PersistenceScenario requires one".into(),
        })?
        .to_path_buf();

    // Seed the filesystem.
    seed_files(&temp_root, &s.initial_fs, &s.description)?;

    // Open the initial buffer if requested. Empty `initial_open`
    // ⇒ stay on the harness's default empty unnamed buffer (the
    // shape Save-As scenarios start from).
    if !s.initial_open.is_empty() {
        let open_path = relative_under(&temp_root, &s.initial_open);
        harness
            .open_file(&open_path)
            .map_err(|e| ScenarioFailure::InputProjectionFailed {
                description: s.description.clone(),
                reason: format!("failed to open {open_path:?}: {e}"),
            })?;
    }

    // Run events.
    for ev in &s.events {
        dispatch(&temp_root, &mut harness, ev, &s.description)?;
    }

    // Assert buffer state if requested.
    if let Some(want) = &s.expected_buffer {
        let api = harness.api_mut();
        let actual = BufferState {
            buffer_text: api.buffer_text(),
            primary: api.primary_caret(),
            all_carets: api.carets(),
            selection_text: api.selection_text(),
        };
        if &actual != want {
            return Err(ScenarioFailure::BufferTextMismatch {
                description: s.description,
                expected: format!("{want:?}"),
                actual: format!("{actual:?}"),
            });
        }
    }

    // Assert files-on-disk. Paths are treated relative to the temp
    // root so scenarios are portable.
    for (rel, want_content) in &s.expected_fs.expected_files {
        let abs = relative_under(&temp_root, rel);
        let got =
            std::fs::read_to_string(&abs).map_err(|e| ScenarioFailure::WorkspaceStateMismatch {
                description: s.description.clone(),
                field: format!("fs[{rel:?}] read_to_string"),
                expected: format!("{want_content:?}"),
                actual: format!("err: {e}"),
            })?;
        if &got != want_content {
            return Err(ScenarioFailure::WorkspaceStateMismatch {
                description: s.description.clone(),
                field: format!("fs[{rel:?}]"),
                expected: format!("{want_content:?}"),
                actual: format!("{got:?}"),
            });
        }
    }
    Ok(())
}

pub fn assert_persistence_scenario(s: PersistenceScenario) {
    if let Err(f) = check_persistence_scenario(s) {
        panic!("{f}");
    }
}

fn seed_files(root: &Path, fs: &VirtualFs, description: &str) -> Result<(), ScenarioFailure> {
    for (path, file) in &fs.files {
        let abs = relative_under(root, path);
        if let Some(parent) = abs.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("create_dir_all {parent:?}: {e}"),
                }
            })?;
        }
        std::fs::write(&abs, &file.content).map_err(|e| {
            ScenarioFailure::InputProjectionFailed {
                description: description.into(),
                reason: format!("write {abs:?}: {e}"),
            }
        })?;
    }
    Ok(())
}

/// Resolve a scenario-relative path under the harness root,
/// stripping any leading `/` so absolute paths stay portable.
fn relative_under(root: &Path, p: impl AsRef<Path>) -> PathBuf {
    let p = p.as_ref();
    let rel = p.strip_prefix("/").unwrap_or(p);
    root.join(rel)
}

fn dispatch(
    root: &Path,
    harness: &mut EditorTestHarness,
    ev: &InputEvent,
    description: &str,
) -> Result<(), ScenarioFailure> {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::test_api::Action;
    match ev {
        InputEvent::Action(a) => {
            harness.api_mut().dispatch(a.clone());
            Ok(())
        }
        InputEvent::FsExternalEdit { path, content } => {
            // Mutate the file behind the editor's back, then notify
            // the editor's auto-revert path. Without the notify,
            // the editor has no live file-watcher in tests and a
            // subsequent read-back of the same file would just
            // reflect the disk bytes we just wrote — making the
            // scenario tautological. With the notify, if the editor
            // has the path open, its auto-revert reloads the
            // buffer from disk and the editor-side state actually
            // reacts to the external change.
            let abs = relative_under(root, path);
            std::fs::write(&abs, content).map_err(|e| ScenarioFailure::InputProjectionFailed {
                description: description.into(),
                reason: format!("FsExternalEdit write {abs:?}: {e}"),
            })?;
            // The editor's auto-revert path (`handle_file_changed`)
            // only reloads when the file's mtime is STRICTLY newer
            // than the mtime captured at open. On filesystems with
            // coarse (1-second) mtime granularity, an open + external
            // write landing in the same second leave equal mtimes and
            // the revert is skipped — a real flake we hit under
            // parallel CI load. The original e2e (`auto_revert.rs`)
            // worked around this with a 2.1s real sleep before each
            // write; here we instead push the mtime deterministically
            // forward so the revert always fires, no sleeping.
            let future = std::time::SystemTime::now() + std::time::Duration::from_secs(10);
            std::fs::File::options()
                .write(true)
                .open(&abs)
                .and_then(|f| f.set_modified(future))
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("FsExternalEdit set mtime {abs:?}: {e}"),
                })?;
            let abs_str = abs.to_string_lossy().to_string();
            harness.api_mut().notify_file_changed(&abs_str);
            Ok(())
        }
        InputEvent::EditorFileChangedReaction { path } => {
            // Notify *without* writing — the load-bearing case for
            // "file-watcher fired for a save we just performed; the
            // on-disk content matches the buffer; the auto-revert
            // path must NOT clear the undo log" (issue #191
            // follow-up). The path comes in as scenario-relative, so
            // resolve under the temp root.
            let abs = relative_under(root, path);
            let abs_str = abs.to_string_lossy().to_string();
            harness.api_mut().notify_file_changed(&abs_str);
            Ok(())
        }
        InputEvent::AssertBufferText(want) => {
            let got = harness.api_mut().buffer_text();
            if &got != want {
                return Err(ScenarioFailure::BufferTextMismatch {
                    description: description.into(),
                    expected: want.clone(),
                    actual: got,
                });
            }
            Ok(())
        }
        InputEvent::AssertIsModified(want) => {
            let got = harness.api_mut().is_modified();
            if got != *want {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: description.into(),
                    field: "is_modified".into(),
                    expected: format!("{want}"),
                    actual: format!("{got}"),
                });
            }
            Ok(())
        }
        InputEvent::AssertEventLogLen(want) => {
            let got = harness.api_mut().active_event_log_len();
            if got != *want {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: description.into(),
                    field: "active_event_log_len".into(),
                    expected: format!("{want}"),
                    actual: format!("{got}"),
                });
            }
            Ok(())
        }
        InputEvent::AssertPrimaryCursorAtMost(max) => {
            let pos = harness.api_mut().primary_caret().position;
            if pos > *max {
                return Err(ScenarioFailure::WorkspaceStateMismatch {
                    description: description.into(),
                    field: "primary_caret.position".into(),
                    expected: format!("<= {max}"),
                    actual: format!("{pos}"),
                });
            }
            Ok(())
        }
        InputEvent::OpenSaveAsPrompt => {
            harness.api_mut().dispatch(Action::SaveAs);
            // Defensive: confirm the prompt opened. Without this,
            // a regression in `Action::SaveAs` that silently fails
            // to open the prompt would let the subsequent
            // PromptFillText flow type into the buffer instead.
            if harness.api_mut().modal_snapshot().prompt.is_none() {
                return Err(ScenarioFailure::ModalStateMismatch {
                    description: description.into(),
                    expected: "active SaveAs prompt".into(),
                    actual: "no prompt".into(),
                });
            }
            Ok(())
        }
        InputEvent::PromptBackspace { count } => {
            for _ in 0..*count {
                harness
                    .send_key(KeyCode::Backspace, KeyModifiers::NONE)
                    .map_err(|e| ScenarioFailure::InputProjectionFailed {
                        description: description.into(),
                        reason: format!("PromptBackspace send_key(Backspace): {e}"),
                    })?;
            }
            Ok(())
        }
        InputEvent::PromptFillText(text) => {
            // `type_text` routes each char through `handle_key`,
            // which delegates to the prompt input handler while the
            // prompt is active — same path the keymap uses.
            harness
                .type_text(text)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("PromptFillText type_text({text:?}): {e}"),
                })?;
            Ok(())
        }
        InputEvent::PromptFillTempPath { rel } => {
            let abs = relative_under(root, rel);
            let abs_str = abs
                .to_str()
                .ok_or_else(|| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("temp path {abs:?} is not valid UTF-8"),
                })?
                .to_string();
            harness
                .type_text(&abs_str)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("PromptFillTempPath type_text({abs_str:?}): {e}"),
                })?;
            Ok(())
        }
        InputEvent::PromptConfirm => {
            harness
                .send_key(KeyCode::Enter, KeyModifiers::NONE)
                .map_err(|e| ScenarioFailure::InputProjectionFailed {
                    description: description.into(),
                    reason: format!("PromptConfirm send_key(Enter): {e}"),
                })?;
            Ok(())
        }
        other => Err(ScenarioFailure::InputProjectionFailed {
            description: description.into(),
            reason: format!("PersistenceScenario does not handle {other:?} — wrong scenario type"),
        }),
    }
}

/// Convenience constructor for the simplest case: one initial
/// file, type some characters, save, expect the on-disk content
/// to reflect the typing.
pub fn write_then_save(
    description: &str,
    filename: &str,
    initial: &str,
    typed: &str,
    expected_on_disk: &str,
) -> PersistenceScenario {
    let initial_path = PathBuf::from(filename);
    let initial_files: BTreeMap<PathBuf, VirtualFile> = std::iter::once((
        initial_path.clone(),
        VirtualFile {
            content: initial.to_string(),
            mode: None,
            mtime_unix_secs: None,
        },
    ))
    .collect();
    let typed_actions =
        std::iter::once(InputEvent::Action(fresh::test_api::Action::MoveDocumentEnd))
            .chain(
                typed
                    .chars()
                    .map(|c| InputEvent::Action(fresh::test_api::Action::InsertChar(c))),
            )
            .chain(std::iter::once(InputEvent::Action(
                fresh::test_api::Action::Save,
            )))
            .collect();
    PersistenceScenario {
        description: description.to_string(),
        initial_fs: VirtualFs {
            files: initial_files,
        },
        initial_open: filename.into(),
        events: typed_actions,
        expected_buffer: None,
        expected_fs: FsState {
            expected_files: std::iter::once((filename.to_string(), expected_on_disk.to_string()))
                .collect(),
        },
    }
}
