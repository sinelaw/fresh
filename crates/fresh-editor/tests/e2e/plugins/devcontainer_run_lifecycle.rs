//! E2E coverage for `Dev Container: Run Lifecycle Command`.
//!
//! Reproduces a bug where the `prompt_confirmed` handler in
//! `devcontainer.ts` reads `data.value` while the runtime delivers
//! `{ prompt_type, input, selected_index }`. The handler's early-return
//! on `!cmdName` swallows the request and the lifecycle command never
//! runs.
//!
//! Why `initializeCommand` and not `postCreateCommand`: per the dev-
//! container spec, `initializeCommand` is the *host-side* prologue —
//! the plugin's own comment in `devcontainer_run_lifecycle` calls it
//! out as "surface it in the picker so users can re-run it on demand."
//! That makes a no-container "Run Lifecycle" of `initializeCommand`
//! semantically valid; using `postCreateCommand` would be a host-side
//! run of a command the spec defines as container-side. The same
//! field-name bug trips both, so we pick the one that models a real
//! supported flow.
//!
//! No fake `devcontainer`/`docker` CLI is required: the lifecycle
//! handler invokes the chosen command directly via
//! `editor.spawnProcess` (here, `sh -c "<initializeCommand>"`). The
//! command writes a sentinel file inside the test tempdir; presence
//! of the file proves the handler reached the spawn path. The harness
//! routes spawns through the local `ProcessSpawner`, so the test runs
//! anywhere `sh` is on PATH.
//!
//! Without the fix the sentinel never appears and `wait_until` hangs
//! until nextest's external timeout fires (intentional — CONTRIBUTING
//! forbids in-test timeouts).

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::Path;

/// Build a workspace whose `devcontainer.json` declares an
/// `initializeCommand` that touches `sentinel`. Returns the workspace
/// `TempDir` (kept alive by the caller) and its path.
fn set_up_workspace(sentinel: &Path) -> (tempfile::TempDir, std::path::PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    // Canonicalize to dodge macOS `/var` → `/private/var` symlink
    // surprises when comparing paths the spawned shell sees.
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    let dc_json = format!(
        r#"{{
            "name": "fake",
            "image": "ubuntu:22.04",
            "initializeCommand": "touch {}"
        }}"#,
        sentinel.display()
    );
    fs::write(dc.join("devcontainer.json"), dc_json).unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    (temp, workspace)
}

/// Wait until the `Dev Container: Run Lifecycle Command` palette entry
/// is registered with its localized name. Mirrors
/// `devcontainer_ports_panel.rs`'s readiness check so a pending plugin
/// load surfaces a diagnostic instead of a silent hang.
fn wait_for_run_lifecycle_command(harness: &mut EditorTestHarness) {
    let want_key = "%cmd.run_lifecycle";
    let want_localized = "Dev Container: Run Lifecycle Command";
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            reg.get_all()
                .iter()
                .any(|c| c.name == want_key && c.get_localized_name() == want_localized)
        })
        .unwrap();
}

/// Drives the bug: `Run Lifecycle` → confirm `initializeCommand` →
/// expect the sentinel file. The handler's wrong field name keeps it
/// from ever calling `spawnProcess`, so the sentinel never appears.
///
/// Unix-only: the test's `initializeCommand` is `sh -c "touch <path>"`,
/// which doesn't survive the round-trip through `cmd.exe` on
/// Windows (backslash paths get treated as escapes). The bug being
/// guarded is in the plugin's prompt-handler field name (`data.input`
/// vs `data.value`), which is platform-agnostic — so the Linux/macOS
/// runs of this test are already a sufficient regression guard.
#[cfg(unix)]
#[test]
fn run_lifecycle_executes_initialize_command() {
    let sentinel_temp = tempfile::tempdir().unwrap();
    let sentinel = sentinel_temp
        .path()
        .canonicalize()
        .unwrap()
        .join("lifecycle.marker");

    let (_workspace_temp, workspace) = set_up_workspace(&sentinel);
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();
    harness.tick_and_render().unwrap();

    let plugin_names: Vec<_> = harness
        .editor()
        .plugin_manager()
        .list_plugins()
        .into_iter()
        .map(|p| p.name)
        .collect();
    assert!(
        plugin_names.iter().any(|n| n == "devcontainer"),
        "`devcontainer` plugin must be loaded. Loaded: {:?}",
        plugin_names
    );

    wait_for_run_lifecycle_command(&mut harness);

    // First palette: pick `Dev Container: Run Lifecycle Command`.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Dev Container: Run Lifecycle").unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Dev Container: Run Lifecycle Command")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Second prompt: lifecycle picker. `initializeCommand` is the
    // only defined entry, so it's the default selection.
    harness
        .wait_until(|h| h.screen_to_string().contains("initializeCommand"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The handler should now spawn `sh -c "touch <sentinel>"`. With
    // the bug, the handler returns early on the wrong field name and
    // the sentinel never appears.
    harness.wait_until(|_| sentinel.exists()).unwrap();
}
