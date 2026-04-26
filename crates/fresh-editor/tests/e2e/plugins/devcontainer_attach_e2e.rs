//! End-to-end coverage for the dev-container *attach* flow against
//! the in-tree fake CLI ([`scripts/fake-devcontainer/`]).
//!
//! Drives Flow A from `docs/internal/FAKE_DEVCONTAINER_TEST_PLAN.md`:
//! launch the editor in a workspace with a `.devcontainer/devcontainer.json`,
//! accept the "Reopen in Container?" popup, and assert that the
//! container authority lands.
//!
//! No Docker daemon, no Node, no `@devcontainers/cli` install — the
//! harness's [`with_fake_devcontainer`] helper points the editor at
//! the bash shims that ship in-tree and an isolated state directory.
//!
//! Asserts go through the editor's public state surface
//! (`Authority::display_label`, on-disk state files, plugin global
//! state) — never internal popup state — per CONTRIBUTING.md §2.
//!
//! [`with_fake_devcontainer`]: crate::common::harness::HarnessOptions::with_fake_devcontainer

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::Path;

/// Install the devcontainer plugin and its lib stubs into a workspace,
/// write a minimal `.devcontainer/devcontainer.json`, and return the
/// canonicalized workspace path. Canonicalize because the spawned
/// shell sees `/private/var/...` on macOS even when the test thinks
/// it's in `/var/...`; matching paths back to the workspace later
/// would otherwise fail.
fn set_up_workspace() -> (tempfile::TempDir, std::path::PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
            "name": "fake-e2e",
            "image": "mcr.microsoft.com/devcontainers/base:ubuntu",
            "remoteUser": "vscode"
        }"#,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    (temp, workspace)
}

/// Wait until the devcontainer plugin's `Reopen in Container?` action
/// popup has rendered. The plugin defers the prompt to the
/// `plugins_loaded` hook, so this races plugin init. We assert via
/// screen text (rather than internal popup state) per CONTRIBUTING
/// §2.
fn wait_for_attach_popup(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Dev Container Detected")
                && screen.contains("Reopen in Container")
        })
        .unwrap();
}

/// Wait until the active authority advertises a `Container:<id>`
/// display label. This is the post-`setAuthority`-restart steady
/// state: plugin parsed the success JSON, built the payload, set the
/// authority, the editor rebuilt with it.
fn wait_for_container_authority(harness: &mut EditorTestHarness) -> String {
    harness
        .wait_until(|h| h.editor().authority().display_label.starts_with("Container:"))
        .unwrap();
    harness.editor().authority().display_label.clone()
}

/// Happy-path attach: popup → Reopen → setAuthority → display label.
/// Mirrors Flow A in the interactive test plan.
#[test]
fn attach_via_fake_devcontainer_lands_container_authority() {
    let (_workspace_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
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

    wait_for_attach_popup(&mut harness);

    // The popup is global; accept "Reopen in Container" by pressing
    // Enter on the default selection. (The first row is the
    // "attach" action — Esc would dismiss instead.) Send Escape
    // first to release any default file-explorer focus, matching the
    // interactive walk in the test plan.
    harness
        .send_key(KeyCode::Esc, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let label = wait_for_container_authority(&mut harness);
    let container_id = label
        .strip_prefix("Container:")
        .expect("display_label starts with Container:");
    assert!(
        !container_id.is_empty(),
        "container id must be non-empty (label = {label:?})"
    );

    // The fake CLI persisted the container under
    // `<state>/containers/<id>/`. Authority's display label only
    // carries the short id (12 hex), so match by prefix.
    let state = harness
        .fake_devcontainer_state()
        .expect("with_fake_devcontainer was set");
    let last_id_path = state.join("last_id");
    let last_id = fs::read_to_string(&last_id_path)
        .unwrap_or_else(|e| panic!("fake CLI never wrote last_id at {last_id_path:?}: {e}"));
    assert!(
        last_id.trim().starts_with(container_id) || container_id.starts_with(last_id.trim()),
        "authority short id {container_id:?} must match fake CLI's last_id {last_id:?}"
    );

    // Build log lives at `<workspace>/.fresh-cache/devcontainer-logs/build-<ts>.log`.
    // Plugin opens it in a split before `up` runs, so the file must
    // exist by the time the authority lands.
    let log_dir = workspace.join(".fresh-cache").join("devcontainer-logs");
    let log_count = fs::read_dir(&log_dir)
        .unwrap_or_else(|e| panic!("expected build-log dir at {log_dir:?}: {e}"))
        .count();
    assert!(
        log_count >= 1,
        "expected at least one build-<ts>.log under {log_dir:?}, found {log_count}"
    );

    drop_workspace_temp(&workspace);
}

/// Pin the canonicalized workspace path so the unused-let warning
/// stays out of the way; the actual TempDir cleanup is owned by the
/// caller's `_workspace_temp`.
fn drop_workspace_temp(_workspace: &Path) {}

/// Wait until the failed-attach popup has rendered. Title comes from
/// `popup.failed_attach_title` in the plugin's i18n bundle.
fn wait_for_failed_attach_popup(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Dev Container Attach Failed")
                && s.contains("Retry")
                && s.contains("Reopen Locally")
        })
        .unwrap();
}

/// Drive the attach popup from the post-`set_up_workspace` state.
fn accept_attach(harness: &mut EditorTestHarness) {
    wait_for_attach_popup(harness);
    harness
        .send_key(KeyCode::Esc, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
}

/// `FAKE_DC_UP_FAIL=1` → fake exits 1 with `error: …` on stderr →
/// plugin's `enterFailedAttach` surfaces the action popup.
#[test]
fn attach_failure_surfaces_failed_attach_popup() {
    let (_workspace_temp, workspace) = set_up_workspace();
    // Per-test env knob — the harness lock from with_fake_devcontainer
    // serializes us with other tests, so this set_var is safe.
    std::env::set_var("FAKE_DC_UP_FAIL", "1");
    std::env::set_var(
        "FAKE_DC_UP_FAIL_REASON",
        "image not found: bogus:latest",
    );

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    accept_attach(&mut harness);
    wait_for_failed_attach_popup(&mut harness);

    // Authority must NOT have flipped to a container — the failure
    // path keeps us local.
    assert!(
        !harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:"),
        "failed attach must not install a container authority; label = {:?}",
        harness.editor().authority().display_label,
    );

    // Clean up env vars so we don't leak into the next test (the
    // mutex serializes us, but the FAKE_DC_UP_FAIL flag would still
    // be set on next runs in this process if we didn't unset).
    std::env::remove_var("FAKE_DC_UP_FAIL");
    std::env::remove_var("FAKE_DC_UP_FAIL_REASON");
    drop_workspace_temp(&workspace);
}

/// `FAKE_DC_UP_BAD_JSON=1` → fake exits 0 but stdout has no parseable
/// JSON line → plugin's `parseDevcontainerUpOutput` returns null →
/// `enterFailedAttach("rebuild_parse_failed")`.
#[test]
fn attach_bad_json_surfaces_failed_attach_popup() {
    let (_workspace_temp, workspace) = set_up_workspace();
    std::env::set_var("FAKE_DC_UP_BAD_JSON", "1");

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    accept_attach(&mut harness);
    wait_for_failed_attach_popup(&mut harness);

    assert!(
        !harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:"),
        "bad-JSON failure must not install a container authority"
    );

    std::env::remove_var("FAKE_DC_UP_BAD_JSON");
    drop_workspace_temp(&workspace);
}

/// `FAKE_DC_UP_NO_CONTAINER_ID=1` → fake emits `outcome:success` JSON
/// but omits `containerId` → plugin's `buildContainerAuthorityPayload`
/// returns null → `enterFailedAttach("rebuild_missing_container_id")`.
#[test]
fn attach_missing_container_id_surfaces_failed_attach_popup() {
    let (_workspace_temp, workspace) = set_up_workspace();
    std::env::set_var("FAKE_DC_UP_NO_CONTAINER_ID", "1");

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    accept_attach(&mut harness);
    wait_for_failed_attach_popup(&mut harness);

    assert!(
        !harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:"),
        "missing-containerId failure must not install a container authority"
    );

    std::env::remove_var("FAKE_DC_UP_NO_CONTAINER_ID");
    drop_workspace_temp(&workspace);
}
