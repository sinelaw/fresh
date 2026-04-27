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

/// Wait for the devcontainer plugin to register its commands, then
/// fire `plugins_loaded` (mirroring `main.rs`) so the plugin's
/// `devcontainer_maybe_show_attach_prompt` handler runs and the
/// "Reopen in Container?" popup is shown. The harness doesn't fire
/// the lifecycle hook on its own — production paths
/// (`main.rs`, `gui/mod.rs`) call `fire_plugins_loaded_hook()` after
/// the registry settles, and tests that depend on the popup must do
/// the same.
///
/// Asserts via rendered screen text per CONTRIBUTING §2.
fn wait_for_attach_popup(harness: &mut EditorTestHarness) {
    bounded_wait(harness, "devcontainer plugin command registration", |h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.name == "%cmd.run_lifecycle")
    });
    harness.editor().fire_plugins_loaded_hook();
    bounded_wait(harness, "Reopen in Container popup", |h| {
        let screen = h.screen_to_string();
        screen.contains("Dev Container Detected") && screen.contains("Reopen in Container")
    });
}

/// Bounded poll loop: ticks the harness until `cond` returns true or
/// `max_iters * 50ms` elapses, panicking with the screen + plugin
/// list on timeout. Replaces `wait_until` for steps where we want
/// targeted diagnostics rather than the test-runner's external
/// timeout firing minutes later with no context.
fn bounded_wait<F>(harness: &mut EditorTestHarness, what: &str, mut cond: F)
where
    F: FnMut(&EditorTestHarness) -> bool,
{
    let max_iters = 200;
    for _ in 0..max_iters {
        harness.tick_and_render().unwrap();
        if cond(harness) {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    let plugin_names: Vec<_> = harness
        .editor()
        .plugin_manager()
        .list_plugins()
        .into_iter()
        .map(|p| p.name)
        .collect();
    panic!(
        "bounded_wait timed out: {what} not satisfied in {max_iters} ticks (~10s).\n\
         plugins loaded: {plugin_names:?}\n\
         Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Wait until the plugin stages a new authority via `setAuthority`,
/// then promote it to the active authority — the production
/// equivalent is `main.rs` `take_pending_authority` →
/// `set_boot_authority` after the editor restart drops the old
/// process. The harness has no main loop, so the test does that
/// step itself.
fn wait_for_container_authority(harness: &mut EditorTestHarness) -> String {
    let max_iters = 200; // ~10s at 50ms per tick
    for _ in 0..max_iters {
        harness.tick_and_render().unwrap();
        // The plugin stages the new authority via
        // `editor.setAuthority(payload)`, which `install_authority`
        // turns into a `pending_authority` slot plus a restart
        // request. Production's `main.rs` consumes both: it drops the
        // old editor and builds a fresh one with `set_boot_authority`.
        // The harness has no such loop, so we do the swap inline.
        if let Some(auth) = harness.editor_mut().take_pending_authority() {
            harness.editor_mut().set_boot_authority(auth);
            return harness.editor().authority().display_label.clone();
        }
        if harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:")
        {
            return harness.editor().authority().display_label.clone();
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    let plugin_names: Vec<_> = harness
        .editor()
        .plugin_manager()
        .list_plugins()
        .into_iter()
        .map(|p| p.name)
        .collect();
    panic!(
        "container authority never staged after {max_iters} ticks (~10s).\n\
         current display_label: {:?}\n\
         plugins loaded: {plugin_names:?}\n\
         Screen:\n{}",
        harness.editor().authority().display_label,
        harness.screen_to_string()
    );
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

    // The popup is on the global stack; the first action row is
    // "Reopen in Container" so a bare Enter confirms. The harness
    // doesn't simulate the default file-explorer focus that the
    // production launch path has, so we don't need an Esc to
    // release explorer focus first — sending Esc here would
    // dismiss the popup instead.
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

/// `userEnvProbe` capture is plumbed through to the docker spawner,
/// so subsequent `docker exec` invocations carry the captured env via
/// `-e KEY=VAL` flags. Without this, an LSP server installed by a
/// `postCreateCommand` into a shell-only PATH (e.g. `~/.local/bin`)
/// fails the editor's `command_exists` probe with "executable not
/// found in the active authority's PATH".
///
/// The fake docker exposes two hooks that make this checkable
/// without a real container:
///   - `FAKE_DC_PROBE_RESPONSE`: stdout for any `docker exec ... -c env`,
///     standing in for what `bash -lic env` would print inside a
///     real container.
///   - `<state>/exec_history`: tab-separated record of every `docker
///     exec` (id, semicolon-joined `-e KEY=VAL` pairs, command).
///
/// Test sequence:
///   1. Workspace + plugin set up; fake `bash -lic env` returns a
///      known PATH/HOME.
///   2. Attach via the popup; the plugin's pre-restart probe call
///      gets recorded in `exec_history` (assertion #1: probe ran).
///   3. After authority lands, exercise a `docker exec` through the
///      authority's spawner (LSP `command_exists` is the production
///      caller; we stand in by spawning a process via the plugin
///      runtime so the harness doesn't need an LSP wired up).
///   4. Assertion #2: that exec carries `PATH=...` we set up — the
///      whole point of the env-plumbing fix.
#[cfg(unix)]
#[test]
fn user_env_probe_capture_propagates_path_into_subsequent_execs() {
    use crossterm::event::KeyCode;

    let (_workspace_temp, workspace) = set_up_workspace();

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();

    // Pin the env-probe response in the per-test state dir (process
    // env vars would leak across parallel test bins). The fake docker
    // shim cats this file when invoked with `... -c env`.
    let state = harness
        .fake_devcontainer_state()
        .expect("fake state present")
        .to_path_buf();
    fs::write(
        state.join("probe_response"),
        "PATH=/home/vscode/.local/bin:/usr/local/bin:/usr/bin\nHOME=/home/vscode\nLANG=C.UTF-8\n",
    )
    .expect("write probe_response");

    harness.tick_and_render().unwrap();

    wait_for_attach_popup(&mut harness);
    harness
        .send_key(KeyCode::Enter, crossterm::event::KeyModifiers::NONE)
        .unwrap();

    let _label = wait_for_container_authority(&mut harness);

    // Read the recorded exec history. The probe call should be in
    // there: a `bash -l -i -c env` invocation against the just-up
    // container — the plugin's `captureContainerLoginEnv` runs this
    // before handing the payload to `setAuthority`.
    let history_path = state.join("exec_history");
    let history = fs::read_to_string(&history_path)
        .unwrap_or_else(|e| panic!("exec_history not found at {history_path:?}: {e}"));
    let probe_lines: Vec<_> = history
        .lines()
        .filter(|l| l.contains("bash -l -i -c env") || l.contains("bash -l -c env"))
        .collect();
    assert!(
        !probe_lines.is_empty(),
        "plugin must call `bash -lic env` to capture userEnvProbe; \
         exec_history was:\n{history}"
    );

    // Now drive a post-attach `docker exec` through the authority's
    // long-running spawner. The actual production caller is
    // `LongRunningSpawner::command_exists` (the LSP path probe).
    // Construct a fresh tokio runtime for the awaiter — the harness
    // doesn't expose its own and creating one here is cheap.
    let spawner = harness.editor().authority().long_running_spawner.clone();
    let rt = tokio::runtime::Runtime::new().expect("tokio runtime starts");
    rt.block_on(async move { spawner.command_exists("ls").await });
    drop(rt);

    // The post-attach `command -v ls` probe should carry the env
    // captured by the pre-restart `bash -lic env` call.
    let final_history = fs::read_to_string(&history_path).expect("history readable post-spawn");
    let cmd_exists_calls: Vec<_> = final_history
        .lines()
        .filter(|l| l.contains("sh -c command -v ls"))
        .collect();
    assert!(
        !cmd_exists_calls.is_empty(),
        "post-attach command_exists must have run a `command -v` probe; \
         final history:\n{final_history}"
    );
    let last = cmd_exists_calls.last().unwrap();
    assert!(
        last.contains("PATH=/home/vscode/.local/bin:/usr/local/bin:/usr/bin"),
        "command_exists probe must include the captured PATH; \
         got line: {last:?}\nfull history:\n{final_history}"
    );

    drop_workspace_temp(&workspace);
}

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
    std::env::set_var("FAKE_DC_UP_FAIL_REASON", "image not found: bogus:latest");

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

/// F1 regression: a build-log buffer left over from a previous attach
/// (the kind workspace restore brings back on cold start) must NOT
/// survive the next attach. Pre-create a stale log file under
/// `.fresh-cache/devcontainer-logs/`, open it as a buffer, then drive
/// a fresh attach. The plugin's `closeStaleBuildLogBuffers` must drop
/// the stale buffer before opening the new live log.
///
/// Asserts via `plugin_manager().state_snapshot_handle()` — the same
/// `BufferInfo` snapshot plugins read via `editor.listBuffers()` — so
/// the test exercises the plugin-facing buffer surface, not internal
/// editor state.
#[test]
fn attach_closes_stale_build_log_buffer_from_previous_run() {
    let (_workspace_temp, workspace) = set_up_workspace();

    // Pre-create the stale log: workspace-restore-style. Real
    // restores would ALSO bring the log back as an open buffer; we
    // simulate that with `harness.open_file` right after the harness
    // is built.
    let stale_dir = workspace.join(".fresh-cache").join("devcontainer-logs");
    std::fs::create_dir_all(&stale_dir).unwrap();
    let stale_log = stale_dir.join("build-2026-01-01_00-00-00.log");
    std::fs::write(
        &stale_log,
        "[+] Building 0.0s ... (from a previous attach, restored on cold start)\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();
    harness.open_file(&stale_log).unwrap();

    // Sanity: the stale log is open as a buffer before the attach.
    assert!(
        snapshot_has_buffer_at(&harness, &stale_log),
        "test setup: stale log must be open as a buffer before attach.\n\
         buffers: {:?}",
        snapshot_buffer_paths(&harness)
    );

    accept_attach(&mut harness);
    let _ = wait_for_container_authority(&mut harness);
    harness.tick_and_render().unwrap();

    let paths_after = snapshot_buffer_paths(&harness);
    assert!(
        !paths_after.iter().any(|p| p == &stale_log),
        "F1 regression: stale build-log buffer at {stale_log:?} must be \
         closed when a new attach starts. Buffers after attach: {paths_after:?}"
    );

    // The fresh build log should be open under the same dir, but at
    // a *different* path (timestamp differs from the stale one).
    let fresh = paths_after
        .iter()
        .find(|p| p.starts_with(&stale_dir) && **p != stale_log);
    assert!(
        fresh.is_some(),
        "expected at least one fresh build-log buffer under {stale_dir:?} \
         (different from {stale_log:?}). Buffers: {paths_after:?}"
    );
}

/// Read every buffer's `path` from the plugin-state snapshot — same
/// surface plugins see via `editor.listBuffers()`. Only paths that
/// resolve to a `Some(PathBuf)` are returned (unnamed buffers
/// dropped).
fn snapshot_buffer_paths(harness: &EditorTestHarness) -> Vec<std::path::PathBuf> {
    let handle = harness
        .editor()
        .plugin_manager()
        .state_snapshot_handle()
        .expect("plugin manager must have a state snapshot in plugins-feature builds");
    let snap = handle.read().unwrap();
    snap.buffers
        .values()
        .filter_map(|b| b.path.clone())
        .collect()
}

fn snapshot_has_buffer_at(harness: &EditorTestHarness, path: &Path) -> bool {
    snapshot_buffer_paths(harness).iter().any(|p| p == path)
}

/// F2 reproducer: a successful attach must persist the
/// `attach:<cwd> = "attached"` per-workspace decision so the
/// "Reopen in Container?" popup doesn't re-fire on the next cold
/// start. We assert via `Editor::capture_workspace()` — the same
/// `plugin_global_state` blob that the workspace serializer writes
/// to disk on quit and reads back on relaunch.
///
/// The plugin writes the decision in `devcontainer_on_attach_popup`
/// before kicking off `runDevcontainerUp`, so by the time the
/// container authority lands the key must be visible in the
/// captured state. If this test ever starts failing, the regression
/// is in either the plugin's call ordering (pre-`setAuthority`) or
/// in how `capture_workspace` snapshots `plugin_global_state` — the
/// production cold-restart bug from the test plan would surface
/// here first.
#[test]
fn attach_decision_persists_in_plugin_global_state() {
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

    accept_attach(&mut harness);
    let _ = wait_for_container_authority(&mut harness);
    harness.tick_and_render().unwrap();

    let workspace_state = harness.editor().capture_workspace();
    let dc_state = workspace_state
        .plugin_global_state
        .get("devcontainer")
        .unwrap_or_else(|| {
            panic!(
                "expected `devcontainer` plugin to have written global state. \
                 Plugin map: {:?}",
                workspace_state
                    .plugin_global_state
                    .keys()
                    .collect::<Vec<_>>()
            )
        });

    let key = format!("attach:{}", workspace.display());
    let value = dc_state.get(&key).unwrap_or_else(|| {
        panic!(
            "expected key {key:?} in devcontainer plugin state. \
             Keys present: {:?}",
            dc_state.keys().collect::<Vec<_>>()
        )
    });
    assert_eq!(
        value.as_str(),
        Some("attached"),
        "attach decision must be \"attached\" after a successful \
         Reopen-in-Container; got {value:?}"
    );
}

/// **Follow-up to PR #1704**: the attach prompt used to offer a single
/// `Ignore` action. User feedback was that they need _two_ separate
/// dismissals: a session-only "not now" (re-asks next launch) and a
/// permanent "stop asking" (persisted). Pin the new three-action
/// shape and the side-effects of each:
///   - `Ignore (once)` → no plugin global state writes; popup not
///     re-shown in this session, but next editor launch re-asks.
///   - `Ignore (always …)` → writes `attach:<cwd> = "dismissed"`
///     to plugin global state, persisted across launches.
#[test]
fn attach_popup_offers_separate_once_and_always_dismiss() {
    use crossterm::event::{KeyCode, KeyModifiers};

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
    wait_for_attach_popup(&mut harness);

    // Both new option labels must appear on the rendered popup.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ignore (once)"),
        "popup must offer session-only dismiss option. Screen:\n{screen}"
    );
    assert!(
        screen.contains("Ignore (always"),
        "popup must offer permanent dismiss option. Screen:\n{screen}"
    );

    // Pick "Ignore (once)" — second row in the popup. Down arrow
    // moves the focus, Enter activates.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.tick_and_render().unwrap();

    // After "Ignore (once)" the plugin must NOT have written a
    // persistent dismissal — re-launches should re-ask.
    let workspace_state = harness.editor().capture_workspace();
    let dc_state = workspace_state.plugin_global_state.get("devcontainer");
    let key = format!("attach:{}", workspace.display());
    let persisted = dc_state.and_then(|m| m.get(&key));
    assert!(
        persisted.is_none() || persisted.and_then(|v| v.as_str()) != Some("dismissed"),
        "Ignore (once) must NOT persist `dismissed` to plugin global state. \
         Got: {persisted:?}"
    );
}

/// **Follow-up to PR #1704**: pin the persistent dismissal path.
/// Picking `Ignore (always in this folder)` writes `dismissed` to
/// plugin global state so a subsequent editor launch finds it and
/// skips the popup.
#[test]
fn attach_popup_dismiss_always_persists_decision() {
    use crossterm::event::{KeyCode, KeyModifiers};

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
    wait_for_attach_popup(&mut harness);

    // Pick the third row: `Ignore (always in this folder)`.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.tick_and_render().unwrap();

    let workspace_state = harness.editor().capture_workspace();
    let dc_state = workspace_state
        .plugin_global_state
        .get("devcontainer")
        .unwrap_or_else(|| {
            panic!(
                "Ignore (always …) must write to plugin global state. \
                 Plugin map: {:?}",
                workspace_state
                    .plugin_global_state
                    .keys()
                    .collect::<Vec<_>>()
            )
        });
    let key = format!("attach:{}", workspace.display());
    let value = dc_state.get(&key).unwrap_or_else(|| {
        panic!(
            "expected key {key:?} after Ignore (always). Keys present: {:?}",
            dc_state.keys().collect::<Vec<_>>()
        )
    });
    assert_eq!(
        value.as_str(),
        Some("dismissed"),
        "Ignore (always …) must persist as \"dismissed\"; got {value:?}"
    );
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
