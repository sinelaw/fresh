//! Failing reproducers for spec-conformance bugs surfaced by the
//! interactive walk in this branch (see
//! `docs/internal/DEVCONTAINER_SPEC_TEST_GAPS.md`).
//!
//! These tests are intentionally **expected to fail** on master —
//! they pin the bug as a real regression that has to be fixed,
//! rather than being silently quirky behavior. Each is a `#[test]`
//! (not `#[ignore]`) so CI signals red until the corresponding
//! plugin / spawner fix lands. Once that fix exists, the test stays
//! as the regression guard.
//!
//! Bugs covered here:
//!   - **S1** — Lifecycle commands run with the host workspace path
//!     as `-w` instead of `remoteWorkspaceFolder` (the in-container
//!     path). The terminal-wrapper code path uses
//!     `remoteWorkspaceFolder`, so the two surfaces disagree.
//!   - **S2** — `remoteEnv` from `devcontainer.json` is never
//!     applied to lifecycle commands. The spec says remoteEnv
//!     "should be applied to all created processes."
//!
//! `containerEnv` is *not* a plugin bug (it's the container's job
//! at create time; real `docker exec` inherits it). The fake-CLI
//! upgrade in this branch replays containerEnv from
//! `<state>/containers/<id>/container_env`, so a `containerEnv`-
//! dependent test passes today; that's a regression guard, not a
//! reproducer — see `attach_e2e_lifecycle_sees_container_env_via_fake`
//! at the bottom.
//!
//! Asserts go through the plugin-state snapshot
//! (`plugin_manager().state_snapshot_handle()`) and on-disk probe
//! files written by the lifecycle command itself. No internal
//! popup/widget state is read.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::{Path, PathBuf};

/// Common setup for these reproducers: a workspace whose
/// `postCreateCommand` writes a probe file with three values —
/// `$PWD` (where docker exec landed it), `$FAKE_DC_REQUESTED_CWD`
/// (what the fake docker actually got via `-w`, regardless of
/// whether the path existed on the host), and the env vars we want
/// to assert on. Returns (TempDir guard, workspace path,
/// probe-file path).
fn set_up_probe_workspace(
    name: &str,
    container_env: Option<(&str, &str)>,
    remote_env: Option<(&str, &str)>,
) -> (tempfile::TempDir, PathBuf, PathBuf) {
    fresh::i18n::set_locale("en");

    let workspace_temp = tempfile::tempdir().unwrap();
    let workspace = workspace_temp.path().canonicalize().unwrap();
    let probe = workspace.join("probe.log");

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();

    let container_env_block = match container_env {
        Some((k, v)) => format!(r#"  "containerEnv": {{ "{k}": "{v}" }},
"#),
        None => String::new(),
    };
    let remote_env_block = match remote_env {
        Some((k, v)) => format!(r#"  "remoteEnv": {{ "{k}": "{v}" }},
"#),
        None => String::new(),
    };

    // The probe writes EVERY interesting env var unconditionally.
    // Tests assert on whichever ones are relevant.
    let dc_json = format!(
        r#"{{
  "name": "{name}",
  "image": "ubuntu:22.04",
  "remoteUser": "vscode",
{container_env_block}{remote_env_block}  "postCreateCommand": "{{ echo PWD=$PWD; echo REQUESTED_CWD=$FAKE_DC_REQUESTED_CWD; echo CE_TEST=${{CE_TEST-unset}}; echo RE_TEST=${{RE_TEST-unset}}; }} > {probe} 2>&1"
}}
"#,
        probe = probe.display(),
    );
    fs::write(dc.join("devcontainer.json"), dc_json).unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    (workspace_temp, workspace, probe)
}

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
    panic!(
        "bounded_wait timed out waiting for {what}. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Drive the popup → attach flow, simulate the production restart
/// the plugin's `setAuthority` triggers, and run `postCreateCommand`
/// from the lifecycle picker. Returns the probe-file content.
fn run_attach_and_postcreate(harness: &mut EditorTestHarness, probe: &Path) -> String {
    // Wait for plugin command registration (plugin loaded).
    bounded_wait(harness, "plugin command registration", |h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.name == "%cmd.run_lifecycle")
    });

    // Production paths fire `plugins_loaded` after the registry
    // settles; the harness doesn't, so we do it manually.
    harness.editor().fire_plugins_loaded_hook();

    // Wait for the "Reopen in Container?" popup, then accept.
    bounded_wait(harness, "Reopen popup", |h| {
        let s = h.screen_to_string();
        s.contains("Dev Container Detected") && s.contains("Reopen in Container")
    });
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // setAuthority stages a pending authority + signals quit;
    // production's main.rs swaps it in via set_boot_authority. We
    // do the swap inline.
    let max_iters = 200;
    for _ in 0..max_iters {
        harness.tick_and_render().unwrap();
        if let Some(auth) = harness.editor_mut().take_pending_authority() {
            harness.editor_mut().set_boot_authority(auth);
            break;
        }
        if harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:")
        {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    assert!(
        harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:"),
        "expected container authority after attach"
    );

    // Drive the lifecycle picker: Ctrl+P → "Run Lifecycle" → Enter
    // → pick postCreateCommand (Down once, since
    // initializeCommand is first in the picker).
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    bounded_wait(harness, "palette prompt open", |h| h.editor().is_prompting());
    harness.type_text("Dev Container: Run Lifecycle").unwrap();
    bounded_wait(harness, "lifecycle palette match", |h| {
        h.screen_to_string()
            .contains("Dev Container: Run Lifecycle Command")
    });
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    bounded_wait(harness, "lifecycle picker shows postCreateCommand", |h| {
        h.screen_to_string().contains("postCreateCommand")
    });
    // The plugin's picker order is: initializeCommand,
    // onCreateCommand, updateContentCommand, postCreateCommand,
    // postStartCommand, postAttachCommand. Only post* hooks defined
    // → it's at index 0 since others are absent. But to avoid
    // flake, type the name to filter to a unique match.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap(); // wipe filter (the picker shares state)
    // Type the suggestion text (some pickers filter on it).
    // Simpler: just press Enter on the default, which is
    // postCreateCommand because we only define that hook.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the probe file to land.
    bounded_wait(harness, "probe file written", |_| probe.exists());

    fs::read_to_string(probe).unwrap_or_default()
}

/// **S1 reproducer (failing on master).**
///
/// Lifecycle commands must run with `cwd = remoteWorkspaceFolder`,
/// the in-container path the spec says they live under. The plugin
/// today sends the *host* workspace path as `-w` instead, so the
/// fake's `FAKE_DC_REQUESTED_CWD` (whatever the editor passed via
/// `-w`) is the host path, not `remoteWorkspaceFolder`.
///
/// Real-world impact: when `workspaceFolder` is overridden in the
/// devcontainer.json (so the in-container path differs from the
/// host path), this fails inside a real container with "no such
/// directory."
#[test]
fn lifecycle_command_cwd_must_be_remote_workspace_folder() {
    let (_w_temp, workspace, probe) = set_up_probe_workspace("s1-cwd", None, None);
    // Fake will report `remoteWorkspaceFolder` as
    // `/workspaces/s1-cwd-distinct` — distinct from the host path.
    std::env::set_var("FAKE_DC_REMOTE_WORKSPACE", "/workspaces/s1-cwd-distinct");

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    let probe_text = run_attach_and_postcreate(&mut harness, &probe);
    std::env::remove_var("FAKE_DC_REMOTE_WORKSPACE");

    let requested = probe_text
        .lines()
        .find_map(|l| l.strip_prefix("REQUESTED_CWD="))
        .unwrap_or("")
        .to_string();
    assert_eq!(
        requested, "/workspaces/s1-cwd-distinct",
        "S1 (failing on master): lifecycle commands should pass the in-container \
         workspace as `-w` to docker exec; today they pass the host path. Probe:\n{probe_text}"
    );
}

/// **S2 reproducer (failing on master).**
///
/// `remoteEnv` from `devcontainer.json` must be applied to all
/// remote processes per the spec. The plugin reads `config.remoteEnv`
/// into the type but never propagates it through the spawner, so a
/// lifecycle command running as a remote process never sees the
/// declared vars.
#[test]
fn lifecycle_command_must_see_remote_env() {
    let (_w_temp, workspace, probe) =
        set_up_probe_workspace("s2-remote-env", None, Some(("RE_TEST", "from-remoteEnv")));

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    let probe_text = run_attach_and_postcreate(&mut harness, &probe);

    let value = probe_text
        .lines()
        .find_map(|l| l.strip_prefix("RE_TEST="))
        .unwrap_or("")
        .to_string();
    assert_eq!(
        value, "from-remoteEnv",
        "S2 (failing on master): lifecycle commands should inherit \
         `remoteEnv` per the spec; today the plugin never propagates it. \
         Probe:\n{probe_text}"
    );
}

/// **S3 / containerEnv regression guard (passes today).**
///
/// `containerEnv` is set at container creation in real
/// devcontainers; real `docker exec` inherits it. Our fake replays
/// it from `<state>/containers/<id>/container_env`, written by the
/// fake's `up` from `devcontainer.json`. So this test passes
/// today — it's the regression guard for the fake's containerEnv
/// passthrough, not a plugin-bug reproducer.
///
/// Locked in alongside S1 / S2 so a future "we removed the
/// containerEnv feature in the fake" change has to update this
/// expectation explicitly.
#[test]
fn lifecycle_command_must_see_container_env() {
    let (_w_temp, workspace, probe) =
        set_up_probe_workspace("s3-container-env", Some(("CE_TEST", "from-containerEnv")), None);

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    let probe_text = run_attach_and_postcreate(&mut harness, &probe);

    let value = probe_text
        .lines()
        .find_map(|l| l.strip_prefix("CE_TEST="))
        .unwrap_or("")
        .to_string();
    assert_eq!(
        value, "from-containerEnv",
        "S3 regression guard: fake docker exec replays containerEnv from \
         `<state>/containers/<id>/container_env`. Probe:\n{probe_text}"
    );
}
