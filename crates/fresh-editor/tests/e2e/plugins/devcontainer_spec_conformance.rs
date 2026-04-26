//! Spec-conformance tests beyond the bug reproducers in
//! `devcontainer_spec_repros.rs`. References the official spec at
//! <https://containers.dev/implementors/spec/>.
//!
//! This file mixes one **failing reproducer** (R1, parallel
//! lifecycle commands) with five **regression guards** (G1-G5)
//! covering parser/detection paths that already work today but
//! aren't otherwise tested. R1 stays red until the plugin is
//! fixed; G1-G5 are green from day one and signal a regression if
//! a future change breaks them.
//!
//! Spec coverage map (full list lives in
//! `docs/internal/DEVCONTAINER_SPEC_TEST_GAPS.md`):
//!
//!   - R1 — lifecycle "object form" parallelism. Spec: each entry
//!     in the object form runs in parallel, the stage waits for
//!     all to complete, and the stage succeeds iff every entry
//!     exited 0. Plugin runs them sequentially in a `for` loop
//!     (`devcontainer.ts:709-728`) — spec violation.
//!   - G1 — lifecycle "array form" runs the command verbatim
//!     without shell-splitting (`devcontainer.ts:700-707`).
//!   - G2 — neither `remoteUser` nor `containerUser` declared →
//!     spawner emits no `-u` flag.
//!   - G3 — only `containerUser` declared → falls back to that
//!     user (per spec; the CLI computes the fallback and emits it
//!     in the success JSON; the fake mirrors that behaviour).
//!   - G4 — JSONC config (line + block comments + trailing commas)
//!     is parsed by the plugin's `parseJsonc`.
//!   - G5 — config under `.devcontainer/<sub>/devcontainer.json`
//!     is detected by the plugin's third-priority discovery path.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::{Path, PathBuf};

/// Bounded poll loop that panics with the screen on timeout, used
/// instead of `harness.wait_until` so a bug surfaces in seconds
/// with full context rather than waiting for the test runner's
/// external timeout.
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
        "bounded_wait timed out: {what}. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Build a workspace with the given `devcontainer.json` content.
/// Returns (TempDir guard, canonicalized workspace path).
fn workspace_with_devcontainer(dc_json: &str) -> (tempfile::TempDir, PathBuf) {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();
    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(dc.join("devcontainer.json"), dc_json).unwrap();
    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");
    (temp, workspace)
}

/// Attach end-to-end: wait for plugin commands, fire
/// `plugins_loaded`, accept the popup, and apply the staged
/// authority (the production restart path the harness doesn't
/// have).
fn attach(harness: &mut EditorTestHarness) {
    bounded_wait(harness, "plugin command registration", |h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.name == "%cmd.run_lifecycle")
    });
    harness.editor().fire_plugins_loaded_hook();
    bounded_wait(harness, "Reopen popup", |h| {
        let s = h.screen_to_string();
        s.contains("Dev Container Detected") && s.contains("Reopen in Container")
    });
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let max_iters = 200;
    for _ in 0..max_iters {
        harness.tick_and_render().unwrap();
        if let Some(auth) = harness.editor_mut().take_pending_authority() {
            harness.editor_mut().set_boot_authority(auth);
            return;
        }
        if harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:")
        {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    panic!("attach never landed an authority");
}

/// Drive the lifecycle picker for the (assumed-only) postCreateCommand.
/// Waits for `probe` to materialize, returns its content. If the
/// plugin runs entries in parallel the file appears quickly; if
/// sequentially the wall clock balloons and tests can detect that.
fn run_post_create(harness: &mut EditorTestHarness, probe: &Path) -> String {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    bounded_wait(harness, "palette open", |h| h.editor().is_prompting());
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
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    bounded_wait_for_file(harness, probe, std::time::Duration::from_secs(10));
    fs::read_to_string(probe).unwrap_or_default()
}

/// Wait for `path` to materialize, ticking the harness on every
/// iteration so the plugin's pending async work (the
/// `editor.spawnProcess` Promise the lifecycle handler is awaiting)
/// can resolve. Without the tick the spawned child runs but its
/// completion message is never drained, and the post-spawn
/// `setStatus("completed")` call never happens.
fn bounded_wait_for_file(
    harness: &mut EditorTestHarness,
    path: &Path,
    deadline: std::time::Duration,
) {
    let start = std::time::Instant::now();
    while start.elapsed() < deadline {
        harness.tick_and_render().unwrap();
        if path.exists() {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(25));
        harness.advance_time(std::time::Duration::from_millis(25));
    }
    panic!(
        "file {path:?} never appeared within {deadline:?}. Screen:\n{}",
        harness.screen_to_string()
    );
}

// ============================================================================
// R1 — failing reproducer: object-form lifecycle should run in parallel.
// ============================================================================

/// **R1.** Spec: `postCreateCommand: { "a": "...", "b": "..." }`
/// runs entries in parallel. The plugin's
/// `devcontainer_on_lifecycle_confirmed` runs them in a sequential
/// `for` loop, so wall-clock = sum of entry sleeps instead of the
/// max. Test sets each entry to sleep 0.4s and asserts wall-clock
/// is well under the sequential lower bound.
///
/// Fails on master (sequential ⇒ ~0.8s+); will pass once the
/// plugin uses `Promise.all` (or the equivalent) to fan out.
#[test]
fn lifecycle_object_form_must_run_in_parallel() {
    let probe_temp = tempfile::tempdir().unwrap();
    let probe_a = probe_temp.path().join("a.touched");
    let probe_b = probe_temp.path().join("b.touched");
    let probe_done = probe_temp.path().join("done");

    // Each entry sleeps then touches its own sentinel. A trailing
    // postStartCommand-like sentinel `done` is touched by a third
    // entry so the test has a clear "everything finished" signal.
    let dc_json = format!(
        r#"{{
  "name": "r1-parallel",
  "image": "ubuntu:22.04",
  "remoteUser": "vscode",
  "postCreateCommand": {{
    "a": "sleep 0.4 && touch {a}",
    "b": "sleep 0.4 && touch {b}",
    "done": "sleep 0.5 && touch {done}"
  }}
}}
"#,
        a = probe_a.display(),
        b = probe_b.display(),
        done = probe_done.display(),
    );
    let (_w_temp, workspace) = workspace_with_devcontainer(&dc_json);

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();
    attach(&mut harness);

    let start = std::time::Instant::now();
    let _ = run_post_create(&mut harness, &probe_done);
    let elapsed = start.elapsed();

    // All three entries must have run.
    assert!(probe_a.exists(), "entry `a` never ran");
    assert!(probe_b.exists(), "entry `b` never ran");
    assert!(probe_done.exists(), "entry `done` never ran");

    // Sequential lower bound is 0.4 + 0.4 + 0.5 = 1.3s. Parallel
    // upper bound is ~0.5s plus per-entry docker-exec overhead.
    // Pick a threshold safely between the two.
    let max_parallel = std::time::Duration::from_millis(1100);
    assert!(
        elapsed < max_parallel,
        "R1 (failing on master): postCreateCommand object form should run \
         entries in parallel. Wall clock = {:?} > {:?} (sequential lower \
         bound was 1.3s, parallel upper bound ~0.5s).",
        elapsed,
        max_parallel,
    );
}

// ============================================================================
// G1-G5 — regression guards (pass today).
// ============================================================================

/// **G1.** Lifecycle command in array form: the plugin's
/// `devcontainer_on_lifecycle_confirmed` array branch
/// (`devcontainer.ts:700-707`) calls `editor.spawnProcess(bin,
/// args, ...)` with the array's first element as `bin` and the
/// rest as `args`. The test verifies a command of the form
/// `["sh", "-c", "..."]` actually runs through that path.
#[test]
fn lifecycle_array_form_executes_verbatim() {
    let probe_temp = tempfile::tempdir().unwrap();
    let probe = probe_temp.path().join("g1.sentinel");

    let dc_json = format!(
        r#"{{
  "name": "g1-array-form",
  "image": "ubuntu:22.04",
  "remoteUser": "vscode",
  "postCreateCommand": ["sh", "-c", "touch {}"]
}}
"#,
        probe.display(),
    );
    let (_w_temp, workspace) = workspace_with_devcontainer(&dc_json);

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();
    attach(&mut harness);
    let _ = run_post_create(&mut harness, &probe);

    assert!(
        probe.exists(),
        "G1: array-form lifecycle command should execute via the \
         array-branch in the plugin's lifecycle handler. Sentinel \
         file at {probe:?} never appeared."
    );
}

/// **G2.** Spec: when neither `remoteUser` nor `containerUser` is
/// declared, the spawner must not pass a `-u` flag. The fake's
/// `docker exec` only sets `FAKE_DC_USER` from `-u`; with no flag
/// the env var is empty.
#[test]
fn no_user_means_no_dash_u_flag() {
    let probe_temp = tempfile::tempdir().unwrap();
    let probe = probe_temp.path().join("g2.log");

    let dc_json = format!(
        r#"{{
  "name": "g2-no-user",
  "image": "ubuntu:22.04",
  "postCreateCommand": "echo USER_FLAG=${{FAKE_DC_USER-NONE}} > {}"
}}
"#,
        probe.display(),
    );
    let (_w_temp, workspace) = workspace_with_devcontainer(&dc_json);

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();
    attach(&mut harness);
    let probe_text = run_post_create(&mut harness, &probe);

    // FAKE_DC_USER is exported as an empty string when -u is
    // absent (the fake initializes `user=""` and only overwrites
    // it from `-u`). Empty / NONE both prove no -u was passed.
    let line = probe_text.trim();
    assert!(
        line == "USER_FLAG=" || line == "USER_FLAG=NONE",
        "G2: no remoteUser/containerUser should mean no `-u` flag. \
         Probe: {line:?}"
    );
}

/// **G3.** Spec: when `remoteUser` is unset, fall back to
/// `containerUser`. The CLI is responsible for resolving the
/// fallback before reporting `remoteUser` in the success JSON;
/// the fake mirrors that. Asserts the spawner ends up passing
/// `-u <containerUser>` and the child sees `FAKE_DC_USER=<that>`.
#[test]
fn remote_user_defaults_to_container_user() {
    let probe_temp = tempfile::tempdir().unwrap();
    let probe = probe_temp.path().join("g3.log");

    let dc_json = format!(
        r#"{{
  "name": "g3-fallback",
  "image": "ubuntu:22.04",
  "containerUser": "node",
  "postCreateCommand": "echo USER=$FAKE_DC_USER > {}"
}}
"#,
        probe.display(),
    );
    let (_w_temp, workspace) = workspace_with_devcontainer(&dc_json);

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();
    attach(&mut harness);
    let probe_text = run_post_create(&mut harness, &probe);

    assert_eq!(
        probe_text.trim(),
        "USER=node",
        "G3: with no remoteUser declared, spawner should pass \
         `-u <containerUser>`. Probe: {probe_text:?}"
    );
}

/// **G4.** JSONC support: the plugin's `parseJsonc` must accept
/// `// line comments`, `/* block comments */`, and trailing
/// commas. Asserted indirectly: if parsing failed, the popup
/// would never fire because `findConfig` would skip the file.
#[test]
fn jsonc_config_with_comments_and_trailing_commas_is_detected() {
    let dc_json = r#"{
  // Top-level comment.
  "name": "g4-jsonc",
  /* block comment
     spanning lines */
  "image": "ubuntu:22.04",
  "remoteUser": "vscode",
  "forwardPorts": [8080,], // trailing comma in array
}
"#;
    let (_w_temp, workspace) = workspace_with_devcontainer(dc_json);

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    bounded_wait(&mut harness, "plugin command registration", |h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.name == "%cmd.run_lifecycle")
    });
    harness.editor().fire_plugins_loaded_hook();
    bounded_wait(&mut harness, "Reopen popup", |h| {
        let s = h.screen_to_string();
        s.contains("Dev Container Detected") && s.contains("Reopen in Container")
    });
}

/// **G5.** Spec: the plugin discovers configs at
/// `.devcontainer/devcontainer.json`, `.devcontainer.json`, and
/// `.devcontainer/<sub>/devcontainer.json`. This test puts the
/// config under a subfolder only and asserts it's still detected.
#[test]
fn subfolder_devcontainer_json_is_detected() {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();
    // No top-level `.devcontainer/devcontainer.json` — only the subfolder one.
    let sub = workspace.join(".devcontainer").join("rust-dev");
    fs::create_dir_all(&sub).unwrap();
    fs::write(
        sub.join("devcontainer.json"),
        r#"{
  "name": "g5-subfolder",
  "image": "ubuntu:22.04",
  "remoteUser": "vscode"
}
"#,
    )
    .unwrap();
    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    bounded_wait(&mut harness, "plugin command registration", |h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.name == "%cmd.run_lifecycle")
    });
    harness.editor().fire_plugins_loaded_hook();
    bounded_wait(&mut harness, "Reopen popup", |h| {
        let s = h.screen_to_string();
        s.contains("Dev Container Detected") && s.contains("Reopen in Container")
    });
}
