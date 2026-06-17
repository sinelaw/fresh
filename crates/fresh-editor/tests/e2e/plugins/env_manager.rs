//! E2E tests for the trust + env open-time UX (see
//! `docs/internal/trust-env-devcontainer-ux-plan.md`). Each test drives
//! keyboard input and asserts only on rendered screen content — never on
//! internal state — per CONTRIBUTING.md's "observe, not inspect" rule.
//!
//! These cover four flows visible to the user when opening a folder:
//!
//! - **Path-only env (`.venv`)**: auto-activate silently, status pill shows
//!   `.venv ✓`, no popup, no trust modal.
//! - **Shell env (`.envrc`)**: the *core* trust modal (the same single prompt
//!   manifests get), with concrete framing naming `.envrc`. Picking "Trust"
//!   elevates trust and env-manager activates direnv in response — driven by
//!   the `trust_changed` hook, not a separate plugin "Trust & activate" popup.
//! - **Project manifest (`Cargo.toml`)**: trust modal fires with concrete
//!   framing that names the actual marker.
//! - **Cancel the trust modal** (T19 — Ctrl+Q quit without picking): the
//!   on-disk trust file must NOT be written, so the next open of the same
//!   folder re-fires the prompt.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Copy env-manager + its lib into the project's `plugins/` directory so the
/// plugin loads when the harness boots the editor against `working_dir`.
fn setup_env_manager(working_dir: &PathBuf) {
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "env-manager");
    copy_plugin_lib(&plugins_dir);
}

/// Synthetic `.venv/bin/python` so env-manager's `detect()` recognizes the
/// folder as a Python venv (it walks the same file checks live).
fn make_venv(root: &Path) {
    let venv = root.join(".venv").join("bin");
    fs::create_dir_all(&venv).expect("create venv");
    fs::write(
        venv.join("activate"),
        b"#!/bin/sh\nexport VIRTUAL_ENV=/tmp\n",
    )
    .expect("write activate");
    fs::write(venv.join("python"), b"").expect("write python");
}

/// `.envrc` content doesn't matter for detection (env-manager only checks
/// existence); we put something sane so an accidental shell eval doesn't
/// fail loudly during debugging.
fn make_envrc(root: &Path) {
    fs::write(root.join(".envrc"), b"export FOO=bar\n").expect("write .envrc");
}

fn make_cargo_toml(root: &Path) {
    fs::write(
        root.join("Cargo.toml"),
        b"[package]\nname = \"demo\"\nversion = \"0.0.1\"\nedition = \"2021\"\n",
    )
    .expect("write Cargo.toml");
}

/// PTY availability guard — `create_window_with_terminal` spawns a real shell,
/// which the CI sandbox occasionally can't allocate.
fn pty_available() -> bool {
    portable_pty::native_pty_system()
        .openpty(portable_pty::PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

/// Boot the editor harness exactly like a real `fresh /path` launch does
/// at startup. `EditorTestHarness::with_config_and_working_dir` skips two
/// hooks `main.rs` runs immediately after editor construction:
///
/// 1. `editor.maybe_prompt_workspace_trust()` (`main.rs:3902`) — resolves
///    the per-folder trust level. Without this, trust stays at the default
///    `Restricted`, so env-manager's `maybeAutoActivate` short-circuits on
///    the `isTrusted()` check for path-only envs and the open-time trust
///    modal never fires for project manifests.
/// 2. `editor.fire_plugins_loaded_hook()` (`main.rs:3840`) — dispatches
///    the `plugins_loaded` lifecycle event. Without this, env-manager's
///    `maybeAutoActivate` (registered on that hook) never runs at all.
///
/// Wire both here so the test surface mirrors what a user actually sees on
/// `fresh /path`.
fn boot_harness_like_main(width: u16, height: u16, project: PathBuf) -> EditorTestHarness {
    boot_with_dir_context(width, height, project, None)
}

/// Same as `boot_harness_like_main` but takes an explicit `DirectoryContext`
/// so two consecutive boots can share their data dir. Required by the
/// "cancel doesn't leak a decision" test (T19) — without a shared
/// dir_context, each harness gets its own temp `data_dir` and any
/// persisted `trust.json` from boot 1 is invisible to boot 2, hiding the
/// bug the test exists to catch.
fn boot_with_dir_context(
    width: u16,
    height: u16,
    project: PathBuf,
    dir_context: Option<DirectoryContext>,
) -> EditorTestHarness {
    let mut harness = match dir_context {
        Some(dc) => EditorTestHarness::with_shared_dir_context(
            width,
            height,
            Config::default(),
            project,
            dc,
        )
        .unwrap(),
        None => EditorTestHarness::with_config_and_working_dir(
            width,
            height,
            Config::default(),
            project,
        )
        .unwrap(),
    };
    // Wire a per-project trust store so `set_level` calls inside
    // `maybe_prompt_workspace_trust` actually persist to disk. The
    // harness builds `WorkspaceTrust::permissive()` without a store;
    // without this step, T19's persistence assertion can't tell the
    // difference between "decision recorded" and "no store wired" — both
    // look identical to the gate.
    let store_path = {
        let editor = harness.editor();
        let working_dir = editor.working_dir().to_path_buf();
        editor.dir_context().project_state_dir(&working_dir)
    };
    let store = fresh::services::workspace_trust::TrustStore::for_project_dir(&store_path);
    harness
        .editor()
        .authority()
        .workspace_trust
        .set_store(Some(store));
    harness.editor_mut().maybe_prompt_workspace_trust();
    // Republish the plugin state snapshot so JS reads the trust level
    // we just installed. Without this, `editor.workspaceTrustLevel()`
    // from the plugin returns whatever was current when the editor was
    // first constructed (the harness's permissive default), which would
    // make env-manager's `isTrusted()` check disagree with the gate's
    // actual decision.
    harness.editor_mut().update_plugin_state_snapshot();
    harness.editor_mut().fire_plugins_loaded_hook();
    harness.render().unwrap();
    harness
}

/// A bare `.venv` is executable content: activating it runs the repo's Python,
/// which auto-executes any `.pth`/`sitecustomize.py` shipped inside the venv (a
/// documented malware-drop vector), and a virtualenv is a module-namespace
/// boundary — NOT a security boundary. So a venv-only folder raises the single
/// core trust modal like any other executable content; it must NOT silently
/// auto-trust. Once trusted, the path-only env activates with no second prompt.
#[test]
fn test_venv_prompts_then_activates_on_trust() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    make_venv(&project);
    setup_env_manager(&project);

    let mut harness = boot_harness_like_main(120, 40, project);

    // venv-only raises the core trust modal naming the marker — not silently
    // trusted, not silently activated.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("SECURITY WARNING") && s.contains("Detected: .venv")
        })
        .unwrap();
    let s = harness.screen_to_string();
    assert!(
        !s.contains("Trust & activate") && !s.contains("Environment detected"),
        "venv must not surface the env-manager popup"
    );

    // Trust the folder: mnemonic 't' selects "Trust", Enter confirms.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Now the path-only env activates silently (no second prompt) via the
    // `trust_changed` hook.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("SECURITY WARNING")
                && s.contains(".venv")
                && (s.contains("Activating") || s.contains("active"))
        })
        .unwrap();
}

/// A session opened through the Orchestrator — a window created via
/// `create_window_with_terminal`, the path the "New Session (Local)" flow and
/// the session dock drive — must run the same workspace-trust decision a direct
/// `fresh <dir>` launch gets. For a `.venv` that means: raise the core trust
/// modal (a venv is executable content, not silently trusted), and once trusted
/// activate the env in the new window via the `trust_changed` hook.
///
/// Regression (issue #2355): the orchestrator window used to bypass
/// `maybe_prompt_workspace_trust` entirely, so the session never got the trust
/// decision and never activated. This drives the create-window action and
/// asserts the modal appears in the new window and that trusting activates the
/// env there.
#[test]
fn test_orchestrator_session_prompts_then_activates_venv() {
    if !pty_available() {
        eprintln!("Skipping orchestrator env activation test: PTY not available");
        return;
    }

    let tmp = TempDir::new().unwrap();
    // Launch project: plain (no env marker) so booting it activates nothing —
    // env-manager loads here and serves every window in the editor.
    let launch = tmp.path().join("launch");
    fs::create_dir_all(&launch).unwrap();
    setup_env_manager(&launch);
    // The project opened *through the orchestrator*: a path-only `.venv`.
    let venv_proj = tmp.path().join("venvproj");
    fs::create_dir_all(&venv_proj).unwrap();
    make_venv(&venv_proj);

    let mut harness = boot_harness_like_main(120, 40, launch);

    // Orchestrator "New Session (Local)" for the venv project — the same call
    // the plugin dispatcher makes for `createWindowWithTerminal`. The new
    // window is born under its own per-session local authority.
    let born = harness.editor().local_session_authority(&venv_proj);
    harness
        .editor_mut()
        .create_window_with_terminal(
            venv_proj.clone(),
            "venvproj".into(),
            Some(venv_proj.clone()),
            // A harmless long-running child so the PTY doesn't exit immediately.
            Some(vec!["sh".into(), "-c".into(), "sleep 60".into()]),
            None,
            born,
            None,
        )
        .expect("create orchestrator session window");

    // The new session gets the trust decision just like a direct launch: the
    // core modal fires in the new window, naming the marker. (Before the
    // regression fix it never appeared — the session bypassed the prompt.)
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("SECURITY WARNING") && s.contains("Detected: .venv")
        })
        .unwrap();

    // Trust it (mnemonic 't' + Enter); the path-only env then activates in the
    // new window via `trust_changed`.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("SECURITY WARNING")
                && s.contains(".venv")
                && (s.contains("Activating") || s.contains("active"))
        })
        .unwrap();
}

/// `.envrc` raises the *core* trust modal — the single trust prompt, same as
/// any other executable content — with concrete framing naming the marker.
/// Picking "Trust" elevates trust, and env-manager activates direnv in
/// response to the resulting `trust_changed` hook. There is no separate,
/// plugin-owned "Trust & activate" popup (that duplicate was removed).
#[test]
fn test_envrc_raises_core_trust_modal_and_activates_on_trust() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    make_envrc(&project);
    setup_env_manager(&project);

    let mut harness = boot_harness_like_main(140, 40, project);

    // The core trust modal fires for `.envrc`, naming the marker concretely.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("SECURITY WARNING") && s.contains("Detected: .envrc")
        })
        .unwrap();
    // The old plugin-owned combined popup must NOT appear — that's the
    // duplicate this change removed.
    let snapshot = harness.screen_to_string();
    assert!(
        !snapshot.contains("Environment detected") && !snapshot.contains("Trust & activate"),
        ".envrc must not surface the env-manager trust popup (duplicate removed)"
    );

    // Select "Trust this folder" (mnemonic `t`), then confirm with Enter.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Trusting fires `trust_changed`; env-manager activates direnv in
    // response. The activation message from `applyActivation` includes
    // "Activating direnv", and the modal is gone.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            !s.contains("SECURITY WARNING") && s.contains("Activating direnv")
        })
        .unwrap();
}

/// A project manifest (`Cargo.toml`) raises the core trust modal with
/// *concrete framing* — the body must name the actual marker so the user
/// knows why they're being asked.
#[test]
fn test_cargo_toml_raises_trust_modal_with_concrete_framing() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    make_cargo_toml(&project);
    setup_env_manager(&project);

    let mut harness = boot_harness_like_main(140, 40, project);

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("SECURITY WARNING") && s.contains("Detected: Cargo.toml")
        })
        .unwrap();
    // env popup must NOT be present — manifest-only flow is the core modal.
    let s = harness.screen_to_string();
    assert!(
        !s.contains("Environment detected"),
        "Cargo.toml-only folder must not fire the env popup"
    );
}

/// T19 — the user opens a folder that fires the trust modal, then quits
/// without picking a row (Ctrl+Q). The contract: trust must NOT be
/// recorded, so the next open re-prompts. Before the fix in
/// `0424342`, the initial-Restricted seed was being persisted, so this
/// test would observe the modal absent on second open — a clear
/// indicator that the cancel had been (incorrectly) interpreted as a
/// choice.
///
/// Critical setup detail: the two boots share a `DirectoryContext` so
/// any `trust.json` written by boot 1 is visible to boot 2. Without
/// this, each harness gets a fresh `data_dir` and the test would pass
/// even with the bug present.
#[test]
fn test_quit_cancels_trust_modal_without_recording_decision() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    make_cargo_toml(&project);
    setup_env_manager(&project);

    // Shared state dir so trust persistence (if any) survives between
    // the two harness lifetimes.
    let state_tmp = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(state_tmp.path());

    {
        let mut harness =
            boot_with_dir_context(140, 40, project.clone(), Some(dir_context.clone()));
        harness
            .wait_until(|h| {
                let s = h.screen_to_string();
                s.contains("SECURITY WARNING") && s.contains("Detected: Cargo.toml")
            })
            .unwrap();
        // Quit via Ctrl+Q. The trust modal's non-cancellable variant
        // binds the global quit key to "close + quit editor."
        harness
            .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
            .unwrap();
        // Harness drops here; equivalent to a clean exit. Any decision
        // the editor recorded is now on disk under `dir_context`.
    }

    // Boot a *second* harness with the same dir_context. The trust
    // store reads from the same `<data_dir>/workspaces/<encoded>/
    // trust.json`, so any persisted decision from boot 1 short-
    // circuits `maybe_prompt_workspace_trust`. If the modal appears
    // again, no decision was recorded — the fix is correct. If the
    // modal does NOT appear, the quit was (incorrectly) interpreted
    // as choosing Restricted.
    let mut harness2 = boot_with_dir_context(140, 40, project, Some(dir_context));
    harness2
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("SECURITY WARNING") && s.contains("Detected: Cargo.toml")
        })
        .unwrap();
}
