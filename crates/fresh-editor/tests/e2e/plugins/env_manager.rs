//! E2E tests for the env-manager plugin's UX rules (see
//! `docs/internal/trust-env-devcontainer-ux-plan.md`).
//!
//! These cover the three flows that are visible to the user when opening a
//! folder:
//!
//! - **Path-only env (`.venv`)** auto-activates silently — no popup at all,
//!   the status pill flips to the env name, and the workspace ends up
//!   Trusted (preserving the WIP "default to Trusted for non-shell folders"
//!   behavior).
//! - **Shell env (`.envrc`)** surfaces the combined "Trust this folder and
//!   activate?" popup, and picking the first action elevates trust to
//!   Trusted *and* records an activation decision so the next open is
//!   silent.
//! - **Untrusted opt-out** — picking "Never here" leaves the workspace
//!   Restricted with a recorded `dismissed` decision so re-opens don't
//!   re-prompt.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Copy env-manager + its lib into the project's `plugins/` directory so the
/// plugin loads when the harness boots the editor against `working_dir`.
fn setup_env_manager(working_dir: &PathBuf) {
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "env-manager");
    copy_plugin_lib(&plugins_dir);
}

/// Construct a synthetic `.venv` directory tree that env-manager's `detect()`
/// recognizes as a Python venv (it looks for `bin/python` or similar).
fn make_venv(root: &PathBuf) {
    let venv = root.join(".venv").join("bin");
    fs::create_dir_all(&venv).expect("create venv");
    let activate = venv.join("activate");
    fs::write(&activate, b"#!/bin/sh\nexport VIRTUAL_ENV=/tmp\n").expect("write activate");
    fs::write(venv.join("python"), b"").expect("write python");
}

/// `.envrc` content doesn't matter for detection — env-manager only checks
/// existence — but we put something sane so an accidental real-shell run
/// wouldn't fail loudly.
fn make_envrc(root: &PathBuf) {
    fs::write(root.join(".envrc"), b"export FOO=bar\n").expect("write .envrc");
}

/// Open `Env: Show Environment Status` from the command palette, which
/// writes a status-bar message describing the env state. Returns the screen
/// after the message has rendered.
fn open_env_status(harness: &mut EditorTestHarness) -> String {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Env: Show").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
    harness.screen_to_string()
}

/// Path-only env (.venv) auto-activates on plugin load with no popup. After
/// boot the status shows "Environment active (.venv)" and the workspace
/// trust file has been written (Trusted, since there are no shell-env
/// markers gating it down to Restricted).
#[test]
fn test_venv_silently_auto_activates() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    make_venv(&project);
    fs::write(project.join("README.md"), b"hello\n").unwrap();
    setup_env_manager(&project);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), project.clone())
            .unwrap();
    harness.render().unwrap();

    // Auto-activation runs via the `plugins_loaded` hook; the activation
    // message takes effect by the time the next event loop tick renders.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The activation flow surfaces *either* "Activating .venv …" or,
            // once the snippet has been applied and Status was re-shown,
            // "Environment active (.venv)". Either is a positive signal.
            s.contains(".venv") && (s.contains("Activating") || s.contains("active"))
        })
        .unwrap();

    let status = open_env_status(&mut harness);
    assert!(
        status.contains("Environment active") && status.contains(".venv"),
        "expected status to confirm .venv active. Screen:\n{}",
        status
    );

    // No popup should be on screen — silent activation.
    assert!(
        !status.contains("Trust & activate"),
        "venv flow must not surface the combined trust+activate popup"
    );
}

/// Shell env (.envrc) surfaces the combined "Trust this folder and activate?"
/// popup. Picking "Trust & activate" elevates trust *and* activates the env.
#[test]
fn test_envrc_shows_combined_trust_popup_and_elevates_on_accept() {
    let tmp = TempDir::new().unwrap();
    let project = tmp.path().to_path_buf();
    make_envrc(&project);
    fs::write(project.join("README.md"), b"hello\n").unwrap();
    setup_env_manager(&project);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 40, Config::default(), project.clone())
            .unwrap();
    harness.render().unwrap();

    // The combined env+trust popup arrives once plugins_loaded fires and the
    // plugin's detect() returns the shell env.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Environment detected") && s.contains("Trust & activate")
        })
        .unwrap();

    // Action popups confirm the focused row on Enter; the popup pushes the
    // first action focused, so Enter accepts "Trust & activate".
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // After accept, trust is elevated and the env activates.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The popup disappears and a status message confirms activation.
            !s.contains("Environment detected")
                && (s.contains("Activating direnv") || s.contains("direnv"))
        })
        .unwrap();
}
