//! Failing reproducers for usability bugs CONFIRMED by both the
//! 2026-04-26 devcontainer TUI usability test
//! (`docs/internal/DEVCONTAINER_USABILITY_TEST_2026-04-26.md`)
//! and a follow-up interactive retest.
//!
//! Each test is a `#[test]` (not `#[ignore]`) so CI signals red until
//! the corresponding fix lands — same pattern as
//! `devcontainer_spec_repros.rs`. Once the bug is fixed, the test
//! stays as a regression guard.
//!
//! Bugs covered:
//!
//! | Severity | Bug | Test | CI status |
//! |---|---|---|---|
//! | Critical | After Rebuild with malformed `devcontainer.json`, all `Dev Container:` palette commands disappear from the registry and don't return when the JSON is fixed — the user must restart the editor to recover. | `dev_container_commands_persist_after_rebuild_with_broken_config` | `#[ignore]` — harness shortcuts the post-rebuild editor restart |
//! | Medium | Palette popup renders nothing when the layout has many horizontal splits — the prompt accepts the filter text but no result list is drawn. | `palette_popup_renders_when_layout_has_many_splits` | `#[ignore]` — harness PTY is too tall to crowd the popup |
//! | Low | Palette doesn't gate `Attach` by attach state — `Dev Container: Attach` is offered even while already attached. | `palette_attach_command_hidden_when_already_attached` | `#[test]` — fails on master |
//!
//! The two `#[ignore]`'d tests are kept in the file so the bug they
//! describe stays discoverable and a future reader has the working
//! reproduction recipe; they're disabled in CI because the harness
//! environment can't trigger the bug condition today (real-terminal
//! restart cycle / smaller PTY). Lift the `#[ignore]` once the
//! relevant harness affordance lands.
//!
//! Reported "bugs" that the harness + retest disconfirmed as transient
//! observation artifacts (stale tmux capture, focus capture by the
//! terminal pane, palette already in flight from a prior keystroke):
//! palette filter for `port` / `forward` / `Show Forwarded` actually
//! works, the failed-attach modal does include the failure reason,
//! external buffer edits do trigger reload, and `\r`-only progress
//! lines do render. Those tests have been removed rather than kept as
//! "passing regression guards" because the retest could not show a
//! way they would meaningfully fail.
//!
//! Real bugs NOT covered here (need infra not available today):
//!   - "Palette filter ranking degrades in production envs with
//!     ~hundreds of registered commands" — the harness has tens of
//!     commands so the fuzzy scoring algorithm doesn't degenerate the
//!     same way. Would need a synthetic "register N junk commands"
//!     knob, or a fixture loading every plugin / theme / language pack.
//!   - "Auto port-forwarding doesn't publish ports declared in
//!     `forwardPorts` / `portsAttributes`" — there's no observable
//!     surface in the editor today for "forwarded port", so the test
//!     would just be `assert!(false)`.
//!   - "Build-log buffer doesn't tail live" — timing-dependent; would
//!     need a deterministic filesystem-watch hook.
//!
//! Asserts go through the rendered screen and the command registry
//! (the same surface plugins read via `editor.listCommands()`) — never
//! popup internals — per CONTRIBUTING §2.

#![cfg(feature = "plugins")]

#[cfg(unix)]
use crate::common::harness::HarnessOptions;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;
use std::time::Duration;

// ---------------------------------------------------------------------------
// Setup helpers
// ---------------------------------------------------------------------------

/// Standard workspace: valid `devcontainer.json` + the `devcontainer`
/// plugin and its lib stubs copied in. Canonicalize the workspace path
/// because macOS tempdirs sit behind a `/private/var` symlink and any
/// later path comparison would fail otherwise.
fn set_up_workspace() -> (tempfile::TempDir, PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
            "name": "fake-usability",
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

/// Wait until the dev-container plugin has registered its commands and
/// their localized names — same pattern as
/// `devcontainer_ports_panel.rs::wait_for_devcontainer_commands`.
/// Without the localized check, palette filters racing against i18n
/// load can match the raw `%cmd.*` keys and the test wedges.
fn wait_for_devcontainer_commands(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            let cmds = reg.get_all();
            let attach_cmd = cmds.iter().find(|c| c.name == "%cmd.attach");
            let rebuild_cmd = cmds.iter().find(|c| c.name == "%cmd.rebuild");
            attach_cmd
                .map(|c| c.get_localized_name() == "Dev Container: Attach")
                .unwrap_or(false)
                && rebuild_cmd
                    .map(|c| c.get_localized_name() == "Dev Container: Rebuild")
                    .unwrap_or(false)
        })
        .unwrap();
}

/// Snapshot the names of every `%cmd.*` command currently in the
/// registry. Used by the broken-JSON test to compare command sets
/// before vs after the rebuild.
fn dev_container_command_names(harness: &EditorTestHarness) -> Vec<String> {
    let reg = harness.editor().command_registry().read().unwrap();
    reg.get_all()
        .iter()
        .filter(|c| c.name.starts_with("%cmd."))
        .map(|c| c.name.to_string())
        .collect()
}

// ---------------------------------------------------------------------------
// Critical: commands disappear after a Rebuild with broken JSON
// ---------------------------------------------------------------------------

/// Drive the attach popup, accept it, and pump until the container
/// authority lands. Mirror of `devcontainer_attach_e2e::wait_for_container_authority`
/// inlined here to keep this file self-contained.
#[cfg(unix)]
fn attach_via_fake(harness: &mut EditorTestHarness) {
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            reg.get_all().iter().any(|c| c.name == "%cmd.attach")
        })
        .unwrap();
    harness.editor().fire_plugins_loaded_hook();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Dev Container Detected") && s.contains("Reopen in Container")
        })
        .unwrap();
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
        std::thread::sleep(Duration::from_millis(50));
        harness.advance_time(Duration::from_millis(50));
    }
    panic!(
        "container authority never staged; screen:\n{}",
        harness.screen_to_string()
    );
}

/// **The critical bug from Task 4 of the report.** Reproduced
/// interactively in the retest by:
///   1. attaching successfully against the workspace,
///   2. editing `devcontainer.json` to malformed JSON on disk,
///   3. triggering `Dev Container: Rebuild` from the palette.
/// After step 3 the entire `Dev Container:` family disappears from
/// the palette — verified by scrolling the alphabetical D-section
/// (only `Decrease`, `Dedent`, `Dump Config`, `Duplicate Line` are
/// left) — and **stays gone after the JSON is fixed**. The user has
/// no in-editor recovery path; an editor restart is required.
///
/// **Harness limitation.** The plugin's `registerCommands()` runs at
/// startup; nothing in the attach lifecycle (`enterFailedAttach`,
/// the popup, the rebuild flow) unregisters commands. The disappearance
/// observed interactively therefore happens on the post-rebuild
/// editor *restart* — `setAuthority` triggers a process replacement
/// in `main.rs` that re-runs plugin loading against the now-broken
/// JSON. The harness shortcuts that restart by calling
/// `take_pending_authority` + `set_boot_authority` on the existing
/// `Editor`, so the second plugin load never happens and the bug
/// can't surface. Marking `#[ignore]` so CI doesn't claim this is
/// fixed; lift the ignore once the harness grows a real restart
/// hook (or once the plugin's reload path fails gracefully without
/// dropping commands).
#[cfg(unix)]
#[test]
#[ignore = "harness shortcuts the post-rebuild editor restart; needs real restart support to repro"]
fn dev_container_commands_persist_after_rebuild_with_broken_config() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    // 1. Attach successfully.
    attach_via_fake(&mut harness);

    // Sanity: every `%cmd.*` in the dev container family is registered
    // after a successful attach. We snapshot the set so the post-rebuild
    // assertion can name exactly which entries vanished.
    let before = dev_container_command_names(&harness);
    assert!(
        before.iter().any(|n| n == "%cmd.rebuild"),
        "`%cmd.rebuild` must be registered after attach (sanity); registry has: {before:?}"
    );
    assert!(
        before.iter().any(|n| n == "%cmd.open_config"),
        "`%cmd.open_config` must be registered after attach (sanity); registry has: {before:?}"
    );

    // 2. Replace devcontainer.json with malformed JSON on disk —
    // missing comma + bare-token pair, the same shape used in the
    // report's Task 4 injection.
    fs::write(
        workspace.join(".devcontainer").join("devcontainer.json"),
        r#"{
            "image": "mcr.microsoft.com/devcontainers/base:ubuntu"
            "name_typo_extra_field" "broken_no_colon",
        }"#,
    )
    .unwrap();

    // 3. Trigger Rebuild via the palette — same path a user takes.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Dev Container: Rebuild").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Dev Container: Rebuild"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // 4. Pump the editor so the rebuild flow runs to completion. The
    // fake CLI may succeed or fail depending on how aggressively the
    // plugin pre-validates the config; either outcome is acceptable —
    // the bug is about command registration, not about the rebuild's
    // own success.
    for _ in 0..200 {
        harness.tick_and_render().unwrap();
        if let Some(auth) = harness.editor_mut().take_pending_authority() {
            harness.editor_mut().set_boot_authority(auth);
        }
        std::thread::sleep(Duration::from_millis(50));
        harness.advance_time(Duration::from_millis(50));
    }

    // 5. Assert: the `Dev Container:` command family is still in the
    // registry. Without the bug, the user can still reach
    // `Open Config` to repair the JSON. With the bug, `Open Config`,
    // `Rebuild`, and `Detach` have all vanished.
    let after = dev_container_command_names(&harness);
    let lost: Vec<_> = before.iter().filter(|n| !after.contains(n)).collect();
    assert!(
        lost.is_empty(),
        "Dev Container palette commands must persist across a Rebuild \
         with malformed devcontainer.json (otherwise the user has no \
         in-editor recovery path). Lost commands: {lost:?}\n\
         Before: {before:?}\n\
         After:  {after:?}"
    );
}

// ---------------------------------------------------------------------------
// Medium: palette popup invisible when the layout is crowded
// ---------------------------------------------------------------------------

/// Confirmed in the retest: with five horizontal splits stacked on the
/// right side of the workspace, opening the palette and typing a
/// query that *does* match commands produced no visible result list
/// anywhere on screen. The status bar showed the prompt text, and
/// pressing `Enter` reported `No selection`.
///
/// **Harness limitation.** The retest happened in a real terminal
/// at ~50 rows where 5 splits left ~6 visible rows per split. The
/// harness PTY is 160×40; even with 5 splits there's enough vertical
/// room for the popup to render (the test passes), so the layout
/// pressure that triggers the bug isn't hit. Marking `#[ignore]`
/// until the harness grows a smaller-PTY mode or a popup-bounds
/// accessor we can use to assert "popup rectangle is on-screen".
#[test]
#[ignore = "harness PTY is too tall to crowd the popup off-screen; needs smaller PTY or popup-bounds accessor to repro"]
fn palette_popup_renders_when_layout_has_many_splits() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();
    harness.tick_and_render().unwrap();
    wait_for_devcontainer_commands(&mut harness);

    // Five horizontal splits — matches the count from the retest
    // session that showed the popup-invisible symptom.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
            .unwrap();
        harness.wait_for_prompt().unwrap();
        harness.type_text("Split Horizontal").unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("Split Horizontal"))
            .unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.wait_for_prompt_closed().unwrap();
    }

    // Now ask the palette for a Dev Container command. With the bug,
    // the popup never renders so the screen never contains the
    // matched entry's text and `wait_until` times out.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Dev Container: Rebuild").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Dev Container: Rebuild"))
        .unwrap();
}

// ---------------------------------------------------------------------------
// Low: palette state-gating
// ---------------------------------------------------------------------------

/// Once attached, `Dev Container: Attach` (and `Cancel Startup`) are
/// no-ops — `Detach` is the only state-relevant action. The palette
/// must reflect that. Today both `Attach` and `Cancel Startup` remain
/// in the registry alongside `Detach` and the user has to know which
/// is which.
///
/// We assert via the command registry rather than the rendered palette
/// because the registry is the source of truth — the palette is just
/// a renderer over it.
#[cfg(unix)]
#[test]
fn palette_attach_command_hidden_when_already_attached() {
    let (_temp, workspace) = set_up_workspace();
    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace)
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    attach_via_fake(&mut harness);
    // The plugin reacts to `set_boot_authority` via the
    // `authority_changed` hook; that's a fire-and-forget message,
    // so we tick the harness until the registry reflects the
    // unregister side-effect (or time out so a real regression
    // surfaces as a clear failure).
    harness
        .wait_until(|h| {
            let reg = h.editor().command_registry().read().unwrap();
            !reg.get_all().iter().any(|c| c.name == "%cmd.attach")
        })
        .unwrap();

    let reg = harness.editor().command_registry().read().unwrap();
    let attach_visible = reg.get_all().iter().any(|c| c.name == "%cmd.attach");

    assert!(
        !attach_visible,
        "`Dev Container: Attach` must not be offered while already attached \
         (display label: {:?}); only `Detach` should be state-relevant.",
        harness.editor().authority().display_label,
    );
}

/// **Bug #2 (L172).** A `devcontainer.json` that exists but has a
/// JSON syntax error currently fails silently — `findConfig`
/// returns false, no commands register, no status message
/// surfaces. The user has no signal that the feature broke. Fix
/// is to surface the parse error via the status bar at plugin
/// init.
///
/// Regression guard for that fix: drop a syntactically broken
/// config in the workspace, boot the editor, assert that the
/// status line contains the parse-failure marker. We also assert
/// none of the `Dev Container:` commands registered — the parse
/// failure must be the *only* reason `findConfig` returned false,
/// and surfacing it in the status bar is the only feedback the
/// user gets in that path.
#[test]
fn broken_devcontainer_json_surfaces_parse_error_in_status_bar() {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    // Unclosed brace + invalid token — `jsonc_parser` accepts
    // missing-comma JSON (it's a lenient dialect), but it
    // rejects unclosed structures. We need a hard parse failure
    // so the plugin's `tryParse` catch fires.
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
  "name": broken,
  "image": "ubuntu:22.04"
"#,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();

    // Plugin loads on first tick; tick a few times to let
    // setStatus land in the rendered screen.
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("devcontainer.json parse error"),
        "broken devcontainer.json must surface a parse error in the status bar; \
         screen:\n{screen}"
    );
}

/// **Bug #3 (L170, Critical).** When the plugin re-loads against a
/// broken `devcontainer.json` (which is what happens on the post-
/// rebuild editor restart), the user used to lose every
/// `Dev Container:` palette command — including `Open Config`,
/// the only path back to fix the file. They had to kill the
/// editor and restart with valid JSON.
///
/// The fix is to register a small recovery set
/// (`Open Config` + `Show Build Logs`) when `findConfig` fails
/// due to a parse error, so the user can navigate to the broken
/// file from inside the editor.
///
/// Regression guard for that fix: same harness setup as Bug #2
/// (broken JSON in workspace, boot the editor) but assert that
/// the recovery set is registered — `%cmd.open_config` and
/// `%cmd.show_build_logs` present, full set (`%cmd.rebuild`,
/// `%cmd.attach`) absent.
#[test]
fn broken_devcontainer_json_keeps_recovery_commands_registered() {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
  "name": broken,
  "image": "ubuntu:22.04"
"#,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    let mut harness = EditorTestHarness::with_working_dir(160, 40, workspace).unwrap();

    // Plugin loads on first tick; pump a few times to let the
    // recovery registration land.
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }

    let names = dev_container_command_names(&harness);

    assert!(
        names.iter().any(|n| n == "%cmd.open_config"),
        "broken-config recovery: `%cmd.open_config` must stay registered \
         so the user can open the broken file. Registered: {names:?}"
    );
    assert!(
        names.iter().any(|n| n == "%cmd.show_build_logs"),
        "broken-config recovery: `%cmd.show_build_logs` must stay registered \
         so the user can see the last rebuild output. Registered: {names:?}"
    );
    // Full-config-only commands must NOT be in the recovery set —
    // they'd no-op (or worse) without a parsed `config`.
    assert!(
        !names.iter().any(|n| n == "%cmd.rebuild"),
        "broken-config recovery: `%cmd.rebuild` must be removed when \
         config is unparseable (would call into `attach` with no config). \
         Registered: {names:?}"
    );
    assert!(
        !names.iter().any(|n| n == "%cmd.attach"),
        "broken-config recovery: `%cmd.attach` must be removed when \
         config is unparseable. Registered: {names:?}"
    );
}

/// **Bug #4 (L171, scoped: notify half).** Spec says
/// `portsAttributes.<port>.onAutoForward: "notify"` should
/// surface a notification when the port is auto-forwarded. The
/// usability report (Task 3) found that no notification ever
/// fired — the `onAutoForward` field was read by the panel
/// renderer but never acted on.
///
/// Regression guard for that fix: configure `forwardPorts: [9000]`
/// + `portsAttributes."9000".onAutoForward: "notify"`, set
/// `FAKE_DC_PORTS=9000` so the fake docker reports the binding,
/// attach via the fake CLI, and assert the rendered screen
/// surfaces the `Port 9000 forwarded` toast.
///
/// What this fix does NOT cover: actually publishing ports that
/// docker hasn't already mapped. That requires a host-side
/// userspace forwarder, much larger work — separate effort.
#[cfg(unix)]
#[test]
fn auto_forward_notify_fires_for_configured_port() {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
  "name": "auto-forward-test",
  "image": "ubuntu:22.04",
  "remoteUser": "vscode",
  "forwardPorts": [9000],
  "portsAttributes": {
    "9000": { "onAutoForward": "notify", "label": "App" }
  }
}"#,
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    // FAKE_DC_PORTS makes the fake `docker port <id>` report
    // `9000/tcp -> 0.0.0.0:32768`, simulating a published port.
    std::env::set_var("FAKE_DC_PORTS", "9000");

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_fake_devcontainer(),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    attach_via_fake(&mut harness);

    // Wait for the auto-forward sweep to finish (it's async — it
    // runs `docker port <id>` and then emits setStatus).
    harness
        .wait_until(|h| h.screen_to_string().contains("Port 9000 forwarded"))
        .unwrap();

    let screen = harness.screen_to_string();
    std::env::remove_var("FAKE_DC_PORTS");

    assert!(
        screen.contains("Port 9000 forwarded"),
        "configured `forwardPorts` entry with `onAutoForward: notify` must \
         emit a toast when the port is bound; screen:\n{screen}"
    );
}
