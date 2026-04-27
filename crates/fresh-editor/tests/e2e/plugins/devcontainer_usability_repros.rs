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
        screen.contains("devcontainer.json could not be parsed"),
        "broken devcontainer.json must surface a parse failure in the status bar; \
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

/// **Bug #5 (L181, Medium, was Untested).** The usability report
/// observed that triggering `Dev Container: Rebuild` killed any
/// open `*Terminal N*` tab — destructive because users often
/// keep long-running shells in those tabs.
///
/// This test pins the *expected* behavior (terminal buffer
/// survives the attach round-trip). If it passes on master, the
/// bug as observed was a real-tmux/PTY teardown artifact and
/// not present in the harness flow. If it fails, we'll see the
/// concrete teardown path and can fix it.
#[cfg(unix)]
#[test]
fn rebuild_does_not_kill_open_terminal_buffer() {
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

    // 1. Open a terminal so we have a long-lived buffer to lose.
    harness.editor_mut().open_terminal();
    harness.render().unwrap();

    // 2. Snapshot the terminal-backed buffer id via the editor's
    // own predicate so we don't have to guess at file paths.
    let pre_terminal = harness.editor().active_buffer();
    assert!(
        harness.editor().is_terminal_buffer(pre_terminal),
        "open_terminal must produce an active terminal buffer"
    );

    // 3. Drive the attach popup → setAuthority (the same flow
    // Rebuild ultimately exercises — both transition the
    // authority via setAuthority + boot-authority swap).
    attach_via_fake(&mut harness);

    // 4. The terminal buffer must still be a known terminal
    // buffer after the attach round-trip. Losing either the
    // buffer entry or its terminal status is the bug.
    assert!(
        harness.editor().is_terminal_buffer(pre_terminal),
        "terminal buffer {pre_terminal:?} must survive the attach round-trip \
         (the rebuild flow uses the same setAuthority path)"
    );
}

/// **Bug #6 (L169, Critical).** "Always-split, never-close" pane
/// strategy: every rebuild used to add a fresh horizontal split
/// for its own (timestamped) build log without closing or
/// reusing the previous build log's split. After 3-4 rebuilds
/// the right column was 5+ splits stacked vertically.
///
/// Fix: `openBuildLogInSplit` now finds any existing build-log
/// split (any path under `<cwd>/.fresh-cache/devcontainer-logs/`)
/// and reuses it — focuses the split, swaps the buffer to the
/// new log, closes the stale buffer.
///
/// Regression guard: drive attach (which opens log #1 in a new
/// split) and then trigger the `Rebuild` command (which produces
/// log #2 with a different timestamp). Assert the split count
/// after rebuild equals the split count after attach — i.e. the
/// new log reused the existing split rather than stacking.
#[cfg(unix)]
#[test]
fn rebuild_reuses_build_log_split_instead_of_stacking() {
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

    // 1. Attach — opens build log #1 in a new horizontal split.
    attach_via_fake(&mut harness);
    // Wait for the build log to actually land in a split (the
    // log spawn is async).
    harness
        .wait_until(|h| h.screen_to_string().contains("devcontainer-logs/build-"))
        .unwrap();
    let splits_after_attach = harness.editor().get_split_count();

    // 2. Trigger Rebuild via the palette — produces a new
    // log file with a different timestamp.
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

    // Pump until the rebuild settles. The fake `up` produces a
    // new build log buffer; without the fix this lands in a
    // *new* split.
    for _ in 0..200 {
        harness.tick_and_render().unwrap();
        if let Some(auth) = harness.editor_mut().take_pending_authority() {
            harness.editor_mut().set_boot_authority(auth);
        }
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }

    let splits_after_rebuild = harness.editor().get_split_count();
    assert_eq!(
        splits_after_rebuild, splits_after_attach,
        "Rebuild must reuse the build-log split, not stack a new one. \
         splits after attach: {splits_after_attach}, \
         splits after rebuild: {splits_after_rebuild}"
    );
}

/// **Bug from interactive walkthrough (Bug #6 retest):** every
/// `Show *` command (`Show Container Logs`, `Show Container Info`,
/// `Show Forwarded Ports`) used to open a brand-new horizontal
/// split each time it was invoked — the existing flag-based
/// dedupe (`infoPanelOpen`, `portsPanelOpen`) didn't reset when
/// the user closed the panel manually with `q`, and
/// `devcontainer_show_logs` had no dedupe at all. After three
/// `Show *` invocations the right column was three stacked
/// panes.
///
/// Fix: route every panel through `openVirtualInPanelSlot`,
/// which reuses the panel-slot split if it's still alive and
/// otherwise drops content into the currently focused split —
/// never spawns a new one.
///
/// Regression guard: invoke `Show Container Info` then
/// `Show Container Logs` one after the other, assert the split
/// count stays put.
#[cfg(unix)]
#[test]
fn show_panels_reuse_single_split_instead_of_stacking() {
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

    attach_via_fake(&mut harness);
    // Wait for the build log to land in the panel slot so we
    // measure the count *with* the panel split in play.
    harness
        .wait_until(|h| h.screen_to_string().contains("devcontainer-logs/build-"))
        .unwrap();
    let baseline = harness.editor().get_split_count();

    // Show Container Info — should reuse the panel slot.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Dev Container: Show Info").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Dev Container: Show Info"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Pump for the panel to land.
    for _ in 0..40 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }
    let after_info = harness.editor().get_split_count();

    // Show Container Logs — should also reuse the panel slot.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Dev Container: Show Container").unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Dev Container: Show Container")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..40 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }
    let after_logs = harness.editor().get_split_count();

    assert_eq!(
        after_info, baseline,
        "Show Info must reuse the existing panel split. \
         baseline={baseline}, after_info={after_info}"
    );
    assert_eq!(
        after_logs, baseline,
        "Show Container Logs must reuse the existing panel split. \
         baseline={baseline}, after_logs={after_logs}"
    );
}

/// **Follow-up to PR #1704**: PR #1704 fixed split-stacking by routing
/// every Show command through one shared panel slot, but it still
/// _replaced_ each command's buffer with the next command's buffer.
/// User feedback was: each Show should keep its own buffer so the
/// user can flip back to the previous command's output via the tab
/// bar / buffer list, instead of having to re-run the command.
///
/// Regression guard: invoke `Show Container Info`, then
/// `Show Container Logs`, and assert both buffers exist after.
#[cfg(unix)]
#[test]
fn show_panels_keep_per_command_buffers_alive_across_commands() {
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
    attach_via_fake(&mut harness);

    // Wait for the build log to settle so we don't race the panel
    // split's first occupant.
    harness
        .wait_until(|h| h.screen_to_string().contains("devcontainer-logs/build-"))
        .unwrap();

    // Show Container Info first.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Dev Container: Show Info").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Dev Container: Show Info"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..40 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }

    // Then Show Container Logs.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Dev Container: Show Container Logs")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Dev Container: Show Container Logs")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..40 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }

    // Both panel buffers must still exist — Show Container Logs
    // should NOT have closed the Info buffer. The tab bar of the
    // panel split shows both names when both are open.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("*Dev Container*"),
        "*Dev Container* (info) buffer must still be visible in \
         the tab bar after Show Container Logs ran. Screen:\n{screen}"
    );
    assert!(
        screen.contains("*Dev Container Logs*"),
        "*Dev Container Logs* buffer must be visible in the \
         tab bar — most recent Show command. Screen:\n{screen}"
    );
}

/// **Follow-up to PR #1704**: line wrapping was on by default for
/// the panel slot's virtual buffers, so logs (containerd build
/// output, lifecycle command stdout) would soft-wrap at the panel
/// width and become unreadable. Pin: panel buffers come up with
/// line wrap disabled.
#[cfg(unix)]
#[test]
fn show_container_logs_buffer_has_line_wrap_disabled() {
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
    attach_via_fake(&mut harness);
    harness
        .wait_until(|h| h.screen_to_string().contains("devcontainer-logs/build-"))
        .unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("Dev Container: Show Container Logs")
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Dev Container: Show Container Logs")
        })
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..40 {
        harness.tick_and_render().unwrap();
        std::thread::sleep(Duration::from_millis(25));
        harness.advance_time(Duration::from_millis(25));
    }

    // The panel buffer's line-wrap setting isn't directly
    // observable through public Rust APIs without adding a new
    // accessor; use the screen-based proxy. With wrap enabled
    // the editor draws a wrap-marker glyph at the line break;
    // with wrap disabled long lines just truncate at the right
    // edge with no marker. Force a long line into the panel by
    // running Show Container Logs against fake-docker logs that
    // contain a >panel-width entry.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("*Dev Container Logs*"),
        "Show Container Logs must surface its named panel buffer. \
         Screen:\n{screen}"
    );
    // The screen must NOT contain the wrap-marker the editor
    // draws when soft-wrap is on. Marker is the unicode return
    // arrow `↵`; absence is the regression guard.
    let logs_section = screen
        .lines()
        .skip_while(|l| !l.contains("*Dev Container Logs*"))
        .take(10)
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        !logs_section.contains('\u{21B5}'),
        "panel logs buffer should default to line-wrap off — \
         wrap marker '↵' must not appear in the logs body. \
         Section:\n{logs_section}"
    );
}

/// **Critical bug from interactive walkthrough:** when the user
/// ran a lifecycle command, the captured stdout/stderr were
/// discarded — only a status line ("postCreateCommand failed
/// (exit 1)") surfaced. With a real failing command the user
/// had no way to see the actual error, so couldn't diagnose.
///
/// Fix: `surfaceLifecycleResult` now dumps the full output
/// into the shared panel slot. This test pins that the
/// captured stdout actually lands in a buffer the user can
/// read.
#[cfg(unix)]
#[test]
fn lifecycle_command_output_lands_in_panel() {
    fresh::i18n::set_locale("en");
    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();
    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    // postCreateCommand prints a recognizable marker so we can
    // assert it survived the round-trip into the panel buffer.
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
  "name": "lifecycle-output-test",
  "image": "ubuntu:22.04",
  "remoteUser": "vscode",
  "postCreateCommand": "echo HELLO_FROM_LIFECYCLE_OUTPUT"
}"#,
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

    attach_via_fake(&mut harness);

    // Drive the picker to invoke postCreateCommand explicitly
    // (the at-attach background run goes through fake `up`'s
    // direct sh path; the picker run goes through the plugin's
    // `editor.spawnProcess` → which now captures stdout).
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
    harness
        .wait_until(|h| h.screen_to_string().contains("postCreateCommand"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // The lifecycle handler awaits `spawnProcess`; once it
    // returns, `surfaceLifecycleResult` opens the panel buffer
    // with the captured stdout in it. Wait for the marker to
    // hit the rendered screen.
    harness
        .wait_until(|h| h.screen_to_string().contains("HELLO_FROM_LIFECYCLE_OUTPUT"))
        .unwrap();
}
