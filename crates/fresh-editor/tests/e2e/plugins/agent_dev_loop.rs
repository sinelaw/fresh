//! The headless author → reload → run loop an agent needs to customize Fresh
//! without a human at the keyboard.
//!
//! An agent running in one of Fresh's own embedded terminals can already
//! *write* `~/.config/fresh/init.ts` and syntax-check it. What it could not do
//! was exercise the result: reload was a palette command, and there was no way
//! to invoke a registered command outside the palette. Every iteration
//! therefore ended with "now press Ctrl+P and reload" — which is precisely the
//! step a one-shot customization cannot include.
//!
//! These tests drive the two APIs that close that loop (`editor.reloadInit()`
//! and `editor.runCommand()`) *through JavaScript*, from a real init.ts, so
//! the binding layer is covered and not just the Rust handlers underneath it.
//! `fresh --cmd init reload` and `fresh --cmd command run` are thin wrappers
//! that submit exactly these calls over the script channel.

use crate::common::harness::EditorTestHarness;
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// A harness whose `config_dir` is a scratch directory, so the tests write a
/// real init.ts without touching the developer's own.
fn harness_with_scratch_config_dir() -> (EditorTestHarness, tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();

    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();

    let harness = EditorTestHarness::with_shared_dir_context(
        80,
        24,
        Default::default(),
        working_dir,
        dir_context,
    )
    .expect("harness");
    (harness, temp, config_dir)
}

fn write_init_ts(config_dir: &Path, body: &str) {
    fs::write(config_dir.join("init.ts"), body).unwrap();
}

/// Pump the editor until `done` holds, or give up.
///
/// Real `std::thread::sleep`, not the harness's logical clock: what we are
/// waiting on is another OS thread (the plugin runtime) resolving a promise
/// and sending commands back, and advancing logical time does not make that
/// happen. The bound is generous because a slow CI box is the only thing that
/// should ever approach it; a passing run settles in a few iterations.
fn pump_until(harness: &mut EditorTestHarness, done: impl Fn(&EditorTestHarness) -> bool) -> bool {
    for _ in 0..300 {
        harness.editor_mut().process_async_messages();
        if done(harness) {
            return true;
        }
        std::thread::sleep(Duration::from_millis(10));
    }
    // One last drain, so a result that landed during the final sleep counts.
    harness.editor_mut().process_async_messages();
    done(harness)
}

/// Give the plugin thread a brief chance to produce commands, and drain them.
///
/// Used where there is no observable condition to wait on — after dispatching
/// a command whose only effect is to arm a timer, say. Short and fixed: the
/// tests that assert on an outcome use `pump_until`, so this only has to be
/// long enough that the plugin thread's reply has crossed the channel.
fn settle(harness: &mut EditorTestHarness) {
    for _ in 0..20 {
        harness.editor_mut().process_async_messages();
        std::thread::sleep(Duration::from_millis(5));
    }
    harness.editor_mut().process_async_messages();
}

fn status(harness: &EditorTestHarness) -> String {
    harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default()
}

/// `editor.listCommands()` reports a command the same init.ts just registered,
/// and `editor.runCommand()` dispatches it by the name the palette displays.
///
/// Both halves matter to an agent: `listCommands` is how it confirms
/// registration landed (instead of guessing at why "the palette doesn't show
/// it"), and `runCommand` is how it tests the handler through the real
/// dispatch path rather than by calling the exported function directly.
#[test]
fn list_commands_sees_a_freshly_registered_command_and_run_command_dispatches_it() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();

        globalThis.agent_probe_handler = function () {
            editor.setStatus("AGENT_PROBE_RAN");
        };

        editor.registerCommand(
            "Agent Probe",
            "a command an agent registers and then tests",
            "agent_probe_handler",
            null
        );

        (async () => {
            const names = (await editor.listCommands()).map(c => c.name);
            if (names.indexOf("Agent Probe") < 0) {
                editor.setStatus("AGENT_PROBE_NOT_LISTED");
                return;
            }
            await editor.runCommand("Agent Probe");
        })();
        "#,
    );

    harness.editor_mut().load_init_script(true);
    let ran = pump_until(&mut harness, |h| {
        let s = status(h);
        s.contains("AGENT_PROBE_RAN") || s.contains("AGENT_PROBE_NOT_LISTED")
    });

    assert!(
        ran,
        "listCommands()/runCommand() never settled; last status = {:?}",
        status(&harness)
    );
    assert!(
        status(&harness).contains("AGENT_PROBE_RAN"),
        "the command registered by init.ts should be listed and then dispatched \
         by its palette name; status = {:?}",
        status(&harness)
    );
}

/// `runCommand` rejects — rather than silently doing nothing — when no command
/// carries the given name.
///
/// A no-op would be the worst outcome for an agent: it would read "the command
/// ran" and go looking for the bug in its handler, when the real problem is
/// that registration never happened or the name is spelled differently.
#[test]
fn run_command_rejects_an_unknown_name() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        (async () => {
            try {
                await editor.runCommand("No Such Command At All");
                editor.setStatus("UNKNOWN_RESOLVED");
            } catch (e) {
                editor.setStatus("UNKNOWN_REJECTED");
            }
        })();
        "#,
    );

    harness.editor_mut().load_init_script(true);
    let settled = pump_until(&mut harness, |h| status(h).contains("UNKNOWN_"));

    assert!(
        settled,
        "runCommand() with an unknown name never settled; status = {:?}",
        status(&harness)
    );
    assert!(
        status(&harness).contains("UNKNOWN_REJECTED"),
        "an unknown command name must reject, not resolve; status = {:?}",
        status(&harness)
    );
}

/// `editor.setInterval` keeps firing when it was armed from inside a command
/// handler — the shape a panel's "open" command actually has.
///
/// Worth pinning separately from the plain case: the handler that arms the
/// timer has itself returned by the time the first tick is due, so nothing on
/// the plugin side is holding the schedule open. The host is, which is the
/// point of the primitive.
#[test]
fn set_interval_started_inside_a_command_handler_keeps_firing() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        globalThis.tick_count = 0;

        globalThis.panel_tick = function () {
            globalThis.tick_count += 1;
            editor.setStatus("TICKS=" + globalThis.tick_count);
        };

        // Started from a *command handler*, not from module top-level.
        globalThis.open_panel = function () {
            globalThis.panel_timer = editor.setInterval(50, "panel_tick");
        };

        editor.registerCommand("Open Panel", "open the panel", "open_panel", null);
        "#,
    );

    harness.editor_mut().load_init_script(true);
    // Let init.ts finish registering before the command is dispatched.
    settle(&mut harness);

    harness
        .editor_mut()
        .handle_plugin_command(fresh_core::api::PluginCommand::RunEditorCommand {
            name: "Open Panel".to_string(),
            callback_id: fresh_core::api::JsCallbackId::new(9_100),
        })
        .expect("dispatching the open command should not error");
    // Let the setInterval command reach the editor.
    settle(&mut harness);

    // Drive logical time forward one period at a time. The harness's clock is
    // the editor's clock, so this is deterministic rather than a race with
    // wall-clock.
    for _ in 0..3 {
        harness.advance_time(Duration::from_millis(60));
        harness.tick_and_render().expect("tick");
        settle(&mut harness);
    }

    let s = status(&harness);
    assert!(
        s.starts_with("TICKS="),
        "a timer created inside a command handler must still fire; status = {s:?}"
    );
    let count: u32 = s.trim_start_matches("TICKS=").parse().unwrap_or(0);
    assert!(
        count >= 3,
        "the timer should keep firing, not fire once: saw {count} ticks (status = {s:?})"
    );
}

/// `clearInterval` stops a running timer.
#[test]
fn clear_interval_stops_the_timer() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        globalThis.tick_count = 0;

        globalThis.counting_tick = function () {
            globalThis.tick_count += 1;
            editor.setStatus("TICKS=" + globalThis.tick_count);
        };

        globalThis.stop_ticking = function () {
            editor.clearInterval(globalThis.the_timer);
        };

        globalThis.the_timer = editor.setInterval(50, "counting_tick");
        editor.registerCommand("Stop Ticking", "stop", "stop_ticking", null);
        "#,
    );

    harness.editor_mut().load_init_script(true);
    settle(&mut harness);

    for _ in 0..2 {
        harness.advance_time(Duration::from_millis(60));
        harness.tick_and_render().expect("tick");
        settle(&mut harness);
    }
    let before = status(&harness);
    assert!(
        before.starts_with("TICKS="),
        "the timer should have fired at least once; status = {before:?}"
    );

    harness
        .editor_mut()
        .handle_plugin_command(fresh_core::api::PluginCommand::RunEditorCommand {
            name: "Stop Ticking".to_string(),
            callback_id: fresh_core::api::JsCallbackId::new(9_200),
        })
        .expect("dispatching the stop command should not error");
    settle(&mut harness);

    for _ in 0..3 {
        harness.advance_time(Duration::from_millis(60));
        harness.tick_and_render().expect("tick");
        settle(&mut harness);
    }

    assert_eq!(
        status(&harness),
        before,
        "no further ticks should land after clearInterval"
    );
}

/// A reload cancels the previous copy's timers.
///
/// Without this, the inner loop of plugin development — edit, reload, look —
/// accumulates one extra live timer per iteration, all firing into handlers
/// from versions of the code the author has already replaced.
#[test]
fn reloading_init_cancels_the_previous_copys_timers() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    // v1 ticks, and can reload itself on command.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        globalThis.v1_ticks = 0;

        globalThis.v1_tick = function () {
            globalThis.v1_ticks += 1;
            editor.setStatus("V1_TICKS=" + globalThis.v1_ticks);
        };

        globalThis.do_reload = function () {
            editor.reloadInit();
        };

        editor.setInterval(50, "v1_tick");
        editor.registerCommand("Do Reload", "reload", "do_reload", null);
        "#,
    );

    harness.editor_mut().load_init_script(true);
    settle(&mut harness);
    harness.advance_time(Duration::from_millis(60));
    harness.tick_and_render().expect("tick");
    settle(&mut harness);
    assert!(
        status(&harness).starts_with("V1_TICKS="),
        "v1's timer should be running; status = {:?}",
        status(&harness)
    );

    // v2 has no timer at all.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("V2_LOADED");
        "#,
    );

    harness
        .editor_mut()
        .handle_plugin_command(fresh_core::api::PluginCommand::RunEditorCommand {
            name: "Do Reload".to_string(),
            callback_id: fresh_core::api::JsCallbackId::new(9_300),
        })
        .expect("dispatching the reload command should not error");
    assert!(
        pump_until(&mut harness, |h| status(h).contains("V2_LOADED")),
        "v2 should have loaded; status = {:?}",
        status(&harness)
    );

    // v1's timer, had it survived, would overwrite the status on the next
    // period — which is exactly how a leaked timer announces itself.
    for _ in 0..3 {
        harness.advance_time(Duration::from_millis(60));
        harness.tick_and_render().expect("tick");
        settle(&mut harness);
    }

    assert_eq!(
        status(&harness),
        "V2_LOADED",
        "reloading must cancel the previous copy's timers"
    );
}

/// The whole loop: an init.ts that is edited on disk and then reloaded *by a
/// command it registered itself* picks up the new source.
///
/// This is the shape of an agent's iteration — write the file, run the reload,
/// observe the effect — with every step going through the same paths the CLI
/// verbs use: `runCommand` resolves the name and dispatches the action, the
/// handler calls `reloadInit`, and the reload re-runs the file from disk.
#[test]
fn reload_init_reruns_the_edited_file() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    // v1 registers the reload command and marks itself as the live version.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();

        globalThis.agent_reload_handler = function () {
            editor.reloadInit();
        };

        editor.registerCommand(
            "Agent Reload",
            "reload init.ts from a script",
            "agent_reload_handler",
            null
        );

        editor.setStatus("INIT_VERSION_ONE");
        "#,
    );

    harness.editor_mut().load_init_script(true);
    assert!(
        pump_until(&mut harness, |h| status(h).contains("INIT_VERSION_ONE")),
        "v1 of init.ts should have run; status = {:?}",
        status(&harness)
    );

    // The agent edits the file. Nothing has re-read it yet.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("INIT_VERSION_TWO");
        "#,
    );
    assert!(
        !status(&harness).contains("INIT_VERSION_TWO"),
        "editing the file must not take effect on its own"
    );

    // Run the command v1 registered. Its handler calls reloadInit(), which
    // re-reads the file — so v2's body runs without an editor restart and
    // without a keystroke.
    harness
        .editor_mut()
        .handle_plugin_command(fresh_core::api::PluginCommand::RunEditorCommand {
            name: "Agent Reload".to_string(),
            callback_id: fresh_core::api::JsCallbackId::new(9_001),
        })
        .expect("dispatching a registered command should not error");

    assert!(
        pump_until(&mut harness, |h| status(h).contains("INIT_VERSION_TWO")),
        "reloadInit() should re-read init.ts and run the new body; status = {:?}",
        status(&harness)
    );
}
