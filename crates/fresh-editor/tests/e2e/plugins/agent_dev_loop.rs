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
//!
//! Everything a registered command needs to be *dispatched* goes through the
//! command palette here (CONTRIBUTING.md Testing §2): a synthetic
//! `PluginCommand::RunEditorCommand` with an invented `callback_id` skips the
//! sequencing production has and answers a promise nobody is holding, which
//! the runtime rightly logs as `No plugin found for callback_id=…`.

use crate::common::harness::EditorTestHarness;
use fresh::config_io::DirectoryContext;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// A harness whose `config_dir` is a scratch directory, so the tests write a
/// real init.ts without touching the developer's own.
///
/// Wide enough that the status line these tests read back is not elided: the
/// status bar drops elements to fit, and an assertion on a message that never
/// had room to render proves nothing.
fn harness_with_scratch_config_dir() -> (EditorTestHarness, tempfile::TempDir, PathBuf) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();

    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();

    let harness = EditorTestHarness::with_shared_dir_context(
        120,
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

/// Give the plugin thread a chance to produce commands, drain them, and
/// repaint.
///
/// Only ever used to give a *negative* assertion something to catch: "no
/// further tick lands" has no event to wait for, so the test has to hand the
/// plugin thread a window in which a stray tick could show up. Every positive
/// condition is awaited semantically instead (`wait_until`), per
/// CONTRIBUTING.md Testing §3 — no assertion here depends on this window being
/// long enough.
fn settle(harness: &mut EditorTestHarness) {
    for _ in 0..20 {
        harness.editor_mut().process_async_messages();
        std::thread::sleep(Duration::from_millis(5));
    }
    harness.editor_mut().process_async_messages();
    harness.render().expect("render");
}

/// The rendered status line.
///
/// CONTRIBUTING.md Testing §2 — observe, don't inspect: what the plugin
/// announced is read off the screen the user would be looking at, not out of
/// `Editor::get_status_message()`.
fn status(harness: &EditorTestHarness) -> String {
    harness.get_status_bar()
}

/// The number in a `<prefix>=<n>` marker on the status line, if one is shown.
fn marker_count(harness: &EditorTestHarness, prefix: &str) -> Option<u32> {
    let line = status(harness);
    let rest = line.split(prefix).nth(1)?;
    let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
    digits.parse().ok()
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
    harness
        .wait_until(|h| {
            let s = status(h);
            s.contains("AGENT_PROBE_RAN") || s.contains("AGENT_PROBE_NOT_LISTED")
        })
        .expect("listCommands()/runCommand() should settle");

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
    harness
        .wait_until(|h| status(h).contains("UNKNOWN_"))
        .expect("runCommand() with an unknown name should settle");

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

    // Ctrl+P → "Open Panel" → Enter: the palette waits for init.ts to have
    // registered the command before confirming, so this is also the gate that
    // the old fixed settle() was standing in for.
    harness
        .run_palette_command("Open Panel")
        .expect("running the open command from the palette");

    // Wait for the third tick to appear on screen rather than advancing a
    // fixed number of periods and hoping three have landed: `wait_until`
    // advances the harness clock as it goes, so the timer keeps coming due
    // until the condition holds however slow the box is.
    harness
        .wait_until(|h| marker_count(h, "TICKS=").unwrap_or(0) >= 3)
        .expect("a timer created inside a command handler must keep firing");
}

/// `clearInterval` stops a running timer.
///
/// The stop is announced only after a host round-trip, and the test waits for
/// that announcement before it starts counting: `clearInterval` is
/// fire-and-forget, so a tick the host had already dispatched can still be on
/// its way to the plugin thread when the JS call returns. Snapshotting the
/// count before the cancellation is *observably* in effect is what used to
/// make this test fail on a loaded CI box with `TICKS=3` vs `TICKS=2`.
#[test]
fn clear_interval_stops_the_timer() {
    let (mut harness, _tmp, config_dir) = harness_with_scratch_config_dir();

    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        globalThis.tick_count = 0;

        // Deliberately synchronous: `start_action` calls a handler directly
        // rather than queueing it, so a sync body has already sent its
        // setStatus by the time the plugin thread takes its next request.
        // Making this `async` would let its setStatus land *after* the
        // barrier below resolves and would reintroduce the race.
        globalThis.counting_tick = function () {
            globalThis.tick_count += 1;
            editor.setStatus("TICKS=" + globalThis.tick_count);
        };

        globalThis.stop_ticking = async function () {
            editor.clearInterval(globalThis.the_timer);
            // Any awaited host call is a barrier here, and both halves of it
            // are ordered by a FIFO channel:
            //
            //  * `clearInterval` and this `listCommands` are two sends on the
            //    plugin's one PluginCommand channel, and the host dispatches
            //    that channel in arrival order (deferring a budget overrun
            //    without reordering it). So the host has already dropped the
            //    timer from its table — and can therefore never dispatch it
            //    again — before it answers.
            //  * The answer goes back as a PluginRequest on the same channel
            //    that carries handler invocations, so any tick the host had
            //    already dispatched sits ahead of it and has run to
            //    completion before we resume here.
            //
            // Whatever the status says after this line, no tick — in flight
            // or scheduled — can still overwrite it.
            await editor.listCommands();
            editor.setStatus("STOPPED_AFTER=" + globalThis.tick_count);
        };

        globalThis.the_timer = editor.setInterval(50, "counting_tick");
        editor.registerCommand("Stop Ticking", "stop", "stop_ticking", null);
        "#,
    );

    harness.editor_mut().load_init_script(true);

    // Stopping a timer that was never running would pass vacuously.
    harness
        .wait_until(|h| marker_count(h, "TICKS=").unwrap_or(0) >= 1)
        .expect("the timer should fire before it is stopped");

    // Ctrl+P → "Stop Ticking" → Enter.
    harness
        .run_palette_command("Stop Ticking")
        .expect("running the stop command from the palette");

    // Semantic wait: the marker is published *after* the host applied the
    // cancellation, so seeing it is proof the stop took effect — no guess
    // about when the dispatch landed.
    harness
        .wait_until(|h| status(h).contains("STOPPED_AFTER="))
        .expect("the stop handler should report once the cancellation applied");
    let stopped = status(&harness);

    // From here a surviving timer announces itself by overwriting the line
    // with `TICKS=<n>`.
    for _ in 0..3 {
        harness.advance_time(Duration::from_millis(60));
        harness.tick_and_render().expect("tick");
        settle(&mut harness);
    }

    assert_eq!(
        status(&harness),
        stopped,
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
    harness
        .wait_until(|h| status(h).contains("V1_TICKS="))
        .expect("v1's timer should be running");

    // v2 has no timer at all.
    write_init_ts(
        &config_dir,
        r#"
        const editor = getEditor();
        editor.setStatus("V2_LOADED");
        "#,
    );

    harness
        .run_palette_command("Do Reload")
        .expect("running the reload command from the palette");
    // v2's greeting is emitted by the plugin thread only after it has finished
    // tearing v1 down, and the host drains v1's last commands before it — so
    // once this is on screen, anything that follows came from a leaked timer.
    harness
        .wait_until(|h| status(h).contains("V2_LOADED"))
        .expect("v2 should have loaded");
    let after_reload = status(&harness);

    // v1's timer, had it survived, would overwrite the status on the next
    // period — which is exactly how a leaked timer announces itself.
    for _ in 0..3 {
        harness.advance_time(Duration::from_millis(60));
        harness.tick_and_render().expect("tick");
        settle(&mut harness);
    }

    assert_eq!(
        status(&harness),
        after_reload,
        "reloading must cancel the previous copy's timers"
    );
}

/// The whole loop: an init.ts that is edited on disk and then reloaded *by a
/// command it registered itself* picks up the new source.
///
/// This is the shape of an agent's iteration — write the file, run the reload,
/// observe the effect — with every step going through the same paths a user
/// (or the CLI verbs) would take: the palette resolves the name and dispatches
/// the action, the handler calls `reloadInit`, and the reload re-runs the file
/// from disk.
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
    harness
        .wait_until(|h| status(h).contains("INIT_VERSION_ONE"))
        .expect("v1 of init.ts should have run");

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

    // Run the command v1 registered, from the palette. Its handler calls
    // reloadInit(), which re-reads the file — so v2's body runs without an
    // editor restart.
    harness
        .run_palette_command("Agent Reload")
        .expect("running the reload command from the palette");

    harness
        .wait_until(|h| status(h).contains("INIT_VERSION_TWO"))
        .expect("reloadInit() should re-read init.ts and run the new body");
}
