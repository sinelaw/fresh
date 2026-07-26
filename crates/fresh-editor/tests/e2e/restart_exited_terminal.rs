//! E2E tests for restarting a terminal whose process has quit.
//!
//! Restoring a workspace re-runs each terminal's agent-resume argv (falling
//! back to the launch command, then a plain shell) so a session rejoins its
//! agent instead of coming back as a bare shell. These tests pin the *per
//! buffer, on demand* form of that same mechanism: when a terminal's process
//! exits mid-session the buffer stays open, and the user can bring it back
//! live in place from the status-bar indicator or the command palette.
//!
//! Every assertion is on rendered output, and the restart itself is always
//! driven by a real click or keystroke — the agent under test is a small shell
//! script that prints which argv it was started with, so "did it resume or
//! relaunch?" is answerable from the screen alone.
//!
//! Requires a working PTY (/dev/ptmx); skips when unavailable, like the other
//! terminal e2e tests.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh_core::api::PluginCommand;
use portable_pty::{native_pty_system, PtySize};
use tempfile::TempDir;

fn pty_available() -> bool {
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

fn terminal_config() -> Config {
    let mut config = Config::default();
    // Keep assertions off terminal render timing: the tests wait for their
    // marker text rather than for a jump-to-end scroll.
    config.terminal.jump_to_end_on_output = false;
    config
}

fn harness(working_dir: std::path::PathBuf) -> EditorTestHarness {
    EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(terminal_config())
            .with_working_dir(working_dir)
            .without_empty_plugins_dir(),
    )
    .unwrap()
}

/// Write an executable script into `dir` and return its path.
fn write_script(dir: &std::path::Path, name: &str, body: &str) -> String {
    let path = dir.join(name);
    std::fs::write(&path, body).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    path.to_string_lossy().into_owned()
}

/// Spawn an agent-style terminal the way the Orchestrator's "Run Agent…" does:
/// through the plugin `createTerminal` API, with a launch command and an
/// optional agent-resume argv.
///
/// Setup only — the behaviour under test is driven through the UI below. It
/// deliberately goes through `PluginCommand::CreateTerminal` rather than
/// poking `terminal_commands` / `terminal_resume_commands` directly: those
/// maps being written *by the spawn path* is part of what these tests check.
/// An earlier version of this helper set them by hand, which is exactly why a
/// real bug went unnoticed — `createTerminal` recorded neither, so an agent
/// started in the current workspace restarted as a bare shell.
fn spawn_agent_terminal(harness: &mut EditorTestHarness, launch: &[&str], resume: Option<&[&str]>) {
    let argv = |a: &[&str]| a.iter().map(|s| s.to_string()).collect::<Vec<_>>();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CreateTerminal {
            cwd: None,
            direction: None,
            ratio: None,
            focus: Some(true),
            // Ephemeral, like every plugin-created terminal. Carrying a command
            // is what makes it a restorable *session* terminal regardless.
            persistent: false,
            window_id: None,
            command: Some(argv(launch)),
            title: None,
            resume: resume.map(argv),
            env: None,
            command_allowlist: None,
            request_id: 0,
        })
        .expect("agent terminal should spawn");
}

/// How many tabs the tab bar shows for `title` — the observable form of
/// "the restart reused this buffer" versus "it opened another terminal".
fn terminal_tabs(harness: &EditorTestHarness, title: &str) -> usize {
    // Row 1 is the tab bar (row 0 is the menu bar).
    harness.screen_row_text(1).matches(title).count()
}

/// Assert the command palette *offers* `command` for `query`, then dismiss it.
///
/// Deliberately stops at "offered" rather than pressing Enter. Confirming a
/// quick-open entry re-computes the suggestion list and indexes it by the
/// highlighted row, while plugin-registered commands keep arriving in the
/// background — so which command a blind Enter runs is genuinely racy, and a
/// test built on it hangs intermittently. Discoverability is what the palette
/// contributes here; the restart itself is driven through the status-bar
/// indicator, whose hit area is exact.
fn assert_palette_offers(harness: &mut EditorTestHarness, query: &str, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(query).unwrap();
    let command = command.to_string();
    harness
        .wait_until(|h| h.screen_to_string().contains(&command))
        .expect("the palette should offer the command");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// Click the status-bar restart indicator, whichever wording it currently has.
fn click_restart_indicator(harness: &mut EditorTestHarness, label: &str) {
    let (col, row) = harness.find_text_on_screen(label).unwrap_or_else(|| {
        panic!(
            "no restart indicator on the status bar\n{}",
            harness.screen_to_string()
        )
    });
    harness.mouse_click(col + 2, row).unwrap();
    harness.render().unwrap();
}

/// The headline flow: an agent quits, the status bar offers to resume it, and
/// clicking that indicator rejoins the conversation — the resume argv runs, not
/// the launch command.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell script
fn test_status_bar_indicator_restarts_exited_agent_by_resuming() {
    if !pty_available() {
        eprintln!("Skipping terminal-restart test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // A stand-in agent that reports how it was started. `--session-id` is the
    // launch form (quits straight away, as a finished agent does);
    // `--resume` is the rejoin form and stays alive at its prompt.
    let agent = write_script(
        temp_dir.path(),
        "claude",
        "#!/bin/sh\n\
         case \"$1\" in\n\
           --resume) echo \"AGENT-RESUMED $2\"; exec sleep 30 ;;\n\
           *)        echo \"AGENT-LAUNCHED $2\" ;;\n\
         esac\n",
    );

    let mut harness = harness(project_dir);
    spawn_agent_terminal(
        &mut harness,
        &[agent.as_str(), "--session-id", "sess-1"],
        Some(&[agent.as_str(), "--resume", "sess-1"]),
    );
    harness.render().unwrap();

    // The agent ran and quit, and the status bar now offers to resume it —
    // named after the dead program, worded for a rejoin rather than a fresh
    // start. The indicator is the exit signal this test waits on: it is
    // rendered straight from window state, where the `[Terminal process
    // exited]` marker additionally depends on a file append, a buffer revert
    // and a viewport scroll.
    harness
        .wait_until(|h| h.screen_to_string().contains("⟳ Resume claude"))
        .expect("the status bar should offer to resume the exited agent");
    harness.assert_screen_not_contains("AGENT-RESUMED");

    click_restart_indicator(&mut harness, "⟳ Resume claude");

    // The conversation is rejoined: the *resume* argv ran.
    harness
        .wait_until(|h| h.screen_to_string().contains("AGENT-RESUMED sess-1"))
        .expect("clicking the indicator should run the agent's resume argv");
    // And the call to action is spent.
    harness
        .wait_until(|h| !h.screen_to_string().contains("⟳ Resume claude"))
        .expect("a restarted terminal should stop advertising a restart");

    // The resumed agent came back in the *same* pane: still one `claude` tab,
    // not a second one beside it. That — not the scrollback text — is the
    // durable statement of "reuses this buffer instead of opening a new
    // terminal"; re-entering the live grid rewrites the backing file's tail,
    // so the pre-restart banner is not a stable thing to assert on.
    assert_eq!(
        terminal_tabs(&harness, "claude"),
        1,
        "restart must reuse the agent's own tab\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Restart is not an agent feature. A plain terminal with no resume argv comes
/// back too — re-running what it was launched with, in the same buffer, so the
/// user can keep working instead of opening a new terminal.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell script
fn test_palette_restarts_exited_plain_terminal_in_place() {
    if !pty_available() {
        eprintln!("Skipping terminal-restart test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    // Prints a different banner on each run, so a restart is distinguishable
    // from the original launch on screen. The first run exits (that's what
    // arms the restart); the restarted one stays alive, so its banner sits on
    // the live grid instead of racing the exit handling for a place in the
    // scrollback.
    let counter = temp_dir.path().join("runs");
    let script = write_script(
        temp_dir.path(),
        "job.sh",
        &format!(
            "#!/bin/sh\n\
             n=$(cat '{c}' 2>/dev/null || echo 0)\n\
             n=$((n + 1))\n\
             echo \"$n\" > '{c}'\n\
             echo \"JOB-RUN-$n\"\n\
             [ \"$n\" -ge 2 ] && exec sleep 30\n",
            c = counter.display()
        ),
    );

    let mut harness = harness(project_dir);
    spawn_agent_terminal(&mut harness, &[&script], None);
    harness.render().unwrap();

    // The job ran and exited. With no agent to rejoin, the indicator offers a
    // plain restart (see the agent test for why the indicator, rather than the
    // `[Terminal process exited]` marker, is the signal waited on).
    harness
        .wait_until(|h| h.screen_to_string().contains("⟳ Restart"))
        .expect("a plain exited terminal should still offer a restart");

    // The palette offers it too, worded for a plain restart.
    assert_palette_offers(
        &mut harness,
        "restart terminal process",
        "Restart Terminal Process",
    );

    click_restart_indicator(&mut harness, "⟳ Restart");
    harness
        .wait_until(|h| h.screen_to_string().contains("JOB-RUN-2"))
        .expect("the restart should re-run the launch command");

    // Same buffer: the second run replaced the first in the one `job.sh` tab
    // rather than opening another terminal beside it.
    assert_eq!(
        terminal_tabs(&harness, "job.sh"),
        1,
        "restart must reuse the terminal's own tab\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// A live terminal is never restarted out from under the user — the request
/// becomes a status message instead of killing a running agent.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell command
fn test_restart_refuses_while_process_is_running() {
    if !pty_available() {
        eprintln!("Skipping terminal-restart test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let script = write_script(
        temp_dir.path(),
        "long.sh",
        "#!/bin/sh\necho LONG-JOB-STARTED\nexec sleep 30\n",
    );

    let mut harness = harness(project_dir);
    spawn_agent_terminal(&mut harness, &[&script], None);
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("LONG-JOB-STARTED"))
        .expect("the long-running job should start");
    // A live terminal never advertises a restart — the indicator is the only
    // way the UI offers one, so a running agent can't be killed by a stray
    // click on the status bar.
    harness.assert_screen_not_contains("⟳ Restart");
    harness.assert_screen_not_contains("⟳ Resume");
    // Nor has anything exited.
    harness.assert_screen_not_contains("[Terminal process exited]");

    // Leaving the live grid for read-only scrollback doesn't change that: the
    // process is still running, so there is still nothing to restart.
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("⟳ Restart");
    harness.assert_screen_not_contains("⟳ Resume");
    harness.assert_screen_contains("LONG-JOB-STARTED");
}

/// The restart offer survives the editor itself. A workspace saved while an
/// agent's process was dead comes back showing that agent's transcript with the
/// resume still on the status bar — rather than losing the pane (which is what
/// happened before exited terminals were persisted at all).
///
/// It deliberately comes back *dead*: restoring a workspace must not silently
/// re-run a process the user had already finished with, which for an agent
/// would mean resuming a conversation — and spending tokens — just because the
/// editor reopened.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell script
fn test_exited_agent_restart_offer_survives_an_editor_restart() {
    if !pty_available() {
        eprintln!("Skipping terminal-restart test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let agent = write_script(
        temp_dir.path(),
        "claude",
        "#!/bin/sh\n\
         case \"$1\" in\n\
           --resume) echo \"AGENT-RESUMED $2\"; exec sleep 30 ;;\n\
           *)        echo \"AGENT-LAUNCHED $2\" ;;\n\
         esac\n",
    );

    let session = |dir_context: &DirectoryContext| {
        EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(terminal_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    // ---- Session 1: the agent runs, quits, and the workspace is saved with
    // the restart on offer. ----
    {
        let mut harness = session(&dir_context);
        harness.editor_mut().set_session_mode(true);
        spawn_agent_terminal(
            &mut harness,
            &[agent.as_str(), "--session-id", "sess-1"],
            Some(&[agent.as_str(), "--resume", "sess-1"]),
        );
        harness.render().unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains("⟳ Resume claude"))
            .expect("the agent should exit and offer a resume");
        harness.shutdown(true).unwrap();
    }

    // ---- Session 2: reopen. ----
    {
        let mut harness = session(&dir_context);
        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "the workspace should have been restored");
        harness.render().unwrap();

        // The dead agent's pane is back, still offering to resume it…
        harness
            .wait_until(|h| h.screen_to_string().contains("⟳ Resume claude"))
            .expect("a restored exited agent should still offer a resume");
        // …and nothing was resumed behind the user's back.
        harness.assert_screen_not_contains("AGENT-RESUMED");

        // Taking the offer rejoins the conversation, exactly as it would have
        // in the session where the agent died.
        click_restart_indicator(&mut harness, "⟳ Resume claude");
        harness
            .wait_until(|h| h.screen_to_string().contains("AGENT-RESUMED sess-1"))
            .expect("the restored offer should still run the agent's resume argv");
    }
}
