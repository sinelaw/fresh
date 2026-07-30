//! Regression test: an Orchestrator agent terminal survives a restart.
//!
//! Orchestrator sessions spawn their agent as an *ephemeral* terminal whose
//! spawn argv is recorded in `Window::terminal_commands` (see
//! `create_window_with_terminal`). Before the fix, workspace-save dropped every
//! ephemeral terminal, so a saved session held no terminal at all and came back
//! as a blank `[No Name]` pane on restore. The fix persists a command-carrying
//! ephemeral terminal and re-runs that command on restore.
//!
//! This test reproduces the round-trip at the window level: spawn an ephemeral
//! terminal with a recognizable command, save, restore in a fresh editor that
//! shares the same data dir, and assert the terminal comes back (a terminal
//! buffer, showing the command's marker) rather than a blank pane.
//!
//! Requires a working PTY (/dev/ptmx); skips when unavailable, like the other
//! terminal e2e tests.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
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

fn session_config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    // Isolate the restored-terminal behaviour from the "new output re-enters
    // terminal mode" path so the assertions don't depend on shell timing.
    config.terminal.jump_to_end_on_output = false;
    config
}

/// Spawn an ephemeral, command-carrying terminal into `window` the way
/// `create_window_with_terminal` does: an ephemeral PTY plus a
/// `terminal_commands` entry marking it as a restorable *session* terminal.
fn spawn_agent_terminal(window: &mut fresh::app::window::Window, argv: &[&str]) {
    let argv: Vec<String> = argv.iter().map(|s| s.to_string()).collect();
    let window_id = window.id;
    let (terminal_id, _buffer_id, _leaf) = window
        .create_plugin_terminal(fresh::app::PluginTerminalSpec {
            cwd: None,
            direction: None, // no split direction — seed/attach in the active split
            ratio: None,
            focus: true,       // the agent terminal is the seed
            persistent: false, // ephemeral — exactly the Orchestrator agent case
            command: Some(argv.clone()),
            title: None,
            env: fresh::app::agent_command_env(window_id, None, false),
        })
        .expect("agent terminal should spawn");
    // create_window_with_terminal records this marker; mirror it here so the
    // ephemeral terminal is recognised as a restorable session terminal.
    window.terminal_commands.insert(terminal_id, argv);
}

/// Like `spawn_agent_terminal`, but also records an agent-resume argv — the
/// way `create_window_with_terminal` does when the Orchestrator provisions a
/// resumable agent (launch with `--session-id`, resume with `--resume`).
fn spawn_resumable_agent_terminal(
    window: &mut fresh::app::window::Window,
    launch: &[&str],
    resume: &[&str],
) {
    let launch: Vec<String> = launch.iter().map(|s| s.to_string()).collect();
    let resume: Vec<String> = resume.iter().map(|s| s.to_string()).collect();
    let window_id = window.id;
    let (terminal_id, _buffer_id, _leaf) = window
        .create_plugin_terminal(fresh::app::PluginTerminalSpec {
            cwd: None,
            direction: None,
            ratio: None,
            focus: true,
            persistent: false,
            command: Some(launch.clone()),
            title: None,
            env: fresh::app::agent_command_env(window_id, None, false),
        })
        .expect("agent terminal should spawn");
    window.terminal_commands.insert(terminal_id, launch);
    window.terminal_resume_commands.insert(terminal_id, resume);
}

/// Like `spawn_agent_terminal`, but grants the terminal editor control — the
/// `allow_script` case, where `agent_command_env` mints a `FRESH_CMD_TOKEN`
/// into the child's environment.
fn spawn_granted_agent_terminal(window: &mut fresh::app::window::Window, argv: &[&str]) {
    let argv: Vec<String> = argv.iter().map(|s| s.to_string()).collect();
    let window_id = window.id;
    let (terminal_id, _buffer_id, _leaf) = window
        .create_plugin_terminal(fresh::app::PluginTerminalSpec {
            cwd: None,
            direction: None,
            ratio: None,
            focus: true,
            persistent: false,
            command: Some(argv.clone()),
            title: None,
            env: fresh::app::agent_command_env(window_id, None, true),
        })
        .expect("agent terminal should spawn");
    window.terminal_commands.insert(terminal_id, argv);
}

/// A restored agent terminal comes back able to drive the editor.
///
/// The capability token is per-process and unforgeable, so it cannot be
/// persisted — restore has to mint a *new* one. Before the fix, restore (and
/// PTY respawn) built the child env inline as an empty map, so a restored agent
/// came back with `FRESH_SESSION` but no `FRESH_CMD_TOKEN`: every
/// `fresh --cmd script …` it ran failed with "no capability token", silently,
/// until the terminal was recreated from scratch.
///
/// Observed through the child process itself: the agent's argv writes its own
/// `$FRESH_CMD_TOKEN` to a sentinel file. The sentinel is removed between the
/// two sessions, so what lands there after restore came from the restored
/// child. Asserting the token is *live* in this process — not merely non-empty
/// — is what proves it was re-minted rather than carried over as a stale
/// string.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell command
fn test_restored_agent_terminal_gets_a_fresh_command_token() {
    if !pty_available() {
        eprintln!("Skipping restored-agent-token test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let sentinels = temp_dir.path().join("sentinels");
    std::fs::create_dir(&sentinels).unwrap();
    let token_file = sentinels.join("TOKEN");
    // Write the token the child actually received, then stay alive so the
    // terminal is live (not exited) at save time.
    let cmd = format!(
        "printf '%s' \"${{FRESH_CMD_TOKEN:-}}\" > '{}'; exec sleep 30",
        token_file.display()
    );
    let argv = ["sh", "-c", cmd.as_str()];

    // ---- Session 1: a granted agent terminal, then save. ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);

        spawn_granted_agent_terminal(harness.editor_mut().active_window_mut(), &argv);
        harness.render().unwrap();
        harness
            .wait_until(|_| std::fs::read(&token_file).is_ok_and(|b| !b.is_empty()))
            .expect("a granted terminal should receive a token when first spawned");

        harness.shutdown(true).unwrap();
    }

    // Drop the first session's token so the file can only be repopulated by
    // the restored child.
    std::fs::remove_file(&token_file).unwrap();

    // ---- Session 2: restart; the restored agent must get a new token. ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "session should have been restored");
        harness.render().unwrap();

        // Without the fix this never arrives: the restored child is spawned
        // with an empty extra env, so `$FRESH_CMD_TOKEN` expands to "" and the
        // sentinel stays empty.
        harness
            .wait_until(|_| std::fs::read(&token_file).is_ok_and(|b| !b.is_empty()))
            .expect("a restored agent terminal should be given a command token");

        let token = std::fs::read_to_string(&token_file).unwrap();
        assert!(
            fresh::server::command_access::may_script(token.trim()),
            "the restored terminal's token should be live in this process, \
             i.e. freshly minted rather than a stale value from the last run"
        );
    }
}

#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell command
fn test_orchestrator_agent_terminal_restores_after_restart() {
    if !pty_available() {
        eprintln!("Skipping agent-terminal restore test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // A long-lived command so the terminal is live (not exited) at save time.
    let argv = ["sh", "-c", "exec sleep 30"];

    // ---- Session 1: spawn the agent terminal, then save. ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);

        spawn_agent_terminal(harness.editor_mut().active_window_mut(), &argv);
        harness.render().unwrap();
        // The spawned agent terminal is the active buffer in this session.
        let active = harness.editor().active_buffer_id();
        assert!(
            harness.editor().active_window().is_terminal_buffer(active),
            "agent terminal should be the active buffer before save"
        );

        harness.shutdown(true).unwrap();
    }

    // ---- Session 2: restart sharing the same data dir, then verify the
    // agent terminal is back (not a blank pane). ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "session should have been restored");
        harness.render().unwrap();

        // The fix: a terminal buffer comes back. Without it, the ephemeral
        // terminal was dropped on save and the restored window holds only an
        // empty `[No Name]` buffer, so the active buffer is NOT a terminal.
        let active = harness.editor().active_buffer_id();
        assert!(
            harness.editor().active_window().is_terminal_buffer(active),
            "restored Orchestrator session should come back as a terminal, not a blank pane"
        );
    }
}

/// On restore, a terminal carrying an agent-resume spec runs the *resume*
/// argv, not the launch command — proving agent sessions rejoin rather than
/// restart. Asserted via a filesystem side effect so there's no dependence on
/// live PTY output timing: launch and resume `touch` different sentinel files;
/// after restart only the resume sentinel should appear.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses a Unix shell command
fn test_agent_resume_runs_resume_command_on_restart() {
    if !pty_available() {
        eprintln!("Skipping agent-resume test: PTY not available");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Sentinels in a dir that survives between the two sessions.
    let sentinels = temp_dir.path().join("sentinels");
    std::fs::create_dir(&sentinels).unwrap();
    let launched = sentinels.join("LAUNCHED");
    let resumed = sentinels.join("RESUMED");
    let launch_cmd = format!("touch '{}'; exec sleep 30", launched.display());
    let resume_cmd = format!("touch '{}'; exec sleep 30", resumed.display());
    let launch = ["sh", "-c", launch_cmd.as_str()];
    let resume = ["sh", "-c", resume_cmd.as_str()];

    // ---- Session 1: launch the resumable agent, then save. ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);

        spawn_resumable_agent_terminal(harness.editor_mut().active_window_mut(), &launch, &resume);
        harness.render().unwrap();
        // The launch command ran (not the resume one).
        harness
            .wait_until(|_| launched.exists())
            .expect("launch command should run in the first session");
        assert!(
            !resumed.exists(),
            "resume command must not run during the initial launch"
        );

        harness.shutdown(true).unwrap();
    }

    // ---- Session 2: restart; the resume argv should run, not the launch. ----
    {
        let mut harness = EditorTestHarness::create(
            120,
            30,
            HarnessOptions::new()
                .with_config(session_config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let restored = harness.startup(true, &[]).unwrap();
        assert!(restored, "session should have been restored");
        harness.render().unwrap();

        harness
            .wait_until(|_| resumed.exists())
            .expect("restore should run the agent-resume command, not the launch command");
    }
}
