//! Regression: an agent terminal restored from the workspace must come back
//! holding a *live* capability token (fresh#2903).
//!
//! The Orchestrator spawns every workspace's agent with `allowScript`, which
//! mints a `FRESH_CMD_TOKEN` bound to that window and stamps it into the
//! child's environment; that token is what authorizes `fresh --cmd script
//! run`. The token table is in-memory and process-global by design — nothing
//! about it is persisted — and the saved workspace recorded everything a
//! terminal needs to come back (argv, resume argv, transcript paths, title)
//! *except* that it ever held the grant. So restore respawned the PTY with no
//! extra environment at all: the agent came back able to reach the editor
//! (`FRESH_SESSION` rides every local terminal) but not to drive it, and every
//! script it submitted was refused with "no capability token: script
//! evaluation is not authorized" until the workspace was recreated by hand.
//!
//! The assertion is on what the *child process* saw: the terminal runs a
//! command that prints `$FRESH_CMD_TOKEN` into its own transcript, so the
//! test reads back the value the restored agent would itself read, and then
//! checks that value is a token this editor honours — resolvable, carrying
//! the script grant, and pointed at the restored window rather than the one
//! that died.
//!
//! Own integration binary because it sets the process-global `XDG_DATA_HOME`
//! to isolate workspace persistence, mirroring `terminal_restore_live.rs`.
//! Skips when there is no PTY.
//!
//! Gated on `plugins`: the grant under test *is* a plugin-API capability
//! ("may drive this editor as a plugin would"), and the spawn path that hands
//! it out is the plugin `createTerminal` command, which the trimmed
//! no-plugins build doesn't compile at all.
#![cfg(all(target_os = "linux", feature = "plugins"))]

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use fresh::server::command_access;
use fresh_core::api::PluginCommand;
use std::path::Path;
use std::sync::Arc;

/// Marker the terminal's child prints, followed by whatever
/// `$FRESH_CMD_TOKEN` held when it started. `none` when the variable is unset
/// — which is precisely the regression, so it must be a value the test can
/// read rather than an empty line it might mistake for missing output.
const MARKER: &str = "FRESH_CMD_TOKEN_IS:";

fn isolated_dir_context(base: &Path) -> DirectoryContext {
    let xdg_data = base.join("xdg-data");
    std::fs::create_dir_all(&xdg_data).unwrap();
    std::env::set_var("XDG_DATA_HOME", &xdg_data);
    DirectoryContext {
        data_dir: xdg_data.join("fresh"),
        config_dir: base.join("config"),
        home_dir: Some(base.join("home")),
        documents_dir: None,
        downloads_dir: None,
    }
}

fn editor_in(project: &Path, dir_context: &DirectoryContext) -> fresh::app::Editor {
    let filesystem: Arc<dyn fresh::model::filesystem::FileSystem + Send + Sync> =
        Arc::new(StdFileSystem);
    let config = Config {
        check_for_updates: false,
        ..Config::default()
    };
    fresh::app::Editor::for_test(
        config,
        80,
        24,
        Some(project.to_path_buf()),
        dir_context.clone(),
        fresh::view::color_support::ColorCapability::TrueColor,
        filesystem,
        None,
        None,
        false,
        false,
    )
    .unwrap()
}

fn pty_available() -> bool {
    use portable_pty::{native_pty_system, PtySize};
    native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_ok()
}

/// Spawn an agent terminal the way the Orchestrator's "Run Agent…" does —
/// through the plugin `createTerminal` API with `allowScript`, which is what
/// mints the token. The child announces the token it was given and then parks
/// on `cat`, so the terminal is still *live* when the workspace is saved (an
/// exited one restores dead, by design, and would never respawn).
fn spawn_agent_terminal(editor: &mut fresh::app::Editor) {
    let script = format!("printf '{MARKER}%s\\n' \"${{FRESH_CMD_TOKEN:-none}}\"; exec cat");
    editor
        .handle_plugin_command(PluginCommand::CreateTerminal {
            cwd: None,
            direction: None,
            ratio: None,
            focus: Some(true),
            // Ephemeral, like every plugin-created terminal; carrying a launch
            // command is what makes it a restorable session terminal anyway.
            persistent: false,
            window_id: None,
            command: Some(vec!["sh".to_string(), "-c".to_string(), script]),
            title: None,
            resume: None,
            env: None,
            allow_script: true,
            request_id: 0,
        })
        .expect("agent terminal should spawn");
}

/// The raw PTY transcript of the active window's only terminal. Restore
/// *continues* this file, so it accumulates one announcement per incarnation.
fn transcript_path(editor: &fresh::app::Editor) -> std::path::PathBuf {
    let window = editor.active_window();
    let terminal_id = window
        .last_focused_terminal()
        .expect("the window has a terminal");
    window
        .terminal_log_files
        .get(&terminal_id)
        .cloned()
        .expect("a spawned terminal records its log file")
}

/// Block until the transcript holds at least `count` announcements, then
/// return the last one's token.
///
/// Semantic wait, no deadline: the PTY reader thread writes this file on its
/// own, so the only thing to wait for is the child having run. A regression
/// keeps the *count* honest — a restore that spawns without the grant still
/// announces, it just announces `none`.
fn nth_announced_token(path: &Path, count: usize) -> String {
    loop {
        let transcript = std::fs::read_to_string(path).unwrap_or_default();
        let announcements: Vec<&str> = transcript.split(MARKER).skip(1).collect();
        if announcements.len() >= count {
            return announcements[count - 1]
                .split(|c: char| c.is_whitespace())
                .next()
                .unwrap_or_default()
                .to_string();
        }
        std::thread::yield_now();
    }
}

#[test]
fn a_restored_agent_terminal_can_still_drive_the_editor() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }

    let sandbox = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(sandbox.path());
    let project = sandbox.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let project = project.canonicalize().unwrap();

    // Session 1: an agent is running with editor control when the editor quits.
    let first_token = {
        let mut e1 = editor_in(&project, &dir_context);
        spawn_agent_terminal(&mut e1);
        let log = transcript_path(&e1);
        let token = nth_announced_token(&log, 1);
        assert!(
            command_access::may_script(&token),
            "sanity: a freshly spawned agent terminal is handed a token that \
             authorizes scripts, got {token:?}"
        );
        e1.save_workspace().unwrap();
        token
    };

    // Session 2: cold reboot, exactly what the user gets on relaunch.
    let mut e2 = editor_in(&project, &dir_context);
    e2.restore_active_window_on_launch(false).unwrap();
    let restored_window = e2.active_window_id();

    let log = transcript_path(&e2);
    let restored_token = nth_announced_token(&log, 2);

    assert_ne!(
        restored_token, "none",
        "the restored agent's child must be handed a capability token; without \
         one every `fresh --cmd script run` it makes is refused with \"no \
         capability token: script evaluation is not authorized\""
    );
    assert_ne!(
        restored_token, first_token,
        "the restored agent must get a token minted for this run, not the \
         string the dead editor handed its predecessor"
    );

    let grant = command_access::lookup(&restored_token)
        .expect("the restored agent's token must resolve in this editor");
    assert!(
        grant.may_script,
        "the restored agent's token must carry the script grant, not just be \
         addressable"
    );
    assert_eq!(
        grant.window_id,
        Some(restored_window.0),
        "the restored agent's token must drive the window it came back in"
    );
}
