//! Regression: a terminal restored from the workspace must come back *live*,
//! not focused-but-inert (fresh#2836).
//!
//! Restore creates the terminal buffer live (empty per-split scrollback set)
//! and makes it the active buffer — but nothing on that path ever derives the
//! terminal flags from it, the way every focus path does. So `key_context`
//! stayed `Normal` and the buffer stayed editing-disabled: keys did not reach
//! the PTY and the pane rendered the static backing-file view instead of the
//! live grid.
//!
//! It hid easily. Typing flips a terminal live on the first keystroke, and
//! with `jump_to_end_on_output` (on by default) the shell's own prompt output
//! jumps the buffer back to live — so a quiet `bash` looks fine. Point a
//! terminal at something that prints on its own (an agent, `tail -f`, a clock)
//! and the restored pane just sits there frozen while its process runs.
//!
//! Asserted here at the state level, since "the pane stopped repainting" is
//! what the user sees but not what the editor can be asked: after a restore,
//! the focused terminal must be live and its buffer must accept input.
//!
//! Workspace persistence is isolated by
//! `common::global_state::isolated_dir_context`; Linux-gated for the same
//! reason as `orchestrator_co_tenant_restore.rs`. Skips when there is no PTY.
#![cfg(target_os = "linux")]

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::input::keybindings::KeyContext;
use fresh::model::filesystem::StdFileSystem;
use std::path::Path;
use std::sync::Arc;

use crate::common::global_state::isolated_dir_context;

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

#[test]
fn a_restored_terminal_comes_back_live() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }

    let sandbox = tempfile::tempdir().unwrap();
    let (dir_context, _data_dir_pin) = isolated_dir_context(sandbox.path());
    let project = sandbox.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let project = project.canonicalize().unwrap();

    // Session 1: a terminal is open and focused when the editor quits.
    {
        let mut e1 = editor_in(&project, &dir_context);
        e1.open_terminal();
        assert!(
            e1.active_window().focused_terminal_live(),
            "sanity: a freshly opened terminal is live"
        );
        e1.save_workspace().unwrap();
    }

    // Session 2: cold reboot. Nothing is typed and no key is sent — exactly
    // what the user sees on relaunch.
    let mut e2 = editor_in(&project, &dir_context);
    e2.restore_active_window_on_launch(false).unwrap();

    let buffer_id = e2.active_buffer();
    assert!(
        e2.active_window().is_terminal_buffer(buffer_id),
        "sanity: the restored workspace focuses its terminal"
    );
    assert!(
        e2.active_window().focused_terminal_live(),
        "a restored terminal must come back live, not stranded in read-only \
         scrollback with its process running unseen"
    );
    assert_eq!(
        e2.active_window().key_context,
        KeyContext::Terminal,
        "keys must route to the restored terminal's PTY without a wake-up \
         keystroke first"
    );
    assert!(
        !e2.active_window().is_editing_disabled(),
        "the restored terminal's buffer must not still be editing-disabled: \
         that is the read-only scrollback view, not the live grid"
    );
}
