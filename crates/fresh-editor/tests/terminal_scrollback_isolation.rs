//! Regression: a brand-new terminal must never come up showing some *other*
//! terminal's scrollback (fresh#2836).
//!
//! Terminal transcripts live in `fresh-terminal-<id>.{txt,log}`, named after a
//! terminal id that restarts at 0 every editor run — while the files (and the
//! workspace entries pointing at them) outlive the run that wrote them. The
//! spawn path used to infer "this is a restore, append and seed the history
//! end" from nothing more than "that file already has bytes", so two unrelated
//! terminals could end up sharing one transcript:
//!
//!   * **Restore.** A restored terminal keeps the backing path it was saved
//!     with but takes a *fresh* id. Restore a workspace whose surviving
//!     terminal was saved as `…-1`, and it comes back as id 0 — so the next
//!     terminal the user opens is handed id 1, i.e. the path the restored
//!     terminal is still streaming into. Scroll up in the new terminal and the
//!     old one's history is there.
//!   * **Leftovers.** A run that ended without closing its terminals (a crash,
//!     a kill) leaves `…-0.txt` on disk. The next run's first terminal is
//!     offered that same path and inherits a dead session's scrollback.
//!
//! Both are covered below. The fix is two-part and both halves are asserted:
//! a new terminal never gets a path a *live* terminal owns, and a new terminal
//! opens its transcript `BackingMode::Fresh` (truncating) so leftovers can't
//! survive into it.
//!
//! Own integration binary because it sets the process-global `XDG_DATA_HOME`
//! to isolate workspace persistence; Linux-gated for the same reason as
//! `orchestrator_co_tenant_restore.rs` (`dirs::data_dir()` ignores
//! `XDG_DATA_HOME` elsewhere). Skips when the environment has no PTY.
#![cfg(target_os = "linux")]

use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::model::filesystem::StdFileSystem;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Isolate all editor persistence (workspaces, terminal transcripts) into
/// `base`, with the returned context's `data_dir` pointing at the same tree
/// the editor's own boot discovery will read.
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

/// The backing (rendered-scrollback) file of the terminal in the editor's
/// currently active buffer.
fn active_terminal_backing(editor: &fresh::app::Editor) -> PathBuf {
    let buffer_id = editor.active_buffer();
    let window = editor.active_window();
    let terminal_id = window
        .get_terminal_id(buffer_id)
        .expect("active buffer must be a terminal buffer");
    window
        .terminal_backing_files
        .get(&terminal_id)
        .cloned()
        .expect("a spawned terminal must have a backing file")
}

fn read_to_string(path: &Path) -> String {
    std::fs::read(path)
        .map(|bytes| String::from_utf8_lossy(&bytes).into_owned())
        .unwrap_or_default()
}

/// Both scenarios run from one `#[test]`: each sets the process-global
/// `XDG_DATA_HOME`, so running them as separate (concurrent) test functions
/// would let one scenario's sandbox move out from under the other's
/// workspace save.
#[test]
fn a_new_terminal_never_shows_another_terminals_scrollback() {
    if !pty_available() {
        eprintln!("Skipping: no PTY available in this environment");
        return;
    }
    new_terminal_does_not_adopt_a_restored_terminal_transcript();
    new_terminal_discards_a_leftover_transcript_on_its_path();
}

fn new_terminal_does_not_adopt_a_restored_terminal_transcript() {
    let sandbox = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(sandbox.path());
    let project = sandbox.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let project = project.canonicalize().unwrap();

    // Session 1: two terminals, then close the first. What survives to the
    // workspace is the *second* one — the one holding `…-1`.
    {
        let mut e1 = editor_in(&project, &dir_context);
        e1.open_terminal();
        let first_terminal_buffer = e1.active_buffer();
        e1.open_terminal();
        e1.force_close_buffer(first_terminal_buffer).unwrap();
        e1.save_workspace().unwrap();
    }

    // Session 2: cold reboot. The survivor restores under a *new* id while
    // keeping its saved path. Plant a marker in that transcript standing in
    // for the history the user still has in their scroll-back.
    let mut e2 = editor_in(&project, &dir_context);
    e2.restore_active_window_on_launch(false).unwrap();
    let restored_backing = active_terminal_backing(&e2);
    const MARKER: &str = "RESTORED_TERMINAL_HISTORY";
    std::fs::write(&restored_backing, format!("{MARKER}\n")).unwrap();

    // …and now the user opens a brand-new terminal.
    e2.open_terminal();
    let new_backing = active_terminal_backing(&e2);

    assert_ne!(
        new_backing, restored_backing,
        "a new terminal must not be handed the transcript a live (restored) \
         terminal is still writing to"
    );
    assert!(
        !read_to_string(&new_backing).contains(MARKER),
        "the new terminal's scroll-back must not contain the restored \
         terminal's history; got: {:?}",
        read_to_string(&new_backing)
    );
    // The restored terminal keeps its own history — the fix must not have
    // moved *it* off its file either.
    assert!(
        read_to_string(&restored_backing).contains(MARKER),
        "the restored terminal must keep streaming into its own transcript"
    );
}

fn new_terminal_discards_a_leftover_transcript_on_its_path() {
    let sandbox = tempfile::tempdir().unwrap();
    let dir_context = isolated_dir_context(sandbox.path());
    let project = sandbox.path().join("project");
    std::fs::create_dir(&project).unwrap();
    let project = project.canonicalize().unwrap();

    // A previous run died without closing its terminal, leaving a transcript
    // on the path this run's first terminal will be offered.
    const MARKER: &str = "DEAD_SESSION_HISTORY";
    let terminal_root = dir_context.terminal_dir_for(&project);
    std::fs::create_dir_all(&terminal_root).unwrap();
    let leftover = terminal_root.join("fresh-terminal-0.txt");
    std::fs::write(&leftover, format!("{MARKER}\n")).unwrap();

    let mut editor = editor_in(&project, &dir_context);
    editor.open_terminal();
    let backing = active_terminal_backing(&editor);

    assert!(
        !read_to_string(&backing).contains(MARKER),
        "a new terminal must start from an empty transcript, not inherit the \
         one a dead session left on its path; got: {:?}",
        read_to_string(&backing)
    );
}
