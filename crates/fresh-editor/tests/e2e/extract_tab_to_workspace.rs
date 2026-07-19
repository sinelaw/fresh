//! E2E coverage for "Extract Tab to New Workspace": the command palette
//! command and the matching tab right-click context menu item that move the
//! focused tab's buffer into a new orchestrator workspace (a `Window`)
//! rooted at the file's parent directory.
//!
//! Following CONTRIBUTING.md's "observe, not inspect" rule, these tests
//! assert only on rendered output: the status bar confirms the extraction
//! ("Extracted <name> into workspace <label>"), the new workspace shows the
//! moved tab, and cycling back to the source window shows the tab is gone.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
#[cfg(target_os = "linux")]
use fresh::config::{Config, TerminalShellConfig};
use portable_pty::{native_pty_system, PtySize};
use std::fs;

/// Helper: open the command palette, type the given query, accept the first
/// suggestion via Tab, and execute it with Enter.
fn run_command_palette(harness: &mut EditorTestHarness, query: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(query).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Wide harness so the status bar isn't clipped at the right edge — the
/// extraction status line includes a file name and a workspace label.
fn wide_temp_project_harness() -> EditorTestHarness {
    EditorTestHarness::with_temp_project_and_config(220, 30, Default::default()).unwrap()
}

/// Locate the active tab on screen so right-click events land on a real tab
/// rather than empty header space.
fn active_tab_position(harness: &EditorTestHarness) -> (u16, u16) {
    let active = harness.editor().active_buffer();
    for tab_layout in harness.editor().get_tab_layouts().values() {
        for tab in &tab_layout.tabs {
            if tab.buffer_id() == Some(active) {
                let center_col = tab.tab_area.x + tab.tab_area.width / 2;
                return (center_col, tab.tab_area.y);
            }
        }
    }
    panic!("active tab not found in tab layouts");
}

/// Set up a project with `keep.txt` at the root (so the source window keeps
/// a real tab after the extraction) and `subproj/notes.txt`, which is opened
/// last so its tab is the focused one.
fn harness_with_subproject_file() -> EditorTestHarness {
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("keep.txt"), "keep\n").unwrap();
    let subdir = project_root.join("subproj");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("notes.txt"), "notes\n").unwrap();

    harness.open_file(&project_root.join("keep.txt")).unwrap();
    harness.open_file(&subdir.join("notes.txt")).unwrap();
    harness.render().unwrap();
    harness
}

#[test]
fn extract_tab_via_command_palette_moves_buffer_to_new_workspace() {
    let mut harness = harness_with_subproject_file();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");

    // The status line confirms both the moved buffer and the new workspace's
    // label (the parent directory's basename).
    harness.assert_screen_contains("Extracted notes.txt into workspace subproj");
    // The new workspace is active and shows the moved tab.
    harness.assert_screen_contains("notes.txt");

    // Cycle back to the source window: the extracted tab is gone, the
    // remaining tab survived.
    run_command_palette(&mut harness, "Next Window");
    harness.assert_screen_contains("keep.txt");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("notes.txt"),
        "extracted tab should no longer render in the source window, got screen:\n{screen}"
    );
}

#[test]
fn extract_tab_preserves_unsaved_edits_and_undo_history() {
    let mut harness = harness_with_subproject_file();

    // Edit the buffer without saving — the extraction must move the live
    // buffer state, not re-read the file from disk.
    harness.type_text("EDITED ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("EDITED notes");

    run_command_palette(&mut harness, "Extract Tab to New Workspace");

    harness.assert_screen_contains("Extracted notes.txt into workspace subproj");
    // The unsaved edit is still there in the new workspace...
    harness.assert_screen_contains("EDITED notes");

    // ...and undo still works, because the undo history traveled with the
    // buffer instead of being stranded in the source window. Each keystroke
    // is its own undo step, so undo once per typed character.
    for _ in 0.."EDITED ".len() {
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("EDITED"),
        "undo after extraction should revert the pre-extraction edit, got screen:\n{screen}"
    );
}

#[test]
fn tab_right_click_menu_lists_extract_entry() {
    let mut harness = harness_with_subproject_file();

    let (col, row) = active_tab_position(&harness);
    harness.mouse_right_click(col, row).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Extract to New Workspace");
}

#[test]
fn tab_right_click_extract_moves_buffer_to_new_workspace() {
    let mut harness = harness_with_subproject_file();

    let (col, row) = active_tab_position(&harness);
    harness.mouse_right_click(col, row).unwrap();
    harness.render().unwrap();

    let (item_col, item_row) = harness
        .find_text_on_screen("Extract to New Workspace")
        .expect("'Extract to New Workspace' should be visible after tab right-click");
    harness.mouse_click(item_col, item_row).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Extracted notes.txt into workspace subproj");
}

#[test]
fn extract_last_tab_leaves_source_window_with_scratch_buffer() {
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();

    let subdir = project_root.join("subproj");
    fs::create_dir(&subdir).unwrap();
    fs::write(subdir.join("notes.txt"), "notes\n").unwrap();
    // The initial scratch buffer is repurposed by the first open, so
    // notes.txt is the source window's only tab.
    harness.open_file(&subdir.join("notes.txt")).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness.assert_screen_contains("Extracted notes.txt into workspace subproj");

    // The source window must stay renderable: cycling back shows a fresh
    // scratch buffer where the extracted tab used to be.
    run_command_palette(&mut harness, "Next Window");
    harness.assert_screen_contains("[No Name]");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("notes.txt"),
        "extracted tab should no longer render in the source window, got screen:\n{screen}"
    );
}

#[test]
fn extract_tab_on_unsaved_buffer_reports_no_path() {
    let mut harness = EditorTestHarness::new(120, 24).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");

    harness.assert_screen_contains("Cannot extract: buffer has no file path");
}

/// The extracted workspace inherits the source workspace's authority
/// *configuration* (its backend spec) rather than being silently downgraded
/// to a plain local backend. A `Plugin` (devcontainer/docker) spec is used as
/// the non-local marker: it makes `authority_spec.is_remote()` true, and its
/// reconnect is the owning plugin's job so core's reconnect-on-activate is a
/// no-op — no live connection is attempted in the test.
#[test]
fn extract_inherits_source_workspace_authority_spec() {
    use fresh::services::authority::{
        AuthorityPayload, FilesystemSpec, SessionAuthoritySpec, SpawnerSpec, TerminalWrapperSpec,
    };

    let mut harness = harness_with_subproject_file();

    // Give the source window a non-local backend spec (its live authority
    // stays the local placeholder — only the persisted config is remote).
    let source_spec = SessionAuthoritySpec::Plugin(AuthorityPayload {
        filesystem: FilesystemSpec::Local,
        spawner: SpawnerSpec::Local,
        terminal_wrapper: TerminalWrapperSpec::HostShell,
        display_label: "test-container".to_string(),
        path_translation: None,
    });
    let source_id = harness.editor().active_window_id();
    harness
        .editor_mut()
        .set_session_authority_spec(source_id, source_spec.clone());

    // Extract the focused tab (notes.txt → a new workspace rooted at subproj).
    let bid = harness.editor().active_buffer();
    harness.editor_mut().extract_tab_to_new_workspace(bid);
    harness.render().unwrap();
    harness.assert_screen_contains("into workspace subproj");

    // The extraction landed in a new window whose authority config matches the
    // source — not a downgraded `Local`.
    let target_id = harness.editor().active_window_id();
    assert_ne!(
        target_id, source_id,
        "extraction should land in a distinct new window"
    );
    let target_spec = harness
        .editor()
        .session(target_id)
        .expect("target window exists")
        .authority_spec
        .clone();
    assert_eq!(
        target_spec, source_spec,
        "extracted workspace must inherit the source's authority configuration, \
         not downgrade to Local"
    );
    assert!(
        target_spec.is_remote(),
        "sanity: the inherited spec must be the non-local one we set"
    );
}

// ── Terminal tab coverage ────────────────────────────────────────────────────

/// True when this environment can open a PTY (containers/sandboxes may not).
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

/// A wide temp-project harness whose terminals run a deterministic POSIX
/// shell (no rc files, predictable `cd`/`echo`/arithmetic behavior). Only
/// used by the Linux-gated live-cwd test — `/bin/sh` does not exist on
/// Windows.
#[cfg(target_os = "linux")]
fn sh_terminal_harness() -> EditorTestHarness {
    let mut config = Config::default();
    config.terminal.shell = Some(TerminalShellConfig {
        command: "/bin/sh".to_string(),
        args: Vec::new(),
    });
    EditorTestHarness::with_temp_project_and_config(220, 30, config).unwrap()
}

/// Extracting a terminal tab moves the *live* PTY into a new workspace
/// rooted at the shell's current working directory: the running shell keeps
/// working (its output streams into the new window), and the tab disappears
/// from the source window.
///
/// Linux-only: the live cwd is read from `/proc/<pid>/cwd`; elsewhere the
/// extraction falls back to the spawn cwd, which this test's `cd` would not
/// update.
#[cfg(target_os = "linux")]
#[test]
fn extract_terminal_tab_moves_live_terminal_to_cwd_workspace() {
    if !pty_available() {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return;
    }
    let mut harness = sh_terminal_harness();
    let project_root = harness.project_dir().unwrap();

    fs::write(project_root.join("keep.txt"), "keep\n").unwrap();
    let subdir = project_root.join("termproj");
    fs::create_dir(&subdir).unwrap();

    harness.open_file(&project_root.join("keep.txt")).unwrap();
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // `cd` into the subdirectory; the arithmetic marker only renders once
    // the shell has actually run the command (the echoed *input* line says
    // "CDMARK$((40+2))", never "CDMARK42"), so waiting on it is a race-free
    // signal that the live cwd now points at termproj.
    harness
        .type_text(&format!("cd {} && echo CDMARK$((40+2))", subdir.display()))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("CDMARK42").unwrap();

    // The palette is reachable from terminal mode (Ctrl+P bypasses PTY
    // capture) and the command is available in the Terminal context.
    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness
        .wait_for_screen_contains("into workspace termproj")
        .unwrap();

    // The PTY moved live: it still runs, has terminal focus in the new
    // workspace, and its (retagged) output streams into this window.
    harness.type_text("echo LIVE$((2+3))").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("LIVE5").unwrap();

    // Back in the source window: the terminal tab is gone, keep.txt stayed.
    run_command_palette(&mut harness, "Next Window");
    harness.assert_screen_contains("keep.txt");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("*Terminal"),
        "extracted terminal tab should no longer render in the source window, got screen:\n{screen}"
    );
}

/// A terminal still sitting in the workspace root has nowhere to extract to.
/// Uses the platform's default shell — nothing is typed into it, so the
/// test is shell-agnostic (on non-Linux the live-cwd read falls back to the
/// spawn cwd, which is the workspace root here either way).
#[test]
fn extract_terminal_tab_at_workspace_root_reports_status() {
    if !pty_available() {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return;
    }
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(220, 30, Default::default()).unwrap();

    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness.assert_screen_contains("Already in a workspace rooted at");
}

#[test]
fn extract_tab_already_at_workspace_root_reports_status() {
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();

    let file_path = project_root.join("rooted.txt");
    fs::write(&file_path, "x\n").unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");

    // The file's parent directory IS the current workspace root — there is
    // nowhere to extract to, and the status line says so.
    harness.assert_screen_contains("Already in a workspace rooted at");
}

/// Extracting a *terminal* tab into a session whose root matches an existing
/// **dormant** (persisted-but-not-yet-materialized) window keeps the PTY live:
/// `create_window_at` dedups by root against dormant windows too, so the
/// extract reuses that session, and the moved terminal must still run and
/// render once the session is materialized. This pins the interaction between
/// terminal extraction and the lazy dormant-window restore (the extract now
/// materializes the reused session *before* the move — see
/// `extract_terminal_tab_to_new_workspace`).
///
/// Linux-only for two reasons that happen to coincide: the live cwd read
/// (`/proc/<pid>/cwd`) and `dirs::data_dir()` honoring `XDG_DATA_HOME` (used
/// here to align the global save/load dir with the discovery `DirectoryContext`
/// so the seeded dormant workspace is both found and loadable). nextest's
/// process-per-test isolation makes the env mutation safe.
#[cfg(target_os = "linux")]
#[test]
fn extract_terminal_into_dormant_session_keeps_live_pty() {
    use fresh::config_io::DirectoryContext;
    use fresh::workspace::Workspace;
    use fresh_core::WindowId;

    if !pty_available() {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return;
    }

    let sandbox = tempfile::tempdir().unwrap();
    // Align the global (XDG) data dir with the DirectoryContext so the seeded
    // workspace is both discoverable (context path) and loadable (global path);
    // `for_testing` would split them and the dormant window would come back
    // empty regardless of the bug.
    let xdg = sandbox.path().join("xdg");
    std::fs::create_dir_all(&xdg).unwrap();
    std::env::set_var("XDG_DATA_HOME", &xdg);
    let data_dir = xdg.join("fresh");

    let project = sandbox.path().join("project");
    let subproj = project.join("termproj");
    std::fs::create_dir_all(&subproj).unwrap();
    let project = project.canonicalize().unwrap();
    let subproj = subproj.canonicalize().unwrap();
    fs::write(project.join("keep.txt"), "keep\n").unwrap();

    // Seed a dormant session at `subproj`: a persisted (empty) workspace file
    // boot-time discovery surfaces as a not-yet-materialized window.
    let mut ws = Workspace::new(subproj.clone());
    ws.stable_id = Some("ws-dormant-terminal-regression".to_string());
    ws.save().unwrap();

    let dir_context = DirectoryContext {
        data_dir,
        config_dir: sandbox.path().join("config"),
        home_dir: Some(sandbox.path().join("home")),
        documents_dir: None,
        downloads_dir: None,
    };
    let mut config = Config {
        check_for_updates: false,
        ..Default::default()
    };
    // Deterministic POSIX shell (no rc files) so `cd`/`echo`/arithmetic behave.
    config.terminal.shell = Some(TerminalShellConfig {
        command: "/bin/sh".to_string(),
        args: Vec::new(),
    });
    let mut harness = EditorTestHarness::create(
        220,
        30,
        crate::common::harness::HarnessOptions::new()
            .with_working_dir(project.clone())
            .with_shared_dir_context(dir_context)
            .with_config(config)
            .with_empty_plugins_dir(),
    )
    .unwrap();
    harness.startup(true, &[]).unwrap();

    // Guard against a false negative: if discovery didn't surface the dormant
    // session, the extract would create a *fresh* window (materialize is a
    // no-op) and the terminal would survive regardless of the fix.
    let subproj_discovered = (1..=16u64).map(WindowId).any(|id| {
        harness
            .editor()
            .session(id)
            .map(|w| w.root.canonicalize().ok().as_deref() == Some(subproj.as_path()))
            .unwrap_or(false)
    });
    assert!(
        subproj_discovered,
        "test setup: the dormant subproj session was not discovered at boot"
    );

    // Open a terminal in the foreground (project) window and `cd` it into the
    // dormant session's root, so the extract dedups onto that session. The
    // echoed arithmetic marker only resolves once the shell has actually run
    // the command, making it a race-free signal the live cwd now points there.
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");
    harness
        .type_text(&format!("cd {} && echo CDMARK$((40+2))", subproj.display()))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("CDMARK42").unwrap();

    let term_bid = harness.editor().active_buffer();
    harness.editor_mut().extract_tab_to_new_workspace(term_bid);
    harness
        .wait_for_screen_contains("into workspace termproj")
        .unwrap();

    // The PTY moved live into the reused dormant session: it still runs and
    // has terminal focus, so a fresh command's output streams into this window.
    harness.type_text("echo LIVE$((2+3))").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("LIVE5").unwrap();
}
