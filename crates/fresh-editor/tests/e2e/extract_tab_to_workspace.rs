//! E2E coverage for "Extract Tab to New Workspace": the command palette
//! command and the matching tab right-click context menu item that move the
//! focused tab into a new orchestrator workspace (a `Window`) co-tenanting the
//! same project root — a second, independent window over the same project,
//! distinguished by a numbered label.
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
    let source_root = harness.editor().active_window().root.clone();
    let source_id = harness.editor().active_window_id();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");

    // The status line confirms the moved buffer and the new co-tenant
    // workspace's numbered label (a 2nd window over the same project root).
    harness.assert_screen_contains("Extracted notes.txt into workspace project_root (2)");
    // The new workspace is active and shows the moved tab.
    harness.assert_screen_contains("notes.txt");

    // The co-tenant shares the SAME root as the source — a second workspace
    // over the same project, not a directory guessed from the file path.
    let target_id = harness.editor().active_window_id();
    assert_ne!(target_id, source_id, "extraction lands in a new window");
    assert_eq!(
        harness.editor().active_window().root,
        source_root,
        "the extracted co-tenant is rooted at the source project root"
    );

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

    harness.assert_screen_contains("Extracted notes.txt into workspace project_root (2)");
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

    harness.assert_screen_contains("Extracted notes.txt into workspace project_root (2)");
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
    harness.assert_screen_contains("Extracted notes.txt into workspace project_root (2)");

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

    // Extract the focused tab into a co-tenant over the same project root.
    let bid = harness.editor().active_buffer();
    harness.editor_mut().extract_tab_to_new_workspace(bid);
    harness.render().unwrap();
    harness.assert_screen_contains("into workspace project_root (2)");

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

/// Extracting a terminal tab moves the *live* PTY into a new co-tenant
/// workspace over the same project root: the running shell keeps working (its
/// output streams into the new window), and the tab disappears from the
/// source window.
///
/// Linux-only: uses `/bin/sh` for a deterministic shell.
#[cfg(target_os = "linux")]
#[test]
fn extract_terminal_tab_moves_live_terminal_to_co_tenant() {
    if !pty_available() {
        eprintln!("Skipping terminal test: PTY not available in this environment");
        return;
    }
    let mut harness = sh_terminal_harness();
    let project_root = harness.project_dir().unwrap();
    let source_root = harness.editor().active_window().root.clone();

    fs::write(project_root.join("keep.txt"), "keep\n").unwrap();
    harness.open_file(&project_root.join("keep.txt")).unwrap();
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    harness.assert_screen_contains("*Terminal 0*");

    // A live marker so we can prove the *same* shell keeps running after the
    // move (the echoed input line never resolves the arithmetic).
    harness.type_text("echo PRE$((1+1))").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("PRE2").unwrap();

    // The palette is reachable from terminal mode (Ctrl+P bypasses PTY
    // capture) and the command is available in the Terminal context.
    run_command_palette(&mut harness, "Extract Tab to New Workspace");
    harness
        .wait_for_screen_contains("into workspace project_root (2)")
        .unwrap();
    // Rooted at the same project — a co-tenant, not the shell's cwd.
    assert_eq!(harness.editor().active_window().root, source_root);

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

/// A file sitting at the workspace root extracts fine now — it becomes a
/// co-tenant workspace over that same root (the old "already rooted, nowhere
/// to go" refusal is gone; multiple workspaces per root is the point).
#[test]
fn extract_root_level_file_creates_co_tenant() {
    let mut harness = wide_temp_project_harness();
    let project_root = harness.project_dir().unwrap();
    let source_root = harness.editor().active_window().root.clone();

    fs::write(project_root.join("keep.txt"), "keep\n").unwrap();
    fs::write(project_root.join("rooted.txt"), "x\n").unwrap();
    harness.open_file(&project_root.join("keep.txt")).unwrap();
    harness.open_file(&project_root.join("rooted.txt")).unwrap();
    harness.render().unwrap();

    run_command_palette(&mut harness, "Extract Tab to New Workspace");

    harness.assert_screen_contains("Extracted rooted.txt into workspace project_root (2)");
    assert_eq!(
        harness.editor().active_window().root,
        source_root,
        "the extracted co-tenant shares the source project root"
    );
}

// NOTE: the load-bearing cold-reboot round-trip for co-tenants — two windows
// over one root each restoring their OWN file — lives in the dedicated
// `orchestrator_co_tenant_restore` integration binary. It mutates the
// process-global `XDG_DATA_HOME` to isolate persistence, so it must NOT share
// this binary's process with the thousands of other e2e tests.
