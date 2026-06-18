//! E2E reproductions for two bugs in the Utility Dock interaction model.
//!
//! 1. **Quick Open routes new file into the dock.** With the Search/Replace
//!    panel open in the bottom dock (which takes focus), pressing Ctrl+P
//!    and selecting a file opens that file as a tab in the dock instead
//!    of the main editor split above. The dock is intended for
//!    panel-style content (search results, diagnostics, terminal,
//!    quickfix); ordinary file buffers should never land there.
//!
//! 2. **Maximize button targets the active split, not the clicked one.**
//!    Each split's tab bar has a `□` maximize button. Clicking the top
//!    split's button while the dock is focused maximizes the dock
//!    instead of the top split — the click handler ignores which
//!    button was clicked and toggles the *active* split.
//!
//! Both tests assert only on rendered output, per CONTRIBUTING.md §2.
//! Each fails on `master` and passes once the corresponding fix lands.
//!
//! See repro session in tmux for the manual steps these tests automate.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Set up a project directory with the search_replace plugin and a few text files.
fn setup_dock_project() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    fs::write(project_root.join("main.txt"), "main file content\n").unwrap();
    fs::write(project_root.join("other.txt"), "other file content\n").unwrap();

    // The Quick Open file finder uses `git ls-files`, so the project must
    // be a git repo with the test files tracked for them to appear in the
    // picker.
    let status = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&project_root)
        .status()
        .unwrap();
    assert!(status.success());
    let status = std::process::Command::new("git")
        .args(["add", "main.txt", "other.txt"])
        .current_dir(&project_root)
        .status()
        .unwrap();
    assert!(status.success());

    (temp_dir, project_root)
}

/// Open the Search/Replace panel via the command palette and wait for it
/// to render in the bottom dock with focus.
fn open_search_replace_panel(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Search and Replace").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Search and Replace"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Panel renders with the Search field — this is the dock taking focus.
    harness
        .wait_until(|h| h.screen_to_string().contains("Search:"))
        .unwrap();
}

/// Return the screen row index (0-based) of the line containing the given text,
/// or panic with the screen for debugging.
fn row_of(harness: &EditorTestHarness, needle: &str) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("expected screen to contain '{needle}'\nScreen:\n{screen}"))
}

// ---------------------------------------------------------------------------
// Bug 1
// ---------------------------------------------------------------------------

/// With the Search/Replace dock open and focused, pressing Ctrl+P and
/// selecting a file should open the file in the **main** split — not as a
/// new tab in the dock.
#[test]
fn test_quick_open_after_search_replace_opens_in_main_split() {
    let (_temp_dir, project_root) = setup_dock_project();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&project_root.join("main.txt")).unwrap();
    harness.render().unwrap();

    // Open Search/Replace — splits the bottom dock and gives it focus.
    open_search_replace_panel(&mut harness);

    // Sanity: dock and main split are distinct rows on screen.
    let dock_row = row_of(&harness, "*Search/Replace*");
    let main_tab_row = row_of(&harness, "main.txt");
    assert!(
        main_tab_row < dock_row,
        "expected main split's tab bar above the dock's tab bar (main_tab_row={main_tab_row}, \
         dock_row={dock_row})"
    );

    // Now: Ctrl+P → Backspace (drop the ">command" prefix that Quick Open
    // remembers from the last invocation) → type filename → Enter.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("other").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("other.txt"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the file to actually finish opening (status message confirms).
    harness
        .wait_until(|h| h.screen_to_string().contains("other.txt"))
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    // Let any post-open re-renders settle.
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("other.txt"))
        .unwrap();

    let screen = harness.screen_to_string();

    // The dock's tab bar (the row containing "*Search/Replace*") must not
    // host the newly opened ordinary file. Under the bug, this row reads
    // " *Search/Replace* ×   other.txt ×", which is wrong: the dock is
    // for utility panels, not file buffers.
    let dock_tab_line = screen
        .lines()
        .find(|l| l.contains("*Search/Replace*"))
        .unwrap_or_else(|| panic!("dock tab bar gone after open\nScreen:\n{screen}"));
    assert!(
        !dock_tab_line.contains("other.txt"),
        "Bug: ordinary file opened in the bottom dock instead of the main split.\n\
         Dock tab line: '{dock_tab_line}'\nFull screen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// Bug 2
// ---------------------------------------------------------------------------

/// Clicking the **top** split's maximize button while the dock is focused
/// must maximize the top split, not the dock. The click handler should
/// honor the split that owns the clicked button rather than blindly
/// toggling the currently-active split.
#[test]
fn test_maximize_button_click_targets_clicked_split_not_active() {
    let (_temp_dir, project_root) = setup_dock_project();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&project_root.join("main.txt")).unwrap();
    harness.render().unwrap();

    // Open Search/Replace — bottom dock takes focus.
    open_search_replace_panel(&mut harness);

    // Locate the maximize button on the *top* split's tab bar. The icon
    // is "□" when not maximized; the first occurrence on screen belongs
    // to the topmost split (the main editor pane).
    let (max_col, max_row) = harness
        .find_text_on_screen("□")
        .expect("expected a '□' maximize button on the top split's tab bar");
    // Sanity: the button is on the main split's tab bar (above the dock).
    let dock_row = row_of(&harness, "*Search/Replace*");
    assert!(
        (max_row as usize) < dock_row,
        "expected the first '□' button to belong to the top split (max_row={max_row}, \
         dock_row={dock_row})"
    );

    harness.mouse_click(max_col, max_row).unwrap();

    // Wait for the maximize transition to complete (status message confirms).
    harness
        .wait_until(|h| h.screen_to_string().contains("Maximized split"))
        .unwrap();
    harness
        .wait_until_stable(|h| h.screen_to_string().contains("Maximized split"))
        .unwrap();

    let screen = harness.screen_to_string();

    // After maximizing the top split, the dock's *Search/Replace* tab
    // must no longer be visible — only one split is shown. Under the bug
    // the dock gets maximized instead, so *Search/Replace* stays on
    // screen and the main split's "main.txt" tab disappears.
    assert!(
        !screen.contains("*Search/Replace*"),
        "Bug: clicking the top split's maximize button maximized the dock instead. \
         The Search/Replace panel is still visible.\nScreen:\n{screen}"
    );
    assert!(
        screen.contains("main.txt"),
        "After maximizing the top split, main.txt should still be on screen.\nScreen:\n{screen}"
    );
}

// ---------------------------------------------------------------------------
// Bug 3
// ---------------------------------------------------------------------------

/// Maximize the focused dock via the command palette ("Toggle Maximize
/// Split"). The dock currently holds focus, so it becomes the maximized
/// split and the editor content is hidden.
fn maximize_focused_dock(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Maximize Split").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Toggle Maximize Split"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // A maximized split shows the restore glyph "⧉" on its tab bar in
    // place of the "□" maximize glyph. (The global status bar's "Maximized
    // split" message is covered by the dock's own footer here, so the glyph
    // is the reliable rendered signal.)
    harness
        .wait_until(|h| h.screen_to_string().contains('⧉'))
        .unwrap();
}

/// Opening a file while a *different* split is maximized must restore the
/// layout so the freshly-focused buffer is actually visible.
///
/// This is the rendered-output repro of the embedded-`fresh` hang: with the
/// docked panel maximized, forwarding a file open from a nested process (or
/// any Quick Open — both go through `Editor::open_file`) focuses the buffer
/// in the editor split, but `get_visible_buffers` only draws the maximized
/// dock. The buffer renders hidden behind the dock, so the user sees no
/// change and a blocking `fresh <file>` forward looks like it has hung.
///
/// Fails on the buggy build (the opened file's content never renders) and
/// passes once the open path un-maximizes to reveal the focused split.
#[test]
fn test_open_file_while_split_maximized_reveals_buffer() {
    let (_temp_dir, project_root) = setup_dock_project();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&project_root.join("main.txt")).unwrap();
    harness.render().unwrap();

    // Open Search/Replace in the bottom dock (it takes focus), then
    // maximize it so only the dock is shown.
    open_search_replace_panel(&mut harness);
    maximize_focused_dock(&mut harness);

    // Precondition: maximizing the dock hides the editor — main.txt's
    // content is no longer on screen.
    harness
        .wait_until_stable(|h| !h.screen_to_string().contains("main file content"))
        .unwrap();

    // Now open another file via Quick Open. This focuses other.txt in the
    // editor split. Under the bug the dock stays maximized and the new
    // buffer renders behind it; the fix restores the layout.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    // Drop the ">" command-mode prefix the palette remembers from the
    // previous (maximize) invocation, so the filename query runs in file mode.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("other").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("other.txt"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // The "Opened …/other.txt" status (full path) lands in the always-visible
    // status bar in both the buggy and fixed builds, so this wait completes
    // either way — it just marks "the open finished". The assertion below is
    // what distinguishes them.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            s.contains("Opened ") && s.contains("other.txt")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("other file content"),
        "Bug: the opened file is hidden behind the maximized dock — its content never \
         rendered, so the user can't see the buffer that was opened.\nScreen:\n{screen}"
    );
}
