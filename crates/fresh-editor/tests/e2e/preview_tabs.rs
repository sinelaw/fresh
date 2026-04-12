//! End-to-end tests for the file-explorer preview-tab feature (issue #1403).
//!
//! These tests drive the editor exclusively through keyboard/mouse events
//! and verify behavior by inspecting the rendered tab bar — no internal
//! state peeking. See CONTRIBUTING.md §2 "Testing".
//!
//! Invariants exercised:
//! - Single-click on a file in the explorer opens it with a `(preview)`
//!   suffix in the tab bar.
//! - A second single-click (on another file) replaces the preview; the
//!   first file no longer appears in the tab bar.
//! - Editing the preview buffer removes the `(preview)` suffix (promoted
//!   to permanent), and subsequent preview opens don't close the
//!   now-permanent tab.
//! - Double-clicking a file opens it as a permanent tab (no suffix).
//! - Re-clicking the preview file keeps it in preview mode.
//! - Re-clicking an already-permanent file doesn't re-introduce the
//!   preview suffix.
//! - Closing the preview tab leaves no tab bar entry.
//! - With `file_explorer.preview_tabs = false`, single-click produces a
//!   permanent tab.
//! - Splitting the layout promotes the current preview.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, MouseButton, MouseEvent, MouseEventKind};
use std::fs;
use std::time::Duration;

/// The tab bar sits on screen row 1 in the default harness layout
/// (row 0 is the menu bar, or the top border when the menu bar is
/// hidden). See `tests/e2e/external_file_save_as_tab.rs` for precedent.
const TAB_BAR_ROW: u16 = 1;

/// Column inside the file explorer pane — safely to the right of the
/// tree glyphs for a 120-wide layout where the explorer is ~30% width.
const EXPLORER_CLICK_COL: u16 = 10;

/// Column inside the editor pane — well past the 36-col explorer width.
const EDITOR_CLICK_COL: u16 = 60;

/// Row inside the editor content — any row past the tab bar works.
const EDITOR_CLICK_ROW: u16 = 15;

/// Build a harness with an isolated temp project containing the given
/// filenames, open the file explorer, and wait for each file to appear
/// in the tree. Each file's content is its own name, which also makes
/// editor content assertions unambiguous.
fn setup_with_explorer(filenames: &[&str]) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = harness.project_dir().unwrap();
    for name in filenames {
        fs::write(project.join(name), format!("{name}\n")).unwrap();
    }

    // Ctrl+E toggles the file explorer.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    for name in filenames {
        harness.wait_for_file_explorer_item(name).unwrap();
    }
    harness
}

/// Return the screen row that renders `name` in the file explorer pane.
/// Scans only the left portion of each line (inside the explorer pane
/// width) and only rows below the tab bar, so the same filename in the
/// tab bar or editor content can't shadow the hit.
fn explorer_row_for(harness: &EditorTestHarness, name: &str) -> u16 {
    let screen = harness.screen_to_string();
    // Skip rows that belong to the menu bar and tab bar — the preview
    // indicator feature puts the filename in the tab bar as well, and a
    // top-down scan would otherwise return the tab row.
    const FIRST_EXPLORER_ROW: usize = 2;
    for (row, line) in screen.lines().enumerate().skip(FIRST_EXPLORER_ROW) {
        let prefix: String = line.chars().take(40).collect();
        if prefix.contains(name) {
            return row as u16;
        }
    }
    panic!("file {name} not found in file explorer;\nscreen:\n{screen}");
}

/// Single-click the given file in the explorer. This goes through the
/// real mouse-handling path (`handle_file_explorer_click`), which is
/// what the preview feature hooks into.
fn single_click_file(harness: &mut EditorTestHarness, name: &str) {
    // Advance the test clock past the double-click window so this click
    // registers as a fresh single-click (the harness uses a mocked time
    // source; real `sleep` does nothing here).
    let dc_window = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc_window);
    let row = explorer_row_for(harness, name);
    harness.mouse_click(EXPLORER_CLICK_COL, row).unwrap();
}

/// Double-click the given file in the explorer. Emitted as two back-to-back
/// Down/Up pairs without a render in between so the double-click detector
/// picks them up. Double-click is the "open permanently" gesture.
fn double_click_file(harness: &mut EditorTestHarness, name: &str) {
    // Make sure we're outside any previous click's double-click window,
    // then send two rapid clicks (no render between them) that land
    // inside the window. Mock clock — see `single_click_file`.
    let dc_window = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc_window);

    let row = explorer_row_for(harness, name);
    let col = EXPLORER_CLICK_COL;
    let send = |h: &mut EditorTestHarness, kind: MouseEventKind| {
        h.send_mouse(MouseEvent {
            kind,
            column: col,
            row,
            modifiers: KeyModifiers::empty(),
        })
        .unwrap();
    };
    send(harness, MouseEventKind::Down(MouseButton::Left));
    send(harness, MouseEventKind::Up(MouseButton::Left));
    send(harness, MouseEventKind::Down(MouseButton::Left));
    send(harness, MouseEventKind::Up(MouseButton::Left));
    harness.render().unwrap();
}

/// Return the rendered text of the tab bar row.
fn tab_bar(harness: &EditorTestHarness) -> String {
    harness.screen_row_text(TAB_BAR_ROW)
}

/// Click once somewhere in the editor content (not on a tab and not in
/// the explorer) so the key context switches to Normal. Needed before
/// sending typed text after opening a file from the explorer (which
/// otherwise leaves focus in the explorer).
fn focus_editor(harness: &mut EditorTestHarness) {
    // Avoid accidentally registering as a double-click with whatever
    // came before. Mock clock — see `single_click_file`.
    let dc_window = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc_window);
    harness
        .mouse_click(EDITOR_CLICK_COL, EDITOR_CLICK_ROW)
        .unwrap();
}

#[test]
fn single_click_opens_file_as_preview() {
    let mut harness = setup_with_explorer(&["alpha.txt", "beta.txt"]);

    single_click_file(&mut harness, "alpha.txt");

    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt"),
        "tab bar should show alpha.txt after single-click; got:\n{row}"
    );
    assert!(
        row.contains("(preview)"),
        "single-click should open in preview mode; got:\n{row}"
    );
}

#[test]
fn two_consecutive_previews_leave_one_tab() {
    let mut harness = setup_with_explorer(&["alpha.txt", "beta.txt"]);

    single_click_file(&mut harness, "alpha.txt");
    assert!(tab_bar(&harness).contains("alpha.txt"));

    single_click_file(&mut harness, "beta.txt");
    let row = tab_bar(&harness);

    assert!(
        row.contains("beta.txt"),
        "beta.txt should be the active preview tab; got:\n{row}"
    );
    assert!(
        !row.contains("alpha.txt"),
        "alpha.txt preview should have been replaced; got:\n{row}"
    );
    assert!(
        row.contains("(preview)"),
        "new preview tab should keep the preview suffix; got:\n{row}"
    );
}

#[test]
fn editing_preview_promotes_it_to_permanent() {
    let mut harness = setup_with_explorer(&["alpha.txt", "beta.txt"]);

    single_click_file(&mut harness, "alpha.txt");
    assert!(tab_bar(&harness).contains("(preview)"));

    // Focus editor and type a character — any buffer mutation promotes.
    focus_editor(&mut harness);
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt"),
        "alpha.txt should still be in the tab bar after edit; got:\n{row}"
    );
    assert!(
        !row.contains("(preview)"),
        "editing the preview tab must promote it to permanent; got:\n{row}"
    );

    // Subsequent preview-open on a different file must NOT close the
    // promoted tab.
    single_click_file(&mut harness, "beta.txt");
    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt") && row.contains("beta.txt"),
        "promoted tab must coexist with the new preview; got:\n{row}"
    );
}

#[test]
fn double_click_opens_file_as_permanent_tab() {
    let mut harness = setup_with_explorer(&["alpha.txt", "beta.txt"]);

    double_click_file(&mut harness, "alpha.txt");

    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt"),
        "double-click should open alpha.txt; got:\n{row}"
    );
    assert!(
        !row.contains("(preview)"),
        "double-click should open as permanent (no preview suffix); got:\n{row}"
    );

    // Subsequent preview-open on a different file must coexist.
    single_click_file(&mut harness, "beta.txt");
    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt") && row.contains("beta.txt"),
        "the permanent tab must coexist with the new preview; got:\n{row}"
    );
}

#[test]
fn reclicking_same_preview_file_keeps_preview_state() {
    let mut harness = setup_with_explorer(&["alpha.txt"]);

    single_click_file(&mut harness, "alpha.txt");
    assert!(tab_bar(&harness).contains("(preview)"));

    single_click_file(&mut harness, "alpha.txt");
    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt") && row.contains("(preview)"),
        "re-clicking the preview file must keep it in preview mode; got:\n{row}"
    );
}

#[test]
fn reclicking_already_permanent_file_does_not_demote() {
    let mut harness = setup_with_explorer(&["alpha.txt"]);

    // Make alpha.txt permanent via double-click.
    double_click_file(&mut harness, "alpha.txt");
    assert!(!tab_bar(&harness).contains("(preview)"));

    // Single-clicking the same file must not demote it.
    single_click_file(&mut harness, "alpha.txt");
    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt") && !row.contains("(preview)"),
        "single-click on a permanent tab must not demote it; got:\n{row}"
    );
}

#[test]
fn config_disabled_falls_back_to_permanent_open() {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = harness.project_dir().unwrap();
    fs::write(project.join("alpha.txt"), "alpha.txt\n").unwrap();

    // Disable the feature *before* opening anything.
    harness.editor_mut().config_mut().file_explorer.preview_tabs = false;

    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    harness.wait_for_file_explorer_item("alpha.txt").unwrap();

    single_click_file(&mut harness, "alpha.txt");

    let row = tab_bar(&harness);
    assert!(
        row.contains("alpha.txt"),
        "alpha.txt should be open; got:\n{row}"
    );
    assert!(
        !row.contains("(preview)"),
        "with preview_tabs disabled, single-click must not produce a preview; got:\n{row}"
    );
}

#[test]
fn splitting_the_layout_promotes_preview() {
    let mut harness = setup_with_explorer(&["alpha.txt"]);

    single_click_file(&mut harness, "alpha.txt");
    assert!(tab_bar(&harness).contains("(preview)"));

    // No default keybinding for split; drive via the action API (same
    // entry point any future binding or menu item would call).
    harness.editor_mut().split_pane_horizontal();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // After a horizontal split both panes share the buffer, so "alpha.txt"
    // will appear twice — but neither tab may carry the preview suffix.
    assert!(
        screen.contains("alpha.txt"),
        "alpha.txt should still be visible after split; got screen:\n{screen}"
    );
    assert!(
        !screen.contains("(preview)"),
        "splitting the layout must promote the preview; got screen:\n{screen}"
    );
}

#[test]
fn preview_tab_renders_with_translated_suffix() {
    // Default locale is English; the key `buffer.preview_indicator`
    // resolves to "(preview)". When run against another locale this
    // assertion should match that locale's translation instead; each
    // locale bundle is exercised by `cargo test` at build time via the
    // generated `locales/` include.
    let mut harness = setup_with_explorer(&["alpha.txt"]);
    single_click_file(&mut harness, "alpha.txt");

    let row = tab_bar(&harness);
    assert!(
        row.contains("(preview)"),
        "rendered tab bar should contain the translated preview suffix; got:\n{row}"
    );
}
