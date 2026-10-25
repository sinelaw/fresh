//! E2E tests for buffer-group panels that show a buffer the user already has
//! open (a layout leaf with `bufferId`), rather than one the group creates.
//!
//! Such a panel is a view of an ordinary buffer:
//!
//! - the buffer stays editable, and keeps its own tab;
//! - closing the group leaves the buffer open, and closing the buffer closes
//!   the group;
//! - the panels open where the pane was in the buffer, and the pane takes
//!   the focused panel's place back when the group closes;
//! - every close of a group, however it happens, fires `buffer_group_closed`;
//! - two panels in a scroll-sync group scroll together.
//!
//! The test plugin opens a group "PAIR" with the active file in both panels,
//! side by side and scroll-synced. Assertions read the screen only
//! (CONTRIBUTING.md Testing §2), and each step waits for the screen it
//! expects (§3).

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

const PLUGIN_NAME: &str = "fpanel";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

let groupId = -1;
let fileBuffer = -1;
let closed = 0;

registerHandler("fpanel_open", async () => {
    fileBuffer = editor.getActiveBufferId();
    const panel = (id: string) => ({ type: "scrollable", id, bufferId: fileBuffer });
    const group = await editor.createBufferGroup("PAIR", "", JSON.stringify({
        type: "split",
        direction: "h",
        ratio: 0.5,
        first: panel("left"),
        second: panel("right"),
    }));
    groupId = group.groupId;
    editor.createScrollSyncGroup(7, group.panelSplits.left, group.panelSplits.right);
    editor.setStatus("fpanel-open");
});
editor.registerCommand("fpanel: Open", "Show the file in a two-panel group", "fpanel_open", null);

registerHandler("fpanel_close", () => {
    editor.closeBufferGroup(groupId);
});
editor.registerCommand("fpanel: Close", "Close the group", "fpanel_close", null);

registerHandler("fpanel_close_file", () => {
    editor.closeBuffer(fileBuffer);
});
editor.registerCommand("fpanel: Close File", "Close the file", "fpanel_close_file", null);

editor.on("buffer_group_closed", (data) => {
    closed++;
    editor.setStatus(`fpanel-closed-${data.group_id === groupId ? "pair" : "other"}-${closed}`);
});
"#;

fn setup() -> (EditorTestHarness, tempfile::TempDir) {
    setup_with(Config::default())
}

fn setup_with(config: Config) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().unwrap();
    let root = temp.path().join("work");
    let plugins_dir = root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    fs::write(plugins_dir.join(format!("{PLUGIN_NAME}.ts")), PLUGIN_SOURCE).unwrap();
    copy_plugin_lib(&plugins_dir);

    let file = root.join("a.txt");
    let mut text = String::from("alpha zebrafish\n");
    for i in 1..=200 {
        text.push_str(&format!("row {i:03}\n"));
    }
    fs::write(&file, text).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, root).unwrap();
    harness.open_file(&file).unwrap();
    harness.wait_for_screen_contains("row 001").unwrap();
    (harness, temp)
}

fn run(h: &mut EditorTestHarness, command: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(command).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
}

fn count(h: &EditorTestHarness, text: &str) -> usize {
    h.screen_to_string().matches(text).count()
}

/// The group's tab and the file's own tab, both in the pane's tab bar.
fn tabs_show_group_and_file(h: &EditorTestHarness) -> bool {
    let tabs = h.screen_row_text(1);
    tabs.contains("PAIR") && tabs.contains("a.txt")
}

/// The screen column `text` starts at in the tab bar, if it is there.
fn tab_col(h: &EditorTestHarness, text: &str) -> Option<u16> {
    let tabs = h.screen_row_text(1);
    let byte = tabs.find(text)?;
    Some(tabs[..byte].chars().count() as u16)
}

/// Open the group and wait for both panels to show the file.
fn open_pair(h: &mut EditorTestHarness) {
    run(h, "fpanel: Open");
    h.wait_until(|h| count(h, "row 001") == 2 && tabs_show_group_and_file(h))
        .unwrap();
}

/// Typing in a panel edits the file, which the other panel shows too; the
/// file keeps its own tab beside the group's.
#[test]
fn typing_in_a_panel_edits_the_file() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    h.type_text("XYZ").unwrap();
    h.wait_until(|h| count(h, "XYZalpha") == 2 && tabs_show_group_and_file(h))
        .unwrap();
}

/// Closing the group leaves the file open, in the pane that showed the group.
#[test]
fn closing_the_group_leaves_the_file_open() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    run(&mut h, "fpanel: Close");
    h.wait_until(|h| {
        count(h, "row 001") == 1
            && !h.screen_row_text(1).contains("PAIR")
            && h.screen_to_string().contains("fpanel-closed-pair-1")
    })
    .unwrap();
}

/// Closing the file closes the group showing it.
#[test]
fn closing_the_file_closes_its_group() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    run(&mut h, "fpanel: Close File");
    h.wait_until(|h| {
        count(h, "row 001") == 0
            && !h.screen_row_text(1).contains("PAIR")
            && h.screen_to_string().contains("fpanel-closed-pair-1")
    })
    .unwrap();
}

/// The user closing the group's tab fires `buffer_group_closed` too.
#[test]
fn closing_the_group_tab_tells_the_plugin() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    run(&mut h, "Close Tab");
    h.wait_until(|h| {
        count(h, "row 001") == 1 && h.screen_to_string().contains("fpanel-closed-pair-1")
    })
    .unwrap();
}

/// The panels open where the pane was in the file, and closing the group
/// leaves the pane where the focused panel was.
#[test]
fn the_pane_and_its_panels_keep_the_place_in_the_file() {
    let (mut h, _tmp) = setup();
    h.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| count(h, "row 200") == 1).unwrap();

    run(&mut h, "fpanel: Open");
    h.wait_until(|h| count(h, "row 200") == 2 && tabs_show_group_and_file(h))
        .unwrap();

    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| count(h, "row 001") == 2).unwrap();
    run(&mut h, "fpanel: Close");
    h.wait_until(|h| count(h, "row 001") == 1 && !h.screen_row_text(1).contains("PAIR"))
        .unwrap();
}

/// A popup anchored to the caret opens under the caret of the panel with the
/// keyboard. Here it is the completion list, which typing a word's start in
/// a panel brings up: its frame on the row below the caret's, and the
/// suggestion on the row after.
#[test]
fn a_popup_at_the_caret_opens_under_the_panels_caret() {
    let mut config = Config::default();
    config.editor.completion_popup_auto_show = true;
    let (mut h, _tmp) = setup_with(config);
    open_pair(&mut h);

    // A new first line, and the start of a word the file has.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    h.type_text("zeb").unwrap();
    h.wait_until(|h| {
        // The caret's row is the first to show what was typed: the popup
        // is under it.
        let Some(caret_row) = (0..30).find(|&r| h.screen_row_text(r).contains("zeb")) else {
            return false;
        };
        h.screen_row_text(caret_row + 1).contains("[×]")
            && h.screen_row_text(caret_row + 2).contains("zebrafish")
    })
    .unwrap();
}

/// Clicking the file's own tab while the group is shown shows the file: one
/// pane of it, with the group's tab still there to come back to.
#[test]
fn the_files_own_tab_switches_away_from_the_group() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    let col = tab_col(&h, "a.txt").unwrap();
    h.mouse_click(col + 1, 1).unwrap();
    // One pane, settled: the row has one gutter, not a panel's on each side
    // (the tab switch slides, and a frame of the slide can show one row).
    h.wait_until(|h| {
        let one_pane = (0..30)
            .map(|r| h.screen_row_text(r))
            .find(|row| row.contains("row 001"))
            .is_some_and(|row| row.matches('│').count() == 1);
        count(h, "row 001") == 1 && one_pane && tabs_show_group_and_file(h)
    })
    .unwrap();
}

/// Closing the file's tab while a panel shows it closes the file, so a file
/// with unsaved changes asks first, as closing its last tab always does. A
/// panel showing it is not another tab of it.
#[test]
fn closing_the_files_tab_asks_about_unsaved_changes() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);
    h.type_text("XYZ").unwrap();
    h.wait_until(|h| count(h, "XYZalpha") == 2).unwrap();

    let tabs = h.screen_row_text(1);
    let name = tabs.find("a.txt").unwrap();
    let close = name + tabs[name..].find('×').unwrap();
    h.mouse_click(tabs[..close].chars().count() as u16, 1)
        .unwrap();
    h.wait_for_screen_contains("Unsaved Changes").unwrap();
}

/// Closing the group returns the pane to the file the group showed, even
/// when another tab was opened in the pane while the group was up.
#[test]
fn closing_the_group_returns_to_its_file() {
    let (mut h, tmp) = setup();
    open_pair(&mut h);
    let other = tmp.path().join("work").join("b.txt");
    fs::write(&other, "bee bee bee\n").unwrap();
    h.open_file(&other).unwrap();
    h.wait_for_screen_contains("bee bee bee").unwrap();

    // Back to the group's tab, which now has b.txt behind it.
    let col = tab_col(&h, "PAIR").unwrap();
    h.mouse_click(col + 1, 1).unwrap();
    h.wait_until(|h| count(h, "row 001") == 2).unwrap();

    run(&mut h, "fpanel: Close");
    h.wait_until(|h| {
        count(h, "row 001") == 1
            && !h.screen_to_string().contains("bee bee bee")
            && !h.screen_row_text(1).contains("PAIR")
    })
    .unwrap();
}

/// A file that moves to another workspace takes no group of the window it
/// leaves with it: a group showing it there closes, rather than staying
/// behind with empty panels.
#[test]
fn a_file_moved_to_another_workspace_closes_its_group() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    run(&mut h, "Extract Tab to New Workspace");
    h.wait_until(|h| count(h, "row 001") == 1 && !h.screen_row_text(1).contains("PAIR"))
        .unwrap();

    // The window it left: no group tab there either.
    run(&mut h, "Previous Window");
    h.wait_until(|h| {
        let tabs = h.screen_row_text(1);
        !tabs.contains("a.txt") && !tabs.contains("PAIR")
    })
    .unwrap();
}

/// Two panels in a scroll-sync group scroll together: the focused one jumps
/// to the end, and the other follows.
#[test]
fn panels_in_a_scroll_sync_group_scroll_together() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    h.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| count(h, "row 200") == 2).unwrap();
}

/// Two panes: the left one's only tab is the file, with the group open
/// beside it and the file modified in it; the right one shows `b.txt`.
fn pair_in_a_split_beside_another_file(h: &mut EditorTestHarness, tmp: &tempfile::TempDir) {
    run(h, "split vert");
    let other = tmp.path().join("work").join("b.txt");
    fs::write(&other, "bee bee bee\n").unwrap();
    h.open_file(&other).unwrap();
    h.wait_for_screen_contains("bee bee bee").unwrap();
    // Drop the file's tab from the new pane, so only the left one has it.
    h.send_key(KeyCode::PageUp, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| count(h, "row 001") == 2).unwrap();
    h.send_key(KeyCode::Char('w'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| count(h, "row 001") == 1 && count(h, "bee bee bee") == 1)
        .unwrap();

    // Focus the left pane, its caret at the start of the file.
    let (col, row) = h.find_text_on_screen("alpha zebrafish").unwrap();
    h.mouse_click(col, row).unwrap();
    open_pair(h);
    h.type_text("XYZ").unwrap();
    h.wait_until(|h| count(h, "XYZalpha") == 2).unwrap();
}

/// Close the file's tab with its × and answer the unsaved-changes dialog
/// with `key`.
fn close_the_files_tab_answering(h: &mut EditorTestHarness, key: char) {
    let tabs = h.screen_row_text(1);
    let name = tabs.find("a.txt").unwrap();
    let close = name + tabs[name..].find('×').unwrap();
    h.mouse_click(tabs[..close].chars().count() as u16, 1)
        .unwrap();
    h.wait_for_screen_contains("Unsaved Changes").unwrap();
    h.send_key(KeyCode::Char(key), KeyModifiers::NONE).unwrap();
}

/// The left pane, emptied by closing the file, is gone with the group: the
/// right pane is alone on screen. Its tab bar has no separator through it.
fn only_the_other_file_is_left(h: &EditorTestHarness) -> bool {
    let tabs = h.screen_row_text(1);
    !tabs.contains('│')
        && !tabs.contains("PAIR")
        && !tabs.contains("a.txt")
        && count(h, "row 001") == 0
        && count(h, "bee bee bee") == 1
}

/// Discarding the changes of a file whose only tab is in its pane closes
/// the pane, as closing an unmodified file's last tab does. The group's
/// panels showing the file are no tabs of it, so they do not keep the pane.
#[test]
fn discarding_the_files_changes_closes_its_emptied_pane() {
    let (mut h, tmp) = setup();
    pair_in_a_split_beside_another_file(&mut h, &tmp);

    close_the_files_tab_answering(&mut h, 'd');
    h.wait_until(only_the_other_file_is_left).unwrap();
}

/// Same with Save: the file is written, then its emptied pane closes.
#[test]
fn saving_the_file_on_close_closes_its_emptied_pane() {
    let (mut h, tmp) = setup();
    pair_in_a_split_beside_another_file(&mut h, &tmp);

    close_the_files_tab_answering(&mut h, 's');
    h.wait_until(only_the_other_file_is_left).unwrap();
    let saved = fs::read_to_string(tmp.path().join("work").join("a.txt")).unwrap();
    assert!(
        saved.starts_with("XYZalpha zebrafish\n"),
        "saved: {saved:?}"
    );
}
