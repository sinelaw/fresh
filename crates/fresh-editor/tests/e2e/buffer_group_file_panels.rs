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
    let temp = tempfile::TempDir::new().unwrap();
    let root = temp.path().join("work");
    let plugins_dir = root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    fs::write(plugins_dir.join(format!("{PLUGIN_NAME}.ts")), PLUGIN_SOURCE).unwrap();
    copy_plugin_lib(&plugins_dir);

    let file = root.join("a.txt");
    let mut text = String::from("alpha\n");
    for i in 1..=200 {
        text.push_str(&format!("row {i:03}\n"));
    }
    fs::write(&file, text).unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, Config::default(), root).unwrap();
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

/// Two panels in a scroll-sync group scroll together: the focused one jumps
/// to the end, and the other follows.
#[test]
fn panels_in_a_scroll_sync_group_scroll_together() {
    let (mut h, _tmp) = setup();
    open_pair(&mut h);

    h.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| count(h, "row 200") == 2).unwrap();
}
