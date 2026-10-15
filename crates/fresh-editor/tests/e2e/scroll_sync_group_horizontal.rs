//! E2E tests for scroll-sync groups (`editor.createScrollSyncGroup`) beyond
//! the vertical lockstep they always had:
//!
//! - horizontal panning in one pane of a group pans its partner, whether the
//!   pan comes from the wheel (over either pane), a scrollbar drag or the
//!   cursor;
//! - a partner in compose (page) view is left where it is, because it wraps
//!   its own lines and has nothing to pan to;
//! - a group does not outlive one of its panes: a plugin that closes a pane
//!   and pairs again under the same group id gets a working group.
//!
//! After each action the tests paint only the frames the event loop would
//! (`paint_owed_frames`), so a partner that lags a frame behind fails.
//!
//! The test plugin builds the same layout a "source | preview" plugin
//! would: it splits the active pane and joins the two halves in group 42.
//! Both panes show the same buffer, so every assertion counts how many
//! panes render a marker (CONTRIBUTING.md Testing §2: observe the screen).

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers, MouseEvent, MouseEventKind};
use fresh::config::Config;
use std::fs;

const PLUGIN_NAME: &str = "hsync";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

let pairs = 0;
let right = -1;

registerHandler("hsync_pair", async () => {
    const created = await editor.splitWindow({
        direction: "vertical",
        place: "after",
        keepFocus: true,
    });
    right = created.splitId;
    editor.createScrollSyncGroup(42, created.sourceSplitId, created.splitId);
    pairs++;
    editor.setStatus(`hsync-paired-${pairs}`);
});
editor.registerCommand("hsync: Pair", "Split and sync", "hsync_pair", null);

// Pair the active pane with a new pane on a different, narrower file.
registerHandler("hsync_pair_short", async () => {
    const created = await editor.splitWindow({
        direction: "vertical",
        place: "after",
        keepFocus: true,
        file: "short.txt",
    });
    right = created.splitId;
    editor.createScrollSyncGroup(42, created.sourceSplitId, created.splitId);
    editor.setStatus("hsync-paired-short");
});
editor.registerCommand("hsync: Pair Short", "Split on short.txt and sync", "hsync_pair_short", null);

registerHandler("hsync_compose_right", async () => {
    const left = editor.getActiveSplitId();
    const buffer = editor.getActiveBufferId();
    editor.focusSplit(right);
    editor.setViewMode(buffer, "compose");
    editor.focusSplit(left);
    await editor.flush();
    editor.setStatus("hsync-composed");
});
editor.registerCommand("hsync: Compose Right", "Compose the right pane", "hsync_compose_right", null);

registerHandler("hsync_close_right", async () => {
    editor.closeSplit(right);
    await editor.flush();
    editor.setStatus("hsync-closed");
});
editor.registerCommand("hsync: Close Right", "Close the right pane", "hsync_close_right", null);

// Close the right pane and pair the left one with a new pane under the same
// id, with no frame in between: the close and the create reach the editor in
// one batch.
registerHandler("hsync_repair", async () => {
    const created = await editor.splitWindow({
        direction: "vertical",
        place: "after",
        keepFocus: true,
    });
    editor.closeSplit(right);
    editor.createScrollSyncGroup(42, created.sourceSplitId, created.splitId);
    right = created.splitId;
    pairs++;
    editor.setStatus(`hsync-paired-${pairs}`);
});
editor.registerCommand("hsync: Repair", "Close the right pane and pair again", "hsync_repair", null);
"#;

/// First line: a marker at column 0 and another one far past any pane's
/// width. The rest give the buffer enough height to scroll vertically.
fn fixture_text() -> String {
    let mut text = format!("START{}FINISH\n", "-".repeat(200));
    for i in 1..=200 {
        text.push_str(&format!("line {i:04}\n"));
    }
    text
}

fn harness_with_pair() -> (EditorTestHarness, tempfile::TempDir) {
    harness_paired_by("hsync: Pair", "hsync-paired-1", 2)
}

/// Pair `long.txt` with `short.txt`, whose lines are all narrower than a pane
/// (200 of them, so it scrolls vertically).
fn harness_with_short_partner() -> (EditorTestHarness, tempfile::TempDir) {
    harness_paired_by("hsync: Pair Short", "hsync-paired-short", 1)
}

fn harness_paired_by(
    command: &str,
    done_status: &str,
    start_markers: usize,
) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    fs::write(plugins_dir.join(format!("{PLUGIN_NAME}.ts")), PLUGIN_SOURCE).unwrap();
    copy_plugin_lib(&plugins_dir);

    let file = working_dir.join("long.txt");
    fs::write(&file, fixture_text()).unwrap();
    let short: String = (1..=200).map(|i| format!("short {i:04}\n")).collect();
    fs::write(working_dir.join("short.txt"), short).unwrap();

    let mut config = Config::default();
    config.editor.line_wrap = false;
    config.editor.show_horizontal_scrollbar = true;
    let mut harness = EditorTestHarness::with_config_and_working_dir(120, 30, config, working_dir)
        .expect("harness");
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    run_command(&mut harness, command, done_status);
    paint_owed_frames(&mut harness);
    assert_eq!(
        marker_count(&harness, "START"),
        start_markers,
        "setup: the panes should start at column 0. Screen:\n{}",
        harness.screen_to_string()
    );
    (harness, temp)
}

fn run_command(h: &mut EditorTestHarness, name: &str, done_status: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(name).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
    h.wait_for_screen_contains(done_status).unwrap();
}

fn marker_count(h: &EditorTestHarness, marker: &str) -> usize {
    h.screen_to_string().matches(marker).count()
}

/// Paint the frames the event loop owes after an event, as its tick would:
/// a tick that reports nothing owed paints nothing. Bounded, so two passes
/// that keep moving a pane back and forth fail rather than spin.
fn paint_owed_frames(h: &mut EditorTestHarness) {
    for _ in 0..10 {
        if !fresh::app::editor_tick(h.editor_mut(), || Ok(())).unwrap() {
            return;
        }
        h.render().unwrap();
    }
    panic!(
        "frames still owed after 10 ticks:\n{}",
        h.screen_to_string()
    );
}

/// Where `START` is drawn in the left pane and in the right pane, in screen
/// cells (the row's text holds multi-byte box-drawing glyphs).
fn start_cells(h: &EditorTestHarness) -> ((u16, u16), (u16, u16)) {
    let (left_col, row) = h.find_text_on_screen("START").unwrap();
    let text = h.screen_row_text(row);
    let byte = text.rfind("START").expect("START in the right pane");
    let right_col = text[..byte].chars().count() as u16;
    assert_ne!(left_col, right_col, "START should be drawn in both panes");
    ((left_col, row), (right_col, row))
}

/// A pair whose right pane is in compose view.
fn harness_with_compose_partner() -> (EditorTestHarness, tempfile::TempDir) {
    let (mut harness, temp) = harness_with_pair();
    run_command(&mut harness, "hsync: Compose Right", "hsync-composed");
    paint_owed_frames(&mut harness);
    assert_eq!(
        marker_count(&harness, "START"),
        2,
        "setup: both panes should show column 0. Screen:\n{}",
        harness.screen_to_string()
    );
    (harness, temp)
}

/// Shift+wheel (`ScrollRight`) `notches` times over the cell at `(col, row)`.
fn pan_right(h: &mut EditorTestHarness, col: u16, row: u16, notches: usize) {
    for _ in 0..notches {
        h.send_mouse(MouseEvent {
            kind: MouseEventKind::ScrollRight,
            column: col,
            row,
            modifiers: KeyModifiers::NONE,
        })
        .unwrap();
        h.render().unwrap();
        paint_owed_frames(h);
    }
}

/// Panning the focused pane with the wheel pans its partner too.
#[test]
fn wheel_pan_over_the_focused_pane_pans_its_partner() {
    let (mut harness, _tmp) = harness_with_pair();
    let ((col, row), _) = start_cells(&harness);

    pan_right(&mut harness, col + 2, row, 10);

    assert_eq!(
        marker_count(&harness, "START"),
        0,
        "both panes should have panned past column 0. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// The wheel over the *unfocused* pane pans both, and the next frame's sync
/// (which runs from the focused pane) does not pan them back.
#[test]
fn wheel_pan_over_the_unfocused_pane_pans_both_and_sticks() {
    let (mut harness, _tmp) = harness_with_pair();
    let (_, (col, row)) = start_cells(&harness);

    pan_right(&mut harness, col + 2, row, 10);

    assert_eq!(
        marker_count(&harness, "START"),
        0,
        "both panes should have panned past column 0. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// A pan that comes from the cursor (End on the long line) reaches the
/// partner as well: the frame-level sync carries it, not just the wheel.
#[test]
fn cursor_driven_pan_reaches_the_partner() {
    let (mut harness, _tmp) = harness_with_pair();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "FINISH"),
        2,
        "both panes should show the end of the long line. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// The partner takes the column the focused pane was *drawn* at. On a line
/// of tabs the cursor's visual column is far past its byte column, and only
/// the laid-out rows know the former; syncing the pre-frame (byte-measured)
/// placement left the partner near the start of the line.
#[test]
fn cursor_driven_pan_on_a_tabbed_line_reaches_the_partner() {
    let (mut harness, _tmp) = harness_with_pair();
    harness
        .type_text(&format!("{}TABEND\n", "\t".repeat(30)))
        .unwrap();
    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    paint_owed_frames(&mut harness);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "TABEND"),
        2,
        "both panes should show the end of the tabbed line. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// A partner on a narrower file cannot scroll as far as the focused pane: it
/// settles short of the copied column on every frame. That must not owe a
/// frame every frame — the editor would repaint forever.
#[test]
fn a_partner_that_cannot_scroll_as_far_does_not_repaint_forever() {
    let (mut harness, _tmp) = harness_with_short_partner();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    paint_owed_frames(&mut harness);

    harness.assert_screen_contains("FINISH");
    harness.assert_screen_contains("short 0001");
}

/// A compose-view partner wraps its own lines: it has no horizontal scroll,
/// so a pan in the source pane must not shift it. Shifting it blanked the
/// whole pane, since every wrapped row is narrower than the shift.
#[test]
fn horizontal_pan_leaves_a_compose_partner_in_place() {
    let (mut harness, _tmp) = harness_with_compose_partner();
    let ((col, row), _) = start_cells(&harness);

    pan_right(&mut harness, col + 2, row, 10);
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "START"),
        1,
        "the source pane should have panned and the compose pane stayed at \
         column 0. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Shift+wheel over the compose pane pans that pane, as it would unpaired,
/// and only that pane: a compose pane's column is not shared, so the source
/// pane stays at column 0.
#[test]
fn wheel_pan_over_a_compose_pane_pans_only_that_pane() {
    let (mut harness, _tmp) = harness_with_compose_partner();
    let ((left_col, row), (right_col, _)) = start_cells(&harness);

    pan_right(&mut harness, right_col + 2, row, 10);

    assert_eq!(
        marker_count(&harness, "START"),
        1,
        "the compose pane should have panned past column 0. Screen:\n{}",
        harness.screen_to_string()
    );
    assert_eq!(
        harness.find_text_on_screen("START"),
        Some((left_col, row)),
        "the source pane should still show column 0. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// With the compose pane focused, the frame's sync runs from it. It has no
/// horizontal scroll to hand out, so a pan of the source pane must stick
/// rather than be reset to the compose pane's column 0.
#[test]
fn source_pan_sticks_while_the_compose_pane_is_focused() {
    let (mut harness, _tmp) = harness_with_compose_partner();
    let ((left_col, row), (right_col, _)) = start_cells(&harness);
    harness.mouse_click(right_col + 2, row + 3).unwrap();
    paint_owed_frames(&mut harness);

    pan_right(&mut harness, left_col + 2, row, 10);

    assert_eq!(
        marker_count(&harness, "START"),
        1,
        "the source pane should stay panned and the compose pane at column \
         0. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Closing one pane of a group drops the group at once, so pairing again
/// under the same id works, even in the same plugin handler as the close.
/// A leftover group made the second `createScrollSyncGroup` a silent no-op
/// (the id was taken) and the new pair never synced.
#[test]
fn pairing_again_after_closing_a_pane_syncs_the_new_pair() {
    let (mut harness, _tmp) = harness_with_pair();
    run_command(&mut harness, "hsync: Repair", "hsync-paired-2");

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "line 0200"),
        2,
        "the new right pane should follow the left one to the end. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// The top cell of the right pane's vertical scrollbar thumb.
fn right_pane_vertical_thumb(h: &EditorTestHarness) -> (u16, u16) {
    let (first_row, last_row) = h.content_area_rows();
    (60..120u16)
        .find_map(|col| {
            let rows: Vec<u16> = (first_row..=last_row)
                .map(|r| r as u16)
                .filter(|&row| h.is_scrollbar_thumb_at(col, row))
                .collect();
            (rows.len() > 1).then(|| (col, rows[0]))
        })
        .unwrap_or_else(|| {
            panic!(
                "no vertical thumb in the right pane:\n{}",
                h.screen_to_string()
            )
        })
}

/// The left cell of the right pane's horizontal scrollbar thumb: on the
/// bottom content row, which holds a run of thumb cells (the vertical bar
/// crosses every row in a single cell).
fn right_pane_horizontal_thumb(h: &EditorTestHarness) -> (u16, u16) {
    let row = h.content_area_rows().1 as u16;
    let thumb: Vec<u16> = (60..120u16)
        .filter(|&col| h.is_scrollbar_thumb_at(col, row))
        .collect();
    assert!(
        thumb.len() > 1,
        "no horizontal thumb in the right pane:\n{}",
        h.screen_to_string()
    );
    (thumb[0], row)
}

/// Dragging the vertical scrollbar thumb of the *unfocused* pane of a group
/// scrolls it and its partner: the press focuses the dragged pane, so the
/// frame's sync runs from it rather than snapping it back to the other.
#[test]
fn dragging_the_vertical_thumb_scrolls_both_panes() {
    let (mut harness, _tmp) = harness_with_pair();
    let (col, row) = right_pane_vertical_thumb(&harness);
    let (_, last_row) = harness.content_area_rows();

    harness.mouse_drag(col, row, col, last_row as u16).unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "START"),
        0,
        "both panes should have scrolled away from line 1. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Dragging the horizontal scrollbar thumb of the unfocused pane of a group
/// pans it and its partner, the same way.
#[test]
fn dragging_the_horizontal_thumb_pans_both_panes() {
    let (mut harness, _tmp) = harness_with_pair();
    let (col, row) = right_pane_horizontal_thumb(&harness);

    harness.mouse_drag(col + 1, row, col + 41, row).unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "START"),
        0,
        "both panes should have panned past column 0. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Dragging a pane's only tab into the other pane closes the pane — another
/// of the ways a pane closes, besides closing the split. The group dies with
/// it there too, so pairing again under the same id works.
#[test]
fn pairing_again_after_a_tab_drag_closes_a_pane_syncs_the_new_pair() {
    let (mut harness, _tmp) = harness_with_short_partner();
    let (left_col, row) = harness.find_text_on_screen("START").unwrap();
    // The right pane holds two tabs (the file it was split from, and
    // short.txt). Drag both into the left pane; the second drag empties it.
    for name in ["short.txt", "long.txt"] {
        let tab_row = harness.screen_row_text(1);
        let byte = tab_row.rfind(name).expect("the right pane's tab");
        let tab_col = tab_row[..byte].chars().count() as u16;
        harness
            .mouse_drag(tab_col + 2, 1, left_col + 10, row + 8)
            .unwrap();
        // The drop is handled with the events; draw it so the next tab is
        // found where it now is.
        harness.render().unwrap();
    }
    // A split's panes each carry a close-split button; one pane, none.
    harness
        .wait_until(|h| !h.screen_row_text(1).contains('□'))
        .unwrap();

    // Pair the remaining pane (showing long.txt, the last tab dragged) again
    // under group 42, jump to the end, and wait for the new right pane to
    // follow. Waiting for that, rather than counting frames, keeps the test
    // apart from how many frames the jump and the sync take: after these tab
    // drags the event loop owes a frame on every tick even with no group at
    // all (a separate, pre-existing bug), so `paint_owed_frames` cannot be
    // used here. Without the fix the right pane never follows, and the wait
    // fails.
    run_command(&mut harness, "hsync: Pair", "hsync-paired-1");
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| marker_count(h, "line 0200") == 2)
        .unwrap();
}

/// A group whose pane closed is gone at the next frame, not only when a new
/// group is created. A leftover group still counted its surviving pane as
/// synced, which keeps that pane out of same-buffer scroll sync: a new,
/// ungrouped split of the same file then scrolled alone.
#[test]
fn a_pane_whose_partner_closed_rejoins_same_buffer_scroll_sync() {
    let (mut harness, _tmp) = harness_with_pair();
    run_command(&mut harness, "hsync: Close Right", "hsync-closed");
    paint_owed_frames(&mut harness);
    run_command(&mut harness, "Toggle Scroll Sync", "Scroll sync enabled");
    // A new split of the same file, in no group; it takes focus.
    run_command(&mut harness, "split vert", "");
    paint_owed_frames(&mut harness);
    assert_eq!(
        marker_count(&harness, "START"),
        2,
        "setup: two panes at the top of long.txt. Screen:\n{}",
        harness.screen_to_string()
    );

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    paint_owed_frames(&mut harness);

    assert_eq!(
        marker_count(&harness, "line 0200"),
        2,
        "the original pane should follow the new split to the end. Screen:\n{}",
        harness.screen_to_string()
    );
}
