//! E2E tests for the compare_files plugin: two arbitrary files side by side,
//! both editable, differences aligned and highlighted — what `fresh --diff A
//! B` opens.
//!
//! Each test starts the comparison the way `fresh --diff` does:
//! `Editor::set_diff_launch` records the pair and the `ready` hook hands it
//! to the plugin, which opens both files into the panels of one tab.
//! Assertions read the screen (and, for saves, the files on disk) only
//! (CONTRIBUTING.md Testing §2), and each step waits for the screen it
//! expects (§3).

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::PathBuf;

const LEFT: &str = "alpha\nbravo\ncharlie\ndelta\necho\nfoxtrot\n";
const RIGHT: &str = "alpha\nbravo\nCHARLIE changed\ndelta\nfoxtrot\nextra one\nextra two\n";

struct Compare {
    h: EditorTestHarness,
    left: PathBuf,
    right: PathBuf,
    _tmp: tempfile::TempDir,
}

/// Start `fresh --diff left.txt right.txt` over `left` and `right`.
fn compare(left: &[u8], right: &[u8]) -> Compare {
    compare_sized(left, right, 120, 30)
}

fn compare_sized(left: &[u8], right: &[u8], width: u16, height: u16) -> Compare {
    let c = launch(left, right, width, height);
    let mut c = c;
    c.h.wait_until(|h| h.screen_to_string().contains("left.txt ↔ right.txt"))
        .unwrap();
    c
}

/// The launch alone, for a test that expects the comparison not to open.
fn launch(left: &[u8], right: &[u8], width: u16, height: u16) -> Compare {
    let tmp = tempfile::TempDir::new().unwrap();
    let root = tmp.path().join("work");
    let plugins_dir = root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "compare_files");
    copy_plugin_lib(&plugins_dir);

    let left_path = root.join("left.txt");
    let right_path = root.join("right.txt");
    fs::write(&left_path, left).unwrap();
    fs::write(&right_path, right).unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(width, height, Config::default(), root)
            .unwrap();
    // Production order (real_main): the pair is recorded, then `ready`
    // fires. The editor opens neither file itself.
    let canonical = |p: &PathBuf| fs::canonicalize(p).unwrap();
    h.editor_mut()
        .set_diff_launch(canonical(&left_path), canonical(&right_path));
    h.editor_mut().fire_ready_hook();
    Compare {
        h,
        left: left_path,
        right: right_path,
        _tmp: tmp,
    }
}

/// The screen row `text` is on, if it is on screen.
fn row_of(h: &EditorTestHarness, text: &str) -> Option<u16> {
    h.find_text_on_screen(text).map(|(_, row)| row)
}

/// The rows of every occurrence of `text`, top to bottom.
fn rows_of(h: &EditorTestHarness, text: &str) -> Vec<u16> {
    let screen = h.screen_to_string();
    screen
        .lines()
        .enumerate()
        .flat_map(|(row, line)| std::iter::repeat_n(row as u16, line.matches(text).count()))
        .collect()
}

/// `text` shows on the same screen row in both panels.
fn level(h: &EditorTestHarness, text: &str) -> bool {
    let rows = rows_of(h, text);
    rows.len() == 2 && rows[0] == rows[1]
}

/// The tab bar (row 1, under the menu bar).
fn tabs(h: &EditorTestHarness) -> String {
    h.screen_row_text(1)
}

fn status_has(h: &EditorTestHarness, text: &str) -> bool {
    h.get_status_bar().contains(text)
}

/// The two files open side by side in one tab, and only that tab: neither
/// file gets one of its own. The lines they share sit on the same row:
/// filler rows stand in for the lines only one side has.
#[test]
fn diff_launch_opens_one_tab_with_matching_lines_level() {
    let Compare { mut h, _tmp, .. } = compare(LEFT.as_bytes(), RIGHT.as_bytes());

    h.wait_until(|h| {
        let changed = row_of(h, "CHARLIE changed");
        changed.is_some()
            && changed == row_of(h, "charlie")
            // `echo` is only on the left: the right holds a filler row
            // there, so `foxtrot` stays level below it.
            && row_of(h, "echo").is_some()
            && level(h, "foxtrot")
            && level(h, "alpha")
            && level(h, "delta")
            && status_has(h, "Compare: 3 difference(s)")
    })
    .unwrap();
    let tab_bar = tabs(&h);
    assert_eq!(tab_bar.matches('×').count(), 1, "one tab: {tab_bar:?}");

    // Changed lines carry a gutter glyph: `~` for a line changed on both
    // sides, `-` for one only the left has, `+` for one only the right has.
    let echo_row = h.screen_row_text(row_of(&h, "echo").unwrap());
    assert!(echo_row.contains('-'), "echo row: {echo_row:?}");
    let extra_row = h.screen_row_text(row_of(&h, "extra one").unwrap());
    assert!(extra_row.contains('+'), "extra row: {extra_row:?}");
    let charlie_row = h.screen_row_text(row_of(&h, "charlie").unwrap());
    assert_eq!(
        charlie_row.matches('~').count(),
        2,
        "charlie row: {charlie_row:?}"
    );
}

/// Both sides are the files themselves: typing edits the file, the diff
/// follows the edit, and saving writes that file.
#[test]
fn typing_in_a_side_edits_the_file_and_rediffs() {
    let same = b"one\ntwo\nthree\n";
    let Compare {
        mut h, left, _tmp, ..
    } = compare(same, same);
    h.wait_until(|h| status_has(h, "Compare: the files are identical"))
        .unwrap();

    // The left panel has the focus, its caret at the start of the file.
    h.type_text("Z").unwrap();
    h.wait_until(|h| {
        let Some(row) = row_of(h, "Zone") else {
            return false;
        };
        // Both halves of the row are marked changed.
        h.screen_row_text(row).matches('~').count() == 2
    })
    .unwrap();

    h.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|_| fs::read_to_string(&left).unwrap() == "Zone\ntwo\nthree\n")
        .unwrap();
}

/// Next Difference walks the changes in order and wraps at the end.
#[test]
fn next_difference_steps_through_the_changes_and_wraps() {
    let Compare { mut h, _tmp, .. } = compare(LEFT.as_bytes(), RIGHT.as_bytes());
    // Opening lands on the first difference.
    h.wait_until(|h| status_has(h, "Compare: 3 difference(s)"))
        .unwrap();

    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| status_has(h, "Difference 2/3")).unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| status_has(h, "Difference 3/3")).unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| status_has(h, "Difference 1/3 [wrapped]"))
        .unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    h.wait_until(|h| status_has(h, "Difference 3/3 [wrapped]"))
        .unwrap();
}

/// Presses sent faster than the editor answers them each move one
/// difference: a step reads the cursor only once the step before has put
/// it in place.
#[test]
fn quick_presses_each_move_one_difference() {
    let left: Vec<String> = (0..100).map(|i| format!("l{i}")).collect();
    let mut right = left.clone();
    for k in (5..100).step_by(10) {
        right[k] = "X".to_string();
    }
    let Compare { mut h, _tmp, .. } = compare(
        (left.join("\n") + "\n").as_bytes(),
        (right.join("\n") + "\n").as_bytes(),
    );
    h.wait_until(|h| status_has(h, "Compare: 10 difference(s)"))
        .unwrap();

    for _ in 0..5 {
        h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    }
    h.wait_until(|h| status_has(h, "Difference 6/10")).unwrap();
}

/// Take Change copies the other side's lines of the change under the cursor
/// into this side, as one undo step; Switch Side and Copy Change do the same
/// from the other panel. Either way the change is gone from the diff, and
/// nothing is saved until the user saves.
#[test]
fn copying_a_change_across_makes_both_sides_match_there() {
    let Compare {
        mut h,
        left,
        right,
        _tmp,
    } = compare(LEFT.as_bytes(), RIGHT.as_bytes());
    h.wait_until(|h| status_has(h, "Compare: 3 difference(s)"))
        .unwrap();

    let charlie_unmarked = |h: &EditorTestHarness| {
        level(h, "CHARLIE changed")
            && !h
                .screen_row_text(row_of(h, "CHARLIE changed").unwrap())
                .contains('~')
    };

    // The caret is on the first difference (charlie). Take the right's line.
    h.run_palette_command("Compare: Take Change from Other Side")
        .unwrap();
    // Level, and no longer marked changed: the diff has caught up.
    h.wait_until(charlie_unmarked).unwrap();

    // One undo puts the left's line back whole.
    h.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_until(|h| {
        row_of(h, "charlie").is_some()
            && rows_of(h, "CHARLIE changed").len() == 1
            && h.screen_row_text(row_of(h, "charlie").unwrap())
                .matches('~')
                .count()
                == 2
    })
    .unwrap();

    // From the right panel, send its extra lines over to the left. Its
    // caret is on the first difference, where opening put both.
    h.send_key(KeyCode::F(6), KeyModifiers::CONTROL).unwrap();
    for _ in 0..2 {
        h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    }
    h.wait_until(|h| status_has(h, "Difference 3/3")).unwrap();
    h.run_palette_command("Compare: Copy Change to Other Side")
        .unwrap();
    h.wait_until(|h| level(h, "extra one") && level(h, "extra two"))
        .unwrap();

    assert_eq!(fs::read_to_string(&left).unwrap(), LEFT);
    assert_eq!(fs::read_to_string(&right).unwrap(), RIGHT);
}

/// Two lines that differ only in how they end say so; taking the change
/// makes the bytes match, newline included, so the files compare equal.
#[test]
fn a_missing_final_newline_is_named_and_can_be_taken() {
    let Compare { mut h, _tmp, .. } = compare(b"a\nb", b"a\nb\n");
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("no newline at end of file") && screen.contains("LF line ending")
    })
    .unwrap();

    h.run_palette_command("Compare: Take Change from Other Side")
        .unwrap();
    h.wait_until(|h| status_has(h, "Compare: the files are identical"))
        .unwrap();
}

/// The panels scroll together, and the matching lines stay on the same row
/// far from the top — past changes that added and removed lines.
#[test]
fn the_panels_scroll_together_with_matching_lines_level() {
    let left: Vec<String> = (1..=200).map(|i| format!("row {i:03}")).collect();
    let mut right = left.clone();
    right.drain(20..30);
    right[60] = "row CHANGED".to_string();
    for k in 0..15 {
        right.insert(100 + k, format!("inserted {k}"));
    }
    let Compare { mut h, _tmp, .. } = compare_sized(
        (left.join("\n") + "\n").as_bytes(),
        (right.join("\n") + "\n").as_bytes(),
        120,
        24,
    );
    h.wait_until(|h| status_has(h, "Compare: 3 difference(s)"))
        .unwrap();

    for _ in 0..140 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.wait_until(|h| level(h, "row 150") && level(h, "row 151"))
        .unwrap();

    h.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| level(h, "row 200")).unwrap();
}

/// Scrolled so the first line after lines only one side has is at the top,
/// both panels start with it: the filler rows belong to the line before.
#[test]
fn the_line_after_a_one_sided_change_is_level_at_the_top() {
    let left: Vec<String> = (1..=80).map(|i| format!("row {i:03}")).collect();
    let mut right = left.clone();
    right.drain(19..29);
    let Compare { mut h, _tmp, .. } = compare_sized(
        (left.join("\n") + "\n").as_bytes(),
        (right.join("\n") + "\n").as_bytes(),
        120,
        24,
    );
    h.wait_until(|h| status_has(h, "Compare: 1 difference(s)"))
        .unwrap();

    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    // Scroll the view (not the caret) down 29 lines: row 030 on top.
    for _ in 0..29 {
        h.send_key(KeyCode::Down, KeyModifiers::CONTROL).unwrap();
    }
    h.wait_until(|h| row_of(h, "row 030") == Some(2) && level(h, "row 030"))
        .unwrap();
}

/// `rows` lines `row 001`… on the left; the right has `inserted` extra lines
/// after `row 050`. The left's filler rows for them hang below its `row 050`.
fn with_inserted_block(rows: usize, inserted: usize) -> (String, String) {
    with_inserted_block_after(rows, 50, inserted)
}

fn with_inserted_block_after(rows: usize, after: usize, inserted: usize) -> (String, String) {
    let left: Vec<String> = (1..=rows).map(|i| format!("row {i:03}")).collect();
    let mut right = left.clone();
    for k in 0..inserted {
        right.insert(after + k, format!("inserted {k}"));
    }
    (left.join("\n") + "\n", right.join("\n") + "\n")
}

/// Every `row NNN` line on screen is on the same row in both panels.
fn all_rows_level(h: &EditorTestHarness) -> bool {
    let screen = h.screen_to_string();
    let mut labels: Vec<&str> = screen
        .lines()
        .flat_map(|line| {
            line.match_indices("row ")
                .filter_map(|(i, _)| line.get(i..i + 7))
        })
        .collect();
    labels.sort_unstable();
    labels.dedup();
    labels.iter().all(|label| level(h, label))
}

/// The wheel over the side that has no lines for a change scrolls through
/// the filler rows standing in for them, row by row, with the other side
/// showing the change's lines beside them. It used to step over the whole
/// filler block in one notch, the other side jumping past the change.
#[test]
fn the_wheel_scrolls_through_filler_rows() {
    let (left, right) = with_inserted_block(120, 40);
    let Compare { mut h, _tmp, .. } = compare_sized(left.as_bytes(), right.as_bytes(), 120, 24);
    h.wait_until(|h| status_has(h, "Compare: 1 difference(s)"))
        .unwrap();
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| row_of(h, "row 001") == Some(2)).unwrap();

    let mut saw_middle_of_change = false;
    while !(level(&h, "row 080") && row_of(&h, "row 080").is_some_and(|r| r < 10)) {
        // Over the left panel.
        h.mouse_scroll_down(10, 10).unwrap();
        assert!(
            all_rows_level(&h),
            "panels out of step:\n{}",
            h.screen_to_string()
        );
        saw_middle_of_change |= row_of(&h, "inserted 20").is_some();
    }
    assert!(
        saw_middle_of_change,
        "never showed the middle of the change"
    );
}

/// The wheel over the side that has the change's lines keeps the other side
/// level: its filler rows go by beside them, and the lines after the change
/// arrive on the same row in both. The other side used to stop at the edge
/// of the change, showing the lines after it beside lines that don't match.
#[test]
fn the_wheel_through_a_change_keeps_the_other_side_level() {
    let (left, right) = with_inserted_block(120, 40);
    let Compare { mut h, _tmp, .. } = compare_sized(left.as_bytes(), right.as_bytes(), 120, 24);
    h.wait_until(|h| status_has(h, "Compare: 1 difference(s)"))
        .unwrap();
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| row_of(h, "row 001") == Some(2)).unwrap();

    while !(level(&h, "row 080") && row_of(&h, "row 080").is_some_and(|r| r < 10)) {
        // Over the right panel.
        h.mouse_scroll_down(90, 10).unwrap();
        assert!(
            all_rows_level(&h),
            "panels out of step:\n{}",
            h.screen_to_string()
        );
    }
    while row_of(&h, "row 001").is_none() {
        h.mouse_scroll_up(90, 10).unwrap();
        assert!(
            all_rows_level(&h),
            "panels out of step:\n{}",
            h.screen_to_string()
        );
    }
}

/// Moving the cursor past a block of filler rows scrolls the panels
/// together: the line it lands on is level in both, in the frame the key
/// draws. The pane with the cursor was first placed counting lines, then
/// re-placed in rows after its partner had been synced to the first answer,
/// leaving the two a row apart.
#[test]
fn the_cursor_past_filler_rows_keeps_the_panels_level() {
    let (left, right) = with_inserted_block(120, 40);
    let Compare { mut h, _tmp, .. } = compare_sized(left.as_bytes(), right.as_bytes(), 120, 24);
    h.wait_until(|h| status_has(h, "Compare: 1 difference(s)"))
        .unwrap();
    h.send_key(KeyCode::Home, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| row_of(h, "row 001") == Some(2)).unwrap();

    for _ in 0..60 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        assert!(
            all_rows_level(&h),
            "panels out of step:\n{}",
            h.screen_to_string()
        );
    }
    assert!(
        level(&h, "row 061"),
        "the cursor's line:\n{}",
        h.screen_to_string()
    );
}

/// The same past the size where the editor stops indexing a file's rows:
/// the cursor's line, just past a block of filler rows, is on screen. Placed
/// by counting lines, the pane scrolled it off the bottom again.
#[test]
fn the_cursor_past_filler_rows_stays_on_screen_in_a_long_file() {
    let (left, right) = with_inserted_block_after(6_000, 5_950, 40);
    let Compare { mut h, _tmp, .. } = compare_sized(left.as_bytes(), right.as_bytes(), 120, 24);
    h.wait_until(|h| status_has(h, "Compare: 1 difference(s)"))
        .unwrap();
    h.send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    h.type_text("5948").unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| level(h, "row 5948")).unwrap();

    for _ in 0..3 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.wait_until(|h| status_has(h, "Ln 5951")).unwrap();
    assert!(
        level(&h, "row 5951"),
        "the cursor's line:\n{}",
        h.screen_to_string()
    );
}

/// Closing the comparison closes its tab and the files it opened, as they
/// had no changes: nothing of the comparison is left.
#[test]
fn closing_the_comparison_closes_the_files_it_opened() {
    let Compare { mut h, _tmp, .. } = compare(LEFT.as_bytes(), RIGHT.as_bytes());
    h.wait_until(|h| status_has(h, "Compare: 3 difference(s)"))
        .unwrap();

    h.run_palette_command("Compare: Close").unwrap();
    h.wait_until(|h| {
        let tab_bar = tabs(h);
        !tab_bar.contains("left.txt") && !tab_bar.contains("right.txt")
    })
    .unwrap();
}

/// A file with unsaved changes isn't closed with the comparison: it gets a
/// tab of its own, where it can be saved or discarded.
#[test]
fn closing_the_comparison_keeps_a_file_with_unsaved_changes_in_a_tab() {
    let Compare { mut h, _tmp, .. } = compare(LEFT.as_bytes(), RIGHT.as_bytes());
    h.wait_until(|h| status_has(h, "Compare: 3 difference(s)"))
        .unwrap();

    h.type_text("EDIT").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("EDIT"))
        .unwrap();
    h.send_key(KeyCode::Char('w'), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| {
        let tab_bar = tabs(h);
        tab_bar.contains("left.txt*") && !tab_bar.contains("right.txt")
    })
    .unwrap();
}

/// A binary file is not compared: the comparison doesn't open, and the
/// status says why.
#[test]
fn a_binary_file_is_not_compared() {
    let Compare { mut h, _tmp, .. } = launch(b"\xff\x00\nold\nz\n", b"\xff\x00\nnew\nz\n", 120, 30);
    h.wait_until(|h| status_has(h, "is a binary file")).unwrap();
    assert!(!tabs(&h).contains("↔"), "no comparison tab: {:?}", tabs(&h));
}
