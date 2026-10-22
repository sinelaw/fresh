//! E2E tests for the compare_files plugin: two arbitrary files side by side,
//! both editable, differences aligned and highlighted — what `fresh --diff A
//! B` opens.
//!
//! Each test starts the comparison the way `fresh --diff` does: the two files
//! are opened as CLI files, `Editor::set_diff_launch` records the pair, and
//! the `ready` hook hands it to the plugin. Assertions read the screen only
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
fn compare(left: &str, right: &str) -> Compare {
    compare_sized(left, right, 120, 30)
}

fn compare_sized(left: &str, right: &str, width: u16, height: u16) -> Compare {
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
    // Production order (real_main): the CLI files are open, the pair is
    // recorded, then `ready` fires.
    h.open_file(&left_path).unwrap();
    h.open_file(&right_path).unwrap();
    let canonical = |p: &PathBuf| fs::canonicalize(p).unwrap();
    h.editor_mut()
        .set_diff_launch(canonical(&left_path), canonical(&right_path));
    h.editor_mut().fire_ready_hook();
    h.wait_until(|h| h.screen_to_string().contains("left.txt ↔ right.txt"))
        .unwrap();
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

/// The two files open side by side in one tab, the lines they share on the
/// same row: filler rows stand in for the lines only one side has.
#[test]
fn diff_launch_shows_the_files_side_by_side_with_matching_lines_level() {
    let Compare { mut h, _tmp, .. } = compare(LEFT, RIGHT);

    h.wait_until(|h| {
        let echo = row_of(h, "echo");
        let changed = row_of(h, "CHARLIE changed");
        changed.is_some()
            && changed == row_of(h, "charlie")
            // `echo` is only on the left: the right holds a filler row
            // there, so `foxtrot` stays level below it.
            && echo.is_some()
            && level(h, "foxtrot")
            && level(h, "alpha")
            && level(h, "delta")
            && h.screen_to_string().contains("Compare: 3 difference(s)")
    })
    .unwrap();

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

/// Both sides are the files themselves: typing edits the file, and the diff
/// follows the edit.
#[test]
fn typing_in_a_side_edits_the_file_and_rediffs() {
    let same = "one\ntwo\nthree\n";
    let Compare { mut h, _tmp, .. } = compare(same, same);
    h.wait_until(|h| {
        h.screen_to_string()
            .contains("Compare: the files are identical")
    })
    .unwrap();

    // The left panel has the focus, its caret at the start of the file.
    h.type_text("Z").unwrap();
    h.wait_until(|h| {
        let Some(row) = row_of(h, "Zone") else {
            return false;
        };
        let text = h.screen_row_text(row);
        // Both halves of the row are marked changed.
        text.matches('~').count() == 2
    })
    .unwrap();
    // The file's own tab shows it modified.
    h.wait_until(|h| h.screen_to_string().contains("left.txt*"))
        .unwrap();
}

/// Next Difference walks the changes in order and wraps at the end.
#[test]
fn next_difference_steps_through_the_changes_and_wraps() {
    let Compare { mut h, _tmp, .. } = compare(LEFT, RIGHT);
    // Opening lands on the first difference.
    h.wait_until(|h| h.screen_to_string().contains("Compare: 3 difference(s)"))
        .unwrap();

    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.get_status_bar().contains("Difference 2/3"))
        .unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.get_status_bar().contains("Difference 3/3"))
        .unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.get_status_bar().contains("Difference 1/3 [wrapped]"))
        .unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    h.wait_until(|h| h.get_status_bar().contains("Difference 3/3 [wrapped]"))
        .unwrap();
}

/// Take Change copies the other side's lines of the change under the cursor
/// into this side; Switch Side and Copy Change do the same from the other
/// panel. Either way the change is gone from the diff.
#[test]
fn copying_a_change_across_makes_both_sides_match_there() {
    let Compare {
        mut h,
        left,
        right,
        _tmp,
    } = compare(LEFT, RIGHT);
    h.wait_until(|h| h.screen_to_string().contains("Compare: 3 difference(s)"))
        .unwrap();

    // The caret is on the first difference (charlie). Take the right's line.
    h.run_palette_command("Compare: Take Change from Other Side")
        .unwrap();
    // Level, and no longer marked changed: the diff has caught up.
    h.wait_until(|h| {
        level(h, "CHARLIE changed")
            && !h
                .screen_row_text(row_of(h, "CHARLIE changed").unwrap())
                .contains('~')
    })
    .unwrap();

    // From the right panel, send its extra lines over to the left.
    h.send_key(KeyCode::F(6), KeyModifiers::CONTROL).unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.send_key(KeyCode::F(5), KeyModifiers::ALT).unwrap();
    h.wait_until(|h| h.get_status_bar().contains("Difference 2/2"))
        .unwrap();
    h.run_palette_command("Compare: Copy Change to Other Side")
        .unwrap();
    h.wait_until(|h| level(h, "extra one") && level(h, "extra two"))
        .unwrap();

    // Nothing is saved until the user saves.
    assert_eq!(fs::read_to_string(&left).unwrap(), LEFT);
    assert_eq!(fs::read_to_string(&right).unwrap(), RIGHT);
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
        &(left.join("\n") + "\n"),
        &(right.join("\n") + "\n"),
        120,
        24,
    );
    h.wait_until(|h| h.screen_to_string().contains("Compare: 3 difference(s)"))
        .unwrap();

    for _ in 0..140 {
        h.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    h.wait_until(|h| level(h, "row 150") && level(h, "row 151"))
        .unwrap();

    h.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
    h.wait_until(|h| level(h, "row 200")).unwrap();
}

/// Closing the comparison closes its tab and takes its highlights off the
/// files, which stay open.
#[test]
fn closing_the_comparison_leaves_the_files_open_and_undecorated() {
    let Compare { mut h, _tmp, .. } = compare(LEFT, RIGHT);
    h.wait_until(|h| h.screen_to_string().contains("Compare: 3 difference(s)"))
        .unwrap();

    h.run_palette_command("Compare: Close").unwrap();
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        !screen.contains("left.txt ↔ right.txt")
            && screen.contains("left.txt")
            && screen.contains("right.txt")
    })
    .unwrap();
    // The file in view shows no diff glyphs any more.
    h.wait_until(|h| {
        let screen = h.screen_to_string();
        !screen
            .lines()
            .any(|l| l.contains("extra one") && l.contains('+'))
            && !screen
                .lines()
                .any(|l| l.contains("CHARLIE") && l.contains('~'))
    })
    .unwrap();
}
