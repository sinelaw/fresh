//! E2E tests for search navigation (find next/previous) after cursor movement.
//!
//! Reproduces <https://github.com/sinelaw/fresh/issues/1305>:
//! After a search, moving the cursor and then invoking Find Next or Find Previous
//! navigates relative to the last match index instead of the current cursor position.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::io::Write;

// ---------------------------------------------------------------------------
// Small-file tests (overlay-based search path)
// ---------------------------------------------------------------------------

/// After searching and then moving the cursor past several matches,
/// Find Next should jump to the match nearest *after* the cursor,
/// not to the one after the last-visited match.
#[test]
fn test_find_next_respects_cursor_position_after_move() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // 10 lines, "NEEDLE" on lines 2, 5, and 8 (0-indexed)
    let content = "\
line 0 filler text\n\
line 1 filler text\n\
line 2 NEEDLE here\n\
line 3 filler text\n\
line 4 filler text\n\
line 5 NEEDLE here\n\
line 6 filler text\n\
line 7 filler text\n\
line 8 NEEDLE here\n\
line 9 filler text";
    std::fs::write(&file_path, content).unwrap();

    let match1_pos = content.find("NEEDLE").unwrap(); // line 2
    let match2_pos = content[match1_pos + 1..].find("NEEDLE").unwrap() + match1_pos + 1; // line 5
    let match3_pos = content[match2_pos + 1..].find("NEEDLE").unwrap() + match2_pos + 1; // line 8

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for NEEDLE
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("NEEDLE").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();

    // Should land on first match (line 2)
    assert_eq!(
        harness.cursor_position(),
        match1_pos,
        "Initial search should land on first match"
    );

    // Move cursor to line 7 (between match 2 on line 5 and match 3 on line 8)
    // by pressing Down 5 times (from line 2 -> line 7)
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 5)
        .unwrap();
    harness.render().unwrap();

    // Now Find Next — should go to match 3 (line 8), the next match after cursor
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        match3_pos,
        "Find Next after moving cursor to line 7 should jump to match on line 8, \
         not to the match after the previously visited match index"
    );
}

/// After searching and then moving the cursor before several matches,
/// Find Previous should jump to the match nearest *before* the cursor,
/// not to the one before the last-visited match.
#[test]
fn test_find_previous_respects_cursor_position_after_move() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // "NEEDLE" on lines 2, 5, and 8
    let content = "\
line 0 filler text\n\
line 1 filler text\n\
line 2 NEEDLE here\n\
line 3 filler text\n\
line 4 filler text\n\
line 5 NEEDLE here\n\
line 6 filler text\n\
line 7 filler text\n\
line 8 NEEDLE here\n\
line 9 filler text";
    std::fs::write(&file_path, content).unwrap();

    let match1_pos = content.find("NEEDLE").unwrap();
    let match2_pos = content[match1_pos + 1..].find("NEEDLE").unwrap() + match1_pos + 1;

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for NEEDLE — lands on match 1
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("NEEDLE").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(harness.cursor_position(), match1_pos);

    // Move cursor to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Find Previous — should go to match 3 (line 8), the nearest match before EOF
    // The bug would send us to match 3 only by coincidence (wrapping from match 1 - 1 = last).
    // Instead let's go to a specific position: line 4 (between match 1 and match 2).
    // Reset: go to beginning
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Re-search to reset match state — lands on match 1
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(harness.cursor_position(), match1_pos);

    // Move cursor down to line 6 (between match 2 on line 5 and match 3 on line 8)
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 4)
        .unwrap();
    harness.render().unwrap();

    // Find Previous — should go to match 2 (line 5), the nearest match before cursor
    harness
        .send_key(
            KeyCode::Char('n'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.process_async_and_render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        match2_pos,
        "Find Previous after moving cursor to line 6 should jump to match on line 5, \
         not to the match before the previously visited match index"
    );
}

/// After searching, F3 to an intermediate match, then Ctrl+End, then
/// Find Previous should go to the last match — not to the match before
/// the intermediate one we had navigated to.
#[test]
fn test_find_next_and_previous_after_ctrl_end() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Build content: 30 lines, NEEDLE on lines 5, 15, 25
    let mut content = String::new();
    for i in 0..30 {
        if i == 5 || i == 15 || i == 25 {
            content.push_str(&format!("line {} NEEDLE here\n", i));
        } else {
            content.push_str(&format!("line {} filler padding text\n", i));
        }
    }
    std::fs::write(&file_path, &content).unwrap();

    let match1_pos = content.find("NEEDLE").unwrap();
    let match2_pos = content[match1_pos + 1..].find("NEEDLE").unwrap() + match1_pos + 1;
    let match3_pos = content[match2_pos + 1..].find("NEEDLE").unwrap() + match2_pos + 1;

    let mut harness = EditorTestHarness::new(100, 40).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Search for NEEDLE — lands on match 1 (line 5)
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("NEEDLE").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(harness.cursor_position(), match1_pos);

    // F3 to match 2 (line 15) — now current_match_index = 1
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.process_async_and_render().unwrap();
    assert_eq!(harness.cursor_position(), match2_pos);

    // Go to end of file with Ctrl+End
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Find Previous from EOF — should go to match 3 (line 25), the last match.
    // BUG: because current_match_index is 1 (match 2), the buggy code does
    // index 1-1=0 and goes to match 1 instead of match 3.
    harness
        .send_key(
            KeyCode::Char('n'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.process_async_and_render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        match3_pos,
        "Find Previous from EOF should go to match 3 (line 25), \
         not to match 1 (line 5) via stale current_match_index"
    );
}

// ---------------------------------------------------------------------------
// Large-file tests (search_state.matches path, >1MB threshold)
// ---------------------------------------------------------------------------

/// Same as the small-file find-next test, but with a large file to exercise
/// the `search_state.matches` code path instead of overlays.
#[test]
fn test_find_next_respects_cursor_position_large_file() {
    let mut config = Config::default();
    config.editor.large_file_threshold_bytes = 1024 * 1024; // 1MB

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("large_nav.txt");

    let mut file = std::fs::File::create(&file_path).unwrap();

    // ~1.2MB of filler, then NEEDLE #1
    for i in 0..25_000 {
        writeln!(
            file,
            "Line {:06}: filler padding to bulk up the file size.",
            i
        )
        .unwrap();
    }
    writeln!(file, "first NEEDLE here").unwrap(); // match 1

    // ~1.2MB of filler, then NEEDLE #2
    for i in 0..25_000 {
        writeln!(
            file,
            "Line {:06}: more filler content in the middle area.",
            i
        )
        .unwrap();
    }
    writeln!(file, "second NEEDLE here").unwrap(); // match 2

    // ~1.2MB of filler, then NEEDLE #3
    for i in 0..25_000 {
        writeln!(file, "Line {:06}: yet more filler near the end of file.", i).unwrap();
    }
    writeln!(file, "third NEEDLE here").unwrap(); // match 3

    // trailing lines
    for i in 0..200 {
        writeln!(file, "Trailing {:06}", i).unwrap();
    }
    file.flush().unwrap();
    drop(file);

    let file_size = std::fs::metadata(&file_path).unwrap().len();
    assert!(
        file_size > 3 * 1024 * 1024,
        "File should be >3MB, got {} bytes",
        file_size
    );

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify large-file mode
    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "Buffer should be in large-file mode"
    );

    // Search for NEEDLE — drive incremental scan to completion
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("NEEDLE").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    while harness.editor_mut().process_search_scan() {}
    harness.process_async_and_render().unwrap();

    harness.assert_screen_contains("Found 3 matches");

    // Cursor should be on match 1
    let match1_pos = harness.cursor_position();

    // F3 to match 2
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.tick_and_render().unwrap();
    harness.assert_screen_contains("Match 2 of 3");
    let match2_pos = harness.cursor_position();
    assert!(match2_pos > match1_pos);

    // F3 to match 3
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.tick_and_render().unwrap();
    harness.assert_screen_contains("Match 3 of 3");
    let match3_pos = harness.cursor_position();
    assert!(match3_pos > match2_pos);

    // Now go back to the beginning with Ctrl+Home
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.tick_and_render().unwrap();

    // Find Next — should go to match 1 (nearest after cursor at BOF),
    // NOT to match 1 because of wrapping from match 3+1.
    // Either way the position is match 1, but the status should say "Match 1 of 3".
    harness.send_key(KeyCode::F(3), KeyModifiers::NONE).unwrap();
    harness.tick_and_render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        match1_pos,
        "Find Next from BOF should go to match 1"
    );
    // The key assertion: it should be reported as match 1, not match 1 via wrap-around
    harness.assert_screen_contains("Match 1 of 3");

    // Now go to end of file
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.tick_and_render().unwrap();

    // Find Previous — should go to match 3 (nearest before EOF)
    harness
        .send_key(
            KeyCode::Char('n'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.tick_and_render().unwrap();

    assert_eq!(
        harness.cursor_position(),
        match3_pos,
        "Find Previous from EOF should go to match 3, \
         not to match before the last-visited match index"
    );
    harness.assert_screen_contains("Match 3 of 3");
}

/// Find Previous from the end of a large file should go to the last match,
/// reproducing the exact scenario from issue #1305.
#[test]
fn test_find_previous_from_end_of_large_file_issue_1305() {
    let mut config = Config::default();
    config.editor.large_file_threshold_bytes = 1024 * 1024;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 24, config).unwrap();
    let project_dir = harness.project_dir().unwrap();
    let file_path = project_dir.join("issue1305.txt");

    let mut file = std::fs::File::create(&file_path).unwrap();

    // Build a >3MB file with NEEDLE near the end (as described in issue 1305:
    // "phrase is close to end of file")
    for i in 0..65_000 {
        writeln!(
            file,
            "Line {:06}: padding content to bulk up the file size.",
            i
        )
        .unwrap();
    }
    // Place multiple NEEDLEs close to the end
    writeln!(file, "NEEDLE occurrence 1 near end").unwrap();
    for i in 0..1_000 {
        // Filler, and deliberately not a near-miss for the query: searches
        // fold case by default, so a lowercase "needles" here would be a
        // thousand extra matches and this test would be about case
        // sensitivity rather than about find-previous.
        writeln!(file, "Line {:06}: gap between markers.", i).unwrap();
    }
    writeln!(file, "NEEDLE occurrence 2 near end").unwrap();
    for i in 0..500 {
        writeln!(file, "Trailing {:06}", i).unwrap();
    }
    file.flush().unwrap();
    drop(file);

    let file_size = std::fs::metadata(&file_path).unwrap().len();
    assert!(file_size > 3 * 1024 * 1024, "File should be >3MB");

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    assert!(harness.editor().active_state().buffer.is_large_file());

    // Search for NEEDLE
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("NEEDLE").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    while harness.editor_mut().process_search_scan() {}
    harness.process_async_and_render().unwrap();

    harness.assert_screen_contains("Found 2 matches");
    let match1_pos = harness.cursor_position();

    // Go to end of file — reproducing the user's step: "put cursor at end of file"
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.tick_and_render().unwrap();

    let eof_pos = harness.cursor_position();
    assert!(
        eof_pos > match1_pos,
        "EOF position should be past all matches"
    );

    // "select find previous from edit menu" — Find Previous from EOF
    harness
        .send_key(
            KeyCode::Char('n'),
            KeyModifiers::CONTROL | KeyModifiers::SHIFT,
        )
        .unwrap();
    harness.tick_and_render().unwrap();

    // Should go to match 2 (the last NEEDLE, nearest before EOF)
    harness.assert_screen_contains("Match 2 of 2");

    let found_pos = harness.cursor_position();
    assert!(
        found_pos > match1_pos,
        "Find Previous from EOF should go to match 2 (pos {}), not match 1 (pos {})",
        found_pos,
        match1_pos
    );
}
