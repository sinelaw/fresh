//! E2E tests for the live-diff plugin.
//!
//! The plugin renders a unified-diff view directly inside the live editable
//! buffer:
//!   - `+` / `~` / `-` glyphs in the gutter for added/modified/removed lines
//!   - virtual lines containing the OLD text rendered above edited lines
//!
//! These tests assert only on rendered output (`screen_to_string`), per the
//! "E2E tests observe, not inspect" rule in CONTRIBUTING.md.

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

// =============================================================================
// Test helpers
// =============================================================================

/// Skip the menu bar (row 0), tab bar (row 1), and the bottom 2 rows
/// (status + prompt) — same convention as `gutter.rs`.
fn content_lines(screen: &str) -> Vec<&str> {
    let lines: Vec<&str> = screen.lines().collect();
    let start = 2;
    let end = lines.len().saturating_sub(2);
    if end > start {
        lines[start..end].to_vec()
    } else {
        Vec::new()
    }
}

/// Returns true if any content row's first non-line-number column is `glyph`.
/// The gutter column position depends on whether line numbers are visible;
/// to keep this robust we scan every cell from the start of each row.
fn has_glyph(screen: &str, glyph: char) -> bool {
    for line in content_lines(screen) {
        if line.chars().any(|c| c == glyph) {
            return true;
        }
    }
    false
}

/// Returns true if any content row contains the substring `text`.
fn has_text(screen: &str, text: &str) -> bool {
    content_lines(screen).iter().any(|l| l.contains(text))
}

fn open_file(harness: &mut EditorTestHarness, repo_path: &std::path::Path, relative: &str) {
    let full = repo_path.join(relative);
    harness.open_file(&full).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(relative))
        .unwrap();
}

/// Live-diff is opt-in (off by default). Trigger the global-toggle
/// command via the command palette so the rest of the test can observe
/// gutter glyphs and virtual lines.
fn enable_live_diff_globally(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Live Diff: Toggle (Global)").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

// =============================================================================
// Tests
// =============================================================================

/// vs HEAD: an added line shows `+` in the gutter once the file is opened.
/// Live-diff fetches `git show HEAD:<path>` and diffs against the on-disk
/// content (which has one new line vs HEAD), so the new line should be
/// flagged with `+`.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_added_line_shows_plus_in_gutter() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_live_diff_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Insert one new line at the top vs HEAD.
    repo.modify_file(
        "src/utils.rs",
        r#"// brand new top line added by the agent
pub fn format_output(msg: &str) -> String {
    format!("[INFO] {}", msg)
}

pub fn validate_config(config: &Config) -> bool {
    config.port > 0 && !config.host.is_empty()
}
"#,
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '+'))
        .unwrap();
}

/// vs HEAD: a modified line shows `~` in the gutter AND a virtual line
/// rendered above it containing the OLD text.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_modified_line_shows_old_content_above() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_live_diff_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Replace one line; the original text is unique enough to assert on.
    repo.modify_file(
        "src/utils.rs",
        r#"pub fn format_output(msg: &str) -> String {
    format!("LIVE_DIFF_REPLACED_LINE {}", msg)
}

pub fn validate_config(config: &Config) -> bool {
    config.port > 0 && !config.host.is_empty()
}
"#,
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    // Wait for the modified glyph to appear.
    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '~'))
        .unwrap();

    // The virtual line carries the OLD content (no leading "- " prefix —
    // the red bg/fg is the visual signal).
    // Original line was: `    format!("[INFO] {}", msg)`
    harness
        .wait_until(|h| has_text(&h.screen_to_string(), "[INFO]"))
        .unwrap();

    // And the new content is also still present (it lives in the real buffer).
    let screen = harness.screen_to_string();
    assert!(
        has_text(&screen, "LIVE_DIFF_REPLACED_LINE"),
        "expected new content visible:\n{screen}"
    );
}

/// Live-update: while a buffer is open and the on-disk file changes, the
/// plugin's diff updates the next time the editor reloads the buffer.
///
/// We can't trigger Fresh's external-file-watch reload from the harness,
/// but we can assert the closely-related path: typing into the buffer
/// fires `after_insert`, and `recompute` rebuilds the diff against the
/// (still-HEAD) reference. So a fresh edit should produce a fresh `~`
/// glyph on the line we just changed.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_updates_on_buffer_edit() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_live_diff_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    // Open a clean (HEAD-equal) file. No glyphs initially.
    open_file(&mut harness, &repo.path, "src/utils.rs");
    harness
        .wait_until(|h| h.screen_to_string().contains("format_output"))
        .unwrap();

    // Type a brand-new line into the buffer; this fires after_insert and
    // forces a recompute against HEAD.
    harness.type_text("// LIVE_DIFF_TYPED_INSERT\n").unwrap();
    harness.render().unwrap();

    // Wait for the new-line glyph to appear and for the typed text to be
    // visible on screen.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            has_glyph(&s, '+') && has_text(&s, "LIVE_DIFF_TYPED_INSERT")
        })
        .unwrap();
}

/// Regression: a buffer with multi-byte UTF-8 (emoji that needs a JS
/// surrogate pair) used to crash the plugin with
/// "TypeError: Conversion from string failed: invalid utf-8 sequence
/// of 1 bytes from index 0", because the line-byte-start calculator
/// indexed the buffer text by UTF-16 code unit and handed half-
/// surrogates to `editor.utf8ByteLength`.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_handles_surrogate_pair_content() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_live_diff_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // 🎉 (U+1F389) is a 4-byte UTF-8 char that needs a surrogate pair
    // in JS strings. Modify the line so the diff has actual content.
    repo.modify_file(
        "src/utils.rs",
        "pub fn format_output(msg: &str) -> String {\n    \
         format!(\"\u{1F389} {}\", msg)\n}\n\n\
         pub fn validate_config(config: &Config) -> bool {\n    \
         config.port > 0 && !config.host.is_empty()\n}\n",
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    // The plugin should run cleanly and produce a `~` glyph for the
    // modified line. If the surrogate-pair bug regresses, the plugin
    // throws and never paints the gutter.
    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '~'))
        .unwrap();
}

/// Regression: an empty line in the middle of an added block used to
/// be rendered without a green stripe ("skipped"), while the lines
/// around it were highlighted. The plugin emitted a zero-width overlay
/// `[lineStart, lineStart)` for empty lines; the renderer's overlay
/// sweep is driven by visible chars, of which an empty line has zero,
/// so a zero-width overlay never enters `line_touched_overlays` and
/// the trailing-fill path was never invoked. Fix bumps the end by 1
/// for empty lines so the range covers the trailing newline byte.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_highlights_empty_added_line() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_live_diff_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Original utils.rs ends after `validate_config`. Append three new
    // lines: a blank line, then a function, then another blank line.
    // The blank lines are the ones that used to be skipped.
    repo.modify_file(
        "src/utils.rs",
        "pub fn format_output(msg: &str) -> String {\n    \
         format!(\"[INFO] {}\", msg)\n}\n\n\
         pub fn validate_config(config: &Config) -> bool {\n    \
         config.port > 0 && !config.host.is_empty()\n}\n\
         \n\
         pub fn UNIQUE_NEW_FN_MARKER() {}\n\
         \n",
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("UNIQUE_NEW_FN_MARKER") && has_glyph(&s, '+')
        })
        .unwrap();

    let buf = harness.buffer();
    let mut marker_row: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if row.contains("UNIQUE_NEW_FN_MARKER") {
            marker_row = Some(y);
            break;
        }
    }
    let marker_row = marker_row.expect("never found new fn on screen");
    assert!(
        marker_row > 0,
        "expected an empty added line above the new fn",
    );
    let empty_row = marker_row - 1;
    let bg = buf[(40, empty_row)].style().bg;
    assert_eq!(
        bg,
        Some(ratatui::style::Color::Rgb(0, 80, 0)),
        "empty added line at row {empty_row} should have the green \
         diff_add_bg out to col 40; saw {bg:?}",
    );
}

/// Regression: pressing Down through empty lines used to skip them
/// when live-diff was enabled. With the plugin off, cursor moved one
/// line at a time as expected; with the plugin on, Down jumped from
/// the line above the empty block straight to the first non-empty
/// line below it.
///
/// Hypothesis: the per-line `addOverlay` calls (one per added line)
/// somehow interact with `move_visual_line`. Repro asserts cursor
/// position changes by one source line at a time on Down.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_does_not_skip_empty_lines_on_arrow_keys() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let repo = GitTestRepo::new();
    repo.setup_live_diff_plugin();
    // No setup_typical_project — we want a clean repo with one
    // committed file so the diff is a pure-additions hunk (no
    // confusing modify-vs-add LCS classification).
    repo.create_file("src/utils.rs", "head\n");
    repo.git_add(&["src/utils.rs"]);
    repo.git_commit("init");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Working tree: "head\n" + two empty lines + "tail\n". The two
    // blank middle lines are added empty lines — the ones the user
    // saw cursor skip over.
    repo.modify_file("src/utils.rs", "head\n\n\ntail\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    // Wait for the plugin to render the added-line `+` glyph so we
    // know its overlays are in place before we try to move the cursor.
    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '+'))
        .unwrap();

    // Move cursor to start of buffer.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let pos0 = harness.cursor_position();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos1 = harness.cursor_position();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos2 = harness.cursor_position();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos3 = harness.cursor_position();

    // Buffer is "head\n\n\ntail\n":
    //   line 0 ("head") starts at byte 0
    //   line 1 (empty) starts at byte 5
    //   line 2 (empty) starts at byte 6
    //   line 3 ("tail") starts at byte 7
    // Down should move 0 → 5 → 6 → 7. With live-diff buggy, Down skips
    // the two empty lines and lands directly at "tail" (byte 7).
    assert_eq!(pos0, 0, "expected cursor at start");
    assert_eq!(
        pos1, 5,
        "Down once should land at first empty line (byte 5); saw byte {pos1}",
    );
    assert_eq!(
        pos2, 6,
        "Down twice should land at second empty line (byte 6); saw byte {pos2}",
    );
    assert_eq!(
        pos3, 7,
        "Down thrice should land on 'tail' (byte 7); saw byte {pos3}",
    );
}

/// Regression: when two non-adjacent lines were modified with
/// unchanged context lines between them, the OLD content (rendered as
/// a `LineAbove` virtual line for each modified hunk) was anchored to
/// the wrong line. The virtual line for the SECOND modification
/// appeared above the unchanged context line, not above the modified
/// line itself.
///
/// User repro: changed two assignments separated by `} else {`. The
/// virtual "current_visual_column" (old line 3's content) appeared
/// between the new line 1 and the unchanged line 2, instead of
/// between the unchanged line 2 and the new line 3.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_virtual_line_anchored_to_correct_modified_line() {
    let repo = GitTestRepo::new();
    repo.setup_live_diff_plugin();

    // Use distinct unique markers so the row finders can't confuse
    // OLD virtual lines with NEW source lines or with the unchanged
    // "else" context.
    // Mirror the user's edit sequence. The HEAD content is identical
    // to what they had on disk; the user then *typed* the two ` + 1`
    // additions in sequence, first on the if-body line and then on
    // the else-body line. Each keystroke fires `after_insert`, which
    // schedules a debounced recompute — so both modifications and
    // both virtual lines should end up in place after a stable wait.
    // Long file that forces the modifications onto a scrolled
    // viewport — the user's bug only showed up at line 1280 of a
    // big buffer, not on a 5-line repro. Each filler line is a
    // unique string so the LCS can't accidentally match it
    // against the OLD/NEW markers.
    let mut head_lines = Vec::with_capacity(1290);
    for i in 1..=1280 {
        head_lines.push(format!("FILLER_LINE_NUMBER_{i:04}_unique"));
    }
    head_lines.push("        let goal = if cond {".into());
    head_lines.push("            UNIQUE_IF_BODY_OLD_MARKER".into());
    head_lines.push("        } else {".into());
    head_lines.push("            UNIQUE_ELSE_BODY_OLD_MARKER".into());
    head_lines.push("        };".into());
    let head_text = head_lines.join("\n") + "\n";

    repo.create_file("code.rs", &head_text);
    repo.git_add(&["code.rs"]);
    repo.git_commit("init");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "code.rs");

    // Jump to the if-body line (line 1282 / idx 1281), append ` + 1`.
    use crossterm::event::{KeyCode, KeyModifiers};
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    // Cursor now at end of last line. Up 4 → if-body line.
    for _ in 0..4 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" + 1").unwrap();
    harness.render().unwrap();

    // Wait for the first modification to render with its OLD virtual line.
    let virtual_row_present = |screen: &str, marker: &str| {
        screen
            .lines()
            .any(|l| l.contains(marker) && !l.contains(" + 1"))
    };
    harness
        .wait_until(|h| virtual_row_present(&h.screen_to_string(), "UNIQUE_IF_BODY_OLD_MARKER"))
        .unwrap();

    // Down 2 → else-body line (one unchanged "} else {" between).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" + 1").unwrap();
    harness.render().unwrap();

    // Wait until BOTH OLD virtual lines are present as their own rows.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            virtual_row_present(&s, "UNIQUE_IF_BODY_OLD_MARKER")
                && virtual_row_present(&s, "UNIQUE_ELSE_BODY_OLD_MARKER")
        })
        .unwrap();

    let buf = harness.buffer();
    let rows: Vec<String> = (0..buf.area.height)
        .map(|y| {
            (0..buf.area.width)
                .map(|x| buf[(x, y)].symbol().to_string())
                .collect::<String>()
        })
        .collect();

    let dump = || {
        rows.iter()
            .enumerate()
            .map(|(i, r)| format!("{i:3} | {}", r.trim_end()))
            .collect::<Vec<_>>()
            .join("\n")
    };

    // After both edits the buffer holds `UNIQUE_IF_BODY_OLD_MARKER + 1` and `UNIQUE_ELSE_BODY_OLD_MARKER + 1`
    // (the user appended ` + 1` to each line), and the virtual lines
    // hold the bare `UNIQUE_IF_BODY_OLD_MARKER` / `UNIQUE_ELSE_BODY_OLD_MARKER`. Distinguish the source
    // rows from the virtual rows by whether ` + 1` is present.
    let row_new_top = rows
        .iter()
        .position(|r| r.contains("UNIQUE_IF_BODY_OLD_MARKER + 1"))
        .unwrap_or_else(|| panic!("new top line not on screen. screen:\n{}", dump()));
    let row_else = rows
        .iter()
        .position(|r| r.contains("} else {"))
        .unwrap_or_else(|| panic!("unchanged else line not on screen. screen:\n{}", dump()));
    let row_new_bot = rows
        .iter()
        .position(|r| r.contains("UNIQUE_ELSE_BODY_OLD_MARKER + 1"))
        .unwrap_or_else(|| panic!("new bot line not on screen. screen:\n{}", dump()));
    let row_old_top = rows
        .iter()
        .position(|r| r.contains("UNIQUE_IF_BODY_OLD_MARKER") && !r.contains(" + 1"))
        .unwrap_or_else(|| panic!("old top virtual line not on screen. screen:\n{}", dump()));
    let row_old_bot = rows
        .iter()
        .position(|r| r.contains("UNIQUE_ELSE_BODY_OLD_MARKER") && !r.contains(" + 1"))
        .unwrap_or_else(|| panic!("old bot virtual line not on screen. screen:\n{}", dump()));

    // Layout invariants:
    //   * the OLD virtual line for the first modification sits directly
    //     above the NEW line that replaced it
    //   * the OLD virtual line for the second modification sits directly
    //     above the NEW line that replaced it (NOT above the unchanged
    //     "else" context line — that's the user-reported bug)
    assert_eq!(
        row_old_top + 1,
        row_new_top,
        "OLD top virtual line ({row_old_top}) should be directly above NEW top ({row_new_top})",
    );
    assert!(
        row_new_top < row_else,
        "NEW top row ({row_new_top}) should come before the unchanged else row ({row_else})",
    );
    assert_eq!(
        row_old_bot + 1,
        row_new_bot,
        "OLD bot virtual line ({row_old_bot}) should be directly above NEW bot ({row_new_bot}); \
         the user-reported bug puts it above the unchanged 'else' line instead",
    );
    assert!(
        row_else < row_old_bot,
        "unchanged 'else' row ({row_else}) should come before OLD bot virtual line ({row_old_bot})",
    );
}
