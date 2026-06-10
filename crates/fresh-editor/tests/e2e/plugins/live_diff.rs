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

    // Replace the `format!` line with a deliberately rewrite-style
    // line: the new content must share so little with the original
    // that the live-diff classifier's Sørensen–Dice similarity
    // (`SIMILARITY_THRESHOLD = 0.5`) falls below the threshold and the
    // hunk is split into separate `removed` + `added` halves — only
    // the `removed` half emits the OLD virtual line this test asserts
    // on. A trivial 1-token swap looks like an in-place `modified`
    // (bg-only, no virtual line) and never shows `[INFO]` on screen.
    repo.modify_file(
        "src/utils.rs",
        r#"pub fn format_output(msg: &str) -> String {
    panic!("EXTENSIVELY_REWRITTEN_BODY_LIVE_DIFF_REPLACED_LINE_WITH_LONG_UNIQUE_PAYLOAD_TO_DROP_SIMILARITY");
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

    // Low-similarity rewrite → `removed` + `added` split, so the
    // OLD-content virtual line carries the `-` deletion glyph rather
    // than the `~` modified glyph.
    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '-'))
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

/// Regression: a deletion virtual row whose OLD content is empty was
/// rendered without the red `diff_remove_bg` stripe — only its `-`
/// gutter glyph showed up, so an empty deleted line looked like blank
/// editor space rather than part of the deletion block.
///
/// Cause: the bg-fill fallback for virtual lines reads
/// `current_view_line.char_styles.first()` to recover the line's
/// intended bg, but an empty virtual line has zero chars, so `.first()`
/// returns None and no fill style is applied.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_highlights_empty_removed_line() {
    let repo = GitTestRepo::new();
    repo.setup_live_diff_plugin();

    // HEAD has a blank line in the middle; working tree drops it. The
    // plugin should render that blank line as a virtual deletion row
    // (just the gutter `-` plus a full-width red stripe).
    repo.create_file("src/utils.rs", "head\n\ntail\n");
    repo.git_add(&["src/utils.rs"]);
    repo.git_commit("init");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    repo.modify_file("src/utils.rs", "head\ntail\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        20,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            has_glyph(&s, '-') && has_text(&s, "tail")
        })
        .unwrap();

    // Find the row whose first non-whitespace cell is `-` AND whose
    // text body (after the gutter) is empty — that's our empty
    // deletion virtual row.
    let buf = harness.buffer();
    let mut empty_del_row: Option<u16> = None;
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        let trimmed = row.trim_end();
        // Gutter shape on a deletion virtual row: leading `-`, then the
        // pipe separator, then nothing else (empty content).
        if trimmed.contains('-')
            && trimmed.contains('│')
            && trimmed
                .split('│')
                .nth(1)
                .is_some_and(|body| body.chars().all(|c| c.is_whitespace()))
        {
            empty_del_row = Some(y);
            break;
        }
    }
    let empty_del_row = empty_del_row.expect("never found an empty deletion virtual row on screen");

    // The empty deletion row should be filled with the red
    // diff_remove_bg out to the viewport edge — sample at col 40.
    // `Config::default()` selects the `high-contrast` theme, whose
    // `diff_remove_bg` is `[100, 0, 0]` (see `themes/high-contrast.json`).
    let bg = buf[(40, empty_del_row)].style().bg;
    assert_eq!(
        bg,
        Some(ratatui::style::Color::Rgb(100, 0, 0)),
        "empty deletion virtual row at y={empty_del_row} should be \
         filled with diff_remove_bg (100, 0, 0) at col 40; saw {bg:?}",
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
    // to what they had on disk; the user then *typed* a long unique
    // suffix on each marker line, first on the if-body line and then
    // on the else-body line. Each keystroke fires `after_insert`, which
    // schedules a debounced recompute — so both modifications and
    // both virtual lines should end up in place after a stable wait.
    // Long file that forces the modifications onto a scrolled
    // viewport — the user's bug only showed up at line 1280 of a
    // big buffer, not on a 5-line repro. Each filler line is a
    // unique string so the LCS can't accidentally match it
    // against the OLD/NEW markers.
    //
    // The appended payload is long enough to drop the live-diff
    // Sørensen-Dice line-similarity below the `SIMILARITY_THRESHOLD`
    // (0.5). Below the threshold each modification is split into
    // separate `removed` + `added` halves and the `removed` half is
    // what emits the OLD virtual line this test asserts on. A short
    // tail like " + 1" classifies as in-place `modified` (bg-only,
    // word-diff bold/underline) and never produces a virtual line.
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

    // Wide viewport so the marker line + the long appended payload fit
    // on a single visual row — wrap would invalidate the row-index
    // assertions below.
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        220,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "code.rs");

    // Long rewrite-style suffix the user "types" on each marker line.
    // Same suffix on both lines is fine — the OLD/NEW row predicates
    // disambiguate by per-line marker (IF vs ELSE), and use the suffix
    // only to tell OLD virtual rows (don't contain it) from NEW source
    // rows (do contain it).
    const APPEND_PAYLOAD: &str =
        " + REWRITE_PAYLOAD_DROPS_SIMILARITY_BELOW_HALF_THRESHOLD_LIVE_DIFF_REGRESSION_PADDING_xyz_ABC_DEF_GHI_001";

    // Jump to the if-body line (line 1282 / idx 1281), append the payload.
    use crossterm::event::{KeyCode, KeyModifiers};
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    // Cursor now at end of last line. Up 4 → if-body line.
    for _ in 0..4 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(APPEND_PAYLOAD).unwrap();
    harness.render().unwrap();

    // Wait for the first modification to render with its OLD virtual line.
    let virtual_row_present = |screen: &str, marker: &str| {
        screen
            .lines()
            .any(|l| l.contains(marker) && !l.contains(APPEND_PAYLOAD))
    };
    harness
        .wait_until(|h| virtual_row_present(&h.screen_to_string(), "UNIQUE_IF_BODY_OLD_MARKER"))
        .unwrap();

    // Down 2 → else-body line (one unchanged "} else {" between).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(APPEND_PAYLOAD).unwrap();
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

    // After both edits the buffer holds the marker followed by
    // `APPEND_PAYLOAD` on each line, and the virtual lines hold the
    // bare marker. Distinguish source rows from virtual rows by
    // whether the payload is present.
    let row_new_top = rows
        .iter()
        .position(|r| r.contains("UNIQUE_IF_BODY_OLD_MARKER") && r.contains(APPEND_PAYLOAD))
        .unwrap_or_else(|| panic!("new top line not on screen. screen:\n{}", dump()));
    let row_else = rows
        .iter()
        .position(|r| r.contains("} else {"))
        .unwrap_or_else(|| panic!("unchanged else line not on screen. screen:\n{}", dump()));
    let row_new_bot = rows
        .iter()
        .position(|r| r.contains("UNIQUE_ELSE_BODY_OLD_MARKER") && r.contains(APPEND_PAYLOAD))
        .unwrap_or_else(|| panic!("new bot line not on screen. screen:\n{}", dump()));
    let row_old_top = rows
        .iter()
        .position(|r| r.contains("UNIQUE_IF_BODY_OLD_MARKER") && !r.contains(APPEND_PAYLOAD))
        .unwrap_or_else(|| panic!("old top virtual line not on screen. screen:\n{}", dump()));
    let row_old_bot = rows
        .iter()
        .position(|r| r.contains("UNIQUE_ELSE_BODY_OLD_MARKER") && !r.contains(APPEND_PAYLOAD))
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

/// Regression: with live-diff active, deleting 3+ consecutive lines in
/// the middle of a buffer (so the deletion is rendered as a block of
/// virtual `-` lines between two real lines) used to block Down-arrow
/// motion when the deletion block starts with an EMPTY line. The
/// cursor would sit on the real line just before the deletion block,
/// jump to end-of-line on the first Down (as if it were on the last
/// line of the buffer), and then refuse to move further. Pressing
/// Down repeatedly never reached the next real line.
///
/// Trigger details: a deletion virtual row whose visible text is empty
/// confuses the visual-line walker, so the next-real-line lookup falls
/// through to the byte-based fallback and snaps the cursor to EOL of
/// the current line. Non-empty deletion rows alone do not trigger it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_down_arrow_traverses_deletion_block() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let repo = GitTestRepo::new();
    repo.setup_live_diff_plugin();

    // Build a HEAD with a long deletion block in the middle: 5 lines
    // before, 12 lines that will be deleted, and 5 lines after. The
    // bug only manifests when the "real next line" after the deletion
    // is OUTSIDE the cached view-line mappings — i.e. off-screen —
    // because that's the case where `move_visual_line` returns None
    // and the byte-based fallback takes over and snaps to end-of-line.
    let mut head_lines = Vec::new();
    for i in 1..=5 {
        head_lines.push(format!("before_{i:02}"));
    }
    // First deleted line is empty — that's the user-reported shape
    // (their README hunk started with a blank line). Then a mix of
    // text and blanks, since their hunk had several blanks too.
    head_lines.push(String::new());
    head_lines.push("DELETED_LINE_02".into());
    head_lines.push(String::new());
    head_lines.push("DELETED_LINE_04".into());
    head_lines.push(String::new());
    head_lines.push("DELETED_LINE_06".into());
    head_lines.push(String::new());
    for i in 1..=5 {
        head_lines.push(format!("after_{i:02}"));
    }
    let head_text = head_lines.join("\n") + "\n";
    repo.create_file("src/utils.rs", &head_text);
    repo.git_add(&["src/utils.rs"]);
    repo.git_commit("init");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Working tree: drop the 12 middle lines. The plugin should render
    // them as virtual `-` rows anchored above `after_01`.
    let mut work_lines = Vec::new();
    for i in 1..=5 {
        work_lines.push(format!("before_{i:02}"));
    }
    for i in 1..=5 {
        work_lines.push(format!("after_{i:02}"));
    }
    let work_text = work_lines.join("\n") + "\n";
    repo.modify_file("src/utils.rs", &work_text);

    // Small viewport so the 12 virtual deletion rows + the rows after
    // them don't all fit on screen at once. ~14 content rows after
    // menu/tab/status overhead means the `after_*` lines can be off
    // screen when the cursor is on `before_05`.
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        18,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/utils.rs");

    // Wait for the deletion glyph + a known middle deletion marker.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            has_glyph(&s, '-') && has_text(&s, "DELETED_LINE_02")
        })
        .unwrap();

    // Move cursor to start of buffer, then to end of `before_05` —
    // the real line immediately before the deletion block.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let pos_before = harness.cursor_position();
    // before_01..before_05 each occupy 10 bytes ("before_NN\n"). After 4
    // Downs from byte 0 we should be at the start of `before_05`, byte 40.
    assert_eq!(
        pos_before,
        40,
        "expected cursor at start of `before_05` (byte 40); saw byte \
         {pos_before}. Screen:\n{}",
        harness.screen_to_string()
    );

    // Buffer (working tree) is "before_01\n...before_05\nafter_01\n...after_05\n".
    //   before_05 starts at byte 40
    //   after_01  starts at byte 50 (10 bytes per `before_NN\n` line)
    // Down once from byte 40 should land at byte 50. With the bug, the
    // deletion virtual block fools the visual-line motion into thinking
    // the buffer ends after `before_05`, so the cursor either snaps to
    // end-of-line on `before_05` (byte 49) or stays stuck at 40.
    let screen_before_down = harness.screen_to_string();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let pos_after = harness.cursor_position();
    assert_eq!(
        pos_after, 50,
        "Down from `before_05` should reach `after_01` (byte 50); saw \
         byte {pos_after} — deletion virtual block is blocking cursor \
         motion (cursor snaps to EOL of `before_05` instead). \
         Screen at moment of Down:\n{screen_before_down}",
    );
}

/// Regression for #2177: at a degenerate effective text width (here a
/// tiny `wrap_column`, but a narrow split pane or a wide gutter reaches
/// the same condition), normal lines stop wrapping — but the deletion
/// virtual line used to be force-wrapped at width 1, splitting the OLD
/// content one character per row ("single-letter wrapping").
///
/// With the fix the virtual-line wrap mirrors the source-line bail-out
/// (`available_width < 2` → don't wrap), so the OLD content stays on a
/// single row and its distinctive substring is visible contiguously.
/// Before the fix every grapheme sat on its own row and the substring
/// never appeared, so the final assertion failed.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_removed_line_not_split_per_char_at_tiny_width() {
    let repo = GitTestRepo::new();

    // Committed baseline (the OLD / HEAD reference). Deliberately free of
    // `-` characters so the only `-` that can appear on screen is the
    // deletion gutter glyph we wait on (a `->` return arrow would be a
    // false positive). The body line carries a distinctive token we
    // later assert renders contiguously.
    repo.create_file(
        "src/note.rs",
        "pub fn greet() {\n    println!(\"LIVEDIFF_OLD_DISTINCTIVE_TOKEN hello there friend\");\n}\n",
    );
    repo.git_add_all();
    repo.git_commit("baseline");
    repo.setup_live_diff_plugin();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Working tree: rewrite the body line into something with very low
    // Sørensen–Dice similarity to the original, so the hunk splits into
    // separate removed + added halves and the deletion emits the OLD
    // virtual line this test inspects (a 1-token swap would render as an
    // in-place `modified` with no virtual line). Still `-`-free.
    repo.modify_file(
        "src/note.rs",
        "pub fn greet() {\n    panic!(\"COMPLETELY_DIFFERENT_REWRITE_PAYLOAD_DROPPING_SIMILARITY_FAR_BELOW_THE_THRESHOLD\");\n}\n",
    );

    // `wrap_column = 2` drives the per-row content width below the gutter
    // (`effective_width - gutter_width < 2`) regardless of the 120-col
    // window — the same degenerate condition a narrow split pane or a
    // very wide gutter would reach.
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config.editor.wrap_column = Some(2);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, repo.path.clone()).unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "src/note.rs");

    // Wait for the deletion virtual line to render — its `-` gutter glyph
    // appears in both the buggy (per-char) and fixed layouts.
    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '-'))
        .unwrap();

    // The OLD line was `    println!("LIVEDIFF_OLD_DISTINCTIVE_TOKEN ...")`.
    // With per-char wrapping this run is shattered one grapheme per row
    // and the substring never appears; with the fix the removed line
    // renders on a single row, so the token is contiguous.
    let screen = harness.screen_to_string();
    assert!(
        has_text(&screen, "LIVEDIFF_OLD_DISTINCTIVE_TOKEN"),
        "removed line should render on a single row, not one char per \
         row (#2177). Screen:\n{screen}"
    );
}

/// Find the row containing `needle` and return `(y, x_of_needle_start)`.
/// Assumes single-width ASCII content (each cell is one char).
fn find_text_cell(buf: &ratatui::buffer::Buffer, needle: &str) -> Option<(u16, u16)> {
    for y in 0..buf.area.height {
        let mut row = String::new();
        for x in 0..buf.area.width {
            row.push_str(buf[(x, y)].symbol());
        }
        if let Some(idx) = row.find(needle) {
            return Some((y, idx as u16));
        }
    }
    None
}

/// True when the cell at `(x, y)` carries both the bold and underline
/// modifiers — the word-level diff emphasis used by live-diff.
fn is_word_diff_emphasized(buf: &ratatui::buffer::Buffer, x: u16, y: u16) -> bool {
    use ratatui::style::Modifier;
    let m = buf[(x, y)].style().add_modifier;
    m.contains(Modifier::BOLD) && m.contains(Modifier::UNDERLINED)
}

/// Issue #1949: a low-similarity rewrite splits into a deletion virtual
/// line + an added line. The deletion line must bold + underline the
/// words that were *removed*, and the added line the words that were
/// *added* — while tokens common to both lines stay unemphasized, so
/// the user can spot what actually changed inside the pair.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_live_diff_word_highlight_on_low_similarity_removed_added_pair() {
    let repo = GitTestRepo::new();
    repo.setup_live_diff_plugin();

    // Plain text file: no syntax highlighting, so the only bold +
    // underline cells on screen come from the word-diff overlays.
    // The first and last tokens are shared between HEAD and the
    // working tree; the middle word is fully rewritten with enough
    // unique characters to push Sørensen–Dice similarity below the
    // 0.95 split threshold. No `-` characters anywhere so the only
    // `-` on screen is the deletion gutter glyph.
    repo.create_file(
        "note.txt",
        "SHARED_HEAD_TOKEN OLD_REMOVED_WORD_PAYLOAD_ZZZZ SHARED_TAIL_TOKEN\n",
    );
    repo.git_add(&["note.txt"]);
    repo.git_commit("baseline");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    repo.modify_file(
        "note.txt",
        "SHARED_HEAD_TOKEN NEW_ADDED_REPLACEMENT_QQQQ SHARED_TAIL_TOKEN\n",
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    enable_live_diff_globally(&mut harness);
    open_file(&mut harness, &repo.path, "note.txt");

    // Deletion virtual line (old content) + added line (new content)
    // both on screen.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            has_glyph(&s, '-')
                && has_text(&s, "OLD_REMOVED_WORD_PAYLOAD_ZZZZ")
                && has_text(&s, "NEW_ADDED_REPLACEMENT_QQQQ")
        })
        .unwrap();

    let buf = harness.buffer();

    // Removed side: the rewritten word is emphasized...
    let (old_y, old_x) = find_text_cell(buf, "OLD_REMOVED_WORD_PAYLOAD_ZZZZ")
        .expect("deletion virtual line not found on screen");
    assert!(
        is_word_diff_emphasized(buf, old_x, old_y),
        "removed word on the deletion virtual line (row {old_y}, col {old_x}) \
         should be bold + underlined",
    );
    // ...while the token shared with the new line is not.
    let (head_y, head_x) =
        find_text_cell(buf, "SHARED_HEAD_TOKEN").expect("shared head token not found on screen");
    assert_eq!(
        head_y, old_y,
        "expected the first SHARED_HEAD_TOKEN occurrence on the deletion \
         virtual line (it renders above the added line)",
    );
    assert!(
        !is_word_diff_emphasized(buf, head_x, head_y),
        "shared token on the deletion virtual line (row {head_y}, col \
         {head_x}) should NOT be bold + underlined",
    );

    // Added side: the replacement word is emphasized, its shared
    // neighbor is not.
    let (new_y, new_x) =
        find_text_cell(buf, "NEW_ADDED_REPLACEMENT_QQQQ").expect("added line not found on screen");
    assert!(
        is_word_diff_emphasized(buf, new_x, new_y),
        "added word on the new line (row {new_y}, col {new_x}) should be \
         bold + underlined",
    );
    assert!(
        !is_word_diff_emphasized(buf, head_x, new_y),
        "shared token on the added line (row {new_y}, col {head_x}) should \
         NOT be bold + underlined",
    );
}
