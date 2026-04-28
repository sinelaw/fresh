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

    // Wait for the new function and a `+` glyph to be visible — that
    // means live-diff has finished its first recompute.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("UNIQUE_NEW_FN_MARKER") && has_glyph(&s, '+')
        })
        .unwrap();

    // Now find the row immediately above `UNIQUE_NEW_FN_MARKER` (the
    // empty added line) and check that a column well past the gutter
    // has the bg color the plugin paints for added lines (the
    // theme-resolved `editor.diff_add_bg`). For the bundled
    // high-contrast theme that's `Rgb(0, 80, 0)`.
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
