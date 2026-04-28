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

    open_file(&mut harness, &repo.path, "src/utils.rs");

    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '+'))
        .unwrap();
}

/// vs HEAD: a modified line shows `~` in the gutter AND a virtual line
/// rendered above it containing the OLD text prefixed with `- `.
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

    open_file(&mut harness, &repo.path, "src/utils.rs");

    // Wait for the modified glyph to appear.
    harness
        .wait_until(|h| has_glyph(&h.screen_to_string(), '~'))
        .unwrap();

    // The virtual line carries the OLD content prefixed with "- ".
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
