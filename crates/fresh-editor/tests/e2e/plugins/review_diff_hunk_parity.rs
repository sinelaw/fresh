//! E2E tests for the hunk-parity Review Diff increments
//! (docs/internal/REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md):
//!   * §5.2 — the file sidebar (status glyph, +/- counts, comment badge)
//!   * §5.1 — the 1/2/0 split/stack layout toggle
//!   * §5.6 — bordered inline review notes
//!
//! All assertions observe rendered screen output only.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Repo with one committed file and one unstaged modification that has a
/// few added lines (so there is a diff line to comment on).
fn repo_with_modification() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");
    fs::write(
        repo.path.join("src/main.rs"),
        "fn main() {\n    println!(\"one\");\n    println!(\"two\");\n    println!(\"three\");\n}\n",
    )
    .unwrap();
    repo
}

/// Repo with two committed, then modified, files (for filter tests).
fn repo_with_two_files() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    fs::write(repo.path.join("src/main.rs"), "fn main() {}\n").unwrap();
    fs::write(repo.path.join("src/widget.rs"), "pub fn widget() {}\n").unwrap();
    repo.git_add_all();
    repo.git_commit("Initial commit");
    fs::write(
        repo.path.join("src/main.rs"),
        "fn main() {\n    println!(\"changed main\");\n}\n",
    )
    .unwrap();
    fs::write(
        repo.path.join("src/widget.rs"),
        "pub fn widget() {\n    // changed widget\n}\n",
    )
    .unwrap();
    repo
}

fn harness_for(repo: &GitTestRepo) -> EditorTestHarness {
    EditorTestHarness::with_config_and_working_dir(160, 44, Config::default(), repo.path.clone())
        .unwrap()
}

/// Open Review Diff via the command palette and wait for it to load.
fn open_review_diff(harness: &mut EditorTestHarness) -> String {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            if s.contains("TypeError") || s.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", s);
            }
            s.contains("next hunk")
        })
        .unwrap();
    harness.screen_to_string()
}

/// §5.2 — the file sidebar lists the changed file with a FILES header and
/// add/remove counts.
#[test]
fn test_review_sidebar_lists_files() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    let screen = open_review_diff(&mut harness);

    assert!(
        screen.contains("FILES"),
        "sidebar header should be visible. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("main.rs"),
        "the modified file should appear in the sidebar. Screen:\n{}",
        screen
    );
    // The sidebar row carries the add count (the file has added lines).
    assert!(
        screen.contains("+3") || screen.contains("+4") || screen.contains("+5"),
        "sidebar row should show an add count. Screen:\n{}",
        screen
    );
}

/// §5.1 — `1` switches to the side-by-side split, `2` returns to the
/// unified stack with the sidebar intact.
#[test]
fn test_review_layout_toggle_split_and_back() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // `1` renders the focused file as an in-panel side-by-side (the sidebar
    // stays); the status line confirms the mode.
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Side-by-side view") && s.contains("FILES")
        })
        .unwrap();

    // `2` returns to the unified stack, sidebar intact.
    harness
        .send_key(KeyCode::Char('2'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("Unified view") && s.contains("FILES")
        })
        .unwrap();
}

/// §5.6 — a review note renders as a bordered box anchored under its diff
/// line, not the old single `»` row.
#[test]
fn test_review_inline_comment_renders_as_box() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // Land on a diff content line: jump to the hunk, then step down past
    // the hunk header into an added line.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("needs a wrapping note that proves the box")
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The bordered callout: top/bottom border glyphs are present
            // and the note text shows up inside.
            s.contains("╭") && s.contains("╰") && s.contains("wrapping note")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("\u{00bb} ["),
        "the old single-line `\u{00bb} [ref]` rendering should be gone. Screen:\n{}",
        screen
    );
}

/// §5.11 — `/` filters the file list: typing a query narrows the sidebar to
/// matching files and hides the rest.
#[test]
fn test_review_filter_narrows_files() {
    init_tracing_from_env();
    let repo = repo_with_two_files();
    let mut harness = harness_for(&repo);
    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("main.rs") && screen.contains("widget.rs"),
        "both files initially. Screen:\n{}",
        screen
    );

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("widget").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("widget.rs") && !s.contains("main.rs") && s.contains("/widget")
        })
        .unwrap();
}

/// The comment hazard: pressing `c` off a diff line (e.g. on a header) hops
/// to the nearest diff line and opens the prompt instead of no-opping (which
/// would leave the next keystrokes to execute as commands).
#[test]
fn test_review_comment_from_header_opens_prompt() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // Cursor starts at the top of the stream (a section/file header row, not
    // a diff line). `c` should still open the comment prompt.
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    // If the hazard regressed, no prompt opens and this waits out (external
    // timeout). On success the comment prompt is up.
    harness.wait_for_prompt().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Comment on"))
        .unwrap();
}

/// §5.6 — the COMMENTS side panel renders the full note wrapped over
/// multiple lines, not a single truncated row: both the first and a late
/// word of a long note are visible.
#[test]
fn test_review_comments_panel_wraps_full_note() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness
        .type_text("ALPHAWORD this note is long enough to wrap across several panel rows OMEGAWORD")
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // The whole note is present (not truncated): the trailing word shows.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("ALPHAWORD") && s.contains("OMEGAWORD")
        })
        .unwrap();
}

/// §5.12 — `W` toggles watch (auto-reload on save) and reports its state.
#[test]
fn test_review_watch_toggle_status() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('W'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Watching for changes"))
        .unwrap();

    harness
        .send_key(KeyCode::Char('W'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Watch off"))
        .unwrap();
}

/// The `?` help reference opens and, per its own "Press q to close" hint,
/// `q` dismisses it back to the review (regression: it used to be a plain
/// buffer with no close binding, trapping the user).
#[test]
fn test_review_help_opens_and_q_closes() {
    init_tracing_from_env();
    let repo = repo_with_modification();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('?'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("keyboard reference"))
        .unwrap();

    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // Help gone, back to the review (sidebar visible).
            !s.contains("keyboard reference") && s.contains("FILES")
        })
        .unwrap();
}
