//! E2E tests for Review Diff line-level visual staging / unstaging / discard
//! (issue #2317).
//!
//! The feature's own help bar advertises `Visual: j/k extend, s/u/d apply`,
//! and the docs promise "a line-level visual selection on the cursor row".
//! These tests drive that exact path: put the cursor on a real added line,
//! press `v` to start a visual selection, then `s`/`u`/`d`, and assert the
//! git index/worktree actually changed.
//!
//! All assertions observe rendered screen output and real `git` state only.

use crate::common::git_test_helper::{git_command, GitTestRepo};
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

/// A repo with one committed file and a single appended line in the working
/// tree — exactly the reproduction from issue #2317.
fn repo_with_one_added_line() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("README.md", "# Calc\nA tiny calculator.\n");
    repo.git_add_all();
    repo.git_commit("initial");
    // One unstaged "+extra line" addition.
    fs::write(
        repo.path.join("README.md"),
        "# Calc\nA tiny calculator.\nextra line\n",
    )
    .unwrap();
    repo
}

fn harness_for(repo: &GitTestRepo) -> EditorTestHarness {
    EditorTestHarness::with_config_and_working_dir(160, 44, Config::default(), repo.path.clone())
        .unwrap()
}

fn open_review_diff(harness: &mut EditorTestHarness) {
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
}

/// Parse the `Ln N` indicator from the status bar (the diff buffer's
/// 1-indexed cursor line).
fn status_line_number(harness: &EditorTestHarness) -> Option<usize> {
    let screen = harness.screen_to_string();
    for line in screen.lines() {
        if let Some(idx) = line.find("Ln ") {
            let rest = &line[idx + 3..];
            let num: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
            if let Ok(n) = num.parse::<usize>() {
                return Some(n);
            }
        }
    }
    None
}

/// Find the diff-buffer line number whose rendered center-panel row contains
/// `needle`. The center diff panel starts at a fixed screen row, so the
/// buffer line is `screen_row - CENTER_FIRST_ROW + 1`.
const CENTER_FIRST_ROW: usize = 7;
fn diff_line_of(harness: &mut EditorTestHarness, needle: &str) -> usize {
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    for (row, line) in screen.lines().enumerate() {
        if row >= CENTER_FIRST_ROW && line.contains(needle) {
            return row - CENTER_FIRST_ROW + 1;
        }
    }
    panic!(
        "no center-panel row renders {:?}. Screen:\n{}",
        needle, screen
    );
}

/// Step the diff cursor down until the status bar reports `target` line.
fn move_cursor_to_line(harness: &mut EditorTestHarness, target: usize) {
    for _ in 0..60 {
        harness.render().unwrap();
        if status_line_number(harness) == Some(target) {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    panic!(
        "never reached diff line {} (status: {:?}). Screen:\n{}",
        target,
        status_line_number(harness),
        harness.screen_to_string()
    );
}

/// Bounded settle loop: pump the async runtime up to ~3s waiting for
/// `cond` to hold, then return whether it did. Unlike `wait_until`, this
/// never hangs — a failing operation simply returns `false`.
fn settle_until<F: Fn() -> bool>(harness: &mut EditorTestHarness, cond: F) -> bool {
    for _ in 0..60 {
        harness.tick_and_render().unwrap();
        if cond() {
            return true;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    cond()
}

fn cached_diff(repo: &GitTestRepo) -> String {
    let out = git_command(&repo.path)
        .args(["diff", "--cached"])
        .output()
        .expect("git diff --cached");
    String::from_utf8_lossy(&out.stdout).to_string()
}

/// #2317 — `v` then `s` stages exactly the selected added line.
#[test]
fn test_review_visual_stage_single_added_line() {
    init_tracing_from_env();
    let repo = repo_with_one_added_line();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // Jump to the hunk, then walk down onto the green "+extra line" row.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    let target = diff_line_of(&mut harness, "+extra line");
    move_cursor_to_line(&mut harness, target);

    // Start a visual selection and stage it.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    // The line-level selection must stage the line. Give the async git
    // apply + refresh a chance to complete.
    settle_until(&mut harness, || cached_diff(&repo).contains("+extra line"));

    let staged = cached_diff(&repo);
    assert!(
        staged.contains("+extra line"),
        "line-level visual stage should stage the selected added line; \
         `git diff --cached` was:\n{}\nScreen:\n{}",
        staged,
        harness.screen_to_string()
    );
}

/// A repo with a one-line modification in the working tree (produces a
/// `-old`/`+new` pair in the hunk).
fn repo_with_one_modified_line() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("README.md", "alpha\nbeta\ngamma\n");
    repo.git_add_all();
    repo.git_commit("initial");
    fs::write(repo.path.join("README.md"), "alpha\nBETA\ngamma\n").unwrap();
    repo
}

/// #2317 — `v` then `j` (extend over the `-old`/`+new` pair) then `s` stages
/// the whole one-line modification.
#[test]
fn test_review_visual_stage_modified_line_pair() {
    init_tracing_from_env();
    let repo = repo_with_one_modified_line();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    // Land on the removed "-beta" row, then visual-extend down over "+BETA".
    let target = diff_line_of(&mut harness, "-beta");
    move_cursor_to_line(&mut harness, target);
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    settle_until(&mut harness, || cached_diff(&repo).contains("+BETA"));

    let staged = cached_diff(&repo);
    assert!(
        staged.contains("+BETA") && staged.contains("-beta"),
        "line-level visual stage should stage the -/+ modification pair; \
         `git diff --cached` was:\n{}\nScreen:\n{}",
        staged,
        harness.screen_to_string()
    );
}

/// #2317 — `v` then `d` discards the selected added line from the working tree.
#[test]
fn test_review_visual_discard_single_added_line() {
    init_tracing_from_env();
    let repo = repo_with_one_added_line();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    let target = diff_line_of(&mut harness, "+extra line");
    move_cursor_to_line(&mut harness, target);
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();

    settle_until(&mut harness, || {
        let content = fs::read_to_string(repo.path.join("README.md")).unwrap_or_default();
        !content.contains("extra line")
    });

    let content = fs::read_to_string(repo.path.join("README.md")).unwrap();
    assert!(
        !content.contains("extra line"),
        "line-level visual discard should remove the added line from the \
         working tree; README.md is:\n{}\nScreen:\n{}",
        content,
        harness.screen_to_string()
    );
}

/// A repo whose single hunk contains two *separate* added lines, so a
/// line-level selection of one is observably different from staging the
/// whole hunk.
fn repo_with_two_separate_additions() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("README.md", "a\nb\nc\n");
    repo.git_add_all();
    repo.git_commit("initial");
    fs::write(repo.path.join("README.md"), "a\nADD1\nb\nADD2\nc\n").unwrap();
    repo
}

/// #2317 — the decisive test: selecting *only* the first added line and
/// staging must stage `+ADD1` but leave `+ADD2` unstaged. If the line-level
/// path silently fell through to whole-hunk staging, both would appear.
#[test]
fn test_review_visual_stage_only_selected_line_of_hunk() {
    init_tracing_from_env();
    let repo = repo_with_two_separate_additions();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    let target = diff_line_of(&mut harness, "+ADD1");
    move_cursor_to_line(&mut harness, target);
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    settle_until(&mut harness, || cached_diff(&repo).contains("+ADD1"));

    let staged = cached_diff(&repo);
    assert!(
        staged.contains("+ADD1"),
        "the selected line should be staged; `git diff --cached`:\n{}\nScreen:\n{}",
        staged,
        harness.screen_to_string()
    );
    assert!(
        !staged.contains("+ADD2"),
        "ONLY the selected line should be staged — `+ADD2` must remain \
         unstaged, proving this is line-level (not whole-hunk) staging; \
         `git diff --cached`:\n{}\nScreen:\n{}",
        staged,
        harness.screen_to_string()
    );
}

/// Two unstaged files, each with one added line. Used to exercise the
/// hunk-header row lookup when a *preceding* file is collapsed.
fn repo_with_two_files_each_one_addition() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("alpha.txt", "alpha-line\n");
    repo.create_file("bravo.txt", "bravo-line\n");
    repo.git_add_all();
    repo.git_commit("initial");
    fs::write(repo.path.join("alpha.txt"), "alpha-line\nADDED_A\n").unwrap();
    fs::write(repo.path.join("bravo.txt"), "bravo-line\nADDED_B\n").unwrap();
    repo
}

/// #2317 — regression for the hunk-header row mapping in the default
/// focus-mode view. Only the focused file's hunks are emitted into the diff
/// stream, but the old `selectionLineRange` counted hunks across *all* files
/// to index `hunkHeaderRows`. For any file that isn't the first, that index
/// overshot, the header row came back `undefined`, and the operation failed
/// with "Selection has no add/remove lines or crosses hunk boundary".
#[test]
fn test_review_visual_stage_line_in_second_file() {
    init_tracing_from_env();
    let repo = repo_with_two_files_each_one_addition();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    // Jump hunk-to-hunk until the second file's added line is rendered
    // (focus mode only paints the focused file's body), then land on it.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    let bravo_added = diff_line_of(&mut harness, "+ADDED_B");
    move_cursor_to_line(&mut harness, bravo_added);

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    settle_until(&mut harness, || cached_diff(&repo).contains("+ADDED_B"));

    let staged = cached_diff(&repo);
    assert!(
        staged.contains("+ADDED_B"),
        "line-staging a line in the second (focused) file should stage \
         `+ADDED_B`; `git diff --cached`:\n{}\nScreen:\n{}",
        staged,
        harness.screen_to_string()
    );
}

/// A repo with a single added line already staged in the index.
fn repo_with_one_staged_added_line() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("README.md", "# Calc\nA tiny calculator.\n");
    repo.git_add_all();
    repo.git_commit("initial");
    fs::write(
        repo.path.join("README.md"),
        "# Calc\nA tiny calculator.\nextra line\n",
    )
    .unwrap();
    repo.git_add_all(); // stage the addition
    repo
}

/// #2317 — `v` then `u` unstages the selected staged line (the scenario that
/// reported `patch does not apply`).
#[test]
fn test_review_visual_unstage_single_added_line() {
    init_tracing_from_env();
    let repo = repo_with_one_staged_added_line();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    let target = diff_line_of(&mut harness, "+extra line");
    move_cursor_to_line(&mut harness, target);
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();

    settle_until(&mut harness, || !cached_diff(&repo).contains("+extra line"));

    let staged = cached_diff(&repo);
    assert!(
        !staged.contains("+extra line"),
        "line-level visual unstage should unstage the selected line; \
         `git diff --cached` still shows it:\n{}\nScreen:\n{}",
        staged,
        harness.screen_to_string()
    );
}
