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

/// The diff-buffer line whose rendered center-panel row contains `needle`, or
/// `None` if no visible center-panel row does. The center diff panel starts at
/// a fixed screen row, so the buffer line is `screen_row - CENTER_FIRST_ROW + 1`
/// (both this and `status_line_number` are read in the same buffer-line space,
/// which is what lets `move_cursor_onto` converge the caret onto the needle).
const CENTER_FIRST_ROW: usize = 7;
fn screen_row_of(harness: &EditorTestHarness, needle: &str) -> Option<usize> {
    for (row, line) in harness.screen_to_string().lines().enumerate() {
        if row >= CENTER_FIRST_ROW && line.contains(needle) {
            return Some(row - CENTER_FIRST_ROW + 1);
        }
    }
    None
}

/// Move the diff cursor onto the row that renders `needle`.
///
/// Self-correcting and bounded. Each iteration re-reads the caret's line
/// (`status_line_number`) and the needle's current line (`screen_row_of`) from
/// the *same* frame and takes a single step toward it — in *either* direction.
/// Re-deriving the target every step is what makes this robust to the async,
/// multi-phase hunk-jump/focus repaint: a mid-reflow frame can at worst cost
/// one corrective step, never strand the cursor. `send_key` moves the caret
/// synchronously and renders, so no per-step `wait_until` is needed.
///
/// The loop is bounded so a needle that never lines up (e.g. a genuine mapping
/// regression) fails fast with the screen for triage, instead of hanging an
/// indefinite wait until nextest's external 180s timeout (CONTRIBUTING.md §3
/// semantic waiting, §16 don't hide a wrong state behind an unbounded wait).
///
/// Replaces the old down-only `move_cursor_to_line`, which pressed Down forever
/// — hanging to the external timeout — whenever the target was mis-derived
/// above the cursor or the caret was already past it.
fn move_cursor_onto(harness: &mut EditorTestHarness, needle: &str) {
    // These diff buffers are a handful of lines; the cap is generous and only
    // reached on genuine failure. Each step is one synchronous key + render.
    const MAX_STEPS: usize = 300;
    for _ in 0..MAX_STEPS {
        match (status_line_number(harness), screen_row_of(harness, needle)) {
            (Some(current), Some(target)) if current == target => return,
            (Some(current), Some(target)) => {
                let key = if current < target {
                    KeyCode::Down
                } else {
                    KeyCode::Up
                };
                harness.send_key(key, KeyModifiers::NONE).unwrap();
            }
            // Needle not visible yet, or no `Ln` indicator this frame (the focus
            // repaint is still settling). Pump one tick and re-read without
            // moving, so we don't chase a target that hasn't rendered.
            _ => harness.tick_and_render().unwrap(),
        }
    }
    panic!(
        "could not land the diff cursor on a row rendering {:?} within {} steps \
         (caret at {:?}); screen:\n{}",
        needle,
        MAX_STEPS,
        status_line_number(harness),
        harness.screen_to_string()
    );
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

    // Jump to the hunk, then walk onto the green "+extra line" row. Wait for the
    // async focus repaint to first render the line; `move_cursor_onto` then
    // self-corrects onto it even if the panel is still settling.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("+extra line").unwrap();
    move_cursor_onto(&mut harness, "+extra line");

    // Start a visual selection and stage it.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    // The line-level selection must stage the line. Wait (indefinitely, per the
    // testing guidelines) for the async git apply + refresh to land it in the
    // index — this wait IS the assertion.
    harness
        .wait_until(|_| cached_diff(&repo).contains("+extra line"))
        .unwrap();
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
    // Wait for the async focus repaint to first render the line; the caret walk
    // then self-corrects onto it even if the panel is still settling.
    harness.wait_for_screen_contains("-beta").unwrap();
    move_cursor_onto(&mut harness, "-beta");
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

    // Wait for the whole -/+ modification pair to land in the index; the
    // compound condition is the assertion.
    harness
        .wait_until(|_| {
            let staged = cached_diff(&repo);
            staged.contains("+BETA") && staged.contains("-beta")
        })
        .unwrap();
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
    // Wait for the async focus repaint to first render the line; the caret walk
    // then self-corrects onto it even if the panel is still settling.
    harness.wait_for_screen_contains("+extra line").unwrap();
    move_cursor_onto(&mut harness, "+extra line");
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();

    // Wait for the discard to remove the added line from the working tree; the
    // worktree read is the assertion.
    harness
        .wait_until(|_| {
            let content = fs::read_to_string(repo.path.join("README.md")).unwrap_or_default();
            !content.contains("extra line")
        })
        .unwrap();
}

/// #2420 — after a line-level discard, the status bar must show the
/// *localized* confirmation ("Lines discarded"), not the raw i18n lookup
/// key. The emitter built the key by naively appending `d` to the action
/// name (`status.lines_${action}d`), which yields `status.lines_staged`
/// (correct) but `status.lines_discardd` (a typo with no translation), so
/// the untranslated key leaked verbatim into the status bar.
#[test]
fn test_review_visual_discard_status_is_localized() {
    init_tracing_from_env();
    let repo = repo_with_one_added_line();
    let mut harness = harness_for(&repo);
    open_review_diff(&mut harness);

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    // Wait for the async focus repaint to first render the line; the caret walk
    // then self-corrects onto it even if the panel is still settling.
    harness.wait_for_screen_contains("+extra line").unwrap();
    move_cursor_onto(&mut harness, "+extra line");
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();

    // The confirmation is shown transiently: applyLineSelection sets it,
    // then refreshMagitData's updateReviewStatus overwrites it with the
    // hunk-count summary. Poll every tick and record what the status bar
    // ever showed, so we deterministically catch the frame regardless of
    // timing. (This is exactly the window the bug report screenshotted.)
    let mut saw_localized = false;
    let mut saw_raw_key = false;
    let mut reverted = false;
    for _ in 0..120 {
        harness.tick_and_render().unwrap();
        let s = harness.screen_to_string();
        if s.contains("Lines discarded") {
            saw_localized = true;
        }
        // The buggy emitter leaked `status.lines_discardd` verbatim; guard
        // against any untranslated `status.lines_*` key reaching the screen.
        if s.contains("status.lines_") {
            saw_raw_key = true;
        }
        let content = fs::read_to_string(repo.path.join("README.md")).unwrap_or_default();
        if !content.contains("extra line") {
            reverted = true;
        }
        // Stop once the discard landed *and* we observed its confirmation.
        if reverted && saw_localized {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
        harness.advance_time(std::time::Duration::from_millis(20));
    }

    assert!(
        reverted,
        "line-level discard never reverted the working tree"
    );
    assert!(
        !saw_raw_key,
        "line-level discard must not leak a raw i18n key (e.g. \
         `status.lines_discardd`) into the status bar; last screen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        saw_localized,
        "line-level discard should show the localized confirmation \
         \"Lines discarded\"; last screen:\n{}",
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
    // Wait for the async focus repaint to first render the line; `move_cursor_onto`
    // then self-corrects onto it even while the panel is still reflowing, so a
    // mid-reflow frame can't strand the caret off `+ADD1`.
    harness.wait_for_screen_contains("+ADD1").unwrap();
    move_cursor_onto(&mut harness, "+ADD1");
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    // Wait for the selected line to be staged...
    harness
        .wait_until(|_| cached_diff(&repo).contains("+ADD1"))
        .unwrap();

    // ...then assert the *other* line did NOT get staged. This is a distinct
    // invariant the wait above can't cover, so it stays an explicit assert.
    let staged = cached_diff(&repo);
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
    // Focus mode only paints the focused (second) file's body, and after
    // `+ADDED_B` first appears the plugin can still reflow the panel. Wait for
    // the line to render, then `move_cursor_onto` self-corrects the caret onto
    // it across any remaining reflow — and panics with the screen if it can't,
    // so a mapping regression is a clear diagnostic rather than the opaque 180s
    // timeout this test used to hit (CONTRIBUTING.md §3/§16).
    harness.wait_for_screen_contains("+ADDED_B").unwrap();
    move_cursor_onto(&mut harness, "+ADDED_B");

    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    // Wait for the second (focused) file's line to land in the index; this wait
    // is the assertion.
    harness
        .wait_until(|_| cached_diff(&repo).contains("+ADDED_B"))
        .unwrap();
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
    // Wait for the async focus repaint to first render the line; the caret walk
    // then self-corrects onto it even if the panel is still settling.
    harness.wait_for_screen_contains("+extra line").unwrap();
    move_cursor_onto(&mut harness, "+extra line");
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();

    // Wait for the selected line to leave the index (unstaged); this wait is the
    // assertion.
    harness
        .wait_until(|_| !cached_diff(&repo).contains("+extra line"))
        .unwrap();
}
