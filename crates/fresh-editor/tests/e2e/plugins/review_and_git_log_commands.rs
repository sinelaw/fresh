//! Functional coverage for the two command families in the palette: every
//! command is invoked *by name* from the palette, and the assertion is on what
//! it did — a panel that closed, a change that appeared after a refresh, a
//! comment that landed in the diff, a file that was written.
//!
//! `Review Diff`, `Review Diff: Range` and `Review Diff: Stash` open reviews
//! and are covered by `audit_mode.rs`, `review_diff_ux_bugs.rs` and
//! `review_diff_hunk_parity.rs`; `Git Log`, `Git Log: Current File` and
//! `Git Log: PR Branch` likewise by `git_log_*.rs` and `audit_mode.rs`. What
//! was missing, and is here, is the rest of both families: the commands that
//! stop, refresh, comment on, and export a review, and the ones that close and
//! refresh a log.

use crate::common::git_test_helper::{git_command, GitTestRepo};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::Path;

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

fn run_git(repo: &GitTestRepo, args: &[&str]) {
    let status = git_command(&repo.path)
        .args(args)
        .status()
        .expect("git should run");
    assert!(status.success(), "git {args:?} failed");
}

fn harness_for(repo: &GitTestRepo) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();
    harness
}

/// A repo with one commit and one working-tree modification, so a review has
/// something to show.
fn repo_with_a_change() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("src/main.rs", "fn main() {\n    println!(\"hello\");\n}\n");
    repo.git_add_all();
    repo.git_commit("initial commit");
    repo.create_file(
        "src/main.rs",
        "fn main() {\n    println!(\"hello world\");\n}\n",
    );
    repo
}

/// Open a working-tree review and wait for it to finish loading.
fn open_review(harness: &mut EditorTestHarness) {
    harness.run_palette_command("Review Diff").unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("UNSTAGED") && s.contains("main.rs")
        })
        .unwrap();
}

// ---------------------------------------------------------------------------
// Review Diff: Stop / Refresh
// ---------------------------------------------------------------------------

/// `Review Diff: Stop` ends the session: the review's panels go away.
#[test]
fn review_diff_stop_command_closes_the_review() {
    init_tracing_from_env();
    let repo = repo_with_a_change();
    let mut harness = harness_for(&repo);

    open_review(&mut harness);

    harness.run_palette_command("Review Diff: Stop").unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("UNSTAGED"))
        .unwrap();
}

/// `Review Diff: Refresh` re-reads the working tree: a file created after the
/// review opened shows up in it.
#[test]
fn review_diff_refresh_command_picks_up_a_later_change() {
    init_tracing_from_env();
    let repo = repo_with_a_change();
    let mut harness = harness_for(&repo);

    open_review(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("late_addition.rs"),
        "the file must not be in the review before it exists. Screen:\n{screen}"
    );

    repo.create_file("src/late_addition.rs", "pub fn added_later() {}\n");

    harness.run_palette_command("Review Diff: Refresh").unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("late_addition.rs"))
        .unwrap();
}

// ---------------------------------------------------------------------------
// Review Diff: Add Comment / Edit Note
// ---------------------------------------------------------------------------

/// `Review Diff: Add Comment` attaches a comment to the cursor's diff line,
/// and the review renders it.
#[test]
fn review_diff_add_comment_command_puts_the_comment_in_the_review() {
    init_tracing_from_env();
    let repo = repo_with_a_change();
    let mut harness = harness_for(&repo);

    open_review(&mut harness);

    // `n` lands the cursor on the first hunk; a comment needs a diff line.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .run_palette_command("Review Diff: Add Comment")
        .unwrap();
    // The comment prompt names the line it will attach to.
    harness
        .wait_until(|h| h.screen_to_string().contains("Comment on"))
        .unwrap();

    harness.type_text("NEEDS_A_TEST").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("NEEDS_A_TEST"))
        .unwrap();
}

/// `Review Diff: Edit Note` records the session note, which the Markdown
/// export then carries under its own heading.
#[test]
fn review_diff_edit_note_command_records_the_note() {
    init_tracing_from_env();
    let repo = repo_with_a_change();
    let mut harness = harness_for(&repo);

    open_review(&mut harness);

    harness
        .run_palette_command("Review Diff: Edit Note")
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Note:"))
        .unwrap();
    harness.type_text("SESSION_NOTE_TEXT").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .run_palette_command("Review Diff: Export to Markdown")
        .unwrap();

    let exported = wait_for_export(&mut harness, &repo.path.join(".review").join("session.md"));
    assert!(
        exported.contains("SESSION_NOTE_TEXT"),
        "the exported session should carry the note. Exported:\n{exported}"
    );
}

/// Put a comment on the first hunk, through the palette command that does it.
fn add_comment(harness: &mut EditorTestHarness, text: &str) {
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .run_palette_command("Review Diff: Add Comment")
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Comment on"))
        .unwrap();
    harness.type_text(text).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(text))
        .unwrap();
}

// ---------------------------------------------------------------------------
// Review Diff: Export to Markdown / JSON
// ---------------------------------------------------------------------------

/// `Review Diff: Export to Markdown` writes the review out to
/// `.review/session.md`, carrying the comments it holds.
#[test]
fn review_diff_export_to_markdown_command_writes_the_session_file() {
    init_tracing_from_env();
    let repo = repo_with_a_change();
    let mut harness = harness_for(&repo);

    open_review(&mut harness);
    add_comment(&mut harness, "MARKDOWN_EXPORT_COMMENT");

    harness
        .run_palette_command("Review Diff: Export to Markdown")
        .unwrap();

    let exported = wait_for_export(&mut harness, &repo.path.join(".review").join("session.md"));
    assert!(
        exported.contains("# Code Review Session"),
        "the export should be the review session document. Exported:\n{exported}"
    );
    assert!(
        exported.contains("main.rs"),
        "the export should name the commented file. Exported:\n{exported}"
    );
    assert!(
        exported.contains("MARKDOWN_EXPORT_COMMENT"),
        "the export should carry the comment. Exported:\n{exported}"
    );
}

/// `Review Diff: Export to JSON` writes the same session as machine-readable
/// JSON, with the comments in it.
#[test]
fn review_diff_export_to_json_command_writes_the_session_file() {
    init_tracing_from_env();
    let repo = repo_with_a_change();
    let mut harness = harness_for(&repo);

    open_review(&mut harness);
    add_comment(&mut harness, "JSON_EXPORT_COMMENT");

    harness
        .run_palette_command("Review Diff: Export to JSON")
        .unwrap();

    let exported = wait_for_export(
        &mut harness,
        &repo.path.join(".review").join("session.json"),
    );
    let parsed: serde_json::Value =
        serde_json::from_str(&exported).expect("the JSON export should parse");
    let comments = parsed["comments"].as_array().unwrap_or_else(|| {
        panic!("the export should carry a comments array. Exported:\n{exported}")
    });
    assert!(
        comments.iter().any(|c| c["text"] == "JSON_EXPORT_COMMENT"
            && c["file"].as_str().is_some_and(|f| f.contains("main.rs"))),
        "the exported comment should name its text and file. Exported:\n{exported}"
    );
}

/// Wait for an export command to report on screen that it wrote the session,
/// then return what it wrote. The status line is the rendered evidence the
/// command ran; the file is the artifact it exists to produce.
fn wait_for_export(harness: &mut EditorTestHarness, path: &Path) -> String {
    harness
        .wait_until(|h| h.screen_to_string().contains("Review exported to"))
        .unwrap();
    fs::read_to_string(path).unwrap_or_else(|e| {
        panic!(
            "the export reported success, so {} should be readable: {e}",
            path.display()
        )
    })
}

// ---------------------------------------------------------------------------
// Git Log: Close / Refresh
// ---------------------------------------------------------------------------

fn repo_with_history() -> GitTestRepo {
    let repo = GitTestRepo::new();
    repo.setup_git_log_plugin();
    repo.create_file("a.txt", "one\n");
    repo.git_add_all();
    repo.git_commit("FIRST_COMMIT_SUBJECT");
    repo
}

/// `Git Log: Close` closes the log panel.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_close_command_closes_the_log() {
    init_tracing_from_env();
    let repo = repo_with_history();
    let mut harness = harness_for(&repo);

    harness.run_palette_command("Git Log").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("FIRST_COMMIT_SUBJECT"))
        .unwrap();

    harness.run_palette_command("Git Log: Close").unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("FIRST_COMMIT_SUBJECT"))
        .unwrap();
}

/// `Git Log: Refresh` re-reads the history: a commit made after the log opened
/// shows up in it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_refresh_command_picks_up_a_later_commit() {
    init_tracing_from_env();
    let repo = repo_with_history();
    let mut harness = harness_for(&repo);

    harness.run_palette_command("Git Log").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("FIRST_COMMIT_SUBJECT"))
        .unwrap();

    repo.create_file("b.txt", "two\n");
    repo.git_add_all();
    repo.git_commit("SECOND_COMMIT_SUBJECT");

    harness.run_palette_command("Git Log: Refresh").unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("SECOND_COMMIT_SUBJECT"))
        .unwrap();
}

// ---------------------------------------------------------------------------
// Git Log: Close PR Branch / Refresh PR Branch
// ---------------------------------------------------------------------------

/// A repo whose HEAD branch is ahead of `master` by one commit, so the branch
/// log has commits to list.
fn repo_with_a_feature_branch() -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    repo.create_file("a.txt", "base\n");
    repo.git_add_all();
    repo.git_commit("base commit");
    run_git(&repo, &["branch", "-M", "master"]);
    run_git(&repo, &["checkout", "-b", "feature"]);
    repo.create_file("a.txt", "base\nfeature\n");
    repo.git_add_all();
    repo.git_commit("BRANCH_COMMIT_SUBJECT");
    repo
}

/// Open the branch log against the default base ref and wait for its commits.
fn open_branch_log(harness: &mut EditorTestHarness) {
    harness.run_palette_command("Git Log: PR Branch").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Base ref"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("BRANCH_COMMIT_SUBJECT"))
        .unwrap();
}

/// `Git Log: Close PR Branch` closes the branch log panel, and says so.
///
/// The report is part of the behaviour: `editor.t()` answers a missing string
/// with the key itself, so the plugin's `editor.t(key) || fallback` idiom never
/// reached its fallback and closing the panel used to print a literal
/// `status.closed` where the message belongs.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_close_pr_branch_command_closes_the_branch_log() {
    init_tracing_from_env();
    let repo = repo_with_a_feature_branch();
    let mut harness = harness_for(&repo);

    open_branch_log(&mut harness);

    harness
        .run_palette_command("Git Log: Close PR Branch")
        .unwrap();

    harness
        .wait_until(|h| !h.screen_to_string().contains("Commits (master..HEAD)"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Branch log closed"),
        "closing should report what it did. Screen:\n{screen}"
    );
    assert_no_raw_i18n_key(&screen);
}

/// `Git Log: PR Branch` on an open branch log says it is already open rather
/// than prompting for a base ref a second time.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_pr_branch_command_declines_to_open_twice() {
    init_tracing_from_env();
    let repo = repo_with_a_feature_branch();
    let mut harness = harness_for(&repo);

    open_branch_log(&mut harness);

    harness.run_palette_command("Git Log: PR Branch").unwrap();

    // The palette closes either way; reading the status only once it has is
    // what makes a regression fail here instead of waiting forever.
    harness
        .wait_until(|h| !h.screen_to_string().contains(">command"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Branch log already open"),
        "the second invocation should say the log is already open. Screen:\n{screen}"
    );
    assert!(
        !screen.contains("Base ref"),
        "the second invocation should not re-prompt. Screen:\n{screen}"
    );
    assert_no_raw_i18n_key(&screen);
}

/// Escaping the base-ref prompt leaves no branch log open, and reports the
/// cancellation.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_pr_branch_command_cancels_at_the_base_ref_prompt() {
    init_tracing_from_env();
    let repo = repo_with_a_feature_branch();
    let mut harness = harness_for(&repo);

    harness.run_palette_command("Git Log: PR Branch").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Base ref"))
        .unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // The prompt goes away either way; assert on what it left behind.
    harness
        .wait_until(|h| !h.screen_to_string().contains("Base ref"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Cancelled"),
        "escaping the prompt should report the cancellation. Screen:\n{screen}"
    );
    assert!(
        !screen.contains("BRANCH_COMMIT_SUBJECT"),
        "cancelling should not open the branch log. Screen:\n{screen}"
    );
    assert_no_raw_i18n_key(&screen);
}

/// The status line should carry a message, never the i18n key that names one.
fn assert_no_raw_i18n_key(screen: &str) {
    for key in ["status.", "panel.", "prompt.", "cmd."] {
        assert!(
            !screen.contains(key),
            "a raw `{key}` i18n key reached the screen. Screen:\n{screen}"
        );
    }
}

/// `Git Log: Refresh PR Branch` re-fetches the commit list: a commit made on
/// the branch after the panel opened shows up in it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_refresh_pr_branch_command_picks_up_a_later_commit() {
    init_tracing_from_env();
    let repo = repo_with_a_feature_branch();
    let mut harness = harness_for(&repo);

    open_branch_log(&mut harness);

    repo.create_file("a.txt", "base\nfeature\nmore\n");
    repo.git_add_all();
    repo.git_commit("LATER_BRANCH_COMMIT");

    harness
        .run_palette_command("Git Log: Refresh PR Branch")
        .unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("LATER_BRANCH_COMMIT"))
        .unwrap();
}
