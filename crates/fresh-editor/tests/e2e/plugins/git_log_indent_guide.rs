//! Regression test: indentation guides must not leak into the Git Log view.
//!
//! The Git Log command opens a magit-style buffer group — a commit list beside
//! a commit-detail diff — rendered as inner group-leaf panels with no
//! code-editing chrome. With `editor.indentation_guide = "all"` the global
//! setting used to apply to those panels too, painting a stray `▏` into column
//! 0 of the commit list and over the four-space-indented commit-message lines
//! in the diff. Indentation guides are a source-code editing aid; they don't
//! belong in a tool view. This opens the log with guides enabled and asserts no
//! guide glyph renders — independent of line numbers (a separate preference).

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, IndentationGuideMode};

// TODO: git command output differs on Windows; the other git_log tests skip it.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_view_does_not_render_indentation_guides() {
    let repo = GitTestRepo::new();
    // An indented source file: `git show`'s commit message is itself indented
    // four spaces, so the commit-detail panel carries column-0 guide bait
    // regardless of the diff body.
    repo.create_file("indented.rs", "fn main() {\n    let x = 1;\n}\n");
    repo.git_add(&["indented.rs"]);
    repo.git_commit("Add indented file");
    repo.setup_git_log_plugin();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, repo.path.clone()).unwrap();

    // Anchor on a real file tab, then open the full-repo Git Log.
    harness.open_file(&repo.path.join("indented.rs")).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Both panels have painted once the toolbar hint ("switch pane") and the
    // commit-detail diff (the selected commit's `Author:` line) are on screen.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane") && s.contains("Author:")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains('▏'),
        "Git Log panels must not render indentation guides (`▏`).\nScreen:\n{screen}"
    );
}

// Pressing Enter on a diff line opens that file at the commit version — a
// read-only *virtual* buffer showing real source. Virtual buffers default to
// no guides, but this one is code the user is reading, so git_log opts it back
// in via `createVirtualBuffer({ indentationGuide: true })`. The guides must
// render there (regression: they didn't, because the virtual default hid them).
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn git_log_file_opened_at_commit_shows_indentation_guides() {
    let repo = GitTestRepo::new();
    repo.create_file("indented.rs", "fn main() {\n    let x = 1;\n}\n");
    repo.git_add(&["indented.rs"]);
    repo.git_commit("Add indented file");
    repo.setup_git_log_plugin();

    let mut config = Config::default();
    config.editor.indentation_guide = IndentationGuideMode::All;

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, config, repo.path.clone()).unwrap();
    harness.open_file(&repo.path.join("indented.rs")).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Git Log").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Wait for the detail pane's diff body to finish rendering — not just the
    // commit list — before navigating. The `+++ b/indented.rs` header is emitted
    // by `git show` and is exactly the line git_log scans back for, so its
    // presence proves the diff the cursor is about to enter is on screen. Keying
    // only on the commit list (as before) let the `Down`/`Enter` keys race the
    // async `git show`, landing the cursor on unloaded content.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("switch pane")
                && s.contains("Add indented file")
                && s.contains("+++ b/indented.rs")
        })
        .unwrap();

    // Focus the detail pane, then move down into the diff body so the cursor
    // sits inside indented.rs's `+++ b/…` section (git_log derives the file to
    // open by scanning backward from the cursor for that header).
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    for _ in 0..40 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the file-at-commit view itself, not merely a code line that also
    // appears in the diff. git_log titles that tab `*<hash>:indented.rs*`, so the
    // `:indented.rs` marker is unique to the opened view — the diff pane shows
    // `b/indented.rs`, never `:indented.rs`. The old wait keyed on `let x = 1`,
    // which the diff pane already renders as `+    let x = 1;`; it could pass
    // without the view ever opening and then fail the guide assertion, because
    // the git-log diff pane intentionally renders no guides.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains(":indented.rs") && s.contains("let x = 1")
        })
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('▏'),
        "a file opened at a commit version should render indentation guides (`▏`).\nScreen:\n{screen}"
    );
}
