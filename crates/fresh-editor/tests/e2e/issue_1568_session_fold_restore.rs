//! E2E test for issue #1568: Loading a file with folded code from the
//! previous session, but after the file was modified externally, the folds
//! are restored at the wrong line numbers.
//!
//! Repro (reproduced manually in tmux):
//! 1. Open `fold.rs` (two functions).
//! 2. Move the cursor into `fn helper()` (line 7 originally), fold it.
//! 3. Quit the editor cleanly (Ctrl+Q) so the session persists.
//! 4. Externally edit the file to prepend a new line (e.g. a comment),
//!    shifting every existing line down by one. `fn helper()` is now on
//!    line 8.
//! 5. Reopen the editor on the same file. The session restore picks up
//!    the fold at its stored `header_line == 7`, but line 7 in the new
//!    file is the blank line *between* the two functions. Result: the
//!    fold header sits on a line that shouldn't be folded, and the
//!    folded body covers the wrong lines (the top of `fn helper()`
//!    instead of its contents).
//!
//! Expected: either the fold is dropped (safest), or it is reattached to
//! `fn helper()` at its new position. Either way, the fold header must
//! NOT land on an unrelated line whose content doesn't look foldable.
//!
//! This test uses the existing session-restore harness (`DirectoryContext`
//! + `hot_exit = true`) to cover the external-edit case deterministically.
//!
//! <https://github.com/sinelaw/fresh/issues/1568>

use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

fn find_collapsed_fold_rows(harness: &EditorTestHarness) -> Vec<usize> {
    let (start, end) = harness.content_area_rows();
    let mut rows = Vec::new();
    for row in start..=end {
        if harness
            .get_cell(0, row as u16)
            .as_deref()
            .map(|s| s == "▸")
            .unwrap_or(false)
        {
            rows.push(row);
        }
    }
    rows
}

fn row_contains(harness: &EditorTestHarness, row: usize, needle: &str) -> bool {
    harness.get_row_text(row as u16).contains(needle)
}

#[test]
fn test_fold_restore_after_external_edit_does_not_land_on_unrelated_line() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("fold.rs");
    let original = "\
fn main() {
    let x = 1;
    let y = 2;
    let z = 3;
}

fn helper() {
    println!(\"hello\");
    println!(\"world\");
}
";
    std::fs::write(&file_path, original).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // --- Session 1: open file, fold `fn helper()`, shut down cleanly.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Collapse the `fn helper()` block (line 6, 0-indexed) via the
        // public toggle API — the same code path Toggle Fold invokes.
        let buffer_id = harness.editor().active_buffer();
        harness.editor_mut().toggle_fold_at_line(buffer_id, 6);
        harness.render().unwrap();

        // Sanity: `fn helper()` is now collapsed — a `▸` indicator
        // appears next to it.
        let rows = find_collapsed_fold_rows(&harness);
        assert_eq!(
            rows.len(),
            1,
            "Precondition: exactly one collapsed fold should exist. \
             Screen:\n{}",
            harness.screen_to_string()
        );
        let row = rows[0];
        assert!(
            row_contains(&harness, row, "fn helper()"),
            "Precondition: the collapsed fold should be on the \
             `fn helper()` header. Row {} text: {:?}",
            row,
            harness.get_row_text(row as u16)
        );

        harness.shutdown(true).unwrap();
    }

    // --- External edit between sessions: prepend a new line, shifting
    //     every line down by one. `fn helper()` is now on line 8 (1-indexed).
    let modified = format!("// new line added!\n{}", original);
    std::fs::write(&file_path, &modified).unwrap();

    // --- Session 2: restore and check where the fold lands.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let _ = harness.startup(true, &[]).unwrap();
        // Re-open the file to ensure the buffer is materialized, in case
        // the session only remembered its presence.
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Any collapsed fold must be on a line whose text plausibly
        // corresponds to a foldable construct — specifically, on
        // `fn helper()`.
        //
        // Bug: the fold is restored at its stored line 7 (now the
        // blank line between the two functions), so a `▸` sits next to
        // empty content and `println!("hello")` is hidden under a stale
        // placeholder.
        let rows = find_collapsed_fold_rows(&harness);
        let screen = harness.screen_to_string();
        assert!(
            !rows.is_empty(),
            "Issue #1568: After session restore the fold on fn helper \
             should either be preserved (on its new line) or discarded. \
             Seeing no fold indicators at all is also a failure mode \
             worth flagging here. Screen:\n{}",
            screen
        );

        for row in &rows {
            let text = harness.get_row_text(*row as u16);
            assert!(
                text.contains("fn ") || text.contains("pub fn"),
                "Issue #1568: Restored fold at content row {} lands on \
                 text {:?}, which isn't a function header. The fold's \
                 header_line reference wasn't reconciled with the external \
                 edit that added a line at the top of the file. \
                 Screen:\n{}",
                row,
                text,
                screen
            );
        }

        // Stronger assertion: the stale line number (index 6 in the new
        // file, which is now the blank line between functions) must NOT
        // have the collapsed marker.
        let stale_row = layout::CONTENT_START_ROW + 6;
        let cell = harness.get_cell(0, stale_row as u16);
        assert!(
            cell.as_deref() != Some("▸"),
            "Issue #1568: A collapsed fold marker landed on content row \
             {} which, after the external edit, shows: {:?}. This is the \
             blank separator line between the two functions, not the \
             fn helper header. Screen:\n{}",
            stale_row,
            harness.get_row_text(stale_row as u16),
            screen
        );
    }
}
