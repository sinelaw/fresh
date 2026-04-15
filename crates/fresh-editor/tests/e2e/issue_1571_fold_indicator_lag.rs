//! E2E tests for issue #1571: Fold indicators update positions are updated
//! with a laggy delay after inserting/removing lines.
//!
//! From the issue:
//! > Fold indicator should be refactored to use the overlay system to avoid
//! > this issue
//!
//! Repro scenario (manual, reproduced in tmux):
//! 1. Open a Rust file with foldable functions.
//! 2. LSP (or similar) publishes folding ranges; a `▾` indicator appears in
//!    the gutter next to `fn main() { ... }`.
//! 3. Press Enter at the beginning of the file to insert a blank line.
//!    The `fn main()` text shifts down by one line.
//! 4. The fold indicator `▾` stays anchored at its old line number (now empty)
//!    until the LSP re-publishes updated folding ranges — which might never
//!    happen until the next full reparse.
//!
//! Expected: the fold indicator tracks the buffer's current content, either by
//! having the fold-range data structure track edits (overlay system) or by
//! some other mechanism. After inserting a line before the fold, the `▾`
//! should be on the new line of `fn main()`, not on the empty line.
//!
//! Current (buggy) behavior: the fold indicator is rendered using the raw
//! `folding_ranges` line numbers from the last LSP response, without
//! adjusting for subsequent edits. This leaves the indicator visually
//! desynchronized from the code it's supposed to mark.
//!
//! <https://github.com/sinelaw/fresh/issues/1571>

use crate::common::harness::{layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use lsp_types::FoldingRange;
use tempfile::TempDir;

/// Install a fold range covering lines `start_line..=end_line`.
fn set_fold_range(harness: &mut EditorTestHarness, start_line: usize, end_line: usize) {
    let state = harness.editor_mut().active_state_mut();
    let ranges = vec![FoldingRange {
        start_line: start_line as u32,
        end_line: end_line as u32,
        start_character: None,
        end_character: None,
        kind: None,
        collapsed_text: None,
    }];
    state
        .folding_ranges
        .set_from_lsp(&state.buffer, &mut state.marker_list, ranges);
}

/// Return the leftmost non-empty gutter cell at the given content row,
/// ignoring the line-number area. Indicator chars include `▾` (expanded)
/// and `▸` (collapsed).
fn gutter_indicator_at(harness: &EditorTestHarness, content_line: usize) -> Option<String> {
    let row = (layout::CONTENT_START_ROW + content_line) as u16;
    let cell = harness.get_cell(0, row);
    cell.filter(|s| s == "▾" || s == "▸")
}

/// The text on a content-area row.
fn row_text(harness: &EditorTestHarness, content_line: usize) -> String {
    let row = (layout::CONTENT_START_ROW + content_line) as u16;
    harness.get_row_text(row)
}

/// Issue #1571: Inserting a line before a fold should move the fold
/// indicator to the new location of the fold, not leave it stranded on
/// an empty line (or a different line) until a fresh LSP response arrives.
#[test]
fn test_fold_indicator_follows_insert_before_fold() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("fold_lag.rs");
    let content = "fn main() {\n    let x = 1;\n    let y = 2;\n    let z = 3;\n}\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Simulate LSP publishing folding ranges for `fn main() { ... }`
    // (line 0 header ..= line 4 close brace).
    set_fold_range(&mut harness, 0, 4);
    harness.render().unwrap();

    // Baseline: fold indicator sits on the first content line (the `fn main()` header).
    assert_eq!(
        gutter_indicator_at(&harness, 0),
        Some("▾".to_string()),
        "Baseline: fold indicator should be on line 1 (fn main header). \
         Screen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        row_text(&harness, 0).contains("fn main()"),
        "Baseline: first content row should show `fn main()`"
    );

    // Insert a blank line at the very beginning of the buffer, pushing
    // `fn main()` from line 0 to line 1.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // `fn main()` is now on content row 1.
    assert!(
        row_text(&harness, 1).contains("fn main()"),
        "After insert: `fn main()` should be on content row index 1. \
         Screen:\n{}",
        harness.screen_to_string()
    );

    // The fold range should now cover `fn main()`'s new location. We
    // exercise this end-to-end by clicking the gutter on the supposed
    // fold-header row and checking that fn main's body collapses.
    //
    // Before the fix (issue #1571): clicking on content row 1 does not
    // collapse anything (the stale fold at start_line=0 is mis-targeted).
    //
    // After the fix: content row 1 is the fold header and clicking it
    // hides `let x = 1;` / `let y = 2;` / `let z = 3;`.
    let fold_header_row = (layout::CONTENT_START_ROW + 1) as u16;
    harness.mouse_click(0, fold_header_row).unwrap();
    harness.render().unwrap();

    let screen_after_click = harness.screen_to_string();
    assert!(
        !screen_after_click.contains("let x = 1;"),
        "Issue #1571: Clicking the gutter on the fold header row \
         (content row 1, where fn main() now lives) should collapse the \
         function body and hide `let x = 1;`. The fold range is still \
         pinned to its old line numbers. Screen:\n{}",
        screen_after_click
    );
    assert!(
        !screen_after_click.contains("let y = 2;"),
        "Body line `let y = 2;` should be hidden by the collapsed fold. \
         Screen:\n{}",
        screen_after_click
    );
    assert!(
        !screen_after_click.contains("let z = 3;"),
        "Body line `let z = 3;` should be hidden by the collapsed fold. \
         Screen:\n{}",
        screen_after_click
    );
    // fn main header should still be visible.
    assert!(
        screen_after_click.contains("fn main()"),
        "fn main() header should still be visible after collapsing. \
         Screen:\n{}",
        screen_after_click
    );
}

/// Issue #1571: Deleting a line before a fold should also update the
/// indicator synchronously.
#[test]
fn test_fold_indicator_follows_delete_before_fold() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("fold_lag_del.rs");
    // An extra blank line at the start, then fn main() starting on line 1.
    let content = "\nfn main() {\n    let x = 1;\n    let y = 2;\n    let z = 3;\n}\n";
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Fold covers `fn main() { ... }` which starts on line 1 in the file.
    set_fold_range(&mut harness, 1, 5);
    harness.render().unwrap();

    // Baseline: indicator on content row 1.
    assert_eq!(
        gutter_indicator_at(&harness, 1),
        Some("▾".to_string()),
        "Baseline: fold indicator should be on content row 1. Screen:\n{}",
        harness.screen_to_string()
    );
    assert!(row_text(&harness, 1).contains("fn main()"));

    // Put the cursor at the start of the buffer (on the blank line),
    // then delete the blank line with a forward delete.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Delete, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // `fn main()` is now on content row 0.
    assert!(
        row_text(&harness, 0).contains("fn main()"),
        "After delete: `fn main()` should be on content row 0. Screen:\n{}",
        harness.screen_to_string()
    );

    // The fold range should track the shift. Clicking the gutter on
    // content row 0 (the new home of fn main) must collapse its body.
    //
    // Before the fix (issue #1571): clicking row 0 doesn't collapse
    // anything because the stored fold still starts at line 1 (now the
    // body line `let x = 1;`), and clicking on row 1 would incorrectly
    // collapse a "fold" whose header is a body line.
    //
    // After the fix: the fold marker has moved with the buffer content.
    let fold_header_row = layout::CONTENT_START_ROW as u16;
    harness.mouse_click(0, fold_header_row).unwrap();
    harness.render().unwrap();

    let screen_after_click = harness.screen_to_string();
    assert!(
        !screen_after_click.contains("let x = 1;"),
        "Issue #1571: After deleting the blank line above and clicking \
         the gutter on fn main's new row, `let x = 1;` should be hidden. \
         Screen:\n{}",
        screen_after_click
    );
    assert!(
        !screen_after_click.contains("let y = 2;"),
        "Body line `let y = 2;` should be hidden by the collapsed fold. \
         Screen:\n{}",
        screen_after_click
    );
    assert!(
        screen_after_click.contains("fn main()"),
        "fn main() header should still be visible. Screen:\n{}",
        screen_after_click
    );
}
