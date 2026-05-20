//! Phase 3 — layout theorems.
//!
//! These tests assert on viewport state after a render pass. The
//! current surface only exposes `viewport_top_byte`; the real
//! issue-#1147-style rewrites that need cursor-screen-position and
//! per-row visible-byte mappings are deferred until the
//! `RenderSnapshot` design (§9.1 of the migration doc) lands.
//!
//! What this file demonstrates:
//! - `LayoutScenario` runs a render pass, exposes `viewport_top_byte`,
//!   and asserts on it without screen scraping.
//! - The rest of the test stays declarative — no `harness.send_key`,
//!   no `crossterm::KeyCode`, no manual render loops.

use crate::common::scenario::layout_scenario::{assert_layout_scenario, LayoutScenario};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;
use fresh::test_api::Action;

// Vertical / line-end movement is layout-dependent: it resolves against
// the rendered line structure, so it only works once a render has run.
// These moved here from the BufferScenario corpus, where they silently
// no-op'd (no render → cursor never moved). LayoutScenario renders, so
// they resolve correctly. (`cursor_byte` is the layout-resolved
// observable; LayoutScenario can't yet assert selection *text*.)

#[test]
fn theorem_move_down_then_line_end_reaches_end_of_second_line() {
    assert_layout_scenario(LayoutScenario {
        description: "MoveDown then MoveLineEnd lands at end of the second line".into(),
        initial_text: "ab\ncde".into(),
        width: 80,
        height: 24,
        actions: vec![Action::MoveDown, Action::MoveLineEnd],
        expected_snapshot: RenderSnapshotExpect {
            cursor_byte: Some(6),
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn theorem_select_line_end_reaches_first_line_end() {
    assert_layout_scenario(LayoutScenario {
        description: "SelectLineEnd moves the caret to the end of the first line".into(),
        initial_text: "hello\nworld".into(),
        width: 80,
        height: 24,
        actions: vec![Action::SelectLineEnd],
        expected_snapshot: RenderSnapshotExpect {
            cursor_byte: Some(5),
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn theorem_freshly_loaded_buffer_has_top_byte_zero() {
    // Trivial baseline: after loading text and rendering, the viewport
    // has not scrolled. Real Class B coverage (issue #1147 etc.) needs
    // the richer RenderSnapshot observables.
    assert_layout_scenario(LayoutScenario {
        description: "load + render leaves viewport at top of buffer".into(),
        initial_text: "alpha\nbravo\ncharlie\n".into(),
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: Some(0),
        ..Default::default()
    });
}

#[test]
fn theorem_move_document_start_resets_viewport() {
    // Long buffer + tight viewport ⇒ MoveDocumentEnd should scroll
    // somewhere; MoveDocumentStart from there returns top_byte to 0.
    // The intermediate scroll position is intentionally not asserted —
    // doing that precisely needs cursor-position observables, which
    // are out of scope until RenderSnapshot.
    let lines: Vec<String> = (0..50).map(|i| format!("line {i:02}")).collect();
    let big_buffer = lines.join("\n");

    assert_layout_scenario(LayoutScenario {
        description: "MoveDocumentEnd then MoveDocumentStart returns viewport to top_byte=0".into(),
        initial_text: big_buffer,
        width: 40,
        height: 10,
        actions: vec![Action::MoveDocumentEnd, Action::MoveDocumentStart],
        expected_top_byte: Some(0),
        ..Default::default()
    });
}
