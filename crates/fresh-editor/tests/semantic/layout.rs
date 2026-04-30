//! Phase 3 — layout theorems.
//!
//! These tests assert on viewport state after a render pass. The
//! current surface only exposes `viewport_top_byte`; the real
//! issue-#1147-style rewrites that need cursor-screen-position and
//! per-row visible-byte mappings are deferred until the
//! `RenderSnapshot` design (§9.1 of the migration doc) lands.
//!
//! What this file demonstrates:
//! - `LayoutTheorem` runs a render pass, exposes `viewport_top_byte`,
//!   and asserts on it without screen scraping.
//! - The rest of the test stays declarative — no `harness.send_key`,
//!   no `crossterm::KeyCode`, no manual render loops.

use crate::common::theorem::layout_theorem::{assert_layout_theorem, LayoutTheorem};
use fresh::test_api::Action;

#[test]
fn theorem_freshly_loaded_buffer_has_top_byte_zero() {
    // Trivial baseline: after loading text and rendering, the viewport
    // has not scrolled. Real Class B coverage (issue #1147 etc.) needs
    // the richer RenderSnapshot observables.
    assert_layout_theorem(LayoutTheorem {
        description: "load + render leaves viewport at top of buffer",
        initial_text: "alpha\nbravo\ncharlie\n",
        width: 80,
        height: 24,
        actions: vec![],
        expected_top_byte: 0,
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
    let big_buffer: &'static str = Box::leak(lines.join("\n").into_boxed_str());

    assert_layout_theorem(LayoutTheorem {
        description: "MoveDocumentEnd then MoveDocumentStart returns viewport to top_byte=0",
        initial_text: big_buffer,
        width: 40,
        height: 10,
        actions: vec![Action::MoveDocumentEnd, Action::MoveDocumentStart],
        expected_top_byte: 0,
    });
}
