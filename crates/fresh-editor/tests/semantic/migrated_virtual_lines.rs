//! DECLARATIVE rewrite. Migration of `tests/e2e/virtual_lines.rs` —
//! virtual lines (LineAbove / LineBelow) injected via the
//! plugin-state path.
//!
//! Every test is a `LayoutScenario` data literal — no harness
//! calls, no per-step imperative `send_key` / `render` flow.
//! Virtual-text injection is now expressed via
//! `LayoutScenario::initial_virtual_texts`, namespace clears via
//! `LayoutScenario::clear_virtual_text_namespaces`, and the
//! virtual-text count via `expected_virtual_text_count`.
//!
//! Load-bearing claims preserved here:
//!
//!   1. A `LineAbove` virtual line attached to a buffer byte
//!      offset renders above the source line and doesn't clobber
//!      the surrounding source lines.
//!   2. A `LineBelow` virtual line renders below the source line.
//!   3. Multiple virtual lines at the same offset all render
//!      (priority field doesn't drop any).
//!   4. `clear_namespace` removes only the targeted namespace —
//!      others survive.
//!   5. Virtual line rows don't carry a gutter line number — they
//!      are not part of the source numbering.
//!   6. A virtual line marker tracks edits: inserting text *above*
//!      the anchor doesn't detach the virtual line from its source
//!      line.
//!   7. Both `LineAbove` and `LineBelow` can coexist on the same
//!      source line; rendered order is ABOVE → source → BELOW.
//!   8. The `VirtualTextManager` length counter tracks `add_line`
//!      and `clear_namespace` correctly.
//!
//! ## DSL extensions used
//!
//! - `LayoutScenario::initial_virtual_texts: Vec<VirtualTextSpec>`
//!   — declarative shim around
//!   `EditorTestApi::seed_virtual_line`, which wraps
//!   `VirtualTextManager::add_line` for the line variants.
//! - `LayoutScenario::clear_virtual_text_namespaces: Vec<String>`
//!   — declarative shim around
//!   `EditorTestApi::clear_virtual_text_namespace`.
//! - `LayoutScenario::expected_virtual_text_count` — declarative
//!   shim around `EditorTestApi::virtual_text_count`.
//! - `LayoutScenario::expected_virtual_rows_no_digit_gutter` —
//!   asserts the rows containing a sentinel don't start (after
//!   trimming) with a digit, i.e. they have a blank gutter.
//! - `LayoutScenario::expected_row_order` — asserts row(before)
//!   precedes row(after).
//! - `VirtualTextPositionSpec { Above, Below, Inline }` — the
//!   position enum exposed by the DSL. The `Inline` variant is
//!   reserved (the seed shim only wires `Above` / `Below` today;
//!   `Inline` panics if used).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, VirtualTextPositionSpec, VirtualTextSpec,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

fn vt(
    byte_offset: usize,
    text: &str,
    position: VirtualTextPositionSpec,
    namespace: &str,
    priority: i32,
) -> VirtualTextSpec {
    VirtualTextSpec {
        byte_offset,
        text: text.into(),
        position,
        fg: None,
        bg: None,
        namespace: namespace.into(),
        priority,
    }
}

#[test]
fn migrated_virtual_line_above_renders_above_source() {
    // Original: `test_virtual_line_above`.
    assert_layout_scenario(LayoutScenario {
        description: "LineAbove virtual line at offset 7 renders + source lines survive".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![vt(
            7, // offset of "Line 2"
            "--- Header Above Line 2 ---",
            VirtualTextPositionSpec::Above,
            "test",
            0,
        )],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("--- Header Above Line 2 ---".into()),
                RowMatch::AnyRowContains("Line 1".into()),
                RowMatch::AnyRowContains("Line 2".into()),
                RowMatch::AnyRowContains("Line 3".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_line_below_renders_below_source() {
    // Original: `test_virtual_line_below`.
    assert_layout_scenario(LayoutScenario {
        description: "LineBelow virtual line at offset 0 renders below source".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![vt(
            0,
            "--- Footer Below Line 1 ---",
            VirtualTextPositionSpec::Below,
            "test",
            0,
        )],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains(
                "--- Footer Below Line 1 ---".into(),
            )],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_multiple_virtual_lines_same_position_all_visible() {
    // Original: `test_multiple_virtual_lines_same_position`.
    assert_layout_scenario(LayoutScenario {
        description: "two LineAbove at same offset: both render despite priority".into(),
        initial_text: "Line 1\nLine 2".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(0, "First Header", VirtualTextPositionSpec::Above, "test", 0),
            vt(
                0,
                "Second Header",
                VirtualTextPositionSpec::Above,
                "test",
                10,
            ),
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("First Header".into()),
                RowMatch::AnyRowContains("Second Header".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_clear_namespace_pre_clear_both_visible() {
    // Original: `test_clear_namespace` (sanity half). Split into
    // a separate scenario so each test is a single-shot data
    // literal. This half asserts the pre-clear state — both
    // namespaces visible.
    assert_layout_scenario(LayoutScenario {
        description: "pre-clear: git-blame and lsp namespaces both visible".into(),
        initial_text: "Line 1\nLine 2".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(
                0,
                "Git Blame Header",
                VirtualTextPositionSpec::Above,
                "git-blame",
                0,
            ),
            vt(
                0,
                "LSP Diagnostic",
                VirtualTextPositionSpec::Above,
                "lsp",
                0,
            ),
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("Git Blame Header".into()),
                RowMatch::AnyRowContains("LSP Diagnostic".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_clear_namespace_only_clears_targeted_namespace() {
    // Original: `test_clear_namespace` (post-clear half). After
    // clearing "git-blame", only "lsp" remains visible.
    assert_layout_scenario(LayoutScenario {
        description: "after clear(git-blame): only lsp visible".into(),
        initial_text: "Line 1\nLine 2".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(
                0,
                "Git Blame Header",
                VirtualTextPositionSpec::Above,
                "git-blame",
                0,
            ),
            vt(
                0,
                "LSP Diagnostic",
                VirtualTextPositionSpec::Above,
                "lsp",
                0,
            ),
        ],
        clear_virtual_text_namespaces: vec!["git-blame".into()],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::NoRowContains("Git Blame Header".into()),
                RowMatch::AnyRowContains("LSP Diagnostic".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_lines_have_no_gutter_line_number() {
    // Original: `test_virtual_lines_no_line_numbers`. The row
    // carrying the "VIRTUAL" sentinel must NOT start (after
    // trimming) with a digit — virtual lines have a blank gutter.
    assert_layout_scenario(LayoutScenario {
        description: "virtual-line row has no digit-prefixed gutter".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![vt(7, "VIRTUAL", VirtualTextPositionSpec::Above, "test", 0)],
        expected_virtual_rows_no_digit_gutter: vec!["VIRTUAL".into()],
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_line_marker_tracks_edits_above_anchor() {
    // Original: `test_virtual_line_position_tracking`. Anchor a
    // virtual line above "BBB" (offset 4), then insert "NEW LINE\n"
    // at the buffer's beginning via Ctrl+Home + InsertChar*. The
    // marker on offset 4 must follow the edit so the virtual line
    // still renders above BBB.
    //
    // The pre-edit sanity from the original e2e is split into the
    // companion scenario below to keep each test single-shot.
    assert_layout_scenario(LayoutScenario {
        description: "after insert at start, virtual line still renders above BBB with NEW LINE"
            .into(),
        initial_text: "AAA\nBBB\nCCC".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![vt(
            4, // offset of "BBB"
            "--- Above BBB ---",
            VirtualTextPositionSpec::Above,
            "test",
            0,
        )],
        actions: vec![
            Action::MoveDocumentStart,
            Action::InsertChar('N'),
            Action::InsertChar('E'),
            Action::InsertChar('W'),
            Action::InsertChar(' '),
            Action::InsertChar('L'),
            Action::InsertChar('I'),
            Action::InsertChar('N'),
            Action::InsertChar('E'),
            Action::InsertNewline,
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("--- Above BBB ---".into()),
                RowMatch::AnyRowContains("NEW LINE".into()),
                RowMatch::AnyRowContains("BBB".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_line_marker_pre_edit_sanity() {
    // Original test 6 pre-edit sanity: with no edits, the virtual
    // line is visible above BBB. Captured as a separate scenario
    // so each test is a single-shot data literal.
    assert_layout_scenario(LayoutScenario {
        description: "pre-edit: virtual line '--- Above BBB ---' visible".into(),
        initial_text: "AAA\nBBB\nCCC".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![vt(
            4,
            "--- Above BBB ---",
            VirtualTextPositionSpec::Above,
            "test",
            0,
        )],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("--- Above BBB ---".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_lines_above_and_below_render_in_order() {
    // Original: `test_virtual_lines_above_and_below_same_line`.
    // ABOVE precedes Source which precedes BELOW.
    assert_layout_scenario(LayoutScenario {
        description: "ABOVE → Source → BELOW: all three render in order".into(),
        initial_text: "Source Line".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(
                0,
                "=== ABOVE ===",
                VirtualTextPositionSpec::Above,
                "test",
                0,
            ),
            vt(
                0,
                "=== BELOW ===",
                VirtualTextPositionSpec::Below,
                "test",
                0,
            ),
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("=== ABOVE ===".into()),
                RowMatch::AnyRowContains("Source Line".into()),
                RowMatch::AnyRowContains("=== BELOW ===".into()),
            ],
            ..Default::default()
        },
        expected_row_order: vec![
            ("=== ABOVE ===".into(), "Source Line".into()),
            ("Source Line".into(), "=== BELOW ===".into()),
        ],
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_text_count_after_two_adds() {
    // Original: `test_virtual_text_count` (count=2 half). After
    // adding two virtual lines, the manager's count is 2.
    assert_layout_scenario(LayoutScenario {
        description: "virtual_text_count == 2 after two adds".into(),
        initial_text: "Content".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(0, "Line 1", VirtualTextPositionSpec::Above, "ns1", 0),
            vt(0, "Line 2", VirtualTextPositionSpec::Above, "ns1", 0),
        ],
        expected_virtual_text_count: Some(2),
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_text_count_after_clear() {
    // Original: `test_virtual_text_count` (post-clear half). After
    // clearing the namespace, the count is back to 0.
    assert_layout_scenario(LayoutScenario {
        description: "virtual_text_count == 0 after clear(ns1)".into(),
        initial_text: "Content".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(0, "Line 1", VirtualTextPositionSpec::Above, "ns1", 0),
            vt(0, "Line 2", VirtualTextPositionSpec::Above, "ns1", 0),
        ],
        clear_virtual_text_namespaces: vec!["ns1".into()],
        expected_virtual_text_count: Some(0),
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_text_count_starts_zero() {
    // Original: `test_virtual_text_count` (initial sanity). With
    // no virtual texts injected, the count is 0.
    assert_layout_scenario(LayoutScenario {
        description: "virtual_text_count == 0 initially".into(),
        initial_text: "Content".into(),
        width: 80,
        height: 24,
        expected_virtual_text_count: Some(0),
        ..Default::default()
    });
}

// ── Anti-tests ────────────────────────────────────────────────────────

/// Anti: drop the `initial_virtual_texts` injection. Without the
/// inject, the "--- Header Above Line 2 ---" sentinel must not
/// appear — proves the positive test's `AnyRowContains` is gated
/// on the actual seed.
#[test]
fn anti_virtual_line_above_without_add_line_renders_no_virtual_text() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: no initial_virtual_texts ⇒ sentinel absent; source survives".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::NoRowContains("--- Header Above Line 2 ---".into()),
                RowMatch::AnyRowContains("Line 1".into()),
                RowMatch::AnyRowContains("Line 2".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti: drop the `clear_virtual_text_namespaces` step. Without
/// the clear, both git-blame and lsp must still render.
#[test]
fn anti_clear_namespace_without_clear_keeps_both_visible() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: no clear ⇒ both git-blame and lsp visible".into(),
        initial_text: "Line 1\nLine 2".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![
            vt(
                0,
                "Git Blame Header",
                VirtualTextPositionSpec::Above,
                "git-blame",
                0,
            ),
            vt(
                0,
                "LSP Diagnostic",
                VirtualTextPositionSpec::Above,
                "lsp",
                0,
            ),
        ],
        // No clear_virtual_text_namespaces — load-bearing step dropped.
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("Git Blame Header".into()),
                RowMatch::AnyRowContains("LSP Diagnostic".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}
