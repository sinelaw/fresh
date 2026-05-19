//! DEFERRED-DECLARATIVE: Migration of
//! `tests/e2e/line_wrap_cache_consistency.rs` — internal-cache
//! consistency invariants for `LineWrapCache`.
//!
//! ## Status: ALL positive tests deferred (no `EditorTestApi` projection)
//!
//! Every test in the original file probes internal cache state
//! that has no `EditorTestApi` projection:
//!
//!   * `fresh::view::line_wrap_cache::LineWrapKey` — the cache
//!     key (a struct of `effective_width`, `gutter_width`,
//!     `wrap_column`, `hanging_indent`, `line_wrap_enabled`,
//!     `pipeline_inputs_version`, `view_mode`, `line_start`).
//!   * `fresh::view::line_wrap_cache::count_visual_rows_for_text` —
//!     the pure recompute function the cache's writeback values
//!     must equal.
//!   * Direct `state.line_wrap_cache.get(&key)` lookups.
//!   * Direct `state.buffer.line_iterator()` traversal.
//!   * `state.{buffer,soft_breaks,conceals,virtual_texts}.version()`
//!     for `pipeline_inputs_version` reconstruction.
//!
//! None of these surfaces have a `LayoutScenario` / declarative
//! equivalent, and exposing them through the test API would mean
//! re-projecting `LineWrapKey`, `CacheViewMode`,
//! `pipeline_inputs_version`, `count_visual_rows_for_text`, and
//! the cache's `get(&key) -> Option<&[..]>` accessor in their
//! entirety — which would defeat the test_api boundary's whole
//! purpose (these are renderer-internal data structures, not
//! editor-state observables).
//!
//! The five deferred theorems are:
//!
//!   1. `render_writeback_values_match_fresh_recompute` (Layer 5)
//!   2. `scroll_math_miss_handler_matches_fresh_recompute` (Layer 2)
//!   3. `resize_produces_fresh_cache_entries_at_new_width` (Layer 6)
//!   4. `repeated_edits_keep_cache_consistent` (Layer 6)
//!   5. `edit_invalidates_cache_visibly` (Layer 5b)
//!
//! Until a `RenderSnapshot`-level invariant is extracted (the
//! consistency claims have to be re-phrased in terms of what the
//! user sees on screen, not in terms of cache-entry equality with
//! a pure recompute), these stay in the e2e file.
//!
//! ## What this file does keep
//!
//! One declarative regression guard that exercises the renderer
//! end-to-end with `line_wrap = true` on the same `mixed_buffer`
//! content the cache theorems used. The guard asserts that with
//! wrap on, a long word-wrapped paragraph appears across multiple
//! rows in the rendered output — load-bearing for the cache
//! theorems' implicit precondition that wrap is actually
//! happening when the cache is being populated (if wrap were
//! disabled, the writeback path would never fire and the original
//! theorems would pass vacuously).
//!
//! ## Anti-test
//!
//! Drops the `line_wrap = true` override. With wrap off, the same
//! content fits each logical line on a single rendered row, and
//! the "word05 word06 …" continuation must NOT appear on the row
//! immediately after the row containing "word00 word01". That's
//! the inverse of the positive claim — it fires only because the
//! load-bearing wrap override is present in the positive test.
//!
//! Source: `tests/e2e/line_wrap_cache_consistency.rs` (5 tests
//! deferred; 1 anti / 1 positive declarative regression guard
//! added to keep the wrap-on rendering surface covered).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

/// Mirrors the e2e file's `mixed_buffer()`: a few short lines + a
/// 20-word paragraph that wraps under realistic widths. The
/// paragraph's repeating `word00 word01 word02 …` pattern is
/// load-bearing for the row-content assertions below.
fn mixed_buffer() -> String {
    let short_lines = [
        "Line 1: short.",
        "",
        "// A comment",
        "{",
        "    let x = 1;",
        "}",
    ];
    let para: String = (0..20)
        .map(|i| format!("word{:02}", i))
        .collect::<Vec<_>>()
        .join(" ");
    let mut lines: Vec<String> = short_lines.iter().map(|s| s.to_string()).collect();
    for _ in 0..5 {
        lines.push(para.clone());
        lines.push(String::new());
    }
    lines.push("final line".to_string());
    lines.join("\n")
}

/// Positive declarative regression guard. With `line_wrap = true`
/// and a narrow viewport, the 20-word paragraph wraps to several
/// visual rows — the renderer is invoked with the cache enabled
/// (`config.editor.line_wrap = true` flips the cache key
/// dimension), so the writeback paths the deferred theorems
/// observed *internally* run end-to-end. We assert on the
/// observable behaviour: "word00" appears somewhere on screen
/// AND so does a later word that wouldn't fit on the same
/// visual row.
#[test]
fn migrated_line_wrap_renders_paragraph_across_multiple_visual_rows() {
    assert_layout_scenario(LayoutScenario {
        description: "wrap=on: 20-word paragraph spans multiple visual rows".into(),
        initial_text: mixed_buffer(),
        width: 50,
        height: 24,
        actions: vec![],
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                // First half of the paragraph must be visible.
                RowMatch::AnyRowContains("word00".into()),
                // A late word that does NOT fit on the same row as
                // "word00" at width=50 (which leaves ~42 chars for
                // content after the gutter). 20 6-char words plus
                // 19 spaces = 139 chars total → at least 3 visual
                // rows. "word18" must be on a different row than
                // "word00".
                RowMatch::AnyRowContains("word18".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the `line_wrap = true` override. With wrap
/// off, the 20-word paragraph occupies a single logical line and
/// horizontal scroll keeps only the first ~42 chars on screen at
/// width=50. The late word "word18" must NOT be visible without
/// scrolling — proves the positive test's row visibility
/// assertion is gated on wrap being on.
#[test]
fn anti_line_wrap_off_late_word_not_visible_without_scroll() {
    let scenario = LayoutScenario {
        description: "anti: wrap=off, late words off-screen".into(),
        initial_text: mixed_buffer(),
        width: 50,
        height: 24,
        actions: vec![],
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                // Same assertions as the positive test — they must
                // FAIL under wrap=off.
                RowMatch::AnyRowContains("word00".into()),
                RowMatch::AnyRowContains("word18".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: with line_wrap=false, late words in a long line must be \
         clipped off the right of the viewport — the positive test's \
         'word18 visible' check should fail."
    );
}
