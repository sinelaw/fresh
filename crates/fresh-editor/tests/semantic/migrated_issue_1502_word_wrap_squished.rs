//! DECLARATIVE: Migration of `tests/e2e/issue_1502_word_wrap_squished.rs`.
//!
//! Regression cover for issue #1502 ("Word wrap does not seem right:
//! squished"): on a narrow terminal with `wrap_indent` (hanging
//! indent) enabled, deeply indented lines must not wrap to ~7 chars
//! per continuation row.
//!
//! Root cause being guarded against: hanging indent was being
//! double-counted in the wrapping transform — once when
//! `effective_width` subtracted `line_indent` from `available_width`,
//! and again because `emit_break_with_indent` emitted the indent as
//! actual text content that counted toward `current_line_width`. With
//! `available_width=27` and `line_indent=10`, the bug shrank
//! continuation capacity from 17 to 7 chars.
//!
//! Scenario is data; the runner executes it. The continuation-row
//! width check is expressed declaratively via the
//! [`RowMatch::ContinuationRowsMinContentWidth`] matcher (extension
//! added in this migration): it iterates every snapshot row, picks
//! the rows whose gutter area before the last `│` has no ASCII
//! digit (i.e. wrapped-continuation rows), and asserts the trimmed
//! content width is `>= min`. `skip_last = true` excludes the final
//! continuation, which can be arbitrarily short (the remainder
//! after the last full wrap).
//!
//! Load-bearing claim preserved here:
//!
//!   * On a 35-col terminal with `line_wrap=true`, `wrap_indent=true`,
//!     a 10-space-indented long line wraps with at least 10 visible
//!     content chars per continuation row (excluding the final
//!     remainder row). The "squished" regression would put each
//!     continuation at ~7 chars.
//!
//! Source: `tests/e2e/issue_1502_word_wrap_squished.rs` (1 test
//! migrated + 1 anti-test; no tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

// 10-space hanging indent + a long unbroken token. On a 35-col
// terminal (gutter ~8, available ~27) the line must wrap; each
// full continuation row holds 27 - 10 = 17 chars when the
// hanging-indent transform is correct. The bug shrank that to ~7.
const INDENTED_LONG_LINE: &str =
    "          abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

#[test]
fn migrated_issue_1502_wrap_indent_squished_on_narrow_terminal() {
    assert_layout_scenario(LayoutScenario {
        description: "issue #1502: 10-space hanging indent on a 35-col \
                      terminal must give continuation rows >= 10 chars \
                      of content (bug: 7)"
            .into(),
        initial_text: INDENTED_LONG_LINE.into(),
        // 35-column terminal: gutter ~8, available ~27. With 10-space
        // indent the bug yields ~7-char continuations; the fix
        // restores 17.
        width: 35,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            wrap_indent: Some(true),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::ContinuationRowsMinContentWidth {
                min: 10,
                // The final continuation (remainder after the last
                // full wrap) can be arbitrarily short — skip it
                // so the assertion targets only the "stable" rows
                // whose width is set by the transform geometry,
                // not by where the source string happens to end.
                skip_last: true,
            }],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the `line_wrap = true` config flag (the
/// load-bearing precondition that makes wrapping happen at all).
/// Without it, no continuation rows are produced. The positive
/// matcher would fail "no continuation rows found", but the
/// claim-under-test here is the inverse — we assert NO continuation
/// rows exist via [`RowMatch::NoContinuationRows`] (extension added
/// in this migration). Proves the positive test's claim is gated on
/// `line_wrap=true`, not on something the renderer does at the
/// default config.
#[test]
fn anti_issue_1502_without_line_wrap_produces_no_continuation_rows() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: line_wrap=false → no wrapped-continuation \
                      rows exist; the line stays on one logical row \
                      and the renderer scrolls horizontally instead"
            .into(),
        initial_text: INDENTED_LONG_LINE.into(),
        width: 35,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            // line_wrap defaults to false; explicit for clarity.
            line_wrap: Some(false),
            wrap_indent: Some(true),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::NoContinuationRows],
            ..Default::default()
        },
        ..Default::default()
    });
}
