//! DECLARATIVE rewrite. Migration of
//! `tests/e2e/virtual_line_bg_and_wrap.rs` — two renderer defects
//! on virtual lines (LineAbove / LineBelow).
//!
//! Every test is a `LayoutScenario` data literal — no harness
//! calls, no per-step imperative `send_key` / `render` flow.
//! Virtual-text injection is expressed via
//! `LayoutScenario::initial_virtual_texts`.
//!
//! Load-bearing claims preserved here:
//!
//!   2. **Long virtual line soft-wraps.** A virtual line whose
//!      text is wider than the viewport's content area soft-wraps
//!      to additional visual rows under `line_wrap = true`. Both
//!      halves of the long text appear on screen.
//!
//! ## Deferred
//!
//! Claim 1 (**background fill to viewport edge**) requires
//! probing the rendered cell's *bg color* at a specific
//! `(col, row)` — the live-diff red stripe must paint from the
//! end of the virtual text all the way to the viewport edge. The
//! `LayoutScenario` DSL today exposes only text-level row
//! matchers (`RowMatch::AnyRowContains`), not cell-bg-color
//! matchers. Adding a `RenderSnapshot::cell_bg` projection
//! requires plumbing the rendered ratatui buffer's per-cell
//! style through to the snapshot — a non-trivial DSL extension
//! orthogonal to this rewrite.
//!
//! - Deferred: `virtual_line_bg_fills_to_viewport_edge` — needs
//!   `RenderSnapshot::cell_bg_at(col, row) -> Option<Color>`
//!   projection.
//! - Deferred (anti):
//!   `anti_virtual_line_bg_without_add_line_has_no_red_trailing_cell`
//!   — same DSL gap.
//!
//! ## DSL extensions used
//!
//! - `LayoutScenario::initial_virtual_texts: Vec<VirtualTextSpec>`
//!   — declarative virtual-line injection (see also
//!   `migrated_virtual_lines.rs`).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, ScenarioConfigOverrides, VirtualTextPositionSpec,
    VirtualTextSpec,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

#[test]
fn migrated_long_virtual_line_wraps_under_line_wrap_default() {
    // Original: `long_virtual_line_wraps_under_line_wrap_default`.
    // A 64-char virtual line on a 40-col viewport with
    // `line_wrap=true` (default) must wrap; the head (32 'A's)
    // and tail (32 'B's) both appear on screen.
    let head = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"; // 32
    let tail = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"; // 32
    let long = format!("{head}{tail}");

    assert_layout_scenario(LayoutScenario {
        description: "64-char virtual line wraps on 40-col viewport with line_wrap=true".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 40,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            ..Default::default()
        },
        initial_virtual_texts: vec![VirtualTextSpec {
            byte_offset: 7,
            text: long,
            position: VirtualTextPositionSpec::Above,
            fg: Some((255, 255, 255)),
            bg: None,
            namespace: "repro".into(),
            priority: 0,
        }],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains(head.into()),
                RowMatch::AnyRowContains(tail.into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_virtual_line_bg_fills_to_viewport_edge() {
    // Deferred: needs `RenderSnapshot::cell_bg_at(col, row) ->
    // Option<Color>` projection on the snapshot. The load-bearing
    // claim is "the cell at (60, hit_row) has bg color = the
    // virtual-line's red" — pure cell-style assertion that
    // `LayoutScenario`'s text-level matchers can't express.
    //
    // The companion anti below is deferred for the same reason.
    //
    // Once the bg-color projection lands, the scenario shape is:
    //
    //     LayoutScenario {
    //         initial_virtual_texts: vec![VirtualTextSpec {
    //             bg: Some((180, 30, 30)),
    //             fg: Some((255, 255, 255)),
    //             text: "DELETED".into(),
    //             position: Above,
    //             ..
    //         }],
    //         expected_snapshot: RenderSnapshotExpect {
    //             cell_bg_at: vec![CellBgExpect {
    //                 col: 60,
    //                 row_with_substring: "DELETED".into(),
    //                 expected_rgb: (180, 30, 30),
    //             }],
    //             ..
    //         },
    //         ..
    //     }
}

// ── Anti-tests ────────────────────────────────────────────────────────

/// Anti: drop the long-text virtual-line injection. Without it,
/// neither head nor tail sentinel must appear — proves the wrap
/// claim depends on actually injecting the long virtual text.
#[test]
fn anti_long_virtual_line_without_add_line_renders_no_sentinels() {
    let head = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
    let tail = "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB";
    assert_layout_scenario(LayoutScenario {
        description: "anti: no virtual-line inject ⇒ neither head nor tail visible".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 40,
        height: 24,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::NoRowContains(head.into()),
                RowMatch::NoRowContains(tail.into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn anti_virtual_line_bg_without_add_line_has_no_red_trailing_cell() {
    // Deferred: same DSL gap as the positive bg-fill scenario.
    // Once `RenderSnapshot::cell_bg_at` lands, the anti shape is:
    //
    //     RenderSnapshotExpect {
    //         cell_bg_rgb_count: Some((180, 30, 30), 0),
    //         row_checks: vec![RowMatch::NoRowContains("DELETED".into())],
    //         ..
    //     }
}
