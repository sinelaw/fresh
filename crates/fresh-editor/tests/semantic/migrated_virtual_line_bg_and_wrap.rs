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
//!   1. **Background fill to viewport edge.** A `LineAbove`
//!      virtual line whose `Style` has a `bg` paints that bg on
//!      the trailing cells of its visual row, all the way to the
//!      viewport edge — not just behind the literal text. Pinned
//!      via the `LayoutScenario::expected_cell_bg` cell-background
//!      matcher: the cell at column 60 of the `DELETED` row must
//!      have the virtual line's red bg.
//!
//!   2. **Long virtual line soft-wraps.** A virtual line whose
//!      text is wider than the viewport's content area soft-wraps
//!      to additional visual rows under `line_wrap = true`. Both
//!      halves of the long text appear on screen.
//!
//! ## DSL extensions used
//!
//! - `LayoutScenario::initial_virtual_texts: Vec<VirtualTextSpec>`
//!   — declarative virtual-line injection (see also
//!   `migrated_virtual_lines.rs`).
//! - `LayoutScenario::expected_cell_bg: Vec<CellBgExpect>` — locate
//!   a rendered row by substring and assert the ratatui cell
//!   background at a column (reads `harness.get_cell_style`, the
//!   same observable the e2e test used via `buf[(60, row)].style()`).
//!
//! Source: `tests/e2e/virtual_line_bg_and_wrap.rs` (2 tests + 2
//! anti-tests; 0 deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, CellBgExpect, LayoutScenario,
    ScenarioConfigOverrides, VirtualTextPositionSpec, VirtualTextSpec,
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
    // Original: `virtual_line_bg_fills_to_viewport_edge`.
    // A `LineAbove` virtual line with a red bg, anchored at byte 7
    // (start of "Line 2") on an 80-col viewport. The load-bearing
    // claim: the cell at column 60 — well past the 7-char
    // "DELETED" text but inside the content area — must carry the
    // virtual line's red bg, proving the fill reaches the viewport
    // edge rather than stopping at the end of the literal text.
    let red = (180u8, 30u8, 30u8);
    assert_layout_scenario(LayoutScenario {
        description: "virtual-line red bg fills to viewport edge (col 60 of DELETED row)".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![VirtualTextSpec {
            byte_offset: 7,
            text: "DELETED".into(),
            position: VirtualTextPositionSpec::Above,
            fg: Some((255, 255, 255)),
            bg: Some(red),
            namespace: "repro".into(),
            priority: 0,
        }],
        expected_cell_bg: vec![CellBgExpect {
            row_with_substring: "DELETED".into(),
            col: 60,
            expected_rgb: Some(red),
        }],
        ..Default::default()
    });
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
fn anti_virtual_line_without_bg_has_no_red_trailing_cell() {
    // Anti: inject the SAME `DELETED` virtual line but with NO bg
    // (fg only). The trailing cell at column 60 of the DELETED row
    // must then carry the theme's default content background
    // (Rgb(20,20,20)), NOT the red fill — proving the red stripe
    // is gated on the virtual line actually carrying a bg style,
    // not on the line's mere presence.
    let theme_default_bg = (20u8, 20u8, 20u8);
    let scenario = LayoutScenario {
        description: "anti: virtual line with no bg ⇒ col 60 of DELETED row is theme-default bg"
            .into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![VirtualTextSpec {
            byte_offset: 7,
            text: "DELETED".into(),
            position: VirtualTextPositionSpec::Above,
            fg: Some((255, 255, 255)),
            bg: None,
            namespace: "repro".into(),
            priority: 0,
        }],
        expected_cell_bg: vec![CellBgExpect {
            row_with_substring: "DELETED".into(),
            col: 60,
            expected_rgb: Some(theme_default_bg),
        }],
        ..Default::default()
    };
    // The trailing cell is the theme default, not red — assert the
    // positive scenario succeeds with the default-bg expectation.
    assert_layout_scenario(scenario);

    // And prove the inverse: the positive test's "red at col 60"
    // expectation would FAIL when the virtual line has no bg.
    let red_scenario = LayoutScenario {
        description: "anti: no-bg virtual line is NOT red at col 60".into(),
        initial_text: "Line 1\nLine 2\nLine 3".into(),
        width: 80,
        height: 24,
        initial_virtual_texts: vec![VirtualTextSpec {
            byte_offset: 7,
            text: "DELETED".into(),
            position: VirtualTextPositionSpec::Above,
            fg: Some((255, 255, 255)),
            bg: None,
            namespace: "repro".into(),
            priority: 0,
        }],
        expected_cell_bg: vec![CellBgExpect {
            row_with_substring: "DELETED".into(),
            col: 60,
            expected_rgb: Some((180, 30, 30)),
        }],
        ..Default::default()
    };
    assert!(
        check_layout_scenario(red_scenario).is_err(),
        "anti: with no bg style the trailing cell must NOT be red"
    );
}
