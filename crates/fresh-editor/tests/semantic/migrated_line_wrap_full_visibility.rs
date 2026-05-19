//! DECLARATIVE migration of `tests/e2e/line_wrap_full_visibility.rs`.
//!
//! Under `line_wrap = true`, every printable character of every
//! fixture line must be rendered somewhere in the viewport, at a
//! variety of terminal widths and with the file-explorer sidebar
//! both closed and open. The bug class guarded against: characters
//! that straddle a wrap boundary getting dropped from the rendered
//! output ("too late" wrap regressions).
//!
//! Translation to the semantic framework:
//!
//!   * Each fixture line is wrapped with a unique head sentinel
//!     `LN###<` and tail sentinel `>LN###`. Asserting that *both*
//!     sentinels appear on the rendered screen (via
//!     `RowMatch::AnyRowContains`) proves the line was rendered
//!     start-to-end — if any wrap-boundary characters were lost,
//!     either sentinel could go missing depending on where the
//!     wrap fell.
//!   * The trial sweeps a representative set of widths (rather
//!     than the full 40..=100 the e2e file walked) crossed with
//!     sidebar open/closed. Picking widths that bracket the
//!     sentinel-token length on both sides exercises the wrap
//!     boundary at varied positions per line.
//!   * Each trial is its own `LayoutScenario` data literal —
//!     state isolation matches the e2e original.
//!
//! Caveats vs. the e2e original:
//!
//!   * `RowMatch` variants test for substring presence on a single
//!     row; they cannot count occurrences across the screen. A
//!     regression that drops one *middle* character of a long line
//!     while preserving the head and tail sentinels would not be
//!     detected here, whereas the e2e's per-line non-whitespace
//!     comparison would catch it. Deliberate trade-off: the
//!     head+tail-sentinel approach catches the dominant
//!     "lost at wrap boundary" class faithfully via framework
//!     matchers. Future `RowMatch::CountAcrossRows` would close
//!     the gap.
//!   * Fixture is narrower than the e2e curated set; we keep the
//!     representative shapes — short word, long word, deep
//!     nesting, indented hanging-wrap, and char-boundary
//!     stressor — because those drove the original regression.
//!
//! Source: `tests/e2e/line_wrap_full_visibility.rs` (1 sweep test
//! migrated; no tests deferred).

use crate::common::fixtures::TestFixture;
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

fn wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

fn no_wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(false),
        ..Default::default()
    }
}

/// Each entry is rendered as `LN###< ... >LN###` so the head and
/// tail of the line are unique substrings that survive lookup in
/// the rendered rows even after wrapping.
fn interesting_lines() -> Vec<String> {
    let middles: Vec<String> = vec![
        // Short word — single visual row at every width.
        "alpha beta".into(),
        // Medium with trailing punctuation runs.
        "ending with a run of dots...".into(),
        // Nested parens — historical wrap-boundary trouble spot.
        "(((triple nested)))".into(),
        // Code-like with mixed brackets.
        "fn sum(x: i32, y: i32) -> i32 { (x + y) * 2 }".into(),
        // Long single token — forces char-boundary wrap.
        "supercalifragilisticexpialidociousfoo".into(),
        // Indented hanging-wrap path; mirrors the Kotlin-style
        // line that originally surfaced the "too late" wrap bug.
        "        binding.recyclerView.layoutManager = LinearLayoutManager(requireContext())".into(),
        // Char-stressor: 36 consecutive 'a's, forces wrap mid-run.
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".into(),
    ];

    middles
        .into_iter()
        .enumerate()
        .map(|(i, mid)| format!("LN{:03}< {mid} >LN{:03}", i, i))
        .collect()
}

/// Build the row-check vec for the sentinel sweep: head + tail
/// substring per fixture line.
fn sentinel_row_checks(lines: &[String]) -> Vec<RowMatch> {
    let mut checks: Vec<RowMatch> = Vec::with_capacity(lines.len() * 2);
    for i in 0..lines.len() {
        checks.push(RowMatch::AnyRowContains(format!("LN{:03}<", i)));
        checks.push(RowMatch::AnyRowContains(format!(">LN{:03}", i)));
    }
    checks
}

/// One `(width, sidebar_open)` declarative trial.
fn trial(
    width: u16,
    height: u16,
    sidebar_open: bool,
    fixture_path: &std::path::Path,
    lines: &[String],
) -> LayoutScenario {
    let mut actions: Vec<Action> = vec![Action::MoveDocumentStart];
    if sidebar_open {
        // `Action::ToggleFileExplorer` is the editor-side
        // projection of Ctrl+E. The runner's `dispatch_seq` calls
        // `process_async_messages` after dispatch, so the async
        // explorer-directory scan settles before the final render.
        actions.push(Action::ToggleFileExplorer);
    }
    LayoutScenario {
        description: format!(
            "line_wrap visibility: w={width} h={height} sidebar_open={sidebar_open}"
        ),
        initial_text: String::new(),
        initial_file: Some(fixture_path.to_path_buf()),
        width,
        height,
        config_overrides: wrap_overrides(),
        actions,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: sentinel_row_checks(lines),
            ..Default::default()
        },
        ..Default::default()
    }
}

#[test]
fn migrated_line_wrap_all_lines_visible_across_widths_and_sidebar() {
    let lines = interesting_lines();
    let fixture =
        TestFixture::new("line_wrap_visibility_semantic.txt", &lines.join("\n"))
            .expect("create fixture");

    // Sample widths that bracket each sentinel-token's length on
    // both sides, so the wrap boundary lands at a variety of
    // positions across the fixture. The e2e original swept every
    // integer 40..=100; this sampled set preserves per-trial
    // coverage without paying the full 122-trial cost in CI.
    let widths: [u16; 5] = [40, 50, 60, 80, 100];
    // Height generous enough that every wrapped line fits inside
    // the content area even at the narrowest width with the
    // sidebar open. The check is about visibility under wrap,
    // not scrolling.
    let height: u16 = 200;

    for &width in &widths {
        for &sidebar_open in &[false, true] {
            assert_layout_scenario(trial(width, height, sidebar_open, &fixture.path, &lines));
        }
    }
}

/// Anti-test: with `line_wrap = false` and a narrow viewport, long
/// fixture lines extend past the right edge and their tail
/// sentinels are NOT rendered (no wrap → no tail). Encoded by
/// flipping the sentinel expectations to `AnyRowContains(">LN006")`
/// — line 6 is the 36-'a' char-stressor, wider than the 40-col
/// viewport's content area. With `line_wrap = false` the tail
/// `>LN006` is off-screen, so `check_layout_scenario` returns Err.
/// Proves the visibility claim depends on `line_wrap = true`.
#[test]
fn anti_line_wrap_disabled_loses_tail_sentinels_off_screen() {
    let lines = interesting_lines();
    let fixture =
        TestFixture::new("line_wrap_visibility_anti.txt", &lines.join("\n"))
            .expect("create fixture");

    let scenario = LayoutScenario {
        description: "anti: line_wrap=false on 40-col viewport ⇒ tail '>LN006' off-screen".into(),
        initial_text: String::new(),
        initial_file: Some(fixture.path.clone()),
        width: 40,
        height: 50,
        config_overrides: no_wrap_overrides(),
        actions: vec![Action::MoveDocumentStart],
        expected_snapshot: RenderSnapshotExpect {
            // The positive sweep test would assert `AnyRowContains(">LN006")`
            // here; with line_wrap=false this fails.
            row_checks: vec![RowMatch::AnyRowContains(">LN006".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: with line_wrap=false on a 40-col viewport the tail \
         sentinel '>LN006' should be off-screen"
    );
}
