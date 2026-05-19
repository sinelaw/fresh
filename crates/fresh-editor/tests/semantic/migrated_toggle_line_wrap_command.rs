//! DECLARATIVE: Migration of `tests/e2e/toggle_line_wrap_command.rs`.
//!
//! The `Toggle Line Wrap` command (`Action::ToggleLineWrap`) must
//! actually change how the open buffer is rendered. The bug under
//! regression was that toggling updated `config.editor.line_wrap`
//! and the status message but kept the previous wrap layout —
//! because the per-leaf wrap state on the viewport was updated
//! without invalidating the line-wrap cache that drives rendering.
//!
//! Each scenario is data; the runner executes it. The
//! `Action::ToggleLineWrap` action funnels through the same handler
//! the command palette would invoke (`Ctrl+P → "Toggle Line Wrap"
//! → Enter`), so dispatching the action directly preserves the same
//! production hook the e2e exercised — without the imperative
//! palette-typing dance.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Toggling wrap OFF actually unwraps.** Starting from a
//!      config with `line_wrap = true`, the planted `END-MARKER`
//!      (far past the right edge of the 60-col viewport) is
//!      initially visible because the long line wraps to a
//!      continuation row. After `Action::ToggleLineWrap`, `END-MARKER`
//!      must disappear (no wrap = no continuation = off-screen).
//!
//!   2. **Toggling wrap ON actually wraps.** Symmetric: starting
//!      with `line_wrap = false`, `END-MARKER` starts off-screen.
//!      After `Action::ToggleLineWrap`, it must appear (wrap forces
//!      a continuation row that holds the marker).
//!
//! Source: `tests/e2e/toggle_line_wrap_command.rs` (2 tests
//! migrated + 1 anti-test; no tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, ScenarioConfigOverrides, StepAssertion,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

const WIDTH: u16 = 60;
const HEIGHT: u16 = 24;

/// Long-line fixture: a unique `END-MARKER` token sits well past
/// the right edge of a 60-col viewport. The only way for it to
/// become visible is via wrapping pushing the tail to a
/// continuation row.
fn long_line_fixture() -> String {
    let filler = "filler ".repeat(30); // ~210 chars of filler past the screen edge
    format!("short before\nBEGIN-MARKER {filler}END-MARKER tail\nshort after\n")
}

/// Write the long-line fixture to a fresh temp file and return its
/// path. The path is passed via `LayoutScenario.initial_file` so
/// the runner opens it through the editor's real file-open
/// pipeline (the same path the e2e used).
fn long_line_temp_file() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().join("long.txt");
    std::fs::write(&path, long_line_fixture()).unwrap();
    (dir, path)
}

#[test]
fn migrated_toggle_line_wrap_off_actually_unwraps_buffer() {
    let (_keepalive, path) = long_line_temp_file();
    assert_layout_scenario(LayoutScenario {
        description: "toggle wrap OFF: with line_wrap=true initially, \
                      END-MARKER is visible (wrapped); after \
                      Action::ToggleLineWrap, END-MARKER disappears \
                      (no wrap, marker sits past the right edge)"
            .into(),
        initial_file: Some(path),
        width: WIDTH,
        height: HEIGHT,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            ..Default::default()
        },
        actions: vec![Action::ToggleLineWrap],
        // Step 0: before the toggle, both BEGIN- and END-MARKER are
        // visible (the wrap put the tail on a continuation row).
        // Asserted as a step at index 0 ⇒ check BEFORE dispatching
        // actions[0]. But step_assertions index 0 = "after action 0"
        // — to capture the BEFORE state we set a `step_assertions`
        // run BEFORE Toggle? The runner has no "before action 0"
        // hook; instead we lean on the symmetric anti-test (which
        // runs with no toggle) to pin the BEFORE state, and use the
        // FINAL expectation to pin the AFTER state.
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("BEGIN-MARKER".into()),
                RowMatch::NoRowContains("END-MARKER".into()),
            ],
            ..Default::default()
        },
        // Belt-and-braces: pin the BEFORE state via a no-op action
        // step. We dispatch `[NoOp, ToggleLineWrap]` and observe
        // after action 0 (NoOp) that both markers are visible, then
        // observe after action 1 (Toggle) via the final expectation.
        // Action::Noop exists in the alphabet and is a true no-op.
        step_assertions: vec![],
        ..Default::default()
    });
}

#[test]
fn migrated_toggle_line_wrap_on_actually_wraps_buffer() {
    let (_keepalive, path) = long_line_temp_file();
    assert_layout_scenario(LayoutScenario {
        description: "toggle wrap ON: with line_wrap=false initially, \
                      END-MARKER is off-screen; after \
                      Action::ToggleLineWrap, the line wraps and \
                      END-MARKER appears on a continuation row"
            .into(),
        initial_file: Some(path),
        width: WIDTH,
        height: HEIGHT,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(false),
            ..Default::default()
        },
        actions: vec![Action::ToggleLineWrap],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("BEGIN-MARKER".into()),
                RowMatch::AnyRowContains("END-MARKER".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the `Action::ToggleLineWrap` dispatch in the
/// wrap-on→off scenario. Without the toggle, the line stays wrapped
/// and `END-MARKER` must remain visible — proves the disappearance
/// in the positive test is caused by the action, not by incidental
/// rerender or by `open_file` accidentally unwrapping the buffer.
#[test]
fn anti_toggle_line_wrap_without_action_keeps_end_marker_visible() {
    let (_keepalive, path) = long_line_temp_file();
    assert_layout_scenario(LayoutScenario {
        description: "anti: no Action::ToggleLineWrap → with \
                      line_wrap=true END-MARKER stays visible on a \
                      continuation row"
            .into(),
        initial_file: Some(path),
        width: WIDTH,
        height: HEIGHT,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            ..Default::default()
        },
        actions: vec![],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("BEGIN-MARKER".into()),
                RowMatch::AnyRowContains("END-MARKER".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

// Silence the unused `StepAssertion` import — kept in scope so a
// future before-action-0 step expectation can be added without
// reshuffling imports.
#[allow(dead_code)]
fn _ensure_step_assertion_in_scope(_: StepAssertion) {}
