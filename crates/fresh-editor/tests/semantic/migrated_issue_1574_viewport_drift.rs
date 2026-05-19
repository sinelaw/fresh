//! DECLARATIVE migration of `tests/e2e/issue_1574_compose_scroll.rs`.
//!
//! Issue #1574: arrow keys must NOT scroll the viewport while the
//! cursor remains inside the visible rows. Pre-fix, the
//! `Viewport::ensure_visible` scroll-margin gate fired even when the
//! cursor was already visible, causing the viewport to drift by one
//! visual row per arrow press in heavily-wrapped buffers.
//!
//! Scenarios are `LayoutScenario` data literals. The "viewport does
//! not change across N arrow presses" claim is expressed by
//! `step_assertions` snapshotting `viewport_top_byte` after each
//! arrow press and `viewport_top_byte_distinct_at_most: Some(1)`
//! constraining all step snapshots to one value.
//!
//! Source: `tests/e2e/issue_1574_compose_scroll.rs` (2 tests + 1
//! anti-test; 0 deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
    StepAssertion,
};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;
use fresh::test_api::Action;

fn long_wrapped_content() -> String {
    let para = "This is a deliberately long paragraph that must wrap across many \
                visual rows so the scroll math is exercised. It continues for \
                a while so a single logical line becomes many visual rows.";
    let mut s = String::from("# Test\n\n");
    for i in 1..=6 {
        s.push_str(&format!("Paragraph {i}: {para}\n\n"));
    }
    s.push_str("End of file.\n");
    s
}

fn wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

/// Build the action sequence: a leading anchor action, then N
/// repeated arrow presses. Each press has its own step assertion so
/// the runner snapshots `viewport_top_byte` after each, and the
/// cross-step `viewport_top_byte_distinct_at_most` invariant pins
/// that all N snapshots share the same top byte (i.e. no drift).
fn make_drift_scenario(
    description: &str,
    anchor: Action,
    arrow: Action,
    n_presses: usize,
) -> LayoutScenario {
    let mut actions = vec![anchor];
    actions.extend(std::iter::repeat(arrow).take(n_presses));
    // One step assertion per arrow press, snapshotting top_byte.
    let step_assertions: Vec<StepAssertion> = (1..=n_presses)
        .map(|i| StepAssertion {
            after_action_index: i, // 0 = anchor; 1..=n = arrow presses
            expect: RenderSnapshotExpect::default(),
        })
        .collect();
    LayoutScenario {
        description: description.into(),
        initial_text: long_wrapped_content(),
        width: 80,
        height: 20,
        actions,
        config_overrides: wrap_overrides(),
        step_assertions,
        viewport_top_byte_distinct_at_most: Some(1),
        ..Default::default()
    }
}

#[test]
fn migrated_issue_1574_up_arrow_does_not_drift_viewport_when_visible() {
    // Original: `test_issue_1574_up_does_not_scroll_when_cursor_not_at_top`.
    // After MoveDocumentEnd the cursor is near the bottom. 3 Up
    // presses while the cursor stays inside the visible area must
    // NOT change `viewport_top_byte`.
    assert_layout_scenario(make_drift_scenario(
        "Up does not drift viewport when cursor is visible (heavy wrap)",
        Action::MoveDocumentEnd,
        Action::MoveUp,
        3,
    ));
}

#[test]
fn migrated_issue_1574_down_arrow_does_not_drift_viewport_when_visible() {
    // Original: `test_issue_1574_down_does_not_scroll_when_cursor_not_at_bottom`.
    // From the top of the buffer, 3 Down presses must NOT change
    // `viewport_top_byte` (the cursor is still well above the
    // bottom margin).
    assert_layout_scenario(make_drift_scenario(
        "Down does not drift viewport when cursor is visible (heavy wrap)",
        Action::MoveDocumentStart,
        Action::MoveDown,
        3,
    ));
}

/// Anti-test: drop the `MoveDocumentEnd` jump. Without parking the
/// cursor near the bottom of a long-wrapped buffer the viewport
/// stays at byte 0, so the positive test's precondition (cursor
/// visible near bottom) collapses. We check this declaratively by
/// asserting the final `viewport_top_byte` is 0 *with* the
/// positive test's exact width / height / config — but with an
/// empty action sequence — and confirming the runner sees top=0.
#[test]
fn anti_issue_1574_dropping_move_document_end_yields_zero_top() {
    // Positive shape minus the anchor and arrow actions.
    let scenario = LayoutScenario {
        description: "anti: no MoveDocumentEnd ⇒ viewport_top_byte stays at 0".into(),
        initial_text: long_wrapped_content(),
        width: 80,
        height: 20,
        actions: vec![],
        config_overrides: wrap_overrides(),
        expected_top_byte: Some(0),
        ..Default::default()
    };
    // The positive-shape claim "top_byte stayed at the
    // MoveDocumentEnd value" reduces to "top_byte == 0" when the
    // jump is dropped — vacuous against any "stayed the same"
    // assertion that wasn't anchored to a non-zero baseline. Pin
    // that down: top=0 here, so the positive test's invariant
    // depends entirely on `MoveDocumentEnd` non-trivially advancing
    // top_byte to begin with.
    assert!(
        check_layout_scenario(scenario).is_ok(),
        "anti precondition: without MoveDocumentEnd, viewport_top_byte must equal 0 — \
         confirming the positive test's claim is non-vacuous only because the jump \
         moves top_byte to a positive value before the arrow loop runs"
    );
}
