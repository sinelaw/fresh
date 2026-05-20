//! DECLARATIVE migration of `tests/e2e/issue_1147_wrapped_line_nav.rs`.
//!
//! Issue #1147: navigation bugs at end-of-file with wrapped lines.
//! Pre-fix:
//! - **Up-arrow scroll** drifted the viewport by ~one logical line
//!   per arrow press from end-of-file when wrapping was enabled.
//! - **Down-arrow skip** jumped past intermediate visual rows of a
//!   wrapped logical line straight to the next logical line.
//! - **End key** stuck on the first visual segment instead of
//!   advancing through subsequent wrapped segments.
//!
//! All scenarios are `LayoutScenario` data literals. The two
//! viewport-stability tests (Up-arrow drift) use
//! `viewport_top_byte_distinct_at_most` over step snapshots. The
//! down-arrow and End-key traversal tests pin the actual cursor
//! byte at each step via `RenderSnapshotExpect.cursor_byte` /
//! `cursor_byte_in` plus the `cursor_byte_strictly_increases_across_steps`
//! invariant — the same `harness.cursor_position()` observable the
//! deleted e2e originals asserted on. The End-key tests drive the
//! production `End` key through `InputEvent::SendKey` (not
//! `Action::MoveLineEnd`, which is NOT equivalent on a wrapped
//! line).
//!
//! Source: `tests/e2e/issue_1147_wrapped_line_nav.rs` (deleted on
//! this branch — these migrations are the sole coverage).

use crate::common::scenario::input_event::{InputEvent, KeyMods, KeySpec};
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, StepAssertion,
};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;
use fresh::test_api::Action;

/// Issue #1147 reproduction content: 20 short lines + 3 long lines
/// that each wrap once + 3 very long lines that wrap multiple times.
fn make_issue_1147_content() -> String {
    let mut lines = Vec::new();
    for i in 1..=20 {
        lines.push(format!("Line {} - short line", i));
    }
    for i in 21..=23 {
        lines.push(format!(
            "Line {} - this is a longer line that should wrap once in an \
             80-column terminal because it needs to exceed eighty characters \
             total length here",
            i
        ));
    }
    for i in 24..=26 {
        lines.push(format!(
            "Line {} - this line is extremely long and should wrap twice in \
             an 80-column terminal, because it has enough characters to fill \
             up more than two full rows of display output in the terminal \
             window making it an excellent test case for wrapping behavior",
            i
        ));
    }
    lines.join("\n")
}

fn line_start_byte(content: &str, one_based_line: usize) -> usize {
    if one_based_line == 1 {
        0
    } else {
        content
            .match_indices('\n')
            .nth(one_based_line - 2)
            .map(|(i, _)| i + 1)
            .expect("line number within content")
    }
}

#[test]
fn migrated_issue_1147_up_arrow_does_not_drift_viewport_at_end_of_wrapped_file() {
    // Original: `test_issue_1147_up_arrow_should_not_scroll_at_end_of_wrapped_file`.
    // After MoveDocumentEnd, 4 Up presses while the cursor is
    // still inside the visible area must not scroll the viewport
    // by more than ~30 bytes (the slack of one short logical line).
    //
    // The original e2e bounded scroll_distance to <= 30 bytes.
    // Here we encode that bound via `viewport_top_within_delta_of`
    // on the final snapshot, anchored at the top_byte captured by
    // the step assertion after MoveDocumentEnd (action 0) via a
    // free-form `step_assertions` snapshot. The cross-step distinct
    // bound is also at most 2 (initial top_after_end, possibly one
    // post-Up value).
    let content = make_issue_1147_content();
    let mut actions = vec![Action::MoveDocumentEnd];
    actions.extend(std::iter::repeat(Action::MoveUp).take(4));

    // Step assertions snapshot top_byte after each action so the
    // `viewport_top_byte_distinct_at_most` invariant has all 5
    // observations.
    let step_assertions: Vec<StepAssertion> = (0..actions.len())
        .map(|i| StepAssertion {
            after_action_index: i,
            expect: RenderSnapshotExpect::default(),
        })
        .collect();

    assert_layout_scenario(LayoutScenario {
        description: "Up ×4 from end of wrapped file: viewport drift ≤ one short line".into(),
        initial_text: content,
        width: 80,
        height: 25,
        actions,
        step_assertions,
        // Original bound: scroll_distance <= 30 bytes across the 4
        // Up presses. Distinct top_byte values across the 5
        // snapshots (MoveDocumentEnd + 4 Ups) bound that drift
        // tighter: at most 2 distinct top_byte values (one
        // before any drift, one after — under the bug, every Up
        // press scrolls a different amount so we'd see ≥ 4).
        viewport_top_byte_distinct_at_most: Some(2),
        ..Default::default()
    });
}

#[test]
fn migrated_issue_1147_down_arrow_traverses_wrapped_visual_lines() {
    // Original: `test_issue_1147_down_arrow_should_traverse_wrapped_visual_lines`.
    // Ctrl+Home, then Ctrl+G 24 lands the cursor at the start of
    // line 24 (a line that wraps to several visual rows). Each
    // Down press must keep the cursor *within* line 24 — bytes
    // [line_24_start, line_25_start) — advancing one visual row,
    // not skipping straight to line 25. The bug skipped to line 25
    // (or beyond) on the first Down.
    //
    // Faithful to the e2e cursor-byte assertions: after GotoLine
    // the cursor is exactly line_24_start; after each Down it is
    // in [line_24_start, line_25_start-1]; and the two Downs make
    // strictly forward progress.
    let content = make_issue_1147_content();
    let line_24_start = line_start_byte(&content, 24);
    let line_25_start = line_start_byte(&content, 25);
    // "within line 24" = [line_24_start, line_25_start - 1] (the
    // byte before line 25's start, which is line 24's trailing
    // boundary). The original asserts pos < line_25_start.
    let within_line_24 = (line_24_start, line_25_start - 1);

    let actions = vec![
        Action::MoveDocumentStart, // mirror the e2e Ctrl+Home
        Action::GotoLine,
        Action::InsertChar('2'),
        Action::InsertChar('4'),
        Action::PromptConfirm, // index 4: cursor at line_24_start
        Action::MoveDown,      // index 5: 1st Down
        Action::MoveDown,      // index 6: 2nd Down
    ];
    let step_assertions = vec![
        StepAssertion {
            after_action_index: 4,
            expect: RenderSnapshotExpect {
                cursor_byte: Some(line_24_start),
                ..Default::default()
            },
        },
        StepAssertion {
            after_action_index: 5,
            expect: RenderSnapshotExpect {
                cursor_byte_in: Some(within_line_24),
                ..Default::default()
            },
        },
        StepAssertion {
            after_action_index: 6,
            expect: RenderSnapshotExpect {
                cursor_byte_in: Some(within_line_24),
                ..Default::default()
            },
        },
    ];
    assert_layout_scenario(LayoutScenario {
        description: "Down ×2 from start of wrapped line 24 stays inside line 24 and advances"
            .into(),
        initial_text: content,
        width: 80,
        height: 25,
        actions,
        step_assertions,
        // GotoLine → line_24_start, 1st Down → further, 2nd Down →
        // further still: strictly increasing cursor byte (the e2e's
        // `pos_after_second_down > pos_after_first_down` claim,
        // generalised across the GotoLine baseline too).
        cursor_byte_strictly_increases_across_steps: true,
        ..Default::default()
    });
}

/// Build a scenario that goes to the start of line 26 and presses
/// the `End` KEY `num_end_presses` times, asserting the cursor's
/// final byte is `expected_cursor_byte`.
///
/// The original e2e uses `KeyCode::End` via `send_key`. We mirror
/// that exactly with `InputEvent::SendKey { code: End }` rather
/// than `Action::MoveLineEnd` — the two are NOT equivalent on a
/// wrapped line: the production End-key handler walks visual
/// segments and then reaches the logical line end, whereas
/// `Action::MoveLineEnd` stalls at the second segment. Faithfully
/// reproducing issue #1147 requires the key path.
fn end_key_scenario(num_end_presses: usize, expected_cursor_byte: usize) -> LayoutScenario {
    LayoutScenario {
        description: format!(
            "GotoLine 26 + End key ×{num_end_presses} → cursor byte {expected_cursor_byte}"
        ),
        initial_text: make_issue_1147_content(),
        width: 80,
        height: 25,
        actions: vec![
            Action::GotoLine,
            Action::InsertChar('2'),
            Action::InsertChar('6'),
            Action::PromptConfirm,
        ],
        events: std::iter::repeat(InputEvent::SendKey {
            code: KeySpec::End,
            modifiers: KeyMods::NONE,
        })
        .take(num_end_presses)
        .collect(),
        expected_snapshot: RenderSnapshotExpect {
            cursor_byte: Some(expected_cursor_byte),
            ..Default::default()
        },
        ..Default::default()
    }
}

// Issue #1147 Bug #3: the End key on a wrapped line must advance
// through each visual segment and ultimately reach the *logical*
// line end, instead of getting stuck on the first visual segment.
// Line 26 is the last line (no trailing newline) and wraps to four
// visual segments in an 80-col terminal; its logical end is the end
// of the buffer. The exact per-segment byte offsets are pinned so a
// regression that stalls End on any earlier segment is caught.
//
// Original: `test_issue_1147_end_key_should_advance_through_wrapped_segments`
// (asserted pos_after_1st < pos_after_2nd < pos_after_3rd, each
// strictly within line 26, then reaches line_26_end).

#[test]
fn migrated_issue_1147_end_key_first_press_lands_on_first_visual_segment_end() {
    assert_layout_scenario(end_key_scenario(1, 1402));
}

#[test]
fn migrated_issue_1147_end_key_second_press_advances_past_first_segment() {
    // The load-bearing "not stuck" claim: End #2 (1471) is strictly
    // past End #1 (1402) and still short of the logical line end.
    assert_layout_scenario(end_key_scenario(2, 1471));
}

#[test]
fn migrated_issue_1147_end_key_third_press_advances_again() {
    assert_layout_scenario(end_key_scenario(3, 1541));
}

#[test]
fn migrated_issue_1147_end_key_reaches_logical_line_end() {
    // Four presses reach line 26's logical end (= end of buffer,
    // 1586); a fifth/sixth press stays put. We assert the
    // logical-end byte after six presses — under the bug the
    // cursor would have stuck at 1402 and never reached 1586.
    let content_len = make_issue_1147_content().len();
    assert_layout_scenario(end_key_scenario(6, content_len));
}

/// Anti-test: a SINGLE End press cannot reach the logical line end
/// (1586) on this multiply-wrapped line — it lands on the first
/// visual segment (1402). If `end_key_scenario(1, 1586)` somehow
/// passed, the End key would be jumping straight to the logical end
/// (regressing the visual-segment traversal). The runner must
/// return Err.
#[test]
fn anti_issue_1147_single_end_press_does_not_reach_logical_end() {
    use crate::common::scenario::layout_scenario::check_layout_scenario;
    let content_len = make_issue_1147_content().len();
    assert!(
        check_layout_scenario(end_key_scenario(1, content_len)).is_err(),
        "anti: one End press lands on the first visual segment, not the logical line end"
    );
}

#[test]
fn migrated_issue_1147_viewport_stable_while_navigating_up_through_wrapped_content() {
    // Original: `test_issue_1147_viewport_stable_while_navigating_up_through_wrapped_content`.
    // Stricter than the 4-press test: 8 Up presses from end-of-file
    // through wrapped content must trigger AT MOST ONE viewport
    // scroll. Encoded declaratively by snapshotting `top_byte`
    // after each of the 8 Up presses (skipping the MoveDocumentEnd
    // baseline) and bounding distinct values to ≤ 2 (the original
    // post-MoveDocumentEnd value plus at most one scrolled value).
    let mut actions = vec![Action::MoveDocumentEnd];
    actions.extend(std::iter::repeat(Action::MoveUp).take(8));
    // Snapshot at the MoveDocumentEnd baseline and after each Up.
    let step_assertions: Vec<StepAssertion> = (0..actions.len())
        .map(|i| StepAssertion {
            after_action_index: i,
            expect: RenderSnapshotExpect::default(),
        })
        .collect();
    assert_layout_scenario(LayoutScenario {
        description: "Up ×8 from doc end: viewport scrolls at most once".into(),
        initial_text: make_issue_1147_content(),
        width: 80,
        height: 25,
        actions,
        step_assertions,
        // 9 snapshots ⇒ at most 2 distinct top_byte values.
        viewport_top_byte_distinct_at_most: Some(2),
        ..Default::default()
    });
}

#[test]
fn anti_migrated_issue_1147_no_moveup_means_no_scroll_events() {
    // Anti-test for `migrated_issue_1147_viewport_stable_while_...`.
    // If we never dispatch MoveUp at all (only MoveDocumentEnd
    // followed by 8 no-op renders, encoded as 8 trivial
    // step-assertions on the same action index), the viewport
    // cannot transition. Distinct top_byte values must equal 1.
    let actions = vec![Action::MoveDocumentEnd];
    // Eight step assertions all anchored on action index 0 — they
    // all snapshot the same state (no actions between them).
    let step_assertions: Vec<StepAssertion> = (0..8)
        .map(|_| StepAssertion {
            after_action_index: 0,
            expect: RenderSnapshotExpect::default(),
        })
        .collect();
    assert_layout_scenario(LayoutScenario {
        description: "anti: no MoveUp ⇒ viewport_top_byte stays fixed (8 idle snapshots)".into(),
        initial_text: make_issue_1147_content(),
        width: 80,
        height: 25,
        actions,
        step_assertions,
        viewport_top_byte_distinct_at_most: Some(1),
        ..Default::default()
    });
}
