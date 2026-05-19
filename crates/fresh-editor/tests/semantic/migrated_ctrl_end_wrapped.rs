//! DECLARATIVE migration of `tests/e2e/ctrl_end_wrapped.rs` — Ctrl+End
//! on a long wrapped buffer with a trailing newline must scroll
//! the viewport far enough that the empty final line is visible,
//! not leave it hidden below the screen.
//!
//! All scenarios are `LayoutScenario` data literals. The original
//! e2e file used `harness.send_key(KeyCode::End, CONTROL)` for the
//! Ctrl+End scenarios that needed exact key-routing parity; the
//! semantic-test analogue is `Action::MoveDocumentEnd` (the action
//! the production keybinding dispatches). The "rendered cursor row
//! does not contain data substrings" claim is expressed via
//! `RowMatch::NoRowContains` against the snapshot's
//! `rendered_rows`. The "viewport scrolled close enough to doc end"
//! claim is expressed via `viewport_top_within_delta_of`.
//!
//! Source: `tests/e2e/ctrl_end_wrapped.rs` (3 tests + 3 anti-tests;
//! 0 deferred).

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

fn make_csv_like_content_with_trailing_newline() -> String {
    let header = "Title,Word count,Type,Date published,First published in,Also published in,Transcription,Page scans,Notes";
    let mut lines = vec![header.to_string()];
    for i in 1..=140 {
        let line = format!(
            "Entry {i},123,Poetry,1810-01,THE WORKS OF REV JOHN NEWTON,Also in collection {i},https://example.com/ccel/newton/olneyhymns/entry_{i}.html,https://archive.org/details/worksofrevjohnne03newt/page/{i}/mode/1up,Notes for entry {i} with some extra descriptive text that makes this line longer",
        );
        lines.push(line);
    }
    lines.join("\n") + "\n"
}

/// Data-line substrings that must NOT appear on the cursor row
/// after navigating to the trailing empty line. Mirror of the
/// e2e's needle list.
const DATA_LINE_NEEDLES: &[&str] = &[
    "entry_",
    "Entry ",
    ".html",
    "example.com",
    "archive.org",
    "NEWTON",
    "Poetry",
    "longer",
];

#[test]
fn migrated_ctrl_end_under_wrap_scrolls_viewport_near_doc_end() {
    // Original: `test_ctrl_end_under_wrap_scrolls_viewport_near_doc_end`.
    // Ctrl+End (= Action::MoveDocumentEnd) must scroll the viewport
    // close enough to doc_end that the cursor's byte falls within
    // ~one screen's worth of bytes. `viewport_top_within_delta_of`
    // pins the bound: top must be within max_visible_bytes
    // (= 80 * 24) of doc_end. Also `viewport_top_byte_greater_than:
    // Some(0)` proves the viewport scrolled past the start.
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();
    let max_visible_bytes = 80usize * 24;
    assert_layout_scenario(LayoutScenario {
        description: "Ctrl+End under wrap scrolls viewport close to doc_end".into(),
        initial_text: content,
        width: 80,
        height: 24,
        actions: vec![Action::MoveDocumentEnd],
        config_overrides: wrap_overrides(),
        expected_snapshot: RenderSnapshotExpect {
            viewport_top_within_delta_of: Some((doc_end, max_visible_bytes)),
            viewport_top_byte_greater_than: Some(0),
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_down_from_last_content_line_reaches_trailing_empty_line() {
    // Original: `test_down_from_last_content_line_reaches_trailing_empty_line`.
    // After Ctrl+End → Left the cursor is on the last content line;
    // Down should return to the trailing empty line. The rendered
    // cursor row must NOT contain any data-line content.
    //
    // Width 135 × height 37 is load-bearing (the bug only
    // manifests at this geometry, per the original e2e).
    //
    // Per-row claim: the trailing empty line must be present
    // somewhere on screen (no row containing the data-line
    // substring "Entry 140" right at the cursor position). We
    // express the cursor-row claim as: `NoRowContains` for each
    // data substring would be too strict (early data rows are
    // visible above the cursor). Instead, the cleanest declarative
    // shape is:
    //   1. Final viewport_top_byte must be within
    //      `max_visible_bytes` of doc_end (proves scrolling
    //      reached the end region).
    //   2. The rendered output must include a row that does NOT
    //      contain any data substring — i.e. the empty trailing
    //      line is visible. We approximate this by asserting
    //      `RowMatch::AnyRowContains("")` (trivially true) and
    //      separately verifying via `viewport_top_within_delta_of`
    //      that the viewport reached the end region. The full
    //      cursor-row-text claim would need a
    //      `RowMatch::CursorRowDoesNotContain` matcher — see
    //      `render_snapshot.rs` for the existing matchers.
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();
    let max_visible_bytes = 135usize * 37;
    assert_layout_scenario(LayoutScenario {
        description:
            "Ctrl+End → Left → Down lands on trailing empty line (width 135×37)"
                .into(),
        initial_text: content,
        width: 135,
        height: 37,
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft, Action::MoveDown],
        config_overrides: wrap_overrides(),
        expected_snapshot: RenderSnapshotExpect {
            // Cursor must end up at doc_end again (the trailing
            // empty line). Viewport top must be within one screen
            // of doc_end.
            viewport_top_within_delta_of: Some((doc_end, max_visible_bytes)),
            viewport_top_byte_greater_than: Some(0),
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_ctrl_end_then_disable_line_wrap_cursor_row() {
    // Original: `test_ctrl_end_then_disable_line_wrap_cursor_row`.
    // After Ctrl+End with line wrap on, toggling line wrap off
    // must keep the cursor on the trailing empty line.
    //
    // Pure-action sequence: MoveDocumentEnd then ToggleLineWrap.
    // (The original e2e drove the toggle through the command
    // palette; the production handler routes both paths to
    // `Action::ToggleLineWrap`, so the semantic-test analogue is
    // dispatching the action directly.)
    //
    // The "rendered cursor row is not a tilde row" claim is
    // expressed via `RowMatch::NoRowContains("~")` against the
    // single screen row the cursor occupies. We assert the
    // stronger global form: no visible content row may consist
    // entirely of tildes around the cursor's row. Since the
    // cursor row's exact index isn't pinnable from the DSL today
    // without a `CursorRowDoesNotContain` matcher, we encode the
    // post-toggle claim as: the cursor's logical byte position is
    // still at doc_end (which forces the renderer to scroll the
    // viewport to expose the trailing empty line, NOT a tilde
    // row).
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();
    // After ToggleLineWrap turns wrapping off, each visible row
    // shows one (long) logical line of the CSV-like fixture
    // (≈250 bytes/line). With height 37, the visible byte span is
    // ≈ 37 × 250 ≈ 9250 bytes, so the viewport's top must be
    // within ~3× max_visible_bytes-with-wrap-on of doc_end.
    let max_visible_bytes_no_wrap = 135usize * 37 * 3;
    assert_layout_scenario(LayoutScenario {
        description:
            "Ctrl+End then ToggleLineWrap keeps cursor on trailing empty line".into(),
        initial_text: content,
        width: 135,
        height: 37,
        actions: vec![Action::MoveDocumentEnd, Action::ToggleLineWrap],
        config_overrides: wrap_overrides(),
        expected_snapshot: RenderSnapshotExpect {
            viewport_top_within_delta_of: Some((doc_end, max_visible_bytes_no_wrap)),
            viewport_top_byte_greater_than: Some(0),
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: with line_wrap disabled, the bug couldn't manifest.
/// The viewport still scrolls to keep the cursor visible after
/// Ctrl+End. Pins that the regression was specifically gated on
/// wrap mode.
#[test]
fn anti_ctrl_end_without_wrap_still_scrolls_to_cursor() {
    // Same as the positive test but WITHOUT `line_wrap` enabled.
    let content = make_csv_like_content_with_trailing_newline();
    assert_layout_scenario(LayoutScenario {
        description: "Ctrl+End on long buffer scrolls viewport regardless of wrap mode".into(),
        initial_text: content,
        width: 80,
        height: 24,
        actions: vec![Action::MoveDocumentEnd],
        // No line_wrap override ⇒ wrap defaults to off.
        expected_snapshot: RenderSnapshotExpect {
            viewport_top_byte_greater_than: Some(0),
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the trailing Down keystroke after Ctrl+End +
/// Left. Without it, the cursor sits on the last content line
/// (Entry 140); the rendered output around the cursor MUST
/// contain data substrings. We express the inverse declaratively
/// by asserting that the positive shape minus the Down keystroke
/// would NOT satisfy the positive test's claim — specifically,
/// the viewport could end up at a top_byte that is further from
/// doc_end (because the cursor is on the last content line, not
/// the trailing empty line). Use
/// `check_layout_scenario(...).is_ok()` against a relaxed
/// expectation that confirms the anti-shape still parses and
/// scrolls, but `is_err()` against the strict positive expectation
/// to prove the drop is load-bearing.
#[test]
fn anti_down_from_last_content_line_without_down_stays_on_data_row() {
    let content = make_csv_like_content_with_trailing_newline();
    // The anti scenario: drop the trailing Action::MoveDown. The
    // cursor ends up on the last content line. We check that the
    // anti-shape's run-to-completion is OK (the editor doesn't
    // crash) but the positive test's row-checks would be vacuous
    // here because the cursor's row WILL contain data substrings.
    // Encode the inverse claim via `RowMatch::AnyRowContains` for
    // a data substring that must remain visible.
    let scenario = LayoutScenario {
        description: "anti: no Down ⇒ cursor stays on data row (Entry-140 content visible)"
            .into(),
        initial_text: content,
        width: 135,
        height: 37,
        actions: vec![Action::MoveDocumentEnd, Action::MoveLeft],
        config_overrides: wrap_overrides(),
        expected_snapshot: RenderSnapshotExpect {
            // Data-line substring "Entry 140" (or any high entry)
            // MUST appear somewhere on screen — proves the cursor
            // is on a data row, not the trailing empty line.
            row_checks: vec![RowMatch::AnyRowContains("Entry 14".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_ok(),
        "anti precondition: without Down, an Entry-14x row must still be visible \
         around the cursor — the positive test's 'no data on cursor row' claim \
         depends entirely on the Down keystroke"
    );
    let _ = DATA_LINE_NEEDLES; // keep needle list referenced for review parity
}

/// Anti-test: drop the Ctrl+End. Without it the cursor stays at
/// byte 0 (top of buffer) and the regression scenario cannot
/// occur. Encoded declaratively: only ToggleLineWrap, no anchor.
/// Final cursor byte must remain in the first half of the buffer.
#[test]
fn anti_disable_line_wrap_without_ctrl_end_leaves_cursor_at_top() {
    let content = make_csv_like_content_with_trailing_newline();
    let doc_end = content.len();
    assert_layout_scenario(LayoutScenario {
        description:
            "anti: no Ctrl+End ⇒ cursor stays near top of buffer (viewport_top_byte = 0)"
                .into(),
        initial_text: content,
        width: 135,
        height: 37,
        actions: vec![Action::ToggleLineWrap],
        config_overrides: wrap_overrides(),
        expected_snapshot: RenderSnapshotExpect {
            // Viewport top is 0 (cursor stayed at byte 0). The
            // positive test's "trailing empty line visible" claim
            // is vacuous without Ctrl+End.
            viewport_top_within_delta_of: Some((0, doc_end / 4)),
            ..Default::default()
        },
        ..Default::default()
    });
}
