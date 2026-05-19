//! DECLARATIVE: migrates `tests/e2e/popup_wrap_indent.rs` — popup
//! line wrapping must preserve a hanging indent on continuation
//! lines.
//!
//! Load-bearing claim: when a long indented line in a `Centered`
//! popup wraps because the popup `width` is narrower than the line,
//! the continuation visual row must start with at least as many
//! leading spaces as the source line. Without this, wrapped
//! signature-help parameter descriptions visually merge into the
//! next entry.
//!
//! Pure `LayoutScenario` data; the runner injects the popup via
//! `LayoutScenario::show_popup` (becomes `Event::ShowPopup` on the
//! active buffer) and the `RenderSnapshotExpect::popup_hanging_indent`
//! matcher locates the source row + a wrapped continuation row and
//! verifies the hanging-indent invariant against the per-row text
//! the terminal renders.
//!
//! Source: `tests/e2e/popup_wrap_indent.rs` (1 test migrated; no
//! tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, PopupSpec,
};
use crate::common::scenario::render_snapshot::{PopupHangingIndent, RenderSnapshotExpect};

/// The signature-help-style fixture: a brief header, a separator,
/// and two indented parameter descriptions long enough that a
/// 40-col popup must wrap them.
fn popup_lines() -> Vec<String> {
    vec![
        "print(*values, sep, end, file, flush)".to_string(),
        "---".to_string(),
        "    sep  string inserted between values, default a space, used to join all output values together".to_string(),
        "    end  string appended after the last value, default a newline character sequence".to_string(),
    ]
}

#[test]
fn migrated_popup_wrapped_lines_have_hanging_indent() {
    // Narrow terminal (60 cols) so the 40-wide popup forces wrapping
    // of the long indented parameter descriptions. The source "sep"
    // line is indented 4 spaces; its wrapped continuation must
    // inherit at least 4 leading spaces inside the popup border.
    assert_layout_scenario(LayoutScenario {
        description:
            "40-col popup wraps the 'sep' parameter; continuation row must have hanging indent >= 4"
                .into(),
        initial_text: String::new(),
        width: 60,
        height: 24,
        actions: vec![],
        show_popup: Some(PopupSpec {
            title: Some("Signature Help".into()),
            lines: popup_lines(),
            width: 40,
            max_height: 20,
            bordered: true,
            position: Default::default(),
        }),
        expected_snapshot: RenderSnapshotExpect {
            popup_hanging_indent: Some(PopupHangingIndent {
                anchor_substring: "sep  string inserted".into(),
                // Either of the wrapped-portion fragments uniquely
                // identifies a continuation row; matching either is
                // enough to pin the hanging-indent claim.
                continuation_substring: "output values".into(),
                min_leading_spaces: 4,
            }),
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop `show_popup`. Without the popup, no row may
/// contain the "sep  string inserted" fixture text, so the
/// `popup_hanging_indent` matcher fails ("anchor row not found").
/// Proves the visibility checked in the positive test depends on
/// actually showing the popup, not on incidental editor state.
#[test]
fn anti_popup_wrap_indent_without_show_popup_renders_no_popup_content() {
    let scenario = LayoutScenario {
        description: "anti: no show_popup → anchor row 'sep  string inserted' is absent".into(),
        initial_text: String::new(),
        width: 60,
        height: 24,
        actions: vec![],
        // Deliberately no show_popup — the load-bearing step we drop.
        expected_snapshot: RenderSnapshotExpect {
            popup_hanging_indent: Some(PopupHangingIndent {
                anchor_substring: "sep  string inserted".into(),
                continuation_substring: "output values".into(),
                min_leading_spaces: 4,
            }),
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without Event::ShowPopup the 'sep  string inserted' anchor row should not exist on screen"
    );
}
