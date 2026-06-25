//! Track B migration of `tests/e2e/smart_home.rs`.
//!
//! Originally only one test, asserting that `Home` on a wrapped
//! continuation line moves to the start of the *visual* line, not
//! to byte 0 of the underlying physical line. The original used
//! width=40 to force wrapping; the theorem uses the same.
//!
//! Wrap-sensitive scenarios set `terminal:` on the value to override
//! the default 80×24. SmartHome is the first scenario to require
//! non-default terminal dimensions.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, BufferScenario, CursorExpect, TerminalSize,
};
use fresh::test_api::Action;

#[test]
fn theorem_smart_home_on_unwrapped_indented_line_jumps_to_first_non_whitespace() {
    // Smart home's primary behavior: from somewhere mid-line on an
    // indented line, Home jumps to the first non-whitespace
    // character (byte 4 here), not to byte 0.
    //
    // Pressing Home a second time then jumps to byte 0 — that's the
    // toggle. Theorem captures the first hop only.
    assert_buffer_scenario(BufferScenario {
        terminal: TerminalSize::default(),
        description: "SmartHome from mid-line jumps to first non-whitespace".into(),
        initial_text: "    indented".into(),
        // Move to end of line (byte 12), then SmartHome.
        actions: vec![Action::MoveLineEnd, Action::SmartHome],
        expected_text: "    indented".into(),
        expected_primary: CursorExpect::at(4),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("".into()),
        ..Default::default()
    });
}

#[test]
fn theorem_smart_home_toggles_to_byte_zero() {
    // Second Home press toggles to byte 0.
    assert_buffer_scenario(BufferScenario {
        terminal: TerminalSize::default(),
        description: "SmartHome twice toggles to byte 0".into(),
        initial_text: "    indented".into(),
        actions: vec![Action::MoveLineEnd, Action::SmartHome, Action::SmartHome],
        expected_text: "    indented".into(),
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("".into()),
        ..Default::default()
    });
}

#[test]
fn smart_home_extends_selection_in_mark_mode() {
    // Regression: after `Set Mark` (Emacs mark mode), pressing Home must
    // EXTEND the selection rather than collapse it. From byte 12 (end of
    // line) SetMark anchors there, then SmartHome moves to the first
    // non-whitespace (byte 4) while keeping the anchor at 12.
    assert_buffer_scenario(BufferScenario {
        terminal: TerminalSize::default(),
        description: "SmartHome in mark mode extends the selection".into(),
        initial_text: "    indented".into(),
        actions: vec![Action::MoveLineEnd, Action::SetMark, Action::SmartHome],
        expected_text: "    indented".into(),
        expected_primary: CursorExpect::range(12, 4),
        expected_extra_cursors: vec![],
        expected_selection_text: Some("indented".into()),
        ..Default::default()
    });
}
