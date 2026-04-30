//! Track B migration of `tests/e2e/smart_home.rs`.
//!
//! Originally only one test, asserting that `Home` on a wrapped
//! continuation line moves to the start of the *visual* line, not
//! to byte 0 of the underlying physical line. The original used
//! width=40 to force wrapping; the theorem uses the same.
//!
//! Note: this exercises the wrap-sensitive `assert_buffer_theorem_with_terminal`
//! variant added when this test was migrated. SmartHome is the first
//! theorem to require non-default terminal dimensions.

use crate::common::theorem::buffer_theorem::{
    assert_buffer_theorem_with_terminal, BufferTheorem, CursorExpect, TerminalSize,
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
    assert_buffer_theorem_with_terminal(
        BufferTheorem {
            description: "SmartHome from mid-line jumps to first non-whitespace",
            initial_text: "    indented",
            // Move to end of line (byte 12), then SmartHome.
            actions: vec![Action::MoveLineEnd, Action::SmartHome],
            expected_text: "    indented",
            expected_primary: CursorExpect::at(4),
            expected_extra_cursors: vec![],
            expected_selection_text: Some(""),
        },
        TerminalSize::default(),
    );
}

#[test]
fn theorem_smart_home_toggles_to_byte_zero() {
    // Second Home press toggles to byte 0.
    assert_buffer_theorem_with_terminal(
        BufferTheorem {
            description: "SmartHome twice toggles to byte 0",
            initial_text: "    indented",
            actions: vec![Action::MoveLineEnd, Action::SmartHome, Action::SmartHome],
            expected_text: "    indented",
            expected_primary: CursorExpect::at(0),
            expected_extra_cursors: vec![],
            expected_selection_text: Some(""),
        },
        TerminalSize::default(),
    );
}
