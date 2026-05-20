//! DECLARATIVE: Migration of `tests/e2e/redraw_screen.rs` — the
//! "Redraw Screen" action (issue #1070).
//!
//! Two load-bearing claims preserved here:
//!
//!   1. Dispatching `Action::RedrawScreen` must flip the editor's
//!      full-redraw flag so the main event loop clears the terminal
//!      on the next tick. The flag is exposed on the test API as
//!      `EditorTestApi::take_full_redraw_request_for_tests()` (a
//!      one-shot accessor that consumes the flag and returns its
//!      previous value), and asserted declaratively via
//!      `LayoutScenario::expected_full_redraw_requested`.
//!
//!   2. The "Redraw Screen" entry must be discoverable from the
//!      command palette (Ctrl+P → "redraw"). The palette is
//!      opened via `Action::CommandPalette`, "redraw" is typed by
//!      a sequence of `Action::InsertChar(_)` (the editor routes
//!      `InsertChar` into the active prompt), and the row is
//!      asserted via `RowMatch::AnyRowContains`.
//!
//! Anti-tests drop the load-bearing step (the `RedrawScreen`
//! dispatch / the "redraw" filter) and assert the inverse.
//!
//! Source: `tests/e2e/redraw_screen.rs` (2 positive tests + 2
//! anti-tests; no tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

/// Build the `Action` sequence that opens the command palette and
/// types `query` into it. The editor routes `InsertChar` to the
/// active prompt when one is open, so a sequence of
/// `[CommandPalette, InsertChar('r'), InsertChar('e'), ...]` is
/// equivalent to `harness.send_key(Char('p'), CONTROL) +
/// type_text("redraw")`.
fn palette_query(query: &str) -> Vec<Action> {
    let mut actions = vec![Action::CommandPalette];
    actions.extend(query.chars().map(Action::InsertChar));
    actions
}

#[test]
fn migrated_redraw_screen_action_requests_full_redraw() {
    assert_layout_scenario(LayoutScenario {
        description: "Action::RedrawScreen sets the full-redraw flag".into(),
        initial_text: String::new(),
        width: 80,
        height: 24,
        actions: vec![Action::RedrawScreen],
        expected_full_redraw_requested: Some(true),
        ..Default::default()
    });
}

#[test]
fn migrated_redraw_screen_visible_in_command_palette() {
    assert_layout_scenario(LayoutScenario {
        description: "Command palette filter 'redraw' surfaces the Redraw Screen entry".into(),
        initial_text: String::new(),
        width: 100,
        height: 24,
        actions: palette_query("redraw"),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Redraw Screen".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the `Action::RedrawScreen` dispatch. With no
/// dispatch, `take_full_redraw_request_for_tests()` must return
/// `false` — proves the positive test's flag assertion is gated on
/// the action, not on harness construction.
#[test]
fn anti_redraw_screen_without_dispatch_keeps_redraw_flag_unset() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: no Action::RedrawScreen — full-redraw flag stays false".into(),
        initial_text: String::new(),
        width: 80,
        height: 24,
        actions: vec![],
        expected_full_redraw_requested: Some(false),
        ..Default::default()
    });
}

/// Anti-test: open the command palette but type a query that can't
/// match "Redraw Screen" under fuzzy matching. The rendered rows
/// must NOT contain "Redraw Screen" — proves the positive test's
/// visibility assertion is gated on the filter being "redraw", not
/// on the palette being open at all.
#[test]
fn anti_redraw_screen_palette_with_unrelated_query_hides_entry() {
    let scenario = LayoutScenario {
        description: "anti: filter 'xyznomatch' hides Redraw Screen palette entry".into(),
        initial_text: String::new(),
        width: 100,
        height: 24,
        actions: palette_query("xyznomatch"),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Redraw Screen".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: with an unrelated filter, the Redraw Screen palette entry must be \
         absent — RowMatch::AnyRowContains('Redraw Screen') should fail."
    );
}
