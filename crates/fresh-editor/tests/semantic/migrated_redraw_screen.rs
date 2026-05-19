//! Migration of `tests/e2e/redraw_screen.rs` — the "Redraw Screen"
//! action (issue #1070).
//!
//! Two load-bearing claims, both preserved here:
//!
//!   1. Dispatching `Action::RedrawScreen` must flip the editor's
//!      full-redraw flag so the main loop clears the terminal on the
//!      next tick. The flag is observed via the harness-direct
//!      `editor_mut().take_full_redraw_request()` accessor — the
//!      production-internal `Editor` field has no `EditorTestApi`
//!      projection (it's a one-shot flag consumed by the event
//!      loop, not a stable observable).
//!
//!   2. The "Redraw Screen" entry must be discoverable from the
//!      command palette (Ctrl+P → "redraw"). Migrated using
//!      `RenderSnapshot::extract_with_rendered_rows` +
//!      `RowMatch::AnyRowContains` so the assertion runs against
//!      the real vt100 round-trip output, matching the e2e
//!      `assert_screen_contains` semantics.
//!
//! Anti-test drops the `Action::RedrawScreen` dispatch and proves
//! the full-redraw flag stays false — the action is what flips
//! it, not some incidental harness state.
//!
//! Tracks the orphan that `migrated_terminal_io.rs` previously
//! noted as unmigrated.

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::Action;

#[test]
fn migrated_redraw_screen_action_requests_full_redraw() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Sanity: no redraw pending on a fresh editor.
    assert!(
        !harness.editor_mut().take_full_redraw_request(),
        "fresh editor must not have a pending full-redraw request"
    );

    harness.api_mut().dispatch(Action::RedrawScreen);

    assert!(
        harness.editor_mut().take_full_redraw_request(),
        "Action::RedrawScreen must flip the full-redraw flag so the event \
         loop clears and repaints the terminal on the next tick"
    );
}

#[test]
fn migrated_redraw_screen_visible_in_command_palette() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("redraw").unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains("Redraw Screen".into())],
        ..Default::default()
    };
    if let Some((field, expected, actual)) = expect.check_against(&snap) {
        panic!(
            "Redraw Screen palette entry missing: {field} expected {expected}; \
             actual {actual}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

/// Anti-test: drop the `Action::RedrawScreen` dispatch. Without it,
/// `take_full_redraw_request()` must stay false — proves the action
/// dispatch is what flips the flag, not harness construction or some
/// other incidental side-effect.
#[test]
fn anti_redraw_screen_without_dispatch_keeps_redraw_flag_unset() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // No Action::RedrawScreen dispatch here.
    assert!(
        !harness.editor_mut().take_full_redraw_request(),
        "anti: without Action::RedrawScreen the full-redraw flag must NOT \
         be set on a fresh editor"
    );
}

/// Anti-test: open the command palette but type a query that doesn't
/// match the redraw entry. The rendered rows must NOT contain
/// "Redraw Screen" — proves the visibility check in the positive
/// test is gated on the query "redraw", not on the palette being
/// open at all.
#[test]
fn anti_redraw_screen_palette_with_unrelated_query_hides_entry() {
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    // A query that can't match "Redraw Screen" under fuzzy palette
    // matching.
    harness.type_text("xyznomatch").unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::NoRowContains("Redraw Screen".into())],
        ..Default::default()
    };
    if let Some((field, expected, actual)) = expect.check_against(&snap) {
        panic!(
            "anti: palette with unrelated query should hide Redraw Screen, \
             but: {field} expected {expected}; actual {actual}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}
