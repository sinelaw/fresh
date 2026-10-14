//! Regression coverage for PageDown / Up cursor motion on a buffer that
//! is a SINGLE very long soft-wrapped line (e.g. a minified JS bundle).
//!
//! Two user-reported bugs, both rooted in `Window::handle_page_motion`
//! (`app/action_events.rs`):
//!
//!   * **Bug #1 — PageDown overshoots.** With the cursor at the top of the
//!     document, pressing PageDown jumps the cursor all the way to the END
//!     of the buffer instead of advancing one page of visual rows. Cause:
//!     for a single logical line the scroll only advances
//!     `top_view_line_offset` (the wrap-segment index) — `top_byte` stays
//!     0 — so the "did the viewport move?" check (which compared only
//!     `top_byte`) concluded the viewport couldn't scroll and fell through
//!     to the logical-line `MovePageDown` handler, which on a one-line
//!     document clamps the cursor to EOF.
//!
//!   * **Bug #2 — Up after PageDown jumps to the top.** Because Bug #1 left
//!     the caret at EOF (far off-screen, so the wrap-aware visual-row
//!     intercept can't resolve it), pressing Up fell through to the
//!     byte-based logical-line MoveUp handler, which teleported the caret
//!     back to the start of the document.
//!
//! ## What is observed
//!
//! The bug is fundamentally about where the CARET lands, and `ensure_visible`
//! does not always scroll the viewport to follow a caret that jumped to EOF
//! (so a rendered-rows-only check can miss it). We therefore assert on
//! `cursor_byte` — `primary_caret().position`, the same observable the
//! original e2e movement tests use via `harness.cursor_position()`, and the
//! exact thing the user sees move. The fixture's single line is ~36KB, so a
//! one-page motion from the top lands the caret only a few thousand bytes in;
//! the bug slams it to ~36KB (EOF) for PageDown and back to ~0 for the
//! following Up.
//!
//! These scenarios drive `MovePageDown` / `MoveUp` through the real
//! `Window::action_to_events` path (via `EditorTestApi::dispatch`), with a
//! render between every step so the wrap-aware motion code sees a fresh
//! layout cache — exactly the conditions the bug needs.

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, LayoutScenario, ScenarioConfigOverrides, StepAssertion,
};
use crate::common::scenario::render_snapshot::RenderSnapshotExpect;
use fresh::test_api::Action;
use std::path::PathBuf;

/// The fixture is one ~36KB line. A single page of visual rows from the top
/// is at most a couple thousand bytes at any of the tested widths, so the
/// caret must stay well below this bound. The bug parks it at ~36KB (EOF).
const MAX_ONE_PAGE_BYTE: usize = 8_000;

fn wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

fn fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("single_long_wrapped_line.txt")
}

/// Bug #1: PageDown from the top of a single hugely-wrapped line must
/// advance roughly one page of visual rows — NOT clamp the caret to EOF.
///
/// `cursor_byte_in: (1, MAX_ONE_PAGE_BYTE)` enforces both halves: the caret
/// moved (> 0) and it did not jump to EOF (<= one page of bytes). Without
/// the fix the caret lands at ~36KB and the upper bound fails.
#[test]
fn page_down_on_single_long_line_does_not_jump_to_eof() {
    let widths: [u16; 3] = [50, 60, 80];
    let heights: [u16; 2] = [20, 28];
    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "PageDown on single long wrapped line stays near the top \
                     (width={width}, height={height})"
                ),
                initial_file: Some(fixture_path()),
                width,
                height,
                actions: vec![Action::MoveDocumentStart, Action::MovePageDown],
                config_overrides: wrap_overrides(),
                expected_snapshot: RenderSnapshotExpect {
                    cursor_byte_in: Some((1, MAX_ONE_PAGE_BYTE)),
                    ..Default::default()
                },
                ..Default::default()
            });
        }
    }
}

/// Bug #2: after PageDown, pressing Up must move the caret up one visual
/// row — NOT teleport it back to the start of the document.
///
/// `cursor_byte_in: (1, MAX_ONE_PAGE_BYTE)` enforces that the caret is still
/// roughly a page down (one visual row above the PageDown landing spot),
/// never at byte 0. Without the fix the cascade (PageDown→EOF, then Up→top)
/// drops the caret to byte 0 and the lower bound fails.
///
/// A render is forced after the PageDown (via `step_assertions`) so the
/// wrap-aware Up handler sees a layout cache reflecting the post-PageDown
/// viewport — the exact precondition under which the bug manifests.
#[test]
fn up_after_page_down_on_single_long_line_does_not_jump_to_top() {
    let widths: [u16; 3] = [50, 60, 80];
    let heights: [u16; 2] = [20, 28];
    let actions = vec![
        Action::MoveDocumentStart,
        Action::MovePageDown,
        Action::MoveUp,
    ];
    let step_assertions = vec![StepAssertion {
        after_action_index: 1,
        expect: RenderSnapshotExpect::default(),
    }];
    for &height in &heights {
        for &width in &widths {
            assert_layout_scenario(LayoutScenario {
                description: format!(
                    "Up after PageDown on single long wrapped line stays \
                     mid-document (width={width}, height={height})"
                ),
                initial_file: Some(fixture_path()),
                width,
                height,
                actions: actions.clone(),
                config_overrides: wrap_overrides(),
                step_assertions: step_assertions.clone(),
                expected_snapshot: RenderSnapshotExpect {
                    cursor_byte_in: Some((1, MAX_ONE_PAGE_BYTE)),
                    ..Default::default()
                },
                ..Default::default()
            });
        }
    }
}

/// Anti-test: with no page motion, the caret stays at the document start.
/// Pins that the positive tests' "caret is one page down, not at EOF/0"
/// claim is produced by the PageDown action — not by setup alone.
#[test]
fn anti_no_motion_keeps_caret_at_document_start() {
    assert_layout_scenario(LayoutScenario {
        description: "anti: no motion ⇒ caret at byte 0".into(),
        initial_file: Some(fixture_path()),
        width: 60,
        height: 20,
        actions: vec![Action::MoveDocumentStart],
        config_overrides: wrap_overrides(),
        expected_snapshot: RenderSnapshotExpect {
            cursor_byte: Some(0),
            ..Default::default()
        },
        ..Default::default()
    });
}
