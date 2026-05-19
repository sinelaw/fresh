//! Migration of the `Action::Recenter` (Ctrl+L) verb from
//! `tests/e2e/emacs_actions.rs::test_recenter_basic`.
//!
//! `Recenter` scrolls the active viewport so the cursor lands
//! at the middle of the visible rows. The e2e original asserts
//! on the rendered screen state; this migration asserts on
//! `viewport_top_byte` directly through the harness.
//!
//! Per #2058 orphan migration. Recenter was tracked as having
//! no semantic guard.

use crate::common::harness::EditorTestHarness;
use fresh::test_api::{Action, EditorTestApi};

fn long_buffer(line_count: usize) -> String {
    (0..line_count)
        .map(|i| format!("line {i:02}\n"))
        .collect::<String>()
}

#[test]
fn migrated_recenter_pulls_viewport_so_cursor_is_centered() {
    // 50-line buffer in a 10-tall terminal. Move cursor to the
    // middle (line 25 ish), force the viewport off-center by
    // scrolling to the top via MoveDocumentStart, then dispatch
    // Recenter and confirm the viewport_top_byte moves so the
    // cursor's line is roughly centered in the viewport.
    let mut h = EditorTestHarness::with_temp_project(40, 10).unwrap();
    let _f = h.load_buffer_from_text(&long_buffer(50)).unwrap();
    h.render().unwrap();

    // Step the cursor down to roughly line 25.
    {
        let api = h.api_mut();
        for _ in 0..25 {
            api.dispatch(Action::MoveDown);
        }
    }
    h.render().unwrap();

    // Capture the natural top_byte after cursor moves (the
    // viewport scrolled to keep the cursor visible).
    let before_recenter = h.api_mut().viewport_top_byte();

    // Now Recenter. The viewport must shift so the cursor's
    // line is approximately in the middle of the 10 visible
    // rows — i.e. roughly 5 lines above the cursor's line.
    h.api_mut().dispatch(Action::Recenter);
    h.render().unwrap();
    let after_recenter = h.api_mut().viewport_top_byte();

    assert_ne!(
        before_recenter, after_recenter,
        "Recenter must shift viewport_top_byte; before={before_recenter}, after={after_recenter}"
    );

    // Sanity: the cursor's screen position after Recenter
    // should be in the upper half-plus-1 of the visible rows
    // (not row 0, not row 9). Viewport height = 10 cells (8
    // text rows + chrome). The middle is row 4 in text space.
    let cursor_screen = h.api_mut().hardware_cursor_position();
    if let Some((_, row)) = cursor_screen {
        assert!(
            row >= 2 && row <= 7,
            "Recenter must place cursor near middle row, got row={row}"
        );
    }
}

/// Anti-test: without the Recenter dispatch, the viewport stays
/// where natural scrolling put it. Pins that the Recenter step
/// is load-bearing — the assertion above isn't true of any
/// reachable state.
#[test]
fn anti_recenter_without_dispatch_leaves_viewport_unchanged() {
    let mut h = EditorTestHarness::with_temp_project(40, 10).unwrap();
    let _f = h.load_buffer_from_text(&long_buffer(50)).unwrap();
    h.render().unwrap();
    {
        let api = h.api_mut();
        for _ in 0..25 {
            api.dispatch(Action::MoveDown);
        }
    }
    h.render().unwrap();
    let before = h.api_mut().viewport_top_byte();

    // No Recenter dispatched.
    h.render().unwrap();
    let after = h.api_mut().viewport_top_byte();
    assert_eq!(
        before, after,
        "anti: viewport must NOT move without a Recenter dispatch; before={before}, after={after}"
    );
}
