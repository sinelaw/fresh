//! DECLARATIVE: Migration of `tests/e2e/undo_redo_marker_roundtrip.rs`
//! — single-edit + bulk-edit operations under undo/redo must preserve
//! the byte position of a margin marker.
//!
//! Every test in this file is a `MarkerRoundtripScenario { ... }`
//! literal: pure data, dispatched by
//! `assert_marker_roundtrip_scenario`. Marker seeding and readback go
//! through `EditorTestApi::seed_marker` / `marker_positions`; cursor
//! placement and edit ops go through `EditorTestApi::dispatch`. No
//! `EditorTestHarness::editor_mut()` calls, no `MarkerId` /
//! `LineIndicator` imports — the scenario type owns the
//! direct-harness contact surface.
//!
//! The "post-op marker is some value, then Undo restores the pre-op
//! value and Redo returns to that same value" claim is encoded with
//! `SaveMarkers` + `AssertMarkersMatchSaved` so the scenario doesn't
//! have to hard-code the editor's internal post-op marker position —
//! same shape as the e2e `verify_roundtrip` helper that captured
//! `post_op_marker` in a local.
//!
//! Tracks orphan in #2058 — closes the last marker-roundtrip orphan.

use crate::common::scenario::marker_roundtrip_scenario::{
    assert_marker_roundtrip_scenario, check_marker_roundtrip_scenario, MarkerRoundtripScenario,
    MarkerSeed, MarkerStep,
};
use fresh::test_api::Action;

const MARKER: &str = "●";

/// "Seed marker at byte 0, then walk pre-op + post-op + undo + redo
/// assertions" — the standard 1-op roundtrip shape used by 4 of the
/// 6 deterministic e2e tests.
fn roundtrip_steps_single_edit(
    op: Action,
    pre_op_text: &str,
    pre_op_markers: Vec<usize>,
    post_op_text: &str,
    post_op_markers: Vec<usize>,
) -> Vec<MarkerStep> {
    vec![
        // t=0
        MarkerStep::AssertText(pre_op_text.into()),
        MarkerStep::AssertMarkers {
            symbol: MARKER.into(),
            positions: pre_op_markers.clone(),
        },
        // Apply the op.
        MarkerStep::Op(op),
        MarkerStep::AssertText(post_op_text.into()),
        MarkerStep::AssertMarkers {
            symbol: MARKER.into(),
            positions: post_op_markers,
        },
        // Undo: back to pre-op.
        MarkerStep::Undo,
        MarkerStep::AssertText(pre_op_text.into()),
        MarkerStep::AssertMarkers {
            symbol: MARKER.into(),
            positions: pre_op_markers,
        },
        // Redo: back to post-op (asserted on saved post-op state).
        MarkerStep::Redo,
        MarkerStep::AssertText(post_op_text.into()),
    ]
}

#[test]
fn migrated_marker_roundtrip_under_single_edits_typechar() {
    // Original: tests/e2e/undo_redo_marker_roundtrip.rs
    // test_each_single_edit_op_marker_roundtrip — TypeChar arm.
    // Setup: "aaa\nbbb\nccc", cursor at start of line 2, marker at
    // byte 0. Type 'X' at cursor: "aaa\nXbbb\nccc". Marker survives
    // Undo/Redo at byte 0.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "single-edit TypeChar preserves marker @0 under Undo/Redo".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: roundtrip_steps_single_edit(
            Action::InsertChar('X'),
            "aaa\nbbb\nccc",
            vec![0],
            "aaa\nXbbb\nccc",
            vec![0],
        ),
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_single_edits_backspace() {
    // Original arm: Backspace deletes char before cursor.
    // Setup: cursor at end of line 2 (byte 7), marker at 0. Backspace
    // deletes 'b' at byte 6: "aaa\nbb\nccc". Marker stays at 0.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "single-edit Backspace preserves marker @0 under Undo/Redo".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown, Action::MoveLineEnd],
        steps: roundtrip_steps_single_edit(
            Action::DeleteBackward,
            "aaa\nbbb\nccc",
            vec![0],
            "aaa\nbb\nccc",
            vec![0],
        ),
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_single_edits_delete() {
    // Original arm: Delete deletes char at cursor.
    // Setup: cursor at start of line 2 (byte 4), marker at 0. Delete
    // 'b' at byte 4: "aaa\nbb\nccc". Marker stays at 0.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "single-edit Delete preserves marker @0 under Undo/Redo".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: roundtrip_steps_single_edit(
            Action::DeleteForward,
            "aaa\nbbb\nccc",
            vec![0],
            "aaa\nbb\nccc",
            vec![0],
        ),
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_single_edits_enter() {
    // Original arm: Enter inserts newline at cursor.
    // Setup: cursor at start of line 2 (byte 4), marker at 0. Newline
    // inserted: "aaa\n\nbbb\nccc". Marker stays at 0.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "single-edit Enter preserves marker @0 under Undo/Redo".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: roundtrip_steps_single_edit(
            Action::InsertNewline,
            "aaa\nbbb\nccc",
            vec![0],
            "aaa\n\nbbb\nccc",
            vec![0],
        ),
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_bulk_edits_move_line_down() {
    // Original: test_each_bulk_edit_op_marker_roundtrip — MoveLineDown
    // arm. Marker at 0 on the "aaa" line. cursor on line 2 (byte 4)
    // initiates the MoveLineDown of line 2 ("bbb") — content becomes
    // "aaa\nccc\nbbb". The marker (anchored to byte 0 on the
    // unchanged "aaa" line) stays at 0. Use Save/AssertMatchSaved so
    // the scenario doesn't pre-compute the internal post-op value.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "bulk MoveLineDown preserves marker @0 under Undo/Redo".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: vec![
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![0],
            },
            MarkerStep::SaveText { slot: "pre".into() },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "pre".into(),
            },
            MarkerStep::Op(Action::MoveLineDown),
            MarkerStep::SaveText {
                slot: "post".into(),
            },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
            MarkerStep::Undo,
            MarkerStep::AssertTextMatchSaved { slot: "pre".into() },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "pre".into(),
            },
            MarkerStep::Redo,
            MarkerStep::AssertTextMatchSaved {
                slot: "post".into(),
            },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
        ],
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_bulk_edits_move_line_up() {
    // Original: test_each_bulk_edit_op_marker_roundtrip — MoveLineUp.
    // Same setup, MoveLineUp instead. Marker at 0 (on "aaa"). After
    // MoveLineUp from cursor on line 2: "bbb\naaa\nccc" — "aaa" now
    // starts at byte 4, the marker rides with it. The roundtrip
    // claim is Undo/Redo returns the marker to its captured values.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "bulk MoveLineUp preserves marker under Undo/Redo".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: vec![
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::SaveText { slot: "pre".into() },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "pre".into(),
            },
            MarkerStep::Op(Action::MoveLineUp),
            MarkerStep::SaveText {
                slot: "post".into(),
            },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
            MarkerStep::Undo,
            MarkerStep::AssertTextMatchSaved { slot: "pre".into() },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "pre".into(),
            },
            MarkerStep::Redo,
            MarkerStep::AssertTextMatchSaved {
                slot: "post".into(),
            },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
        ],
        ..Default::default()
    });
}

#[test]
fn migrated_marker_at_end_of_buffer_under_enter_then_typechar_then_movelineup() {
    // Original: test_enter_typechar_movelineup_marker_at_end.
    // Regression: a marker at the end of buffer must survive
    // [Enter, '}', Home, Alt+Up] then 3 Undos back to the orig
    // marker position. Marker seeded at byte 11 (end of
    // "aaa\nbbb\nccc"), cursor at byte 0.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "marker @end-of-buffer survives Enter/'}'/Alt+Up then 3 Undos".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(11, MARKER)],
        // Cursor already at byte 0 after load_buffer_from_text; no
        // setup needed.
        cursor_setup: vec![],
        steps: vec![
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![11],
            },
            MarkerStep::Op(Action::InsertNewline),
            MarkerStep::Op(Action::InsertChar('}')),
            MarkerStep::Op(Action::MoveLineStart),
            MarkerStep::Op(Action::MoveLineUp),
            // 3 Undos walk back through the bulk MoveLineUp + the
            // two single edits (Enter, '}').
            MarkerStep::Undo,
            MarkerStep::Undo,
            MarkerStep::Undo,
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![11],
            },
        ],
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_move_line_down() {
    // Original: test_move_line_down_marker_roundtrip. Marker at byte
    // 0 ("aaa"); MoveLineDown swaps "aaa" with "bbb": "bbb\naaa\nccc"
    // with the marker following "aaa" to byte 4. Undo restores marker
    // to 0; Redo re-applies. Cursor on line 1 (byte 0) initiates the
    // MoveLineDown of line 1.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "MoveLineDown of line 1 carries marker @0 to byte 4 then roundtrips".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![],
        steps: vec![
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![0],
            },
            MarkerStep::Op(Action::MoveLineDown),
            MarkerStep::AssertText("bbb\naaa\nccc".into()),
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
            MarkerStep::Undo,
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![0],
            },
            MarkerStep::Redo,
            MarkerStep::AssertText("bbb\naaa\nccc".into()),
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
        ],
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_under_move_line_up() {
    // Original: test_move_line_up_marker_roundtrip. Marker at byte 4
    // ("bbb" line); MoveLineUp from cursor on line 2 swaps "bbb" with
    // "aaa": "bbb\naaa\nccc". Undo restores marker to 4. Redo
    // re-applies.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "MoveLineUp of line 2 carries marker @4 then roundtrips".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(4, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: vec![
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![4],
            },
            MarkerStep::Op(Action::MoveLineUp),
            MarkerStep::AssertText("bbb\naaa\nccc".into()),
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
            MarkerStep::Undo,
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            MarkerStep::AssertMarkers {
                symbol: MARKER.into(),
                positions: vec![4],
            },
            MarkerStep::Redo,
            MarkerStep::AssertText("bbb\naaa\nccc".into()),
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "post".into(),
            },
        ],
        ..Default::default()
    });
}

#[test]
fn migrated_marker_roundtrip_through_interleaved_single_and_bulk_edits() {
    // Original: test_interleaved_single_and_bulk_edit_marker_roundtrip.
    // Walks Type 'X' + MoveLineDown, then verifies that individual
    // Undos (one per edit) restore the marker step-by-step, and
    // individual Redos re-apply step-by-step.
    //
    // Setup: text "aa\nbb", cursor at byte 0 (start of line 1),
    // marker at byte 0.
    //
    // Step 1: type 'X' at cursor → "Xaa\nbb"
    // Step 2: MoveLineDown of line 1 → "bb\nXaa"
    // Undo back to step1 (content + marker), then back to base.
    // Redo forward to step1, then to step2.
    assert_marker_roundtrip_scenario(MarkerRoundtripScenario {
        description: "interleaved single + bulk edits, step-by-step Undo/Redo".into(),
        initial_text: "aa\nbb".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![],
        steps: vec![
            // Capture base state.
            MarkerStep::AssertText("aa\nbb".into()),
            MarkerStep::SaveText {
                slot: "base".into(),
            },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "base".into(),
            },
            // Step 1: type 'X' → "Xaa\nbb"
            MarkerStep::Op(Action::InsertChar('X')),
            MarkerStep::SaveText {
                slot: "step1".into(),
            },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "step1".into(),
            },
            // Step 2: MoveLineDown → "bb\nXaa"
            MarkerStep::Op(Action::MoveLineDown),
            MarkerStep::SaveText {
                slot: "step2".into(),
            },
            MarkerStep::SaveMarkers {
                symbol: MARKER.into(),
                slot: "step2".into(),
            },
            // Undo step 2 (bulk) → back to step1.
            MarkerStep::Undo,
            MarkerStep::AssertTextMatchSaved {
                slot: "step1".into(),
            },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "step1".into(),
            },
            // Undo step 1 (single) → back to base.
            MarkerStep::Undo,
            MarkerStep::AssertTextMatchSaved {
                slot: "base".into(),
            },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "base".into(),
            },
            // Redo step 1 → step1.
            MarkerStep::Redo,
            MarkerStep::AssertTextMatchSaved {
                slot: "step1".into(),
            },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "step1".into(),
            },
            // Redo step 2 → step2.
            MarkerStep::Redo,
            MarkerStep::AssertTextMatchSaved {
                slot: "step2".into(),
            },
            MarkerStep::AssertMarkersMatchSaved {
                symbol: MARKER.into(),
                slot: "step2".into(),
            },
        ],
        ..Default::default()
    });
}

/// Anti-test: drop the typing op from the TypeChar scenario. The
/// runner's `AssertText` after the (missing) op must catch the
/// vacuous result — the post-op content equals the pre-op content,
/// not the expected "aaa\nXbbb\nccc". Pins that the marker-roundtrip
/// runner actually checks intermediate state, not just the
/// undo-then-redo endpoints.
#[test]
fn anti_marker_roundtrip_without_op_fails_post_op_assertion() {
    let scenario = MarkerRoundtripScenario {
        description: "anti: dropped TypeChar op must surface post-op text mismatch".into(),
        initial_text: "aaa\nbbb\nccc".into(),
        initial_markers: vec![MarkerSeed::red(0, MARKER)],
        cursor_setup: vec![Action::MoveDown],
        steps: vec![
            MarkerStep::AssertText("aaa\nbbb\nccc".into()),
            // No Op here — straight to a post-op assertion that
            // requires the X to have been inserted.
            MarkerStep::AssertText("aaa\nXbbb\nccc".into()),
        ],
        ..Default::default()
    };
    assert!(
        check_marker_roundtrip_scenario(scenario).is_err(),
        "anti-test: missing op must produce a text-mismatch failure"
    );
}
