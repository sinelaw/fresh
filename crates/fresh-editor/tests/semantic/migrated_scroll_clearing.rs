//! DECLARATIVE: Migration of `tests/e2e/scroll_clearing.rs` —
//! view-clearing invariants around overscroll, mouse-wheel
//! scrolling on top of keyboard navigation, and cursor / glyph
//! rendering on tab-prefixed lines.
//!
//! ## What's preserved declaratively
//!
//!   1. **No leftover content at end-of-file.** After
//!      `MoveDocumentEnd` + 5× `MovePageDown` + 10× `MoveDown`, the
//!      viewport must show the file's tail (`lfs_mirror_extend`)
//!      AND must NOT show `pool_to_id_cbdata` (defined on lines
//!      7-10 of the fixture).
//!   2. **Mouse-wheel scrolling reaches the buffer tail.** A run
//!      of `InputEvent::Mouse(Wheel { dy: -1, .. })` events
//!      delivered into the content area must scroll far enough to
//!      put `lfs_mirror_extend` on screen.
//!   3. **Tab indicator renders at every expanded tab position.**
//!      On the 3-tab-prefixed line of the fixture, the rendered
//!      output must contain the `→` indicator glyph.
//!   4. **Cursor at column 0 on a tab-prefixed line lands before
//!      the tab expansion.** After `MoveDown ×2 + MoveLineStart`
//!      (the file's smart-home behaviour treats a single MoveLineStart
//!      as "go to first non-whitespace, then column 0 on a second
//!      invocation if already at first non-ws"), the hardware
//!      cursor column must be in a tight range near the gutter
//!      width — NOT 7 columns further into the first expanded tab.
//!
//! ## What's deferred
//!
//! Two original e2e tests have NO `EditorTestApi` / declarative
//! equivalent today and stay in `tests/e2e/scroll_clearing.rs`:
//!
//!   * `test_mouse_wheel_after_keyboard_navigation` (Bug #248) —
//!     asserts on the *change* in `harness.top_line_number()`
//!     between a post-keyboard checkpoint and a post-mouse
//!     checkpoint. The declarative pipeline only exposes the final
//!     snapshot's `viewport_top_byte`, not a multi-point delta
//!     observation. Re-phrasing the invariant in terms of "after
//!     the full sequence, top_byte must be > X" requires hard-coded
//!     byte boundaries that depend on fixture geometry.
//!   * `test_leftover_characters_after_last_line` — walks every
//!     cell of every below-last-content row looking for non-
//!     whitespace, non-gutter, non-scrollbar characters. The
//!     `RowMatch` matchers don't have a per-cell allowlist; the
//!     translation would be a row-of-spaces equality check, but
//!     the exact line on which the buffer ends (and therefore the
//!     row at which "below last content" starts) depends on tab
//!     expansion details that vary across renderer changes.
//!
//! Plus two original tests are `println!`-only diagnostics
//! (`test_scroll_clearing_render_buffer_analysis`,
//! `test_scroll_clearing_real_terminal`) — no asserts, so there's
//! nothing to migrate; the anti-test discipline ("must fail when
//! the load-bearing action is dropped") has nothing to bind to.
//!
//! Source: `tests/e2e/scroll_clearing.rs` (4 of 8 tests migrated;
//! 2 deferred with note above + 2 diagnostic-only tests left in
//! e2e file).

use crate::common::scenario::context::MouseEvent;
use crate::common::scenario::input_event::InputEvent;
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

/// Path to the shared scroll fixture (47-line C source with tab
/// indentation on line 3 and a recognisable tail marker
/// `lfs_mirror_extend`).
fn scroll_test_file_path() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/scroll_test_file.txt")
}

fn many<T: Clone>(item: T, n: usize) -> Vec<T> {
    std::iter::repeat(item).take(n).collect()
}

#[test]
fn migrated_scroll_clearing_at_bottom_of_file_keeps_tail_and_clears_head() {
    // Original: `test_scroll_clearing_at_bottom_of_file`. Mirrors
    // the e2e fixture (scroll_test_file.txt), terminal size
    // (80x24), and action sequence:
    //   Ctrl+End → 5× PageDown → 10× Down.
    //
    // After settle:
    //   (a) `lfs_mirror_extend` (near line 45) is visible.
    //   (b) `pool_to_id_cbdata` (lines 7-10) is NOT visible.
    //
    // Pre-fix, the renderer left characters from earlier scroll
    // positions in cells the new draw didn't touch — so the head
    // marker would still appear even after Ctrl+End.
    let mut actions = vec![Action::MoveDocumentEnd];
    actions.extend(many(Action::MovePageDown, 5));
    actions.extend(many(Action::MoveDown, 10));

    assert_layout_scenario(LayoutScenario {
        description: "Ctrl+End + overscroll: tail visible, head cleared".into(),
        initial_text: String::new(), // unused — initial_file wins
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("lfs_mirror_extend".into()),
                RowMatch::NoRowContains("pool_to_id_cbdata".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_scroll_clearing_with_scroll_wheel_reaches_tail() {
    // Original: `test_scroll_clearing_with_scroll_wheel`. 20+
    // `MouseEventKind::ScrollDown` events at row=content_first+5
    // (≈ row 6 on 80x24) must bring `lfs_mirror_extend` on screen.
    let scroll_events: Vec<InputEvent> = (0..30)
        .map(|_| {
            InputEvent::Mouse(MouseEvent::Wheel {
                col: 40,
                row: 6,
                dy: -1,
            })
        })
        .collect();

    assert_layout_scenario(LayoutScenario {
        description: "30× wheel-down on scroll fixture: tail visible".into(),
        initial_text: String::new(),
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions: vec![],
        events: scroll_events,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("lfs_mirror_extend".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_tab_indicator_visible_on_tab_prefixed_line() {
    // Original: `test_tab_cursor_positioning_and_rendering` —
    // mirrors the load-bearing tab-indicator claim. Line 3 of the
    // fixture starts with three tabs followed by
    // `__u64 migration_flags`; the rendered output must contain
    // the `→` tab indicator glyph somewhere on screen.
    assert_layout_scenario(LayoutScenario {
        description: "tab-prefixed line 3 visible: '→' indicator rendered".into(),
        initial_text: String::new(),
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions: vec![
            Action::MoveDown,
            Action::MoveDown,
            Action::MoveLineStart,
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("→".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_cursor_at_column_zero_on_tab_line_lands_before_first_tab() {
    // Original: `test_cursor_before_first_tab`. Down,Down to reach
    // line 3 (tab-prefixed), then Home,Home — the first Home is
    // smart-home (jumps to first non-whitespace), the second goes
    // to true column 0. The hardware cursor's column must then be
    // in a tight band at the start of the content area (~gutter
    // width), NOT 7 columns further into the first expanded tab.
    //
    // 80x24 gutter for a 47-line file: 1 + max(2, 2) + 3 = 6
    // cells. The cursor at column 0 of a tab-prefixed line lands
    // at gutter_width (column 6) or gutter_width+1 (column 7) —
    // emphatically NOT at column 13 (= 6 + 7, where 7 is the
    // first tab's expanded width).
    assert_layout_scenario(LayoutScenario {
        description: "cursor at column 0 on tab-prefixed line: before first tab".into(),
        initial_text: String::new(),
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions: vec![
            Action::MoveDown,
            Action::MoveDown,
            Action::MoveLineStart,
            Action::MoveLineStart,
        ],
        expected_snapshot: RenderSnapshotExpect {
            // Cursor must land in cells 0..=7 (well before the
            // first tab expansion at column ~13). The exact column
            // depends on whether the gutter trailing-separator
            // counts, so we accept the inclusive range [0, 8].
            hardware_cursor_col_in: Some((0, 8)),
            // And the tab indicator must still render on the
            // cursor's line.
            row_checks: vec![RowMatch::AnyRowContains("→".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop `MoveDocumentEnd` + overscroll. Without
/// scrolling, the viewport stays at the top of the fixture and
/// the tail marker must NOT be visible — proves the positive
/// test's "tail visible" claim depends on the navigation, not on
/// the 47-line fixture trivially fitting in a 24-row viewport (it
/// doesn't).
#[test]
fn anti_scroll_clearing_without_navigation_keeps_tail_off_screen() {
    let scenario = LayoutScenario {
        description: "anti: no navigation — tail must NOT be on screen".into(),
        initial_text: String::new(),
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions: vec![],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("lfs_mirror_extend".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without Ctrl+End + overscroll, the file's tail \
         (`lfs_mirror_extend`, near line 45) must NOT be visible in the \
         initial 80x24 viewport (47-line fixture; content area ~21 rows). \
         The positive test's 'tail visible' check should fail."
    );
}

/// Anti-test: drop the wheel-down events from
/// `migrated_scroll_clearing_with_scroll_wheel_reaches_tail`.
/// Without them, the viewport stays at the top of the fixture and
/// `lfs_mirror_extend` (near line 45) must NOT be visible —
/// proves the positive test's tail-visibility assertion is gated
/// on the wheel events.
#[test]
fn anti_scroll_clearing_without_wheel_events_keeps_tail_off_screen() {
    let scenario = LayoutScenario {
        description: "anti: no wheel events — tail must NOT be on screen".into(),
        initial_text: String::new(),
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions: vec![],
        events: vec![],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("lfs_mirror_extend".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without wheel-down events the viewport stays at the start \
         of the fixture, so `lfs_mirror_extend` (near line 45) must NOT be \
         visible. The positive test's 'tail visible' check should fail."
    );
}

/// Anti-test: drop the Down,Down keypresses that move the cursor
/// onto line 3 (tab-prefixed). With the cursor still on line 1
/// (which is blank), MoveLineStart × 2 lands the cursor on line
/// 1's content row — the hardware cursor must NOT land on line 3.
/// Concretely, line 3 is two content rows below line 1, so the
/// cursor's row must NOT match the tab-prefixed line's row.
#[test]
fn anti_cursor_on_tab_line_without_down_keys_stays_on_line_one() {
    // Positive expectation: cursor row equals line-3's content row.
    // Without the Down,Down navigation, the cursor stays on line
    // 1's content row, so the row-equality check must fail. At
    // 80x24 with the tab bar at row 0, the first content row is
    // typically row 1; line 3's content row is row 3.
    let scenario = LayoutScenario {
        description: "anti: no Down,Down — cursor must NOT be on line-3 row".into(),
        initial_text: String::new(),
        initial_file: Some(scroll_test_file_path()),
        width: 80,
        height: 24,
        actions: vec![Action::MoveLineStart, Action::MoveLineStart],
        expected_snapshot: RenderSnapshotExpect {
            // Pin cursor to line 3's content row (rows 2..3 in
            // hardware-cursor coordinates after the tab bar). The
            // positive variant lands here; without Down,Down the
            // cursor stays on line 1 (row 0).
            hardware_cursor_row_in: Some((2, 3)),
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without Down,Down the cursor must remain on line 1 of the \
         fixture, NOT on line 3 (tab-prefixed). The positive test's \
         hardware_cursor_row_in check should fail."
    );
}
