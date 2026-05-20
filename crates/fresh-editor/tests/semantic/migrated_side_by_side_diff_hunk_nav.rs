//! DECLARATIVE: Migration of `tests/e2e/side_by_side_diff_hunk_nav.rs`
//! — hunk navigation in side-by-side diff composite buffers
//! (issue #2058 per-row-text sweep).
//!
//! Scenarios are pure data: each test is a single `LayoutScenario`
//! literal with `composite_buffer: Some(CompositeBufferSpec)` for
//! the diff setup and an `events` list for the post-setup hunk-nav
//! / keybinding events. No direct harness usage anywhere
//! in this file.
//!
//! Load-bearing claims preserved here:
//!
//!   1. `composite_next_hunk_active` jumps to each successive hunk
//!      in a multi-hunk diff and centers it so the hunk's MODIFIED
//!      content becomes visible.
//!   2. `composite_prev_hunk_active` jumps back to the previous
//!      hunk.
//!   3. The hunk is centered with context lines above it (not just
//!      placed at the top row of the viewport).
//!   4. `initial_focus_hunk = Some(0)` auto-scrolls to the first
//!      hunk on the first render — Line 1 (the buffer start) is
//!      pushed off the top of the viewport.
//!   5. `initial_focus_hunk = Some(2)` auto-scrolls to the third
//!      hunk on the first render.
//!   6. `initial_focus_hunk` is a one-shot — after the first render
//!      consumes it, the field is set back to `None`, and a
//!      subsequent user scroll is not snapped back to the focus
//!      hunk.
//!   7. Without `flush_layout` before the first render, a single
//!      `composite_next_hunk_active` call does not advance the
//!      viewport (no view state to mutate). With `flush_layout`
//!      first, the hunk becomes visible after the next render.
//!   8. `flush_layout` + multiple `composite_next_hunk_active`
//!      calls let a test reach hunk 3 before the first render — the
//!      full imperative alternative to `initial_focus_hunk`.
//!   9. Keybinding `n` in a composite buffer view navigates to the
//!      next hunk via the Action-based keymap.
//!  10. Keybindings `]` / `[` / `p` work as forward / back aliases
//!      for hunk navigation, alongside `n`.
//!
//! Source: `tests/e2e/side_by_side_diff_hunk_nav.rs` (10 tests
//! migrated; no tests deferred).

use crate::common::scenario::context::MouseEvent;
use crate::common::scenario::input_event::{InputEvent, KeyMods, KeySpec};
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, CompositeBufferSpec, LayoutScenario,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

/// Generate `(old_content, new_content, hunks)` for a 150-line file
/// with three modified hunks at lines 20, 60, 120. Mirrors the e2e
/// `generate_multi_hunk_content` helper byte-for-byte.
fn multi_hunk() -> (String, String, Vec<(usize, usize, usize, usize)>) {
    let line_count = 150;
    let old_lines: Vec<String> = (1..=line_count)
        .map(|i| format!("Line {i} original content"))
        .collect();

    let mut new_lines = old_lines.clone();
    for i in 19..22 {
        new_lines[i] = format!("Line {} MODIFIED in hunk 1", i + 1);
    }
    for i in 59..63 {
        new_lines[i] = format!("Line {} MODIFIED in hunk 2", i + 1);
    }
    for i in 119..124 {
        new_lines[i] = format!("Line {} MODIFIED in hunk 3", i + 1);
    }

    let old_content = old_lines.join("\n") + "\n";
    let new_content = new_lines.join("\n") + "\n";
    let hunks = vec![
        (19, 3, 19, 3),   // hunk 1 at line 20
        (59, 4, 59, 4),   // hunk 2 at line 60
        (119, 5, 119, 5), // hunk 3 at line 120
    ];
    (old_content, new_content, hunks)
}

fn multi_hunk_spec() -> CompositeBufferSpec {
    let (old_content, new_content, hunks) = multi_hunk();
    CompositeBufferSpec {
        name: "Diff View".into(),
        mode: "diff-view".into(),
        old_content,
        new_content,
        hunks,
        ..Default::default()
    }
}

#[test]
fn migrated_next_hunk_navigation_shows_hunk_content() {
    // Original: `test_next_hunk_navigation_shows_hunk_content`.
    // 3 CompositeNextHunk events; after the third, hunk 3 must be
    // visible.
    assert_layout_scenario(LayoutScenario {
        description: "3x next_hunk reaches hunk 3 content".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![
            InputEvent::CompositeNextHunk { count: 1 },
            InputEvent::CompositeNextHunk { count: 1 },
            InputEvent::CompositeNextHunk { count: 1 },
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 3".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_prev_hunk_navigation() {
    // Original: `test_prev_hunk_navigation`.
    assert_layout_scenario(LayoutScenario {
        description: "navigate to hunk 3, then prev_hunk lands on hunk 2".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![
            InputEvent::CompositeNextHunk { count: 3 },
            InputEvent::CompositePrevHunk { count: 1 },
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 2".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_hunk_navigation_shows_context_above() {
    // Original: `test_hunk_navigation_shows_context_above`. Hunk is
    // centered with ~1/3 of the viewport as context above it. With
    // a 40-row viewport and hunk 2 at line 60, lines around 54-57
    // should be visible above the hunk.
    assert_layout_scenario(LayoutScenario {
        description: "hunk 2 has at least one line of context visible above it".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![InputEvent::CompositeNextHunk { count: 2 }],
        expected_snapshot: RenderSnapshotExpect {
            // The hunk's MODIFIED text must be visible AND at least
            // one of the pre-hunk context lines (54-57) must be on
            // screen, proving the hunk wasn't placed at the very
            // top of the viewport.
            row_checks: vec![
                RowMatch::AnyRowContains("MODIFIED in hunk 2".into()),
                RowMatch::AnyRowContains("Line 54 original".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_initial_focus_hunk_scrolls_to_first_hunk_on_first_render() {
    // Original: `test_initial_focus_hunk_scrolls_to_first_hunk_on_first_render`.
    let mut spec = multi_hunk_spec();
    spec.initial_focus_hunk = Some(0);
    assert_layout_scenario(LayoutScenario {
        description: "initial_focus_hunk=0 auto-scrolls past Line 1 to hunk 1".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(spec),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::NoRowContains("Line 1 original".into()),
                RowMatch::AnyRowContains("MODIFIED in hunk 1".into()),
            ],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_initial_focus_hunk_scrolls_to_nth_hunk() {
    // Original: `test_initial_focus_hunk_scrolls_to_nth_hunk`.
    let mut spec = multi_hunk_spec();
    spec.initial_focus_hunk = Some(2);
    assert_layout_scenario(LayoutScenario {
        description: "initial_focus_hunk=2 auto-scrolls to hunk 3".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(spec),
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 3".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_initial_focus_hunk_is_consumed_after_first_render() {
    // Original: `test_initial_focus_hunk_is_consumed_after_first_render`.
    // After auto-focus to hunk 3, scroll back to the top with 50
    // wheel-ups. Line 1 must be visible (we are NOT re-snapped back
    // to hunk 3), AND the composite's initial_focus_hunk field has
    // been consumed (set to None) by the first render.
    let mut spec = multi_hunk_spec();
    spec.initial_focus_hunk = Some(2);
    let events: Vec<InputEvent> = (0..50)
        .map(|_| {
            InputEvent::Mouse(MouseEvent::Wheel {
                row: 20,
                col: 60,
                dy: 1,
            })
        })
        .collect();
    assert_layout_scenario(LayoutScenario {
        description: "initial_focus_hunk is one-shot; scrolling back leaves Line 1 visible".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(spec),
        events,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("Line 1 original".into())],
            ..Default::default()
        },
        expected_initial_focus_hunk_consumed: Some(true),
        ..Default::default()
    });
}

#[test]
fn migrated_flush_layout_enables_hunk_nav_before_render() {
    // Original: `test_flush_layout_enables_hunk_nav_before_render`.
    // Without `FlushLayout` before the first render, the composite
    // view state isn't materialized, so `CompositeNextHunk` can't
    // mutate it — after the final render hunk 1's MODIFIED text
    // must NOT be on screen. (Compare with the companion test
    // below where FlushLayout DOES make it visible.)
    let mut spec = multi_hunk_spec();
    spec.skip_initial_render = true;
    assert_layout_scenario(LayoutScenario {
        description: "flush_layout + next_hunk before first render shows hunk 1".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(spec),
        events: vec![
            InputEvent::FlushLayout,
            InputEvent::CompositeNextHunk { count: 1 },
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 1".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_flush_layout_jump_to_third_hunk_before_render() {
    // Original: `test_flush_layout_jump_to_third_hunk_before_render`.
    let mut spec = multi_hunk_spec();
    spec.skip_initial_render = true;
    assert_layout_scenario(LayoutScenario {
        description: "flush_layout + 3x next_hunk before first render shows hunk 3".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(spec),
        events: vec![
            InputEvent::FlushLayout,
            InputEvent::CompositeNextHunk { count: 3 },
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 3".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_keybinding_n_navigates_to_next_hunk() {
    // Original: `test_keybinding_n_navigates_to_next_hunk`.
    assert_layout_scenario(LayoutScenario {
        description: "'n' key navigates through hunks via the keymap".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![
            InputEvent::SendKey {
                code: KeySpec::Char('n'),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Char('n'),
                modifiers: KeyMods::NONE,
            },
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 2".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_keybinding_p_and_brackets_navigate_hunks() {
    // Original: `test_keybinding_p_and_brackets_navigate_hunks`.
    // Sequence: ']' → ']' → '[' → 'n' → 'n' → 'p'. Final state
    // should be hunk 2.
    assert_layout_scenario(LayoutScenario {
        description: "]/[/n/p keys all navigate hunks; final state = hunk 2".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![
            InputEvent::SendKey {
                code: KeySpec::Char(']'),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Char(']'),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Char('['),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Char('n'),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Char('n'),
                modifiers: KeyMods::NONE,
            },
            InputEvent::SendKey {
                code: KeySpec::Char('p'),
                modifiers: KeyMods::NONE,
            },
        ],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 2".into())],
            ..Default::default()
        },
        ..Default::default()
    });
}

// =============================================================================
// Anti-tests
// =============================================================================

/// Anti-test: drop the second + third `CompositeNextHunk` events.
/// Without them the viewport never reaches hunk 3 (line 120,
/// outside the default 40-row viewport that starts at line 1) and
/// "MODIFIED in hunk 3" must NOT be on screen. Proves the positive
/// `migrated_next_hunk_navigation_shows_hunk_content` claim depends
/// on the repeated nav calls actually advancing through the hunk
/// list.
#[test]
fn anti_next_hunk_without_call_keeps_hunk_off_screen() {
    let scenario = LayoutScenario {
        description: "anti: 0x next_hunk leaves hunk 3 off-screen".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![], // dropped
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 3".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without next_hunk, hunk 3 must NOT be visible"
    );
}

/// Anti-test: drop the `'n'` keypresses. Without them the viewport
/// never reaches hunk 2 (line 60, outside the default viewport) so
/// "MODIFIED in hunk 2" must NOT appear. Proves the positive
/// `migrated_keybinding_n_navigates_to_next_hunk` claim depends on
/// the keypress dispatch routing through the Action-based keymap.
#[test]
fn anti_keybinding_n_without_press_keeps_hunk_off_screen() {
    let scenario = LayoutScenario {
        description: "anti: without 'n' keypresses, hunk 2 must stay off-screen".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()),
        events: vec![], // dropped
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 2".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without 'n', hunk 2 must NOT be visible"
    );
}

/// Anti-test: drop the `initial_focus_hunk = Some(2)` assignment.
/// Without it, the first render leaves the viewport at the top of
/// the buffer and Line 1 must be visible (hunk 3 must NOT be).
/// Proves the positive
/// `migrated_initial_focus_hunk_scrolls_to_nth_hunk` claim depends
/// on the field actually being set.
#[test]
fn anti_initial_focus_hunk_unset_starts_at_buffer_top() {
    let scenario = LayoutScenario {
        description: "anti: without initial_focus_hunk, first render shows Line 1".into(),
        width: 120,
        height: 40,
        composite_buffer: Some(multi_hunk_spec()), // initial_focus_hunk = None
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("MODIFIED in hunk 3".into())],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without initial_focus_hunk=Some(2), hunk 3 must NOT auto-appear"
    );
}
