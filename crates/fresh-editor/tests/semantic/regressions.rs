//! Bug repros found by the semantic test framework, captured as
//! standalone regression tests.
//!
//! Each `#[ignore]`d test in this file has a clear `expected =`
//! reason pointing at the production bug. When that bug is fixed,
//! the developer removes the `#[ignore]` (and the corresponding
//! `#[ignore]` on any property test that surfaces it). If the test
//! still passes, great — the fix worked and we now have permanent
//! coverage. If the test panics, the fix was incomplete.
//!
//! Discovery context: each bug was first found by a proptest
//! property under `properties.rs`. Proptest's shrinking produced
//! the minimal action sequence captured here.

use crate::common::theorem::buffer_theorem::{check_buffer_theorem, BufferTheorem, CursorExpect};
use fresh::test_api::Action;

/// Latent production bug: `crates/fresh-editor/src/input/actions.rs:1613`
/// panics with `index out of bounds: the len is 0 but the index is N`
/// when DeleteBackward executes smart-dedent in a state where the
/// cursor's recorded line_start is out of sync with the buffer's
/// actual byte layout.
///
/// **Reachability:** *not* triggered by interactive keystrokes
/// (verified in tmux). Every keystroke renders, and the render pass
/// reconciles cursor state with buffer geometry. Reachable from
/// production code paths that dispatch multiple actions WITHOUT
/// per-action layout reconciliation. The verified production analog
/// is `handle_execute_actions` in app/plugin_dispatch.rs, used by
/// vi-mode count prefixes (`3dw`, etc.) and plugin-driven action
/// batches. Macros are NOT vulnerable — `play_macro` calls
/// `recompute_layout` between every action specifically to avoid
/// this class of bug.
///
/// Discovered by `property_arbitrary_actions_do_not_panic` in 70s of
/// fuzzing. Proptest shrunk to 4 actions on a 4-byte buffer.
///
/// Diagnosis (educated guess from src/input/actions.rs:1600-1623):
/// `state.buffer.line_iterator(cursor.position, …).current_position()`
/// returns a value that is no longer in sync with the buffer after
/// the action chain; `prefix_len = cursor.position - line_start` is
/// computed as positive even though
/// `slice_bytes(line_start..cursor.position)` returns an empty Vec.
/// Then `prefix_bytes[prefix_len - 1]` indexes beyond the slice.
///
/// See `diagnosis_bug1_does_not_panic_with_render_between` below for
/// the diagnostic that confirms the layout-reconciliation hypothesis.
#[test]
#[ignore = "BUG: actions.rs:1613 — smart-dedent panics in no-render dispatch paths (vi count prefix, plugin batches)"]
fn regression_smart_dedent_panic_on_phantom_line() {
    // The exact shrunk repro from
    // tests/semantic/properties.proptest-regressions.
    // We don't claim a specific expected_text — just that the
    // dispatch returns *at all* without unwinding the stack.
    let result = check_buffer_theorem(BufferTheorem {
        description: "shrunk repro: MoveDown past EOF then DeleteBackward",
        initial_text: "   \n",
        actions: vec![
            Action::SelectLineEnd,
            Action::InsertChar(' '),
            Action::MoveDown,
            Action::DeleteBackward,
        ],
        // Whatever the correct end state is, it shouldn't panic. We
        // pick a probable one; if the eventual fix produces a
        // different text the developer updates the expectation.
        expected_text: "    \n",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
    // Either Ok(()) or a TheoremFailure is acceptable — both prove
    // the panic is gone. Only an actual panic (which is what we're
    // tracking) would short-circuit this and never reach the assert.
    assert!(result.is_ok() || result.is_err(), "should not panic");
}

/// Latent production bug: `crates/fresh-editor/src/state.rs:462`
/// panics when DeleteBackward's deleted-newline-counting code reads
/// `deleted_text[..bytes_before_cursor]` while
/// `bytes_before_cursor > deleted_text.len()`. Same family as the
/// smart-dedent bug above: cursor position out of sync with buffer
/// state after a chain.
///
/// **Reachability:** identical story to the smart-dedent bug —
/// interactive keystrokes do NOT trigger it (verified in tmux);
/// reachable only via no-render dispatch paths
/// (`handle_execute_actions` for vi-mode count prefixes / plugin
/// batches).
///
/// Discovered by `property_dispatch_is_deterministic` during routine
/// suite runs. Proptest shrunk to 4 actions on a 3-byte buffer.
///
/// See `diagnosis_bug2_does_not_panic_with_render_between` below for
/// the diagnostic.
#[test]
#[ignore = "BUG: state.rs:462 — DeleteBackward over whitespace-only buffer panics in no-render dispatch paths"]
fn regression_delete_backward_panic_on_whitespace_only_buffer() {
    let result = check_buffer_theorem(BufferTheorem {
        description:
            "shrunk repro: SelectLineEnd / InsertChar(' ') / SelectLineEnd / DeleteBackward",
        initial_text: "   ",
        actions: vec![
            Action::SelectLineEnd,
            Action::InsertChar(' '),
            Action::SelectLineEnd,
            Action::DeleteBackward,
        ],
        expected_text: "    ",
        expected_primary: CursorExpect::at(0),
        expected_extra_cursors: vec![],
        expected_selection_text: None,
    });
    assert!(result.is_ok() || result.is_err(), "should not panic");
}

// ─────────────────────────────────────────────────────────────────────────
// Diagnosis: confirm the bugs only manifest without per-action layout
// reconciliation. Run the same shrunk repros, but call
// harness.render() between every action — mimicking macros (which
// recompute_layout per-action) and interactive editing (which renders
// per-keystroke).
//
// If these tests pass without panicking, the diagnosis is confirmed:
// the bugs are real but only reachable from production paths that
// skip layout reconciliation. Verified production analog:
// `handle_execute_actions` in app/plugin_dispatch.rs (used by
// vi-mode count prefixes like "3dw" and plugin-driven action
// batches) loops `handle_action` with no recompute_layout between.
// Interactively verified in tmux: keystroke sequences for both bugs
// do NOT crash, because every keystroke triggers a render.
// ─────────────────────────────────────────────────────────────────────────

use crate::common::harness::EditorTestHarness;
use fresh::test_api::EditorTestApi;

fn run_with_render_per_action(initial_text: &str, actions: &[Action]) {
    let mut harness = EditorTestHarness::with_temp_project(80, 24)
        .expect("EditorTestHarness::with_temp_project failed");
    let _fixture = harness
        .load_buffer_from_text(initial_text)
        .expect("load_buffer_from_text failed");
    harness.render().expect("initial render");
    for action in actions {
        harness.api_mut().dispatch(action.clone());
        harness.render().expect("per-action render");
    }
}

#[test]
fn diagnosis_bug1_does_not_panic_with_render_between() {
    // Same actions as regression_smart_dedent_panic_on_phantom_line,
    // but with render() between every action. Passing confirms the
    // bug is only reachable from no-render dispatch paths.
    run_with_render_per_action(
        "   \n",
        &[
            Action::SelectLineEnd,
            Action::InsertChar(' '),
            Action::MoveDown,
            Action::DeleteBackward,
        ],
    );
}

#[test]
fn diagnosis_bug2_does_not_panic_with_render_between() {
    // Same actions as
    // regression_delete_backward_panic_on_whitespace_only_buffer,
    // but with render between every action.
    run_with_render_per_action(
        "   ",
        &[
            Action::SelectLineEnd,
            Action::InsertChar(' '),
            Action::SelectLineEnd,
            Action::DeleteBackward,
        ],
    );
}
