//! Reproduction for issue #1620: Panic on `Option::unwrap()` in
//! `apply_event_to_active_buffer` when clicking inside an editor pane.
//!
//! Reporter @zipproth described a vertical-split layout (KDL file on the
//! left, another file on the right) with "an integrated terminal pane at
//! the bottom"; clicking inside the KDL pane panicked with:
//!
//! ```text
//! thread 'main' panicked at crates/fresh-editor/src/app/event_apply.rs:101:18:
//! called `Option::unwrap()` on a `None` value
//! ```
//!
//! ## Root cause (what the repro below exercises)
//!
//! The panic site:
//!
//! ```ignore
//! let split_id = self.effective_active_split();
//! let active_buf = self.active_buffer();
//! let cursors = &mut self
//!     .split_view_states
//!     .get_mut(&split_id)
//!     .unwrap()
//!     .keyed_states
//!     .get_mut(&active_buf)   // <-- line 101: unwraps None
//!     .unwrap()
//!     .cursors;
//! ```
//!
//! `active_buffer()` reads through `effective_active_pair()` in
//! `editor_accessors.rs`, whose fallback branch returns the split tree's
//! `active_buffer_id()` *without* checking that the tree-buffer is
//! actually in that split's `SplitViewState.keyed_states`. The bug is
//! that these two stores can drift apart.
//!
//! They drift when a buffer is closed from a different split than the
//! one that was showing it. `Editor::close_buffer_internal` at
//! `crates/fresh-editor/src/app/buffer_close.rs:221-225` updates the
//! split tree for every non-active split that had the closed buffer:
//!
//! ```ignore
//! let splits_to_update = self.split_manager.splits_for_buffer(id);
//! for split_id in splits_to_update {
//!     self.split_manager.set_split_buffer(split_id, replacement_buffer);
//! }
//! ```
//!
//! but never calls `switch_buffer` on those splits' `SplitViewState`.
//! The subsequent `view_state.remove_buffer(id)` loop at
//! `buffer_close.rs:247-250` / `split.rs:473-480` preserves
//! `keyed_states[id]` when `id` is still that split's `active_buffer`.
//!
//! Result: for the non-closing split S1 we end up with
//! `tree[S1] = F2 (replacement)` but `SVS[S1].active_buffer = F1 (stale)`
//! and `SVS[S1].keyed_states = {F1}` — no `F2` entry at all, even though
//! `F1` is no longer in `self.buffers`.
//!
//! Normally the mouse-click path rescues this via `focus_split`, which
//! calls `view_state.switch_buffer(buffer_id)` (`active_focus.rs:189`).
//! `switch_buffer` inserts a default `BufferViewState` if the buffer is
//! missing from `keyed_states`, which would patch the inconsistency.
//! But that branch only runs when `split_changed`. When you navigate to
//! S1 first via `next_split()` (which just flips `active_split` without
//! touching any `SplitViewState` — see `split_actions.rs:196-199`) and
//! then click inside S1, `split_changed == false`; `focus_split` takes
//! the else branch (`active_focus.rs:203-206`) and calls
//! `set_active_buffer`, which *early-returns* because
//! `active_buffer() == buffer_id` (both read as `F2` from the tree —
//! see `active_focus.rs:21-23`). No `switch_buffer` is invoked, the
//! stale state survives, and the click's eventual
//! `apply_event_to_active_buffer` panics.
//!
//! The reporter's terminal pane is incidental scenery — the panic's
//! real precondition is "a buffer got closed while still being shown in
//! a non-active split". Closing a terminal buffer is a natural way to
//! end up there, which is why the reporter's layout had one.

use crate::common::harness::EditorTestHarness;
use std::fs;
use tempfile::TempDir;

/// Find the cached split area for a given buffer and return its
/// `content_rect` so the test can click somewhere inside that pane.
/// Panics if the buffer isn't represented in the cached layout —
/// we rely on that to catch harness drift early.
fn content_rect_for_buffer(
    harness: &EditorTestHarness,
    buffer_id: fresh::model::event::BufferId,
) -> ratatui::layout::Rect {
    for (_, bid, content_rect, _, _, _) in harness.editor().get_split_areas() {
        if *bid == buffer_id {
            return *content_rect;
        }
    }
    panic!(
        "expected split area for buffer {:?}, cached layout has: {:?}",
        buffer_id,
        harness
            .editor()
            .get_split_areas()
            .iter()
            .map(|(_, b, _, _, _, _)| *b)
            .collect::<Vec<_>>()
    );
}

/// Reproduce the issue #1620 panic: close a buffer while it's still
/// the tree-buffer of an inactive split, then navigate into that
/// split via `next_split` and click. Clicking must not panic — today
/// it aborts the process at `event_apply.rs:101`.
#[test]
fn clicking_split_after_closing_buffer_in_another_split_does_not_panic() {
    let mut harness = EditorTestHarness::new(140, 40).unwrap();

    let temp_dir = TempDir::new().unwrap();
    let file_a_path = temp_dir.path().join("settings.kdl");
    fs::write(
        &file_a_path,
        "// KDL settings file from the issue reporter\n\
         node \"value\" {\n    child 1\n    child 2\n}\n",
    )
    .unwrap();
    let file_b_path = temp_dir.path().join("other.txt");
    fs::write(&file_b_path, "plain text file in the second pane\n").unwrap();

    // Step 1: open file A in the initial (only) split S1 — buffer F1.
    harness.open_file(&file_a_path).unwrap();
    let f1 = harness.editor().active_buffer_id();

    // Step 2: split vertically. The new split S2 becomes the active
    // split; both panes initially show F1.
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();
    let split_after_split = harness.editor().get_active_split();

    // Step 3: open file B in S2 — tree[S2] = F2, SVS[S2] gets F2 keyed.
    // S1 is untouched: tree[S1] = F1, SVS[S1] = { active=F1, keyed={F1} }.
    harness.open_file(&file_b_path).unwrap();
    let f2 = harness.editor().active_buffer_id();
    assert_ne!(f2, f1, "opening file B should yield a distinct buffer id");
    assert_eq!(
        harness.editor().get_active_split(),
        split_after_split,
        "opening a file should open it in the currently active split, not create a new one"
    );

    // Sanity-check the precondition for the drift: S1 is non-active
    // and still has F1 in both its tree slot and its SplitViewState.
    assert_eq!(
        harness.editor().get_split_count(),
        2,
        "expected exactly two splits after a single vertical split"
    );

    // Step 4: close F1 from S2. `closing_active == false` because S2's
    // active buffer is F2, so this takes the non-active close path that
    // updates the tree for S1 without touching SVS[S1].
    harness.editor_mut().close_buffer(f1).unwrap();
    harness.render().unwrap();

    // Step 5: navigate to S1 via next_split. This flips
    // `split_manager.active_split` to S1 but does not call
    // `switch_buffer` on SVS[S1], so SVS[S1] keeps its stale
    // `active_buffer = F1` / `keyed_states = {F1}`, even though
    // `self.buffers` no longer contains F1 and `tree[S1]` is F2.
    harness.editor_mut().next_split();
    harness.render().unwrap();

    // `active_buffer()` now resolves through the tree and reports F2.
    // `effective_active_split()` reports S1. But SVS[S1].keyed_states
    // contains only F1. The mouse-click path about to run will hit
    // the `.unwrap()` at `event_apply.rs:101`.
    assert_eq!(
        harness.editor().active_buffer_id(),
        f2,
        "after next_split, the editor should report F2 as active (via the split tree)"
    );

    // Step 6: click inside S1's content area. With the bug present,
    // this panics.
    let s1_rect = content_rect_for_buffer(&harness, f2);
    let click_col = s1_rect.x + s1_rect.width / 2;
    let click_row = s1_rect.y + s1_rect.height / 2;
    harness.mouse_click(click_col, click_row).unwrap();

    // If we got here, the panic did not occur. Sanity-check focus did
    // route to S1 and the editor is still in a sane keyboard state.
    assert_eq!(
        harness.editor().active_buffer_id(),
        f2,
        "clicking inside S1 must keep F2 (the tree-buffer of S1) as active"
    );
    assert!(
        !harness.editor().is_terminal_mode(),
        "no terminal is involved in the minimal repro; terminal_mode must be off"
    );
}
