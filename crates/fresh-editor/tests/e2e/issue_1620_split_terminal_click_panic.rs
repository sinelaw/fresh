//! Reproduction for issue #1620: Panic on `Option::unwrap()` in
//! `apply_event_to_active_buffer` when clicking inside an editor pane in a
//! layout that mixes vertical splits with a terminal pane.
//!
//! Reporter: @zipproth. Setup they described: a vertical split with a KDL
//! file on the left and a different file on the right, plus an "integrated
//! terminal pane at the bottom". Clicking inside the KDL pane panicked
//! with:
//!
//! ```text
//! thread 'main' panicked at crates/fresh-editor/src/app/event_apply.rs:101:18:
//! called `Option::unwrap()` on a `None` value
//! ```
//!
//! The unwrap at `event_apply.rs:101` is the second of the two inside
//! `apply_event_to_active_buffer` — i.e. the
//! `split_view_states[effective_active_split()].keyed_states[active_buffer()]`
//! lookup. That means the editor reached a state where
//! `effective_active_split` / `active_buffer` disagree with the keyed-state
//! map owned by that split's `SplitViewState`.
//!
//! A pre-existing comment above the unwrap notes that focusing a buffer
//! group panel used to trigger exactly this shape of panic (fixed by
//! switching to `effective_active_split`). The issue here is the
//! regression's cousin: the same shape of inconsistency resurfaces when a
//! terminal pane is part of the layout and the user clicks back into a
//! non-terminal pane.
//!
//! This test builds the minimum layout the reporter described — two
//! vertical editor panes (top-left holds a KDL file, top-right a
//! different file) with a terminal pane below — and then clicks inside
//! the KDL pane. A healthy editor should reposition the cursor without
//! panicking; today it aborts the process.
//!
//! The test uses a live PTY (via `portable_pty::native_pty_system`) and
//! skips cleanly in environments where PTYs are unavailable, matching
//! the pattern in `terminal_split_focus_live.rs`.

use crate::common::harness::EditorTestHarness;
use portable_pty::{native_pty_system, PtySize};
use std::fs;
use tempfile::TempDir;

/// Build an `EditorTestHarness`, skipping the test (returning `None`) if
/// the host cannot allocate a PTY. Mirrors `harness_or_skip` in
/// `terminal_split_focus_live.rs` so this test degrades gracefully on CI
/// runners without `/dev/ptmx`.
fn harness_or_skip(width: u16, height: u16) -> Option<EditorTestHarness> {
    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping issue #1620 repro: PTY not available in this environment");
        return None;
    }
    EditorTestHarness::new(width, height).ok()
}

/// Find a `(split_id, buffer_id, content_rect)` in the editor's cached
/// split areas whose buffer is the one we expect. Panics if the layout
/// doesn't contain the expected buffer — that would mean the harness
/// setup diverged from what this test is trying to reproduce.
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
        "expected split area for buffer {:?} but the cached layout only has {:?}",
        buffer_id,
        harness
            .editor()
            .get_split_areas()
            .iter()
            .map(|(_, b, _, _, _, _)| *b)
            .collect::<Vec<_>>()
    );
}

/// Reproduce the panic: a vertical pair of editor panes (KDL on left,
/// another file on right) sitting above a terminal pane. Clicking inside
/// the KDL pane should land the cursor there; with the bug present, the
/// editor panics on `Option::unwrap()` in `apply_event_to_active_buffer`.
#[test]
fn clicking_into_editor_pane_with_terminal_below_does_not_panic() {
    let mut harness = match harness_or_skip(140, 40) {
        Some(h) => h,
        None => return,
    };

    // Two real files on disk so the editor has concrete buffers to open
    // rather than [No Name] scratch buffers. The KDL extension is not
    // load-bearing for the panic — any non-terminal buffer works — but
    // we keep it to stay faithful to the reporter's description.
    let temp_dir = TempDir::new().unwrap();
    let kdl_path = temp_dir.path().join("settings.kdl");
    fs::write(
        &kdl_path,
        "// KDL settings file from the issue reporter\n\
         node \"value\" {\n    child 1\n    child 2\n}\n",
    )
    .unwrap();
    let other_path = temp_dir.path().join("other.txt");
    fs::write(&other_path, "plain text file in the right pane\n").unwrap();

    // --- Build the layout -----------------------------------------------
    //
    // Start by opening the KDL file; this populates the initial (only)
    // split with the KDL buffer.
    harness.open_file(&kdl_path).unwrap();
    let kdl_buffer = harness.editor().active_buffer_id();

    // Horizontal split so we end up with a top pane and a bottom pane.
    // The new (active) pane is the bottom half.
    harness.editor_mut().split_pane_horizontal();
    harness.render().unwrap();

    // Open a terminal in the now-active (bottom) split. This is what the
    // reporter referred to as "an integrated terminal pane at the bottom".
    harness.editor_mut().open_terminal();
    harness.render().unwrap();
    let terminal_buffer = harness.editor().active_buffer_id();
    assert!(
        harness.editor().is_terminal_buffer(terminal_buffer),
        "open_terminal should have made the bottom pane a terminal buffer, \
         otherwise we're not reproducing the reporter's layout"
    );

    // Move focus back up to the top pane so the next split lands there.
    // The concrete traversal order (prev/next) isn't load-bearing — we
    // just need to land on the non-terminal side. Loop defensively in
    // case next_split skips us past.
    for _ in 0..4 {
        if !harness
            .editor()
            .is_terminal_buffer(harness.editor().active_buffer_id())
        {
            break;
        }
        harness.editor_mut().next_split();
    }
    harness.render().unwrap();
    assert!(
        !harness
            .editor()
            .is_terminal_buffer(harness.editor().active_buffer_id()),
        "expected to be focused on the non-terminal (top) pane before \
         splitting vertically"
    );

    // Vertical split of the top pane — this produces the left/right pair
    // the reporter described, leaving a third (bottom) pane with the
    // terminal. After this, the active pane is the newly-created right
    // side and both top panes hold the KDL buffer.
    harness.editor_mut().split_pane_vertical();
    harness.render().unwrap();

    // Swap the right pane's buffer to the "other" file so the final
    // layout is: top-left = KDL, top-right = other, bottom = terminal,
    // which matches the issue description exactly.
    harness.open_file(&other_path).unwrap();
    let other_buffer = harness.editor().active_buffer_id();
    assert_ne!(
        other_buffer, kdl_buffer,
        "open_file should have created a fresh buffer for other.txt"
    );

    assert_eq!(
        harness.editor().get_split_count(),
        3,
        "setup should have produced exactly 3 panes \
         (KDL, other, terminal); instead got {} — \
         the rest of the test's coordinates assume 3",
        harness.editor().get_split_count()
    );

    // --- The clicked-pane-panics step -----------------------------------
    //
    // Click somewhere inside the KDL pane's content area. The reporter
    // observed a panic here; this test expects no panic.
    let kdl_rect = content_rect_for_buffer(&harness, kdl_buffer);
    // Aim for a cell strictly inside the pane so gutter / edge edge-cases
    // don't mask the real bug. Middle of the pane is fine.
    let click_col = kdl_rect.x + kdl_rect.width / 2;
    let click_row = kdl_rect.y + kdl_rect.height / 2;

    // If the bug is present, this call aborts the process with
    // `called Option::unwrap() on a None value` from event_apply.rs:101.
    harness.mouse_click(click_col, click_row).unwrap();

    // If we got here, the panic did not occur. Additionally verify the
    // click did route focus to the KDL pane (the reporter's expected
    // behavior: "clicking inside an editor pane should move the cursor
    // (or focus the pane) without crashing").
    assert_eq!(
        harness.editor().active_buffer_id(),
        kdl_buffer,
        "after clicking inside the KDL pane, the KDL buffer should be \
         the active buffer"
    );
    assert!(
        !harness.editor().is_terminal_mode(),
        "clicking a non-terminal pane must stop capturing keyboard for \
         the terminal"
    );
}
