//! The pane rects a window retains are the layout's.
//!
//! Stage 2b of the retained-mode migration: the scratch grid is gone, and
//! the action paths that asked it where a pane is — a terminal sized to its
//! pane, a tab strip's width, the plugin snapshot, the pane beside this one —
//! read `Window::pane_rects`, which every layout writes. Two things have to
//! hold for that to be a read of the layout and not a record of a stale one:
//! after a frame, the retained rects are the ones the frame painted with; and
//! after an action that changes the grid, they are refreshed before the frame
//! that would paint it.
//!
//! The layout counters exist only with debug assertions on, which is what
//! `cargo test` builds with.
#![cfg(debug_assertions)]

mod common;

use common::harness::EditorTestHarness;
use fresh::input::keybindings::Action;
use fresh::view::shell::geometry::{stats, PaneRects};
use ratatui::layout::Rect;

/// After a frame, the retained rects are what `PaneRects::read` gives off the
/// tree that laid the frame out — box and content slot, pane for pane.
#[test]
fn a_frame_retains_the_rects_it_painted_with() {
    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitHorizontal);
    h.render().expect("render");

    let editor = h.editor();
    let ui = editor
        .shell_ui()
        .expect("the tree is back once the frame is painted");
    let panes = editor.active_window().visible_panes();
    assert_eq!(panes.len(), 3, "a nested split");
    let read = PaneRects::read(
        ui,
        panes.iter().map(|(leaf, _, _)| *leaf),
        Rect::new(0, 0, 80, 24),
    );
    let retained = editor.active_window().pane_rects();
    for (leaf, _, rect) in &panes {
        assert_eq!(read.pane(*leaf), Some(*rect), "{leaf:?}'s box");
        assert_ne!(rect.width, 0, "{leaf:?} was placed");
        assert_eq!(
            retained.content(*leaf),
            read.content(*leaf),
            "{leaf:?}'s content slot"
        );
    }
    // Three panes, three distinct boxes.
    let boxes: Vec<Rect> = panes.iter().map(|(_, _, r)| *r).collect();
    assert_ne!(boxes[0], boxes[1]);
    assert_ne!(boxes[1], boxes[2]);
    assert_ne!(boxes[0], boxes[2]);
}

/// A split is placed before the frame that would paint it: the action's
/// relayout lays the frame out once, and the retained rects are the grid as
/// it is now — and are what the next frame paints with.
#[test]
fn a_split_refreshes_the_rects_before_the_frame() {
    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    h.render().expect("render");
    let before = h.editor().active_window().visible_panes();
    assert_eq!(before.len(), 1);
    let whole = before[0].2;

    let _ = stats::take();
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    let counts = stats::take();
    assert_eq!(
        counts.offscreen_grids, 0,
        "no grid is laid out alone; the frame is"
    );
    assert!(counts.shell >= 1, "the split's relayout laid the frame out");

    let after = h.editor().active_window().visible_panes();
    assert_eq!(after.len(), 2, "both panes are placed, no frame between");
    let (left, right) = (after[0].2, after[1].2);
    assert_eq!(left.y, whole.y);
    assert_eq!(left.height, whole.height);
    assert_eq!(right.y, whole.y);
    assert_eq!(right.height, whole.height);
    assert_eq!(
        right.x,
        left.x + left.width + 1,
        "a separator column between"
    );
    assert_eq!(left.width + 1 + right.width, whole.width);

    // The frame agrees: it paints with the rects the action already had.
    h.render().expect("render");
    assert_eq!(h.editor().active_window().visible_panes(), after);
}

/// A chrome toggle moves every pane; the toggle's relayout places them again
/// before the frame, and a maximized pane is the only one placed, at the
/// whole body.
#[test]
fn chrome_and_maximize_refresh_the_rects_before_the_frame() {
    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    h.render().expect("render");
    let shut = h.editor().active_window().visible_panes();
    assert_eq!(shut.len(), 2);

    h.editor_mut().toggle_file_explorer();
    let open = h.editor().active_window().visible_panes();
    assert_eq!(open.len(), 2);
    assert!(
        open[0].2.x > shut[0].2.x,
        "the explorer pushed the first pane right before any frame"
    );
    h.render().expect("render");
    assert_eq!(h.editor().active_window().visible_panes(), open);
    h.editor_mut().toggle_file_explorer();

    h.editor_mut()
        .dispatch_action_for_tests(Action::ToggleMaximizeSplit);
    let maximized = h.editor().active_window().visible_panes();
    assert_eq!(maximized.len(), 1, "only the maximized pane is visible");
    assert_eq!(
        maximized[0].2,
        Rect::new(
            shut[0].2.x,
            shut[0].2.y,
            shut[0].2.width + 1 + shut[1].2.width,
            shut[0].2.height
        ),
        "at the whole body"
    );
    h.render().expect("render");
    assert_eq!(h.editor().active_window().visible_panes(), maximized);
}
