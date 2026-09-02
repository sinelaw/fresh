//! One geometry pass per frame.
//!
//! Stage 2 of the retained-mode migration (Blocker A.3): pane content rects
//! come off the same `Ui` that describes the frame, and there is exactly one
//! source of pane geometry per frame. Before it, the render path laid the
//! pane grid out three ways — the shell tree, a scratch `Ui<()>` in
//! `SplitManager::get_leaves_with_rects`, and (on macro replay) a
//! `layout_only` of the shell followed by the scratch grid again. The scratch
//! grid is gone now (Stage 2b): the action paths that asked it read the pane
//! rects the window retains from the last layout instead.
//!
//! `view::shell::geometry::stats` counts the passes; these tests pin the
//! counts a frame and a replayed action are allowed: **one shell layout, no
//! offscreen grid**, over the shapes the grid takes — one pane, nested splits,
//! a maximized pane, the explorer open and shut. The counters exist only
//! with debug assertions on, which is what `cargo test` builds with.
#![cfg(debug_assertions)]

use crate::common::harness::EditorTestHarness;
use fresh::input::keybindings::Action;
use fresh::view::shell::geometry::stats::{self, LayoutCounts};

/// One frame's layout counts.
fn frame(h: &mut EditorTestHarness) -> LayoutCounts {
    let _ = stats::take();
    h.render().expect("render");
    stats::take()
}

/// One replayed action's geometry pass, as `play_macro` runs it between
/// actions.
fn replay(h: &mut EditorTestHarness) -> LayoutCounts {
    let _ = stats::take();
    h.editor_mut().recompute_layout(80, 24);
    stats::take()
}

const ONE_PASS: LayoutCounts = LayoutCounts {
    shell: 1,
    offscreen_grids: 0,
};

#[test]
fn a_frame_lays_the_grid_out_once() {
    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    assert_eq!(frame(&mut h), ONE_PASS, "a single pane");

    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    assert_eq!(frame(&mut h), ONE_PASS, "two panes side by side");

    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitHorizontal);
    assert_eq!(frame(&mut h), ONE_PASS, "a nested split");

    h.editor_mut()
        .dispatch_action_for_tests(Action::ToggleMaximizeSplit);
    assert_eq!(frame(&mut h), ONE_PASS, "a maximized pane");
    h.editor_mut()
        .dispatch_action_for_tests(Action::ToggleMaximizeSplit);
    assert_eq!(frame(&mut h), ONE_PASS, "restored");
}

/// A chrome toggle changes every pane's width between frames; the frame after
/// it still lays the grid out once, and the panes come off that layout — not
/// last frame's.
#[test]
fn a_chrome_toggle_costs_no_second_layout() {
    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    assert_eq!(frame(&mut h), ONE_PASS);

    h.editor_mut().toggle_file_explorer();
    assert_eq!(frame(&mut h), ONE_PASS, "explorer open");
    h.editor_mut().toggle_file_explorer();
    assert_eq!(frame(&mut h), ONE_PASS, "explorer shut");

    h.editor_mut()
        .dispatch_action_for_tests(Action::ToggleTabBar);
    assert_eq!(frame(&mut h), ONE_PASS, "no tab strip");
    h.editor_mut()
        .dispatch_action_for_tests(Action::ToggleTabBar);
    assert_eq!(frame(&mut h), ONE_PASS, "tab strip back");
}

/// Macro replay's geometry pass is the frame's, minus the frame: one
/// `layout_only`, and the panes read off it. It used to be a layout of the
/// shell *plus* the scratch grid inside `compute_content_layout`.
#[test]
fn a_replayed_action_lays_the_grid_out_once() {
    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    h.render().expect("render");
    assert_eq!(replay(&mut h), ONE_PASS, "a single pane");

    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitHorizontal);
    h.render().expect("render");
    assert_eq!(replay(&mut h), ONE_PASS, "nested splits");

    h.editor_mut().toggle_file_explorer();
    assert_eq!(replay(&mut h), ONE_PASS, "explorer open, no frame between");
}
