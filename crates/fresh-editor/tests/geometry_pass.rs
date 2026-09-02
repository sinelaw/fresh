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

mod common;

use common::harness::EditorTestHarness;
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

/// **A divider drag that does not move the divider lays out nothing.**
///
/// A grip captures the pointer for the whole drag, so its `Move` fires for
/// every motion report the terminal sends while the button is held —
/// including the ones that travel *along* the divider rather than across it.
/// Each of those used to run the full layout funnel (`Editor::relayout`:
/// every window's panes placed, every visible PTY resized) to arrive at the
/// ratio the split already had, so a drag paid a geometry pass per report
/// instead of per column crossed.
///
/// Counted rather than timed, so it states the invariant instead of a
/// threshold: a press and the moves that stay on the same column cost no
/// pass, and the one that crosses a column costs exactly one.
#[test]
fn a_divider_drag_that_moves_nothing_lays_out_nothing() {
    use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
    use fresh::model::event::SplitDirection;

    let mut h = EditorTestHarness::new(80, 24).expect("harness");
    h.editor_mut()
        .dispatch_action_for_tests(Action::SplitVertical);
    h.render().expect("render");

    let separators = h.editor().get_separator_areas().to_vec();
    let (_, direction, sep_x, sep_y, sep_length) = separators[0];
    assert_eq!(direction, SplitDirection::Vertical, "a vertical divider");

    let at = |kind, col, row| MouseEvent {
        kind,
        column: col,
        row,
        modifiers: crossterm::event::KeyModifiers::empty(),
    };
    let row = sep_y + sep_length / 2;

    h.send_mouse(at(MouseEventKind::Down(MouseButton::Left), sep_x, row))
        .expect("press the divider");
    let _ = stats::take();

    // Four reports that slide *along* the divider: the column never changes,
    // so neither does the ratio, so there is nothing to lay out.
    for dy in 1..=4u16 {
        h.send_mouse(at(
            MouseEventKind::Drag(MouseButton::Left),
            sep_x,
            row.saturating_sub(dy),
        ))
        .expect("drag along the divider");
    }
    assert_eq!(
        stats::take(),
        LayoutCounts {
            shell: 0,
            offscreen_grids: 0
        },
        "a drag that stays on the divider's column reflows nothing",
    );

    // And one that does cross a column still reflows, exactly once.
    h.send_mouse(at(MouseEventKind::Drag(MouseButton::Left), sep_x + 6, row))
        .expect("drag across a column");
    assert_eq!(
        stats::take(),
        ONE_PASS,
        "the report that moves the divider reflows once",
    );

    h.send_mouse(at(MouseEventKind::Up(MouseButton::Left), sep_x + 6, row))
        .expect("release");
}
