//! The pane showing a group tab shows the group, not the buffer it showed
//! before.
//!
//! A pane that switches to a group tab (Review Diff) keeps the buffer it was
//! on as the tab to return to. That buffer is not on screen, and nothing of
//! it may be: its scrollbar used to stay painted in the pane's own bar
//! column beside the group panel's bar, and a click on it switched the pane
//! back to the hidden file and scrolled it.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use fresh::config::Config;
use std::fs;

const WIDTH: u16 = 100;
const HEIGHT: u16 = 30;

fn review_diff_showing(h: &EditorTestHarness) -> bool {
    let screen = h.screen_to_string();
    screen.contains("next hunk") && !screen.contains("Generating Review")
}

#[test]
fn test_group_tab_pane_has_no_bar_of_the_hidden_buffer() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);

    // Long enough that its own scrollbar has a thumb well short of the track.
    let body = |changed: bool| -> String {
        (1..=500)
            .map(|n| match (changed, n) {
                (true, 100) => "changed line\n".to_string(),
                _ => format!("hidden file line {n}\n"),
            })
            .collect()
    };
    let big = repo.create_file("big.txt", &body(false));
    repo.git_add_all();
    repo.git_commit("baseline");
    repo.create_file("big.txt", &body(true));

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        WIDTH,
        HEIGHT,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&big).unwrap();
    harness.render().unwrap();

    let pane = harness
        .editor()
        .active_window()
        .split_manager()
        .active_split();
    assert!(
        harness.editor().pane_vscroll_rect(pane).is_some(),
        "the file's pane has a scrollbar while it shows the file"
    );

    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.wait_until(review_diff_showing).unwrap();

    assert_eq!(
        harness.editor().pane_vscroll_rect(pane),
        None,
        "the pane showing the Review Diff group has no bar of its own: the \
         group's panels have theirs, and big.txt is not on screen"
    );

    // Where the hidden file's bar used to be: the rightmost column, halfway
    // down. It belongs to the group now, so the pane stays on it.
    harness.mouse_click(WIDTH - 1, HEIGHT / 2).unwrap();
    assert!(
        review_diff_showing(&harness),
        "a click at the pane's right edge left the Review Diff tab:\n{}",
        harness.screen_to_string()
    );
    assert!(
        !harness.screen_to_string().contains("hidden file line 2"),
        "the hidden file came back on screen:\n{}",
        harness.screen_to_string()
    );
}
