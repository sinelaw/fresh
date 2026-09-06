//! A buffer-group panel reports the geometry it is given.
//!
//! A side panel that starts hidden is laid out for the first time on the
//! frame it is *shown*, and after that it never changes size on its own.
//! The host used to skip `viewport_changed` for a split it had not seen
//! before ("new splits are not established yet"), so that first layout —
//! the only announcement such a panel's size ever gets — could be
//! swallowed, and a plugin that lays its own rows out to the panel width
//! was left guessing. That is what left the review diff's FILES sidebar
//! eliding filenames to a guess until the user dragged the divider
//! (`review_diff_ux_bugs::test_files_panel_rows_fit_a_narrow_panel` is
//! that symptom end to end).
//!
//! This pins the contract the plugin side now relies on: show a panel and
//! its width arrives, with no resize to prompt it.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use fresh::config::Config;
use std::fs;

const PLUGIN_SRC: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_panel_viewport_report.ts"
));

/// Run a palette command by name and wait for the prompt to close.
fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness.run_palette_command(name).unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

/// The `side=<n>` figure from the probe plugin's status line.
fn reported_side_width(harness: &mut EditorTestHarness) -> i64 {
    run_command(harness, "TestVP: Report");
    harness
        .wait_until(|h| h.screen_to_string().contains("TestVP: side="))
        .unwrap();
    let screen = harness.screen_to_string();
    let tail = screen
        .split("TestVP: side=")
        .nth(1)
        .expect("the report is on screen");
    tail.split_whitespace()
        .next()
        .expect("a width follows `side=`")
        .parse()
        .expect("the width is a number")
}

#[test]
fn test_panel_shown_for_the_first_time_reports_its_width() {
    init_tracing_from_env();

    let temp = tempfile::TempDir::new().unwrap();
    let project_root = temp.path().join("project");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(
        plugins_dir.join("test_panel_viewport_report.ts"),
        PLUGIN_SRC,
    )
    .unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "TestVP: Create");
    harness
        .wait_until(|h| h.screen_to_string().contains("MAIN-PANEL-MARKER"))
        .unwrap();

    // Hidden: never laid out, so there is nothing for the host to report.
    assert_eq!(
        reported_side_width(&mut harness),
        -1,
        "a hidden panel has no geometry to report. Screen:\n{}",
        harness.screen_to_string()
    );

    run_command(&mut harness, "TestVP: Show");
    harness
        .wait_until(|h| h.screen_to_string().contains("SIDE-PANEL-MARKER"))
        .unwrap();
    // A couple of frames for the hook to make the round trip.
    harness.tick_and_render().unwrap();
    harness.tick_and_render().unwrap();

    let width = reported_side_width(&mut harness);
    assert!(
        width > 0 && width < 120,
        "showing the panel must report the width the host gave it — a \
         quarter of a 120-column screen, not {width}. Screen:\n{}",
        harness.screen_to_string()
    );
    // The layout gives `side` a quarter of the group; the exact column
    // count depends on separators, so pin the neighbourhood rather than
    // the number.
    assert!(
        (20..=35).contains(&width),
        "the reported width should be the panel's own quarter-share, got \
         {width}. Screen:\n{}",
        harness.screen_to_string()
    );
}
