//! A tab name is shortened only when the tabs do not fit, and that is
//! decided in the frame the strip is laid out in: a resize or a tab that
//! opens shows its answer in the frame that brings it, not the next one.

use crate::common::harness::EditorTestHarness;
use std::fs;

const LONG: &str = "a_rather_long_file_name_for_a_tab_strip";

/// The strip's row: the one under the menu bar.
fn tab_row(h: &EditorTestHarness) -> String {
    h.screen_to_string()
        .lines()
        .nth(1)
        .unwrap_or_default()
        .to_string()
}

/// Whether the strip shows a name shortened to the cap. A whole name the
/// window scrolls across is cut by the window's edge, not elided.
fn capped(h: &EditorTestHarness) -> bool {
    tab_row(h).contains("a_rather_long_file_name_…")
}

#[test]
fn a_resize_caps_the_names_in_the_frame_it_arrives_in() {
    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().join(format!("{LONG}.rs"));
    fs::write(&path, "x\n").unwrap();
    let mut h = EditorTestHarness::new(160, 20).unwrap();
    h.open_file(&path).unwrap();
    h.render().unwrap();
    assert!(!capped(&h), "whole: {}", tab_row(&h));

    // `resize` renders once: that frame is the narrow one.
    h.resize(40, 20).unwrap();
    assert!(
        capped(&h),
        "capped in the frame that narrowed the strip: {}",
        h.screen_to_string()
    );

    h.resize(160, 20).unwrap();
    assert!(
        !capped(&h),
        "whole in the frame that widened it: {}",
        h.screen_to_string()
    );
}

#[test]
fn a_tab_that_does_not_fit_caps_the_names_in_the_frame_it_opens_in() {
    let dir = tempfile::TempDir::new().unwrap();
    let mut h = EditorTestHarness::new(120, 20).unwrap();
    let first = dir.path().join(format!("{LONG}_0.rs"));
    fs::write(&first, "x\n").unwrap();
    h.open_file(&first).unwrap();
    h.render().unwrap();
    assert!(!capped(&h), "whole: {}", tab_row(&h));

    for i in 1..3 {
        let p = dir.path().join(format!("{LONG}_{i}.rs"));
        fs::write(&p, "x\n").unwrap();
        h.open_file(&p).unwrap();
    }
    h.render().unwrap();
    assert!(
        capped(&h),
        "capped in the frame the tabs opened in: {}",
        h.screen_to_string()
    );
}

/// Three long names on a narrow strip, drawn for the first time: the frame
/// that first lays the strip out is the one that knows it is narrow.
#[test]
fn the_first_frame_of_a_narrow_strip_caps_its_names() {
    let dir = tempfile::TempDir::new().unwrap();
    let mut h = EditorTestHarness::new(80, 20).unwrap();
    for i in 0..3 {
        let p = dir.path().join(format!("{LONG}_{i}.rs"));
        fs::write(&p, "x\n").unwrap();
        h.open_file(&p).unwrap();
    }
    h.render().unwrap();
    assert!(
        capped(&h),
        "capped on the first frame: {}",
        h.screen_to_string()
    );
}
