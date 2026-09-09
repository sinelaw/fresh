//! The per-tick cost of a decorated buffer must not grow with what the
//! buffer holds.
//!
//! A frame and a tick are supposed to cost what is on screen. The plugin
//! state snapshot broke that: it copied every text property of every
//! buffer on every tick, and a plugin-drawn buffer carries one per row —
//! 20 000 for a review diff, 100 000 for a large one, cloned between
//! frames whether or not anything had changed. Reviewing a 100-commit
//! range cost ~3.8s per keystroke, and the review plugin had grown a
//! layout budget to compensate.
//!
//! The snapshot now shares each buffer's property set instead of copying
//! it, so a tick's work is one refcount bump per buffer and cannot track
//! their content at all.
//!
//! This pins the shape rather than a duration. Timings can't hold a line
//! in CI — they move with the machine and the build profile, and the same
//! defect measured 53ms in debug and 4ms in release — but the counters are
//! exact: what a tick does is proportional to the number of buffers, and a
//! review five times the size gives the same number.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

/// A repo whose working tree differs from HEAD by `files * lines * 2` diff
/// lines, with the review plugin available.
fn repo_with_diff(files: usize, lines_per_file: usize) -> GitTestRepo {
    let repo = GitTestRepo::new();
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);

    for f in 0..files {
        let body: String = (0..lines_per_file)
            .map(|l| format!("fn original_{f}_{l}() {{ let x = {l}; }}\n"))
            .collect();
        repo.create_file(&format!("src/mod{}/file{}.rs", f % 3, f), &body);
    }
    repo.git_add_all();
    repo.git_commit("baseline");
    for f in 0..files {
        let body: String = (0..lines_per_file)
            .map(|l| format!("fn changed_{f}_{l}() {{ let y = {l}; }}\n"))
            .collect();
        repo.create_file(&format!("src/mod{}/file{}.rs", f % 3, f), &body);
    }
    repo
}

/// What idling and moving the cursor costs the snapshot, for a review of
/// the given size: `(property sets shared, properties copied)`.
fn snapshot_work_while_idle_and_moving(files: usize, lines_per_file: usize) -> (u64, u64) {
    let repo = repo_with_diff(files, lines_per_file);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk") && !s.contains("Generating Review")
        })
        .unwrap();
    // A rewritten file diffs as removals first, so this is the first row.
    harness
        .wait_until(|h| h.screen_to_string().contains("original_0_0"))
        .unwrap();

    // Let the stream settle: the build itself legitimately copies, and
    // this test is about the ticks *after* a change, not the change.
    for _ in 0..10 {
        harness.tick_and_render().unwrap();
    }

    let before = harness.editor().perf_counters();
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
    }
    for _ in 0..20 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.tick_and_render().unwrap();
    }
    let after = harness.editor().perf_counters();
    (
        after.text_property_shares - before.text_property_shares,
        after.text_properties_copied - before.text_properties_copied,
    )
}

/// A review five times the size must not cost five times as much per
/// tick, and no tick may copy a property at all.
#[test]
fn snapshot_work_does_not_scale_with_review_size() {
    let (small_shares, small_copied) = snapshot_work_while_idle_and_moving(4, 250);
    let (big_shares, big_copied) = snapshot_work_while_idle_and_moving(20, 250);

    assert_eq!(
        small_copied, 0,
        "the snapshot shares property sets; copying {small_copied} of them \
         back means a tick's cost tracks the buffers' content again"
    );
    assert_eq!(
        big_copied, 0,
        "the 5x review copied {big_copied} properties into the snapshot"
    );

    // Sharing is per buffer, and both reviews open the same panels, so the
    // counts should match outright. Compared with a small tolerance rather
    // than exactly: the number of ticks a review takes to settle is not
    // something this test should pin.
    assert!(
        small_shares > 0,
        "the snapshot should be sharing something — the counter never moved, \
         so this test is not measuring the path it claims to"
    );
    assert!(
        big_shares <= small_shares * 2,
        "per-tick work scaled with review size: {small_shares} shares for \
         the small review, {big_shares} for the 5x one"
    );
}
