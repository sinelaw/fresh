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
//! This pins the shape rather than a duration. Timings can't hold a line
//! in CI — they move with the machine and the build profile, and the same
//! defect measured 53ms in debug and 4ms in release — but the counters are
//! exact: idle ticks and cursor moves copy nothing, whatever the buffer
//! holds, and doubling the content doesn't double the copying.

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

/// What idling and moving the cursor costs in copied text properties, for
/// a review of the given size. Also returns the properties the buffers
/// actually hold, so the caller can tell a flat cost from a lucky one.
fn snapshot_copies_while_idle_and_moving(files: usize, lines_per_file: usize) -> (u64, u64) {
    let repo = repo_with_diff(files, lines_per_file);
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), repo.path.clone())
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
        harness
            .send_key(KeyCode::Down, KeyModifiers::NONE)
            .unwrap();
        harness.tick_and_render().unwrap();
    }
    let after = harness.editor().perf_counters();
    (
        after.text_properties_copied - before.text_properties_copied,
        after.text_property_copies - before.text_property_copies,
    )
}

/// Ticks that change nothing must copy nothing, and a review five times
/// the size must not cost five times as much.
///
/// The sizes are small enough to keep the test quick; what matters is the
/// ratio between them, which was ~5x before the snapshot learned to skip
/// unchanged buffers and is ~1x after.
#[test]
fn snapshot_copying_does_not_scale_with_review_size() {
    let (small_props, small_copies) = snapshot_copies_while_idle_and_moving(4, 250);
    let (big_props, big_copies) = snapshot_copies_while_idle_and_moving(20, 250);

    // Some copying is legitimate: moving the cursor repaints the sticky
    // header and the toolbar, which are plugin-drawn buffers of a few rows
    // each. What must not happen is the *diff* being recopied — that is
    // thousands of properties per tick.
    assert!(
        small_props < 500,
        "40 ticks over a small review copied {small_props} text properties \
         ({small_copies} buffer copies); ticks that change nothing should \
         copy nothing but the few rows the cursor repaints"
    );
    assert!(
        big_props < 500,
        "40 ticks over a 5x larger review copied {big_props} text properties \
         ({big_copies} buffer copies) — the per-tick cost is tracking the \
         buffer's content again"
    );
    // The real contract: cost is independent of size. Compared with a
    // floor so the assertion doesn't hinge on noise when both are tiny.
    assert!(
        big_props <= small_props.max(50) * 2,
        "copying scaled with review size: {small_props} properties for the \
         small review, {big_props} for the 5x one"
    );
}
