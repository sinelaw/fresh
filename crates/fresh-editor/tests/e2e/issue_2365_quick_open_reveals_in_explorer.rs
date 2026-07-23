//! E2E regression tests for issue #2365: using Quick Open (Ctrl+P) to jump to
//! a file should also expand the file explorer down to that file — but only
//! when `file_explorer.follow_active_buffer` is enabled.
//!
//! Repro (manual):
//!   1. Open the file explorer (Ctrl+B) with a nested project; leave a
//!      sub-directory collapsed.
//!   2. With focus in the editor, press Ctrl+P and jump to a file inside that
//!      collapsed sub-directory.
//!   3. Expected (follow on, now the default): the explorer expands the
//!      ancestor directories and reveals the freshly opened file.
//!   4. Actual (before the fix): the explorer stayed put — the sub-directory
//!      remained collapsed even with the setting on, because the jump path
//!      never triggered the follow sync.
//!
//! The fix makes `follow_active_buffer` default to `true` *and* wires the jump
//! path (Quick Open / Open File / Live Grep) into the same follow sync, gated
//! on the setting. The two tests below lock in both halves of that contract:
//! with the setting on, a jump reveals the file; with it off, a jump must leave
//! the explorer exactly where it was.
//!
//! <https://github.com/sinelaw/fresh/issues/2365>

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

/// Returns `true` when `name` appears on a file-explorer *tree* line (one that
/// also carries a tree connector), mirroring the `wait_for_file_explorer_item`
/// heuristic. This distinguishes the tree from the tab bar, which shows the
/// opened file's name without a tree connector. Reads only rendered output.
fn explorer_tree_shows(harness: &EditorTestHarness, name: &str) -> bool {
    harness.screen_to_string().lines().any(|line| {
        line.contains(name) && (line.contains('│') || line.contains('>') || line.contains('▼'))
    })
}

/// Build a temp project whose `nested/` directory (holding the jump target)
/// starts collapsed, open the explorer with focus in the editor, and assert the
/// precondition that the target is not yet revealed. The `sibling/` directory
/// keeps the root from collapsing into a single compacted chain.
fn setup(config: Config) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    fs::create_dir_all(project_root.join("nested")).unwrap();
    fs::create_dir_all(project_root.join("sibling")).unwrap();
    fs::write(
        project_root.join("nested/deep_target.txt"),
        "needle-content",
    )
    .unwrap();
    fs::write(project_root.join("sibling/other.txt"), "other").unwrap();

    // Open the explorer but keep focus in the editor — the exact scenario from
    // the report (sidebar open while editing).
    harness.editor_mut().toggle_file_explorer();
    harness.editor_mut().active_window_mut().focus_editor();

    // Wait for the tree to load (the collapsed `nested` row appears).
    harness.wait_for_file_explorer_item("nested").unwrap();

    // Precondition: `nested` is collapsed, so its child is not in the tree yet.
    assert!(
        !explorer_tree_shows(&harness, "deep_target.txt"),
        "Precondition: `nested` should be collapsed (deep_target.txt not yet \
         visible) before the Quick Open jump.\nScreen:\n{}",
        harness.screen_to_string()
    );
    harness
}

/// Jump to `nested/deep_target.txt` via Quick Open and wait until the buffer's
/// content renders, proving the file actually opened (independently of whatever
/// the explorer does).
fn quick_open_jump(harness: &mut EditorTestHarness) {
    // Ctrl+P opens command mode by default; Backspace drops the command prefix
    // to switch to file mode.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("deep_target.txt").unwrap();
    // Wait for the picker to actually surface the match before committing. The
    // file list is populated asynchronously (a directory walk / `git ls-files`
    // provider), so on slower filesystems pressing Enter immediately can commit
    // an empty result set and open nothing — which showed up as a Windows-only
    // timeout. The result row renders the relative path with forward slashes on
    // every platform ("nested/deep_target.txt"), which the typed input line
    // ("deep_target.txt") does not contain, so this is an unambiguous signal.
    harness
        .wait_until(|h| h.screen_to_string().contains("nested/deep_target.txt"))
        .expect("Quick Open should list nested/deep_target.txt before it is opened");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("needle-content"))
        .expect("Quick Open should open the jumped-to file");
}

/// With `follow_active_buffer` on (the default), a Quick Open jump expands the
/// explorer down to the opened file. Before the fix this row never appeared and
/// the wait timed out.
#[test]
fn test_quick_open_jump_reveals_file_when_following() {
    let mut harness = setup(Config::default());
    quick_open_jump(&mut harness);
    harness
        .wait_for_file_explorer_item("deep_target.txt")
        .expect(
        "with follow_active_buffer on, a Quick Open jump should expand the explorer to the file",
    );
}

/// With `follow_active_buffer` off, a Quick Open jump must NOT move the
/// explorer: `nested` stays collapsed and its child never appears in the tree,
/// even though the file itself opens. Guards against the reveal being wired up
/// unconditionally.
#[test]
fn test_quick_open_jump_leaves_explorer_when_not_following() {
    let mut config = Config::default();
    config.file_explorer.follow_active_buffer = false;

    let mut harness = setup(config);
    quick_open_jump(&mut harness);

    // The reveal is gated synchronously on the setting (neither the jump-path
    // trigger nor the passive `set_active_buffer` hook schedules a sync when
    // it is off), so once the file has opened the tree state is final.
    assert!(
        !explorer_tree_shows(&harness, "deep_target.txt"),
        "with follow_active_buffer off, a Quick Open jump must NOT expand the \
         explorer to the target.\nScreen:\n{}",
        harness.screen_to_string()
    );
    // The explorer is still open (the collapsed `nested` row is present), so the
    // assertion above means "collapsed", not "explorer absent".
    assert!(
        explorer_tree_shows(&harness, "nested"),
        "the file explorer should still be showing the collapsed `nested` \
         directory.\nScreen:\n{}",
        harness.screen_to_string()
    );
}
