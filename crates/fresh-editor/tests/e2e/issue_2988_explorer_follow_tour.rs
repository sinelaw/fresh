//! E2E coverage for issue #2988: with `file_explorer.follow_active_buffer`
//! on, the explorer must follow along when a code tour opens a step's file.
//!
//! Repro (manual, reproduced in tmux 160x40 against this repo):
//!   1. Config `{ "file_explorer": { "follow_active_buffer": true } }`.
//!   2. Open the file explorer (Ctrl+E).
//!   3. Command palette -> `Tour: Load Definition...` -> `.fresh-tour.json`.
//!   4. Expected: the explorer expands to reveal the first step's file, the
//!      same way it does for quick-open or goto-definition.
//!   5. Actual: the tree stays parked at the root. Stepping forward *does*
//!      move it, which is the tell: the opening step is the one that lands
//!      in the session's untouched scratch buffer.
//!
//! Two independent defects are covered here, one test each:
//!
//!   * A file opened into the fresh session's empty `[No Name]` buffer
//!     replaces it *in place*: same buffer id, new file. `set_active_buffer`
//!     — where the follow sync hangs — early-returns on "already active", so
//!     nothing ever syncs. Every tour begins by opening a file, so its first
//!     step hit this every single time.
//!   * A sync request that arrives while an earlier expand is still in
//!     flight used to be discarded, with nothing to retry it. Any pair of
//!     opens fast enough to overlap left the tree on the first one.
//!
//! <https://github.com/sinelaw/fresh/issues/2988>

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;
use std::path::{Path, PathBuf};

/// A path as a JSON string literal — quotes and escapes included, so a
/// Windows path interpolated into the manifest stays valid JSON.
fn json_path(path: &Path) -> String {
    serde_json::to_string(&path.display().to_string()).expect("path is UTF-8")
}

/// A two-step tour whose step files live in *different* collapsed
/// subdirectories, so revealing either one requires the explorer to expand.
fn tour_json(root: &Path) -> String {
    let alpha = json_path(&root.join("alpha/step_one.rs"));
    let beta = json_path(&root.join("beta/step_two.rs"));
    format!(
        r###"{{
  "title": "Follow Tour",
  "description": "Two steps in two directories",
  "schema_version": "1.0",
  "steps": [
    {{
      "step_id": 1,
      "title": "First",
      "file_path": {alpha},
      "lines": [1, 2],
      "explanation": "The first step."
    }},
    {{
      "step_id": 2,
      "title": "Second",
      "file_path": {beta},
      "lines": [1, 2],
      "explanation": "The second step."
    }}
  ]
}}"###
    )
}

/// Project with the code-tour plugin, a manifest at the root, and the two
/// step files buried one directory down.
fn setup_tour_project() -> (tempfile::TempDir, PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    // Canonicalize before it reaches the manifest: on macOS a tempdir is a
    // symlink, and the editor stores the resolved path on the buffer, so an
    // unresolved manifest path never matches a step's buffer.
    let project_root = fs::canonicalize(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "code-tour");

    fs::create_dir(project_root.join("alpha")).unwrap();
    fs::create_dir(project_root.join("beta")).unwrap();
    fs::write(
        project_root.join("alpha/step_one.rs"),
        "fn one() {}\nfn one_more() {}\n",
    )
    .unwrap();
    fs::write(
        project_root.join("beta/step_two.rs"),
        "fn two() {}\nfn two_more() {}\n",
    )
    .unwrap();
    fs::write(
        project_root.join(".fresh-tour.json"),
        tour_json(&project_root),
    )
    .unwrap();

    (temp_dir, project_root)
}

/// The explorer sidebar's own columns, read off the rendered screen: every
/// panel row starts with the sidebar's left border, and the sidebar ends at
/// the first right-border glyph after it. Reading only these columns keeps
/// the tab bar, the editor pane and the tour dock out of the assertions.
fn explorer_panel_text(screen: &str) -> String {
    screen
        .lines()
        .filter(|line| line.starts_with('┌') || line.starts_with('└') || line.starts_with('│'))
        .map(|line| {
            let mut it = line.char_indices();
            let _left = it.next();
            let end = it
                .find(|(_, c)| matches!(c, '┐' | '┘' | '│'))
                .map(|(i, c)| i + c.len_utf8())
                .unwrap_or(line.len());
            line[..end].to_string()
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn explorer_shows(harness: &EditorTestHarness, name: &str) -> bool {
    explorer_panel_text(&harness.screen_to_string()).contains(name)
}

/// Open the explorer with the toggle key, then hand the keyboard back to the
/// editor — the state the reported repro is in once the tour takes over.
fn open_explorer(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();
    harness.editor_mut().active_window_mut().focus_editor();
    harness.render().unwrap();
}

/// Loading a tour opens its first step's file — into the session's untouched
/// scratch buffer, which is replaced in place rather than switched to. With
/// `follow_active_buffer` on, the explorer must still expand to reveal it,
/// and must keep up when the tour steps on to a file in another directory.
///
/// Before the fix neither happened for step 1: no buffer switch, so no sync,
/// so `step_one.rs` never appeared in the sidebar and this wait hangs until
/// nextest kills the test.
#[test]
fn test_explorer_follows_tour_steps() {
    let (_temp, project_root) = setup_tour_project();
    let mut config = Config::default();
    config.file_explorer.follow_active_buffer = true;

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(160, 40, config, project_root.clone())
            .unwrap();
    harness.render().unwrap();

    open_explorer(&mut harness);

    // Precondition: the step directories are collapsed, so neither step file
    // is on screen before the tour runs.
    assert!(
        !explorer_shows(&harness, "step_one.rs") && !explorer_shows(&harness, "step_two.rs"),
        "Precondition: `alpha/` and `beta/` should still be collapsed before \
         the tour opens anything.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Load the tour the way the user does, through the palette.
    harness
        .run_palette_command("Tour: Load Definition")
        .unwrap();
    // Wait for the plugin's own file-pick prompt: the palette is still
    // closing when the command fires, so any-prompt is not a sound wait.
    harness
        .wait_until(|h| h.screen_to_string().contains("tour file path"))
        .unwrap();
    let manifest = project_root.join(".fresh-tour.json");
    harness.type_text(&manifest.display().to_string()).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Step 1's file is revealed in the sidebar.
    harness
        .wait_until(|h| explorer_shows(h, "step_one.rs"))
        .unwrap();

    // The panel holds the keyboard once the step settles, so `n` advances
    // the tour. Wait for the panel to say so before pressing it, or the key
    // races the open → focus-handback chain and lands in the source file.
    harness
        .wait_until(|h| h.screen_to_string().contains("Step 1 of 2"))
        .unwrap();
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();

    // Step 2 lives in a different directory, so the tree has to move again.
    harness
        .wait_until(|h| explorer_shows(h, "step_two.rs"))
        .unwrap();
}

/// A follow request that overlaps an in-flight expand must not be lost.
///
/// Three opens back to back, with no editor tick in between, so the second
/// one's expand is guaranteed to still be running when the third arrives.
/// Before the fix the third request was discarded and nothing retried it:
/// the tree stayed on the second file for good. Written without the tour
/// because the race is not tour-specific — any burst of opens hits it.
#[test]
fn test_overlapping_follow_requests_are_not_dropped() {
    let mut config = Config::default();
    config.file_explorer.follow_active_buffer = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    for dir in ["dir_a", "dir_b", "dir_c"] {
        fs::create_dir_all(project_root.join(dir)).unwrap();
    }
    fs::write(project_root.join("dir_a/file_a.txt"), "a").unwrap();
    fs::write(project_root.join("dir_b/file_b.txt"), "b").unwrap();
    fs::write(project_root.join("dir_c/file_c.txt"), "c").unwrap();

    open_explorer(&mut harness);
    assert!(
        !explorer_shows(&harness, "file_c.txt"),
        "Precondition: `dir_c/` should still be collapsed.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // No render/tick between these, so nothing can install an expand result
    // in the gap: the third request necessarily overlaps the second's.
    for file in ["dir_a/file_a.txt", "dir_b/file_b.txt", "dir_c/file_c.txt"] {
        harness
            .editor_mut()
            .open_file(&project_root.join(file))
            .unwrap();
    }

    harness
        .wait_until(|h| explorer_shows(h, "file_c.txt"))
        .unwrap();
}

/// A deferred follow request still names its own file once the active buffer
/// has moved on to something with no file behind it.
///
/// This is the shape a code tour leaves behind: it opens the step's file and
/// then hands the keyboard straight back to its own panel, a virtual buffer
/// with no path. A retry that re-read "the active buffer's file" at that
/// point would find nothing to reveal and quietly give up, so the deferred
/// *path* is what gets replayed. Modelled here with a plain scratch buffer
/// rather than a tour, because nothing about it is tour-specific.
#[test]
fn test_deferred_follow_survives_a_pathless_active_buffer() {
    let mut config = Config::default();
    config.file_explorer.follow_active_buffer = true;

    let mut harness = EditorTestHarness::with_temp_project_and_config(120, 40, config).unwrap();
    let project_root = harness.project_dir().unwrap();

    for dir in ["dir_a", "dir_b"] {
        fs::create_dir_all(project_root.join(dir)).unwrap();
    }
    fs::write(project_root.join("dir_a/file_a.txt"), "a").unwrap();
    fs::write(project_root.join("dir_b/file_b.txt"), "b").unwrap();
    fs::write(project_root.join("seed.txt"), "seed").unwrap();

    open_explorer(&mut harness);
    assert!(
        !explorer_shows(&harness, "file_b.txt"),
        "Precondition: `dir_b/` should still be collapsed.\nScreen:\n{}",
        harness.screen_to_string()
    );

    // Consume the session's scratch buffer first, so both opens below are
    // genuine buffer switches and each one really does request a follow.
    harness
        .editor_mut()
        .open_file(&project_root.join("seed.txt"))
        .unwrap();

    // Again no tick in between: file_b's request lands while file_a's expand
    // still holds the tree, and the scratch buffer then takes over as the
    // active buffer before that expand can install.
    harness
        .editor_mut()
        .open_file(&project_root.join("dir_a/file_a.txt"))
        .unwrap();
    harness
        .editor_mut()
        .open_file(&project_root.join("dir_b/file_b.txt"))
        .unwrap();
    let _scratch = harness.editor_mut().new_buffer();

    harness
        .wait_until(|h| explorer_shows(h, "file_b.txt"))
        .unwrap();
}
