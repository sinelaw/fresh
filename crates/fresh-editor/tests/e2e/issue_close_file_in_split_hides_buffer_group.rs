//! E2E reproduction for the "buffer group disappears when closing the
//! sibling file in a split" bug (reproduced manually in tmux):
//!
//! 1. Open an empty editor.
//! 2. Open a buffer group panel (the manual repro used "Git Log"; here we
//!    use the `test_buffer_groups` plugin which opens an equivalent group
//!    tab with on-screen markers).
//! 3. Open a code file (`main.rs`) as a second tab in the same split.
//! 4. Split vertically — the new split also shows `main.rs`.
//! 5. Move focus back to the left split (which holds the group tab AND
//!    `main.rs`).
//! 6. Close `main.rs`.
//!
//! Expected: the group panel is the only tab left in the left split, so
//! it becomes visible. Bug: the whole left split is torn down instead,
//! leaving only the right split's `main.rs`, and the group panel vanishes.
//!
//! Root cause: `close_tab_in_split` counts the split's tabs via
//! `buffer_tab_ids_vec()`, which excludes group tabs. With `main.rs` also
//! open in the other split (`is_last_viewport == false`), the left split's
//! buffer-tab count is 1 (the group is invisible to the count), so the
//! `split_tabs.len() <= 1` branch closes the entire split instead of
//! falling back to the remaining group tab.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Copy the `test_buffer_groups` plugin into the given project root.
fn setup_test_buffer_groups_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);

    const PLUGIN_SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_buffer_groups.ts"
    ));
    let dst = plugins_dir.join("test_buffer_groups.ts");
    fs::write(&dst, PLUGIN_SRC)
        .unwrap_or_else(|e| panic!("Failed to write test_buffer_groups.ts to {:?}: {}", dst, e));
}

/// Run a command through the command palette by name.
fn run_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Open the 2-panel test buffer group and wait for its markers.
fn open_test_bg(harness: &mut EditorTestHarness) {
    run_command(harness, "TestBG: Create");
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("LEFT-PANEL-MARKER") && s.contains("RIGHT-PANEL-MARKER")
        })
        .unwrap();
}

#[test]
fn test_closing_file_in_split_keeps_buffer_group_visible() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    setup_test_buffer_groups_plugin(&project_root);

    // A code file to open alongside the group.
    let main_rs = project_root.join("main.rs");
    fs::write(
        &main_rs,
        "fn main() {\n    println!(\"FILE-CONTENT-MARKER\");\n}\n",
    )
    .unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    // Open main.rs first — this replaces the startup empty `[No Name]`
    // buffer, so the split's only buffer-tab is main.rs. (In the manual
    // repro the "Git Log" panel left no separate `[No Name]` tab either.)
    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("FILE-CONTENT-MARKER");
    harness.assert_screen_not_contains("[No Name]");

    // Open the buffer group panel (acts like "Git Log") as a second tab in
    // the same split. The split now holds exactly [main.rs, *TestBG*], with
    // the group as the active tab.
    open_test_bg(&mut harness);

    // Re-focus main.rs (re-opening focuses the existing tab and clears the
    // active group tab), matching the manual repro where main.rs is the
    // active tab right before the split.
    harness.open_file(&main_rs).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("FILE-CONTENT-MARKER");

    // Split vertically — the new (right) split also shows main.rs, so
    // main.rs is now open in two splits.
    run_command(&mut harness, "Split Vertical");
    {
        let screen = harness.screen_to_string();
        let count = screen.matches("FILE-CONTENT-MARKER").count();
        assert!(
            count >= 2,
            "Precondition: after splitting, main.rs should be visible in \
             both splits (found {count}). Screen:\n{screen}"
        );
    }

    // Move focus back to the left split (which holds [main.rs, *TestBG*]).
    run_command(&mut harness, "Previous Split");

    // Close main.rs in the left split.
    run_command(&mut harness, "Close Buffer");
    harness.render().unwrap();

    // The buffer group should now be the visible tab in the left split.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LEFT-PANEL-MARKER"),
        "After closing main.rs in the left split, the buffer group panel \
         should become visible (it was the only remaining tab there). \
         Instead the whole split was torn down and the group vanished. \
         Screen:\n{screen}"
    );
}
