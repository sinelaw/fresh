//! E2E regression test for the compact-directory chain feature.
//!
//! Repro (manual): with `compact_directories` on (default), pressing Enter
//! on a directory whose subtree folds into a single-child chain leaves the
//! cursor pointing at the now-absorbed directory. The selected node id is
//! no longer in `filtered_visible_nodes()`, so:
//!   - the rendered cursor indicator (`▌`) disappears entirely;
//!   - the hardware cursor is no longer placed on the explorer pane;
//!   - subsequent Down/Up arrow presses silently no-op because
//!     `select_next/prev` look up the cursor by id within the visible
//!     list, find nothing, and do nothing.
//!
//! This test drives real key events end-to-end and asserts on the
//! rendered screen output: it expands a chain, verifies the compact
//! `foo/a/b/c` breadcrumb appears as a single rendered row, verifies the
//! hardware cursor is still placed on the explorer pane, and verifies
//! Down arrow actually moves the cursor row down.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

#[test]
fn test_compact_chain_keeps_cursor_on_visible_row_after_enter() {
    // Layout (mirrors the unit-test fixture in view::file_tree::view):
    //
    //   <root>/
    //     chain/a/b/c/leaf.txt   ← single-child chain
    //     sibling/other.txt      ← keeps root from itself becoming a
    //                              chain anchor
    //
    // With compact mode on, expanding `chain` should fold `chain → a → b`
    // into `c`'s row and render it as a single `chain/a/b/c` breadcrumb.
    let mut harness = EditorTestHarness::with_temp_project(120, 30).unwrap();
    let project_root = harness.project_dir().unwrap();
    fs::create_dir_all(project_root.join("chain/a/b/c")).unwrap();
    fs::write(project_root.join("chain/a/b/c/leaf.txt"), "leaf").unwrap();
    fs::create_dir_all(project_root.join("sibling")).unwrap();
    fs::write(project_root.join("sibling/other.txt"), "other").unwrap();

    // Open the file explorer with the default Ctrl+E binding.
    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("File Explorer"))
        .unwrap();

    // Wait for the root's children to populate. Root auto-expands on
    // open (its only child is the project root row, which has multiple
    // visible children — `chain` and `sibling` — so it isn't itself a
    // chain anchor).
    harness.wait_for_file_explorer_item("chain").unwrap();
    harness.wait_for_file_explorer_item("sibling").unwrap();

    // Cursor starts on the project-root row. Press Down to land on the
    // first child — `chain` (directories sort before `sibling` under the
    // default Type sort).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    // Sanity: pre-expand the screen shows `chain` as a separate row, no
    // breadcrumb yet. We're really just checking the test got us to a
    // pre-expand state where the chain hasn't been folded.
    let before = harness.screen_to_string();
    assert!(
        before.contains("chain"),
        "expected `chain` row before expansion, got:\n{before}"
    );
    assert!(
        !before.contains("chain/a/b/c"),
        "did not expect compact breadcrumb before expansion, got:\n{before}"
    );

    // Press Enter on `chain`. With compact mode on, this expands the
    // entire `chain → a → b → c` chain in one shot and folds the row
    // into `c`'s anchor.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // Expansion is async — wait for the compact breadcrumb itself to
    // hit the screen.
    harness
        .wait_until(|h| h.screen_to_string().contains("chain/a/b/c"))
        .unwrap();

    let after_enter = harness.screen_to_string();

    // The compact breadcrumb is rendered as a single row.
    assert!(
        after_enter.contains("chain/a/b/c"),
        "expected compact `chain/a/b/c` breadcrumb after Enter, got:\n{after_enter}"
    );
    // None of the absorbed ancestors should remain on a separate row —
    // i.e. the per-line text `chain` (with no slash) and standalone `a`
    // / `b` rows are gone. (We check the chained breadcrumb form is
    // present and the bare-name forms are not their own rows by
    // searching for tree-indicator + bare-name on a single line.)
    let absorbed_as_row = |needle: &str| {
        after_enter.lines().any(|line| {
            let trimmed = line.trim_start_matches(['│', ' ']);
            // A row for `chain` would render with the expanded indicator
            // (`▼ chain`) since it would have to be expanded to lead a
            // chain. We look for that explicit pattern so we don't get
            // tricked by the breadcrumb itself (which contains `chain/`).
            trimmed.starts_with(&format!("▼ {needle} "))
                || trimmed.starts_with(&format!("▼ {needle}│"))
                || trimmed == format!("▼ {needle}")
        })
    };
    assert!(
        !absorbed_as_row("chain"),
        "absorbed dir `chain` should not render as its own row, got:\n{after_enter}"
    );
    assert!(
        !absorbed_as_row("a"),
        "absorbed dir `a` should not render as its own row, got:\n{after_enter}"
    );
    assert!(
        !absorbed_as_row("b"),
        "absorbed dir `b` should not render as its own row, got:\n{after_enter}"
    );

    // Regression #1: the cursor indicator (`▌`) must be drawn on the
    // compact breadcrumb row. Before the fix, the cursor sat on an
    // absorbed node whose id wasn't in the visible list, so the
    // renderer skipped drawing the indicator entirely (the explorer
    // only paints `▌` when `get_selected_index()` resolves the cursor
    // against the visible rows).
    let breadcrumb_row = after_enter
        .lines()
        .find(|line| line.contains("chain/a/b/c"))
        .unwrap_or_else(|| panic!("compact breadcrumb row missing from screen:\n{after_enter}"));
    assert!(
        breadcrumb_row.contains('▌'),
        "selected-row indicator `▌` should be drawn on the chain anchor row \
         after expansion (cursor was promoted from absorbed `chain` to `c`). \
         Breadcrumb row: {breadcrumb_row:?}\nScreen:\n{after_enter}"
    );

    // The hardware cursor (used for blinking) is also placed on that
    // same visible row.
    let (cursor_x_after, cursor_row_after) = harness.screen_cursor_position();
    assert!(
        cursor_row_after >= 1 && cursor_x_after >= 1,
        "hardware cursor should sit inside the explorer pane after expansion, \
         got ({cursor_x_after}, {cursor_row_after}). \
         A cursor at the origin sentinel indicates the renderer never placed \
         it on a visible explorer row. Screen:\n{after_enter}"
    );

    // Regression #2: arrow-key navigation must keep working after the
    // expansion. Down arrow should move the cursor from `c` to
    // `leaf.txt` (the next visible row). Before the fix, the cursor id
    // was missing from the visible list, so `select_next` no-op'd and
    // the cursor row didn't change.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    let _ = harness.editor_mut().process_async_messages();
    harness.render().unwrap();

    let (_, cursor_row_after_down) = harness.screen_cursor_position();
    assert_eq!(
        cursor_row_after_down,
        cursor_row_after + 1,
        "Down arrow should advance the cursor one row (chain anchor `c` → `leaf.txt`). \
         Before fix: the absorbed cursor id meant select_next() couldn't locate the cursor in \
         the visible list and silently no-op'd. \
         Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Every directory name on a compact chain row — the folded segments as well
/// as the anchor — takes `ui.file_explorer_directory_fg`, not the keyword
/// colour (#3697). Before, `chain/a/b/` were drawn in `syntax.keyword` and
/// only the anchor `c` in the folder colour.
#[test]
fn test_compact_chain_segments_use_the_directory_colour() {
    use crate::common::harness::HarnessOptions;
    use fresh::config::Config;
    use fresh::config_io::DirectoryContext;
    use ratatui::style::Color;

    let temp_dir = tempfile::TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let themes_dir = temp_dir.path().join("config").join("themes");
    fs::create_dir_all(&themes_dir).unwrap();
    fs::write(
        themes_dir.join("folders.json"),
        r#"{"name": "folders", "extends": "builtin://dark",
            "syntax": {"keyword": [237, 65, 65]},
            "ui": {"file_explorer_directory_fg": [200, 209, 217]}}"#,
    )
    .unwrap();

    let project_root = temp_dir.path().join("project_root");
    fs::create_dir_all(project_root.join("plugins")).unwrap();
    fs::create_dir_all(project_root.join("chain/a/b/c")).unwrap();
    fs::write(project_root.join("chain/a/b/c/leaf.txt"), "leaf").unwrap();
    fs::create_dir_all(project_root.join("sibling")).unwrap();
    fs::write(project_root.join("sibling/other.txt"), "other").unwrap();

    let config = Config {
        theme: "folders".to_string().into(),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    let directory_fg = Color::Rgb(200, 209, 217);
    assert_eq!(
        harness.editor().theme().file_explorer_directory_fg,
        directory_fg,
        "the custom theme should be the active one"
    );

    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer_item("chain").unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("chain/a/b/c"))
        .unwrap();
    // Move off the row so its colours are the neutral ones, not the cursor's.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (x, y) = harness.find_text_on_screen("chain/a/b/c").unwrap();
    for (offset, name) in [(0u16, "chain"), (6, "a"), (8, "b"), (10, "c")] {
        assert_eq!(
            harness.get_cell_style(x + offset, y).and_then(|s| s.fg),
            Some(directory_fg),
            "`{name}` on the compact row should be drawn in the directory colour"
        );
    }
}
