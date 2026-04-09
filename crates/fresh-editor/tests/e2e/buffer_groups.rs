//! End-to-end tests for the buffer-group rendering/input-routing plumbing,
//! independent of any concrete plugin like theme_editor or audit_mode.
//!
//! These tests use the tiny `test_buffer_groups` plugin which opens a
//! two-panel group (`left` / `right`) and exposes a `TestBG: Which`
//! command that reports the currently focused buffer via the status
//! bar. That lets us verify focus routing without being coupled to the
//! specific UI of the production plugins.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Copy the `test_buffer_groups` plugin into the given project root.
///
/// The plugin source is embedded at compile time via `include_str!` so
/// the test doesn't depend on `CARGO_MANIFEST_DIR` being set at runtime
/// (which it isn't when the test binary is run outside cargo).
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

/// Run the "TestBG: Create" command and wait for both panel markers
/// to be visible on screen.
fn open_test_bg(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("TestBG: Create").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("LEFT-PANEL-MARKER") && s.contains("RIGHT-PANEL-MARKER")
        })
        .unwrap();
}

/// Run the "TestBG: Which" command and return the reported focus via
/// the status bar — one of "LEFT", "RIGHT", "OTHER", or `None` if the
/// command wasn't executed / status not updated.
fn run_which(harness: &mut EditorTestHarness) -> Option<&'static str> {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("TestBG: Which").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("TestBG: FOCUS="))
        .unwrap();
    let s = harness.screen_to_string();
    if s.contains("TestBG: FOCUS=LEFT") {
        Some("LEFT")
    } else if s.contains("TestBG: FOCUS=RIGHT") {
        Some("RIGHT")
    } else if s.contains("TestBG: FOCUS=OTHER") {
        Some("OTHER")
    } else {
        None
    }
}

/// Locate the screen coordinates of a marker substring. Returns
/// (col, row) of the first cell of the match, or panics if not found.
fn find_marker_position(harness: &EditorTestHarness, marker: &str) -> (u16, u16) {
    let screen = harness.screen_to_string();
    for (row_idx, line) in screen.lines().enumerate() {
        if let Some(col) = line.find(marker) {
            return (col as u16, row_idx as u16);
        }
    }
    panic!(
        "marker {:?} not found on screen:\n{}",
        marker,
        harness.screen_to_string()
    );
}

/// Reproduces the cursor-routing bug fixed alongside the buffer-groups
/// refactor: clicking inside the RIGHT panel of a buffer group must
/// route the click (focus, cursor events) to the RIGHT panel's buffer,
/// not to the main split's background buffer or some other buffer.
///
/// Previously `handle_editor_click` called `focus_split(inner_leaf, ...)`
/// but `inner_leaf` is not in `SplitManager`'s main tree (Grouped
/// subtrees live in a side-map), so `set_active_split` silently failed
/// and the subsequent `apply_event_to_active_buffer` applied cursor
/// movements to the wrong buffer (determined by `active_buffer()` which
/// was still pointing at the previously-focused panel).
#[test]
fn test_group_panel_click_routes_to_clicked_panel() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    setup_test_buffer_groups_plugin(&project_root);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    // Open a 2-panel buffer group.
    open_test_bg(&mut harness);

    // After `createBufferGroup`, the plugin doesn't explicitly focus
    // either panel. Our Rust-side create_buffer_group defaults to the
    // first scrollable panel (left). Verify with TestBG: Which.
    assert_eq!(
        run_which(&mut harness),
        Some("LEFT"),
        "after TestBG: Create, focus should default to LEFT panel"
    );

    // Locate the RIGHT panel marker on screen and click on it.
    let (right_col, right_row) = find_marker_position(&harness, "RIGHT-PANEL-MARKER");
    // Click in the middle of the marker text to be robust against
    // any cell-column vs char-index mismatch for a plain-ASCII marker.
    let click_col = right_col + 5;
    harness.mouse_click(click_col, right_row).unwrap();
    harness.render().unwrap();

    // After the click, a subsequent TestBG: Which should report RIGHT.
    assert_eq!(
        run_which(&mut harness),
        Some("RIGHT"),
        "after clicking the RIGHT panel, focus should be RIGHT"
    );

    // And clicking back on the LEFT panel should re-route focus there.
    let (left_col, left_row) = find_marker_position(&harness, "LEFT-PANEL-MARKER");
    harness.mouse_click(left_col + 5, left_row).unwrap();
    harness.render().unwrap();

    assert_eq!(
        run_which(&mut harness),
        Some("LEFT"),
        "after clicking the LEFT panel, focus should be LEFT"
    );
}
