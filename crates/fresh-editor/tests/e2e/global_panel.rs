use crate::common::harness::EditorTestHarness;
use fresh_core::api::{GlobalPanelAnchor, GlobalPanelRow, PluginCommand};

fn row(text: &str) -> GlobalPanelRow {
    GlobalPanelRow {
        text: text.to_string(),
        style: None,
    }
}

fn styled_row(text: &str, style: &str) -> GlobalPanelRow {
    GlobalPanelRow {
        text: text.to_string(),
        style: Some(style.to_string()),
    }
}

fn show(harness: &mut EditorTestHarness, id: &str, rows: Vec<GlobalPanelRow>) {
    show_anchored(harness, id, rows, GlobalPanelAnchor::Bottom);
}

fn show_anchored(
    harness: &mut EditorTestHarness,
    id: &str,
    rows: Vec<GlobalPanelRow>,
    anchor: GlobalPanelAnchor,
) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::ShowGlobalPanel {
            id: id.to_string(),
            rows,
            anchor,
        })
        .unwrap();
}

// ---------------------------------------------------------------------------
// Basic show / hide / update
// ---------------------------------------------------------------------------

/// A newly created editor has no global panel visible.
#[test]
fn test_global_panel_hidden_by_default() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let h = 24u16;
    let above_status = harness.get_screen_row(h.saturating_sub(3).into());
    assert!(
        !above_status.starts_with('┌') && !above_status.starts_with('└'),
        "Expected no panel border above status bar by default.\nRow: {above_status}\nScreen:\n{screen}"
    );
}

/// Showing a panel renders its text content on screen.
#[test]
fn test_show_global_panel_renders_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(&mut harness, "p", vec![row("Hello from panel")]);
    harness.render().unwrap();
    harness.assert_screen_contains("Hello from panel");
}

/// Closing the panel removes its content from the screen.
#[test]
fn test_close_global_panel_removes_content() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(&mut harness, "p", vec![row("Panel content")]);
    harness.render().unwrap();
    harness.assert_screen_contains("Panel content");

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CloseGlobalPanel { id: "p".to_string() })
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Panel content");
}

/// Updating a panel replaces its rows in place.
#[test]
fn test_update_global_panel_replaces_rows() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(&mut harness, "p", vec![row("Original row")]);
    harness.render().unwrap();
    harness.assert_screen_contains("Original row");

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UpdateGlobalPanel {
            id: "p".to_string(),
            rows: vec![row("Updated row")],
        })
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Original row");
    harness.assert_screen_contains("Updated row");
}

/// Showing a panel with the same id replaces the existing one.
#[test]
fn test_show_same_id_replaces_panel() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(&mut harness, "dupe", vec![row("First version")]);
    harness.render().unwrap();
    harness.assert_screen_contains("First version");

    show(&mut harness, "dupe", vec![row("Second version")]);
    harness.render().unwrap();
    harness.assert_screen_not_contains("First version");
    harness.assert_screen_contains("Second version");
}

/// When two panels are shown the topmost (last added) one is rendered.
#[test]
fn test_topmost_panel_is_rendered() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(&mut harness, "a", vec![row("Bottom panel")]);
    show(&mut harness, "b", vec![row("Top panel")]);
    harness.render().unwrap();
    harness.assert_screen_contains("Top panel");
    harness.assert_screen_not_contains("Bottom panel");
}

/// Closing the top panel reveals the one underneath.
#[test]
fn test_closing_top_panel_reveals_bottom() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(&mut harness, "a", vec![row("Bottom panel")]);
    show(&mut harness, "b", vec![row("Top panel")]);
    harness.render().unwrap();
    harness.assert_screen_contains("Top panel");

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CloseGlobalPanel { id: "b".to_string() })
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Top panel");
    harness.assert_screen_contains("Bottom panel");
}

/// Multiple rows are all rendered inside the panel.
#[test]
fn test_panel_renders_multiple_rows() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show(
        &mut harness,
        "multi",
        vec![
            styled_row("Panel Title", "title"),
            styled_row("Group Header", "group-header"),
            row("Normal row"),
            styled_row("Hint text", "hint"),
        ],
    );
    harness.render().unwrap();
    harness.assert_screen_contains("Panel Title");
    harness.assert_screen_contains("Group Header");
    harness.assert_screen_contains("Normal row");
    harness.assert_screen_contains("Hint text");
}

/// Closing a non-existent panel id is a no-op and does not crash.
#[test]
fn test_close_nonexistent_panel_is_noop() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CloseGlobalPanel {
            id: "does-not-exist".to_string(),
        })
        .unwrap();
    harness.render().unwrap();
}

/// Updating a non-existent panel id is a no-op and does not crash.
#[test]
fn test_update_nonexistent_panel_is_noop() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::UpdateGlobalPanel {
            id: "ghost".to_string(),
            rows: vec![row("Should not appear")],
        })
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_not_contains("Should not appear");
}

// ---------------------------------------------------------------------------
// Anchor positioning
// ---------------------------------------------------------------------------

/// Bottom anchor (default): content appears in the lower half of the screen.
#[test]
fn test_anchor_bottom_positions_panel_near_bottom() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show_anchored(
        &mut harness,
        "p",
        vec![row("Bottom content")],
        GlobalPanelAnchor::Bottom,
    );
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let height = lines.len();
    let idx = lines
        .iter()
        .position(|l| l.contains("Bottom content"))
        .expect("Bottom content not on screen");
    assert!(
        idx >= height / 2,
        "Bottom-anchored panel should be in lower half (row {idx}/{height})"
    );
}

/// Top anchor: content appears in the upper portion of the screen (below the bars).
#[test]
fn test_anchor_top_positions_panel_near_top() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show_anchored(
        &mut harness,
        "p",
        vec![row("Top content")],
        GlobalPanelAnchor::Top,
    );
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    let height = lines.len();
    let idx = lines
        .iter()
        .position(|l| l.contains("Top content"))
        .expect("Top content not on screen");
    assert!(
        idx < height / 2,
        "Top-anchored panel should be in upper half (row {idx}/{height})"
    );
}

/// Left anchor: content appears on the left side of the screen.
#[test]
fn test_anchor_left_positions_panel_on_left() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show_anchored(
        &mut harness,
        "p",
        vec![row("Left content")],
        GlobalPanelAnchor::Left,
    );
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    // Find the row containing the text and check it starts near column 0.
    let line = screen
        .lines()
        .find(|l| l.contains("Left content"))
        .expect("Left content not on screen");
    let col = line.find("Left content").unwrap();
    assert!(
        col < 10,
        "Left-anchored panel content should be near column 0 (found at col {col})"
    );
}

/// Right anchor: content appears on the right side of the screen.
#[test]
fn test_anchor_right_positions_panel_on_right() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    show_anchored(
        &mut harness,
        "p",
        vec![row("Right content")],
        GlobalPanelAnchor::Right,
    );
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let line = screen
        .lines()
        .find(|l| l.contains("Right content"))
        .expect("Right content not on screen");
    let col = line.find("Right content").unwrap();
    // Screen is 80 columns wide; right-anchored content should be past the midpoint.
    assert!(
        col > 40,
        "Right-anchored panel content should be in right half (found at col {col})"
    );
}

/// Default anchor (no field set) behaves the same as Bottom.
#[test]
fn test_default_anchor_is_bottom() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Show one panel with explicit Bottom anchor, one with Default.
    show_anchored(
        &mut harness,
        "explicit",
        vec![row("Explicit bottom")],
        GlobalPanelAnchor::Bottom,
    );
    harness.render().unwrap();

    let screen_explicit = harness.screen_to_string();
    let idx_explicit = screen_explicit
        .lines()
        .position(|l| l.contains("Explicit bottom"))
        .expect("Explicit bottom not found");

    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::CloseGlobalPanel {
            id: "explicit".to_string(),
        })
        .unwrap();

    // Default anchor — same as Bottom
    show_anchored(
        &mut harness,
        "default",
        vec![row("Default anchor")],
        GlobalPanelAnchor::default(),
    );
    harness.render().unwrap();

    let screen_default = harness.screen_to_string();
    let idx_default = screen_default
        .lines()
        .position(|l| l.contains("Default anchor"))
        .expect("Default anchor panel not found");

    assert_eq!(
        idx_explicit, idx_default,
        "Default anchor should place panel at same row as explicit Bottom"
    );
}
