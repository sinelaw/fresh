//! ←/→ from one tree to the tree beside it land on the row level with the
//! one the cursor left, not on that tree's first row (R4,
//! `docs/internal/widget-controls-own-interaction.md`).
//!
//! Two card trees side by side, as the Workboard's Board lays out its
//! columns: the cursor on the left tree's third card, → moves focus to the
//! right tree, and its third card — the one beside — is the one selected.
//! Before, the right tree seeded its cursor to its first row, so the arrow
//! seemed to jump up the board.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh_core::api::{PluginCommand, WidgetSpec};
use serde_json::json;

fn column(key: &str, prefix: &str, selected: i32) -> serde_json::Value {
    let ids: Vec<String> = (1..=4).map(|i| format!("{prefix}{i}")).collect();
    json!({
        "kind": "tree",
        "key": key,
        "itemKeys": ids,
        "selectedIndex": selected,
        "itemHeight": 2,
        "cardBorders": true,
        "nodes": ids.iter().map(|id| json!({
            "text": { "text": format!("card {id}") },
            "depth": 0,
            "extraLines": [{ "text": "detail" }],
        })).collect::<Vec<_>>(),
    })
}

/// The id of the card drawn with the heavy (selected) frame, if any.
fn selected_card(screen: &str) -> Option<String> {
    screen.lines().find_map(|l| {
        let at = l.find("┃card ")?;
        let rest = &l[at + "┃card ".len()..];
        Some(
            rest.split_whitespace()
                .next()?
                .trim_end_matches('┃')
                .to_string(),
        )
    })
}

#[test]
fn an_arrow_into_the_next_tree_lands_on_the_row_beside() {
    let mut h = EditorTestHarness::with_temp_project(100, 40).unwrap();
    let spec: WidgetSpec = serde_json::from_value(json!({
        "kind": "row",
        "children": [
            column("left", "L", 2),
            { "kind": "spacer", "cols": 2 },
            column("right", "R", -1),
        ],
    }))
    .unwrap();
    h.editor_mut()
        .handle_plugin_command(PluginCommand::MountFloatingWidget {
            plugin: "test-plugin".to_string(),
            panel_id: 1,
            spec,
            width_pct: 80,
            height_pct: 80,
            as_dock: false,
            focus_marker: false,
            label_align: Default::default(),
            title: None,
            closable: false,
            start_blurred: false,
            mode: None,
        })
        .unwrap();
    h.render().unwrap();
    h.wait_until(|h| selected_card(&h.screen_to_string()).as_deref() == Some("L3"))
        .unwrap();

    h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| selected_card(&h.screen_to_string()).is_some_and(|c| c.starts_with('R')))
        .unwrap();
    let screen = h.screen_to_string();
    assert_eq!(
        selected_card(&screen).as_deref(),
        Some("R3"),
        "→ lands on the card beside the one left, not the next tree's first:\n{screen}"
    );

    // And back: ← from R3 is L3 again, wherever the left tree's cursor was.
    h.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    h.wait_until(|h| selected_card(&h.screen_to_string()).is_some_and(|c| c.starts_with('L')))
        .unwrap();
    assert_eq!(selected_card(&h.screen_to_string()).as_deref(), Some("L3"));
}
