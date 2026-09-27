//! A plugin `Tree`'s expansion has one owner: the host, once the tree has
//! rendered.
//!
//! The spec's `expandedKeys` is a seed. A disclosure click, →/←, and the
//! plugin's own `setExpandedKeys` all write the host's instance state, and
//! none of them re-sends the spec — so the tree that is drawn has to be
//! drawn from that state. It used to be drawn from the spec's field, which
//! meant every one of those gestures moved the tree's navigation and left
//! its picture where it was until something else happened to re-send the
//! spec.
//!
//! The `test_tree_expansion.ts` plugin mounts a plain tree and a
//! `cardBorders` tree, both collapsed, and never re-sends them. Each gesture
//! ends in a status line from the plugin; the tests wait for it and then
//! read the panel.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

const PLUGIN: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/tests/plugins/test_tree_expansion.ts"
));

fn mounted() -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    let plugins_dir = project.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_tree_expansion.ts"), PLUGIN).unwrap();

    let mut h =
        EditorTestHarness::with_config_and_working_dir(100, 40, Default::default(), project)
            .unwrap();
    h.render().unwrap();
    h.run_palette_command("TreeExp: Mount").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("TreeExp: MOUNTED"))
        .unwrap();
    h.wait_until(|h| {
        let s = h.screen_to_string();
        s.contains("plain-root") && s.contains("card-root")
    })
    .unwrap();
    // Both start collapsed: the seed is honoured on the first frame.
    assert_collapsed(&h, "plain");
    assert_collapsed(&h, "card");
    (h, temp_dir)
}

fn shows(h: &EditorTestHarness, text: &str) -> bool {
    h.screen_to_string().contains(text)
}

fn assert_collapsed(h: &EditorTestHarness, prefix: &str) {
    assert!(
        !shows(h, &format!("{prefix}-child")),
        "{prefix}-root is collapsed, so its child is hidden:\n{}",
        h.screen_to_string()
    );
    assert!(shows(h, &format!("{prefix}-sibling")));
}

fn assert_expanded(h: &EditorTestHarness, prefix: &str, how: &str) {
    assert!(
        shows(h, &format!("{prefix}-child")),
        "{how} opened {prefix}-root without a spec re-send, so its child is drawn:\n{}",
        h.screen_to_string()
    );
}

fn wait_status(h: &mut EditorTestHarness, status: &str) {
    let want = format!("TreeExp: {status}");
    h.wait_until(|h| h.screen_to_string().contains(&want))
        .unwrap();
}

fn tab_to_cards(h: &mut EditorTestHarness) {
    h.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    h.render().unwrap();
}

/// (a) The plugin's `setExpandedKeys`, with no spec after it, is what the
/// tree draws — plain and card alike.
#[test]
fn set_expanded_keys_without_a_spec_resend_redraws_the_tree() {
    let (mut h, _dir) = mounted();

    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "SET plain-r open");
    assert_expanded(&h, "plain", "setExpandedKeys");
    assert_collapsed(&h, "card");

    tab_to_cards(&mut h);
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "SET card-r open");
    assert_expanded(&h, "card", "setExpandedKeys");

    // And back: shutting it is drawn too.
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "SET card-r shut");
    assert_collapsed(&h, "card");
}

/// (b) → opens and ← shuts the selected row in the host's state, and the
/// tree draws that — the plugin only hears about it.
#[test]
fn right_and_left_redraw_the_tree_without_a_spec_resend() {
    let (mut h, _dir) = mounted();

    h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "EXPAND plain-r open");
    assert_expanded(&h, "plain", "Right");
    h.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "EXPAND plain-r shut");
    assert_collapsed(&h, "plain");

    tab_to_cards(&mut h);
    h.send_key(KeyCode::Right, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "EXPAND card-r open");
    assert_expanded(&h, "card", "Right");
    h.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    wait_status(&mut h, "EXPAND card-r shut");
    assert_collapsed(&h, "card");
}

/// (b) A press on the disclosure glyph is the same gesture as →.
#[test]
fn a_disclosure_click_redraws_the_tree_without_a_spec_resend() {
    let (mut h, _dir) = mounted();

    for prefix in ["plain", "card"] {
        let (col, row) = h
            .find_text_on_screen(&format!("{prefix}-root"))
            .expect("the root row is on screen");
        // The glyph is the `▶` before the label on the same row.
        let line = h.screen_row_text(row);
        let label_at = line
            .char_indices()
            .position(|(i, _)| line[i..].starts_with(&format!("{prefix}-root")))
            .expect("label on its row");
        let glyph_at = line
            .chars()
            .take(label_at)
            .collect::<Vec<_>>()
            .iter()
            .rposition(|c| *c == '▶')
            .unwrap_or_else(|| panic!("a collapsed branch wears ▶: {line:?}"));
        assert!(glyph_at < col as usize);
        h.mouse_click(glyph_at as u16, row).unwrap();
        wait_status(&mut h, &format!("EXPAND {prefix}-r open"));
        assert_expanded(&h, prefix, "a disclosure click");
    }
}
