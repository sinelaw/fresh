//! A `pane` widget pointed at a **composite** buffer must render the
//! composite — both source columns and their headers — not an empty leaf.
//!
//! A composite carries no text of its own: its content is the source
//! buffers it names, laid into columns, and the split path branches to a
//! dedicated renderer for exactly that reason. `render_pane_into_rect`
//! did not branch, so it sent the composite through the per-leaf text
//! pipeline and painted one blank gutter row — which is what a `Pane`
//! aimed at a side-by-side diff used to show, contradicting the
//! documented "works for any buffer kind".
//!
//! Per CONTRIBUTING.md §2 this drives the keyboard and asserts on
//! rendered output; per §3 it waits with `wait_until` rather than
//! sleeping.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

fn install_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);
    const SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_composite_pane.ts"
    ));
    fs::write(plugins_dir.join("test_composite_pane.ts"), SRC).unwrap();
}

#[test]
fn a_pane_over_a_composite_buffer_renders_both_columns() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    install_plugin(&project_root);

    let mut h =
        EditorTestHarness::with_config_and_working_dir(140, 40, Default::default(), project_root)
            .unwrap();
    h.render().unwrap();

    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text("Composite Pane Open").unwrap();
    h.wait_until(|h| h.screen_to_string().contains("Composite Pane Open"))
        .unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();

    // The panel itself arriving is not the assertion — an unfixed build
    // mounts the panel too, and paints an empty pane inside it.
    h.wait_until(|h| h.screen_to_string().contains("composite"))
        .unwrap();

    // Both source headers, side by side on one row: that is the composite
    // renderer's own output and nothing else produces it.
    h.wait_until(|h| {
        h.screen_to_string()
            .lines()
            .any(|l| l.contains("LEFTHDR") && l.contains("RIGHTHDR"))
    })
    .unwrap();

    // ...and the sources' text, including the line that differs between
    // them, so this is the real content rather than a header-only frame.
    let screen = h.screen_to_string();
    for needle in ["alpha", "bravo", "charlie"] {
        assert!(
            screen.contains(needle),
            "composite pane is missing source text {needle:?}:\n{screen}"
        );
    }
}
