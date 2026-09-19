//! The Contents outline follows the viewport whenever the pane does not hold
//! the keyboard (sinelaw/fresh#3326, G; sidebar design §5.5).
//!
//! The plugin used to infer "the pane has focus" from its own section's
//! focus events, so the file *explorer* holding the keyboard looked like
//! editing and a scroll left the outline behind. The host now says which
//! chrome region has the keyboard (`chrome_focus_changed`), and the outline
//! follows the scroll while the tree is focused.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crate::common::sidebar::{
    explorer_focused, install_markdown_plugins, selected_contents_row, wait_for,
};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

fn long_doc() -> String {
    let mut s = String::from("# Long Doc\n\n");
    for h in ["Alpha", "Beta", "Gamma", "Delta"] {
        s.push_str(&format!("## {h}\n\n"));
        for i in 1..=25 {
            s.push_str(&format!("{h} body line {i}\n"));
        }
        s.push('\n');
    }
    s
}

#[test]
fn the_outline_follows_a_scroll_while_the_explorer_holds_the_keyboard() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    install_markdown_plugins(&project);
    let md = project.join("long.md");
    fs::write(&md, long_doc()).unwrap();

    let mut h = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_working_dir(project.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    h.open_file(&md).unwrap();
    h.render().unwrap();

    // The column, with the explorer holding the keyboard.
    h.send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_file_explorer().unwrap();
    wait_for(&mut h, "the explorer focused", explorer_focused);
    wait_for(&mut h, "the outline at the top", |h| {
        selected_contents_row(h).as_deref() == Some("Long Doc")
    });

    // Scroll the pane, not the tree: the wheel over the editor's text.
    for _ in 0..60 {
        h.mouse_scroll_down(100, 12).unwrap();
    }
    wait_for(
        &mut h,
        "the outline follows the viewport with the tree focused",
        |h| {
            matches!(
                selected_contents_row(h).as_deref(),
                Some("Beta") | Some("Gamma") | Some("Delta")
            )
        },
    );
    assert!(explorer_focused(&h), "the tree kept the keyboard");
}
