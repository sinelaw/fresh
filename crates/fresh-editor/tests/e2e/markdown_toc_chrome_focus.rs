//! The Contents outline follows the viewport whenever the pane does not hold
//! the keyboard (sinelaw/fresh#3326, G; sidebar design §5.5).
//!
//! The plugin used to infer "the pane has focus" from its own section's
//! focus events, so the file *explorer* holding the keyboard looked like
//! editing and a scroll left the outline behind. The host now says which
//! chrome region has the keyboard (`chrome_focus_changed`), and the outline
//! follows the scroll while the tree is focused.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
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

/// The Contents row that carries the selection mark, trimmed to its text.
fn selected_heading(h: &EditorTestHarness) -> Option<String> {
    let s = h.screen_to_string();
    let mut inside = false;
    for line in s.lines() {
        if line.contains("Contents") && line.contains('▼') {
            inside = true;
            continue;
        }
        if inside {
            if line.starts_with('└') {
                break;
            }
            if let Some(i) = line.find('▌') {
                let rest = &line[i + '▌'.len_utf8()..];
                let text = rest.split('│').next().unwrap_or("").trim();
                return Some(text.to_string());
            }
        }
    }
    None
}

/// The explorer is painting its focus caret.
fn explorer_focused(h: &EditorTestHarness) -> bool {
    h.screen_to_string()
        .lines()
        .any(|l| l.contains("File Explorer") && !l.contains("(Ctrl+E)"))
}

#[test]
fn the_outline_follows_a_scroll_while_the_explorer_holds_the_keyboard() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project = temp_dir.path().join("project");
    fs::create_dir(&project).unwrap();
    let plugins_dir = project.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_toc");
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin(&plugins_dir, "markdown_source");
    copy_plugin_lib(&plugins_dir);
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
    h.wait_until(|h| explorer_focused(h)).unwrap();
    h.wait_until(|h| selected_heading(h).as_deref() == Some("Long Doc"))
        .unwrap_or_else(|e| panic!("the outline at the top: {e}\n{}", h.screen_to_string()));

    // Scroll the pane, not the tree: the wheel over the editor's text.
    let col = 100u16;
    let row = 12u16;
    for _ in 0..60 {
        h.mouse_scroll_down(col, row).unwrap();
    }
    h.wait_until(|h| {
        matches!(
            selected_heading(h).as_deref(),
            Some("Beta") | Some("Gamma") | Some("Delta")
        )
    })
    .unwrap_or_else(|e| {
        panic!(
            "the outline follows the viewport with the tree focused: {e}\n{}",
            h.screen_to_string()
        )
    });
    assert!(explorer_focused(&h), "the tree kept the keyboard");
}
