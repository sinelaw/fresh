//! E2E test: decorations a plugin adds as `composeOnly`.
//!
//! Decorations live on the buffer, but compose view is a property of a
//! pane, so a plugin drawing a composed rendering (markdown_compose's
//! frames, rendered blocks) marks what it adds as compose-only, and a pane
//! showing the buffer as source leaves it out. Every kind of decoration takes
//! the flag — virtual lines, inline virtual text, conceals and overlays — and
//! nothing about it depends on the namespace or id: the test uses one
//! namespace for both the flagged and the plain decoration of each kind.
//!
//! The test plugin adds one of each, flagged and plain, and flips the pane
//! between Source and compose; the test reads the screen.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Modifier;
use std::fs;

const PLUGIN_NAME: &str = "composedecor";
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

registerHandler("composedecor_add", () => {
    const b = editor.getActiveBufferId();
    const ns = "decor";
    // Buffer: "alpha line\nbravo line\ncharlie line\n"
    editor.addVirtualLine(b, 0, "ROW-COMPOSE", { composeOnly: true }, true, ns, 0);
    editor.addVirtualLine(b, 0, "ROW-ALWAYS", {}, true, ns, 1);
    editor.addVirtualTextStyled(b, "decor:c", 5, "INL-COMPOSE", { composeOnly: true }, false);
    editor.addVirtualTextStyled(b, "decor:a", 16, "INL-ALWAYS", {}, false);
    editor.addConceal(b, ns, 0, 5, "CONCEAL-COMPOSE", null, null, null, true);
    editor.addConceal(b, ns, 11, 16, "CONCEAL-ALWAYS");
    editor.addOverlay(b, ns, 22, 29, { bold: true, composeOnly: true });
    editor.setStatus("composedecor-added");
});
editor.registerCommand("composedecor: Add", "Add the decorations", "composedecor_add", null);

registerHandler("composedecor_compose", () => {
    editor.setViewMode(editor.getActiveBufferId(), "compose");
    editor.setStatus("composedecor-composed");
});
editor.registerCommand("composedecor: Compose", "Compose the pane", "composedecor_compose", null);
"#;

fn run_command(h: &mut EditorTestHarness, name: &str, done_status: &str) {
    h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    h.wait_for_prompt().unwrap();
    h.type_text(name).unwrap();
    h.send_key(KeyCode::Enter, KeyModifiers::NONE).unwrap();
    h.wait_for_prompt_closed().unwrap();
    h.wait_for_screen_contains(done_status).unwrap();
}

/// Whether the first cell of `text` on screen is bold.
fn is_bold(h: &EditorTestHarness, text: &str) -> bool {
    let (x, y) = h
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text:?} not on screen:\n{}", h.screen_to_string()));
    h.get_cell_style(x, y)
        .is_some_and(|s| s.add_modifier.contains(Modifier::BOLD))
}

#[test]
fn source_pane_leaves_out_compose_only_decorations_and_compose_draws_them() {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    fs::write(plugins_dir.join(format!("{PLUGIN_NAME}.ts")), PLUGIN_SOURCE).unwrap();
    copy_plugin_lib(&plugins_dir);
    let file = working_dir.join("notes.txt");
    fs::write(&file, "alpha line\nbravo line\ncharlie line\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 20, Default::default(), working_dir)
            .expect("harness");
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    run_command(&mut harness, "composedecor: Add", "composedecor-added");
    harness
        .wait_until(|h| h.screen_to_string().contains("ROW-ALWAYS"))
        .unwrap();
    for shown in ["ROW-ALWAYS", "INL-ALWAYS", "CONCEAL-ALWAYS", "alpha line"] {
        harness.assert_screen_contains(shown);
    }
    for hidden in ["ROW-COMPOSE", "INL-COMPOSE", "CONCEAL-COMPOSE"] {
        harness.assert_screen_not_contains(hidden);
    }
    assert!(
        !is_bold(&harness, "charlie"),
        "the compose-only overlay should not style a Source pane"
    );

    run_command(
        &mut harness,
        "composedecor: Compose",
        "composedecor-composed",
    );
    harness
        .wait_until(|h| h.screen_to_string().contains("ROW-COMPOSE"))
        .unwrap();
    for shown in [
        "ROW-ALWAYS",
        "INL-ALWAYS",
        "CONCEAL-ALWAYS",
        "ROW-COMPOSE",
        "INL-COMPOSE",
        "CONCEAL-COMPOSE",
    ] {
        harness.assert_screen_contains(shown);
    }
    assert!(
        is_bold(&harness, "charlie"),
        "the compose-only overlay should style the compose pane"
    );
}
