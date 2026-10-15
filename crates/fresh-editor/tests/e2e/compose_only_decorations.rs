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
//! The test plugin adds one of each, flagged and plain, to a pane in Source
//! view; then splits it and composes the new pane — the case the flag is for:
//! a Source pane beside a pane composing the same buffer. The test reads the
//! screen.

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
    // Buffer: "alpha line\nbravo line\ncharlie line\ndelta line\n"
    editor.addVirtualLine(b, 0, "ROW-COMPOSE", { composeOnly: true }, true, ns, 0);
    editor.addVirtualLine(b, 0, "ROW-ALWAYS", {}, true, ns, 1);
    editor.addVirtualTextStyled(b, "decor:c", 5, "INL-COMPOSE", { composeOnly: true }, false);
    editor.addVirtualTextStyled(b, "decor:a", 16, "INL-ALWAYS", {}, false);
    editor.addConceal(b, ns, 0, 5, "CONCEAL-COMPOSE", null, null, null, true);
    editor.addConceal(b, ns, 11, 16, "CONCEAL-ALWAYS");
    editor.addOverlay(b, ns, 22, 29, { bold: true, composeOnly: true });
    editor.addOverlay(b, ns, 35, 40, { bold: true });
    editor.setStatus("composedecor-added");
});
editor.registerCommand("composedecor: Add", "Add the decorations", "composedecor_add", null);

// Split the pane and compose the new one; the original stays in Source view.
registerHandler("composedecor_split_compose", async () => {
    const created = await editor.splitWindow({ direction: "vertical", place: "after" });
    editor.focusSplit(created.splitId);
    editor.setViewMode(created.bufferId, "compose");
    editor.focusSplit(created.sourceSplitId);
    await editor.flush();
    editor.setStatus("composedecor-composed");
});
editor.registerCommand(
    "composedecor: Split Compose", "Compose a new pane beside this one",
    "composedecor_split_compose", null,
);
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

fn count(h: &EditorTestHarness, text: &str) -> usize {
    h.screen_to_string().matches(text).count()
}

/// Whether each place `text` is drawn starts with a bold cell, left to right.
fn bold_at_each(h: &EditorTestHarness, text: &str) -> Vec<bool> {
    (0..h.buffer().area.height)
        .flat_map(|y| {
            let row = h.screen_row_text(y);
            row.match_indices(text)
                .map(|(byte, _)| (row[..byte].chars().count() as u16, y))
                .collect::<Vec<_>>()
        })
        .map(|(x, y)| {
            h.get_cell_style(x, y)
                .is_some_and(|s| s.add_modifier.contains(Modifier::BOLD))
        })
        .collect()
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
    fs::write(&file, "alpha line\nbravo line\ncharlie line\ndelta line\n").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 20, Default::default(), working_dir)
            .expect("harness");
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // One pane, in Source view: only the plain decorations.
    run_command(&mut harness, "composedecor: Add", "composedecor-added");
    harness
        .wait_until(|h| h.screen_to_string().contains("ROW-ALWAYS"))
        .unwrap();
    for (text, n) in [
        ("ROW-ALWAYS", 1),
        ("INL-ALWAYS", 1),
        ("CONCEAL-ALWAYS", 1),
        ("alpha line", 1),
        ("ROW-COMPOSE", 0),
        ("INL-COMPOSE", 0),
        ("CONCEAL-COMPOSE", 0),
    ] {
        assert_eq!(
            count(&harness, text),
            n,
            "{text}:\n{}",
            harness.screen_to_string()
        );
    }
    assert_eq!(bold_at_each(&harness, "charlie"), [false]);
    assert_eq!(bold_at_each(&harness, "delta"), [true]);

    // Beside it, a pane composing the same buffer: it draws both kinds, and
    // the Source pane still draws only the plain ones.
    run_command(
        &mut harness,
        "composedecor: Split Compose",
        "composedecor-composed",
    );
    harness
        .wait_until(|h| h.screen_to_string().contains("ROW-COMPOSE"))
        .unwrap();
    for (text, n) in [
        ("ROW-ALWAYS", 2),
        ("INL-ALWAYS", 2),
        ("CONCEAL-ALWAYS", 2),
        ("alpha line", 1),
        ("ROW-COMPOSE", 1),
        ("INL-COMPOSE", 1),
        ("CONCEAL-COMPOSE", 1),
    ] {
        assert_eq!(
            count(&harness, text),
            n,
            "{text}:\n{}",
            harness.screen_to_string()
        );
    }
    assert_eq!(
        bold_at_each(&harness, "charlie"),
        [false, true],
        "the compose-only overlay styles only the compose pane (right):\n{}",
        harness.screen_to_string()
    );
    assert_eq!(bold_at_each(&harness, "delta"), [true, true]);
}
