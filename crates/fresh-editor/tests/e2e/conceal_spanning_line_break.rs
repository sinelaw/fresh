//! E2E: typing on a line that a conceal reaches into from the line above.
//!
//! A conceal range is free to span a line break — markdown_compose's paragraph
//! *joins* are exactly that, one conceal from the end of a source line to the
//! start of the next — and its replacement glyphs have nothing to do with the
//! bytes it hides (`---` renders as a single `─`).
//!
//! The wrap index repairs an edited line by resuming its layout from a row
//! start, and a line's first row starts at the line's own first byte. When a
//! conceal reaches across that byte, the line's token stream opens with the
//! replacement, anchored back on the line above. The repair used to take "the
//! rest" of that token by subtracting source offsets — `text[byte - offset..]`
//! — which is meaningless for a replacement, and lands inside a multi-byte
//! glyph:
//!
//! ```text
//! start byte index 2 is not a char boundary; it is inside '─' (bytes 0..3)
//! ```
//!
//! The editor aborted on the keystroke. This drives the real plugin runtime
//! and asserts only on what the screen shows.

#![cfg(feature = "plugins")]

use crate::common::harness::{copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Conceals `aaa[a\nbb]bb` — four bytes straddling the line break — and renders
/// them as one `─`, the shape a compose-mode join takes.
const PLUGIN: &str = r#"
const editor = getEditor();

globalThis.span_lines_changed = function(data: { buffer_id: number }): void {
  editor.clearConcealNamespace(data.buffer_id, "test-span");
  editor.addConceal(data.buffer_id, "test-span", 3, 7, "─");
};
editor.on("lines_changed", "span_lines_changed");
"#;

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn typing_below_a_conceal_that_spans_a_line_break_keeps_the_editor_alive() {
    let temp = tempfile::tempdir().unwrap();
    let project_root = temp.path().join("project");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("test_conceal_span.ts"), PLUGIN).unwrap();

    let file = project_root.join("span.txt");
    fs::write(&file, "aaaa\nbbbb\n").unwrap();

    let mut harness = EditorTestHarness::create(
        100,
        12,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&file).unwrap();
    harness.render().unwrap();

    // The conceal has landed once its replacement is on screen: the fourth
    // `a`, the newline and the first two `b`s are hidden behind the one glyph,
    // so the two source lines are drawn as `aaa─bb`.
    harness
        .wait_until(|h| h.screen_to_string().contains("aaa─"))
        .unwrap();

    // Walk the cursor into the second line — the concealed newline joins the
    // two into one drawn row, so `Down` would step over it.
    for _ in 0..8 {
        if harness.screen_to_string().contains("Ln 2,") {
            break;
        }
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2,"),
        "precondition: the cursor must reach the concealed line.\nScreen:\n{screen}"
    );

    // Type on the line the conceal reaches into. This is the keystroke that
    // used to abort the process.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text("Z").unwrap();
    harness.render().unwrap();

    // `aaa─bbZ`: the first line's tail, the replacement standing in for the
    // four hidden bytes (`a`, the newline and two `b`s), then what is left of
    // the second line with the typed character at its end.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("aaa─bbZ"),
        "the typed character must reach the concealed line.\nScreen:\n{screen}"
    );
}
