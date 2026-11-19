//! End-to-end tests for Markdown list editing and language edit rules.
//!
//! Markdown's list editing is its built-in `edit_rules` setting
//! (`languages.markdown.edit_rules`), which the editor applies inside
//! Enter / Tab / Shift+Tab:
//! - Enter continues list items (bullets, ordered, checkboxes)
//! - Enter on an empty list item removes the marker
//! - Tab on a blank list item indents and cycles the bullet; elsewhere Tab
//!   is the normal Tab
//! - The rules apply to Markdown buffers only, and keep up with keys sent
//!   back to back (#3813)
//!
//! Any language can have rules in config.json (`[]` turns them off), and
//! plugins can register more (`registerEditRules`), tried after the config's.

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Create a harness in a fresh project directory, with the built-in
/// config: Markdown's list editing needs no plugin.
fn markdown_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    markdown_harness_with_config(width, height, Default::default())
}

/// [`markdown_harness`] with a given config.
fn markdown_harness_with_config(
    width: u16,
    height: u16,
    config: fresh::config::Config,
) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let harness =
        EditorTestHarness::with_config_and_working_dir(width, height, config, project_root)
            .unwrap();
    (harness, temp_dir)
}

/// Helper: open a markdown file. The rules need no activation: they apply
/// from the first key.
fn open_md(harness: &mut EditorTestHarness, path: &std::path::Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();
}

/// Helper: get buffer content, panicking if unavailable.
fn buf(harness: &EditorTestHarness) -> String {
    harness
        .get_buffer_content()
        .expect("buffer content should be available")
}

/// Open `name` holding `content`, press End then Enter, and return the text.
fn enter_at_end_of_first_line(
    harness: &mut EditorTestHarness,
    name: &str,
    content: &str,
) -> String {
    let fixture = TestFixture::new(name, content).unwrap();
    open_md(harness, &fixture.path);
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    buf(harness)
}

// ---------------------------------------------------------------------------
// Where the rules apply
// ---------------------------------------------------------------------------

/// In a .md file Enter continues a list item, with no activation step.
#[test]
fn test_rules_apply_in_md_file() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = enter_at_end_of_first_line(&mut harness, "readme.md", "- item\n");
    assert_eq!(content, "- item\n- \n");
    harness.assert_no_plugin_errors();
}

/// In a non-markdown file Enter is the plain newline: no list continuation.
#[test]
fn test_rules_do_not_apply_in_non_md_file() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = enter_at_end_of_first_line(&mut harness, "main.rs", "- item\n");
    assert_eq!(
        content, "- item\n\n",
        "a Rust file must not continue Markdown list markers"
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Enter key: list continuation
// ---------------------------------------------------------------------------

/// Pressing Enter at the end of an unordered list item should insert a newline
/// followed by the same indent and bullet.
#[test]
fn test_enter_continues_unordered_list() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "- top\n  - nested\n";
    let fixture = TestFixture::new("list.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Move to line 2 ("  - nested"), then End to go to end of line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should insert newline + "  - " (same indent + bullet)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("  - nested\n  - "))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Pressing Enter at the end of an ordered list item should insert the next number.
#[test]
fn test_enter_continues_ordered_list() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "1. first\n2. second\n";
    let fixture = TestFixture::new("ordered.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Move to line 2 ("2. second"), End
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should insert "\n3. "
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("2. second\n3. "))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Pressing Enter at the end of a checkbox item should insert an unchecked checkbox.
#[test]
fn test_enter_continues_checkbox() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "- [x] done task\n";
    let fixture = TestFixture::new("checkbox.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should insert "\n- [ ] " (unchecked)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("- [x] done task\n- [ ] "))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Pressing Enter on an empty list item (just the bullet) should remove the marker.
#[test]
fn test_enter_clears_empty_bullet() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "- item\n- \n";
    let fixture = TestFixture::new("empty.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Move to line 2 ("- "), End
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should remove "- " leaving an empty line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("- item\n\n"))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Enter key: non-list (whitespace copying)
// ---------------------------------------------------------------------------

/// Pressing Enter on a line with no indentation should just insert a plain newline.
#[test]
fn test_enter_no_indent_on_unindented_line() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "Hello world\n";
    let fixture = TestFixture::new("plain.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Cursor starts at line 1 col 1; move to end
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("Hello world\n\n"))
        })
        .unwrap();

    let content = buf(&harness);
    let lines: Vec<&str> = content.lines().collect();
    assert!(
        lines.len() >= 2,
        "Expected at least 2 lines after Enter. Got:\n{:?}",
        content,
    );
    // The second line (index 1) should be empty (no leading spaces)
    assert_eq!(
        lines[1], "",
        "Expected empty line (no indent) after unindented line. Got: {:?}",
        lines[1],
    );
    harness.assert_no_plugin_errors();
}

/// Pressing Enter in the middle of a deeply-indented line should still
/// match the *line's* leading whitespace (not the cursor column).
#[test]
fn test_enter_deep_indent() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "        deep indent text\n";
    let fixture = TestFixture::new("deep.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Go to end of the line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("deep indent text\n        "))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Tab key: insert spaces (non-list context)
// ---------------------------------------------------------------------------

/// Tab should insert 4 spaces at the cursor position.
#[test]
fn test_tab_inserts_spaces() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "text\n";
    let fixture = TestFixture::new("tab.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Cursor is at the beginning of "text"; press Home to be sure
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab — should insert 4 spaces before "text"
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("    text"))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Tab follows `editor.tab_size` instead of a fixed four columns, both for a
/// plain line and for the indent + bullet cycle on a blank list item, and
/// Shift+Tab takes the same width back off (#3713).
#[test]
fn test_tab_follows_editor_tab_size() {
    let mut config = fresh::config::Config::default();
    config.editor.tab_size = 2;
    let (mut harness, _temp_dir) = markdown_harness_with_config(80, 24, config);

    let fixture = TestFixture::new("tab2.md", "text\n- \n").unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("  text\n"))
        })
        .unwrap();

    // Blank list item: indent by one level and cycle the bullet.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c == "  text\n  + \n")
        })
        .unwrap();

    // Shift+Tab removes that level again and cycles the bullet back.
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .wait_until(|h| h.get_buffer_content().is_some_and(|c| c == "  text\n- \n"))
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// `languages.markdown.tab_size` and `.use_tabs` override the global setting
/// for the list rules' indentation too (#3713).
#[test]
fn test_tab_follows_markdown_language_settings() {
    let mut config = fresh::config::Config::default();
    config.editor.tab_size = 2;
    config.languages.get_mut("markdown").unwrap().tab_size = Some(3);
    let (mut harness, _temp_dir) = markdown_harness_with_config(80, 24, config.clone());

    let fixture = TestFixture::new("tab3.md", "text\n").unwrap();
    open_md(&mut harness, &fixture.path);
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.get_buffer_content().is_some_and(|c| c == "   text\n"))
        .unwrap();
    harness.assert_no_plugin_errors();

    config.languages.get_mut("markdown").unwrap().use_tabs = Some(true);
    let (mut harness, _temp_dir) = markdown_harness_with_config(80, 24, config);
    let fixture = TestFixture::new("tabs.md", "text\n- \n").unwrap();
    open_md(&mut harness, &fixture.path);
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("\ttext\n"))
        })
        .unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c == "\ttext\n\t+ \n")
        })
        .unwrap();
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .wait_until(|h| h.get_buffer_content().is_some_and(|c| c == "\ttext\n- \n"))
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// A Markdown file named on open but not yet on disk gets the
/// `languages.markdown` settings from the start, not only once it has been
/// saved (#3713).
#[test]
fn test_new_markdown_file_uses_markdown_language_settings() {
    let mut config = fresh::config::Config::default();
    config.languages.get_mut("markdown").unwrap().tab_size = Some(3);
    let (mut harness, temp_dir) = markdown_harness_with_config(80, 24, config);

    let path = temp_dir.path().join("not_yet_saved.md");
    open_md(&mut harness, &path);
    harness.type_text("text").unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.get_buffer_content().is_some_and(|c| c == "   text"))
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Tab with several lines selected indents every selected line, as it does
/// outside Markdown, instead of replacing the selection with spaces (#3666).
#[test]
fn test_tab_indents_a_multi_line_selection() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "https://a.example/one\nhttps://b.example/two\nhttps://c.example/three\n";
    let fixture = TestFixture::new("urls.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::SHIFT)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::SHIFT).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content().is_some_and(|c| {
                c == "    https://a.example/one\n    https://b.example/two\n    https://c.example/three\n"
            })
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Tab with several cursors indents at every cursor, not just the primary.
#[test]
fn test_tab_with_multiple_cursors_indents_each_line() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "one\ntwo\nthree\n";
    let fixture = TestFixture::new("multi.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    // Add the cursors and press Tab with no frame in between, as a fast key
    // sequence does: the handler must still see all three cursors.
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c == "    one\n    two\n    three\n")
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Multiple Tab presses should accumulate spaces.
#[test]
fn test_multiple_tabs() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "x\n";
    let fixture = TestFixture::new("tabs.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab twice -> 8 spaces
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("        x"))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Tab key: indent + cycle bullet on blank list item
// ---------------------------------------------------------------------------

/// Tab on a blank unordered list item should indent and cycle the bullet.
#[test]
fn test_tab_cycles_bullet_on_blank_item() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    // Start with "* " (asterisk bullet, no content)
    let content = "* \n";
    let fixture = TestFixture::new("cycle.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Tab: * -> - (indented by 4)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("    - "))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Shift+Tab: de-indent + reverse-cycle bullet on blank list item
// ---------------------------------------------------------------------------

/// Shift+Tab on a blank list item should de-indent and cycle the bullet
/// in reverse order (the exact opposite of Tab).
///
/// Forward (Tab):  * → - → + → *  (each step +4 spaces indent)
/// Reverse (Shift+Tab): * → + → - → *  (each step −4 spaces indent)
///
/// This test starts with an indented "    - " (from a previous Tab on "* "),
/// then presses Shift+Tab through a full reverse cycle.
#[test]
fn test_shift_tab_reverse_cycles_bullet_on_blank_item() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    // Start with "    - " — as if Tab was already pressed once on "* "
    let content = "    - \n";
    let fixture = TestFixture::new("reverse_cycle.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Shift+Tab #1:  "    - " → "* "   (de-indent, reverse cycle - → *)
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("* \n"))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Full round-trip: Tab cycles * → - → + → *, each adding 4 spaces of indent,
/// then Shift+Tab reverses the entire sequence back to the starting state.
#[test]
fn test_tab_shift_tab_full_round_trip() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    // Start with "* " (asterisk bullet, no content)
    let content = "* \n";
    let fixture = TestFixture::new("roundtrip.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Helper: drain any remaining async commands (e.g. setBufferCursor) so the
    // plugin state snapshot is fully up-to-date before sending the next key.
    let drain = |h: &mut EditorTestHarness| {
        for _ in 0..3 {
            h.process_async_and_render().unwrap();
        }
    };

    // --- Forward cycle via Tab ---

    // Tab #1: "* " → "    - "
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("    - \n"))
        })
        .unwrap();
    drain(&mut harness);

    // Tab #2: "    - " → "        + "
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("        + \n"))
        })
        .unwrap();
    drain(&mut harness);

    // Tab #3: "        + " → "            * "
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("            * \n"))
        })
        .unwrap();
    drain(&mut harness);

    // --- Reverse cycle via Shift+Tab (exact opposite) ---

    // Shift+Tab #1: "            * " → "        + "
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("        + \n"))
        })
        .unwrap();
    drain(&mut harness);

    // Shift+Tab #2: "        + " → "    - "
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("    - \n"))
        })
        .unwrap();
    drain(&mut harness);

    // Shift+Tab #3: "    - " → "* "  (back to original)
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.starts_with("* \n"))
        })
        .unwrap();

    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Normal typing still works
// ---------------------------------------------------------------------------

/// Characters should be inserted normally (the mode is not read-only).
#[test]
fn test_normal_typing_works() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "\n";
    let fixture = TestFixture::new("type.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Type some text
    harness.type_text("Hello markdown").unwrap();
    harness.render().unwrap();

    let content = buf(&harness);
    assert!(
        content.contains("Hello markdown"),
        "Typed text should appear in buffer. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Switching buffers
// ---------------------------------------------------------------------------

/// The rules follow the buffer the key lands in: after switching from a .md
/// buffer to a plain-text one, Enter there is the plain newline, and back in
/// the .md buffer it continues lists again.
#[test]
fn test_rules_follow_the_active_buffer() {
    let (mut harness, temp_dir) = markdown_harness(80, 24);

    let md_fixture = TestFixture::new("doc.md", "- a\n").unwrap();
    open_md(&mut harness, &md_fixture.path);

    let txt_path = temp_dir.path().join("project_root").join("notes.txt");
    fs::write(&txt_path, "- plain\n").unwrap();
    harness.open_file(&txt_path).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(buf(&harness), "- plain\n\n");

    open_md(&mut harness, &md_fixture.path);
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(buf(&harness), "- a\n- \n");
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// .mdx and .markdown extensions
// ---------------------------------------------------------------------------

/// `.mdx` files are plain text by default (no built-in language claims the
/// extension), so they get no list editing. Configured as a language with
/// the id `mdx` — or added to `markdown`'s extensions — they do.
#[test]
fn test_rules_apply_for_mdx_when_configured() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let content = enter_at_end_of_first_line(&mut harness, "plain.mdx", "- item\n");
    assert_eq!(harness.editor().active_state().language, "text");
    assert_eq!(content, "- item\n\n");

    let mut config = fresh::config::Config::default();
    let mut mdx = config.languages["markdown"].clone();
    mdx.extensions = vec!["mdx".to_string()];
    config.languages.insert("mdx".to_string(), mdx);
    let (mut harness, _temp_dir) = markdown_harness_with_config(80, 24, config);
    let content = enter_at_end_of_first_line(&mut harness, "component.mdx", "- item\n");
    assert_eq!(harness.editor().active_state().language, "mdx");
    assert_eq!(content, "- item\n- \n");
    harness.assert_no_plugin_errors();
}

/// The rules also apply to .markdown files.
#[test]
fn test_rules_apply_for_markdown_extension() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = enter_at_end_of_first_line(&mut harness, "readme.markdown", "1. one\n");
    assert_eq!(content, "1. one\n2. \n");
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Enter + Tab combined workflow
// ---------------------------------------------------------------------------

/// Simulate a realistic list editing flow: type on a list item, press Enter
/// (auto-continue bullet), then type a sub-item.
#[test]
fn test_enter_then_type_workflow() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let content = "- item\n";
    let fixture = TestFixture::new("workflow.md", content).unwrap();
    open_md(&mut harness, &fixture.path);

    // Go to end of "- item"
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Enter -> should auto-insert "- "
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.contains("- item\n- "))
        })
        .unwrap();

    // Type continuation text
    harness.type_text("next").unwrap();
    harness.render().unwrap();

    let content = buf(&harness);
    assert!(
        content.contains("- item\n- next"),
        "Expected '- item\\n- next'. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Multi-cursor Enter (issue #1140)
// ---------------------------------------------------------------------------

/// Pressing Enter with multiple cursors in markdown mode should insert newlines
/// at ALL cursor positions, not just the primary.
/// Issue #1140: "Pressing enter with multiple cursors only adds a newline to the last cursor"
#[test]
fn test_multi_cursor_enter_inserts_at_all_cursors() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let fixture = TestFixture::new("multi.md", "aaa\nbbb\nccc\n").unwrap();
    open_md(&mut harness, &fixture.path);

    // Go to beginning, then right once (after first char of line 1)
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();

    // Add cursor below (Ctrl+Alt+Down)
    harness
        .send_key(KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();

    // Press Enter — should insert newline at BOTH cursor positions
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the Enter to land
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.lines().count() >= 5)
        })
        .unwrap();

    let content = buf(&harness);
    assert!(
        content.lines().count() >= 5,
        "Enter with 2 cursors should produce at least 5 lines (was 4, added 2 newlines). Got {} lines:\n{:?}",
        content.lines().count(),
        content,
    );

    // Type a character to verify both cursors are active via output
    harness.type_text("X").unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.matches('X').count() == 2)
        })
        .unwrap();

    let content = buf(&harness);
    let x_count = content.matches('X').count();
    assert_eq!(
        x_count, 2,
        "Typing after Enter should insert at ALL cursors. Expected 2 X's, found {}:\n{:?}",
        x_count, content,
    );
    harness.assert_no_plugin_errors();
}

/// Pressing Enter with multiple cursors on markdown list items should insert newlines
/// at all cursor positions (falling back to insert_newline for multi-cursor).
#[test]
fn test_multi_cursor_enter_on_list_items() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);

    let fixture =
        TestFixture::new("list_multi.md", "- item one\n- item two\n- item three\n").unwrap();
    open_md(&mut harness, &fixture.path);

    // Go to end of line 1
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();

    // Add cursor on line 2 at end
    harness
        .send_key(KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT)
        .unwrap();

    // Press Enter — with multi-cursor, should fall back to insert_newline
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.lines().count() >= 5)
        })
        .unwrap();

    let content = buf(&harness);
    assert!(
        content.lines().count() >= 5,
        "Enter with 2 cursors on list items should produce at least 5 lines. Got {} lines:\n{:?}",
        content.lines().count(),
        content,
    );

    // Type to verify both cursors work
    harness.type_text("X").unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .is_some_and(|c| c.matches('X').count() == 2)
        })
        .unwrap();

    let content = buf(&harness);
    let x_count = content.matches('X').count();
    assert_eq!(
        x_count, 2,
        "Typing after Enter on list items should insert at ALL cursors. Expected 2 X's, found {}:\n{:?}",
        x_count,
        content,
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Keys sent back to back (#3813)
// ---------------------------------------------------------------------------

/// Hand keys to the editor back to back, as a fast typist or a paste-like
/// burst does: no render, no wait, no draining of plugin work in between.
fn burst(harness: &mut EditorTestHarness, keys: &[(KeyCode, KeyModifiers)]) {
    for &(code, modifiers) in keys {
        harness.editor_mut().handle_key(code, modifiers).unwrap();
    }
}

fn chars(text: &str) -> Vec<(KeyCode, KeyModifiers)> {
    text.chars()
        .map(|c| (KeyCode::Char(c), KeyModifiers::NONE))
        .collect()
}

/// #3813: `- one`, Enter, `two`, Enter, `three` in one burst comes out in
/// the order typed. The list continuation used to come from an async plugin
/// handler, so the characters after Enter landed before it did.
#[test]
fn test_burst_typing_around_enter_keeps_key_order() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let fixture = TestFixture::new("burst.md", "").unwrap();
    open_md(&mut harness, &fixture.path);

    let enter = (KeyCode::Enter, KeyModifiers::NONE);
    let mut keys = chars("- one");
    keys.push(enter);
    keys.extend(chars("two"));
    keys.push(enter);
    keys.extend(chars("three"));
    burst(&mut harness, &keys);
    // The edits are already there, before any async work is looked at.
    assert_eq!(buf(&harness), "- one\n- two\n- three");

    // Tab / Shift+Tab on a blank item and Enter on an empty one, in a burst.
    let tab = (KeyCode::Tab, KeyModifiers::NONE);
    let back_tab = (KeyCode::BackTab, KeyModifiers::SHIFT);
    let mut keys = vec![enter, tab];
    keys.extend(chars("deep"));
    keys.extend([enter, back_tab]);
    keys.extend(chars("out"));
    keys.extend([enter, enter]);
    keys.extend(chars("para"));
    burst(&mut harness, &keys);
    harness.render().unwrap();
    assert_eq!(
        buf(&harness),
        "- one\n- two\n- three\n    + deep\n- out\npara"
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Multiple cursors, undo
// ---------------------------------------------------------------------------

/// Enter continues the marker at every cursor, each with its own kind.
#[test]
fn test_multi_cursor_enter_continues_each_marker() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let fixture = TestFixture::new("mc.md", "- one\n* two\n1. th\n").unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();
    assert_eq!(buf(&harness), "- one\n- X\n* two\n* X\n1. th\n2. X\n");
    harness.assert_no_plugin_errors();
}

/// With several cursors, one on a blank bullet and one on an indented
/// plain line, Shift+Tab applies the rule at the first and the normal
/// dedent at the second, and both cursors land where their line's text
/// moved to.
#[test]
fn test_multi_cursor_shift_tab_mixes_rule_and_normal_dedent() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let fixture = TestFixture::new("mix.md", "    - \n    text\n").unwrap();
    open_md(&mut harness, &fixture.path);

    // Cursor at the end of line 1 and at column 6 of line 2.
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.editor_mut().add_cursor_below();
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness.type_text("X").unwrap();
    assert_eq!(buf(&harness), "* X\nteXxt\n");
    harness.assert_no_plugin_errors();
}

/// A rule's edit is one undo step, like the key it stands in for.
#[test]
fn test_rule_edits_undo_in_one_step() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let fixture = TestFixture::new("undo.md", "- one\n").unwrap();
    open_md(&mut harness, &fixture.path);
    let undo = |h: &mut EditorTestHarness| {
        h.send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    };

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(buf(&harness), "- one\n- \n");
    // Tab rewrites both sides of the cursor (a delete and an insert).
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    assert_eq!(buf(&harness), "- one\n    + \n");
    undo(&mut harness);
    assert_eq!(buf(&harness), "- one\n- \n");
    // Enter on the empty item clears it; undo brings the marker back.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(buf(&harness), "- one\n\n");
    undo(&mut harness);
    assert_eq!(buf(&harness), "- one\n- \n");
    undo(&mut harness);
    assert_eq!(buf(&harness), "- one\n");
    harness.assert_no_plugin_errors();
}

/// A CRLF buffer continues its list with CRLF line breaks: the rule's line
/// ends before the `\r`, and its template's line break is written as the
/// buffer's line ending.
#[test]
fn test_enter_continues_list_in_crlf_buffer() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let fixture = TestFixture::new("crlf.md", "- one\r\n2. two\r\n").unwrap();
    open_md(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();
    assert_eq!(buf(&harness), "- one\r\n- X\r\n2. two\r\n");

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Y").unwrap();
    assert_eq!(buf(&harness), "- one\r\n- X\r\n2. two\r\n3. Y\r\n");
    harness.assert_no_plugin_errors();
}

/// A line that isn't UTF-8 gets the key's normal behaviour: the rules match
/// text, and a lossy copy of the line (U+FFFD for each bad byte) has a
/// different length than the bytes the edit would be made at. It used to
/// compute the edit's offsets from that copy (and underflow in debug builds).
#[test]
fn test_enter_on_a_line_that_is_not_utf8_is_the_plain_newline() {
    let (mut harness, temp_dir) = markdown_harness(100, 24);
    let path = temp_dir.path().join("project_root").join("bytes.md");
    fs::write(&path, b"- a\xffb\n").unwrap();
    open_md(&mut harness, &path);
    // Opened as UTF-8 regardless, so the buffer holds the 0xFF byte.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Reload with Encoding").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.assert_screen_contains("Reload with encoding:");
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("UTF-8").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.assert_screen_contains("a<FF>b");

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("X").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"- a\xffb\nX\n");

    // A UTF-8 line below it still continues its list.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.type_text("- ").unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Y").unwrap();
    assert!(buf(&harness).ends_with("- X\n- Y\n"), "{:?}", buf(&harness));
}

/// With one cursor on a line a rule handles and another between brackets
/// (Enter's bracket expansion moves that cursor back up a line), both
/// cursors land where they belong. The expansion's move was shifted for the
/// rule's edit twice, once by the rules and once by the multi-cursor apply,
/// which put the second cursor past the end of the buffer.
#[test]
fn test_multi_cursor_enter_mixes_rule_and_bracket_expansion() {
    let (mut harness, temp_dir) = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "python": { "edit_rules": [
            { "on": "newline", "before": r"^(\s*# ?)\S", "insert": "\n$1" }
        ] } } }),
    );
    assert!(harness.config().editor.auto_indent);
    assert!(harness.config().editor.auto_close);

    let py = temp_dir.path().join("project_root").join("mix.py");
    fs::write(&py, "# a\nff()").unwrap();
    harness.open_file(&py).unwrap();
    harness.render().unwrap();
    // Cursors between `(` and `)`, and at the end of `# a` above it.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.editor_mut().add_cursor_above();
    assert_eq!(harness.editor().active_cursors().count(), 2);

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let len = buf(&harness).len();
    for (_, c) in harness.editor().active_cursors().iter() {
        assert!(c.position <= len, "a cursor past the end: {}", c.position);
    }
    harness.type_text("X").unwrap();
    let content = buf(&harness);
    let lines: Vec<&str> = content.split('\n').collect();
    assert_eq!(lines.len(), 5, "{content:?}");
    assert_eq!(lines[0], "# a");
    assert_eq!(lines[1], "# X", "{content:?}");
    assert_eq!(lines[2], "ff(", "{content:?}");
    assert_eq!(lines[3].trim_start(), "X", "{content:?}");
    assert_eq!(lines[4], ")", "{content:?}");
}

/// A rule that rewrites only the text after the cursor leaves the cursor
/// where it was (its insert would otherwise carry the cursor past it).
#[test]
fn test_rule_changing_only_the_text_after_the_cursor_keeps_the_cursor() {
    let (mut harness, temp_dir) = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "python": { "edit_rules": [
            { "on": "indent", "before": r"\($", "after": r"^\)$", "replace_after": "  )" }
        ] } } }),
    );
    let py = temp_dir.path().join("project_root").join("after.py");
    fs::write(&py, "f()\n").unwrap();
    harness.open_file(&py).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    assert_eq!(buf(&harness), "f(  )\n");
    harness.type_text("X").unwrap();
    assert_eq!(buf(&harness), "f(X  )\n");
}

// ---------------------------------------------------------------------------
// Edit rules are a language setting: config.json turns them off, adds them
// for other languages, and a reload brings the current ones
// ---------------------------------------------------------------------------

/// Write `config` as the user's config.json and reload, as editing the file
/// and `editor.reloadConfig()` do.
fn write_user_config_and_reload(harness: &mut EditorTestHarness, config: serde_json::Value) {
    let path = harness.editor().dir_context().config_path();
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(&path, config.to_string()).unwrap();
    harness.editor_mut().reload_config();
}

/// `"edit_rules": []` for Markdown in config.json turns list editing off:
/// Enter is the plain newline, Tab the plain Tab.
#[test]
fn test_user_config_turns_markdown_list_editing_off() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "markdown": { "edit_rules": [] } } }),
    );
    assert!(harness.config().languages["markdown"].edit_rules.is_empty());

    let content = enter_at_end_of_first_line(&mut harness, "off.md", "- item\n");
    assert_eq!(content, "- item\n\n", "no list continuation");
    let fixture = TestFixture::new("off_tab.md", "* \n").unwrap();
    open_md(&mut harness, &fixture.path);
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    assert!(
        buf(&harness).starts_with("* "),
        "no bullet cycling: {:?}",
        buf(&harness)
    );
}

/// Rules for another language, written in config.json, apply in its
/// buffers. One that doesn't compile is dropped on its own (with a warning
/// naming `languages.python.edit_rules` and its index); the others apply.
#[test]
fn test_user_config_edit_rules_for_another_language() {
    let (mut harness, temp_dir) = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "python": { "edit_rules": [
            { "on": "newline", "before": "([", "insert": "" },
            { "on": "newline", "before": r"^([ \t]*# ?)\S", "insert": "\n$1" }
        ] } } }),
    );
    let rules = &harness.config().languages["python"].edit_rules;
    assert_eq!(
        rules.len(),
        1,
        "the invalid rule is dropped, the other kept"
    );

    let py = temp_dir.path().join("project_root").join("t.py");
    fs::write(&py, "    # note\nx = 1\n").unwrap();
    harness.open_file(&py).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("more").unwrap();
    assert_eq!(buf(&harness), "    # note\n    # more\nx = 1\n");

    // Markdown keeps its built-in rules: the user's file set Python's only.
    let content = enter_at_end_of_first_line(&mut harness, "still.md", "- item\n");
    assert_eq!(content, "- item\n- \n");
}

/// A buffer already open follows a reload: it gets the rules config.json
/// has now, not the ones it opened with.
#[test]
fn test_config_reload_brings_changed_edit_rules() {
    let (mut harness, _temp_dir) = markdown_harness(80, 24);
    let fixture = TestFixture::new("reload.md", "- item\n> quote\n").unwrap();
    open_md(&mut harness, &fixture.path);
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(buf(&harness), "- item\n- \n> quote\n");
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    assert_eq!(buf(&harness), "- item\n> quote\n");

    // Now Markdown's rules continue block quotes and nothing else.
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "markdown": { "edit_rules": [
            { "on": "newline", "before": "^(>+ )", "insert": "\n$1" }
        ] } } }),
    );
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(
        buf(&harness),
        "- item\n\n> quote\n",
        "the list rule is gone"
    );
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert_eq!(buf(&harness), "- item\n\n> quote\n> \n");
}

// ---------------------------------------------------------------------------
// The mechanism is not Markdown's: a plugin's rules for another language
// ---------------------------------------------------------------------------

/// A test plugin's rules for Python: continue `#` comments on Enter. A
/// malformed set is refused with an error naming the rule; unloading the
/// plugin drops its rules.
#[cfg(feature = "plugins")]
#[test]
fn test_edit_rules_from_another_plugin_for_another_language() {
    use crate::common::harness::copy_plugin_lib;
    use fresh_core::api::EditRuleTrigger;

    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(
        plugins_dir.join("py_comments.ts"),
        r#"/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
try {
  editor.registerEditRules("bad", {
    languages: ["python"],
    rules: [{ on: "newline", before: "(", insert: "" }],
  });
} catch (e) {
  // The message names the rule and the field.
  const named = String(e).includes('rules[0].before:');
  editor.setStatus(named ? "REFUSED-NAMED" : "REFUSED-OTHER " + e);
}
editor.registerEditRules("comments", {
  languages: ["python"],
  rules: [{ on: "newline", before: "^([ \\t]*# ?)\\S", insert: "\n$1" }],
});
"#,
    )
    .unwrap();
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    let python_rules = |h: &EditorTestHarness| {
        h.editor()
            .edit_rules()
            .read()
            .unwrap()
            .has_rules("python", EditRuleTrigger::Newline)
    };
    harness.wait_until(python_rules).unwrap();
    harness
        .wait_until(|h| h.get_status_bar().contains("REFUSED"))
        .unwrap();
    assert!(
        harness.get_status_bar().contains("REFUSED-NAMED"),
        "the error names the rule: {}",
        harness.get_status_bar()
    );

    let py = project_root.join("t.py");
    fs::write(&py, "    # note\n").unwrap();
    harness.open_file(&py).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("more").unwrap();
    assert_eq!(buf(&harness), "    # note\n    # more\n");

    harness
        .editor()
        .plugin_manager()
        .unload_plugin("py_comments")
        .unwrap();
    harness.wait_until(|h| !python_rules(h)).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    assert!(
        !buf(&harness).ends_with("# more\n    # \n"),
        "an unloaded plugin's rules no longer apply: {:?}",
        buf(&harness)
    );
}
