//! End-to-end tests for the markdown_source plugin
//!
//! Tests the markdown-source mode that auto-activates for .md files:
//! - Auto-activation when opening a markdown file in source mode
//! - Enter key auto-continues list items (bullets, ordered, checkboxes)
//! - Enter on empty list item removes the marker
//! - Tab key inserts spaces (non-list context) or indents + cycles bullet
//! - Mode deactivates when switching to a non-markdown buffer

use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::time::Duration;

/// Create a harness with the markdown_source plugin loaded.
/// Waits for the plugin to fully initialise (mode defined + event handler registered).
fn markdown_source_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_source");
    copy_plugin_lib(&plugins_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root,
    )
    .unwrap();

    // Wait for the plugin to be fully loaded.
    // The plugin registers the "markdown-source" mode at load time via defineMode.
    // process_async_and_render picks up the DefineMode command from the plugin thread.
    let loaded = harness
        .wait_for_async(
            |h| h.editor().mode_registry().has_mode("markdown-source"),
            10_000,
        )
        .unwrap();
    assert!(
        loaded,
        "markdown_source plugin did not load within 10 seconds"
    );

    (harness, temp_dir)
}

/// Helper: open a markdown file and wait for the markdown-source mode to auto-activate.
fn open_md_and_wait_for_mode(harness: &mut EditorTestHarness, path: &std::path::Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();

    // Wait for the plugin's buffer_activated handler to set the mode
    let activated = harness
        .wait_for_async(
            |h| h.editor().editor_mode() == Some("markdown-source".to_string()),
            5_000,
        )
        .unwrap();
    assert!(
        activated,
        "markdown-source mode did not activate for {:?}. Current mode: {:?}",
        path,
        harness.editor().editor_mode(),
    );
}

/// Helper: get buffer content, panicking if unavailable.
fn buf(harness: &EditorTestHarness) -> String {
    harness
        .get_buffer_content()
        .expect("buffer content should be available")
}

// ---------------------------------------------------------------------------
// Auto-activation
// ---------------------------------------------------------------------------

/// Opening a .md file in source mode should auto-activate the markdown-source mode.
#[test]
fn test_markdown_source_mode_auto_activates() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let fixture = TestFixture::new("readme.md", "# Hello\n\nWorld\n").unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    assert_eq!(
        harness.editor().editor_mode(),
        Some("markdown-source".to_string()),
        "markdown-source mode should auto-activate for .md files"
    );
    harness.assert_no_plugin_errors();
}

/// Opening a non-markdown file should NOT activate the markdown-source mode.
#[test]
fn test_markdown_source_mode_not_active_for_non_md() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let fixture = TestFixture::new("main.rs", "fn main() {}\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Give plugins time to react
    for _ in 0..5 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }

    assert_eq!(
        harness.editor().editor_mode(),
        None,
        "markdown-source mode should NOT activate for non-markdown files"
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
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "- top\n  - nested\n";
    let fixture = TestFixture::new("list.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Move to line 2 ("  - nested"), then End to go to end of line
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should insert newline + "  - " (same indent + bullet)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("  - nested\n  - "))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.contains("  - nested\n  - "),
        "Expected newline with continued bullet '  - ' after '  - nested'. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

/// Pressing Enter at the end of an ordered list item should insert the next number.
#[test]
fn test_enter_continues_ordered_list() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "1. first\n2. second\n";
    let fixture = TestFixture::new("ordered.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Move to line 2 ("2. second"), End
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should insert "\n3. "
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("2. second\n3. "))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.contains("2. second\n3. "),
        "Expected '3. ' after '2. second'. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

/// Pressing Enter at the end of a checkbox item should insert an unchecked checkbox.
#[test]
fn test_enter_continues_checkbox() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "- [x] done task\n";
    let fixture = TestFixture::new("checkbox.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should insert "\n- [ ] " (unchecked)
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("- [x] done task\n- [ ] "))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.contains("- [x] done task\n- [ ] "),
        "Expected unchecked checkbox continuation. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

/// Pressing Enter on an empty list item (just the bullet) should remove the marker.
#[test]
fn test_enter_clears_empty_bullet() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "- item\n- \n";
    let fixture = TestFixture::new("empty.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Move to line 2 ("- "), End
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter — should remove "- " leaving an empty line
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("- item\n\n"))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.contains("- item\n\n"),
        "Expected empty bullet to be cleared. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Enter key: non-list (whitespace copying)
// ---------------------------------------------------------------------------

/// Pressing Enter on a line with no indentation should just insert a plain newline.
#[test]
fn test_enter_no_indent_on_unindented_line() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "Hello world\n";
    let fixture = TestFixture::new("plain.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Cursor starts at line 1 col 1; move to end
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("Hello world\n\n"))
            },
            5_000,
        )
        .unwrap();
    assert!(ok, "Enter should have been processed");

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
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "        deep indent text\n";
    let fixture = TestFixture::new("deep.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Go to end of the line
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("deep indent text\n        "))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.contains("deep indent text\n        "),
        "Expected 8-space indent on new line. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Tab key: insert spaces (non-list context)
// ---------------------------------------------------------------------------

/// Tab should insert 4 spaces at the cursor position.
#[test]
fn test_tab_inserts_spaces() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "text\n";
    let fixture = TestFixture::new("tab.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Cursor is at the beginning of "text"; press Home to be sure
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab — should insert 4 spaces before "text"
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.starts_with("    text"))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.starts_with("    text"),
        "Expected 4 spaces before 'text' after Tab. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

/// Multiple Tab presses should accumulate spaces.
#[test]
fn test_multiple_tabs() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "x\n";
    let fixture = TestFixture::new("tabs.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Tab twice -> 8 spaces
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.starts_with("        x"))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.starts_with("        x"),
        "Expected 8 spaces (two tabs) before 'x'. Got:\n{:?}",
        content,
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Tab key: indent + cycle bullet on blank list item
// ---------------------------------------------------------------------------

/// Tab on a blank unordered list item should indent and cycle the bullet.
#[test]
fn test_tab_cycles_bullet_on_blank_item() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    // Start with "* " (asterisk bullet, no content)
    let content = "* \n";
    let fixture = TestFixture::new("cycle.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Tab: * -> - (indented by 4)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();

    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.starts_with("    - "))
            },
            5_000,
        )
        .unwrap();

    let content = buf(&harness);
    assert!(
        ok && content.starts_with("    - "),
        "Expected '    - ' after Tab on '* '. Got:\n{:?}",
        content,
    );
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
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    // Start with "    - " — as if Tab was already pressed once on "* "
    let content = "    - \n";
    let fixture = TestFixture::new("reverse_cycle.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Shift+Tab #1:  "    - " → "* "   (de-indent, reverse cycle - → *)
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();

    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .map_or(false, |c| c.starts_with("* \n"))
        })
        .unwrap();
    harness.assert_no_plugin_errors();
}

/// Full round-trip: Tab cycles * → - → + → *, each adding 4 spaces of indent,
/// then Shift+Tab reverses the entire sequence back to the starting state.
#[test]
fn test_tab_shift_tab_full_round_trip() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    // Start with "* " (asterisk bullet, no content)
    let content = "* \n";
    let fixture = TestFixture::new("roundtrip.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

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
                .map_or(false, |c| c.starts_with("    - \n"))
        })
        .unwrap();
    drain(&mut harness);

    // Tab #2: "    - " → "        + "
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .map_or(false, |c| c.starts_with("        + \n"))
        })
        .unwrap();
    drain(&mut harness);

    // Tab #3: "        + " → "            * "
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| {
            h.get_buffer_content()
                .map_or(false, |c| c.starts_with("            * \n"))
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
                .map_or(false, |c| c.starts_with("        + \n"))
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
                .map_or(false, |c| c.starts_with("    - \n"))
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
                .map_or(false, |c| c.starts_with("* \n"))
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
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "\n";
    let fixture = TestFixture::new("type.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

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
// Mode deactivation when switching buffers
// ---------------------------------------------------------------------------

/// Switching from a .md buffer to a non-.md buffer should deactivate the mode.
#[test]
fn test_mode_deactivates_on_buffer_switch() {
    let (mut harness, temp_dir) = markdown_source_harness(80, 24);

    // Open markdown file first — mode activates
    let md_fixture = TestFixture::new("doc.md", "# Doc\n").unwrap();
    open_md_and_wait_for_mode(&mut harness, &md_fixture.path);
    assert_eq!(
        harness.editor().editor_mode(),
        Some("markdown-source".to_string()),
    );

    // Create and open a plain text file
    let txt_path = temp_dir.path().join("project_root").join("notes.txt");
    fs::write(&txt_path, "plain text\n").unwrap();
    harness.open_file(&txt_path).unwrap();
    harness.render().unwrap();

    // Wait for mode to deactivate
    let deactivated = harness
        .wait_for_async(|h| h.editor().editor_mode().is_none(), 5_000)
        .unwrap();

    assert!(
        deactivated,
        "markdown-source mode should deactivate when switching to a non-md file. Current mode: {:?}",
        harness.editor().editor_mode(),
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// .mdx and .markdown extensions
// ---------------------------------------------------------------------------

/// The plugin should also activate for .mdx files.
#[test]
fn test_activates_for_mdx() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let fixture = TestFixture::new("component.mdx", "# MDX file\n").unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    assert_eq!(
        harness.editor().editor_mode(),
        Some("markdown-source".to_string()),
        "Should activate for .mdx files"
    );
    harness.assert_no_plugin_errors();
}

/// The plugin should also activate for .markdown files.
#[test]
fn test_activates_for_markdown_extension() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let fixture = TestFixture::new("readme.markdown", "# Readme\n").unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    assert_eq!(
        harness.editor().editor_mode(),
        Some("markdown-source".to_string()),
        "Should activate for .markdown files"
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Enter + Tab combined workflow
// ---------------------------------------------------------------------------

/// Simulate a realistic list editing flow: type on a list item, press Enter
/// (auto-continue bullet), then type a sub-item.
#[test]
fn test_enter_then_type_workflow() {
    let (mut harness, _temp_dir) = markdown_source_harness(80, 24);

    let content = "- item\n";
    let fixture = TestFixture::new("workflow.md", content).unwrap();
    open_md_and_wait_for_mode(&mut harness, &fixture.path);

    // Go to end of "- item"
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Enter -> should auto-insert "- "
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let ok = harness
        .wait_for_async(
            |h| {
                h.get_buffer_content()
                    .map_or(false, |c| c.contains("- item\n- "))
            },
            5_000,
        )
        .unwrap();
    assert!(ok, "Enter should continue bullet");

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
