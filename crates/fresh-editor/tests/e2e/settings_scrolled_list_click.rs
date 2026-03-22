//! E2E test for clicking on items in a scrolled settings list.
//!
//! Regression test: when the settings panel is scrolled and you click on an item
//! in a multi-row control (e.g., the Languages map), the click should select the
//! correct item, not an item offset by the scroll amount.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// When the Languages list is scrolled and we click on a visible entry,
/// the correct language should become focused (show "[Enter to edit]").
///
/// The bug: ControlLayoutInfo stores only screen Rects for map/list rows,
/// losing the real data index. When the settings panel is scrolled so that
/// a multi-row control's top is clipped, enumerate() returns visual indices
/// (0, 1, 2...) that don't match the real data indices, causing clicks to
/// focus the wrong entry.
#[test]
fn test_settings_scrolled_languages_list_mouse_click() {
    // Use a shorter terminal to force more scrolling (Languages has ~45 entries)
    let mut harness = EditorTestHarness::new(100, 30).unwrap();

    // Open settings (General category is selected by default)
    harness.open_settings().unwrap();

    // Use search to navigate directly to the Languages setting
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness.type_text("languages").unwrap();
    harness.render().unwrap();

    // Press Enter to jump to the Languages setting
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The Languages map should now be visible. Verify it.
    harness.assert_screen_contains("Languages:");

    // Now scroll the settings panel down using mouse scroll. This pushes the
    // Languages label (and first entries) above the viewport while keeping
    // later entries visible. The scroll happens on the settings content area.
    // We need the content area coordinates - use the center of the right panel.
    let scroll_col = 60u16;
    let scroll_row = 15u16;

    // Scroll down enough to clip the Languages label and first several entries
    for _ in 0..15 {
        harness.mouse_scroll_down(scroll_col, scroll_row).unwrap();
    }
    harness.render().unwrap();

    // Verify that Languages label is no longer visible (scrolled above viewport)
    // and that some language entries ARE visible
    let screen = harness.screen_to_string();

    // We should see some language entries but not the "Languages:" label
    // (it should be scrolled above). If it's still visible, scroll more.
    if screen.contains("Languages:") {
        for _ in 0..10 {
            harness.mouse_scroll_down(scroll_col, scroll_row).unwrap();
        }
        harness.render().unwrap();
    }

    // Find a visible language entry to click on. Look for known language names.
    let target = find_clickable_language(&harness);
    let screen = harness.screen_to_string();
    assert!(
        target.is_some(),
        "Expected to find a language entry on screen after scrolling.\nScreen:\n{}",
        screen
    );
    let (target_name, click_col, click_row) = target.unwrap();

    // Click on the target language entry
    harness.mouse_click(click_col, click_row).unwrap();
    harness.render().unwrap();

    // After clicking, the target language should now be the focused entry.
    // The "[Enter to edit]" hint should appear on the row with our target language.
    let screen_after = harness.screen_to_string();

    let mut found = false;
    for line in screen_after.lines() {
        if line.contains("[Enter to edit]") {
            assert!(
                line.contains(&target_name),
                "After clicking on '{}' (at row {}), expected it to become focused, \
                 but '[Enter to edit]' appeared on a different entry.\n\
                 Focused line: {}\nFull screen:\n{}",
                target_name,
                click_row,
                line.trim(),
                screen_after
            );
            found = true;
            break;
        }
    }
    assert!(
        found,
        "No '[Enter to edit]' found on screen after clicking on '{}'.\nScreen:\n{}",
        target_name, screen_after
    );
}

/// Find a visible language entry on screen. Returns (name, col, row).
fn find_clickable_language(harness: &EditorTestHarness) -> Option<(String, u16, u16)> {
    let known_langs = [
        "astro",
        "bash",
        "clojure",
        "cmake",
        "cpp",
        "csharp",
        "css",
        "dart",
        "dockerfile",
        "earthfile",
        "elixir",
        "erlang",
        "fsharp",
        "gleam",
        "go",
        "gomod",
        "graphql",
        "haskell",
        "html",
        "hyprlang",
        "ini",
        "java",
        "javascript",
        "json",
        "julia",
        "kotlin",
        "lua",
        "markdown",
        "nix",
        "ocaml",
        "perl",
        "php",
        "python",
        "ruby",
        "rust",
        "scala",
        "sql",
        "swift",
        "toml",
        "typescript",
        "yaml",
        "zig",
    ];

    let buf = harness.buffer();
    for row in 0..buf.area.height {
        let row_text = harness.screen_row_text(row);
        // Skip focused rows, headers, and non-entry rows
        if row_text.contains("[Enter to edit]")
            || row_text.contains("Languages")
            || row_text.contains("Name")
            || row_text.contains("[+]")
            || row_text.contains("──")
        {
            continue;
        }
        for lang in &known_langs {
            // Language map entries look like: "      langname              Grammar"
            if row_text.contains(&format!("  {}  ", lang)) {
                let col = 40u16; // Within the content area
                return Some((lang.to_string(), col, row));
            }
        }
    }
    None
}
