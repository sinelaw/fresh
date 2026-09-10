use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

/// The resize shortcut must reach the layout while the dock's PTY owns focus.
/// Observe both the rendered dock boundary and the size reported by its shell.
#[test]
#[cfg(unix)]
fn test_kitty_split_resize_in_focused_terminal_dock() {
    use crossterm::event::{KeyCode, KeyModifiers};
    use fresh::config::{Config, Keybinding, TerminalShellConfig};
    use fresh::server::input_parser::{Event, InputParser};

    fn send_bytes(harness: &mut EditorTestHarness, bytes: &[u8]) {
        for event in InputParser::new().parse(bytes) {
            if let Event::Key(press) = event {
                harness.send_key_press(press).unwrap();
            }
        }
    }

    fn probe(harness: &mut EditorTestHarness, tag: &str) -> (usize, u16) {
        harness.type_text(tag).unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        let marker = format!("SIZE_{tag}_");
        harness.wait_for_screen_contains(&marker).unwrap();
        let screen = harness.screen_to_string();
        let (row, line) = screen
            .lines()
            .enumerate()
            .find(|(_, line)| line.contains(&marker))
            .unwrap();
        let height = line
            .split_once(&marker)
            .unwrap()
            .1
            .split('_')
            .next()
            .unwrap()
            .parse()
            .unwrap();
        (row, height)
    }

    let temp = TempDir::new().unwrap();
    let file = temp.path().join("editor.txt");
    std::fs::write(&file, "EDITOR_UNCHANGED\n").unwrap();
    let mut config = Config::default();
    config.terminal.shell = Some(TerminalShellConfig {
        command: "/bin/sh".into(),
        args: vec![
            "-c".into(),
            "stty -echo; printf 'READY\\n'; \
             while IFS= read -r line; do \
               [ \"$line\" = exit ] && break; \
               set -- $(stty size); \
               printf '\\033[2J\\033[HSIZE_%s_%s_%s\\n' \"$line\" \"$1\" \"$2\"; \
             done; printf 'STOPPED\\n'"
                .into(),
        ],
    });
    for (key, action) in [("l", "increase_split_size"), ("k", "decrease_split_size")] {
        config.keybindings.push(Keybinding {
            key: key.into(),
            modifiers: vec!["ctrl".into(), "shift".into(), "super".into()],
            keys: Vec::new(),
            action: action.into(),
            args: Default::default(),
            when: Some("terminal".into()),
        });
    }
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(130, 42, config, temp.path().to_path_buf())
            .unwrap();
    harness.open_file(&file).unwrap();
    harness.run_palette_command("Split Vertical").unwrap();
    harness
        .run_palette_command("Open Terminal in Utility Dock")
        .unwrap();
    harness.wait_for_screen_contains("READY").unwrap();
    let before = probe(&mut harness, "before");
    send_bytes(&mut harness, b"\x1b[108;14u");
    let increased = probe(&mut harness, "increased");
    // Native IncreaseSplitSize grows the parent's FIRST child (the editors),
    // so the bottom dock gets shorter, irrespective of which child has focus.
    assert!(increased.0 > before.0, "dock boundary must move down");
    assert!(
        increased.1 < before.1,
        "shell must receive the shorter PTY size"
    );
    send_bytes(&mut harness, b"\x1b[107;14u");
    assert_eq!(probe(&mut harness, "restored"), before);
    let screen = harness.screen_to_string();
    assert_eq!(screen.matches("EDITOR_UNCHANGED").count(), 2);
    assert_eq!(screen.matches("editor.txt").count(), 2);
    harness.type_text("exit").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("STOPPED").unwrap();
    harness.run_palette_command("Close Buffer").unwrap();
    harness.assert_screen_contains("EDITOR_UNCHANGED");
}

/// Test that viewport uses full available area after terminal resize at startup
///
/// This test reproduces a bug where:
/// 1. Terminal starts with small size (e.g. 15 rows)
/// 2. Program starts
/// 3. Terminal is resized to larger size (e.g. 30 rows)
/// 4. File is opened
/// 5. Viewport should use all available rows but instead only shows a few lines
///    matching the initial small size
#[test]
fn test_viewport_uses_full_area_after_startup_resize() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_file.txt");

    // Create a test file with 50 lines so we have plenty of content
    let content: String = (1..=50).map(|i| format!("Line {}\n", i)).collect();
    std::fs::write(&file_path, content).unwrap();

    // 1. Start with small terminal size (15 rows)
    let mut harness = EditorTestHarness::new(80, 15).unwrap();
    harness.render().unwrap();

    // 2. Resize terminal to larger size (30 rows) before opening file
    harness.resize(80, 30).unwrap();

    // 3. Open file after resize
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // 4. Check how many lines are visible in the rendered screen
    let screen = harness.screen_to_string();
    let screen_lines: Vec<&str> = screen.lines().collect();

    println!("Terminal height: 30 rows");
    println!("Total screen lines: {}", screen_lines.len());
    println!("\nScreen content:");
    for (i, line) in screen_lines.iter().enumerate() {
        println!("Row {:2}: {:?}", i, line);
    }

    // Count how many content lines are visible (lines with " │ " separator)
    // This filters out tab bar, status bar, etc.
    let content_lines: Vec<&str> = screen_lines
        .iter()
        .filter(|line| line.contains(" │ "))
        .copied()
        .collect();

    println!(
        "\nContent lines with ' │ ' separator: {}",
        content_lines.len()
    );
    for (i, line) in content_lines.iter().enumerate() {
        println!("Content line {:2}: {:?}", i, line);
    }

    // With a 30-row terminal:
    // - 1 row for tab bar (at top)
    // - 1 row for status bar (at bottom)
    // - This leaves 28 rows for content
    //
    // The bug: If the viewport was initialized with the small size (15 rows),
    // we would only see ~13 content lines (15 - 2 for tab/status bars).
    // After the fix, we should see ~28 content lines.

    let expected_min_content_lines = 25; // Allow some margin for status bars, etc.

    assert!(
        content_lines.len() >= expected_min_content_lines,
        "Expected at least {} visible content lines after resize to 30 rows, but only found {}.\n\
         This suggests the viewport is still using the old small size ({} rows) instead of \
         the new size (30 rows).",
        expected_min_content_lines,
        content_lines.len(),
        15
    );

    // Additionally verify we can see lines beyond what would be visible in a 15-row terminal
    // In a 15-row terminal, we'd only see lines 1-13 (roughly)
    // After resize to 30 rows, we should be able to see more
    harness.assert_screen_contains("Line 20");
}

/// Test that viewport correctly updates when resizing after file is already open
#[test]
fn test_viewport_updates_after_resize_with_open_file() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_file.txt");

    // Create a test file with 50 lines
    let content: String = (1..=50).map(|i| format!("Line {}\n", i)).collect();
    std::fs::write(&file_path, content).unwrap();

    // Start with small terminal and open file
    let mut harness = EditorTestHarness::new(80, 15).unwrap();
    harness.open_file(&file_path).unwrap();

    // Count visible lines before resize
    let screen_before = harness.screen_to_string();
    let content_lines_before: Vec<&str> = screen_before
        .lines()
        .filter(|line| line.contains(" │ "))
        .collect();

    println!(
        "Visible content lines before resize (15 rows): {}",
        content_lines_before.len()
    );

    // Resize to larger terminal
    harness.resize(80, 30).unwrap();

    // Count visible lines after resize
    let screen_after = harness.screen_to_string();
    let content_lines_after: Vec<&str> = screen_after
        .lines()
        .filter(|line| line.contains(" │ "))
        .collect();

    println!(
        "Visible content lines after resize (30 rows): {}",
        content_lines_after.len()
    );

    // After resize, we should see more lines
    assert!(
        content_lines_after.len() > content_lines_before.len(),
        "After resize from 15 to 30 rows, should see more content lines. \
         Before: {}, After: {}",
        content_lines_before.len(),
        content_lines_after.len()
    );

    // Should see at least 25 lines with 30-row terminal
    let expected_min_content_lines = 25;
    assert!(
        content_lines_after.len() >= expected_min_content_lines,
        "Expected at least {} visible content lines after resize to 30 rows, but only found {}",
        expected_min_content_lines,
        content_lines_after.len()
    );
}
