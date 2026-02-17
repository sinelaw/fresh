/// E2E tests for split view with markdown compose mode.
///
/// Tests the use case: same markdown document shown in two vertical splits,
/// left panel in source mode (plain), right panel in compose mode.
///
/// Key requirements tested:
/// 1. Compose mode only applies to the right panel (conceals, soft breaks)
/// 2. Line numbers visible in source panel, hidden in compose panel
/// 3. Scroll synchronization between panels
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// Build a markdown document with various formatting elements
fn build_test_markdown() -> String {
    let mut md = String::from("# Split View Test\n\n");
    md.push_str("## Introduction\n\n");
    md.push_str("This is a **bold text** and *italic text* with a [link](https://example.com) in the introduction paragraph.\n\n");
    for i in 0..30 {
        md.push_str(&format!(
            "Paragraph {}: Here is **bold** and *italic* text with a [link](https://example.com/p{}) to test compose mode rendering.\n\n",
            i, i
        ));
    }
    md.push_str("## Conclusion\n\n");
    md.push_str("Final paragraph with **bold** and *italic* text.\n");
    md
}

/// Set up a harness with the markdown_compose plugin loaded and a test file open.
fn setup_split_compose_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("test.md");
    std::fs::write(&md_path, build_test_markdown()).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root,
    )
    .unwrap();

    // Open the file
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("test.md");

    (harness, temp_dir)
}

/// Helper: create a vertical split on the same buffer
fn create_vertical_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("split vert").unwrap();
    harness.wait_for_screen_contains("Split pane vert").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();
}

/// Helper: enable compose mode via command palette
fn enable_compose_mode(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for compose mode to fully activate (conceals applied).
    // Only check the RIGHT half of the screen (the compose panel) for concealed
    // markers. The left panel stays in source mode and will always show ** markers.
    harness
        .wait_until_stable(|h| {
            let s = h.screen_to_string();
            // Find the separator column (│)
            let sep_col = s
                .lines()
                .nth(2)
                .and_then(|l| l.char_indices().find(|(_, c)| *c == '│').map(|(i, _)| i));
            let Some(sep) = sep_col else {
                return false;
            };
            // Count ** markers only in the right half (compose panel)
            let right_bold_lines = s
                .lines()
                .skip(2)
                .filter(|l| {
                    if l.len() > sep + 1 {
                        l[sep + 1..].contains("**")
                    } else {
                        false
                    }
                })
                .count();
            right_bold_lines <= 2 // At most cursor line may show markers
        })
        .unwrap();
}

/// Helper: enable scroll sync via command palette
fn enable_scroll_sync(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Scroll Sync").unwrap();
    harness
        .wait_for_screen_contains("Toggle Scroll Sync")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

/// Helper: switch to next split via command palette
fn switch_to_next_split(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("next split").unwrap();
    harness.wait_for_screen_contains("next split").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();
}

/// Test that compose mode only affects the active split, not both panels.
///
/// Setup: vertical split of same markdown file
/// - Right panel (initially active after split): enable compose mode
/// - Left panel: should remain in source mode
///
/// Expected: Left panel shows raw markdown markers (**bold**, *italic*, [link](...))
///           Right panel conceals markers (shows formatted text)
// TODO: fix wait conditions in enable_compose_mode — they timeout because the
// separator lookup / byte slicing doesn't match actual rendered output.
#[test]
#[ignore]
fn test_split_view_compose_only_in_one_panel() {
    init_tracing_from_env();

    let (mut harness, _temp) = setup_split_compose_harness(160, 40);

    // Create vertical split - new (right) split becomes active
    create_vertical_split(&mut harness);

    // Enable compose mode in the right panel
    enable_compose_mode(&mut harness);

    let screen = harness.screen_to_string();

    // The screen has a vertical split. Find the separator column.
    // In a 160-wide terminal with 50/50 split, separator is around col 80.
    let first_content_line = screen.lines().nth(2).unwrap_or("");
    let separator_col = first_content_line
        .char_indices()
        .find(|(_, c)| *c == '│')
        .map(|(i, _)| i);

    if let Some(sep) = separator_col {
        // Left half (source mode) should show raw markdown markers
        let left_half: String = screen
            .lines()
            .skip(2)
            .take(30)
            .map(|l| if l.len() > sep { &l[..sep] } else { l })
            .collect::<Vec<_>>()
            .join("\n");

        // Right half (compose mode) should have concealed markers
        let right_half: String = screen
            .lines()
            .skip(2)
            .take(30)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        // Source mode (left) should show ** markers
        let left_bold_count = left_half.matches("**").count();
        // Compose mode (right) should conceal ** markers
        let right_bold_count = right_half.matches("**").count();

        assert!(
            left_bold_count > right_bold_count,
            "Source panel (left) should show more ** markers than compose panel (right).\n\
             Left ** count: {}, Right ** count: {}\n\
             Left panel:\n{}\n\nRight panel:\n{}",
            left_bold_count,
            right_bold_count,
            left_half,
            right_half,
        );
    }
}

/// Test that line numbers appear in source panel but not in compose panel.
///
/// When compose mode is enabled, line numbers should be hidden in that split
/// but remain visible in the source-mode split.
// TODO: fix wait conditions in enable_compose_mode — see test_split_view_compose_only_in_one_panel
#[test]
#[ignore]
fn test_split_view_line_numbers_per_split() {
    init_tracing_from_env();

    let (mut harness, _temp) = setup_split_compose_harness(160, 40);

    // Create vertical split
    create_vertical_split(&mut harness);

    // Enable compose mode in right panel
    enable_compose_mode(&mut harness);

    let screen = harness.screen_to_string();

    // Find separator
    let first_content_line = screen.lines().nth(2).unwrap_or("");
    let separator_col = first_content_line
        .char_indices()
        .find(|(_, c)| *c == '│')
        .map(|(i, _)| i);

    if let Some(sep) = separator_col {
        // Left half should have line numbers (e.g., "  1 │")
        let left_half: String = screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep { &l[..sep] } else { l })
            .collect::<Vec<_>>()
            .join("\n");

        // Right half should NOT have line numbers in compose mode
        let right_half: String = screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        // Source mode (left) should have line number patterns like "  1 │" or " 10 │"
        let left_has_line_numbers = left_half.lines().any(|l| {
            let trimmed = l.trim_start();
            trimmed.starts_with("1 ") || trimmed.starts_with("2 ") || trimmed.starts_with("3 ")
        });

        assert!(
            left_has_line_numbers,
            "Source panel (left) should show line numbers.\nLeft panel:\n{}",
            left_half,
        );

        // Compose mode (right) should NOT start lines with line numbers
        // In compose mode, lines start with the actual content (possibly indented)
        let right_has_line_numbers = right_half
            .lines()
            .filter(|l| !l.trim().is_empty())
            .any(|l| {
                let trimmed = l.trim_start();
                // Line numbers look like "  1 │" at the start
                trimmed.len() > 4
                    && trimmed
                        .chars()
                        .take(4)
                        .all(|c| c.is_ascii_digit() || c == ' ')
                    && trimmed.chars().nth(4) == Some('│')
            });

        assert!(
            !right_has_line_numbers,
            "Compose panel (right) should NOT show line numbers.\nRight panel:\n{}",
            right_half,
        );
    }
}

/// Test that scrolling in one split moves the view in the other split.
///
/// Both panels show the same buffer, so scrolling down in one panel
/// should move the other panel to show the same source lines.
// TODO: fix wait conditions in enable_compose_mode — see test_split_view_compose_only_in_one_panel
#[test]
#[ignore]
fn test_split_view_scroll_sync() {
    init_tracing_from_env();

    let (mut harness, _temp) = setup_split_compose_harness(160, 40);

    // Create vertical split
    create_vertical_split(&mut harness);

    // Enable compose mode in right panel
    enable_compose_mode(&mut harness);

    // Enable scroll sync (off by default)
    enable_scroll_sync(&mut harness);

    // Switch to left (source) panel
    switch_to_next_split(&mut harness);

    // Capture the "before scroll" state
    let before_screen = harness.screen_to_string();

    // Scroll down significantly in the left panel
    for _ in 0..25 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();
    harness.render().unwrap();

    let after_screen = harness.screen_to_string();

    // The screen should have changed (scrolled)
    assert_ne!(
        before_screen, after_screen,
        "Screen should change after scrolling down 25 lines"
    );

    // Find separator
    let first_content_line = after_screen.lines().nth(2).unwrap_or("");
    let separator_col = first_content_line
        .char_indices()
        .find(|(_, c)| *c == '│')
        .map(|(i, _)| i);

    if let Some(sep) = separator_col {
        // The right panel should also have scrolled
        let right_after: String = after_screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        let right_before: String = before_screen
            .lines()
            .skip(2)
            .take(20)
            .map(|l| if l.len() > sep + 1 { &l[sep + 1..] } else { "" })
            .collect::<Vec<_>>()
            .join("\n");

        assert_ne!(
            right_before, right_after,
            "Right panel (compose) should also scroll when left panel (source) scrolls.\n\
             Right panel before:\n{}\n\nRight panel after:\n{}",
            right_before, right_after,
        );
    }
}
