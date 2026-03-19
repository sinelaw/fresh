use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use std::io::Write;

/// Test that wrapped continuation lines are indented to match the leading whitespace
/// of the original line when wrap_indent is enabled.
#[test]
fn test_hanging_wrap_indent_basic() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Type an indented line that will wrap.
    // Terminal 60 cols, gutter ~8, scrollbar 1 → ~51 cols for text.
    // 4-space indent + long content → continuation should also be indented 4 spaces.
    let text = "    This is a long indented line that will wrap around because it is too long to fit in a single visual line in the editor.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Find lines with the gutter separator
    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    // There should be at least 2 content lines (the original + continuation)
    assert!(
        content_lines.len() >= 2,
        "Should have at least 2 content lines (original + wrapped). Got: {}.\nScreen:\n{}",
        content_lines.len(),
        screen
    );

    // The first content line should start with indented text after the gutter
    let first_line = content_lines[0];
    let bar_pos = first_line.find('│').unwrap();
    let first_content = &first_line[bar_pos + '│'.len_utf8()..];

    // The continuation line (second content line) should also start with spaces
    // matching the original line's indentation
    if content_lines.len() >= 2 {
        let second_line = content_lines[1];
        let bar_pos2 = second_line.find('│').unwrap();
        let second_content = &second_line[bar_pos2 + '│'.len_utf8()..];

        // Count leading spaces in first and second content lines
        let first_leading = first_content.chars().take_while(|c| *c == ' ').count();
        let second_leading = second_content.chars().take_while(|c| *c == ' ').count();

        // The continuation line should have at least as many leading spaces as the
        // original line's indentation (4 spaces)
        assert!(
            second_leading >= 4,
            "Continuation line should be indented by at least 4 spaces (matching original indent). \
             Got first_leading={}, second_leading={}.\nFirst: {:?}\nSecond: {:?}\nScreen:\n{}",
            first_leading,
            second_leading,
            first_content,
            second_content,
            screen
        );
    }
}

/// Test that wrap_indent can be disabled
#[test]
fn test_hanging_wrap_indent_disabled() {
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: true,
            wrap_indent: false,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(60, 24, config).unwrap();

    // Type an indented line that will wrap
    let text = "    This is a long indented line that will wrap around because it is too long to fit in a single visual line in the editor.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    assert!(
        content_lines.len() >= 2,
        "Should have at least 2 content lines. Screen:\n{}",
        screen
    );

    // With wrap_indent disabled, the continuation should NOT have extra indentation
    let second_line = content_lines[1];
    let bar_pos = second_line.find('│').unwrap();
    let second_content = &second_line[bar_pos + '│'.len_utf8()..];
    let second_leading = second_content.chars().take_while(|c| *c == ' ').count();

    // The continuation should start with content, not with indent spaces
    // (It might have 0 or 1 leading space from the wrapping, but not 4)
    assert!(
        second_leading < 4,
        "With wrap_indent disabled, continuation should not be indented. \
         Got {} leading spaces.\nSecond: {:?}\nScreen:\n{}",
        second_leading,
        second_content,
        screen
    );
}

/// Test that wrapped continuation lines are indented when the original line uses tab indentation.
/// This is the same as test_hanging_wrap_indent_basic but with tabs instead of spaces.
#[test]
fn test_hanging_wrap_indent_with_tabs() {
    let mut harness = EditorTestHarness::with_temp_project(60, 24).unwrap();

    // Create a temp file with a tab-indented long line
    let dir = harness.project_dir().unwrap();
    let file_path = dir.join("tab_indent_test.txt");
    {
        let mut f = std::fs::File::create(&file_path).unwrap();
        // One tab + long content that will wrap (tab expands to 4 cols in the editor)
        writeln!(
            f,
            "\tThis is a long tab-indented line that will wrap around because it is too long to fit in a single visual line."
        )
        .unwrap();
    }
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    assert!(
        content_lines.len() >= 2,
        "Should have at least 2 content lines (original + wrapped). Got: {}.\nScreen:\n{}",
        content_lines.len(),
        screen
    );

    // The continuation line should have hanging indent matching the tab's visual width (4 spaces)
    let second_line = content_lines[1];
    let bar_pos = second_line.find('│').unwrap();
    let second_content = &second_line[bar_pos + '│'.len_utf8()..];
    let second_leading = second_content.chars().take_while(|c| *c == ' ').count();

    assert!(
        second_leading >= 4,
        "Tab-indented continuation line should be indented by at least 4 spaces (matching tab width). \
         Got {} leading spaces.\nSecond: {:?}\nScreen:\n{}",
        second_leading,
        second_content,
        screen
    );
}

/// Test that unindented lines wrap without extra indentation
#[test]
fn test_hanging_wrap_indent_no_indent() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Unindented line that will wrap
    let text = "This line has no indentation but is long enough to wrap around because it exceeds the terminal width significantly here.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    assert!(content_lines.len() >= 2, "Should wrap. Screen:\n{}", screen);

    // For unindented lines, continuation should not be indented
    let second_line = content_lines[1];
    let bar_pos = second_line.find('│').unwrap();
    let second_content = &second_line[bar_pos + '│'.len_utf8()..];

    // Check that continuation starts directly with content (maybe 1 space from wrapping)
    let second_leading = second_content.chars().take_while(|c| *c == ' ').count();
    assert!(
        second_leading < 3,
        "Unindented lines should not get hanging indent. Got {} leading spaces.\nScreen:\n{}",
        second_leading,
        screen
    );
}
