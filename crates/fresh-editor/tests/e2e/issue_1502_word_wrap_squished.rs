use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

/// Reproduce issue #1502: "Word wrap does not seem right: squished"
///
/// On a narrow terminal with wrap_indent (hanging indent) enabled, deeply
/// indented lines wrap to very few characters per continuation line, producing
/// a "squished" vertical column of text.
///
/// The root cause is that hanging indent is double-counted in the wrapping
/// transform: once when `effective_width` subtracts `line_indent` from
/// `available_width`, and again because `emit_break_with_indent` emits
/// the indent as actual text content that counts toward `current_line_width`.
///
/// For example, with available_width=27 and line_indent=10:
///   - effective_width on continuation = 27 - 10 = 17
///   - current_line_width after break = 10 (from emitted indent text)
///   - remaining_width = 17 - 10 = 7
///   → only 7 chars of content per continuation, instead of 17
#[test]
fn test_issue_1502_wrap_indent_squished_on_narrow_terminal() {
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: true,
            wrap_indent: true,
            ..Default::default()
        },
        ..Default::default()
    };
    // 35-column terminal: gutter ~8, available ~27.
    // With 10-space indent, the bug causes:
    //   eff_width = 27 - 10 = 17, current_line_width = 10, remaining = 7
    // So continuation lines are severely squished to ~7 chars each.
    // Without the bug they'd show 27 - 10 = 17 chars each.
    let mut harness = EditorTestHarness::with_config(35, 24, config).unwrap();

    // Type a line with 10-space indentation followed by enough content to wrap.
    // We use a known alphabet so we can verify the content on each line.
    let indented_text = "          abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    harness.type_text(indented_text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Parse continuation lines from the screen output.
    // The screen format has lines like:
    //   "│   1 │          abcdefghijklmnopq"   (line with number)
    //   "      │          rstuvwxyz01234567"   (continuation, no number)
    // We use rfind('│') to find the last gutter separator on each line,
    // since the leftmost '│' may be a pane border.
    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    let mut continuation_content_widths: Vec<usize> = Vec::new();
    for line in &content_lines {
        // Find the last │ separator (the gutter-to-content boundary)
        let bar_byte = match line.rfind('│') {
            Some(pos) => pos,
            None => continue,
        };
        let gutter_area = &line[..bar_byte];

        // A line-number line has digits in the gutter; a continuation line does not.
        let has_line_number = gutter_area.chars().any(|c| c.is_ascii_digit());
        if has_line_number {
            continue;
        }

        // Extract the content after the gutter separator
        let content = &line[bar_byte + '│'.len_utf8()..];
        let trimmed = content.trim();
        if !trimmed.is_empty() {
            continuation_content_widths.push(trimmed.chars().count());
        }
    }

    assert!(
        !continuation_content_widths.is_empty(),
        "Text should wrap on a 35-col terminal with 10-space indent.\nScreen:\n{}",
        screen
    );

    // Without the double-counting bug, each continuation should show
    // available_width - indent ≈ 17 chars of content.
    // With the bug, it shows only about 6-7 chars (available - 2*indent ≈ 7).
    //
    // We assert that continuations show at least 10 chars — enough to distinguish
    // correct wrapping from squished wrapping.  The final continuation holds
    // the leftover after the last full wrap and can be arbitrarily short; skip
    // it so this stays an "is-it-squished" check.
    let check_up_to = continuation_content_widths.len().saturating_sub(1);
    for (i, &width) in continuation_content_widths[..check_up_to]
        .iter()
        .enumerate()
    {
        assert!(
            width >= 10,
            "Issue #1502: Continuation line {} has only {} visible characters — \
             word wrap is 'squished' because hanging indent is double-counted \
             in apply_wrapping_transform.\nScreen:\n{}",
            i,
            width,
            screen
        );
    }
}
