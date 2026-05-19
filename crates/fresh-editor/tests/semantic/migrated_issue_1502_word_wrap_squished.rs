//! Migration of `tests/e2e/issue_1502_word_wrap_squished.rs` —
//! regression cover for issue #1502 ("Word wrap does not seem right:
//! squished"): on a narrow terminal with `wrap_indent` (hanging
//! indent) enabled, deeply indented lines must not wrap to ~7 chars
//! per continuation row.
//!
//! Root cause being guarded against: hanging indent was being
//! double-counted in the wrapping transform — once when
//! `effective_width` subtracted `line_indent` from `available_width`,
//! and again because `emit_break_with_indent` emitted the indent as
//! actual text content that counted toward `current_line_width`. With
//! `available_width=27` and `line_indent=10`, the bug shrank
//! continuation capacity from 17 to 7 chars.
//!
//! Load-bearing claim preserved here:
//!
//!   * On a 35-col terminal with `line_wrap=true`, `wrap_indent=true`,
//!     a 10-space-indented long line wraps with at least 10 visible
//!     content chars per continuation row (excluding the final
//!     remainder row, which can be arbitrarily short). The "squished"
//!     regression would put each continuation at ~7 chars.
//!
//! ## Harness-direct pattern
//!
//! The test parses the per-row screen text via `screen_to_string()`
//! to find continuation rows (rows with no digits in the gutter
//! area). The gutter character '│' and the digit/no-digit split
//! are rendering details with no `EditorTestApi` projection;
//! migrated as harness-direct, matching the e2e's parsing.
//!
//! Source: `tests/e2e/issue_1502_word_wrap_squished.rs` (1 test
//! migrated; no tests deferred).

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

#[test]
fn migrated_issue_1502_wrap_indent_squished_on_narrow_terminal() {
    // Original: `test_issue_1502_wrap_indent_squished_on_narrow_terminal`.
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

    let indented_text =
        "          abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    harness.type_text(indented_text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // Parse continuation lines from the screen output exactly the
    // way the e2e did: rows containing '│', and a continuation row
    // is one whose gutter area (text before the last '│') has no
    // ASCII digit.
    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    let mut continuation_content_widths: Vec<usize> = Vec::new();
    for line in &content_lines {
        let bar_byte = match line.rfind('│') {
            Some(pos) => pos,
            None => continue,
        };
        let gutter_area = &line[..bar_byte];

        let has_line_number = gutter_area.chars().any(|c| c.is_ascii_digit());
        if has_line_number {
            continue;
        }

        let content = &line[bar_byte + '│'.len_utf8()..];
        let trimmed = content.trim();
        if !trimmed.is_empty() {
            continuation_content_widths.push(trimmed.chars().count());
        }
    }

    assert!(
        !continuation_content_widths.is_empty(),
        "Text should wrap on a 35-col terminal with 10-space \
         indent.\nScreen:\n{}",
        screen
    );

    // Skip the final continuation (the remainder after the last
    // full wrap, which can be arbitrarily short).
    let check_up_to = continuation_content_widths.len().saturating_sub(1);
    for (i, &width) in continuation_content_widths[..check_up_to]
        .iter()
        .enumerate()
    {
        assert!(
            width >= 10,
            "Issue #1502: Continuation line {} has only {} visible \
             characters — word wrap is 'squished' because hanging \
             indent is double-counted in apply_wrapping_transform.\n\
             Screen:\n{}",
            i,
            width,
            screen
        );
    }
}

/// Anti-test: drop the `line_wrap = true` config flag (the
/// load-bearing precondition that makes wrapping happen at all).
/// Without it, no continuation rows are produced and the parser
/// finds zero continuation widths — proves the positive
/// `migrated_issue_1502_*` claim is gated on `line_wrap=true`,
/// not on something the renderer does at the default config.
#[test]
fn anti_issue_1502_without_line_wrap_produces_no_continuation_rows() {
    let config = Config {
        editor: fresh::config::EditorConfig {
            // line_wrap defaults to false; explicit for clarity.
            line_wrap: false,
            wrap_indent: true,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(35, 24, config).unwrap();

    let indented_text =
        "          abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    harness.type_text(indented_text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let content_lines: Vec<&str> = screen.lines().filter(|l| l.contains('│')).collect();

    let mut continuation_content_widths: Vec<usize> = Vec::new();
    for line in &content_lines {
        let bar_byte = match line.rfind('│') {
            Some(pos) => pos,
            None => continue,
        };
        let gutter_area = &line[..bar_byte];
        if gutter_area.chars().any(|c| c.is_ascii_digit()) {
            continue;
        }
        let content = &line[bar_byte + '│'.len_utf8()..];
        let trimmed = content.trim();
        if !trimmed.is_empty() {
            continuation_content_widths.push(trimmed.chars().count());
        }
    }

    assert!(
        continuation_content_widths.is_empty(),
        "anti: without line_wrap=true, no wrapped-continuation \
         rows must be produced (the line stays on one logical row \
         and the renderer scrolls horizontally instead). Got \
         widths: {:?}.\nScreen:\n{}",
        continuation_content_widths,
        screen
    );
}
