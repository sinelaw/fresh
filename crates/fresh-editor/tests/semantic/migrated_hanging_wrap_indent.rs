//! Migration of `tests/e2e/hanging_wrap_indent.rs` — wrapped
//! continuation rows must inherit a hanging indent that matches the
//! leading whitespace of the source line when `wrap_indent` is on,
//! and must NOT add an indent when `wrap_indent` is off or when the
//! source line has no leading whitespace.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Space-indented hanging indent.** A 4-space indent on the
//!      source line forces continuation rows to begin with at least
//!      4 leading spaces under the default config (`wrap_indent`
//!      defaults on).
//!   2. **`wrap_indent = false` disables the hanging indent.** With
//!      the config flag explicitly off, the continuation row must
//!      have fewer than 4 leading spaces (the source's indent is
//!      no longer mirrored).
//!   3. **Tab-indented hanging indent.** A line opened from disk
//!      with a single leading tab (which expands to a 4-cell visual
//!      width) gives continuation rows at least 4 leading spaces.
//!      Exercises the same hanging-indent path against tab-derived
//!      width rather than literal spaces.
//!   4. **Unindented lines stay unindented.** A long line with no
//!      leading whitespace wraps without injecting any hanging
//!      indent (`<3` leading spaces on the continuation row, since
//!      a single wrap-boundary space can leak through).
//!
//! ## Harness-direct pattern
//!
//! All four claims need to inspect the rendered ratatui buffer's
//! per-row text and parse the gutter separator '│' to split the
//! line-number area from the content area. `screen_to_string()`
//! is the harness's direct buffer-to-string accessor — there's no
//! `EditorTestApi` projection because the gutter character itself
//! is a rendering detail. Migrated as harness-direct, matching
//! the e2e's exact parsing strategy (filter rows containing '│',
//! take the bytes after the first '│' as the content area, count
//! leading spaces).
//!
//! Source: `tests/e2e/hanging_wrap_indent.rs` (4 tests migrated;
//! no tests deferred).

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use std::io::Write;

/// Filter the screen down to "content" rows — rows that show the
/// gutter separator '│'. Matches the e2e's filtering exactly.
fn content_lines(screen: &str) -> Vec<String> {
    screen
        .lines()
        .filter(|l| l.contains('│'))
        .map(|s| s.to_string())
        .collect()
}

/// For a content line, return the text after the FIRST '│'
/// separator (matches the e2e parsing precisely).
fn content_after_first_bar(line: &str) -> &str {
    let bar = line.find('│').expect("content line must contain '│'");
    &line[bar + '│'.len_utf8()..]
}

fn leading_spaces(s: &str) -> usize {
    s.chars().take_while(|c| *c == ' ').count()
}

#[test]
fn migrated_hanging_wrap_indent_basic() {
    // Original: `test_hanging_wrap_indent_basic`.
    // Terminal 60 cols, gutter ~8, scrollbar 1 → ~51 cols for text.
    // 4-space indent + long content → continuation should also be
    // indented 4 spaces.
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    let text = "    This is a long indented line that will wrap around because it is too long to fit in a single visual line in the editor.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines = content_lines(&screen);

    assert!(
        lines.len() >= 2,
        "Should have at least 2 content lines (original + wrapped). \
         Got: {}.\nScreen:\n{}",
        lines.len(),
        screen
    );

    let first_content = content_after_first_bar(&lines[0]);
    let second_content = content_after_first_bar(&lines[1]);
    let first_leading = leading_spaces(first_content);
    let second_leading = leading_spaces(second_content);

    assert!(
        second_leading >= 4,
        "Continuation line should be indented by at least 4 spaces \
         (matching original indent). Got first_leading={}, \
         second_leading={}.\nFirst: {:?}\nSecond: {:?}\nScreen:\n{}",
        first_leading,
        second_leading,
        first_content,
        second_content,
        screen
    );
}

#[test]
fn migrated_hanging_wrap_indent_disabled() {
    // Original: `test_hanging_wrap_indent_disabled`.
    let config = Config {
        editor: fresh::config::EditorConfig {
            line_wrap: true,
            wrap_indent: false,
            ..Default::default()
        },
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(60, 24, config).unwrap();

    let text = "    This is a long indented line that will wrap around because it is too long to fit in a single visual line in the editor.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines = content_lines(&screen);

    assert!(
        lines.len() >= 2,
        "Should have at least 2 content lines. Screen:\n{}",
        screen
    );

    let second_content = content_after_first_bar(&lines[1]);
    let second_leading = leading_spaces(second_content);

    assert!(
        second_leading < 4,
        "With wrap_indent disabled, continuation should not be \
         indented. Got {} leading spaces.\nSecond: {:?}\nScreen:\n{}",
        second_leading,
        second_content,
        screen
    );
}

#[test]
fn migrated_hanging_wrap_indent_with_tabs() {
    // Original: `test_hanging_wrap_indent_with_tabs`. A tab-indented
    // line opened from disk — the tab expands to a 4-cell visual
    // width and the continuation must mirror that width.
    let mut harness = EditorTestHarness::with_temp_project(60, 24).unwrap();

    let dir = harness.project_dir().unwrap();
    let file_path = dir.join("tab_indent_test.txt");
    {
        let mut f = std::fs::File::create(&file_path).unwrap();
        writeln!(
            f,
            "\tThis is a long tab-indented line that will wrap around because it is too long to fit in a single visual line."
        )
        .unwrap();
    }
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines = content_lines(&screen);

    assert!(
        lines.len() >= 2,
        "Should have at least 2 content lines (original + wrapped). \
         Got: {}.\nScreen:\n{}",
        lines.len(),
        screen
    );

    let second_content = content_after_first_bar(&lines[1]);
    let second_leading = leading_spaces(second_content);

    assert!(
        second_leading >= 4,
        "Tab-indented continuation line should be indented by at \
         least 4 spaces (matching tab width). Got {} leading \
         spaces.\nSecond: {:?}\nScreen:\n{}",
        second_leading,
        second_content,
        screen
    );
}

#[test]
fn migrated_hanging_wrap_indent_no_indent() {
    // Original: `test_hanging_wrap_indent_no_indent`. Long line with
    // no leading whitespace wraps without injecting a hanging indent.
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    let text = "This line has no indentation but is long enough to wrap around because it exceeds the terminal width significantly here.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines = content_lines(&screen);

    assert!(lines.len() >= 2, "Should wrap. Screen:\n{}", screen);

    let second_content = content_after_first_bar(&lines[1]);
    let second_leading = leading_spaces(second_content);
    assert!(
        second_leading < 3,
        "Unindented lines should not get hanging indent. Got {} \
         leading spaces.\nScreen:\n{}",
        second_leading,
        screen
    );
}

/// Anti-test: drop the leading-4-space indent on the typed text.
/// Without the source indent the continuation row must NOT inherit
/// >= 4 leading spaces — proves the positive
/// `migrated_hanging_wrap_indent_basic` claim is gated on the
/// source line actually starting with whitespace, not on the
/// continuation row trivially having spaces from some other
/// source (e.g. padding).
#[test]
fn anti_hanging_wrap_indent_without_leading_spaces_has_no_hanging_indent() {
    let mut harness = EditorTestHarness::new(60, 24).unwrap();

    // Same long content but with the 4-space prefix dropped.
    let text = "This is a long indented line that will wrap around because it is too long to fit in a single visual line in the editor.";
    harness.type_text(text).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    let lines = content_lines(&screen);
    assert!(
        lines.len() >= 2,
        "Content must still wrap to >= 2 rows for the anti to be \
         meaningful. Screen:\n{}",
        screen
    );

    let second_content = content_after_first_bar(&lines[1]);
    let second_leading = leading_spaces(second_content);
    assert!(
        second_leading < 4,
        "anti: without the source's 4-space indent, the continuation \
         row must NOT inherit a >= 4-space hanging indent. Got {} \
         leading spaces.\nSecond: {:?}\nScreen:\n{}",
        second_leading,
        second_content,
        screen
    );
}
