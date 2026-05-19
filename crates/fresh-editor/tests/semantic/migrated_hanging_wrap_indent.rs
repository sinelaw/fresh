//! DECLARATIVE: Migration of `tests/e2e/hanging_wrap_indent.rs` —
//! wrapped continuation rows must inherit a hanging indent that
//! matches the leading whitespace of the source line when
//! `wrap_indent` is on, and must NOT add an indent when
//! `wrap_indent` is off or when the source line has no leading
//! whitespace.
//!
//! Scenarios are pure data; the runner executes them. Each
//! `LayoutScenario` carries its terminal geometry, the buffer text
//! (or on-disk fixture for the tab-indent case), the declarative
//! editor-config overrides for `wrap_indent`, and a
//! `RenderSnapshotExpect` whose `row_checks` express the
//! hanging-indent claim via the
//! [`RowMatch::ContentRowLeadingSpaces`] matcher (extension added
//! in this migration).
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
//!      indent (`< 3` leading spaces on the continuation row, since
//!      a single wrap-boundary space can leak through).
//!
//! Source: `tests/e2e/hanging_wrap_indent.rs` (4 tests migrated +
//! 1 anti-test; no tests deferred).

use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};

const LONG_INDENTED: &str = "    This is a long indented line that will wrap around because it is too long to fit in a single visual line in the editor.";
const LONG_NO_INDENT: &str = "This line has no indentation but is long enough to wrap around because it exceeds the terminal width significantly here.";
const LONG_TAB_INDENTED: &str = "\tThis is a long tab-indented line that will wrap around because it is too long to fit in a single visual line.";

#[test]
fn migrated_hanging_wrap_indent_basic() {
    // Terminal 60 cols, gutter ~8, scrollbar 1 → ~51 cols for text.
    // 4-space indent + long content → second content row (the wrap
    // continuation) must be indented >= 4 spaces.
    assert_layout_scenario(LayoutScenario {
        description: "wrap_indent default on: 4-space source indent → \
                      continuation has >= 4 leading spaces"
            .into(),
        initial_text: LONG_INDENTED.into(),
        width: 60,
        height: 24,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::ContentRowLeadingSpaces {
                nth_content_row: 1,
                min: Some(4),
                max: None,
            }],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_hanging_wrap_indent_disabled() {
    // `wrap_indent = false` overrides the default — the continuation
    // row must NOT inherit the source's 4-space indent.
    assert_layout_scenario(LayoutScenario {
        description: "wrap_indent=false: continuation row drops \
                      the source's leading-space indent"
            .into(),
        initial_text: LONG_INDENTED.into(),
        width: 60,
        height: 24,
        config_overrides: ScenarioConfigOverrides {
            line_wrap: Some(true),
            wrap_indent: Some(false),
            ..Default::default()
        },
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::ContentRowLeadingSpaces {
                nth_content_row: 1,
                min: None,
                max: Some(3),
            }],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_hanging_wrap_indent_with_tabs() {
    // Tab-indented line written to a temp file and opened via the
    // editor's real file-open pipeline (`initial_file`). The tab
    // expands to a 4-cell visual width and the continuation must
    // mirror that width.
    let dir = tempfile::TempDir::new().unwrap();
    let path = dir.path().join("tab_indent_test.txt");
    std::fs::write(&path, format!("{LONG_TAB_INDENTED}\n")).unwrap();

    assert_layout_scenario(LayoutScenario {
        description: "tab-indented source line opened from disk: \
                      continuation row mirrors tab width (>= 4 cols)"
            .into(),
        initial_file: Some(path),
        width: 60,
        height: 24,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::ContentRowLeadingSpaces {
                nth_content_row: 1,
                min: Some(4),
                max: None,
            }],
            ..Default::default()
        },
        ..Default::default()
    });
}

#[test]
fn migrated_hanging_wrap_indent_no_indent() {
    // Long line with no leading whitespace wraps without injecting
    // a hanging indent.
    assert_layout_scenario(LayoutScenario {
        description: "no source indent: continuation row has < 3 \
                      leading spaces (one wrap-boundary space can leak)"
            .into(),
        initial_text: LONG_NO_INDENT.into(),
        width: 60,
        height: 24,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::ContentRowLeadingSpaces {
                nth_content_row: 1,
                min: None,
                max: Some(2),
            }],
            ..Default::default()
        },
        ..Default::default()
    });
}

/// Anti-test: drop the leading-4-space indent on the source text.
/// Without the source indent the continuation row must NOT inherit
/// >= 4 leading spaces — proves the positive
/// `migrated_hanging_wrap_indent_basic` claim is gated on the
/// source line actually starting with whitespace, not on the
/// continuation row trivially having spaces from some other source.
#[test]
fn anti_hanging_wrap_indent_without_leading_spaces_has_no_hanging_indent() {
    let scenario = LayoutScenario {
        description: "anti: source has no indent → continuation row \
                      must NOT have >= 4 leading spaces (asserting \
                      so would be a vacuous claim)"
            .into(),
        initial_text: LONG_NO_INDENT.into(),
        width: 60,
        height: 24,
        expected_snapshot: RenderSnapshotExpect {
            // Mirror the positive test's claim. The anti drops the
            // source's leading whitespace; this assertion must fail.
            row_checks: vec![RowMatch::ContentRowLeadingSpaces {
                nth_content_row: 1,
                min: Some(4),
                max: None,
            }],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: without the source's 4-space indent, the \
         continuation row must NOT inherit a >= 4-space hanging \
         indent — asserting it does should fail"
    );
}
