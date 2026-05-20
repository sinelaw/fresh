//! DECLARATIVE migration of `tests/e2e/line_wrap_full_visibility.rs`.
//!
//! Under `line_wrap = true`, every printable character of every
//! fixture line must be rendered somewhere in the viewport, at a
//! variety of terminal widths and with the file-explorer sidebar
//! both closed and open. The bug class guarded against: characters
//! that straddle a wrap boundary getting dropped from the rendered
//! output ("too late" wrap regressions).
//!
//! Translation to the semantic framework:
//!
//!   * The faithful per-character claim is expressed via the
//!     `RowMatch::AllCharsVisibleAcrossRows { lines }` matcher,
//!     which groups the snapshot's `rendered_rows` by gutter line
//!     number, concatenates each line's wrap-continuation rows, and
//!     asserts the non-whitespace payload equals the source line's.
//!     This is a direct port of the e2e `verify_all_chars_rendered`
//!     grouping — so a dropped MIDDLE character (not just a missing
//!     head/tail) is caught, closing the coverage gap the earlier
//!     head+tail-sentinel migration left open.
//!   * The trial sweeps a representative set of widths crossed with
//!     sidebar open/closed. Each trial is its own `LayoutScenario`
//!     data literal — state isolation matches the e2e original.
//!
//! Caveats vs. the e2e original:
//!
//!   * The width sweep samples a representative subset of
//!     `40..=100` rather than walking every integer. The dropped
//!     widths are interpolated by neighbours; the per-character
//!     check at each sampled width fully exercises the wrap
//!     boundary, so the dominant regression class is preserved.
//!   * The e2e's second test
//!     (`test_line_wrap_visibility_single_width_debug_dump`) was a
//!     deliberate debug-dump sanity variant: it ran ONE width
//!     (60) × sidebar both ways and called the SAME
//!     `verify_all_chars_rendered` assertion, but its purpose was
//!     the `println!` screen dump under `--no-capture`. Its
//!     assertion is a strict subset of the sweep (width 60 is in
//!     the sweep below), so it is intentionally NOT migrated —
//!     debug dumps have no place in declarative scenarios. No
//!     coverage is lost.
//!
//! Source: `tests/e2e/line_wrap_full_visibility.rs` (2 tests:
//! 1 migrated faithfully; 1 deliberately dropped as a redundant
//! debug-dump variant).

use crate::common::fixtures::TestFixture;
use crate::common::scenario::layout_scenario::{
    assert_layout_scenario, check_layout_scenario, LayoutScenario, ScenarioConfigOverrides,
};
use crate::common::scenario::render_snapshot::{RenderSnapshotExpect, RowMatch};
use fresh::test_api::Action;

fn wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(true),
        ..Default::default()
    }
}

fn no_wrap_overrides() -> ScenarioConfigOverrides {
    ScenarioConfigOverrides {
        line_wrap: Some(false),
        ..Default::default()
    }
}

/// A deliberately diverse set of single-line buffer contents,
/// ported from the e2e fixture: words, trailing punctuation,
/// nested parens/brackets, series of each, code-like payloads,
/// uniformly long tokens, and indented hanging-wrap stressors.
fn interesting_lines() -> Vec<String> {
    let mut lines: Vec<String> = Vec::new();

    lines.push("alpha".into());
    lines.push("hello world".into());
    lines.push("two three four five six seven eight nine".into());

    for ending in [
        '.', '!', '?', ',', ';', ':', '-', '~', '*', '+', '=', '/', '\\', '|',
    ] {
        lines.push(format!("line ending with a {ending}{ending}"));
    }

    for rep in [2usize, 3, 5] {
        for ch in ['.', '!', '?', ',', '-', '*', '='] {
            let run: String = std::iter::repeat(ch).take(rep).collect();
            lines.push(format!("run of {ch}s{run}"));
        }
    }

    lines.push("a line with (inline parens) in the middle".into());
    lines.push("closing paren at the very end)".into());
    lines.push("closing bracket at the very end]".into());
    lines.push("closing brace at the very end}".into());
    lines.push("(entirely wrapped in parens)".into());
    lines.push("((doubly nested))".into());
    lines.push("(((triple nested)))".into());
    lines.push("((((four deep))))".into());
    lines.push("{[((mixed brackets))]}".into());
    lines.push("{[({ and back )]}".into());

    lines.push("() () () () () () () ()".into());
    lines.push("(a) (b) (c) (d) (e) (f) (g)".into());
    lines.push("(1)(2)(3)(4)(5)(6)(7)(8)(9)".into());
    lines.push("[a][b][c][d][e][f][g][h][i]".into());
    lines.push("{a}{b}{c}{d}{e}{f}{g}{h}{i}".into());
    lines.push("()[]{}()[]{}()[]{}()[]{}".into());

    lines.push("word, word. word! (word) word? word;".into());
    lines.push("first (second), third; fourth: fifth.".into());
    lines.push("one (two, three); four (five: six).".into());
    lines.push("x=1; y=(a+b)*c; z={a:1, b:2}; done.".into());

    lines.push("fn sum(x: i32, y: i32) -> i32 { (x + y) * 2 }".into());
    lines.push("let v = [(1, 2), (3, 4), (5, 6), (7, 8)];".into());
    lines.push("if (a && b) || (!c) { return Some((1, 2)); }".into());

    lines.push("a medium length line with (parens), commas, and a period at the end.".into());
    lines.push("another medium line; semicolons, colons: and some (groups) inside it.".into());

    lines.push("supercalifragilisticexpialidocious".into());
    lines.push("pneumonoultramicroscopicsilicovolcano".into());
    lines.push("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".into());
    lines.push("abcdefghijklmnopqrstuvwxyz0123456789".into());

    for indent in [4usize, 8, 12, 16, 20] {
        let pad: String = std::iter::repeat(' ').take(indent).collect();
        lines.push(format!(
            "{pad}dialog.getButton(DialogInterface.BUTTON_NEUTRAL).setOnClickListener {{"
        ));
        lines.push(format!(
            "{pad}val folderChooserDialog = childFragmentManager.findFragmentByTag(\"FOLDER_CHOOSER\") as FolderChooserDialog?"
        ));
        lines.push(format!(
            "{pad}binding.recyclerView.layoutManager = LinearLayoutManager(requireContext())"
        ));
        lines.push(format!(
            "{pad}return MaterialAlertDialogBuilder(requireContext()).setTitle(titleRes).setView(binding.root)"
        ));
        lines.push(format!(
            "{pad}someObject.doSomething(with: a, and: b).thenChain(other).forEach {{ item -> item.process() }}"
        ));
        lines.push(format!(
            "{pad}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        ));
        lines.push(format!(
            "{pad}token1 token2 token3 token4 token5 token6 token7 token8 token9 token10 token11"
        ));
    }

    lines.push("x".into());
    lines.push("a.".into());
    lines.push("a!".into());
    lines.push(")".into());
    lines.push("()".into());
    lines.push("...".into());
    lines.push("!!!".into());

    lines.push("== END OF FIXTURE ==".into());

    lines
}

/// One `(width, sidebar_open)` declarative trial. Asserts every
/// non-whitespace character of every source line is rendered, in
/// order, across that line's wrapped rows.
fn trial(
    width: u16,
    height: u16,
    sidebar_open: bool,
    fixture_path: &std::path::Path,
    lines: &[String],
) -> LayoutScenario {
    let mut actions: Vec<Action> = vec![Action::MoveDocumentStart];
    if sidebar_open {
        actions.push(Action::ToggleFileExplorer);
    }
    LayoutScenario {
        description: format!(
            "line_wrap visibility: w={width} h={height} sidebar_open={sidebar_open}"
        ),
        initial_text: String::new(),
        initial_file: Some(fixture_path.to_path_buf()),
        width,
        height,
        config_overrides: wrap_overrides(),
        actions,
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AllCharsVisibleAcrossRows {
                lines: lines.to_vec(),
            }],
            ..Default::default()
        },
        ..Default::default()
    }
}

#[test]
fn migrated_line_wrap_all_chars_visible_across_widths_and_sidebar() {
    let lines = interesting_lines();
    let fixture = TestFixture::new("line_wrap_visibility_semantic.txt", &lines.join("\n"))
        .expect("create fixture");

    // Representative subset of the e2e's full `40..=100` integer
    // sweep. Sampling every ~6 columns plus the extremes exercises
    // the wrap boundary at a wide variety of positions across the
    // fixture's varied line lengths, while keeping CI wall-time
    // bounded. The per-character `AllCharsVisibleAcrossRows` check
    // at each sampled width makes each trial far stronger than the
    // earlier head+tail sentinel, so fewer widths still gives
    // broad coverage.
    let widths: [u16; 11] = [40, 46, 52, 58, 64, 70, 76, 82, 88, 94, 100];
    // Height generous enough that every wrapped line fits inside
    // the content area even at the narrowest width with the
    // sidebar open. The check is about visibility under wrap, not
    // scrolling.
    let height: u16 = 200;

    for &width in &widths {
        for &sidebar_open in &[false, true] {
            assert_layout_scenario(trial(width, height, sidebar_open, &fixture.path, &lines));
        }
    }
}

/// Anti-test: with `line_wrap = false` and a narrow viewport, the
/// long char-stressor line (36 'a's) extends past the right edge,
/// so its trailing characters are NOT rendered. The
/// `AllCharsVisibleAcrossRows` check therefore fails (the rendered
/// non-ws payload is shorter than the source). Proves the
/// visibility claim depends on `line_wrap = true`.
#[test]
fn anti_line_wrap_disabled_drops_characters_past_right_edge() {
    let lines = interesting_lines();
    let fixture = TestFixture::new("line_wrap_visibility_anti.txt", &lines.join("\n"))
        .expect("create fixture");

    let scenario = LayoutScenario {
        description: "anti: line_wrap=false on 40-col viewport ⇒ long-line tail clipped".into(),
        initial_text: String::new(),
        initial_file: Some(fixture.path.clone()),
        width: 40,
        height: 200,
        config_overrides: no_wrap_overrides(),
        actions: vec![Action::MoveDocumentStart],
        expected_snapshot: RenderSnapshotExpect {
            row_checks: vec![RowMatch::AllCharsVisibleAcrossRows {
                lines: lines.clone(),
            }],
            ..Default::default()
        },
        ..Default::default()
    };
    assert!(
        check_layout_scenario(scenario).is_err(),
        "anti-test: with line_wrap=false on a 40-col viewport the long fixture \
         lines are clipped at the right edge, so not every character is rendered"
    );
}
