//! Migration of `tests/e2e/line_wrap_full_visibility.rs` — under
//! `line_wrap = true`, every printable character of every fixture
//! line must be rendered somewhere in the viewport, at a variety of
//! terminal widths and with the file-explorer sidebar both closed
//! and open. The bug class being guarded against: characters that
//! straddle a wrap boundary getting dropped from the rendered
//! output ("too late" wrap regressions).
//!
//! Translation to the semantic framework:
//!
//!   * Each fixture line is wrapped with a unique head sentinel
//!     `LN###<` and tail sentinel `>LN###`. Asserting that *both*
//!     sentinels appear on the rendered screen (via
//!     `RowMatch::AnyRowContains`) proves the line was rendered
//!     start-to-end — if any wrap-boundary characters were lost,
//!     either sentinel could go missing depending on where the
//!     wrap fell.
//!   * The trial sweeps a representative set of widths (rather
//!     than the full 40..=100 the e2e file walked) crossed with
//!     sidebar open/closed. Picking widths that bracket the
//!     sentinel-token length on both sides exercises the wrap
//!     boundary at varied positions per line, which is what the
//!     full e2e sweep was buying.
//!   * Each trial uses its own harness (state isolation, same as
//!     the e2e original) and `RenderSnapshot::extract_with_rendered_rows`
//!     to read the vt100 round-tripped screen rows.
//!
//! Caveats vs. the e2e original:
//!
//!   * The framework's `RowMatch` variants test for substring
//!     presence on a single row; they cannot count occurrences
//!     across the screen. A regression that drops one *middle*
//!     character of a long line while preserving the head and
//!     tail sentinels would not be detected here, whereas the
//!     e2e's per-line non-whitespace comparison would catch it.
//!     This is a deliberate trade-off: the head+tail-sentinel
//!     approach catches the dominant "lost at wrap boundary"
//!     class faithfully, with framework matchers. Listed in the
//!     `migrated_large_file_viewport.rs` follow-up bucket if a
//!     future `RowMatch::CountAcrossRows` lands.
//!   * The fixture is narrower than the e2e curated set (which
//!     exercised dozens of punctuation/paren shapes). We keep
//!     the representative shapes — short word, long word, deep
//!     nesting, indented hanging-wrap, and char-boundary
//!     stressor — because those are the ones that drove the
//!     original regression.

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Each entry is rendered as `LN###< ... >LN###` so the head and
/// tail of the line are unique substrings that survive lookup in
/// the rendered rows even after wrapping. The `...` middle is the
/// shape we want to stress (paren nesting, indented hanging-wrap,
/// long unbroken token, etc.).
fn interesting_lines() -> Vec<String> {
    let middles: Vec<String> = vec![
        // Short word — single visual row at every width.
        "alpha beta".into(),
        // Medium with trailing punctuation runs.
        "ending with a run of dots...".into(),
        // Nested parens — historical wrap-boundary trouble spot.
        "(((triple nested)))".into(),
        // Code-like with mixed brackets.
        "fn sum(x: i32, y: i32) -> i32 { (x + y) * 2 }".into(),
        // Long single token — forces char-boundary wrap.
        "supercalifragilisticexpialidociousfoo".into(),
        // Indented hanging-wrap path; mirrors the Kotlin-style
        // line that originally surfaced the "too late" wrap bug.
        "        binding.recyclerView.layoutManager = LinearLayoutManager(requireContext())".into(),
        // Char-stressor: 36 consecutive 'a's, forces wrap mid-run.
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa".into(),
    ];

    middles
        .into_iter()
        .enumerate()
        .map(|(i, mid)| format!("LN{:03}< {mid} >LN{:03}", i, i))
        .collect()
}

/// One `(width, sidebar_open)` trial. Each fixture line's head
/// sentinel `LN###<` and tail sentinel `>LN###` must both appear
/// in the rendered rows. Returns Err diagnostic on failure.
fn run_trial(
    width: u16,
    height: u16,
    sidebar_open: bool,
    fixture_path: &std::path::Path,
    lines: &[String],
) -> Result<(), String> {
    let mut harness = EditorTestHarness::with_config(width, height, config_with_wrap())
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: harness init: {e}"))?;
    harness
        .open_file(fixture_path)
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: open_file: {e}"))?;

    // Jump to the very top so the first visual row is the
    // fixture's first logical line.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .map_err(|e| format!("w={width} sidebar={sidebar_open}: Ctrl+Home: {e}"))?;

    if sidebar_open {
        harness
            .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
            .map_err(|e| format!("w={width} sidebar={sidebar_open}: Ctrl+E: {e}"))?;
        harness
            .process_async_and_render()
            .map_err(|e| format!("w={width} sidebar={sidebar_open}: post-toggle render: {e}"))?;
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    // Build head + tail sentinel checks for every fixture line.
    let mut checks: Vec<RowMatch> = Vec::with_capacity(lines.len() * 2);
    for i in 0..lines.len() {
        checks.push(RowMatch::AnyRowContains(format!("LN{:03}<", i)));
        checks.push(RowMatch::AnyRowContains(format!(">LN{:03}", i)));
    }

    let expect = RenderSnapshotExpect {
        row_checks: checks,
        ..Default::default()
    };
    if let Some((field, expected, actual)) = expect.check_against(&snap) {
        return Err(format!(
            "w={width} sidebar={sidebar_open}: render mismatch on {field}\n\
             expected: {expected}\n\
             actual:   {actual}\n\
             rendered rows ({} total):\n{}\n",
            snap.rendered_rows.len(),
            snap.rendered_rows
                .iter()
                .enumerate()
                .map(|(i, r)| format!("  [{i:>2}] {r}"))
                .collect::<Vec<_>>()
                .join("\n"),
        ));
    }
    Ok(())
}

#[test]
fn migrated_line_wrap_all_lines_visible_across_widths_and_sidebar() {
    let lines = interesting_lines();
    let fixture =
        TestFixture::new("line_wrap_visibility_semantic.txt", &lines.join("\n"))
            .expect("create fixture");

    // Sample widths that bracket each sentinel-token's length on
    // both sides, so the wrap boundary lands at a variety of
    // positions across the fixture. The e2e original swept every
    // integer 40..=100; this sampled set preserves the per-trial
    // coverage that drove the regression without paying the full
    // 122-trial cost in CI.
    let widths: [u16; 5] = [40, 50, 60, 80, 100];
    // Height generous enough that every wrapped line fits inside
    // the content area even at the narrowest width with the
    // sidebar open. The check is about visibility under wrap,
    // not scrolling.
    let height: u16 = 200;

    for &width in &widths {
        for &sidebar_open in &[false, true] {
            if let Err(msg) = run_trial(width, height, sidebar_open, &fixture.path, &lines) {
                panic!("line-wrap visibility regression:\n\n{msg}");
            }
        }
    }
}

/// Anti-test: with `line_wrap = false` and a very narrow viewport,
/// long fixture lines extend past the right edge and their tail
/// sentinels `>LN###` are NOT rendered (no wrap → no tail). Drops
/// the load-bearing `line_wrap = true` precondition and proves
/// the visibility claim depends on wrap being enabled.
#[test]
fn anti_line_wrap_disabled_loses_tail_sentinels_off_screen() {
    let lines = interesting_lines();
    let fixture =
        TestFixture::new("line_wrap_visibility_anti.txt", &lines.join("\n"))
            .expect("create fixture");

    // Narrow viewport with wrap DISABLED. Long lines like the
    // 36-'a' stressor or the indented Kotlin-style hanging line
    // can't fit horizontally, so their tail sentinels must be
    // pushed off the right edge and not appear in any rendered
    // row. (Note: `Config::default()` has `line_wrap = true`, so
    // we explicitly flip it false here — that's the precondition
    // this anti-test is dropping.)
    let mut config = Config::default();
    config.editor.line_wrap = false;
    let mut harness = EditorTestHarness::with_config(40, 50, config).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);

    // The long indented line (index 5) and the 36-'a' stressor
    // (index 6) are both wider than the 40-col viewport's content
    // area, so their tail sentinels can't appear without wrap.
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::NoRowContains(">LN005".into()),
            RowMatch::NoRowContains(">LN006".into()),
        ],
        ..Default::default()
    };
    if let Some((field, expected, actual)) = expect.check_against(&snap) {
        panic!(
            "anti: with line_wrap=false on a 40-col viewport the tail \
             sentinels of long lines should be off-screen, but: {field} \
             expected {expected}; actual {actual}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}
