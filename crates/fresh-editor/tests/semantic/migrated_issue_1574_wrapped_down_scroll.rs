//! Faithful migration of `tests/e2e/issue_1574_wrapped_down_scroll.rs`.
//!
//! Issue #1574: "Weird scrolling behavior in a buffer with a lot of line
//! wrapping." This file mirrors the seven width-sweep regression tests
//! from the original e2e module, preserving:
//!
//!   * fixtures (`tests/fixtures/issue_1574_wrapped_lines.md` and
//!     `tests/fixtures/issue_1574_encodings.md`),
//!   * the load-bearing `line_wrap=true` config,
//!   * the per-width terminal sizes (the original sweeps a range of
//!     widths at two heights — preserved exactly so we exercise the
//!     same wrap-boundary positions),
//!   * the action sequences (arrow / Ctrl+Up / Ctrl+Down / Find walks),
//!   * the assertion shapes — both the rendered top content row text
//!     and the hardware cursor row are observed exclusively through
//!     `harness.get_screen_row`, `harness.content_area_rows`, and
//!     `harness.screen_cursor_position`. No `top_byte`,
//!     no `top_view_line_offset`, no cursor byte. The whole point
//!     of issue #1574's regression posture is that the bug is about
//!     rendered output, not viewport internals.
//!
//! Surfaces used (harness-direct, see
//! `docs/internal/e2e-test-migration-design.md` §2.1):
//!
//!   - `EditorTestHarness::with_config` (line_wrap=true Config)
//!   - `EditorTestHarness::open_file` (the markdown fixture)
//!   - `harness.send_key` for arrow / Find / Ctrl+Home / Ctrl+End
//!   - `harness.content_area_rows`, `harness.get_screen_row` for the
//!     rendered top-row snapshot and end-of-buffer marker probes
//!   - `harness.screen_cursor_position` for hardware cursor row
//!
//! Anti-test: `anti_issue_1574_wrapped_dropping_down_keeps_top_row_pinned`
//! drops every `Down` press from the positive Down-sweep flow and
//! confirms that without the load-bearing arrow walk the top row never
//! changes (so the positive test's "viewport eventually scrolls"
//! invariant would be vacuously satisfied at zero).
//!
//! The original e2e tests are large (1312 lines, 7 tests with width
//! sweeps). All seven scenarios migrate cleanly because they only ever
//! read rendered output — no observable is missing. Tests deferred: 0.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::path::{Path, PathBuf};

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

fn fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("issue_1574_wrapped_lines.md")
}

fn encodings_fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("issue_1574_encodings.md")
}

/// Snapshot the first visible *content* row (the row just below the tab bar).
/// When the viewport scrolls, the contents of this row change; when the
/// viewport stays put, they stay identical. Comparing this row across key
/// presses is a purely rendered-output way of detecting whether scrolling
/// happened.
fn top_content_row(harness: &EditorTestHarness) -> String {
    let (content_first_row, _) = harness.content_area_rows();
    harness.get_screen_row(content_first_row)
}

/// Return the full content area as one string (rows joined by '\n').
/// Used for richer diagnostics when an assertion fails.
fn content_area_snapshot(harness: &EditorTestHarness) -> String {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .map(|r| harness.get_screen_row(r))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Distinctive marker placed as the final line of the wrapped-lines
/// fixture. Once this line is visible anywhere in the content area, the
/// viewport has scrolled as far as it can.
const END_MARKER: &str = "End of the wrapped-buffer scroll fixture.";

/// Distinctive marker placed as the first line of the wrapped-lines
/// fixture. Mirror of `END_MARKER` for the Up-direction sweep.
const TOP_MARKER: &str = "# Wrapped Buffer Scroll Test";

fn reached_max_scroll(harness: &EditorTestHarness) -> bool {
    let (first, last) = harness.content_area_rows();
    (first..=last).any(|r| harness.get_screen_row(r).contains(END_MARKER))
}

fn reached_min_scroll(harness: &EditorTestHarness) -> bool {
    let (first, last) = harness.content_area_rows();
    (first..=last).any(|r| harness.get_screen_row(r).contains(TOP_MARKER))
}

// =====================================================================
// Down-arrow scrolling invariants (width sweep)
// =====================================================================

#[test]
fn migrated_issue_1574_down_arrow_scrolling_invariants_rendered() {
    // Original: `test_issue_1574_down_arrow_scrolling_invariants_rendered`.
    // Walks the cursor from the top of the fixture down to EOF using
    // only the Down arrow, asserting (1) no premature scroll while the
    // cursor is well above the bottom margin and (2) the walk
    // eventually reaches the end-of-file marker.

    // Default scroll_offset from Viewport::new.  This test observes only
    // rendered output, so it just needs to know the size of the zone that
    // should trigger scrolling.
    const SCROLL_OFFSET: usize = 3;
    const MAX_STEPS: usize = 500;

    let widths: [u16; 5] = [60, 70, 80, 90, 100];
    let heights: [u16; 2] = [20, 28];

    for &height in &heights {
        for &width in &widths {
            let mut harness =
                EditorTestHarness::with_config(width, height, config_with_wrap()).unwrap();
            harness.open_file(&fixture_path()).unwrap();
            harness.render().unwrap();

            let (_content_first_row, content_last_row) = harness.content_area_rows();
            let initial_top_row = top_content_row(&harness);
            assert!(
                initial_top_row
                    .chars()
                    .any(|c| !c.is_whitespace() && c != '│'),
                "[{width}x{height}] Expected fixture content to appear on the first content row, \
                 got: {initial_top_row:?}"
            );

            harness
                .send_key(KeyCode::Home, KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
            let top_row_at_start = top_content_row(&harness);

            let mut seen_scroll = false;
            let mut stalled_steps = 0usize;

            for step in 1..=MAX_STEPS {
                let top_before = top_content_row(&harness);
                let (_cx_before, cy_before) = harness.screen_cursor_position();

                harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();

                let top_after = top_content_row(&harness);
                let (_cx_after, cy_after) = harness.screen_cursor_position();

                let scrolled = top_before != top_after;
                let cursor_moved = cy_before != cy_after;

                let rows_from_bottom = (content_last_row as isize) - (cy_before as isize);
                let cursor_near_bottom = rows_from_bottom <= SCROLL_OFFSET as isize;

                // Invariant 1: no premature scrolling.
                if !cursor_near_bottom && cursor_moved {
                    assert!(
                        !scrolled,
                        "[{width}x{height}] Step #{step}: cursor was at screen row \
                         {cy_before} ({rows_from_bottom} rows from the bottom of the \
                         content area at row {content_last_row}), so pressing Down should \
                         NOT have scrolled the viewport. But the top content row \
                         changed.\n\
                         BEFORE top row: {top_before:?}\n\
                         AFTER  top row: {top_after:?}\n\
                         Content area after press:\n{snap}",
                        snap = content_area_snapshot(&harness),
                    );
                }

                if scrolled {
                    seen_scroll = true;
                    stalled_steps = 0;
                } else if !cursor_moved {
                    stalled_steps += 1;
                    if stalled_steps >= 2 {
                        break;
                    }
                } else {
                    stalled_steps = 0;
                }
            }

            assert!(
                seen_scroll,
                "[{width}x{height}] Test never observed a scroll — fixture may be too \
                 short for the terminal size, or scrolling is completely broken. \
                 Content after exhaustion:\n{}",
                content_area_snapshot(&harness),
            );
            let final_top_row = top_content_row(&harness);
            assert_ne!(
                top_row_at_start, final_top_row,
                "[{width}x{height}] After walking Down through the whole document, the \
                 top row should have advanced past the initial top row.  \
                 Start: {top_row_at_start:?} End: {final_top_row:?}",
            );

            assert!(
                reached_max_scroll(&harness),
                "[{width}x{height}] After walking Down through the whole document, the \
                 end-of-file marker should be visible.  Content:\n{}",
                content_area_snapshot(&harness),
            );
            let (_cx_final, cy_final) = harness.screen_cursor_position();
            assert!(
                (cy_final as usize) <= content_last_row,
                "[{width}x{height}] Final cursor row {cy_final} exceeds content_last_row \
                 {content_last_row}",
            );
        }
    }
}

#[test]
fn migrated_issue_1574_up_arrow_scrolling_invariants_rendered() {
    // Original: `test_issue_1574_up_arrow_scrolling_invariants_rendered`.
    // Mirror of the Down sweep — walks from EOF back to BOF.
    const SCROLL_OFFSET: usize = 3;
    const MAX_STEPS: usize = 500;

    let widths: [u16; 5] = [60, 70, 80, 90, 100];
    let heights: [u16; 2] = [20, 28];

    for &height in &heights {
        for &width in &widths {
            let mut harness =
                EditorTestHarness::with_config(width, height, config_with_wrap()).unwrap();
            harness.open_file(&fixture_path()).unwrap();
            harness.render().unwrap();

            let (content_first_row, _content_last_row) = harness.content_area_rows();
            let initial_top_row = top_content_row(&harness);
            assert!(
                initial_top_row
                    .chars()
                    .any(|c| !c.is_whitespace() && c != '│'),
                "[{width}x{height}] Expected fixture content to appear on the first content \
                 row, got: {initial_top_row:?}"
            );

            harness
                .send_key(KeyCode::End, KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
            let top_row_at_start = top_content_row(&harness);

            let mut seen_scroll = false;
            let mut stalled_steps = 0usize;

            for step in 1..=MAX_STEPS {
                let top_before = top_content_row(&harness);
                let (_cx_before, cy_before) = harness.screen_cursor_position();

                harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();

                let top_after = top_content_row(&harness);
                let (_cx_after, cy_after) = harness.screen_cursor_position();

                let scrolled = top_before != top_after;
                let cursor_moved = cy_before != cy_after;

                let rows_from_top = (cy_before as isize) - (content_first_row as isize);
                let cursor_near_top = rows_from_top <= SCROLL_OFFSET as isize;

                if !cursor_near_top && cursor_moved {
                    assert!(
                        !scrolled,
                        "[{width}x{height}] Step #{step}: cursor was at screen row \
                         {cy_before} ({rows_from_top} rows from the top of the content \
                         area at row {content_first_row}), so pressing Up should \
                         NOT have scrolled the viewport. But the top content row \
                         changed.\n\
                         BEFORE top row: {top_before:?}\n\
                         AFTER  top row: {top_after:?}\n\
                         Content area after press:\n{snap}",
                        snap = content_area_snapshot(&harness),
                    );
                }

                if scrolled {
                    seen_scroll = true;
                    stalled_steps = 0;
                } else if seen_scroll && !reached_min_scroll(&harness) {
                    if !cursor_moved {
                        stalled_steps += 1;
                        if stalled_steps >= 2 {
                            break;
                        }
                    } else {
                        panic!(
                            "[{width}x{height}] Step #{step}: the viewport scrolled on a \
                             previous Up press (cursor had entered the top margin), so \
                             every subsequent Up press must also scroll until the \
                             first-line marker is visible.  But this press moved the \
                             cursor from row {cy_before} to row {cy_after} without \
                             changing the top content row, and the first-line marker \
                             ({TOP_MARKER:?}) is not yet visible.\n\
                             top row (unchanged): {top_before:?}\n\
                             Content area:\n{snap}",
                            snap = content_area_snapshot(&harness),
                        );
                    }
                } else if !cursor_moved {
                    stalled_steps += 1;
                    if stalled_steps >= 2 {
                        break;
                    }
                } else {
                    stalled_steps = 0;
                }
            }

            assert!(
                seen_scroll,
                "[{width}x{height}] Test never observed a scroll on Up — fixture may \
                 be too short for the terminal size, or scroll-up is broken. \
                 Content after exhaustion:\n{}",
                content_area_snapshot(&harness),
            );
            let final_top_row = top_content_row(&harness);
            assert_ne!(
                top_row_at_start, final_top_row,
                "[{width}x{height}] After walking Up through the whole document, the top \
                 row should have moved from its start-of-walk value.  \
                 Start: {top_row_at_start:?} End: {final_top_row:?}",
            );
            assert!(
                reached_min_scroll(&harness),
                "[{width}x{height}] After walking Up through the whole document, the \
                 first-line marker should be visible.  Content:\n{}",
                content_area_snapshot(&harness),
            );
            let (_cx_final, cy_final) = harness.screen_cursor_position();
            assert!(
                (cy_final as usize) >= content_first_row,
                "[{width}x{height}] Final cursor row {cy_final} is above \
                 content_first_row {content_first_row}",
            );
        }
    }
}

// =====================================================================
// Empty-line-at-bottom Down-jump variant (and Up mirror)
// =====================================================================

const END_OF_PARA1: &str = "data as UTF-8.";
const START_OF_PARA1: &str = "Text files come in";
const START_OF_PARA2: &str = "Due to the fact";
const MIDDLE_OF_PARA2: &str = "resets the encoder state";
const FIRST_LINE_MARKER: &str = "Padding line 01";

enum ScenarioOutcome {
    Ok,
    SetupSkipped(String),
    JumpReproduced(String),
    UnexpectedRow(String),
}

fn run_jump_scenario_at_width(width: u16, height: u16) -> ScenarioOutcome {
    run_jump_scenario_at_width_with_fixture(width, height, &encodings_fixture_path())
}

fn run_jump_scenario_at_width_with_fixture(
    width: u16,
    height: u16,
    fixture_path: &Path,
) -> ScenarioOutcome {
    let mut harness = match EditorTestHarness::with_config(width, height, config_with_wrap()) {
        Ok(h) => h,
        Err(e) => return ScenarioOutcome::SetupSkipped(format!("harness init failed: {e}")),
    };
    if harness.open_file(fixture_path).is_err() {
        return ScenarioOutcome::SetupSkipped("open_file failed".into());
    }
    if harness.render().is_err() {
        return ScenarioOutcome::SetupSkipped("initial render failed".into());
    }

    let (content_first_row, content_last_row) = harness.content_area_rows();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(END_OF_PARA1).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (_cx_empty, cy_empty) = harness.screen_cursor_position();
    let empty_row = harness.get_screen_row(cy_empty as usize);
    let empty_body: String = empty_row
        .split('│')
        .nth(1)
        .unwrap_or("")
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    if !empty_body.is_empty() {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: cursor row after Down-to-empty is not \
             empty: {empty_row:?}"
        ));
    }

    const SETUP_STEP_LIMIT: usize = 100;
    let mut step_count = 0usize;
    let ok = loop {
        let (_, cy) = harness.screen_cursor_position();
        let cursor_row_text = harness.get_screen_row(cy as usize);
        let cursor_row_is_empty: bool = cursor_row_text
            .split('│')
            .nth(1)
            .unwrap_or("")
            .chars()
            .all(char::is_whitespace);
        let cursor_at_bottom = (cy as usize) >= content_last_row;
        let para2_hidden = !(content_first_row..=content_last_row)
            .any(|r| harness.get_screen_row(r).contains(START_OF_PARA2));
        if cursor_at_bottom && para2_hidden {
            break true;
        }
        if !cursor_row_is_empty {
            break false;
        }
        let top_before = top_content_row(&harness);
        harness
            .send_key(KeyCode::Up, KeyModifiers::CONTROL)
            .unwrap();
        let top_after = top_content_row(&harness);
        step_count += 1;
        if top_before == top_after || step_count >= SETUP_STEP_LIMIT {
            break false;
        }
    };
    if !ok {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: could not park cursor at bottom with \
             paragraph two hidden.\nContent:\n{}",
            content_area_snapshot(&harness)
        ));
    }

    let (_, cy_before) = harness.screen_cursor_position();
    let before_row_body: String = harness
        .get_screen_row(cy_before as usize)
        .split('│')
        .nth(1)
        .unwrap_or("")
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    if !before_row_body.is_empty() || (cy_before as usize) != content_last_row {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: cursor shifted off the empty separator \
             row during Ctrl+Up loop.  cy={cy_before}, content_last_row={content_last_row}"
        ));
    }
    let start_visible_before = (content_first_row..=content_last_row)
        .any(|r| harness.get_screen_row(r).contains(START_OF_PARA2));
    if start_visible_before {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: paragraph two became visible before the \
             critical Down press"
        ));
    }
    let top_row_before = top_content_row(&harness);

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (_cx_after, cy_after) = harness.screen_cursor_position();
    let row_after = harness.get_screen_row(cy_after as usize);

    if row_after.contains(MIDDLE_OF_PARA2) {
        return ScenarioOutcome::JumpReproduced(format!(
            "width={width}, height={height}: Bug #1574 (jump variant) reproduced — \
             Down from empty line at bottom of viewport landed on row containing \
             {MIDDLE_OF_PARA2:?} instead of {START_OF_PARA2:?}.\n\
             top row before Down: {top_row_before:?}\n\
             Cursor row after Down: {row_after:?}\n\
             Full content:\n{snap}",
            snap = content_area_snapshot(&harness),
        ));
    }

    if !row_after.contains(START_OF_PARA2) {
        return ScenarioOutcome::UnexpectedRow(format!(
            "width={width}, height={height}: cursor did not land on first visual row \
             of paragraph two (looking for {START_OF_PARA2:?}).\n\
             Cursor row after Down: {row_after:?}\n\
             Full content:\n{snap}",
            snap = content_area_snapshot(&harness),
        ));
    }

    ScenarioOutcome::Ok
}

fn run_up_jump_scenario_at_width(width: u16, height: u16) -> ScenarioOutcome {
    run_up_jump_scenario_at_width_with_fixture(width, height, &encodings_fixture_path())
}

fn run_up_jump_scenario_at_width_with_fixture(
    width: u16,
    height: u16,
    fixture_path: &Path,
) -> ScenarioOutcome {
    let mut harness = match EditorTestHarness::with_config(width, height, config_with_wrap()) {
        Ok(h) => h,
        Err(e) => return ScenarioOutcome::SetupSkipped(format!("harness init failed: {e}")),
    };
    if harness.open_file(fixture_path).is_err() {
        return ScenarioOutcome::SetupSkipped("open_file failed".into());
    }
    if harness.render().is_err() {
        return ScenarioOutcome::SetupSkipped("initial render failed".into());
    }

    let (content_first_row, _content_last_row) = harness.content_area_rows();

    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(END_OF_PARA1).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (_cx_empty, cy_empty) = harness.screen_cursor_position();
    let empty_row = harness.get_screen_row(cy_empty as usize);
    let empty_body: String = empty_row
        .split('│')
        .nth(1)
        .unwrap_or("")
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    if !empty_body.is_empty() {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: cursor row after Down-to-empty is not \
             empty: {empty_row:?}"
        ));
    }

    const SETUP_STEP_LIMIT: usize = 100;
    let mut step_count = 0usize;
    let ok = loop {
        let (_, cy) = harness.screen_cursor_position();
        let cursor_row_text = harness.get_screen_row(cy as usize);
        let cursor_row_is_empty: bool = cursor_row_text
            .split('│')
            .nth(1)
            .unwrap_or("")
            .chars()
            .all(char::is_whitespace);
        let cursor_at_top = (cy as usize) <= content_first_row;
        let para1_hidden = !(content_first_row..=_content_last_row)
            .any(|r| harness.get_screen_row(r).contains(END_OF_PARA1));
        if cursor_at_top && para1_hidden {
            break true;
        }
        if !cursor_row_is_empty {
            break false;
        }

        let top_before = top_content_row(&harness);
        harness
            .send_key(KeyCode::Down, KeyModifiers::CONTROL)
            .unwrap();
        let top_after = top_content_row(&harness);
        step_count += 1;
        if top_before == top_after || step_count >= SETUP_STEP_LIMIT {
            break false;
        }
    };
    if !ok {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: could not park cursor at top with \
             paragraph one hidden (viewport hit bottom of buffer first).\nContent:\n{}",
            content_area_snapshot(&harness)
        ));
    }

    let (_, cy_before) = harness.screen_cursor_position();
    let before_row_body: String = harness
        .get_screen_row(cy_before as usize)
        .split('│')
        .nth(1)
        .unwrap_or("")
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect();
    if !before_row_body.is_empty() || (cy_before as usize) != content_first_row {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: cursor shifted off the empty separator \
             row during Ctrl+Down loop.  cy={cy_before}, content_first_row={content_first_row}"
        ));
    }
    let end_visible_before = (content_first_row..=_content_last_row)
        .any(|r| harness.get_screen_row(r).contains(END_OF_PARA1));
    if end_visible_before {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: paragraph one's end became visible before \
             the critical Up press"
        ));
    }

    harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (_cx_after, cy_after) = harness.screen_cursor_position();
    let row_after = harness.get_screen_row(cy_after as usize);

    if row_after.contains(START_OF_PARA1) {
        return ScenarioOutcome::JumpReproduced(format!(
            "width={width}, height={height}: Bug #1574 (Up jump variant) reproduced — \
             Up from empty line at top of viewport landed on row containing \
             {START_OF_PARA1:?} (the START of paragraph one) instead of {END_OF_PARA1:?} \
             (the END of paragraph one).\n\
             Cursor row after Up: {row_after:?}\n\
             Full content:\n{snap}",
            snap = content_area_snapshot(&harness),
        ));
    }

    if !row_after.contains(END_OF_PARA1) {
        return ScenarioOutcome::UnexpectedRow(format!(
            "width={width}, height={height}: cursor did not land on last visual row \
             of paragraph one (looking for {END_OF_PARA1:?}).\n\
             Cursor row after Up: {row_after:?}\n\
             Full content:\n{snap}",
            snap = content_area_snapshot(&harness),
        ));
    }

    ScenarioOutcome::Ok
}

fn drive_width_sweep(
    label: &'static str,
    widths: &[u16],
    heights: &[u16],
    scenario: impl Fn(u16, u16) -> ScenarioOutcome,
) {
    let mut successful: Vec<(u16, u16)> = Vec::new();
    let mut skipped: Vec<String> = Vec::new();
    let mut jump_failures: Vec<String> = Vec::new();
    let mut unexpected_row_failures: Vec<String> = Vec::new();

    for &height in heights {
        for &width in widths {
            match scenario(width, height) {
                ScenarioOutcome::Ok => successful.push((width, height)),
                ScenarioOutcome::SetupSkipped(msg) => skipped.push(msg),
                ScenarioOutcome::JumpReproduced(msg) => jump_failures.push(msg),
                ScenarioOutcome::UnexpectedRow(msg) => unexpected_row_failures.push(msg),
            }
        }
    }

    assert!(
        jump_failures.is_empty(),
        "[{label}] Bug #1574 (jump variant) reproduced at {n} terminal size(s):\n\n{joined}",
        n = jump_failures.len(),
        joined = jump_failures.join("\n\n---\n\n"),
    );

    assert!(
        unexpected_row_failures.is_empty(),
        "[{label}] At {n} width(s), the cursor landed on neither the \
         expected anchor nor a known-bad row:\n\n{joined}",
        n = unexpected_row_failures.len(),
        joined = unexpected_row_failures.join("\n\n---\n\n"),
    );

    assert!(
        !successful.is_empty(),
        "[{label}] No terminal size in the sweep reached the precondition \
         for this test — every width was skipped.  The fixture or layout \
         math may have drifted.  Skipped reasons (first 5):\n{}",
        skipped
            .iter()
            .take(5)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n---\n"),
    );
}

#[test]
fn migrated_issue_1574_down_from_empty_line_at_bottom_lands_on_paragraph_start() {
    // Original: `test_issue_1574_down_from_empty_line_at_bottom_lands_on_paragraph_start`.
    // Sweep a dense range of widths × two heights — the original
    // jump-variant invocation.
    let widths: Vec<u16> = (30u16..=120).step_by(3).collect();
    let heights: [u16; 2] = [15, 20];
    drive_width_sweep("down-jump", &widths, &heights, run_jump_scenario_at_width);
}

#[test]
fn migrated_issue_1574_up_from_empty_line_at_top_lands_on_paragraph_end() {
    // Original: `test_issue_1574_up_from_empty_line_at_top_lands_on_paragraph_end`.
    let widths: Vec<u16> = (30u16..=120).step_by(3).collect();
    let heights: [u16; 2] = [15, 20];
    drive_width_sweep("up-jump", &widths, &heights, run_up_jump_scenario_at_width);
}

// =====================================================================
// CRLF cursor-math regression guard (Down + Up directions)
// =====================================================================

#[test]
fn migrated_issue_1574_crlf_fixture_down_jump_lands_on_paragraph_start() {
    // Original: `test_issue_1574_crlf_fixture_down_jump_lands_on_paragraph_start`.
    // Writes a CRLF copy of the encodings fixture and verifies the
    // Down-jump scenario passes on it at the widths that Windows CI
    // showed failing before the fix.

    let original = std::fs::read_to_string(encodings_fixture_path())
        .expect("failed to read encodings fixture");
    let crlf: String = original.replace("\r\n", "\n").replace('\n', "\r\n");
    let dir = tempfile::TempDir::new().expect("tempdir");
    let crlf_path = dir.path().join("issue_1574_encodings_crlf.md");
    std::fs::write(&crlf_path, crlf.as_bytes()).expect("write crlf fixture");

    let written = std::fs::read(&crlf_path).expect("reread tempfile");
    let crlf_count = written.windows(2).filter(|w| w == b"\r\n").count();
    let bare_lf_count = written
        .iter()
        .enumerate()
        .filter(|(i, &b)| b == b'\n' && (*i == 0 || written[i - 1] != b'\r'))
        .count();
    let crcr_count = written.windows(2).filter(|w| w == b"\r\r").count();
    assert!(
        crlf_count > 0,
        "CRLF fixture has no \\r\\n sequences; test setup is broken"
    );
    assert_eq!(
        bare_lf_count, 0,
        "CRLF fixture has bare \\n not preceded by \\r; test setup is broken"
    );
    assert_eq!(
        crcr_count, 0,
        "CRLF fixture has `\\r\\r` sequences; LF normalization failed"
    );

    let widths_seen_failing: [u16; 8] = [33, 36, 42, 45, 48, 51, 60, 90];
    let mut failures: Vec<String> = Vec::new();
    let mut skipped: Vec<String> = Vec::new();
    let mut passed: Vec<u16> = Vec::new();
    for &width in &widths_seen_failing {
        match run_jump_scenario_at_width_with_fixture(width, 20, &crlf_path) {
            ScenarioOutcome::Ok => passed.push(width),
            ScenarioOutcome::SetupSkipped(msg) => skipped.push(format!("w={width}: {msg}")),
            ScenarioOutcome::JumpReproduced(msg) => {
                failures.push(format!("w={width} (JumpReproduced): {msg}"))
            }
            ScenarioOutcome::UnexpectedRow(msg) => {
                failures.push(format!("w={width} (UnexpectedRow): {msg}"))
            }
        }
    }

    assert!(
        failures.is_empty(),
        "CRLF cursor-math regression: {} width(s) failed the Down-jump \
         scenario on a CRLF-encoded fixture. The cursor-move fallback \
         must step past CRLF as a two-byte unit (same way \
         `build_base_tokens` does). Failures:\n{}",
        failures.len(),
        failures.join("\n---\n"),
    );
    assert!(
        !passed.is_empty(),
        "CRLF regression guard: every width was skipped during setup — \
         the test is not actually exercising the bug path. Skipped \
         reasons:\n{}",
        skipped.join("\n")
    );
}

#[test]
fn migrated_issue_1574_crlf_fixture_up_jump_lands_on_paragraph_end() {
    // Original: `test_issue_1574_crlf_fixture_up_jump_lands_on_paragraph_end`.

    let original = std::fs::read_to_string(encodings_fixture_path())
        .expect("failed to read encodings fixture");
    let crlf: String = original.replace("\r\n", "\n").replace('\n', "\r\n");
    let dir = tempfile::TempDir::new().expect("tempdir");
    let crlf_path = dir.path().join("issue_1574_encodings_crlf.md");
    std::fs::write(&crlf_path, crlf.as_bytes()).expect("write crlf fixture");
    let written = std::fs::read(&crlf_path).expect("reread tempfile");
    assert_eq!(
        written.windows(2).filter(|w| w == b"\r\r").count(),
        0,
        "CRLF fixture has `\\r\\r` sequences; LF normalization failed"
    );

    let widths_seen_failing: [u16; 8] = [33, 36, 42, 45, 48, 51, 60, 90];
    let mut failures: Vec<String> = Vec::new();
    let mut skipped: Vec<String> = Vec::new();
    let mut passed: Vec<u16> = Vec::new();
    for &width in &widths_seen_failing {
        match run_up_jump_scenario_at_width_with_fixture(width, 20, &crlf_path) {
            ScenarioOutcome::Ok => passed.push(width),
            ScenarioOutcome::SetupSkipped(msg) => skipped.push(format!("w={width}: {msg}")),
            ScenarioOutcome::JumpReproduced(msg) => {
                failures.push(format!("w={width} (JumpReproduced): {msg}"))
            }
            ScenarioOutcome::UnexpectedRow(msg) => {
                failures.push(format!("w={width} (UnexpectedRow): {msg}"))
            }
        }
    }

    assert!(
        failures.is_empty(),
        "CRLF cursor-math regression (Up direction): {} width(s) failed. \
         Failures:\n{}",
        failures.len(),
        failures.join("\n---\n"),
    );
    assert!(
        !passed.is_empty(),
        "CRLF Up regression guard: every width was skipped. Skipped \
         reasons:\n{}",
        skipped.join("\n")
    );
}

// =====================================================================
// Ctrl+Up / Ctrl+Down scroll round-trip sweep
// =====================================================================

fn run_ctrl_up_down_roundtrip_scenario_at_width(width: u16, height: u16) -> ScenarioOutcome {
    let mut harness = match EditorTestHarness::with_config(width, height, config_with_wrap()) {
        Ok(h) => h,
        Err(e) => return ScenarioOutcome::SetupSkipped(format!("harness init failed: {e}")),
    };
    if harness.open_file(&encodings_fixture_path()).is_err() {
        return ScenarioOutcome::SetupSkipped("open_file failed".into());
    }
    if harness.render().is_err() {
        return ScenarioOutcome::SetupSkipped("initial render failed".into());
    }

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let is_viewport_at_top = |h: &EditorTestHarness| -> bool {
        let (first, last) = h.content_area_rows();
        (first..=last).any(|r| h.get_screen_row(r).contains(FIRST_LINE_MARKER))
    };

    const MAX_STEPS: usize = 30;

    let mut steps_exercised_scroll = 0usize;
    let mut steps_exercised_at_top = 0usize;
    let mut prev_cursor_y: Option<u16> = None;

    for step in 1..=MAX_STEPS {
        let at_top = is_viewport_at_top(&harness);
        let top_before = top_content_row(&harness);

        harness
            .send_key(KeyCode::Up, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        let top_after_up = top_content_row(&harness);

        if at_top {
            steps_exercised_at_top += 1;
            if top_after_up != top_before {
                return ScenarioOutcome::UnexpectedRow(format!(
                    "width={width}, height={height}, step={step}: viewport was at \
                     top of buffer ({FIRST_LINE_MARKER:?} visible) but Ctrl+Up \
                     still changed the top content row.\n\
                     BEFORE: {top_before:?}\n\
                     AFTER : {top_after_up:?}\n\
                     Content:\n{}",
                    content_area_snapshot(&harness),
                ));
            }
        } else {
            steps_exercised_scroll += 1;
            if top_after_up == top_before {
                return ScenarioOutcome::UnexpectedRow(format!(
                    "width={width}, height={height}, step={step}: viewport was NOT \
                     at top of buffer ({FIRST_LINE_MARKER:?} not visible) but \
                     Ctrl+Up did not scroll — top content row unchanged.\n\
                     top row (unchanged): {top_before:?}\n\
                     Content:\n{}",
                    content_area_snapshot(&harness),
                ));
            }
        }

        let ctrl_up_scrolled = top_after_up != top_before;
        harness
            .send_key(KeyCode::Down, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        let top_after_down = top_content_row(&harness);

        if ctrl_up_scrolled && top_after_down != top_before {
            return ScenarioOutcome::UnexpectedRow(format!(
                "width={width}, height={height}, step={step}: Ctrl+Down after \
                 Ctrl+Up did not restore the original top content row.  \
                 Scroll actions should be exact round-trips at a one-row \
                 granularity.\n\
                 BEFORE Ctrl+Up : {top_before:?}\n\
                 AFTER  Ctrl+Up : {top_after_up:?}\n\
                 AFTER  Ctrl+Dn : {top_after_down:?}\n\
                 viewport_was_at_top_of_buffer: {at_top}\n\
                 Content:\n{}",
                content_area_snapshot(&harness),
            ));
        }

        let (_cx_pre, cy_pre) = harness.screen_cursor_position();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let (_cx_post, cy_post) = harness.screen_cursor_position();
        let top_after_walk = top_content_row(&harness);

        let nothing_moved =
            cy_pre == cy_post && top_after_walk == top_before && prev_cursor_y == Some(cy_post);
        if nothing_moved {
            break;
        }
        prev_cursor_y = Some(cy_post);
    }

    if steps_exercised_scroll == 0 || steps_exercised_at_top == 0 {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: walk did not exercise both the \
             top-of-buffer and the scrolled-off-top regimes \
             (scroll-hits={steps_exercised_scroll}, top-hits={steps_exercised_at_top})"
        ));
    }

    ScenarioOutcome::Ok
}

#[test]
fn migrated_issue_1574_ctrl_up_down_scroll_roundtrip_sweep() {
    // Original: `test_issue_1574_ctrl_up_down_scroll_roundtrip_sweep`.
    // Sparser width grid than the jump-variant sweep because this
    // scenario is heavier per width (runs Ctrl+Up/Ctrl+Down/Down at
    // every step).
    let widths: Vec<u16> = (30u16..=120).step_by(10).collect();
    let heights: [u16; 1] = [15];
    drive_width_sweep(
        "ctrl-up-down-roundtrip",
        &widths,
        &heights,
        run_ctrl_up_down_roundtrip_scenario_at_width,
    );
}

// =====================================================================
// Anti-test
// =====================================================================

/// Anti-test: drop every `Down` press from the positive Down-sweep
/// flow. Without the arrow walk, the top content row never changes —
/// proving that the positive test's "viewport eventually scrolls"
/// invariant is load-bearing on the `Down` action sequence, not on
/// the fixture/config setup alone.
#[test]
fn anti_issue_1574_wrapped_dropping_down_keeps_top_row_pinned() {
    let mut harness = EditorTestHarness::with_config(80, 20, config_with_wrap()).unwrap();
    harness.open_file(&fixture_path()).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let top_initial = top_content_row(&harness);

    // No Down presses — the dropped load-bearing action.
    // The positive Down-sweep test relies on the arrow walk to move
    // the viewport. With no Downs, the top row must stay pinned.
    for _ in 0..50 {
        // Pump a render-only loop to prove no spontaneous scrolling.
        harness.render().unwrap();
    }

    let top_after = top_content_row(&harness);
    assert_eq!(
        top_initial, top_after,
        "anti: without the Down arrow walk, the top content row must NOT \
         change.  Initial: {top_initial:?}  After idle renders: {top_after:?}.  \
         This pins the positive test's reliance on the arrow walk."
    );
    assert!(
        !reached_max_scroll(&harness),
        "anti: without Down presses, the end-of-file marker {END_MARKER:?} \
         must NOT be visible (the positive test's 'eventually reaches end' \
         claim depends entirely on the arrow walk).  Content:\n{}",
        content_area_snapshot(&harness),
    );
}
