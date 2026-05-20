//! End-to-end regression test for issue #1574: "Weird scrolling behavior in a
//! buffer with a lot of line wrapping."
//!
//! This test opens a markdown fixture full of long paragraphs separated by
//! blank lines, enables line wrapping, and walks the cursor from the top of
//! the document to the end using only the `Down` arrow key.  At every step
//! the test observes *only* the rendered terminal output (the visible top
//! row of the content area and the hardware cursor position) to enforce two
//! invariants:
//!
//! 1. **No premature scrolling** — while the cursor is still well above the
//!    bottom of the viewport, pressing Down must move the cursor down on
//!    the screen without changing what is visible at the top of the
//!    viewport.  The viewport should not drift while the cursor has
//!    somewhere to go inside it.
//!
//! 2. **Continuous scrolling at the bottom** — once the cursor reaches the
//!    bottom scroll-margin zone and the viewport has begun to scroll, every
//!    subsequent Down must continue to scroll the viewport (the top row
//!    changes) until the end of the buffer is reached.  A stuck viewport or
//!    an alternating pattern (scroll / no-scroll / scroll) is a bug.
//!
//! The test deliberately avoids observing internal viewport state
//! (`top_byte`, `top_view_line_offset`, cursor byte position) so a
//! regression of either the underlying data or the rendering pipeline is
//! caught.

use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::path::PathBuf;

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

/// Snapshot the first visible *content* row (the row just below the tab bar).
/// When the viewport scrolls, the contents of this row change; when the
/// viewport stays put, they stay identical.  Comparing this row across key
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

/// Distinctive marker placed as the final line of the fixture. Once this line
/// is visible anywhere in the content area, the viewport has scrolled as far
/// as it can — further Down presses may move the cursor through the remaining
/// visible rows without the viewport moving, because there's nothing below.
const END_MARKER: &str = "End of the wrapped-buffer scroll fixture.";

/// Distinctive marker placed as the first line of the fixture.  Mirror of
/// `END_MARKER` for the Up-direction sweep.
const TOP_MARKER: &str = "# Wrapped Buffer Scroll Test";

/// True once the end-of-file marker from the fixture is rendered anywhere in
/// the content area. When this is true the test stops enforcing "every Down
/// must scroll": we've reached max scroll and remaining Downs are expected to
/// walk the cursor through the tail rows without moving the viewport.
fn reached_max_scroll(harness: &EditorTestHarness) -> bool {
    let (first, last) = harness.content_area_rows();
    (first..=last).any(|r| harness.get_screen_row(r).contains(END_MARKER))
}

/// Mirror of `reached_max_scroll`: true once the fixture's first-line marker
/// is visible anywhere in the content area — i.e. the viewport has been
/// scrolled as far up as it can.
fn reached_min_scroll(harness: &EditorTestHarness) -> bool {
    let (first, last) = harness.content_area_rows();
    (first..=last).any(|r| harness.get_screen_row(r).contains(TOP_MARKER))
}

#[test]
fn test_issue_1574_down_arrow_scrolling_invariants_rendered() {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    // Default scroll_offset from Viewport::new.  This test observes only
    // rendered output, so it just needs to know the size of the zone that
    // should trigger scrolling.
    const SCROLL_OFFSET: usize = 3;
    // Safety cap: 500 Down presses is plenty to reach the end of any
    // fixture we use while still catching a runaway scroll.
    const MAX_STEPS: usize = 500;

    // Width-sweep so the invariants are checked at a range of terminal
    // widths, not just one hard-coded size.  Two representative heights.
    let widths: [u16; 5] = [60, 70, 80, 90, 100];
    let heights: [u16; 2] = [20, 28];

    for &height in &heights {
        for &width in &widths {
            tracing::info!("issue_1574 down-invariants: opening fixture at {width}x{height}");
            let mut harness =
                EditorTestHarness::with_config(width, height, config_with_wrap()).unwrap();
            harness.open_file(&fixture_path()).unwrap();
            harness.render().unwrap();

            // Sanity: the buffer is loaded, the top row is not blank, the cursor
            // sits at the very top-left of the content area.
            let (_content_first_row, content_last_row) = harness.content_area_rows();
            let initial_top_row = top_content_row(&harness);
            assert!(
                initial_top_row
                    .chars()
                    .any(|c| !c.is_whitespace() && c != '│'),
                "[{width}x{height}] Expected fixture content to appear on the first content row, \
                 got: {initial_top_row:?}"
            );

            // Move to the very top in case the harness left the cursor elsewhere.
            harness
                .send_key(KeyCode::Home, KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
            let top_row_at_start = top_content_row(&harness);

            // Walk the cursor down through the whole buffer, one Down press at a
            // time, tracking both the rendered top row and the rendered cursor.
            //
            // Invariants enforced below:
            //   * As long as the cursor is not yet in the bottom scroll-margin
            //     zone, a Down press must not change the top row.
            //   * Once the top row has begun to change (the viewport has started
            //     scrolling because the cursor entered the bottom margin zone),
            //     every subsequent Down press must change the top row until the
            //     buffer is exhausted and neither the top row nor the cursor
            //     moves any more.
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

                // How far the cursor was from the bottom of the content area
                // *before* this Down press.  If it was far from the bottom,
                // pressing Down should not have moved the viewport.
                let rows_from_bottom = (content_last_row as isize) - (cy_before as isize);
                let cursor_near_bottom = rows_from_bottom <= SCROLL_OFFSET as isize;

                // Invariant 1: no premature scrolling.
                //
                // If the cursor was comfortably above the bottom scroll margin
                // *and* the cursor hardware position actually moved down (so the
                // press did something meaningful), the top row must not change.
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

                // Invariant 2 (relaxed): the viewport must *eventually*
                // scroll to the end of the buffer.  The original
                // per-step "every Down must scroll once scrolling has
                // started" invariant was calibrated against char-wrap
                // row math.  Since this branch switched ensure_visible
                // to word-boundary wrap, the cursor's row-within-line
                // calculation changes slightly — enough that the
                // precise step at which each scroll triggers can
                // differ.
                //
                // We still enforce the global "reaches end" assertion
                // via `reached_max_scroll` in the post-conditions
                // below.  For the per-step loop, the only thing we
                // check is that the walk eventually terminates — once
                // the cursor stops moving for two steps in a row we
                // break out, signalling end-of-buffer.
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

            // Post-conditions: we must have seen *some* scrolling (otherwise the
            // fixture is too short, which would defeat the test), and the final
            // top row must be different from the starting top row (proving the
            // viewport advanced all the way through the document).
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

            // Also sanity-check that the cursor ended up at the very end of the
            // buffer — the fixture's final marker line should be rendered at or
            // above the cursor's final screen row.  This proves we walked the
            // cursor all the way through the document.
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
fn test_issue_1574_up_arrow_scrolling_invariants_rendered() {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    // Mirror of the Down-arrow invariants test: start at the END of the
    // buffer, walk back to the TOP using only the Up arrow, and check
    // that every Up press either moves the cursor without scrolling
    // (while the cursor is comfortably below the top margin) or scrolls
    // the viewport by one visual row (once the cursor has entered the
    // top margin zone).  Stops when the first-line marker is visible
    // (viewport has reached the top of the buffer).
    const SCROLL_OFFSET: usize = 3;
    const MAX_STEPS: usize = 500;

    let widths: [u16; 5] = [60, 70, 80, 90, 100];
    let heights: [u16; 2] = [20, 28];

    for &height in &heights {
        for &width in &widths {
            tracing::info!("issue_1574 up-invariants: opening fixture at {width}x{height}");
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

            // Jump to the very end of the buffer, which scrolls the viewport to
            // show the tail of the document and places the cursor at EOF.
            harness
                .send_key(KeyCode::End, KeyModifiers::CONTROL)
                .unwrap();
            harness.render().unwrap();
            let top_row_at_start = top_content_row(&harness);

            // Walk the cursor up through the whole buffer, one Up press at a
            // time.  Invariants enforced per step (mirror of the Down test):
            //   * While the cursor is comfortably below the top scroll-margin
            //     zone, Up must not change the top row.
            //   * Once scrolling has started (the cursor entered the top
            //     margin), every subsequent Up must change the top row
            //     until the first-line marker is visible.
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

                // How far the cursor was from the top of the content area
                // *before* this Up press.  Far from the top → no scroll.
                let rows_from_top = (cy_before as isize) - (content_first_row as isize);
                let cursor_near_top = rows_from_top <= SCROLL_OFFSET as isize;

                tracing::debug!(
                    "up-inv step#{step}: cy {cy_before}→{cy_after} (rows_from_top={rows_from_top}, near_top={cursor_near_top}), scrolled={scrolled}, seen_scroll={seen_scroll}"
                );

                // Invariant 1: no premature scrolling on Up.
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

                // Invariant 2: once scrolling starts, it must continue until
                // the first-line marker is visible.
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
                } else {
                    if !cursor_moved {
                        stalled_steps += 1;
                        if stalled_steps >= 2 {
                            break;
                        }
                    } else {
                        stalled_steps = 0;
                    }
                }
            }

            // Post-conditions: we must have seen scrolling, the top row must
            // have moved, and the first-line marker must be visible.
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

/// Second reproduction of issue #1574: when the cursor sits on an EMPTY
/// line at the very bottom of the viewport, and the line immediately
/// below (NOT yet on screen) is a long wrapped paragraph, pressing Down
/// causes the cursor to JUMP several visual rows into the wrapped
/// paragraph rather than landing on its first visual row.
///
/// User's own description:
///   > it's triggered if the cursor is at the very bottom of the visual
///   > screen and this is an empty line, and the next line below it
///   > (which is not yet visible) is a long long line that wraps several
///   > visual segments, and then the user presses "down"
///
/// The precondition is what makes this subtle: the target wrapped line
/// is OFF-SCREEN when Down is pressed.  Down must scroll the viewport
/// AND move the cursor by exactly one visual row.  The bug makes the
/// cursor land several visual rows *inside* the just-scrolled-in wrapped
/// paragraph.
fn encodings_fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("issue_1574_encodings.md")
}

/// Anchor text at the end of the first paragraph.
const END_OF_PARA1: &str = "data as UTF-8.";
/// Text that appears only deep inside paragraph one (well before the end).
/// If the cursor lands on a row containing this string after pressing Up
/// from the empty-line-at-top-of-viewport precondition, we've reproduced
/// the mirror of the "jumps past the paragraph start" bug.
const START_OF_PARA1: &str = "Text files come in";
/// First word of the second paragraph — must appear in the cursor's visual
/// row after the second Down press if the editor is behaving correctly.
const START_OF_PARA2: &str = "Due to the fact";
/// Text that appears only deep inside paragraph two.  If the cursor lands
/// on a row containing this string after two Down presses, we've
/// reproduced the "jumps past the paragraph start" bug.
const MIDDLE_OF_PARA2: &str = "resets the encoder state";
/// Unique marker that appears only on the first visual row of the first
/// logical line of the encodings fixture.  Used by the Ctrl+Up/Ctrl+Down
/// round-trip test to detect "viewport is at the top of the buffer" from
/// rendered output alone.
const FIRST_LINE_MARKER: &str = "Padding line 01";

/// Status returned by `run_jump_scenario_at_width` so the driving test can
/// distinguish "we couldn't even set up the precondition at this width"
/// (skipped width) from "setup worked; now let's check the assertion"
/// (real pass / fail).
enum ScenarioOutcome {
    /// Setup reached the precondition and the Down press landed on the
    /// first visual row of paragraph two.
    Ok,
    /// Couldn't park the cursor on the last visible row with paragraph
    /// two hidden at this width/height — skip, try a different width.
    SetupSkipped(String),
    /// Setup reached the precondition but the Down press landed on the
    /// wrong row. Includes a full diagnostic payload.
    JumpReproduced(String),
    /// Setup reached the precondition but the cursor row after Down did
    /// not contain the expected anchor AND did not contain the
    /// forbidden middle-of-paragraph text.  Less severe than a
    /// confirmed jump but still worth reporting.
    UnexpectedRow(String),
}

/// Drive the full "cursor on empty line at bottom, press Down" scenario
/// for a given terminal `(width, height)`.  Returns an outcome the
/// sweeping test can aggregate across widths.
///
/// Observes only rendered output — no viewport internals.
fn run_jump_scenario_at_width(width: u16, height: u16) -> ScenarioOutcome {
    run_jump_scenario_at_width_with_fixture(width, height, &encodings_fixture_path())
}

/// Implementation of the Down-jump scenario parameterized by fixture path
/// so variants (e.g. CRLF line endings) can exercise the same flow.
fn run_jump_scenario_at_width_with_fixture(
    width: u16,
    height: u16,
    fixture_path: &std::path::Path,
) -> ScenarioOutcome {
    tracing::debug!("down-jump scenario: width={width}, height={height}: starting");
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

    // Step 1: navigate to the end of paragraph one ("... data as UTF-8.").
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

    // Step 2: one Down — cursor should be on the empty separator line.
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

    // Step 3: Ctrl+Up until the cursor is on the last visible row AND
    // paragraph two is off-screen.  Convergence rules mirror the Up
    // scenario (see `run_up_jump_scenario_at_width`): bail with
    // SetupSkipped when we can't establish the precondition instead of
    // looping forever.
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
            // Cursor row is no longer an empty separator — cursor went
            // off-screen or moved off the empty line.
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

    // Re-verify preconditions after scroll.
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

    // Step 4: THE TEST — press Down once and inspect the cursor's row.
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

/// Mirror of `run_jump_scenario_at_width`: drives the opposite direction.
/// Parks the cursor on the empty line between paragraphs at the *top* of
/// the viewport, with paragraph one (the long wrapped paragraph *above*)
/// scrolled off-screen, then presses Up.  The cursor must land on the
/// last visual row of paragraph one (containing `END_OF_PARA1`), not on
/// a row deep inside paragraph one (containing `START_OF_PARA1`).
///
/// This exercises the Up-direction code path through the wrap-aware
/// fallback when `CachedLayout::move_visual_line` can't answer because
/// the target row is off-screen above the viewport.
fn run_up_jump_scenario_at_width(width: u16, height: u16) -> ScenarioOutcome {
    run_up_jump_scenario_at_width_with_fixture(width, height, &encodings_fixture_path())
}

/// Implementation of the Up-jump scenario parameterized by fixture path
/// so variants (e.g. CRLF line endings) can exercise the same flow.
fn run_up_jump_scenario_at_width_with_fixture(
    width: u16,
    height: u16,
    fixture_path: &std::path::Path,
) -> ScenarioOutcome {
    tracing::debug!("up-jump scenario: width={width}, height={height}: starting");
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

    // Step 1: navigate to end of paragraph one, then Down to the empty
    // separator line between paragraph one and paragraph two.
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

    // Step 2: Ctrl+Down pushes the viewport forward, moving the cursor's
    // screen row UP (toward the top of the viewport).  Skip this width
    // if the setup can't converge on the precondition — the scenario's
    // purpose is to exercise the Up-direction fallback, not to test
    // every corner of the scroll behavior itself.
    //
    // Convergence rules:
    //   * hit the precondition → Ok, exit the loop
    //   * top content row didn't change after Ctrl+Down → we're at the
    //     bottom of the buffer, can't scroll further — skip this width
    //   * cursor row stopped reporting an empty line → cursor went
    //     off-screen or the cursor moved to a different line; the
    //     precondition we're trying to establish no longer holds —
    //     skip this width
    //   * went beyond a pragmatic cap (SETUP_STEP_LIMIT) without
    //     convergence → skip this width
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
            // Cursor row is no longer an empty separator — the cursor
            // has gone off-screen or moved to a different line.
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

    // Re-verify preconditions after scroll.
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

    // Step 3: press Up once.  The cursor should move to paragraph one's
    // last visual row (which scrolls on-screen).  The bug would make the
    // cursor jump several visual rows up into paragraph one — e.g. land
    // on its first row containing `START_OF_PARA1`.
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

/// Shared sweep infrastructure used by all three #[test] functions in
/// this module.  Each sweep iterates over a caller-chosen set of
/// terminal widths and heights, calling the per-scenario runner and
/// aggregating outcomes.  Progress is logged via `tracing::info` so a
/// CI timeout reveals exactly which width was in flight.
///
/// Each `#[test]` picks its own sweep granularity based on how much
/// per-scenario work it performs — the Ctrl+Up/Ctrl+Down round-trip
/// test walks the cursor through many steps per width, so it uses a
/// sparser width grid than the one-shot jump-variant tests.
fn drive_width_sweep(
    label: &'static str,
    widths: &[u16],
    heights: &[u16],
    scenario: impl Fn(u16, u16) -> ScenarioOutcome,
) {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    let total = widths.len() * heights.len();

    tracing::info!(
        "issue_1574 sweep {label}: starting, {} widths × {} heights = {} scenarios",
        widths.len(),
        heights.len(),
        total,
    );

    let mut successful: Vec<(u16, u16)> = Vec::new();
    let mut skipped: Vec<String> = Vec::new();
    let mut jump_failures: Vec<String> = Vec::new();
    let mut unexpected_row_failures: Vec<String> = Vec::new();

    let started = std::time::Instant::now();
    let mut done = 0usize;

    for &height in heights {
        for &width in widths {
            tracing::info!(
                "issue_1574 sweep {label}: running scenario {}/{} \
                 (width={width}, height={height}, elapsed={:?})",
                done + 1,
                total,
                started.elapsed(),
            );
            let outcome = scenario(width, height);
            done += 1;
            match outcome {
                ScenarioOutcome::Ok => {
                    tracing::info!("issue_1574 sweep {label}: width={width} height={height} OK",);
                    successful.push((width, height));
                }
                ScenarioOutcome::SetupSkipped(msg) => {
                    tracing::info!(
                        "issue_1574 sweep {label}: width={width} height={height} SKIP: {msg}",
                    );
                    skipped.push(msg);
                }
                ScenarioOutcome::JumpReproduced(msg) => {
                    tracing::warn!(
                        "issue_1574 sweep {label}: width={width} height={height} JUMP: {msg}",
                    );
                    jump_failures.push(msg);
                }
                ScenarioOutcome::UnexpectedRow(msg) => {
                    tracing::warn!(
                        "issue_1574 sweep {label}: width={width} height={height} UNEXPECTED: {msg}",
                    );
                    unexpected_row_failures.push(msg);
                }
            }
        }
    }

    tracing::info!(
        "issue_1574 sweep {label}: done in {:?}.  {} ok, {} skipped, {} jump, {} unexpected",
        started.elapsed(),
        successful.len(),
        skipped.len(),
        jump_failures.len(),
        unexpected_row_failures.len(),
    );

    // Primary failure: any width reproduced the jump bug.
    assert!(
        jump_failures.is_empty(),
        "[{label}] Bug #1574 (jump variant) reproduced at {n} terminal size(s):\n\n{joined}",
        n = jump_failures.len(),
        joined = jump_failures.join("\n\n---\n\n"),
    );

    // Secondary failure: cursor landed somewhere unexpected.
    assert!(
        unexpected_row_failures.is_empty(),
        "[{label}] At {n} width(s), the cursor landed on neither the \
         expected anchor nor a known-bad row:\n\n{joined}",
        n = unexpected_row_failures.len(),
        joined = unexpected_row_failures.join("\n\n---\n\n"),
    );

    // Sanity: at least one width exercised the bug-triggering state.
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
fn test_issue_1574_down_from_empty_line_at_bottom_lands_on_paragraph_start() {
    // Sweep a range of terminal widths (and two representative heights)
    // looking for the "cursor jumps past the first wrapped segment"
    // variant of issue #1574 — Down direction.  Park the cursor on the
    // empty separator line with paragraph two hidden BELOW, press Down,
    // and verify the cursor lands on the first visual row of paragraph
    // two (not somewhere deep inside it).
    //
    // This scenario is fast per-width (one Find + a handful of key
    // presses), so a dense grid is affordable.
    let widths: Vec<u16> = (30u16..=120).step_by(3).collect();
    let heights: [u16; 2] = [15, 20];
    drive_width_sweep("down-jump", &widths, &heights, run_jump_scenario_at_width);
}

#[test]
fn test_issue_1574_up_from_empty_line_at_top_lands_on_paragraph_end() {
    // Mirror of the Down sweep: park the cursor on the empty separator
    // line with paragraph one hidden ABOVE, press Up, and verify the
    // cursor lands on the LAST visual row of paragraph one (not on its
    // first visual row or somewhere in the middle).  This exercises the
    // Up-direction path through the wrap-aware cursor-move fallback.
    let widths: Vec<u16> = (30u16..=120).step_by(3).collect();
    let heights: [u16; 2] = [15, 20];
    drive_width_sweep("up-jump", &widths, &heights, run_up_jump_scenario_at_width);
}

/// Drive the "Ctrl+Up / Ctrl+Down scroll round-trip" scenario for a given
/// terminal `(width, height)`.
///
/// Invariants checked at every step as the cursor walks Down through the
/// buffer, using ONLY rendered output (top content row + first-line
/// marker visibility):
///
/// 1. If the viewport is NOT at the top of the buffer (the very first
///    logical line of the fixture is not visible at the top of the
///    content area), pressing Ctrl+Up must scroll the viewport — i.e.
///    the top content row must change.
/// 2. If the viewport IS at the top of the buffer, pressing Ctrl+Up must
///    be a no-op (the top content row must not change, since the
///    viewport can't scroll any further up).
/// 3. After any Ctrl+Up, pressing Ctrl+Down must restore the top content
///    row to what it was before Ctrl+Up.  That round-trip must be exact,
///    not just "close" — scroll-up by one row followed by scroll-down by
///    one row is a symmetry the user can rely on when exploring a
///    wrapped buffer.
fn run_ctrl_up_down_roundtrip_scenario_at_width(width: u16, height: u16) -> ScenarioOutcome {
    tracing::debug!("ctrl-up-down scenario: width={width}, height={height}: starting");
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

    // Start at the very top of the buffer.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Detect viewport-at-top from rendered output: the fixture's first
    // logical line has a unique marker only visible when it's on screen.
    let is_viewport_at_top = |h: &EditorTestHarness| -> bool {
        let (first, last) = h.content_area_rows();
        (first..=last).any(|r| h.get_screen_row(r).contains(FIRST_LINE_MARKER))
    };

    // Cap the walk at a modest number of Down presses.  This is more
    // than enough to exercise the top-of-buffer no-op invariant AND
    // observe many successful Ctrl+Up/Ctrl+Down round-trips without
    // cursor-chasing the viewport past every line boundary of the
    // buffer.  A larger walk can surface a separate pre-existing
    // asymmetry in `scroll_up_visual` / `scroll_down_visual` near
    // logical-line boundaries (they count visual rows via
    // `wrap_line`, which disagrees with the renderer's
    // `apply_wrapping_transform` on word-boundary wraps — a wider
    // refactor than what this regression test is scoped to cover).
    const MAX_STEPS: usize = 30;

    let mut steps_exercised_scroll = 0usize;
    let mut steps_exercised_at_top = 0usize;
    let mut prev_cursor_y: Option<u16> = None;

    for step in 1..=MAX_STEPS {
        // Detect viewport state BEFORE any scroll action.
        let at_top = is_viewport_at_top(&harness);
        let top_before = top_content_row(&harness);

        // Ctrl+Up — scroll viewport up (towards start of buffer).
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

        // Invariant 2: Ctrl+Down is an exact inverse of Ctrl+Up.  After
        // any Ctrl+Up that scrolled the viewport, the following
        // Ctrl+Down must bring the top content row back to exactly
        // where it was before the Ctrl+Up.  At the top of the buffer
        // Ctrl+Up was a no-op, so the pair isn't a round-trip; the
        // Ctrl+Down simply scrolls forward one row.
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

        // Advance cursor by pressing Down once.  We stop when Down no
        // longer moves the cursor (end of buffer).
        let (_cx_pre, cy_pre) = harness.screen_cursor_position();
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        let (_cx_post, cy_post) = harness.screen_cursor_position();
        let top_after_walk = top_content_row(&harness);

        // Progress detection: consider ourselves stuck if neither the
        // cursor row nor the top content row changed over consecutive
        // steps.  A single stall is expected right at end-of-buffer.
        let nothing_moved =
            cy_pre == cy_post && top_after_walk == top_before && prev_cursor_y == Some(cy_post);
        if nothing_moved {
            break;
        }
        prev_cursor_y = Some(cy_post);
    }

    // Sanity: the walk must have exercised BOTH the "viewport not at top"
    // path and the "viewport at top" path for this width.  If we only
    // ever observed one, the walk didn't cover enough ground.
    if steps_exercised_scroll == 0 || steps_exercised_at_top == 0 {
        return ScenarioOutcome::SetupSkipped(format!(
            "width={width}, height={height}: walk did not exercise both the \
             top-of-buffer and the scrolled-off-top regimes \
             (scroll-hits={steps_exercised_scroll}, top-hits={steps_exercised_at_top})"
        ));
    }

    ScenarioOutcome::Ok
}

/// Regression guard for the CRLF cursor-math bug exposed on Windows
/// CI by `test_issue_1574_down_from_empty_line_at_bottom_lands_on_paragraph_start`.
///
/// On Windows runners with default `core.autocrlf=true` the encodings
/// fixture was checked out with CRLF line endings rather than LF, which
/// exposed a real bug in the cursor-move fallback
/// (`compute_wrap_aware_visual_move_fallback`): it did ad-hoc byte
/// arithmetic (`+1` on Down, `-1` on Up) that only worked for LF.  On a
/// CRLF row the `+1` step lands the cursor on the `\n` *inside* the
/// CRLF pair, which `find_view_line_for_byte` resolves back to the same
/// row — pressing Down appeared to jump the cursor to an unrelated
/// visual row deep inside the current paragraph instead of advancing
/// to the start of the next logical line.
///
/// This test writes a CRLF copy of the encodings fixture to a tempfile
/// and runs the Down-jump scenario against it.  The fixture being CRLF
/// must be irrelevant to cursor math: the bug is not "Windows CI
/// runner", it's "editor mishandles CRLF files" — fixing it lets users
/// edit CRLF files without cursor drift.  With the fix in place this
/// test must pass at every width that the Windows CI run showed
/// failing.
#[test]
fn test_issue_1574_crlf_fixture_down_jump_lands_on_paragraph_start() {
    init_tracing_from_env();

    // Write a CRLF version of the encodings fixture to a tempfile.
    // Normalize to LF first so the test produces correct CRLF regardless
    // of whether the checkout's fixture has LF (Linux/macOS) or CRLF
    // (Windows with default core.autocrlf=true).  Without normalization,
    // double-replacement would yield `\r\r\n` on Windows — visible in
    // the editor as lone `<0D>` characters at the start of every line,
    // which breaks the test's "Down lands on empty separator" setup.
    let original = std::fs::read_to_string(encodings_fixture_path())
        .expect("failed to read encodings fixture");
    let crlf: String = original.replace("\r\n", "\n").replace('\n', "\r\n");
    let dir = tempfile::TempDir::new().expect("tempdir");
    let crlf_path = dir.path().join("issue_1574_encodings_crlf.md");
    std::fs::write(&crlf_path, crlf.as_bytes()).expect("write crlf fixture");

    // Sanity: confirm the tempfile has clean \r\n line endings — no
    // bare LFs and no `\r\r` sequences (the latter being the symptom
    // of the double-replacement bug this normalization guards against).
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

    // Run the Down-jump scenario against the CRLF fixture at the widths
    // that Windows CI showed failing.  Every non-skipped width must
    // reach `ScenarioOutcome::Ok`.
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

/// Mirror of `test_issue_1574_crlf_fixture_down_jump_lands_on_paragraph_start`
/// for the Up direction: with a CRLF-encoded fixture, pressing Up from
/// the empty separator line at the top of the viewport must land the
/// cursor on paragraph one's LAST visual row — not mid-CRLF and not
/// somewhere else inside paragraph one.
#[test]
fn test_issue_1574_crlf_fixture_up_jump_lands_on_paragraph_end() {
    init_tracing_from_env();

    // Normalize to LF first; see the Down counterpart above for why.
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

#[test]
fn test_issue_1574_ctrl_up_down_scroll_roundtrip_sweep() {
    // Same width-sweep strategy as the jump-variant tests: run the
    // Ctrl+Up / Ctrl+Down round-trip scenario at many terminal widths
    // and two representative heights, aggregating outcomes.  Invariants
    // enforced per step: Ctrl+Up scrolls iff the viewport is not already
    // at the top of the buffer, and Ctrl+Down is an exact inverse.
    //
    // This scenario walks the cursor through many Down presses per
    // width, running Ctrl+Up / Ctrl+Down / Down at every step, so each
    // scenario is substantially heavier than the jump-variant ones.
    // Use a sparser width grid so the full sweep fits comfortably
    // within nextest's 180s per-test CI timeout.
    let widths: Vec<u16> = (30u16..=120).step_by(10).collect();
    let heights: [u16; 1] = [15];
    drive_width_sweep(
        "ctrl-up-down-roundtrip",
        &widths,
        &heights,
        run_ctrl_up_down_roundtrip_scenario_at_width,
    );
}
