//! Reproduction for two related bugs reported against the scroll
//! mechanisms (mouse wheel, scrollbar drag, PageDown) when line wrap
//! is enabled and the buffer contains very long wrapped lines:
//!
//! 1. **Over-scroll into empty viewport.**  `scroll_down_visual`'s
//!    within-line fast-path advances `top_view_line_offset` without
//!    re-clamping against the buffer's real tail.  After enough scroll
//!    ticks the viewport ends up showing only the last wrapped segment
//!    of the last logical line, with the rest drawn as past-EOF `~`
//!    rows.
//!
//! 2. **Under-scroll — last line never visible.**  The scroll math
//!    (`scrollbar_math::build_visual_row_map`,
//!    `Viewport::find_max_visual_scroll_position`) counts visual rows
//!    with `primitives::line_wrapping::wrap_line`, which is a pure
//!    char-width hard wrap.  The renderer's
//!    `split_rendering::transforms::apply_wrapping_transform` does a
//!    word-boundary-aware wrap with a 16-column lookback — so on real
//!    text (words separated by spaces) the renderer produces more
//!    visual rows than the scroll math accounts for.  `max_scroll_row`
//!    is therefore too small, and mouse wheel / scrollbar drag /
//!    PageDown all stop scrolling before the real last visual row is
//!    on-screen.  Only the Down-arrow keyboard path can reach the end.
//!
//! The two classes of test below mirror the two bugs.  Both use the
//! width-sweep pattern from `issue_1574_wrapped_down_scroll.rs`:
//! the scenario is run at several terminal widths (and two heights) so
//! a regression that only fires at some specific wrap width is still
//! caught.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Marker appearing only on the tail visual row of the last logical
/// line of the test buffer.
const LAST_LINE_MARKER: &str = "LAST_LINE_MARKER_XYZ";

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

fn content_area_snapshot(harness: &EditorTestHarness) -> String {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .map(|r| harness.get_screen_row(r))
        .collect::<Vec<_>>()
        .join("\n")
}

/// A content row is "empty" if it contains only the `~` out-of-buffer
/// indicator.  The renderer draws `~` for rows past EOF — i.e. when
/// the viewport has over-scrolled and has fewer than viewport-height
/// real visual rows to show.
fn row_is_past_eof_marker(row: &str) -> bool {
    row.trim() == "~"
}

fn count_populated_rows(harness: &EditorTestHarness) -> usize {
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter(|r| !row_is_past_eof_marker(&harness.get_screen_row(*r)))
        .count()
}

fn marker_row(harness: &EditorTestHarness) -> Option<usize> {
    let (first, last) = harness.content_area_rows();
    (first..=last).find(|r| harness.get_screen_row(*r).contains(LAST_LINE_MARKER))
}

/// Status returned by each per-width scenario runner so the sweep
/// driver can distinguish "we couldn't set up the precondition at this
/// width" (skipped) from "setup worked; here's what the scroll did"
/// (pass / fail).
enum Outcome {
    /// The scroll mechanism converged on a fully-populated viewport
    /// with the tail marker visible on the last content row.
    Ok,
    /// The buffer/viewport combo didn't exercise the bug at this
    /// width (e.g. whole buffer fits in the viewport).
    SetupSkipped(String),
    /// The scroll mechanism failed — specific failure string.
    Failure(String),
}

/// Drive a per-width scenario across a range of widths × heights.
/// Fails if ANY width returned Failure.  Requires at least one width
/// to return Ok (otherwise the sweep isn't actually exercising the
/// bug path, which is a sign the scenario has drifted).
fn drive_width_sweep(
    label: &'static str,
    widths: &[u16],
    heights: &[u16],
    scenario: impl Fn(u16, u16) -> Outcome,
) {
    let mut ok_count = 0usize;
    let mut skipped: Vec<String> = Vec::new();
    let mut failures: Vec<String> = Vec::new();
    for &height in heights {
        for &width in widths {
            match scenario(width, height) {
                Outcome::Ok => ok_count += 1,
                Outcome::SetupSkipped(msg) => skipped.push(format!("w={width} h={height}: {msg}")),
                Outcome::Failure(msg) => failures.push(format!("w={width} h={height}: {msg}")),
            }
        }
    }

    assert!(
        failures.is_empty(),
        "[{label}] {} of {} (width, height) combo(s) reproduced the bug:\n\n{}",
        failures.len(),
        failures.len() + ok_count + skipped.len(),
        failures.join("\n---\n"),
    );
    assert!(
        ok_count > 0,
        "[{label}] No width in the sweep exercised the bug-triggering \
         state — every combo was skipped, so the test isn't actually \
         checking anything.  Skipped reasons:\n{}",
        skipped.join("\n---\n"),
    );
}

// ---------------------------------------------------------------------------
// Bug 1 fixture: a handful of very long homogeneous-character lines
// (no word boundaries).  Each logical line wraps to many visual rows;
// the last logical line is itself long and carries the marker at its
// tail.  With Bug 1, `scroll_down_visual` pushes
// `top_view_line_offset` past the clamp point within the last line,
// and the final viewport has only a few content rows and many `~`
// rows.
// ---------------------------------------------------------------------------

fn build_bug1_buffer(line_chars: usize, long_lines: usize) -> String {
    let letters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'];
    let mut lines: Vec<String> = (0..long_lines)
        .map(|i| letters[i % letters.len()].to_string().repeat(line_chars))
        .collect();
    let mut last = "Z".repeat(line_chars);
    last.push_str(LAST_LINE_MARKER);
    lines.push(last);
    lines.join("\n")
}

/// Invariant asserted by all three Bug 1 scenarios: after the scroll
/// has converged, the viewport must be fully populated (no past-EOF
/// rows), the marker must be visible, and it must sit on the LAST
/// content row (the canonical "clamped at the bottom of the buffer"
/// state).
fn bug1_check_clamped(harness: &EditorTestHarness) -> Outcome {
    let (_, content_last_row) = harness.content_area_rows();
    let (content_first_row, _) = harness.content_area_rows();
    let viewport_height = content_last_row - content_first_row + 1;
    let populated = count_populated_rows(harness);
    let marker = marker_row(harness);
    let snap = content_area_snapshot(harness);
    if populated != viewport_height {
        return Outcome::Failure(format!(
            "viewport over-scrolled: only {populated} of {viewport_height} \
             content rows are populated (rest are past-EOF `~` rows).\n\
             Content:\n{snap}"
        ));
    }
    if marker != Some(content_last_row) {
        return Outcome::Failure(format!(
            "marker should sit on the last content row ({content_last_row}); \
             marker_row={marker:?}.\nContent:\n{snap}"
        ));
    }
    Outcome::Ok
}

fn setup_bug1_at_top(width: u16, height: u16) -> Result<EditorTestHarness, String> {
    let mut harness = EditorTestHarness::with_config(width, height, config_with_wrap())
        .map_err(|e| format!("harness init failed: {e}"))?;
    // Choose line_chars so each line wraps to a decent number of
    // visual rows regardless of the (width, height) we're at.
    let line_chars = (width as usize).saturating_sub(10) * 16;
    let content = build_bug1_buffer(line_chars.max(200), 5);
    let fixture = harness
        .load_buffer_from_text(&content)
        .map_err(|e| format!("load_buffer_from_text failed: {e}"))?;
    std::mem::forget(fixture);
    harness
        .render()
        .map_err(|e| format!("render failed: {e}"))?;
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .map_err(|e| format!("ctrl+home failed: {e}"))?;
    harness
        .render()
        .map_err(|e| format!("render failed: {e}"))?;
    if marker_row(&harness).is_some() {
        return Err(format!(
            "marker already visible at the top — buffer isn't large \
             enough to require scrolling.\nContent:\n{}",
            content_area_snapshot(&harness)
        ));
    }
    Ok(harness)
}

fn bug1_mouse_wheel_scenario(width: u16, height: u16) -> Outcome {
    let mut harness = match setup_bug1_at_top(width, height) {
        Ok(h) => h,
        Err(e) => return Outcome::SetupSkipped(e),
    };
    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = width / 2;
    let scroll_row = content_first_row as u16 + 2;
    // 60 wheel ticks × ~3 rows/tick = ~180 visual rows, ~2x the
    // buffer's ~96 visual rows at these widths — comfortably past the
    // end while keeping the full sweep inside CI's per-test budget.
    for _ in 0..60 {
        if harness.mouse_scroll_down(scroll_col, scroll_row).is_err() {
            return Outcome::SetupSkipped("mouse_scroll_down failed".into());
        }
    }
    bug1_check_clamped(&harness)
}

fn bug1_pagedown_scenario(width: u16, height: u16) -> Outcome {
    let mut harness = match setup_bug1_at_top(width, height) {
        Ok(h) => h,
        Err(e) => return Outcome::SetupSkipped(e),
    };
    // 20 PageDowns is 5× viewport_height(12)=~60+ logical lines of
    // jumps — well past end-of-buffer in this fixture.
    for _ in 0..20 {
        if harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .is_err()
        {
            return Outcome::SetupSkipped("PageDown failed".into());
        }
    }
    bug1_check_clamped(&harness)
}

fn bug1_scrollbar_drag_scenario(width: u16, height: u16) -> Outcome {
    let mut harness = match setup_bug1_at_top(width, height) {
        Ok(h) => h,
        Err(e) => return Outcome::SetupSkipped(e),
    };
    let scrollbar_col = width - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();
    if harness
        .mouse_drag(
            scrollbar_col,
            content_first_row as u16,
            scrollbar_col,
            content_last_row as u16,
        )
        .is_err()
    {
        return Outcome::SetupSkipped("mouse_drag failed".into());
    }
    bug1_check_clamped(&harness)
}

#[test]
fn test_bug1_mouse_wheel_does_not_over_scroll_into_empty_viewport() {
    // Sweep kept compact so the whole sweep fits inside nextest's
    // 180s per-test CI budget. Three widths are enough to exercise the
    // wrap-at-word-boundary edge cases that trigger the bug; adding
    // more didn't catch anything the three already catch.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep(
        "bug1/mouse-wheel",
        &widths,
        &heights,
        bug1_mouse_wheel_scenario,
    );
}

#[test]
fn test_bug1_page_down_does_not_over_scroll_into_empty_viewport() {
    // Sweep kept compact so the whole sweep fits inside nextest's
    // 180s per-test CI budget. Three widths are enough to exercise the
    // wrap-at-word-boundary edge cases that trigger the bug; adding
    // more didn't catch anything the three already catch.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep("bug1/page-down", &widths, &heights, bug1_pagedown_scenario);
}

#[test]
fn test_bug1_scrollbar_drag_does_not_over_scroll_into_empty_viewport() {
    // Sweep kept compact so the whole sweep fits inside nextest's
    // 180s per-test CI budget. Three widths are enough to exercise the
    // wrap-at-word-boundary edge cases that trigger the bug; adding
    // more didn't catch anything the three already catch.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep(
        "bug1/scrollbar-drag",
        &widths,
        &heights,
        bug1_scrollbar_drag_scenario,
    );
}

// ---------------------------------------------------------------------------
// Bug 2 fixture: realistic word-wrapped text.  The renderer's
// word-aware wrap produces MORE segments than the scroll math's
// char-wrap, so the scroll math's computed `max_scroll_row` stops
// short of the real final visual row of the buffer.  Mouse wheel,
// PageDown, and scrollbar drag all fail to make the tail marker
// visible, even though the buffer itself is larger than the viewport.
// ---------------------------------------------------------------------------

fn build_bug2_buffer() -> String {
    // Paragraphs of realistic word text.  Each paragraph wraps many
    // times at the terminal widths we test.  The last paragraph
    // carries the marker at its tail — the marker is a long word so
    // word-aware wrapping always pushes it to its own row.
    let paragraph: String = (0..40)
        .map(|i| format!("word{:02}", i % 100))
        .collect::<Vec<_>>()
        .join(" ");
    let mut lines: Vec<String> = (0..12).map(|_| paragraph.clone()).collect();
    let mut last = paragraph.clone();
    last.push(' ');
    last.push_str(LAST_LINE_MARKER);
    lines.push(last);
    lines.join("\n")
}

fn setup_bug2_at_top(width: u16, height: u16) -> Result<EditorTestHarness, String> {
    let mut harness = EditorTestHarness::with_config(width, height, config_with_wrap())
        .map_err(|e| format!("harness init failed: {e}"))?;
    let content = build_bug2_buffer();
    let fixture = harness
        .load_buffer_from_text(&content)
        .map_err(|e| format!("load_buffer_from_text failed: {e}"))?;
    std::mem::forget(fixture);
    harness
        .render()
        .map_err(|e| format!("render failed: {e}"))?;
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .map_err(|e| format!("ctrl+home failed: {e}"))?;
    harness
        .render()
        .map_err(|e| format!("render failed: {e}"))?;
    if marker_row(&harness).is_some() {
        return Err(format!(
            "marker already visible at the top — buffer isn't large \
             enough at this width to require scrolling.\nContent:\n{}",
            content_area_snapshot(&harness)
        ));
    }
    Ok(harness)
}

/// Invariant asserted by all Bug 2 scenarios: after the scroll
/// mechanism has done its work, the tail marker must be visible
/// somewhere in the viewport.  (The stronger "marker on last content
/// row" invariant is covered by Bug 1 scenarios; here we only care
/// that we can reach the marker at all.)
fn bug2_check_marker_visible(harness: &EditorTestHarness) -> Outcome {
    if marker_row(harness).is_some() {
        Outcome::Ok
    } else {
        Outcome::Failure(format!(
            "tail marker never became visible — scroll stopped short \
             of the real end of the buffer.\nContent:\n{}",
            content_area_snapshot(harness),
        ))
    }
}

fn bug2_mouse_wheel_scenario(width: u16, height: u16) -> Outcome {
    let mut harness = match setup_bug2_at_top(width, height) {
        Ok(h) => h,
        Err(e) => return Outcome::SetupSkipped(e),
    };
    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = width / 2;
    let scroll_row = content_first_row as u16 + 2;
    // 150 wheel ticks ≈ 450 visual rows, plenty to reach the end of a
    // 12-paragraph word-wrapped buffer (~100 visual rows at these
    // widths).
    for _ in 0..150 {
        if harness.mouse_scroll_down(scroll_col, scroll_row).is_err() {
            return Outcome::SetupSkipped("mouse_scroll_down failed".into());
        }
    }
    bug2_check_marker_visible(&harness)
}

fn bug2_pagedown_scenario(width: u16, height: u16) -> Outcome {
    let mut harness = match setup_bug2_at_top(width, height) {
        Ok(h) => h,
        Err(e) => return Outcome::SetupSkipped(e),
    };
    // 30 PageDowns each jumps ~(viewport_height-1) logical lines; with
    // 13 paragraph lines in the fixture, that's enough to fly past the
    // end even at the widest sweep width.
    for _ in 0..30 {
        if harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .is_err()
        {
            return Outcome::SetupSkipped("PageDown failed".into());
        }
    }
    bug2_check_marker_visible(&harness)
}

fn bug2_scrollbar_drag_scenario(width: u16, height: u16) -> Outcome {
    let mut harness = match setup_bug2_at_top(width, height) {
        Ok(h) => h,
        Err(e) => return Outcome::SetupSkipped(e),
    };
    let scrollbar_col = width - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();
    if harness
        .mouse_drag(
            scrollbar_col,
            content_first_row as u16,
            scrollbar_col,
            content_last_row as u16,
        )
        .is_err()
    {
        return Outcome::SetupSkipped("mouse_drag failed".into());
    }
    bug2_check_marker_visible(&harness)
}

#[test]
fn test_bug2_mouse_wheel_reaches_last_line_of_word_wrapped_buffer() {
    // Compact sweep — see note on bug1 sweep widths.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep(
        "bug2/mouse-wheel",
        &widths,
        &heights,
        bug2_mouse_wheel_scenario,
    );
}

#[test]
fn test_bug2_page_down_reaches_last_line_of_word_wrapped_buffer() {
    // Compact sweep — see note on bug1 sweep widths.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep("bug2/page-down", &widths, &heights, bug2_pagedown_scenario);
}

#[test]
fn test_bug2_scrollbar_drag_reaches_last_line_of_word_wrapped_buffer() {
    // Compact sweep — see note on bug1 sweep widths.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep(
        "bug2/scrollbar-drag",
        &widths,
        &heights,
        bug2_scrollbar_drag_scenario,
    );
}
