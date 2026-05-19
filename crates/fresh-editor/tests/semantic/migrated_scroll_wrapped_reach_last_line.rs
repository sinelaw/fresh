//! Migration of `tests/e2e/scroll_wrapped_reach_last_line.rs` —
//! reproductions for two related scroll-when-wrapped bugs.
//!
//! ## DECLARATIVE-REWRITE DEFERRAL
//!
//! A purely declarative rewrite (scenarios-as-data,
//! zero `EditorTestHarness::` usage) was attempted and DEFERRED. The
//! load-bearing claims here cannot be expressed in the current
//! scenario DSL without further extensions. Status of each piece:
//!
//!   * Mouse wheel / scrollbar drag — RESOLVED.
//!     `LayoutScenario.events` now accepts `InputEvent::Mouse(Wheel
//!     { row, col, dy })` and `Mouse(Drag { from_row, from_col,
//!     to_row, to_col, button })`, and `LayoutScenario.mouse_drags`
//!     accepts symbolic `MouseDragSpec::VerticalScrollbarFullRange`.
//!     Either could carry the scroll mechanism in scenario data.
//!
//!   * `content_area_rows()` / `get_screen_row(row)` — STILL BLOCKING.
//!     Bug 1's "viewport fully populated (no past-EOF `~` rows)"
//!     check needs to count populated rows over the content-area
//!     row range. The "marker sits on the last content row" check
//!     needs the same range. There is no `EditorTestApi` projection
//!     of `content_area_rows()`. Extension needed:
//!       - Add `EditorTestApi::content_area_rows() -> (u16, u16)`.
//!       - Surface it on `RenderSnapshot.content_area_rows: Option<(u16,
//!         u16)>` so a scenario matcher can address rows relative to
//!         the content area instead of hard-coding terminal-chrome
//!         offsets.
//!       - Add `RowMatch::AllRowsInRangeMatch { lo, hi, predicate }`
//!         (or a `NoRowInRangeContains`) so a scenario can express
//!         "across rows [first_content..=last_content], none is just
//!         `~`" without expanding the matcher list per row.
//!
//!   * Width-sweep + skip-on-precondition logic — STILL BLOCKING.
//!     Each sweep entry can succeed, fail, or skip ("buffer not
//!     large enough at this width to require scrolling"). The
//!     declarative runner has no SetupSkipped concept. Extension
//!     needed: a sweep wrapper that consumes
//!     `Vec<LayoutScenario>` and collects per-entry outcomes,
//!     asserting at least one non-skipped entry succeeded.
//!
//! Keeping the current harness-direct implementation (documented in
//! the section below) until the remaining DSL extensions land.
//!
//! See `docs/internal/scenario-migration-status.md` for the broader
//! migration roadmap.
//!
//! The two bugs (verbatim from the e2e):
//!
//!   1. **Over-scroll into empty viewport.**  `scroll_down_visual`'s
//!      within-line fast-path advances `top_view_line_offset`
//!      without re-clamping against the buffer's real tail. After
//!      enough scroll ticks the viewport ends up showing only the
//!      last wrapped segment of the last logical line, with the
//!      rest drawn as past-EOF `~` rows.
//!
//!   2. **Under-scroll — last line never visible.**  Scroll math
//!      counts visual rows with a char-wrap (`wrap_line`), but the
//!      renderer uses a word-boundary-aware wrap. On realistic
//!      text the renderer produces more visual rows than the
//!      scroll math accounts for, so `max_scroll_row` is too
//!      small. Mouse wheel / scrollbar drag / PageDown all stop
//!      short of the real last visual row; only the Down-arrow
//!      keyboard path can reach the end.
//!
//! Six tests are migrated, one per (bug, scroll mechanism) combo:
//!   * Bug 1 × {mouse-wheel, page-down, scrollbar-drag} — assert
//!     viewport is fully populated AND marker on the last content
//!     row (the "clamped at the bottom of the buffer" state).
//!   * Bug 2 × {mouse-wheel, page-down, scrollbar-drag} — assert
//!     tail marker is visible somewhere in the viewport.
//!
//! Each test runs a width sweep (`drive_width_sweep`) so a
//! regression that only fires at a specific wrap width is still
//! caught. Sweep widths and counts are preserved verbatim from
//! the e2e — they were tuned to fit nextest's per-test budget
//! while still exercising the wrap-at-word-boundary edge cases.
//!
//! ## Harness-direct pattern
//!
//! All six claims need `EditorTestHarness` surfaces with no
//! `EditorTestApi` projection: `content_area_rows`,
//! `get_screen_row`, `mouse_drag`, `mouse_scroll_down`. The
//! migration uses the harness-direct pattern (the same pattern
//! `migrated_line_wrap_parity.rs` uses for scroll/cursor
//! parity checks).
//!
//! Source: `tests/e2e/scroll_wrapped_reach_last_line.rs` (6
//! tests migrated; no tests deferred).

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

enum Outcome {
    Ok,
    SetupSkipped(String),
    Failure(String),
}

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
         checking anything. Skipped reasons:\n{}",
        skipped.join("\n---\n"),
    );
}

// ---------------------------------------------------------------------------
// Bug 1 fixture: very long homogeneous-character lines.
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
fn migrated_bug1_mouse_wheel_does_not_over_scroll_into_empty_viewport() {
    // Original: `test_bug1_mouse_wheel_does_not_over_scroll_into_empty_viewport`.
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
fn migrated_bug1_page_down_does_not_over_scroll_into_empty_viewport() {
    // Original: `test_bug1_page_down_does_not_over_scroll_into_empty_viewport`.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep("bug1/page-down", &widths, &heights, bug1_pagedown_scenario);
}

#[test]
fn migrated_bug1_scrollbar_drag_does_not_over_scroll_into_empty_viewport() {
    // Original: `test_bug1_scrollbar_drag_does_not_over_scroll_into_empty_viewport`.
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
// Bug 2 fixture: realistic word-wrapped text.
// ---------------------------------------------------------------------------

fn build_bug2_buffer() -> String {
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
fn migrated_bug2_mouse_wheel_reaches_last_line_of_word_wrapped_buffer() {
    // Original: `test_bug2_mouse_wheel_reaches_last_line_of_word_wrapped_buffer`.
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
fn migrated_bug2_page_down_reaches_last_line_of_word_wrapped_buffer() {
    // Original: `test_bug2_page_down_reaches_last_line_of_word_wrapped_buffer`.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep("bug2/page-down", &widths, &heights, bug2_pagedown_scenario);
}

#[test]
fn migrated_bug2_scrollbar_drag_reaches_last_line_of_word_wrapped_buffer() {
    // Original: `test_bug2_scrollbar_drag_reaches_last_line_of_word_wrapped_buffer`.
    let widths: [u16; 3] = [50, 80, 120];
    let heights: [u16; 1] = [16];
    drive_width_sweep(
        "bug2/scrollbar-drag",
        &widths,
        &heights,
        bug2_scrollbar_drag_scenario,
    );
}

/// Anti-test: drop the 150 mouse-wheel-down scrolls. Without
/// them, the Bug 2 word-wrapped buffer starts at the top with the
/// marker NOT visible (verified by setup's own precondition).
/// Proves the positive `migrated_bug2_mouse_wheel_*` test's
/// "marker visible" claim depends on the scroll sweep, not on the
/// buffer accidentally fitting on screen.
#[test]
fn anti_bug2_without_mouse_wheel_marker_never_visible() {
    let mut harness =
        EditorTestHarness::with_config(80, 16, config_with_wrap()).expect("harness");
    let content = build_bug2_buffer();
    let fixture = harness.load_buffer_from_text(&content).expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    harness.render().expect("render");
    // No mouse_scroll_down calls — that's the load-bearing step we drop.

    assert!(
        marker_row(&harness).is_none(),
        "anti: without mouse-wheel scrolling, the tail marker on a \
         12-paragraph word-wrapped buffer at width=80 / height=16 must \
         NOT be visible in the initial viewport. Content:\n{}",
        content_area_snapshot(&harness)
    );
}

/// Anti-test: drop the Ctrl+Home in setup_bug1_at_top + skip the
/// scrollbar-drag entirely. The viewport starts somewhere
/// (typically top), and without any scroll action the
/// last-content-row marker placement (the Bug 1 invariant) is NOT
/// satisfied. Proves `bug1_check_clamped`'s positive case is
/// gated on the actual scroll mechanism running.
#[test]
fn anti_bug1_without_scroll_marker_not_on_last_row() {
    let mut harness =
        EditorTestHarness::with_config(80, 16, config_with_wrap()).expect("harness");
    let line_chars = (80usize).saturating_sub(10) * 16;
    let content = build_bug1_buffer(line_chars.max(200), 5);
    let fixture = harness.load_buffer_from_text(&content).expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    harness.render().expect("render");
    // No mouse_drag here — that's the load-bearing step we drop.

    let (_, content_last_row) = harness.content_area_rows();
    let marker = marker_row(&harness);
    assert_ne!(
        marker,
        Some(content_last_row),
        "anti: without a scroll action, the Bug 1 marker (sitting on \
         the last logical line of a 6-line long-line buffer) must NOT \
         already be on the last content row from a fresh top-of-buffer \
         viewport. marker={marker:?} content_last_row={content_last_row}"
    );
}
