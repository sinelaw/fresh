//! Coverage for the wrapped-line navigation fast path.
//!
//! Moving the cursor up/down through a very long soft-wrapped line used
//! to be extremely slow: every keypress re-tokenised and re-wrapped the
//! whole logical line (the cursor's line was keyed in the line-wrap
//! cache by a cursor-position signature, so each move invalidated it),
//! the reference-highlight word scan walked the line with quadratic
//! grapheme lookups, and several line-boundary scans read the buffer one
//! byte at a time.
//!
//! The fix serves the visible window straight from the per-line
//! line-wrap cache (`try_cached_window` in `view_data.rs`), keyed with
//! `cursor_sig: 0` whenever no soft breaks / conceals make the layout
//! cursor-dependent.  These tests pin the correctness side: a render
//! served from warm cache entries must be indistinguishable from a cold
//! full-pipeline render of the same state.

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// A buffer mixing short lines, an empty line, and one very long line so
/// the window crosses cached-entry boundaries in every configuration.
fn mixed_content(long_len: usize, line_ending: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!("fn main() {{{line_ending}"));
    s.push_str(&format!("    let a = 1;{line_ending}"));
    s.push_str(line_ending); // empty line
    s.push_str(&"x".repeat(long_len));
    s.push_str(line_ending);
    s.push_str(&format!("    let b = 2;{line_ending}"));
    s.push_str(&format!("}}{line_ending}"));
    s
}

/// After scrolling with a render per keypress (so the window is being
/// assembled by the cached-window fast path), clearing the line-wrap
/// cache and re-rendering the SAME state — forcing the full pipeline —
/// must produce an identical screen.  Any divergence means the fast
/// path assembled something the full pipeline would not have produced.
fn assert_warm_equals_cold(content: &str, downs: usize) {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fx = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();
    for _ in 0..downs {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    let warm_screen = harness.screen_to_string();

    // Drop every cached per-line layout, then render the same state
    // again: the fast path misses, and the full pipeline rebuilds the
    // window from scratch.
    harness
        .editor_mut()
        .active_state_mut()
        .line_wrap_cache
        .clear();
    harness.render().unwrap();
    let cold_screen = harness.screen_to_string();

    assert_eq!(
        warm_screen, cold_screen,
        "screen diverged after {downs} Downs"
    );
}

#[test]
fn warm_cache_render_matches_cold_render_through_long_line() {
    // 5000 chars wraps to ~65 visual rows at width 80: the walk starts
    // above the long line, traverses through it (viewport fully inside
    // the line), and exits below it.
    let content = mixed_content(5000, "\n");
    for downs in [3, 10, 40, 80] {
        assert_warm_equals_cold(&content, downs);
    }
}

#[test]
fn warm_cache_render_matches_cold_render_crlf() {
    // CRLF exercises the trailing-empty-EOF-row key, which sits one
    // byte earlier than for LF (the Newline token lives on the `\r`).
    let content = mixed_content(3000, "\r\n");
    for downs in [10, 60] {
        assert_warm_equals_cold(&content, downs);
    }
}

#[test]
fn warm_cache_up_down_round_trip_is_stable() {
    let content = mixed_content(5000, "\n");
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fx = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();
    let initial = harness.screen_to_string();

    for _ in 0..30 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    for _ in 0..30 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
    }
    // 30 Down then 30 Up over uniformly wrapped content returns the
    // cursor to its start; the screen must be back to the initial
    // state (no drift from cached-window assembly).
    assert_eq!(harness.cursor_position(), 0);
    assert_eq!(harness.screen_to_string(), initial);
}

#[test]
fn edit_inside_long_line_invalidates_cached_window() {
    // Typing inside the long line bumps the buffer version, so every
    // cached entry goes stale at once; the next frame must re-run the
    // full pipeline and show the edit.
    let content = mixed_content(2000, "\n");
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fx = harness.load_buffer_from_text(&content).unwrap();
    harness.render().unwrap();
    // Move into the long wrapped line (warming the cache), then type.
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.type_text("HELLO").unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("HELLO"),
        "edit did not appear in the rendered screen:\n{screen}"
    );
    let buffer = harness.get_buffer_content().unwrap();
    assert!(buffer.contains("HELLO"));
}
