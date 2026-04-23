//! Performance regression test for a buffer consisting of a single
//! very long line.
//!
//! User-reported symptom (on top of the correctness fixes for
//! scroll_wrapped_reach_last_line.rs):
//!
//!   "i created a file of a single very very long line, pushed it to
//!    long-file branch. when i scroll it in prev version of fresh it
//!    works quickly but wrong (reaches past the end, jumps, etc); when
//!    I use this branch that you built — it is extremely very very
//!    very slow."
//!
//! Root cause of the slowdown: switching the scroll math from
//! `wrap_line` (pure char-width, O(n)) to `apply_wrapping_transform`
//! (word-boundary with grapheme segmentation + UAX #29 word bounds,
//! substantially more expensive per call) makes correctness right but
//! every scroll event that re-runs the wrap on the same line pays the
//! full cost. The scrollbar-math path is cached (commit 0469978), but
//! the Viewport-side scroll handlers (mouse wheel, PageDown) pass
//! `None` for the cache, so they recompute on every event. On a buffer
//! of one 500KB-ish line, that's visibly slow.
//!
//! The test exists to make that slowdown fail in CI rather than in a
//! user's face.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::time::{Duration, Instant};

/// A "single very long line" buffer.  Length chosen to be big enough
/// that an O(n²) per-tick wrap (which is what the user saw in the
/// unfixed branch) shows up well above the noise floor in CI debug
/// builds, but small enough that the first cache-miss wrap pass still
/// fits the budget:
///
///   - ~50KB = ~8K words. No newlines anywhere.
///   - Mixed words+spaces so `apply_wrapping_transform`'s word-
///     boundary logic has boundaries to probe (pure "aaaa…" would
///     short-circuit the expensive path).
///
/// Before plumbing `LineWrapCache` into the viewport scroll path,
/// every mouse-wheel tick ran `apply_wrapping_transform` over the
/// whole line — 40 ticks × O(n²) per-tick wrap cost blew way past
/// the budget (24 minutes in one CI-like run). With the cache wired,
/// only the first tick pays the wrap cost; subsequent ticks are O(1)
/// cache hits.
fn build_long_single_line() -> String {
    let word_pool = [
        "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
        "lambda", "mu", "nu", "xi", "omicron", "pi", "rho", "sigma", "tau", "upsilon",
    ];
    let mut s = String::with_capacity(50_000);
    let mut i: usize = 0;
    while s.len() < 50_000 {
        if !s.is_empty() {
            s.push(' ');
        }
        s.push_str(word_pool[i % word_pool.len()]);
        i += 1;
    }
    assert!(!s.contains('\n'), "fixture must be a single line");
    s
}

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Ceiling on how long a fixed batch of mouse-wheel scrolls may take
/// on a single very long line. 12s is comfortably above any plausible
/// "fast" path (local runs finish well under 1s when the hot path is
/// cached / cheap) while being a clean fail signal when the path is
/// running `apply_wrapping_transform` over a 200KB line per tick.
///
/// This is intentionally NOT tight: CI machines vary; the goal is to
/// catch obvious O(line_length × ticks) regressions, not to benchmark.
const SCROLL_BUDGET: Duration = Duration::from_secs(12);

/// Number of mouse-wheel ticks to perform. Enough to saturate the
/// viewport advance several times so the hot paths
/// (`scroll_down_visual`, `apply_visual_scroll_limit`,
/// `find_max_visual_scroll_position`) all get exercised.
const N_TICKS: usize = 40;

#[test]
fn mouse_wheel_on_single_long_line_stays_fast() {
    const WIDTH: u16 = 100;
    const HEIGHT: u16 = 30;

    let mut harness =
        EditorTestHarness::with_config(WIDTH, HEIGHT, config_with_wrap()).expect("harness");

    let content = build_long_single_line();
    let fixture = harness
        .load_buffer_from_text(&content)
        .expect("load_buffer_from_text");
    std::mem::forget(fixture);
    harness.render().expect("render");

    // Start at the very top so the scroll math has to traverse the
    // line's wrapped segments forward rather than finishing
    // immediately.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    harness.render().expect("render");

    let (content_first_row, _) = harness.content_area_rows();
    let scroll_col = WIDTH / 2;
    let scroll_row = content_first_row as u16 + 5;

    let start = Instant::now();
    for _ in 0..N_TICKS {
        harness
            .mouse_scroll_down(scroll_col, scroll_row)
            .expect("mouse_scroll_down");
    }
    let elapsed = start.elapsed();

    assert!(
        elapsed < SCROLL_BUDGET,
        "mouse wheel on single-long-line buffer took {:?} for {} ticks \
         (budget: {:?}).  If this is a real slowdown, the scroll hot path \
         is likely running apply_wrapping_transform uncached on every \
         tick — plumb LineWrapCache into Viewport::count_visual_rows_for_line \
         callers.",
        elapsed,
        N_TICKS,
        SCROLL_BUDGET,
    );
}

#[test]
fn scrollbar_drag_on_single_long_line_stays_fast() {
    const WIDTH: u16 = 100;
    const HEIGHT: u16 = 30;

    let mut harness =
        EditorTestHarness::with_config(WIDTH, HEIGHT, config_with_wrap()).expect("harness");

    let content = build_long_single_line();
    let fixture = harness
        .load_buffer_from_text(&content)
        .expect("load_buffer_from_text");
    std::mem::forget(fixture);
    harness.render().expect("render");

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    harness.render().expect("render");

    let scrollbar_col = WIDTH - 1;
    let (content_first_row, content_last_row) = harness.content_area_rows();

    // Drag the thumb from top to bottom. Internally this performs
    // many mouse-move events; the first triggers a full
    // `build_visual_row_map` walk that fills the cache, and all
    // subsequent move events should be O(1) on that cache.
    //
    // NB: for a single-line buffer, `build_visual_row_map` wraps
    // exactly ONE logical line — the cache holds at most one entry
    // for this geometry. The cost should be dominated by the single
    // initial wrap, not the hundreds of per-mouse-move computations
    // the pre-cache version did.
    let start = Instant::now();
    harness
        .mouse_drag(
            scrollbar_col,
            content_first_row as u16,
            scrollbar_col,
            content_last_row as u16,
        )
        .expect("mouse_drag");
    let elapsed = start.elapsed();

    assert!(
        elapsed < SCROLL_BUDGET,
        "scrollbar drag on single-long-line buffer took {:?} (budget: {:?}). \
         If this is slow, LineWrapCache isn't being hit — `build_visual_row_map` \
         is re-running the wrap on every drag event instead of reading from cache.",
        elapsed,
        SCROLL_BUDGET,
    );
}
