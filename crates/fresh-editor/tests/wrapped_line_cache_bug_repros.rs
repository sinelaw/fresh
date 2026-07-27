//! Regression tests for the confirmed findings of the cached-window
//! fast-path review.  Each test asserts the CORRECT behavior; all four
//! findings are fixed and these run live.
//!
//! Observability status (what a user can actually see):
//!
//! * Finding 1 was the only finding with a USER-OBSERVABLE symptom:
//!   navigation through a long wrapped line mid-file was several times
//!   slower than the same line at end-of-file, because the fast path
//!   never engaged there.  FIXED: the cached-window walk is now
//!   row-bounded instead of demanding `visible_count + 4` logical
//!   lines; `midfile_long_line_navigation_matches_eof_throughput` is
//!   the live end-to-end regression test (keystrokes in, throughput
//!   out).
//!
//! * Findings 2-4 were LATENT contract violations (end-to-end probing
//!   found no rendering difference while they existed — masked by
//!   redundant downstream mappings and fallback-on-miss).  FIXED:
//!   the completeness check now requires the exact newline-token
//!   offset per line ending (2), `extend_streaming` bumps
//!   `buffer.version()` (3), and `prev_grapheme_boundary` returns the
//!   containing cluster's start for mid-code-point input (4).  These
//!   tests pin the contracts at the level where they are provable.
//!
//! Finding 1 — mid-file fast-path gap: the cached-window walk demands a
//! cache entry for `visible_count + 4` *logical lines*, but the full
//! pipeline spends that same budget in `MAX_SAFE_LINE_WIDTH`-char
//! chunks, so with the viewport inside a long line mid-file the
//! writeback never caches the trailing lines the walk demands and the
//! fast path never engages.
//!
//! Finding 2 — completeness-check slack: the fast path rejects
//! truncated entries via `last_source_byte + 2 >= expected_end`; the
//! 2-byte slack (needed for CRLF) also admits an LF entry missing
//! exactly its trailing newline cell, which corrupts the served row's
//! end-of-line mapping.
//!
//! Findings 3 (`extend_streaming` version bump) and 4
//! (`prev_grapheme_boundary` mid-code-point) are fixed in the base PR;
//! their regression tests live in
//! `streaming_and_grapheme_regression_tests.rs`.

mod common;

use common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::view::line_wrap_cache::{pipeline_inputs_version, CacheViewMode, LineWrapKey};
use std::time::{Duration, Instant};

/// Down-arrow keypresses (each including a render, like an interactive
/// session) completed within `budget`, after a warm-up so the first
/// paint and cache fill are excluded.
fn moves_in_budget(content: &str, budget: Duration) -> u32 {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fx = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap();
    for _ in 0..3 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    let start = Instant::now();
    let mut moves = 0u32;
    while start.elapsed() < budget {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        moves += 1;
    }
    moves
}

/// Finding 1, end-to-end observable symptom: navigating the SAME long
/// wrapped line must not become several times slower just because
/// other lines follow it in the file.  Today the mid-file shape never
/// engages the cached-window fast path, so its throughput is ~2.5x
/// lower; the 1.75x threshold leaves margin on both sides (post-fix
/// the ratio is ~1.0).
#[test]
fn midfile_long_line_navigation_matches_eof_throughput() {
    let eof_shape = format!("{}\n", "x".repeat(200_000));
    let mut midfile_shape = format!("{}\n", "x".repeat(200_000));
    for i in 0..40 {
        midfile_shape.push_str(&format!("short line {i}\n"));
    }

    let budget = Duration::from_millis(2500);
    let eof_moves = moves_in_budget(&eof_shape, budget);
    let midfile_moves = moves_in_budget(&midfile_shape, budget);

    assert!(
        eof_moves >= 4,
        "degenerate run: only {eof_moves} moves completed in the budget"
    );
    let ratio = eof_moves as f64 / midfile_moves.max(1) as f64;
    assert!(
        ratio <= 1.75,
        "navigating a 200 KB wrapped line mid-file is {ratio:.2}x slower than \
         the same line at EOF ({midfile_moves} vs {eof_moves} moves in {budget:?}) — \
         the cached-window fast path is not engaging for the mid-file shape"
    );
}

/// Build the Source-mode cache key the renderer's writeback and the
/// fast-path walk both construct for `line_start` under the harness's
/// current geometry (mirrors `current_keys` in
/// `e2e/line_wrap_cache_consistency.rs`).
fn source_key(harness: &EditorTestHarness, line_start: usize) -> LineWrapKey {
    let editor = harness.editor();
    let viewport = editor.active_viewport();
    let state = editor.active_state();
    let gutter = viewport.gutter_width(&state.buffer) as u16;
    let effective = (viewport.width as usize).saturating_sub(1).max(1);
    LineWrapKey {
        pipeline_inputs_version: pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
            state.virtual_texts.version(),
        ),
        view_mode: CacheViewMode::Source,
        line_start,
        effective_width: effective as u32,
        gutter_width: gutter,
        wrap_column: viewport.wrap_column.map(|c| c as u32),
        hanging_indent: viewport.wrap_indent,
        line_wrap_enabled: true,
        grid_wrap: false,
        tab_size: state.buffer_settings.tab_size as u16,
        cursor_sig: 0,
    }
}

/// Finding 2: the fast path must not serve an entry whose last row is
/// missing its trailing newline cell, but the completeness check's
/// 2-byte slack (needed for CRLF) accepts exactly that shape — the one
/// a chunk-budget truncation at a line's final character produces.
///
/// Detection: if the incomplete entry is rejected, the frame falls back
/// to the full pipeline, whose writeback REPLACES the entry with the
/// complete form.  So after a render, the cached entry carrying the
/// newline cell again is the signature of correct behavior; the entry
/// remaining newline-less proves the fast path accepted and served it.
/// (Downstream click/caret paths happen to mask visible corruption for
/// this minimal fixture — the served window is still wrong: its row
/// text and byte mappings disagree with the buffer.)
#[test]
fn fast_path_must_not_serve_entry_missing_its_newline_cell() {
    let content = "abcdef\nsecond line\n";
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fx = harness.load_buffer_from_text(content).unwrap();
    harness.render().unwrap(); // writeback populates complete entries

    // Doctor line 0's entry: strip the trailing newline cell from its
    // last row — exactly the shape a chunk-budget truncation at the
    // line's final character produces.  `last_source_byte` becomes 5
    // ('f'); `expected_end` is 7 (start of line 1); 5 + 2 >= 7 passes
    // the slack, so the fast path serves this entry.
    let key = source_key(&harness, 0);
    let doctored = {
        let entry = harness
            .editor()
            .active_state()
            .line_wrap_cache
            .get(&key)
            .expect("line 0 must be cached after a render");
        let mut rows = (*entry).clone();
        let last = rows.last_mut().expect("entry has rows");
        assert!(
            last.text.ends_with('\n'),
            "precondition: complete entry carries the newline cell"
        );
        last.text.pop();
        let removed_char_idx = last.char_source_bytes.len() - 1;
        last.char_source_bytes.pop();
        last.char_styles.pop();
        last.char_visual_cols.pop();
        while last.visual_to_char.last() == Some(&removed_char_idx) {
            last.visual_to_char.pop();
        }
        last.ends_with_newline = false;
        rows
    };
    harness
        .editor_mut()
        .active_state_mut()
        .line_wrap_cache
        .put(key, std::sync::Arc::new(doctored));

    // Render.  A correct implementation rejects the incomplete entry,
    // falls back to the full pipeline, and the writeback replaces the
    // entry with its complete form (newline cell restored).
    harness.render().unwrap();

    let entry_after = harness
        .editor()
        .active_state()
        .line_wrap_cache
        .get(&source_key(&harness, 0))
        .expect("entry present");
    let last = entry_after.last().unwrap();
    assert!(
        last.ends_with_newline && last.text.ends_with('\n'),
        "the incomplete (newline-less) entry survived the render — the \
         fast path's completeness check accepted it and served a window \
         whose row text/byte mappings disagree with the buffer"
    );
}
