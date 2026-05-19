//! Migration of `tests/e2e/line_wrap_cache_consistency.rs` —
//! end-to-end consistency tests for the `LineWrapCache`.
//!
//! Layers 2, 5, 5b, and 6 from the test plan in
//! `docs/internal/line-wrap-cache-plan.md` are preserved here.
//! Briefly:
//!
//!   * **Layer 5 (`migrated_render_writeback_values_match_fresh_recompute`).**
//!     After a render, every visible-line entry in the cache must
//!     equal a fresh `count_visual_rows_for_text` on that line's
//!     content under the same geometry. The "no drift between the
//!     two writers" invariant.
//!
//!   * **Layer 2 (`migrated_scroll_math_miss_handler_matches_fresh_recompute`).**
//!     Drag-to-bottom triggers `build_visual_row_map`'s full
//!     buffer walk, populating every line via the miss handler.
//!     Each entry must then agree with a fresh recompute — ties
//!     the miss-handler path to the pure
//!     `count_visual_rows_for_text` value.
//!
//!   * **Layer 6 (`migrated_resize_produces_fresh_cache_entries_at_new_width`).**
//!     Resizing changes `effective_width`, a cache-key dimension.
//!     The OLD-keyed entry must still be retrievable; NEW-keyed
//!     entries written by re-render must match a fresh recompute
//!     at the new width.
//!
//!   * **Layer 6 (`migrated_repeated_edits_keep_cache_consistent`).**
//!     Many rapid small edits bump `buffer.version()` each time —
//!     no post-edit query may return a pre-edit (stale) cached
//!     value.
//!
//!   * **Layer 5b (`migrated_edit_invalidates_cache_visibly`).**
//!     A single edit followed by a render must leave no stale
//!     V-keyed entries servicing V+1 queries.
//!
//! ## Harness-direct pattern
//!
//! All five claims read internal state on the cache (`LineWrapKey`,
//! `CacheViewMode`, `pipeline_inputs_version`,
//! `count_visual_rows_for_text`) — these have no `EditorTestApi`
//! projection. The migration takes the harness-direct path with
//! `fresh::view::line_wrap_cache::*` imports (the harness-only
//! escape hatch documented in
//! `scripts/check-semantic-test-isolation.sh`).
//!
//! Source: `tests/e2e/line_wrap_cache_consistency.rs` (5 tests
//! migrated; no tests deferred).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::view::line_wrap_cache::{
    count_visual_rows_for_text, pipeline_inputs_version, CacheViewMode, LineWrapKey,
};

const TERMINAL_HEIGHT: u16 = 24;

fn config_with_wrap() -> Config {
    let mut config = Config::default();
    config.editor.line_wrap = true;
    config
}

/// Buffer with a mix of short and long lines to exercise both
/// single-row and multi-row per logical line. Includes word-wrapped
/// realistic text so the count path is the word-boundary one
/// rather than the hard-cap one.
fn mixed_buffer() -> String {
    let short_lines = [
        "Line 1: short.",
        "",
        "// A comment",
        "{",
        "    let x = 1;",
        "}",
    ];
    let para: String = (0..20)
        .map(|i| format!("word{:02}", i))
        .collect::<Vec<_>>()
        .join(" ");
    let mut lines: Vec<String> = short_lines.iter().map(|s| s.to_string()).collect();
    for _ in 0..5 {
        lines.push(para.clone());
        lines.push(String::new());
    }
    lines.push("final line".to_string());
    lines.join("\n")
}

/// Read the LineWrapKey inputs the renderer and scroll-math paths
/// both build from the harness's current state.
fn current_keys(harness: &EditorTestHarness, line_start: usize) -> (LineWrapKey, LineWrapKey) {
    let (effective_width, gutter_width, hanging_indent, wrap_column) = {
        let editor = harness.editor();
        let viewport = editor.active_viewport();
        let state = editor.active_state();
        let gutter = viewport.gutter_width(&state.buffer) as u16;
        let content_width = viewport.width as usize;
        let effective = content_width.saturating_sub(1).max(1);
        let wrap_col = viewport.wrap_column.map(|c| c as u32);
        (effective as u32, gutter, viewport.wrap_indent, wrap_col)
    };
    let pipeline_ver = {
        let editor = harness.editor();
        let state = editor.active_state();
        pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
            state.virtual_texts.version(),
        )
    };
    let compose = LineWrapKey {
        pipeline_inputs_version: pipeline_ver,
        view_mode: CacheViewMode::Compose,
        line_start,
        effective_width,
        gutter_width,
        wrap_column,
        hanging_indent,
        line_wrap_enabled: true,
    };
    let source = LineWrapKey {
        view_mode: CacheViewMode::Source,
        ..compose
    };
    (compose, source)
}

/// Read a cache entry's row count.
fn read_cache_entry(harness: &EditorTestHarness, key: &LineWrapKey) -> Option<u32> {
    let editor = harness.editor();
    let state = editor.active_state();
    state.line_wrap_cache.get(key).map(|v| v.len() as u32)
}

/// Walk buffer lines and return `Vec<(line_start, line_text)>`.
fn enumerate_lines(harness: &mut EditorTestHarness, max_lines: usize) -> Vec<(usize, String)> {
    let mut out = Vec::new();
    let editor = harness.editor_mut();
    let state = editor.active_state_mut();
    let mut iter = state.buffer.line_iterator(0, 80);
    while let Some((start, content)) = iter.next_line() {
        let text = content.trim_end_matches(['\n', '\r']).to_string();
        out.push((start, text));
        if out.len() >= max_lines {
            break;
        }
    }
    out
}

#[test]
fn migrated_render_writeback_values_match_fresh_recompute() {
    // Original: `render_writeback_values_match_fresh_recompute`.
    let widths: [u16; 5] = [50, 70, 90, 110, 140];
    for &width in &widths {
        let mut harness =
            EditorTestHarness::with_config(width, TERMINAL_HEIGHT, config_with_wrap())
                .expect("harness");
        let fixture = harness
            .load_buffer_from_text(&mixed_buffer())
            .expect("load");
        std::mem::forget(fixture);
        harness.render().expect("render");
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .expect("ctrl+home");
        harness.render().expect("render");

        let lines = enumerate_lines(&mut harness, 30);

        let mut checked = 0usize;
        for (line_start, line_text) in &lines {
            let (compose_key, source_key) = current_keys(&harness, *line_start);
            let cached_compose = read_cache_entry(&harness, &compose_key);
            let cached_source = read_cache_entry(&harness, &source_key);

            if cached_compose.is_none() && cached_source.is_none() {
                continue;
            }
            checked += 1;

            let fresh = count_visual_rows_for_text(
                line_text,
                compose_key.effective_width as usize,
                compose_key.gutter_width as usize,
                compose_key.hanging_indent,
            );
            if let Some(v) = cached_compose {
                assert_eq!(
                    v, fresh,
                    "[w={width}] renderer writeback (Compose) disagrees with fresh recompute for \
                     line_start={} line_text={:?}",
                    line_start, line_text
                );
            }
            if let Some(v) = cached_source {
                assert_eq!(
                    v, fresh,
                    "[w={width}] renderer writeback (Source) disagrees with fresh recompute for \
                     line_start={} line_text={:?}",
                    line_start, line_text
                );
            }
        }
        assert!(
            checked > 0,
            "[w={width}] no cache entries observed after render — \
             writeback path may have silently stopped populating",
        );
    }
}

#[test]
fn migrated_scroll_math_miss_handler_matches_fresh_recompute() {
    // Original: `scroll_math_miss_handler_matches_fresh_recompute`.
    let widths: [u16; 5] = [50, 70, 90, 110, 140];
    for &width in &widths {
        let mut harness =
            EditorTestHarness::with_config(width, TERMINAL_HEIGHT, config_with_wrap())
                .expect("harness");
        let fixture = harness
            .load_buffer_from_text(&mixed_buffer())
            .expect("load");
        std::mem::forget(fixture);
        harness.render().expect("render");

        let scrollbar_col = width - 1;
        let (content_first_row, content_last_row) = harness.content_area_rows();
        harness
            .mouse_drag(
                scrollbar_col,
                content_first_row as u16,
                scrollbar_col,
                content_last_row as u16,
            )
            .expect("drag");
        harness.render().expect("render");

        let lines = enumerate_lines(&mut harness, 1000);

        let mut checked = 0usize;
        for (line_start, line_text) in &lines {
            let (compose_key, source_key) = current_keys(&harness, *line_start);
            let cached_source = read_cache_entry(&harness, &source_key);
            let cached_compose = read_cache_entry(&harness, &compose_key);
            if cached_source.is_none() && cached_compose.is_none() {
                continue;
            }
            checked += 1;
            let fresh = count_visual_rows_for_text(
                line_text,
                source_key.effective_width as usize,
                source_key.gutter_width as usize,
                source_key.hanging_indent,
            );
            if let Some(v) = cached_source {
                assert_eq!(
                    v, fresh,
                    "[w={width}] miss-handler (Source) disagrees with fresh recompute for \
                     line_start={} line_text={:?}",
                    line_start, line_text
                );
            }
            if let Some(v) = cached_compose {
                assert_eq!(
                    v, fresh,
                    "[w={width}] renderer-written (Compose) disagrees with fresh recompute for \
                     line_start={} line_text={:?}",
                    line_start, line_text
                );
            }
        }
        assert!(
            checked > 0,
            "[w={width}] drag sweep populated 0 cache entries — setup issue",
        );
    }
}

#[test]
fn migrated_resize_produces_fresh_cache_entries_at_new_width() {
    // Original: `resize_produces_fresh_cache_entries_at_new_width`.
    let mut harness =
        EditorTestHarness::with_config(80, TERMINAL_HEIGHT, config_with_wrap()).expect("harness");
    let fixture = harness
        .load_buffer_from_text(&mixed_buffer())
        .expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");

    let sample_line = {
        let lines = enumerate_lines(&mut harness, 10);
        lines[0].0
    };
    let (_compose_before, source_before) = current_keys(&harness, sample_line);
    let v_before = read_cache_entry(&harness, &source_before);

    harness.resize(50, TERMINAL_HEIGHT).expect("resize");
    harness.render().expect("render");

    let (_compose_after, source_after) = current_keys(&harness, sample_line);
    assert_ne!(
        source_before.effective_width, source_after.effective_width,
        "resize didn't change effective_width — test setup is broken"
    );

    if let Some(v) = v_before {
        assert_eq!(read_cache_entry(&harness, &source_before), Some(v));
    }

    let post = read_cache_entry(&harness, &source_after);
    if let Some(v) = post {
        let lines = enumerate_lines(&mut harness, 10);
        let text = &lines[0].1;
        let fresh = count_visual_rows_for_text(
            text,
            source_after.effective_width as usize,
            source_after.gutter_width as usize,
            source_after.hanging_indent,
        );
        assert_eq!(v, fresh, "post-resize entry disagrees with fresh recompute");
    }
}

#[test]
fn migrated_repeated_edits_keep_cache_consistent() {
    // Original: `repeated_edits_keep_cache_consistent`.
    let mut harness =
        EditorTestHarness::with_config(80, TERMINAL_HEIGHT, config_with_wrap()).expect("harness");
    let fixture = harness
        .load_buffer_from_text(&mixed_buffer())
        .expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");

    for i in 0..30usize {
        harness.type_text(&format!("{}", i % 10)).expect("type");
        harness.render().expect("render");

        let lines = enumerate_lines(&mut harness, 20);
        for (line_start, line_text) in &lines {
            let (_compose_key, source_key) = current_keys(&harness, *line_start);
            if let Some(v) = read_cache_entry(&harness, &source_key) {
                let fresh = count_visual_rows_for_text(
                    line_text,
                    source_key.effective_width as usize,
                    source_key.gutter_width as usize,
                    source_key.hanging_indent,
                );
                assert_eq!(
                    v, fresh,
                    "iteration {i}: stale value at line_start={line_start}, \
                     line_text={line_text:?}, cached={v}, fresh={fresh}"
                );
            }
        }
    }
}

#[test]
fn migrated_edit_invalidates_cache_visibly() {
    // Original: `edit_invalidates_cache_visibly`.
    let width: u16 = 80;
    let mut harness = EditorTestHarness::with_config(width, TERMINAL_HEIGHT, config_with_wrap())
        .expect("harness");
    let fixture = harness
        .load_buffer_from_text(&mixed_buffer())
        .expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    harness
        .send_key(KeyCode::End, KeyModifiers::NONE)
        .expect("end");
    harness.type_text("X").expect("type X");
    harness.render().expect("render");

    let lines = enumerate_lines(&mut harness, 20);
    let mut checked = 0usize;
    for (line_start, line_text) in &lines {
        let (_compose_key, source_key) = current_keys(&harness, *line_start);
        if let Some(v) = read_cache_entry(&harness, &source_key) {
            let fresh = count_visual_rows_for_text(
                line_text,
                source_key.effective_width as usize,
                source_key.gutter_width as usize,
                source_key.hanging_indent,
            );
            assert_eq!(
                v, fresh,
                "post-edit cache returned stale value: line_start={line_start}, \
                 line_text={line_text:?}, cached={v}, fresh={fresh}"
            );
            checked += 1;
        }
    }
    assert!(
        checked > 0,
        "no cache entries under current version — edit may have cleared the cache \
         but the renderer didn't repopulate",
    );
}

/// Anti-test: drop the render call before probing the cache. On a
/// freshly-constructed harness with a loaded buffer but NO render,
/// the cache must be empty (no entries have been written yet) —
/// proving the positive `render_writeback_values_match_fresh_recompute`
/// claim depends on the render writeback actually populating the
/// cache, not on the cache being pre-loaded from elsewhere.
#[test]
fn anti_cache_is_empty_before_first_render() {
    let mut harness =
        EditorTestHarness::with_config(80, TERMINAL_HEIGHT, config_with_wrap()).expect("harness");
    let fixture = harness
        .load_buffer_from_text(&mixed_buffer())
        .expect("load");
    std::mem::forget(fixture);
    // No render call here — that's the load-bearing step we drop.

    let lines = enumerate_lines(&mut harness, 30);
    let mut populated = 0usize;
    for (line_start, _) in &lines {
        let (compose_key, source_key) = current_keys(&harness, *line_start);
        if read_cache_entry(&harness, &compose_key).is_some()
            || read_cache_entry(&harness, &source_key).is_some()
        {
            populated += 1;
        }
    }
    assert_eq!(
        populated, 0,
        "anti: without an initial render, the line-wrap cache must \
         be empty (no entries written yet). Got {populated} populated \
         line(s); positive test relies on render writeback being the \
         source of those entries."
    );
}
