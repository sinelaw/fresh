//! End-to-end consistency tests for the line-wrap cache.
//!
//! Layers 2 and 5 from the test plan in
//! `docs/internal/line-wrap-cache-plan.md`:
//!
//! * **Layer 2 — mini-pipeline equivalence.**  The cache's miss handler
//!   (`count_visual_rows_via_pipeline`) runs the same four-step pipeline
//!   the renderer runs, scoped to exactly one logical line.  For every
//!   logical line in a rendered buffer, the miss-handler result must
//!   agree with the renderer's per-line row count (number of non-empty
//!   rows between Newlines in the wrapped token stream).
//!
//! * **Layer 5 — render-vs-scroll agreement.**  After the renderer runs,
//!   scroll-math queries for the same logical lines under the same
//!   geometry must return the same row counts the renderer produced.
//!   This closes the loop: the renderer writes entries, scroll math
//!   reads them (or fills on miss with a value that matches what the
//!   renderer would have written).
//!
//! Both layers exist because the cache architecture depends on an
//! invariant — "renderer writes and scroll-math miss-handler always
//! produce the same value for the same (line, geometry)" — that's not
//! locally enforceable inside the cache itself; it requires running the
//! pipeline end-to-end.  A drift between the two would silently corrupt
//! scroll math on any buffer the cache is populated for.

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
/// single-row and multi-row per logical line.  Includes word-wrapped
/// realistic text (different from the homogeneous-character buffer in
/// `scroll_wrapped_reach_last_line.rs`) so the count path is the
/// word-boundary one rather than the hard-cap one.
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
        lines.push(String::new()); // blank separator
    }
    lines.push("final line".to_string());
    lines.join("\n")
}

/// Read the LineWrapKey inputs the renderer and scroll-math paths both
/// build from the harness's current state.  These match the key the
/// renderer's writeback in `view_data::build_view_data` constructs.
fn current_keys(harness: &EditorTestHarness, line_start: usize) -> (LineWrapKey, LineWrapKey) {
    let (effective_width, gutter_width, hanging_indent, wrap_column) = {
        let editor = harness.editor();
        let viewport = editor.active_viewport();
        let state = editor.active_state();
        let gutter = viewport.gutter_width(&state.buffer) as u16;
        // Viewport.width excludes the scrollbar column; the renderer's
        // effective_width is `viewport.width - 1` reserved for EOL
        // cursor, then apply_wrapping_transform subtracts gutter again
        // internally.  Scroll math matches via
        // WrapConfig::first_line_width + gutter.
        let content_width = viewport.width as usize;
        let effective = content_width.saturating_sub(1).max(1);
        let wrap_col = viewport.wrap_column.map(|c| c as u32);
        (
            effective as u32,
            gutter,
            viewport.wrap_indent,
            wrap_col,
        )
    };
    let pipeline_ver = {
        let editor = harness.editor();
        let state = editor.active_state();
        pipeline_inputs_version(
            state.buffer.version(),
            state.soft_breaks.version(),
            state.conceals.version(),
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

/// Retrieve an editor `fresh::view::viewport::Viewport` reference to
/// read geometry. Kept as a tiny helper so the test is resilient to
/// future harness changes.
fn read_cache_entry(harness: &EditorTestHarness, key: &LineWrapKey) -> Option<u32> {
    let editor = harness.editor();
    let state = editor.active_state();
    state.line_wrap_cache.get(key)
}

/// Walk buffer lines and return `Vec<(line_start, line_text)>` for the
/// first `max_lines` logical lines.  Used to drive per-line checks.
fn enumerate_lines(harness: &mut EditorTestHarness, max_lines: usize) -> Vec<(usize, String)> {
    let mut out = Vec::new();
    let editor = harness.editor_mut();
    let state = editor.active_state_mut();
    let mut iter = state.buffer.line_iterator(0, 80);
    while let Some((start, content)) = iter.next_line() {
        let text = content
            .trim_end_matches(['\n', '\r'])
            .to_string();
        out.push((start, text));
        if out.len() >= max_lines {
            break;
        }
    }
    out
}

/// Layer 5: after a render, every visible-line entry in the cache must
/// equal a fresh `count_visual_rows_for_text` on that line's content
/// under the same geometry.  This is the core "no drift between the two
/// writers" check: if the renderer's walk-the-wrapped-tokens writeback
/// ever disagrees with the miss handler's count-from-one-Text-token
/// computation, this test fails.
#[test]
fn render_writeback_values_match_fresh_recompute() {
    let widths: [u16; 5] = [50, 70, 90, 110, 140];
    for &width in &widths {
        let mut harness =
            EditorTestHarness::with_config(width, TERMINAL_HEIGHT, config_with_wrap())
                .expect("harness");
        let fixture = harness
            .load_buffer_from_text(&mixed_buffer())
            .expect("load");
        std::mem::forget(fixture); // keep the tempfile alive
        harness.render().expect("render");
        harness
            .send_key(KeyCode::Home, KeyModifiers::CONTROL)
            .expect("ctrl+home");
        harness.render().expect("render");

        // Enumerate the first N logical lines; the renderer touches
        // lines in the visible window, so at least the top few should
        // have cache entries.
        let lines = enumerate_lines(&mut harness, 30);

        let mut checked = 0usize;
        for (line_start, line_text) in &lines {
            // The renderer writes under (compose, source) pairs.  We
            // query the Source key; it should be present for every
            // visible line.
            let (compose_key, source_key) = current_keys(&harness, *line_start);
            let cached_compose = read_cache_entry(&harness, &compose_key);
            let cached_source = read_cache_entry(&harness, &source_key);

            // Skip lines the renderer never reached (below visible area
            // at initial scroll position).  Those will be cache-missed
            // on demand by scroll-math — checked in the next test.
            if cached_compose.is_none() && cached_source.is_none() {
                continue;
            }
            checked += 1;

            // Fresh recompute under the same geometry.
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

/// Layer 2: mini-pipeline equivalence via the scroll-math read path.
///
/// Drive the harness through a short scroll sequence that exercises
/// the cache's miss handler (scrollbar-drag-to-bottom performs a
/// full-buffer row-map walk, filling uncached entries via the
/// miss handler in `build_visual_row_map`).  After the drag, check
/// that every cached entry still agrees with a fresh recompute —
/// this ties the miss-handler value to the pure `count_visual_rows
/// _for_text` function used by Layer 4 / 5.
#[test]
fn scroll_math_miss_handler_matches_fresh_recompute() {
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

        // Drag the scrollbar top-to-bottom: triggers a full
        // build_visual_row_map walk in scrollbar_math, which populates
        // the cache for every logical line via the miss handler.
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

        // Enumerate all lines; now every line should either have a
        // (Source or Compose) entry thanks to the drag sweep.
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

/// Layer 6: terminal resize mid-session.  Resize changes
/// `effective_width`, which is part of the cache key.  After the
/// resize, existing entries (under the old width) must not be
/// returned for queries at the new width — fresh entries are
/// computed and populated.  Tests that the width dimension is
/// correctly honored as a separation dimension.
#[test]
fn resize_produces_fresh_cache_entries_at_new_width() {
    let mut harness =
        EditorTestHarness::with_config(80, TERMINAL_HEIGHT, config_with_wrap()).expect("harness");
    let fixture = harness
        .load_buffer_from_text(&mixed_buffer())
        .expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");

    // Capture geometry BEFORE and sample a line start.
    let sample_line = {
        let lines = enumerate_lines(&mut harness, 10);
        lines[0].0
    };
    let (_compose_before, source_before) = current_keys(&harness, sample_line);
    let v_before = read_cache_entry(&harness, &source_before);

    // Resize to a narrower width.  This changes both width AND gutter
    // (indirectly through viewport.width).
    harness.resize(50, TERMINAL_HEIGHT).expect("resize");
    harness.render().expect("render");

    let (_compose_after, source_after) = current_keys(&harness, sample_line);
    // The effective width must have changed; otherwise the resize
    // didn't propagate to viewport.width and this test doesn't
    // exercise what it claims.
    assert_ne!(
        source_before.effective_width, source_after.effective_width,
        "resize didn't change effective_width — test setup is broken"
    );

    // A query under the OLD key must still return its stored value
    // (it's in the map; it just won't be queried by active consumers).
    if let Some(v) = v_before {
        assert_eq!(read_cache_entry(&harness, &source_before), Some(v));
    }

    // A query under the NEW key hits fresh entries written by the
    // re-render.  The value must match a fresh recompute at the new
    // width.
    let post = read_cache_entry(&harness, &source_after);
    if let Some(v) = post {
        // Need the line text to recompute.  Enumerate lines fresh.
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

/// Layer 6: many small edits in sequence.  Each edit bumps
/// `buffer.version()`, so each render populates entries under a fresh
/// key.  The cache should stay bounded and never return a stale value
/// under the current version.
#[test]
fn repeated_edits_keep_cache_consistent() {
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

    // Rapid-fire small edits.  Each toggles buffer version, so the
    // cache should never return a pre-edit value after the edit.
    for i in 0..30usize {
        harness.type_text(&format!("{}", i % 10)).expect("type");
        harness.render().expect("render");

        // Probe all currently-cached lines against a fresh recompute.
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

/// Layer 5b: after an edit, stale entries must never be returned.
///
/// Flow: load buffer, render (populates cache at version V).  Type a
/// character (buffer version bumps to V+1).  Render again.  Now the
/// cache holds entries under V AND V+1.  Probe a per-line key under
/// V+1 and require that either it's present with a value matching a
/// fresh recompute, OR it's absent (miss path will fill it next
/// access).  Under no circumstance should a V-keyed entry leak into
/// a V+1 query.
#[test]
fn edit_invalidates_cache_visibly() {
    let width: u16 = 80;
    let mut harness = EditorTestHarness::with_config(width, TERMINAL_HEIGHT, config_with_wrap())
        .expect("harness");
    let fixture = harness
        .load_buffer_from_text(&mixed_buffer())
        .expect("load");
    std::mem::forget(fixture);
    harness.render().expect("render");

    // Move to end of first line and insert a char to bump buffer version.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .expect("ctrl+home");
    harness.send_key(KeyCode::End, KeyModifiers::NONE).expect("end");
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
