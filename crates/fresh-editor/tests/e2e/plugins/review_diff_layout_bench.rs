//! Benchmark: what a large review costs, and whether that cost tracks the
//! size of the review.
//!
//! Ignored by default; run with `--ignored --nocapture --test-threads=1`.
//! The assertion-carrying version of this is
//! `plugin_snapshot_scaling` — counters, which hold a line in CI where
//! timings cannot. This file is for reading actual durations while
//! working on the render and tick paths.
//!
//! Release, per operation (20 samples each; harness frames are heavier
//! than a real terminal's, so read the ratios):
//!
//! | laid out | frame | cursor move | open  |
//! |----------|-------|-------------|-------|
//! | 20k      | 11ms  | 35ms        | 1.3s  |
//! | 100k     | 12ms  | 35ms        | 5.7s  |
//!
//! Interaction is flat in the size of the review; only the one-time
//! layout still tracks it. Getting there took three fixes, each found by
//! measuring rather than reading:
//!
//! * the plugin state snapshot deep-copied every text property of every
//!   buffer on every tick, and a review carries one per row;
//! * rainbow-bracket colorization republished its overlays across the
//!   whole buffer on every frame;
//! * the overlay set kept a globally sorted vector, so every add and
//!   every namespace clear touched the whole set.
//!
//! Before them, a cursor move over a 100k-line review cost 3.8s and the
//! review plugin capped its own layout to compensate. The cap is gone.
//!
//! The control case (`bench_plain_buffer_frame_cost`) is what made the
//! search tractable: an ordinary buffer of the same line counts was flat
//! throughout, which ruled out the renderer in one run and pointed at
//! what the review adds on top of it.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::Config;
use std::fs;
use std::time::Instant;

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Build a repo whose working tree differs from HEAD by
/// `files * lines_per_file * 2` diff lines (every line rewritten).
fn repo_with_big_diff(files: usize, lines_per_file: usize) -> GitTestRepo {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    for f in 0..files {
        // Spread across nested directories, like a real tree.
        let path = format!("src/mod{}/file{}.rs", f % 7, f);
        let body: String = (0..lines_per_file)
            .map(|l| format!("fn original_{f}_{l}() {{ let x = {l}; }}\n"))
            .collect();
        repo.create_file(&path, &body);
    }
    repo.git_add_all();
    repo.git_commit("baseline");
    for f in 0..files {
        let path = format!("src/mod{}/file{}.rs", f % 7, f);
        let body: String = (0..lines_per_file)
            .map(|l| format!("fn changed_{f}_{l}() {{ let y = {l}; }}\n"))
            .collect();
        repo.create_file(&path, &body);
    }
    repo
}

fn bench_case(label: &str, files: usize, lines_per_file: usize) {
    let repo = repo_with_big_diff(files, lines_per_file);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    let open_start = Instant::now();
    harness.run_palette_command("Review Diff").unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk") && !s.contains("Generating Review")
        })
        .unwrap();
    // The toolbar lands before the stream does; wait for real diff rows.
    // A rewritten file diffs as 500 removals then 500 additions, so the
    // first rows on screen are the `original_` side.
    harness
        .wait_until(|h| h.screen_to_string().contains("original_0_0"))
        .unwrap();
    let open_ms = open_start.elapsed().as_millis();

    // A full rebuild with no git in it: `a` toggles inline notes, which
    // re-emits the whole stream.
    let mut rebuild_ms = Vec::new();
    for _ in 0..2 {
        let t = Instant::now();
        harness
            .send_key(crossterm::event::KeyCode::Char('a'), crossterm::event::KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| {
                let s = h.screen_to_string().to_lowercase();
                s.contains("notes shown") || s.contains("notes hidden")
            })
            .unwrap();
        rebuild_ms.push(t.elapsed().as_millis());
    }

    // Idle repaint: no input at all, so this is pure render cost over a
    // buffer of this size. The wrap-index counters say whether those
    // frames are re-laying-out the whole buffer or just the viewport.
    let stats_before = harness.editor().active_state().wrap_indices.stats();
    let idle_start = Instant::now();
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
    }
    let idle_ms = idle_start.elapsed().as_millis();
    // Split the paint out of the tick: `tick_and_render` is one editor tick
    // plus one full render, and which half carries the cost decides whether
    // to chase the render pipeline or whatever the tick pumps.
    let paint_start = Instant::now();
    for _ in 0..20 {
        harness.render().unwrap();
    }
    let paint_ms = paint_start.elapsed().as_millis();
    let stats_after = harness.editor().active_state().wrap_indices.stats();
    let idle_rebuilds = stats_after.rebuilds - stats_before.rebuilds;
    let idle_lines_built = stats_after.lines_built - stats_before.lines_built;
    let idle_repairs = stats_after.decoration_repairs - stats_before.decoration_repairs;
    let idle_lines_repaired = stats_after.lines_repaired - stats_before.lines_repaired;

    // Cursor move: same viewport, but the plugin re-does its per-cursor
    // work (cursor-line overlay, sticky header, status).
    let stats_before = harness.editor().active_state().wrap_indices.stats();
    let down_start = Instant::now();
    for _ in 0..20 {
        harness
            .send_key(crossterm::event::KeyCode::Down, crossterm::event::KeyModifiers::NONE)
            .unwrap();
        harness.tick_and_render().unwrap();
    }
    let down_ms = down_start.elapsed().as_millis();
    let stats_after = harness.editor().active_state().wrap_indices.stats();
    let down_rebuilds = stats_after.rebuilds - stats_before.rebuilds;
    let down_lines_built = stats_after.lines_built - stats_before.lines_built;
    let down_repairs = stats_after.decoration_repairs - stats_before.decoration_repairs;
    let down_lines_repaired = stats_after.lines_repaired - stats_before.lines_repaired;

    // Scrolling: no rebuild, so this is the host's cost over a big buffer.
    let scroll_start = Instant::now();
    for _ in 0..20 {
        harness
            .send_key(crossterm::event::KeyCode::PageDown, crossterm::event::KeyModifiers::NONE)
            .unwrap();
        harness.tick_and_render().unwrap();
    }
    let scroll_ms = scroll_start.elapsed().as_millis();

    println!(
        "BENCH {label}: files={files} diff_lines~{} \
         open={open_ms}ms rebuild={rebuild_ms:?}ms idle20={idle_ms}ms paint20={paint_ms}ms \
         down20={down_ms}ms scroll20={scroll_ms}ms",
        files * lines_per_file * 2
    );
    println!(
        "BENCH {label} wrap-index over 20 idle frames: rebuilds={idle_rebuilds} \
         lines_built={idle_lines_built} repairs={idle_repairs} lines_repaired={idle_lines_repaired}"
    );
    println!(
        "BENCH {label} wrap-index over 20 cursor moves: rebuilds={down_rebuilds} \
         lines_built={down_lines_built} repairs={down_repairs} lines_repaired={down_lines_repaired}"
    );
}

#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_review_layout_30k() {
    bench_case("30k", 30, 500);
}

#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_review_layout_100k() {
    bench_case("100k", 100, 500);
}

// ---------------------------------------------------------------------------
// Where the per-frame cost lives
// ---------------------------------------------------------------------------

/// Control for the review benchmarks above: an ordinary text buffer of the
/// same line counts, with none of the review's overlays, properties or
/// plugin event handlers. If a frame over a plain buffer is flat in line
/// count while the review's frame is linear, the cost is in what the
/// review decorates its rows with, not in buffer size as such.
#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_plain_buffer_frame_cost() {
    // `.txt` gets no grammar; `.rs` is parsed and carries highlight spans
    // for the whole file (under `MAX_PARSE_BYTES`). Same line counts, so
    // any difference between the two is the cost of the spans.
    for (name, lines) in [
        ("plain.txt", 20_000usize),
        ("plain.txt", 100_000),
        ("plain.rs", 20_000),
        ("plain.rs", 100_000),
    ] {
        let mut harness = EditorTestHarness::new(120, 40).unwrap();
        let text: String = (0..lines)
            .map(|i| format!("fn f_{i}() {{ let x = {i}; }}\n"))
            .collect();
        harness.load_buffer_from_text_named(name, &text).unwrap();
        harness.render().unwrap();

        let idle = Instant::now();
        for _ in 0..20 {
            harness.tick_and_render().unwrap();
        }
        let idle_ms = idle.elapsed().as_millis();

        let down = Instant::now();
        for _ in 0..20 {
            harness
                .send_key(crossterm::event::KeyCode::Down, crossterm::event::KeyModifiers::NONE)
                .unwrap();
        }
        let down_ms = down.elapsed().as_millis();

        let page = Instant::now();
        for _ in 0..20 {
            harness
                .send_key(crossterm::event::KeyCode::PageDown, crossterm::event::KeyModifiers::NONE)
                .unwrap();
        }
        let page_ms = page.elapsed().as_millis();

        println!(
            "BENCH plain-buffer: file={name} lines={lines} idle20={idle_ms}ms \
             down20={down_ms}ms page20={page_ms}ms"
        );
    }
}
