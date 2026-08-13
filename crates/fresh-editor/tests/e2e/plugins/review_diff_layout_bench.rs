//! Benchmark: what laying out a very large review actually costs.
//!
//! Not a correctness test — it prints timings for the unified stream at a
//! given diff size and layout budget, so `maxExpandedDiffLines` can be
//! argued from numbers instead of intuition. Ignored by default; run with
//! `--ignored --nocapture --test-threads=1`.
//!
//! Release build, per operation (20 samples each, harness numbers — a test
//! frame is heavier than a real terminal one, so read the ratios, not the
//! absolutes):
//!
//! | laid out | frame | cursor Down | PageDown | open  | rebuild |
//! |----------|-------|-------------|----------|-------|---------|
//! | 20k      | 135ms | 672ms       | 748ms    | 2.2s  | 2.5s    |
//! | 30k      | 234ms | 1071ms      | 1162ms   | 2.9s  | 3.9s    |
//! | 100k     | 862ms | 3759ms      | 4109ms   | 9.7s  | 14.7s   |
//!
//! Two things fall out of that:
//!
//! Every column is linear in the lines laid out, and nothing tracks the
//! part of the diff left header-only — a 100k-line diff capped at 20k
//! costs exactly what a 30k-line diff capped at 20k costs. The budget is
//! what holds the cost flat as reviews get bigger.
//!
//! The cost that dominates is per *frame*, not the one-time load: a key
//! press runs ~4.5 frames' worth of work at every size. So laying more out
//! in the background — loading files async after the first paint — would
//! not buy what it looks like it buys. It would hide the smaller term
//! (open) and leave every subsequent keystroke paying the larger one. The
//! fix that would let the budget go away is a frame that costs O(viewport)
//! instead of O(buffer), which is a host-side renderer/overlay question,
//! not a plugin one.

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use fresh::config::{Config, PluginConfig};
use std::fs;
use std::time::Instant;

fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

fn config_with_budget(budget: u64) -> Config {
    let mut config = Config::default();
    config.plugins.insert(
        "audit_mode".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "maxExpandedDiffLines": budget }),
        },
    );
    config
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

fn bench_case(label: &str, files: usize, lines_per_file: usize, budget: u64) {
    let repo = repo_with_big_diff(files, lines_per_file);
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        config_with_budget(budget),
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
    // buffer of this size.
    let idle_start = Instant::now();
    for _ in 0..20 {
        harness.tick_and_render().unwrap();
    }
    let idle_ms = idle_start.elapsed().as_millis();

    // Cursor move: same viewport, but the plugin re-does its per-cursor
    // work (cursor-line overlay, sticky header, status).
    let down_start = Instant::now();
    for _ in 0..20 {
        harness
            .send_key(crossterm::event::KeyCode::Down, crossterm::event::KeyModifiers::NONE)
            .unwrap();
        harness.tick_and_render().unwrap();
    }
    let down_ms = down_start.elapsed().as_millis();

    // Scrolling: no rebuild, so this is the host's cost over a big buffer.
    let scroll_start = Instant::now();
    for _ in 0..20 {
        harness
            .send_key(crossterm::event::KeyCode::PageDown, crossterm::event::KeyModifiers::NONE)
            .unwrap();
        harness.tick_and_render().unwrap();
    }
    let scroll_ms = scroll_start.elapsed().as_millis();

    let screen = harness.screen_to_string();
    let not_loaded = screen.contains("not loaded");
    println!(
        "BENCH {label}: files={files} diff_lines~{} budget={budget} \
         open={open_ms}ms rebuild={rebuild_ms:?}ms idle20={idle_ms}ms down20={down_ms}ms \
         scroll20={scroll_ms}ms not_loaded_on_screen={not_loaded}",
        files * lines_per_file * 2
    );
}

#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_review_layout_30k_capped() {
    bench_case("30k/capped", 30, 500, 20000);
}

#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_review_layout_30k_uncapped() {
    bench_case("30k/uncapped", 30, 500, 100_000_000);
}

#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_review_layout_100k_uncapped() {
    bench_case("100k/uncapped", 100, 500, 100_000_000);
}

#[test]
#[ignore = "benchmark: run explicitly with --nocapture"]
fn bench_review_layout_100k_capped() {
    bench_case("100k/capped", 100, 500, 20000);
}
