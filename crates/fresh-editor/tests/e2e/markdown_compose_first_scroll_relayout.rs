//! Regression tests for the bug where the *first* mouse-wheel scroll down a
//! markdown file in compose mode was extremely slow while every pass after it
//! was fast.
//!
//! # The loop that caused it
//!
//! 1. `lines_changed` only carries lines the editor has never offered the
//!    plugin (`Window::seen_byte_ranges`), so a first downward scroll produces
//!    a fresh batch on every frame — and a second pass over the same lines
//!    produces none.
//! 2. For each batch `markdown_compose` adds conceals and soft breaks, which
//!    bump `ConcealManager::version` / `SoftBreakManager::version`.
//! 3. Those versions are part of
//!    [`PipelineInputs`](fresh::view::line_wrap_cache::PipelineInputs), the
//!    cache key of [`WrapIndex`](fresh::view::wrap_index::WrapIndex).
//! 4. The key was a packed integer, so `ensure_built` could only compare it
//!    for equality — any decoration change forced a re-layout of **every
//!    logical line in the document**, once per frame. Cost was O(notches ×
//!    document); measured against the release binary in tmux, a 60-notch
//!    first pass on a 4 915-line file cost 1.95 s of CPU against 0.14 s for
//!    the second pass, growing linearly with document length up to
//!    `MAX_WRAP_SCROLLBAR_LINES` (5 000), past which the index is not built.
//!
//! # The fix these tests lock in
//!
//! `WrapIndex` now keeps its decoration snapshot in current buffer
//! coordinates and, when only the decoration versions moved, diffs the stored
//! snapshot against the fresh one and re-lays-out exactly the disagreeing
//! lines (`WrapIndex::repair_decorations`). A first scroll therefore costs
//! O(notches × batch) — the lines the plugin actually decorated — and the
//! whole-document rebuild happens once, when the index is first built.
//!
//! These tests assert on `WrapIndexStats` — counted layout work, not
//! wall-clock — so they mean the same thing on any machine.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::view::wrap_index::WrapIndexStats;

/// Lines per document. Comfortably under `MAX_WRAP_SCROLLBAR_LINES` (5 000),
/// which is where the index stops being built and the effect stops existing.
const DOC_LINES: usize = 2_400;
/// Wheel notches per pass, sent one frame at a time the way a wheel arrives.
const NOTCHES: usize = 60;

/// Prose with the inline markup compose mode decorates — emphasis becomes
/// conceals, and the length makes it wrap, which produces soft breaks. Both
/// are what bump the version that invalidates the layout index.
fn document(lines: usize) -> String {
    let mut md = String::from("# Document\n\n");
    let mut n = 2;
    let mut section = 0;
    while n < lines {
        md.push_str(&format!("## Section {section}\n\n"));
        n += 2;
        for p in 0..6 {
            md.push_str(&format!(
                "Paragraph {p} of section {section}, with **bold** and *italic* \
                 and `code` spans, written long enough that compose mode has to \
                 reflow it across several rows of the terminal.\n\n"
            ));
            n += 2;
        }
        section += 1;
    }
    md
}

fn compose_harness(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("doc.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        40,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Compose").unwrap();
    harness.wait_for_screen_contains("Toggle Compose").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    (harness, temp_dir)
}

fn wrap_stats(harness: &EditorTestHarness) -> WrapIndexStats {
    harness.editor().active_state().wrap_indices.stats()
}

fn delta(before: WrapIndexStats, after: WrapIndexStats) -> WrapIndexStats {
    WrapIndexStats {
        rebuilds: after.rebuilds - before.rebuilds,
        lines_built: after.lines_built - before.lines_built,
        decoration_repairs: after.decoration_repairs - before.decoration_repairs,
        lines_repaired: after.lines_repaired - before.lines_repaired,
    }
}

/// Scroll down `notches` wheel clicks, settling the plugin's decorations
/// between them the way a real wheel leaves time between frames.
fn wheel_down(harness: &mut EditorTestHarness, notches: usize) -> WrapIndexStats {
    let before = wrap_stats(harness);
    for _ in 0..notches {
        harness.mouse_scroll_down(10, 10).unwrap();
        for _ in 0..3 {
            harness.tick_and_render().unwrap();
            std::thread::sleep(std::time::Duration::from_millis(8));
            harness.advance_time(std::time::Duration::from_millis(8));
        }
    }
    delta(before, wrap_stats(harness))
}

fn wheel_up(harness: &mut EditorTestHarness, notches: usize) {
    for _ in 0..notches {
        harness.mouse_scroll_up(10, 10).unwrap();
    }
    harness.wait_until_stable(|_| true).unwrap();
}

/// The regression this file exists for: a first wheel-scroll must cost the
/// lines the plugin decorated, not the document times the frame count.
///
/// Layout work is bounded by a small multiple of the document: the initial
/// index build(s) are O(document) each and legitimate, and the diff repairs
/// touch each newly decorated line a bounded number of times. Before the fix
/// this scroll laid out ~82x the document (197 702 lines for a 2 411-line
/// file); the bound here is an order of magnitude under that while leaving
/// room for an extra build on a geometry change.
#[test]
fn first_wheel_scroll_costs_the_batches_not_the_document() {
    let (mut harness, _tmp) = compose_harness(&document(DOC_LINES));
    let doc_lines = harness
        .editor()
        .active_state()
        .buffer
        .line_count()
        .unwrap_or(DOC_LINES) as u64;

    let first = wheel_down(&mut harness, NOTCHES);

    wheel_up(&mut harness, NOTCHES + 20);
    let second = wheel_down(&mut harness, NOTCHES);

    eprintln!(
        "document {doc_lines} lines; first pass: {} rebuilds / {} lines built, \
         {} repairs / {} lines repaired; second pass: {} rebuilds / {} lines \
         built, {} repairs / {} lines repaired",
        first.rebuilds,
        first.lines_built,
        first.decoration_repairs,
        first.lines_repaired,
        second.rebuilds,
        second.lines_built,
        second.decoration_repairs,
        second.lines_repaired,
    );

    assert!(
        first.lines_built <= doc_lines * 4,
        "a first wheel-scroll must not re-lay-out the document per frame: \
         {} lines built for a {doc_lines}-line document",
        first.lines_built,
    );
    assert!(
        first.decoration_repairs > 0,
        "the plugin's batches should be arriving as diff repairs"
    );
    assert!(
        first.lines_repaired <= doc_lines * 4,
        "diff repairs should touch each decorated line a bounded number of \
         times: {} lines repaired for a {doc_lines}-line document",
        first.lines_repaired,
    );
    assert_eq!(
        second.lines_built, 0,
        "a second pass over the same lines produces no batches and must \
         build nothing"
    );
}

/// The control that names the trigger: the same document, the same scroll,
/// with compose mode off. No plugin decorations arrive, so the pipeline
/// inputs never move, the index is built once, and the first pass costs the
/// same as the second.
#[test]
fn without_compose_mode_the_index_is_built_once() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let md_path = temp_dir.path().join("doc.md");
    std::fs::write(&md_path, document(DOC_LINES)).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        40,
        Default::default(),
        temp_dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(&md_path).unwrap();
    harness.render().unwrap();

    let doc_lines = harness
        .editor()
        .active_state()
        .buffer
        .line_count()
        .unwrap_or(DOC_LINES) as u64;

    let first = wheel_down(&mut harness, NOTCHES);

    eprintln!(
        "no compose: {} rebuilds, {} lines laid out for a {doc_lines}-line document",
        first.rebuilds, first.lines_built,
    );

    assert!(
        first.lines_built < doc_lines * 3,
        "with no decorations arriving, a scroll should not keep re-laying out \
         the document: {} lines laid out for {doc_lines} lines",
        first.lines_built,
    );
}
