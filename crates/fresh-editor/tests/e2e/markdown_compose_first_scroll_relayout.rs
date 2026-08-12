//! Why the *first* mouse-wheel scroll down a markdown file in compose mode is
//! extremely slow, and every pass after it is fast.
//!
//! # The loop
//!
//! 1. `lines_changed` only carries lines the editor has never offered the
//!    plugin (`Window::seen_byte_ranges`), so a first downward scroll produces
//!    a fresh batch on every frame — and a second pass over the same lines
//!    produces none.
//! 2. For each batch `markdown_compose` adds conceals and soft breaks, which
//!    bump `ConcealManager::version` / `SoftBreakManager::version`.
//! 3. Those versions are folded into
//!    [`pipeline_inputs_version`](fresh::view::line_wrap_cache::pipeline_inputs_version),
//!    which is the cache key of [`WrapIndex`](fresh::view::wrap_index::WrapIndex).
//! 4. A key change means `WrapIndex::ensure_built` cannot repair, so it lays
//!    out **every logical line in the document** again — the one O(buffer)
//!    operation in the design — on that frame.
//!
//! So a first scroll re-lays-out the whole file once per frame. Wheel
//! scrolling is the worst case because each notch is its own frame with its
//! own batch: cost is O(notches × document), not O(notches × viewport).
//!
//! # Measured against the real binary
//!
//! Driving `fresh` in tmux with SGR wheel events, 60 notches, measuring the
//! process's own CPU (first pass / second pass over the same lines):
//!
//! | document | first | second |
//! |---|---|---|
//! | 2 003 lines | 0.92 s | 0.12 s |
//! | 3 511 lines | 1.50 s | 0.11 s |
//! | 4 915 lines | 1.95 s | 0.14 s |
//! | 5 201 lines | 0.18 s | 0.12 s |
//! | 4 915 lines, compose **off** | 0.10 s | 0.11 s |
//!
//! First-pass cost is linear in document length and vanishes just past 5 000
//! lines — `MAX_WRAP_SCROLLBAR_LINES`, above which the index is not built at
//! all and the effect disappears along with it. So the pathology is bounded to
//! documents under that ceiling; a genuinely huge file is accidentally spared.
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

/// The reproduction: a first wheel-scroll re-lays-out the whole document over
/// and over, a second scroll over the same lines lays out nothing.
#[test]
fn first_wheel_scroll_relayouts_the_whole_document_repeatedly() {
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
        "document {doc_lines} lines; first pass: {} rebuilds, {} lines laid out \
         ({:.1}x the document); second pass: {} rebuilds, {} lines laid out",
        first.rebuilds,
        first.lines_built,
        first.lines_built as f64 / doc_lines as f64,
        second.rebuilds,
        second.lines_built,
    );

    assert!(
        first.lines_built > doc_lines * 5,
        "a first wheel-scroll should be re-laying out the whole document many \
         times over: {} lines laid out for a {doc_lines}-line document",
        first.lines_built,
    );
    assert!(
        second.lines_built * 4 < first.lines_built,
        "the second pass over the same lines should lay out far less; \
         first {} lines, second {}",
        first.lines_built,
        second.lines_built,
    );
}

/// The control that names the trigger: the same document, the same scroll,
/// with compose mode off. No plugin decorations arrive, so
/// `pipeline_inputs_version` never moves, the index is built once, and the
/// first pass costs the same as the second.
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
