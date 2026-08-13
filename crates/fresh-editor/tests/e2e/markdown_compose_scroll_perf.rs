//! Attribution tests for "scrolling down a large markdown buffer is very slow
//! the first time" — driven through the real `markdown_compose` plugin, on a
//! document with the inline markup compose mode actually decorates.
//!
//! The asymmetry the report described comes from `Window::seen_byte_ranges`:
//! `lines_changed` only ever carries lines the editor has never offered the
//! plugin before, so a first pass runs the whole per-line decoration pipeline
//! — emphasis overlays, conceals, soft breaks, heading marks — and a second
//! pass over the same lines runs none of it. The dominant cost — the wrap
//! index rebuilding the whole document once per frame — is fixed (diff
//! repair; see `markdown_compose_first_scroll_relayout`, which locks it), so
//! the first pass now pays only the one-time decoration work itself.
//!
//! What this file pins down is the scrollbar half of the story:
//!
//! * The heading marks were the report's suspect, and there is a genuine
//!   inefficiency there — the plugin republishes for every batch, which moves
//!   the marker version, misses `ProjectionKey`, and re-walks every heading
//!   found so far, once per frame: O(frames × headings).
//!   `first_scroll_reprojects_every_heading_on_every_frame` measures it, and
//!   `view::ui::split_rendering::scrollbar_marker_scroll_perf` measures its
//!   growth in isolation.
//! * But it was never big enough to be the reported slowness:
//!   `heading_marks_are_not_the_reason` runs the same scroll with the
//!   headings removed — the marker work drops by an order of magnitude and
//!   the pass costs about the same.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::view::scrollbar_marker::ProjectionStats;
use std::time::Duration;

/// A body paragraph with the inline markup compose mode actually decorates —
/// emphasis, which becomes overlays and conceals — and long enough to wrap,
/// which produces soft breaks. Without these a synthetic document is
/// decoration-free and measures nothing but the bare renderer.
fn body_line(section: usize, line: usize) -> String {
    format!(
        "Body line {line} of section {section}, with **bold** and *italic* and \
         `code` spans, written long enough that compose mode has to reflow it \
         across several rows of the terminal.\n"
    )
}

/// Sections of body text under an ATX heading, the shape of a long document
/// with real structure (a changelog, a spec, a book chapter).
fn document_with_headings(sections: usize, filler_lines: usize) -> String {
    let mut md = String::new();
    for s in 0..sections {
        // Level 1: only top-level headings are marked on the scrollbar by
        // default, and these tests are about the cost of the marks.
        md.push_str(&format!("# Section {s}\n\n"));
        for l in 0..filler_lines {
            md.push_str(&body_line(s, l));
        }
        md.push('\n');
    }
    md
}

/// The same document with no headings at all: same length, same inline markup,
/// same conceal and soft-break load — nothing to mark on the scrollbar.
fn document_without_headings(lines: usize) -> String {
    let mut md = String::from("# Title\n\n");
    for l in 0..lines {
        md.push_str(&body_line(0, l));
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

    let md_path = project_root.join("long.md");
    std::fs::write(&md_path, md).unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        30,
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

fn stats(harness: &EditorTestHarness) -> ProjectionStats {
    harness
        .editor()
        .active_state()
        .scrollbar_marker_buckets
        .stats()
}

fn marker_count(harness: &EditorTestHarness) -> usize {
    harness.editor().active_state().scrollbar_markers.len()
}

/// Everything the compose plugin has accumulated for this buffer. All of these
/// grow as the document is explored, and the per-line clear+re-add pass walks
/// each store in full for every line it touches.
fn decoration_census(harness: &EditorTestHarness) -> String {
    let state = harness.editor().active_state();
    format!(
        "{} overlays, {} conceals, {} soft breaks, {} virtual texts, \
         {} scrollbar markers",
        state.overlays.all().len(),
        state.conceals.len(),
        state.soft_breaks.len(),
        state.virtual_texts.len(),
        state.scrollbar_markers.len(),
    )
}

/// What one downward pass cost.
struct ScrollCost {
    stats: ProjectionStats,
    /// Time inside `tick_and_render` only — the settle sleeps that let the
    /// plugin thread catch up are deliberately excluded, so this is the work
    /// the editor itself does, comparable between passes.
    per_page: Vec<Duration>,
}

impl ScrollCost {
    fn render_total(&self) -> Duration {
        self.per_page.iter().sum()
    }
}

/// Ticks spent settling each page, and the real time allowed between them for
/// the plugin thread to answer the `lines_changed` batch.
const SETTLE_TICKS: usize = 8;
const SETTLE_SLEEP: Duration = Duration::from_millis(10);
const PAGES: usize = 40;

/// Page down `pages` times, letting the plugin's decorations settle after each.
fn scroll_down(harness: &mut EditorTestHarness, pages: usize) -> ScrollCost {
    let before = stats(harness);
    let mut per_page = Vec::with_capacity(pages);

    for _ in 0..pages {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        let mut page = Duration::ZERO;
        for _ in 0..SETTLE_TICKS {
            let t = std::time::Instant::now();
            harness.tick_and_render().unwrap();
            page += t.elapsed();
            // Untimed: the plugin answers hooks on its own thread, and this
            // measures the editor's frame, not the plugin's latency.
            std::thread::sleep(SETTLE_SLEEP);
            harness.advance_time(SETTLE_SLEEP);
        }
        per_page.push(page);
    }

    let after = stats(harness);
    ScrollCost {
        stats: ProjectionStats {
            rebuilds: after.rebuilds - before.rebuilds,
            markers_walked: after.markers_walked - before.markers_walked,
        },
        per_page,
    }
}

/// The first/second-pass asymmetry, post-fix: the first pass still does the
/// one-time decoration work, but the second pass is a pure replay — nothing
/// republishes, so the marker projection is a cache hit on every frame. (The
/// wall-clock ratio between the passes is reported for eyeballs but not
/// asserted; the counted bound on first-pass layout work lives in
/// `markdown_compose_first_scroll_relayout`.)
#[test]
fn second_scroll_is_a_pure_replay() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(120, 8));

    let first = scroll_down(&mut harness, PAGES);

    // Back to the top. Ctrl+Home scrolls over lines the editor has already
    // offered the plugin, so no `lines_changed` batch is produced and none of
    // the per-line decoration work runs again.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_until_stable(|_| true).unwrap();

    let second = scroll_down(&mut harness, PAGES);

    eprintln!(
        "first scroll: render {:?}, {} markers walked; \
         second scroll: render {:?}, {} markers walked; accumulated {}",
        first.render_total(),
        first.stats.markers_walked,
        second.render_total(),
        second.stats.markers_walked,
        decoration_census(&harness),
    );

    assert_eq!(
        second.stats.markers_walked, 0,
        "nothing republishes on a second pass, so the projection is cached"
    );
}

/// ...but the heading marks are not why. The same scroll over a document with
/// no headings does a fraction of the marker work and costs the same.
#[test]
fn heading_marks_are_not_the_reason() {
    let (mut with_headings, _a) = compose_harness(&document_with_headings(120, 8));
    let marked = scroll_down(&mut with_headings, PAGES);
    let marked_census = decoration_census(&with_headings);

    let (mut without_headings, _b) = compose_harness(&document_without_headings(120 * 10));
    let control = scroll_down(&mut without_headings, PAGES);

    eprintln!(
        "with headings:    render {:?}, {} markers walked, accumulated {marked_census}",
        marked.render_total(),
        marked.stats.markers_walked,
    );
    eprintln!(
        "without headings: render {:?}, {} markers walked, accumulated {}",
        control.render_total(),
        control.stats.markers_walked,
        decoration_census(&without_headings),
    );

    // The counted work is unambiguous where wall-clock is noisy: the control
    // accumulates no marks, so its rebuilds walk almost nothing.
    assert!(
        control.stats.markers_walked * 10 < marked.stats.markers_walked,
        "the control should do almost no marker work; control {} vs {}",
        control.stats.markers_walked,
        marked.stats.markers_walked,
    );

    // And removing every mark does not make the first scroll cheaper — so the
    // marks are not what the first scroll is spending its time on.
    assert!(
        control.render_total() * 2 > marked.render_total(),
        "removing the heading marks should not materially change the cost of \
         a first scroll; with headings {:?}, without {:?}",
        marked.render_total(),
        control.render_total(),
    );
}

/// The inefficiency the report pointed at is real, even though it is not the
/// cause here: during a first scroll the whole heading set is re-projected
/// once per frame, so the work is O(frames × headings) rather than O(headings).
/// Its absolute size is measured in
/// `view::ui::split_rendering::scrollbar_marker_scroll_perf`.
#[test]
fn first_scroll_reprojects_every_heading_on_every_frame() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(120, 8));

    let first = scroll_down(&mut harness, PAGES);
    let headings = marker_count(&harness);

    eprintln!(
        "first scroll: {} rebuilds, {} markers walked for {headings} headings",
        first.stats.rebuilds, first.stats.markers_walked,
    );

    assert!(
        headings > 10,
        "the scroll should have discovered a good number of headings, saw {headings}"
    );
    assert!(
        first.stats.markers_walked > (headings * 5) as u64,
        "the marker set should have been re-walked several times over during \
         one downward scroll — {} markers walked for {headings} headings",
        first.stats.markers_walked
    );
}
