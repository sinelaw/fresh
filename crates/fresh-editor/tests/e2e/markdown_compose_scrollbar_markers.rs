//! End-to-end proof that the scrollbar-marker API (issue #2713) works from a
//! real plugin: the `markdown_compose` plugin marks headings on the scrollbar.
//!
//! These drive the editor the way a user does — open a file, toggle compose
//! mode from the command palette, scroll — and assert only on rendered cells
//! in the scrollbar column.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

/// The half-block glyph a scrollbar marker paints.
const MARKER_GLYPH: &str = "▌";

/// A document with headings spread from top to bottom, and enough filler that
/// it scrolls well past one screen.
fn document_with_headings(sections: usize, filler_lines: usize) -> String {
    let mut md = String::new();
    for s in 0..sections {
        md.push_str(&format!("# Section {s}\n\n"));
        for l in 0..filler_lines {
            md.push_str(&format!("Body line {l} of section {s}.\n"));
        }
        md.push('\n');
    }
    md
}

fn marker_rows(harness: &EditorTestHarness) -> Vec<u16> {
    let col = harness.buffer().area.width - 1;
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter(|row| harness.get_cell(col, *row as u16).as_deref() == Some(MARKER_GLYPH))
        .map(|r| r as u16)
        .collect()
}

/// Open a markdown document with the real `markdown_compose` plugin loaded and
/// compose mode enabled through the command palette.
fn compose_harness(md: &str) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let md_path = project_root.join("headings.md");
    std::fs::write(&md_path, md).unwrap();

    // `with_full_grammar_registry` is what makes the editor resolve embedded
    // regions, which is how a line inside a fenced block reports `region`.
    // Without it every line comes back unclassified and the marks are decided
    // by the plugin's textual fence tracking alone — the fallback, not the path
    // that runs in the editor.
    let mut harness = EditorTestHarness::create(
        80,
        30,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_full_grammar_registry(),
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

/// The headline end-to-end case for issue #2713: with compose mode on, the
/// plugin's heading marks appear as colored cells on the scrollbar.
#[test]
fn markdown_compose_marks_headings_on_the_scrollbar() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(12, 12));

    // Wait for the plugin's lines_changed pass to publish its marks.
    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("markdown_compose should mark headings on the scrollbar");

    let rows = marker_rows(&harness);
    assert!(
        rows.len() >= 2,
        "several headings are on screen, so several marks are expected; saw {rows:?}"
    );
}

/// A `#` at the start of a line inside a fenced code block is a comment in the
/// block's language, not a heading, and must not be marked.
///
/// Shell blocks make this constant: `# Binary package (recommended)` above an
/// install command is ordinary prose in a bash fence, and every such line used
/// to put a mark on the track.
#[test]
fn comments_in_fenced_code_blocks_are_not_headings() {
    let mut md = String::from("# Install\n\n");
    md.push_str("**Using an AUR helper (such as `yay` or `paru`):**\n\n");
    for s in 0..20 {
        md.push_str("```bash\n");
        md.push_str(&format!(
            "# Binary package (recommended, faster install) {s}\n"
        ));
        md.push_str("yay -S fresh-editor-bin\n\n");
        md.push_str("# Or build from source\n");
        md.push_str("yay -S fresh-editor\n");
        md.push_str("```\n\n");
        for l in 0..15 {
            md.push_str(&format!("Prose line {l} of section {s}.\n"));
        }
        md.push('\n');
    }

    let (mut harness, _tmp) = compose_harness(&md);

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("the one real `# Install` heading should be marked");
    // Let any marks for the 40 `#` comment lines have their chance to appear
    // before asserting that they don't.
    harness.wait_for_async_quiescence(8).unwrap();

    let (first, _) = harness.content_area_rows();
    assert_eq!(
        marker_rows(&harness),
        vec![first as u16],
        "only the `# Install` heading is a heading; the 40 shell comments \
         inside the bash fences are not"
    );
}

/// The track describes the whole document from the moment compose mode is on
/// — the headline fix for issue #2990.
///
/// Marks used to be published viewport by viewport out of `lines_changed`, so
/// the track only showed the parts already scrolled through. A structure map
/// that needs the reader to explore the document before it describes it is not
/// a structure map.
#[test]
fn every_heading_is_marked_without_scrolling() {
    // 30 sections of 20 lines: the opening viewport holds barely one of them.
    let (mut harness, _tmp) = compose_harness(&document_with_headings(30, 20));

    // The pre-scan reads the buffer over the async plugin bridge, so the full
    // set lands a few ticks after the toggle returns.
    harness
        .wait_until(|h| marker_rows(h).len() >= 10)
        .expect("compose mode should mark headings across the whole document");

    let rows = marker_rows(&harness);
    let (first, last) = harness.content_area_rows();
    let (first, last) = (first as u16, last as u16);
    let track = (last - first + 1) as f32;

    // Coverage reaches content that has never been on screen: the last
    // section's heading is at the end of the file, so its mark belongs near
    // the bottom of the track.
    let deepest = (*rows.last().unwrap() - first) as f32 / track;
    assert!(
        deepest > 0.8,
        "deepest mark should sit near the end of the track, was at \
         {deepest:.2} of it; rows {rows:?}"
    );
}

/// Only top-level headings are marked by default: a track cell covers many
/// lines, so marking every level packs several headings into one cell where
/// only the shallowest survives anyway.
#[test]
fn only_top_level_headings_are_marked_by_default() {
    let mut md = String::from("# The one top level heading\n\n");
    for s in 0..30 {
        md.push_str(&format!("## Sub {s}\n\n### Deeper {s}\n\n"));
        for l in 0..20 {
            md.push_str(&format!("Body line {l} of section {s}.\n"));
        }
    }

    let (mut harness, _tmp) = compose_harness(&md);

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("the `#` heading should be marked");
    // Give any deeper-level marks the same chance to appear before asserting
    // that they don't.
    harness.wait_for_async_quiescence(8).unwrap();

    let (first, _) = harness.content_area_rows();
    assert_eq!(
        marker_rows(&harness),
        vec![first as u16],
        "only the single `#` heading should be marked, at the top of the track"
    );
}

/// Exploring the document does not disturb the marks: scrolling neither drops
/// the headings outside the viewport nor adds to a set that is already
/// complete.
///
/// This is the property that `setScrollbarMarkersInRange` exists for — a
/// whole-namespace replace on every `lines_changed` batch would leave only the
/// headings near the viewport marked.
#[test]
fn heading_marks_survive_exploring_the_document() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(20, 14));

    // What follows compares this set against the one after scrolling, so it
    // has to be the *complete* set: read it early and the test compares two
    // partial sets and proves nothing.
    //
    // Two marks is what the per-batch `lines_changed` pass publishes for the
    // headings in the opening viewport; the rest of the document is marked by
    // `prescanHeadingMarkers`, which reads the whole buffer over the async
    // bridge and then publishes every heading in one whole-namespace call.
    // So the complete set arrives in a single step, and waiting for *it* is
    // the gate — `wait_until(|h| !marker_rows(h).is_empty())` resolves on the
    // viewport pass, several ticks earlier.
    //
    // Quiescence is not that gate either, and that is how this test failed on
    // CI with an `initial` of `[2, 3]`: the pre-scan is fire-and-forget
    // (`void prescanHeadingMarkers(...)`), so while its `getBufferText` round
    // trip is outstanding the editor reports no work for the tick and
    // `wait_for_async_quiescence` returns with only the viewport's marks
    // published. Gate on the marks themselves, the way
    // `every_heading_is_marked_without_scrolling` does — the viewport pass
    // cannot reach ten on a document this long, so the threshold is a real
    // gate on the pre-scan having landed. Quiescence afterwards only settles
    // any per-batch republish on top of a set that is already whole.
    harness.wait_until(|h| marker_rows(h).len() >= 10).expect(
        "the pre-scan should mark the whole document before scrolling starts, \
             otherwise this test compares two partial sets and proves nothing",
    );
    harness.wait_for_async_quiescence(8).unwrap();
    let initial = marker_rows(&harness);

    // Page down through the document so later sections enter the viewport,
    // settling the frame after each page rather than waiting for the whole
    // async pipeline to fall silent.
    //
    // This loop is the only place in the file that settles per iteration, and
    // it is the one test in the file that timed out on CI. Quiescence is
    // bounded but not cheap: it needs several consecutive no-work ticks at
    // 50ms apiece, and with the plugin re-decorating every newly visible page
    // it measured over a second per call on a loaded runner — 13 calls, ~19s,
    // against ~8s for the whole test unloaded — and gives up only after 30s
    // when the pipeline never goes quiet.
    //
    // The property under test survives without the per-page settle: every
    // page's `lines_changed` batch is queued by the scroll whether or not this
    // thread waits for it, and a whole-namespace republish — the bug this test
    // exists for — is still visible in the set the final settle below hands to
    // the comparison.
    //
    // Not "wait for the screen to change", though: a PageDown at the end of
    // the document is a no-op, and this sweep runs over a document only about
    // thirteen pages deep, so that wait would be one viewport row away from
    // never resolving. A settled frame is true whether or not the page moved.
    //
    // Six pages, not twelve. Compose mode sizes its window from the line
    // breaks its conceals already swallow, so a scrolled-to page settles over
    // a few frames rather than one — self-correcting by design, but it
    // multiplies the renders each `wait_until_stable` has to sit through, and
    // this test settles once per page. Twelve pages of that timed out at
    // nextest's 180s cap on a loaded macOS runner while taking under three
    // seconds unloaded. Six still carries the property: the marks under test
    // are the ones for sections the viewport has left behind, and by page six
    // most of the document is behind it.
    for _ in 0..6 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.wait_until_stable(|_| true).unwrap();
    }
    harness.wait_for_async_quiescence(8).unwrap();

    // Each batch republishes only its own byte span, so the marks for the
    // sections scrolled past are left alone. With a whole-namespace replace
    // per batch, only the headings near the viewport would survive — the set
    // would shrink to the last page's two or three marks and start well down
    // the track instead of at its top.
    //
    // The count and the two ends, not the exact rows: the track's coordinate
    // space is *visual* rows (`MarkerBasis::VisualRows`, chosen so marks and
    // thumb agree), and compose mode reflows a paragraph by concealing the
    // line breaks inside it — a conceal that only exists for the lines the
    // plugin has decorated. Exploring the document therefore shortens it, by
    // design, and every mark below the part already visited slides up a row or
    // two as that happens. What must not change is which headings are marked.
    let after = marker_rows(&harness);
    assert_eq!(
        after.len(),
        initial.len(),
        "every heading should still be marked after exploring the document; \
         before {initial:?}, after {after:?}"
    );
    assert_eq!(
        (after.first(), after.last()),
        (initial.first(), initial.last()),
        "the marks should still span the whole track — the first heading is at \
         the top of the document and the last at its end; \
         before {initial:?}, after {after:?}"
    );
}

/// Turning compose mode back off removes the plugin's marks from the
/// scrollbar, so nothing lingers after the feature is disabled.
#[test]
fn disabling_compose_mode_clears_the_heading_marks() {
    let (mut harness, _tmp) = compose_harness(&document_with_headings(12, 12));

    harness
        .wait_until(|h| !marker_rows(h).is_empty())
        .expect("initial heading marks");

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

    harness
        .wait_until(|h| marker_rows(h).is_empty())
        .expect("marks should be cleared when compose mode is turned off");
}
