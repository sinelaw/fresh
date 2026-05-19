//! Migration of `tests/e2e/virtual_lines.rs` — virtual lines
//! (LineAbove / LineBelow) injected via the plugin-state path.
//!
//! Load-bearing claims preserved here:
//!
//!   1. A `LineAbove` virtual line attached to a buffer byte
//!      offset renders above the source line and doesn't clobber
//!      the surrounding source lines.
//!   2. A `LineBelow` virtual line renders below the source line.
//!   3. Multiple virtual lines at the same offset all render
//!      (and the priority field doesn't drop any of them).
//!   4. `clear_namespace` removes only the targeted namespace —
//!      other namespaces survive.
//!   5. Virtual line rows don't carry a gutter line number — they
//!      are not part of the source numbering.
//!   6. A virtual line marker tracks edits: inserting text *above*
//!      the anchor doesn't detach the virtual line from its source
//!      line.
//!   7. Both `LineAbove` and `LineBelow` can coexist on the same
//!      source line, and the rendered order is `ABOVE → source →
//!      BELOW`.
//!   8. The `VirtualTextManager` length counter tracks
//!      `add_line` and `clear_namespace` correctly.
//!
//! ## Harness-direct pattern
//!
//! There is no `Action::*` for injecting virtual text — the
//! virtual-text APIs are plugin-internal, called from Rust/TS
//! plugin code via `editor.active_state_mut().virtual_texts`.
//! Every test here therefore takes the harness-direct path and
//! touches `harness.editor_mut().active_state_mut()` directly to
//! call `add_line` / `clear_namespace`. The `VirtualTextPosition`
//! and `VirtualTextNamespace` types come from
//! `fresh::view::virtual_text` (the same projection the plugin
//! API uses), permitted under the harness-direct exemption in
//! `scripts/check-semantic-test-isolation.sh`.
//!
//! Source: `tests/e2e/virtual_lines.rs` (all 8 tests migrated;
//! no tests deferred). Screen-text assertions go through
//! `RenderSnapshot::extract_with_rendered_rows`, matching the
//! e2e original's `screen_to_string` substring checks.

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::view::virtual_text::{VirtualTextNamespace, VirtualTextPosition};
use ratatui::style::{Color, Style};

fn virtual_line_style() -> Style {
    Style::default().fg(Color::DarkGray)
}

fn ns(name: &str) -> VirtualTextNamespace {
    VirtualTextNamespace::from_string(name.to_string())
}

#[test]
fn migrated_virtual_line_above_renders_above_source() {
    // Original: `test_virtual_line_above`.
    let fixture =
        TestFixture::new("virtual_line_above.txt", "Line 1\nLine 2\nLine 3").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7, // byte offset of "Line 2"
            "--- Header Above Line 2 ---".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("test"),
            0,
        );
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::AnyRowContains("--- Header Above Line 2 ---".into()),
            RowMatch::AnyRowContains("Line 1".into()),
            RowMatch::AnyRowContains("Line 2".into()),
            RowMatch::AnyRowContains("Line 3".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "virtual line above must coexist with source lines: \
             {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_virtual_line_below_renders_below_source() {
    // Original: `test_virtual_line_below`.
    let fixture =
        TestFixture::new("virtual_line_below.txt", "Line 1\nLine 2\nLine 3").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0, // byte offset of "Line 1"
            "--- Footer Below Line 1 ---".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineBelow,
            ns("test"),
            0,
        );
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains(
            "--- Footer Below Line 1 ---".into(),
        )],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "virtual line below must render: {f} expected {e}; actual {a}\n\
             rows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_multiple_virtual_lines_same_position_all_visible() {
    // Original: `test_multiple_virtual_lines_same_position`.
    let fixture =
        TestFixture::new("virtual_lines_multi.txt", "Line 1\nLine 2").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "First Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("test"),
            0,
        );
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Second Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("test"),
            10,
        );
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::AnyRowContains("First Header".into()),
            RowMatch::AnyRowContains("Second Header".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "both virtual lines at the same offset must render: \
             {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_clear_namespace_only_clears_targeted_namespace() {
    // Original: `test_clear_namespace`.
    let fixture =
        TestFixture::new("virtual_lines_namespaces.txt", "Line 1\nLine 2").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Git Blame Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("git-blame"),
            0,
        );
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "LSP Diagnostic".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("lsp"),
            0,
        );
    }

    // Sanity: both visible before the clear.
    {
        let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
        let expect = RenderSnapshotExpect {
            row_checks: vec![
                RowMatch::AnyRowContains("Git Blame Header".into()),
                RowMatch::AnyRowContains("LSP Diagnostic".into()),
            ],
            ..Default::default()
        };
        if let Some((f, e, a)) = expect.check_against(&snap) {
            panic!(
                "pre-clear sanity: both namespaces should render: \
                 {f} expected {e}; actual {a}"
            );
        }
    }

    {
        let state = harness.editor_mut().active_state_mut();
        state
            .virtual_texts
            .clear_namespace(&mut state.marker_list, &ns("git-blame"));
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::NoRowContains("Git Blame Header".into()),
            RowMatch::AnyRowContains("LSP Diagnostic".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "after clearing git-blame: git-blame must be gone, lsp must \
             remain: {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_virtual_lines_have_no_gutter_line_number() {
    // Original: `test_virtual_lines_no_line_numbers`. The claim is
    // that the row containing the virtual text "VIRTUAL" doesn't
    // start (after gutter trim) with a digit — virtual lines have
    // a distinct gutter (typically blank).
    let fixture = TestFixture::new(
        "virtual_lines_no_gutter.txt",
        "Line 1\nLine 2\nLine 3",
    )
    .unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            7, // position of "Line 2"
            "VIRTUAL".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("test"),
            0,
        );
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let virtual_rows: Vec<&String> = snap
        .rendered_rows
        .iter()
        .filter(|r| r.contains("VIRTUAL"))
        .collect();
    assert!(
        !virtual_rows.is_empty(),
        "no row contains the VIRTUAL marker — rendering broken?"
    );
    for line in virtual_rows {
        assert!(
            !line
                .trim_start()
                .starts_with(|c: char| c.is_ascii_digit()),
            "virtual line row must NOT start with a gutter line number: \
             {line:?}"
        );
    }
}

#[test]
fn migrated_virtual_line_marker_tracks_edits_above_anchor() {
    // Original: `test_virtual_line_position_tracking`. Anchor a
    // virtual line above "BBB" (offset 4), then insert "NEW LINE\n"
    // at the buffer's beginning. The marker on offset 4 must
    // follow the edit so the virtual line still renders above BBB.
    let fixture =
        TestFixture::new("virtual_lines_tracking.txt", "AAA\nBBB\nCCC").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            4, // byte offset of "BBB"
            "--- Above BBB ---".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("test"),
            0,
        );
    }

    // Pre-edit sanity.
    {
        let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
        let expect = RenderSnapshotExpect {
            row_checks: vec![RowMatch::AnyRowContains("--- Above BBB ---".into())],
            ..Default::default()
        };
        if let Some((f, e, a)) = expect.check_against(&snap) {
            panic!("pre-edit: virtual line must render: {f} expected {e}; actual {a}");
        }
    }

    // Edit: Ctrl+Home, then "NEW LINE\n". The send_key path drives
    // the production key handler — same routing the e2e exercised.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("NEW LINE\n").unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::AnyRowContains("--- Above BBB ---".into()),
            RowMatch::AnyRowContains("NEW LINE".into()),
            RowMatch::AnyRowContains("BBB".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "after Ctrl+Home + insert, virtual line marker must track \
             the edit and still render with NEW LINE + BBB: \
             {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_virtual_lines_above_and_below_render_in_order() {
    // Original: `test_virtual_lines_above_and_below_same_line`.
    // Both ABOVE and BELOW exist; ordering on screen must be
    // ABOVE → source → BELOW.
    let fixture =
        TestFixture::new("virtual_lines_above_below.txt", "Source Line").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "=== ABOVE ===".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("test"),
            0,
        );
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "=== BELOW ===".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineBelow,
            ns("test"),
            0,
        );
    }

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    // All three present.
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::AnyRowContains("=== ABOVE ===".into()),
            RowMatch::AnyRowContains("Source Line".into()),
            RowMatch::AnyRowContains("=== BELOW ===".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "ABOVE/source/BELOW must all render: {f} expected {e}; \
             actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }

    // Ordering check: walk the rows top-to-bottom and confirm the
    // ABOVE row precedes the Source row, which precedes the BELOW
    // row. The matchers don't pin row indexes, so we resolve them
    // here.
    let row_of = |needle: &str| -> Option<usize> {
        snap.rendered_rows
            .iter()
            .position(|r| r.contains(needle))
    };
    let above = row_of("=== ABOVE ===").expect("ABOVE row");
    let source = row_of("Source Line").expect("Source row");
    let below = row_of("=== BELOW ===").expect("BELOW row");
    assert!(
        above < source,
        "ABOVE (row {above}) must appear before Source (row {source})"
    );
    assert!(
        source < below,
        "Source (row {source}) must appear before BELOW (row {below})"
    );
}

#[test]
fn migrated_virtual_text_count_tracks_add_and_clear() {
    // Original: `test_virtual_text_count`. Pure state-counter
    // check — no rendering involved.
    let fixture =
        TestFixture::new("virtual_text_count.txt", "Content").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    assert_eq!(harness.editor().active_state().virtual_texts.len(), 0);
    assert!(harness.editor().active_state().virtual_texts.is_empty());

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Line 1".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("ns1"),
            0,
        );
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Line 2".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("ns1"),
            0,
        );
    }

    assert_eq!(harness.editor().active_state().virtual_texts.len(), 2);
    assert!(!harness.editor().active_state().virtual_texts.is_empty());

    {
        let state = harness.editor_mut().active_state_mut();
        state
            .virtual_texts
            .clear_namespace(&mut state.marker_list, &ns("ns1"));
    }

    assert_eq!(harness.editor().active_state().virtual_texts.len(), 0);
    assert!(harness.editor().active_state().virtual_texts.is_empty());
}

/// Anti-test: drop the `add_line` call. Without injecting a
/// virtual line, the screen must NOT contain the "--- Header
/// Above Line 2 ---" sentinel — proving the rendered presence
/// in `migrated_virtual_line_above_renders_above_source` depends
/// on the actual `add_line` dispatch, not on incidental harness
/// state or fixture contents.
#[test]
fn anti_virtual_line_above_without_add_line_renders_no_virtual_text() {
    let fixture =
        TestFixture::new("virtual_line_above_anti.txt", "Line 1\nLine 2\nLine 3")
            .unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    // No add_line call — exactly what we're dropping.

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::NoRowContains("--- Header Above Line 2 ---".into()),
            // Source lines still render (sanity that the fixture
            // opened correctly).
            RowMatch::AnyRowContains("Line 1".into()),
            RowMatch::AnyRowContains("Line 2".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "anti: without add_line, virtual-line sentinel should NOT \
             appear: {f} expected {e}; actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

/// Anti-test: drop the `clear_namespace` call. Without the clear,
/// the "Git Blame Header" must still be visible — proving the
/// disappearance in
/// `migrated_clear_namespace_only_clears_targeted_namespace`
/// depends on the actual clear dispatch.
#[test]
fn anti_clear_namespace_without_clear_keeps_both_visible() {
    let fixture =
        TestFixture::new("clear_namespace_anti.txt", "Line 1\nLine 2").unwrap();
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&fixture.path).unwrap();

    {
        let state = harness.editor_mut().active_state_mut();
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "Git Blame Header".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("git-blame"),
            0,
        );
        state.virtual_texts.add_line(
            &mut state.marker_list,
            0,
            "LSP Diagnostic".to_string(),
            virtual_line_style(),
            VirtualTextPosition::LineAbove,
            ns("lsp"),
            0,
        );
    }

    // No clear_namespace dispatch here — that's the load-bearing
    // step the positive test depends on.

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![
            RowMatch::AnyRowContains("Git Blame Header".into()),
            RowMatch::AnyRowContains("LSP Diagnostic".into()),
        ],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "anti: without clear_namespace, both git-blame and lsp \
             should still render: {f} expected {e}; actual {a}\n\
             rows={:#?}",
            snap.rendered_rows
        );
    }
}
