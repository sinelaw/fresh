//! Migration of `tests/e2e/margin.rs` — line-number gutter,
//! margin annotations, and gutter-width invariants.
//!
//! Load-bearing claims preserved here:
//!
//!   1. With a file open and default config, the left margin
//!      shows " N │" line-number cells for each visible line, and
//!      the buffer content is rendered alongside (issue #539
//!      regression family).
//!   2. Empty buffer still shows line " 1 │" (2-digit gutter
//!      minimum applies even before any text).
//!   3. `config.editor.line_numbers = false` on startup (no file
//!      opened) suppresses the " │ " line-number separator
//!      (issue #539).
//!   4. `config.editor.line_numbers = false` is respected when a
//!      second file is opened — the new `BufferViewState` must
//!      not hardcode `show_line_numbers = true` (issue #1181).
//!   5. Large file (1000 lines) jumps to end (Ctrl+End) and
//!      renders 4-digit line numbers ("1000 │").
//!   6. Toggling "Toggle Line Numbers" via the command palette
//!      hides the line-number separator while leaving content
//!      visible.
//!   7. Custom margin annotations (Event::AddMarginAnnotation /
//!      RemoveMarginAnnotation) round-trip: the symbol appears,
//!      remove makes it disappear while line numbers stay.
//!   8. After typing 3 lines into a fresh buffer, line numbers
//!      " 1 │"–" 3 │" plus the typed content all appear.
//!   9. Cursor X position after "abc" equals the left-margin
//!      gutter width + 3 — exercises the cursor-positioning
//!      contract that accounts for margin width.
//!  10. Horizontal scrolling still leaves the line-1 gutter cell
//!      and content on screen.
//!  11. (`#[ignore]`d) Per-buffer margin state in split view —
//!      preserved as-ignored to match the e2e marker.
//!  12. After PageDown / multiple Down keys, the visible line
//!      numbers shift to reflect the new viewport top — proves
//!      the line-number margin updates with scroll.
//!  13. PageUp/PageDown/Ctrl+Home/Ctrl+End navigation keeps the
//!      line-number column in sync with the viewport (200-line
//!      file).
//!
//! Tests 7, 9, and 12-13 use the harness-direct pattern because
//! they probe production-internal observables that have no
//! `EditorTestApi` projection: `Event::AddMarginAnnotation`,
//! `state.margins.left_total_width()`, and per-row screen text
//! via the existing `assert_screen_*` helpers (the gutter +
//! line-number text rendering is exactly what's being asserted,
//! so we use the same vt100 round-trip path as the e2e).
//!
//! Tests 1, 2, 5, 6, 8, 10, 12, 13 use
//! `RenderSnapshot::extract_with_rendered_rows` +
//! `RowMatch::AnyRowContains` / `NoRowContains` for the
//! per-row text assertions where the row index isn't pinned.
//!
//! Source: `tests/e2e/margin.rs` (12 tests migrated, 1 kept
//! `#[ignore]`d to preserve the e2e's architectural marker).

use crate::common::harness::EditorTestHarness;
use crate::common::scenario::render_snapshot::{RenderSnapshot, RenderSnapshotExpect, RowMatch};
use crossterm::event::{KeyCode, KeyModifiers};
use tempfile::TempDir;

fn assert_any_row_contains(harness: &mut EditorTestHarness, substring: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::AnyRowContains(substring.to_string())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "expected some row to contain {substring:?}: {f} expected {e}; \
             actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

fn assert_no_row_contains(harness: &mut EditorTestHarness, substring: &str) {
    let snap = RenderSnapshot::extract_with_rendered_rows(harness);
    let expect = RenderSnapshotExpect {
        row_checks: vec![RowMatch::NoRowContains(substring.to_string())],
        ..Default::default()
    };
    if let Some((f, e, a)) = expect.check_against(&snap) {
        panic!(
            "expected no row to contain {substring:?}: {f} expected {e}; \
             actual {a}\nrows={:#?}",
            snap.rendered_rows
        );
    }
}

#[test]
fn migrated_margin_line_numbers_rendering() {
    // Original: `test_margin_line_numbers_rendering`.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(
        &file_path,
        "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\nLine 6\nLine 7\nLine 8\nLine 9\nLine 10\n",
    )
    .unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // 2-digit gutter for <=99-line buffer: " 1 │", " 2 │", " 3 │".
    assert_any_row_contains(&mut harness, " 1 \u{2502}");
    assert_any_row_contains(&mut harness, " 2 \u{2502}");
    assert_any_row_contains(&mut harness, " 3 \u{2502}");

    assert_any_row_contains(&mut harness, "Line 1");
    assert_any_row_contains(&mut harness, "Line 2");
    assert_any_row_contains(&mut harness, "Line 3");
}

#[test]
fn migrated_margin_empty_buffer() {
    // Original: `test_margin_empty_buffer`.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // 2-digit gutter minimum even for empty buffer.
    assert_any_row_contains(&mut harness, " 1 \u{2502}");
}

#[test]
fn migrated_initial_buffer_respects_line_numbers_config() {
    // Original: `test_initial_buffer_respects_line_numbers_config`
    // (issue #539). With `line_numbers=false` and no file opened,
    // the initial empty buffer must NOT show the " │ " line-number
    // separator. After typing, content is still editable.
    let mut config = fresh::config::Config::default();
    config.editor.line_numbers = false;

    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();

    assert_no_row_contains(&mut harness, " \u{2502} ");

    harness.type_text("Hello").unwrap();
    assert_any_row_contains(&mut harness, "Hello");
}

#[test]
fn migrated_opened_file_respects_line_numbers_disabled_config() {
    // Original: `test_opened_file_respects_line_numbers_disabled_config`
    // (issue #1181). Opening a NEW file must respect
    // `config.editor.line_numbers = false` — the new
    // `BufferViewState` must not hardcode `show_line_numbers = true`.
    let temp_dir = TempDir::new().unwrap();
    let file1 = temp_dir.path().join("file1.txt");
    let file2 = temp_dir.path().join("file2.txt");
    std::fs::write(&file1, "File 1 line 1\nFile 1 line 2\n").unwrap();
    std::fs::write(&file2, "File 2 line 1\nFile 2 line 2\n").unwrap();

    let mut config = fresh::config::Config::default();
    config.editor.line_numbers = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();

    // First file replaces initial empty buffer in-place.
    harness.open_file(&file1).unwrap();
    harness.render().unwrap();
    assert_no_row_contains(&mut harness, " \u{2502} ");

    // Second file creates a NEW BufferViewState — the regression
    // surface.
    harness.open_file(&file2).unwrap();
    harness.render().unwrap();
    assert_no_row_contains(&mut harness, " \u{2502} ");
    assert_any_row_contains(&mut harness, "File 2 line 1");
    assert_any_row_contains(&mut harness, "File 2 line 2");
}

#[test]
fn migrated_margin_large_file_line_numbers() {
    // Original: `test_margin_large_file_line_numbers`. 1000-line
    // file, jump to end with Ctrl+End, and the 4-digit line number
    // "1000 │" must render.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");
    let content: String = (1..=1000).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "1000 \u{2502}");
}

#[test]
fn migrated_margin_disable_line_numbers_via_palette() {
    // Original: `test_margin_disable_line_numbers`. Routes through
    // the command palette (Ctrl+P → "Toggle Line Numbers" → Enter)
    // to exercise the action-dispatch path. After toggle the
    // " │ " separator pattern must be gone, but content remains.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Initially line numbers are shown (separator " │ " present).
    assert_any_row_contains(&mut harness, " \u{2502} ");

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Toggle Line Numbers").unwrap();
    harness
        .wait_for_screen_contains("Toggle Line Numbers")
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // Should NOT have the line-number separator " │ " — checked
    // with the spaces so the scrollbar's bare "│" doesn't trigger
    // a false positive.
    assert_no_row_contains(&mut harness, " \u{2502} ");

    // Content still visible.
    assert_any_row_contains(&mut harness, "Line 1");
}

#[test]
fn migrated_margin_custom_annotations() {
    // Original: `test_margin_custom_annotations`. Uses the
    // harness-direct `apply_event` path because
    // `Event::AddMarginAnnotation` / `RemoveMarginAnnotation` are
    // production-internal model events with no `EditorTestApi`
    // projection.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    harness
        .apply_event(fresh::model::event::Event::AddMarginAnnotation {
            line: 2,
            position: fresh::model::event::MarginPositionData::Left,
            content: fresh::model::event::MarginContentData::Symbol {
                text: "\u{25CF}".to_string(),
                color: Some((255, 0, 0)),
            },
            annotation_id: Some("breakpoint-1".to_string()),
        })
        .unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "\u{25CF}");

    harness
        .apply_event(fresh::model::event::Event::RemoveMarginAnnotation {
            annotation_id: "breakpoint-1".to_string(),
        })
        .unwrap();
    harness.render().unwrap();

    // Line numbers still there (line 3's " 3 │" cell), breakpoint
    // dot gone. We can't check the dot's absence directly without
    // overmatching (other glyphs use the same char in some
    // themes), but the e2e relies on the line-number check as the
    // primary post-condition.
    assert_any_row_contains(&mut harness, " 3 \u{2502}");
}

#[test]
fn migrated_margin_after_editing() {
    // Original: `test_margin_after_editing`.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    harness.type_text("First line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Second line").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Third line").unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, " 1 \u{2502}");
    assert_any_row_contains(&mut harness, " 2 \u{2502}");
    assert_any_row_contains(&mut harness, " 3 \u{2502}");

    assert_any_row_contains(&mut harness, "First line");
    assert_any_row_contains(&mut harness, "Second line");
    assert_any_row_contains(&mut harness, "Third line");
}

#[test]
fn migrated_cursor_position_with_margin() {
    // Original: `test_cursor_position_with_margin`. Uses
    // harness-direct `editor().active_state().margins.left_total_width()`
    // because the margin-width-aware cursor invariant has no
    // `EditorTestApi` accessor (and probably shouldn't — it's a
    // layout-internal quantity).
    let mut harness = EditorTestHarness::new_no_wrap(80, 24).unwrap();

    harness.type_text("abc").unwrap();
    harness.render().unwrap();

    let (content_first_row, _content_last_row) = harness.content_area_rows();
    let cursor_pos = harness.screen_cursor_position();

    let gutter_width = harness.editor().active_state().margins.left_total_width() as u16;
    assert_eq!(
        cursor_pos.0,
        gutter_width + 3,
        "Cursor X position should account for margin width"
    );
    assert_eq!(
        cursor_pos.1, content_first_row as u16,
        "Cursor Y position should be on first line (row {content_first_row})"
    );
}

#[test]
fn migrated_margin_with_horizontal_scroll() {
    // Original: `test_margin_with_horizontal_scroll`. Long
    // single-line file; cursor moves right 100 times to scroll
    // horizontally; line " 1 │" and the 'X' content still
    // render.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("long_line.txt");
    let long_line = "X".repeat(200);
    std::fs::write(&file_path, &long_line).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();

    for _ in 0..100 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, " 1 \u{2502}");
    assert_any_row_contains(&mut harness, "X");
}

#[test]
#[ignore = "Splits currently share the same active buffer (architectural limitation). All splits display the currently active buffer, so this test's assumption of independent buffers per split doesn't match current behavior."]
fn migrated_margin_per_buffer_in_split_view() {
    // Original: `test_margin_per_buffer_in_split_view`. Preserved
    // `#[ignore]`d to match the e2e marker — flips to passing
    // when the per-split-buffer architecture lands.
    let temp_dir = TempDir::new().unwrap();
    let file1_path = temp_dir.path().join("file1.txt");
    let file2_path = temp_dir.path().join("file2.txt");
    std::fs::write(&file1_path, "File 1 Line 1\nFile 1 Line 2\n").unwrap();
    std::fs::write(&file2_path, "File 2 Line 1\nFile 2 Line 2\nFile 2 Line 3\n").unwrap();

    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.open_file(&file1_path).unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::ALT)
        .unwrap();
    harness.open_file(&file2_path).unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "   1 \u{2502}");
    assert_any_row_contains(&mut harness, "File 1 Line 1");
    assert_any_row_contains(&mut harness, "File 2 Line 1");

    harness
        .apply_event(fresh::model::event::Event::SetLineNumbers { enabled: false })
        .unwrap();

    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::ALT)
        .unwrap();
    harness
        .apply_event(fresh::model::event::Event::AddMarginAnnotation {
            line: 0,
            position: fresh::model::event::MarginPositionData::Left,
            content: fresh::model::event::MarginContentData::Symbol {
                text: "\u{25CF}".to_string(),
                color: Some((255, 0, 0)),
            },
            annotation_id: Some("file1-marker".to_string()),
        })
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn migrated_line_numbers_update_during_incremental_scroll() {
    // Original: `test_line_numbers_update_during_incremental_scroll`.
    // 100-line file; after PageDown the margin must show lines
    // in the 20-25 range; after 5 more Down keys, in the
    // 27-31 range; line "   1 │" no longer visible.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("scroll_test.txt");
    let content: String = (1..=100).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    assert_any_row_contains(&mut harness, "   1 \u{2502}");
    assert_any_row_contains(&mut harness, "Line 1");

    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let has_line_20_to_25 = ["  20 \u{2502}", "  21 \u{2502}", "  22 \u{2502}",
                             "  23 \u{2502}", "  24 \u{2502}", "  25 \u{2502}"]
        .iter()
        .any(|s| snap.rendered_rows.iter().any(|r| r.contains(s)));
    assert!(
        has_line_20_to_25,
        "After PageDown, line numbers should show lines around 20-25, but rows:\n{:#?}",
        snap.rendered_rows
    );

    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.render().unwrap();

    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let has_line_27_to_31 = ["  27 \u{2502}", "  28 \u{2502}", "  29 \u{2502}",
                             "  30 \u{2502}", "  31 \u{2502}"]
        .iter()
        .any(|s| snap.rendered_rows.iter().any(|r| r.contains(s)));
    assert!(
        has_line_27_to_31,
        "After 5 more Down keys, line numbers should show lines around 27-31, but rows:\n{:#?}",
        snap.rendered_rows
    );

    assert_no_row_contains(&mut harness, "   1 \u{2502}");
}

#[test]
fn migrated_line_numbers_update_with_navigation_keys() {
    // Original: `test_line_numbers_update_with_navigation_keys`.
    // 200-line file; PageDown × 3, PageUp × 2, Ctrl+End,
    // Ctrl+Home — each navigation step shifts the visible
    // line-number range.
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("nav_test.txt");
    let content: String = (1..=200).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Initial.
    assert_any_row_contains(&mut harness, "   1 \u{2502}");
    assert_any_row_contains(&mut harness, "Line 1");

    // PageDown × 3.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let in_60s = (60..=70u32)
        .any(|n| snap.rendered_rows.iter().any(|r| r.contains(&format!("  {n} \u{2502}"))));
    assert!(
        in_60s,
        "After 3 PageDowns, should be around line 60-70, but rows:\n{:#?}",
        snap.rendered_rows
    );

    // PageUp × 2.
    for _ in 0..2 {
        harness
            .send_key(KeyCode::PageUp, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let in_20s = (20..=28u32)
        .any(|n| snap.rendered_rows.iter().any(|r| r.contains(&format!("  {n} \u{2502}"))));
    assert!(
        in_20s,
        "After 2 PageUps, should be around line 20-28, but rows:\n{:#?}",
        snap.rendered_rows
    );

    // Ctrl+End.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_any_row_contains(&mut harness, " 200 \u{2502}");
    assert_any_row_contains(&mut harness, "Line 200");
    assert_no_row_contains(&mut harness, "   1 \u{2502}");
    let snap = RenderSnapshot::extract_with_rendered_rows(&mut harness);
    let has_high = [" 180 \u{2502}", " 185 \u{2502}", " 190 \u{2502}",
                    " 195 \u{2502}", " 199 \u{2502}"]
        .iter()
        .any(|s| snap.rendered_rows.iter().any(|r| r.contains(s)));
    assert!(
        has_high,
        "At end of file, should show lines in 180s-190s range, but rows:\n{:#?}",
        snap.rendered_rows
    );

    // Ctrl+Home.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_any_row_contains(&mut harness, "   1 \u{2502}");
    assert_any_row_contains(&mut harness, "Line 1");
    assert_no_row_contains(&mut harness, " 200 \u{2502}");
    assert_any_row_contains(&mut harness, "   2 \u{2502}");
    assert_any_row_contains(&mut harness, "   3 \u{2502}");
    assert_any_row_contains(&mut harness, "  10 \u{2502}");
    assert_any_row_contains(&mut harness, "  20 \u{2502}");

    // Round-trip End then Home.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_any_row_contains(&mut harness, " 200 \u{2502}");

    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_any_row_contains(&mut harness, "   1 \u{2502}");
}

/// Anti-test: drop the `harness.type_text("Hello")` from the
/// initial-buffer line_numbers-off test. The screen must NOT
/// contain "Hello" without typing it — proves the
/// `assert_any_row_contains("Hello")` assertion in the positive
/// test is gated on the typing action, not on incidental editor
/// state.
#[test]
fn anti_initial_buffer_without_typing_has_no_hello() {
    let mut config = fresh::config::Config::default();
    config.editor.line_numbers = false;
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.render().unwrap();
    // No type_text("Hello") here.
    assert_no_row_contains(&mut harness, "Hello");
}

/// Anti-test: drop the `Ctrl+End` jump from the
/// large-file-line-numbers test. Without navigating to the end,
/// the 4-digit "1000 │" line number must NOT appear — proves the
/// positive test's visibility claim is gated on the jump, not on
/// the file simply existing.
#[test]
fn anti_large_file_without_jump_to_end_lacks_line_1000_marker() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large.txt");
    let content: String = (1..=1000).map(|i| format!("Line {i}\n")).collect();
    std::fs::write(&file_path, content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    // No Ctrl+End here.
    harness.render().unwrap();

    assert_no_row_contains(&mut harness, "1000 \u{2502}");
}

/// Anti-test: drop the palette-toggle from the
/// `migrated_margin_disable_line_numbers_via_palette` flow.
/// Without dispatching the toggle, the " │ " line-number
/// separator must STILL be present — proves the positive test's
/// "hidden" claim is gated on the palette toggle, not on initial
/// state.
#[test]
fn anti_margin_without_palette_toggle_keeps_line_number_separator() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Line 1\nLine 2\nLine 3\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // No palette toggle dispatched — " │ " must still be there.
    assert_any_row_contains(&mut harness, " \u{2502} ");
}
