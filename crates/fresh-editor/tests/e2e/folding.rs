// End-to-end tests for folding behavior and interactions

use crate::common::fixtures::TestFixture;
use crate::common::harness::{layout, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use lsp_types::FoldingRange;

fn set_fold_range(harness: &mut EditorTestHarness, start_line: usize, end_line: usize) {
    let state = harness.editor_mut().active_state_mut();
    state.folding_ranges = vec![FoldingRange {
        start_line: start_line as u32,
        end_line: end_line as u32,
        start_character: None,
        end_character: None,
        kind: None,
        collapsed_text: None,
    }];
}

fn set_top_line(harness: &mut EditorTestHarness, line: usize) {
    let top_byte = {
        let buffer = &mut harness.editor_mut().active_state_mut().buffer;
        buffer
            .line_start_offset(line)
            .unwrap_or_else(|| buffer.len())
    };
    let viewport = harness.editor_mut().active_viewport_mut();
    viewport.top_byte = top_byte;
    viewport.top_view_line_offset = 0;

    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = top_byte;
    cursors.primary_mut().anchor = None;
    cursors.primary_mut().sticky_column = 0;
}

fn set_cursor_line(harness: &mut EditorTestHarness, line: usize) {
    let pos = {
        let buffer = &mut harness.editor_mut().active_state_mut().buffer;
        buffer
            .line_start_offset(line)
            .unwrap_or_else(|| buffer.len())
    };
    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = pos;
    cursors.primary_mut().anchor = None;
    cursors.primary_mut().sticky_column = 0;
}

fn find_text_position(harness: &EditorTestHarness, needle: &str) -> (u16, u16) {
    let (start_row, end_row) = harness.content_area_rows();
    for row in start_row..=end_row {
        let text = harness.get_row_text(row as u16);
        if let Some(byte_offset) = text.find(needle) {
            // Convert byte offset to cell (column) position.
            // get_row_text concatenates cell symbols which may be multi-byte
            // (e.g. "│" is 3 bytes but 1 cell), so we count cells up to the
            // byte offset.
            let cell_col = text[..byte_offset].chars().count();
            return (row as u16, cell_col as u16);
        }
    }
    panic!(
        "Expected to find '{}' on screen.\nScreen:\n{}",
        needle,
        harness.screen_to_string()
    );
}

#[test]
fn test_fold_gutter_double_click_toggles_like_single() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..30).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_double_click.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    set_fold_range(&mut harness, 2, 6);
    harness.render().unwrap();

    let row = (layout::CONTENT_START_ROW + 2) as u16;
    let col = 0;

    // Two rapid clicks at the same gutter position should act like two single clicks
    // (fold then unfold), not trigger word selection.
    harness.mouse_click(col, row).unwrap();
    harness.mouse_click(col, row).unwrap();

    // After two toggles, the folded lines should be visible again.
    let row_text = harness.get_row_text(row + 1);
    assert!(
        row_text.contains("line 3"),
        "Expected folded lines to be visible after double click. Row text: '{row_text}'"
    );
}

#[test]
fn test_fold_click_moves_cursor_out_of_fold() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..30).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_cursor_inside.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    set_fold_range(&mut harness, 2, 6);
    harness.render().unwrap();

    // Move cursor into the fold body (line 4).
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 4)
        .unwrap();

    let cursor_line_before = harness
        .editor()
        .active_state()
        .buffer
        .get_line_number(harness.editor().active_cursors().primary().position);
    assert_eq!(
        cursor_line_before, 4,
        "Precondition failed: cursor not inside fold body."
    );

    let row = (layout::CONTENT_START_ROW + 2) as u16;
    harness.mouse_click(0, row).unwrap();

    let cursor_line_after = harness
        .editor()
        .active_state()
        .buffer
        .get_line_number(harness.editor().active_cursors().primary().position);
    assert_eq!(
        cursor_line_after, 2,
        "Cursor should move to fold header when collapsing."
    );

    let row_text = harness.get_row_text(row + 1);
    assert!(
        row_text.contains("line 7"),
        "Expected fold to collapse even when cursor was inside. Row text: '{row_text}'"
    );
}

#[test]
fn test_mouse_scroll_skips_folded_lines() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..120).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_scroll.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    let header_line = 10usize;
    let end_line = 20usize;
    set_fold_range(&mut harness, header_line, end_line);
    harness.render().unwrap();
    let header_row = (layout::CONTENT_START_ROW + header_line) as u16;
    harness.mouse_click(0, header_row).unwrap();

    set_top_line(&mut harness, header_line);
    harness.render().unwrap();

    // Scroll down once; top line should not land inside the folded range.
    harness
        .mouse_scroll_down(0, layout::CONTENT_START_ROW as u16)
        .unwrap();

    let top_line = harness.top_line_number();
    assert!(
        top_line <= header_line || top_line > end_line,
        "Top line should skip folded region. top_line={top_line}, folded=[{}..{}]",
        header_line + 1,
        end_line
    );
}

#[test]
fn test_cursor_down_skips_folded_lines() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..40).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_cursor_down.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    let header_line = 2usize;
    let end_line = 6usize;
    set_fold_range(&mut harness, header_line, end_line);
    harness.render().unwrap();

    // Collapse the fold without moving the cursor into it.
    let buffer_id = harness.editor().active_buffer();
    harness
        .editor_mut()
        .toggle_fold_at_line(buffer_id, header_line);
    harness.render().unwrap();

    // Move cursor to line before header (line 1).
    let line1_byte = harness
        .editor_mut()
        .active_state_mut()
        .buffer
        .line_start_offset(1)
        .unwrap();
    harness
        .editor_mut()
        .active_cursors_mut()
        .primary_mut()
        .position = line1_byte;
    harness.render().unwrap();

    // Move down into the fold; it should skip to the first visible line after the fold.
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 2)
        .unwrap();

    let cursor_line_after = harness
        .editor()
        .active_state()
        .buffer
        .get_line_number(harness.editor().active_cursors().primary().position);
    assert_eq!(
        cursor_line_after,
        end_line + 1,
        "Cursor should skip folded lines when moving down."
    );

    let row = (layout::CONTENT_START_ROW + header_line) as u16;
    let row_text = harness.get_row_text(row + 1);
    assert!(
        row_text.contains("line 7"),
        "Fold should remain collapsed while moving down. Row text: '{row_text}'"
    );
}

#[test]
fn test_folding_preserves_syntax_highlighting_after_skip() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let mut lines: Vec<String> = (0..120).map(|i| format!("line {i}\n")).collect();
    lines[80] = "def highlighted_function():\n".to_string();
    lines[81] = "    return 1\n".to_string();
    let content = lines.concat();

    let fixture = TestFixture::new("fold_highlight.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    // Baseline: capture highlight style for "def" without folding.
    set_cursor_line(&mut harness, 80);
    harness.render().unwrap();

    let (def_row, def_col) = find_text_position(&harness, "def highlighted_function");
    let (plain_row, plain_col) = find_text_position(&harness, "line 79");

    let def_style = harness
        .get_cell_style(def_col, def_row)
        .expect("Expected style for 'def'");
    let plain_style = harness
        .get_cell_style(plain_col, plain_row)
        .expect("Expected style for plain text");

    assert_ne!(
        def_style, plain_style,
        "Precondition failed: keyword highlight should differ from plain text."
    );

    // Fold a large range above the highlighted line.
    set_fold_range(&mut harness, 10, 70);
    harness.render().unwrap();
    let header_row = (layout::CONTENT_START_ROW + 10) as u16;
    harness.mouse_click(0, header_row).unwrap();
    set_cursor_line(&mut harness, 80);
    harness.render().unwrap();

    let (def_row_after, def_col_after) = find_text_position(&harness, "def highlighted_function");
    let def_style_after = harness
        .get_cell_style(def_col_after, def_row_after)
        .expect("Expected style for 'def' after folding");

    assert_eq!(
        def_style_after, def_style,
        "Syntax highlighting should remain stable after folding."
    );
}

#[test]
fn test_cursor_down_up_keeps_fold_collapsed() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..40).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_cursor_down_up.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    let header_line = 2usize;
    let end_line = 6usize;
    set_fold_range(&mut harness, header_line, end_line);
    harness.render().unwrap();

    // Collapse the fold without moving the cursor into it.
    let buffer_id = harness.editor().active_buffer();
    harness
        .editor_mut()
        .toggle_fold_at_line(buffer_id, header_line);
    harness.render().unwrap();

    // Move cursor to line before header (line 1).
    let line1_byte = harness
        .editor_mut()
        .active_state_mut()
        .buffer
        .line_start_offset(1)
        .unwrap();
    harness
        .editor_mut()
        .active_cursors_mut()
        .primary_mut()
        .position = line1_byte;
    harness.render().unwrap();

    // Move down across the fold and then back up.
    harness
        .send_key_repeat(KeyCode::Down, KeyModifiers::NONE, 2)
        .unwrap();
    harness
        .send_key_repeat(KeyCode::Up, KeyModifiers::NONE, 1)
        .unwrap();

    let row = (layout::CONTENT_START_ROW + header_line) as u16;
    let row_text = harness.get_row_text(row + 1);
    assert!(
        row_text.contains("line 7"),
        "Fold should remain collapsed after down/up movement. Row text: '{row_text}'"
    );
}

#[test]
fn test_folded_viewport_inside_range_fills_lines() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..200).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_viewport_inside.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    let header_line = 10usize;
    let end_line = 120usize;
    set_fold_range(&mut harness, header_line, end_line);
    harness.render().unwrap();

    let header_row = (layout::CONTENT_START_ROW + header_line) as u16;
    harness.mouse_click(0, header_row).unwrap();

    // Simulate a scroll position inside the folded range.
    {
        let top_byte = {
            let buffer = &mut harness.editor_mut().active_state_mut().buffer;
            buffer
                .line_start_offset(header_line + 20)
                .unwrap_or_else(|| buffer.len())
        };
        let viewport = harness.editor_mut().active_viewport_mut();
        viewport.top_byte = top_byte;
        viewport.top_view_line_offset = 0;
        viewport.set_skip_ensure_visible();
    }
    {
        let cursor_pos = {
            let buffer = &mut harness.editor_mut().active_state_mut().buffer;
            buffer
                .line_start_offset(end_line + 5)
                .unwrap_or_else(|| buffer.len())
        };
        let cursors = harness.editor_mut().active_cursors_mut();
        cursors.primary_mut().position = cursor_pos;
        cursors.primary_mut().anchor = None;
        cursors.primary_mut().sticky_column = 0;
    }
    harness.render().unwrap();

    let (start_row, end_row) = harness.content_area_rows();
    let top_row_text = harness.get_row_text(start_row as u16);
    let bottom_row_text = harness.get_row_text(end_row as u16);
    let expected = format!("line {}", end_line + 1);

    assert!(
        top_row_text.contains(&expected),
        "Expected viewport to start after fold. Top row: '{top_row_text}'"
    );
    assert!(
        bottom_row_text.contains("line "),
        "Expected viewport to be filled with content, not tildes. Bottom row: '{bottom_row_text}'"
    );
}

/// Extract a 0-indexed line number from content matching the pattern "line X".
fn parse_content_line_num(content: &str) -> Option<usize> {
    let pos = content.find("line ")?;
    let after = &content[pos + 5..];
    let num_str: String = after.chars().take_while(|c| c.is_ascii_digit()).collect();
    if num_str.is_empty() {
        return None;
    }
    num_str.parse().ok()
}

/// For every visible row, assert that the gutter line number matches the "line X"
/// content on that row.  Gutter numbers are 1-indexed; content uses 0-indexed
/// "line X".  Rows that don't have a parseable line number (continuations, tilde
/// lines) are skipped.
fn assert_gutter_matches_content(harness: &EditorTestHarness) {
    let (start_row, end_row) = harness.content_area_rows();
    let mut checked = 0usize;

    for row in start_row..=end_row {
        let text = harness.get_row_text(row as u16);

        // Skip tilde lines (EOF markers)
        if text.trim_start().starts_with('~') {
            continue;
        }

        // The gutter and content are separated by '│'.
        let Some(sep) = text.find('│') else {
            continue;
        };
        let gutter = &text[..sep];
        let content = &text[sep + '│'.len_utf8()..];

        // Extract the line number from the gutter (digits only, ignoring indicators).
        let digits: String = gutter.chars().filter(|c| c.is_ascii_digit()).collect();
        let Ok(gutter_num) = digits.parse::<usize>() else {
            continue; // continuation / blank gutter
        };

        // Extract the 0-indexed number from "line X" in the content.
        let Some(content_num) = parse_content_line_num(content) else {
            continue;
        };

        assert_eq!(
            gutter_num,
            content_num + 1,
            "Gutter shows {} but content is 'line {}' (expected gutter {})\nRow {}: '{}'",
            gutter_num,
            content_num,
            content_num + 1,
            row,
            text,
        );
        checked += 1;
    }
    assert!(checked > 0, "Should have verified at least one line number");
}

/// Reproduce the gutter-line-number bug after folding.
///
/// With a fold active, `current_source_line_num` in the renderer increments
/// sequentially instead of reflecting the true buffer line.  This test creates
/// a 60-line file, folds lines 10..30 via a fake LSP, then scrolls down and
/// back up one line at a time, checking every rendered frame.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_folded_gutter_line_numbers_match_content_during_scroll() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    // 1. Spawn fake LSP that advertises foldingRangeProvider and returns
    //    a single range covering lines 10..30.
    let _fake_server = FakeLspServer::spawn_with_folding_ranges()?;

    // 2. Create a 60-line file where every line is "line N\n".
    let temp_dir = tempfile::tempdir()?;
    let content: String = (0..60).map(|i| format!("line {i}\n")).collect();
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, &content)?;

    // 3. Wire up the editor to use the fake LSP for Rust files.
    //    Semantic tokens must be enabled because the current render path only
    //    calls maybe_request_folding_ranges_debounced inside the semantic-ranges
    //    loop.
    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::folding_ranges_script_path()
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
        },
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // 4. Wait for the LSP to deliver folding ranges (the expand indicator ▾
    //    appears in the gutter once ranges are available).
    harness.wait_for_screen_contains("▾")?;

    // 5. Collapse the fold by clicking the gutter indicator at line 10.
    let fold_row = layout::CONTENT_START_ROW as u16 + 10;
    harness.mouse_click(0, fold_row)?;

    // Sanity: fold indicator should now be the collapsed marker ▸.
    harness.assert_screen_contains("▸");
    // Hidden lines must disappear, first post-fold line must be visible.
    harness.assert_screen_not_contains("line 11");
    harness.assert_screen_contains("line 31");

    // 6. Verify gutter numbers are correct in the initial view.
    assert_gutter_matches_content(&harness);

    // 7. Scroll down one line at a time, checking at every step.
    //    60 presses is more than enough to reach the end (only ~40 visible lines).
    for _ in 0..60 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
        assert_gutter_matches_content(&harness);
    }

    // 8. Scroll back up, checking at every step.
    for _ in 0..60 {
        harness.send_key(KeyCode::Up, KeyModifiers::NONE)?;
        assert_gutter_matches_content(&harness);
    }

    Ok(())
}

/// Unfold must work even when `folding_ranges` is empty (e.g. LSP disconnected
/// or returned an empty response after ranges were previously available).
///
/// `toggle_fold_at_line` currently returns early when `folding_ranges.is_empty()`,
/// which prevents expanding an already-collapsed fold.  This test folds a range
/// while LSP ranges are present, then clears `folding_ranges` and attempts to
/// unfold.
#[test]
fn test_unfold_works_after_folding_ranges_cleared() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    let content: String = (0..30).map(|i| format!("line {i}\n")).collect();
    let fixture = TestFixture::new("fold_unfold_no_lsp.py", &content).unwrap();
    harness.open_file(&fixture.path).unwrap();

    // Set up a fold range and collapse it.
    set_fold_range(&mut harness, 5, 10);
    harness.render().unwrap();

    let buffer_id = harness.editor().active_buffer();
    harness.editor_mut().toggle_fold_at_line(buffer_id, 5);
    harness.render().unwrap();

    // Verify lines 6-10 are hidden.
    harness.assert_screen_not_contains("line 6");
    harness.assert_screen_not_contains("line 9");

    // Simulate LSP disconnect: clear folding_ranges.
    harness
        .editor_mut()
        .active_state_mut()
        .folding_ranges
        .clear();

    // Attempt to unfold — the fold markers still exist in the FoldManager.
    let buffer_id = harness.editor().active_buffer();
    harness.editor_mut().toggle_fold_at_line(buffer_id, 5);
    harness.render().unwrap();

    // The fold should have been expanded: hidden lines should be visible again.
    harness.assert_screen_contains("line 6");
    harness.assert_screen_contains("line 9");
}

/// Scrolling should trigger at the same cursor-to-edge distance with and
/// without folded code.  The viewport's `scroll_offset` (default 3) keeps
/// the cursor at least 3 visible lines from the top/bottom edge.
///
/// This test places a fold *inside* the viewport so the cursor actually
/// traverses through/across it while scrolling.  We build two equivalent
/// scenarios:
///
///   - "plain": 60 contiguous visible lines (lines 0..59).
///   - "folded": 70 source lines with lines 11-20 folded away, leaving the
///     same 60 visible lines.
///
/// Starting from the top and pressing Down repeatedly, every step should
/// produce identical (cursor_screen_row, viewport_top_visible_line) pairs.
/// Same for scrolling back up.
#[test]
fn test_scroll_margin_identical_with_and_without_fold() {
    /// Record (cursor_screen_row_in_content, top_visible_line) after each
    /// Down / Up key press.
    fn collect_scroll_trace(
        harness: &mut EditorTestHarness,
        steps_down: usize,
        steps_up: usize,
    ) -> (Vec<(usize, usize)>, Vec<(usize, usize)>) {
        let mut down = Vec::new();
        let mut up = Vec::new();
        let (start_row, _) = harness.content_area_rows();

        for _ in 0..steps_down {
            harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
            let (_, cy) = harness.screen_cursor_position();
            down.push((
                (cy as usize).saturating_sub(start_row),
                harness.top_line_number(),
            ));
        }
        for _ in 0..steps_up {
            harness.send_key(KeyCode::Up, KeyModifiers::NONE).unwrap();
            let (_, cy) = harness.screen_cursor_position();
            up.push((
                (cy as usize).saturating_sub(start_row),
                harness.top_line_number(),
            ));
        }
        (down, up)
    }

    // visible_lines is the number of visible lines the user can see in both
    // scenarios.  In the "plain" file these are source lines 0..59.
    // In the "folded" file lines 11..20 are hidden, so visible lines map to
    // source lines 0..10, 21..69  (same count: 60).
    let visible_lines = 60usize;
    let fold_header = 10usize; // header line (stays visible)
    let fold_end = 20usize; // last hidden line
    let hidden = fold_end - fold_header; // 10 lines hidden

    let steps = visible_lines; // scroll the full visible range

    // --- Plain file: 60 lines, no folds ---
    let plain_content: String = (0..visible_lines).map(|i| format!("line {i}\n")).collect();
    let mut harness_plain = EditorTestHarness::new(80, 24).unwrap();
    let fixture_plain = TestFixture::new("scroll_plain.py", &plain_content).unwrap();
    harness_plain.open_file(&fixture_plain.path).unwrap();
    harness_plain.render().unwrap();
    let (down_plain, up_plain) = collect_scroll_trace(&mut harness_plain, steps, steps);

    // --- Folded file: 70 source lines, lines 11..20 hidden ---
    // The visible content is identical to the plain file:
    //   source  0..10  →  visible 0..10   ("line 0" .. "line 10")
    //   source 11..20  →  (hidden by fold)
    //   source 21..69  →  visible 11..59  ("line 11" .. "line 59")
    // So we label source lines to match: source N shows "line N" if
    // visible, ensuring the rendered text is the same.
    let folded_total = visible_lines + hidden; // 70 source lines
    let folded_content: String = (0..folded_total)
        .map(|src| {
            // Map source line to visible line number for the label.
            let vis = if src <= fold_header {
                src
            } else if src <= fold_end {
                // hidden lines — label doesn't matter, they won't render
                src
            } else {
                src - hidden
            };
            format!("line {vis}\n")
        })
        .collect();

    let mut harness_folded = EditorTestHarness::new(80, 24).unwrap();
    let fixture_folded = TestFixture::new("scroll_folded.py", &folded_content).unwrap();
    harness_folded.open_file(&fixture_folded.path).unwrap();

    set_fold_range(&mut harness_folded, fold_header, fold_end);
    harness_folded.render().unwrap();
    let buffer_id = harness_folded.editor().active_buffer();
    harness_folded
        .editor_mut()
        .toggle_fold_at_line(buffer_id, fold_header);
    harness_folded.render().unwrap();

    // Sanity: hidden lines must not be on screen, first post-fold line must be.
    harness_folded.assert_screen_not_contains("line 11\n");
    harness_folded.assert_screen_contains("line 10");

    let (down_folded, up_folded) = collect_scroll_trace(&mut harness_folded, steps, steps);

    // --- Compare: cursor screen row must match at every step ---
    // We only compare the cursor's screen row (index 0 of the tuple),
    // not the top_line_number, because the fold changes source line
    // numbering.  The key invariant is that the cursor stays at the
    // same distance from the viewport edges.
    //
    // Collect all mismatches before asserting so we can see the full
    // picture for both directions.
    let mut failures = Vec::new();

    for (i, (plain, folded)) in down_plain.iter().zip(down_folded.iter()).enumerate() {
        if plain.0 != folded.0 {
            failures.push(format!(
                "Down step {i}: cursor screen row differs.\n\
                 Without fold: row={}, top_line={}\n\
                 With fold:    row={}, top_line={}",
                plain.0, plain.1, folded.0, folded.1,
            ));
        }
    }
    for (i, (plain, folded)) in up_plain.iter().zip(up_folded.iter()).enumerate() {
        if plain.0 != folded.0 {
            failures.push(format!(
                "Up step {i}: cursor screen row differs.\n\
                 Without fold: row={}, top_line={}\n\
                 With fold:    row={}, top_line={}",
                plain.0, plain.1, folded.0, folded.1,
            ));
        }
    }

    assert!(
        failures.is_empty(),
        "Scroll margin mismatches ({} total):\n{}",
        failures.len(),
        failures.join("\n"),
    );
}

/// Byte-based indent folding in large-file mode (no line scan) should fold
/// exactly the correct block when the cursor is on the second of three
/// foldable regions, using the "toggle fold" command.
///
/// The file has three distinct indent blocks.  We position the cursor on
/// the header of the second block, invoke `toggle_fold_at_cursor()` (the
/// same codepath as the Toggle Fold command), and verify:
///   - the second block's body lines are hidden
///   - the first and third blocks' body lines remain visible
///   - expanding again restores the hidden lines
#[test]
fn test_byte_based_fold_toggle_targets_correct_block() {
    // Three indent blocks with unique body markers.
    let content = "\
fn alpha() {
    alpha_body_1
    alpha_body_2
    alpha_body_3
}
fn beta() {
    beta_body_1
    beta_body_2
    beta_body_3
}
fn gamma() {
    gamma_body_1
    gamma_body_2
    gamma_body_3
}
";

    // Lower the large-file threshold so this file triggers large-file mode.
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("byte_fold_target.rs", content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();

    // Precondition: large-file mode active, no LSP ranges.
    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "File should be loaded in large-file mode"
    );
    assert!(
        harness.editor().active_state().folding_ranges.is_empty(),
        "No LSP folding ranges should be present"
    );

    // Do NOT run a line scan — we're testing the byte-based path.
    // Render twice: first loads chunks, second computes fold indicators.
    harness.render().unwrap();
    harness.render().unwrap();

    // All three blocks should be visible initially.
    harness.assert_screen_contains("alpha_body_1");
    harness.assert_screen_contains("beta_body_1");
    harness.assert_screen_contains("gamma_body_1");

    // Position cursor directly on the "fn beta()" header by byte offset.
    let beta_offset = content.find("fn beta()").unwrap();
    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = beta_offset;
    cursors.primary_mut().anchor = None;
    harness.render().unwrap();

    // Fold the block at the cursor using the Toggle Fold command path.
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();

    // Beta block body should be hidden, with placeholder shown on header.
    harness.assert_screen_not_contains("beta_body_1");
    harness.assert_screen_not_contains("beta_body_2");
    harness.assert_screen_not_contains("beta_body_3");
    harness.assert_screen_contains("fn beta() { ...");

    // Alpha and gamma blocks should remain visible.
    harness.assert_screen_contains("alpha_body_1");
    harness.assert_screen_contains("gamma_body_1");

    // Expand the fold again via the same command.
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();

    // All body lines should be visible again.
    harness.assert_screen_contains("beta_body_1");
    harness.assert_screen_contains("beta_body_2");
    harness.assert_screen_contains("alpha_body_1");
    harness.assert_screen_contains("gamma_body_1");
}

/// Gutter click on a fold indicator in large-file mode (no line scan) must
/// fold the clicked block, not a different block.
///
/// Regression test for a bug where the gutter click path round-tripped
/// through estimated line numbers:
///
///   exact_byte → get_line_number (byte/80) → toggle_fold_at_line → line*80
///
/// With short lines the round-trip loses precision and lands on a completely
/// different block.  The fix passes the exact byte position from the click
/// through to `toggle_fold_at_byte`, bypassing estimated line numbers.
///
/// Content layout (lines ~8 bytes, est_ll=80):
///   - Block A: "fn alpha() {" + 11 body lines + "}" (lines 0..12)
///   - Blank line (line 13)
///   - Block B: "fn beta() {"  + 3 body lines + "}"  (lines 14..18)
///   - Blank line (line 19)
///   - Block C: "fn gamma() {" + 3 body lines + "}"  (lines 20..24)
///
/// Block B starts at ~byte 96.  Old code: `96/80 = line 1` → `1*80 = byte 80`
/// → lands inside block A body → folds block A instead of B.
#[test]
fn test_gutter_click_folds_correct_block_in_large_file_mode() {
    // Build content with short lines so est_ll (80) >> actual line length (~8).
    // This maximises the round-trip error through estimated line numbers.
    let mut content = String::from("fn alpha() {\n"); // line 0
    for i in 1..=11 {
        content.push_str(&format!("    a{i}\n")); // lines 1..11
    }
    content.push_str("}\n"); // line 12
    content.push_str("\n"); // line 13 (blank)

    let beta_byte = content.len();
    content.push_str("fn beta() {\n"); // line 14
    for i in 1..=3 {
        content.push_str(&format!("    b{i}\n")); // lines 15..17
    }
    content.push_str("}\n"); // line 18
    content.push_str("\n"); // line 19 (blank)
    content.push_str("fn gamma() {\n"); // line 20
    for i in 1..=3 {
        content.push_str(&format!("    g{i}\n")); // lines 21..23
    }
    content.push_str("}\n"); // line 24

    // Precondition: the round-trip through est_ll=80 MUST land before beta.
    // beta_byte / 80 * 80 < beta_byte  ⟹  the old code would miss block B.
    let round_tripped = (beta_byte / 80) * 80;
    assert!(
        round_tripped < beta_byte,
        "Test setup: round-trip byte {} should be less than beta_byte {} \
         for the regression to trigger",
        round_tripped,
        beta_byte,
    );

    // Force large-file mode so get_line_number uses byte/est_ll estimation.
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("gutter_click_fold.rs", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();

    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "File should be in large-file mode"
    );
    assert!(
        harness.editor().active_state().folding_ranges.is_empty(),
        "No LSP folding ranges should be present"
    );

    // Render twice: first loads chunks from disk, second computes fold indicators.
    harness.render().unwrap();
    harness.render().unwrap();

    // All blocks visible initially.
    harness.assert_screen_contains("a1");
    harness.assert_screen_contains("b1");
    harness.assert_screen_contains("g1");

    // Click the gutter on the row showing "fn beta() {".
    // line 14 (0-indexed) → screen row = CONTENT_START_ROW + 14
    let beta_row = (layout::CONTENT_START_ROW + 14) as u16;

    // Sanity: that row should contain beta's header.
    let row_text = harness.get_row_text(beta_row);
    assert!(
        row_text.contains("fn beta()"),
        "Expected row {} to show beta header, got: '{}'",
        beta_row,
        row_text,
    );

    // Click gutter (col 0) to fold.
    harness.mouse_click(0, beta_row).unwrap();

    // Beta body must be hidden.
    harness.assert_screen_not_contains("    b1");
    harness.assert_screen_not_contains("    b2");
    harness.assert_screen_not_contains("    b3");

    // Alpha and gamma must remain visible — this is the key assertion.
    // With the old line-number round-trip, alpha would be folded instead.
    harness.assert_screen_contains("a1");
    harness.assert_screen_contains("g1");
}

/// Indent-based folding should work on files loaded in large-file mode
/// once line feeds have been scanned.
///
/// We lower `large_file_threshold_bytes` so a small test file triggers
/// large-file mode, run the incremental line scan, then verify the fold
/// indicator appears and the fold toggle collapses/expands correctly.
#[test]
fn test_indent_folding_works_in_large_file_mode() {
    // Build a file with clear indentation structure, bigger than our
    // lowered threshold.  The header "def foo():" is followed by indented
    // body lines, making it foldable via indent heuristics.
    let mut content = String::from("def foo():\n");
    for i in 0..20 {
        content.push_str(&format!("    body {i}\n"));
    }
    content.push_str("after\n");

    // Lower the large-file threshold so this small file triggers large-file mode.
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("large_fold.py", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();

    // Precondition: the buffer should be in large-file mode.
    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "Test file should be loaded in large-file mode"
    );

    // Precondition: no LSP folding ranges (we're testing the indent fallback).
    assert!(
        harness.editor().active_state().folding_ranges.is_empty(),
        "No LSP folding ranges should be present"
    );

    // Run the incremental line-feed scan so line metadata becomes available.
    // For small files this completes in a single batch.
    harness.editor_mut().start_incremental_line_scan(false);
    while harness.editor_mut().process_line_scan() {}

    // First render loads the visible chunks from disk.
    // Second render computes fold indicators with the now-loaded data.
    harness.render().unwrap();
    harness.render().unwrap();

    // The fold indicator (▾) should appear on line 0 (the header).
    harness.assert_screen_contains("▾");

    // Collapse the fold by clicking the gutter on the first content row.
    let header_row = layout::CONTENT_START_ROW as u16;
    harness.mouse_click(0, header_row).unwrap();

    // After collapsing: the collapsed indicator (▸) should appear, body
    // lines should be hidden, and the line after the fold should be visible.
    harness.assert_screen_contains("▸");
    harness.assert_screen_not_contains("body 0");
    harness.assert_screen_contains("after");

    // Expand the fold again.
    harness.mouse_click(0, header_row).unwrap();

    // After expanding: body lines should be visible again.
    harness.assert_screen_contains("body 0");
    harness.assert_screen_contains("▾");
}

/// In large-file mode (byte offset gutter), the cursor line highlight in the
/// gutter must match the actual cursor line's byte offset — not an estimated
/// line number.
///
/// This verifies that when the cursor is near the end of a file with short
/// lines (where `get_line_number` estimates badly due to `byte / est_ll`),
/// the gutter highlights the row containing the cursor content.
#[test]
fn test_gutter_highlight_correct_at_end_of_large_file() {
    // Build a file with very short lines so est_ll (80) >> actual (~5).
    // This maximises the estimation error in get_line_number.
    let mut content = String::new();
    for i in 0..80 {
        content.push_str(&format!("L{i:03}\n")); // "L000\n" .. "L079\n" = 5 bytes each
    }
    // Total: 80 * 5 = 400 bytes. Line 70 starts at byte 350.
    let target_line = 70;
    let target_byte = target_line * 5; // 350
    let target_text = format!("L{target_line:03}");

    // Force large-file mode
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("gutter_highlight.txt", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();

    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "File should be in large-file mode"
    );

    // Render twice to load chunks
    harness.render().unwrap();
    harness.render().unwrap();

    // Position cursor on target line and scroll viewport there
    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = target_byte;
    cursors.primary_mut().anchor = None;
    let viewport = harness.editor_mut().active_viewport_mut();
    viewport.top_byte = target_byte;
    viewport.top_view_line_offset = 0;

    harness.render().unwrap();

    // The target line content should be visible on screen.
    harness.assert_screen_contains(&target_text);

    // Find the row showing the target text.
    let (start_row, end_row) = harness.content_area_rows();
    let mut target_row = None;
    for row in start_row..=end_row {
        let text = harness.get_row_text(row as u16);
        if text.contains(&target_text) {
            target_row = Some(row as u16);
            break;
        }
    }
    let target_row = target_row.expect("Target text should be on screen");

    // The gutter should show the byte offset (350) on the target row,
    // and it should be highlighted (brighter style).
    let row_text = harness.get_row_text(target_row);
    assert!(
        row_text.contains(&format!("{target_byte}")),
        "Gutter should show byte offset {target_byte}, got row: '{row_text}'"
    );

    // Verify the gutter for this row uses the highlighted (cursor line) style.
    // In byte_offset_mode, cursor line gets theme.editor_fg; non-cursor gets
    // theme.line_number_fg. We check that the gutter cell style differs from
    // the row above or below.
    let above_row = if target_row > start_row as u16 {
        target_row - 1
    } else {
        target_row + 1
    };

    // Column 1 is inside the gutter line number area (column 0 is fold indicator)
    let target_gutter_style = harness.get_cell_style(1, target_row);
    let other_gutter_style = harness.get_cell_style(1, above_row);
    assert_ne!(
        target_gutter_style, other_gutter_style,
        "Cursor line gutter should have different (highlighted) style from non-cursor line"
    );
}

/// In large-file mode, fold indicators (▾) must appear on the correct lines
/// adjacent to fold header content, not offset due to estimation errors.
///
/// This tests indent-based fold detection: the fold indicator should appear
/// on the same row as the fold header text, even when the fold header is
/// near the end of the file where byte-to-line estimation diverges.
///
/// Layout: 10 short filler lines + fold structure, so everything fits on screen
/// but the fold header is at a byte offset where est_ll=80 estimation diverges.
#[test]
fn test_fold_indicators_correct_at_end_of_large_file() {
    // Build content with short filler lines followed by an indent-foldable block.
    // Short lines (5 bytes each) cause est_ll=80 >> actual line length.
    let mut content = String::new();
    for i in 0..10 {
        content.push_str(&format!("X{i:03}\n")); // 5 bytes each
    }
    // Fold structure starting at byte 50
    content.push_str("fn fold_me() {\n"); // fold header
    for i in 0..5 {
        content.push_str(&format!("    body_{i}\n")); // indented body
    }
    content.push_str("}\n"); // closing
    content.push_str("after_fold\n"); // line after fold

    // Force large-file mode
    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("fold_indicator_end.rs", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();

    assert!(
        harness.editor().active_state().buffer.is_large_file(),
        "File should be in large-file mode"
    );

    // Render twice: first loads chunks, second computes fold indicators.
    harness.render().unwrap();
    harness.render().unwrap();

    // The fold header and body should be visible (content fits on screen)
    harness.assert_screen_contains("fn fold_me()");
    harness.assert_screen_contains("body_0");
    harness.assert_screen_contains("after_fold");

    // Find the row with the fold header
    let (start_row, end_row) = harness.content_area_rows();
    let mut header_row = None;
    for row in start_row..=end_row {
        let text = harness.get_row_text(row as u16);
        if text.contains("fn fold_me()") {
            header_row = Some(row as u16);
            break;
        }
    }
    let header_row = header_row.expect("Fold header should be on screen");

    // The fold indicator (▾) should appear on the same row as the fold header.
    let header_row_text = harness.get_row_text(header_row);
    assert!(
        header_row_text.contains('▾'),
        "Fold indicator ▾ should appear on the fold header row.\n\
         Header row {header_row}: '{header_row_text}'\n\
         Screen:\n{}",
        harness.screen_to_string()
    );

    // The indicator should NOT appear on body lines
    for row in (header_row + 1)..=(end_row as u16) {
        let text = harness.get_row_text(row);
        if text.contains("body_") {
            assert!(
                !text.contains('▾'),
                "Fold indicator should not appear on body line row {row}: '{text}'"
            );
        }
    }

    // --- Fold/unfold round-trip via gutter click ---

    // Click the gutter on the fold header row to collapse.
    harness.mouse_click(0, header_row).unwrap();

    // Body lines should be hidden, header and after_fold should remain.
    harness.assert_screen_not_contains("body_0");
    harness.assert_screen_not_contains("body_4");
    harness.assert_screen_contains("fn fold_me()");
    harness.assert_screen_contains("after_fold");

    // Collapsed indicator (▸) should appear on the header row.
    let header_row_text = harness.get_row_text(header_row);
    assert!(
        header_row_text.contains('▸'),
        "Collapsed indicator ▸ should appear after folding.\n\
         Header row {header_row}: '{header_row_text}'"
    );

    // Click again to expand.
    harness.mouse_click(0, header_row).unwrap();

    // Body lines should be visible again.
    harness.assert_screen_contains("body_0");
    harness.assert_screen_contains("body_4");
    harness.assert_screen_contains("after_fold");

    // Expanded indicator (▾) should be back.
    let header_row_text = harness.get_row_text(header_row);
    assert!(
        header_row_text.contains('▾'),
        "Expanded indicator ▾ should appear after unfolding.\n\
         Header row {header_row}: '{header_row_text}'"
    );
}

/// Fold/unfold at the END of a large file via toggle_fold_at_cursor.
///
/// Many short filler lines push foldable blocks past the point where
/// est_ll=80 estimation diverges badly from real line positions.
/// The test scrolls to the end via Ctrl+End, folds each block, and verifies
/// exactly which lines are hidden and which remain visible.
#[test]
fn test_fold_unfold_at_end_of_large_file_cursor() {
    // 200 short filler lines (5 bytes each = 1000 bytes total).
    // est_ll=80 thinks byte 1000 ≈ line 12, but it's actually line 200.
    let mut content = String::new();
    for i in 0..200 {
        content.push_str(&format!("L{i:03}\n"));
    }

    // Two foldable blocks at the end.
    let block_a_header = content.len();
    content.push_str("fn block_a() {\n");
    content.push_str("    a_body_1\n");
    content.push_str("    a_body_2\n");
    content.push_str("    a_body_3\n");
    content.push_str("}\n");

    let block_b_header = content.len();
    content.push_str("fn block_b() {\n");
    content.push_str("    b_body_1\n");
    content.push_str("    b_body_2\n");
    content.push_str("    b_body_3\n");
    content.push_str("}\n");
    content.push_str("THE_END\n");

    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("fold_end_cursor.rs", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();
    assert!(harness.editor().active_state().buffer.is_large_file());

    // Render to load initial chunks.
    harness.render().unwrap();
    harness.render().unwrap();

    // Scroll to end of file via Ctrl+End.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();

    // Precondition: both blocks and THE_END should be visible.
    harness.assert_screen_contains("fn block_a()");
    harness.assert_screen_contains("a_body_1");
    harness.assert_screen_contains("fn block_b()");
    harness.assert_screen_contains("b_body_1");
    harness.assert_screen_contains("THE_END");

    // --- Fold block_a via cursor ---
    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = block_a_header;
    cursors.primary_mut().anchor = None;
    harness.render().unwrap();
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();

    // block_a body hidden, header shows placeholder, block_b untouched.
    harness.assert_screen_not_contains("a_body_1");
    harness.assert_screen_not_contains("a_body_2");
    harness.assert_screen_not_contains("a_body_3");
    harness.assert_screen_contains("fn block_a() { ...");
    harness.assert_screen_contains("fn block_b()");
    harness.assert_screen_contains("b_body_1");
    harness.assert_screen_contains("b_body_3");
    harness.assert_screen_contains("THE_END");

    // --- Unfold block_a ---
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();
    harness.assert_screen_contains("a_body_1");
    harness.assert_screen_contains("a_body_3");

    // --- Fold block_b via cursor ---
    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = block_b_header;
    cursors.primary_mut().anchor = None;
    harness.render().unwrap();
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();

    // block_b body hidden, block_a untouched.
    harness.assert_screen_not_contains("b_body_1");
    harness.assert_screen_not_contains("b_body_2");
    harness.assert_screen_not_contains("b_body_3");
    harness.assert_screen_contains("fn block_b() { ...");
    harness.assert_screen_contains("a_body_1");
    harness.assert_screen_contains("a_body_3");
    harness.assert_screen_contains("THE_END");

    // --- Unfold block_b ---
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();
    harness.assert_screen_contains("b_body_1");
    harness.assert_screen_contains("b_body_3");
}

/// Fold + unfold round-trip at the end of a large file.
///
/// Reproduces the real scenario: Rust code with multi-line function
/// signatures. The `) {` line is foldable (its body is indented).
/// Cursor toggle and re-toggle must correctly hide/show the body.
#[test]
fn test_fold_unfold_function_body_at_end_of_large_file() {
    // ~4MB of short filler lines to match real-world large file sizes.
    // Each line is 9 bytes ("L0000000\n"), ~450k lines ≈ 4MB.
    // est_ll=80 thinks 4MB ≈ 50k lines, but it's actually ~450k — a 9×
    // error that causes get_line_number() estimates to be wildly wrong.
    let mut content = String::with_capacity(4_200_000);
    for i in 0..450_000 {
        content.push_str(&format!("L{i:07}\n")); // 9 bytes each
    }

    // Rust-like code with multi-line fn signature + body.
    content.push_str("/// doc comment\n");
    content.push_str("#[test]\n");
    content.push_str("fn my_test(\n");
    content.push_str("    arg1: usize,\n");
    content.push_str("    arg2: usize\n");
    let body_header_byte = content.len();
    content.push_str(") {\n");
    content.push_str("    let x = arg1 + arg2;\n");
    content.push_str("    assert!(x > 0);\n");
    content.push_str("}\n");
    content.push_str("AFTER\n");

    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("fold_body_end.rs", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();
    assert!(harness.editor().active_state().buffer.is_large_file());

    // Jump to end of file.
    harness.render().unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();

    // Everything should be visible after Ctrl+End scrolls to the end.
    harness.assert_screen_contains("fn my_test(");
    harness.assert_screen_contains(") {");
    harness.assert_screen_contains("let x = arg1 + arg2;");
    harness.assert_screen_contains("assert!(x > 0);");
    harness.assert_screen_contains("AFTER");

    // Position cursor on the `) {` line (fold header for the body).
    let cursors = harness.editor_mut().active_cursors_mut();
    cursors.primary_mut().position = body_header_byte;
    cursors.primary_mut().anchor = None;
    harness.render().unwrap();

    // Toggle fold at cursor — should hide the function body.
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();

    // The body should be hidden; header, args, and AFTER must remain.
    harness.assert_screen_contains("fn my_test(");
    harness.assert_screen_contains(") {");
    harness.assert_screen_not_contains("let x = arg1 + arg2;");
    harness.assert_screen_not_contains("assert!(x > 0);");
    harness.assert_screen_contains("AFTER");

    // The collapsed indicator (▸) should appear on the `) {` row.
    let (start_row, end_row) = harness.content_area_rows();
    let body_row = (start_row..=end_row)
        .map(|r| r as u16)
        .find(|&r| harness.get_row_text(r).contains(") {"))
        .expect(") { should be on screen");
    let body_row_text = harness.get_row_text(body_row);
    assert!(
        body_row_text.contains('▸'),
        "Collapsed indicator ▸ should appear on `) {{` row after folding.\n\
         Row {body_row}: '{body_row_text}'\n\
         Screen:\n{}",
        harness.screen_to_string()
    );

    // Toggle fold again to unfold.
    harness.editor_mut().toggle_fold_at_cursor();
    harness.render().unwrap();

    // Everything should be visible again.
    harness.assert_screen_contains("let x = arg1 + arg2;");
    harness.assert_screen_contains("assert!(x > 0);");
    harness.assert_screen_contains("AFTER");

    // The expanded indicator (▾) should be back on `) {`.
    let body_row_text = harness.get_row_text(body_row);
    assert!(
        body_row_text.contains('▾'),
        "Expanded indicator ▾ should appear on `) {{` row after unfolding.\n\
         Row {body_row}: '{body_row_text}'\n\
         Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Fold/unfold at the END of a large file via gutter click.
///
/// Same layout as the cursor test above, but triggers folds by clicking
/// the gutter indicator. This exercises the click → byte-offset → fold path.
#[test]
fn test_fold_unfold_at_end_of_large_file_gutter_click() {
    let mut content = String::new();
    for i in 0..200 {
        content.push_str(&format!("L{i:03}\n"));
    }

    content.push_str("fn block_a() {\n");
    content.push_str("    a_body_1\n");
    content.push_str("    a_body_2\n");
    content.push_str("    a_body_3\n");
    content.push_str("}\n");
    content.push_str("fn block_b() {\n");
    content.push_str("    b_body_1\n");
    content.push_str("    b_body_2\n");
    content.push_str("    b_body_3\n");
    content.push_str("}\n");
    content.push_str("THE_END\n");

    let mut config = fresh::config::Config::default();
    config.editor.large_file_threshold_bytes = 3;

    let fixture = TestFixture::new("fold_end_click.rs", &content).unwrap();
    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.open_file(&fixture.path).unwrap();
    assert!(harness.editor().active_state().buffer.is_large_file());

    // Render + Ctrl+End to scroll to end of file.
    harness.render().unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("fn block_a()");
    harness.assert_screen_contains("fn block_b()");
    harness.assert_screen_contains("THE_END");

    // Find the row showing block_a header.
    let (start_row, end_row) = harness.content_area_rows();
    let block_a_row = (start_row..=end_row)
        .map(|r| r as u16)
        .find(|&r| harness.get_row_text(r).contains("fn block_a()"))
        .expect("block_a header should be on screen");

    // --- Click gutter to fold block_a ---
    harness.mouse_click(0, block_a_row).unwrap();

    harness.assert_screen_not_contains("a_body_1");
    harness.assert_screen_not_contains("a_body_3");
    harness.assert_screen_contains("fn block_a() { ...");
    // block_b must be completely untouched.
    harness.assert_screen_contains("fn block_b()");
    harness.assert_screen_contains("b_body_1");
    harness.assert_screen_contains("b_body_3");
    harness.assert_screen_contains("THE_END");

    // --- Click gutter to unfold block_a ---
    harness.mouse_click(0, block_a_row).unwrap();

    harness.assert_screen_contains("a_body_1");
    harness.assert_screen_contains("a_body_3");
    harness.assert_screen_contains("b_body_1");

    // Re-find block_b row after unfold (positions may shift).
    let block_b_row = (start_row..=end_row)
        .map(|r| r as u16)
        .find(|&r| harness.get_row_text(r).contains("fn block_b()"))
        .expect("block_b header should be on screen after unfold");

    // --- Click gutter to fold block_b ---
    harness.mouse_click(0, block_b_row).unwrap();

    harness.assert_screen_not_contains("b_body_1");
    harness.assert_screen_not_contains("b_body_3");
    harness.assert_screen_contains("fn block_b() { ...");
    // block_a must be completely untouched.
    harness.assert_screen_contains("a_body_1");
    harness.assert_screen_contains("a_body_3");
    harness.assert_screen_contains("THE_END");

    // --- Click gutter to unfold block_b ---
    harness.mouse_click(0, block_b_row).unwrap();

    harness.assert_screen_contains("b_body_1");
    harness.assert_screen_contains("b_body_3");
}
