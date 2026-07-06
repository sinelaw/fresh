//! E2E tests for virtual space (`editor.virtual_space`): the cursor may sit
//! past the end of a line. See docs/internal/virtual-space-scoping.md.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, VirtualSpaceMode};

fn harness_with_mode(mode: VirtualSpaceMode) -> EditorTestHarness {
    let mut config = Config::default();
    config.editor.virtual_space = mode;
    EditorTestHarness::with_config(80, 24, config).unwrap()
}

/// With virtual space on, moving down onto a shorter line keeps the cursor
/// at its on-screen column instead of snapping to the line end.
#[test]
fn test_arrow_down_renders_cursor_past_eol() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("abcdef")
        .expect("first line visible");

    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!(
        (cx, cy),
        (x0 + 4, y0 + 1),
        "cursor floats at column 4, two columns past 'ab'"
    );
}

/// With virtual space off (the default), the same movement snaps the cursor
/// to the short line's end.
#[test]
fn test_arrow_down_snaps_to_eol_when_off() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Off);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("abcdef")
        .expect("first line visible");

    for _ in 0..4 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!((cx, cy), (x0 + 2, y0 + 1), "cursor clamps to end of 'ab'");
}

/// With virtual space on, ArrowRight at end of line walks the cursor into
/// the empty space instead of wrapping to the next line.
#[test]
fn test_arrow_right_renders_cursor_past_eol() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    let (x1, y1) = harness
        .find_text_on_screen("xyz")
        .expect("second line visible");
    let (x0, y0) = (x1, y1 - 1); // "ab" starts at the same column, one row up

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!(
        (cx, cy),
        (x0 + 5, y0),
        "cursor sits three columns past 'ab'"
    );

    // Left walks back through the virtual columns before bytes move.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!((cx, cy), (x0 + 4, y0));
}

/// Typing in virtual space materializes the gap with spaces, as a single
/// undo step.
#[test]
fn test_typing_pads_with_spaces() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("ab   X\nxyz");

    // One undo removes both the padding and the typed character.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("ab\nxyz");
}

/// Typing after floating down onto a shorter line pads that line to the
/// cursor's column — the marquee virtual-space workflow.
#[test]
fn test_typing_after_vertical_move_pads() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.type_text("!").unwrap();
    harness.assert_buffer_content("abcdef\nab   !\nabcdef");
}

/// Backspace in virtual space steps the cursor left without deleting; only
/// at the real content end does it start deleting characters.
#[test]
fn test_backspace_steps_back_through_virtual_space() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    let (x1, y1) = harness
        .find_text_on_screen("xyz")
        .expect("second line visible");
    let (x0, y0) = (x1, y1 - 1);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("ab\nxyz");
    harness.render().unwrap();
    assert_eq!(harness.screen_cursor_position(), (x0 + 3, y0));

    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("ab\nxyz");
    harness.render().unwrap();
    assert_eq!(harness.screen_cursor_position(), (x0 + 2, y0));

    // Back at the real content end: Backspace deletes again.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("a\nxyz");
}

/// Enter in virtual space inserts a plain newline at the real content end —
/// no trailing padding is materialized.
#[test]
fn test_enter_in_virtual_space_adds_no_padding() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("ab\n\nxyz");
}

/// Pasting at a virtual position materializes the gap first.
#[test]
fn test_paste_pads_with_spaces() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_paste("PP").unwrap();
    harness.assert_buffer_content("ab  PP\nxyz");
}

/// Tab at a virtual position materializes the gap, then indents.
#[test]
fn test_tab_pads_with_spaces() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    // 3 columns of padding + one 4-space indent unit.
    harness.assert_buffer_content("ab       \nxyz");
}

/// Clicking past the end of a line places the cursor at the clicked column;
/// typing there pads the gap.
#[test]
fn test_click_past_eol_places_virtual_cursor() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    let (x1, y1) = harness
        .find_text_on_screen("xyz")
        .expect("second line visible");
    let (x0, y0) = (x1, y1 - 1);

    // Click 3 columns past the end of "ab".
    harness.mouse_click(x0 + 5, y0).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.screen_cursor_position(),
        (x0 + 5, y0),
        "cursor lands at the clicked column"
    );

    harness.type_text("X").unwrap();
    harness.assert_buffer_content("ab   X\nxyz");
}

/// With virtual space off, the same click snaps to the line end.
#[test]
fn test_click_past_eol_snaps_when_off() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Off);
    harness.load_buffer_from_text("ab\nxyz").unwrap();

    let (x1, y1) = harness
        .find_text_on_screen("xyz")
        .expect("second line visible");
    let (x0, y0) = (x1, y1 - 1);

    harness.mouse_click(x0 + 5, y0).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("abX\nxyz");
}

/// With virtual_space = "block", a block selection extends past short lines
/// and block copy yields a true (space-padded) rectangle.
#[test]
fn test_block_copy_is_rectangular_past_short_line() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Block);
    harness.load_buffer_from_text("aaaa\nbb\ncccc").unwrap();

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    assert_eq!(
        clipboard, "a\n \nc",
        "rectangle column 3 is padded on the short middle line"
    );
}

/// Without virtual space, the same copy stays ragged (no padding).
#[test]
fn test_block_copy_stays_ragged_when_off() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Off);
    harness.load_buffer_from_text("aaaa\nbb\ncccc").unwrap();

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();

    let clipboard = harness.editor_mut().clipboard_content_for_test();
    // Without virtual space, the block column collapses to the short middle
    // line's width as the selection passes through it (pre-existing
    // behavior), so the copy comes out empty rather than rectangular.
    assert_eq!(clipboard, "\n\n", "columns collapse without virtual space");
}

/// Typing with a block selection whose left edge is past a short line pads
/// the short line to the block column, so the typed character lands in a
/// straight column — the marquee block-editing workflow.
#[test]
fn test_block_insert_pads_short_lines() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Block);
    harness.load_buffer_from_text("aaaa\nbb\ncccc").unwrap();

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("aaaaX\nbb  X\nccccX");
}

/// The block rectangle paints its full extent on screen, including the part
/// floating past a short line's end.
#[test]
fn test_block_rectangle_renders_past_short_line() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Block);
    harness.load_buffer_from_text("aaaa\nbb\ncccc").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("aaaa")
        .expect("first line visible");

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Down, KeyModifiers::ALT | KeyModifiers::SHIFT)
            .unwrap();
    }
    harness
        .send_key(KeyCode::Right, KeyModifiers::ALT | KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    let selected_bg = harness
        .get_cell_style(x0 + 3, y0)
        .expect("selected cell on first line")
        .bg;
    let virtual_bg = harness
        .get_cell_style(x0 + 3, y0 + 1)
        .expect("virtual rect cell on short line")
        .bg;
    let outside_bg = harness
        .get_cell_style(x0 + 6, y0 + 1)
        .expect("cell outside the rect")
        .bg;
    assert_eq!(
        virtual_bg, selected_bg,
        "rect cell past 'bb' paints with the selection background"
    );
    assert_ne!(
        virtual_bg, outside_bg,
        "cells right of the rect stay unselected"
    );
}

/// Clicking below the last line parks the cursor on a virtual line at the
/// clicked column; typing there materializes the missing newlines plus the
/// column padding, as one undo step.
#[test]
fn test_click_below_last_line_places_virtual_cursor() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab").unwrap();

    let (x0, y0) = harness.find_text_on_screen("ab").expect("line visible");

    // Two rows below the (only) line, three columns in.
    harness.mouse_click(x0 + 5, y0 + 2).unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.screen_cursor_position(),
        (x0 + 5, y0 + 2),
        "cursor floats on the clicked virtual line"
    );

    // Left/Right move along the virtual line.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert_eq!(harness.screen_cursor_position(), (x0 + 4, y0 + 2));

    // Backspace steps left without touching the buffer.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.assert_buffer_content("ab");
    harness.render().unwrap();
    assert_eq!(harness.screen_cursor_position(), (x0 + 3, y0 + 2));

    // Typing materializes two newlines + three columns of padding.
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("ab\n\n   X");

    // One undo removes the whole materialization.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.assert_buffer_content("ab");
}

/// With a newline-terminated buffer, the implicit trailing line is the last
/// display line; clicking below it creates lines after it.
#[test]
fn test_click_below_trailing_newline_extends() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("ab\n").unwrap();

    let (x0, y0) = harness.find_text_on_screen("ab").expect("line visible");

    // Rows: "ab" at y0, the implicit empty line at y0+1; click one below it.
    harness.mouse_click(x0 + 3, y0 + 2).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("ab\n\n   X");
}

/// With virtual space off, clicking below the last line snaps to the end of
/// the buffer as before.
#[test]
fn test_click_below_last_line_snaps_when_off() {
    let mut harness = harness_with_mode(VirtualSpaceMode::Off);
    harness.load_buffer_from_text("ab").unwrap();

    let (x0, y0) = harness.find_text_on_screen("ab").expect("line visible");
    harness.mouse_click(x0 + 5, y0 + 2).unwrap();
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("abX");
}

/// Run a command-palette entry by fuzzy-typing its full name and pressing
/// Enter.
fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// "Toggle Virtual Space (Current Buffer)" cycles off → block → on for the
/// active buffer only; other buffers keep following the global default.
#[test]
fn test_toggle_virtual_space_scopes_to_buffer() {
    let mut harness = EditorTestHarness::with_temp_project(80, 24).unwrap();
    let dir = harness.project_dir().unwrap().to_path_buf();
    std::fs::write(dir.join("a.txt"), "ab\nxyz").unwrap();
    std::fs::write(dir.join("b.txt"), "cd\nuvw").unwrap();

    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.open_file(&dir.join("b.txt")).unwrap();
    harness.render().unwrap();

    // Cycle b.txt (active): off → block → on.
    run_command(&mut harness, "Toggle Virtual Space (Current Buffer)");
    run_command(&mut harness, "Toggle Virtual Space (Current Buffer)");

    // Typing past EOL pads in b.txt...
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("X").unwrap();
    harness.assert_buffer_content("cd   X\nuvw");

    // ...but a.txt still follows the global default (off): Right at EOL
    // wraps to the next line, so typing lands at the start of line 2.
    harness.open_file(&dir.join("a.txt")).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("Y").unwrap();
    harness.assert_buffer_content("ab\nYxyz");
}

/// The per-buffer virtual-space override survives a workspace save/restore.
#[test]
fn test_toggle_virtual_space_persists_across_restart() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let file = project_dir.join("a.txt");
    std::fs::write(&file, "ab\nxyz").unwrap();

    // Session 1: cycle the buffer to "on", then save the workspace.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        harness.open_file(&file).unwrap();
        harness.render().unwrap();
        run_command(&mut harness, "Toggle Virtual Space (Current Buffer)");
        run_command(&mut harness, "Toggle Virtual Space (Current Buffer)");
        harness.editor_mut().save_workspace().unwrap();
    }

    // Session 2: restore — typing past EOL must still pad this buffer.
    {
        let mut harness = EditorTestHarness::with_config_and_working_dir(
            80,
            24,
            Config::default(),
            project_dir.clone(),
        )
        .unwrap();
        let restored = harness.editor_mut().try_restore_workspace().unwrap();
        assert!(restored, "workspace should restore");
        harness.render().unwrap();
        harness.open_file(&file).unwrap();

        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        for _ in 0..3 {
            harness
                .send_key(KeyCode::Right, KeyModifiers::NONE)
                .unwrap();
        }
        harness.type_text("X").unwrap();
        harness.assert_buffer_content("ab   X\nxyz");
    }
}

/// Vertical movement through a short line and back onto a long one restores
/// the original column (the goal column survives the virtual segment).
#[test]
fn test_column_survives_through_short_line() {
    let mut harness = harness_with_mode(VirtualSpaceMode::On);
    harness.load_buffer_from_text("abcdef\nab\nabcdef").unwrap();

    let (x0, y0) = harness
        .find_text_on_screen("abcdef")
        .expect("first line visible");

    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cx, cy) = harness.screen_cursor_position();
    assert_eq!((cx, cy), (x0 + 5, y0 + 2), "column 5 restored on line 3");
}
