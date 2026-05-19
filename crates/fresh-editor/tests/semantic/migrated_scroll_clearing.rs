//! Migration of `tests/e2e/scroll_clearing.rs` — view-clearing
//! invariants around overscroll, mouse-wheel scrolling on top of
//! keyboard navigation, and cursor/glyph rendering on tab-prefixed
//! lines.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **No leftover content at end-of-file.** After Ctrl+End,
//!      followed by PageDown / Down past the last line, the
//!      viewport must show the file's tail (`lfs_mirror_extend`)
//!      and must NOT show content from near the top of the file
//!      (`pool_to_id_cbdata` near line 7). Pre-fix, the renderer
//!      would leave characters from earlier scroll positions in the
//!      cells the new draw didn't touch.
//!
//!   2. **Mouse-wheel scrolling reaches the buffer tail.** Scrolling
//!      down with `MouseEventKind::ScrollDown` events at the
//!      content area must scroll far enough to put the file's tail
//!      (`lfs_mirror_extend`) on screen and keep the rendered rows
//!      legible (no leftover characters).
//!
//!   3. **Mouse wheel keeps working after keyboard navigation
//!      (#248).** A sequence of `Down` keypresses followed by mouse
//!      `ScrollDown` events must actually advance the viewport's
//!      top line — the original bug had the mouse path silently
//!      ignored once a keyboard navigation event had run.
//!
//!   4. **No leftover characters past the last content row.** After
//!      Ctrl+End + Down past the end of file, rows below the last
//!      visible buffer line (within the content area) must contain
//!      only whitespace / gutter / scrollbar glyphs — no leftover
//!      printable characters from earlier renders.
//!
//!   5. **Tab indicator renders at every expanded tab position.**
//!      On a line that starts with multiple tabs (line 3 of the
//!      fixture: `\t\t\t__u64 migration_flags`), the rendered row
//!      must contain the tab indicator glyph (`→`) at least three
//!      times — once per tab.
//!
//!   6. **Cursor at column 0 on a tab-prefixed line lands before
//!      the tab expansion, not inside it.** After Home,Home (smart
//!      home → true column 0) on the tabbed line, the hardware
//!      cursor's x position must be at or just past the gutter
//!      width — NOT 7 columns further (which would mean the
//!      cursor was placed inside the first expanded tab).
//!
//! ## Harness-direct pattern
//!
//! Most assertions here probe surfaces that have no `EditorTestApi`
//! projection: `top_line_number`, `content_area_rows`,
//! `screen_cursor_position`, the buffer's per-row scrollbar / cell
//! probes, and direct access to `editor().active_state().margins`
//! for the gutter width. The migrated tests therefore take the
//! harness-direct path (the same pattern
//! `migrated_horizontal_scrollbar.rs` and `migrated_line_wrap_parity.rs`
//! use).
//!
//! ## Deferred
//!
//! Two e2e tests are intentionally NOT migrated and remain in
//! `tests/e2e/scroll_clearing.rs`:
//!
//!   * `test_scroll_clearing_render_buffer_analysis` — exploratory
//!     `println!`-only diagnostic test; no asserts / panics, so the
//!     anti-test discipline ("must fail when the load-bearing
//!     action is dropped") has nothing to bind to. Deferred until a
//!     concrete invariant is extracted.
//!   * `test_scroll_clearing_real_terminal` — pairs `TestBackend`
//!     against `vt100` and reports differences via `println!` only;
//!     no panic on mismatch. Same rationale.
//!
//! Source: `tests/e2e/scroll_clearing.rs` (6 of 8 tests migrated;
//! 2 deferred — file retained).

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers, MouseEvent, MouseEventKind};
use std::path::PathBuf;

/// Path to the shared scroll fixture (47-line C source with tab
/// indentation on line 3 and a recognisable tail marker
/// `lfs_mirror_extend`).
fn scroll_test_file_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/scroll_test_file.txt")
}

#[test]
fn migrated_scroll_clearing_at_bottom_of_file_keeps_tail_and_clears_head() {
    // Original: `test_scroll_clearing_at_bottom_of_file`.
    // Mirrors the e2e fixture (scroll_test_file.txt), terminal
    // size (80x24), and action sequence (Ctrl+End → 5x PageDown →
    // 10x Down). The load-bearing assertions are:
    //   (a) the file's tail (`lfs_mirror_extend`) is visible, and
    //   (b) content from near the top (`pool_to_id_cbdata`,
    //       lines 7-10) is NOT visible once the viewport is
    //       scrolled past line 10.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    // Sanity: file loaded.
    assert!(
        harness.screen_to_string().contains("static int lfs_migrate_to_dom"),
        "fixture must load and render its first content line"
    );

    // Jump to end of file.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Overscroll attempts: PageDown 5x then Down 10x. These should
    // be no-ops at end of file, but the bug manifested as leftover
    // characters from previous render passes.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let final_screen = harness.screen_to_string();
    let top_line = harness.top_line_number();

    // (a) tail visible
    assert!(
        final_screen.contains("lfs_mirror_extend"),
        "after Ctrl+End + overscroll, the file's tail \
         (`lfs_mirror_extend`) must remain visible on screen.\n\
         top_line={top_line}\n--- screen ---\n{final_screen}"
    );

    // (b) head not visible if viewport scrolled past line 10
    if top_line > 10 {
        assert!(
            !final_screen.contains("pool_to_id_cbdata"),
            "leftover-content bug: viewport top_line={top_line} is well \
             past line 10, but `pool_to_id_cbdata` (defined on lines 7-10 of \
             the fixture) is still visible — indicates improper screen \
             clearing during scroll.\n--- screen ---\n{final_screen}"
        );
    }
}

#[test]
fn migrated_scroll_clearing_with_scroll_wheel_reaches_tail() {
    // Original: `test_scroll_clearing_with_scroll_wheel`. Same
    // fixture and terminal size; mouse-wheel ScrollDown events
    // routed into the content area must scroll far enough to bring
    // the tail marker on screen and leave no rendering artifacts.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    let (content_first_row, _content_last_row) = harness.content_area_rows();

    // Scroll down 20 times — should be more than enough to reach
    // end of the 47-line fixture.
    for _ in 0..20 {
        let scroll_event = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: (content_first_row + 5) as u16,
            modifiers: KeyModifiers::empty(),
        };
        harness.send_mouse(scroll_event).unwrap();
        harness.render().unwrap();
    }

    // Continue scrolling past the end to exercise overscroll.
    for _ in 0..10 {
        let scroll_event = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: (content_first_row + 5) as u16,
            modifiers: KeyModifiers::empty(),
        };
        harness.send_mouse(scroll_event).unwrap();
        harness.render().unwrap();

        // Cheap artifact probe: no null characters in any rendered
        // row (raw buffer corruption signal).
        let screen = harness.screen_to_string();
        assert!(
            !screen.contains('\0'),
            "render artifact: null character found in screen after mouse \
             scroll overscroll.\n--- screen ---\n{screen}"
        );
    }

    let final_screen = harness.screen_to_string();
    assert!(
        final_screen.contains("lfs_mirror_extend"),
        "mouse-wheel scrolling 20+ events must reach the file's tail \
         (`lfs_mirror_extend`).\n--- screen ---\n{final_screen}"
    );
}

#[test]
fn migrated_mouse_wheel_after_keyboard_navigation_still_scrolls() {
    // Original: `test_mouse_wheel_after_keyboard_navigation` (Bug
    // #248). Keyboard navigation followed by mouse-wheel scroll
    // events: the mouse path must still advance the viewport's
    // top_line. Pre-fix the mouse handler dropped events after a
    // keyboard event had been processed.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    // Step 1: keyboard navigation (10 Down).
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let top_line_after_keyboard = harness.top_line_number();

    // Step 2: 5 mouse ScrollDown events in the content area.
    let (content_first_row, _) = harness.content_area_rows();
    for _ in 0..5 {
        let scroll_event = MouseEvent {
            kind: MouseEventKind::ScrollDown,
            column: 40,
            row: (content_first_row + 5) as u16,
            modifiers: KeyModifiers::empty(),
        };
        harness.send_mouse(scroll_event).unwrap();
        harness.render().unwrap();
    }
    let top_line_after_mouse = harness.top_line_number();

    let actual_scroll = top_line_after_mouse.saturating_sub(top_line_after_keyboard);
    assert!(
        actual_scroll > 0,
        "Bug #248: after keyboard navigation, mouse-wheel ScrollDown events \
         must still advance the viewport. top_line stayed at \
         {top_line_after_keyboard} after 5 ScrollDown events."
    );
}

#[test]
fn migrated_leftover_characters_after_last_line() {
    // Original: `test_leftover_characters_after_last_line`. After
    // Ctrl+End + 10 Down, any row within the content area that
    // appears AFTER the last visible buffer line must contain only
    // whitespace / gutter (`│`) / scrollbar (`█`) glyphs — any
    // other printable character is a leftover from a previous
    // render.
    let terminal_width: u16 = 80;
    let terminal_height: u16 = 24;
    let mut harness = EditorTestHarness::new(terminal_width, terminal_height).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    // Jump to end + scroll past end with Down.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    let (content_first_row, content_last_row) = harness.content_area_rows();
    let screen = harness.screen_to_string();
    let screen_lines: Vec<&str> = screen.lines().collect();

    // Find the last content row (one that has a line-number
    // gutter, i.e. contains `│` not at line start).
    let mut last_content_row = content_first_row;
    for row_idx in content_first_row..=content_last_row {
        if let Some(line) = screen_lines.get(row_idx) {
            if line.contains('│') && !line.trim_start().starts_with('│') {
                last_content_row = row_idx;
            }
        }
    }

    // Probe rows below last_content_row + 1 (skipping the immediate
    // cursor-empty row) for leftover printable characters.
    let mut leftover_issues = Vec::new();
    for row_idx in (last_content_row + 2)..=content_last_row {
        let Some(line) = screen_lines.get(row_idx) else {
            continue;
        };
        for (col, ch) in line.chars().enumerate() {
            if col >= (terminal_width - 1) as usize {
                continue; // skip scrollbar column
            }
            if !ch.is_whitespace() && ch != '│' && ch != '█' {
                leftover_issues.push(format!(
                    "row {row_idx}, col {col}: found {ch:?} (expected whitespace / gutter / scrollbar)"
                ));
            }
        }
    }

    assert!(
        leftover_issues.is_empty(),
        "leftover-character bug: rows below the last content line ({last_content_row}) \
         within the content area should be empty, but found {} issue(s):\n  {}\n\
         --- screen ---\n{screen}",
        leftover_issues.len(),
        leftover_issues.join("\n  ")
    );
}

#[test]
fn migrated_tab_indicator_visible_on_tab_prefixed_line() {
    // Original: `test_tab_cursor_positioning_and_rendering` —
    // mirrors the load-bearing tab-indicator claim (the cursor-
    // movement exploration in the e2e is println-only with no
    // assertions, so it isn't preserved). Line 3 of the fixture
    // starts with three tabs followed by `__u64 migration_flags`;
    // the rendered row must contain at least three `→` glyphs.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    // Move cursor to line 3 (file starts on line 1 → Down,Down).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains('→'),
        "tab indicator (→) must be visible somewhere on the screen \
         when the cursor is on a tab-prefixed line.\n--- screen ---\n{screen}"
    );

    // Specifically: the line-3 row should have at least three
    // tab indicators.
    let (content_first_row, _) = harness.content_area_rows();
    let line_with_tabs = content_first_row + 2;
    let screen_lines: Vec<&str> = screen.lines().collect();
    if let Some(row) = screen_lines.get(line_with_tabs) {
        let tab_count = row.chars().filter(|&c| c == '→').count();
        assert!(
            tab_count >= 3,
            "expected at least 3 tab indicators on row {line_with_tabs} \
             (line 3 of the fixture, prefixed with \\t\\t\\t), found {tab_count}.\n\
             row={row:?}"
        );
    }
}

#[test]
fn migrated_cursor_at_column_zero_on_tab_line_lands_before_first_tab() {
    // Original: `test_cursor_before_first_tab`. Down,Down to reach
    // line 3 (tab-prefixed), then Home,Home — the first Home is
    // smart-home (jumps to first non-whitespace), the second goes
    // to true column 0. The hardware cursor must then land at the
    // start of the content area (~gutter width), NOT 7 columns
    // further into the first expanded tab.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (cursor_x, cursor_y) = harness.screen_cursor_position();
    let (content_first_row, _) = harness.content_area_rows();
    let expected_cursor_row = content_first_row as u16 + 2;

    assert_eq!(
        cursor_y, expected_cursor_row,
        "cursor must be on the line-3 content row ({expected_cursor_row}), got {cursor_y}"
    );

    let gutter_width = harness.editor().active_state().margins.left_total_width() as u16;
    assert!(
        cursor_x <= gutter_width + 1,
        "cursor at column 0 on a tab-prefixed line must land at the start of \
         content (~gutter width = {gutter_width}), NOT inside the first expanded \
         tab. Got cursor_x = {cursor_x} (would be ~{} if positioned after tab \
         expansion).",
        gutter_width + 7
    );

    // Visual: line 3's rendered row must still show the tab indicator.
    let screen = harness.screen_to_string();
    let lines: Vec<&str> = screen.lines().collect();
    if let Some(cursor_line) = lines.get(expected_cursor_row as usize) {
        assert!(
            cursor_line.contains('→'),
            "tab indicator (→) must still be rendered on the cursor's line \
             after Home,Home.\nrow={cursor_line:?}"
        );
    }
}

/// Anti-test: drop the Ctrl+End + overscroll keypresses entirely.
/// Without scrolling, the viewport stays at the top of the fixture
/// and the tail marker (`lfs_mirror_extend`, near line 45) must
/// NOT be visible — proves the positive test's tail-visibility
/// assertion depends on the actual navigation, not on the buffer
/// trivially fitting on screen at 80x24 (it doesn't — 47 lines,
/// content area is ~21 rows).
#[test]
fn anti_scroll_clearing_without_navigation_keeps_tail_off_screen() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();
    // No Ctrl+End, no PageDown, no Down — that's the load-bearing
    // sequence we drop.

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("lfs_mirror_extend"),
        "anti: without Ctrl+End + overscroll, the file's tail \
         (`lfs_mirror_extend`, near line 45 of the 47-line fixture) \
         must NOT be visible in the initial 80x24 viewport. \
         If this fires, the positive `migrated_scroll_clearing_at_bottom_of_file_*` \
         test's tail-visibility assertion is vacuously true.\n--- screen ---\n{screen}"
    );
}

/// Anti-test: drop the mouse `ScrollDown` events from the
/// keyboard→mouse sequence. After only the 10 keyboard Down
/// presses, `top_line_number` must remain at its post-keyboard
/// value — proves the positive Bug #248 assertion is gated on the
/// mouse events actually advancing the viewport, not on the
/// keyboard alone scrolling it.
#[test]
fn anti_mouse_wheel_after_keyboard_navigation_without_mouse_no_scroll() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    for _ in 0..10 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    let top_line_after_keyboard = harness.top_line_number();
    // No mouse ScrollDown events — that's the load-bearing step we drop.

    let top_line_now = harness.top_line_number();
    assert_eq!(
        top_line_now, top_line_after_keyboard,
        "anti: without mouse ScrollDown events the viewport top_line \
         must not advance further. If this fires, the positive Bug #248 \
         test's `actual_scroll > 0` assertion is vacuously satisfied \
         by keyboard-only effects."
    );
}

/// Anti-test: drop the Down,Down keypresses that move the cursor
/// onto the tab-prefixed line 3. With the cursor still on line 1
/// (which is blank), the tab indicator (`→`) on line 3 may or may
/// not be visible in the rendered viewport, but the cursor row's
/// rendered content must NOT match line 3's tab glyphs. Concretely:
/// after Home only (no Down), the hardware cursor's row equals
/// `content_first_row` (line 1), not `content_first_row + 2`
/// (line 3). Proves the positive `cursor_at_column_zero_on_tab_line_*`
/// test's "cursor lands on line-3 row" assertion is gated on the
/// Down,Down keypresses.
#[test]
fn anti_cursor_on_tab_line_without_down_keys_stays_on_line_one() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&scroll_test_file_path()).unwrap();
    harness.render().unwrap();

    // No Down,Down — cursor stays on line 1.
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let (_, cursor_y) = harness.screen_cursor_position();
    let (content_first_row, _) = harness.content_area_rows();
    let line_three_row = content_first_row as u16 + 2;
    assert_ne!(
        cursor_y, line_three_row,
        "anti: without Down,Down the cursor must NOT be on line 3 \
         (content row {line_three_row}). If this fires, the positive \
         `cursor_at_column_zero_on_tab_line_*` test's row-equality \
         assertion is vacuously satisfied without any navigation."
    );
}
