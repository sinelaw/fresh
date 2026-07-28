//! Regression tests for issue #2843 — a file that is one very long line.
//!
//! The defects only show up once the viewport has scrolled *into* a
//! soft-wrapped line, i.e. when `top_byte` stays at the line's start and the
//! position is carried by the wrap-segment offset instead.
//!
//! Asserts on rendered output only (CONTRIBUTING §2).

use crate::common::harness::EditorTestHarness;

/// Rightmost column of a `width`-wide terminal — where the vertical
/// scrollbar is drawn.
fn scrollbar_col(width: u16) -> u16 {
    width - 1
}

/// First `M`-prefixed six-digit marker on the screen, in reading order.
/// The markers ascend along the line, so this says which part of the line
/// the viewport is showing.
fn first_marker(screen: &str) -> Option<u32> {
    for line in screen.lines() {
        let bytes = line.as_bytes();
        for (i, _) in line.match_indices('M') {
            let digits = &bytes[i + 1..];
            if digits.len() >= 6 && digits[..6].iter().all(|b| b.is_ascii_digit()) {
                return std::str::from_utf8(&digits[..6]).ok()?.parse().ok();
            }
        }
    }
    None
}

/// Click the scrollbar to land deep inside the single line, then wheel down
/// one notch. The view must advance a little — the bug threw the scrollbar
/// position away and snapped `top_byte` to 100,000 (`MAX_LINE_BYTES`, the
/// size of the chunks `LineIterator` splits over-long lines into), which on
/// this file is far *above* where the click landed. Scroll math was treating
/// a read-budget boundary as a line boundary.
#[test]
fn wheel_after_scrollbar_jump_continues_from_the_jump_position() {
    const WIDTH: u16 = 100;
    const HEIGHT: u16 = 30;
    // Comfortably more than the 100,000-byte chunk size, so a snap to the
    // chunk boundary is unmistakably backwards from where the click lands.
    const MARKERS: u32 = 50_000;

    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    let dir = harness.project_dir().unwrap();
    let path = dir.join("one_line.txt");

    // One line of ascending 8-byte markers (~400 KB), so the topmost visible
    // marker says exactly how far into the line the viewport sits.
    let mut content = String::with_capacity(MARKERS as usize * 8);
    for i in 0..MARKERS {
        content.push_str(&format!("M{i:06} "));
    }
    std::fs::write(&path, &content).unwrap();

    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Land well past the 100,000-byte chunk boundary (~row 20 of 30 on the
    // track is roughly two thirds down a ~400 KB line).
    harness.mouse_click(scrollbar_col(WIDTH), 20).unwrap();
    harness.render().unwrap();

    let before = harness.screen_to_string();
    let top_before = first_marker(&before)
        .unwrap_or_else(|| panic!("no marker visible after the scrollbar jump:\n{before}"));
    assert!(
        top_before > 20_000,
        "precondition: the scrollbar jump should land past the 100,000-byte \
         chunk boundary (marker {top_before} is byte ~{}). Screen:\n{before}",
        top_before * 8
    );

    harness.mouse_scroll_down(WIDTH / 2, HEIGHT / 2).unwrap();
    harness.render().unwrap();

    let after = harness.screen_to_string();
    let top_after = first_marker(&after)
        .unwrap_or_else(|| panic!("no marker visible after the wheel notch:\n{after}"));

    assert!(
        top_after > top_before,
        "one wheel notch must scroll forward from where the scrollbar left the \
         viewport (top marker was M{top_before:06}, now M{top_after:06}) — \
         going backwards means the wheel snapped to a chunk boundary. \
         Screen:\n{after}"
    );
    // A notch is three visual rows; each row holds at most `WIDTH` bytes, so
    // a legitimate notch cannot move more than a few hundred markers.
    assert!(
        top_after - top_before < 500,
        "one wheel notch moved from M{top_before:06} to M{top_after:06} — that \
         is a jump, not a scroll. Screen:\n{after}"
    );
}

/// Foreground colour of the first cell of `needle` on screen.
fn fg_of(harness: &EditorTestHarness, needle: &str) -> Option<ratatui::style::Color> {
    let (col, row) = harness.find_text_on_screen(needle)?;
    harness.get_cell_style(col, row).map(|s| s.fg)?
}

/// A single-line JSON whose tail carries a string and a number, behind a
/// padding string long enough that the tail is over a hundred KB in. After
/// scrolling to the bottom, the two tail tokens must render in their own
/// syntax colours.
///
/// The bug asked the highlighter for a byte window anchored at `top_byte`
/// and one screen row wide (`0..952` on the issue's file) no matter how far
/// the viewport had scrolled into the line, so every row past the first few
/// wrapped ones was painted by whatever single span happened to overlap
/// that window — uniformly, with no structure — or left undecorated.
#[test]
fn wrapped_long_line_keeps_syntax_colours_when_scrolled_to_the_tail() {
    const WIDTH: u16 = 100;
    const HEIGHT: u16 = 30;

    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    let dir = harness.project_dir().unwrap();
    let path = dir.join("one_line.json");

    // ~150 KB of padding inside one JSON string, then a string value and a
    // numeric value. No trailing newline: the whole file is one line, and the
    // tail sits well past the first screenful of wrap segments.
    let padding = "pad ".repeat(37_500);
    let content = format!(r#"{{"pad":"{padding}","tag":"ZZSTRINGZZ","num":424242}}"#);
    std::fs::write(&path, &content).unwrap();

    harness.open_file(&path).unwrap();
    harness.render().unwrap();

    // Jump to the bottom of the scrollbar track so the tail is on screen.
    let (_, last_content_row) = harness.content_area_rows();
    harness
        .mouse_click(scrollbar_col(WIDTH), last_content_row as u16)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("ZZSTRINGZZ") && screen.contains("424242"),
        "precondition: the line's tail should be visible after a scrollbar \
         jump to the bottom. Screen:\n{screen}"
    );

    let string_fg = fg_of(&harness, "ZZSTRINGZZ")
        .unwrap_or_else(|| panic!("no style for the tail string. Screen:\n{screen}"));
    let number_fg = fg_of(&harness, "424242")
        .unwrap_or_else(|| panic!("no style for the tail number. Screen:\n{screen}"));

    assert_ne!(
        string_fg, number_fg,
        "after scrolling into a soft-wrapped line the tail's string and number \
         must still be syntax-coloured differently (string {string_fg:?}, \
         number {number_fg:?}) — identical colours mean the decoration pass is \
         still describing the top of the line. Screen:\n{screen}"
    );
}
