//! Byte offsets on multi-byte text in large-file mode (issue #3285).
//!
//! Large-file mode computes some offsets by arithmetic — a reach back from the
//! end of a line too long to search, a byte typed into "Go to Byte Offset" —
//! and on CJK text most bytes are inside a character. An anchor there drew
//! replacement glyphs, and the lossy decode of them shifted every later offset
//! on the row, so a click put the caret inside a character: typing there wrote
//! invalid UTF-8 to disk, and measuring the caret's column panicked.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// A tiny threshold so the fixture below loads in large-file mode.
fn force_large_file_config() -> fresh::config::Config {
    fresh::config::Config {
        editor: fresh::config::EditorConfig {
            large_file_threshold_bytes: 1024,
            ..Default::default()
        },
        ..Default::default()
    }
}

/// A short first line, then one CJK line longer than the 64 KB the renderer
/// searches back for a line start, so scrolling to its end has to start the
/// walk mid-line.
///
/// `tail` ASCII bytes end the long line. The walk reaches back a fixed number
/// of bytes from the end, so varying the tail moves where it lands relative
/// to the three-byte characters: one of 0, 1, 2 lands on a boundary and the
/// other two inside a character.
fn write_fixture(dir: &Path, tail: usize) -> PathBuf {
    let path = dir.join(format!("cjk_{tail}.txt"));
    let mut content = String::from("head\n");
    // Cycle through distinct characters so a row is not one glyph repeated.
    for i in 0..30_000u32 {
        content.push(char::from_u32(0x4E00 + (i % 2000)).unwrap());
    }
    content.push_str(&"a".repeat(tail));
    content.push('\n');
    std::fs::write(&path, content).unwrap();
    path
}

fn open(dir: &TempDir, path: &Path) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        force_large_file_config(),
        dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(path).unwrap();
    harness.render().unwrap();
    harness
}

/// Whether the screen shows a decoding artefact: a replacement glyph, or a
/// stray byte drawn as `<XX>`. The fixture is valid UTF-8, so either one means
/// a row started inside a character.
fn shows_broken_characters(screen: &str) -> bool {
    screen.contains('\u{FFFD}')
        || screen
            .as_bytes()
            .windows(4)
            .any(|w| w[0] == b'<' && w[3] == b'>' && (b'8'..=b'B').contains(&w[1]))
}

/// Screen row of the `n`th row that shows CJK text.
fn nth_cjk_row(harness: &EditorTestHarness, n: usize) -> u16 {
    (0..30u16)
        .filter(|r| {
            harness
                .screen_row_text(*r)
                .chars()
                .any(|c| ('\u{4E00}'..='\u{9FFF}').contains(&c))
        })
        .nth(n)
        .expect("the long line fills the screen")
}

#[test]
fn ctrl_end_on_a_long_cjk_line_starts_the_top_row_on_a_character() {
    let dir = TempDir::new().unwrap();
    for tail in 0..3 {
        let path = write_fixture(dir.path(), tail);
        let mut harness = open(&dir, &path);

        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        assert!(
            !shows_broken_characters(&screen),
            "tail {tail}: a row starts inside a character. Screen:\n{screen}"
        );
    }
}

/// The observable end of the offset drift: what is typed where the user
/// clicked lands between two characters, and the file saved afterwards is
/// still UTF-8.
#[test]
fn typing_where_a_long_cjk_line_was_clicked_keeps_the_file_utf8() {
    let dir = TempDir::new().unwrap();
    for tail in 0..3 {
        let path = write_fixture(dir.path(), tail);
        let mut harness = open(&dir, &path);

        harness
            .send_key(KeyCode::End, KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        let row = nth_cjk_row(&harness, 2);
        harness.mouse_click(50, row).unwrap();
        harness.type_text("x").unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        assert!(
            screen.contains('x') && !shows_broken_characters(&screen),
            "tail {tail}: the typed character should sit between whole characters. \
             Screen:\n{screen}"
        );

        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        let saved = std::fs::read(&path).unwrap();
        let text = String::from_utf8(saved)
            .unwrap_or_else(|e| panic!("tail {tail}: the save wrote invalid UTF-8: {e}"));
        assert_eq!(
            text.matches('x').count(),
            1,
            "tail {tail}: exactly the typed character was added"
        );
    }
}

/// "Go to Byte Offset" takes whatever number is typed. On CJK text two bytes in
/// three are inside a character, and the caret used to go exactly there.
#[test]
fn go_to_byte_offset_inside_a_character_puts_the_caret_after_it() {
    let dir = TempDir::new().unwrap();
    let path = write_fixture(dir.path(), 0);
    let mut harness = open(&dir, &path);

    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Go to Byte Offset");
    // Past the default button, [ Scan ], to the byte-offset one.
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // "head\n" is five bytes, so byte 6 is the second byte of the first
    // character of the long line.
    harness.type_text("6").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Byte 8");

    harness.type_text("x").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let saved =
        String::from_utf8(std::fs::read(&path).unwrap()).expect("the save must still be UTF-8");
    assert!(
        saved.starts_with("head\n\u{4E00}x\u{4E01}"),
        "the typed character goes after the character the offset was inside"
    );
}
