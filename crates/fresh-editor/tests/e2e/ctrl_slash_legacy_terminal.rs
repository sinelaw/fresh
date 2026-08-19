//! Regression tests for sinelaw/fresh#2933: `Ctrl+/` did nothing on any
//! terminal without the kitty keyboard protocol.
//!
//! Those terminals have no CSI-u form for the chord — they send the bare `US`
//! byte (0x1F), which the parser reported as `Ctrl+_`. Nothing connected that
//! back to the `ctrl+/` binding, so toggle-comment fired under kitty and
//! nowhere else.
//!
//! These drive the raw bytes a real terminal would send through `InputParser`
//! and assert on what ends up rendered, so they cover the whole path
//! (bytes → key event → binding → edit → screen) rather than any one layer.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::server::input_parser::{Event, InputParser};
use tempfile::TempDir;

/// Feed raw terminal bytes through the parser into the editor, exactly as
/// session mode does on the server side.
fn send_bytes(harness: &mut EditorTestHarness, bytes: &[u8]) {
    let mut parser = InputParser::new();
    for event in parser.parse(bytes) {
        if let Event::Key(press) = event {
            harness.send_key_press(press).unwrap();
        }
    }
}

/// Open a Rust file (`//` line comments) on its own temp dir, so the tests stay
/// isolated from each other and from the host.
fn harness_with_rust_file() -> (TempDir, EditorTestHarness) {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.rs");
    std::fs::write(&file_path, "fn main() {}\n").unwrap();

    let mut config = Config::default();
    // Pin the "default" keymap: `Config::default()` selects `macos` on macOS,
    // which binds this chord differently.
    config.active_keybinding_map = fresh::config::KeybindingMapName("default".to_string());

    let mut harness =
        EditorTestHarness::create(80, 24, HarnessOptions::new().with_config(config)).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    (temp_dir, harness)
}

/// The bug as reported: on a terminal with no kitty protocol, pressing Ctrl+/
/// sends 0x1F and must comment the line.
#[test]
fn ctrl_slash_toggles_comment_from_a_legacy_terminal_byte() {
    let (_temp_dir, mut harness) = harness_with_rust_file();
    harness.assert_screen_contains("fn main() {}");

    send_bytes(&mut harness, &[0x1f]);
    harness.render().unwrap();

    harness.assert_screen_contains("// fn main() {}");
}

/// The same chord under the kitty keyboard protocol, which already worked —
/// kept as the control that says both encodings now land on one binding.
#[test]
fn ctrl_slash_toggles_comment_from_the_kitty_encoding() {
    let (_temp_dir, mut harness) = harness_with_rust_file();

    send_bytes(&mut harness, b"\x1b[47;5u");
    harness.render().unwrap();

    harness.assert_screen_contains("// fn main() {}");
}

/// The chord is a toggle, so the same byte twice must comment and then
/// uncomment — proving the second press resolves to the binding as well, not
/// just the first.
#[test]
fn the_legacy_byte_round_trips_the_comment() {
    let (_temp_dir, mut harness) = harness_with_rust_file();

    send_bytes(&mut harness, &[0x1f]);
    harness.render().unwrap();
    harness.assert_screen_contains("// fn main() {}");

    send_bytes(&mut harness, &[0x1f]);
    harness.render().unwrap();
    harness.assert_screen_not_contains("// fn main() {}");
    harness.assert_screen_contains("fn main() {}");
}

// ---- The non-US half of the same issue ----
//
// On a layout where `/` needs Shift (German, French, Spanish, …) there is no
// `Ctrl+/` for a terminal to report. kitty sends the physical chord plus the
// character it types — `CSI 55:47;6u`: base 55 (`7`), shifted 47 (`/`),
// Ctrl+Shift — and `default.json` binds `ctrl+shift+7` to `set_bookmark`, so
// the chord users press for "comment this line" silently set a bookmark.

/// A German keyboard's Ctrl+/ must comment the line, exactly as a US one does.
#[test]
fn ctrl_slash_toggles_comment_on_a_layout_where_slash_needs_shift() {
    let (_temp_dir, mut harness) = harness_with_rust_file();
    harness.assert_screen_contains("fn main() {}");

    send_bytes(&mut harness, b"\x1b[55:47;6u");
    harness.render().unwrap();

    harness.assert_screen_contains("// fn main() {}");
}

/// …and the US reading of the very same physical chord is untouched: Shift+7
/// types `&` there, nothing binds `ctrl+&`, so `Ctrl+Shift+7` still reaches
/// `set_bookmark` rather than commenting the line. This is the test that says
/// the layout reading is a fallback, not a rewrite.
#[test]
fn the_same_chord_on_a_us_layout_still_reaches_its_digit_binding() {
    let (_temp_dir, mut harness) = harness_with_rust_file();

    send_bytes(&mut harness, b"\x1b[55:38;6u");
    harness.render().unwrap();

    harness.assert_screen_not_contains("// fn main() {}");
    harness.assert_screen_contains("fn main() {}");
}
