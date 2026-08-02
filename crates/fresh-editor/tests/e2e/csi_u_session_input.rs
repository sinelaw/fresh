//! Regression tests for issue #1113: CSI u escape sequences are written as
//! literal text in session attach mode.
//!
//! In session mode, raw terminal bytes flow through `InputParser` on the server
//! side. The parser must recognise CSI u sequences (the fixterms / kitty
//! keyboard protocol) and convert them to the appropriate crossterm events.
//! Before the fix, `parse_csi_final` had no handler for the `u` final byte,
//! causing the sequence to be treated as `Invalid` and its bytes dumped as
//! literal text into the editor buffer.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{Event, KeyCode, KeyModifiers};
use fresh::server::input_parser::InputParser;

/// Helper: assert that InputParser produces exactly one Key event matching the
/// expected keycode and modifiers.
fn assert_csi_u_parses(input: &[u8], expected_code: KeyCode, expected_mods: KeyModifiers) {
    let mut parser = InputParser::new();
    let events = parser.parse(input);
    assert_eq!(
        events.len(),
        1,
        "Input {:02x?}: expected 1 event, got {:?}",
        input,
        events
    );
    match &events[0] {
        Event::Key(ke) => {
            assert_eq!(
                ke.code, expected_code,
                "Input {:02x?}: wrong keycode",
                input
            );
            assert_eq!(
                ke.modifiers, expected_mods,
                "Input {:02x?}: wrong modifiers",
                input
            );
        }
        other => panic!("Input {:02x?}: expected Key event, got {:?}", input, other),
    }
}

/// End-to-end: CSI u sequences fed through InputParser → Editor must not leak
/// as literal text into the buffer. Covers Ctrl+Enter, Ctrl+Tab, Shift+Enter,
/// and a plain key — the original issue report plus variants.
#[test]
fn test_csi_u_sequences_not_inserted_as_literal_text() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut parser = InputParser::new();

    // Type some text, then send several CSI u sequences
    let sequences: &[&[u8]] = &[
        b"hello",
        b"\x1b[13;5u", // Ctrl+Enter
        b"\x1b[9;5u",  // Ctrl+Tab
        b"\x1b[13;2u", // Shift+Enter
        b"\x1b[97u",   // plain 'a'
    ];
    for seq in sequences {
        for event in parser.parse(seq) {
            if let Event::Key(ke) = event {
                harness.send_key(ke.code, ke.modifiers).unwrap();
            }
        }
    }

    let content = harness.get_buffer_content().unwrap_or_default();
    for literal in &["[13;5u", "[9;5u", "[13;2u", "[97u"] {
        assert!(
            !content.contains(literal),
            "CSI u sequence leaked as literal text {literal:?}: {content:?}",
        );
    }
}

/// End-to-end: with `keyboard_report_all_keys_as_escape_codes` on, every
/// printable key arrives as a CSI u sequence carrying the *base* key plus a
/// shift modifier. Typing shifted keys must put the shifted characters on
/// screen — before the fix the base characters were typed instead, so `A`
/// showed up as `a` and the Shift key looked dead (issue #2880).
#[test]
fn test_shifted_keys_type_their_character_on_screen() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let mut parser = InputParser::new();

    // Shift+F, Shift+r, Shift+e (the shifted codepoint reported alongside the
    // base key), then Shift+1 and Shift+/ for the symbols a keyboard layout —
    // not the character's case — decides.
    let sequences: &[&[u8]] = &[
        b"\x1b[102:70;2u",
        b"\x1b[114:82;2u",
        b"\x1b[101:69;2u",
        b"\x1b[49:33;2u",
        b"\x1b[47:63;2u",
    ];
    for seq in sequences {
        for event in parser.parse(seq) {
            if let Event::Key(ke) = event {
                harness.send_key(ke.code, ke.modifiers).unwrap();
            }
        }
    }
    harness.render().unwrap();

    harness.assert_screen_contains("FRE!?");
}

/// InputParser must map CSI u sequences to the correct KeyCode and modifiers.
/// Covers: special keycodes (Enter, Tab, Esc, Backspace, Space), printable
/// chars, no-modifier and multi-modifier combinations.
#[test]
fn test_input_parser_csi_u_keycodes_and_modifiers() {
    // (raw bytes, expected KeyCode, expected KeyModifiers)
    let cases: &[(&[u8], KeyCode, KeyModifiers)] = &[
        (b"\x1b[13;5u", KeyCode::Enter, KeyModifiers::CONTROL),
        (b"\x1b[9;5u", KeyCode::Tab, KeyModifiers::CONTROL),
        (b"\x1b[27u", KeyCode::Esc, KeyModifiers::empty()),
        (b"\x1b[127;5u", KeyCode::Backspace, KeyModifiers::CONTROL),
        (b"\x1b[97u", KeyCode::Char('a'), KeyModifiers::empty()),
        // modifier 4 → param-1 = 3 → shift(1) | alt(2)
        (
            b"\x1b[13;4u",
            KeyCode::Enter,
            KeyModifiers::SHIFT.union(KeyModifiers::ALT),
        ),
        (b"\x1b[13;2u", KeyCode::Enter, KeyModifiers::SHIFT),
    ];

    for &(input, code, mods) in cases {
        assert_csi_u_parses(input, code, mods);
    }
}

/// xterm modifyOtherKeys mode 2: CSI 27 ; modifier ; keycode ~
/// Must produce the correct KeyCode and modifiers, not leak as literal text.
#[test]
fn test_input_parser_xterm_modify_other_keys() {
    let cases: &[(&[u8], KeyCode, KeyModifiers)] = &[
        // CSI 27 ; 5 ; 97 ~ = Ctrl+a
        (b"\x1b[27;5;97~", KeyCode::Char('a'), KeyModifiers::CONTROL),
        // CSI 27 ; 5 ; 13 ~ = Ctrl+Enter
        (b"\x1b[27;5;13~", KeyCode::Enter, KeyModifiers::CONTROL),
        // CSI 27 ; 2 ; 9 ~ = Shift+Tab
        (b"\x1b[27;2;9~", KeyCode::Tab, KeyModifiers::SHIFT),
        // CSI 27 ; 3 ; 127 ~ = Alt+Backspace
        (b"\x1b[27;3;127~", KeyCode::Backspace, KeyModifiers::ALT),
        // CSI 27 ; 1 ; 27 ~ = Escape (no modifiers, param 1 = none)
        (b"\x1b[27;1;27~", KeyCode::Esc, KeyModifiers::empty()),
    ];

    for &(input, code, mods) in cases {
        assert_csi_u_parses(input, code, mods);
    }
}

/// CSI u sequence split across two parse() calls must still be recognised.
#[test]
fn test_input_parser_csi_u_split_across_chunks() {
    let mut parser = InputParser::new();

    // First chunk: incomplete
    let events = parser.parse(b"\x1b[13");
    assert!(events.is_empty(), "Incomplete CSI u should buffer");

    // Second chunk completes the sequence
    let events = parser.parse(b";5u");
    assert_eq!(events.len(), 1, "Expected 1 event, got: {:?}", events);
    match &events[0] {
        Event::Key(ke) => {
            assert_eq!(ke.code, KeyCode::Enter);
            assert!(ke.modifiers.contains(KeyModifiers::CONTROL));
        }
        other => panic!("Expected Key event, got {:?}", other),
    }
}
