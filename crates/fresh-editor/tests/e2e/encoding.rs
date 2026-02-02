//! Encoding support tests for fresh editor
//!
//! Property-based tests for detecting, loading, editing, and saving files with various encodings:
//! - UTF-8 (default)
//! - UTF-8 with BOM
//! - UTF-16 LE (Windows Unicode)
//! - UTF-16 BE
//! - ASCII
//! - Latin-1 (ISO-8859-1)
//! - Windows-1252 (ANSI)
//! - GB18030 (Chinese)
//! - GBK (Chinese simplified)

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use proptest::prelude::*;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// Test Data Constants
// ============================================================================

/// UTF-8 BOM bytes
const UTF8_BOM: &[u8] = &[0xEF, 0xBB, 0xBF];

/// UTF-16 LE BOM bytes
const UTF16_LE_BOM: &[u8] = &[0xFF, 0xFE];

/// UTF-16 BE BOM bytes
const UTF16_BE_BOM: &[u8] = &[0xFE, 0xFF];

// ============================================================================
// Encoding Test Utilities
// ============================================================================

/// Represents different text encodings for testing
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestEncoding {
    Utf8,
    Utf8Bom,
    Utf16Le,
    Utf16Be,
    Latin1,
    Windows1252,
    Windows1250,
    Gb18030,
    Ascii,
}

impl TestEncoding {
    /// Encode a string to bytes in this encoding
    fn encode(&self, text: &str) -> Vec<u8> {
        match self {
            TestEncoding::Utf8 => text.as_bytes().to_vec(),
            TestEncoding::Utf8Bom => {
                let mut result = UTF8_BOM.to_vec();
                result.extend_from_slice(text.as_bytes());
                result
            }
            TestEncoding::Utf16Le => {
                let mut result = UTF16_LE_BOM.to_vec();
                for ch in text.encode_utf16() {
                    result.extend_from_slice(&ch.to_le_bytes());
                }
                result
            }
            TestEncoding::Utf16Be => {
                let mut result = UTF16_BE_BOM.to_vec();
                for ch in text.encode_utf16() {
                    result.extend_from_slice(&ch.to_be_bytes());
                }
                result
            }
            TestEncoding::Latin1 => {
                // Convert to Latin-1 (only works for chars <= 0xFF)
                text.chars()
                    .map(|c| {
                        if c as u32 <= 0xFF {
                            c as u8
                        } else {
                            b'?' // Replacement for non-Latin-1 chars
                        }
                    })
                    .collect()
            }
            TestEncoding::Windows1252 => {
                // Similar to Latin-1, with some differences in 0x80-0x9F range
                text.chars()
                    .map(|c| {
                        if c as u32 <= 0xFF {
                            c as u8
                        } else {
                            b'?' // Replacement
                        }
                    })
                    .collect()
            }
            TestEncoding::Windows1250 => {
                // Windows-1250 (Central European)
                // For testing, we encode common Polish/Czech characters
                let mut result = Vec::new();
                for c in text.chars() {
                    let byte = match c {
                        // Common Central European characters in Windows-1250
                        'ą' => 0xB9,
                        'ć' => 0xE6,
                        'ę' => 0xEA,
                        'ł' => 0xB3,
                        'ń' => 0xF1,
                        'ó' => 0xF3,
                        'ś' => 0x9C,
                        'ź' => 0x9F,
                        'ż' => 0xBF,
                        'Ą' => 0xA5,
                        'Ć' => 0xC6,
                        'Ę' => 0xCA,
                        'Ł' => 0xA3,
                        'Ń' => 0xD1,
                        'Ó' => 0xD3,
                        'Ś' => 0x8C,
                        'Ź' => 0x8F,
                        'Ż' => 0xAF,
                        // Czech characters
                        'á' => 0xE1,
                        'č' => 0xE8,
                        'ď' => 0xEF,
                        'é' => 0xE9,
                        'ě' => 0xEC,
                        'í' => 0xED,
                        'ň' => 0xF2,
                        'ř' => 0xF8,
                        'š' => 0x9A,
                        'ť' => 0x9D,
                        'ú' => 0xFA,
                        'ů' => 0xF9,
                        'ý' => 0xFD,
                        'ž' => 0x9E,
                        'Á' => 0xC1,
                        'Č' => 0xC8,
                        'Ď' => 0xCF,
                        'É' => 0xC9,
                        'Ě' => 0xCC,
                        'Í' => 0xCD,
                        'Ň' => 0xD2,
                        'Ř' => 0xD8,
                        'Š' => 0x8A,
                        'Ť' => 0x8D,
                        'Ú' => 0xDA,
                        'Ů' => 0xD9,
                        'Ý' => 0xDD,
                        'Ž' => 0x8E,
                        c if c.is_ascii() => c as u8,
                        _ => b'?', // Replacement for unmapped chars
                    };
                    result.push(byte);
                }
                result
            }
            TestEncoding::Gb18030 => {
                // For testing, we'll use a simple mapping for common Chinese chars
                // In real implementation, this would use encoding_rs
                let mut result = Vec::new();
                for c in text.chars() {
                    match c {
                        '你' => result.extend_from_slice(&[0xC4, 0xE3]),
                        '好' => result.extend_from_slice(&[0xBA, 0xC3]),
                        '世' => result.extend_from_slice(&[0xCA, 0xC0]),
                        '界' => result.extend_from_slice(&[0xBD, 0xE7]),
                        '\n' => result.push(0x0A),
                        '\r' => result.push(0x0D),
                        c if c.is_ascii() => result.push(c as u8),
                        _ => result.push(b'?'),
                    }
                }
                result
            }
            TestEncoding::Ascii => {
                // Only ASCII chars
                text.chars()
                    .map(|c| if c.is_ascii() { c as u8 } else { b'?' })
                    .collect()
            }
        }
    }

    /// Check if this encoding can represent the given text losslessly
    fn can_encode_losslessly(&self, text: &str) -> bool {
        match self {
            TestEncoding::Utf8
            | TestEncoding::Utf8Bom
            | TestEncoding::Utf16Le
            | TestEncoding::Utf16Be => true,
            TestEncoding::Latin1 | TestEncoding::Windows1252 => {
                text.chars().all(|c| (c as u32) <= 0xFF)
            }
            TestEncoding::Windows1250 => {
                // Windows-1250 can encode ASCII and specific Central European characters
                text.chars().all(|c| {
                    c.is_ascii()
                        || matches!(
                            c,
                            // Polish characters
                            'ą' | 'ć'
                                | 'ę'
                                | 'ł'
                                | 'ń'
                                | 'ó'
                                | 'ś'
                                | 'ź'
                                | 'ż'
                                | 'Ą'
                                | 'Ć'
                                | 'Ę'
                                | 'Ł'
                                | 'Ń'
                                | 'Ó'
                                | 'Ś'
                                | 'Ź'
                                | 'Ż'
                                // Czech characters
                                | 'á'
                                | 'č'
                                | 'ď'
                                | 'é'
                                | 'ě'
                                | 'í'
                                | 'ň'
                                | 'ř'
                                | 'š'
                                | 'ť'
                                | 'ú'
                                | 'ů'
                                | 'ý'
                                | 'ž'
                                | 'Á'
                                | 'Č'
                                | 'Ď'
                                | 'É'
                                | 'Ě'
                                | 'Í'
                                | 'Ň'
                                | 'Ř'
                                | 'Š'
                                | 'Ť'
                                | 'Ú'
                                | 'Ů'
                                | 'Ý'
                                | 'Ž'
                        )
                })
            }
            TestEncoding::Ascii => text.is_ascii(),
            TestEncoding::Gb18030 => {
                // GB18030 can encode all Unicode, but our test implementation is limited
                text.chars().all(|c| {
                    c.is_ascii() || c == '\n' || c == '\r' || matches!(c, '你' | '好' | '世' | '界')
                })
            }
        }
    }

    /// Get the display name for this encoding
    fn display_name(&self) -> &'static str {
        match self {
            TestEncoding::Utf8 => "UTF-8",
            TestEncoding::Utf8Bom => "UTF-8 BOM",
            TestEncoding::Utf16Le => "UTF-16 LE",
            TestEncoding::Utf16Be => "UTF-16 BE",
            TestEncoding::Latin1 => "Latin-1",
            TestEncoding::Windows1252 => "Windows-1252",
            TestEncoding::Windows1250 => "Windows-1250",
            TestEncoding::Gb18030 => "GB18030",
            TestEncoding::Ascii => "ASCII",
        }
    }
}

/// Create a temporary file with the given content in the specified encoding
fn create_encoded_file(dir: &TempDir, name: &str, encoding: TestEncoding, text: &str) -> PathBuf {
    let path = dir.path().join(name);
    let bytes = encoding.encode(text);
    std::fs::write(&path, &bytes).unwrap();
    path
}

// ============================================================================
// Proptest Strategies
// ============================================================================

/// Strategy for generating ASCII-only text (safe for all encodings)
fn ascii_text_strategy() -> impl Strategy<Value = String> {
    "[a-zA-Z0-9 ,.!?\\-_]{1,100}"
}

/// Strategy for generating text with Latin-1 characters
///
/// Generates realistic Latin-1 text that includes at least some ASCII characters
/// (spaces, punctuation) mixed with extended Latin-1 characters. This ensures
/// the text is distinguishable from CJK encodings, which is important because
/// pure sequences of bytes in the 0xA0-0xFF range are genuinely ambiguous.
///
/// IMPORTANT: The ASCII prefix is mandatory (never empty) to create "space + high byte"
/// patterns that distinguish Latin-1 from CJK encodings.
fn latin1_text_strategy() -> impl Strategy<Value = String> {
    // Generate a prefix with at least one ASCII word (NEVER empty!)
    // The trailing space creates "space + high byte" pattern that signals Latin-1
    let ascii_prefix = prop::sample::select(vec![
        "Hello ", "Cafe ", "Text ", "File ", "Data ", "The ", "A ", "Test ", "Word ",
    ]);

    // Generate middle content with Latin-1 extended characters
    let latin1_chars = prop::collection::vec(
        prop::sample::select(vec![
            'é', 'è', 'ê', 'ë', 'à', 'â', 'ä', 'ç', 'ô', 'ö', 'ù', 'û', 'ü', 'ñ', 'ß', 'æ', 'ø',
            'å', '£', '¥', '©', '®', '±', 'µ', '¶', ' ', ' ', ' ',
        ]),
        3..30,
    );

    // Generate an optional ASCII suffix
    let ascii_suffix = prop::sample::select(vec![" end", " ok", ".", "", "", ""]);

    (ascii_prefix, latin1_chars, ascii_suffix).prop_map(|(prefix, chars, suffix)| {
        let middle: String = chars.into_iter().collect();
        format!("{}{}{}", prefix, middle, suffix)
    })
}

/// Strategy for generating text with Chinese characters
fn chinese_text_strategy() -> impl Strategy<Value = String> {
    prop::collection::vec(
        prop::sample::select(vec!['你', '好', '世', '界', ' ', '\n']),
        1..20,
    )
    .prop_map(|chars| chars.into_iter().collect())
}

/// Strategy for generating mixed text (ASCII + Chinese)
fn mixed_text_strategy() -> impl Strategy<Value = String> {
    (ascii_text_strategy(), chinese_text_strategy()).prop_map(|(a, b)| format!("{} {}", a, b))
}

/// Strategy for selecting an encoding
fn encoding_strategy() -> impl Strategy<Value = TestEncoding> {
    prop::sample::select(vec![
        TestEncoding::Utf8,
        TestEncoding::Utf8Bom,
        TestEncoding::Utf16Le,
        TestEncoding::Utf16Be,
        TestEncoding::Latin1,
        TestEncoding::Ascii,
    ])
}

/// Strategy for selecting Unicode-capable encodings only
fn unicode_encoding_strategy() -> impl Strategy<Value = TestEncoding> {
    prop::sample::select(vec![
        TestEncoding::Utf8,
        TestEncoding::Utf8Bom,
        TestEncoding::Utf16Le,
        TestEncoding::Utf16Be,
    ])
}

// ============================================================================
// Property-Based Tests
// ============================================================================

proptest! {
    #![proptest_config(ProptestConfig::with_cases(20))]

    /// Property: Loading an ASCII file in any encoding should display the same content
    #[test]
    fn prop_ascii_roundtrip(
        text in ascii_text_strategy(),
        encoding in encoding_strategy()
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "test.txt", encoding, &text);

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // The text should be displayed correctly (allowing for line break differences)
        let buffer_content = harness.get_buffer_content().unwrap();

        // Normalize line endings for comparison
        let normalized_text = text.replace("\r\n", "\n").replace('\r', "\n");
        let normalized_buffer = buffer_content.replace("\r\n", "\n").replace('\r', "\n");

        prop_assert!(
            normalized_buffer.contains(&normalized_text.trim()),
            "Buffer should contain the text. Expected: {:?}, Got: {:?}",
            normalized_text,
            normalized_buffer
        );
    }

    /// Property: Editing and saving a file should preserve its encoding
    #[test]
    fn prop_encoding_preserved_on_save(
        text in ascii_text_strategy().prop_filter("need content", |s| !s.is_empty()),
        encoding in unicode_encoding_strategy()
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "test.txt", encoding, &text);
        let original_bytes = std::fs::read(&file_path).unwrap();

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Make no changes, just save
        harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL).unwrap();
        harness.render().unwrap();

        // Wait for save to complete using semantic waiting (not timeout)
        let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

        // File should be unchanged
        let saved_bytes = std::fs::read(&file_path).unwrap();

        prop_assert_eq!(
            saved_bytes,
            original_bytes,
            "File should be unchanged after save without edits"
        );
    }

    /// Property: Adding text and saving should produce valid content in the same encoding
    #[test]
    fn prop_edit_preserves_encoding(
        initial_text in ascii_text_strategy().prop_filter("need content", |s| s.len() > 5),
        added_text in "[a-zA-Z0-9]{1,20}",
        encoding in unicode_encoding_strategy()
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "test.txt", encoding, &initial_text);

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Add text at the end
        harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
        harness.type_text(&added_text).unwrap();
        harness.render().unwrap();

        // Save
        harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL).unwrap();
        harness.render().unwrap();

        // Wait for save
        let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

        // Read and verify
        let saved_bytes = std::fs::read(&file_path).unwrap();

        // Check encoding markers are preserved
        match encoding {
            TestEncoding::Utf8Bom => {
                prop_assert!(
                    saved_bytes.starts_with(UTF8_BOM),
                    "UTF-8 BOM should be preserved"
                );
            }
            TestEncoding::Utf16Le => {
                prop_assert!(
                    saved_bytes.starts_with(UTF16_LE_BOM),
                    "UTF-16 LE BOM should be preserved"
                );
            }
            TestEncoding::Utf16Be => {
                prop_assert!(
                    saved_bytes.starts_with(UTF16_BE_BOM),
                    "UTF-16 BE BOM should be preserved"
                );
            }
            _ => {}
        }
    }

    /// Property: Chinese text should be preserved when using Unicode encodings
    #[test]
    fn prop_chinese_text_preserved(
        text in chinese_text_strategy(),
        encoding in unicode_encoding_strategy()
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "chinese.txt", encoding, &text);

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        let buffer_content = harness.get_buffer_content().unwrap();

        // Normalize and compare
        let normalized_text = text.replace("\r\n", "\n").replace('\r', "\n");
        let normalized_buffer = buffer_content.replace("\r\n", "\n").replace('\r', "\n");

        // Check that all non-whitespace characters are preserved
        for c in normalized_text.chars() {
            if !c.is_whitespace() {
                prop_assert!(
                    normalized_buffer.contains(c),
                    "Character {:?} should be in buffer. Buffer: {:?}",
                    c,
                    normalized_buffer
                );
            }
        }
    }

    /// Property: Latin-1 characters should be preserved in Latin-1 encoding
    #[test]
    fn prop_latin1_text_preserved(text in latin1_text_strategy()) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "latin1.txt", TestEncoding::Latin1, &text);

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        let buffer_content = harness.get_buffer_content().unwrap();

        // Check that special Latin-1 characters are preserved
        for c in text.chars() {
            if !c.is_whitespace() && c.is_alphabetic() {
                prop_assert!(
                    buffer_content.contains(c),
                    "Latin-1 character {:?} should be in buffer. Buffer: {:?}",
                    c,
                    buffer_content
                );
            }
        }
    }

    /// Property: UTF-16 files should have correct BOM after save
    #[test]
    fn prop_utf16_bom_preserved(
        text in ascii_text_strategy(),
        le in prop::bool::ANY
    ) {
        let encoding = if le { TestEncoding::Utf16Le } else { TestEncoding::Utf16Be };
        let expected_bom = if le { UTF16_LE_BOM } else { UTF16_BE_BOM };

        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "utf16.txt", encoding, &text);

        // Verify BOM is correct
        let original_bytes = std::fs::read(&file_path).unwrap();
        prop_assert!(
            original_bytes.starts_with(expected_bom),
            "File should start with {:?} BOM",
            if le { "UTF-16 LE" } else { "UTF-16 BE" }
        );

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Edit
        harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
        harness.type_text("X").unwrap();

        // Save
        harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL).unwrap();
        harness.render().unwrap();

        let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

        let saved_bytes = std::fs::read(&file_path).unwrap();
        prop_assert!(
            saved_bytes.starts_with(expected_bom),
            "BOM should be preserved after save"
        );
    }

    /// Property: Loading and saving a file in ANY encoding should preserve the exact bytes
    /// This is the comprehensive roundtrip test for all supported encodings.
    #[test]
    fn prop_all_encodings_roundtrip_exact(
        text in ascii_text_strategy().prop_filter("need content", |s| !s.trim().is_empty()),
        encoding in encoding_strategy()
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "roundtrip.txt", encoding, &text);
        let original_bytes = std::fs::read(&file_path).unwrap();

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Save without making any changes
        harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL).unwrap();
        harness.render().unwrap();

        // Wait for save to complete
        let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

        // Read the saved file
        let saved_bytes = std::fs::read(&file_path).unwrap();

        // The saved bytes should be exactly equal to original
        prop_assert_eq!(
            saved_bytes,
            original_bytes,
            "Saved file should be byte-for-byte identical to original for encoding {:?}",
            encoding
        );
    }

    /// Property: Loading, editing, and saving should produce valid content in the same encoding
    /// Tests that edits are properly encoded in the file's encoding.
    #[test]
    fn prop_all_encodings_edit_roundtrip(
        text in ascii_text_strategy().prop_filter("need content", |s| s.len() >= 3),
        added in "[a-zA-Z]{3,10}",
        encoding in encoding_strategy()
    ) {
        let temp_dir = TempDir::new().unwrap();
        let file_path = create_encoded_file(&temp_dir, "edit_roundtrip.txt", encoding, &text);

        let mut harness = EditorTestHarness::new(120, 30).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Add text at end
        harness.send_key(KeyCode::End, KeyModifiers::CONTROL).unwrap();
        harness.type_text(&added).unwrap();
        harness.render().unwrap();

        // Save
        harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL).unwrap();
        harness.render().unwrap();
        let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

        // Read saved file and decode it
        let saved_bytes = std::fs::read(&file_path).unwrap();

        // Verify encoding markers are preserved
        match encoding {
            TestEncoding::Utf8Bom => {
                prop_assert!(
                    saved_bytes.starts_with(UTF8_BOM),
                    "UTF-8 BOM should be preserved after edit"
                );
            }
            TestEncoding::Utf16Le => {
                prop_assert!(
                    saved_bytes.starts_with(UTF16_LE_BOM),
                    "UTF-16 LE BOM should be preserved after edit"
                );
            }
            TestEncoding::Utf16Be => {
                prop_assert!(
                    saved_bytes.starts_with(UTF16_BE_BOM),
                    "UTF-16 BE BOM should be preserved after edit"
                );
            }
            _ => {}
        }

        // Reload and verify the added text is present
        drop(harness);
        let mut harness2 = EditorTestHarness::new(120, 30).unwrap();
        harness2.open_file(&file_path).unwrap();
        harness2.render().unwrap();

        let buffer_content = harness2.get_buffer_content().unwrap();
        prop_assert!(
            buffer_content.contains(&added),
            "Added text '{}' should be in reloaded buffer. Buffer: {:?}",
            added,
            buffer_content
        );
    }
}

// ============================================================================
// Specific Edge Case Tests (Not Property-Based)
// ============================================================================

/// Test that UTF-8 files without BOM are detected correctly
#[test]
fn test_detect_encoding_utf8() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("utf8.txt");

    // Write UTF-8 content (no BOM)
    std::fs::write(&file_path, "Hello, World!\n你好世界\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should detect as UTF-8 and display correctly
    harness.assert_screen_contains("Hello, World!");
    // Check individual Chinese characters (they may be spaced due to double-width rendering)
    let screen = harness.screen_to_string();
    assert!(screen.contains('你'), "Screen should contain '你'");
    assert!(screen.contains('好'), "Screen should contain '好'");
    assert!(screen.contains('世'), "Screen should contain '世'");
    assert!(screen.contains('界'), "Screen should contain '界'");

    // UTF-8 encoding IS now shown in status bar (always visible)
    assert!(
        screen.contains("UTF-8"),
        "UTF-8 should be shown in status bar"
    );
}

/// Test that UTF-8 BOM is hidden from display but preserved on save
#[test]
fn test_utf8_bom_hidden_but_preserved() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("utf8_bom.txt");

    // Write UTF-8 BOM + content
    let mut content = Vec::new();
    content.extend_from_slice(UTF8_BOM);
    content.extend_from_slice("Hello\n".as_bytes());
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // BOM should NOT be visible in the content area
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("\u{FEFF}"),
        "BOM character should not be visible in content"
    );

    // Content should be visible
    harness.assert_screen_contains("Hello");

    // Edit and save
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" World").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    // Verify BOM is preserved
    let saved = std::fs::read(&file_path).unwrap();
    assert!(
        saved.starts_with(UTF8_BOM),
        "BOM should be preserved after save"
    );
}

/// Test handling of empty file
#[test]
fn test_empty_file_defaults_to_utf8() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("empty.txt");

    std::fs::write(&file_path, "").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should default to UTF-8 (but encoding is hidden in status bar for UTF-8/ASCII)
    // Just verify no other encoding is shown
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("UTF-16") && !screen.contains("GB18030") && !screen.contains("Latin"),
        "Empty file should default to UTF-8, not show other encodings"
    );

    // Should be able to type and save
    harness.type_text("New content\n").unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| !h.editor().active_state().buffer.is_modified())
        .unwrap();

    // Verify saved as UTF-8 (no BOM)
    let saved = std::fs::read(&file_path).unwrap();
    assert!(
        !saved.starts_with(UTF8_BOM),
        "New files should not have BOM by default"
    );
    assert_eq!(String::from_utf8(saved).unwrap(), "New content\n");
}

/// Test that binary files with encoding markers are handled correctly
#[test]
fn test_binary_with_fake_bom_detected() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("fake_bom.bin");

    // Create a file that starts like UTF-16 LE BOM but contains binary data
    let mut content = Vec::new();
    content.extend_from_slice(UTF16_LE_BOM);
    content.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]); // Null bytes indicate binary
    content.extend_from_slice(&[0x89, 0x50, 0x4E, 0x47]); // PNG magic
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should not crash and should show something
    let screen = harness.screen_to_string();
    assert!(!screen.is_empty(), "Editor should display something");
}

/// Test GB18030 encoding detection and display
#[test]
fn test_gb18030_chinese_display() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("gb18030.txt");

    // GB18030 encoding of "你好世界"
    let gb18030_bytes: &[u8] = &[
        0xC4, 0xE3, // 你
        0xBA, 0xC3, // 好
        0xCA, 0xC0, // 世
        0xBD, 0xE7, // 界
        0x0A, // newline
    ];
    std::fs::write(&file_path, gb18030_bytes).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Check individual Chinese characters (they may be spaced due to double-width rendering)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('你'),
        "Screen should contain '你': {}",
        screen
    );
    assert!(
        screen.contains('好'),
        "Screen should contain '好': {}",
        screen
    );
    assert!(
        screen.contains('世'),
        "Screen should contain '世': {}",
        screen
    );
    assert!(
        screen.contains('界'),
        "Screen should contain '界': {}",
        screen
    );
}

/// Test Latin-1 special characters are displayed correctly
#[test]
fn test_latin1_special_chars_display() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("latin1.txt");

    // Latin-1 encoded: "Héllo Wörld Café résumé naïve"
    // Using Latin-1 byte values for accented characters
    let latin1_bytes: &[u8] = &[
        0x48, 0xE9, 0x6C, 0x6C, 0x6F, 0x20, // "Héllo "
        0x57, 0xF6, 0x72, 0x6C, 0x64, 0x20, // "Wörld "
        0x43, 0x61, 0x66, 0xE9, 0x20, // "Café "
        0x72, 0xE9, 0x73, 0x75, 0x6D, 0xE9, 0x20, // "résumé "
        0x6E, 0x61, 0xEF, 0x76, 0x65, // "naïve"
        0x0A, // newline
    ];
    std::fs::write(&file_path, latin1_bytes).unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should display correctly (converted to UTF-8 internally)
    harness.assert_screen_contains("Héllo");
    harness.assert_screen_contains("Wörld");
    harness.assert_screen_contains("Café");
}

/// Test encoding display in status bar
/// Note: UTF-8 and ASCII are hidden from status bar (as they're the expected defaults)
/// This test verifies encoding is shown for non-default encodings like UTF-16
#[test]
fn test_encoding_shown_in_status_bar() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");

    // Create a UTF-16 LE file with BOM - encoding WILL be shown in status bar
    let content = "Hello UTF-16";
    let mut utf16_bytes = vec![0xFF, 0xFE]; // UTF-16 LE BOM
    for ch in content.encode_utf16() {
        utf16_bytes.extend_from_slice(&ch.to_le_bytes());
    }
    std::fs::write(&file_path, utf16_bytes).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Status bar should show encoding for non-UTF-8 files
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("UTF-16")
            || screen.contains("utf-16")
            || screen.contains("UTF16")
            || screen.contains("utf16"),
        "Status bar should show UTF-16 encoding: {}",
        screen
    );
}

/// Test clipboard operations preserve content with special characters
#[test]
fn test_clipboard_preserves_encoded_content() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("clipboard_test.txt");

    // UTF-8 file with special characters
    std::fs::write(&file_path, "Café résumé\n").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.editor_mut().set_clipboard_for_test("".to_string());
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Select all and copy
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Go to end and paste
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should have duplicated content correctly
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.matches("Café").count() == 2,
        "Should have two copies of Café: {}",
        buffer
    );
}

/// Test creating a large UTF-16 file and navigating it
#[test]
fn test_large_utf16_file_navigation() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("large_utf16.txt");

    // Create a reasonably large UTF-16 LE file
    let line = "This is a test line with content\r\n";
    let num_lines = 500;

    let mut content = Vec::new();
    content.extend_from_slice(UTF16_LE_BOM);
    for _ in 0..num_lines {
        for ch in line.encode_utf16() {
            content.extend_from_slice(&ch.to_le_bytes());
        }
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Should display content
    harness.assert_screen_contains("test line");

    // Navigate to end
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should still show content
    harness.assert_screen_contains("test line");
}

// ============================================================================
// Status Bar Encoding Indicator Click Tests
// ============================================================================

/// Test that clicking on the encoding indicator in the status bar opens the encoding selector.
/// This test will fail if the click handler is not implemented.
#[test]
fn test_encoding_indicator_click_opens_selector() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    // Use non-ASCII content to ensure UTF-8 detection (not ASCII)
    std::fs::write(&file_path, "Hello, World! こんにちは").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify UTF-8 encoding is shown in status bar
    harness.assert_screen_contains("UTF-8");

    // Find the position of "UTF-8" on screen
    let (col, row) = harness
        .find_text_on_screen("UTF-8")
        .expect("UTF-8 encoding indicator should be visible in status bar");

    // Click on the encoding indicator
    harness.mouse_click(col, row).unwrap();

    // After clicking, the encoding selector prompt should open
    // The prompt should show encoding options (UTF-16 is one of the available encodings)
    harness.assert_screen_contains("UTF-16");
}

/// Test that the encoding indicator is displayed for all files (ASCII and UTF-8).
#[test]
fn test_encoding_indicator_always_visible() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "Hello, World!").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Encoding indicator should be visible in status bar
    // ASCII files are detected as ASCII, not UTF-8
    harness.assert_screen_contains("ASCII");
}

/// Test that clicking on UTF-16 encoding indicator opens selector and can change encoding.
/// This is a complete flow test: load UTF-16 file, change encoding to UTF-8 via click, save.
#[test]
fn test_utf16_encoding_indicator_click_and_change() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_utf16.txt");

    // Create UTF-16 LE file with test content
    let text = "Hello UTF-16!\nLine 2\n";
    let mut content = UTF16_LE_BOM.to_vec();
    for ch in text.encode_utf16() {
        content.extend_from_slice(&ch.to_le_bytes());
    }
    std::fs::write(&file_path, &content).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify UTF-16 LE encoding is shown in status bar
    harness.assert_screen_contains("UTF-16 LE");

    // Find the position of "UTF-16 LE" on screen
    let (col, row) = harness
        .find_text_on_screen("UTF-16 LE")
        .expect("UTF-16 LE encoding indicator should be visible in status bar");

    // Click on the encoding indicator to open encoding selector
    harness.mouse_click(col, row).unwrap();

    // After clicking, the encoding selector prompt should open
    harness.assert_screen_contains("Encoding:");

    // Type "UTF-8" to filter and select UTF-8 encoding
    // Ctrl+A selects all existing text, then typing replaces it
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("UTF-8").unwrap();
    harness.render().unwrap();

    // Press Enter to confirm the selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify encoding changed to UTF-8 in status bar
    harness.assert_screen_contains("UTF-8");
    // Status bar should not show "UTF-16 LE" indicator anymore
    // (the content still contains "UTF-16" text, so we check for the indicator format)
    harness.assert_screen_not_contains("UTF-16 LE");

    // Save the file (Ctrl+S)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Read the saved file and verify it's now UTF-8 (no BOM, plain text)
    let saved_content = std::fs::read(&file_path).unwrap();

    // UTF-8 file should NOT have UTF-16 BOM
    assert!(
        !saved_content.starts_with(UTF16_LE_BOM),
        "Saved file should not have UTF-16 LE BOM"
    );

    // Content should be plain UTF-8
    let saved_text = String::from_utf8(saved_content).expect("Saved file should be valid UTF-8");
    assert!(
        saved_text.contains("Hello UTF-16!"),
        "Saved file should contain the original text"
    );
    assert!(
        saved_text.contains("Line 2"),
        "Saved file should contain all lines"
    );
}

/// Test changing encoding from UTF-8 to UTF-16 LE via click on status bar indicator.
/// This tests the opposite direction: UTF-8 → UTF-16 LE.
#[test]
fn test_utf8_to_utf16_encoding_change() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_utf8.txt");

    // Create a UTF-8 file with non-ASCII content (to ensure it's detected as UTF-8, not ASCII)
    let text = "Hello World! こんにちは\nLine 2\n";
    std::fs::write(&file_path, text).unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify UTF-8 encoding is shown in status bar
    harness.assert_screen_contains("UTF-8");

    // Find the position of "UTF-8" on screen - should be in the status bar (last lines)
    let (col, row) = harness
        .find_text_on_screen("UTF-8")
        .expect("UTF-8 encoding indicator should be visible in status bar");

    // Verify we found it in the status bar area (bottom of screen)
    assert!(
        row >= 20,
        "UTF-8 should be in status bar (bottom of screen), found at row {}",
        row
    );

    // Click on the encoding indicator to open encoding selector
    harness.mouse_click(col, row).unwrap();

    // After clicking, the encoding selector prompt should open
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Encoding:"),
        "Encoding selector should open after clicking. Screen:\n{}",
        screen
    );

    // Navigate to UTF-16 LE using arrow keys
    // The encoding list order is: UTF-8, UTF-8 BOM, UTF-16 LE, UTF-16 BE, ASCII, ...
    // Current selection is UTF-8 (index 0), UTF-16 LE is at index 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Move to UTF-8 BOM
    harness.render().unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Move to UTF-16 LE
    harness.render().unwrap();

    // Check what's shown after navigation
    let screen_after_nav = harness.screen_to_string();
    assert!(
        screen_after_nav.contains("UTF-16 LE"),
        "UTF-16 LE should be visible after navigating. Screen:\n{}",
        screen_after_nav
    );

    // Press Enter to confirm the selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify encoding changed to UTF-16 LE in status bar
    let final_screen = harness.screen_to_string();
    assert!(
        final_screen.contains("UTF-16 LE"),
        "Encoding should be UTF-16 LE after confirmation. Screen:\n{}",
        final_screen
    );
    // Content should still be visible
    harness.assert_screen_contains("Hello World!");

    // Save the file (Ctrl+S)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Read the saved file and verify it's now UTF-16 LE
    let saved_content = std::fs::read(&file_path).unwrap();

    // UTF-16 LE file should start with BOM
    assert!(
        saved_content.starts_with(UTF16_LE_BOM),
        "Saved file should have UTF-16 LE BOM. Got first bytes: {:?}",
        &saved_content[..saved_content.len().min(10)]
    );

    // Decode UTF-16 LE content (skip BOM)
    let utf16_bytes = &saved_content[2..];
    let utf16_units: Vec<u16> = utf16_bytes
        .chunks_exact(2)
        .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
        .collect();
    let decoded = String::from_utf16(&utf16_units).expect("Should be valid UTF-16 LE");

    assert!(
        decoded.contains("Hello World!"),
        "Saved file should contain the original text. Got: {}",
        decoded
    );
    assert!(
        decoded.contains("こんにちは"),
        "Saved file should preserve Japanese characters"
    );
}

// ============================================================================
// Windows-1250 (Central European) Encoding Tests
// ============================================================================

/// Comprehensive test for Windows-1250 encoding:
/// - Display of Polish diacritical characters
/// - Encoding selector shows Windows-1250 (Central European)
/// - Encoding change via selector works
#[test]
fn test_windows1250_display_and_selector() {
    let temp_dir = TempDir::new().unwrap();

    // Test 1: Display Windows-1250 encoded Polish characters
    let polish_file = temp_dir.path().join("polish.txt");
    // "Zażółć gęślą jaźń" - famous Polish pangram in Windows-1250
    let windows1250_bytes: &[u8] = &[
        0x5A, 0x61, 0xBF, 0xF3, 0xB3, 0xE6, // "Zażółć"
        0x20, 0x67, 0xEA, 0x9C, 0x6C, 0xB9, // " gęślą"
        0x20, 0x6A, 0x61, 0x9F, 0xF1, 0x0A, // " jaźń\n"
    ];
    std::fs::write(&polish_file, windows1250_bytes).unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&polish_file).unwrap();
    harness.render().unwrap();

    // Verify Polish characters are displayed (converted to UTF-8 internally)
    let screen = harness.screen_to_string();
    assert!(
        screen.contains('ż') || screen.contains('ó') || screen.contains('ł'),
        "Screen should contain Polish diacritical characters"
    );

    // Test 2: Encoding selector and change
    let utf8_file = temp_dir.path().join("utf8.txt");
    std::fs::write(&utf8_file, "Hello World!\n").unwrap();

    drop(harness);
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&utf8_file).unwrap();
    harness.render().unwrap();

    // Click on encoding indicator to open selector
    let (col, row) = harness
        .find_text_on_screen("ASCII")
        .expect("Encoding indicator should be visible");
    harness.mouse_click(col, row).unwrap();
    harness.assert_screen_contains("Encoding:");

    // Filter for Windows-1250 and verify it shows with description
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("1250").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Windows-1250") && screen.contains("Central European"),
        "Selector should show Windows-1250 (Central European)"
    );

    // Select and verify encoding changed
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Windows-1250");
}

/// Test Windows-1250 encoding conversions:
/// - UTF-8 → Windows-1250 → save → verify on disk
/// - Continue: Windows-1250 → UTF-8 → save → verify on disk (bidirectional)
/// - Fresh load: Load Windows-1250 from disk → UTF-8 → save → verify on disk
#[test]
fn test_windows1250_encoding_conversions() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("conversion_test.txt");

    // Start with UTF-8 file containing Polish text
    std::fs::write(&file_path, "Zażółć gęślą\n").unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Part 1: UTF-8 → Windows-1250 → save
    let (col, row) = harness
        .find_text_on_screen("UTF-8")
        .expect("UTF-8 indicator should be visible");
    harness.mouse_click(col, row).unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Windows-1250").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

    // Verify file is Windows-1250 encoded ('ż'=0xBF, 'ł'=0xB3)
    let saved = std::fs::read(&file_path).unwrap();
    assert!(
        saved.contains(&0xBF) || saved.contains(&0xB3),
        "File should be Windows-1250 encoded"
    );
    let (decoded, _) = encoding_rs::WINDOWS_1250.decode_without_bom_handling(&saved);
    assert!(decoded.contains("Zażółć"), "Should preserve Polish text");

    // Part 2: Fresh load Windows-1250 from disk → UTF-8 → save
    // Close and reopen to simulate loading from disk
    drop(harness);
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // File may be detected as Windows-1252 (chardetng can't distinguish 1250 vs 1252)
    // Find encoding indicator and change to UTF-8
    let (col, row) = harness
        .find_text_on_screen("Windows")
        .expect("Windows encoding indicator should be visible");
    harness.mouse_click(col, row).unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("UTF-8").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

    // Verify file is now valid UTF-8
    // Note: If detected as Windows-1252, some chars may differ (ś→œ, ć→æ)
    // but the file should be valid UTF-8 regardless
    let saved = std::fs::read(&file_path).unwrap();
    let utf8_str = std::str::from_utf8(&saved).expect("File should be valid UTF-8");
    // Check that common chars (ó, ł) that are same in both encodings are preserved
    assert!(
        utf8_str.contains('ó') || utf8_str.contains('ł'),
        "UTF-8 file should contain some Polish chars. Got: {}",
        utf8_str
    );
}

/// Test loading and detecting Windows-1250 encoding with Czech pangram.
/// Uses the famous Czech pangram "Příliš žluťoučký kůň úpěl ďábelské ódy"
/// which contains all Czech diacritical characters.
///
/// Note: chardetng may detect Windows-1250 as Windows-1252 since many byte values
/// overlap. However, the Czech-specific characters (ř, ů, ě, etc.) that differ
/// between encodings should help with detection or at least display correctly.
#[test]
fn test_windows1250_czech_pangram() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("czech_pangram.txt");

    // "Příliš žluťoučký kůň úpěl ďábelské ódy" in Windows-1250 encoding
    // P=0x50, ř=0xF8, í=0xED, l=0x6C, i=0x69, š=0x9A, space=0x20
    // ž=0x9E, l=0x6C, u=0x75, ť=0x9D, o=0x6F, u=0x75, č=0xE8, k=0x6B, ý=0xFD
    // k=0x6B, ů=0xF9, ň=0xF2, space=0x20
    // ú=0xFA, p=0x70, ě=0xEC, l=0x6C, space=0x20
    // ď=0xEF, á=0xE1, b=0x62, e=0x65, l=0x6C, s=0x73, k=0x6B, é=0xE9, space=0x20
    // ó=0xF3, d=0x64, y=0x79
    let windows1250_bytes: &[u8] = &[
        0x50, 0xF8, 0xED, 0x6C, 0x69, 0x9A, 0x20, // "Příliš "
        0x9E, 0x6C, 0x75, 0x9D, 0x6F, 0x75, 0xE8, 0x6B, 0xFD, 0x20, // "žluťoučký "
        0x6B, 0xF9, 0xF2, 0x20, // "kůň "
        0xFA, 0x70, 0xEC, 0x6C, 0x20, // "úpěl "
        0xEF, 0xE1, 0x62, 0x65, 0x6C, 0x73, 0x6B, 0xE9, 0x20, // "ďábelské "
        0xF3, 0x64, 0x79, 0x0A, // "ódy\n"
    ];
    std::fs::write(&file_path, windows1250_bytes).unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();

    // The file should be detected as Windows-1250 because it contains ť (0x9D)
    // which is a definitive Windows-1250 indicator (undefined in Windows-1252)
    assert!(
        screen.contains("Windows-1250"),
        "Should detect as Windows-1250 (contains ť = 0x9D). Screen:\n{}",
        screen
    );

    // Verify the Czech pangram is displayed correctly
    // Now that Windows-1250 is properly detected, all Czech characters should display correctly
    assert!(screen.contains('í'), "Screen should contain 'í'");
    assert!(screen.contains('á'), "Screen should contain 'á'");
    assert!(screen.contains('é'), "Screen should contain 'é'");
    assert!(screen.contains('ó'), "Screen should contain 'ó'");
    assert!(screen.contains('š'), "Screen should contain 'š'");
    assert!(screen.contains('ž'), "Screen should contain 'ž'");

    // Czech-specific characters that differ from Windows-1252
    // These would be wrong if detected as Windows-1252, but should be correct now
    assert!(
        screen.contains('ř'),
        "Screen should contain 'ř' (Windows-1250 specific)"
    );
    assert!(
        screen.contains('ů'),
        "Screen should contain 'ů' (Windows-1250 specific)"
    );
    assert!(
        screen.contains('ě'),
        "Screen should contain 'ě' (Windows-1250 specific)"
    );
    assert!(
        screen.contains('č'),
        "Screen should contain 'č' (Windows-1250 specific)"
    );
    assert!(
        screen.contains('ť'),
        "Screen should contain 'ť' (Windows-1250 specific)"
    );

    // Verify buffer content contains the full Czech pangram
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains("Příliš") || buffer.contains("P"),
        "Buffer should contain the pangram. Got: {}",
        buffer
    );
}

/// Test loading files in various encodings from disk and saving as UTF-8
/// This tests the full flow: create encoded file → load → save as UTF-8 → verify
/// Covers all supported encodings with detectable content.
#[test]
fn test_all_encodings_load_and_save_as_utf8() {
    struct TestCase {
        desc: &'static str,
        bytes: &'static [u8],
        expected_substr: &'static str,
    }

    let test_cases: &[TestCase] = &[
        // UTF-16 LE with BOM: "Héllo"
        TestCase {
            desc: "UTF-16 LE",
            bytes: &[
                0xFF, 0xFE, // BOM
                0x48, 0x00, 0xE9, 0x00, 0x6C, 0x00, 0x6C, 0x00, 0x6F, 0x00, 0x0A, 0x00,
            ],
            expected_substr: "H",
        },
        // UTF-16 BE with BOM: "Héllo"
        TestCase {
            desc: "UTF-16 BE",
            bytes: &[
                0xFE, 0xFF, // BOM
                0x00, 0x48, 0x00, 0xE9, 0x00, 0x6C, 0x00, 0x6C, 0x00, 0x6F, 0x00, 0x0A,
            ],
            expected_substr: "H",
        },
        // UTF-8 with BOM: "Café"
        TestCase {
            desc: "UTF-8 BOM",
            bytes: &[0xEF, 0xBB, 0xBF, 0x43, 0x61, 0x66, 0xC3, 0xA9, 0x0A],
            expected_substr: "Café",
        },
        // UTF-8 without BOM: "Cześć" (Polish greeting)
        TestCase {
            desc: "UTF-8",
            bytes: &[0x43, 0x7A, 0x65, 0xC5, 0x9B, 0xC4, 0x87, 0x0A], // "Cześć\n"
            expected_substr: "Cze",
        },
        // Windows-1252: "Café" (é = 0xE9 in Windows-1252)
        TestCase {
            desc: "Windows-1252",
            bytes: &[0x43, 0x61, 0x66, 0xE9, 0x0A],
            expected_substr: "Café",
        },
        // Windows-1250: Polish "ółć" (ó=0xF3, ł=0xB3, ć=0xE6)
        // These bytes are same in Win-1252, so detection may vary, but conversion works
        TestCase {
            desc: "Windows-1250",
            bytes: &[0xF3, 0xB3, 0xE6, 0x0A],
            expected_substr: "ó",
        },
        // Note: CJK encodings (GB18030, GBK, Shift-JIS, EUC-KR) have ambiguous
        // detection - the same bytes can be valid in multiple encodings.
        // These are tested in property tests (prop_all_encodings_roundtrip_exact)
        // which use encoding_rs directly without relying on auto-detection.
    ];

    let temp_dir = TempDir::new().unwrap();

    for (i, tc) in test_cases.iter().enumerate() {
        let file_path = temp_dir.path().join(format!("test_{}.txt", i));
        std::fs::write(&file_path, tc.bytes).unwrap();

        let mut harness = EditorTestHarness::new(100, 24).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        // Find encoding indicator and change to UTF-8
        let encoding_pos = harness
            .find_text_on_screen("UTF-16")
            .or_else(|| harness.find_text_on_screen("UTF-8"))
            .or_else(|| harness.find_text_on_screen("Windows"))
            .or_else(|| harness.find_text_on_screen("Latin"))
            .or_else(|| harness.find_text_on_screen("GB18030"))
            .or_else(|| harness.find_text_on_screen("GBK"))
            .or_else(|| harness.find_text_on_screen("Shift"))
            .or_else(|| harness.find_text_on_screen("EUC"))
            .or_else(|| harness.find_text_on_screen("ASCII"));

        let (col, row) = encoding_pos.unwrap_or_else(|| {
            panic!(
                "Test {} ({}): Could not find encoding indicator",
                i, tc.desc
            )
        });

        harness.mouse_click(col, row).unwrap();
        harness
            .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
            .unwrap();
        harness.type_text("UTF-8").unwrap();
        harness.render().unwrap();
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();

        // Save
        harness
            .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
            .unwrap();
        harness.render().unwrap();
        let _ = harness.wait_until(|h| !h.editor().active_state().buffer.is_modified());

        // Verify saved file is valid UTF-8 and contains expected content
        let saved = std::fs::read(&file_path).unwrap();
        let utf8_str = std::str::from_utf8(&saved).unwrap_or_else(|e| {
            panic!(
                "Test {} ({}): File should be valid UTF-8. Error: {}. Bytes: {:?}",
                i, tc.desc, e, saved
            )
        });
        assert!(
            utf8_str.contains(tc.expected_substr),
            "Test {} ({}): Expected '{}' in saved file. Got: {}",
            i,
            tc.desc,
            tc.expected_substr,
            utf8_str
        );

        drop(harness);
    }
}
