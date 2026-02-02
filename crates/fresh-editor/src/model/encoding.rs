//! Text encoding detection and conversion
//!
//! This module handles:
//! - Detecting text encodings from byte content (UTF-8, UTF-16, Latin-1, CJK, etc.)
//! - Binary file detection (distinguishing text from binary content)
//! - Converting between encodings (normalizing to UTF-8 on load, converting back on save)
//!
//! # Encoding Detection Strategy
//!
//! 1. **BOM Detection**: Check for Byte Order Marks (UTF-8 BOM, UTF-16 LE/BE)
//! 2. **UTF-8 Validation**: Fast path for most modern files
//! 3. **UTF-16 Heuristics**: Detect UTF-16 without BOM via null byte patterns
//! 4. **Binary Detection**: Check for control characters that indicate binary content
//! 5. **Statistical Detection**: Use chardetng for legacy encoding detection
//! 6. **Fallback**: Default to Windows-1252 for ambiguous cases

use super::encoding_heuristics::has_windows1250_pattern;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// ============================================================================
// Encoding Type
// ============================================================================

/// Supported text encodings for file I/O
///
/// The editor internally uses UTF-8 for all text processing. When loading files,
/// content is converted from the detected encoding to UTF-8. When saving, content
/// is converted back to the original (or user-selected) encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize, JsonSchema)]
pub enum Encoding {
    /// UTF-8 (default, most common)
    #[default]
    Utf8,
    /// UTF-8 with Byte Order Mark
    Utf8Bom,
    /// UTF-16 Little Endian (Windows default for Unicode files)
    Utf16Le,
    /// UTF-16 Big Endian
    Utf16Be,
    /// ASCII (7-bit, subset of UTF-8)
    Ascii,
    /// Latin-1 / ISO-8859-1 (Western European)
    Latin1,
    /// Windows-1252 / CP-1252 (Windows Western European, often called "ANSI")
    Windows1252,
    /// Windows-1250 / CP-1250 (Windows Central European)
    Windows1250,
    /// GB18030 (Chinese, superset of GBK)
    Gb18030,
    /// GBK (Chinese Simplified, subset of GB18030)
    Gbk,
    /// Shift-JIS (Japanese)
    ShiftJis,
    /// EUC-KR (Korean)
    EucKr,
}

impl Encoding {
    /// Get the display name for status bar
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Utf8 => "UTF-8",
            Self::Utf8Bom => "UTF-8 BOM",
            Self::Utf16Le => "UTF-16 LE",
            Self::Utf16Be => "UTF-16 BE",
            Self::Ascii => "ASCII",
            Self::Latin1 => "Latin-1",
            Self::Windows1252 => "Windows-1252",
            Self::Windows1250 => "Windows-1250",
            Self::Gb18030 => "GB18030",
            Self::Gbk => "GBK",
            Self::ShiftJis => "Shift-JIS",
            Self::EucKr => "EUC-KR",
        }
    }

    /// Get a longer description for UI (e.g., command palette)
    pub fn description(&self) -> &'static str {
        match self {
            Self::Utf8 => "UTF-8 (Unicode)",
            Self::Utf8Bom => "UTF-8 with BOM",
            Self::Utf16Le => "UTF-16 LE (Windows Unicode)",
            Self::Utf16Be => "UTF-16 BE",
            Self::Ascii => "ASCII (7-bit)",
            Self::Latin1 => "Latin-1 / ISO-8859-1 (Western European)",
            Self::Windows1252 => "Windows-1252 / ANSI (Western European)",
            Self::Windows1250 => "Windows-1250 (Central European)",
            Self::Gb18030 => "GB18030 (Chinese)",
            Self::Gbk => "GBK (Chinese Simplified)",
            Self::ShiftJis => "Shift-JIS (Japanese)",
            Self::EucKr => "EUC-KR (Korean)",
        }
    }

    /// Get the encoding_rs Encoding for this type
    pub fn to_encoding_rs(&self) -> &'static encoding_rs::Encoding {
        match self {
            Self::Utf8 | Self::Utf8Bom | Self::Ascii => encoding_rs::UTF_8,
            Self::Utf16Le => encoding_rs::UTF_16LE,
            Self::Utf16Be => encoding_rs::UTF_16BE,
            Self::Latin1 => encoding_rs::WINDOWS_1252, // ISO-8859-1 maps to Windows-1252 per WHATWG
            Self::Windows1252 => encoding_rs::WINDOWS_1252,
            Self::Windows1250 => encoding_rs::WINDOWS_1250,
            Self::Gb18030 => encoding_rs::GB18030,
            Self::Gbk => encoding_rs::GBK,
            Self::ShiftJis => encoding_rs::SHIFT_JIS,
            Self::EucKr => encoding_rs::EUC_KR,
        }
    }

    /// Returns true if this encoding uses a BOM (Byte Order Mark)
    pub fn has_bom(&self) -> bool {
        matches!(self, Self::Utf8Bom | Self::Utf16Le | Self::Utf16Be)
    }

    /// Get the BOM bytes for this encoding (if any)
    pub fn bom_bytes(&self) -> Option<&'static [u8]> {
        match self {
            Self::Utf8Bom => Some(&[0xEF, 0xBB, 0xBF]),
            Self::Utf16Le => Some(&[0xFF, 0xFE]),
            Self::Utf16Be => Some(&[0xFE, 0xFF]),
            _ => None,
        }
    }

    /// All available encodings for UI display
    pub fn all() -> &'static [Encoding] {
        &[
            Self::Utf8,
            Self::Utf8Bom,
            Self::Utf16Le,
            Self::Utf16Be,
            Self::Ascii,
            Self::Latin1,
            Self::Windows1252,
            Self::Windows1250,
            Self::Gb18030,
            Self::Gbk,
            Self::ShiftJis,
            Self::EucKr,
        ]
    }

    /// Returns true if this encoding supports "resynchronization" - the ability to
    /// find character boundaries when jumping into the middle of a file.
    ///
    /// Resynchronizable encodings can be safely used with lazy/streaming file loading
    /// because you can determine character boundaries from any position.
    ///
    /// - **UTF-8**: Excellent - unique bit patterns distinguish lead/continuation bytes
    /// - **ASCII/Latin-1/Windows-1252**: Trivial - every byte is a character
    /// - **UTF-16**: Good with 2-byte alignment - can detect surrogate pairs
    /// - **UTF-32**: Good with 4-byte alignment
    ///
    /// Non-resynchronizable encodings (legacy CJK like Shift-JIS, GB18030, GBK, Big5)
    /// have ambiguous byte sequences where a byte could be either a standalone character
    /// or part of a multi-byte sequence. You must scan from the beginning to be certain.
    pub fn is_resynchronizable(&self) -> bool {
        match self {
            // Fixed-width single byte - every byte is a character
            Self::Ascii | Self::Latin1 | Self::Windows1252 | Self::Windows1250 => true,

            // UTF-8 has unique bit patterns for lead vs continuation bytes
            Self::Utf8 | Self::Utf8Bom => true,

            // UTF-16 is resynchronizable with 2-byte alignment
            // (can detect surrogate pairs by checking 0xD800-0xDFFF range)
            Self::Utf16Le | Self::Utf16Be => true,

            // Legacy CJK encodings are NOT resynchronizable
            // The second byte of a double-byte char can equal a valid single-byte char
            Self::Gb18030 | Self::Gbk | Self::ShiftJis | Self::EucKr => false,
        }
    }

    /// Returns the byte alignment required for this encoding when doing random access.
    ///
    /// For lazy loading of large files, reads must be aligned to this boundary.
    /// Returns None if the encoding is not resynchronizable (requires full file scan).
    pub fn alignment(&self) -> Option<usize> {
        match self {
            // Single-byte encodings - no alignment needed
            Self::Ascii | Self::Latin1 | Self::Windows1252 | Self::Windows1250 => Some(1),

            // UTF-8 - no alignment needed (self-synchronizing)
            Self::Utf8 | Self::Utf8Bom => Some(1),

            // UTF-16 - must be 2-byte aligned
            Self::Utf16Le | Self::Utf16Be => Some(2),

            // Legacy CJK - not resynchronizable, no valid alignment
            Self::Gb18030 | Self::Gbk | Self::ShiftJis | Self::EucKr => None,
        }
    }

    /// Returns true if this encoding requires the entire file to be loaded
    /// for correct decoding (cannot use lazy/streaming loading).
    ///
    /// This is the inverse of `is_resynchronizable()` and indicates that
    /// the user should be warned before loading large files in this encoding.
    pub fn requires_full_file_load(&self) -> bool {
        !self.is_resynchronizable()
    }
}

// ============================================================================
// Encoding Detection
// ============================================================================

/// Detect the text encoding from a sample of bytes
///
/// This function delegates to `detect_encoding_or_binary` and returns only
/// the encoding, ignoring the binary flag. Use `detect_encoding_or_binary`
/// when you need to know if the content should be treated as binary.
pub fn detect_encoding(bytes: &[u8]) -> Encoding {
    detect_encoding_or_binary(bytes).0
}

/// Detect the text encoding and whether content is binary.
///
/// Returns (Encoding, is_binary) where:
/// - Encoding is the detected encoding (or default if binary)
/// - is_binary is true if the content should be treated as raw binary
///
/// # Detection Strategy
///
/// 1. Check for BOM (Byte Order Mark) - highest priority, definitely not binary
/// 2. Try UTF-8 validation (fast path for most files), definitely not binary
/// 3. Check for UTF-16 patterns without BOM, definitely not binary
/// 4. Check for binary control characters (null bytes, etc.) - if found, it's binary
/// 5. Use chardetng for statistical detection of legacy encodings
/// 6. If encoding detection is uncertain, default to Windows-1252
pub fn detect_encoding_or_binary(bytes: &[u8]) -> (Encoding, bool) {
    // Only check the first 8KB for encoding detection
    let check_len = bytes.len().min(8 * 1024);
    let sample = &bytes[..check_len];

    // 1. Check for BOM (Byte Order Mark) - highest priority, definitely text
    if sample.starts_with(&[0xEF, 0xBB, 0xBF]) {
        return (Encoding::Utf8Bom, false);
    }
    if sample.starts_with(&[0xFF, 0xFE]) {
        // Could also be UTF-32 LE, but UTF-16 LE is much more common
        return (Encoding::Utf16Le, false);
    }
    if sample.starts_with(&[0xFE, 0xFF]) {
        return (Encoding::Utf16Be, false);
    }

    // 2. Try UTF-8 validation (fast path for most modern files)
    if std::str::from_utf8(sample).is_ok() {
        // Check if it's pure ASCII (subset of UTF-8)
        // Also check for binary indicators in valid ASCII/UTF-8
        let has_binary_control = sample.iter().any(|&b| is_binary_control_char(b));
        if has_binary_control {
            return (Encoding::Utf8, true);
        }
        if sample.iter().all(|&b| b < 128) {
            return (Encoding::Ascii, false);
        }
        return (Encoding::Utf8, false);
    }

    // 3. Check for UTF-16 without BOM (common in some Windows files)
    // Heuristic: Look for patterns of null bytes alternating with printable chars
    // The non-null byte should be printable (0x20-0x7E) or a valid high byte
    if sample.len() >= 4 {
        let is_printable_or_high = |b: u8| (0x20..=0x7E).contains(&b) || b >= 0x80;

        let le_pairs = sample
            .chunks(2)
            .filter(|chunk| chunk.len() == 2 && chunk[1] == 0 && is_printable_or_high(chunk[0]))
            .count();
        let be_pairs = sample
            .chunks(2)
            .filter(|chunk| chunk.len() == 2 && chunk[0] == 0 && is_printable_or_high(chunk[1]))
            .count();
        let pair_count = sample.len() / 2;

        // If more than 50% of pairs look like valid UTF-16 text, it's text
        if le_pairs > pair_count / 2 {
            return (Encoding::Utf16Le, false);
        }
        if be_pairs > pair_count / 2 {
            return (Encoding::Utf16Be, false);
        }
    }

    // 4. Check for binary indicators EARLY (before chardetng)
    // Binary files often contain control characters and null bytes that should not
    // appear in any valid text encoding. Check this before chardetng because
    // chardetng might still be "confident" about some encoding for binary data.
    let has_binary_control = sample
        .iter()
        .any(|&b| b == 0x00 || is_binary_control_char(b));
    if has_binary_control {
        return (Encoding::Utf8, true);
    }

    // 5. Check for Latin-1 patterns: high bytes followed by invalid CJK trail bytes
    // In GB18030/GBK, trail bytes must be 0x40-0x7E or 0x80-0xFE
    // If a high byte is followed by a byte outside these ranges (e.g., space, newline,
    // punctuation < 0x40), it's likely Latin-1, not CJK
    let has_latin1_pattern = has_latin1_high_byte_pattern(sample);

    // Also check for bytes in CJK-only range (0x81-0x9F) which can only be CJK lead bytes
    let has_cjk_only_bytes = sample.iter().any(|&b| (0x81..0xA0).contains(&b));

    // 6. Use chardetng for statistical encoding detection
    let mut detector = chardetng::EncodingDetector::new();
    detector.feed(sample, true);
    let (detected_encoding, confident) = detector.guess_assess(None, true);

    // If chardetng is confident, use that encoding (not binary)
    if confident {
        let is_cjk_encoding = detected_encoding == encoding_rs::GB18030
            || detected_encoding == encoding_rs::GBK
            || detected_encoding == encoding_rs::SHIFT_JIS
            || detected_encoding == encoding_rs::EUC_KR;

        // For CJK encodings, prefer Windows-1252 if we have clear Latin-1 indicators:
        // - Space followed by high byte (0xA0-0xFF) is common in Latin-1 text
        //
        // If there are CJK-only bytes (0x81-0x9F), it's definitely CJK (not ambiguous).
        // If there are Latin-1 patterns (space + high byte), prefer Windows-1252.
        // Otherwise, trust chardetng's detection.
        if is_cjk_encoding && !has_cjk_only_bytes && has_latin1_pattern {
            return (Encoding::Windows1252, false);
        }

        let encoding = if detected_encoding == encoding_rs::GB18030 {
            Encoding::Gb18030
        } else if detected_encoding == encoding_rs::GBK {
            Encoding::Gbk
        } else if detected_encoding == encoding_rs::SHIFT_JIS {
            Encoding::ShiftJis
        } else if detected_encoding == encoding_rs::EUC_KR {
            Encoding::EucKr
        } else if detected_encoding == encoding_rs::WINDOWS_1252
            || detected_encoding == encoding_rs::WINDOWS_1250
        {
            // chardetng often returns Windows-1252 for Central European text
            // Check for Windows-1250 specific patterns
            if has_windows1250_pattern(sample) {
                Encoding::Windows1250
            } else {
                Encoding::Windows1252
            }
        } else if detected_encoding == encoding_rs::UTF_8 {
            // chardetng thinks it's UTF-8, but validation failed above
            // Could still be Windows-1250 if it has Central European patterns
            if has_windows1250_pattern(sample) {
                Encoding::Windows1250
            } else {
                Encoding::Windows1252
            }
        } else {
            // Unknown encoding - check for Windows-1250 patterns
            if has_windows1250_pattern(sample) {
                Encoding::Windows1250
            } else {
                Encoding::Windows1252
            }
        };
        return (encoding, false);
    }

    // 7. chardetng not confident, but no binary indicators - check for Windows-1250 patterns
    // We already checked for binary control chars earlier, so this is valid text
    if has_windows1250_pattern(sample) {
        (Encoding::Windows1250, false)
    } else {
        (Encoding::Windows1252, false)
    }
}

// ============================================================================
// Binary Detection Helpers
// ============================================================================

/// Check if a byte is a binary control character
///
/// Returns true for control characters that typically indicate binary content,
/// excluding common text control chars (tab, newline, CR, form feed, etc.)
pub fn is_binary_control_char(byte: u8) -> bool {
    if byte < 0x20 {
        // Allow common text control characters:
        // 0x09 = Tab, 0x0A = LF, 0x0D = CR, 0x0C = Form Feed, 0x0B = Vertical Tab, 0x1B = ESC
        !matches!(byte, 0x09 | 0x0A | 0x0D | 0x0C | 0x0B | 0x1B)
    } else if byte == 0x7F {
        // DEL character
        true
    } else {
        false
    }
}

/// Check if sample has Latin-1 patterns that cannot be valid CJK encoding
///
/// In GB18030/GBK, valid sequences are:
/// - ASCII bytes (0x00-0x7F) as standalone characters
/// - Lead byte (0x81-0xFE) + Trail byte (0x40-0x7E or 0x80-0xFE)
///
/// This function looks for patterns that indicate Latin-1:
/// 1. High bytes followed by invalid CJK trail bytes (space, newline, etc.)
/// 2. ASCII word followed by space followed by high byte (like "Hello é")
/// 3. High byte immediately after ASCII space (like " é")
fn has_latin1_high_byte_pattern(sample: &[u8]) -> bool {
    let mut latin1_indicators = 0;
    let mut i = 0;

    while i < sample.len() {
        let byte = sample[i];

        if byte < 0x80 {
            // ASCII byte
            // Check for pattern: space followed by high byte (0xA0-0xFF)
            // This is common in Latin-1 text like "Hello é" or "Café résumé"
            if byte == 0x20 && i + 1 < sample.len() {
                let next = sample[i + 1];
                // Space followed by Latin-1 extended char (not CJK-only lead byte)
                if next >= 0xA0 {
                    latin1_indicators += 1;
                }
            }
            i += 1;
            continue;
        }

        // High byte (0x80-0xFF) - could be Latin-1 or CJK lead byte
        if i + 1 < sample.len() {
            let next = sample[i + 1];

            // Check if this could be a valid CJK double-byte sequence
            let is_valid_cjk_lead = (0x81..=0xFE).contains(&byte);
            let is_valid_cjk_trail = (0x40..=0x7E).contains(&next) || (0x80..=0xFE).contains(&next);

            if is_valid_cjk_lead && is_valid_cjk_trail {
                // Valid CJK pair - skip both bytes
                i += 2;
                continue;
            }

            // Not a valid CJK pair - check for Latin-1 indicator
            // High byte followed by space, newline, or other low ASCII
            if byte >= 0xA0 && next < 0x40 {
                latin1_indicators += 1;
            }
        }

        i += 1;
    }

    // Latin-1 is likely if we have indicators
    latin1_indicators > 0
}

// ============================================================================
// Encoding Conversion
// ============================================================================

/// Detect encoding and convert bytes to UTF-8
///
/// Returns the detected encoding and the UTF-8 converted content.
/// This is the core function for normalizing file content to UTF-8 on load.
pub fn detect_and_convert(bytes: &[u8]) -> (Encoding, Vec<u8>) {
    if bytes.is_empty() {
        return (Encoding::Utf8, Vec::new());
    }

    let encoding = detect_encoding(bytes);

    // For UTF-8 (with or without BOM), we can use the content directly
    match encoding {
        Encoding::Utf8 | Encoding::Ascii => {
            // Already UTF-8, just clone
            (encoding, bytes.to_vec())
        }
        Encoding::Utf8Bom => {
            // Skip the BOM (3 bytes) and use the rest
            let content = if bytes.len() > 3 {
                bytes[3..].to_vec()
            } else {
                Vec::new()
            };
            (encoding, content)
        }
        Encoding::Utf16Le | Encoding::Utf16Be => {
            // Decode UTF-16 to UTF-8
            let enc_rs = encoding.to_encoding_rs();
            let start_offset =
                if bytes.starts_with(&[0xFF, 0xFE]) || bytes.starts_with(&[0xFE, 0xFF]) {
                    2 // Skip BOM
                } else {
                    0
                };
            let data = &bytes[start_offset..];

            let (cow, _had_errors) = enc_rs.decode_without_bom_handling(data);
            (encoding, cow.into_owned().into_bytes())
        }
        _ => {
            // Use encoding_rs to convert to UTF-8
            let enc_rs = encoding.to_encoding_rs();
            let (cow, _had_errors) = enc_rs.decode_without_bom_handling(bytes);
            (encoding, cow.into_owned().into_bytes())
        }
    }
}

/// Convert UTF-8 content to the specified encoding for saving
///
/// Used when saving files to convert internal UTF-8 representation
/// back to the original (or user-selected) encoding.
///
/// Note: This does NOT add BOM - the BOM should be handled separately.
pub fn convert_from_utf8(utf8_bytes: &[u8], encoding: Encoding) -> Vec<u8> {
    match encoding {
        Encoding::Utf8 | Encoding::Ascii | Encoding::Utf8Bom => {
            // UTF-8 (with or without BOM) - just clone, BOM added separately
            utf8_bytes.to_vec()
        }
        Encoding::Utf16Le => {
            // Convert UTF-8 to UTF-16 LE (no BOM - added separately)
            let text = String::from_utf8_lossy(utf8_bytes);
            let mut result = Vec::new();
            for code_unit in text.encode_utf16() {
                result.extend_from_slice(&code_unit.to_le_bytes());
            }
            result
        }
        Encoding::Utf16Be => {
            // Convert UTF-8 to UTF-16 BE (no BOM - added separately)
            let text = String::from_utf8_lossy(utf8_bytes);
            let mut result = Vec::new();
            for code_unit in text.encode_utf16() {
                result.extend_from_slice(&code_unit.to_be_bytes());
            }
            result
        }
        _ => {
            // Use encoding_rs to convert from UTF-8
            let enc_rs = encoding.to_encoding_rs();
            let text = String::from_utf8_lossy(utf8_bytes);
            let (cow, _encoding_used, _had_errors) = enc_rs.encode(&text);
            cow.into_owned()
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encoding_display_names() {
        assert_eq!(Encoding::Utf8.display_name(), "UTF-8");
        assert_eq!(Encoding::Utf8Bom.display_name(), "UTF-8 BOM");
        assert_eq!(Encoding::Utf16Le.display_name(), "UTF-16 LE");
        assert_eq!(Encoding::Gb18030.display_name(), "GB18030");
        assert_eq!(Encoding::Windows1250.display_name(), "Windows-1250");
    }

    #[test]
    fn test_encoding_bom() {
        assert!(Encoding::Utf8Bom.has_bom());
        assert!(Encoding::Utf16Le.has_bom());
        assert!(!Encoding::Utf8.has_bom());
        assert!(!Encoding::Windows1252.has_bom());
        assert!(!Encoding::Windows1250.has_bom());
    }

    #[test]
    fn test_detect_utf8() {
        assert_eq!(detect_encoding(b"Hello, world!"), Encoding::Ascii);
        assert_eq!(detect_encoding("Hello, 世界!".as_bytes()), Encoding::Utf8);
    }

    #[test]
    fn test_detect_utf8_bom() {
        let with_bom = [0xEF, 0xBB, 0xBF, b'H', b'i'];
        assert_eq!(detect_encoding(&with_bom), Encoding::Utf8Bom);
    }

    #[test]
    fn test_detect_utf16_le() {
        let utf16_le_bom = [0xFF, 0xFE, b'H', 0x00, b'i', 0x00];
        assert_eq!(detect_encoding(&utf16_le_bom), Encoding::Utf16Le);
    }

    #[test]
    fn test_detect_binary() {
        let binary_data = [0x00, 0x01, 0x02, 0x03];
        let (_, is_binary) = detect_encoding_or_binary(&binary_data);
        assert!(is_binary);
    }

    #[test]
    fn test_is_binary_control_char() {
        // Binary control chars
        assert!(is_binary_control_char(0x00)); // NUL
        assert!(is_binary_control_char(0x01)); // SOH
        assert!(is_binary_control_char(0x02)); // STX
        assert!(is_binary_control_char(0x7F)); // DEL

        // Text control chars (allowed)
        assert!(!is_binary_control_char(0x09)); // Tab
        assert!(!is_binary_control_char(0x0A)); // LF
        assert!(!is_binary_control_char(0x0D)); // CR
        assert!(!is_binary_control_char(0x1B)); // ESC

        // Regular printable chars
        assert!(!is_binary_control_char(b'A'));
        assert!(!is_binary_control_char(b' '));
    }

    #[test]
    fn test_convert_roundtrip_utf8() {
        let original = "Hello, 世界!";
        let bytes = original.as_bytes();

        let (encoding, utf8_content) = detect_and_convert(bytes);
        assert_eq!(encoding, Encoding::Utf8);
        assert_eq!(utf8_content, bytes);

        let back = convert_from_utf8(&utf8_content, encoding);
        assert_eq!(back, bytes);
    }

    #[test]
    fn test_convert_roundtrip_utf16le() {
        // UTF-16 LE with BOM: "Hi"
        let utf16_le = [0xFF, 0xFE, b'H', 0x00, b'i', 0x00];

        let (encoding, utf8_content) = detect_and_convert(&utf16_le);
        assert_eq!(encoding, Encoding::Utf16Le);
        assert_eq!(utf8_content, b"Hi");

        // Note: convert_from_utf8 doesn't add BOM, so result won't have BOM
        let back = convert_from_utf8(&utf8_content, encoding);
        assert_eq!(back, [b'H', 0x00, b'i', 0x00]);
    }

    #[test]
    fn test_encoding_resynchronizable() {
        // Self-synchronizing encodings (can find char boundaries from middle of file)
        assert!(Encoding::Utf8.is_resynchronizable());
        assert!(Encoding::Utf8Bom.is_resynchronizable());
        assert!(Encoding::Ascii.is_resynchronizable());
        assert!(Encoding::Latin1.is_resynchronizable());
        assert!(Encoding::Windows1252.is_resynchronizable());
        assert!(Encoding::Windows1250.is_resynchronizable());

        // UTF-16 is resynchronizable with proper alignment
        assert!(Encoding::Utf16Le.is_resynchronizable());
        assert!(Encoding::Utf16Be.is_resynchronizable());

        // Legacy CJK encodings are NOT resynchronizable
        // (second byte of double-byte char can equal a valid single-byte char)
        assert!(!Encoding::Gb18030.is_resynchronizable());
        assert!(!Encoding::Gbk.is_resynchronizable());
        assert!(!Encoding::ShiftJis.is_resynchronizable());
        assert!(!Encoding::EucKr.is_resynchronizable());
    }

    #[test]
    fn test_encoding_alignment() {
        // Single-byte encodings have alignment of 1
        assert_eq!(Encoding::Ascii.alignment(), Some(1));
        assert_eq!(Encoding::Latin1.alignment(), Some(1));
        assert_eq!(Encoding::Windows1252.alignment(), Some(1));
        assert_eq!(Encoding::Windows1250.alignment(), Some(1));
        assert_eq!(Encoding::Utf8.alignment(), Some(1));
        assert_eq!(Encoding::Utf8Bom.alignment(), Some(1));

        // UTF-16 requires 2-byte alignment
        assert_eq!(Encoding::Utf16Le.alignment(), Some(2));
        assert_eq!(Encoding::Utf16Be.alignment(), Some(2));

        // Non-resynchronizable encodings have no valid alignment
        assert_eq!(Encoding::Gb18030.alignment(), None);
        assert_eq!(Encoding::Gbk.alignment(), None);
        assert_eq!(Encoding::ShiftJis.alignment(), None);
        assert_eq!(Encoding::EucKr.alignment(), None);
    }

    #[test]
    fn test_requires_full_file_load() {
        // Encodings that can be streamed
        assert!(!Encoding::Utf8.requires_full_file_load());
        assert!(!Encoding::Ascii.requires_full_file_load());
        assert!(!Encoding::Latin1.requires_full_file_load());
        assert!(!Encoding::Windows1250.requires_full_file_load());
        assert!(!Encoding::Utf16Le.requires_full_file_load());

        // Encodings that require full loading
        assert!(Encoding::Gb18030.requires_full_file_load());
        assert!(Encoding::Gbk.requires_full_file_load());
        assert!(Encoding::ShiftJis.requires_full_file_load());
        assert!(Encoding::EucKr.requires_full_file_load());
    }

    #[test]
    fn test_convert_roundtrip_windows1250() {
        // Windows-1250 encoded text with Central European characters
        // "Zażółć" in Windows-1250: Z(0x5A) a(0x61) ż(0xBF) ó(0xF3) ł(0xB3) ć(0xE6)
        let windows1250_bytes: &[u8] = &[0x5A, 0x61, 0xBF, 0xF3, 0xB3, 0xE6];

        // Convert to UTF-8
        let enc_rs = Encoding::Windows1250.to_encoding_rs();
        let (decoded, _had_errors) = enc_rs.decode_without_bom_handling(windows1250_bytes);
        let utf8_content = decoded.as_bytes();

        // The UTF-8 content should contain the Polish characters
        let utf8_str = std::str::from_utf8(utf8_content).unwrap();
        assert!(utf8_str.contains('ż'), "Should contain ż: {}", utf8_str);
        assert!(utf8_str.contains('ó'), "Should contain ó: {}", utf8_str);
        assert!(utf8_str.contains('ł'), "Should contain ł: {}", utf8_str);
        assert!(utf8_str.contains('ć'), "Should contain ć: {}", utf8_str);

        // Convert back to Windows-1250
        let back = convert_from_utf8(utf8_content, Encoding::Windows1250);
        assert_eq!(back, windows1250_bytes, "Round-trip should preserve bytes");
    }

    #[test]
    fn test_windows1250_description() {
        assert_eq!(
            Encoding::Windows1250.description(),
            "Windows-1250 (Central European)"
        );
    }

    #[test]
    fn test_detect_windows1250_definitive_bytes() {
        // Bytes 0x8D (Ť), 0x8F (Ź), 0x9D (ť) are undefined in Windows-1252
        // but valid in Windows-1250, so they definitively indicate Windows-1250

        // Czech text with ť (0x9D): "měsťo" (city, archaic)
        let with_t_caron = [0x6D, 0x9D, 0x73, 0x74, 0x6F]; // mťsto
        assert_eq!(
            detect_encoding(&with_t_caron),
            Encoding::Windows1250,
            "Byte 0x9D (ť) should trigger Windows-1250 detection"
        );

        // Polish text with Ź (0x8F): "Źródło" (source)
        let with_z_acute_upper = [0x8F, 0x72, 0xF3, 0x64, 0xB3, 0x6F]; // Źródło
        assert_eq!(
            detect_encoding(&with_z_acute_upper),
            Encoding::Windows1250,
            "Byte 0x8F (Ź) should trigger Windows-1250 detection"
        );
    }

    #[test]
    fn test_detect_windows1250_strong_indicators() {
        // Polish text with ś (0x9C) and Ś (0x8C) - strong indicators from 0x80-0x9F range
        let polish_text = [
            0x9C, 0x77, 0x69, 0x65, 0x74, 0x79, 0x20, // "świety "
            0x8C, 0x77, 0x69, 0x61, 0x74, // "Świat"
        ];
        assert_eq!(
            detect_encoding(&polish_text),
            Encoding::Windows1250,
            "Multiple Polish characters (ś, Ś) should trigger Windows-1250"
        );
    }

    #[test]
    fn test_detect_ambiguous_bytes_as_windows1252() {
        // Bytes in 0xA0-0xFF range are ambiguous and should default to Windows-1252
        // Polish "żółć" - ż(0xBF) ó(0xF3) ł(0xB3) ć(0xE6) - all ambiguous
        let zolc = [0xBF, 0xF3, 0xB3, 0xE6];
        assert_eq!(
            detect_encoding(&zolc),
            Encoding::Windows1252,
            "Ambiguous bytes should default to Windows-1252"
        );

        // ą (0xB9) and ł (0xB3) could be ¹ and ³ in Windows-1252
        let ambiguous = [
            0x6D, 0xB9, 0x6B, 0x61, 0x20, // "mąka " or "m¹ka "
            0x6D, 0xB3, 0x6F, 0x64, 0x79, // "młody" or "m³ody"
        ];
        assert_eq!(
            detect_encoding(&ambiguous),
            Encoding::Windows1252,
            "Ambiguous Polish bytes should default to Windows-1252"
        );
    }

    #[test]
    fn test_detect_windows1250_czech_pangram() {
        // "Příliš žluťoučký kůň úpěl ďábelské ódy" - Czech pangram in Windows-1250
        // Contains ť (0x9D) which is a definitive Windows-1250 indicator
        let czech_pangram: &[u8] = &[
            0x50, 0xF8, 0xED, 0x6C, 0x69, 0x9A, 0x20, // "Příliš "
            0x9E, 0x6C, 0x75, 0x9D, 0x6F, 0x75, 0xE8, 0x6B, 0xFD, 0x20, // "žluťoučký "
            0x6B, 0xF9, 0xF2, 0x20, // "kůň "
            0xFA, 0x70, 0xEC, 0x6C, 0x20, // "úpěl "
            0xEF, 0xE1, 0x62, 0x65, 0x6C, 0x73, 0x6B, 0xE9, 0x20, // "ďábelské "
            0xF3, 0x64, 0x79, // "ódy"
        ];
        assert_eq!(
            detect_encoding(czech_pangram),
            Encoding::Windows1250,
            "Czech pangram should be detected as Windows-1250 (contains ť = 0x9D)"
        );
    }

    #[test]
    fn test_detect_windows1252_not_1250() {
        // Pure Windows-1252 text without Central European indicators
        // "Café résumé" in Windows-1252
        let windows1252_text = [
            0x43, 0x61, 0x66, 0xE9, 0x20, // "Café "
            0x72, 0xE9, 0x73, 0x75, 0x6D, 0xE9, // "résumé"
        ];
        assert_eq!(
            detect_encoding(&windows1252_text),
            Encoding::Windows1252,
            "French text should remain Windows-1252"
        );
    }
}
