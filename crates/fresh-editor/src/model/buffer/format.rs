//! Encoding and line-ending state for a `TextBuffer`.
//!
//! Owns the four format-related fields (current line-ending,
//! original line-ending at load, current encoding, original encoding
//! at load) as a `BufferFormat` sub-struct composed inside
//! `TextBuffer`. Exposes pure free functions for detection and
//! conversion so they can be used without constructing any buffer
//! state.

use crate::model::encoding::{self, Encoding};

/// Line-ending format detected in (or chosen for) a text buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LineEnding {
    /// Unix/Linux/Mac format (\n)
    #[default]
    LF,
    /// Windows format (\r\n)
    CRLF,
    /// Old Mac format (\r) - rare but supported
    CR,
}

impl LineEnding {
    /// Get the string representation of this line ending
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::LF => "\n",
            Self::CRLF => "\r\n",
            Self::CR => "\r",
        }
    }

    /// Get the display name for status bar
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::LF => "LF",
            Self::CRLF => "CRLF",
            Self::CR => "CR",
        }
    }
}

/// Encoding + line-ending state for one `TextBuffer`.
///
/// Owns both the current and the original-at-load-time values so the
/// save path can detect that the user changed the format and rewrite
/// the bytes accordingly.
#[derive(Debug, Clone, Copy)]
pub struct BufferFormat {
    line_ending: LineEnding,
    original_line_ending: LineEnding,
    encoding: Encoding,
    original_encoding: Encoding,
}

impl BufferFormat {
    pub fn new(line_ending: LineEnding, encoding: Encoding) -> Self {
        Self {
            line_ending,
            original_line_ending: line_ending,
            encoding,
            original_encoding: encoding,
        }
    }

    pub fn line_ending(&self) -> LineEnding {
        self.line_ending
    }

    pub fn encoding(&self) -> Encoding {
        self.encoding
    }

    pub fn original_line_ending(&self) -> LineEnding {
        self.original_line_ending
    }

    pub fn original_encoding(&self) -> Encoding {
        self.original_encoding
    }

    pub fn set_line_ending(&mut self, le: LineEnding) {
        self.line_ending = le;
    }

    pub fn set_encoding(&mut self, e: Encoding) {
        self.encoding = e;
    }

    pub fn set_default_line_ending(&mut self, le: LineEnding) {
        self.line_ending = le;
        self.original_line_ending = le;
    }

    pub fn set_default_encoding(&mut self, e: Encoding) {
        self.encoding = e;
        self.original_encoding = e;
    }

    pub fn line_ending_changed_since_load(&self) -> bool {
        self.line_ending != self.original_line_ending
    }

    pub fn encoding_changed_since_load(&self) -> bool {
        self.encoding != self.original_encoding
    }

    /// Called after a successful save to make the current values the
    /// new "original" baseline.
    pub(super) fn promote_current_to_original(&mut self) {
        self.original_line_ending = self.line_ending;
        self.original_encoding = self.encoding;
    }
}

// ---------- free helpers (Rule 4 in the refactor plan) ----------

/// Detect the line ending format from a sample of bytes
///
/// Uses majority voting: counts CRLF, LF-only, and CR-only
/// occurrences and returns the most common format.
pub fn detect_line_ending(bytes: &[u8]) -> LineEnding {
    // Only check the first 8KB for line ending detection (same as binary detection)
    let check_len = bytes.len().min(8 * 1024);
    let sample = &bytes[..check_len];

    let mut crlf_count = 0;
    let mut lf_only_count = 0;
    let mut cr_only_count = 0;

    let mut i = 0;
    while i < sample.len() {
        if sample[i] == b'\r' {
            // Check if this is CRLF
            if i + 1 < sample.len() && sample[i + 1] == b'\n' {
                crlf_count += 1;
                i += 2; // Skip both \r and \n
                continue;
            } else {
                // CR only (old Mac format)
                cr_only_count += 1;
            }
        } else if sample[i] == b'\n' {
            // LF only (Unix format)
            lf_only_count += 1;
        }
        i += 1;
    }

    // Use majority voting to determine line ending
    if crlf_count > lf_only_count && crlf_count > cr_only_count {
        LineEnding::CRLF
    } else if cr_only_count > lf_only_count && cr_only_count > crlf_count {
        LineEnding::CR
    } else {
        // Default to LF if no clear winner or if LF wins
        LineEnding::LF
    }
}

/// Detect the text encoding from a sample of bytes.
pub fn detect_encoding(bytes: &[u8]) -> Encoding {
    encoding::detect_encoding(bytes)
}

/// Detect the text encoding and whether content is binary.
///
/// Returns `(encoding, is_binary)`.
pub fn detect_encoding_or_binary(bytes: &[u8], truncated: bool) -> (Encoding, bool) {
    encoding::detect_encoding_or_binary(bytes, truncated)
}

/// Detect encoding and convert bytes to UTF-8.
pub fn detect_and_convert_encoding(bytes: &[u8]) -> (Encoding, Vec<u8>) {
    encoding::detect_and_convert(bytes)
}

/// Convert UTF-8 content to the specified encoding for saving.
///
/// Does NOT add BOM — BOM handling lives in the write-recipe path.
pub fn convert_to_encoding(utf8_bytes: &[u8], target_encoding: Encoding) -> Vec<u8> {
    encoding::convert_from_utf8(utf8_bytes, target_encoding)
}

/// Normalize line endings in the given bytes to LF only.
///
/// Converts CRLF (\r\n) and CR (\r) to LF (\n) for internal
/// representation. Kept for tests and potential future use.
#[allow(dead_code)]
pub fn normalize_line_endings(bytes: Vec<u8>) -> Vec<u8> {
    let mut normalized = Vec::with_capacity(bytes.len());
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'\r' {
            // Check if this is CRLF
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                normalized.push(b'\n');
                i += 2;
                continue;
            } else {
                normalized.push(b'\n');
            }
        } else {
            normalized.push(bytes[i]);
        }
        i += 1;
    }

    normalized
}

/// Convert line endings from any source format to any target format.
///
/// Used during save when the user has changed the line-ending format.
/// Pub(super) because only the in-module save path and its tests call it.
pub(super) fn convert_line_endings_to(bytes: &[u8], target_ending: LineEnding) -> Vec<u8> {
    // First pass: normalize everything to LF
    let mut normalized = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\r' {
            if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                normalized.push(b'\n');
                i += 2;
                continue;
            } else {
                normalized.push(b'\n');
            }
        } else {
            normalized.push(bytes[i]);
        }
        i += 1;
    }

    // If target is LF, we're done
    if target_ending == LineEnding::LF {
        return normalized;
    }

    // Second pass: convert LF to target format
    let replacement = target_ending.as_str().as_bytes();
    let mut result = Vec::with_capacity(normalized.len() + normalized.len() / 10);

    for byte in normalized {
        if byte == b'\n' {
            result.extend_from_slice(replacement);
        } else {
            result.push(byte);
        }
    }

    result
}
