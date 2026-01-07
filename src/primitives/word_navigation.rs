//! Word boundary detection and navigation helpers

use crate::model::buffer::Buffer;

/// Check if a byte is a word character (alphanumeric or underscore)
pub fn is_word_char(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

// ============================================================================
// Core byte-level word navigation (shared by Buffer and String operations)
// ============================================================================
//
// These functions contain the pure logic for finding word boundaries in byte
// slices. They are used by both:
// - Buffer operations (which extract windowed byte slices from the rope)
// - String/prompt operations (which use the string's byte array directly)
//
// This eliminates code duplication while maintaining identical behavior across
// buffer editing and prompt input contexts.

/// Find the start of the word at or before the given position in a byte slice.
///
/// This is the core logic shared by both Buffer and String word navigation.
///
/// # Arguments
/// * `bytes` - The byte slice to search in
/// * `pos` - Position within the bytes (0-indexed)
///
/// # Returns
/// Position of the word start (always <= pos)
pub fn find_word_start_bytes(bytes: &[u8], pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let pos = pos.min(bytes.len());
    let mut new_pos = pos;

    // If we're at the end or at a non-word character, scan left
    if new_pos >= bytes.len()
        || (bytes
            .get(new_pos)
            .map(|&b| !is_word_char(b))
            .unwrap_or(true))
    {
        if new_pos > 0 {
            new_pos = new_pos.saturating_sub(1);
        }
    }

    // Find start of current word by scanning backwards
    while new_pos > 0 {
        if let Some(&prev_byte) = bytes.get(new_pos.saturating_sub(1)) {
            if !is_word_char(prev_byte) {
                break;
            }
            new_pos = new_pos.saturating_sub(1);
        } else {
            break;
        }
    }

    new_pos
}

/// Find the end of the word at or after the given position in a byte slice.
///
/// This is the core logic shared by both Buffer and String word navigation.
///
/// # Arguments
/// * `bytes` - The byte slice to search in
/// * `pos` - Position within the bytes (0-indexed)
///
/// # Returns
/// Position of the word end (always >= pos)
pub fn find_word_end_bytes(bytes: &[u8], pos: usize) -> usize {
    let pos = pos.min(bytes.len());
    let mut new_pos = pos;

    // Skip to start of next word if we're at non-word character
    while new_pos < bytes.len() && !is_word_char(bytes[new_pos]) {
        new_pos += 1;
    }

    // Find end of word
    while new_pos < bytes.len() && is_word_char(bytes[new_pos]) {
        new_pos += 1;
    }

    new_pos
}

/// Find the start of the completion word at the cursor position.
/// This is different from find_word_start in that it stops at delimiters like `.` and `::`
/// rather than including them in the deletion range.
///
/// For example:
/// - "args.som|" returns position of 's' (after the dot)
/// - "Self::new|" returns position of 'n' (after the ::)
/// - "hello|" returns position of 'h' (start of word)
/// - "args.|" returns cursor position (no partial word to delete)
pub fn find_completion_word_start(buffer: &Buffer, pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let buf_len = buffer.len();
    let pos = pos.min(buf_len);

    // Only read a small window around the position for efficiency
    let start = pos.saturating_sub(1000);
    let end = (pos + 1).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);
    let offset = pos - start;

    if offset == 0 {
        return pos;
    }

    // Check the character immediately before the cursor
    if let Some(&prev_byte) = bytes.get(offset.saturating_sub(1)) {
        // If the previous character is not a word character (e.g., '.', ':', ' '),
        // then there's no partial word to delete - return cursor position
        if !is_word_char(prev_byte) {
            return pos;
        }
    }

    let mut new_pos = offset;

    // If we're at the end of the buffer or at a non-word character, scan left
    if new_pos >= bytes.len()
        || (bytes
            .get(new_pos)
            .map(|&b| !is_word_char(b))
            .unwrap_or(true))
    {
        if new_pos > 0 {
            new_pos = new_pos.saturating_sub(1);
        }
    }

    // Find start of current identifier segment by scanning backwards
    // Stop at delimiters like '.' or ':'
    while new_pos > 0 {
        if let Some(&prev_byte) = bytes.get(new_pos.saturating_sub(1)) {
            if !is_word_char(prev_byte) {
                // Stop here - don't include the delimiter
                break;
            }
            new_pos = new_pos.saturating_sub(1);
        } else {
            break;
        }
    }

    start + new_pos
}

/// Find the start of the word at or before the given position
///
/// Extracts a windowed byte slice from the buffer and uses the shared
/// byte-level logic to find word boundaries.
pub fn find_word_start(buffer: &Buffer, pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let buf_len = buffer.len();
    let pos = pos.min(buf_len);

    // Only read a small window around the position for efficiency
    let start = pos.saturating_sub(1000);
    let end = (pos + 1).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);
    let offset = pos - start;

    // Use shared byte-level logic
    let result = find_word_start_bytes(&bytes, offset);
    start + result
}

/// Find the end of the word at or after the given position
///
/// Extracts a windowed byte slice from the buffer and uses the shared
/// byte-level logic to find word boundaries.
pub fn find_word_end(buffer: &Buffer, pos: usize) -> usize {
    let buf_len = buffer.len();
    if pos >= buf_len {
        return buf_len;
    }

    // Only read a small window around the position for efficiency
    let start = pos;
    let end = (pos + 1000).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);

    // Use shared byte-level logic
    let result = find_word_end_bytes(&bytes, 0);
    start + result
}

use crate::primitives::grapheme::{next_grapheme_boundary, prev_grapheme_boundary};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CharClass {
    Word,
    Whitespace,
    Punctuation,
}

fn get_grapheme_class(g: &str) -> CharClass {
    if g.chars().any(|c| c.is_alphanumeric() || c == '_') {
        CharClass::Word
    } else if g.chars().all(|c| c.is_whitespace()) {
        CharClass::Whitespace
    } else {
        CharClass::Punctuation
    }
}

/// Find the start of the word to the left of the given position
pub fn find_word_start_left(buffer: &Buffer, pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let buf_len = buffer.len();
    let actual_pos = pos.min(buf_len);

    // Only read a small window around the position for efficiency
    // We grab a bit more to ensure we have context
    let start = actual_pos.saturating_sub(1000);
    let end = actual_pos;
    let bytes = buffer.slice_bytes(start..end);

    // Convert to string safely (replacing invalid sequences which might happen at the start boundary)
    let text = String::from_utf8_lossy(&bytes);

    // The end of the string corresponds to `pos`
    let mut current_idx = text.len();

    // 1. Consume whitespace to the left
    while current_idx > 0 {
        let prev = prev_grapheme_boundary(&text, current_idx);
        let g = &text[prev..current_idx];
        if get_grapheme_class(g) == CharClass::Whitespace {
            current_idx = prev;
        } else {
            break;
        }
    }

    if current_idx == 0 {
        // We consumed everything up to the start of our chunk
        // Best we can do is return the start of the chunk
        // (In reality, words > 1000 chars are rare)
        // We calculate absolute position:
        // logic: bytes.len() - current_idx is the delta we moved back
        let delta = text.len() - current_idx;
        return actual_pos.saturating_sub(delta);
    }

    // 2. Identify class of the token we hit
    let prev = prev_grapheme_boundary(&text, current_idx);
    let target_class = get_grapheme_class(&text[prev..current_idx]);

    // 3. Consume all characters of the same class
    while current_idx > 0 {
        let prev = prev_grapheme_boundary(&text, current_idx);
        let g = &text[prev..current_idx];
        if get_grapheme_class(g) == target_class {
            current_idx = prev;
        } else {
            break;
        }
    }

    let delta = text.len() - current_idx;
    actual_pos.saturating_sub(delta)
}

/// Find the start of the word to the right of the given position
pub fn find_word_start_right(buffer: &Buffer, pos: usize) -> usize {
    let buf_len = buffer.len();
    if pos >= buf_len {
        return buf_len;
    }

    // Only read a small window around the position for efficiency
    let start = pos;
    let end = (pos + 1000).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);
    let text = String::from_utf8_lossy(&bytes);

    let mut current_idx = 0;
    if current_idx >= text.len() {
        return start;
    }

    // Look at the grapheme at current position
    let next_bound = next_grapheme_boundary(&text, current_idx);
    let start_class = get_grapheme_class(&text[current_idx..next_bound]);

    // 1. If starting on whitespace, just consume it and stop
    if start_class == CharClass::Whitespace {
        while current_idx < text.len() {
            let next = next_grapheme_boundary(&text, current_idx);
            let g = &text[current_idx..next];
            if get_grapheme_class(g) == CharClass::Whitespace {
                current_idx = next;
            } else {
                break;
            }
        }
        return start + current_idx;
    }

    // 2. Otherwise (Word or Punctuation), consume all characters of same class
    while current_idx < text.len() {
        let next = next_grapheme_boundary(&text, current_idx);
        let g = &text[current_idx..next];
        if get_grapheme_class(g) == start_class {
            current_idx = next;
        } else {
            break;
        }
    }

    // 3. Then consume subsequent whitespace to land at start of next token
    while current_idx < text.len() {
        let next = next_grapheme_boundary(&text, current_idx);
        let g = &text[current_idx..next];
        if get_grapheme_class(g) == CharClass::Whitespace {
            current_idx = next;
        } else {
            break;
        }
    }

    start + current_idx
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::buffer::Buffer;

    #[test]
    fn test_is_word_char() {
        assert!(is_word_char(b'a'));
        assert!(is_word_char(b'Z'));
        assert!(is_word_char(b'0'));
        assert!(is_word_char(b'_'));
        assert!(!is_word_char(b' '));
        assert!(!is_word_char(b'.'));
        assert!(!is_word_char(b'-'));
    }

    #[test]
    fn test_find_word_start() {
        let buffer = Buffer::from_str_test("hello world test");
        assert_eq!(find_word_start(&buffer, 0), 0); // Start of "hello"
        assert_eq!(find_word_start(&buffer, 3), 0); // Middle of "hello"
        assert_eq!(find_word_start(&buffer, 6), 6); // Start of "world"
        assert_eq!(find_word_start(&buffer, 8), 6); // Middle of "world"
    }

    #[test]
    fn test_find_word_end() {
        let buffer = Buffer::from_str_test("hello world test");
        assert_eq!(find_word_end(&buffer, 0), 5); // End of "hello"
        assert_eq!(find_word_end(&buffer, 3), 5); // Middle of "hello"
        assert_eq!(find_word_end(&buffer, 6), 11); // End of "world"
    }

    #[test]
    fn test_find_word_start_left() {
        let buffer = Buffer::from_str_test("hello world test");
        assert_eq!(find_word_start_left(&buffer, 6), 0); // From "world" to "hello"
        assert_eq!(find_word_start_left(&buffer, 12), 6); // From "test" to "world"
    }

    #[test]
    fn test_find_word_start_right() {
        let buffer = Buffer::from_str_test("hello world test");
        assert_eq!(find_word_start_right(&buffer, 0), 6); // From "hello" to "world"
        assert_eq!(find_word_start_right(&buffer, 6), 12); // From "world" to "test"
    }

    // ========================================================================
    // Tests for byte-level word navigation (shared by Buffer and String)
    // ========================================================================

    #[test]
    fn test_find_word_start_bytes_basic() {
        let s = "hello world test";
        let bytes = s.as_bytes();
        assert_eq!(find_word_start_bytes(bytes, 0), 0); // Start of "hello"
        assert_eq!(find_word_start_bytes(bytes, 3), 0); // Middle of "hello"
        assert_eq!(find_word_start_bytes(bytes, 5), 0); // End of "hello"
        assert_eq!(find_word_start_bytes(bytes, 6), 6); // Start of "world"
        assert_eq!(find_word_start_bytes(bytes, 8), 6); // Middle of "world"
        assert_eq!(find_word_start_bytes(bytes, 11), 6); // End of "world"
        assert_eq!(find_word_start_bytes(bytes, 12), 12); // Start of "test"
    }

    #[test]
    fn test_find_word_end_bytes_basic() {
        let s = "hello world test";
        let bytes = s.as_bytes();
        assert_eq!(find_word_end_bytes(bytes, 0), 5); // End of "hello"
        assert_eq!(find_word_end_bytes(bytes, 3), 5); // Middle of "hello"
        assert_eq!(find_word_end_bytes(bytes, 6), 11); // End of "world"
        assert_eq!(find_word_end_bytes(bytes, 8), 11); // Middle of "world"
        assert_eq!(find_word_end_bytes(bytes, 12), 16); // End of "test"
    }

    #[test]
    fn test_find_word_start_bytes_special_chars() {
        let s = "save-file-as";
        let bytes = s.as_bytes();
        assert_eq!(find_word_start_bytes(bytes, 4), 0); // "save"
        assert_eq!(find_word_start_bytes(bytes, 5), 5); // hyphen stops word
        assert_eq!(find_word_start_bytes(bytes, 9), 5); // "file"
        assert_eq!(find_word_start_bytes(bytes, 10), 10); // hyphen stops word
        assert_eq!(find_word_start_bytes(bytes, 12), 10); // "as"
    }

    #[test]
    fn test_find_word_end_bytes_special_chars() {
        let s = "open.file.now";
        let bytes = s.as_bytes();
        assert_eq!(find_word_end_bytes(bytes, 0), 4); // "open"
        assert_eq!(find_word_end_bytes(bytes, 4), 9); // skip '.', then "file"
        assert_eq!(find_word_end_bytes(bytes, 5), 9); // "file"
        assert_eq!(find_word_end_bytes(bytes, 10), 13); // "now"
    }

    #[test]
    fn test_find_word_start_bytes_whitespace() {
        let s = "  hello  world  ";
        let bytes = s.as_bytes();
        assert_eq!(find_word_start_bytes(bytes, 4), 2); // "hello"
        assert_eq!(find_word_start_bytes(bytes, 7), 2); // After "hello"
        assert_eq!(find_word_start_bytes(bytes, 9), 9); // "world"
        assert_eq!(find_word_start_bytes(bytes, 14), 9); // After "world"
    }

    #[test]
    fn test_find_word_end_bytes_whitespace() {
        let s = "  hello  world  ";
        let bytes = s.as_bytes();
        assert_eq!(find_word_end_bytes(bytes, 0), 7); // Skip spaces, end of "hello"
        assert_eq!(find_word_end_bytes(bytes, 2), 7); // End of "hello"
        assert_eq!(find_word_end_bytes(bytes, 7), 14); // Skip spaces, end of "world"
        assert_eq!(find_word_end_bytes(bytes, 9), 14); // End of "world"
    }

    #[test]
    fn test_find_word_start_bytes_edge_cases() {
        // Empty string
        assert_eq!(find_word_start_bytes(b"", 0), 0);

        // Single character
        assert_eq!(find_word_start_bytes(b"a", 0), 0);
        assert_eq!(find_word_start_bytes(b"a", 1), 0);

        // No words (all special chars) - scans back but finds no word
        assert_eq!(find_word_start_bytes(b"...", 2), 1);

        // Position beyond string length
        assert_eq!(find_word_start_bytes(b"hello", 100), 0);
    }

    #[test]
    fn test_find_word_end_bytes_edge_cases() {
        // Empty string
        assert_eq!(find_word_end_bytes(b"", 0), 0);

        // Single character
        assert_eq!(find_word_end_bytes(b"a", 0), 1);

        // No words (all special chars)
        assert_eq!(find_word_end_bytes(b"...", 0), 3);

        // Position beyond string length
        assert_eq!(find_word_end_bytes(b"hello", 100), 5);
    }

    #[test]
    fn test_find_word_start_bytes_underscores() {
        let s = "some_variable_name";
        let bytes = s.as_bytes();
        assert_eq!(find_word_start_bytes(bytes, 7), 0); // Underscores are word chars
        assert_eq!(find_word_start_bytes(bytes, 18), 0);
    }

    #[test]
    fn test_find_word_end_bytes_underscores() {
        let s = "some_variable_name";
        let bytes = s.as_bytes();
        assert_eq!(find_word_end_bytes(bytes, 0), 18); // Underscores are word chars
        assert_eq!(find_word_end_bytes(bytes, 7), 18);
    }

    // Property-based tests
    #[cfg(test)]
    mod property_tests {
        use super::*;
        use proptest::prelude::*;

        // Strategy to generate ASCII strings with word and non-word characters
        fn ascii_string() -> impl Strategy<Value = String> {
            "[a-zA-Z0-9_ .,-]{0,100}".prop_map(|s| s)
        }

        proptest! {
            /// Property: find_word_start_bytes should always return a position <= input position
            #[test]
            fn prop_word_start_not_after_position(s in ascii_string(), pos in 0usize..100) {
                let bytes = s.as_bytes();
                let result = find_word_start_bytes(bytes, pos);
                prop_assert!(result <= pos.min(s.len()));
            }

            /// Property: find_word_end_bytes should always return a position >= input position
            #[test]
            fn prop_word_end_not_before_position(s in ascii_string(), pos in 0usize..100) {
                let bytes = s.as_bytes();
                let result = find_word_end_bytes(bytes, pos);
                prop_assert!(result >= pos.min(s.len()));
            }

            /// Property: find_word_end_bytes should never exceed string length
            #[test]
            fn prop_word_end_within_bounds(s in ascii_string(), pos in 0usize..100) {
                let bytes = s.as_bytes();
                let result = find_word_end_bytes(bytes, pos);
                prop_assert!(result <= s.len());
            }

            /// Property: find_word_start_bytes at position 0 should return 0
            #[test]
            fn prop_word_start_at_zero(s in ascii_string()) {
                let bytes = s.as_bytes();
                let result = find_word_start_bytes(bytes, 0);
                prop_assert_eq!(result, 0);
            }

            /// Property: find_word_end_bytes at end should return end
            #[test]
            fn prop_word_end_at_end(s in ascii_string()) {
                let bytes = s.as_bytes();
                let result = find_word_end_bytes(bytes, s.len());
                prop_assert_eq!(result, s.len());
            }

            /// Property: Applying find_word_start_bytes should move towards the start
            #[test]
            fn prop_word_start_monotonic(s in ascii_string(), pos in 0usize..100) {
                let bytes = s.as_bytes();
                let first = find_word_start_bytes(bytes, pos);
                let second = find_word_start_bytes(bytes, first);
                // Second application should not move forward
                prop_assert!(second <= first);
            }

            /// Property: The result should always be at a word boundary or start
            #[test]
            fn prop_word_start_at_boundary(s in ascii_string(), pos in 0usize..100) {
                let bytes = s.as_bytes();
                let result = find_word_start_bytes(bytes, pos.min(s.len()));

                // Either at start of string, or previous char is not a word char
                prop_assert!(
                    result == 0 ||
                    result > bytes.len() ||
                    !is_word_char(bytes[result.saturating_sub(1)])
                );
            }

            /// Property: Word start and end should define a valid range
            #[test]
            fn prop_word_range_valid(s in ascii_string(), pos in 0usize..100) {
                let bytes = s.as_bytes();
                let pos = pos.min(s.len());
                let start = find_word_start_bytes(bytes, pos);
                let end = find_word_end_bytes(bytes, pos);

                // Start should be <= pos, end should be >= pos
                prop_assert!(start <= pos);
                prop_assert!(end >= pos);
                // Start should be <= end (forms valid range)
                prop_assert!(start <= end);
            }
        }
    }
}
