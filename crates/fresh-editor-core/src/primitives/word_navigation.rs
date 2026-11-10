//! Word boundary detection and navigation helpers

use crate::model::buffer::Buffer;
use crate::primitives::grapheme::{
    next_grapheme_boundary, prev_grapheme_boundary, snap_to_grapheme_boundary,
};

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

/// Whether `c` is a word character as the regex `\w` (and so `\b`) counts
/// them: any Unicode letter, mark, digit or connector punctuation.
pub fn is_unicode_word_char(c: char) -> bool {
    regex_syntax::is_word_character(c)
}

/// The character starting at byte `pos` of `buffer`; `None` at the end, and
/// U+FFFD for a byte that starts no valid character.
pub fn char_at(buffer: &Buffer, pos: usize) -> Option<char> {
    let bytes = buffer.slice_bytes(pos..(pos + 4).min(buffer.len()));
    String::from_utf8_lossy(&bytes).chars().next()
}

/// The character ending at byte `pos` of `buffer`; `None` at the start, and
/// U+FFFD for bytes that end no valid character.
pub fn char_before(buffer: &Buffer, pos: usize) -> Option<char> {
    let bytes = buffer.slice_bytes(pos.saturating_sub(4)..pos);
    String::from_utf8_lossy(&bytes).chars().next_back()
}

/// Whether the character starting at byte `pos` of `buffer` is a word
/// character (see [`is_unicode_word_char`]).
pub fn is_word_char_at(buffer: &Buffer, pos: usize) -> bool {
    char_at(buffer, pos).is_some_and(is_unicode_word_char)
}

/// Whether the character ending at byte `pos` of `buffer` is a word
/// character (see [`is_unicode_word_char`]).
pub fn is_word_char_before(buffer: &Buffer, pos: usize) -> bool {
    char_before(buffer, pos).is_some_and(is_unicode_word_char)
}

/// Check if a byte is a word character (alphanumeric or underscore)
pub fn is_word_char(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
}

// ============================================================================
// Core string word navigation (shared by prompts and text fields)
// ============================================================================
//
// These functions contain the pure logic for finding word boundaries in a
// `&str`. Words are made of whole grapheme clusters, classified with
// [`is_unicode_word_char`], so accented, decomposed, Greek, CJK, ... text
// behaves the same as ASCII and results always land on grapheme boundaries.

/// Whether the grapheme cluster `g` belongs to a word.
fn is_word_grapheme(g: &str) -> bool {
    g.chars().any(is_unicode_word_char)
}

/// The grapheme ending at `pos`, if any, with its start offset.
fn grapheme_before(text: &str, pos: usize) -> Option<(usize, &str)> {
    (pos > 0).then(|| {
        let start = prev_grapheme_boundary(text, pos);
        (start, &text[start..pos])
    })
}

/// The grapheme starting at `pos`, if any, with its end offset.
fn grapheme_after(text: &str, pos: usize) -> Option<(usize, &str)> {
    (pos < text.len()).then(|| {
        let end = next_grapheme_boundary(text, pos);
        (end, &text[pos..end])
    })
}

/// Clamp `pos` to the text and snap it back to a grapheme boundary.
fn snap_pos(text: &str, pos: usize) -> usize {
    snap_to_grapheme_boundary(text, pos.min(text.len()))
}

/// Find the start of the word at or before `pos` in `text`.
///
/// # Returns
/// Position of the word start (always <= pos)
pub fn find_word_start_str(text: &str, pos: usize) -> usize {
    let mut new_pos = snap_pos(text, pos);

    // If we're at the end or on a non-word grapheme, step left once
    let on_word = grapheme_after(text, new_pos).is_some_and(|(_, g)| is_word_grapheme(g));
    if !on_word {
        if let Some((start, _)) = grapheme_before(text, new_pos) {
            new_pos = start;
        }
    }

    // Scan left while the preceding grapheme is a word grapheme
    while let Some((start, g)) = grapheme_before(text, new_pos) {
        if !is_word_grapheme(g) {
            break;
        }
        new_pos = start;
    }

    new_pos
}

/// Find the end of the word at or after `pos` in `text`.
///
/// # Returns
/// Position of the word end (always >= pos)
pub fn find_word_end_str(text: &str, pos: usize) -> usize {
    let mut new_pos = snap_pos(text, pos);

    // Skip to the start of the next word if we're on a non-word grapheme
    while let Some((end, g)) = grapheme_after(text, new_pos) {
        if is_word_grapheme(g) {
            break;
        }
        new_pos = end;
    }

    // Find the end of the word
    while let Some((end, g)) = grapheme_after(text, new_pos) {
        if !is_word_grapheme(g) {
            break;
        }
        new_pos = end;
    }

    new_pos
}

/// Start of the previous word: skip non-word graphemes before `pos`, then the
/// word graphemes before those (the Ctrl+Left motion).
pub fn find_prev_word_start_str(text: &str, pos: usize) -> usize {
    let mut new_pos = snap_pos(text, pos);

    while let Some((start, g)) = grapheme_before(text, new_pos) {
        if is_word_grapheme(g) {
            break;
        }
        new_pos = start;
    }
    while let Some((start, g)) = grapheme_before(text, new_pos) {
        if !is_word_grapheme(g) {
            break;
        }
        new_pos = start;
    }

    new_pos
}

/// Start of the next word: skip the word graphemes at `pos`, then the
/// non-word graphemes after them (the Ctrl+Right motion).
pub fn find_next_word_start_str(text: &str, pos: usize) -> usize {
    let mut new_pos = snap_pos(text, pos);

    while let Some((end, g)) = grapheme_after(text, new_pos) {
        if !is_word_grapheme(g) {
            break;
        }
        new_pos = end;
    }
    while let Some((end, g)) = grapheme_after(text, new_pos) {
        if is_word_grapheme(g) {
            break;
        }
        new_pos = end;
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
    let pos = pos.min(buffer.len());

    // Only read a small window before the cursor for efficiency
    let start = pos.saturating_sub(1000);
    let bytes = buffer.slice_bytes(start..pos);
    // A window start that splits a multi-byte character decodes to U+FFFD,
    // which is not a word character, so the scan stops before it and the
    // distance moved is measured from the end of the text.
    let text = String::from_utf8_lossy(&bytes);

    // Scan left over the word graphemes before the cursor, stopping at
    // delimiters like '.' or ':' without including them
    let mut idx = text.len();
    while let Some((prev, g)) = grapheme_before(&text, idx) {
        if !is_word_grapheme(g) {
            break;
        }
        idx = prev;
    }

    pos - (text.len() - idx)
}

/// Returns true if the grapheme at `pos` is a word character
/// (alphanumeric or `_`). Position past the end of the buffer returns false.
///
/// This is grapheme-aware (handles multi-byte UTF-8 like accented letters)
/// and is meant for "is the cursor on a word?" checks. Unlike
/// `find_word_start`/`find_word_end`, it does not extend into adjacent
/// graphemes — it only looks at the one at `pos`.
pub fn is_cursor_on_word_char(buffer: &Buffer, pos: usize) -> bool {
    let buf_len = buffer.len();
    if pos >= buf_len {
        return false;
    }
    // A UTF-8 grapheme is at most 4 bytes; read enough to decode the first one.
    const MAX_UTF8_CHAR_LEN: usize = 4;
    let end = (pos + MAX_UTF8_CHAR_LEN).min(buf_len);
    let bytes = buffer.slice_bytes(pos..end);
    let text = String::from_utf8_lossy(&bytes);
    let next = next_grapheme_boundary(&text, 0);
    if next == 0 {
        return false;
    }
    get_grapheme_class(&text[..next]) == CharClass::Word
}

/// Find the start of the word at or before the given position
///
/// Uses grapheme-based classification to correctly handle Unicode characters
/// (e.g., accented letters).
pub fn find_word_start(buffer: &Buffer, pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let buf_len = buffer.len();
    let pos = pos.min(buf_len);

    // Only read a small window around the position for efficiency.
    // Extend `end` past `pos` so we never truncate a multi-byte UTF-8 character
    // at the cursor position.
    const MAX_UTF8_CHAR_LEN: usize = 4;
    let start = pos.saturating_sub(1000);
    let end = (pos + MAX_UTF8_CHAR_LEN).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);
    let text = String::from_utf8_lossy(&bytes);

    // Map `pos` into the (possibly lossy-decoded) string. When the window
    // start splits a multi-byte sequence, `from_utf8_lossy` replaces it with a
    // 3-byte U+FFFD which shifts byte offsets. Snap to the nearest grapheme
    // boundary so we never index into the middle of a character.
    let offset = {
        let raw = (pos - start).min(text.len());
        if text.is_char_boundary(raw) {
            raw
        } else {
            next_grapheme_boundary(&text, raw)
        }
    };
    let mut current_idx = offset;

    // If we're at the end or at a non-word character, step left once
    let is_non_word = current_idx >= text.len() || {
        let next = next_grapheme_boundary(&text, current_idx);
        get_grapheme_class(&text[current_idx..next]) != CharClass::Word
    };
    if is_non_word && current_idx > 0 {
        current_idx = prev_grapheme_boundary(&text, current_idx);
    }

    // Scan left while we're on word characters
    while current_idx > 0 {
        let prev = prev_grapheme_boundary(&text, current_idx);
        if get_grapheme_class(&text[prev..current_idx]) == CharClass::Word {
            current_idx = prev;
        } else {
            break;
        }
    }

    // Convert back to absolute position
    let delta = offset - current_idx;
    pos - delta
}

/// Find the end of the word at or after the given position
///
/// Uses grapheme-based classification to correctly handle Unicode characters
/// (e.g., accented letters).
pub fn find_word_end(buffer: &Buffer, pos: usize) -> usize {
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

    // Skip non-word characters to find start of next word
    while current_idx < text.len() {
        let next = next_grapheme_boundary(&text, current_idx);
        if get_grapheme_class(&text[current_idx..next]) != CharClass::Word {
            current_idx = next;
        } else {
            break;
        }
    }

    // Consume word characters
    while current_idx < text.len() {
        let next = next_grapheme_boundary(&text, current_idx);
        if get_grapheme_class(&text[current_idx..next]) == CharClass::Word {
            current_idx = next;
        } else {
            break;
        }
    }

    start + current_idx
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

/// Find the end of the word at or after the given position
///
/// Ctrl+Right behavior:
/// - From within a word: jump to the end of the current word
/// - From whitespace: skip whitespace, then jump to end of next token (word or punctuation)
/// - From punctuation: consume all punctuation, then stop
pub fn find_word_end_right(buffer: &Buffer, pos: usize) -> usize {
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

    match start_class {
        CharClass::Word => {
            // In a word: jump to end of current word
            while current_idx < text.len() {
                let next = next_grapheme_boundary(&text, current_idx);
                let g = &text[current_idx..next];
                if get_grapheme_class(g) == CharClass::Word {
                    current_idx = next;
                } else {
                    break;
                }
            }
        }
        CharClass::Whitespace => {
            // On whitespace: skip whitespace, then consume next token
            while current_idx < text.len() {
                let next = next_grapheme_boundary(&text, current_idx);
                let g = &text[current_idx..next];
                if get_grapheme_class(g) == CharClass::Whitespace {
                    current_idx = next;
                } else {
                    break;
                }
            }
            // Consume the token we landed on (word or punctuation)
            if current_idx < text.len() {
                let next = next_grapheme_boundary(&text, current_idx);
                let landed_class = get_grapheme_class(&text[current_idx..next]);
                while current_idx < text.len() {
                    let next = next_grapheme_boundary(&text, current_idx);
                    let g = &text[current_idx..next];
                    if get_grapheme_class(g) == landed_class {
                        current_idx = next;
                    } else {
                        break;
                    }
                }
            }
        }
        CharClass::Punctuation => {
            // On punctuation: consume all punctuation, then stop
            while current_idx < text.len() {
                let next = next_grapheme_boundary(&text, current_idx);
                let g = &text[current_idx..next];
                if get_grapheme_class(g) == CharClass::Punctuation {
                    current_idx = next;
                } else {
                    break;
                }
            }
        }
    }

    start + current_idx
}

/// Find the end of the word at or after the given position (vim 'e' behavior).
///
/// Unlike `find_word_end_right` (Ctrl+Right style), this function:
/// - Lands ON the last character of the word, not past it
/// - If already at a word-end, advances to the end of the next word
///
/// Vim 'e' behavior:
/// - From 'h' in "hello world": lands on 'o' (offset 4)
/// - From 'o' in "hello world": lands on 'd' (offset 10)
/// - From space in "hello world": lands on 'd' (offset 10)
pub fn find_vi_word_end(buffer: &Buffer, pos: usize) -> usize {
    let buf_len = buffer.len();
    if pos >= buf_len {
        return buf_len;
    }

    let start = pos;
    let end = (pos + 1000).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);
    let text = String::from_utf8_lossy(&bytes);

    if text.is_empty() {
        return start;
    }

    let mut idx = 0;

    // Step 1: advance past the current character (so 'e' at word-end moves forward)
    if text.len() > 1 {
        idx = next_grapheme_boundary(&text, idx);
    }

    // Step 2: skip any whitespace
    while idx < text.len() {
        let next = next_grapheme_boundary(&text, idx);
        let g = &text[idx..next];
        if get_grapheme_class(g) == CharClass::Whitespace {
            idx = next;
        } else {
            break;
        }
    }

    // Step 3: consume word/punctuation characters of the same class
    if idx < text.len() {
        let next = next_grapheme_boundary(&text, idx);
        let target_class = get_grapheme_class(&text[idx..next]);
        while idx < text.len() {
            let next = next_grapheme_boundary(&text, idx);
            if next >= text.len() {
                break;
            }
            let next_g = &text[next..next_grapheme_boundary(&text, next).min(text.len())];
            if next_g.is_empty() {
                break;
            }
            if get_grapheme_class(next_g) == target_class {
                idx = next;
            } else {
                break;
            }
        }
    }

    start + idx
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
    fn word_char_neighbours_decode_multibyte_characters() {
        let text = "aé日 -";
        let buffer = Buffer::from_str_test(text);
        let e = text.find('é').unwrap();
        let cjk = text.find('日').unwrap();
        assert!(is_word_char_at(&buffer, e));
        assert!(is_word_char_before(&buffer, cjk));
        assert!(is_word_char_before(&buffer, cjk + '日'.len_utf8()));
        assert!(!is_word_char_at(&buffer, text.find(' ').unwrap()));
        assert!(!is_word_char_at(&buffer, text.find('-').unwrap()));
        assert!(!is_word_char_before(&buffer, 0));
        assert!(!is_word_char_at(&buffer, text.len()));
    }

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
    fn test_is_cursor_on_word_char() {
        let buffer = Buffer::from_str_test("hello {world} café");
        // 'h' of hello
        assert!(is_cursor_on_word_char(&buffer, 0));
        // 'o' of hello
        assert!(is_cursor_on_word_char(&buffer, 4));
        // space
        assert!(!is_cursor_on_word_char(&buffer, 5));
        // '{'
        assert!(!is_cursor_on_word_char(&buffer, 6));
        // 'w' of world
        assert!(is_cursor_on_word_char(&buffer, 7));
        // '}'
        assert!(!is_cursor_on_word_char(&buffer, 12));
        // first byte of multi-byte 'é' in "café"
        assert!(is_cursor_on_word_char(&buffer, 17));
        // past the end
        assert!(!is_cursor_on_word_char(&buffer, buffer.len()));
        assert!(!is_cursor_on_word_char(&buffer, buffer.len() + 100));
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
    // Tests for string word navigation
    // ========================================================================

    #[test]
    fn test_find_word_start_str_basic() {
        let s = "hello world test";
        assert_eq!(find_word_start_str(s, 0), 0); // Start of "hello"
        assert_eq!(find_word_start_str(s, 3), 0); // Middle of "hello"
        assert_eq!(find_word_start_str(s, 5), 0); // End of "hello"
        assert_eq!(find_word_start_str(s, 6), 6); // Start of "world"
        assert_eq!(find_word_start_str(s, 8), 6); // Middle of "world"
        assert_eq!(find_word_start_str(s, 11), 6); // End of "world"
        assert_eq!(find_word_start_str(s, 12), 12); // Start of "test"
    }

    #[test]
    fn test_find_word_end_str_basic() {
        let s = "hello world test";
        assert_eq!(find_word_end_str(s, 0), 5); // End of "hello"
        assert_eq!(find_word_end_str(s, 3), 5); // Middle of "hello"
        assert_eq!(find_word_end_str(s, 6), 11); // End of "world"
        assert_eq!(find_word_end_str(s, 8), 11); // Middle of "world"
        assert_eq!(find_word_end_str(s, 12), 16); // End of "test"
    }

    #[test]
    fn test_find_word_start_str_special_chars() {
        let s = "save-file-as";
        assert_eq!(find_word_start_str(s, 4), 0); // "save"
        assert_eq!(find_word_start_str(s, 5), 5); // hyphen stops word
        assert_eq!(find_word_start_str(s, 9), 5); // "file"
        assert_eq!(find_word_start_str(s, 10), 10); // hyphen stops word
        assert_eq!(find_word_start_str(s, 12), 10); // "as"
    }

    #[test]
    fn test_find_word_end_str_special_chars() {
        let s = "open.file.now";
        assert_eq!(find_word_end_str(s, 0), 4); // "open"
        assert_eq!(find_word_end_str(s, 4), 9); // skip '.', then "file"
        assert_eq!(find_word_end_str(s, 5), 9); // "file"
        assert_eq!(find_word_end_str(s, 10), 13); // "now"
    }

    #[test]
    fn test_find_word_start_str_whitespace() {
        let s = "  hello  world  ";
        assert_eq!(find_word_start_str(s, 4), 2); // "hello"
        assert_eq!(find_word_start_str(s, 7), 2); // After "hello"
        assert_eq!(find_word_start_str(s, 9), 9); // "world"
        assert_eq!(find_word_start_str(s, 14), 9); // After "world"
    }

    #[test]
    fn test_find_word_end_str_whitespace() {
        let s = "  hello  world  ";
        assert_eq!(find_word_end_str(s, 0), 7); // Skip spaces, end of "hello"
        assert_eq!(find_word_end_str(s, 2), 7); // End of "hello"
        assert_eq!(find_word_end_str(s, 7), 14); // Skip spaces, end of "world"
        assert_eq!(find_word_end_str(s, 9), 14); // End of "world"
    }

    #[test]
    fn test_find_word_start_str_edge_cases() {
        // Empty string
        assert_eq!(find_word_start_str("", 0), 0);

        // Single character
        assert_eq!(find_word_start_str("a", 0), 0);
        assert_eq!(find_word_start_str("a", 1), 0);

        // No words (all special chars) - scans back but finds no word
        assert_eq!(find_word_start_str("...", 2), 1);

        // Position beyond string length
        assert_eq!(find_word_start_str("hello", 100), 0);
    }

    #[test]
    fn test_find_word_end_str_edge_cases() {
        // Empty string
        assert_eq!(find_word_end_str("", 0), 0);

        // Single character
        assert_eq!(find_word_end_str("a", 0), 1);

        // No words (all special chars)
        assert_eq!(find_word_end_str("...", 0), 3);

        // Position beyond string length
        assert_eq!(find_word_end_str("hello", 100), 5);
    }

    #[test]
    fn test_find_word_start_str_underscores() {
        let s = "some_variable_name";
        assert_eq!(find_word_start_str(s, 7), 0); // Underscores are word chars
        assert_eq!(find_word_start_str(s, 18), 0);
    }

    #[test]
    fn test_find_word_end_str_underscores() {
        let s = "some_variable_name";
        assert_eq!(find_word_end_str(s, 0), 18); // Underscores are word chars
        assert_eq!(find_word_end_str(s, 7), 18);
    }

    #[test]
    fn test_completion_word_start_non_ascii_partial_word() {
        let text = "args.αβ";
        let buffer = Buffer::from_str_test(text);
        assert_eq!(find_completion_word_start(&buffer, text.len()), 5);
        assert_eq!(find_completion_word_start(&buffer, 5), 5);
    }

    #[test]
    fn test_word_str_non_ascii_words() {
        let s = "αβγ δεζ";
        assert_eq!(find_word_start_str(s, 13), 7);
        assert_eq!(find_word_start_str(s, 6), 0);
        assert_eq!(find_word_end_str(s, 0), 6);
        assert_eq!(find_word_end_str(s, 6), 13);
        assert_eq!(find_prev_word_start_str(s, 13), 7);
        assert_eq!(find_prev_word_start_str(s, 7), 0);
        assert_eq!(find_next_word_start_str(s, 0), 7);
        assert_eq!(find_next_word_start_str(s, 7), 13);
    }

    #[test]
    fn test_word_str_keeps_combining_marks_whole() {
        // Decomposed "café": the acute accent (2 bytes) is part of the word.
        let s = "cafe\u{301} x";
        let word_end = "cafe\u{301}".len();
        assert_eq!(find_word_end_str(s, 0), word_end);
        assert_eq!(find_word_start_str(s, word_end), 0);
        assert_eq!(find_next_word_start_str(s, 0), word_end + 1);
        assert_eq!(find_prev_word_start_str(s, word_end), 0);
        // A position inside the cluster snaps to its start.
        assert_eq!(find_word_end_str(s, 4), word_end);
    }

    #[test]
    fn test_word_str_positions_never_split_graphemes() {
        let s = "a 👍🏽 e\u{301}x";
        for pos in 0..=s.len() + 2 {
            for f in [
                find_word_start_str,
                find_word_end_str,
                find_prev_word_start_str,
                find_next_word_start_str,
            ] {
                let r = f(s, pos);
                assert_eq!(snap_to_grapheme_boundary(s, r), r, "pos {pos}");
            }
        }
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
            /// Property: find_word_start_str should always return a position <= input position
            #[test]
            fn prop_word_start_not_after_position(s in ascii_string(), pos in 0usize..100) {
                let result = find_word_start_str(&s, pos);
                prop_assert!(result <= pos.min(s.len()));
            }

            /// Property: find_word_end_str should always return a position >= input position
            #[test]
            fn prop_word_end_not_before_position(s in ascii_string(), pos in 0usize..100) {
                let result = find_word_end_str(&s, pos);
                prop_assert!(result >= pos.min(s.len()));
            }

            /// Property: find_word_end_str should never exceed string length
            #[test]
            fn prop_word_end_within_bounds(s in ascii_string(), pos in 0usize..100) {
                let result = find_word_end_str(&s, pos);
                prop_assert!(result <= s.len());
            }

            /// Property: find_word_start_str at position 0 should return 0
            #[test]
            fn prop_word_start_at_zero(s in ascii_string()) {
                let result = find_word_start_str(&s, 0);
                prop_assert_eq!(result, 0);
            }

            /// Property: find_word_end_str at end should return end
            #[test]
            fn prop_word_end_at_end(s in ascii_string()) {
                let result = find_word_end_str(&s, s.len());
                prop_assert_eq!(result, s.len());
            }

            /// Property: Applying find_word_start_str should move towards the start
            #[test]
            fn prop_word_start_monotonic(s in ascii_string(), pos in 0usize..100) {
                let first = find_word_start_str(&s, pos);
                let second = find_word_start_str(&s, first);
                // Second application should not move forward
                prop_assert!(second <= first);
            }

            /// Property: The result should always be at a word boundary or start
            #[test]
            fn prop_word_start_at_boundary(s in ascii_string(), pos in 0usize..100) {
                let result = find_word_start_str(&s, pos.min(s.len()));

                // Either at start of string, or previous char is not a word char
                prop_assert!(
                    result == 0 ||
                    !is_word_char(s.as_bytes()[result - 1])
                );
            }

            /// Property: Word start and end should define a valid range
            #[test]
            fn prop_word_range_valid(s in ascii_string(), pos in 0usize..100) {
                let pos = pos.min(s.len());
                let start = find_word_start_str(&s, pos);
                let end = find_word_end_str(&s, pos);

                // Start should be <= pos, end should be >= pos
                prop_assert!(start <= pos);
                prop_assert!(end >= pos);
                // Start should be <= end (forms valid range)
                prop_assert!(start <= end);
            }
        }
    }
}
