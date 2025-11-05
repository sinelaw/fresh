//! Word boundary detection and navigation helpers

use crate::buffer::Buffer;

/// Check if a byte is a word character (alphanumeric or underscore)
pub fn is_word_char(byte: u8) -> bool {
    byte.is_ascii_alphanumeric() || byte == b'_'
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
    if new_pos >= bytes.len() || (bytes.get(new_pos).map(|&b| !is_word_char(b)).unwrap_or(true)) {
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
pub fn find_word_start(buffer: &Buffer, pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let buf_len = buffer.len();

    // Clamp position to buffer length
    let pos = pos.min(buf_len);

    // Only read a small window around the position for efficiency
    let start = pos.saturating_sub(1000);
    // Read one extra byte to include the character AT pos (if it exists)
    let end = (pos + 1).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);
    let offset = pos - start;

    let mut new_pos = offset;

    // If we're at the end of the buffer or at a non-word character, scan left
    if new_pos >= bytes.len() || (bytes.get(new_pos).map(|&b| !is_word_char(b)).unwrap_or(true)) {
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

    start + new_pos
}

/// Find the end of the word at or after the given position
pub fn find_word_end(buffer: &Buffer, pos: usize) -> usize {
    let buf_len = buffer.len();
    if pos >= buf_len {
        return buf_len;
    }

    // Only read a small window around the position for efficiency
    let start = pos;
    let end = (pos + 1000).min(buf_len);
    let bytes = buffer.slice_bytes(start..end);

    let mut new_pos = 0;

    // Find end of current word
    while new_pos < bytes.len() {
        if let Some(&byte) = bytes.get(new_pos) {
            if !is_word_char(byte) {
                break;
            }
            new_pos += 1;
        } else {
            break;
        }
    }

    start + new_pos
}

/// Find the start of the word to the left of the given position
pub fn find_word_start_left(buffer: &Buffer, pos: usize) -> usize {
    if pos == 0 {
        return 0;
    }

    let buf_len = buffer.len();
    let actual_pos = pos.min(buf_len);

    // Only read a small window around the position for efficiency
    let start = actual_pos.saturating_sub(1000);
    let end = actual_pos;
    let bytes = buffer.slice_bytes(start..end);

    let mut new_pos = bytes.len().saturating_sub(1);

    // Skip non-word characters (whitespace and punctuation)
    while new_pos > 0 && bytes.get(new_pos).is_some_and(|&b| !is_word_char(b)) {
        new_pos = new_pos.saturating_sub(1);
    }

    // Find start of word
    while new_pos > 0 {
        let prev_byte = bytes.get(new_pos.saturating_sub(1));
        let curr_byte = bytes.get(new_pos);

        match (prev_byte, curr_byte) {
            (Some(&prev), Some(&curr)) => {
                if is_word_char(prev) != is_word_char(curr) {
                    break;
                }
                new_pos = new_pos.saturating_sub(1);
            }
            _ => break,
        }
    }

    start + new_pos
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

    let mut new_pos = 0;

    // Skip current word
    while new_pos < bytes.len() && bytes.get(new_pos).is_some_and(|&b| is_word_char(b)) {
        new_pos += 1;
    }

    // Skip non-word characters (whitespace and punctuation)
    while new_pos < bytes.len() && bytes.get(new_pos).is_some_and(|&b| !is_word_char(b)) {
        new_pos += 1;
    }

    start + new_pos
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;

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
        let buffer = Buffer::from_str("hello world test");
        assert_eq!(find_word_start(&buffer, 0), 0); // Start of "hello"
        assert_eq!(find_word_start(&buffer, 3), 0); // Middle of "hello"
        assert_eq!(find_word_start(&buffer, 6), 6); // Start of "world"
        assert_eq!(find_word_start(&buffer, 8), 6); // Middle of "world"
    }

    #[test]
    fn test_find_word_end() {
        let buffer = Buffer::from_str("hello world test");
        assert_eq!(find_word_end(&buffer, 0), 5); // End of "hello"
        assert_eq!(find_word_end(&buffer, 3), 5); // Middle of "hello"
        assert_eq!(find_word_end(&buffer, 6), 11); // End of "world"
    }

    #[test]
    fn test_find_word_start_left() {
        let buffer = Buffer::from_str("hello world test");
        assert_eq!(find_word_start_left(&buffer, 6), 0); // From "world" to "hello"
        assert_eq!(find_word_start_left(&buffer, 12), 6); // From "test" to "world"
    }

    #[test]
    fn test_find_word_start_right() {
        let buffer = Buffer::from_str("hello world test");
        assert_eq!(find_word_start_right(&buffer, 0), 6); // From "hello" to "world"
        assert_eq!(find_word_start_right(&buffer, 6), 12); // From "world" to "test"
    }
}
