//! The search every buffer search uses: [`build_search_regex`] builds the
//! regex for a query, and a [`RegexSearch`] finds its matches in a
//! `TextBuffer`, a step at a time if need be.
use crate::model::filesystem::SearchMatch;
use regex::bytes::Regex;
use std::ops::Range;

use super::{TextBuffer, LOAD_CHUNK_SIZE};

// ---------------------------------------------------------------------------
// RegexSearch
// ---------------------------------------------------------------------------

/// The one regex every search builds from a query: literal text is
/// escaped, whole words are `\b(?:…)\b`, `^` and `$` match at line breaks
/// (`\n` or `\r\n`), and a pattern holding a newline lets `.` match one.
pub fn build_search_regex(
    query: &str,
    use_regex: bool,
    whole_word: bool,
    case_sensitive: bool,
) -> Result<Regex, regex::Error> {
    let pattern = if use_regex {
        query.to_string()
    } else {
        regex::escape(query)
    };
    let pattern = if whole_word {
        format!(r"\b(?:{pattern})\b")
    } else {
        pattern
    };
    regex::bytes::RegexBuilder::new(&pattern)
        .case_insensitive(!case_sensitive)
        .multi_line(true)
        .crlf(true)
        .dot_matches_new_line(use_regex && query.contains('\n'))
        .build()
}

/// How far each window of a [`RegexSearch`] for `query` must reach back
/// into the last so a match straddling the boundary is found: a plain-text
/// match is at most 4 bytes per query character (case folding can change a
/// character's UTF-8 length — `k` and the Kelvin sign are 1 and 3 bytes),
/// and a regex match gets 4 KB.
pub fn match_overlap(query: &str, use_regex: bool) -> usize {
    if use_regex {
        4096
    } else {
        query.chars().count() * 4
    }
}

/// Bytes read on each side of a window so `\b`, `^`, `$` and look-around
/// judge a match by the text around it (`\r\n` and a UTF-8 character fit).
const CONTEXT: usize = 4;

/// A search of a `TextBuffer` for every match of a regex in a range — in
/// order and never overlapping, as `find_iter` over the text would give —
/// that can run a step at a time, so a large buffer is searched without
/// stalling the editor.
///
/// It reads through `TextBuffer::get_text_range_mut`, a window at a time,
/// so a lazily loaded file is loaded as the search reaches each part and
/// kept like any other read of it. The bytes just outside each window are
/// read too, and a match running to a window's end is read again with a
/// bigger window. Each window reaches `overlap` bytes back into the last,
/// so a match straddling the boundary is found: at least the longest match
/// expected.
///
/// An empty match at the end of the range counts only where the range
/// ends the buffer, so a search up to an earlier match does not find it.
#[derive(Debug)]
pub struct RegexSearch {
    regex: Regex,
    end: usize,
    to_buffer_end: bool,
    overlap: usize,
    base_window: usize,
    window: usize,
    /// Where the next match may start.
    pos: usize,
    /// End of the last match, so no empty match is taken there.
    last_end: Option<usize>,
    lines: Option<Lines>,
    done: bool,
}

/// Line counting for matches: the line and line start at `counted_to`.
#[derive(Debug)]
struct Lines {
    line: usize,
    line_start: usize,
    counted_to: usize,
}

impl RegexSearch {
    /// A search of `range` of `buffer` (clamped to it).
    pub fn new(regex: Regex, range: Range<usize>, overlap: usize, buffer: &TextBuffer) -> Self {
        let len = buffer.len();
        let end = range.end.min(len);
        Self {
            regex,
            end,
            to_buffer_end: end == len,
            overlap,
            base_window: LOAD_CHUNK_SIZE,
            window: LOAD_CHUNK_SIZE,
            pos: range.start.min(end),
            last_end: None,
            lines: None,
            done: false,
        }
    }

    /// Give each match its 1-based line and column and the text of its
    /// line, counting from `first_line`, the line the range starts on.
    pub fn with_lines(mut self, first_line: usize) -> Self {
        self.lines = Some(Lines {
            line: first_line,
            line_start: self.pos,
            counted_to: self.pos,
        });
        self
    }

    /// Read `window` bytes at a time instead of `LOAD_CHUNK_SIZE`.
    pub fn with_window(mut self, window: usize) -> Self {
        self.base_window = window.max(1);
        self.window = self.base_window;
        self
    }

    pub fn is_done(&self) -> bool {
        self.done
    }

    /// Where the search has got to, for progress.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Search on until `limit` more matches are found, about `max_bytes`
    /// have been read, or the range is done; returns the matches found.
    /// `line`, `column` and `context` are set only `with_lines`.
    pub fn step(
        &mut self,
        buffer: &mut TextBuffer,
        max_bytes: usize,
        limit: usize,
    ) -> anyhow::Result<Vec<SearchMatch>> {
        let mut found = Vec::new();
        let mut read = 0usize;
        let len = buffer.len();
        let end = self.end.min(len);
        'windows: while !self.done && found.len() < limit && read < max_bytes {
            if self.pos > end || (self.pos == end && !self.to_buffer_end) {
                self.done = true;
                break;
            }
            let read_from = self.pos.saturating_sub(CONTEXT);
            let window_end = self.pos.saturating_add(self.window).min(end);
            let read_to = (window_end + CONTEXT).min(len);
            let bytes = buffer.get_text_range_mut(read_from, read_to - read_from)?;
            read += bytes.len();
            let mut at = self.pos;
            while at <= read_to {
                let Some(m) = self.regex.find_at(&bytes, at - read_from) else {
                    break;
                };
                let (start, match_end) = (read_from + m.start(), read_from + m.end());
                if start > end || match_end > end || (start == end && !self.to_buffer_end) {
                    self.done = true;
                    break 'windows;
                }
                if window_end < end && match_end + CONTEXT > read_to {
                    // It may go on past this window: read again from it,
                    // with more room if it already starts the window.
                    if start == self.pos {
                        self.window = self.window.saturating_mul(2);
                    }
                    self.count_lines(&bytes, read_from, start);
                    self.pos = start;
                    continue 'windows;
                }
                // As `find_iter`: no empty match where the last match ended.
                if start == match_end && self.last_end == Some(start) {
                    at = start + 1;
                    continue;
                }
                found.push(self.match_at(&bytes, read_from, start, match_end));
                self.last_end = Some(match_end);
                at = if start == match_end {
                    match_end + 1
                } else {
                    match_end
                };
                self.count_lines(&bytes, read_from, at.min(read_to));
                self.pos = at;
                if found.len() >= limit {
                    break 'windows;
                }
            }
            if window_end >= end {
                self.done = true;
                break;
            }
            // Always forward, even for an `overlap` as long as a window.
            let next = self
                .pos
                .max(window_end.saturating_sub(self.overlap).max(self.pos + 1));
            self.count_lines(&bytes, read_from, next);
            self.pos = next;
            self.window = self.base_window;
        }
        Ok(found)
    }

    /// Run the search to the end, or until `limit` matches.
    pub fn run(
        &mut self,
        buffer: &mut TextBuffer,
        limit: usize,
    ) -> anyhow::Result<Vec<SearchMatch>> {
        self.step(buffer, usize::MAX, limit)
    }

    /// Count the line breaks in `bytes` (read from `read_from`) up to `to`.
    fn count_lines(&mut self, bytes: &[u8], read_from: usize, to: usize) {
        let Some(lines) = &mut self.lines else {
            return;
        };
        if to <= lines.counted_to {
            return;
        }
        let from = lines.counted_to - read_from;
        for (i, &b) in bytes[from..to - read_from].iter().enumerate() {
            if b == b'\n' {
                lines.line += 1;
                lines.line_start = lines.counted_to + i + 1;
            }
        }
        lines.counted_to = to;
    }

    fn match_at(
        &mut self,
        bytes: &[u8],
        read_from: usize,
        start: usize,
        end: usize,
    ) -> SearchMatch {
        let mut found = SearchMatch {
            byte_offset: start,
            length: end - start,
            line: 0,
            column: 0,
            context: String::new(),
        };
        self.count_lines(bytes, read_from, start);
        if let Some(lines) = &self.lines {
            found.line = lines.line;
            found.column = start - lines.line_start + 1;
            let from = lines.line_start.max(read_from) - read_from;
            let to = bytes[start - read_from..]
                .iter()
                .position(|&b| b == b'\n')
                .map_or(bytes.len(), |p| start - read_from + p);
            let line = &bytes[from..to];
            found.context =
                String::from_utf8_lossy(line.strip_suffix(b"\r").unwrap_or(line)).into_owned();
        }
        found
    }
}
