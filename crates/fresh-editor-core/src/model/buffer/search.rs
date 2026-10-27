//! The search every search uses. A [`SearchQuery`] is a query compiled
//! once — its regex and how far a search must look back at window edges —
//! and a [`RegexSearch`] finds its matches in a `TextBuffer`, a step at a
//! time if need be; a [`BufferSearch`] runs one a time slice at a time on a
//! buffer that may be edited meanwhile. [`expand_replacement`] turns a
//! match into its replacement text.
use regex::bytes::Regex;
use std::ops::Range;
use std::time::Instant;

use super::{TextBuffer, LOAD_CHUNK_SIZE};

/// A single search match with position and context.
#[derive(Clone, Debug)]
pub struct SearchMatch {
    /// Byte offset of the match in the file/buffer.
    pub byte_offset: usize,
    /// Length of the match in bytes.
    pub length: usize,
    /// 1-based line number.
    pub line: usize,
    /// 1-based byte column within the line.
    pub column: usize,
    /// Content of the line containing the match (no trailing newline).
    pub context: String,
}

/// A query compiled for searching: the regex every search builds for it,
/// and how far each window of a search must reach back into the last so a
/// match straddling the boundary is found.
#[derive(Clone, Debug)]
pub struct SearchQuery {
    pub regex: Regex,
    pub overlap: usize,
}

impl SearchQuery {
    /// `query` searched as a regex (`use_regex`) or as literal text, as a
    /// whole word or not, with or without case. Literal text is escaped,
    /// whole words are `\b(?:…)\b`, `^` and `$` match at line breaks (`\n`
    /// or `\r\n`), and a regex holding a newline lets `.` match one.
    pub fn new(
        query: &str,
        use_regex: bool,
        whole_word: bool,
        case_sensitive: bool,
    ) -> Result<Self, regex::Error> {
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
        let regex = regex::bytes::RegexBuilder::new(&pattern)
            .case_insensitive(!case_sensitive)
            .multi_line(true)
            .crlf(true)
            .dot_matches_new_line(use_regex && query.contains('\n'))
            .build()?;
        // A plain-text match is at most 4 bytes per query character (case
        // folding can change a character's UTF-8 length — `k` and the
        // Kelvin sign are 1 and 3 bytes); a regex match gets 4 KB.
        let overlap = if use_regex {
            4096
        } else {
            query.chars().count() * 4
        };
        Ok(Self { regex, overlap })
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
    /// The buffer version the search is of; stepping it after an edit is
    /// an error, since its positions are for the text before.
    version: u64,
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
    /// A search of `range` of `buffer` (clamped to it) for `query`.
    pub fn new(query: &SearchQuery, range: Range<usize>, buffer: &TextBuffer) -> Self {
        let len = buffer.len();
        let end = range.end.min(len);
        Self {
            regex: query.regex.clone(),
            version: buffer.version(),
            end,
            to_buffer_end: end == len,
            overlap: query.overlap,
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
    /// `line`, `column` and `context` are set only `with_lines`. An error
    /// if `buffer` was edited since the search began.
    pub fn step(
        &mut self,
        buffer: &mut TextBuffer,
        max_bytes: usize,
        limit: usize,
    ) -> anyhow::Result<Vec<SearchMatch>> {
        anyhow::ensure!(
            buffer.version() == self.version,
            "the buffer was edited during the search"
        );
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

/// A [`RegexSearch`] for up to `limit` matches, run a time slice at a time
/// on a buffer that may be edited between slices: an edit starts it over,
/// so its matches are always for the buffer as it is.
#[derive(Debug)]
pub struct BufferSearch {
    query: SearchQuery,
    range: Option<Range<usize>>,
    lines: bool,
    limit: usize,
    search: RegexSearch,
    found: Vec<SearchMatch>,
}

impl BufferSearch {
    /// A search of `range` of `buffer` (all of it if `None`).
    pub fn new(
        query: SearchQuery,
        range: Option<Range<usize>>,
        limit: usize,
        buffer: &TextBuffer,
    ) -> Self {
        let search = Self::search(&query, &range, false, buffer);
        Self {
            query,
            range,
            lines: false,
            limit,
            search,
            found: Vec::new(),
        }
    }

    /// Give each match its line, column and line text (see
    /// [`RegexSearch::with_lines`]); for a search of the whole buffer.
    pub fn with_lines(mut self, buffer: &TextBuffer) -> Self {
        self.lines = true;
        self.search = Self::search(&self.query, &self.range, true, buffer);
        self
    }

    fn search(
        query: &SearchQuery,
        range: &Option<Range<usize>>,
        lines: bool,
        buffer: &TextBuffer,
    ) -> RegexSearch {
        let range = range.clone().unwrap_or(0..buffer.len());
        let search = RegexSearch::new(query, range, buffer);
        if lines {
            search.with_lines(1)
        } else {
            search
        }
    }

    /// Search on until `deadline`, or to the end without one.
    pub fn run(
        &mut self,
        buffer: &mut TextBuffer,
        deadline: Option<Instant>,
    ) -> anyhow::Result<()> {
        if buffer.version() != self.search.version {
            self.found.clear();
            self.search = Self::search(&self.query, &self.range, self.lines, buffer);
        }
        while !self.is_done() {
            let found = self
                .search
                .step(buffer, LOAD_CHUNK_SIZE, self.limit - self.found.len())?;
            self.found.extend(found);
            if deadline.is_some_and(|d| Instant::now() >= d) {
                break;
            }
        }
        Ok(())
    }

    /// Searched to the end, or `limit` matches found.
    pub fn is_done(&self) -> bool {
        self.search.is_done() || self.found.len() >= self.limit
    }

    pub fn found(&self) -> &[SearchMatch] {
        &self.found
    }

    pub fn into_found(self) -> Vec<SearchMatch> {
        self.found
    }

    /// How far through its range the search is, 0–100.
    pub fn progress_percent(&self, buffer_len: usize) -> usize {
        let range = self.range.clone().unwrap_or(0..buffer_len);
        (self.search.position().saturating_sub(range.start) * 100)
            .checked_div(range.len())
            .unwrap_or(100)
            .min(100)
    }
}

/// The replacement for the match of `regex` at `start..start + len` of
/// `haystack`, a template with `$1`, `${name}` and, unlike the regex
/// crate's own expansion, `\n`, `\t`, `\r` and `\\`. The groups are found
/// by matching again at `start` with the bytes around it, so `\b`, `^` and
/// look-around judge the match as the search did; a match it cannot find
/// again leaves the template's group references unexpanded.
pub fn expand_replacement(
    regex: &Regex,
    haystack: &[u8],
    start: usize,
    len: usize,
    template: &str,
) -> String {
    let escaped = interpret_escapes(template);
    match regex.captures_at(haystack, start) {
        Some(caps)
            if caps
                .get(0)
                .is_some_and(|m| m.start() == start && m.len() == len) =>
        {
            let mut dst = Vec::new();
            caps.expand(normalize_replacement(&escaped).as_bytes(), &mut dst);
            String::from_utf8_lossy(&dst).into_owned()
        }
        _ => escaped,
    }
}

/// Interpret the common backslash escapes in a replacement template:
/// `\n`, `\t`, `\r` become their control-character equivalents and `\\`
/// becomes a single backslash. Unknown escapes (e.g. `\q`) are preserved
/// verbatim so users who didn't mean to type an escape sequence don't get
/// surprising behaviour. A trailing lone backslash is likewise passed
/// through unchanged.
fn interpret_escapes(template: &str) -> String {
    let mut out = String::with_capacity(template.len());
    let mut chars = template.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('t') => out.push('\t'),
            Some('r') => out.push('\r'),
            Some('\\') => out.push('\\'),
            Some(other) => {
                out.push('\\');
                out.push(other);
            }
            None => out.push('\\'),
        }
    }
    out
}

/// Normalize `$N` capture references to `${N}` so the regex crate doesn't
/// greedily consume trailing letters as part of the group name.
/// E.g. `oo$1oo` → `oo${1}oo`, matching Python/PCRE semantics.
fn normalize_replacement(template: &str) -> String {
    let mut out = String::with_capacity(template.len());
    let bytes = template.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'$' && i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
            let start = i + 1;
            let mut end = start;
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            out.push_str("${");
            out.push_str(&template[start..end]);
            out.push('}');
            i = end;
        } else {
            let ch = template[i..].chars().next().unwrap();
            out.push(ch);
            i += ch.len_utf8();
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn build_search_regex(
        query: &str,
        use_regex: bool,
        whole_word: bool,
        case_sensitive: bool,
    ) -> Result<Regex, String> {
        SearchQuery::new(query, use_regex, whole_word, case_sensitive)
            .map(|q| q.regex)
            .map_err(|e| e.to_string())
    }

    /// The replacement for a match that is the whole of `matched`.
    fn expand(regex: &Regex, matched: &[u8], template: &str) -> String {
        expand_replacement(regex, matched, 0, matched.len(), template)
    }

    #[test]
    fn build_search_regex_plain_text_escapes_special_chars() {
        // Non-regex mode: "a.b" should match "a.b" literally, not "a?b".
        let re = build_search_regex("a.b", false, false, true).unwrap();
        assert!(re.is_match(b"a.b"));
        assert!(!re.is_match(b"axb"));
    }

    #[test]
    fn build_search_regex_regex_mode_treats_dot_as_wildcard() {
        let re = build_search_regex("a.b", true, false, true).unwrap();
        assert!(re.is_match(b"axb"));
        assert!(re.is_match(b"a.b"));
    }

    #[test]
    fn build_search_regex_whole_word_wraps_pattern() {
        let re = build_search_regex("foo", false, true, true).unwrap();
        assert!(re.is_match(b"foo bar"));
        assert!(!re.is_match(b"foobar"));
    }

    /// Issue #3575: plain text with case folding or whole word gets a regex
    /// over the escaped text, so replace matches what search highlights.
    #[test]
    fn build_regex_plain_text_honors_case_and_whole_word() {
        let re = build_search_regex("a.b", false, false, false).unwrap();
        assert!(re.is_match(b"A.B"));
        assert!(!re.is_match(b"axb"), "plain text must stay literal");

        let re = build_search_regex("cat", false, true, true).unwrap();
        assert!(re.is_match(b"a cat."));
        assert!(!re.is_match(b"concat"));
    }

    #[test]
    fn build_search_regex_case_insensitive_when_flag_off() {
        let re = build_search_regex("Hello", false, false, false).unwrap();
        assert!(re.is_match(b"HELLO"));
        assert!(re.is_match(b"hello"));
    }

    #[test]
    fn build_search_regex_reports_invalid_pattern_in_regex_mode() {
        let err = build_search_regex("[unclosed", true, false, true).unwrap_err();
        assert!(!err.is_empty());
    }

    #[test]
    fn build_search_regex_plain_mode_never_fails_even_on_regex_metachars() {
        // Plain-text mode escapes everything, so even a syntactically-bad
        // regex pattern compiles fine as a literal search.
        assert!(build_search_regex("[unclosed", false, false, true).is_ok());
    }

    #[test]
    fn build_search_regex_caret_anchors_every_line() {
        // Regression: `^use` must match the start of *every* line, not just
        // the start of the buffer. Without multi-line mode this finds 1.
        let re = build_search_regex("^use", true, false, true).unwrap();
        let text = b"use a;\nuse b;\nlet use_x = 1;\nuse c;\n";
        assert_eq!(re.find_iter(text).count(), 3);
    }

    #[test]
    fn build_search_regex_dollar_anchors_every_line() {
        let re = build_search_regex("foo$", true, false, true).unwrap();
        let text = b"foo\nfoobar\nbar foo\n";
        assert_eq!(re.find_iter(text).count(), 2);
    }

    #[test]
    fn build_search_regex_anchors_match_crlf_lines() {
        // On CRLF buffers `$` must match before `\r\n`, so `foo$` still hits.
        let re = build_search_regex("foo$", true, false, true).unwrap();
        let text = b"foo\r\nbar\r\nfoo\r\n";
        assert_eq!(re.find_iter(text).count(), 2);
    }

    #[test]
    fn build_search_regex_dot_does_not_cross_newlines() {
        // Multi-line mode only re-anchors `^`/`$`; `.` must still stop at a
        // line boundary (no `dot_matches_new_line`). So `a.b` cannot span
        // "a\nb", but a greedy `a.*` stays within its line.
        let re = build_search_regex("a.b", true, false, true).unwrap();
        assert!(!re.is_match(b"a\nb"));
        let re2 = build_search_regex("a.*", true, false, true).unwrap();
        let m = re2.find(b"axy\nbcd").unwrap();
        assert_eq!(m.as_bytes(), b"axy");
    }

    #[test]
    fn build_regex_caret_anchors_every_line() {
        // The bytes builder used for replace must anchor per line too.
        let re = build_search_regex("^use", true, false, true).unwrap();
        let text = b"use a;\nuse b;\nuse c;\n";
        assert_eq!(re.find_iter(text).count(), 3);
    }

    #[test]
    fn build_regex_basic_pattern() {
        let re = build_search_regex("foo.*bar", true, false, true).unwrap();
        assert!(re.is_match(b"foo123bar"));
        assert!(!re.is_match(b"baz"));
    }

    #[test]
    fn build_regex_case_insensitive() {
        let re = build_search_regex("hello", true, false, false).unwrap();
        assert!(re.is_match(b"HELLO"));
        assert!(re.is_match(b"hello"));
    }

    #[test]
    fn build_regex_whole_word() {
        let re = build_search_regex("foo", true, true, true).unwrap();
        assert!(re.is_match(b"foo bar"));
        assert!(!re.is_match(b"foobar"));
    }

    /// Whole word applies to the whole pattern, not just its first and
    /// last alternatives.
    #[test]
    fn build_search_regex_whole_word_groups_alternation() {
        let re = build_search_regex("cat|dog", true, true, true).unwrap();
        assert!(re.is_match(b"a dog."));
        assert!(!re.is_match(b"catalog"));
        assert!(!re.is_match(b"hotdog"));
    }

    #[test]
    fn expand_replacement_with_groups() {
        let re = build_search_regex(r"(\d+)-(\d+)", true, false, true).unwrap();
        let matched = b"123-456";
        let result = expand(&re, matched, "$2/$1");
        assert_eq!(result, "456/123");
    }

    #[test]
    fn expand_replacement_no_groups() {
        let re = build_search_regex("hello", true, false, true).unwrap();
        let matched = b"hello";
        let result = expand(&re, matched, "world");
        assert_eq!(result, "world");
    }

    #[test]
    fn normalize_braces_numeric_groups() {
        assert_eq!(normalize_replacement("$1"), "${1}");
        assert_eq!(normalize_replacement("$12"), "${12}");
        assert_eq!(normalize_replacement("oo$1oo"), "oo${1}oo");
        assert_eq!(normalize_replacement("$1-$2"), "${1}-${2}");
        // Already braced → unchanged
        assert_eq!(normalize_replacement("${1}oo"), "${1}oo");
        // No group ref → unchanged
        assert_eq!(normalize_replacement("hello"), "hello");
        // Named group → unchanged
        assert_eq!(normalize_replacement("$name"), "$name");
        // Literal $$ → passed through ($ not followed by digit)
        assert_eq!(normalize_replacement("$$"), "$$");
    }

    #[test]
    fn normalize_preserves_non_ascii_text() {
        assert_eq!(normalize_replacement("、"), "、");
        assert_eq!(normalize_replacement("ほげ$1ふが"), "ほげ${1}ふが");
        assert_eq!(normalize_replacement("café $1 🎉"), "café ${1} 🎉");
    }

    #[test]
    fn expand_replacement_non_ascii_with_groups_and_escapes() {
        let re = build_search_regex(r"(\w+)=(\w+)", true, false, true).unwrap();
        let result = expand(&re, "名前=値".as_bytes(), r"$2・$1\tほげ");
        assert_eq!(result, "値・名前\tほげ");
    }

    #[test]
    fn interpret_escapes_basic_controls() {
        assert_eq!(interpret_escapes(r"\n"), "\n");
        assert_eq!(interpret_escapes(r"\t"), "\t");
        assert_eq!(interpret_escapes(r"\r"), "\r");
        assert_eq!(interpret_escapes(r"\\"), "\\");
    }

    #[test]
    fn interpret_escapes_literal_backslash_round_trip() {
        // User types "\\n" to mean literal backslash + n
        assert_eq!(interpret_escapes(r"\\n"), r"\n");
        // User types "\\\\" to mean literal "\\"
        assert_eq!(interpret_escapes(r"\\\\"), r"\\");
    }

    #[test]
    fn interpret_escapes_unknown_escape_is_preserved() {
        // Unknown escapes pass through untouched: don't surprise users who
        // weren't trying to use escape syntax.
        assert_eq!(interpret_escapes(r"\q"), r"\q");
        assert_eq!(interpret_escapes(r"hello \q world"), r"hello \q world");
    }

    #[test]
    fn interpret_escapes_trailing_backslash_preserved() {
        assert_eq!(interpret_escapes(r"foo\"), r"foo\");
    }

    #[test]
    fn interpret_escapes_mixed_content() {
        assert_eq!(interpret_escapes(r"line1\nline2\tcol"), "line1\nline2\tcol");
    }

    #[test]
    fn interpret_escapes_does_not_touch_dollar_group_refs() {
        // Must not interfere with the regex crate's $N / ${N} expansion syntax.
        assert_eq!(interpret_escapes(r"$1\n$2"), "$1\n$2");
    }

    #[test]
    fn expand_replacement_interprets_escapes() {
        let re = build_search_regex(r"(\w+)", true, false, true).unwrap();
        let result = expand(&re, b"hello", r"$1\tend");
        assert_eq!(result, "hello\tend");
    }

    /// The groups are found again with the bytes around the match, so a
    /// pattern that depends on them expands as the search matched it.
    #[test]
    fn expand_replacement_judges_the_match_in_context() {
        // `\B(x)` matched the `x` in "axb": alone, "x" has a word boundary
        // at its start, so matching the slice again would fail.
        let re = build_search_regex(r"\B(x)", true, false, true).unwrap();
        assert_eq!(expand_replacement(&re, b"axb", 1, 1, "[$1]"), "[x]");
        // A match the regex can't find again there keeps the template.
        assert_eq!(expand_replacement(&re, b"axb", 0, 1, "[$1]"), "[$1]");
    }
}
