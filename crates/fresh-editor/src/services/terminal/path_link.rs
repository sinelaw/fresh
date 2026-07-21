//! Detect file-path "links" in a line of terminal text.
//!
//! Given a line of text (one row of terminal output) and the column the user
//! clicked / hovered, [`detect_link_at`] finds the surrounding file path and
//! parses any trailing `:line`, `:line:col`, or `(line,col)` location suffix.
//! This powers Ctrl+Click / Ctrl+hover "open path in Fresh" from the embedded
//! terminal.
//!
//! The detector is purely textual and host-agnostic: it returns a candidate
//! path string and optional location, plus the character range it occupies (so
//! the caller can underline it). Whether that path actually resolves to a file
//! — relative to the terminal's OSC 7 cwd or Fresh's working directory — is the
//! caller's job; see the resolution logic in the app layer.

use std::ops::Range;

/// A file-path link detected within a line of terminal text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DetectedLink {
    /// Character range (within the source line) covered by the link, including
    /// any `:line:col` suffix. Used to highlight/underline the link.
    pub range: Range<usize>,
    /// The path component (without the location suffix). Not yet resolved.
    pub path: String,
    /// 1-based line number from the suffix, if present.
    pub line: Option<usize>,
    /// 1-based column number from the suffix, if present.
    pub column: Option<usize>,
}

/// Characters that commonly *wrap* a path and should be peeled off the front.
fn is_opener(c: char) -> bool {
    matches!(c, '"' | '\'' | '`' | '(' | '[' | '{' | '<')
}

/// Trailing characters to strip from a bare path (sentence punctuation and
/// closing wrappers). A trailing `:` is handled separately so it isn't peeled
/// off a `path:line` form before the suffix is parsed.
fn is_trailing_punct(c: char) -> bool {
    matches!(
        c,
        '.' | ',' | ';' | '!' | '?' | ')' | ']' | '}' | '>' | '"' | '\'' | '`'
    )
}

/// Detect a file-path link at character offset `col` within `line`.
///
/// Returns `None` when the click isn't over a path-like token. The returned
/// path is textual only — it may be relative and is not checked for existence.
pub fn detect_link_at(line: &str, col: usize) -> Option<DetectedLink> {
    let chars: Vec<char> = line.chars().collect();
    let n = chars.len();
    if n == 0 {
        return None;
    }

    // Resolve the click to a non-whitespace character. A click that lands just
    // past the end of a token (on the following space) still opens the token to
    // its left, matching how editors treat clicks at a word's trailing edge.
    let mut anchor = col.min(n - 1);
    if chars[anchor].is_whitespace() {
        if anchor == 0 || chars[anchor - 1].is_whitespace() {
            return None;
        }
        anchor -= 1;
    }

    // Expand to the whitespace-bounded token surrounding the anchor.
    let mut start = anchor;
    while start > 0 && !chars[start - 1].is_whitespace() {
        start -= 1;
    }
    let mut end = anchor + 1;
    while end < n && !chars[end].is_whitespace() {
        end += 1;
    }

    // Narrow to a parenthesised path when the token wraps one — e.g. Claude
    // Code's `Read(src/main.rs)` / `Update(src/foo.rs:12)`, or a bare
    // `(src/main.rs:12)`. Without this the leading prefix (`Read(`) or the
    // trailing `)` glues onto the path and it never resolves. Only narrow when
    // the parenthesised content is itself path-like, so a `name(line,col)`
    // location suffix (e.g. `Program.cs(34,12)`) keeps its meaning and is parsed
    // by `split_location_suffix` below instead.
    if let Some(lp) = (start..end).find(|&i| chars[i] == '(') {
        if let Some(rp) = (lp + 1..end).rev().find(|&i| chars[i] == ')') {
            if lp + 1 < rp {
                let inner: String = chars[lp + 1..rp].iter().collect();
                if parenthesized_is_path(&inner) {
                    start = lp + 1;
                    end = rp;
                }
            }
        }
    }

    // Peel leading wrappers (quotes/brackets) off the front.
    while start < end && is_opener(chars[start]) {
        start += 1;
    }
    if start >= end {
        return None;
    }

    let token: String = chars[start..end].iter().collect();

    // Parse a trailing location suffix. `path_len` is the char length of the
    // leading path component; `suffix_end` is the char index (within the token)
    // just past the numeric suffix, excluding any trailing `:`.
    let (path_len, suffix_end, line_no, col_no) = split_location_suffix(&token);

    // The path occupies `[start, start + path_len)` in line coordinates (token
    // chars map 1:1 onto line chars from `start`).
    let mut path_end = start + path_len;
    // Trim trailing punctuation/closers from the bare path (only when there was
    // no numeric suffix consuming the tail — with a suffix, the path already
    // ends at a digit boundary).
    if line_no.is_none() {
        while path_end > start && is_trailing_punct(chars[path_end - 1]) {
            path_end -= 1;
        }
    }
    if path_end <= start {
        return None;
    }

    let path: String = chars[start..path_end].iter().collect();
    // Reject tokens that are clearly not paths: a lone separator or pure
    // punctuation, or something with no path-ish character at all.
    if !looks_like_path(&path) {
        return None;
    }

    // The highlight range spans the path plus its numeric suffix (if any).
    let link_end = if line_no.is_some() {
        start + suffix_end
    } else {
        path_end
    };

    Some(DetectedLink {
        range: start..link_end,
        path,
        line: line_no,
        column: col_no,
    })
}

/// Parse a trailing `:line`, `:line:col` (optionally with a trailing `:`, as
/// gcc/ripgrep emit), or `(line,col)` suffix from `token`.
///
/// Returns `(path_char_len, suffix_end, line, column)`:
/// - `path_char_len` is the char length of the leading path component.
/// - `suffix_end` is the char index just past the numeric suffix (excluding any
///   trailing `:`), used to bound the highlight range. Equals the full token
///   length when no suffix is present.
fn split_location_suffix(token: &str) -> (usize, usize, Option<usize>, Option<usize>) {
    let chars: Vec<char> = token.chars().collect();
    let full = chars.len();

    // Strip a single trailing ':' up front — gcc emits `file:12:5:` and MSVC
    // `Program.cs(34,12):`, so the colon must not block either form below.
    let mut end = full;
    if end > 0 && chars[end - 1] == ':' {
        end -= 1;
    }

    // `name(12,5)` / `name(12)` form.
    if end > 0 && chars[end - 1] == ')' {
        if let Some(open) = (0..end - 1).rev().find(|&i| chars[i] == '(') {
            let inner: String = chars[open + 1..end - 1].iter().collect();
            // Require an actual path before the '(' so a bare "(1,2)" is not a
            // link.
            if open > 0 {
                if let Some((l, c)) = parse_line_col_pair(&inner) {
                    return (open, end, Some(l), c);
                }
            }
        }
    }

    // `path:line:col` / `path:line` forms. Pull the trailing run of digits.
    let num1_start = rfind_digit_run(&chars, end);
    if num1_start == end || num1_start == 0 || chars[num1_start - 1] != ':' {
        // No `:digits` tail.
        return (full, full, None, None);
    }
    let num1: String = chars[num1_start..end].iter().collect();
    let n1: usize = match num1.parse() {
        Ok(v) => v,
        Err(_) => return (full, full, None, None),
    };
    let colon1 = num1_start - 1; // index of the ':' before num1

    // Optionally a second `:digits` (column) before it: path:line:col, so num1
    // is the column and the run before colon1 is the line.
    if colon1 > 0 {
        let num2_end = colon1;
        let num2_start = rfind_digit_run(&chars, num2_end);
        if num2_start < num2_end && num2_start > 0 && chars[num2_start - 1] == ':' {
            let num2: String = chars[num2_start..num2_end].iter().collect();
            if let Ok(n2) = num2.parse::<usize>() {
                // path : n2(line) : n1(col)
                return (num2_start - 1, end, Some(n2), Some(n1));
            }
        }
    }

    // Only one number: it's the line.
    (colon1, end, Some(n1), None)
}

/// Parse an `"<line>,<col>"` or `"<line>"` pair (the inside of a `(...)`).
fn parse_line_col_pair(inner: &str) -> Option<(usize, Option<usize>)> {
    let inner = inner.trim();
    if let Some((l, c)) = inner.split_once(',') {
        let line = l.trim().parse().ok()?;
        let col = c.trim().parse().ok()?;
        Some((line, Some(col)))
    } else {
        Some((inner.parse().ok()?, None))
    }
}

/// Index of the start of the maximal run of ASCII digits ending at `end`.
fn rfind_digit_run(chars: &[char], end: usize) -> usize {
    let mut i = end;
    while i > 0 && chars[i - 1].is_ascii_digit() {
        i -= 1;
    }
    i
}

/// Heuristic for the content inside a `(...)`: is it a *path* (so the token is
/// `prefix(path)` / `(path)`, e.g. Claude Code's `Read(src/main.rs:12)`), rather
/// than a numeric `(line,col)` / `(line)` location suffix (e.g.
/// `Program.cs(34,12)`)?
///
/// Path-like: contains a separator or `~`, or a dotted name with a letter
/// (`main.rs`). Deliberately rejects pure-numeric content (`34`, `34,12`) and
/// dotted numbers (`1.5`) so a real location suffix isn't mistaken for a path.
fn parenthesized_is_path(inner: &str) -> bool {
    let inner = inner.trim();
    inner.contains('/')
        || inner.contains('\\')
        || inner.contains('~')
        || (inner.contains('.') && inner.chars().any(|c| c.is_alphabetic()))
}

/// Heuristic: does this token look like a file path rather than, say, a bare
/// word of prose or a `key:value` pair? We accept it if it contains a path
/// separator, a dot (extension/relative marker), a `~`, or is a single
/// recognizable filename. The caller still verifies existence, so this only
/// needs to filter out obvious non-paths cheaply.
fn looks_like_path(path: &str) -> bool {
    if path.is_empty() {
        return false;
    }
    // Anything with a separator or home/relative marker is path-shaped.
    if path.contains('/') || path.contains('\\') || path.starts_with('~') || path.starts_with('.') {
        return true;
    }
    // A single segment with an extension (foo.rs) is path-shaped; a bare word
    // (e.g. "warning") is not.
    path.contains('.')
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Click in the middle of a bare relative path → whole path, no location.
    #[test]
    fn plain_relative_path() {
        let line = "see src/main.rs for details";
        let got = detect_link_at(line, 6).unwrap();
        assert_eq!(got.path, "src/main.rs");
        assert_eq!(got.line, None);
        assert_eq!(got.column, None);
        assert_eq!(&line[got.range], "src/main.rs");
    }

    /// `path:line:col` (the canonical compiler/grep format).
    #[test]
    fn path_line_col() {
        let line = "crates/fresh-editor/src/app.rs:128:5: error";
        let got = detect_link_at(line, 10).unwrap();
        assert_eq!(got.path, "crates/fresh-editor/src/app.rs");
        assert_eq!(got.line, Some(128));
        assert_eq!(got.column, Some(5));
        // Range underlines the path AND the :line:col suffix.
        assert_eq!(&line[got.range], "crates/fresh-editor/src/app.rs:128:5");
    }

    /// `path:line` (no column).
    #[test]
    fn path_line_only() {
        let line = "at ./foo/bar.py:42";
        let got = detect_link_at(line, 5).unwrap();
        assert_eq!(got.path, "./foo/bar.py");
        assert_eq!(got.line, Some(42));
        assert_eq!(got.column, None);
        assert_eq!(&line[got.range], "./foo/bar.py:42");
    }

    /// gcc-style trailing colon: `file:12:5:`.
    #[test]
    fn trailing_colon_after_col() {
        let line = "main.c:12:5:";
        let got = detect_link_at(line, 2).unwrap();
        assert_eq!(got.path, "main.c");
        assert_eq!(got.line, Some(12));
        assert_eq!(got.column, Some(5));
    }

    /// `name(line,col)` form (some toolchains / MSVC).
    #[test]
    fn paren_line_col() {
        let line = "Program.cs(34,12): warning CS0168";
        let got = detect_link_at(line, 3).unwrap();
        assert_eq!(got.path, "Program.cs");
        assert_eq!(got.line, Some(34));
        assert_eq!(got.column, Some(12));
        assert_eq!(&line[got.range], "Program.cs(34,12)");
    }

    /// Absolute path with a trailing period (end of sentence) is trimmed.
    #[test]
    fn trailing_sentence_punctuation_trimmed() {
        let line = "wrote /tmp/out/log.txt.";
        let got = detect_link_at(line, 8).unwrap();
        assert_eq!(got.path, "/tmp/out/log.txt");
        assert_eq!(got.line, None);
        assert_eq!(&line[got.range], "/tmp/out/log.txt");
    }

    /// A quoted path: the closing quote is peeled off. Paths containing spaces
    /// are split on whitespace (a documented limitation), so clicking the
    /// second half resolves just that segment, sans the trailing quote.
    #[test]
    fn quoted_path() {
        let line = "open \"my dir/file.rs\" now";
        // Click inside the "dir/file.rs" segment.
        let got = detect_link_at(line, 14).unwrap();
        assert_eq!(got.path, "dir/file.rs");
        assert_eq!(&line[got.range], "dir/file.rs");
    }

    /// Clicking on whitespace between tokens returns nothing.
    #[test]
    fn click_on_whitespace_between_words() {
        let line = "foo   bar.rs";
        // Column 4 is a space with a space to its left → no token.
        assert_eq!(detect_link_at(line, 4), None);
    }

    /// A bare prose word (no separator/extension) is not treated as a path.
    #[test]
    fn bare_word_is_not_a_path() {
        let line = "this is a warning message";
        assert_eq!(detect_link_at("warning", 0), None);
        assert!(detect_link_at(line, 10).is_none());
    }

    /// A `key:value` style word where value isn't numeric is not a location;
    /// but if it has an extension it's still a path with no suffix.
    #[test]
    fn non_numeric_colon_suffix_kept_in_path() {
        // No digits after the colon → the colon stays part of the (rejected)
        // bare word; nothing path-like here.
        assert_eq!(detect_link_at("note:something", 2), None);
    }

    /// Clicking just past the end of a token (on the trailing space) still
    /// resolves the token to the left.
    #[test]
    fn click_at_trailing_edge() {
        let line = "src/lib.rs done";
        let got = detect_link_at(line, 10).unwrap();
        assert_eq!(got.path, "src/lib.rs");
    }

    /// An empty line yields nothing.
    #[test]
    fn empty_line() {
        assert_eq!(detect_link_at("", 0), None);
    }

    /// A path with no extension but with a separator is accepted (e.g. a dir or
    /// extensionless binary).
    #[test]
    fn extensionless_with_separator() {
        let line = "cd /usr/local/bin";
        let got = detect_link_at(line, 5).unwrap();
        assert_eq!(got.path, "/usr/local/bin");
    }

    /// Column clamps to the line; an out-of-range click on the last token
    /// still works.
    #[test]
    fn col_beyond_line_clamps() {
        let line = "x.rs";
        let got = detect_link_at(line, 999).unwrap();
        assert_eq!(got.path, "x.rs");
    }

    /// Claude Code's tool-call rendering `Read(path)`: the `Read(` prefix must
    /// not glue onto the path — the parenthesised path is what resolves.
    /// (ASCII-only so char offsets equal byte offsets for the range slice.)
    #[test]
    fn tool_call_parenthesized_path() {
        let line = "- Read(src/main.rs)";
        // Click inside the path (col 10) and, separately, on the `Read` prefix
        // (col 3) — both resolve to the parenthesised path.
        for col in [10, 3] {
            let got = detect_link_at(line, col).unwrap();
            assert_eq!(got.path, "src/main.rs", "col {col}");
            assert_eq!(got.line, None);
            assert_eq!(&line[got.range], "src/main.rs");
        }
    }

    /// `Tool(path:line:col)`: the location suffix inside the parens is parsed
    /// and the trailing `)` doesn't block it.
    #[test]
    fn tool_call_parenthesized_path_with_location() {
        let line = "Update(crates/app/src/foo.rs:12:3)";
        let got = detect_link_at(line, 20).unwrap();
        assert_eq!(got.path, "crates/app/src/foo.rs");
        assert_eq!(got.line, Some(12));
        assert_eq!(got.column, Some(3));
        assert_eq!(&line[got.range], "crates/app/src/foo.rs:12:3");
    }

    /// A bare `(path:line)` — a filename wrapped in parens with a line ref.
    #[test]
    fn bare_parenthesized_path_with_line() {
        let line = "see (src/main.rs:2) now";
        let got = detect_link_at(line, 6).unwrap();
        assert_eq!(got.path, "src/main.rs");
        assert_eq!(got.line, Some(2));
        assert_eq!(&line[got.range], "src/main.rs:2");
    }

    /// The `name(line,col)` location form must NOT be reinterpreted as a
    /// parenthesised path (the inner `34,12` is numeric, not path-like).
    #[test]
    fn numeric_paren_suffix_not_treated_as_inner_path() {
        let line = "Program.cs(34,12): warning";
        let got = detect_link_at(line, 3).unwrap();
        assert_eq!(got.path, "Program.cs");
        assert_eq!(got.line, Some(34));
        assert_eq!(got.column, Some(12));
    }
}
