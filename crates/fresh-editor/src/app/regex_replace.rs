/// Pure, buffer-agnostic helpers for regex find-and-replace.
///
/// Build a Unicode [`regex::Regex`] for the search/highlight path.
///
/// Unlike [`build_regex`] (used for replace), this always produces a regex:
/// when `use_regex` is false the query is `regex::escape`d so the resulting
/// pattern matches the literal text.  That lets the caller share one code
/// path for both regex and plain-text search.
///
/// Returns `Err` with a user-facing message when the pattern fails to
/// compile (regex-mode only; escaped literal patterns never fail).
pub fn build_search_regex(
    query: &str,
    use_regex: bool,
    whole_word: bool,
    case_sensitive: bool,
) -> Result<regex::Regex, String> {
    let pattern = if use_regex {
        if whole_word {
            format!(r"\b{}\b", query)
        } else {
            query.to_string()
        }
    } else {
        let escaped = regex::escape(query);
        if whole_word {
            format!(r"\b{}\b", escaped)
        } else {
            escaped
        }
    };

    regex::RegexBuilder::new(&pattern)
        .case_insensitive(!case_sensitive)
        // Match `^`/`$` at every line boundary, like every editor's find.
        // `crlf` makes both `\n` and `\r\n` count as terminators so the
        // anchors behave the same on LF and CRLF buffers.
        .multi_line(true)
        .crlf(true)
        .build()
        .map_err(|e| e.to_string())
}

/// Build a [`regex::bytes::Regex`] from user-supplied search settings, for
/// the replace path.
///
/// Returns `None` only for a plain-text, case-sensitive, non-whole-word
/// search, which the buffer's byte-exact search matches as is. A plain-text
/// search with case folding or whole-word matching gets a regex over the
/// escaped text, so replace matches exactly what the search highlights
/// (#3575). Its replacement is still literal: only regex mode expands `$1`
/// or `\n`; the caller inserts a plain-text replacement as typed.
pub fn build_regex(
    search: &str,
    use_regex: bool,
    whole_word: bool,
    case_sensitive: bool,
) -> Option<regex::bytes::Regex> {
    let query = if use_regex {
        search.to_string()
    } else if case_sensitive && !whole_word {
        return None;
    } else {
        regex::escape(search)
    };

    let pattern = if whole_word {
        format!(r"\b{}\b", query)
    } else {
        query
    };

    regex::bytes::RegexBuilder::new(&pattern)
        .case_insensitive(!case_sensitive)
        // Keep `^`/`$` line-anchored in replace, matching the search path.
        .multi_line(true)
        .crlf(true)
        .build()
        .ok()
}

/// Interpret the common backslash escapes in a replacement template:
/// `\n`, `\t`, `\r` become their control-character equivalents and `\\`
/// becomes a single backslash. Unknown escapes (e.g. `\q`) are preserved
/// verbatim as `\q` so users who didn't mean to type an escape sequence
/// don't get surprising behaviour. A trailing lone backslash is likewise
/// passed through unchanged.
///
/// Only applied in regex-replace mode — plain-text replacement stays
/// literal, which matches the user expectation that plain-text search is
/// symmetric with plain-text replace.
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
            // Already braced: ${ … }
            if i + 1 < bytes.len() && bytes[i + 1] == b'{' {
                out.push(bytes[i] as char);
                i += 1;
                continue;
            }
            // Collect the digit run
            let start = i + 1;
            let mut end = start;
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            // If the next char after digits is alphanumeric (or _), we must
            // brace it, otherwise the regex crate would include those chars
            // in the group name.  Always bracing is harmless, so just do it.
            out.push('$');
            out.push('{');
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

/// Expand capture-group references in `replacement_template` against a single
/// match. Returns the template unchanged when no captures are found.
pub fn expand_replacement(
    regex: &regex::bytes::Regex,
    matched_bytes: &[u8],
    replacement_template: &str,
) -> String {
    let escaped = interpret_escapes(replacement_template);
    if let Some(caps) = regex.captures(matched_bytes) {
        let normalized = normalize_replacement(&escaped);
        let mut dst = Vec::new();
        caps.expand(normalized.as_bytes(), &mut dst);
        String::from_utf8_lossy(&dst).into_owned()
    } else {
        escaped
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_regex_returns_none_when_disabled() {
        assert!(build_regex("foo", false, false, true).is_none());
    }

    #[test]
    fn build_search_regex_plain_text_escapes_special_chars() {
        // Non-regex mode: "a.b" should match "a.b" literally, not "a?b".
        let re = build_search_regex("a.b", false, false, true).unwrap();
        assert!(re.is_match("a.b"));
        assert!(!re.is_match("axb"));
    }

    #[test]
    fn build_search_regex_regex_mode_treats_dot_as_wildcard() {
        let re = build_search_regex("a.b", true, false, true).unwrap();
        assert!(re.is_match("axb"));
        assert!(re.is_match("a.b"));
    }

    #[test]
    fn build_search_regex_whole_word_wraps_pattern() {
        let re = build_search_regex("foo", false, true, true).unwrap();
        assert!(re.is_match("foo bar"));
        assert!(!re.is_match("foobar"));
    }

    /// Issue #3575: plain text with case folding or whole word gets a regex
    /// over the escaped text, so replace matches what search highlights.
    #[test]
    fn build_regex_plain_text_honors_case_and_whole_word() {
        let re = build_regex("a.b", false, false, false).unwrap();
        assert!(re.is_match(b"A.B"));
        assert!(!re.is_match(b"axb"), "plain text must stay literal");

        let re = build_regex("cat", false, true, true).unwrap();
        assert!(re.is_match(b"a cat."));
        assert!(!re.is_match(b"concat"));
    }

    #[test]
    fn build_search_regex_case_insensitive_when_flag_off() {
        let re = build_search_regex("Hello", false, false, false).unwrap();
        assert!(re.is_match("HELLO"));
        assert!(re.is_match("hello"));
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
        let text = "use a;\nuse b;\nlet use_x = 1;\nuse c;\n";
        assert_eq!(re.find_iter(text).count(), 3);
    }

    #[test]
    fn build_search_regex_dollar_anchors_every_line() {
        let re = build_search_regex("foo$", true, false, true).unwrap();
        let text = "foo\nfoobar\nbar foo\n";
        assert_eq!(re.find_iter(text).count(), 2);
    }

    #[test]
    fn build_search_regex_anchors_match_crlf_lines() {
        // On CRLF buffers `$` must match before `\r\n`, so `foo$` still hits.
        let re = build_search_regex("foo$", true, false, true).unwrap();
        let text = "foo\r\nbar\r\nfoo\r\n";
        assert_eq!(re.find_iter(text).count(), 2);
    }

    #[test]
    fn build_search_regex_dot_does_not_cross_newlines() {
        // Multi-line mode only re-anchors `^`/`$`; `.` must still stop at a
        // line boundary (no `dot_matches_new_line`). So `a.b` cannot span
        // "a\nb", but a greedy `a.*` stays within its line.
        let re = build_search_regex("a.b", true, false, true).unwrap();
        assert!(!re.is_match("a\nb"));
        let re2 = build_search_regex("a.*", true, false, true).unwrap();
        let m = re2.find("axy\nbcd").unwrap();
        assert_eq!(m.as_str(), "axy");
    }

    #[test]
    fn build_regex_caret_anchors_every_line() {
        // The bytes builder used for replace must anchor per line too.
        let re = build_regex("^use", true, false, true).unwrap();
        let text = b"use a;\nuse b;\nuse c;\n";
        assert_eq!(re.find_iter(text).count(), 3);
    }

    #[test]
    fn build_regex_basic_pattern() {
        let re = build_regex("foo.*bar", true, false, true).unwrap();
        assert!(re.is_match(b"foo123bar"));
        assert!(!re.is_match(b"baz"));
    }

    #[test]
    fn build_regex_case_insensitive() {
        let re = build_regex("hello", true, false, false).unwrap();
        assert!(re.is_match(b"HELLO"));
        assert!(re.is_match(b"hello"));
    }

    #[test]
    fn build_regex_whole_word() {
        let re = build_regex("foo", true, true, true).unwrap();
        assert!(re.is_match(b"foo bar"));
        assert!(!re.is_match(b"foobar"));
    }

    #[test]
    fn expand_replacement_with_groups() {
        let re = build_regex(r"(\d+)-(\d+)", true, false, true).unwrap();
        let matched = b"123-456";
        let result = expand_replacement(&re, matched, "$2/$1");
        assert_eq!(result, "456/123");
    }

    #[test]
    fn expand_replacement_no_groups() {
        let re = build_regex("hello", true, false, true).unwrap();
        let matched = b"hello";
        let result = expand_replacement(&re, matched, "world");
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
        let re = build_regex(r"(\w+)=(\w+)", true, false, true).unwrap();
        let result = expand_replacement(&re, "名前=値".as_bytes(), r"$2・$1\tほげ");
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
        let re = build_regex(r"(\w+)", true, false, true).unwrap();
        let result = expand_replacement(&re, b"hello", r"$1\tend");
        assert_eq!(result, "hello\tend");
    }
}
