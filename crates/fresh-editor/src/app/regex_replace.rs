/// Pure, buffer-agnostic helpers for regex find-and-replace.
///
/// Build a [`regex::bytes::Regex`] from user-supplied search settings.
/// Returns `None` when `use_regex` is false.
pub fn build_regex(
    search: &str,
    use_regex: bool,
    whole_word: bool,
    case_sensitive: bool,
) -> Option<regex::bytes::Regex> {
    if !use_regex {
        return None;
    }

    let pattern = if whole_word {
        format!(r"\b{}\b", search)
    } else {
        search.to_string()
    };

    regex::bytes::RegexBuilder::new(&pattern)
        .case_insensitive(!case_sensitive)
        .build()
        .ok()
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
            out.push(bytes[i] as char);
            i += 1;
        }
    }
    out
}

/// A single match found in a buffer, together with its expanded replacement.
#[derive(Debug, Clone)]
pub struct ReplaceMatch {
    /// Byte offset of the match start.
    pub offset: usize,
    /// Length of the matched text in bytes.
    pub len: usize,
    /// The replacement text after capture-group expansion.
    pub replacement: String,
}

/// Find every match of `regex` in `haystack` and expand `replacement_template`
/// for each one (honouring `$1`, `${name}`, etc.).
pub fn collect_regex_matches(
    regex: &regex::bytes::Regex,
    haystack: &[u8],
    replacement_template: &str,
) -> Vec<ReplaceMatch> {
    let normalized = normalize_replacement(replacement_template);
    regex
        .captures_iter(haystack)
        .map(|caps| {
            let m = caps.get(0).unwrap();
            let mut expanded = Vec::new();
            caps.expand(normalized.as_bytes(), &mut expanded);
            ReplaceMatch {
                offset: m.start(),
                len: m.len(),
                replacement: String::from_utf8_lossy(&expanded).into_owned(),
            }
        })
        .collect()
}

/// Expand capture-group references in `replacement_template` against a single
/// match. Returns the template unchanged when no captures are found.
pub fn expand_replacement(
    regex: &regex::bytes::Regex,
    matched_bytes: &[u8],
    replacement_template: &str,
) -> String {
    if let Some(caps) = regex.captures(matched_bytes) {
        let normalized = normalize_replacement(replacement_template);
        let mut dst = Vec::new();
        caps.expand(normalized.as_bytes(), &mut dst);
        String::from_utf8_lossy(&dst).into_owned()
    } else {
        replacement_template.to_string()
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
    fn collect_regex_matches_literal_replacement() {
        let re = build_regex("Sig:.*", true, false, true).unwrap();
        let input = b"AAAAAA\nSig: hello\nBBBBBB\nSig: world\nCCCCCC";
        let matches = collect_regex_matches(&re, input, "");

        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].offset, 7);
        assert_eq!(matches[0].replacement, "");
        assert_eq!(matches[1].offset, 25);
        assert_eq!(matches[1].replacement, "");
    }

    #[test]
    fn collect_regex_matches_with_capture_groups() {
        let re = build_regex(r"(\w+)@(\w+)", true, false, true).unwrap();
        let input = b"alice@example bob@test";
        let matches = collect_regex_matches(&re, input, "$2=$1");

        assert_eq!(matches.len(), 2);
        assert_eq!(matches[0].replacement, "example=alice");
        assert_eq!(matches[1].replacement, "test=bob");
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

    /// Matches Python: re.sub(r'bla(bla)', r'oo\1oo', 'blablabla') == 'ooblaoobla'
    #[test]
    fn collect_regex_matches_capture_group_blabla() {
        let re = build_regex(r"bla(bla)", true, false, true).unwrap();
        let input = b"blablabla";
        let matches = collect_regex_matches(&re, input, "oo$1oo");

        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].offset, 0);
        assert_eq!(matches[0].len, 6);
        assert_eq!(matches[0].replacement, "ooblaoo");
    }
}
