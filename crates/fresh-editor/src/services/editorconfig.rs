//! Support for [`.editorconfig`](https://editorconfig.org/) files.
//!
//! When a file is opened, Fresh walks up the directory tree from the file's
//! location, reading any `.editorconfig` files it finds (through the
//! [`FileSystem`] abstraction so the feature works transparently on remote
//! hosts), and applies the matching indentation settings to the buffer.
//!
//! Only the indentation-related properties are mapped to Fresh's per-buffer
//! settings today:
//!
//! | EditorConfig property | Buffer setting |
//! |-----------------------|----------------|
//! | `indent_style = tab`  | `use_tabs = true` |
//! | `indent_style = space`| `use_tabs = false` |
//! | `indent_size = <n>`   | `tab_size = n` |
//! | `tab_width = <n>`     | `tab_size = n` (takes precedence over `indent_size`) |
//!
//! The parser keeps every key/value pair it reads, so wiring up additional
//! properties later is a localized change in [`apply_section`].
//!
//! Glob matching follows the EditorConfig spec: `*`, `**`, `?`, `[seq]` /
//! `[!seq]` character classes, and `{a,b,c}` brace alternation are supported.
//! Numeric `{m..n}` ranges are supported for reasonably sized ranges and fall
//! back to "any integer" for very large ones.

use std::path::{Path, PathBuf};

use regex::Regex;

use crate::model::filesystem::FileSystem;

/// Indentation settings resolved from `.editorconfig` for a single file.
///
/// Each field is `None` when no applicable `.editorconfig` section specified
/// it, leaving the corresponding buffer setting untouched.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EditorConfigSettings {
    /// `Some(true)` => indent with tabs, `Some(false)` => indent with spaces.
    pub use_tabs: Option<bool>,
    /// Effective indent / tab width (`indent_size` or `tab_width`).
    pub tab_size: Option<usize>,
}

impl EditorConfigSettings {
    /// True when no setting was resolved.
    pub fn is_empty(&self) -> bool {
        self.use_tabs.is_none() && self.tab_size.is_none()
    }
}

/// A single `[pattern]` section of an `.editorconfig` file.
#[derive(Debug, Clone)]
struct Section {
    /// The raw glob pattern between the brackets.
    pattern: String,
    /// All key/value pairs declared in the section, keys lowercased.
    props: Vec<(String, String)>,
}

/// A parsed `.editorconfig` file.
#[derive(Debug, Clone, Default)]
struct ParsedFile {
    /// Whether the preamble declared `root = true`, stopping the upward search.
    root: bool,
    sections: Vec<Section>,
}

/// Resolve the indentation settings that apply to `file_path`.
///
/// Walks up from the file's directory, reading `.editorconfig` files via `fs`,
/// stopping once a file declares `root = true`. Sections closer to the file
/// (and later sections within a single file) take precedence.
pub fn resolve_for_file(fs: &dyn FileSystem, file_path: &Path) -> EditorConfigSettings {
    // Collect the `.editorconfig` chain from the file's directory upward.
    let mut chain: Vec<(PathBuf, ParsedFile)> = Vec::new();
    let mut dir_opt = file_path.parent();
    while let Some(dir) = dir_opt {
        let ec_path = dir.join(".editorconfig");
        if let Ok(bytes) = fs.read_file(&ec_path) {
            if let Ok(text) = String::from_utf8(bytes) {
                let parsed = parse(&text);
                let stop = parsed.root;
                chain.push((dir.to_path_buf(), parsed));
                if stop {
                    break;
                }
            }
        }
        dir_opt = dir.parent();
    }

    // Apply farthest-first so that closer files (pushed first) win, and within
    // each file apply sections top-to-bottom so later sections win.
    let mut settings = EditorConfigSettings::default();
    for (ec_dir, parsed) in chain.iter().rev() {
        for section in &parsed.sections {
            if section_matches(&section.pattern, ec_dir, file_path) {
                apply_section(&mut settings, section);
            }
        }
    }
    settings
}

/// Parse the textual contents of an `.editorconfig` file.
fn parse(content: &str) -> ParsedFile {
    let mut file = ParsedFile::default();
    let mut in_preamble = true;
    for raw in content.lines() {
        let line = raw.trim();
        if line.is_empty() || line.starts_with('#') || line.starts_with(';') {
            continue;
        }
        if line.len() >= 2 && line.starts_with('[') && line.ends_with(']') {
            let pattern = line[1..line.len() - 1].to_string();
            file.sections.push(Section {
                pattern,
                props: Vec::new(),
            });
            in_preamble = false;
            continue;
        }
        let Some(eq) = line.find('=') else {
            continue;
        };
        let key = line[..eq].trim().to_lowercase();
        let value = line[eq + 1..].trim().to_lowercase();
        if key.is_empty() {
            continue;
        }
        if in_preamble {
            if key == "root" {
                file.root = value == "true";
            }
        } else if let Some(section) = file.sections.last_mut() {
            section.props.push((key, value));
        }
    }
    file
}

/// Apply a matching section's indentation properties onto `acc`, overwriting
/// any previously resolved values (later/closer sections win).
fn apply_section(acc: &mut EditorConfigSettings, section: &Section) {
    let mut indent_style: Option<&str> = None;
    let mut indent_size: Option<&str> = None;
    let mut tab_width: Option<&str> = None;
    for (key, value) in &section.props {
        match key.as_str() {
            "indent_style" => indent_style = Some(value),
            "indent_size" => indent_size = Some(value),
            "tab_width" => tab_width = Some(value),
            _ => {}
        }
    }

    match indent_style {
        Some("tab") => acc.use_tabs = Some(true),
        Some("space") => acc.use_tabs = Some(false),
        // "unset" and unknown values leave the current setting untouched.
        _ => {}
    }

    // `tab_width` overrides `indent_size` for the display width of a tab;
    // `indent_size = tab` means "use tab_width", which we already cover.
    let width = tab_width
        .and_then(parse_positive)
        .or_else(|| indent_size.and_then(parse_positive));
    if let Some(width) = width {
        acc.tab_size = Some(width);
    }
}

/// Parse a strictly positive integer from an EditorConfig value.
fn parse_positive(value: &str) -> Option<usize> {
    match value.parse::<usize>() {
        Ok(n) if n > 0 => Some(n),
        _ => None,
    }
}

/// Whether a section glob `pattern` (relative to `ec_dir`) matches `file_path`.
fn section_matches(pattern: &str, ec_dir: &Path, file_path: &Path) -> bool {
    if pattern.is_empty() {
        return false;
    }
    if pattern.contains('/') {
        // A pattern containing `/` is anchored to the `.editorconfig` directory.
        let Ok(relative) = file_path.strip_prefix(ec_dir) else {
            return false;
        };
        let relative = to_posix(relative);
        let pat = pattern.strip_prefix('/').unwrap_or(pattern);
        regex_for(pat).is_match(&relative)
    } else {
        // A pattern without `/` matches the file name at any depth.
        let Some(name) = file_path.file_name().and_then(|n| n.to_str()) else {
            return false;
        };
        regex_for(pattern).is_match(name)
    }
}

/// Compile a glob into a regex, falling back to a never-matching regex on the
/// (practically impossible) chance translation produces invalid regex.
fn regex_for(pattern: &str) -> Regex {
    Regex::new(&glob_to_regex(pattern)).unwrap_or_else(|_| Regex::new("$.^").expect("valid regex"))
}

/// Render a relative path as a `/`-separated string regardless of platform.
fn to_posix(path: &Path) -> String {
    path.components()
        .filter_map(|c| c.as_os_str().to_str())
        .collect::<Vec<_>>()
        .join("/")
}

/// Translate an EditorConfig glob pattern into an anchored regex string.
fn glob_to_regex(pattern: &str) -> String {
    let mut out = String::from("^");
    append_glob(&pattern.chars().collect::<Vec<_>>(), &mut out);
    out.push('$');
    out
}

/// Append the regex translation of `chars` to `out`.
fn append_glob(chars: &[char], out: &mut String) {
    let n = chars.len();
    let mut i = 0;
    while i < n {
        let c = chars[i];
        match c {
            '\\' => {
                // Escape: the next character is matched literally.
                if i + 1 < n {
                    push_literal(chars[i + 1], out);
                    i += 2;
                } else {
                    push_literal('\\', out);
                    i += 1;
                }
            }
            '*' => {
                if i + 1 < n && chars[i + 1] == '*' {
                    let mut j = i + 2;
                    while j < n && chars[j] == '*' {
                        j += 1;
                    }
                    if j < n && chars[j] == '/' {
                        // `**/` matches zero or more directory levels.
                        out.push_str("(?:.*/)?");
                        i = j + 1;
                    } else {
                        out.push_str(".*");
                        i = j;
                    }
                } else {
                    out.push_str("[^/]*");
                    i += 1;
                }
            }
            '?' => {
                out.push_str("[^/]");
                i += 1;
            }
            '[' => {
                if let Some(close) = find_class_end(chars, i) {
                    out.push('[');
                    let mut k = i + 1;
                    if chars[k] == '!' || chars[k] == '^' {
                        out.push('^');
                        k += 1;
                    }
                    while k < close {
                        match chars[k] {
                            '\\' | ']' | '^' => {
                                out.push('\\');
                                out.push(chars[k]);
                            }
                            other => out.push(other),
                        }
                        k += 1;
                    }
                    out.push(']');
                    i = close + 1;
                } else {
                    push_literal('[', out);
                    i += 1;
                }
            }
            '{' => {
                if let Some(close) = find_brace_end(chars, i) {
                    let inner = &chars[i + 1..close];
                    if let Some((lo, hi)) = parse_num_range(inner) {
                        push_num_range(lo, hi, out);
                    } else {
                        let alts = split_top_level_commas(inner);
                        if alts.len() <= 1 {
                            // No top-level comma: braces are literal per spec.
                            push_literal('{', out);
                            append_glob(inner, out);
                            push_literal('}', out);
                        } else {
                            out.push_str("(?:");
                            for (idx, alt) in alts.iter().enumerate() {
                                if idx > 0 {
                                    out.push('|');
                                }
                                append_glob(alt, out);
                            }
                            out.push(')');
                        }
                    }
                    i = close + 1;
                } else {
                    push_literal('{', out);
                    i += 1;
                }
            }
            '/' => {
                out.push('/');
                i += 1;
            }
            other => {
                push_literal(other, out);
                i += 1;
            }
        }
    }
}

/// Append a single literal character, escaping regex metacharacters.
fn push_literal(c: char, out: &mut String) {
    out.push_str(&regex::escape(&c.to_string()));
}

/// Find the closing `]` of a character class starting at `start`.
///
/// Per glob rules, a `]` immediately after `[` or `[!`/`[^` is a literal
/// member, not the terminator.
fn find_class_end(chars: &[char], start: usize) -> Option<usize> {
    let n = chars.len();
    let mut k = start + 1;
    if k < n && (chars[k] == '!' || chars[k] == '^') {
        k += 1;
    }
    if k < n && chars[k] == ']' {
        k += 1;
    }
    while k < n {
        match chars[k] {
            '\\' => k += 2,
            ']' => return Some(k),
            _ => k += 1,
        }
    }
    None
}

/// Find the matching `}` for the `{` at `start`, honoring nesting and escapes.
fn find_brace_end(chars: &[char], start: usize) -> Option<usize> {
    let n = chars.len();
    let mut depth = 0usize;
    let mut k = start;
    while k < n {
        match chars[k] {
            '\\' => k += 2,
            '{' => {
                depth += 1;
                k += 1;
            }
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(k);
                }
                k += 1;
            }
            _ => k += 1,
        }
    }
    None
}

/// Split brace contents on top-level commas (ignoring nested braces/classes).
fn split_top_level_commas(chars: &[char]) -> Vec<Vec<char>> {
    let mut parts: Vec<Vec<char>> = Vec::new();
    let mut current: Vec<char> = Vec::new();
    let mut brace_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut k = 0;
    while k < chars.len() {
        let c = chars[k];
        match c {
            '\\' => {
                current.push(c);
                if k + 1 < chars.len() {
                    current.push(chars[k + 1]);
                    k += 2;
                    continue;
                }
            }
            '{' => {
                brace_depth += 1;
                current.push(c);
            }
            '}' => {
                brace_depth = brace_depth.saturating_sub(1);
                current.push(c);
            }
            '[' => {
                bracket_depth += 1;
                current.push(c);
            }
            ']' => {
                bracket_depth = bracket_depth.saturating_sub(1);
                current.push(c);
            }
            ',' if brace_depth == 0 && bracket_depth == 0 => {
                parts.push(std::mem::take(&mut current));
            }
            _ => current.push(c),
        }
        k += 1;
    }
    parts.push(current);
    parts
}

/// Parse a numeric range `{m..n}` body (e.g. `1..10`, `-3..3`).
fn parse_num_range(chars: &[char]) -> Option<(i64, i64)> {
    let s: String = chars.iter().collect();
    let (lo, hi) = s.split_once("..")?;
    let lo: i64 = lo.trim().parse().ok()?;
    let hi: i64 = hi.trim().parse().ok()?;
    Some((lo, hi))
}

/// Append the regex for a numeric range. Small ranges expand to an explicit
/// alternation; very large ranges fall back to matching any integer.
fn push_num_range(lo: i64, hi: i64, out: &mut String) {
    let (lo, hi) = if lo <= hi { (lo, hi) } else { (hi, lo) };
    const MAX_EXPANSION: i64 = 4096;
    if hi - lo <= MAX_EXPANSION {
        out.push_str("(?:");
        for (idx, n) in (lo..=hi).enumerate() {
            if idx > 0 {
                out.push('|');
            }
            out.push_str(&n.to_string());
        }
        out.push(')');
    } else {
        out.push_str("(?:-?[0-9]+)");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::StdFileSystem;
    use std::fs;

    // --- Glob translation / matching ---

    fn matches(pattern: &str, name: &str) -> bool {
        regex_for(pattern).is_match(name)
    }

    #[test]
    fn glob_star_matches_extension() {
        assert!(matches("*.py", "main.py"));
        assert!(!matches("*.py", "main.rs"));
    }

    #[test]
    fn glob_star_does_not_cross_slash() {
        // Anchored against a relative path.
        assert!(section_matches(
            "/*.py",
            Path::new("/proj"),
            Path::new("/proj/main.py")
        ));
        assert!(!section_matches(
            "/*.py",
            Path::new("/proj"),
            Path::new("/proj/sub/main.py")
        ));
    }

    #[test]
    fn glob_doublestar_crosses_directories() {
        assert!(section_matches(
            "**.py",
            Path::new("/proj"),
            Path::new("/proj/a/b/main.py")
        ));
        assert!(section_matches(
            "lib/**/*.rs",
            Path::new("/proj"),
            Path::new("/proj/lib/a/b/c.rs")
        ));
        assert!(section_matches(
            "lib/**/*.rs",
            Path::new("/proj"),
            Path::new("/proj/lib/c.rs")
        ));
    }

    #[test]
    fn glob_brace_alternation() {
        assert!(matches("*.{js,ts,tsx}", "app.ts"));
        assert!(matches("*.{js,ts,tsx}", "app.tsx"));
        assert!(!matches("*.{js,ts,tsx}", "app.css"));
        assert!(matches("{Makefile,Dockerfile}", "Makefile"));
        assert!(matches("{Makefile,Dockerfile}", "Dockerfile"));
    }

    #[test]
    fn glob_char_class() {
        assert!(matches("file[0-9].txt", "file3.txt"));
        assert!(!matches("file[0-9].txt", "fileA.txt"));
        assert!(matches("file[!0-9].txt", "fileA.txt"));
        assert!(!matches("file[!0-9].txt", "file3.txt"));
    }

    #[test]
    fn glob_question_mark() {
        assert!(matches("file?.txt", "file1.txt"));
        assert!(!matches("file?.txt", "file.txt"));
    }

    #[test]
    fn glob_numeric_range() {
        assert!(matches("page{1..3}.md", "page2.md"));
        assert!(!matches("page{1..3}.md", "page5.md"));
    }

    #[test]
    fn glob_literal_dot_is_not_wildcard() {
        assert!(!matches("a.py", "axpy"));
        assert!(matches("a.py", "a.py"));
    }

    // --- Parsing ---

    #[test]
    fn parse_root_and_sections() {
        let parsed = parse(
            "root = true\n\
             \n\
             [*]\n\
             indent_style = space\n\
             indent_size = 2\n\
             \n\
             [*.go]\n\
             indent_style = tab\n",
        );
        assert!(parsed.root);
        assert_eq!(parsed.sections.len(), 2);
        assert_eq!(parsed.sections[0].pattern, "*");
        assert_eq!(parsed.sections[1].pattern, "*.go");
    }

    #[test]
    fn parse_ignores_comments() {
        let parsed = parse("# comment\n; also comment\n[*]\nindent_size = 4\n");
        assert!(!parsed.root);
        assert_eq!(parsed.sections.len(), 1);
        assert_eq!(
            parsed.sections[0].props,
            vec![("indent_size".into(), "4".into())]
        );
    }

    // --- apply_section ---

    #[test]
    fn apply_indent_style_and_size() {
        let mut acc = EditorConfigSettings::default();
        apply_section(
            &mut acc,
            &Section {
                pattern: "*".into(),
                props: vec![
                    ("indent_style".into(), "space".into()),
                    ("indent_size".into(), "2".into()),
                ],
            },
        );
        assert_eq!(acc.use_tabs, Some(false));
        assert_eq!(acc.tab_size, Some(2));
    }

    #[test]
    fn apply_tab_width_overrides_indent_size() {
        let mut acc = EditorConfigSettings::default();
        apply_section(
            &mut acc,
            &Section {
                pattern: "*".into(),
                props: vec![
                    ("indent_size".into(), "4".into()),
                    ("tab_width".into(), "8".into()),
                ],
            },
        );
        assert_eq!(acc.tab_size, Some(8));
    }

    // --- resolve_for_file (real filesystem walk) ---

    #[test]
    fn resolve_walks_up_and_respects_precedence() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        fs::write(
            root.join(".editorconfig"),
            "root = true\n[*]\nindent_style = space\nindent_size = 2\n[*.go]\nindent_style = tab\n",
        )
        .unwrap();
        let sub = root.join("src");
        fs::create_dir(&sub).unwrap();
        // A nearer config overrides the width for everything in `src/`.
        fs::write(sub.join(".editorconfig"), "[*]\nindent_size = 8\n").unwrap();

        let fs_impl = StdFileSystem;

        let go = resolve_for_file(&fs_impl, &sub.join("main.go"));
        assert_eq!(go.use_tabs, Some(true), "*.go -> tabs");
        assert_eq!(go.tab_size, Some(8), "nearer config overrides size");

        let txt = resolve_for_file(&fs_impl, &sub.join("notes.txt"));
        assert_eq!(txt.use_tabs, Some(false), "default [*] -> spaces");
        assert_eq!(txt.tab_size, Some(8));

        let top = resolve_for_file(&fs_impl, &root.join("top.txt"));
        assert_eq!(top.tab_size, Some(2), "root-level uses root config size");
    }

    #[test]
    fn resolve_stops_at_root() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path();
        // No `.editorconfig` here -> nothing resolved, no walk past tmp.
        let result = resolve_for_file(&StdFileSystem, &root.join("foo.rs"));
        assert!(result.is_empty());
    }
}
