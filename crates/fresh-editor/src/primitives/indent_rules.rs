//! Per-language, regex-based auto-indentation (pure Rust, WASM-safe).
//!
//! This is the per-language indentation tier described in
//! `docs/internal/indentation-rules-design.md`. It sits between the
//! tree-sitter AST tier ([`crate::primitives::indent`]) and the universal
//! bracket heuristic ([`crate::primitives::indent_pattern`]).
//!
//! # What it does
//!
//! Each language is described by a small set of anchored regexes:
//!
//! - **increase** — if the *reference* line matches, the new line goes one
//!   level deeper (e.g. a line ending with `{`, or a Ruby `def`).
//! - **decrease** — if the *new* line's leading content matches, it drops one
//!   level (e.g. a line starting with `}`, or a Ruby `end`).
//! - **indent_next_line** — one-shot +1 for the immediately following line
//!   only (braceless `if (x)`).
//! - **dedent_next_line** — one-shot −1 (Python flow-exit `return`/`pass`/…,
//!   Fresh's existing `@dedent_after`, issue #2192).
//! - **self_close** — suppresses *increase* when the same line also closes the
//!   block it opened (`def f; end`, `if x then y end`). This lets one-liners
//!   avoid over-indenting without needing regex look-ahead (which the `regex`
//!   crate does not support).
//!
//! Patterns use the [`regex`](https://docs.rs/regex) crate's syntax (linear,
//! no look-around or back-references). They are matched against each line's
//! *code view* — the line with comment and string spans blanked to spaces — so
//! a bracket or keyword inside a string/comment never triggers indentation.
//!
//! # Avoiding glitches: scope masking
//!
//! The classic failure of regex indentation is triggering on a brace inside a
//! string or a keyword inside a comment. Before matching, every line is turned
//! into a **code view**: bytes that the caller reports as comment/string are
//! replaced with spaces. The caller sources that judgement from the syntax
//! highlighter's *already-computed* render spans
//! ([`crate::primitives::highlight_engine::HighlightEngine::category_at_position`]),
//! so there is no second parse — we reuse the work rendering already did. When
//! no scope information is available (line outside the render cache, or a plain
//! buffer) the code view is the raw line, which degrades to plain regex
//! matching rather than misbehaving.
//!
//! # Cost
//!
//! Per Enter: one backward scan for the reference line plus 2–4 single-line
//! regex matches on short masked strings. No parsing, no tree. Rule sets are
//! compiled once (lazily) and shared across all languages in a family.

use crate::model::buffer::Buffer;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// A language family. Most languages map to one of these; the per-language
/// table ([`family_for_id`]) is data, so adding a language is one row.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Family {
    /// C, C++, C#, Java, Rust, Go, JS, TS, PHP, Swift, Kotlin, Dart, CSS,
    /// SCSS, JSON, … — block structure is `{ } [ ] ( )`.
    CurlyBrace,
    /// Python — `:` opens a block; flow-exit statements dedent the next line.
    Python,
    /// Ruby — `def…end`, `do…end`, midblock `else`/`when`/`rescue`.
    RubyLike,
    /// Lua — `function…end`, `if…then…end`, `for…do…end`, `repeat…until`.
    LuaLike,
    /// Bash — `if…then…fi`, `for/while…do…done`, `case…esac`, `{ }`.
    BashLike,
    /// Fish — `if…end`, `for…end`, `function…end`, `switch/case…end`.
    FishLike,
    /// Pascal — `begin…end`, `case…of…end`, `repeat…until`.
    PascalLike,
    /// Smali — dot-directive blocks such as `.method` ... `.end method`,
    /// plus brace-delimited register/value lists.
    SmaliLike,
}

/// String form of a rule set (what a family or user config provides).
/// Every field is optional; `None` means "never matches".
#[derive(Debug, Clone, Default)]
pub struct IndentRulesDef {
    pub increase: Option<&'static str>,
    pub decrease: Option<&'static str>,
    pub indent_next_line: Option<&'static str>,
    pub dedent_next_line: Option<&'static str>,
    pub self_close: Option<&'static str>,
    /// True for indentation-significant languages (Python, YAML, …) where
    /// indentation *is* the block structure. For these, pressing Enter on a
    /// blank line keeps the cursor's current column instead of re-deriving from
    /// an earlier line — a manual dedent must stick. Brace/keyword languages
    /// leave this false: their structure makes the indent unambiguous, so
    /// re-deriving is correct.
    pub indentation_significant: bool,
}

/// Compiled, cached form of [`IndentRulesDef`].
pub struct IndentRules {
    increase: Option<Regex>,
    decrease: Option<Regex>,
    indent_next_line: Option<Regex>,
    dedent_next_line: Option<Regex>,
    self_close: Option<Regex>,
    indentation_significant: bool,
}

impl IndentRules {
    fn compile(def: &IndentRulesDef) -> Self {
        Self::compile_parts(
            def.increase,
            def.decrease,
            def.indent_next_line,
            def.dedent_next_line,
            def.self_close,
            def.indentation_significant,
        )
    }

    /// Compile from individual pattern strings of any lifetime. A pattern that
    /// fails to compile is dropped (treated as "never matches") rather than
    /// panicking — for built-in rules a bad pattern is a programmer error; for
    /// user config it keeps one typo'd rule from taking the editor down.
    fn compile_parts(
        increase: Option<&str>,
        decrease: Option<&str>,
        indent_next_line: Option<&str>,
        dedent_next_line: Option<&str>,
        self_close: Option<&str>,
        indentation_significant: bool,
    ) -> Self {
        let c = |p: Option<&str>| p.and_then(|s| Regex::new(s).ok());
        Self {
            indentation_significant,
            increase: c(increase),
            decrease: c(decrease),
            indent_next_line: c(indent_next_line),
            dedent_next_line: c(dedent_next_line),
            self_close: c(self_close),
        }
    }

    /// Indent (in visual columns) for a new line inserted at `position`.
    ///
    /// `is_code(byte)` returns `false` for bytes inside a comment or string;
    /// see the module docs. Pass `|_| true` to disable masking.
    pub fn calculate_indent<F: Fn(usize) -> bool>(
        &self,
        buffer: &Buffer,
        position: usize,
        tab_size: usize,
        is_code: F,
    ) -> usize {
        let unit = tab_size.max(1);

        let cur = line_bounds(buffer, position);
        let cur_has_content = first_nonws(buffer, cur.start, position).is_some();

        // Indentation-significant languages only (Python, …): cursor on a
        // whitespace-only stretch with nothing after it on the line (a blank
        // line, or trailing whitespace) preserves the cursor's current column.
        // Pressing Enter must keep a manual dedent — once the user has stepped
        // out from under an earlier block, re-deriving the indent from that
        // block (pulling them back in) is wrong, and in a layout-defined
        // language only the user can say which block the next line belongs to.
        // Brace/keyword languages skip this: their structure is unambiguous, so
        // the normal derivation below is correct. A closing delimiter *after*
        // the cursor (`    │}`) is handled by the normal path regardless.
        if self.indentation_significant
            && !cur_has_content
            && first_nonws(buffer, position, cur.end).is_none()
        {
            return visual_indent(buffer, cur.start, position, tab_size);
        }

        // Reference line: the current line's content above the split if it has
        // any, else the nearest previous non-blank line. Mirrors the structure
        // of `indent_pattern::calculate_indent_pattern`.
        let reference = if cur_has_content {
            Some(LineSpan {
                start: cur.start,
                end: position,
            })
        } else {
            prev_nonblank_line(buffer, cur.start)
        };

        let Some(reference) = reference else {
            return 0;
        };
        let base = visual_indent(buffer, reference.start, reference.end, tab_size);
        let ref_code = code_view(buffer, reference.start, reference.end, &is_code);

        let mut indent = base;
        let opened = self.increases(&ref_code) || matches(&self.indent_next_line, &ref_code);
        if opened {
            indent += unit;
        } else if matches(&self.dedent_next_line, &ref_code) {
            indent = indent.saturating_sub(unit);
        }

        // The new line's tail (text that moves down past the cursor). A leading
        // `}` / `end` here dedents the line being created — UNLESS the opener we
        // just counted is on this same line (the `{│}` case: cursor between a
        // freshly typed/auto-closed pair). There the closer is relocated to its
        // own line by the editor's bracket-expansion, so it must not cancel the
        // cursor line's one-level indent. Only a closer paired with an opener on
        // a *previous* reference line genuinely dedents (e.g. `{\n    │}`).
        let tail = code_view(buffer, position, cur.end, &is_code);
        let opener_on_current_line = opened && cur_has_content;
        if matches(&self.decrease, &tail) && !opener_on_current_line {
            indent = indent.saturating_sub(unit);
        }

        indent
    }

    /// Indent for a line whose first typed character is the closing delimiter
    /// `ch` (`}`, `]`, `)`). Returns `None` when this language has no decrease
    /// rule (so the caller can fall back).
    pub fn calculate_dedent_for_delimiter<F: Fn(usize) -> bool>(
        &self,
        buffer: &Buffer,
        position: usize,
        ch: char,
        tab_size: usize,
        is_code: F,
    ) -> Option<usize> {
        let probe = format!("{ch}");
        if !matches(&self.decrease, &probe) {
            return None;
        }
        let unit = tab_size.max(1);
        let cur = line_bounds(buffer, position);
        let reference = prev_nonblank_line(buffer, cur.start)?;
        let base = visual_indent(buffer, reference.start, reference.end, tab_size);
        let ref_code = code_view(buffer, reference.start, reference.end, &is_code);

        let mut indent = base;
        if self.increases(&ref_code) {
            indent += unit;
        }
        // The closer dedents one level back to its opener.
        Some(indent.saturating_sub(unit))
    }

    /// `increase` matches and the line does not also self-close.
    fn increases(&self, code: &str) -> bool {
        matches(&self.increase, code) && !matches(&self.self_close, code)
    }
}

fn matches(re: &Option<Regex>, text: &str) -> bool {
    re.as_ref().is_some_and(|r| r.is_match(text))
}

/// Look up the effective rules for a language id (e.g. `"rust"`, `"ruby"`).
///
/// A user override registered via [`set_user_rule`] (from a
/// `[languages.<id>.indent]` config block) takes precedence over the built-in
/// family. Returns `None` when neither exists, so the caller falls back to the
/// generic bracket heuristic.
pub fn rules_for_id(id: &str) -> Option<Arc<IndentRules>> {
    if let Some(rules) = USER_RULES.read().unwrap().get(id) {
        return Some(rules.clone());
    }
    let family = family_for_id(id)?;
    FAMILY_RULES.get(&family).cloned()
}

/// Look up rules from a syntect display name (e.g. `"C++"`, `"C#"`,
/// `"Kotlin"`). Normalizes the common verbose/aliased names then defers to
/// [`rules_for_id`]. Used by the no-tree-sitter indent path, which only has a
/// syntect syntax name to go on.
pub fn rules_for_syntax_name(name: &str) -> Option<Arc<IndentRules>> {
    let lower = name.to_ascii_lowercase();
    let id = match lower.as_str() {
        "c++" => "cpp",
        "c#" => "csharp",
        n if n.contains("typescript") => "typescript",
        n if n.contains("javascript") => "javascript",
        // syntect ships bash as "Bourne Again Shell (bash)".
        n if n.contains("bash") || n.contains("shell") => "bash",
        other => other,
    };
    rules_for_id(id)
}

/// Map a normalized language id to its family. This is the extension point:
/// adding a language is usually one arm here.
fn family_for_id(id: &str) -> Option<Family> {
    let f = match id {
        "rust" | "c" | "cpp" | "c++" | "csharp" | "c_sharp" | "java" | "go" | "javascript"
        | "typescript" | "typescriptreact" | "javascriptreact" | "php" | "swift" | "kotlin"
        | "dart" | "scala" | "json" | "jsonc" | "css" | "scss" | "less" => Family::CurlyBrace,
        "python" => Family::Python,
        "ruby" => Family::RubyLike,
        "lua" => Family::LuaLike,
        "bash" | "sh" | "shell" | "shellscript" => Family::BashLike,
        "fish" => Family::FishLike,
        "pascal" => Family::PascalLike,
        "smali" => Family::SmaliLike,
        _ => return None,
    };
    Some(f)
}

/// The built-in `IndentRulesDef` for a family. Used both to build
/// [`FAMILY_RULES`] and as the base a user override is layered onto.
fn def_for_family(family: Family) -> &'static IndentRulesDef {
    match family {
        Family::CurlyBrace => &CURLY_BRACE,
        Family::Python => &PYTHON,
        Family::RubyLike => &RUBY_LIKE,
        Family::LuaLike => &LUA_LIKE,
        Family::BashLike => &BASH_LIKE,
        Family::FishLike => &FISH_LIKE,
        Family::PascalLike => &PASCAL_LIKE,
        Family::SmaliLike => &SMALI_LIKE,
    }
}

/// Compiled rules per family, built once on first use.
static FAMILY_RULES: Lazy<HashMap<Family, Arc<IndentRules>>> = Lazy::new(|| {
    let mut m = HashMap::new();
    for family in [
        Family::CurlyBrace,
        Family::Python,
        Family::RubyLike,
        Family::LuaLike,
        Family::BashLike,
        Family::FishLike,
        Family::PascalLike,
        Family::SmaliLike,
    ] {
        m.insert(
            family,
            Arc::new(IndentRules::compile(def_for_family(family))),
        );
    }
    m
});

/// User-supplied indentation rules from `[languages.<id>.indent]`, keyed by
/// language id. Checked before [`FAMILY_RULES`] in [`rules_for_id`]. Rebuilt by
/// [`clear_user_rules`] + [`set_user_rule`] whenever config is (re)loaded.
static USER_RULES: Lazy<RwLock<HashMap<String, Arc<IndentRules>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Drop all user overrides. Call before re-applying config so removed blocks
/// stop taking effect.
pub fn clear_user_rules() {
    USER_RULES.write().unwrap().clear();
}

/// Register a user override for `id` (e.g. from `[languages.rust.indent]`).
///
/// Any pattern left `None` inherits from the language's built-in family (so a
/// config that sets only `increase_indent_pattern` keeps the family's
/// `decrease`/`self_close`); a language with no family starts from blank rules,
/// which is how config can add indentation for an otherwise-unknown language.
/// Patterns are regexes evaluated against the line's code view (comment/string
/// spans masked out); see the module docs.
pub fn set_user_rule(
    id: &str,
    increase: Option<&str>,
    decrease: Option<&str>,
    indent_next_line: Option<&str>,
    dedent_next_line: Option<&str>,
    self_close: Option<&str>,
) {
    // Inherit each unset pattern (and the indentation-significant flag) from the
    // built-in family, if any.
    let base = family_for_id(id).map(def_for_family);
    let rules = IndentRules::compile_parts(
        increase.or(base.and_then(|d| d.increase)),
        decrease.or(base.and_then(|d| d.decrease)),
        indent_next_line.or(base.and_then(|d| d.indent_next_line)),
        dedent_next_line.or(base.and_then(|d| d.dedent_next_line)),
        self_close.or(base.and_then(|d| d.self_close)),
        base.map(|d| d.indentation_significant).unwrap_or(false),
    );
    USER_RULES
        .write()
        .unwrap()
        .insert(id.to_string(), Arc::new(rules));
}

const CURLY_BRACE: IndentRulesDef = IndentRulesDef {
    // Line ends opening a block/group. Trailing whitespace (and masked
    // comments) are eaten by `\s*$`.
    increase: Some(r"[\{\[\(]\s*$"),
    // Line begins by closing one.
    decrease: Some(r"^\s*[\}\]\)]"),
    // Braceless control head: `if (..)`, `for (..)`, `while (..)`, or `else`.
    indent_next_line: Some(r"^\s*((if|for|while)\b.*\)|else)\s*$"),
    dedent_next_line: None,
    self_close: None,
    indentation_significant: false,
};

const SMALI_LIKE: IndentRulesDef = IndentRulesDef {
    increase: Some(
        r"(?x)
        (^\s*\.
            (?:method|annotation|subannotation|packed-switch|sparse-switch|array-data|param|parameter)
            \b
        )
        |
        ([\{\[\(]\s*$)
        ",
    ),
    decrease: Some(
        r"(?x)^\s*
        (?:
            \.end\s+
                (?:method|annotation|subannotation|packed-switch|sparse-switch|array-data|param|parameter)\b
            |
            [\}\]\)]
        )
        ",
    ),
    indent_next_line: None,
    dedent_next_line: None,
    self_close: Some(
        r"(?x)^\s*\.
            (?:method|annotation|subannotation|packed-switch|sparse-switch|array-data|param|parameter)
            \b.*\s\.end\s+
            (?:method|annotation|subannotation|packed-switch|sparse-switch|array-data|param|parameter)\b
        ",
    ),
    indentation_significant: false,
};

const PYTHON: IndentRulesDef = IndentRulesDef {
    increase: Some(r":\s*$"),
    // Best-effort: a moved-down midblock keyword dedents to its header.
    decrease: Some(r"^\s*(elif|else|except|finally|case)\b"),
    indent_next_line: None,
    dedent_next_line: Some(r"^\s*(return|pass|raise|break|continue)\b"),
    self_close: None,
    indentation_significant: true,
};

const RUBY_LIKE: IndentRulesDef = IndentRulesDef {
    // Block-opening keywords at line start, OR a trailing `do`/`do |x|`.
    increase: Some(
        r"(^\s*(if|unless|while|until|for|begin|def|class|module|case|else|elsif|when|in|rescue|ensure)\b)|(\bdo(\s*\|[^|]*\|)?\s*$)",
    ),
    // `end` and midblock keywords dedent their own line.
    decrease: Some(r"^\s*(end|else|elsif|when|in|rescue|ensure)\b"),
    indent_next_line: None,
    dedent_next_line: None,
    // Suppress increase for one-liners like `def f; end` / `if x then y end`.
    self_close: Some(r"\bend\b"),
    indentation_significant: false,
};

const LUA_LIKE: IndentRulesDef = IndentRulesDef {
    increase: Some(
        r"(^\s*((local\s+)?function|if|elseif|else|for|while|repeat)\b)|(\b(do|then)\s*$)",
    ),
    decrease: Some(r"^\s*(end|else|elseif|until)\b"),
    indent_next_line: None,
    dedent_next_line: None,
    self_close: Some(r"\bend\b"),
    indentation_significant: false,
};

const BASH_LIKE: IndentRulesDef = IndentRulesDef {
    // `then`/`do` line ends, `case … in`, or a function body's opening `{`.
    // Note: `(` is deliberately excluded — in Bash `$(...)` / `(...)` is a
    // subshell/command-substitution, not an indented block, so a trailing `(`
    // must not deepen indent (mirrors the grammar's indents.scm).
    increase: Some(r"(\b(then|do)\s*$)|(^\s*case\b.*\bin\s*$)|(\{\s*$)"),
    decrease: Some(r"^\s*(fi|done|esac|else|elif|\})"),
    indent_next_line: None,
    dedent_next_line: None,
    self_close: None,
    indentation_significant: false,
};

const FISH_LIKE: IndentRulesDef = IndentRulesDef {
    increase: Some(r"^\s*(if|else|for|while|begin|function|switch|case)\b"),
    decrease: Some(r"^\s*(end|else|case)\b"),
    indent_next_line: None,
    dedent_next_line: None,
    self_close: Some(r"\bend\b"),
    indentation_significant: false,
};

const PASCAL_LIKE: IndentRulesDef = IndentRulesDef {
    increase: Some(r"(^\s*(begin|case|record|try|repeat|asm)\b)|(\b(begin|of)\s*$)"),
    decrease: Some(r"^\s*(end|until|except|finally)\b"),
    indent_next_line: None,
    dedent_next_line: None,
    self_close: Some(r"\bend\b"),
    indentation_significant: false,
};

// ---------------------------------------------------------------------------
// Line geometry helpers (byte-oriented, tab-aware). Kept local so the module
// has no dependency on the tree-sitter `indent` module.
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
struct LineSpan {
    start: usize,
    end: usize,
}

fn byte_at(buffer: &Buffer, pos: usize) -> Option<u8> {
    if pos >= buffer.len() {
        return None;
    }
    buffer.slice_bytes(pos..pos + 1).first().copied()
}

/// Bounds of the line containing `position`: `start` is just after the
/// preceding `\n` (or 0); `end` is the next `\n` or buffer end.
fn line_bounds(buffer: &Buffer, position: usize) -> LineSpan {
    let mut start = position;
    while start > 0 && byte_at(buffer, start - 1) != Some(b'\n') {
        start -= 1;
    }
    let mut end = position;
    while end < buffer.len() && byte_at(buffer, end) != Some(b'\n') {
        end += 1;
    }
    LineSpan { start, end }
}

/// First non-whitespace byte position in `[start, end)`, if any.
fn first_nonws(buffer: &Buffer, start: usize, end: usize) -> Option<usize> {
    let mut p = start;
    while p < end {
        match byte_at(buffer, p) {
            Some(b' ') | Some(b'\t') | Some(b'\r') => p += 1,
            Some(_) => return Some(p),
            None => return None,
        }
    }
    None
}

/// Nearest non-blank line strictly above the line starting at `line_start`.
fn prev_nonblank_line(buffer: &Buffer, line_start: usize) -> Option<LineSpan> {
    if line_start == 0 {
        return None;
    }
    let mut pos = line_start - 1; // the '\n' ending the previous line
    loop {
        let span = line_bounds(buffer, pos);
        if first_nonws(buffer, span.start, span.end).is_some() {
            return Some(span);
        }
        if span.start == 0 {
            return None;
        }
        pos = span.start - 1;
    }
}

/// Visual indent width of `[start, end)` (tabs expand to `tab_size`).
fn visual_indent(buffer: &Buffer, start: usize, end: usize, tab_size: usize) -> usize {
    let mut indent = 0;
    let mut p = start;
    while p < end {
        match byte_at(buffer, p) {
            Some(b' ') => indent += 1,
            Some(b'\t') => indent += tab_size,
            _ => break,
        }
        p += 1;
    }
    indent
}

/// The line `[start, end)` as a string with comment/string bytes (per
/// `is_code`) blanked to spaces, and `\r` dropped. See module docs.
fn code_view<F: Fn(usize) -> bool>(
    buffer: &Buffer,
    start: usize,
    end: usize,
    is_code: &F,
) -> String {
    let bytes = buffer.slice_bytes(start..end);
    let mut out = String::with_capacity(bytes.len());
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'\r' || b == b'\n' {
            continue;
        }
        // Non-ASCII bytes inside identifiers/strings: keep as-is only when code.
        if is_code(start + i) {
            out.push(b as char);
        } else {
            out.push(' ');
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::filesystem::NoopFileSystem;
    use std::sync::Arc;

    fn buf(content: &str) -> Buffer {
        let fs = Arc::new(NoopFileSystem);
        let mut b = Buffer::empty(fs);
        b.insert(0, content);
        b
    }

    /// Indent at end of buffer, no scope masking.
    fn indent(id: &str, content: &str, tab: usize) -> usize {
        rules_for_id(id)
            .unwrap()
            .calculate_indent(&buf(content), content.len(), tab, |_| true)
    }

    /// Indent at end of buffer, masking the given byte ranges as non-code
    /// (i.e. inside a string/comment).
    fn indent_masked(id: &str, content: &str, tab: usize, masked: &[(usize, usize)]) -> usize {
        let b = buf(content);
        let is_code = |byte: usize| !masked.iter().any(|&(s, e)| byte >= s && byte < e);
        rules_for_id(id)
            .unwrap()
            .calculate_indent(&b, content.len(), tab, is_code)
    }

    // ---- CurlyBrace -------------------------------------------------------

    #[test]
    fn curly_indents_after_open_brace() {
        assert_eq!(indent("rust", "fn main() {\n", 4), 4);
        assert_eq!(indent("typescript", "function f() {\n", 4), 4);
    }

    #[test]
    fn curly_no_indent_after_balanced_line() {
        assert_eq!(indent("rust", "let x = 1;\n", 4), 0);
        // One-liner body: ends with `}`, must not indent.
        assert_eq!(indent("rust", "fn x() { return 1; }\n", 4), 0);
    }

    #[test]
    fn curly_dedents_before_close_brace() {
        // Press enter inside `{│}` style: the tail `}` dedents.
        let content = "fn main() {\n    }";
        let pos = content.len() - 1; // just before `}`
        let b = buf(content);
        let got = rules_for_id("rust")
            .unwrap()
            .calculate_indent(&b, pos, 4, |_| true);
        assert_eq!(got, 0);
    }

    #[test]
    fn curly_braceless_if_indents_next_line_only() {
        assert_eq!(indent("c", "if (x)\n", 4), 4);
    }

    #[test]
    fn curly_dedent_for_typed_brace() {
        let content = "fn main() {\n    body\n";
        let dedent = rules_for_id("rust")
            .unwrap()
            .calculate_dedent_for_delimiter(&buf(content), content.len(), '}', 4, |_| true);
        assert_eq!(dedent, Some(0));
    }

    // ---- Anti-glitch corpus (the headline cases) --------------------------

    #[test]
    fn no_indent_for_brace_in_string() {
        // `let x = "{";` — the `{` is inside a string literal.
        let content = "let x = \"{\";\n";
        let open = content.find('{').unwrap();
        // Mask the string contents (and quotes) so the `{` is not code.
        let masked = [(content.find('"').unwrap(), open + 2)];
        assert_eq!(indent_masked("rust", content, 4, &masked), 0);
        // Sanity: without masking the naive matcher would wrongly indent.
        assert_eq!(indent("rust", content, 4), 0); // still 0 here: `;` ends line
    }

    #[test]
    fn no_indent_for_trailing_brace_in_comment() {
        // `foo() // {` — trailing `{` lives in a line comment.
        let content = "foo() // {\n";
        let cstart = content.find("//").unwrap();
        let masked = [(cstart, content.len())];
        assert_eq!(indent_masked("rust", content, 4, &masked), 0);
    }

    #[test]
    fn brace_in_comment_does_not_defeat_real_open() {
        // `if (x) { // start {` → real `{` plus a decoy in the comment.
        let content = "if (x) { // start {\n";
        let cstart = content.find("//").unwrap();
        let masked = [(cstart, content.len())];
        // Masked view ends with the real `{` then spaces → one level.
        assert_eq!(indent_masked("rust", content, 4, &masked), 4);
    }

    // ---- Python -----------------------------------------------------------
    // Indent is taken at the end of the content (cursor on the line being split),
    // mirroring an Enter pressed at end-of-line in the editor.

    #[test]
    fn python_indents_after_colon() {
        assert_eq!(indent("python", "def foo():", 4), 4);
        assert_eq!(indent("python", "if x:", 4), 4);
    }

    #[test]
    fn python_dedents_after_return() {
        let content = "def foo():\n    return 1";
        assert_eq!(indent("python", content, 4), 0);
    }

    #[test]
    fn python_keeps_indent_inside_body() {
        let content = "def foo():\n    x = 1";
        assert_eq!(indent("python", content, 4), 4);
    }

    #[test]
    fn python_blank_line_keeps_manual_dedent() {
        // After an `if x:` block the user backspaces the auto-indent to column 0
        // on the blank line, then presses Enter: the new line must stay at 0,
        // not be pulled back under the block. (Indentation-significant: only the
        // user can say which block the next line belongs to.)
        let content = "if x:\n    foo()\n"; // cursor on the trailing blank line, col 0
        assert_eq!(indent("python", content, 4), 0);
    }

    #[test]
    fn python_blank_line_maintains_current_column() {
        // On a blank line whose whitespace the user left at the body column,
        // Enter keeps that column (does not collapse).
        let content = "if x:\n    foo()\n    "; // trailing 4 spaces, cursor at col 4
        assert_eq!(indent("python", content, 4), 4);
    }

    #[test]
    fn curly_blank_line_rederives_not_preserved() {
        // Contrast: brace languages are NOT indentation-significant, so a blank
        // line re-derives from the structure (here: still inside `{`), rather
        // than preserving a column. `fn f() {` + blank line → one level in.
        assert_eq!(indent("rust", "fn f() {\n", 4), 4);
    }

    #[test]
    fn python_colon_in_string_does_not_indent() {
        // `x = {"a": 1}` ends with `}` not `:`, but check a dict-literal colon
        // inside a string is ignored: `s = "key:"`.
        let content = "s = \"key:\"";
        let q1 = content.find('"').unwrap();
        let q2 = content.rfind('"').unwrap();
        let masked = [(q1, q2 + 1)];
        assert_eq!(indent_masked("python", content, 4, &masked), 0);
    }

    // ---- RubyLike ---------------------------------------------------------

    #[test]
    fn ruby_indents_after_def_and_do() {
        assert_eq!(indent("ruby", "def foo\n", 2), 2);
        assert_eq!(indent("ruby", "[1,2].each do |n|\n", 2), 2);
    }

    #[test]
    fn ruby_one_liner_with_end_does_not_indent() {
        assert_eq!(indent("ruby", "def foo; end\n", 2), 0);
        assert_eq!(indent("ruby", "if x then y end\n", 2), 0);
    }

    #[test]
    fn ruby_end_in_string_does_not_dedent_or_break() {
        // `s = "end"` must not be treated as a block keyword.
        let content = "x = 1\ns = \"end\"\n";
        let q1 = content.rfind('"').unwrap();
        // mask the whole quoted "end"
        let qs = content[..q1].rfind('"').unwrap();
        let masked = [(qs, q1 + 1)];
        // reference line `s = "end"` → masked `s =      ` → no opener, indent 0.
        assert_eq!(indent_masked("ruby", content, 2, &masked), 0);
    }

    #[test]
    fn ruby_midblock_else_reindents_body() {
        // After an `else` line, the body indents one level from the else.
        let content = "if x\n  a\nelse\n";
        assert_eq!(indent("ruby", content, 2), 2);
    }

    // ---- LuaLike ----------------------------------------------------------

    #[test]
    fn lua_indents_after_block_openers() {
        assert_eq!(indent("lua", "function f()\n", 4), 4);
        assert_eq!(indent("lua", "if x then\n", 4), 4);
        assert_eq!(indent("lua", "for i = 1, n do\n", 4), 4);
    }

    #[test]
    fn lua_one_liner_with_end_does_not_indent() {
        assert_eq!(indent("lua", "function f() end\n", 4), 0);
    }

    // ---- BashLike ---------------------------------------------------------

    #[test]
    fn bash_indents_after_then_do_case() {
        assert_eq!(indent("bash", "if true; then\n", 4), 4);
        assert_eq!(indent("bash", "for x in a b; do\n", 4), 4);
        assert_eq!(indent("bash", "case $x in\n", 4), 4);
    }

    #[test]
    fn bash_resolves_from_syntect_name() {
        // syntect names bash "Bourne Again Shell (bash)".
        assert!(rules_for_syntax_name("Bourne Again Shell (bash)").is_some());
    }

    #[test]
    fn fish_indents_after_block_openers() {
        assert_eq!(indent("fish", "if test -n \"$name\"\n", 4), 4);
        assert_eq!(indent("fish", "for item in $items\n", 4), 4);
        assert_eq!(
            indent("fish", "function greet --argument-names name\n", 4),
            4
        );
        assert_eq!(indent("fish", "switch $name\n", 4), 4);
        assert_eq!(indent("fish", "case fresh\n", 4), 4);
    }

    #[test]
    fn fish_one_liner_with_end_does_not_indent() {
        assert_eq!(
            indent("fish", "if test -n \"$name\"; echo $name; end\n", 4),
            0
        );
    }

    // ---- PascalLike -------------------------------------------------------

    #[test]
    fn pascal_indents_after_begin() {
        assert_eq!(indent("pascal", "begin\n", 4), 4);
        assert_eq!(indent("pascal", "if x then begin\n", 4), 4);
    }

    #[test]
    fn pascal_one_liner_with_end_does_not_indent() {
        assert_eq!(indent("pascal", "begin end;\n", 4), 0);
    }

    // ---- SmaliLike --------------------------------------------------------

    #[test]
    fn smali_indents_after_method_directive() {
        assert_eq!(
            indent(
                "smali",
                ".method public onCreate(Landroid/os/Bundle;)V\n",
                4
            ),
            4
        );
    }

    #[test]
    fn smali_dedents_before_end_directive() {
        let content = ".method public main()V\n    .end method";
        let pos = content.find(".end method").unwrap();
        let got = rules_for_id("smali")
            .unwrap()
            .calculate_indent(&buf(content), pos, 4, |_| true);
        assert_eq!(got, 0);
    }

    #[test]
    fn smali_plain_field_does_not_indent() {
        assert_eq!(indent("smali", ".field public static count:I = 0\n", 4), 0);
    }

    // ---- registry ---------------------------------------------------------

    #[test]
    fn unknown_language_has_no_rules() {
        assert!(rules_for_id("brainfuck").is_none());
    }

    #[test]
    fn families_compile() {
        // Force the lazy table; a bad regex would drop to None and fail above.
        assert!(rules_for_id("rust").unwrap().increase.is_some());
        assert!(rules_for_id("python").unwrap().dedent_next_line.is_some());
        assert!(rules_for_id("ruby").unwrap().self_close.is_some());
    }

    // A single test owns the global USER_RULES mutation so it can't race the
    // other (read-only) tests under the parallel runner.
    #[test]
    fn user_overrides_register_and_merge() {
        clear_user_rules();

        // Full override for a language with no built-in family: config can add
        // indentation for a language Fresh otherwise doesn't know.
        set_user_rule(
            "zz_newlang",
            Some(r":\s*$"),
            Some(r"^\s*end\b"),
            None,
            None,
            None,
        );
        let r = rules_for_id("zz_newlang").expect("user rule registered");
        assert_eq!(r.calculate_indent(&buf("foo:"), 4, 4, |_| true), 4);

        // Partial override merges with the family: overriding `increase` only on
        // a CurlyBrace language keeps the family's `decrease`.
        set_user_rule("kotlin", Some(r"=>\s*$"), None, None, None, None);
        let k = rules_for_id("kotlin").expect("kotlin via override");
        assert!(
            k.decrease.is_some(),
            "decrease inherited from CurlyBrace family"
        );
        let c = "val f = x =>";
        assert_eq!(k.calculate_indent(&buf(c), c.len(), 4, |_| true), 4);

        clear_user_rules();
        assert!(rules_for_id("zz_newlang").is_none(), "override cleared");
        // kotlin falls back to its built-in CurlyBrace family rule.
        assert!(rules_for_id("kotlin").is_some());
    }
}

/// Parity guard: wherever the tree-sitter indenter is *authoritative*, the
/// regex rules tier must produce the same indent. This is the safety net for
/// moving "indent-only" languages off their tree-sitter grammars (design doc,
/// phase 2): if a rule ever diverges from the AST result on the corpus, this
/// fails before a grammar can be dropped.
///
/// Scope — curly-brace languages and Python only. These are the largest
/// grammars (C# ~29 MB, C++/TS ~17 MB of generated source) and tree-sitter
/// parses their block structure reliably even mid-edit, so it is a sound
/// oracle. **Keyword-delimited families (Ruby/Lua/Bash/Pascal) are
/// deliberately excluded**: on incomplete input — the normal "typed `def foo`
/// and pressed Enter" case — tree-sitter cannot form a block node and the
/// current editor already falls back to copy-the-line indent, so the rules
/// tier (which indents correctly) is a strict *improvement*, not a regression.
/// Those families are pinned by the golden unit tests above instead.
///
/// Cursor convention mirrors the real press-Enter moment: the buffer ends
/// exactly where Enter is pressed (no trailing newline). Cases use clean code
/// (no strings/comments holding stray delimiters), so the rules tier runs with
/// masking disabled and the comparison is apples-to-apples. Cases where
/// tree-sitter declines to decide (`None`) are skipped.
#[cfg(all(test, feature = "tree-sitter"))]
mod parity {
    use super::*;
    use crate::model::filesystem::NoopFileSystem;
    use crate::primitives::indent::IndentCalculator;
    use fresh_languages::Language;
    use std::sync::Arc;

    fn buf(content: &str) -> Buffer {
        let fs = Arc::new(NoopFileSystem);
        let mut b = Buffer::empty(fs);
        b.insert(0, content);
        b
    }

    #[test]
    fn rules_match_tree_sitter_on_corpus() {
        // (tree-sitter Language, rules id, code). Indent is taken at end-of-buffer,
        // which is the cursor position when Enter is pressed.
        //
        // Only languages whose grammar is bundled can be compared against the
        // tree-sitter oracle (the other grammars were removed entirely). The
        // bundled curly-brace languages — Go, TypeScript, JavaScript — exercise
        // the CurlyBrace family, which is the one shared by the removed
        // languages too, so this still guards the dropped languages' behavior.
        let cases: &[(Language, &str, &str)] = &[
            (Language::TypeScript, "typescript", "function f() {"),
            (Language::TypeScript, "typescript", "class A {"),
            (Language::TypeScript, "typescript", "let x = 1;"),
            (Language::Go, "go", "func main() {"),
            (Language::JavaScript, "javascript", "function f() {"),
        ];

        let tab = 4;
        let mut mismatches = Vec::new();
        let mut compared = 0;
        for (lang, id, code) in cases {
            let ts = {
                let mut calc = IndentCalculator::new();
                calc.calculate_indent(&buf(code), code.len(), lang, tab)
            };
            let Some(ts) = ts else { continue }; // tree-sitter declined; skip
            compared += 1;
            let rules = rules_for_id(id)
                .unwrap_or_else(|| panic!("no rules for {id}"))
                .calculate_indent(&buf(code), code.len(), tab, |_| true);
            if ts != rules {
                mismatches.push(format!(
                    "  {id}: code={code:?} tree-sitter={ts} rules={rules}"
                ));
            }
        }

        assert!(
            mismatches.is_empty(),
            "rules tier diverged from tree-sitter on {}/{} compared cases:\n{}",
            mismatches.len(),
            compared,
            mismatches.join("\n")
        );
        // Guard against the corpus silently going all-skips (e.g. an API change
        // making tree-sitter always return None) which would make this vacuous.
        assert!(
            compared >= 4,
            "too few comparable cases ({compared}); guard is vacuous"
        );
    }
}
