//! Lightweight tokenizer for extracting identifiers and keywords from source code.
//!
//! This is a hand-written scanner (not a parser) that identifies:
//! - Keywords that introduce definitions (fn, def, class, struct, etc.)
//! - The identifier immediately following such keywords
//! - String/comment boundaries (to skip false matches inside them)
//!
//! It operates as a single-pass state machine with O(n) time and O(1) memory
//! per file (no AST, no grammar, no allocations beyond the output Vec).
//!
//! ## Unicode Support
//!
//! The scanner correctly handles Unicode identifiers (e.g., `données`, `名前`,
//! `über_config`) by switching to char-boundary-aware iteration when it encounters
//! non-ASCII bytes in identifier positions. Definition keywords are always ASCII,
//! so the hot path for keyword matching stays byte-level for performance.

/// A token extracted by the scanner.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    /// A keyword that introduces a definition (fn, def, class, struct, etc.)
    DefKeyword,
    /// An identifier (variable name, function name, etc.)
    Ident,
}

/// Language family determines comment/string syntax and definition keywords.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LangFamily {
    /// Rust: fn, struct, enum, trait, type, const, static, mod
    Rust,
    /// C/C++: struct, enum, class, typedef, define
    CLike,
    /// Go: func, type, var, const
    Go,
    /// Python: def, class
    Python,
    /// JavaScript/TypeScript: function, class, const, let, var, interface, type, enum
    JsTs,
    /// Java/C#: class, interface, enum, record
    JavaCSharp,
    /// Ruby: def, class, module
    Ruby,
}

impl LangFamily {
    /// Detect language family from file extension.
    pub fn from_extension(ext: &str) -> Option<Self> {
        match ext {
            "rs" => Some(Self::Rust),
            "c" | "h" | "cpp" | "cc" | "cxx" | "hpp" | "hxx" => Some(Self::CLike),
            "go" => Some(Self::Go),
            "py" | "pyi" => Some(Self::Python),
            "js" | "jsx" | "ts" | "tsx" | "mjs" | "cjs" | "mts" => Some(Self::JsTs),
            "java" | "cs" => Some(Self::JavaCSharp),
            "rb" => Some(Self::Ruby),
            _ => None,
        }
    }

    /// Keywords that introduce a named definition in this language family.
    fn def_keywords(&self) -> &[&str] {
        match self {
            Self::Rust => &[
                "fn", "struct", "enum", "trait", "type", "const", "static", "mod",
            ],
            Self::CLike => &["struct", "enum", "class", "union", "typedef", "namespace"],
            Self::Go => &["func", "type", "var", "const"],
            Self::Python => &["def", "class"],
            Self::JsTs => &[
                "function",
                "class",
                "const",
                "let",
                "var",
                "interface",
                "type",
                "enum",
            ],
            Self::JavaCSharp => &["class", "interface", "enum", "record", "struct"],
            Self::Ruby => &["def", "class", "module"],
        }
    }

    /// Does this language use `#` for line comments?
    fn hash_line_comment(&self) -> bool {
        matches!(self, Self::Python | Self::Ruby)
    }

    /// Does this language use `//` for line comments?
    fn slash_line_comment(&self) -> bool {
        !matches!(self, Self::Python | Self::Ruby)
    }

    /// Does this language use `/* */` block comments?
    fn block_comments(&self) -> bool {
        !matches!(self, Self::Python | Self::Ruby)
    }

    /// Does this language have triple-quoted strings?
    fn triple_quote_strings(&self) -> bool {
        matches!(self, Self::Python)
    }
}

/// Check if a character can continue an identifier (Unicode-aware).
/// Matches: ASCII alphanumerics, `_`, and Unicode letters/numbers.
#[inline]
fn is_ident_continue_char(ch: char) -> bool {
    ch == '_' || ch.is_alphanumeric()
}

/// Check if a byte can start an identifier (ASCII fast path).
/// Returns true for ASCII letters, `_`, and UTF-8 leading bytes (0xC0+).
/// Continuation bytes (0x80..0xBF) are NOT identifier starts.
#[inline]
fn is_ident_start_byte(b: u8) -> bool {
    b.is_ascii_alphabetic() || b == b'_' || b >= 0xC0
}

/// Check if a byte can continue an identifier (ASCII fast path).
/// Returns true for ASCII alphanumerics, `_`, and any high byte (>= 0x80)
/// since continuation bytes are valid inside a multi-byte identifier.
#[inline]
fn is_ident_continue_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b >= 0x80
}

/// Scan source code and extract definition-introducing keyword + identifier pairs.
///
/// Returns tokens in document order. Each `DefKeyword` token is followed by
/// an `Ident` token if an identifier was found after the keyword.
pub fn scan(source: &str, lang: LangFamily) -> Vec<Token> {
    let bytes = source.as_bytes();
    let len = bytes.len();
    let def_keywords = lang.def_keywords();

    let mut tokens = Vec::new();
    let mut i = 0;
    let mut line = 0usize;
    let mut line_start = 0usize;

    while i < len {
        let b = bytes[i];

        // Track line numbers
        if b == b'\n' {
            line += 1;
            line_start = i + 1;
            i += 1;
            continue;
        }

        // Skip whitespace (ASCII whitespace only — non-breaking spaces etc. are rare in code)
        if b.is_ascii_whitespace() {
            i += 1;
            continue;
        }

        // Skip line comments
        if lang.hash_line_comment() && b == b'#' {
            i = skip_to_eol(bytes, i);
            continue;
        }
        if lang.slash_line_comment() && b == b'/' && i + 1 < len && bytes[i + 1] == b'/' {
            i = skip_to_eol(bytes, i);
            continue;
        }

        // Skip block comments
        if lang.block_comments() && b == b'/' && i + 1 < len && bytes[i + 1] == b'*' {
            i = skip_block_comment(bytes, i + 2, &mut line, &mut line_start);
            continue;
        }

        // Skip triple-quoted strings (Python)
        if lang.triple_quote_strings()
            && i + 2 < len
            && ((b == b'"' && bytes[i + 1] == b'"' && bytes[i + 2] == b'"')
                || (b == b'\'' && bytes[i + 1] == b'\'' && bytes[i + 2] == b'\''))
        {
            let quote = b;
            i = skip_triple_quote(bytes, i + 3, quote, &mut line, &mut line_start);
            continue;
        }

        // Skip strings
        if b == b'"' || b == b'\'' {
            if b == b'\'' && matches!(lang, LangFamily::Rust) {
                // Rust: single quote is lifetime or char literal
                i += 1;
                if i < len && bytes[i] == b'\\' {
                    i += 2;
                }
                if i < len {
                    i += 1;
                }
                if i < len && bytes[i] == b'\'' {
                    i += 1;
                }
                continue;
            }
            i = skip_string(bytes, i + 1, b, &mut line, &mut line_start);
            continue;
        }

        // Skip backtick template literals (JS/TS)
        if b == b'`' && matches!(lang, LangFamily::JsTs) {
            i = skip_string(bytes, i + 1, b'`', &mut line, &mut line_start);
            continue;
        }

        // Identifier or keyword
        if is_ident_start_byte(b) {
            let start = i;
            let col = compute_col(source, line_start, i);

            // Consume the full identifier (Unicode-aware).
            // If the first byte is ASCII, the keyword fast-path can stay byte-level.
            // If any byte >= 0x80, we need to switch to char iteration.
            i = consume_identifier(source, i);

            let word = &source[start..i];

            // All definition keywords are ASCII, so a word containing non-ASCII
            // can never be a keyword — skip the keyword check.
            let is_ascii_word = word.is_ascii();

            if is_ascii_word && def_keywords.contains(&word) {
                tokens.push(Token {
                    kind: TokenKind::DefKeyword,
                    text: word.to_string(),
                    line,
                    col,
                });

                // Skip whitespace/punctuation to find the identifier name
                i = skip_to_ident(bytes, i, lang, &mut line, &mut line_start);
                if i < len && is_ident_start_byte(bytes[i]) {
                    let name_start = i;
                    let name_col = compute_col(source, line_start, i);
                    i = consume_identifier(source, i);
                    let name = &source[name_start..i];
                    // Skip language noise identifiers (all noise words are ASCII)
                    if !(name.is_ascii() && is_noise_word(name, lang)) {
                        tokens.push(Token {
                            kind: TokenKind::Ident,
                            text: name.to_string(),
                            line,
                            col: name_col,
                        });
                    }
                }
            }
            continue;
        }

        // Skip non-ASCII bytes that aren't identifier starts (e.g., punctuation
        // like em-dashes, exotic operators). Advance by the full UTF-8 character
        // length to avoid landing in the middle of a multi-byte sequence.
        if b >= 0x80 {
            i += utf8_char_len(b);
            continue;
        }

        // Skip everything else (ASCII operators, punctuation, etc.)
        i += 1;
    }

    tokens
}

/// Consume a full identifier starting at byte position `i`, returning the new
/// byte position after the identifier.
///
/// Handles both ASCII-only identifiers (fast path, byte-level) and identifiers
/// containing Unicode characters (char-level iteration).
fn consume_identifier(source: &str, start: usize) -> usize {
    let bytes = source.as_bytes();
    let mut i = start;

    // Fast path: pure ASCII identifier
    while i < bytes.len() && bytes[i] < 0x80 {
        if is_ident_continue_byte(bytes[i]) {
            i += 1;
        } else {
            return i;
        }
    }

    // If we hit a high byte, switch to char-level iteration for the rest
    if i < bytes.len() && bytes[i] >= 0x80 {
        // Continue consuming from position i using chars
        for ch in source[i..].chars() {
            if is_ident_continue_char(ch) {
                i += ch.len_utf8();
            } else {
                break;
            }
        }
    }

    i
}

/// Compute the column as a character offset (not byte offset) for correct
/// Unicode column reporting. Handles the case where `line_start` may not
/// be on a char boundary (can happen when byte-level skipping traverses
/// multi-byte characters in strings/comments).
fn compute_col(source: &str, line_start: usize, byte_pos: usize) -> usize {
    // Find the nearest valid char boundary at or before line_start
    let mut start = line_start.min(source.len());
    while start > 0 && !source.is_char_boundary(start) {
        start -= 1;
    }
    let end = byte_pos.min(source.len());
    source[start..end].chars().count()
}

/// Return the length of a UTF-8 character from its first byte.
#[inline]
fn utf8_char_len(first_byte: u8) -> usize {
    match first_byte {
        0x00..=0x7F => 1,
        0xC0..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xFF => 4,
        _ => 1, // invalid, advance 1 to avoid infinite loop
    }
}

// --- Scanning helpers (byte-level, only used for ASCII constructs) ---

fn skip_to_eol(bytes: &[u8], mut i: usize) -> usize {
    while i < bytes.len() && bytes[i] != b'\n' {
        i += 1;
    }
    i
}

fn skip_block_comment(
    bytes: &[u8],
    mut i: usize,
    line: &mut usize,
    line_start: &mut usize,
) -> usize {
    while i + 1 < bytes.len() {
        if bytes[i] == b'\n' {
            *line += 1;
            *line_start = i + 1;
        }
        if bytes[i] == b'*' && bytes[i + 1] == b'/' {
            return i + 2;
        }
        i += 1;
    }
    bytes.len()
}

fn skip_string(
    bytes: &[u8],
    mut i: usize,
    quote: u8,
    line: &mut usize,
    line_start: &mut usize,
) -> usize {
    while i < bytes.len() {
        if bytes[i] == b'\n' {
            *line += 1;
            *line_start = i + 1;
            if quote != b'`' {
                return i;
            }
        }
        if bytes[i] == b'\\' {
            i += 2;
            continue;
        }
        if bytes[i] == quote {
            return i + 1;
        }
        i += 1;
    }
    bytes.len()
}

fn skip_triple_quote(
    bytes: &[u8],
    mut i: usize,
    quote: u8,
    line: &mut usize,
    line_start: &mut usize,
) -> usize {
    while i + 2 < bytes.len() {
        if bytes[i] == b'\n' {
            *line += 1;
            *line_start = i + 1;
        }
        if bytes[i] == quote && bytes[i + 1] == quote && bytes[i + 2] == quote {
            return i + 3;
        }
        if bytes[i] == b'\\' {
            i += 2;
            continue;
        }
        i += 1;
    }
    bytes.len()
}

/// Skip whitespace, newlines, and minor punctuation between a keyword and its name.
fn skip_to_ident(
    bytes: &[u8],
    mut i: usize,
    lang: LangFamily,
    line: &mut usize,
    line_start: &mut usize,
) -> usize {
    // Skip whitespace first
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        if bytes[i] == b'\n' {
            *line += 1;
            *line_start = i + 1;
        }
        i += 1;
    }

    // In Go, `func (r *Receiver) Name()` — skip the receiver
    if matches!(lang, LangFamily::Go) && i < bytes.len() && bytes[i] == b'(' {
        let mut depth = 0;
        while i < bytes.len() {
            if bytes[i] == b'(' {
                depth += 1;
            } else if bytes[i] == b')' {
                depth -= 1;
                if depth == 0 {
                    i += 1;
                    break;
                }
            } else if bytes[i] == b'\n' {
                *line += 1;
                *line_start = i + 1;
            }
            i += 1;
        }
    }

    // Skip whitespace again
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        if bytes[i] == b'\n' {
            *line += 1;
            *line_start = i + 1;
        }
        i += 1;
    }

    // Skip pointer/reference modifiers: `*`, `&`
    while i < bytes.len() && matches!(bytes[i], b'*' | b'&') {
        i += 1;
    }

    // Skip whitespace again
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        if bytes[i] == b'\n' {
            *line += 1;
            *line_start = i + 1;
        }
        i += 1;
    }

    i
}

/// Filter out noise words that aren't real symbol names.
fn is_noise_word(word: &str, lang: LangFamily) -> bool {
    match lang {
        LangFamily::JsTs => matches!(
            word,
            "async" | "export" | "default" | "abstract" | "static" | "readonly" | "declare" | "new"
        ),
        LangFamily::Rust => matches!(
            word,
            "pub" | "crate" | "super" | "self" | "unsafe" | "async" | "mut" | "ref" | "dyn"
        ),
        LangFamily::CLike => matches!(
            word,
            "static"
                | "inline"
                | "extern"
                | "volatile"
                | "const"
                | "unsigned"
                | "signed"
                | "long"
                | "short"
        ),
        LangFamily::Go => matches!(word, "chan" | "map" | "error"),
        LangFamily::Python => matches!(word, "async"),
        LangFamily::JavaCSharp => matches!(
            word,
            "public"
                | "private"
                | "protected"
                | "static"
                | "final"
                | "abstract"
                | "sealed"
                | "partial"
                | "virtual"
                | "override"
                | "new"
        ),
        LangFamily::Ruby => matches!(word, "self"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn extract_names(source: &str, lang: LangFamily) -> Vec<String> {
        scan(source, lang)
            .into_iter()
            .filter(|t| t.kind == TokenKind::Ident)
            .map(|t| t.text)
            .collect()
    }

    #[test]
    fn rust_functions() {
        let src = r#"
fn main() {}
pub fn process_data(x: i32) -> bool { true }
fn helper() {}
"#;
        assert_eq!(
            extract_names(src, LangFamily::Rust),
            vec!["main", "process_data", "helper"]
        );
    }

    #[test]
    fn rust_structs_enums() {
        let src = r#"
struct Config { field: u32 }
enum Color { Red, Green, Blue }
trait Drawable { fn draw(&self); }
type Alias = Vec<u8>;
"#;
        let names = extract_names(src, LangFamily::Rust);
        assert!(names.contains(&"Config".to_string()));
        assert!(names.contains(&"Color".to_string()));
        assert!(names.contains(&"Drawable".to_string()));
        assert!(names.contains(&"Alias".to_string()));
    }

    #[test]
    fn python_defs() {
        let src = r#"
def process_data(x):
    pass

class MyClass:
    def method(self):
        pass
"#;
        let names = extract_names(src, LangFamily::Python);
        assert_eq!(names, vec!["process_data", "MyClass", "method"]);
    }

    #[test]
    fn python_skips_comments_and_strings() {
        let src = r#"
# def not_a_function():
"""def also_not()"""
def real_function():
    x = "def fake_in_string()"
    pass
"#;
        let names = extract_names(src, LangFamily::Python);
        assert_eq!(names, vec!["real_function"]);
    }

    #[test]
    fn javascript_functions() {
        let src = r#"
function processData(input) {}
class EventEmitter {}
const MAX_SIZE = 100;
let counter = 0;
"#;
        let names = extract_names(src, LangFamily::JsTs);
        assert_eq!(
            names,
            vec!["processData", "EventEmitter", "MAX_SIZE", "counter"]
        );
    }

    #[test]
    fn go_functions() {
        let src = r#"
func main() {}
func (s *Server) handleRequest(w http.ResponseWriter) {}
type Config struct {}
var globalState int
const MaxRetries = 3
"#;
        let names = extract_names(src, LangFamily::Go);
        assert!(names.contains(&"main".to_string()));
        assert!(names.contains(&"handleRequest".to_string()));
        assert!(names.contains(&"Config".to_string()));
        assert!(names.contains(&"globalState".to_string()));
        assert!(names.contains(&"MaxRetries".to_string()));
    }

    #[test]
    fn c_structs() {
        let src = r#"
struct Point { int x; int y; };
enum Color { RED, GREEN, BLUE };
class Widget { public: void draw(); };
"#;
        let names = extract_names(src, LangFamily::CLike);
        assert!(names.contains(&"Point".to_string()));
        assert!(names.contains(&"Color".to_string()));
        assert!(names.contains(&"Widget".to_string()));
    }

    #[test]
    fn skips_block_comments() {
        let src = r#"
/* fn not_a_function() {} */
fn real_function() {}
"#;
        let names = extract_names(src, LangFamily::Rust);
        assert_eq!(names, vec!["real_function"]);
    }

    #[test]
    fn skips_strings() {
        let src = r#"
fn real() {}
let x = "fn fake()";
fn also_real() {}
"#;
        let names = extract_names(src, LangFamily::Rust);
        assert!(names.contains(&"real".to_string()));
        assert!(names.contains(&"also_real".to_string()));
        assert!(!names.contains(&"fake".to_string()));
    }

    #[test]
    fn line_numbers_correct() {
        let src = "fn first() {}\n\nfn second() {}";
        let tokens = scan(src, LangFamily::Rust);
        let idents: Vec<_> = tokens
            .iter()
            .filter(|t| t.kind == TokenKind::Ident)
            .collect();
        assert_eq!(idents[0].text, "first");
        assert_eq!(idents[0].line, 0);
        assert_eq!(idents[1].text, "second");
        assert_eq!(idents[1].line, 2);
    }

    #[test]
    fn typescript_interface() {
        let src = r#"
interface UserProps {
    name: string;
}
type UserId = string;
enum Status { Active, Inactive }
"#;
        let names = extract_names(src, LangFamily::JsTs);
        assert!(names.contains(&"UserProps".to_string()));
        assert!(names.contains(&"UserId".to_string()));
        assert!(names.contains(&"Status".to_string()));
    }

    // --- Unicode tests ---

    #[test]
    fn unicode_identifier_latin_extended() {
        let src = "fn über_config() {}\nstruct Données {}";
        let names = extract_names(src, LangFamily::Rust);
        assert!(
            names.contains(&"über_config".to_string()),
            "Should extract Latin extended identifier: {names:?}"
        );
        assert!(
            names.contains(&"Données".to_string()),
            "Should extract accented identifier: {names:?}"
        );
    }

    #[test]
    fn unicode_identifier_cjk() {
        let src = "def 処理する():\n    pass\nclass 名前:\n    pass";
        let names = extract_names(src, LangFamily::Python);
        assert!(
            names.contains(&"処理する".to_string()),
            "Should extract CJK function name: {names:?}"
        );
        assert!(
            names.contains(&"名前".to_string()),
            "Should extract CJK class name: {names:?}"
        );
    }

    #[test]
    fn unicode_identifier_cyrillic() {
        let src = "fn обработка() {}\nstruct Данные {}";
        let names = extract_names(src, LangFamily::Rust);
        assert!(
            names.contains(&"обработка".to_string()),
            "Should extract Cyrillic identifier: {names:?}"
        );
        assert!(
            names.contains(&"Данные".to_string()),
            "Should extract Cyrillic struct name: {names:?}"
        );
    }

    #[test]
    fn unicode_mixed_ascii_and_extended() {
        let src = "fn café_résumé() {}\nfn plain_ascii() {}";
        let names = extract_names(src, LangFamily::Rust);
        assert!(names.contains(&"café_résumé".to_string()));
        assert!(names.contains(&"plain_ascii".to_string()));
    }

    #[test]
    fn unicode_in_strings_not_extracted() {
        let src = r#"
fn real_fn() {}
let x = "fn 偽物()";
fn also_real() {}
"#;
        let names = extract_names(src, LangFamily::Rust);
        assert!(names.contains(&"real_fn".to_string()));
        assert!(names.contains(&"also_real".to_string()));
        assert!(
            !names.contains(&"偽物".to_string()),
            "Should not extract from strings"
        );
    }

    #[test]
    fn unicode_in_comments_not_extracted() {
        let src = "# def コメント():\ndef 本物():\n    pass";
        let names = extract_names(src, LangFamily::Python);
        assert_eq!(names, vec!["本物"]);
    }

    #[test]
    fn unicode_column_offset() {
        // "fn á() {}" — á is 2 bytes in UTF-8, column should be char-based
        let src = "fn café() {}";
        let tokens = scan(src, LangFamily::Rust);
        let ident = tokens.iter().find(|t| t.kind == TokenKind::Ident).unwrap();
        assert_eq!(ident.text, "café");
        assert_eq!(ident.col, 3); // "fn " = 3 chars
    }

    #[test]
    fn unicode_emoji_in_string_skipped() {
        let src = "fn real() {}\nlet s = \"hello 🌍 world\";\nfn also_real() {}";
        let names = extract_names(src, LangFamily::Rust);
        assert!(names.contains(&"real".to_string()));
        assert!(names.contains(&"also_real".to_string()));
    }

    #[test]
    fn unicode_does_not_corrupt_scanning() {
        // Ensure random Unicode scattered through a file doesn't cause panics
        // or corrupt subsequent ASCII scanning
        let src = "fn a() {} // →→→\nstruct Ω {} /* αβγ */\nfn b() {}";
        let names = extract_names(src, LangFamily::Rust);
        assert!(names.contains(&"a".to_string()));
        assert!(names.contains(&"Ω".to_string()));
        assert!(names.contains(&"b".to_string()));
    }

    #[test]
    fn javascript_unicode_identifier() {
        let src = "function données() {}\nclass Ñoño {}";
        let names = extract_names(src, LangFamily::JsTs);
        assert!(names.contains(&"données".to_string()));
        assert!(names.contains(&"Ñoño".to_string()));
    }

    #[test]
    fn go_unicode_identifier() {
        let src = "func Обработать() {}\ntype Конфиг struct {}";
        let names = extract_names(src, LangFamily::Go);
        assert!(names.contains(&"Обработать".to_string()));
        assert!(names.contains(&"Конфиг".to_string()));
    }
}
