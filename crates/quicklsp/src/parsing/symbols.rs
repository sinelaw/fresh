//! Symbol types and extraction from tokenizer output.

use super::tokenizer::{LangFamily, Token, TokenKind};

/// A symbol extracted from source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub line: usize,
    pub col: usize,
    /// The keyword that introduced this symbol (e.g., "fn", "class").
    pub def_keyword: String,
    /// Doc comment extracted from lines above the definition.
    pub doc_comment: Option<String>,
    /// The full signature line(s) of the definition.
    pub signature: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Function,
    Method,
    Class,
    Struct,
    Enum,
    Interface,
    Constant,
    Variable,
    Module,
    TypeAlias,
    Trait,
    Unknown,
}

impl SymbolKind {
    /// Infer symbol kind from the definition keyword.
    pub fn from_keyword(keyword: &str) -> Self {
        match keyword {
            "fn" | "func" | "function" | "def" => Self::Function,
            "class" | "record" => Self::Class,
            "struct" | "union" => Self::Struct,
            "enum" => Self::Enum,
            "interface" => Self::Interface,
            "trait" => Self::Trait,
            "type" | "typedef" => Self::TypeAlias,
            "const" | "static" => Self::Constant,
            "let" | "var" | "val" => Self::Variable,
            "mod" | "module" | "namespace" => Self::Module,
            _ => Self::Unknown,
        }
    }
}

impl Symbol {
    /// Extract symbols from tokenizer output.
    ///
    /// The tokenizer emits `DefKeyword` tokens followed by `Ident` tokens.
    /// We pair them up to produce symbols.
    pub fn from_tokens(tokens: &[Token]) -> Vec<Symbol> {
        let mut symbols = Vec::new();
        let mut i = 0;

        while i < tokens.len() {
            if tokens[i].kind == TokenKind::DefKeyword
                && i + 1 < tokens.len()
                && tokens[i + 1].kind == TokenKind::Ident
            {
                let keyword = &tokens[i];
                let ident = &tokens[i + 1];
                symbols.push(Symbol {
                    name: ident.text.clone(),
                    kind: SymbolKind::from_keyword(&keyword.text),
                    line: ident.line,
                    col: ident.col,
                    def_keyword: keyword.text.clone(),
                    doc_comment: None,
                    signature: None,
                });
                i += 2;
                continue;
            }
            i += 1;
        }

        symbols
    }

    /// Enrich symbols with doc comments and signatures extracted from source text.
    ///
    /// This is a post-processing step that reads the source lines around each
    /// symbol's definition to extract:
    /// - Doc comments: contiguous comment lines immediately above the definition
    /// - Signature: the definition line(s) up to the opening brace or body
    pub fn enrich_from_source(symbols: &mut [Symbol], source: &str, lang: LangFamily) {
        let lines: Vec<&str> = source.lines().collect();
        for sym in symbols.iter_mut() {
            sym.doc_comment = extract_doc_comment(&lines, sym.line, lang);
            sym.signature = extract_signature(&lines, sym.line, sym.col, lang);
        }
    }
}

/// Extract doc comment from lines above a definition.
///
/// Walks backwards from the line before the definition, collecting contiguous
/// comment lines. Supports `///`, `//!`, `//`, `#`, and `/** ... */` blocks.
fn extract_doc_comment(lines: &[&str], def_line: usize, lang: LangFamily) -> Option<String> {
    let mut doc_lines: Vec<String> = Vec::new();

    // Scan backwards for comment lines above the definition
    if def_line > 0 {
        let mut i = def_line - 1;

        loop {
            let trimmed = lines.get(i).map(|l| l.trim()).unwrap_or("");

            if trimmed.is_empty() {
                // Empty line — stop unless we haven't collected anything yet
                // (allow one blank line gap for languages that use it)
                break;
            }

            match lang {
                LangFamily::Python | LangFamily::Ruby => {
                    if let Some(stripped) = trimmed.strip_prefix('#') {
                        doc_lines.push(stripped.trim().to_string());
                    } else {
                        break;
                    }
                }
                _ => {
                    // /// doc comment (Rust) or /** JSDoc/Javadoc */
                    if let Some(stripped) = trimmed.strip_prefix("///") {
                        doc_lines.push(stripped.trim().to_string());
                    } else if let Some(stripped) = trimmed.strip_prefix("//!") {
                        doc_lines.push(stripped.trim().to_string());
                    } else if let Some(stripped) = trimmed.strip_prefix("//") {
                        doc_lines.push(stripped.trim().to_string());
                    } else if trimmed.starts_with('*') && !trimmed.starts_with("*/") {
                        // Inside a /** ... */ block comment
                        let stripped = trimmed.strip_prefix('*').unwrap_or(trimmed).trim();
                        if !stripped.is_empty() {
                            doc_lines.push(stripped.to_string());
                        }
                    } else if trimmed.starts_with("/**") {
                        // Opening of a block doc comment
                        let stripped = trimmed
                            .strip_prefix("/**")
                            .unwrap_or("")
                            .trim()
                            .trim_end_matches("*/")
                            .trim();
                        if !stripped.is_empty() {
                            doc_lines.push(stripped.to_string());
                        }
                        break; // This is the top of the block comment
                    } else if trimmed == "*/" {
                        // End of block comment — keep scanning upward
                    } else {
                        break;
                    }
                }
            }

            if i == 0 {
                break;
            }
            i -= 1;
        }
    } // end if def_line > 0

    // Also check for Python docstrings (triple-quoted string after def/class line)
    if matches!(lang, LangFamily::Python) && doc_lines.is_empty() {
        if let Some(next_line) = lines.get(def_line + 1) {
            let trimmed = next_line.trim();
            if let Some(rest) = trimmed
                .strip_prefix("\"\"\"")
                .or_else(|| trimmed.strip_prefix("'''"))
            {
                let quote = &trimmed[..3];
                // Single-line docstring
                if let Some(content) = rest.strip_suffix(quote) {
                    return Some(content.trim().to_string());
                }
                // Multi-line docstring
                let mut ds_lines = Vec::new();
                if !rest.is_empty() {
                    ds_lines.push(rest.to_string());
                }
                for line in lines.iter().skip(def_line + 2) {
                    let t = line.trim();
                    if t.contains(quote) {
                        let before = t.strip_suffix(quote).unwrap_or(t).trim();
                        if !before.is_empty() {
                            ds_lines.push(before.to_string());
                        }
                        break;
                    }
                    ds_lines.push(t.to_string());
                }
                if !ds_lines.is_empty() {
                    return Some(ds_lines.join("\n"));
                }
            }
        }
    }

    if doc_lines.is_empty() {
        return None;
    }

    // Reverse since we collected bottom-up
    doc_lines.reverse();
    Some(doc_lines.join("\n"))
}

/// Extract the signature of a definition from the source lines.
///
/// Collects from the definition keyword through the end of parameters/return type,
/// stopping at `{`, `:` (Python), or a reasonable line limit.
fn extract_signature(
    lines: &[&str],
    def_line: usize,
    _def_col: usize,
    lang: LangFamily,
) -> Option<String> {
    let first_line = lines.get(def_line)?;
    let trimmed = first_line.trim();

    // For Python, the signature is everything up to the colon
    if matches!(lang, LangFamily::Python) {
        if let Some(colon_pos) = trimmed.find(':') {
            return Some(trimmed[..colon_pos].trim().to_string());
        }
        return Some(trimmed.to_string());
    }

    // Check if this is a simple declaration (no parens expected):
    // const, static, let, var, type, struct, enum, interface, trait, mod, etc.
    // For these, the signature is just the first line up to '{' or ';'
    let has_parens = trimmed.contains('(');
    if !has_parens {
        let result = trimmed
            .split('{')
            .next()
            .unwrap_or(trimmed)
            .split(';')
            .next()
            .unwrap_or(trimmed)
            .trim()
            .to_string();
        return if result.is_empty() {
            None
        } else {
            Some(result)
        };
    }

    // For definitions with parentheses (functions), collect up to closing paren + return type
    let mut sig = String::new();
    let mut paren_depth: i32 = 0;
    let mut found_open_paren = false;

    for (offset, line) in lines.iter().enumerate().skip(def_line).take(5) {
        let part = if offset == def_line {
            line.trim()
        } else {
            if !sig.is_empty() {
                sig.push(' ');
            }
            line.trim()
        };

        for ch in part.chars() {
            match ch {
                '{' if paren_depth == 0 => {
                    // End of signature at opening brace
                    let result = sig.trim().to_string();
                    return if result.is_empty() {
                        None
                    } else {
                        Some(result)
                    };
                }
                '(' => {
                    found_open_paren = true;
                    paren_depth += 1;
                    sig.push(ch);
                }
                ')' => {
                    paren_depth -= 1;
                    sig.push(ch);
                    if found_open_paren && paren_depth == 0 {
                        // Continue to collect return type on this line
                        continue;
                    }
                }
                _ => {
                    sig.push(ch);
                }
            }
        }

        // If we closed all parens on this line, we have the full signature
        if found_open_paren && paren_depth == 0 {
            let result = sig.trim().to_string();
            return if result.is_empty() {
                None
            } else {
                Some(result)
            };
        }
    }

    // Fallback: return what we collected
    let result = sig.trim().trim_end_matches('{').trim().to_string();
    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

/// Extract parameter names and types from a signature string for signature help.
///
/// Returns a list of (name, full_parameter_text) pairs.
pub fn extract_parameters(signature: &str) -> Vec<String> {
    // Find the parenthesized parameter list
    let open = match signature.find('(') {
        Some(p) => p,
        None => return Vec::new(),
    };
    let close = match signature.rfind(')') {
        Some(p) => p,
        None => return Vec::new(),
    };

    if close <= open + 1 {
        return Vec::new(); // empty parens
    }

    let params_str = &signature[open + 1..close];

    // Split by commas, respecting nested generics/parens
    let mut params = Vec::new();
    let mut depth = 0i32;
    let mut current = String::new();

    for ch in params_str.chars() {
        match ch {
            '<' | '(' | '[' => {
                depth += 1;
                current.push(ch);
            }
            '>' | ')' | ']' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    params.push(trimmed);
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        params.push(trimmed);
    }

    params
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing::tokenizer::{self, LangFamily};

    #[test]
    fn extract_rust_symbols() {
        let src = "fn main() {}\nstruct Config {}\nenum Color {}";
        let tokens = tokenizer::scan(src, LangFamily::Rust);
        let symbols = Symbol::from_tokens(&tokens);

        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "main");
        assert_eq!(symbols[0].kind, SymbolKind::Function);
        assert_eq!(symbols[1].name, "Config");
        assert_eq!(symbols[1].kind, SymbolKind::Struct);
        assert_eq!(symbols[2].name, "Color");
        assert_eq!(symbols[2].kind, SymbolKind::Enum);
    }

    #[test]
    fn extract_python_symbols() {
        let src = "def process():\n    pass\nclass Handler:\n    pass";
        let tokens = tokenizer::scan(src, LangFamily::Python);
        let symbols = Symbol::from_tokens(&tokens);

        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].name, "process");
        assert_eq!(symbols[0].kind, SymbolKind::Function);
        assert_eq!(symbols[1].name, "Handler");
        assert_eq!(symbols[1].kind, SymbolKind::Class);
    }

    #[test]
    fn enrich_rust_doc_comments() {
        let src = "/// This is the doc.\n/// Second line.\nfn foo(x: i32) -> bool {}";
        let tokens = tokenizer::scan(src, LangFamily::Rust);
        let mut symbols = Symbol::from_tokens(&tokens);
        Symbol::enrich_from_source(&mut symbols, src, LangFamily::Rust);

        assert_eq!(symbols.len(), 1);
        assert_eq!(
            symbols[0].doc_comment.as_deref(),
            Some("This is the doc.\nSecond line.")
        );
        assert_eq!(
            symbols[0].signature.as_deref(),
            Some("fn foo(x: i32) -> bool")
        );
    }

    #[test]
    fn enrich_python_docstring() {
        let src =
            "def greet(name):\n    \"\"\"Say hello to someone.\"\"\"\n    print(f\"Hi {name}\")";
        let tokens = tokenizer::scan(src, LangFamily::Python);
        let mut symbols = Symbol::from_tokens(&tokens);
        Symbol::enrich_from_source(&mut symbols, src, LangFamily::Python);

        assert_eq!(symbols.len(), 1);
        assert_eq!(
            symbols[0].doc_comment.as_deref(),
            Some("Say hello to someone.")
        );
        assert_eq!(symbols[0].signature.as_deref(), Some("def greet(name)"));
    }

    #[test]
    fn enrich_python_hash_comment() {
        let src = "# Validates user data.\ndef validate(data):\n    pass";
        let tokens = tokenizer::scan(src, LangFamily::Python);
        let mut symbols = Symbol::from_tokens(&tokens);
        Symbol::enrich_from_source(&mut symbols, src, LangFamily::Python);

        assert_eq!(
            symbols[0].doc_comment.as_deref(),
            Some("Validates user data.")
        );
    }

    #[test]
    fn enrich_jsdoc_block_comment() {
        let src = "/** Create a config object. */\nfunction createConfig(): Config {}";
        let tokens = tokenizer::scan(src, LangFamily::JsTs);
        let mut symbols = Symbol::from_tokens(&tokens);
        Symbol::enrich_from_source(&mut symbols, src, LangFamily::JsTs);

        assert_eq!(
            symbols[0].doc_comment.as_deref(),
            Some("Create a config object.")
        );
        assert_eq!(
            symbols[0].signature.as_deref(),
            Some("function createConfig(): Config")
        );
    }

    #[test]
    fn signature_const_declaration() {
        let src = "const MAX_SIZE: usize = 1024;";
        let tokens = tokenizer::scan(src, LangFamily::Rust);
        let mut symbols = Symbol::from_tokens(&tokens);
        Symbol::enrich_from_source(&mut symbols, src, LangFamily::Rust);

        assert_eq!(
            symbols[0].signature.as_deref(),
            Some("const MAX_SIZE: usize = 1024")
        );
    }

    #[test]
    fn extract_params_rust() {
        let params = extract_parameters("fn foo(a: i32, b: &str, c: Vec<u8>) -> bool");
        assert_eq!(params, vec!["a: i32", "b: &str", "c: Vec<u8>"]);
    }

    #[test]
    fn extract_params_empty() {
        let params = extract_parameters("fn foo() -> bool");
        assert!(params.is_empty());
    }

    #[test]
    fn extract_params_python() {
        let params = extract_parameters("def process(self, data, callback)");
        assert_eq!(params, vec!["self", "data", "callback"]);
    }
}
