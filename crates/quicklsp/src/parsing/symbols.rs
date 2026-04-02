//! Symbol types and extraction from tokenizer output.

use super::tokenizer::{Token, TokenKind};

/// A symbol extracted from source code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub line: usize,
    pub col: usize,
    /// The keyword that introduced this symbol (e.g., "fn", "class").
    pub def_keyword: String,
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
            if tokens[i].kind == TokenKind::DefKeyword {
                if i + 1 < tokens.len() && tokens[i + 1].kind == TokenKind::Ident {
                    let keyword = &tokens[i];
                    let ident = &tokens[i + 1];
                    symbols.push(Symbol {
                        name: ident.text.clone(),
                        kind: SymbolKind::from_keyword(&keyword.text),
                        line: ident.line,
                        col: ident.col,
                        def_keyword: keyword.text.clone(),
                    });
                    i += 2;
                    continue;
                }
            }
            i += 1;
        }

        symbols
    }
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
}
