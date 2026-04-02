//! Lightweight Symbol Extraction
//!
//! A custom, zero-dependency tokenizer that extracts symbol definitions from
//! source code using language-family-aware state machines. No external grammar
//! files or tree-sitter required.
//!
//! Supports: Rust, Go, C/C++, Python, JavaScript/TypeScript, Java, C#, Ruby

pub mod symbols;
pub mod tokenizer;
