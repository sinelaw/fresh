//! Common highlighting types used by both WASM and runtime builds.
//!
//! This module provides the base types needed for syntax highlighting
//! without depending on tree-sitter (which is not WASM-compatible).

use ratatui::style::Color;
use std::ops::Range;

/// Highlight category for syntax elements.
///
/// These categories map to theme colors for consistent styling
/// across different highlighting backends (syntect, tree-sitter).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightCategory {
    Attribute,
    Comment,
    Constant,
    Function,
    Keyword,
    Number,
    Operator,
    Property,
    String,
    Type,
    Variable,
}

/// A highlighted span of text with color information.
#[derive(Debug, Clone)]
pub struct HighlightSpan {
    /// Byte range in the buffer
    pub range: Range<usize>,
    /// Color for this span
    pub color: Color,
}

/// Get the color for a highlight category from the theme.
pub fn highlight_color(category: HighlightCategory, theme: &crate::view::theme::Theme) -> Color {
    match category {
        HighlightCategory::Attribute => theme.syntax_constant,
        HighlightCategory::Comment => theme.syntax_comment,
        HighlightCategory::Constant => theme.syntax_constant,
        HighlightCategory::Function => theme.syntax_function,
        HighlightCategory::Keyword => theme.syntax_keyword,
        HighlightCategory::Number => theme.syntax_constant,
        HighlightCategory::Operator => theme.syntax_operator,
        HighlightCategory::Property => theme.syntax_variable,
        HighlightCategory::String => theme.syntax_string,
        HighlightCategory::Type => theme.syntax_type,
        HighlightCategory::Variable => theme.syntax_variable,
    }
}
