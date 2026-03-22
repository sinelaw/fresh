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
    PunctuationBracket,
    PunctuationDelimiter,
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
    /// The highlight category that produced this span (for theme inspection)
    pub category: Option<HighlightCategory>,
}

impl HighlightCategory {
    /// Get the theme key path for this category (e.g., "syntax.keyword").
    pub fn theme_key(&self) -> &'static str {
        match self {
            Self::Keyword => "syntax.keyword",
            Self::String => "syntax.string",
            Self::Comment => "syntax.comment",
            Self::Function => "syntax.function",
            Self::Type => "syntax.type",
            Self::Variable | Self::Property => "syntax.variable",
            Self::Constant | Self::Number | Self::Attribute => "syntax.constant",
            Self::Operator => "syntax.operator",
            Self::PunctuationBracket => "syntax.punctuation_bracket",
            Self::PunctuationDelimiter => "syntax.punctuation_delimiter",
        }
    }

    /// Get a human-readable display name for this category.
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Attribute => "Attribute",
            Self::Comment => "Comment",
            Self::Constant => "Constant",
            Self::Function => "Function",
            Self::Keyword => "Keyword",
            Self::Number => "Number",
            Self::Operator => "Operator",
            Self::PunctuationBracket => "Punctuation Bracket",
            Self::PunctuationDelimiter => "Punctuation Delimiter",
            Self::Property => "Property",
            Self::String => "String",
            Self::Type => "Type",
            Self::Variable => "Variable",
        }
    }
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
        HighlightCategory::PunctuationBracket => theme.syntax_punctuation_bracket,
        HighlightCategory::PunctuationDelimiter => theme.syntax_punctuation_delimiter,
        HighlightCategory::Property => theme.syntax_variable,
        HighlightCategory::String => theme.syntax_string,
        HighlightCategory::Type => theme.syntax_type,
        HighlightCategory::Variable => theme.syntax_variable,
    }
}
