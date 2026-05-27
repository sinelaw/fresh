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
    VariableBuiltin,
    /// `markup.inserted.*` — added lines in a diff. Fills the row's
    /// background with `editor.diff_add_bg`; foreground stays at the
    /// theme's default so the `+` token reads as plain text.
    Inserted,
    /// `markup.deleted.*` — removed lines. Background fill from
    /// `editor.diff_remove_bg`.
    Deleted,
    /// `meta.diff.range.*` / `markup.changed.*` — hunk header rows
    /// and any "changed" marker rows. Background fill from
    /// `editor.diff_modify_bg`.
    Changed,
}

/// A highlighted span of text with color information.
#[derive(Debug, Clone)]
pub struct HighlightSpan {
    /// Byte range in the buffer
    pub range: Range<usize>,
    /// Foreground color for this span
    pub color: Color,
    /// Optional background color. Set for diff categories (Inserted,
    /// Deleted, Changed); `None` for the existing fg-only categories.
    /// When set on a category whose `bg_extends_to_line_end()` is
    /// true, the renderer fills the rest of the visible row with
    /// this bg even past the span's end byte.
    pub bg: Option<Color>,
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
            Self::VariableBuiltin => "syntax.variable_builtin",
            Self::Constant | Self::Number | Self::Attribute => "syntax.constant",
            Self::Operator => "syntax.operator",
            Self::PunctuationBracket => "syntax.punctuation_bracket",
            Self::PunctuationDelimiter => "syntax.punctuation_delimiter",
            // Diff bg-driven categories reuse the existing
            // editor-level diff colour keys (the same ones live_diff /
            // side-by-side diff already use); the inspector surfaces
            // those rather than a separate syntax.* key.
            Self::Inserted => "editor.diff_add_bg",
            Self::Deleted => "editor.diff_remove_bg",
            Self::Changed => "editor.diff_modify_bg",
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
            Self::VariableBuiltin => "Variable (Builtin)",
            Self::Inserted => "Diff Inserted",
            Self::Deleted => "Diff Deleted",
            Self::Changed => "Diff Changed",
        }
    }

    /// Whether this category's bg fill should extend past the
    /// scoped text to the end of the visible row.
    ///
    /// Syntect's `Diff` grammar scopes the whole `+`/`-`/`@@` line
    /// under `markup.inserted/deleted` / `meta.diff.range`, but the
    /// scope ends at the trailing newline rather than the terminal's
    /// right edge — leaving short rows half-coloured otherwise.
    /// Matches the existing `extend_to_line_end` overlay behaviour.
    pub fn bg_extends_to_line_end(&self) -> bool {
        matches!(self, Self::Inserted | Self::Deleted | Self::Changed)
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
        HighlightCategory::VariableBuiltin => theme.syntax_variable_builtin,
        // Diff categories don't have a dedicated fg — they're a bg
        // wash on top of the buffer's default fg. Return the editor
        // foreground so cells keep readable contrast.
        HighlightCategory::Inserted | HighlightCategory::Deleted | HighlightCategory::Changed => {
            theme.editor_fg
        }
    }
}

/// Optional background color for a category. `None` for the existing
/// fg-only categories; `Some(theme.editor.diff_*_bg)` for the diff
/// categories so the renderer paints the row's bg with the same
/// colours `live_diff` / side-by-side diff use.
pub fn highlight_bg(
    category: HighlightCategory,
    theme: &crate::view::theme::Theme,
) -> Option<Color> {
    match category {
        HighlightCategory::Inserted => Some(theme.diff_add_bg),
        HighlightCategory::Deleted => Some(theme.diff_remove_bg),
        HighlightCategory::Changed => Some(theme.diff_modify_bg),
        _ => None,
    }
}
