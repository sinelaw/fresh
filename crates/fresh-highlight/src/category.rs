//! The categories syntax is highlighted in, and the TextMate scopes that
//! map to them.

/// A highlight category: what a run of text is coloured as. Themes give each
/// one a colour under [`Category::theme_key`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Category {
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
    /// `markup.inserted.*`: added lines in a diff.
    Inserted,
    /// `markup.deleted.*`: removed lines in a diff.
    Deleted,
    /// `meta.diff.range.*` / `markup.changed.*`: hunk headers and changed
    /// lines in a diff.
    Changed,
}

impl Category {
    /// Every category, in [`Category::index`] order.
    pub const ALL: [Category; 17] = [
        Category::Attribute,
        Category::Comment,
        Category::Constant,
        Category::Function,
        Category::Keyword,
        Category::Number,
        Category::Operator,
        Category::PunctuationBracket,
        Category::PunctuationDelimiter,
        Category::Property,
        Category::String,
        Category::Type,
        Category::Variable,
        Category::VariableBuiltin,
        Category::Inserted,
        Category::Deleted,
        Category::Changed,
    ];

    /// The category's position in [`Category::ALL`].
    pub fn index(self) -> u8 {
        self as u8
    }

    /// Whether the category is a whole-row background wash (the diff
    /// categories) rather than styling of the text it scopes; see
    /// [`RowWash`](crate::RowWash).
    pub fn bg_extends_to_line_end(self) -> bool {
        matches!(self, Self::Inserted | Self::Deleted | Self::Changed)
    }

    /// The theme key of the category's colour (e.g. "syntax.keyword").
    pub fn theme_key(self) -> &'static str {
        match self {
            Self::Keyword => "syntax.keyword",
            Self::String => "syntax.string",
            Self::Comment => "syntax.comment",
            Self::Function => "syntax.function",
            Self::Type => "syntax.type",
            Self::Variable => "syntax.variable",
            Self::Property => "syntax.property",
            Self::VariableBuiltin => "syntax.variable_builtin",
            Self::Constant => "syntax.constant",
            Self::Number => "syntax.number",
            Self::Attribute => "syntax.attribute",
            Self::Operator => "syntax.operator",
            Self::PunctuationBracket => "syntax.punctuation_bracket",
            Self::PunctuationDelimiter => "syntax.punctuation_delimiter",
            // The diff categories are a background wash, coloured by the
            // editor-level diff keys live diff and side-by-side diff use.
            Self::Inserted => "editor.diff_add_bg",
            Self::Deleted => "editor.diff_remove_bg",
            Self::Changed => "editor.diff_modify_bg",
        }
    }
}

/// The category a TextMate scope is highlighted as, if any.
///
/// A scope stack is coloured by its innermost scope that has a category.
pub fn scope_to_category(scope: &str) -> Option<Category> {
    let scope_lower = scope.to_lowercase();

    // Comments - highest priority
    if scope_lower.starts_with("comment") {
        return Some(Category::Comment);
    }

    // Strings
    if scope_lower.starts_with("string") {
        return Some(Category::String);
    }

    // Markdown/markup scopes - handle before generic keyword/punctuation checks
    // See: https://macromates.com/manual/en/language_grammars (TextMate scope naming)
    // Headings: markup.heading and entity.name.section (used by syntect's markdown grammar)
    if scope_lower.starts_with("markup.heading") || scope_lower.starts_with("entity.name.section") {
        return Some(Category::Keyword); // Headers styled like keywords (bold, prominent)
    }
    // Bold: markup.bold
    if scope_lower.starts_with("markup.bold") {
        return Some(Category::Constant); // Bold styled like constants (bright)
    }
    // Italic: markup.italic
    if scope_lower.starts_with("markup.italic") {
        return Some(Category::Variable); // Italic styled like variables
    }
    // Inline code and code blocks: markup.raw, markup.inline.raw
    if scope_lower.starts_with("markup.raw") || scope_lower.starts_with("markup.inline.raw") {
        return Some(Category::String); // Code styled like strings
    }
    // Links: markup.underline.link
    if scope_lower.starts_with("markup.underline.link") {
        return Some(Category::Function); // Links styled like functions (distinct color)
    }
    // Generic underline (often links)
    if scope_lower.starts_with("markup.underline") {
        return Some(Category::Function);
    }
    // Block quotes: markup.quote
    if scope_lower.starts_with("markup.quote") {
        return Some(Category::Comment); // Quotes styled like comments (subdued)
    }
    // Lists: markup.list
    if scope_lower.starts_with("markup.list") {
        return Some(Category::Operator); // List markers styled like operators
    }
    // Strikethrough: markup.strikethrough
    if scope_lower.starts_with("markup.strikethrough") {
        return Some(Category::Comment); // Strikethrough styled subdued
    }

    // Diff scopes (syntect's bundled `Diff` grammar). These scope the
    // entire row, not just the leading +/-/@@ marker, so the renderer
    // can paint a whole-line background by reading the span's bg.
    //
    //   markup.inserted.diff      — `+` line
    //   markup.deleted.diff       — `-` line
    //   meta.diff.range.unified   — `@@ ... @@` hunk header
    //   markup.changed.*          — generic "changed" marker (rare)
    //   meta.diff.header.*        — `diff --git`, `index ...`, file
    //                               headers; render like Type so they
    //                               stand out without a bg wash.
    if scope_lower.starts_with("markup.inserted") {
        return Some(Category::Inserted);
    }
    if scope_lower.starts_with("markup.deleted") {
        return Some(Category::Deleted);
    }
    if scope_lower.starts_with("markup.changed") || scope_lower.starts_with("meta.diff.range") {
        return Some(Category::Changed);
    }
    if scope_lower.starts_with("meta.diff.header") {
        return Some(Category::Type);
    }

    // Word operators (`new`, `typeof`, `instanceof`, `in`, `of`) read as keywords.
    //
    // `keyword.operator.word` is the scope Sublime grammars give operators that
    // are spelled as words rather than symbols, so it belongs here too: C/C++
    // `sizeof`, `alignof`, `typeid`, `static_assert`, `and`/`or`/`not`, Java
    // `instanceof`, Fortran `.and.`, VHDL `mod`/`rem`, Typst `and`/`or`/`not`.
    // Without it they fell through to Operator, whose colour equals plain text
    // in most themes, so they read as unhighlighted identifiers (issue #3644)
    // — and Java's `instanceof` was coloured differently from JavaScript's,
    // which already lands on `keyword.operator.expression` just above.
    if scope_lower.starts_with("keyword.operator.new")
        || scope_lower.starts_with("keyword.operator.expression")
        || scope_lower.starts_with("keyword.operator.word")
    {
        return Some(Category::Keyword);
    }

    // Keywords
    if scope_lower.starts_with("keyword.control")
        || scope_lower.starts_with("keyword.other")
        || scope_lower.starts_with("keyword.declaration")
        || scope_lower.starts_with("keyword")
    {
        // keyword.operator should map to Operator, not Keyword
        if !scope_lower.starts_with("keyword.operator") {
            return Some(Category::Keyword);
        }
    }

    // Punctuation that belongs to a parent construct (comment/string delimiters)
    // These must be checked before the generic punctuation rule below.
    // TextMate grammars assign e.g. `punctuation.definition.comment` to # // /* etc.
    if scope_lower.starts_with("punctuation.definition.comment") {
        return Some(Category::Comment);
    }
    if scope_lower.starts_with("punctuation.definition.string") {
        return Some(Category::String);
    }

    // Operators (keyword.operator only)
    if scope_lower.starts_with("keyword.operator") {
        return Some(Category::Operator);
    }

    // Punctuation brackets ({, }, (, ), [, ], <, >)
    // Covers punctuation.section.*, punctuation.bracket.*,
    // and punctuation.definition.{array,block,brackets,group,inline-table,section,table,tag}
    if scope_lower.starts_with("punctuation.section")
        || scope_lower.starts_with("punctuation.bracket")
        || scope_lower.starts_with("punctuation.definition.array")
        || scope_lower.starts_with("punctuation.definition.block")
        || scope_lower.starts_with("punctuation.definition.brackets")
        || scope_lower.starts_with("punctuation.definition.group")
        || scope_lower.starts_with("punctuation.definition.inline-table")
        || scope_lower.starts_with("punctuation.definition.section")
        || scope_lower.starts_with("punctuation.definition.table")
        || scope_lower.starts_with("punctuation.definition.tag")
        || scope_lower.starts_with("punctuation.definition.typeparameters")
        || scope_lower.starts_with("punctuation.definition.parameters")
        || scope_lower.starts_with("meta.brace")
    {
        return Some(Category::PunctuationBracket);
    }

    // Punctuation delimiters (;, ,, .)
    if scope_lower.starts_with("punctuation.separator")
        || scope_lower.starts_with("punctuation.terminator")
        || scope_lower.starts_with("punctuation.accessor")
    {
        return Some(Category::PunctuationDelimiter);
    }

    // Functions
    if scope_lower.starts_with("entity.name.function")
        || scope_lower.starts_with("support.function")
        || scope_lower.starts_with("meta.function-call")
        || scope_lower.starts_with("variable.function")
    {
        return Some(Category::Function);
    }

    // Types
    if scope_lower.starts_with("entity.name.type")
        || scope_lower.starts_with("entity.name.class")
        || scope_lower.starts_with("entity.name.struct")
        || scope_lower.starts_with("entity.name.enum")
        || scope_lower.starts_with("entity.name.interface")
        || scope_lower.starts_with("entity.name.trait")
        || scope_lower.starts_with("support.type")
        || scope_lower.starts_with("support.class")
    {
        return Some(Category::Type);
    }

    // Storage keywords (class, def, function, var, let, const, etc.) and modifiers
    if scope_lower.starts_with("storage.type") || scope_lower.starts_with("storage.modifier") {
        return Some(Category::Keyword);
    }

    // Constants and numbers
    if scope_lower.starts_with("constant.numeric")
        || scope_lower.starts_with("constant.language.boolean")
    {
        return Some(Category::Number);
    }
    if scope_lower.starts_with("constant") {
        return Some(Category::Constant);
    }

    // Member access (`obj.prop`, `.length`); checked before `variable.other`.
    if scope_lower.starts_with("variable.other.property")
        || scope_lower.starts_with("variable.other.object.property")
        || scope_lower.starts_with("support.variable.property")
    {
        return Some(Category::Property);
    }

    // Host globals (`document`, `window`) and library constants (`Math.PI`).
    if scope_lower.starts_with("support.variable") {
        return Some(Category::VariableBuiltin);
    }
    if scope_lower.starts_with("support.constant") {
        return Some(Category::Constant);
    }

    // Variables
    if scope_lower.starts_with("variable.language") {
        return Some(Category::VariableBuiltin);
    }
    if scope_lower.starts_with("variable.parameter") || scope_lower.starts_with("variable.other") {
        return Some(Category::Variable);
    }

    // Properties / object keys
    if scope_lower.starts_with("entity.name.tag")
        || scope_lower.starts_with("support.other.property")
        || scope_lower.starts_with("meta.object-literal.key")
    {
        return Some(Category::Property);
    }

    // Attributes (decorators, annotations)
    if scope_lower.starts_with("entity.other.attribute")
        || scope_lower.starts_with("meta.attribute")
        || scope_lower.starts_with("entity.name.decorator")
    {
        return Some(Category::Attribute);
    }

    // Generic variable fallback
    if scope_lower.starts_with("variable") {
        return Some(Category::Variable);
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn index_is_the_position_in_all() {
        for (i, c) in Category::ALL.iter().enumerate() {
            assert_eq!(usize::from(c.index()), i);
        }
    }

    #[test]
    fn test_comment_delimiter_uses_comment_color() {
        // Comment delimiters (#, //, /*) should use comment color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.comment"),
            Some(Category::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.python"),
            Some(Category::Comment)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.comment.begin"),
            Some(Category::Comment)
        );
    }

    #[test]
    fn test_word_operators_use_keyword_color() {
        // Operators spelled as words are scoped `keyword.operator.word` by
        // Sublime grammars. They must read as keywords, not operators: the
        // operator colour equals plain text in most themes, so C++
        // `static_assert` looked unhighlighted (issue #3644).
        assert_eq!(
            scope_to_category("keyword.operator.word.c++"),
            Some(Category::Keyword)
        );
        assert_eq!(
            scope_to_category("keyword.operator.word.instanceof.java"),
            Some(Category::Keyword)
        );
        // Symbolic operators keep the operator colour.
        assert_eq!(
            scope_to_category("keyword.operator.arithmetic.c"),
            Some(Category::Operator)
        );
        assert_eq!(
            scope_to_category("keyword.operator.assignment.python"),
            Some(Category::Operator)
        );
    }

    #[test]
    fn test_variable_builtin_category() {
        assert_eq!(
            scope_to_category("variable.language.this"),
            Some(Category::VariableBuiltin)
        );
        assert_eq!(
            scope_to_category("variable.language.super"),
            Some(Category::VariableBuiltin)
        );
    }

    #[test]
    fn test_string_delimiter_uses_string_color() {
        // String delimiters (", ', `) should use string color, not operator
        assert_eq!(
            scope_to_category("punctuation.definition.string.begin"),
            Some(Category::String)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.string.end"),
            Some(Category::String)
        );
    }

    #[test]
    fn test_punctuation_bracket() {
        // punctuation.section (TextMate standard for block delimiters)
        assert_eq!(
            scope_to_category("punctuation.section"),
            Some(Category::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.section.block.begin.c"),
            Some(Category::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.bracket"),
            Some(Category::PunctuationBracket)
        );
        // punctuation.definition.* bracket-like scopes from sublime-syntax grammars
        assert_eq!(
            scope_to_category("punctuation.definition.array.begin.toml"),
            Some(Category::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.block.code.typst"),
            Some(Category::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.group.typst"),
            Some(Category::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.inline-table.begin.toml"),
            Some(Category::PunctuationBracket)
        );
        assert_eq!(
            scope_to_category("punctuation.definition.tag.end.svelte"),
            Some(Category::PunctuationBracket)
        );
    }

    #[test]
    fn test_punctuation_delimiter() {
        assert_eq!(
            scope_to_category("punctuation.separator"),
            Some(Category::PunctuationDelimiter)
        );
        assert_eq!(
            scope_to_category("punctuation.terminator.statement.c"),
            Some(Category::PunctuationDelimiter)
        );
        assert_eq!(
            scope_to_category("punctuation.accessor"),
            Some(Category::PunctuationDelimiter)
        );
    }

    #[test]
    fn test_storage_type_keyword() {
        assert_eq!(scope_to_category("storage.type"), Some(Category::Keyword));
        assert_eq!(
            scope_to_category("storage.type.class"),
            Some(Category::Keyword)
        );
        assert_ne!(scope_to_category("storage.type"), Some(Category::Type));
    }
}
