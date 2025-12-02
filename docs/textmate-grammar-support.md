# TextMate Grammar Support for Syntax Highlighting

## Overview

This document describes the design for adding TextMate grammar support to Fresh editor, enabling syntax highlighting for languages without built-in tree-sitter support. The design maintains compatibility with the existing tree-sitter approach and allows users to add grammars without rebuilding the application.

## Goals

1. **Extend Language Support**: Support syntax highlighting for languages not covered by built-in tree-sitter grammars
2. **VSCode Compatibility**: Use the same grammar format as VSCode for broad ecosystem compatibility
3. **Plugin-like Experience**: Users can add grammars by dropping files into a config directory
4. **Performance**: Maintain viewport-only parsing for large file support
5. **Graceful Degradation**: Tree-sitter > TextMate > No highlighting

## Current Architecture

### Existing Components

- `src/primitives/highlighter.rs`: Tree-sitter based highlighting with viewport-only parsing
- `src/view/theme.rs`: Theme system with syntax color categories
- `src/config.rs`: Configuration with `LanguageConfig` for language settings
- Plugins: TypeScript/Deno runtime supporting overlay-based highlighting

### Key Design Principles

1. **Viewport-only parsing**: Only highlights visible lines (~50) plus 1KB context
2. **Category-based caching**: Stores `HighlightCategory` (not colors) for theme-independent caching
3. **Theme resolution**: Colors resolved from theme on-demand, enabling instant theme switching

### HighlightCategory Enum

```rust
pub enum HighlightCategory {
    Attribute, Comment, Constant, Function, Keyword,
    Number, Operator, Property, String, Type, Variable,
}
```

## Proposed Solution

### Technology Choice: `syntect` Crate

The `syntect` crate is the standard Rust library for TextMate grammar parsing:

- Supports `.tmLanguage.json` (VSCode JSON), `.tmLanguage` (plist), `.sublime-syntax`
- Mature, well-maintained, used by `bat`, `delta`, and many other tools
- Line-by-line incremental highlighting (compatible with viewport-only approach)
- Built-in syntax definitions for 100+ languages
- MIT licensed

**Cargo.toml addition:**
```toml
syntect = "5.2"
```

### Grammar Package Format (VSCode-Compatible)

Users install grammars in the config directory using VSCode extension structure:

```
~/.config/fresh/grammars/
  my-language/
    package.json
    syntaxes/
      language.tmLanguage.json
```

**package.json format (VSCode extension manifest):**
```json
{
  "name": "my-language",
  "displayName": "My Language",
  "version": "1.0.0",
  "contributes": {
    "languages": [{
      "id": "mylang",
      "extensions": [".ml", ".myl"],
      "aliases": ["MyLang", "mylang"]
    }],
    "grammars": [{
      "language": "mylang",
      "scopeName": "source.mylang",
      "path": "./syntaxes/language.tmLanguage.json"
    }]
  }
}
```

**Grammar file format (.tmLanguage.json):**
```json
{
  "name": "MyLang",
  "scopeName": "source.mylang",
  "fileTypes": ["ml", "myl"],
  "patterns": [
    { "include": "#comments" },
    { "include": "#strings" },
    { "include": "#keywords" }
  ],
  "repository": {
    "comments": {
      "name": "comment.line.mylang",
      "match": "#.*$"
    },
    "strings": {
      "name": "string.quoted.double.mylang",
      "begin": "\"",
      "end": "\"",
      "patterns": [
        { "name": "constant.character.escape.mylang", "match": "\\\\." }
      ]
    },
    "keywords": {
      "name": "keyword.control.mylang",
      "match": "\\b(if|else|while|for|return)\\b"
    }
  }
}
```

## Architecture

### New Module Structure

```
src/primitives/
  mod.rs                      # Export new modules
  highlighter.rs              # Existing tree-sitter (keep as-is)
  textmate_highlighter.rs     # NEW: TextMate grammar highlighter
  highlight_engine.rs         # NEW: Unified highlighter abstraction
  grammar_registry.rs         # NEW: Grammar discovery and loading
```

### Unified Highlighter Abstraction

```rust
// src/primitives/highlight_engine.rs

/// Unified highlighting engine supporting multiple backends
pub enum HighlightEngine {
    TreeSitter(Highlighter),        // Existing tree-sitter highlighter
    TextMate(TextMateHighlighter),  // New TextMate highlighter
    None,                           // No highlighting available
}

impl HighlightEngine {
    /// Create highlighter for a file, choosing the best available backend
    pub fn for_file(path: &Path, registry: &GrammarRegistry) -> Self {
        // Priority order:
        // 1. Tree-sitter (if built-in support exists) - faster, more accurate
        // 2. User TextMate grammar (from ~/.config/fresh/grammars/)
        // 3. Built-in syntect grammars
        // 4. No highlighting

        if let Some(lang) = Language::from_path(path) {
            if let Ok(highlighter) = Highlighter::new(lang) {
                return Self::TreeSitter(highlighter);
            }
        }

        if let Some(syntax) = registry.find_syntax_for_file(path) {
            return Self::TextMate(TextMateHighlighter::new(syntax, registry.syntax_set()));
        }

        Self::None
    }

    /// Highlight the visible viewport
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
    ) -> Vec<HighlightSpan> {
        match self {
            Self::TreeSitter(h) => h.highlight_viewport(buffer, viewport_start, viewport_end, theme),
            Self::TextMate(h) => h.highlight_viewport(buffer, viewport_start, viewport_end, theme),
            Self::None => Vec::new(),
        }
    }

    /// Invalidate cache for an edited range
    pub fn invalidate_range(&mut self, edit_range: Range<usize>) {
        match self {
            Self::TreeSitter(h) => h.invalidate_range(edit_range),
            Self::TextMate(h) => h.invalidate_range(edit_range),
            Self::None => {}
        }
    }
}
```

### TextMate Highlighter

```rust
// src/primitives/textmate_highlighter.rs

use syntect::parsing::{ParseState, ScopeStack, SyntaxReference, SyntaxSet};
use std::sync::Arc;

pub struct TextMateHighlighter {
    syntax: &'static SyntaxReference,
    syntax_set: Arc<SyntaxSet>,
    cache: Option<TextMateCache>,
    last_buffer_len: usize,
}

struct TextMateCache {
    /// Byte range this cache covers
    range: Range<usize>,
    /// Parse state at the start of cached region (for continuing parse)
    initial_state: ParseState,
    /// Highlighted spans (category-based for theme independence)
    spans: Vec<CachedSpan>,
}

impl TextMateHighlighter {
    pub fn highlight_viewport(
        &mut self,
        buffer: &Buffer,
        viewport_start: usize,
        viewport_end: usize,
        theme: &Theme,
    ) -> Vec<HighlightSpan> {
        // Check cache validity
        if let Some(cache) = &self.cache {
            if cache.range.start <= viewport_start
                && cache.range.end >= viewport_end
                && self.last_buffer_len == buffer.len()
            {
                // Cache hit - filter and resolve colors
                return cache.spans.iter()
                    .filter(|s| s.range.start < viewport_end && s.range.end > viewport_start)
                    .map(|s| HighlightSpan {
                        range: s.range.clone(),
                        color: s.category.color(theme),
                    })
                    .collect();
            }
        }

        // Cache miss - parse viewport region
        let parse_start = viewport_start.saturating_sub(1000);
        let parse_end = (viewport_end + 1000).min(buffer.len());

        let mut spans = Vec::new();
        let mut state = ParseState::new(self.syntax);

        // Parse line by line
        for line in buffer.lines_in_range(parse_start, parse_end) {
            let ops = state.parse_line(&line.content, &self.syntax_set);

            // Convert syntect operations to spans
            for (offset, op) in ops {
                if let Some(category) = scope_stack_to_category(&state.scope_stack) {
                    spans.push(CachedSpan {
                        range: (line.start + offset)..(line.start + offset + 1),
                        category,
                    });
                }
            }
        }

        // Update cache
        self.cache = Some(TextMateCache {
            range: parse_start..parse_end,
            initial_state: ParseState::new(self.syntax),
            spans: spans.clone(),
        });
        self.last_buffer_len = buffer.len();

        // Filter to viewport and resolve colors
        spans.into_iter()
            .filter(|s| s.range.start < viewport_end && s.range.end > viewport_start)
            .map(|s| HighlightSpan {
                range: s.range,
                color: s.category.color(theme),
            })
            .collect()
    }
}
```

### Scope to Category Mapping

TextMate grammars use hierarchical scope names. We map these to our `HighlightCategory`:

```rust
/// Map TextMate scope stack to HighlightCategory
fn scope_to_category(scope: &str) -> Option<HighlightCategory> {
    // Match most specific scope first
    let scope_lower = scope.to_lowercase();

    // Keywords
    if scope_lower.starts_with("keyword") {
        return Some(HighlightCategory::Keyword);
    }

    // Strings
    if scope_lower.starts_with("string") {
        return Some(HighlightCategory::String);
    }

    // Comments
    if scope_lower.starts_with("comment") {
        return Some(HighlightCategory::Comment);
    }

    // Functions
    if scope_lower.starts_with("entity.name.function")
        || scope_lower.starts_with("support.function")
        || scope_lower.starts_with("meta.function-call")
    {
        return Some(HighlightCategory::Function);
    }

    // Types
    if scope_lower.starts_with("entity.name.type")
        || scope_lower.starts_with("support.type")
        || scope_lower.starts_with("storage.type")
        || scope_lower.starts_with("entity.name.class")
    {
        return Some(HighlightCategory::Type);
    }

    // Variables
    if scope_lower.starts_with("variable") {
        return Some(HighlightCategory::Variable);
    }

    // Constants and numbers
    if scope_lower.starts_with("constant.numeric") {
        return Some(HighlightCategory::Number);
    }
    if scope_lower.starts_with("constant") {
        return Some(HighlightCategory::Constant);
    }

    // Operators
    if scope_lower.starts_with("keyword.operator")
        || scope_lower.starts_with("punctuation")
    {
        return Some(HighlightCategory::Operator);
    }

    // Properties
    if scope_lower.starts_with("entity.name.tag")
        || scope_lower.starts_with("support.other.property")
        || scope_lower.starts_with("meta.object-literal.key")
    {
        return Some(HighlightCategory::Property);
    }

    // Attributes
    if scope_lower.starts_with("entity.other.attribute") {
        return Some(HighlightCategory::Attribute);
    }

    None
}
```

### Grammar Registry

```rust
// src/primitives/grammar_registry.rs

use syntect::parsing::{SyntaxSet, SyntaxSetBuilder, SyntaxReference};
use std::path::PathBuf;
use std::collections::HashMap;

/// Registry of all available TextMate grammars
pub struct GrammarRegistry {
    /// Combined syntax set (built-in + user grammars)
    syntax_set: SyntaxSet,
    /// Extension -> scope name mapping for user grammars
    user_extensions: HashMap<String, String>,
}

impl GrammarRegistry {
    /// Load grammar registry, scanning user grammars directory
    pub fn load() -> Self {
        let mut builder = SyntaxSetBuilder::new();

        // Add built-in syntect grammars
        builder.add_plain_text_syntax();
        builder.add_from_folder("", true).ok(); // Built-in defaults

        let mut user_extensions = HashMap::new();

        // Scan user grammars directory
        if let Some(grammars_dir) = Self::grammars_directory() {
            if grammars_dir.exists() {
                Self::load_user_grammars(&grammars_dir, &mut builder, &mut user_extensions);
            }
        }

        Self {
            syntax_set: builder.build(),
            user_extensions,
        }
    }

    /// Get the grammars directory path
    fn grammars_directory() -> Option<PathBuf> {
        dirs::config_dir().map(|p| p.join("fresh/grammars"))
    }

    /// Load user grammars from the grammars directory
    fn load_user_grammars(
        dir: &Path,
        builder: &mut SyntaxSetBuilder,
        user_extensions: &mut HashMap<String, String>,
    ) {
        // Iterate through subdirectories
        for entry in std::fs::read_dir(dir).into_iter().flatten() {
            let entry = match entry {
                Ok(e) => e,
                Err(_) => continue,
            };

            let package_json = entry.path().join("package.json");
            if !package_json.exists() {
                continue;
            }

            // Parse package.json
            if let Ok(manifest) = Self::parse_package_json(&package_json) {
                for grammar in manifest.grammars {
                    let grammar_path = entry.path().join(&grammar.path);

                    // Load grammar file
                    if let Ok(syntax) = Self::load_grammar_file(&grammar_path) {
                        // Add extension mappings
                        for lang in &manifest.languages {
                            if lang.id == grammar.language {
                                for ext in &lang.extensions {
                                    let ext = ext.trim_start_matches('.');
                                    user_extensions.insert(
                                        ext.to_string(),
                                        grammar.scope_name.clone(),
                                    );
                                }
                            }
                        }

                        builder.add(syntax);
                    }
                }
            }
        }
    }

    /// Find syntax for a file by extension
    pub fn find_syntax_for_file(&self, path: &Path) -> Option<&SyntaxReference> {
        let ext = path.extension()?.to_str()?;

        // Check user grammars first
        if let Some(scope) = self.user_extensions.get(ext) {
            return self.syntax_set.find_syntax_by_scope(scope);
        }

        // Fall back to built-in syntect grammars
        self.syntax_set.find_syntax_for_file(path).ok().flatten()
    }

    /// Get the syntax set for highlighting
    pub fn syntax_set(&self) -> &SyntaxSet {
        &self.syntax_set
    }
}

// VSCode package.json structures
#[derive(Deserialize)]
struct PackageManifest {
    #[serde(default)]
    contributes: Contributes,
}

#[derive(Deserialize, Default)]
struct Contributes {
    #[serde(default)]
    languages: Vec<LanguageContribution>,
    #[serde(default)]
    grammars: Vec<GrammarContribution>,
}

#[derive(Deserialize)]
struct LanguageContribution {
    id: String,
    #[serde(default)]
    extensions: Vec<String>,
}

#[derive(Deserialize)]
struct GrammarContribution {
    language: String,
    #[serde(rename = "scopeName")]
    scope_name: String,
    path: String,
}
```

## Configuration

### Extended LanguageConfig

```rust
// In src/config.rs

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageConfig {
    pub extensions: Vec<String>,
    pub grammar: String,
    pub comment_prefix: Option<String>,

    #[serde(default = "default_true")]
    pub auto_indent: bool,

    /// Highlighter preference for this language
    #[serde(default)]
    pub highlighter: HighlighterPreference,

    /// Path to custom TextMate grammar file (optional)
    #[serde(default)]
    pub textmate_grammar: Option<PathBuf>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum HighlighterPreference {
    /// Use tree-sitter if available, fall back to TextMate
    #[default]
    Auto,
    /// Force tree-sitter (no highlighting if unavailable)
    TreeSitter,
    /// Force TextMate grammar
    TextMate,
}
```

### Example Configuration

```json
{
  "languages": {
    "haskell": {
      "extensions": ["hs", "lhs"],
      "grammar": "haskell",
      "comment_prefix": "--",
      "highlighter": "textmate"
    },
    "custom-dsl": {
      "extensions": ["dsl"],
      "grammar": "custom-dsl",
      "comment_prefix": "//",
      "textmate_grammar": "~/.config/fresh/grammars/my-dsl/syntax.tmLanguage.json"
    }
  }
}
```

## Integration Points

### Render System

Update `src/app/render.rs` to use `HighlightEngine`:

```rust
// In BufferView or similar

impl BufferView {
    fn get_highlighter(&mut self, path: &Path, registry: &GrammarRegistry) -> &mut HighlightEngine {
        if self.highlighter.is_none() {
            self.highlighter = Some(HighlightEngine::for_file(path, registry));
        }
        self.highlighter.as_mut().unwrap()
    }
}
```

### Application Initialization

```rust
// In src/app/mod.rs or main initialization

impl App {
    pub fn new() -> Self {
        // Load grammar registry at startup
        let grammar_registry = GrammarRegistry::load();

        Self {
            grammar_registry,
            // ...
        }
    }
}
```

## Performance Considerations

### Viewport-Only Parsing

Both tree-sitter and TextMate highlighters use viewport-only parsing:
- Parse only visible lines + 1KB context
- Cache parsed results
- Invalidate cache on edits

### TextMate Performance Notes

- `syntect` uses lazy regex compilation
- Line-by-line parsing enables incremental updates
- State caching at line boundaries for fast re-parse after edits

### Memory Usage

- Grammar files are loaded once at startup
- Syntax definitions shared via `Arc<SyntaxSet>`
- Per-buffer highlight cache (~few KB for typical viewport)

## Implementation Phases

### Phase 1: Core TextMate Support
1. Add `syntect` dependency
2. Implement `TextMateHighlighter` with viewport-only parsing
3. Implement `GrammarRegistry` for built-in syntect grammars
4. Implement `HighlightEngine` abstraction
5. Update render system integration

### Phase 2: User Grammar Loading
1. Implement VSCode package.json parsing
2. Load user grammars from `~/.config/fresh/grammars/`
3. Extension-to-grammar mapping

### Phase 3: Configuration Integration
1. Extend `LanguageConfig` with highlighter preferences
2. Per-language grammar override support
3. Documentation and examples

### Phase 4: Advanced Features (Future)
1. Grammar hot-reload via file watching
2. VSCode theme tokenColors support (fine-grained scope colors)
3. Embedded language support (language injection)
4. Grammar debugging/scope inspector

## File Structure

### After Implementation

```
src/primitives/
  mod.rs                      # Export all primitives
  highlighter.rs              # Existing tree-sitter highlighter
  textmate_highlighter.rs     # NEW: TextMate grammar highlighter
  highlight_engine.rs         # NEW: Unified abstraction
  grammar_registry.rs         # NEW: Grammar discovery and loading
  semantic_highlight.rs       # Existing word occurrence highlighting

~/.config/fresh/
  config.json                 # User configuration
  grammars/                   # NEW: User grammars directory
    example-language/
      package.json            # VSCode extension manifest
      syntaxes/
        example.tmLanguage.json
```

## Benefits

1. **Backward Compatible**: Existing tree-sitter highlighting unchanged
2. **VSCode Ecosystem**: Access to thousands of existing grammars
3. **No Rebuild Required**: Drop-in grammar installation
4. **Performance Maintained**: Viewport-only parsing, efficient caching
5. **Graceful Fallback**: Automatic backend selection
6. **Plugin Compatible**: Overlay system still works for custom highlighting

## References

- [TextMate Grammar Documentation](https://macromates.com/manual/en/language_grammars)
- [VSCode Syntax Highlight Guide](https://code.visualstudio.com/api/language-extensions/syntax-highlight-guide)
- [syntect Crate Documentation](https://docs.rs/syntect/)
- [VSCode Extension Manifest](https://code.visualstudio.com/api/references/extension-manifest)
