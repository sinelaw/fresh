# Highlight Engine Cleanup Plan

## Background

`highlight_engine.rs` has accumulated complexity from organic growth. It exposes
a dual-backend system (tree-sitter + TextMate/syntect), but in practice TextMate
is the only highlighting backend used — tree-sitter fires solely as a fallback
for TypeScript/TSX (where syntect lacks a grammar). The tree-sitter `Language`
type is still detected and used for non-highlighting features (indentation,
bracket matching, semantic highlighting), but that is orthogonal to this module.

### Current problems

- **Constructor proliferation**: 6 public constructors + 2 private helpers form
  a combinatorial matrix of (path vs name vs language) × (with/without languages
  config) × (with/without preference).
- **Dead preference system**: `HighlighterPreference` has 3 variants (`Auto`,
  `TextMate`, `TreeSitter`) but `Auto` and `TextMate` are identical. `TreeSitter`
  is only used in one test. Every built-in language config (70+) hardcodes `Auto`.
- **Dead constructor**: `for_language()` has zero callers.
- **Duplicated private helpers**: `textmate_for_file` and
  `textmate_for_file_with_languages` are near-identical (~40 lines each).
- **Repeated syntax index lookup**: The same linear scan appears 4 times.
- **Double detection of `ts_language`**: `DetectedLanguage::from_path` and the
  engine's internal helpers both call `Language::from_path` independently.

## Plan

### Step 1 — Delete `HighlighterPreference` and the `highlighter` config field

Remove the `HighlighterPreference` enum entirely. Remove the `highlighter` field
from `LanguageConfig`, `LanguageCliHint`, and all ~70 built-in language entries.
Remove preference parameters from all constructors. The one test that used
`TreeSitter` preference can test `Highlighter` directly.

### Step 2 — Delete `for_language()` (dead code)

Remove the `for_language` constructor — zero callers in the codebase.

### Step 3 — Collapse path-based constructors

Replace `for_file`, `for_file_with_languages`, `for_file_with_preference`, and
`for_file_with_languages_and_preference` with a single:

```rust
pub fn for_file(
    path: &Path,
    registry: &GrammarRegistry,
    languages: Option<&HashMap<String, LanguageConfig>>,
) -> Self
```

Update call sites in `detected_language.rs`.

### Step 4 — Merge duplicated `textmate_for_file` helpers

Combine `textmate_for_file` and `textmate_for_file_with_languages` into one
private method that takes `Option<&HashMap<...>>` and picks the right registry
lookup.

### Step 5 — Extract syntax index lookup helper

Replace the 4 repeated occurrences of:
```rust
syntax_set.syntaxes().iter().position(|s| s.name == syntax.name)
```
with a local `fn syntax_index(...)` helper.

### Step 6 — Eliminate double `ts_language` detection

Pass the already-detected `Option<Language>` into the engine constructor so
`Language::from_path` is called once, not twice.
