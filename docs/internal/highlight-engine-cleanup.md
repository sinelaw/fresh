# Highlight Engine Cleanup

## Status: complete

The cleanup originally planned in this document has been carried out and then
superseded by the unified grammar catalog refactor (see `GrammarRegistry` in
`crates/fresh-editor/src/primitives/grammar/types.rs`).

### What the module looks like now

`HighlightEngine` in `crates/fresh-editor/src/primitives/highlight_engine.rs`
exposes exactly three public constructors:

- `from_entry(&GrammarEntry, &GrammarRegistry) -> Self` — the canonical path.
  Picks syntect if the entry has a `syntect` index, else tree-sitter if it has
  a `Language`, else `HighlightEngine::None`. This is the single place the
  "prefer syntect, fall back to tree-sitter" fallback lives.
- `for_file(path, registry) -> Self` — thin wrapper: `registry.find_by_path` +
  `from_entry`.
- `for_syntax_name(name, registry) -> Self` — thin wrapper: `find_by_name` +
  `from_entry`.

`DetectedLanguage::from_entry` consumes the same catalog entries, so the
highlighter and the per-buffer language state stay in sync through one type.

### What's gone

- `HighlighterPreference` enum (the `Auto`/`TextMate`/`TreeSitter` dead
  preference system).
- `for_language()` constructor.
- `for_file_with_languages`, `for_file_with_preference`,
  `for_file_with_languages_and_preference` — collapsed into `for_file`.
- Duplicated `textmate_for_file` helpers.
- Double detection of `ts_language`: the catalog entry carries it, so
  `Language::from_path` runs at most once per resolution.

### How to add a language now

Extend the catalog, not the engine. See `GrammarRegistry::rebuild_catalog` and
`fresh_languages::Language::extensions` for the shape. Plugin authors use
`GrammarRegistry::with_additional_grammars`.
