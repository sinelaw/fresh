# fresh.d.ts Review: Wrong and Missing Methods/Types

## How `fresh.d.ts` is generated

The file is auto-generated through a multi-step pipeline:

1. **`#[plugin_api_impl]` proc macro** (`crates/fresh-plugin-api-macros/src/lib.rs`) parses the
   `impl JsEditorApi` block in `crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs` and
   generates compile-time constants containing TypeScript method signatures.

2. **ts-rs** (`#[derive(TS)]` on structs in `crates/fresh-core/src/api.rs`) generates TypeScript
   type declarations for each API struct.

3. **`ts_export.rs`** (`crates/fresh-plugin-runtime/src/ts_export.rs`) combines the proc macro
   output with ts-rs types, validates syntax via oxc, formats, and writes the file.

4. **Triggered by**: `cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored --nocapture`

## Issues Found

### 1. Undefined types used in method signatures

These types appear in the `EditorAPI` interface but are **never defined** anywhere in the d.ts:

| Undefined type | Used by method | Root cause |
|---|---|---|
| `CreateTerminalOptions` | `createTerminal(opts?: CreateTerminalOptions)` | Struct exists in `api.rs:2021` with `#[ts(export)]` but missing from `get_type_decl()` and `DEPENDENCY_TYPES` in `ts_export.rs` |
| `TerminalResult` | `createTerminal(): Promise<TerminalResult>` | Struct exists in `api.rs:142` with `#[ts(export)]` but missing from `get_type_decl()` in `ts_export.rs` |
| `CompositeHunk` | `updateCompositeAlignment(..., hunks: CompositeHunk[])` | Rust struct is `CompositeHunk` but ts-rs renames it to `TsCompositeHunk` via `#[ts(rename)]`. The proc macro emits the Rust name unchanged. |
| `CreateCompositeBufferOptions` | `createCompositeBuffer(opts: CreateCompositeBufferOptions)` | Same — ts-rs renames to `TsCreateCompositeBufferOptions` |
| `Suggestion` | `setPromptSuggestions(suggestions: Suggestion[])` | Rust struct is `Suggestion` but ts-rs renames to `PromptSuggestion` via `#[ts(rename = "PromptSuggestion")]` |

The last three are a **systematic bug**: the proc macro uses the Rust struct name in method
signatures, but ts-rs generates the type declaration under a different name via `#[ts(rename)]`.

**Fix options:**
- Add these Rust type names to the known types list in `rust_to_typescript()` in the proc macro, mapping them to the ts-rs renamed names.
- OR add `#[plugin_api(ts_type = "TsCompositeHunk")]` overrides to the affected parameters.
- For `CreateTerminalOptions` and `TerminalResult`: add them to `get_type_decl()` and `DEPENDENCY_TYPES` in `ts_export.rs`, and import them.

### 2. Weak/untyped return values

These methods return `unknown` when the Rust implementation returns well-typed data:

| Method | Declared return | Actual data type | Fix |
|---|---|---|---|
| `getPrimaryCursor()` | `unknown` | `CursorInfo \| null` | Add `#[plugin_api(ts_return = "CursorInfo \| null")]` |
| `getAllCursors()` | `unknown` | `CursorInfo[]` | Add `#[plugin_api(ts_return = "CursorInfo[]")]` |
| `getAllCursorPositions()` | `unknown` | `number[]` | Add `#[plugin_api(ts_return = "number[]")]` |

`CursorInfo` exists in `api.rs:274` with `#[derive(TS)]` and is already mapped in
`get_type_decl()`, but since no method currently references it by name in its TypeScript
return type, it's never pulled into the d.ts output.

### 3. Duplicate type declarations

These types appear **twice** in the output file:

- `TsCompositeHunk` (lines 95–112 and 390–407)
- `TsCreateCompositeBufferOptions` (lines 113–134 and 408–429)
- `PromptSuggestion` (lines 200–221 and 619–640)

**Root cause**: The dedup check in `collect_ts_types()` uses the input lookup key, but these
types get collected under two different aliases — e.g., `"TsCompositeHunk"` from
`DEPENDENCY_TYPES` and `"CompositeHunk"` from `REFERENCED_TYPES` both resolve to the same
`CompositeHunk::decl()` output.

**Fix**: Track dedup by the generated declaration content or the canonical type name, not just
the lookup key.

### 4. Note on `getAllCursors`

The method **does exist** in Rust (`quickjs_backend.rs:805`). It reads `Vec<CursorInfo>` from
the state snapshot and serializes it. The problem is the `unknown` return type in the d.ts
makes it appear broken to TypeScript plugin authors. Adding
`#[plugin_api(ts_return = "CursorInfo[]")]` would fix the typing.
