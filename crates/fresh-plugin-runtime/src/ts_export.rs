//! TypeScript type generation using ts-rs
//!
//! This module collects all API types with `#[derive(TS)]` and generates
//! TypeScript declarations that are combined with the proc macro output.
//! The generated TypeScript is validated and formatted using oxc.
//!
//! Types are automatically collected based on `JSEDITORAPI_REFERENCED_TYPES`
//! from the proc macro, so when you add a new type to method signatures,
//! it will automatically be included if it has `#[derive(TS)]`.

use oxc_allocator::Allocator;
use oxc_codegen::Codegen;
use oxc_parser::Parser;
use oxc_span::SourceType;
use ts_rs::TS;

use fresh_core::api::{
    ActionPopupAction, ActionSpec, BackgroundProcessResult, BufferInfo, BufferSavedDiff,
    CompositeHunk, CompositeLayoutConfig, CompositePaneStyle, CompositeSourceConfig,
    CreateCompositeBufferOptions, CreateVirtualBufferInExistingSplitOptions,
    CreateVirtualBufferInSplitOptions, CreateVirtualBufferOptions, CursorInfo, DirEntry,
    JsTextPropertyEntry, LayoutHints, SpawnResult, TextPropertiesAtCursor, TsHighlightSpan,
    ViewTokenStyle, ViewTokenWire, ViewTokenWireKind, ViewportInfo, VirtualBufferResult,
};
use fresh_core::command::Suggestion;

/// Get the TypeScript declaration for a type by name
///
/// Returns None if the type is not known (not registered in this mapping).
/// Add new types here when they're added to api.rs with `#[derive(TS)]`.
fn get_type_decl(type_name: &str) -> Option<String> {
    // Map TypeScript type names to their ts-rs declarations
    // The type name should match either the Rust struct name or the ts(rename = "...") value
    match type_name {
        // Core types
        "BufferInfo" => Some(BufferInfo::decl()),
        "CursorInfo" => Some(CursorInfo::decl()),
        "ViewportInfo" => Some(ViewportInfo::decl()),
        "ActionSpec" => Some(ActionSpec::decl()),
        "BufferSavedDiff" => Some(BufferSavedDiff::decl()),
        "LayoutHints" => Some(LayoutHints::decl()),

        // Process types
        "SpawnResult" => Some(SpawnResult::decl()),
        "BackgroundProcessResult" => Some(BackgroundProcessResult::decl()),

        // Composite buffer types (ts-rs renames these with Ts prefix)
        "TsCompositeLayoutConfig" | "CompositeLayoutConfig" => Some(CompositeLayoutConfig::decl()),
        "TsCompositeSourceConfig" | "CompositeSourceConfig" => Some(CompositeSourceConfig::decl()),
        "TsCompositePaneStyle" | "CompositePaneStyle" => Some(CompositePaneStyle::decl()),
        "TsCompositeHunk" | "CompositeHunk" => Some(CompositeHunk::decl()),
        "TsCreateCompositeBufferOptions" | "CreateCompositeBufferOptions" => {
            Some(CreateCompositeBufferOptions::decl())
        }

        // View transform types
        "ViewTokenWireKind" => Some(ViewTokenWireKind::decl()),
        "ViewTokenStyle" => Some(ViewTokenStyle::decl()),
        "ViewTokenWire" => Some(ViewTokenWire::decl()),

        // UI types (ts-rs renames these with Ts prefix)
        "TsActionPopupAction" | "ActionPopupAction" => Some(ActionPopupAction::decl()),
        "TsHighlightSpan" => Some(TsHighlightSpan::decl()),

        // Virtual buffer option types
        "TextPropertyEntry" | "JsTextPropertyEntry" => Some(JsTextPropertyEntry::decl()),
        "CreateVirtualBufferOptions" => Some(CreateVirtualBufferOptions::decl()),
        "CreateVirtualBufferInSplitOptions" => Some(CreateVirtualBufferInSplitOptions::decl()),
        "CreateVirtualBufferInExistingSplitOptions" => {
            Some(CreateVirtualBufferInExistingSplitOptions::decl())
        }

        // Return types
        "TextPropertiesAtCursor" => Some(TextPropertiesAtCursor::decl()),
        "VirtualBufferResult" => Some(VirtualBufferResult::decl()),

        // Prompt and directory types
        "PromptSuggestion" | "Suggestion" => Some(Suggestion::decl()),
        "DirEntry" => Some(DirEntry::decl()),

        _ => None,
    }
}

/// Types that are dependencies of other types and must always be included.
/// These are types referenced inside option structs or other complex types
/// that aren't directly in method signatures.
const DEPENDENCY_TYPES: &[&str] = &[
    "TextPropertyEntry",              // Used in CreateVirtualBuffer*Options.entries
    "TsCompositeLayoutConfig",        // Used in createCompositeBuffer opts
    "TsCompositeSourceConfig",        // Used in createCompositeBuffer opts.sources
    "TsCompositePaneStyle",           // Used in TsCompositeSourceConfig.style
    "TsCompositeHunk",                // Used in createCompositeBuffer opts.hunks
    "TsCreateCompositeBufferOptions", // Options for createCompositeBuffer
    "ViewportInfo",                   // Used by plugins for viewport queries
    "LayoutHints",                    // Used by plugins for view transforms
    "ViewTokenWire",                  // Used by plugins for view transforms
    "ViewTokenWireKind",              // Used by ViewTokenWire
    "ViewTokenStyle",                 // Used by ViewTokenWire
    "PromptSuggestion",               // Used by plugins for prompt suggestions
    "DirEntry",                       // Used by plugins for directory entries
];

/// Collect TypeScript type declarations based on referenced types from proc macro
///
/// Uses `JSEDITORAPI_REFERENCED_TYPES` to determine which types to include.
/// Also includes dependency types that are referenced by other types.
pub fn collect_ts_types() -> String {
    use crate::backend::quickjs_backend::JSEDITORAPI_REFERENCED_TYPES;

    let mut types = Vec::new();
    let mut included = std::collections::HashSet::new();

    // First, include dependency types (order matters - dependencies first)
    for type_name in DEPENDENCY_TYPES {
        if let Some(decl) = get_type_decl(type_name) {
            types.push(decl);
            included.insert(*type_name);
        }
    }

    // Collect types referenced by the API
    for type_name in JSEDITORAPI_REFERENCED_TYPES {
        if included.contains(*type_name) {
            continue;
        }
        if let Some(decl) = get_type_decl(type_name) {
            types.push(decl);
            included.insert(*type_name);
        } else {
            // Log warning for unknown types (these need to be added to get_type_decl)
            eprintln!(
                "Warning: Type '{}' is referenced in API but not registered in get_type_decl()",
                type_name
            );
        }
    }

    types.join("\n\n")
}

/// Validate TypeScript syntax using oxc parser
///
/// Returns Ok(()) if the syntax is valid, or an error with the parse errors.
pub fn validate_typescript(source: &str) -> Result<(), String> {
    let allocator = Allocator::default();
    let source_type = SourceType::d_ts();

    let parser_ret = Parser::new(&allocator, source, source_type).parse();

    if parser_ret.errors.is_empty() {
        Ok(())
    } else {
        let errors: Vec<String> = parser_ret
            .errors
            .iter()
            .map(|e: &oxc_diagnostics::OxcDiagnostic| e.to_string())
            .collect();
        Err(format!("TypeScript parse errors:\n{}", errors.join("\n")))
    }
}

/// Format TypeScript source code using oxc codegen
///
/// Parses the TypeScript and regenerates it with consistent formatting.
/// Returns the original source if parsing fails.
pub fn format_typescript(source: &str) -> String {
    let allocator = Allocator::default();
    let source_type = SourceType::d_ts();

    let parser_ret = Parser::new(&allocator, source, source_type).parse();

    if !parser_ret.errors.is_empty() {
        // Return original source if parsing fails
        return source.to_string();
    }

    // Generate formatted code from AST
    Codegen::new().build(&parser_ret.program).code
}

/// Generate and write the complete fresh.d.ts file
///
/// Combines ts-rs generated types with proc macro output,
/// validates the syntax, formats the output, and writes to disk.
pub fn write_fresh_dts() -> Result<(), String> {
    use crate::backend::quickjs_backend::{JSEDITORAPI_TS_EDITOR_API, JSEDITORAPI_TS_PREAMBLE};

    let ts_types = collect_ts_types();

    let content = format!(
        "{}\n{}\n{}",
        JSEDITORAPI_TS_PREAMBLE, ts_types, JSEDITORAPI_TS_EDITOR_API
    );

    // Validate the generated TypeScript syntax
    validate_typescript(&content)?;

    // Format the TypeScript
    let formatted = format_typescript(&content);

    // Determine output path - write to fresh-editor/plugins/lib/fresh.d.ts
    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
    let output_path = std::path::Path::new(&manifest_dir)
        .parent() // crates/
        .and_then(|p| p.parent()) // workspace root
        .map(|p| p.join("crates/fresh-editor/plugins/lib/fresh.d.ts"))
        .unwrap_or_else(|| std::path::PathBuf::from("plugins/lib/fresh.d.ts"));

    // Only write if content changed
    let should_write = match std::fs::read_to_string(&output_path) {
        Ok(existing) => existing != formatted,
        Err(_) => true,
    };

    if should_write {
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| e.to_string())?;
        }
        std::fs::write(&output_path, &formatted).map_err(|e| e.to_string())?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generate, validate, format, and write fresh.d.ts
    /// Run with: cargo test --features plugins write_fresh_dts_file -- --ignored --nocapture
    #[test]
    #[ignore]
    fn write_fresh_dts_file() {
        // write_fresh_dts validates syntax and formats before writing
        write_fresh_dts().expect("Failed to write fresh.d.ts");
        println!("Successfully generated, validated, and formatted fresh.d.ts");
    }
}
