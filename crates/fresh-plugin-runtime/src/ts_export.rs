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
use ts_rs::{Config as TsConfig, TS};

use fresh_core::api::{
    ActionPopupAction, ActionPopupOptions, ActionSpec, BackgroundProcessResult, BufferInfo,
    BufferSavedDiff, CompositeHunk, CompositeLayoutConfig, CompositePaneStyle,
    CompositeSourceConfig, CreateCompositeBufferOptions, CreateTerminalOptions,
    CreateVirtualBufferInExistingSplitOptions, CreateVirtualBufferInSplitOptions,
    CreateVirtualBufferOptions, CursorInfo, DirEntry, FormatterPackConfig, GrepMatch, JsDiagnostic,
    JsPosition, JsRange, JsTextPropertyEntry, LanguagePackConfig, LayoutHints, LspServerPackConfig,
    OverlayColorSpec, OverlayOptions, ProcessLimitsPackConfig, ReplaceResult, SpawnResult,
    TerminalResult, TextPropertiesAtCursor, TsHighlightSpan, ViewTokenStyle, ViewTokenWire,
    ViewTokenWireKind, ViewportInfo, VirtualBufferResult,
};
use fresh_core::command::Suggestion;
use fresh_core::file_explorer::FileExplorerDecoration;
use fresh_core::text_property::InlineOverlay;

/// Get the TypeScript declaration for a type by name
///
/// Returns None if the type is not known (not registered in this mapping).
/// Add new types here when they're added to api.rs with `#[derive(TS)]`.
fn get_type_decl(type_name: &str) -> Option<String> {
    let cfg = TsConfig::default();
    // Map TypeScript type names to their ts-rs declarations
    // The type name should match either the Rust struct name or the ts(rename = "...") value
    match type_name {
        // Core types
        "BufferInfo" => Some(BufferInfo::decl(&cfg)),
        "CursorInfo" => Some(CursorInfo::decl(&cfg)),
        "ViewportInfo" => Some(ViewportInfo::decl(&cfg)),
        "ActionSpec" => Some(ActionSpec::decl(&cfg)),
        "BufferSavedDiff" => Some(BufferSavedDiff::decl(&cfg)),
        "LayoutHints" => Some(LayoutHints::decl(&cfg)),

        // Process types
        "SpawnResult" => Some(SpawnResult::decl(&cfg)),
        "BackgroundProcessResult" => Some(BackgroundProcessResult::decl(&cfg)),

        // Grep/Replace types
        "GrepMatch" => Some(GrepMatch::decl(&cfg)),
        "ReplaceResult" => Some(ReplaceResult::decl(&cfg)),

        // Terminal types
        "TerminalResult" => Some(TerminalResult::decl(&cfg)),
        "CreateTerminalOptions" => Some(CreateTerminalOptions::decl(&cfg)),

        // Composite buffer types (ts-rs renames these with Ts prefix)
        "TsCompositeLayoutConfig" | "CompositeLayoutConfig" => {
            Some(CompositeLayoutConfig::decl(&cfg))
        }
        "TsCompositeSourceConfig" | "CompositeSourceConfig" => {
            Some(CompositeSourceConfig::decl(&cfg))
        }
        "TsCompositePaneStyle" | "CompositePaneStyle" => Some(CompositePaneStyle::decl(&cfg)),
        "TsCompositeHunk" | "CompositeHunk" => Some(CompositeHunk::decl(&cfg)),
        "TsCreateCompositeBufferOptions" | "CreateCompositeBufferOptions" => {
            Some(CreateCompositeBufferOptions::decl(&cfg))
        }

        // View transform types
        "ViewTokenWireKind" => Some(ViewTokenWireKind::decl(&cfg)),
        "ViewTokenStyle" => Some(ViewTokenStyle::decl(&cfg)),
        "ViewTokenWire" => Some(ViewTokenWire::decl(&cfg)),

        // UI types (ts-rs renames these with Ts prefix)
        "TsActionPopupAction" | "ActionPopupAction" => Some(ActionPopupAction::decl(&cfg)),
        "ActionPopupOptions" => Some(ActionPopupOptions::decl(&cfg)),
        "TsHighlightSpan" => Some(TsHighlightSpan::decl(&cfg)),
        "FileExplorerDecoration" => Some(FileExplorerDecoration::decl(&cfg)),

        // Virtual buffer option types
        "TextPropertyEntry" | "JsTextPropertyEntry" => Some(JsTextPropertyEntry::decl(&cfg)),
        "CreateVirtualBufferOptions" => Some(CreateVirtualBufferOptions::decl(&cfg)),
        "CreateVirtualBufferInSplitOptions" => Some(CreateVirtualBufferInSplitOptions::decl(&cfg)),
        "CreateVirtualBufferInExistingSplitOptions" => {
            Some(CreateVirtualBufferInExistingSplitOptions::decl(&cfg))
        }

        // Return types
        "TextPropertiesAtCursor" => Some(TextPropertiesAtCursor::decl(&cfg)),
        "VirtualBufferResult" => Some(VirtualBufferResult::decl(&cfg)),

        // Prompt and directory types
        "PromptSuggestion" | "Suggestion" => Some(Suggestion::decl(&cfg)),
        "DirEntry" => Some(DirEntry::decl(&cfg)),

        // Diagnostic types
        "JsDiagnostic" => Some(JsDiagnostic::decl(&cfg)),
        "JsRange" => Some(JsRange::decl(&cfg)),
        "JsPosition" => Some(JsPosition::decl(&cfg)),

        // Language pack types
        "LanguagePackConfig" => Some(LanguagePackConfig::decl(&cfg)),
        "LspServerPackConfig" => Some(LspServerPackConfig::decl(&cfg)),
        "ProcessLimitsPackConfig" => Some(ProcessLimitsPackConfig::decl(&cfg)),
        "FormatterPackConfig" => Some(FormatterPackConfig::decl(&cfg)),

        // Overlay/inline styling types
        "OverlayOptions" => Some(OverlayOptions::decl(&cfg)),
        "OverlayColorSpec" => Some(OverlayColorSpec::decl(&cfg)),
        "InlineOverlay" => Some(InlineOverlay::decl(&cfg)),

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
    "BufferInfo",                     // Used by listBuffers, getBufferInfo
    "JsDiagnostic",                   // Used by getAllDiagnostics
    "JsRange",                        // Used by JsDiagnostic
    "JsPosition",                     // Used by JsRange
    "ActionSpec",                     // Used by executeActions
    "TsActionPopupAction",            // Used by ActionPopupOptions.actions
    "ActionPopupOptions",             // Used by showActionPopup
    "FileExplorerDecoration",         // Used by setFileExplorerDecorations
    "FormatterPackConfig",            // Used by LanguagePackConfig.formatter
    "ProcessLimitsPackConfig",        // Used by LspServerPackConfig.process_limits
    "TerminalResult",                 // Used by createTerminal return type
    "CreateTerminalOptions",          // Used by createTerminal opts parameter
    "CursorInfo",                     // Used by getPrimaryCursor, getAllCursors
    "OverlayOptions",                 // Used by TextPropertyEntry.style and InlineOverlay
    "OverlayColorSpec",               // Used by OverlayOptions.fg/bg
    "InlineOverlay",                  // Used by TextPropertyEntry.inlineOverlays
];

/// Collect TypeScript type declarations based on referenced types from proc macro
///
/// Uses `JSEDITORAPI_REFERENCED_TYPES` to determine which types to include.
/// Also includes dependency types that are referenced by other types.
pub fn collect_ts_types() -> String {
    use crate::backend::quickjs_backend::JSEDITORAPI_REFERENCED_TYPES;

    let mut types = Vec::new();
    // Track by declaration content to prevent duplicates from aliases
    // (e.g., "CompositeHunk" and "TsCompositeHunk" both resolve to the same decl)
    let mut included_decls = std::collections::HashSet::new();

    // First, include dependency types (order matters - dependencies first)
    for type_name in DEPENDENCY_TYPES {
        if let Some(decl) = get_type_decl(type_name) {
            if included_decls.insert(decl.clone()) {
                types.push(decl);
            }
        }
    }

    // Collect types referenced by the API
    for type_name in JSEDITORAPI_REFERENCED_TYPES {
        if let Some(decl) = get_type_decl(type_name) {
            if included_decls.insert(decl.clone()) {
                types.push(decl);
            }
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
    /// Run with: cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored --nocapture
    #[test]
    #[ignore]
    fn write_fresh_dts_file() {
        // write_fresh_dts validates syntax and formats before writing
        write_fresh_dts().expect("Failed to write fresh.d.ts");
        println!("Successfully generated, validated, and formatted fresh.d.ts");
    }

    /// Type check all plugins using TypeScript compiler
    /// Skips if tsc is not available in PATH
    /// Run with: cargo test -p fresh-plugin-runtime type_check_plugins -- --ignored --nocapture
    #[test]
    #[ignore]
    fn type_check_plugins() {
        // Check if tsc is available
        let tsc_check = std::process::Command::new("tsc").arg("--version").output();

        match tsc_check {
            Ok(output) if output.status.success() => {
                println!(
                    "Found tsc: {}",
                    String::from_utf8_lossy(&output.stdout).trim()
                );
            }
            _ => {
                println!("tsc not found in PATH, skipping type check test");
                return;
            }
        }

        // Find the check-types.sh script
        let manifest_dir = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".to_string());
        let script_path = std::path::Path::new(&manifest_dir)
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("crates/fresh-editor/plugins/check-types.sh"))
            .expect("Failed to find check-types.sh");

        println!("Running type check script: {}", script_path.display());

        // Run the check-types.sh script
        let output = std::process::Command::new("bash")
            .arg(&script_path)
            .output()
            .expect("Failed to run check-types.sh");

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        println!("stdout:\n{}", stdout);
        if !stderr.is_empty() {
            println!("stderr:\n{}", stderr);
        }

        // The script outputs "X file(s) had type errors" if there are errors
        if stdout.contains("had type errors") || !output.status.success() {
            panic!(
                "TypeScript type check failed. Run 'crates/fresh-editor/plugins/check-types.sh' to see details."
            );
        }

        println!("All plugins type check successfully!");
    }

    // ========================================================================
    // Type declaration tests
    // ========================================================================

    #[test]
    fn test_get_type_decl_returns_all_expected_types() {
        let expected_types = vec![
            "BufferInfo",
            "CursorInfo",
            "ViewportInfo",
            "ActionSpec",
            "BufferSavedDiff",
            "LayoutHints",
            "SpawnResult",
            "BackgroundProcessResult",
            "TerminalResult",
            "CreateTerminalOptions",
            "TsCompositeLayoutConfig",
            "TsCompositeSourceConfig",
            "TsCompositePaneStyle",
            "TsCompositeHunk",
            "TsCreateCompositeBufferOptions",
            "ViewTokenWireKind",
            "ViewTokenStyle",
            "ViewTokenWire",
            "TsActionPopupAction",
            "ActionPopupOptions",
            "TsHighlightSpan",
            "FileExplorerDecoration",
            "TextPropertyEntry",
            "CreateVirtualBufferOptions",
            "CreateVirtualBufferInSplitOptions",
            "CreateVirtualBufferInExistingSplitOptions",
            "TextPropertiesAtCursor",
            "VirtualBufferResult",
            "PromptSuggestion",
            "DirEntry",
            "JsDiagnostic",
            "JsRange",
            "JsPosition",
            "LanguagePackConfig",
            "LspServerPackConfig",
            "ProcessLimitsPackConfig",
            "FormatterPackConfig",
        ];

        for type_name in &expected_types {
            assert!(
                get_type_decl(type_name).is_some(),
                "get_type_decl should return a declaration for '{}'",
                type_name
            );
        }
    }

    #[test]
    fn test_get_type_decl_aliases_resolve_same() {
        // Rust name aliases should produce the same declaration as ts-rs name
        let alias_pairs = vec![
            ("CompositeHunk", "TsCompositeHunk"),
            ("CompositeLayoutConfig", "TsCompositeLayoutConfig"),
            ("CompositeSourceConfig", "TsCompositeSourceConfig"),
            ("CompositePaneStyle", "TsCompositePaneStyle"),
            (
                "CreateCompositeBufferOptions",
                "TsCreateCompositeBufferOptions",
            ),
            ("ActionPopupAction", "TsActionPopupAction"),
            ("Suggestion", "PromptSuggestion"),
            ("JsTextPropertyEntry", "TextPropertyEntry"),
        ];

        for (rust_name, ts_name) in &alias_pairs {
            let rust_decl = get_type_decl(rust_name);
            let ts_decl = get_type_decl(ts_name);
            assert!(
                rust_decl.is_some(),
                "get_type_decl should handle Rust name '{}'",
                rust_name
            );
            assert_eq!(
                rust_decl, ts_decl,
                "Alias '{}' and '{}' should produce identical declarations",
                rust_name, ts_name
            );
        }
    }

    #[test]
    fn test_terminal_types_exist() {
        let terminal_result = get_type_decl("TerminalResult");
        assert!(
            terminal_result.is_some(),
            "TerminalResult should be defined"
        );
        let decl = terminal_result.unwrap();
        assert!(
            decl.contains("bufferId"),
            "TerminalResult should have bufferId field"
        );
        assert!(
            decl.contains("terminalId"),
            "TerminalResult should have terminalId field"
        );
        assert!(
            decl.contains("splitId"),
            "TerminalResult should have splitId field"
        );

        let terminal_opts = get_type_decl("CreateTerminalOptions");
        assert!(
            terminal_opts.is_some(),
            "CreateTerminalOptions should be defined"
        );
    }

    #[test]
    fn test_cursor_info_type_exists() {
        let cursor_info = get_type_decl("CursorInfo");
        assert!(cursor_info.is_some(), "CursorInfo should be defined");
        let decl = cursor_info.unwrap();
        assert!(
            decl.contains("position"),
            "CursorInfo should have position field"
        );
        assert!(
            decl.contains("selection"),
            "CursorInfo should have selection field"
        );
    }

    #[test]
    fn test_collect_ts_types_no_duplicates() {
        let output = collect_ts_types();
        let lines: Vec<&str> = output.lines().collect();

        // Check for duplicate type/interface declarations
        let mut declarations = std::collections::HashSet::new();
        for line in &lines {
            let trimmed = line.trim();
            // Match type declarations: "type Foo = {" or "type Foo ="
            if trimmed.starts_with("type ") && trimmed.contains('=') {
                let name = trimmed
                    .strip_prefix("type ")
                    .unwrap()
                    .split(|c: char| c == '=' || c.is_whitespace())
                    .next()
                    .unwrap();
                assert!(
                    declarations.insert(name.to_string()),
                    "Duplicate type declaration found: '{}'",
                    name
                );
            }
        }
    }

    #[test]
    fn test_collect_ts_types_includes_dependency_types() {
        let output = collect_ts_types();
        let required_types = [
            "TextPropertyEntry",
            "TsCompositeLayoutConfig",
            "TsCompositeSourceConfig",
            "TsCompositePaneStyle",
            "TsCompositeHunk",
            "TsCreateCompositeBufferOptions",
            "PromptSuggestion",
            "BufferInfo",
            "CursorInfo",
            "TerminalResult",
            "CreateTerminalOptions",
        ];

        for type_name in &required_types {
            assert!(
                output.contains(type_name),
                "collect_ts_types output should contain type '{}'",
                type_name
            );
        }
    }

    #[test]
    fn test_generated_dts_validates_as_typescript() {
        use crate::backend::quickjs_backend::{JSEDITORAPI_TS_EDITOR_API, JSEDITORAPI_TS_PREAMBLE};

        let ts_types = collect_ts_types();
        let content = format!(
            "{}\n{}\n{}",
            JSEDITORAPI_TS_PREAMBLE, ts_types, JSEDITORAPI_TS_EDITOR_API
        );

        validate_typescript(&content).expect("Generated TypeScript should be syntactically valid");
    }

    #[test]
    fn test_generated_dts_no_undefined_type_references() {
        use crate::backend::quickjs_backend::{JSEDITORAPI_TS_EDITOR_API, JSEDITORAPI_TS_PREAMBLE};

        let ts_types = collect_ts_types();
        let content = format!(
            "{}\n{}\n{}",
            JSEDITORAPI_TS_PREAMBLE, ts_types, JSEDITORAPI_TS_EDITOR_API
        );

        // Collect all defined type names
        let mut defined_types = std::collections::HashSet::new();
        // Built-in types
        for builtin in &[
            "number",
            "string",
            "boolean",
            "void",
            "unknown",
            "null",
            "undefined",
            "Record",
            "Array",
            "Promise",
            "ProcessHandle",
            "PromiseLike",
            "BufferId",
            "SplitId",
            "EditorAPI",
        ] {
            defined_types.insert(builtin.to_string());
        }

        // Extract defined types from declarations
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("type ") && trimmed.contains('=') {
                if let Some(name) = trimmed
                    .strip_prefix("type ")
                    .unwrap()
                    .split(|c: char| c == '=' || c.is_whitespace())
                    .next()
                {
                    defined_types.insert(name.to_string());
                }
            }
            if trimmed.starts_with("interface ") {
                if let Some(name) = trimmed
                    .strip_prefix("interface ")
                    .unwrap()
                    .split(|c: char| !c.is_alphanumeric() && c != '_')
                    .next()
                {
                    defined_types.insert(name.to_string());
                }
            }
        }

        // Extract capitalized identifiers from EditorAPI method signature lines only
        // (skip JSDoc comment lines which contain prose with capitalized words)
        let interface_section = JSEDITORAPI_TS_EDITOR_API;
        let mut undefined_refs = Vec::new();

        for line in interface_section.lines() {
            let trimmed = line.trim();

            // Skip JSDoc comments and blank lines
            if trimmed.starts_with('*')
                || trimmed.starts_with("/*")
                || trimmed.starts_with("//")
                || trimmed.is_empty()
                || trimmed == "{"
                || trimmed == "}"
            {
                continue;
            }

            // This should be a method signature line
            for word in trimmed.split(|c: char| !c.is_alphanumeric() && c != '_') {
                if word.is_empty() {
                    continue;
                }
                // Type references start with uppercase letter
                if word.chars().next().is_some_and(|c| c.is_uppercase())
                    && !defined_types.contains(word)
                {
                    undefined_refs.push(word.to_string());
                }
            }
        }

        // Remove duplicates for clearer error message
        undefined_refs.sort();
        undefined_refs.dedup();

        assert!(
            undefined_refs.is_empty(),
            "Found undefined type references in EditorAPI interface: {:?}",
            undefined_refs
        );
    }

    #[test]
    fn test_editor_api_cursor_methods_have_typed_returns() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // getPrimaryCursor should return CursorInfo | null, not unknown
        assert!(
            api.contains("getPrimaryCursor(): CursorInfo | null;"),
            "getPrimaryCursor should return CursorInfo | null, got: {}",
            api.lines()
                .find(|l| l.contains("getPrimaryCursor"))
                .unwrap_or("not found")
        );

        // getAllCursors should return CursorInfo[], not unknown
        assert!(
            api.contains("getAllCursors(): CursorInfo[];"),
            "getAllCursors should return CursorInfo[], got: {}",
            api.lines()
                .find(|l| l.contains("getAllCursors"))
                .unwrap_or("not found")
        );

        // getAllCursorPositions should return number[], not unknown
        assert!(
            api.contains("getAllCursorPositions(): number[];"),
            "getAllCursorPositions should return number[], got: {}",
            api.lines()
                .find(|l| l.contains("getAllCursorPositions"))
                .unwrap_or("not found")
        );
    }

    #[test]
    fn test_editor_api_terminal_methods_use_defined_types() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // createTerminal should use CreateTerminalOptions and TerminalResult
        assert!(
            api.contains("CreateTerminalOptions"),
            "createTerminal should reference CreateTerminalOptions"
        );
        assert!(
            api.contains("TerminalResult"),
            "createTerminal should reference TerminalResult"
        );
    }

    #[test]
    fn test_editor_api_composite_methods_use_ts_prefix_types() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // updateCompositeAlignment should use TsCompositeHunk (not CompositeHunk)
        assert!(
            api.contains("TsCompositeHunk[]"),
            "updateCompositeAlignment should use TsCompositeHunk[], not CompositeHunk[]"
        );

        // createCompositeBuffer should use TsCreateCompositeBufferOptions
        assert!(
            api.contains("TsCreateCompositeBufferOptions"),
            "createCompositeBuffer should use TsCreateCompositeBufferOptions"
        );
    }

    #[test]
    fn test_editor_api_prompt_suggestions_use_prompt_suggestion() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // setPromptSuggestions should use PromptSuggestion (not Suggestion)
        assert!(
            api.contains("PromptSuggestion[]"),
            "setPromptSuggestions should use PromptSuggestion[], not Suggestion[]"
        );
    }

    #[test]
    fn test_all_editor_api_methods_present() {
        use crate::backend::quickjs_backend::JSEDITORAPI_TS_EDITOR_API;

        let api = JSEDITORAPI_TS_EDITOR_API;

        // Comprehensive list of all expected methods
        let expected_methods = vec![
            "apiVersion",
            "getActiveBufferId",
            "getActiveSplitId",
            "listBuffers",
            "debug",
            "info",
            "warn",
            "error",
            "setStatus",
            "copyToClipboard",
            "setClipboard",
            "registerCommand",
            "unregisterCommand",
            "setContext",
            "executeAction",
            "getCursorPosition",
            "getBufferPath",
            "getBufferLength",
            "isBufferModified",
            "saveBufferToPath",
            "getBufferInfo",
            "getPrimaryCursor",
            "getAllCursors",
            "getAllCursorPositions",
            "getViewport",
            "getCursorLine",
            "getLineStartPosition",
            "getLineEndPosition",
            "getBufferLineCount",
            "scrollToLineCenter",
            "findBufferByPath",
            "getBufferSavedDiff",
            "insertText",
            "deleteRange",
            "insertAtCursor",
            "openFile",
            "openFileInSplit",
            "showBuffer",
            "closeBuffer",
            "on",
            "off",
            "getEnv",
            "getCwd",
            "pathJoin",
            "pathDirname",
            "pathBasename",
            "pathExtname",
            "pathIsAbsolute",
            "utf8ByteLength",
            "fileExists",
            "readFile",
            "writeFile",
            "readDir",
            "createDir",
            "removePath",
            "renamePath",
            "copyPath",
            "getTempDir",
            "getConfig",
            "getUserConfig",
            "reloadConfig",
            "reloadThemes",
            "reloadAndApplyTheme",
            "registerGrammar",
            "registerLanguageConfig",
            "registerLspServer",
            "reloadGrammars",
            "getConfigDir",
            "getThemesDir",
            "applyTheme",
            "getThemeSchema",
            "getBuiltinThemes",
            "getThemeData",
            "saveThemeFile",
            "themeFileExists",
            "deleteTheme",
            "fileStat",
            "isProcessRunning",
            "killProcess",
            "pluginTranslate",
            "createCompositeBuffer",
            "updateCompositeAlignment",
            "closeCompositeBuffer",
            "getHighlights",
            "addOverlay",
            "clearNamespace",
            "clearAllOverlays",
            "clearOverlaysInRange",
            "removeOverlay",
            "addConceal",
            "clearConcealNamespace",
            "clearConcealsInRange",
            "addSoftBreak",
            "clearSoftBreakNamespace",
            "clearSoftBreaksInRange",
            "submitViewTransform",
            "clearViewTransform",
            "setLayoutHints",
            "setFileExplorerDecorations",
            "clearFileExplorerDecorations",
            "addVirtualText",
            "removeVirtualText",
            "removeVirtualTextsByPrefix",
            "clearVirtualTexts",
            "clearVirtualTextNamespace",
            "addVirtualLine",
            "prompt",
            "startPrompt",
            "startPromptWithInitial",
            "setPromptSuggestions",
            "setPromptInputSync",
            "defineMode",
            "setEditorMode",
            "getEditorMode",
            "closeSplit",
            "setSplitBuffer",
            "focusSplit",
            "setSplitScroll",
            "setSplitRatio",
            "setSplitLabel",
            "clearSplitLabel",
            "getSplitByLabel",
            "distributeSplitsEvenly",
            "setBufferCursor",
            "setLineIndicator",
            "clearLineIndicators",
            "setLineNumbers",
            "setViewMode",
            "setViewState",
            "getViewState",
            "setGlobalState",
            "getGlobalState",
            "setLineWrap",
            "createScrollSyncGroup",
            "setScrollSyncAnchors",
            "removeScrollSyncGroup",
            "executeActions",
            "showActionPopup",
            "disableLspForLanguage",
            "setLspRootUri",
            "getAllDiagnostics",
            "getHandlers",
            "createVirtualBuffer",
            "createVirtualBufferInSplit",
            "createVirtualBufferInExistingSplit",
            "setVirtualBufferContent",
            "getTextPropertiesAtCursor",
            "spawnProcess",
            "spawnProcessWait",
            "getBufferText",
            "delay",
            "sendLspRequest",
            "spawnBackgroundProcess",
            "killBackgroundProcess",
            "createTerminal",
            "sendTerminalInput",
            "closeTerminal",
            "refreshLines",
            "getCurrentLocale",
            "loadPlugin",
            "unloadPlugin",
            "reloadPlugin",
            "listPlugins",
        ];

        let mut missing = Vec::new();
        for method in &expected_methods {
            // Check that the method name appears followed by ( in the API
            let pattern = format!("{}(", method);
            if !api.contains(&pattern) {
                missing.push(*method);
            }
        }

        assert!(
            missing.is_empty(),
            "Missing methods in EditorAPI interface: {:?}",
            missing
        );
    }
}
