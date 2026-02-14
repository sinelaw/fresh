//! # Fresh Plugin API Macros
//!
//! Proc macros for generating TypeScript definitions from Rust QuickJS API implementations.
//!
//! ## Overview
//!
//! This crate provides the `#[plugin_api_impl]` attribute macro that:
//! 1. Parses method signatures from a `JsEditorApi` impl block
//! 2. Generates TypeScript type definitions (`.d.ts`)
//! 3. Automatically writes to `plugins/lib/fresh.d.ts` during compilation
//!
//! ## Usage
//!
//! ```rust,ignore
//! use fresh_plugin_api_macros::{plugin_api, plugin_api_impl};
//!
//! #[plugin_api_impl]
//! #[rquickjs::methods(rename_all = "camelCase")]
//! impl JsEditorApi {
//!     /// Get the active buffer ID (0 if none)
//!     pub fn get_active_buffer_id(&self) -> u32 { ... }
//!
//!     /// Create a virtual buffer (async)
//!     #[plugin_api(async_promise, js_name = "createVirtualBuffer", ts_return = "number")]
//!     #[qjs(rename = "_createVirtualBufferStart")]
//!     pub fn create_virtual_buffer_start(&self, opts: Object) -> u64 { ... }
//! }
//! ```
//!
//! ## Attributes
//!
//! ### `#[plugin_api_impl]`
//!
//! Apply to the impl block to enable TypeScript generation. Generates:
//! - `{IMPL_NAME}_TYPESCRIPT_DEFINITIONS: &str` - Full `.d.ts` content
//! - `{IMPL_NAME}_JS_METHODS: &[&str]` - List of all JS method names
//!
//! ### `#[plugin_api(...)]`
//!
//! Apply to individual methods for customization:
//!
//! | Attribute | Description | Example |
//! |-----------|-------------|---------|
//! | `skip` | Exclude from TypeScript | `#[plugin_api(skip)]` |
//! | `js_name = "..."` | Custom JS method name | `#[plugin_api(js_name = "myMethod")]` |
//! | `async_promise` | Returns `Promise<T>` | `#[plugin_api(async_promise)]` |
//! | `async_thenable` | Returns `ProcessHandle<T>` (cancellable) | `#[plugin_api(async_thenable)]` |
//! | `ts_type = "..."` | Custom TypeScript type for parameter | `#[plugin_api(ts_type = "BufferInfo")]` |
//! | `ts_return = "..."` | Custom TypeScript return type | `#[plugin_api(ts_return = "string")]` |
//!
//! ## Type Mapping
//!
//! | Rust Type | TypeScript Type | Notes |
//! |-----------|-----------------|-------|
//! | `u8`, `u16`, `u32`, `i32`, etc. | `number` | All numeric types |
//! | `bool` | `boolean` | |
//! | `String`, `&str` | `string` | |
//! | `()` | `void` | |
//! | `Option<T>` | `T \| null` | |
//! | `Vec<T>` | `T[]` | |
//! | `rquickjs::Ctx<'js>` | *(skipped)* | Runtime context |
//! | `rquickjs::function::Opt<T>` | `T?` | Optional parameter |
//! | `rquickjs::function::Rest<T>` | `...T[]` | Variadic parameter |
//! | `rquickjs::Result<T>` | `T` | Unwrapped |
//! | `rquickjs::Object<'js>` | `Record<string, unknown>` | Use `ts_type` for specifics |
//!
//! ## Async Methods
//!
//! Async methods must be explicitly marked with `#[plugin_api(async_promise)]` or
//! `#[plugin_api(async_thenable)]`. There is no heuristic-based detection.
//!
//! - `async_promise`: For operations that complete with a result
//! - `async_thenable`: For cancellable operations (e.g., process spawning)
//!
//! ## File Output
//!
//! The macro automatically writes `plugins/lib/fresh.d.ts` when:
//! 1. Building the main crate (not the macro crate)
//! 2. The content has changed (avoids unnecessary rebuilds)
//!
//! ## Design Principles
//!
//! 1. **Single Source of Truth**: API defined once in Rust, TypeScript generated
//! 2. **Explicit Over Implicit**: No magic naming conventions, use attributes
//! 3. **Deterministic Output**: Same input always produces same output
//! 4. **Preserve Original Code**: Macro passes through impl block unchanged
//! 5. **Clear Errors**: Compile-time errors with helpful messages

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, Attribute, FnArg, GenericArgument, ImplItem, ImplItemFn,
    ItemImpl, Meta, Pat, PathArguments, ReturnType, Type,
};

// ============================================================================
// Error Handling
// ============================================================================

/// Create a compile error with a helpful message and source span
fn compile_error(span: proc_macro2::Span, message: &str) -> proc_macro2::TokenStream {
    syn::Error::new(span, message).to_compile_error()
}

// ============================================================================
// API Method Classification
// ============================================================================

/// Classification of API method return behavior
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ApiKind {
    /// Synchronous method - returns value directly
    Sync,
    /// Async method returning `Promise<T>`
    AsyncPromise,
    /// Async method returning `ProcessHandle<T>` (cancellable)
    AsyncThenable,
}

impl ApiKind {
    /// Wrap a TypeScript type in the appropriate async wrapper
    fn wrap_return_type(&self, inner: &str) -> String {
        match self {
            ApiKind::Sync => inner.to_string(),
            ApiKind::AsyncPromise => format!("Promise<{}>", inner),
            ApiKind::AsyncThenable => format!("ProcessHandle<{}>", inner),
        }
    }
}

// ============================================================================
// Parsed Structures (Intermediate Representation)
// ============================================================================

/// Parsed API method - intermediate representation for code generation
#[derive(Debug)]
struct ApiMethod {
    /// JavaScript method name (camelCase)
    js_name: String,
    /// Method classification (sync/async)
    kind: ApiKind,
    /// Parsed parameters
    params: Vec<ParamInfo>,
    /// TypeScript return type
    return_type: String,
    /// Documentation from doc comments
    doc: String,
}

/// Parsed parameter information
#[derive(Debug)]
struct ParamInfo {
    /// Parameter name (camelCase)
    name: String,
    /// TypeScript type
    ts_type: String,
    /// Whether parameter is optional (from `Opt<T>`)
    optional: bool,
    /// Whether parameter is variadic (from `Rest<T>`)
    variadic: bool,
}

impl ParamInfo {
    /// Format as TypeScript parameter
    fn to_typescript(&self) -> String {
        if self.variadic {
            format!("...{}: {}[]", self.name, self.ts_type)
        } else if self.optional {
            format!("{}?: {}", self.name, self.ts_type)
        } else {
            format!("{}: {}", self.name, self.ts_type)
        }
    }
}

// ============================================================================
// String Utilities
// ============================================================================

/// Convert snake_case identifier to camelCase
///
/// # Examples
/// ```ignore
/// assert_eq!(to_camel_case("get_active_buffer"), "getActiveBuffer");
/// assert_eq!(to_camel_case("simple"), "simple");
/// ```
fn to_camel_case(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut capitalize_next = false;

    for c in s.chars() {
        if c == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}

// ============================================================================
// Attribute Parsing
// ============================================================================

/// Extract documentation from `#[doc = "..."]` attributes
fn extract_doc_comment(attrs: &[Attribute]) -> String {
    attrs
        .iter()
        .filter_map(|attr| {
            if !attr.path().is_ident("doc") {
                return None;
            }
            if let Meta::NameValue(meta) = &attr.meta {
                if let syn::Expr::Lit(expr_lit) = &meta.value {
                    if let syn::Lit::Str(lit_str) = &expr_lit.lit {
                        return Some(lit_str.value().trim().to_string());
                    }
                }
            }
            None
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse a string value from attribute tokens like `key = "value"`
fn parse_attr_string_value(tokens: &str, key: &str) -> Option<String> {
    let start = tokens.find(key)?;
    let rest = &tokens[start..];
    let eq_pos = rest.find('=')?;
    let after_eq = rest[eq_pos + 1..].trim();

    if !after_eq.starts_with('"') {
        return None;
    }

    let end_quote = after_eq[1..].find('"')?;
    Some(after_eq[1..end_quote + 1].to_string())
}

/// Check if `#[plugin_api(...)]` contains a specific flag
fn has_plugin_api_flag(attrs: &[Attribute], flag: &str) -> bool {
    attrs.iter().any(|attr| {
        if !attr.path().is_ident("plugin_api") {
            return false;
        }
        if let Meta::List(meta_list) = &attr.meta {
            meta_list.tokens.to_string().contains(flag)
        } else {
            false
        }
    })
}

/// Get a string value from `#[plugin_api(key = "value")]`
fn get_plugin_api_value(attrs: &[Attribute], key: &str) -> Option<String> {
    for attr in attrs {
        if !attr.path().is_ident("plugin_api") {
            continue;
        }
        if let Meta::List(meta_list) = &attr.meta {
            if let Some(value) = parse_attr_string_value(&meta_list.tokens.to_string(), key) {
                return Some(value);
            }
        }
    }
    None
}

/// Get custom JS name from `#[qjs(rename = "...")]` or `#[plugin_api(js_name = "...")]`
fn get_js_name(attrs: &[Attribute]) -> Option<String> {
    // First check plugin_api attribute (takes precedence)
    if let Some(name) = get_plugin_api_value(attrs, "js_name") {
        return Some(name);
    }

    // Then check qjs attribute
    for attr in attrs {
        if !attr.path().is_ident("qjs") {
            continue;
        }
        if let Meta::List(meta_list) = &attr.meta {
            if let Some(name) = parse_attr_string_value(&meta_list.tokens.to_string(), "rename") {
                return Some(name);
            }
        }
    }
    None
}

// ============================================================================
// Type Analysis
// ============================================================================

/// Extract inner type from generic wrapper like `Option<T>`, `Vec<T>`
fn extract_inner_type(ty: &Type) -> Option<Type> {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if let PathArguments::AngleBracketed(args) = &segment.arguments {
                if let Some(GenericArgument::Type(inner)) = args.args.first() {
                    return Some(inner.clone());
                }
            }
        }
    }
    None
}

/// Get the final segment name from a type path (e.g., "Opt" from "rquickjs::function::Opt")
fn get_type_name(ty: &Type) -> Option<String> {
    if let Type::Path(type_path) = ty {
        type_path.path.segments.last().map(|s| s.ident.to_string())
    } else {
        None
    }
}

/// Check if type is QuickJS context (`Ctx<'js>`) - should be skipped from parameters
fn is_ctx_type(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        // Check final segment
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident == "Ctx" {
                return true;
            }
        }
        // Check full path for "Ctx" anywhere
        let path_str: String = type_path
            .path
            .segments
            .iter()
            .map(|s| s.ident.to_string())
            .collect::<Vec<_>>()
            .join("::");
        path_str.contains("Ctx")
    } else {
        false
    }
}

/// Check if type is `Opt<T>` (optional parameter)
fn is_opt_type(ty: &Type) -> bool {
    get_type_name(ty).is_some_and(|n| n == "Opt")
}

/// Check if type is `Rest<T>` (variadic parameter)
fn is_rest_type(ty: &Type) -> bool {
    get_type_name(ty).is_some_and(|n| n == "Rest")
}

// ============================================================================
// Rust to TypeScript Type Conversion
// ============================================================================

/// Convert a Rust type to its TypeScript equivalent
///
/// Handles:
/// - Primitive types (numbers, bool, string)
/// - Generic wrappers (Option, Vec, Result)
/// - QuickJS types (Opt, Rest, Object, Value)
/// - Known API types (BufferInfo, etc.)
fn rust_to_typescript(ty: &Type, attrs: &[Attribute]) -> String {
    // Check for explicit ts_type override
    if let Some(custom) = get_plugin_api_value(attrs, "ts_type") {
        return custom;
    }

    match ty {
        Type::Path(type_path) => {
            let type_name = type_path
                .path
                .segments
                .last()
                .map(|s| s.ident.to_string())
                .unwrap_or_else(|| "unknown".to_string());

            match type_name.as_str() {
                // Numeric types -> number
                "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "usize" | "isize"
                | "f32" | "f64" => "number".to_string(),

                // Boolean
                "bool" => "boolean".to_string(),

                // String types
                "String" | "str" => "string".to_string(),

                // Unit type
                "()" => "void".to_string(),

                // Option<T> -> T | null
                "Option" => {
                    let inner = extract_inner_type(ty)
                        .map(|t| rust_to_typescript(&t, &[]))
                        .unwrap_or_else(|| "unknown".to_string());
                    format!("{} | null", inner)
                }

                // Vec<T> -> T[]
                "Vec" => {
                    let inner = extract_inner_type(ty)
                        .map(|t| rust_to_typescript(&t, &[]))
                        .unwrap_or_else(|| "unknown".to_string());
                    format!("{}[]", inner)
                }

                // Opt<T> -> extract inner (optionality handled at param level)
                "Opt" => extract_inner_type(ty)
                    .map(|t| rust_to_typescript(&t, &[]))
                    .unwrap_or_else(|| "unknown".to_string()),

                // Rest<T> -> extract inner (variadic handled at param level)
                "Rest" => extract_inner_type(ty)
                    .map(|t| rust_to_typescript(&t, &[]))
                    .unwrap_or_else(|| "unknown".to_string()),

                // Result<T, E> -> extract T
                "Result" => extract_inner_type(ty)
                    .map(|t| rust_to_typescript(&t, &[]))
                    .unwrap_or_else(|| "unknown".to_string()),

                // QuickJS types
                "Value" => "unknown".to_string(),
                "Object" => "Record<string, unknown>".to_string(),

                // Rust collections
                "HashMap" | "BTreeMap" => "Record<string, unknown>".to_string(),

                // Known API types - pass through unchanged
                "BufferInfo"
                | "CursorInfo"
                | "ViewportInfo"
                | "SpawnResult"
                | "BackgroundProcessResult"
                | "DirEntry"
                | "FileStat"
                | "CreateVirtualBufferResult"
                | "PromptSuggestion"
                | "TextPropertyEntry"
                | "JsTextPropertyEntry"
                | "CreateVirtualBufferOptions"
                | "CreateVirtualBufferInSplitOptions"
                | "CreateVirtualBufferInExistingSplitOptions"
                | "VirtualBufferResult"
                | "ActionSpec"
                | "ActionPopupAction"
                | "ActionPopupOptions"
                | "ViewTokenWire"
                | "ViewTokenStyle"
                | "LayoutHints"
                | "FileExplorerDecoration"
                | "TsCreateCompositeBufferOptions"
                | "TsCompositeLayoutConfig"
                | "TsCompositeSourceConfig"
                | "TsCompositePaneStyle"
                | "TsCompositeHunk"
                | "TsHighlightSpan"
                | "TsActionPopupAction"
                | "JsDiagnostic" => type_name,

                // Default: use type name as-is
                _ => type_name,
            }
        }
        Type::Tuple(tuple) if tuple.elems.is_empty() => "void".to_string(),
        Type::Reference(reference) => rust_to_typescript(&reference.elem, attrs),
        _ => "unknown".to_string(),
    }
}

// ============================================================================
// Method Parsing
// ============================================================================

/// Parse a method from the impl block into an ApiMethod
///
/// Returns `None` if the method should be skipped (marked with `skip` or internal)
fn parse_method(method: &ImplItemFn) -> Option<ApiMethod> {
    // Skip methods marked with #[plugin_api(skip)]
    if has_plugin_api_flag(&method.attrs, "skip") {
        return None;
    }

    let rust_name = method.sig.ident.to_string();
    let doc = extract_doc_comment(&method.attrs);

    // Determine method kind from explicit attributes only (no heuristics)
    let kind = if has_plugin_api_flag(&method.attrs, "async_thenable") {
        ApiKind::AsyncThenable
    } else if has_plugin_api_flag(&method.attrs, "async_promise") {
        ApiKind::AsyncPromise
    } else {
        ApiKind::Sync
    };

    // Get JS name: explicit > snake_to_camel conversion
    let js_name = get_js_name(&method.attrs).unwrap_or_else(|| to_camel_case(&rust_name));

    // Skip internal methods (names starting with underscore)
    if js_name.starts_with('_') {
        return None;
    }

    // Parse parameters
    let params: Vec<ParamInfo> = method
        .sig
        .inputs
        .iter()
        .filter_map(|arg| {
            let FnArg::Typed(pat_type) = arg else {
                return None;
            };
            let Pat::Ident(pat_ident) = &*pat_type.pat else {
                return None;
            };

            let param_name = pat_ident.ident.to_string();

            // Skip self parameter
            if param_name == "self" {
                return None;
            }

            let ty = &*pat_type.ty;

            // Skip QuickJS context parameter
            if is_ctx_type(ty) {
                return None;
            }

            Some(ParamInfo {
                name: to_camel_case(&param_name),
                ts_type: rust_to_typescript(ty, &pat_type.attrs),
                optional: is_opt_type(ty),
                variadic: is_rest_type(ty),
            })
        })
        .collect();

    // Parse return type
    let return_type = match &method.sig.output {
        ReturnType::Default => "void".to_string(),
        ReturnType::Type(_, ty) => {
            // Check for explicit ts_return override
            get_plugin_api_value(&method.attrs, "ts_return")
                .unwrap_or_else(|| rust_to_typescript(ty, &method.attrs))
        }
    };

    Some(ApiMethod {
        js_name,
        kind,
        params,
        return_type,
        doc,
    })
}

// ============================================================================
// TypeScript Code Generation
// ============================================================================

/// Generate TypeScript method signature with JSDoc
fn generate_ts_method(method: &ApiMethod) -> String {
    let mut lines = Vec::new();

    // JSDoc comment
    if !method.doc.is_empty() {
        lines.push("  /**".to_string());
        for line in method.doc.lines() {
            lines.push(format!("   * {}", line));
        }
        lines.push("   */".to_string());
    }

    // Method signature
    let params: String = method
        .params
        .iter()
        .map(ParamInfo::to_typescript)
        .collect::<Vec<_>>()
        .join(", ");

    let return_type = method.kind.wrap_return_type(&method.return_type);

    lines.push(format!(
        "  {}({}): {};",
        method.js_name, params, return_type
    ));

    lines.join("\n")
}

/// Generate the TypeScript preamble (header comment and getEditor declaration)
fn generate_ts_preamble() -> &'static str {
    r#"/**
 * Fresh Editor TypeScript Plugin API
 *
 * This file provides type definitions for the Fresh editor's TypeScript plugin system.
 * Plugins have access to the global `editor` object which provides methods to:
 * - Query editor state (buffers, cursors, viewports)
 * - Modify buffer content (insert, delete text)
 * - Add visual decorations (overlays, highlighting)
 * - Interact with the editor UI (status messages, prompts)
 *
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generated by fresh-plugin-api-macros + ts-rs from JsEditorApi impl
 */

/**
 * Get the editor API instance.
 * Plugins must call this at the top of their file to get a scoped editor object.
 */
declare function getEditor(): EditorAPI;

/** Handle for a cancellable async operation */
interface ProcessHandle<T> extends PromiseLike<T> {
  /** Promise that resolves to the result when complete */
  readonly result: Promise<T>;
  /** Cancel/kill the operation. Returns true if cancelled, false if already completed */
  kill(): Promise<boolean>;
}

/** Buffer identifier */
type BufferId = number;

/** Split identifier */
type SplitId = number;

"#
}

/// Generate the EditorAPI interface (methods only)
/// Types are provided separately via ts-rs
fn generate_editor_api_interface(methods: &[ApiMethod]) -> String {
    let method_sigs: Vec<String> = methods.iter().map(generate_ts_method).collect();

    format!(
        "/**\n * Main editor API interface\n */\ninterface EditorAPI {{\n{}\n}}\n",
        method_sigs.join("\n\n")
    )
}

/// Built-in TypeScript types that don't need to be collected
const BUILTIN_TS_TYPES: &[&str] = &[
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
    "SplitId", // Defined in preamble
];

/// Extract type names from a TypeScript type string
///
/// Handles:
/// - Simple types: "SpawnResult" -> ["SpawnResult"]
/// - Generics: "ProcessHandle<SpawnResult>" -> ["SpawnResult"]
/// - Union: "string | null" -> []
/// - Arrays: "BufferInfo[]" -> ["BufferInfo"]
fn extract_type_references(ts_type: &str) -> Vec<String> {
    let mut types = Vec::new();

    // Remove generic wrappers like ProcessHandle<...>, Promise<...>, Array<...>
    let mut current = ts_type.to_string();

    // Strip outer generics repeatedly
    while let Some(start) = current.find('<') {
        if let Some(end) = current.rfind('>') {
            let outer = current[..start].trim().to_string();
            let inner = current[start + 1..end].trim().to_string();

            // But check if outer is a custom type we need
            if !BUILTIN_TS_TYPES.contains(&outer.as_str()) && !outer.is_empty() {
                types.push(outer);
            }

            // Process the inner type
            current = inner;
        } else {
            break;
        }
    }

    // Handle union types (split by |)
    for part in current.split('|') {
        let part = part.trim();

        // Skip built-in types
        if BUILTIN_TS_TYPES.contains(&part) {
            continue;
        }

        // Handle array types like "BufferInfo[]"
        let part = part.trim_end_matches("[]");

        // Skip Record<...> and other generics
        if part.contains('<') || part.contains('>') {
            continue;
        }

        // Skip empty or built-in
        if part.is_empty() || BUILTIN_TS_TYPES.contains(&part) {
            continue;
        }

        // This looks like a custom type reference
        if part.chars().next().is_some_and(|c| c.is_uppercase()) {
            types.push(part.to_string());
        }
    }

    types
}

/// Collect all type references from methods
fn collect_referenced_types(methods: &[ApiMethod]) -> Vec<String> {
    let mut types = std::collections::HashSet::new();

    for method in methods {
        // Collect from return type
        for ty in extract_type_references(&method.return_type) {
            types.insert(ty);
        }

        // Collect from parameters
        for param in &method.params {
            for ty in extract_type_references(&param.ts_type) {
                types.insert(ty);
            }
        }
    }

    let mut sorted: Vec<String> = types.into_iter().collect();
    sorted.sort();
    sorted
}

// ============================================================================
// Proc Macros
// ============================================================================

/// Generate TypeScript definitions from a QuickJS impl block
///
/// # Generated Constants
///
/// - `{IMPL_NAME}_TS_PREAMBLE: &str` - Header comment + getEditor + ProcessHandle + BufferId/SplitId
/// - `{IMPL_NAME}_TS_EDITOR_API: &str` - Just the EditorAPI interface with methods
/// - `{IMPL_NAME}_JS_METHODS: &[&str]` - List of all JS method names
///
/// The main crate should combine these with ts-rs generated types to create fresh.d.ts.
///
/// # Example
///
/// ```rust,ignore
/// #[plugin_api_impl]
/// #[rquickjs::methods(rename_all = "camelCase")]
/// impl JsEditorApi {
///     /// Get the active buffer ID
///     pub fn get_active_buffer_id(&self) -> u32 { ... }
///
///     /// Spawn a process (cancellable)
///     #[plugin_api(async_thenable, js_name = "spawnProcess", ts_return = "SpawnResult")]
///     #[qjs(rename = "_spawnProcessStart")]
///     pub fn spawn_process_start(&self, cmd: String) -> u64 { ... }
/// }
/// ```
///
/// # Errors
///
/// Compile-time error if applied to non-impl items.
#[proc_macro_attribute]
pub fn plugin_api_impl(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemImpl);

    // Extract impl target name
    let impl_name = match &*input.self_ty {
        Type::Path(type_path) => type_path
            .path
            .segments
            .last()
            .map(|s| s.ident.to_string())
            .unwrap_or_else(|| "Unknown".to_string()),
        _ => {
            return compile_error(
                input.self_ty.span(),
                "plugin_api_impl requires a named type (e.g., `impl JsEditorApi`)",
            )
            .into();
        }
    };

    // Generate constant names
    let preamble_const = format_ident!("{}_TS_PREAMBLE", impl_name.to_uppercase());
    let editor_api_const = format_ident!("{}_TS_EDITOR_API", impl_name.to_uppercase());
    let methods_const = format_ident!("{}_JS_METHODS", impl_name.to_uppercase());

    // Parse methods into intermediate representation
    let methods: Vec<ApiMethod> = input
        .items
        .iter()
        .filter_map(|item| {
            if let ImplItem::Fn(method) = item {
                parse_method(method)
            } else {
                None
            }
        })
        .collect();

    // Generate TypeScript parts
    let preamble = generate_ts_preamble();
    let editor_api = generate_editor_api_interface(&methods);

    // Collect JS method names
    let js_names: Vec<&str> = methods.iter().map(|m| m.js_name.as_str()).collect();

    // Collect referenced types (for ts-rs export)
    let referenced_types = collect_referenced_types(&methods);
    let types_const = format_ident!("{}_REFERENCED_TYPES", impl_name.to_uppercase());

    // Strip #[plugin_api(...)] attributes from method parameters before emitting,
    // since plugin_api is a proc_macro_attribute and can't appear on parameters.
    // The attribute was already read during parse_method for ts_type overrides.
    let mut cleaned_input = input.clone();
    for item in &mut cleaned_input.items {
        if let ImplItem::Fn(method) = item {
            for arg in &mut method.sig.inputs {
                if let FnArg::Typed(pat_type) = arg {
                    pat_type
                        .attrs
                        .retain(|attr| !attr.path().is_ident("plugin_api"));
                }
            }
        }
    }

    // Generate output: original impl + constants
    let expanded = quote! {
        #cleaned_input

        /// TypeScript preamble (header, getEditor, ProcessHandle, BufferId, SplitId)
        ///
        /// Combine with ts-rs types and EDITOR_API to create fresh.d.ts
        pub const #preamble_const: &str = #preamble;

        /// TypeScript EditorAPI interface (methods only)
        ///
        /// Combine with preamble and ts-rs types to create fresh.d.ts
        pub const #editor_api_const: &str = #editor_api;

        /// List of all JavaScript method names exposed in the API
        ///
        /// Useful for verification and debugging.
        pub const #methods_const: &[&str] = &[#(#js_names),*];

        /// List of TypeScript types referenced in method signatures
        ///
        /// These types need to be defined (via ts-rs or manually) in fresh.d.ts.
        /// Use this to automatically collect type definitions.
        pub const #types_const: &[&str] = &[#(#referenced_types),*];
    };

    TokenStream::from(expanded)
}

/// Marker attribute for customizing individual API methods
///
/// This attribute is parsed by `#[plugin_api_impl]` but doesn't generate any code itself.
///
/// # Options
///
/// - `skip` - Exclude method from TypeScript generation
/// - `js_name = "..."` - Custom JavaScript method name
/// - `async_promise` - Method returns `Promise<T>`
/// - `async_thenable` - Method returns `ProcessHandle<T>` (cancellable)
/// - `ts_type = "..."` - Custom TypeScript type for a parameter
/// - `ts_return = "..."` - Custom TypeScript return type
///
/// # Examples
///
/// ```rust,ignore
/// // Skip internal helper
/// #[plugin_api(skip)]
/// fn internal_helper(&self) { ... }
///
/// // Async method with custom return type
/// #[plugin_api(async_promise, js_name = "fetchData", ts_return = "DataResult")]
/// fn fetch_data_start(&self) -> u64 { ... }
///
/// // Cancellable operation
/// #[plugin_api(async_thenable, js_name = "spawnProcess", ts_return = "SpawnResult")]
/// fn spawn_process_start(&self, cmd: String) -> u64 { ... }
/// ```
#[proc_macro_attribute]
pub fn plugin_api(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Marker attribute - passes through unchanged
    item
}

// ============================================================================
// Unit Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("get_active_buffer"), "getActiveBuffer");
        assert_eq!(to_camel_case("simple"), "simple");
        assert_eq!(to_camel_case("a_b_c"), "aBC");
        assert_eq!(to_camel_case("already_camel"), "alreadyCamel");
        assert_eq!(to_camel_case(""), "");
        assert_eq!(to_camel_case("_leading"), "Leading");
        assert_eq!(to_camel_case("trailing_"), "trailing");
    }

    #[test]
    fn test_parse_attr_string_value() {
        assert_eq!(
            parse_attr_string_value(r#"js_name = "myMethod""#, "js_name"),
            Some("myMethod".to_string())
        );
        assert_eq!(
            parse_attr_string_value(r#"skip, js_name = "foo""#, "js_name"),
            Some("foo".to_string())
        );
        assert_eq!(parse_attr_string_value(r#"skip"#, "js_name"), None);
        assert_eq!(parse_attr_string_value(r#"js_name = 123"#, "js_name"), None);
    }

    #[test]
    fn test_api_kind_wrap_return_type() {
        assert_eq!(ApiKind::Sync.wrap_return_type("number"), "number");
        assert_eq!(
            ApiKind::AsyncPromise.wrap_return_type("number"),
            "Promise<number>"
        );
        assert_eq!(
            ApiKind::AsyncThenable.wrap_return_type("SpawnResult"),
            "ProcessHandle<SpawnResult>"
        );
    }

    #[test]
    fn test_param_info_to_typescript() {
        let regular = ParamInfo {
            name: "bufferId".to_string(),
            ts_type: "number".to_string(),
            optional: false,
            variadic: false,
        };
        assert_eq!(regular.to_typescript(), "bufferId: number");

        let optional = ParamInfo {
            name: "line".to_string(),
            ts_type: "number".to_string(),
            optional: true,
            variadic: false,
        };
        assert_eq!(optional.to_typescript(), "line?: number");

        let variadic = ParamInfo {
            name: "parts".to_string(),
            ts_type: "string".to_string(),
            optional: false,
            variadic: true,
        };
        assert_eq!(variadic.to_typescript(), "...parts: string[]");
    }

    #[test]
    fn test_generate_ts_preamble_contains_required_declarations() {
        let preamble = generate_ts_preamble();

        // Check essential declarations
        assert!(preamble.contains("declare function getEditor(): EditorAPI"));
        assert!(preamble.contains("interface ProcessHandle<T>"));
        assert!(preamble.contains("type BufferId = number"));
        assert!(preamble.contains("type SplitId = number"));

        // Check it's marked as auto-generated
        assert!(preamble.contains("AUTO-GENERATED FILE"));
    }

    #[test]
    fn test_extract_type_references() {
        // Simple type
        assert_eq!(extract_type_references("SpawnResult"), vec!["SpawnResult"]);

        // Built-in types return empty
        assert!(extract_type_references("number").is_empty());
        assert!(extract_type_references("string").is_empty());
        assert!(extract_type_references("void").is_empty());

        // Generic wrapper - extracts inner type
        assert_eq!(
            extract_type_references("ProcessHandle<SpawnResult>"),
            vec!["SpawnResult"]
        );
        assert_eq!(
            extract_type_references("Promise<BufferInfo>"),
            vec!["BufferInfo"]
        );

        // Union with null
        assert_eq!(
            extract_type_references("CursorInfo | null"),
            vec!["CursorInfo"]
        );

        // Array type
        assert_eq!(extract_type_references("BufferInfo[]"), vec!["BufferInfo"]);

        // Built-in generics return empty
        assert!(extract_type_references("Record<string, unknown>").is_empty());
        assert!(extract_type_references("Promise<void>").is_empty());
    }

    #[test]
    fn test_collect_referenced_types() {
        let methods = vec![
            ApiMethod {
                js_name: "spawnProcess".to_string(),
                kind: ApiKind::AsyncThenable,
                params: vec![],
                return_type: "SpawnResult".to_string(),
                doc: "".to_string(),
            },
            ApiMethod {
                js_name: "listBuffers".to_string(),
                kind: ApiKind::Sync,
                params: vec![],
                return_type: "BufferInfo[]".to_string(),
                doc: "".to_string(),
            },
        ];

        let types = collect_referenced_types(&methods);
        assert!(types.contains(&"SpawnResult".to_string()));
        assert!(types.contains(&"BufferInfo".to_string()));
    }

    #[test]
    fn test_generate_ts_method_sync() {
        let method = ApiMethod {
            js_name: "getActiveBufferId".to_string(),
            kind: ApiKind::Sync,
            params: vec![],
            return_type: "number".to_string(),
            doc: "Get the active buffer ID".to_string(),
        };

        let ts = generate_ts_method(&method);
        assert!(ts.contains("getActiveBufferId(): number;"));
        assert!(ts.contains("Get the active buffer ID"));
    }

    #[test]
    fn test_generate_ts_method_async_promise() {
        let method = ApiMethod {
            js_name: "delay".to_string(),
            kind: ApiKind::AsyncPromise,
            params: vec![ParamInfo {
                name: "ms".to_string(),
                ts_type: "number".to_string(),
                optional: false,
                variadic: false,
            }],
            return_type: "void".to_string(),
            doc: "".to_string(),
        };

        let ts = generate_ts_method(&method);
        assert!(ts.contains("delay(ms: number): Promise<void>;"));
    }

    #[test]
    fn test_generate_ts_method_async_thenable() {
        let method = ApiMethod {
            js_name: "spawnProcess".to_string(),
            kind: ApiKind::AsyncThenable,
            params: vec![
                ParamInfo {
                    name: "command".to_string(),
                    ts_type: "string".to_string(),
                    optional: false,
                    variadic: false,
                },
                ParamInfo {
                    name: "args".to_string(),
                    ts_type: "string".to_string(),
                    optional: false,
                    variadic: false,
                },
            ],
            return_type: "SpawnResult".to_string(),
            doc: "Spawn a process".to_string(),
        };

        let ts = generate_ts_method(&method);
        assert!(
            ts.contains("spawnProcess(command: string, args: string): ProcessHandle<SpawnResult>;")
        );
    }
}
