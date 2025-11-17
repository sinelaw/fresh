//! Build script for Fresh editor
//!
//! Generates TypeScript type definitions from Rust op definitions

use std::collections::HashMap;
use std::fs;
use std::path::Path;

fn main() {
    // Only rerun if ts_runtime.rs changes
    println!("cargo::rerun-if-changed=src/ts_runtime.rs");

    // NOTE: TypeScript type generation is currently disabled because the auto-generator
    // doesn't handle:
    // - Async ops (async fn with Promise return types)
    // - #[serde] parameters (Vec<String>, custom structs)
    // - Custom return types (Result<SpawnResult, ...>, Result<FileStat, ...>)
    // - Interface definitions for custom types
    //
    // The manually maintained types/fresh.d.ts is more complete and accurate.
    // Re-enable this once build.rs is enhanced to handle these cases.
    //
    // if let Err(e) = generate_typescript_types() {
    //     eprintln!("Warning: Failed to generate TypeScript types: {}", e);
    // }
}

/// Information about a single op
struct OpInfo {
    js_name: String,
    params: Vec<(String, String)>, // (name, ts_type)
    return_type: String,
}

/// Parse Rust type to TypeScript type
fn rust_type_to_ts(rust_type: &str) -> String {
    match rust_type.trim() {
        "u32" | "u8" | "usize" | "i32" | "i64" | "u64" | "f32" | "f64" => "number".to_string(),
        "bool" => "boolean".to_string(),
        "String" | "&str" => "string".to_string(),
        "()" => "void".to_string(),
        _ => rust_type.to_string(),
    }
}

/// Convert op_fresh_xxx to camelCase
fn op_name_to_js(op_name: &str) -> String {
    let name = op_name.strip_prefix("op_fresh_").unwrap_or(op_name);
    let parts: Vec<&str> = name.split('_').collect();
    if parts.is_empty() {
        return name.to_string();
    }

    let mut result = parts[0].to_string();
    for part in &parts[1..] {
        if !part.is_empty() {
            let mut chars = part.chars();
            if let Some(first) = chars.next() {
                result.push(first.to_ascii_uppercase());
                result.extend(chars);
            }
        }
    }
    result
}

/// Extract op definitions from Rust source
fn extract_ops(rust_source: &str) -> Vec<OpInfo> {
    let mut ops = Vec::new();

    // Simple line-by-line parsing
    let lines: Vec<&str> = rust_source.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Look for #[op2...] attribute
        if line.starts_with("#[op2") {
            // Check for #[string] return marker on next line
            let mut has_string_return = false;
            let mut fn_line_idx = i + 1;

            while fn_line_idx < lines.len() {
                let next_line = lines[fn_line_idx].trim();
                if next_line.starts_with("#[string]") {
                    has_string_return = true;
                    fn_line_idx += 1;
                } else if next_line.starts_with("fn ") {
                    break;
                } else if next_line.is_empty() || next_line.starts_with("//") {
                    fn_line_idx += 1;
                } else {
                    break;
                }
            }

            // Parse function signature
            if fn_line_idx < lines.len() {
                let fn_line = lines[fn_line_idx].trim();
                if fn_line.starts_with("fn op_fresh_") {
                    if let Some(op_info) = parse_fn_signature(fn_line, has_string_return, &lines[fn_line_idx..]) {
                        ops.push(op_info);
                    }
                }
            }
        }
        i += 1;
    }

    ops
}

/// Parse a function signature to extract op info
fn parse_fn_signature(line: &str, has_string_return: bool, remaining_lines: &[&str]) -> Option<OpInfo> {
    // Extract function name
    let fn_start = line.find("fn ")? + 3;
    let paren_start = line.find('(')?;
    let fn_name = &line[fn_start..paren_start];

    if !fn_name.starts_with("op_fresh_") {
        return None;
    }

    let js_name = op_name_to_js(fn_name);

    // Find the full parameter list (may span multiple lines)
    let mut full_sig = String::new();
    for l in remaining_lines {
        full_sig.push_str(l.trim());
        full_sig.push(' ');
        if l.contains('{') || (l.contains(')') && (l.contains("->") || l.trim().ends_with('{'))) {
            break;
        }
    }

    // Extract parameters between ( and )
    let params_start = full_sig.find('(')? + 1;
    let params_end = full_sig.find(')')?;
    let params_str = &full_sig[params_start..params_end];

    // Parse parameters
    let mut params = Vec::new();
    let mut depth = 0;
    let mut current = String::new();

    for ch in params_str.chars() {
        match ch {
            '<' | '[' | '(' => {
                depth += 1;
                current.push(ch);
            }
            '>' | ']' | ')' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                if !current.trim().is_empty() {
                    if let Some(param) = parse_param(current.trim()) {
                        params.push(param);
                    }
                }
                current.clear();
            }
            _ => current.push(ch),
        }
    }
    if !current.trim().is_empty() {
        if let Some(param) = parse_param(current.trim()) {
            params.push(param);
        }
    }

    // Extract return type
    let return_type = if has_string_return {
        "string".to_string()
    } else if let Some(arrow_pos) = full_sig.find("->") {
        let ret_start = arrow_pos + 2;
        let ret_end = full_sig[ret_start..].find('{').map(|p| ret_start + p).unwrap_or(full_sig.len());
        let rust_ret = full_sig[ret_start..ret_end].trim();
        rust_type_to_ts(rust_ret)
    } else {
        "void".to_string()
    };

    Some(OpInfo {
        js_name,
        params,
        return_type,
    })
}

/// Parse a single parameter
fn parse_param(param_str: &str) -> Option<(String, String)> {
    let param_str = param_str.trim();

    // Skip state parameter
    if param_str.contains("OpState") || param_str.starts_with("state:") {
        return None;
    }

    // Check for #[string] attribute
    let is_string = param_str.contains("#[string]");
    let clean_param = param_str.replace("#[string]", "").trim().to_string();

    // Parse name: type
    let parts: Vec<&str> = clean_param.splitn(2, ':').collect();
    if parts.len() != 2 {
        return None;
    }

    let name = parts[0].trim().to_string();
    let ts_type = if is_string {
        "string".to_string()
    } else {
        rust_type_to_ts(parts[1].trim())
    };

    Some((name, ts_type))
}

/// Generate the TypeScript definition file
fn generate_typescript_types() -> Result<(), Box<dyn std::error::Error>> {
    let rust_source = fs::read_to_string("src/ts_runtime.rs")?;
    let ops = extract_ops(&rust_source);

    // Categorize ops
    let mut categories: HashMap<&str, Vec<&OpInfo>> = HashMap::new();
    categories.insert("status", Vec::new());
    categories.insert("query", Vec::new());
    categories.insert("mutation", Vec::new());
    categories.insert("overlay", Vec::new());

    for op in &ops {
        let category = if op.js_name == "setStatus" || op.js_name == "debug" {
            "status"
        } else if op.js_name.starts_with("get") || op.js_name.starts_with("is") {
            "query"
        } else if op.js_name.contains("Overlay") || op.js_name.contains("overlay") {
            "overlay"
        } else {
            "mutation"
        };
        categories.get_mut(category).unwrap().push(op);
    }

    // Generate TypeScript
    let mut output = String::new();
    output.push_str(
        r#"/**
 * Fresh Editor TypeScript Plugin API
 *
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generated from src/ts_runtime.rs by build.rs
 *
 * This file provides type definitions for the Fresh editor's TypeScript plugin system.
 * Plugins have access to the global `editor` object which provides methods to:
 * - Query editor state (buffers, cursors, viewports)
 * - Modify buffer content (insert, delete text)
 * - Add visual decorations (overlays, highlighting)
 * - Interact with the editor UI (status messages, prompts)
 */

declare global {
  /**
   * Global editor API object available to all TypeScript plugins
   */
  const editor: EditorAPI;
}

/**
 * Buffer identifier (unique numeric ID)
 */
type BufferId = number;

/**
 * Main editor API interface
 */
interface EditorAPI {
"#,
    );

    // Add status ops
    output.push_str("  // === Status and Logging ===\n");
    for op in &categories["status"] {
        output.push_str(&format_method(op));
    }

    // Add query ops
    output.push_str("\n  // === Buffer Queries ===\n");
    for op in &categories["query"] {
        output.push_str(&format_method(op));
    }

    // Add mutation ops
    output.push_str("\n  // === Buffer Mutations ===\n");
    for op in &categories["mutation"] {
        output.push_str(&format_method(op));
    }

    // Add overlay ops
    output.push_str("\n  // === Overlay Operations ===\n");
    for op in &categories["overlay"] {
        output.push_str(&format_method(op));
    }

    output.push_str(
        r#"}

// Export for module compatibility
export {};
"#,
    );

    // Ensure types directory exists
    let types_dir = Path::new("types");
    if !types_dir.exists() {
        fs::create_dir_all(types_dir)?;
    }

    // Write output
    fs::write("types/fresh.d.ts", output)?;

    println!("cargo::warning=Generated types/fresh.d.ts with {} ops", ops.len());

    Ok(())
}

fn format_method(op: &OpInfo) -> String {
    let params: Vec<String> = op.params.iter().map(|(name, ty)| format!("{}: {}", name, ty)).collect();
    format!("  {}({}): {};\n", op.js_name, params.join(", "), op.return_type)
}
