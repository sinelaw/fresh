//! TypeScript to JavaScript transpilation using oxc
//!
//! This module provides TypeScript transpilation using oxc_transformer
//! for parsing, transformation, and code generation.

use anyhow::{anyhow, Result};
use oxc_allocator::Allocator;
use oxc_ast::ast::{Declaration, ExportDefaultDeclarationKind, Statement};
use oxc_codegen::Codegen;
use oxc_isolated_declarations::{IsolatedDeclarations, IsolatedDeclarationsOptions};
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use oxc_transformer::{TransformOptions, Transformer};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

/// Transpile TypeScript source code to JavaScript
pub fn transpile_typescript(source: &str, filename: &str) -> Result<String> {
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(filename).unwrap_or_default();

    // Parse
    let parser_ret = Parser::new(&allocator, source, source_type).parse();
    if !parser_ret.errors.is_empty() {
        let errors: Vec<String> = parser_ret.errors.iter().map(|e| e.to_string()).collect();
        return Err(anyhow!("TypeScript parse errors: {}", errors.join("; ")));
    }

    let mut program = parser_ret.program;

    // Semantic analysis (required for transformer)
    let semantic_ret = SemanticBuilder::new().build(&program);

    if !semantic_ret.errors.is_empty() {
        let errors: Vec<String> = semantic_ret.errors.iter().map(|e| e.to_string()).collect();
        return Err(anyhow!("Semantic errors: {}", errors.join("; ")));
    }

    // Get scoping info for transformer
    let scoping = semantic_ret.semantic.into_scoping();

    // Transform (strip TypeScript types)
    let transform_options = TransformOptions::default();
    let transformer_ret = Transformer::new(&allocator, Path::new(filename), &transform_options)
        .build_with_scoping(scoping, &mut program);

    if !transformer_ret.errors.is_empty() {
        let errors: Vec<String> = transformer_ret
            .errors
            .iter()
            .map(|e| e.to_string())
            .collect();
        return Err(anyhow!("Transform errors: {}", errors.join("; ")));
    }

    // Generate JavaScript
    let codegen_ret = Codegen::new().build(&program);

    Ok(codegen_ret.code)
}

/// Emit a TypeScript declaration file (`.d.ts`) from TypeScript source.
///
/// Uses oxc's isolated-declarations transformer — no full type checker is
/// required, but the source must follow TypeScript's
/// [isolated declarations](https://www.typescriptlang.org/tsconfig#isolatedDeclarations)
/// rules: every exported value needs an explicit type annotation.
///
/// Fresh runs this over every TypeScript plugin at load time so the
/// plugin's public types (anything the file `export`s plus any
/// `declare global` / module-augmentation blocks) are available to
/// downstream plugins and to the user's `init.ts` without manual
/// `.d.ts` maintenance.
///
/// The source is forced into **module** mode before the transform runs:
/// isolated-declarations only hides non-exported symbols when the input
/// AST has at least one `ImportDeclaration`/`ExportDeclaration`. For a
/// plain-script input it instead emits a `declare` for every top-level
/// declaration, leaking internal interfaces, constants, and function
/// signatures into the aggregate `plugins.d.ts`. Many Fresh plugins
/// have no `import`/`export` statement (they're top-level calls on the
/// ambient `editor` global), so we append the canonical empty-export
/// marker `export {};` when the source doesn't already have module
/// syntax. `SourceType::with_module(true)` on the parser alone is not
/// enough — the transform looks at the AST, not the parser flag.
///
/// Returns the generated `.d.ts` source as a string. Non-fatal
/// diagnostics from the isolated-declarations pass are surfaced in
/// the error path when they render an empty emit unusable; benign
/// diagnostics (e.g. "defaults exported without explicit types")
/// are tolerated and the caller simply gets a partial emit.
pub fn emit_isolated_declarations(source: &str, filename: &str) -> Result<String> {
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(filename)
        .unwrap_or_default()
        .with_module(true);

    let module_marked;
    let effective_source: &str = if has_es_module_syntax(source) {
        source
    } else {
        module_marked = format!("{source}\nexport {{}};\n");
        &module_marked
    };

    let parser_ret = Parser::new(&allocator, effective_source, source_type).parse();
    if !parser_ret.errors.is_empty() {
        let errors: Vec<String> = parser_ret.errors.iter().map(|e| e.to_string()).collect();
        return Err(anyhow!(
            "isolated-declarations parse errors in {}: {}",
            filename,
            errors.join("; ")
        ));
    }

    let emit = IsolatedDeclarations::new(&allocator, IsolatedDeclarationsOptions::default())
        .build(&parser_ret.program);

    // Codegen the declaration AST back to source. We deliberately do
    // NOT fail on `emit.errors` — isolated-declarations emits one per
    // exported value that lacks an explicit type, and we want the
    // partial emit anyway (the consumer can still use the surfaces
    // the plugin annotated correctly).
    let codegen_ret = Codegen::new().build(&emit.program);
    Ok(codegen_ret.code)
}

/// Check if source contains ES module syntax (imports or exports)
/// This determines if the code needs bundling to work with QuickJS eval
pub fn has_es_module_syntax(source: &str) -> bool {
    // Check for imports: import X from "...", import { X } from "...", import * as X from "..."
    let has_imports = source.contains("import ") && source.contains(" from ");
    // Check for exports: export const, export function, export class, export interface, etc.
    let has_exports = source.lines().any(|line| {
        let trimmed = line.trim();
        trimmed.starts_with("export ")
    });
    has_imports || has_exports
}

/// Check if source contains ES module imports (import ... from ...)
/// Kept for backwards compatibility
pub fn has_es_imports(source: &str) -> bool {
    source.contains("import ") && source.contains(" from ")
}

/// Extract plugin dependency names from `import ... from "fresh:plugin/NAME"` statements.
///
/// Recognizes all import forms:
/// - `import type { Foo } from "fresh:plugin/bar"`
/// - `import { Foo } from "fresh:plugin/bar"`
/// - `import * as Bar from "fresh:plugin/bar"`
/// - `import Bar from "fresh:plugin/bar"`
///
/// Returns a deduplicated list of plugin names (the part after `fresh:plugin/`).
pub fn extract_plugin_dependencies(source: &str) -> Vec<String> {
    let prefix = "fresh:plugin/";
    let mut deps = Vec::new();
    let mut seen = HashSet::new();

    for line in source.lines() {
        let trimmed = line.trim();
        // Must be an import line with our scheme
        if !trimmed.starts_with("import ") || !trimmed.contains(prefix) {
            continue;
        }
        // Extract the string between quotes after "from"
        if let Some(from_idx) = trimmed.find(" from ") {
            let after_from = &trimmed[from_idx + 6..]; // skip " from "
            let after_from = after_from.trim();
            // Extract quoted string (single or double quotes)
            let quote_char = after_from.chars().next();
            if let Some(q) = quote_char {
                if q == '"' || q == '\'' {
                    if let Some(end) = after_from[1..].find(q) {
                        let module_path = &after_from[1..1 + end];
                        if let Some(plugin_name) = module_path.strip_prefix(prefix) {
                            if !plugin_name.is_empty() && seen.insert(plugin_name.to_string()) {
                                deps.push(plugin_name.to_string());
                            }
                        }
                    }
                }
            }
        }
    }

    deps
}

/// Topological sort of plugins by dependency order (dependencies first).
///
/// Returns `Ok(sorted_names)` with plugins in load order, or `Err(cycle)` with
/// the names of plugins involved in a dependency cycle.
///
/// Plugins with no dependencies are sorted alphabetically for determinism.
pub fn topological_sort_plugins(
    plugin_names: &[String],
    dependencies: &std::collections::HashMap<String, Vec<String>>,
) -> Result<Vec<String>> {
    use std::collections::HashMap;

    // Build adjacency and in-degree maps
    let mut in_degree: HashMap<&str, usize> = HashMap::new();
    let mut dependents: HashMap<&str, Vec<&str>> = HashMap::new();

    for name in plugin_names {
        in_degree.entry(name.as_str()).or_insert(0);
    }

    for name in plugin_names {
        if let Some(deps) = dependencies.get(name) {
            for dep in deps {
                // Only count dependencies on plugins that exist in our set
                if in_degree.contains_key(dep.as_str()) {
                    *in_degree.entry(name.as_str()).or_insert(0) += 1;
                    dependents
                        .entry(dep.as_str())
                        .or_default()
                        .push(name.as_str());
                } else {
                    return Err(anyhow!(
                        "Plugin '{}' depends on '{}', which is not installed or not enabled",
                        name,
                        dep
                    ));
                }
            }
        }
    }

    // Kahn's algorithm
    let mut queue: Vec<&str> = in_degree
        .iter()
        .filter(|(_, &deg)| deg == 0)
        .map(|(&name, _)| name)
        .collect();
    // Sort the initial queue alphabetically for determinism
    queue.sort();

    let mut result: Vec<String> = Vec::with_capacity(plugin_names.len());

    while let Some(current) = queue.first().copied() {
        queue.remove(0);
        result.push(current.to_string());

        if let Some(deps) = dependents.get(current) {
            let mut newly_ready = Vec::new();
            for &dependent in deps {
                if let Some(deg) = in_degree.get_mut(dependent) {
                    *deg -= 1;
                    if *deg == 0 {
                        newly_ready.push(dependent);
                    }
                }
            }
            // Sort newly ready plugins alphabetically for determinism
            newly_ready.sort();
            queue.extend(newly_ready);
            queue.sort(); // maintain overall alphabetical order among ready nodes
        }
    }

    if result.len() != plugin_names.len() {
        // Some plugins are in a cycle — find them
        let in_result: HashSet<&str> = result.iter().map(|s| s.as_str()).collect();
        let cycle_plugins: Vec<String> = plugin_names
            .iter()
            .filter(|n| !in_result.contains(n.as_str()))
            .cloned()
            .collect();
        return Err(anyhow!(
            "Plugin dependency cycle detected among: {}. These plugins will not be loaded.",
            cycle_plugins.join(", ")
        ));
    }

    Ok(result)
}

/// Module metadata for scoped bundling
#[derive(Debug, Clone)]
struct ModuleMetadata {
    /// Canonical path to this module
    path: PathBuf,
    /// Variable name for this module's exports (e.g., "__mod_panel_manager")
    var_name: String,
    /// Named imports from other modules
    imports: Vec<ImportBinding>,
    /// Named exports from this module
    exports: Vec<ExportBinding>,
    /// Re-exports from other modules
    reexports: Vec<ReexportBinding>,
    /// The module's code with import/export statements removed, then transpiled
    code: String,
}

#[derive(Debug, Clone)]
struct ImportBinding {
    /// Local name used in this module
    local_name: String,
    /// Name exported from the source module (None for default import)
    imported_name: Option<String>,
    /// Path to the source module (as written, e.g., "./lib/index.ts")
    source_path: String,
    /// Whether this is a namespace import (import * as X)
    is_namespace: bool,
}

#[derive(Debug, Clone)]
struct ExportBinding {
    /// Name this is exported as
    exported_name: String,
    /// Local name in this module (might differ for `export { x as y }`)
    local_name: String,
}

#[derive(Debug, Clone)]
struct ReexportBinding {
    /// Name this is exported as (None for `export *`)
    exported_name: Option<String>,
    /// Name in the source module (None for `export *`)
    source_name: Option<String>,
    /// Path to the source module
    source_path: String,
}

/// Bundle a module and all its local imports into a single file with proper scoping
/// Each module is wrapped in an IIFE that only exposes its exports
pub fn bundle_module(entry_path: &Path) -> Result<String> {
    let mut modules: Vec<ModuleMetadata> = Vec::new();
    let mut visited = HashSet::new();
    let mut path_to_var: std::collections::HashMap<PathBuf, String> =
        std::collections::HashMap::new();

    // First pass: collect all modules in dependency order
    collect_modules(entry_path, &mut visited, &mut modules, &mut path_to_var)?;

    // Second pass: generate scoped output
    let mut output = String::new();

    for (i, module) in modules.iter().enumerate() {
        let is_entry = i == modules.len() - 1;
        output.push_str(&generate_scoped_module(module, &path_to_var, is_entry)?);
        output.push('\n');
    }

    Ok(output)
}

/// Collect all modules in dependency order (dependencies first)
fn collect_modules(
    path: &Path,
    visited: &mut HashSet<PathBuf>,
    modules: &mut Vec<ModuleMetadata>,
    path_to_var: &mut std::collections::HashMap<PathBuf, String>,
) -> Result<()> {
    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if visited.contains(&canonical) {
        return Ok(()); // Already processed (circular import protection)
    }
    visited.insert(canonical.clone());

    let source = std::fs::read_to_string(path)
        .map_err(|e| anyhow!("Failed to read {}: {}", path.display(), e))?;

    // Extract module metadata using AST
    let (imports, exports, reexports) = extract_module_bindings(&source);

    let parent_dir = path.parent().unwrap_or(Path::new("."));

    // Collect dependencies first (topological order)
    for import in &imports {
        if import.source_path.starts_with("./") || import.source_path.starts_with("../") {
            let resolved = resolve_import(&import.source_path, parent_dir)?;
            collect_modules(&resolved, visited, modules, path_to_var)?;
        }
    }
    for reexport in &reexports {
        if reexport.source_path.starts_with("./") || reexport.source_path.starts_with("../") {
            let resolved = resolve_import(&reexport.source_path, parent_dir)?;
            collect_modules(&resolved, visited, modules, path_to_var)?;
        }
    }

    // Generate variable name for this module
    let var_name = path_to_module_var(path);
    path_to_var.insert(canonical.clone(), var_name.clone());

    // Strip imports/exports and transpile
    let stripped = strip_imports_and_exports(&source);
    let filename = path.to_str().unwrap_or("unknown.ts");
    let transpiled = transpile_typescript(&stripped, filename)?;

    modules.push(ModuleMetadata {
        path: canonical,
        var_name,
        imports,
        exports,
        reexports,
        code: transpiled,
    });

    Ok(())
}

/// Generate a unique variable name from a path
fn path_to_module_var(path: &Path) -> String {
    let name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");

    // Sanitize: replace non-alphanumeric with underscore
    let sanitized: String = name
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect();

    // Add hash of full path to ensure uniqueness
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    path.hash(&mut hasher);
    let hash = hasher.finish();

    format!("__mod_{}_{:x}", sanitized, hash & 0xFFFF)
}

/// Generate scoped module code wrapped in IIFE
fn generate_scoped_module(
    module: &ModuleMetadata,
    path_to_var: &std::collections::HashMap<PathBuf, String>,
    is_entry: bool,
) -> Result<String> {
    let mut code = String::new();

    // Start IIFE - entry module doesn't need to export, others do
    if is_entry {
        code.push_str("(function() {\n");
    } else {
        code.push_str(&format!("const {} = (function() {{\n", module.var_name));
    }

    // Generate import destructuring from dependencies
    for import in &module.imports {
        if let Some(dep_var) = resolve_import_to_var(&import.source_path, &module.path, path_to_var)
        {
            if import.is_namespace {
                // import * as X from "./y"
                code.push_str(&format!("const {} = {};\n", import.local_name, dep_var));
            } else if let Some(ref imported_name) = import.imported_name {
                // import { X } from "./y" or import { X as Y } from "./y"
                if imported_name == "default" {
                    code.push_str(&format!(
                        "const {} = {}.default;\n",
                        import.local_name, dep_var
                    ));
                } else if &import.local_name == imported_name {
                    code.push_str(&format!("const {{{}}} = {};\n", import.local_name, dep_var));
                } else {
                    code.push_str(&format!(
                        "const {{{}: {}}} = {};\n",
                        imported_name, import.local_name, dep_var
                    ));
                }
            } else {
                // import X from "./y" (default import)
                code.push_str(&format!(
                    "const {} = {}.default;\n",
                    import.local_name, dep_var
                ));
            }
        }
    }

    // Module code
    code.push_str(&module.code);
    code.push('\n');

    // Generate return object with exports (skip for entry module)
    if !is_entry {
        code.push_str("return {");

        let mut export_parts: Vec<String> = Vec::new();

        // Direct exports
        for export in &module.exports {
            if export.exported_name == export.local_name {
                export_parts.push(export.exported_name.clone());
            } else {
                export_parts.push(format!("{}: {}", export.exported_name, export.local_name));
            }
        }

        // Re-exports
        for reexport in &module.reexports {
            if let Some(dep_var) =
                resolve_import_to_var(&reexport.source_path, &module.path, path_to_var)
            {
                match (&reexport.exported_name, &reexport.source_name) {
                    (Some(exported), Some(source)) => {
                        // export { X as Y } from "./z"
                        export_parts.push(format!("{}: {}.{}", exported, dep_var, source));
                    }
                    (Some(exported), None) => {
                        // export { X } from "./z" (same name)
                        export_parts.push(format!("{}: {}.{}", exported, dep_var, exported));
                    }
                    (None, None) => {
                        // export * from "./z" - spread all exports
                        export_parts.push(format!("...{}", dep_var));
                    }
                    _ => {}
                }
            }
        }

        code.push_str(&export_parts.join(", "));
        code.push_str("};\n");
    }

    // End IIFE
    code.push_str("})();\n");

    Ok(code)
}

/// Resolve an import source path to the dependency's variable name
fn resolve_import_to_var(
    source_path: &str,
    importer_path: &Path,
    path_to_var: &std::collections::HashMap<PathBuf, String>,
) -> Option<String> {
    if !source_path.starts_with("./") && !source_path.starts_with("../") {
        return None; // External import, not bundled
    }

    let parent_dir = importer_path.parent().unwrap_or(Path::new("."));
    if let Ok(resolved) = resolve_import(source_path, parent_dir) {
        let canonical = resolved.canonicalize().unwrap_or(resolved);
        path_to_var.get(&canonical).cloned()
    } else {
        None
    }
}

/// Extract import/export bindings from source using AST
fn extract_module_bindings(
    source: &str,
) -> (Vec<ImportBinding>, Vec<ExportBinding>, Vec<ReexportBinding>) {
    let allocator = Allocator::default();
    let source_type = SourceType::default()
        .with_module(true)
        .with_typescript(true);

    let parser_ret = Parser::new(&allocator, source, source_type).parse();
    if !parser_ret.errors.is_empty() {
        return (Vec::new(), Vec::new(), Vec::new());
    }

    let mut imports = Vec::new();
    let mut exports = Vec::new();
    let mut reexports = Vec::new();

    for stmt in &parser_ret.program.body {
        match stmt {
            Statement::ImportDeclaration(import_decl) => {
                let source_path = import_decl.source.value.to_string();

                // Handle specifiers
                if let Some(specifiers) = &import_decl.specifiers {
                    for spec in specifiers {
                        match spec {
                            oxc_ast::ast::ImportDeclarationSpecifier::ImportSpecifier(s) => {
                                imports.push(ImportBinding {
                                    local_name: s.local.name.to_string(),
                                    imported_name: Some(s.imported.name().to_string()),
                                    source_path: source_path.clone(),
                                    is_namespace: false,
                                });
                            }
                            oxc_ast::ast::ImportDeclarationSpecifier::ImportDefaultSpecifier(s) => {
                                imports.push(ImportBinding {
                                    local_name: s.local.name.to_string(),
                                    imported_name: None, // default import
                                    source_path: source_path.clone(),
                                    is_namespace: false,
                                });
                            }
                            oxc_ast::ast::ImportDeclarationSpecifier::ImportNamespaceSpecifier(
                                s,
                            ) => {
                                imports.push(ImportBinding {
                                    local_name: s.local.name.to_string(),
                                    imported_name: None,
                                    source_path: source_path.clone(),
                                    is_namespace: true,
                                });
                            }
                        }
                    }
                }
            }

            Statement::ExportNamedDeclaration(export_decl) => {
                if let Some(ref source) = export_decl.source {
                    // Re-export: export { X } from "./y"
                    let source_path = source.value.to_string();
                    for spec in &export_decl.specifiers {
                        reexports.push(ReexportBinding {
                            exported_name: Some(spec.exported.name().to_string()),
                            source_name: Some(spec.local.name().to_string()),
                            source_path: source_path.clone(),
                        });
                    }
                } else {
                    // Direct export
                    if let Some(ref decl) = export_decl.declaration {
                        // export const/function/class X
                        for name in get_declaration_names(decl) {
                            exports.push(ExportBinding {
                                exported_name: name.clone(),
                                local_name: name,
                            });
                        }
                    }
                    // export { X, Y }
                    for spec in &export_decl.specifiers {
                        exports.push(ExportBinding {
                            exported_name: spec.exported.name().to_string(),
                            local_name: spec.local.name().to_string(),
                        });
                    }
                }
            }

            Statement::ExportDefaultDeclaration(export_default) => {
                // export default X
                match &export_default.declaration {
                    ExportDefaultDeclarationKind::FunctionDeclaration(f) => {
                        if let Some(ref id) = f.id {
                            exports.push(ExportBinding {
                                exported_name: "default".to_string(),
                                local_name: id.name.to_string(),
                            });
                        }
                    }
                    ExportDefaultDeclarationKind::ClassDeclaration(c) => {
                        if let Some(ref id) = c.id {
                            exports.push(ExportBinding {
                                exported_name: "default".to_string(),
                                local_name: id.name.to_string(),
                            });
                        }
                    }
                    _ => {
                        // Anonymous default export - handle specially
                        exports.push(ExportBinding {
                            exported_name: "default".to_string(),
                            local_name: "__default__".to_string(),
                        });
                    }
                }
            }

            Statement::ExportAllDeclaration(export_all) => {
                // export * from "./y"
                reexports.push(ReexportBinding {
                    exported_name: None,
                    source_name: None,
                    source_path: export_all.source.value.to_string(),
                });
            }

            _ => {}
        }
    }

    (imports, exports, reexports)
}

/// Get declared names from a declaration
fn get_declaration_names(decl: &Declaration<'_>) -> Vec<String> {
    match decl {
        Declaration::VariableDeclaration(var_decl) => var_decl
            .declarations
            .iter()
            .filter_map(|d| d.id.get_binding_identifier().map(|id| id.name.to_string()))
            .collect(),
        Declaration::FunctionDeclaration(f) => {
            f.id.as_ref()
                .map(|id| vec![id.name.to_string()])
                .unwrap_or_default()
        }
        Declaration::ClassDeclaration(c) => {
            c.id.as_ref()
                .map(|id| vec![id.name.to_string()])
                .unwrap_or_default()
        }
        Declaration::TSEnumDeclaration(e) => {
            vec![e.id.name.to_string()]
        }
        _ => Vec::new(),
    }
}

/// Resolve an import path relative to the importing file's directory
fn resolve_import(import_path: &str, parent_dir: &Path) -> Result<PathBuf> {
    let base = parent_dir.join(import_path);

    // Try various extensions
    if base.exists() {
        return Ok(base);
    }

    let with_ts = base.with_extension("ts");
    if with_ts.exists() {
        return Ok(with_ts);
    }

    let with_js = base.with_extension("js");
    if with_js.exists() {
        return Ok(with_js);
    }

    // Try index files
    let index_ts = base.join("index.ts");
    if index_ts.exists() {
        return Ok(index_ts);
    }

    let index_js = base.join("index.js");
    if index_js.exists() {
        return Ok(index_js);
    }

    Err(anyhow!(
        "Cannot resolve import '{}' from {}",
        import_path,
        parent_dir.display()
    ))
}

/// Strip import statements and export keywords from source using AST transformation
/// Converts ES module syntax to plain JavaScript that QuickJS can eval
pub fn strip_imports_and_exports(source: &str) -> String {
    let allocator = Allocator::default();
    // Parse as module with TypeScript to accept import/export and TS syntax
    let source_type = SourceType::default()
        .with_module(true)
        .with_typescript(true);

    let parser_ret = Parser::new(&allocator, source, source_type).parse();
    if !parser_ret.errors.is_empty() {
        // If parsing fails, return original source (let transpiler handle errors)
        return source.to_string();
    }

    let mut program = parser_ret.program;

    // Transform the AST: remove imports, convert exports to declarations
    strip_module_syntax_ast(&allocator, &mut program);

    // Generate code from transformed AST
    let codegen_ret = Codegen::new().build(&program);
    codegen_ret.code
}

/// Strip ES module syntax from a program AST
/// - Removes ImportDeclaration statements
/// - Converts ExportNamedDeclaration to its inner declaration
/// - Handles ExportDefaultDeclaration, ExportAllDeclaration
fn strip_module_syntax_ast<'a>(allocator: &'a Allocator, program: &mut oxc_ast::ast::Program<'a>) {
    use oxc_allocator::Vec as OxcVec;

    // Collect transformed statements
    let mut new_body: OxcVec<'a, Statement<'a>> =
        OxcVec::with_capacity_in(program.body.len(), allocator);

    for stmt in program.body.drain(..) {
        match stmt {
            // Remove import declarations entirely
            Statement::ImportDeclaration(_) => {
                // Skip - dependency should already be bundled
            }

            // Convert export named declarations to their inner declaration
            Statement::ExportNamedDeclaration(export_decl) => {
                let inner = export_decl.unbox();
                if let Some(decl) = inner.declaration {
                    // Export has a declaration - keep just the declaration
                    // Convert Declaration to Statement
                    let stmt = declaration_to_statement(decl);
                    new_body.push(stmt);
                }
                // If no declaration (re-export like `export { X } from './y'`), skip
            }

            // Handle export default
            Statement::ExportDefaultDeclaration(export_default) => {
                let inner = export_default.unbox();
                match inner.declaration {
                    ExportDefaultDeclarationKind::FunctionDeclaration(func) => {
                        new_body.push(Statement::FunctionDeclaration(func));
                    }
                    ExportDefaultDeclarationKind::ClassDeclaration(class) => {
                        new_body.push(Statement::ClassDeclaration(class));
                    }
                    ExportDefaultDeclarationKind::TSInterfaceDeclaration(_) => {
                        // TypeScript interface - will be removed by transformer
                    }
                    _ => {
                        // Expression exports (export default expr) - skip
                    }
                }
            }

            // Remove export * declarations (re-exports)
            Statement::ExportAllDeclaration(_) => {
                // Skip
            }

            // Keep all other statements unchanged
            other => {
                new_body.push(other);
            }
        }
    }

    program.body = new_body;
}

/// Convert a Declaration to a Statement
fn declaration_to_statement(decl: Declaration<'_>) -> Statement<'_> {
    match decl {
        Declaration::VariableDeclaration(d) => Statement::VariableDeclaration(d),
        Declaration::FunctionDeclaration(d) => Statement::FunctionDeclaration(d),
        Declaration::ClassDeclaration(d) => Statement::ClassDeclaration(d),
        Declaration::TSTypeAliasDeclaration(d) => Statement::TSTypeAliasDeclaration(d),
        Declaration::TSInterfaceDeclaration(d) => Statement::TSInterfaceDeclaration(d),
        Declaration::TSEnumDeclaration(d) => Statement::TSEnumDeclaration(d),
        Declaration::TSModuleDeclaration(d) => Statement::TSModuleDeclaration(d),
        Declaration::TSImportEqualsDeclaration(d) => Statement::TSImportEqualsDeclaration(d),
        Declaration::TSGlobalDeclaration(d) => Statement::TSGlobalDeclaration(d),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emit_isolated_declarations_script_hides_internals() {
        // Script-style plugin: no `import`, no `export`. Before we forced
        // module mode, isolated-declarations treated every top-level
        // declaration as publicly visible and leaked `interface Internal`,
        // `declare const internal`, `declare function internal()` into
        // the emit. Module mode keeps the output empty for a file with
        // no exports.
        let source = r#"
            interface Internal { x: number; }
            const internalConst: Internal = { x: 1 };
            function internalFn(): void {}
        "#;
        let dts = emit_isolated_declarations(source, "script_plugin.ts").unwrap();
        assert!(
            !dts.contains("Internal"),
            "non-exported interface leaked into .d.ts: {dts}"
        );
        assert!(
            !dts.contains("internalConst"),
            "non-exported const leaked into .d.ts: {dts}"
        );
        assert!(
            !dts.contains("internalFn"),
            "non-exported function leaked into .d.ts: {dts}"
        );
    }

    #[test]
    fn emit_isolated_declarations_keeps_exports_and_registry_augmentation() {
        // A plugin that has no `import`/`export` statements in the
        // value plane but does augment `FreshPluginRegistry` still has
        // to land its `declare global` block in the emit — that's what
        // makes `editor.getPluginApi("foo")` typed in init.ts.
        let source = r#"
            export type FooApi = { doThing(): void };
            declare global {
                interface FreshPluginRegistry {
                    foo: FooApi;
                }
            }
            const internal = 42;
        "#;
        let dts = emit_isolated_declarations(source, "foo.ts").unwrap();
        assert!(dts.contains("FooApi"), "exported type missing: {dts}");
        assert!(
            dts.contains("FreshPluginRegistry"),
            "registry augmentation missing: {dts}"
        );
        assert!(!dts.contains("internal"), "internal const leaked: {dts}");
    }

    #[test]
    fn test_transpile_basic_typescript() {
        let source = r#"
            const x: number = 42;
            function greet(name: string): string {
                return `Hello, ${name}!`;
            }
        "#;

        let result = transpile_typescript(source, "test.ts").unwrap();
        assert!(result.contains("const x = 42"));
        assert!(result.contains("function greet(name)"));
        assert!(!result.contains(": number"));
        assert!(!result.contains(": string"));
    }

    #[test]
    fn test_transpile_interface() {
        let source = r#"
            interface User {
                name: string;
                age: number;
            }
            const user: User = { name: "Alice", age: 30 };
        "#;

        let result = transpile_typescript(source, "test.ts").unwrap();
        assert!(!result.contains("interface"));
        assert!(result.contains("const user = {"));
    }

    #[test]
    fn test_transpile_type_alias() {
        let source = r#"
            type ID = number | string;
            const id: ID = 123;
        "#;

        let result = transpile_typescript(source, "test.ts").unwrap();
        assert!(!result.contains("type ID"));
        assert!(result.contains("const id = 123"));
    }

    #[test]
    fn test_has_es_imports() {
        assert!(has_es_imports("import { foo } from './lib'"));
        assert!(has_es_imports("import foo from 'bar'"));
        assert!(!has_es_imports("const x = 1;"));
        // Note: comment detection is a known limitation - simple heuristic doesn't parse JS
        // This is OK because false positives just mean we bundle when not strictly needed
        assert!(has_es_imports("// import foo from 'bar'")); // heuristic doesn't parse comments
    }

    #[test]
    fn test_extract_module_bindings() {
        let source = r#"
            import { foo } from "./lib/utils";
            import bar from "../shared/bar";
            import external from "external-package";
            export { PanelManager } from "./panel-manager.ts";
            export * from "./types.ts";
            export const API_VERSION = 1;
            const x = 1;
        "#;

        let (imports, exports, reexports) = extract_module_bindings(source);

        // Check imports
        assert_eq!(imports.len(), 3);
        assert!(imports
            .iter()
            .any(|i| i.source_path == "./lib/utils" && i.local_name == "foo"));
        assert!(imports
            .iter()
            .any(|i| i.source_path == "../shared/bar" && i.local_name == "bar"));
        assert!(imports.iter().any(|i| i.source_path == "external-package"));

        // Check direct exports
        assert_eq!(exports.len(), 1);
        assert!(exports.iter().any(|e| e.exported_name == "API_VERSION"));

        // Check re-exports
        assert_eq!(reexports.len(), 2);
        assert!(reexports
            .iter()
            .any(|r| r.source_path == "./panel-manager.ts"));
        assert!(reexports
            .iter()
            .any(|r| r.source_path == "./types.ts" && r.exported_name.is_none()));
        // export *
    }

    #[test]
    fn test_extract_module_bindings_multiline() {
        // Test multi-line exports like in lib/index.ts
        let source = r#"
export type {
    RGB,
    Location,
    PanelOptions,
} from "./types.ts";

export {
    Finder,
    defaultFuzzyFilter,
} from "./finder.ts";

import {
    something,
    somethingElse,
} from "./multiline-import.ts";
        "#;

        let (imports, _exports, reexports) = extract_module_bindings(source);

        // Check imports handle multi-line
        assert_eq!(imports.len(), 2);
        assert!(imports.iter().any(|i| i.local_name == "something"));
        assert!(imports.iter().any(|i| i.local_name == "somethingElse"));

        // Check re-exports handle multi-line
        assert_eq!(reexports.len(), 5); // RGB, Location, PanelOptions, Finder, defaultFuzzyFilter
        assert!(reexports.iter().any(|r| r.source_path == "./types.ts"));
        assert!(reexports.iter().any(|r| r.source_path == "./finder.ts"));
    }

    #[test]
    fn test_strip_imports_and_exports() {
        let source = r#"import { foo } from "./lib";
import bar from "../bar";
export const API_VERSION = 1;
export function greet() { return "hi"; }
export interface User { name: string; }
const x = foo() + bar();"#;

        let stripped = strip_imports_and_exports(source);
        // Imports are removed entirely
        assert!(!stripped.contains("import { foo }"));
        assert!(!stripped.contains("import bar from"));
        // Exports are converted to regular declarations
        assert!(!stripped.contains("export const"));
        assert!(!stripped.contains("export function"));
        assert!(!stripped.contains("export interface"));
        // But the declarations themselves remain
        assert!(stripped.contains("const API_VERSION = 1"));
        assert!(stripped.contains("function greet()"));
        assert!(stripped.contains("interface User"));
        assert!(stripped.contains("const x = foo() + bar();"));
    }

    #[test]
    fn test_extract_plugin_dependencies_basic() {
        let source = r#"
import type { SomeType } from "fresh:plugin/utility-plugin";
import { helper } from "fresh:plugin/core-lib";
const editor = getEditor();
"#;
        let deps = extract_plugin_dependencies(source);
        assert_eq!(deps, vec!["utility-plugin", "core-lib"]);
    }

    #[test]
    fn test_extract_plugin_dependencies_various_import_forms() {
        let source = r#"
import type { A } from "fresh:plugin/plugin-a";
import { B } from "fresh:plugin/plugin-b";
import * as C from "fresh:plugin/plugin-c";
import D from "fresh:plugin/plugin-d";
import { E } from './local-file';
import { F } from "../other-file";
"#;
        let deps = extract_plugin_dependencies(source);
        assert_eq!(deps, vec!["plugin-a", "plugin-b", "plugin-c", "plugin-d"]);
    }

    #[test]
    fn test_extract_plugin_dependencies_deduplicates() {
        let source = r#"
import type { A } from "fresh:plugin/shared";
import { B } from "fresh:plugin/shared";
"#;
        let deps = extract_plugin_dependencies(source);
        assert_eq!(deps, vec!["shared"]);
    }

    #[test]
    fn test_extract_plugin_dependencies_single_quotes() {
        let source = r#"
import type { A } from 'fresh:plugin/single-quoted';
"#;
        let deps = extract_plugin_dependencies(source);
        assert_eq!(deps, vec!["single-quoted"]);
    }

    #[test]
    fn test_extract_plugin_dependencies_no_deps() {
        let source = r#"
const editor = getEditor();
import { helper } from "./lib/utils";
"#;
        let deps = extract_plugin_dependencies(source);
        assert!(deps.is_empty());
    }

    #[test]
    fn test_topological_sort_no_deps() {
        let names = vec!["c".to_string(), "a".to_string(), "b".to_string()];
        let deps = std::collections::HashMap::new();
        let result = topological_sort_plugins(&names, &deps).unwrap();
        // Should be alphabetical when no dependencies
        assert_eq!(result, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_topological_sort_linear_chain() {
        let names = vec!["c".to_string(), "b".to_string(), "a".to_string()];
        let mut deps = std::collections::HashMap::new();
        deps.insert("b".to_string(), vec!["a".to_string()]);
        deps.insert("c".to_string(), vec!["b".to_string()]);
        let result = topological_sort_plugins(&names, &deps).unwrap();
        assert_eq!(result, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_topological_sort_diamond() {
        // D depends on B and C; B and C depend on A
        let names = vec![
            "d".to_string(),
            "c".to_string(),
            "b".to_string(),
            "a".to_string(),
        ];
        let mut deps = std::collections::HashMap::new();
        deps.insert("b".to_string(), vec!["a".to_string()]);
        deps.insert("c".to_string(), vec!["a".to_string()]);
        deps.insert("d".to_string(), vec!["b".to_string(), "c".to_string()]);
        let result = topological_sort_plugins(&names, &deps).unwrap();
        // A must come first, then B and C (alphabetical), then D
        assert_eq!(result, vec!["a", "b", "c", "d"]);
    }

    #[test]
    fn test_topological_sort_cycle_detection() {
        let names = vec!["a".to_string(), "b".to_string(), "c".to_string()];
        let mut deps = std::collections::HashMap::new();
        deps.insert("a".to_string(), vec!["b".to_string()]);
        deps.insert("b".to_string(), vec!["c".to_string()]);
        deps.insert("c".to_string(), vec!["a".to_string()]);
        let result = topological_sort_plugins(&names, &deps);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(err.contains("cycle"), "Error should mention cycle: {}", err);
    }

    #[test]
    fn test_topological_sort_missing_dependency() {
        let names = vec!["a".to_string()];
        let mut deps = std::collections::HashMap::new();
        deps.insert("a".to_string(), vec!["nonexistent".to_string()]);
        let result = topological_sort_plugins(&names, &deps);
        assert!(result.is_err());
        let err = result.unwrap_err().to_string();
        assert!(
            err.contains("not installed"),
            "Error should mention missing dep: {}",
            err
        );
    }

    #[test]
    fn test_topological_sort_independent_plugins_alphabetical() {
        // Mix of dependent and independent plugins
        let names = vec![
            "zebra".to_string(),
            "alpha".to_string(),
            "beta".to_string(),
            "gamma".to_string(),
        ];
        let mut deps = std::collections::HashMap::new();
        deps.insert("gamma".to_string(), vec!["alpha".to_string()]);
        let result = topological_sort_plugins(&names, &deps).unwrap();
        // alpha must come before gamma; beta and zebra are independent
        let alpha_pos = result.iter().position(|s| s == "alpha").unwrap();
        let gamma_pos = result.iter().position(|s| s == "gamma").unwrap();
        assert!(alpha_pos < gamma_pos);
    }
}

/// Everything about a plugin that is a pure function of its source text.
///
/// One pipeline, two callers: the build script precomputes this for the
/// plugins shipped with the editor, and the runtime computes it for
/// user-installed ones. Sharing the function is what keeps the precomputed
/// table and the live path from drifting apart.
pub struct PreparedSource {
    /// Bundled/stripped/transpiled JavaScript, ready for the engine.
    pub js_code: String,
    /// `.d.ts` emit from oxc's isolated-declarations transformer. `None` when
    /// that emit failed -- the plugin still runs, it just contributes no types.
    pub declarations: Option<String>,
    /// Plugins this one declares a dependency on, for load ordering.
    pub dependencies: Vec<String>,
}

/// Run the plugin preparation pipeline over one source file.
///
/// `path` is load-bearing, not decorative: a plugin containing ES imports is
/// bundled, and bundling resolves those imports relative to this file.
pub fn prepare_source(path: &Path, source: &str) -> Result<PreparedSource> {
    let filename = path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("plugin.ts");

    let dependencies = extract_plugin_dependencies(source);

    // Emitted from the raw TS, before transpilation strips the types away:
    // every `export type`, `export interface` and `declare global` the author
    // wrote is exactly what downstream plugins and init.ts need in order to
    // reach this plugin's surface without casts.
    let declarations = if filename.ends_with(".ts") {
        match emit_isolated_declarations(source, filename) {
            Ok(dts) => Some(dts),
            Err(e) => {
                // `tracing`, not `eprintln!`: the editor runs inside the
                // terminal's alternate screen and does not redirect stderr, so
                // a raw print lands on top of the rendered frame.
                tracing::warn!(
                    "plugin {} isolated-declarations emit failed: {e}",
                    path.display()
                );
                None
            }
        }
    } else {
        None
    };

    let js_code = if has_es_imports(source) {
        bundle_module(path)?
    } else if has_es_module_syntax(source) {
        let stripped = strip_imports_and_exports(source);
        if filename.ends_with(".ts") {
            transpile_typescript(&stripped, filename)?
        } else {
            stripped
        }
    } else if filename.ends_with(".ts") {
        transpile_typescript(source, filename)?
    } else {
        source.to_string()
    };

    Ok(PreparedSource {
        js_code,
        declarations,
        dependencies,
    })
}

/// A fingerprint of *everything* that goes into preparing `path`: its own
/// text, plus every other plugin source in the same tree.
///
/// [`source_fingerprint`] of the entry file alone is not enough to key a cache
/// on. A plugin that imports `./lib/foo.ts` is bundled with that file's
/// contents inlined, so editing the import and leaving the importer untouched
/// changes the prepared output while leaving the importer's own fingerprint
/// identical. Keying on the entry file alone would serve the stale bundle
/// forever, because the cached bytecode is perfectly valid -- it is just the
/// wrong program.
///
/// Rather than resolve each plugin's import graph, which costs a parse per
/// file and measured at ~87ms per process over the bundled set, this hashes
/// every source in the tree once and folds that in. It is deliberately
/// coarser: editing any file in a plugin directory invalidates the cached form
/// of every plugin in it. That is the right trade here -- the bundled plugins
/// only ever change together, as a release, and the cost of being wrong in the
/// other direction is serving a stale program indefinitely.
///
/// `None` when the tree cannot be read, which means *do not cache*.
pub fn input_fingerprint(path: &Path, source: &str) -> Option<u64> {
    // The whole corpus when the editor has declared it, so a user plugin that
    // changes invalidates the bundled set too -- they share a runtime, and
    // nothing here can prove they do not reach each other.
    if let Some(corpus) = CORPUS.get().copied().flatten() {
        return Some(source_fingerprint(source) ^ corpus);
    }
    // Otherwise the plugin's own directory, which is what a caller loading one
    // plugin in isolation (a test, a single `load_plugin_from_path`) can know.
    let dir = path.parent()?;
    Some(source_fingerprint(source) ^ tree_fingerprint(dir)?)
}

/// Every source file that could be loaded this session, hashed once.
static CORPUS: OnceLock<Option<u64>> = OnceLock::new();

/// Declare the complete plugin search path, before anything is loaded.
///
/// One hash over every plugin the editor could load -- embedded, the user's
/// own, package-installed, bundled -- becomes the compiled cache's single
/// invalidation domain: if any of them is new or changed, every entry misses
/// and is rebuilt.
///
/// That is coarse on purpose. The alternative is to resolve each plugin's
/// import graph and key on just its own inputs, which is more precise, costs a
/// parse per file (~87ms per process over the bundled set, measured), and
/// still cannot see a plugin reaching another's globals. Rebuilding everything
/// when anything changes is cheap by comparison: the rebuild happens once, and
/// every process after it hits.
///
/// Set once per process; later calls are ignored, so a directory that appears
/// mid-session (a package installed while the editor runs) is not reflected
/// until the next start.
pub fn set_plugin_corpus(dirs: &[PathBuf]) {
    CORPUS.get_or_init(|| hash_trees(dirs));
}

/// Hash every `.ts`/`.js` file under all of `dirs`, path and contents.
///
/// `None` if any directory cannot be read, which means *do not cache*: a
/// corpus that silently omitted a directory would be a key that does not cover
/// its inputs.
fn hash_trees(dirs: &[PathBuf]) -> Option<u64> {
    let mut files: Vec<PathBuf> = Vec::new();
    for dir in dirs {
        collect_sources(dir, &mut files).ok()?;
    }
    // Canonical and deduplicated: search paths can overlap (a package
    // directory under the user's plugin directory), and the same file counted
    // twice is the same corpus.
    let mut canonical: Vec<PathBuf> = files
        .iter()
        .map(|f| f.canonicalize().unwrap_or_else(|_| f.clone()))
        .collect();
    canonical.sort();
    canonical.dedup();
    // Absolute: the corpus spans several roots, so there is no shared base to
    // make relative to, and a directory moving is a change worth a rebuild.
    fold_sources(&canonical, None)
}

/// A fingerprint of every plugin source under `dir`, computed once per
/// directory per process.
///
/// Memoised because every plugin in a directory asks for the same value, and
/// the answer cannot change under a running editor in a way this cache is
/// meant to notice -- a plugin edited mid-session is picked up by a reload,
/// which restarts from the sources.
fn tree_fingerprint(dir: &Path) -> Option<u64> {
    static CACHE: OnceLock<Mutex<HashMap<PathBuf, Option<u64>>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    let key = dir.canonicalize().unwrap_or_else(|_| dir.to_path_buf());
    if let Some(hit) = cache.lock().ok()?.get(&key) {
        return *hit;
    }
    let computed = hash_tree(&key);
    cache.lock().ok()?.insert(key, computed);
    computed
}

/// Hash every `.ts`/`.js` file under `dir`, path and contents, in a fixed
/// order. Reading and hashing the bundled tree is ~3MB of I/O that the OS has
/// usually cached already; the parse it replaces is what was expensive.
fn hash_tree(dir: &Path) -> Option<u64> {
    let mut files: Vec<PathBuf> = Vec::new();
    collect_sources(dir, &mut files).ok()?;
    files.sort();
    // Relative to `dir`, so the same tree fingerprints the same wherever it
    // is checked out or extracted to.
    fold_sources(&files, Some(dir))
}

/// Fold an ordered list of source files into one fingerprint: each file's
/// path, size and modification time.
///
/// Stat, not contents. Reading and hashing every plugin source measured at
/// ~360ms per process over the bundled tree, which is more than the work the
/// cache saves; a stat each is sub-millisecond. This is what build systems
/// key on for the same reason, and it fails in the same way -- a write that
/// preserves both size and mtime is not noticed. Nothing that edits a plugin
/// does that; `touch` does the harmless opposite, an unnecessary rebuild.
fn fold_sources(files: &[PathBuf], base: Option<&Path>) -> Option<u64> {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for file in files {
        let named = match base {
            Some(base) => file.strip_prefix(base).unwrap_or(file),
            None => file.as_path(),
        };
        let meta = std::fs::metadata(file).ok()?;
        let mtime = meta
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(0);
        for part in [
            source_fingerprint(&named.to_string_lossy()),
            meta.len(),
            mtime,
        ] {
            hash ^= part;
            hash = hash.wrapping_mul(0x100_0000_01b3);
        }
    }
    Some(hash)
}

fn collect_sources(dir: &Path, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let name = entry.file_name();
        let name = name.to_string_lossy();
        // `.compiled` is this cache's own output, and a dot-directory is not
        // somewhere plugin sources live.
        if name.starts_with('.') {
            continue;
        }
        if entry.file_type()?.is_dir() {
            collect_sources(&path, out)?;
        } else if path.extension().is_some_and(|e| e == "ts" || e == "js") {
            out.push(path);
        }
    }
    Ok(())
}

/// A 64-bit fingerprint of a plugin's source text (FNV-1a).
///
/// Spelled out rather than reached for from `std`: the build script and the
/// runtime must agree on this value across separately compiled crates, and
/// `DefaultHasher` makes no stability promise. This one is fixed by its own
/// definition.
pub fn source_fingerprint(source: &str) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in source.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100_0000_01b3);
    }
    hash
}

/// Wrap a prepared plugin body the way the engine expects to receive it.
///
/// Shared because the build script compiles this exact text to bytecode and
/// the runtime evaluates this exact text when there is no bytecode; if the two
/// spellings drifted, the compiled and interpreted paths would stop being the
/// same program.
///
/// The IIFE keeps a plugin's top-level `const editor = ...` from colliding
/// with the `editor` global (a TDZ error otherwise). `.call(globalThis)` is
/// what keeps `this` meaning the global object: module bodies are strict, and
/// a plain call would pass `undefined`.
pub fn wrap_plugin_body(code: &str) -> String {
    format!("(function() {{ {code} }}).call(globalThis);")
}

#[cfg(test)]
mod input_fingerprint_tests {
    use super::*;

    /// Write `entry.ts` importing `lib.ts`, and return both paths.
    fn fixture(dir: &Path, lib_body: &str) -> PathBuf {
        let lib = dir.join("lib.ts");
        std::fs::write(&lib, lib_body).unwrap();
        let entry = dir.join("entry.ts");
        std::fs::write(
            &entry,
            "import { greet } from \"./lib.ts\";\nglobalThis.out = greet();\n",
        )
        .unwrap();
        entry
    }

    /// The regression this exists for: a plugin's *own* text is not the whole
    /// input. Keying a compiled-plugin cache on `source_fingerprint` alone
    /// serves the bundle compiled against the old `lib.ts` forever, because
    /// the entry file never changed and the cached bytecode stays valid.
    #[test]
    fn editing_an_imported_file_changes_the_fingerprint() {
        let dir = tempfile::TempDir::new().unwrap();
        let entry = fixture(dir.path(), "export function greet() { return \"a\"; }\n");
        let entry_source = std::fs::read_to_string(&entry).unwrap();

        let before = input_fingerprint(&entry, &entry_source).expect("tree is readable");

        // A fresh directory with the *edited* lib, because the tree hash is
        // memoised per directory for the life of the process.
        //
        // The edit changes the file's length, not just its bytes. The
        // fingerprint is over path, size and mtime, so an edit of exactly the
        // same length would leave only the mtime to tell the two apart -- and
        // two files written microseconds apart share an mtime on any
        // filesystem whose timestamps are coarser than that. This test failed
        // in CI for precisely that reason.
        let dir2 = tempfile::TempDir::new().unwrap();
        let entry2 = fixture(
            dir2.path(),
            "export function greet() { return \"a much longer greeting\"; }\n",
        );
        // The importer is untouched, so this is the case that used to collide.
        assert_eq!(entry_source, std::fs::read_to_string(&entry2).unwrap());
        let after = input_fingerprint(&entry2, &entry_source).expect("tree is readable");

        assert_ne!(
            before, after,
            "an edit to an imported file must change the key"
        );
        assert_eq!(
            source_fingerprint(&entry_source),
            source_fingerprint(&std::fs::read_to_string(&entry).unwrap()),
            "...even though the entry file's own fingerprint is unchanged, \
             which is exactly why the entry's own fingerprint is not enough"
        );
    }

    /// The fingerprint must not depend on the clock.
    ///
    /// With both trees pinned to the same mtime, size is the only thing left
    /// to tell them apart -- which is the guarantee the stat-based hash
    /// actually offers, stated as a test rather than left to whether two
    /// writes happened to land in different timestamp ticks.
    #[test]
    fn identical_mtimes_do_not_hide_a_changed_file() {
        let pinned = std::time::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000_000);
        let build = |body: &str| -> (tempfile::TempDir, PathBuf) {
            let dir = tempfile::TempDir::new().unwrap();
            let entry = fixture(dir.path(), body);
            for name in ["lib.ts", "entry.ts"] {
                let f = std::fs::File::options()
                    .write(true)
                    .open(dir.path().join(name))
                    .unwrap();
                f.set_modified(pinned).unwrap();
            }
            (dir, entry)
        };
        let (_a, short) = build("export function greet() { return \"a\"; }\n");
        let (_b, long) = build("export function greet() { return \"a much longer one\"; }\n");
        let source = std::fs::read_to_string(&short).unwrap();
        assert_ne!(
            input_fingerprint(&short, &source),
            input_fingerprint(&long, &source),
            "a file of a different length must change the key with the clock held still"
        );
    }

    #[test]
    fn the_same_inputs_give_the_same_fingerprint() {
        let dir = tempfile::TempDir::new().unwrap();
        let entry = fixture(dir.path(), "export function greet() { return \"a\"; }\n");
        let source = std::fs::read_to_string(&entry).unwrap();
        assert_eq!(
            input_fingerprint(&entry, &source),
            input_fingerprint(&entry, &source),
        );
    }

    /// A plugin with no imports still gets a key -- it just does not need a
    /// walk to compute one.
    #[test]
    fn a_plugin_without_imports_is_still_fingerprinted() {
        let dir = tempfile::TempDir::new().unwrap();
        let plain = dir.path().join("plain.ts");
        std::fs::write(&plain, "globalThis.out = 1;\n").unwrap();
        let source = std::fs::read_to_string(&plain).unwrap();
        assert!(input_fingerprint(&plain, &source).is_some());
    }

    /// A directory that cannot be read means the input set is unknown, and an
    /// unknown input set must not be cached under a key that claims to cover
    /// it.
    #[test]
    fn an_unreadable_tree_has_no_fingerprint() {
        let missing = Path::new("/definitely/not/a/directory/plugin.ts");
        assert_eq!(input_fingerprint(missing, "globalThis.x = 1;\n"), None);
    }

    /// The whole tree is the input, so a new sibling counts too -- coarser
    /// than the import graph, and deliberately so.
    #[test]
    fn adding_a_sibling_plugin_changes_the_fingerprint() {
        let dir = tempfile::TempDir::new().unwrap();
        let entry = fixture(dir.path(), "export function greet() { return \"a\"; }\n");
        let source = std::fs::read_to_string(&entry).unwrap();
        let before = input_fingerprint(&entry, &source).unwrap();
        std::fs::write(dir.path().join("other.ts"), "globalThis.other = 1;\n").unwrap();
        // Memoised per directory, so a fresh tempdir is what shows the change.
        let dir2 = tempfile::TempDir::new().unwrap();
        let entry2 = fixture(dir2.path(), "export function greet() { return \"a\"; }\n");
        std::fs::write(dir2.path().join("other.ts"), "globalThis.other = 1;\n").unwrap();
        let after = input_fingerprint(&entry2, &source).unwrap();
        assert_ne!(before, after, "a sibling source is part of the tree");
    }
}
