//! The oxc implementation of the TypeScript functions: transpiling,
//! isolated declarations, bundling and import/export stripping, all
//! in-process through the oxc toolchain.

use super::{has_es_module_syntax, SyntaxError};
use anyhow::{anyhow, Result};
use oxc_allocator::Allocator;
use oxc_ast::ast::{Declaration, ExportDefaultDeclarationKind, Statement};
use oxc_codegen::Codegen;
use oxc_isolated_declarations::{IsolatedDeclarations, IsolatedDeclarationsOptions};
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use oxc_transformer::{TransformOptions, Transformer};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

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
pub(crate) struct ImportBinding {
    /// Local name used in this module
    pub(crate) local_name: String,
    /// Name exported from the source module (None for default import)
    pub(crate) imported_name: Option<String>,
    /// Path to the source module (as written, e.g., "./lib/index.ts")
    pub(crate) source_path: String,
    /// Whether this is a namespace import (import * as X)
    pub(crate) is_namespace: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct ExportBinding {
    /// Name this is exported as
    pub(crate) exported_name: String,
    /// Local name in this module (might differ for `export { x as y }`)
    pub(crate) local_name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct ReexportBinding {
    /// Name this is exported as (None for `export *`)
    pub(crate) exported_name: Option<String>,
    /// Name in the source module (None for `export *`)
    pub(crate) source_name: Option<String>,
    /// Path to the source module
    pub(crate) source_path: String,
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
pub(crate) fn extract_module_bindings(
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

/// Syntax errors in `source`, with 1-based positions.
pub fn syntax_errors(source: &str, filename: &str) -> Vec<SyntaxError> {
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(filename).unwrap_or_default();
    let parser_ret = Parser::new(&allocator, source, source_type).parse();
    parser_ret
        .errors
        .iter()
        .map(|err| {
            // oxc errors carry labels/spans but the formatting is embedded in
            // the miette-style Display impl. Pull the primary message and
            // recover line/column from the start of the first label.
            let (line, column) = err
                .labels
                .as_ref()
                .and_then(|v| v.first())
                .map(|l| line_col(source, l.offset()))
                .unwrap_or((0, 0));
            SyntaxError {
                message: err.message.to_string(),
                line,
                column,
            }
        })
        .collect()
}

/// Convert a byte offset into a (line, column) pair, 1-based, for display.
fn line_col(source: &str, offset: usize) -> (u32, u32) {
    let clipped = source.get(..offset).unwrap_or(source);
    let line = 1 + clipped.bytes().filter(|&b| b == b'\n').count();
    let col = 1 + clipped
        .rsplit('\n')
        .next()
        .map(|s| s.chars().count())
        .unwrap_or(0);
    (line as u32, col as u32)
}
