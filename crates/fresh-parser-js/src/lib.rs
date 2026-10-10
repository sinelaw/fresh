//! TypeScript and JavaScript handling for Fresh plugins: transpiling
//! TypeScript to JavaScript, bundling a plugin's relative imports, stripping
//! module syntax, syntax checks, and plugin dependency ordering.
//!
//! Two backends provide the transpiling functions:
//!
//! - **oxc** (the `oxc` feature, on by default): in-process, through the oxc
//!   toolchain.
//! - **esbuild** (the `oxc` feature off): the `esbuild` executable, run as a
//!   subprocess. Debian ships it as a package, while oxc's ~25 crates are not
//!   in Debian, so a Debian build turns `oxc` off and depends on `esbuild`.
//!   It cannot emit `.d.ts` declarations ([`CAN_EMIT_DECLARATIONS`]).
//!
//! Every plugin Fresh loads — bundled, user, package-manager installed,
//! `init.ts`, scripts — goes through these functions, so the backend choice
//! covers all of them.

use anyhow::{anyhow, Result};
use std::collections::HashSet;

#[cfg(feature = "oxc")]
mod oxc_backend;
#[cfg(feature = "oxc")]
pub use oxc_backend::{
    bundle_module, emit_isolated_declarations, strip_imports_and_exports, syntax_errors,
    transpile_typescript,
};

#[cfg(not(feature = "oxc"))]
mod esbuild_backend;
#[cfg(not(feature = "oxc"))]
pub use esbuild_backend::{
    bundle_module, emit_isolated_declarations, strip_imports_and_exports, syntax_errors,
    transpile_typescript,
};

/// Whether [`emit_isolated_declarations`] works with this build's backend
/// (only oxc can emit `.d.ts` declarations).
pub const CAN_EMIT_DECLARATIONS: bool = cfg!(feature = "oxc");

/// A syntax error, with 1-based positions (`0` when unknown).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    pub message: String,
    pub line: u32,
    pub column: u32,
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

#[cfg(test)]
mod tests {
    use super::*;
    #[cfg(feature = "oxc")]
    use crate::oxc_backend::extract_module_bindings;

    #[test]
    #[cfg(feature = "oxc")]
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
    #[cfg(feature = "oxc")]
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
    #[cfg(feature = "oxc")]
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
    #[cfg(feature = "oxc")]
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
    // Checks oxc's output shape (`const` kept, `interface` kept); the
    // esbuild backend has its own test of the same behaviour.
    #[cfg(feature = "oxc")]
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
