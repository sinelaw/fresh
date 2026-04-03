//! npm/Node.js dependency resolution.
//!
//! Reads `package.json` dependencies (including devDependencies) and resolves
//! packages from `node_modules/` and `node_modules/@types/`.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use super::files;
use super::{Ecosystem, PackageDir};

/// Parse `package.json` to extract all declared dependency names.
///
/// Includes dependencies, devDependencies, and peerDependencies.
/// For the full transitive set, we also scan `node_modules/` directly since
/// npm/yarn/pnpm install resolves transitives there.
pub fn parse_package_json(workspace_root: &Path) -> HashSet<String> {
    let pkg_path = workspace_root.join("package.json");
    let content = match std::fs::read_to_string(&pkg_path) {
        Ok(c) => c,
        Err(_) => return HashSet::new(),
    };

    let mut deps = HashSet::new();

    // Simple JSON parsing without serde — extract keys from dependency objects
    for section in &[
        "\"dependencies\"",
        "\"devDependencies\"",
        "\"peerDependencies\"",
    ] {
        if let Some(start) = content.find(section) {
            if let Some(brace_start) = content[start..].find('{') {
                let obj_start = start + brace_start + 1;
                if let Some(brace_end) = find_matching_brace(&content[obj_start..]) {
                    let obj = &content[obj_start..obj_start + brace_end];
                    for dep_name in extract_json_keys(obj) {
                        deps.insert(dep_name);
                    }
                }
            }
        }
    }

    deps
}

/// Resolve declared dependencies to their directories in node_modules.
///
/// For npm, the transitive closure is already flattened into node_modules/,
/// so we scan it and match against declared deps. We also always include
/// `@types/*` packages for TypeScript declaration files.
pub fn resolve_package_dirs(
    workspace_root: &Path,
    declared_deps: &HashSet<String>,
) -> Vec<PackageDir> {
    let mut result = Vec::new();
    let nm = workspace_root.join("node_modules");
    if !nm.is_dir() {
        return result;
    }

    // Scan node_modules for declared deps + all transitive deps
    // Since npm hoists transitives, just include everything in node_modules
    // that isn't hidden
    let entries = match std::fs::read_dir(&nm) {
        Ok(e) => e,
        Err(_) => return result,
    };

    for entry in entries.flatten() {
        let dir = entry.path();
        if !dir.is_dir() {
            continue;
        }
        let name = match dir.file_name().and_then(|n| n.to_str()) {
            Some(n) => n.to_string(),
            None => continue,
        };

        if name.starts_with('.') {
            continue;
        }

        // Scoped packages (@scope/name)
        if name.starts_with('@') {
            if let Ok(scoped_entries) = std::fs::read_dir(&dir) {
                for scoped in scoped_entries.flatten() {
                    let scoped_dir = scoped.path();
                    if scoped_dir.is_dir() {
                        let scoped_name = format!(
                            "{}/{}",
                            name,
                            scoped_dir.file_name().unwrap_or_default().to_string_lossy()
                        );
                        // Always include @types packages; for others check declared
                        if name == "@types" || declared_deps.contains(&scoped_name) {
                            result.push(PackageDir {
                                path: scoped_dir,
                                ecosystem: Ecosystem::Npm,
                            });
                        }
                    }
                }
            }
            continue;
        }

        // Regular packages — include if declared or if it's a transitive dep
        // Since we're scanning node_modules, all installed packages are transitives
        if declared_deps.contains(&name) || is_installed_transitive(&dir) {
            result.push(PackageDir {
                path: dir,
                ecosystem: Ecosystem::Npm,
            });
        }
    }

    result
}

/// Check if node_modules exists.
pub fn node_modules_available(workspace_root: &Path) -> bool {
    workspace_root.join("node_modules").is_dir()
}

/// File extensions for JS/TS source files (stubs first).
pub fn extensions() -> &'static [&'static str] {
    &["d.ts", "ts", "js"]
}

/// npm prefers .d.ts stubs over .ts/.js source.
pub fn prefers_stubs() -> bool {
    true
}

/// Collect source files for an npm package.
pub fn collect_package_files(pkg_dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    files::collect_source_files(pkg_dir, extensions(), prefers_stubs(), 0, &mut files);
    if files.len() > files::MAX_FILES_PER_PACKAGE {
        files.truncate(files::MAX_FILES_PER_PACKAGE);
    }
    files
}

// A package in node_modules is always a transitive dep (npm hoists).
fn is_installed_transitive(_dir: &Path) -> bool {
    true
}

fn find_matching_brace(s: &str) -> Option<usize> {
    let mut depth = 1i32;
    for (i, ch) in s.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

fn extract_json_keys(obj: &str) -> Vec<String> {
    let mut keys = Vec::new();
    let mut i = 0;
    let chars: Vec<char> = obj.chars().collect();
    while i < chars.len() {
        if chars[i] == '"' {
            i += 1;
            let start = i;
            while i < chars.len() && chars[i] != '"' {
                if chars[i] == '\\' {
                    i += 1;
                }
                i += 1;
            }
            let key: String = chars[start..i].iter().collect();
            // Check if this is a key (followed by ':')
            i += 1;
            while i < chars.len() && chars[i].is_whitespace() {
                i += 1;
            }
            if i < chars.len() && chars[i] == ':' {
                keys.push(key);
                // Skip the value
                i += 1;
            }
        } else {
            i += 1;
        }
    }
    keys
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_package_json_deps() {
        let dir = std::env::temp_dir().join("quicklsp_test_npm");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("package.json"),
            r#"{
  "name": "myproject",
  "dependencies": {
    "express": "^4.18.0",
    "@types/node": "^20.0.0"
  },
  "devDependencies": {
    "typescript": "^5.0.0"
  }
}"#,
        )
        .unwrap();

        let deps = parse_package_json(&dir);
        assert!(deps.contains("express"));
        assert!(deps.contains("@types/node"));
        assert!(deps.contains("typescript"));

        let _ = std::fs::remove_dir_all(&dir);
    }
}
