//! Python dependency resolution.
//!
//! Reads `requirements.txt` / `pyproject.toml` for declared dependencies and
//! resolves them from virtualenv `site-packages/`.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use super::files;
use super::{Ecosystem, PackageDir};

/// Parse Python dependency declarations from requirements.txt or pyproject.toml.
///
/// Returns normalized package names (lowercased, hyphens→underscores).
pub fn parse_requirements(workspace_root: &Path) -> HashSet<String> {
    let mut deps = HashSet::new();

    // requirements.txt
    let req_path = workspace_root.join("requirements.txt");
    if let Ok(content) = std::fs::read_to_string(&req_path) {
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') || trimmed.starts_with('-') {
                continue;
            }
            // Extract package name before version specifier
            let name = trimmed
                .split(&['=', '>', '<', '!', '~', '[', ';'][..])
                .next()
                .unwrap_or("")
                .trim();
            if !name.is_empty() {
                deps.insert(normalize_python_name(name));
            }
        }
    }

    // pyproject.toml [project.dependencies] or [tool.poetry.dependencies]
    let pyproject = workspace_root.join("pyproject.toml");
    if let Ok(content) = std::fs::read_to_string(&pyproject) {
        parse_pyproject_deps(&content, &mut deps);
    }

    deps
}

/// Resolve declared dependencies to their directories in site-packages.
///
/// Python packages in site-packages use normalized names (underscores, lowercase).
/// We scan site-packages and match against declared deps, plus include all
/// installed packages as transitives (pip installs the full closure).
pub fn resolve_package_dirs(
    workspace_root: &Path,
    _declared_deps: &HashSet<String>,
) -> Vec<PackageDir> {
    let mut result = Vec::new();

    for sp in find_site_packages(workspace_root) {
        let entries = match std::fs::read_dir(&sp) {
            Ok(e) => e,
            Err(_) => continue,
        };

        for entry in entries.flatten() {
            let dir = entry.path();
            if !dir.is_dir() {
                continue;
            }
            let name = match dir.file_name().and_then(|n| n.to_str()) {
                Some(n) => n,
                None => continue,
            };

            // Skip .dist-info, __pycache__, hidden dirs
            if name.starts_with('.')
                || name.starts_with('_')
                || name.ends_with(".dist-info")
                || name.ends_with(".egg-info")
            {
                continue;
            }

            // All installed packages are transitives in pip
            result.push(PackageDir {
                path: dir,
                ecosystem: Ecosystem::Python,
            });
        }
    }

    result
}

/// Check if any site-packages directory exists.
pub fn site_packages_available(workspace_root: &Path) -> bool {
    !find_site_packages(workspace_root).is_empty()
}

/// File extensions for Python (stubs first).
pub fn extensions() -> &'static [&'static str] {
    &["pyi", "py"]
}

/// Python prefers .pyi stubs over .py source.
pub fn prefers_stubs() -> bool {
    true
}

/// Collect source files for a Python package.
pub fn collect_package_files(pkg_dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    files::collect_source_files(pkg_dir, extensions(), prefers_stubs(), 0, &mut files);
    if files.len() > files::MAX_FILES_PER_PACKAGE {
        files.truncate(files::MAX_FILES_PER_PACKAGE);
    }
    files
}

/// Find all site-packages directories (venvs, system).
fn find_site_packages(workspace_root: &Path) -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // VIRTUAL_ENV env var
    if let Ok(venv) = std::env::var("VIRTUAL_ENV") {
        if let Some(sp) = find_sp_in_venv(Path::new(&venv)) {
            paths.push(sp);
        }
    }

    // Common venv directory names
    for name in &[".venv", "venv", "env", ".env"] {
        let candidate = workspace_root.join(name);
        if candidate.is_dir() {
            if let Some(sp) = find_sp_in_venv(&candidate) {
                paths.push(sp);
            }
        }
    }

    paths
}

fn find_sp_in_venv(venv: &Path) -> Option<PathBuf> {
    let lib = venv.join("lib");
    if !lib.is_dir() {
        return None;
    }
    if let Ok(entries) = std::fs::read_dir(&lib) {
        for entry in entries.flatten() {
            let py_dir = entry.path();
            if py_dir.is_dir() {
                if let Some(name) = py_dir.file_name().and_then(|n| n.to_str()) {
                    if name.starts_with("python") {
                        let sp = py_dir.join("site-packages");
                        if sp.is_dir() {
                            return Some(sp);
                        }
                    }
                }
            }
        }
    }
    None
}

fn normalize_python_name(name: &str) -> String {
    name.to_lowercase().replace('-', "_")
}

fn parse_pyproject_deps(content: &str, deps: &mut HashSet<String>) {
    // Simple extraction of dependency names from pyproject.toml
    // Looks for lines like: "package-name>=1.0" in dependency arrays
    let mut in_deps_section = false;

    for line in content.lines() {
        let trimmed = line.trim();

        if trimmed.starts_with("[project.dependencies]")
            || trimmed.starts_with("[tool.poetry.dependencies]")
        {
            in_deps_section = true;
            continue;
        }

        if trimmed.starts_with('[') {
            in_deps_section = false;
            continue;
        }

        if in_deps_section {
            // Array format: "package-name>=1.0",
            let cleaned = trimmed.trim_matches(&['"', '\'', ',', ' '][..]);
            let name = cleaned
                .split(&['=', '>', '<', '!', '~', '[', ';'][..])
                .next()
                .unwrap_or("")
                .trim();
            if !name.is_empty() && !name.starts_with('#') {
                deps.insert(normalize_python_name(name));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_requirements_txt() {
        let dir = std::env::temp_dir().join("quicklsp_test_python");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join("requirements.txt"),
            "flask>=2.0\nrequests==2.28.0\n# comment\nnumpy\n",
        )
        .unwrap();

        let deps = parse_requirements(&dir);
        assert!(deps.contains("flask"));
        assert!(deps.contains("requests"));
        assert!(deps.contains("numpy"));

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn normalize_names() {
        assert_eq!(normalize_python_name("My-Package"), "my_package");
        assert_eq!(normalize_python_name("numpy"), "numpy");
    }
}
