//! Theme loading and registry.
//!
//! This module provides:
//! - `ThemeRegistry`: A pure data structure holding all loaded themes
//! - `ThemeLoader`: Scans and loads themes into a registry

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::types::{Theme, ThemeFile, ThemeInfo, BUILTIN_THEMES};

/// A registry holding all loaded themes.
///
/// This is a pure data structure - no I/O operations.
/// Use `ThemeLoader` to create and populate a registry.
#[derive(Debug, Clone)]
pub struct ThemeRegistry {
    /// All loaded themes, keyed by name
    themes: HashMap<String, Theme>,
    /// Theme metadata for listing
    theme_list: Vec<ThemeInfo>,
}

impl ThemeRegistry {
    /// Get a theme by name.
    pub fn get(&self, name: &str) -> Option<&Theme> {
        let normalized = name.to_lowercase().replace('_', "-");
        self.themes.get(&normalized)
    }

    /// Get a cloned theme by name.
    pub fn get_cloned(&self, name: &str) -> Option<Theme> {
        self.get(name).cloned()
    }

    /// List all available themes with metadata.
    pub fn list(&self) -> &[ThemeInfo] {
        &self.theme_list
    }

    /// Get all theme names.
    pub fn names(&self) -> Vec<String> {
        self.theme_list.iter().map(|t| t.name.clone()).collect()
    }

    /// Check if a theme exists.
    pub fn contains(&self, name: &str) -> bool {
        let normalized = name.to_lowercase().replace('_', "-");
        self.themes.contains_key(&normalized)
    }

    /// Number of themes in the registry.
    pub fn len(&self) -> usize {
        self.themes.len()
    }

    /// Check if registry is empty.
    pub fn is_empty(&self) -> bool {
        self.themes.is_empty()
    }
}

/// Loads themes and creates a ThemeRegistry.
pub struct ThemeLoader {
    user_themes_dir: Option<PathBuf>,
}

impl ThemeLoader {
    /// Create a ThemeLoader with the given user themes directory.
    pub fn new(user_themes_dir: PathBuf) -> Self {
        Self {
            user_themes_dir: Some(user_themes_dir),
        }
    }

    /// Create a ThemeLoader for embedded themes only (no user themes).
    pub fn embedded_only() -> Self {
        Self {
            user_themes_dir: None,
        }
    }

    /// Get the user themes directory path.
    pub fn user_themes_dir(&self) -> Option<&Path> {
        self.user_themes_dir.as_deref()
    }

    /// Load all themes (embedded + user + packages) into a registry.
    pub fn load_all(&self) -> ThemeRegistry {
        let mut themes = HashMap::new();
        let mut theme_list = Vec::new();

        // Load all embedded themes
        for builtin in BUILTIN_THEMES {
            if let Ok(theme_file) = serde_json::from_str::<ThemeFile>(builtin.json) {
                let theme: Theme = theme_file.into();
                themes.insert(builtin.name.to_string(), theme);
                theme_list.push(ThemeInfo::new(builtin.name, builtin.pack));
            }
        }

        // Load user themes from ~/.config/fresh/themes/ (recursively)
        if let Some(ref user_dir) = self.user_themes_dir {
            self.scan_directory(user_dir, "user", &mut themes, &mut theme_list);
        }

        // Load theme packages from ~/.config/fresh/themes/packages/*/
        // Each package directory may contain multiple theme JSON files
        if let Some(ref user_dir) = self.user_themes_dir {
            let packages_dir = user_dir.join("packages");
            if packages_dir.exists() {
                if let Ok(entries) = std::fs::read_dir(&packages_dir) {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if path.is_dir() {
                            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                                // Skip hidden directories (like .index)
                                if !name.starts_with('.') {
                                    // Check for package.json to get theme metadata
                                    let manifest_path = path.join("package.json");
                                    if manifest_path.exists() {
                                        self.load_package_themes(
                                            &path,
                                            name,
                                            &mut themes,
                                            &mut theme_list,
                                        );
                                    } else {
                                        // Fallback: scan directory for JSON files
                                        let pack_name = format!("pkg/{}", name);
                                        self.scan_directory(
                                            &path,
                                            &pack_name,
                                            &mut themes,
                                            &mut theme_list,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        ThemeRegistry { themes, theme_list }
    }

    /// Load themes from a package with package.json manifest.
    fn load_package_themes(
        &self,
        pkg_dir: &Path,
        pkg_name: &str,
        themes: &mut HashMap<String, Theme>,
        theme_list: &mut Vec<ThemeInfo>,
    ) {
        let manifest_path = pkg_dir.join("package.json");
        let manifest_content = match std::fs::read_to_string(&manifest_path) {
            Ok(c) => c,
            Err(_) => return,
        };

        // Parse manifest to find theme entries
        let manifest: serde_json::Value = match serde_json::from_str(&manifest_content) {
            Ok(v) => v,
            Err(_) => return,
        };

        // Check for fresh.themes array in manifest
        if let Some(fresh) = manifest.get("fresh") {
            if let Some(theme_entries) = fresh.get("themes").and_then(|t| t.as_array()) {
                for entry in theme_entries {
                    if let (Some(file), Some(name)) = (
                        entry.get("file").and_then(|f| f.as_str()),
                        entry.get("name").and_then(|n| n.as_str()),
                    ) {
                        let theme_path = pkg_dir.join(file);
                        if theme_path.exists() {
                            if let Ok(content) = std::fs::read_to_string(&theme_path) {
                                if let Ok(theme_file) = serde_json::from_str::<ThemeFile>(&content)
                                {
                                    let theme: Theme = theme_file.into();
                                    let normalized_name = name.to_lowercase().replace(' ', "-");
                                    // Don't overwrite existing themes
                                    if !themes.contains_key(&normalized_name) {
                                        themes.insert(normalized_name.clone(), theme);
                                        let pack_name = format!("pkg/{}", pkg_name);
                                        theme_list
                                            .push(ThemeInfo::new(normalized_name, &pack_name));
                                    }
                                }
                            }
                        }
                    }
                }
                return;
            }
        }

        // Fallback: if no fresh.themes, scan for JSON files
        let pack_name = format!("pkg/{}", pkg_name);
        self.scan_directory(pkg_dir, &pack_name, themes, theme_list);
    }

    /// Recursively scan a directory for theme files.
    fn scan_directory(
        &self,
        dir: &Path,
        pack: &str,
        themes: &mut HashMap<String, Theme>,
        theme_list: &mut Vec<ThemeInfo>,
    ) {
        let entries = match std::fs::read_dir(dir) {
            Ok(e) => e,
            Err(_) => return,
        };

        for entry in entries.flatten() {
            let path = entry.path();

            if path.is_dir() {
                // Recurse into subdirectory with updated pack name
                let subdir_name = path.file_name().unwrap().to_string_lossy();
                let new_pack = if pack == "user" {
                    format!("user/{}", subdir_name)
                } else {
                    format!("{}/{}", pack, subdir_name)
                };
                self.scan_directory(&path, &new_pack, themes, theme_list);
            } else if path.extension().is_some_and(|ext| ext == "json") {
                // Load theme file
                let name = path.file_stem().unwrap().to_string_lossy().to_string();

                // Skip if already loaded (embedded themes take priority)
                if themes.contains_key(&name) {
                    continue;
                }

                if let Ok(content) = std::fs::read_to_string(&path) {
                    if let Ok(theme_file) = serde_json::from_str::<ThemeFile>(&content) {
                        let theme: Theme = theme_file.into();
                        themes.insert(name.clone(), theme);
                        theme_list.push(ThemeInfo::new(name, pack));
                    }
                }
            }
        }
    }
}

// Cursor color methods on Theme (no I/O for theme loading)
impl Theme {
    /// Set the terminal cursor color using OSC 12 escape sequence.
    /// This makes the hardware cursor visible on any background.
    pub fn set_terminal_cursor_color(&self) {
        use super::types::color_to_rgb;
        use std::io::Write;
        if let Some((r, g, b)) = color_to_rgb(self.cursor) {
            // OSC 12 sets cursor color: \x1b]12;#RRGGBB\x07
            let _ = write!(
                std::io::stdout(),
                "\x1b]12;#{:02x}{:02x}{:02x}\x07",
                r,
                g,
                b
            );
            let _ = std::io::stdout().flush();
        }
    }

    /// Reset the terminal cursor color to default.
    pub fn reset_terminal_cursor_color() {
        use std::io::Write;
        // OSC 112 resets cursor color to default
        let _ = write!(std::io::stdout(), "\x1b]112\x07");
        let _ = std::io::stdout().flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_theme_registry_get() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all();

        // Should find builtin themes
        assert!(registry.get("dark").is_some());
        assert!(registry.get("light").is_some());
        assert!(registry.get("high-contrast").is_some());

        // Name normalization
        assert!(registry.get("Dark").is_some());
        assert!(registry.get("DARK").is_some());
        assert!(registry.get("high_contrast").is_some());

        // Non-existent
        assert!(registry.get("nonexistent-theme").is_none());
    }

    #[test]
    fn test_theme_registry_list() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all();

        let list = registry.list();
        assert!(list.len() >= 7); // At least the builtin themes

        // Check some expected themes
        assert!(list.iter().any(|t| t.name == "dark"));
        assert!(list.iter().any(|t| t.name == "light"));
    }

    #[test]
    fn test_theme_registry_contains() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all();

        assert!(registry.contains("dark"));
        assert!(registry.contains("Dark")); // normalized
        assert!(!registry.contains("nonexistent"));
    }

    #[test]
    fn test_theme_loader_load_all() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all();

        // Should have loaded all embedded themes
        assert!(registry.len() >= 7); // 7 root themes (xscriptor moved to external repo)

        // Verify theme content is correct
        let dark = registry.get("dark").unwrap();
        assert_eq!(dark.name, "dark");
    }
}
