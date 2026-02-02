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
                let subdir_name = path.file_name().unwrap().to_string_lossy();

                // Skip "packages" subdirectory at top level - it's handled separately
                // by load_package_themes for proper package metadata
                if pack == "user" && subdir_name == "packages" {
                    continue;
                }

                // Recurse into subdirectory with updated pack name
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

    /// Test that custom themes in user themes directory are loaded and available.
    /// This is a regression test for the macOS bug where themes in ~/.config/fresh/themes/
    /// were not appearing in the "Select Theme" command because ThemeLoader was using
    /// the wrong directory path on macOS.
    #[test]
    fn test_custom_theme_loading_from_user_dir() {
        // Create isolated temp directory for this test
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let themes_dir = temp_dir.path().to_path_buf();

        // Create a custom theme file directly in the themes directory
        let theme_json = r#"{
            "name": "my-custom-theme",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(themes_dir.join("my-custom-theme.json"), theme_json)
            .expect("Failed to write theme file");

        // Load themes with the custom themes directory
        let loader = ThemeLoader::new(themes_dir.clone());
        let registry = loader.load_all();

        // Verify the custom theme is loaded
        assert!(
            registry.contains("my-custom-theme"),
            "Custom theme should be loaded from user themes directory"
        );
        assert!(
            registry.get("my-custom-theme").is_some(),
            "Custom theme should be retrievable"
        );

        // Verify it appears in the theme list (used for "Select Theme" menu)
        let theme_list = registry.list();
        assert!(
            theme_list.iter().any(|t| t.name == "my-custom-theme"),
            "Custom theme should appear in theme list for Select Theme menu"
        );

        // Verify the theme has the correct pack metadata
        let theme_info = theme_list
            .iter()
            .find(|t| t.name == "my-custom-theme")
            .unwrap();
        assert_eq!(
            theme_info.pack, "user",
            "Custom theme should have 'user' pack"
        );

        // Verify the theme is also available via generate_dynamic_items
        // (the function used for Select Theme menu items)
        #[cfg(not(target_arch = "wasm32"))]
        {
            let menu_items = crate::config::generate_dynamic_items("copy_with_theme", &themes_dir);
            let theme_names: Vec<_> = menu_items
                .iter()
                .filter_map(|item| match item {
                    crate::config::MenuItem::Action { args, .. } => {
                        args.get("theme").map(|v| v.as_str().unwrap_or_default())
                    }
                    _ => None,
                })
                .collect();
            assert!(
                theme_names.contains(&"my-custom-theme"),
                "Custom theme should appear in dynamic menu items"
            );
        }
    }

    /// Test that custom themes in a package directory (with package.json) are loaded.
    #[test]
    fn test_custom_theme_package_loading() {
        // Create isolated temp directory for this test
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let themes_dir = temp_dir.path().to_path_buf();

        // Create packages subdirectory
        let packages_dir = themes_dir.join("packages");
        let pkg_dir = packages_dir.join("my-theme-pack");
        std::fs::create_dir_all(&pkg_dir).expect("Failed to create package dir");

        // Create package.json manifest
        let manifest = r#"{
            "name": "my-theme-pack",
            "fresh": {
                "themes": [
                    { "name": "Packaged Theme", "file": "packaged-theme.json" }
                ]
            }
        }"#;
        std::fs::write(pkg_dir.join("package.json"), manifest)
            .expect("Failed to write package.json");

        // Create the theme file referenced in package.json
        let theme_json = r#"{
            "name": "packaged-theme",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(pkg_dir.join("packaged-theme.json"), theme_json)
            .expect("Failed to write theme file");

        // Load themes
        let loader = ThemeLoader::new(themes_dir);
        let registry = loader.load_all();

        // Verify the packaged theme is loaded (name is normalized from "Packaged Theme")
        assert!(
            registry.contains("packaged-theme"),
            "Packaged theme should be loaded"
        );

        // Verify it appears in the theme list with correct pack name
        let theme_list = registry.list();
        let theme_info = theme_list
            .iter()
            .find(|t| t.name == "packaged-theme")
            .expect("Packaged theme should be in theme list");
        assert_eq!(
            theme_info.pack, "pkg/my-theme-pack",
            "Packaged theme should have correct pack name"
        );
    }

    /// Test that themes in subdirectories of the user themes directory are loaded.
    #[test]
    fn test_custom_theme_in_subdirectory() {
        // Create isolated temp directory for this test
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let themes_dir = temp_dir.path().to_path_buf();

        // Create a subdirectory
        let subdir = themes_dir.join("my-collection");
        std::fs::create_dir_all(&subdir).expect("Failed to create subdir");

        // Create a theme in the subdirectory
        let theme_json = r#"{
            "name": "nested-theme",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(subdir.join("nested-theme.json"), theme_json)
            .expect("Failed to write theme file");

        // Load themes
        let loader = ThemeLoader::new(themes_dir);
        let registry = loader.load_all();

        // Verify the nested theme is loaded
        assert!(
            registry.contains("nested-theme"),
            "Theme in subdirectory should be loaded"
        );

        // Verify pack name includes the subdirectory
        let theme_list = registry.list();
        let theme_info = theme_list
            .iter()
            .find(|t| t.name == "nested-theme")
            .expect("Nested theme should be in theme list");
        assert_eq!(
            theme_info.pack, "user/my-collection",
            "Nested theme should have subdirectory in pack name"
        );
    }
}
