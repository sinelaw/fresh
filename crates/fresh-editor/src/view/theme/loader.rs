//! Theme loading and registry.
//!
//! This module provides:
//! - `ThemeRegistry`: A pure data structure holding all loaded themes
//! - `ThemeLoader`: Scans and loads themes into a registry

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::types::{Theme, ThemeFile, ThemeInfo, BUILTIN_THEMES};

/// Normalize a theme name for consistent lookup and storage.
///
/// Converts to lowercase and replaces underscores and spaces with hyphens.
/// This ensures that theme names can be matched regardless of how they appear
/// in filenames vs. JSON content (e.g., "Catppuccin Mocha" matches "catppuccin-mocha").
pub fn normalize_theme_name(name: &str) -> String {
    name.to_lowercase().replace(['_', ' '], "-")
}

/// Expand `~`, `$VAR`, and `${VAR}` references in a config-supplied path.
///
/// Minimal shell-like expansion (no crate dependency). Unknown variables are
/// left literal so a typo surfaces as a resolution failure rather than
/// silently collapsing to an empty path. Used for theme config values so a
/// shared dotfiles repo can write `file://${HOME}/.config/fresh/themes/x.json`
/// and have it resolve correctly on any machine.
pub(crate) fn expand_env_vars(input: &str) -> String {
    let input = if let Some(rest) = input.strip_prefix('~') {
        match std::env::var("HOME") {
            Ok(home) => format!("{}{}", home, rest),
            Err(_) => input.to_string(),
        }
    } else {
        input.to_string()
    };

    let bytes = input.as_bytes();
    let mut out = String::with_capacity(input.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'$' {
            out.push(bytes[i] as char);
            i += 1;
            continue;
        }
        // `$` at end of string — keep literal.
        if i + 1 >= bytes.len() {
            out.push('$');
            i += 1;
            continue;
        }
        if bytes[i + 1] == b'{' {
            if let Some(close) = input[i + 2..].find('}') {
                let name = &input[i + 2..i + 2 + close];
                match std::env::var(name) {
                    Ok(v) => out.push_str(&v),
                    Err(_) => out.push_str(&input[i..i + 2 + close + 1]),
                }
                i += 2 + close + 1;
                continue;
            }
            // Unterminated `${…` — keep literal.
            out.push('$');
            i += 1;
            continue;
        }
        // `$VAR` — consume ASCII alphanumerics and underscores.
        let start = i + 1;
        let mut end = start;
        while end < bytes.len() && (bytes[end].is_ascii_alphanumeric() || bytes[end] == b'_') {
            end += 1;
        }
        if end == start {
            out.push('$');
            i += 1;
            continue;
        }
        let name = &input[start..end];
        match std::env::var(name) {
            Ok(v) => out.push_str(&v),
            Err(_) => out.push_str(&input[i..end]),
        }
        i = end;
    }
    out
}

/// A registry holding all loaded themes.
///
/// This is a pure data structure - no I/O operations.
/// Use `ThemeLoader` to create and populate a registry.
///
/// Themes are keyed by their unique `ThemeInfo::key` (typically a repository
/// URL or `pack/name` path). Lookups fall back to matching by display name
/// for backward compatibility with configs that store just e.g. `"dark"`.
#[derive(Debug, Clone)]
pub struct ThemeRegistry {
    /// All loaded themes, keyed by ThemeInfo.key
    themes: HashMap<String, Theme>,
    /// Theme metadata for listing
    theme_list: Vec<ThemeInfo>,
    /// User themes directory — used to resolve relative-path config values
    /// (e.g. `"s-dark.json"`, `"packages/nord/dark.json"`) against the
    /// machine's themes dir, so configs stay portable across machines.
    themes_dir: Option<PathBuf>,
}

impl ThemeRegistry {
    /// Look up a theme by its config value (key, scheme URI, relative path,
    /// or legacy bare name). See [`ThemeRegistry::resolve_key`].
    pub fn get(&self, key_or_name: &str) -> Option<&Theme> {
        self.resolve_key(key_or_name)
            .and_then(|key| self.themes.get(&key))
    }

    /// Get a cloned theme by key or name.
    pub fn get_cloned(&self, key_or_name: &str) -> Option<Theme> {
        self.get(key_or_name).cloned()
    }

    /// Resolve a config-value to the canonical registry key.
    ///
    /// Accepted config forms (issue #1621):
    ///
    /// | Form | Example | Resolves to |
    /// |------|---------|-------------|
    /// | `builtin://NAME` | `builtin://dark` | built-in theme by name |
    /// | `file://PATH` (env-expanded) | `file://${HOME}/.config/fresh/themes/x.json` | exact user-theme key |
    /// | `http(s)://...` | `https://github.com/...#dark` | URL-packaged theme |
    /// | relative `.json` path | `s-dark.json`, `packages/nord/dark.json` | user theme under themes dir |
    /// | bare name (legacy) | `dark` | exact key, else normalized-name match |
    ///
    /// `$HOME`, `${HOME}`, `${XDG_CONFIG_HOME}` and a leading `~` are
    /// expanded before path matching so shared dotfile configs work on any
    /// machine. Returns an owned `String` because some forms (e.g. a relative
    /// path) must be reconstructed as an absolute `file://` key.
    pub fn resolve_key(&self, value: &str) -> Option<String> {
        // 1. Exact key match — fast path; covers already-canonical keys
        //    (repo URLs, `file://…` written as-is, plain built-in names).
        if self.themes.contains_key(value) {
            return Some(value.to_string());
        }

        // 2. `builtin://NAME` — look up a built-in by normalized name.
        if let Some(name) = value.strip_prefix("builtin://") {
            let normalized = normalize_theme_name(name);
            return self
                .theme_list
                .iter()
                .find(|info| info.pack.is_empty() && normalize_theme_name(&info.name) == normalized)
                .map(|info| info.key.clone());
        }

        // 3. `file://PATH` — env-expand and retry exact match.
        if let Some(raw_path) = value.strip_prefix("file://") {
            let expanded = expand_env_vars(raw_path);
            // Normalize forward slashes to the OS-native separator so the
            // candidate matches the registry key built from
            // `entry.path().display()` (backslashes on Windows). No-op on
            // Unix where `MAIN_SEPARATOR_STR == "/"`.
            let expanded_native = expanded.replace('/', std::path::MAIN_SEPARATOR_STR);
            let candidate = format!("file://{}", expanded_native);
            if self.themes.contains_key(&candidate) {
                return Some(candidate);
            }
            return None;
        }

        // 4. `http(s)://…` — URL-packaged theme. Only exact match is valid;
        //    don't fall through to name fallback (would mask typos).
        if value.starts_with("http://") || value.starts_with("https://") {
            return None;
        }

        // 5. Relative path (ends with `.json`) — resolve against themes dir.
        //    This is the portable form for user themes (`s-dark.json` or
        //    `packages/nord/dark.json`). Env-var expansion is applied for
        //    the rare case of a hand-edited path like `${HOME}/foo.json`.
        if value.ends_with(".json") {
            if let Some(themes_dir) = self.themes_dir.as_deref() {
                let expanded = expand_env_vars(value);
                // Registry keys are built from `entry.path().display()`,
                // which uses the OS-native separator (`\` on Windows).
                // Normalize the config input's forward slashes to match so
                // the HashMap lookup hits on Windows too. No-op on Unix
                // (MAIN_SEPARATOR_STR is `/`).
                let expanded_native = expanded.replace('/', std::path::MAIN_SEPARATOR_STR);
                let expanded_path = std::path::Path::new(&expanded_native);
                let abs = if expanded_path.is_absolute() {
                    expanded_path.to_path_buf()
                } else {
                    themes_dir.join(expanded_path)
                };
                let candidate = format!("file://{}", abs.display());
                if self.themes.contains_key(&candidate) {
                    return Some(candidate);
                }
            }
            return None;
        }

        // 6. Legacy bare name — keeps `"theme": "dark"` configs working.
        let normalized = normalize_theme_name(value);
        self.theme_list
            .iter()
            .find(|info| normalize_theme_name(&info.name) == normalized)
            .map(|info| info.key.clone())
    }

    /// Portable config form for the given theme, suitable for persisting to
    /// `config.json` and sharing across machines.
    ///
    /// - Built-ins  → `"builtin://NAME"`
    /// - User theme under themes dir → relative path (`"s-dark.json"` or
    ///   `"nord/dark.json"`)
    /// - User theme outside themes dir → `"file://{abs}"` (user can hand-edit
    ///   to use `${HOME}` / `${XDG_CONFIG_HOME}` if they want)
    /// - URL-packaged theme → repo-URL key kept as-is
    pub fn portable_form(&self, key: &str) -> Option<String> {
        let info = self.theme_list.iter().find(|i| i.key == key)?;

        // Built-in: empty pack, key is just the name.
        if info.pack.is_empty() {
            return Some(format!("builtin://{}", info.name));
        }

        // User theme (file:// key): rewrite to a path relative to themes_dir
        // when possible, so the config is portable across machines.
        if let Some(path_str) = info.key.strip_prefix("file://") {
            let path = std::path::Path::new(path_str);
            if let Some(themes_dir) = self.themes_dir.as_deref() {
                if let Ok(rel) = path.strip_prefix(themes_dir) {
                    // Normalize to forward slashes so the config is
                    // reproducible across OSes.
                    let rel_str = rel
                        .components()
                        .map(|c| c.as_os_str().to_string_lossy())
                        .collect::<Vec<_>>()
                        .join("/");
                    return Some(rel_str);
                }
            }
            // Outside themes dir — keep as absolute file:// URI.
            return Some(info.key.clone());
        }

        // Anything else (URL-packaged themes) is already portable.
        Some(info.key.clone())
    }

    /// List all available themes with metadata.
    pub fn list(&self) -> &[ThemeInfo] {
        &self.theme_list
    }

    /// Get all theme display names.
    pub fn names(&self) -> Vec<String> {
        self.theme_list.iter().map(|t| t.name.clone()).collect()
    }

    /// Check if a theme exists (by key or name).
    pub fn contains(&self, key_or_name: &str) -> bool {
        self.get(key_or_name).is_some()
    }

    /// Number of themes in the registry.
    pub fn len(&self) -> usize {
        self.themes.len()
    }

    /// Check if registry is empty.
    pub fn is_empty(&self) -> bool {
        self.themes.is_empty()
    }

    /// Convert all themes to a JSON map (key → serde_json::Value).
    ///
    /// Keyed by the unique registry key. Each value is the theme data with
    /// added `_key` and `_pack` metadata fields so plugins can distinguish
    /// themes and show display names.
    pub fn to_json_map(&self) -> HashMap<String, serde_json::Value> {
        use super::types::ThemeFile;

        let mut map = HashMap::new();
        for info in &self.theme_list {
            if let Some(theme) = self.themes.get(&info.key) {
                let theme_file: ThemeFile = theme.clone().into();
                if let Ok(mut v) = serde_json::to_value(theme_file) {
                    if let Some(obj) = v.as_object_mut() {
                        obj.insert("_key".to_string(), serde_json::json!(info.key));
                        obj.insert("_pack".to_string(), serde_json::json!(info.pack));
                    }
                    map.insert(info.key.clone(), v);
                }
            }
        }
        map
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

    /// Load all themes (embedded + user + packages + bundle dirs) into a registry.
    ///
    /// Pass `&[]` for `bundle_theme_dirs` if there are no bundle themes.
    /// Each bundle directory should contain a `package.json` with a `fresh.themes`
    /// array (same format as theme packages).
    pub fn load_all(&self, bundle_theme_dirs: &[PathBuf]) -> ThemeRegistry {
        let mut themes = HashMap::new();
        let mut theme_list = Vec::new();

        // Load all embedded themes (key = name for builtins)
        for builtin in BUILTIN_THEMES {
            if let Ok(theme_file) = serde_json::from_str::<ThemeFile>(builtin.json) {
                let theme: Theme = theme_file.into();
                let normalized = normalize_theme_name(builtin.name);
                let info = ThemeInfo::new(&normalized, builtin.pack);
                themes.insert(info.key.clone(), theme);
                theme_list.push(info);
            }
        }

        // Load user themes from ~/.config/fresh/themes/ (recursively)
        if let Some(ref user_dir) = self.user_themes_dir {
            self.scan_directory(user_dir, "user", None, &mut themes, &mut theme_list);
        }

        // Load theme packages from ~/.config/fresh/themes/packages/*/
        if let Some(ref user_dir) = self.user_themes_dir {
            let packages_dir = user_dir.join("packages");
            if packages_dir.exists() {
                if let Ok(entries) = std::fs::read_dir(&packages_dir) {
                    for entry in entries.flatten() {
                        let path = entry.path();
                        if path.is_dir() {
                            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                                if !name.starts_with('.') {
                                    let manifest_path = path.join("package.json");
                                    if manifest_path.exists() {
                                        self.load_package_themes(
                                            &path,
                                            name,
                                            &mut themes,
                                            &mut theme_list,
                                        );
                                    } else {
                                        let pack_name = format!("pkg/{}", name);
                                        self.scan_directory(
                                            &path,
                                            &pack_name,
                                            None,
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

        // Load themes from bundle packages
        for bundle_dir in bundle_theme_dirs {
            if let Some(name) = bundle_dir.file_name().and_then(|n| n.to_str()) {
                let manifest_path = bundle_dir.join("package.json");
                if manifest_path.exists() {
                    self.load_package_themes(bundle_dir, name, &mut themes, &mut theme_list);
                }
            }
        }

        ThemeRegistry {
            themes,
            theme_list,
            themes_dir: self.user_themes_dir.clone(),
        }
    }

    /// Read the `repository` field from a package.json manifest value.
    fn read_repository(manifest: &serde_json::Value) -> Option<String> {
        manifest
            .get("repository")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
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

        let manifest: serde_json::Value = match serde_json::from_str(&manifest_content) {
            Ok(v) => v,
            Err(_) => return,
        };

        let repository = Self::read_repository(&manifest);
        let pack_name = format!("pkg/{}", pkg_name);

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
                                if let Ok(theme) = Theme::from_json(&content) {
                                    let normalized_name = normalize_theme_name(name);
                                    let info = if let Some(ref repo) = repository {
                                        ThemeInfo::with_key(
                                            &normalized_name,
                                            &pack_name,
                                            format!("{}#{}", repo, normalized_name),
                                        )
                                    } else {
                                        ThemeInfo::new(&normalized_name, &pack_name)
                                    };
                                    if !themes.contains_key(&info.key) {
                                        themes.insert(info.key.clone(), theme);
                                        theme_list.push(info);
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
        self.scan_directory(
            pkg_dir,
            &pack_name,
            repository.as_deref(),
            themes,
            theme_list,
        );
    }

    /// Recursively scan a directory for theme files.
    ///
    /// If `repository` is provided (from a package.json), it is used to form
    /// the registry key as `{repository}#{theme_name}`.
    fn scan_directory(
        &self,
        dir: &Path,
        pack: &str,
        repository: Option<&str>,
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

                let new_pack = if pack == "user" {
                    format!("user/{}", subdir_name)
                } else {
                    format!("{}/{}", pack, subdir_name)
                };
                self.scan_directory(&path, &new_pack, repository, themes, theme_list);
            } else if path.extension().is_some_and(|ext| ext == "json") {
                if let Ok(content) = std::fs::read_to_string(&path) {
                    if let Ok(theme) = Theme::from_json(&content) {
                        let name = normalize_theme_name(&theme.name);
                        let info = if let Some(repo) = repository {
                            ThemeInfo::with_key(&name, pack, format!("{}#{}", repo, name))
                        } else if pack.starts_with("user") {
                            // User-saved themes: use file:// URL as key
                            ThemeInfo::with_key(&name, pack, format!("file://{}", path.display()))
                        } else {
                            ThemeInfo::new(&name, pack)
                        };

                        // Only skip exact key duplicates
                        if themes.contains_key(&info.key) {
                            continue;
                        }

                        themes.insert(info.key.clone(), theme);
                        theme_list.push(info);
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
            // Best-effort terminal escape writes
            #[allow(clippy::let_underscore_must_use)]
            let _ = write!(
                std::io::stdout(),
                "\x1b]12;#{:02x}{:02x}{:02x}\x07",
                r,
                g,
                b
            );
            #[allow(clippy::let_underscore_must_use)]
            let _ = std::io::stdout().flush();
        }
    }

    /// Reset the terminal cursor color to default.
    pub fn reset_terminal_cursor_color() {
        use std::io::Write;
        // OSC 112 resets cursor color to default
        // Best-effort terminal escape writes
        #[allow(clippy::let_underscore_must_use)]
        let _ = write!(std::io::stdout(), "\x1b]112\x07");
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::io::stdout().flush();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_theme_registry_get() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all(&[]);

        // Should find builtin themes
        assert!(registry.get("dark").is_some());
        assert!(registry.get("light").is_some());
        assert!(registry.get("high-contrast").is_some());

        // Name normalization: casing, underscores, spaces
        assert!(registry.get("Dark").is_some());
        assert!(registry.get("DARK").is_some());
        assert!(registry.get("high_contrast").is_some());
        assert!(registry.get("high contrast").is_some());

        // Non-existent
        assert!(registry.get("nonexistent-theme").is_none());
    }

    #[test]
    fn test_theme_registry_list() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all(&[]);

        let list = registry.list();
        assert!(list.len() >= 7); // At least the builtin themes

        // Check some expected themes
        assert!(list.iter().any(|t| t.name == "dark"));
        assert!(list.iter().any(|t| t.name == "light"));
    }

    #[test]
    fn test_theme_registry_contains() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all(&[]);

        assert!(registry.contains("dark"));
        assert!(registry.contains("Dark")); // normalized
        assert!(!registry.contains("nonexistent"));
    }

    #[test]
    fn test_theme_loader_load_all() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all(&[]);

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
        let registry = loader.load_all(&[]);

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
        // (the function used for Select Theme menu items).
        // The "theme" arg should be the key (file:// URL for user themes).
        #[cfg(not(target_arch = "wasm32"))]
        {
            let menu_items = crate::config::generate_dynamic_items("copy_with_theme", &themes_dir);
            let theme_keys: Vec<_> = menu_items
                .iter()
                .filter_map(|item| match item {
                    crate::config::MenuItem::Action { args, .. } => args
                        .get("theme")
                        .map(|v| v.as_str().unwrap_or_default().to_string()),
                    _ => None,
                })
                .collect();
            assert!(
                theme_keys.iter().any(|k| k.contains("my-custom-theme")),
                "Custom theme key should appear in dynamic menu items, got: {:?}",
                theme_keys
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
        let registry = loader.load_all(&[]);

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

    #[test]
    fn test_normalize_theme_name() {
        assert_eq!(normalize_theme_name("dark"), "dark");
        assert_eq!(normalize_theme_name("Dark"), "dark");
        assert_eq!(normalize_theme_name("high_contrast"), "high-contrast");
        assert_eq!(normalize_theme_name("Catppuccin Mocha"), "catppuccin-mocha");
        assert_eq!(normalize_theme_name("My_Custom Theme"), "my-custom-theme");
        assert_eq!(normalize_theme_name("SOLARIZED_DARK"), "solarized-dark");
    }

    /// Regression test for #1001: theme whose JSON "name" field differs from the
    /// filename (e.g., filename "catppuccin-mocha.json" but JSON name "Catppuccin Mocha")
    /// should be findable by either name after normalization.
    #[test]
    fn test_theme_name_mismatch_json_vs_filename() {
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let themes_dir = temp_dir.path().to_path_buf();

        // Simulate a theme where the JSON name has spaces/mixed case
        // but the filename uses hyphens (common for community themes)
        let theme_json = r#"{
            "name": "Catppuccin Mocha",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(themes_dir.join("catppuccin-mocha.json"), theme_json)
            .expect("Failed to write theme file");

        let loader = ThemeLoader::new(themes_dir);
        let registry = loader.load_all(&[]);

        // Should be findable by the normalized filename
        assert!(
            registry.contains("catppuccin-mocha"),
            "Theme should be found by normalized filename"
        );

        // Should also be findable by the JSON name (spaces normalized to hyphens)
        assert!(
            registry.contains("Catppuccin Mocha"),
            "Theme should be found by JSON name with spaces (normalized to hyphens)"
        );

        // Should also be findable with mixed casing
        assert!(
            registry.contains("CATPPUCCIN-MOCHA"),
            "Theme should be found regardless of casing"
        );

        // The registry key should be the normalized form
        let theme_list = registry.list();
        let theme_info = theme_list
            .iter()
            .find(|t| t.name == "catppuccin-mocha")
            .expect("Theme should appear with normalized name in theme list");
        assert_eq!(theme_info.pack, "user");
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
        let registry = loader.load_all(&[]);

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

    /// Regression test for #1621: a `config.json` shared via a dotfiles repo
    /// must be able to reference user themes without baking in the absolute
    /// `file://` path of the author's machine. Accepted portable forms:
    ///   - `builtin://NAME` for built-ins (disambiguates against same-named
    ///     user themes)
    ///   - relative paths like `s-dark.json` or `nord/dark.json` resolved
    ///     from the themes dir
    ///   - `file://` URIs with `~` / `${HOME}` / `$VAR` expansion so the
    ///     same config works on any machine
    ///   - legacy bare names (e.g. `"dark"`) keep resolving to a built-in
    #[test]
    fn test_resolve_key_portable_config_forms_for_user_themes() {
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let themes_dir = temp_dir.path().to_path_buf();

        // User theme whose name collides with built-in `dark`.
        let user_dark = r#"{
            "name": "dark",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(themes_dir.join("dark.json"), user_dark)
            .expect("Failed to write user dark theme");

        // User theme in a subdirectory (simulates a shipped theme pack).
        let subdir = themes_dir.join("my-collection");
        std::fs::create_dir_all(&subdir).expect("Failed to create subdir");
        let nested = r#"{
            "name": "s-dark",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(subdir.join("s-dark.json"), nested).expect("Failed to write nested theme");

        let loader = ThemeLoader::new(themes_dir.clone());
        let registry = loader.load_all(&[]);

        // 1. `builtin://dark` disambiguates against the user theme.
        assert_eq!(
            registry.resolve_key("builtin://dark").as_deref(),
            Some("dark"),
            "`builtin://dark` must resolve to the built-in"
        );

        // 2. Relative path resolves against the themes dir — the user theme,
        //    not the built-in.
        let user_key = registry
            .resolve_key("dark.json")
            .expect("`dark.json` should resolve");
        assert!(
            user_key.starts_with("file://") && user_key.ends_with("dark.json"),
            "`dark.json` must resolve to the user theme file, got: {}",
            user_key
        );
        let theme = registry
            .get("dark.json")
            .expect("theme should be retrievable by relative path");
        assert_eq!(theme.name, "dark");

        // 3. Nested relative path with subdirectory.
        let nested_key = registry
            .resolve_key("my-collection/s-dark.json")
            .expect("nested relative path should resolve");
        assert!(
            nested_key.starts_with("file://") && nested_key.contains("my-collection"),
            "nested path should resolve under themes dir, got: {}",
            nested_key
        );

        // 4. `file://` with `${VAR}` expansion: we route through a test-owned
        //    env var so the expansion is exercised deterministically.
        std::env::set_var(
            "FRESH_TEST_THEMES_ROOT",
            themes_dir.to_string_lossy().to_string(),
        );
        let uri = "file://${FRESH_TEST_THEMES_ROOT}/dark.json";
        let resolved = registry
            .resolve_key(uri)
            .expect("env-var-expanded file:// URI should resolve");
        assert_eq!(
            resolved, user_key,
            "env-expanded URI should match user theme"
        );
        std::env::remove_var("FRESH_TEST_THEMES_ROOT");

        // 5. Legacy bare name still resolves to a built-in.
        assert_eq!(
            registry.resolve_key("high-contrast").as_deref(),
            Some("high-contrast"),
            "legacy bare-name config must keep working"
        );

        // 6. Unknown reference yields None (no fuzzy masking of typos).
        assert!(registry.resolve_key("does-not-exist").is_none());
        assert!(registry.resolve_key("builtin://no-such-theme").is_none());
        assert!(registry.resolve_key("missing.json").is_none());
    }

    /// `portable_form` round-trips a theme's registry key into the value
    /// that should be persisted to `config.json`. Resolving the persisted
    /// value must find the same theme again.
    #[test]
    fn test_portable_form_round_trip() {
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let themes_dir = temp_dir.path().to_path_buf();
        let theme_json = r#"{
            "name": "s-dark",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {},
            "syntax": {}
        }"#;
        std::fs::write(themes_dir.join("s-dark.json"), theme_json).expect("Failed to write theme");
        let loader = ThemeLoader::new(themes_dir.clone());
        let registry = loader.load_all(&[]);

        // Built-in portable form is `builtin://NAME`.
        let builtin_portable = registry
            .portable_form("dark")
            .expect("built-in should have a portable form");
        assert_eq!(builtin_portable, "builtin://dark");
        assert_eq!(
            registry.resolve_key(&builtin_portable).as_deref(),
            Some("dark")
        );

        // User-theme portable form is a relative path — never an absolute
        // `file://` URI with the author's home dir.
        let user_info = registry
            .list()
            .iter()
            .find(|i| i.name == "s-dark")
            .expect("user theme should be listed")
            .clone();
        let user_portable = registry
            .portable_form(&user_info.key)
            .expect("user theme should have a portable form");
        assert_eq!(
            user_portable, "s-dark.json",
            "user theme must persist as a relative path, got: {}",
            user_portable
        );
        assert!(
            !user_portable.contains(themes_dir.to_string_lossy().as_ref()),
            "portable form must not embed the absolute themes dir path"
        );
        // And it resolves back to the same key.
        assert_eq!(registry.resolve_key(&user_portable), Some(user_info.key));
    }

    #[test]
    fn test_expand_env_vars() {
        std::env::set_var("FRESH_TEST_VAR_A", "/foo/bar");
        std::env::set_var("FRESH_TEST_VAR_B", "baz");
        assert_eq!(expand_env_vars("${FRESH_TEST_VAR_A}/x"), "/foo/bar/x");
        assert_eq!(expand_env_vars("$FRESH_TEST_VAR_A/x"), "/foo/bar/x");
        assert_eq!(expand_env_vars("a/${FRESH_TEST_VAR_B}/c"), "a/baz/c");
        // Unknown vars remain literal.
        assert_eq!(
            expand_env_vars("${FRESH_NO_SUCH_VAR_XYZ}/x"),
            "${FRESH_NO_SUCH_VAR_XYZ}/x"
        );
        // Unterminated `${` stays literal.
        assert_eq!(expand_env_vars("${oops/x"), "${oops/x");
        // `~` is expanded iff HOME is set (which it is in the test harness).
        if let Ok(home) = std::env::var("HOME") {
            assert_eq!(expand_env_vars("~/foo"), format!("{}/foo", home));
        }
        std::env::remove_var("FRESH_TEST_VAR_A");
        std::env::remove_var("FRESH_TEST_VAR_B");
    }

    #[test]
    fn test_to_json_map() {
        let loader = ThemeLoader::embedded_only();
        let registry = loader.load_all(&[]);

        let json_map = registry.to_json_map();

        // Should contain all themes
        assert_eq!(json_map.len(), registry.len());

        // Each entry should be a valid JSON object with a "name" field
        let dark = json_map
            .get("dark")
            .expect("dark theme should be in json map");
        assert!(dark.is_object(), "theme should serialize to a JSON object");
        assert_eq!(
            dark.get("name").and_then(|v| v.as_str()),
            Some("dark"),
            "theme JSON should have correct name"
        );

        // Should have the expected section keys
        assert!(dark.get("editor").is_some(), "should have editor section");
        assert!(dark.get("ui").is_some(), "should have ui section");
        assert!(dark.get("syntax").is_some(), "should have syntax section");

        // Should have metadata fields
        assert_eq!(
            dark.get("_key").and_then(|v| v.as_str()),
            Some("dark"),
            "theme JSON should have _key metadata"
        );
        assert!(
            dark.get("_pack").is_some(),
            "theme JSON should have _pack metadata"
        );
    }
}
