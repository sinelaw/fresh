//! Theme loading with I/O abstraction.
//!
//! This module provides the `ThemeLoader` trait for loading themes from various sources,
//! and `LocalThemeLoader` as the default filesystem-based implementation.

use std::path::{Path, PathBuf};

use super::types::{Theme, ThemeFile, BUILTIN_THEMES};

/// Trait for loading theme files from various sources.
///
/// This abstraction allows:
/// - Testing with mock implementations
/// - WASM builds with fetch-based loaders
/// - Custom theme sources (network, embedded, etc.)
pub trait ThemeLoader: Send + Sync {
    /// Load theme JSON content by name.
    /// Returns None if theme doesn't exist.
    fn load_theme(&self, name: &str) -> Option<String>;

    /// List all available theme names from this loader.
    fn available_themes(&self) -> Vec<String>;

    /// Check if a theme exists by name.
    fn theme_exists(&self, name: &str) -> bool {
        self.load_theme(name).is_some()
    }
}

/// Default implementation using local filesystem.
///
/// Searches for themes in:
/// 1. User themes directory (~/.config/fresh/themes/)
/// 2. Built-in themes directory (themes/ relative paths)
pub struct LocalThemeLoader {
    user_themes_dir: Option<PathBuf>,
}

impl LocalThemeLoader {
    /// Create a new LocalThemeLoader with default directories.
    pub fn new() -> Self {
        Self {
            user_themes_dir: dirs::config_dir().map(|p| p.join("fresh").join("themes")),
        }
    }

    /// Create a LocalThemeLoader with a custom user themes directory.
    pub fn with_user_dir(user_themes_dir: Option<PathBuf>) -> Self {
        Self { user_themes_dir }
    }

    /// Get the user themes directory path.
    pub fn user_themes_dir(&self) -> Option<&Path> {
        self.user_themes_dir.as_deref()
    }

    /// Try to load a theme from a specific file path.
    fn load_from_path(path: &Path) -> Option<String> {
        std::fs::read_to_string(path).ok()
    }

    /// Get paths to search for a theme by name.
    fn theme_paths(&self, name: &str) -> Vec<PathBuf> {
        let mut paths = Vec::new();

        // User themes directory (highest priority)
        if let Some(ref user_dir) = self.user_themes_dir {
            paths.push(user_dir.join(format!("{}.json", name)));
        }

        // Built-in themes directory (various relative paths for development)
        paths.extend([
            PathBuf::from(format!("themes/{}.json", name)),
            PathBuf::from(format!("../themes/{}.json", name)),
            PathBuf::from(format!("../../themes/{}.json", name)),
        ]);

        paths
    }
}

impl Default for LocalThemeLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl ThemeLoader for LocalThemeLoader {
    fn load_theme(&self, name: &str) -> Option<String> {
        for path in self.theme_paths(name) {
            if let Some(content) = Self::load_from_path(&path) {
                return Some(content);
            }
        }
        None
    }

    fn available_themes(&self) -> Vec<String> {
        let mut themes = Vec::new();

        // Scan built-in themes directory
        if let Ok(entries) = std::fs::read_dir("themes") {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().is_some_and(|ext| ext == "json") {
                    if let Some(stem) = path.file_stem() {
                        themes.push(stem.to_string_lossy().to_string());
                    }
                }
            }
        }

        // Scan user themes directory
        if let Some(ref user_dir) = self.user_themes_dir {
            if let Ok(entries) = std::fs::read_dir(user_dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().is_some_and(|ext| ext == "json") {
                        if let Some(stem) = path.file_stem() {
                            let name = stem.to_string_lossy().to_string();
                            if !themes.contains(&name) {
                                themes.push(name);
                            }
                        }
                    }
                }
            }
        }

        themes
    }
}

// Extension methods on Theme that use ThemeLoader
impl Theme {
    /// Load a theme from a JSON file path.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, String> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| format!("Failed to read theme file: {}", e))?;
        let theme_file: ThemeFile = serde_json::from_str(&content)
            .map_err(|e| format!("Failed to parse theme file: {}", e))?;
        Ok(theme_file.into())
    }

    /// Load theme by name using a ThemeLoader.
    /// First checks builtin themes (embedded), then uses loader for filesystem themes.
    pub fn load(name: &str, loader: &dyn ThemeLoader) -> Option<Self> {
        let normalized = name.to_lowercase().replace('_', "-");

        // Try builtin first (no I/O)
        if let Some(theme) = Self::load_builtin(&normalized) {
            return Some(theme);
        }

        // Try loader
        loader
            .load_theme(&normalized)
            .and_then(|json| Self::from_json(&json).ok())
    }

    /// Get all available themes (builtin + from loader).
    pub fn all_available(loader: &dyn ThemeLoader) -> Vec<String> {
        let mut themes: Vec<String> = BUILTIN_THEMES.iter().map(|t| t.name.to_string()).collect();

        for name in loader.available_themes() {
            if !themes.contains(&name) {
                themes.push(name);
            }
        }

        themes
    }

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
    use std::collections::HashMap;

    /// Mock theme loader for testing
    struct MockThemeLoader {
        themes: HashMap<String, String>,
    }

    impl MockThemeLoader {
        fn new() -> Self {
            Self {
                themes: HashMap::new(),
            }
        }

        fn with_theme(mut self, name: &str, json: &str) -> Self {
            self.themes.insert(name.to_string(), json.to_string());
            self
        }
    }

    impl ThemeLoader for MockThemeLoader {
        fn load_theme(&self, name: &str) -> Option<String> {
            self.themes.get(name).cloned()
        }

        fn available_themes(&self) -> Vec<String> {
            self.themes.keys().cloned().collect()
        }
    }

    #[test]
    fn test_mock_theme_loader() {
        let loader = MockThemeLoader::new().with_theme(
            "custom",
            r#"{"name":"custom","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
        );

        assert!(loader.theme_exists("custom"));
        assert!(!loader.theme_exists("nonexistent"));

        let themes = loader.available_themes();
        assert!(themes.contains(&"custom".to_string()));
    }

    #[test]
    fn test_theme_load_with_mock() {
        let loader = MockThemeLoader::new().with_theme(
            "test-theme",
            r#"{"name":"test-theme","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
        );

        let theme = Theme::load("test-theme", &loader);
        assert!(theme.is_some());
        assert_eq!(theme.unwrap().name, "test-theme");
    }

    #[test]
    fn test_theme_load_builtin_priority() {
        // Builtin themes should be loaded even if loader doesn't have them
        let loader = MockThemeLoader::new();

        let theme = Theme::load("dark", &loader);
        assert!(theme.is_some());
        assert_eq!(theme.unwrap().name, "dark");
    }

    #[test]
    fn test_load_with_loader() {
        // load should work for builtin themes with any loader
        let loader = LocalThemeLoader::new();
        let theme = Theme::load("dark", &loader);
        assert!(theme.is_some());
        assert_eq!(theme.unwrap().name, "dark");

        let theme = Theme::load("light", &loader);
        assert!(theme.is_some());
        assert_eq!(theme.unwrap().name, "light");
    }

    #[test]
    fn test_all_available_themes() {
        let loader = LocalThemeLoader::new();
        let themes = Theme::all_available(&loader);
        // Should have at least the builtin themes
        assert!(themes.len() >= 4);
        assert!(themes.contains(&"dark".to_string()));
        assert!(themes.contains(&"light".to_string()));
        assert!(themes.contains(&"high-contrast".to_string()));
        assert!(themes.contains(&"nostalgia".to_string()));
    }
}
