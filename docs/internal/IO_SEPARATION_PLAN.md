# I/O Separation Refactoring Plan

## Overview

This document outlines the plan to separate pure data types from I/O operations in the Fresh codebase. The goal is to improve modularity, testability, and enable future WASM compatibility.

**Related:** This continues the work started in PR #688 which introduced similar patterns.

## Motivation

1. **WASM Compatibility**: Pure types without `std::fs` can compile to WASM
2. **Testability**: I/O traits allow mock implementations for unit tests
3. **Modularity**: Clear separation of concerns with explicit contracts
4. **Flexibility**: Different backends (local FS, network, virtual) can be swapped at runtime

## Current State

### Already Completed (from PR #688)
- `Action` and `KeyContext` extracted to `fresh-core/src/action.rs`
- `CursorId`, `BufferId`, `SplitId` moved to `fresh-core/src/lib.rs`
- `FsBackend` trait exists in `services/fs/backend.rs`

### Needs Refactoring

#### 1. Theme Module (`view/theme.rs` - 1314 lines)
Currently mixes:
- **Pure types** (lines 1-760): `ColorDef`, `ThemeFile`, `EditorColors`, `UiColors`, `SearchColors`, `DiagnosticColors`, `SyntaxColors`, `Theme`
- **I/O operations** (lines 1104-1196): `from_file()`, `load_builtin_theme()`, `from_name()`, `available_themes()`

#### 2. Grammar Registry (`primitives/grammar_registry.rs` - 646 lines)
Currently mixes:
- **Pure types**: `GrammarRegistry` struct, syntax lookup methods
- **I/O operations**: `load()`, `load_user_grammars_into()`, `parse_package_json()`, `load_direct_grammar()`

## Design

### Phase 1: Theme Module with `ThemeLoader` Trait

#### Directory Structure
```
crates/fresh-editor/src/view/theme/
├── mod.rs          # Re-exports for backward compatibility
├── types.rs        # Pure types (WASM-compatible, no std::fs)
└── loader.rs       # ThemeLoader trait + LocalThemeLoader
```

#### `theme/types.rs` - Pure Types (No I/O)
```rust
// Color types
pub enum ColorDef { Rgb(u8, u8, u8), Named(String) }
impl From<ColorDef> for Color { ... }
impl From<Color> for ColorDef { ... }

// Theme file structure (JSON serialization)
pub struct ThemeFile { ... }
pub struct EditorColors { ... }
pub struct UiColors { ... }
pub struct SearchColors { ... }
pub struct DiagnosticColors { ... }
pub struct SyntaxColors { ... }

// Runtime theme (converted from ThemeFile)
pub struct Theme { ... }
impl From<ThemeFile> for Theme { ... }
impl From<Theme> for ThemeFile { ... }

// Embedded builtin themes (no I/O - compiled in)
pub const BUILTIN_THEMES: &[BuiltinTheme] = &[ ... ];

// Helper functions
pub fn color_to_rgb(color: Color) -> Option<(u8, u8, u8)> { ... }
fn brighten_color(color: Color, amount: u8) -> Color { ... }

// Theme methods that don't require I/O
impl Theme {
    /// Load builtin theme from embedded JSON (no I/O)
    pub fn load_builtin(name: &str) -> Option<Self> { ... }

    /// Parse theme from JSON string (no I/O)
    pub fn from_json(json: &str) -> Result<Self, String> { ... }
}
```

#### `theme/loader.rs` - I/O Trait and Implementation
```rust
use std::path::Path;

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

    /// List all available theme names.
    fn available_themes(&self) -> Vec<String>;

    /// Check if a theme exists by name.
    fn theme_exists(&self, name: &str) -> bool;
}

/// Default implementation using local filesystem.
///
/// Searches for themes in:
/// 1. User themes directory (~/.config/fresh/themes/)
/// 2. Built-in themes directory (themes/)
pub struct LocalThemeLoader {
    user_themes_dir: Option<PathBuf>,
    builtin_themes_dir: Option<PathBuf>,
}

impl LocalThemeLoader {
    pub fn new() -> Self { ... }
    pub fn with_dirs(user_dir: Option<PathBuf>, builtin_dir: Option<PathBuf>) -> Self { ... }
}

impl Default for LocalThemeLoader {
    fn default() -> Self { Self::new() }
}

impl ThemeLoader for LocalThemeLoader {
    fn load_theme(&self, name: &str) -> Option<String> {
        // 1. Check user themes directory
        // 2. Check builtin themes directory
        // 3. Return None if not found
    }

    fn available_themes(&self) -> Vec<String> {
        // Scan both directories for .json files
    }

    fn theme_exists(&self, name: &str) -> bool {
        self.load_theme(name).is_some()
    }
}

// Extension methods on Theme that require I/O
impl Theme {
    /// Load theme by name using a ThemeLoader.
    /// First checks builtin themes (embedded), then uses loader.
    pub fn load(name: &str, loader: &dyn ThemeLoader) -> Option<Self> {
        let normalized = name.to_lowercase().replace('_', "-");

        // Try builtin first (no I/O)
        if let Some(theme) = Self::load_builtin(&normalized) {
            return Some(theme);
        }

        // Try loader
        loader.load_theme(&normalized)
            .and_then(|json| Self::from_json(&json).ok())
    }

    /// Get all available themes (builtin + from loader).
    pub fn all_available(loader: &dyn ThemeLoader) -> Vec<String> {
        let mut themes: Vec<String> = BUILTIN_THEMES
            .iter()
            .map(|t| t.name.to_string())
            .collect();

        for name in loader.available_themes() {
            if !themes.contains(&name) {
                themes.push(name);
            }
        }

        themes
    }

    /// Set terminal cursor color (terminal I/O).
    pub fn set_terminal_cursor_color(&self) { ... }

    /// Reset terminal cursor color (terminal I/O).
    pub fn reset_terminal_cursor_color() { ... }
}
```

#### `theme/mod.rs` - Re-exports
```rust
mod types;
mod loader;

pub use types::*;
pub use loader::*;
```

### Phase 2: Grammar Module with `GrammarLoader` Trait

#### Directory Structure
```
crates/fresh-editor/src/primitives/grammar/
├── mod.rs          # Re-exports
├── types.rs        # Pure types and lookup methods
└── loader.rs       # GrammarLoader trait + LocalGrammarLoader
```

#### `grammar/types.rs` - Pure Types
```rust
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use syntect::parsing::{SyntaxReference, SyntaxSet};

/// Embedded grammars (compiled in, no I/O)
pub const TOML_GRAMMAR: &str = include_str!("../../grammars/toml.sublime-syntax");
pub const ODIN_GRAMMAR: &str = include_str!("../../grammars/odin/Odin.sublime-syntax");

/// Registry of all available TextMate grammars.
///
/// This struct holds the compiled syntax set and provides
/// lookup methods. It does not perform I/O directly.
pub struct GrammarRegistry {
    syntax_set: Arc<SyntaxSet>,
    user_extensions: HashMap<String, String>,
    filename_scopes: HashMap<String, String>,
}

impl GrammarRegistry {
    /// Create from pre-built components (no I/O).
    pub fn new(
        syntax_set: SyntaxSet,
        user_extensions: HashMap<String, String>,
        filename_scopes: HashMap<String, String>,
    ) -> Self { ... }

    /// Create empty registry (for tests that don't need highlighting).
    pub fn empty() -> Arc<Self> { ... }

    // All lookup methods (no I/O)
    pub fn find_syntax_for_file(&self, path: &Path) -> Option<&SyntaxReference> { ... }
    pub fn find_syntax_by_scope(&self, scope: &str) -> Option<&SyntaxReference> { ... }
    pub fn find_syntax_by_name(&self, name: &str) -> Option<&SyntaxReference> { ... }
    pub fn find_syntax_by_first_line(&self, line: &str) -> Option<&SyntaxReference> { ... }
    pub fn syntax_set(&self) -> &Arc<SyntaxSet> { ... }
    pub fn available_syntaxes(&self) -> Vec<&str> { ... }
    // ... other lookup methods
}

// Manifest types for VSCode extension format
#[derive(Debug, Deserialize)]
pub struct PackageManifest {
    pub contributes: Option<Contributes>,
}

#[derive(Debug, Deserialize, Default)]
pub struct Contributes {
    pub languages: Vec<LanguageContribution>,
    pub grammars: Vec<GrammarContribution>,
}

#[derive(Debug, Deserialize)]
pub struct LanguageContribution {
    pub id: String,
    pub extensions: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct GrammarContribution {
    pub language: String,
    #[serde(rename = "scopeName")]
    pub scope_name: String,
    pub path: String,
}
```

#### `grammar/loader.rs` - I/O Trait and Implementation
```rust
use std::io;
use std::path::{Path, PathBuf};

/// Trait for loading grammar files from various sources.
pub trait GrammarLoader: Send + Sync {
    /// Get the user grammars directory path.
    fn grammars_dir(&self) -> Option<PathBuf>;

    /// List subdirectories in the grammars directory.
    fn list_grammar_dirs(&self) -> io::Result<Vec<PathBuf>>;

    /// Read file contents as string.
    fn read_file(&self, path: &Path) -> io::Result<String>;

    /// List files in a directory.
    fn list_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>>;

    /// Check if path exists.
    fn exists(&self, path: &Path) -> bool;

    /// Check if path is a directory.
    fn is_dir(&self, path: &Path) -> bool;
}

/// Default implementation using local filesystem.
pub struct LocalGrammarLoader {
    config_dir: Option<PathBuf>,
}

impl LocalGrammarLoader {
    pub fn new() -> Self {
        Self {
            config_dir: dirs::config_dir(),
        }
    }

    pub fn with_config_dir(config_dir: Option<PathBuf>) -> Self {
        Self { config_dir }
    }
}

impl Default for LocalGrammarLoader {
    fn default() -> Self { Self::new() }
}

impl GrammarLoader for LocalGrammarLoader {
    fn grammars_dir(&self) -> Option<PathBuf> {
        self.config_dir.as_ref().map(|p| p.join("fresh/grammars"))
    }

    fn list_grammar_dirs(&self) -> io::Result<Vec<PathBuf>> {
        let dir = self.grammars_dir().ok_or_else(|| {
            io::Error::new(io::ErrorKind::NotFound, "No grammars directory")
        })?;

        let mut dirs = Vec::new();
        for entry in std::fs::read_dir(dir)? {
            let path = entry?.path();
            if path.is_dir() {
                dirs.push(path);
            }
        }
        Ok(dirs)
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        std::fs::read_to_string(path)
    }

    fn list_dir(&self, path: &Path) -> io::Result<Vec<PathBuf>> {
        let mut files = Vec::new();
        for entry in std::fs::read_dir(path)? {
            files.push(entry?.path());
        }
        Ok(files)
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }
}

// Builder/factory methods that use GrammarLoader
impl GrammarRegistry {
    /// Load grammar registry using a GrammarLoader.
    pub fn load(loader: &dyn GrammarLoader) -> Self {
        let mut user_extensions = HashMap::new();
        let defaults = SyntaxSet::load_defaults_newlines();
        let mut builder = defaults.into_builder();

        // Add embedded grammars
        Self::add_embedded_grammars(&mut builder);

        // Add user grammars via loader
        if let Some(grammars_dir) = loader.grammars_dir() {
            if loader.exists(&grammars_dir) {
                Self::load_user_grammars(loader, &grammars_dir, &mut builder, &mut user_extensions);
            }
        }

        let syntax_set = builder.build();
        let filename_scopes = Self::build_filename_scopes();

        Self::new(syntax_set, user_extensions, filename_scopes)
    }

    /// Create fully-loaded registry for the editor.
    pub fn for_editor() -> Arc<Self> {
        Arc::new(Self::load(&LocalGrammarLoader::default()))
    }

    // Private helper that uses loader
    fn load_user_grammars(
        loader: &dyn GrammarLoader,
        dir: &Path,
        builder: &mut SyntaxSetBuilder,
        user_extensions: &mut HashMap<String, String>,
    ) { ... }
}
```

## Migration Strategy

### Step 1: Create New Module Structure
1. Create `view/theme/` directory with `mod.rs`, `types.rs`, `loader.rs`
2. Create `primitives/grammar/` directory with `mod.rs`, `types.rs`, `loader.rs`

### Step 2: Move Code
1. Move pure types to `types.rs` files
2. Move I/O code to `loader.rs` files
3. Create trait definitions
4. Implement `Local*Loader` structs

### Step 3: Update Imports
1. Add re-exports in `mod.rs` for backward compatibility
2. Update imports across codebase
3. Most imports should work unchanged due to re-exports

### Step 4: Update Call Sites
1. `Theme::from_name(name)` → `Theme::load(name, &LocalThemeLoader::default())`
   - Or keep `from_name` as convenience method that uses default loader
2. `GrammarRegistry::load()` → `GrammarRegistry::load(&LocalGrammarLoader::default())`
   - `for_editor()` already does this

### Step 5: Testing
1. Run full test suite
2. Add tests using mock loaders
3. Verify theme loading works
4. Verify grammar loading works

## Backward Compatibility

To minimize disruption, we'll maintain backward-compatible APIs:

```rust
// In theme/loader.rs - convenience method
impl Theme {
    /// Load theme by name (convenience method using default loader).
    /// Equivalent to `Theme::load(name, &LocalThemeLoader::default())`.
    pub fn from_name(name: &str) -> Option<Self> {
        Self::load(name, &LocalThemeLoader::default())
    }

    /// Get all available themes (convenience method using default loader).
    pub fn available_themes() -> Vec<String> {
        Self::all_available(&LocalThemeLoader::default())
    }
}
```

## Future Possibilities

With this abstraction in place, we can later add:

1. **WASM Loaders**: Load themes/grammars via fetch or from bundled data
2. **Network Loaders**: Load from remote servers
3. **Cached Loaders**: Wrap loaders with caching layer
4. **Composite Loaders**: Chain multiple loaders with fallback

## Testing

### Mock Implementations for Tests
```rust
#[cfg(test)]
mod tests {
    struct MockThemeLoader {
        themes: HashMap<String, String>,
    }

    impl ThemeLoader for MockThemeLoader {
        fn load_theme(&self, name: &str) -> Option<String> {
            self.themes.get(name).cloned()
        }

        fn available_themes(&self) -> Vec<String> {
            self.themes.keys().cloned().collect()
        }

        fn theme_exists(&self, name: &str) -> bool {
            self.themes.contains_key(name)
        }
    }

    #[test]
    fn test_theme_loading_with_mock() {
        let mut loader = MockThemeLoader { themes: HashMap::new() };
        loader.themes.insert("test".to_string(), r#"{"name":"test",...}"#.to_string());

        let theme = Theme::load("test", &loader);
        assert!(theme.is_some());
    }
}
```

## Timeline

This refactoring can be done incrementally:
1. Theme module split (independent)
2. Grammar module split (independent)
3. Both can be done in parallel if needed

## References

- PR #688: Original IO separation work
- `services/fs/backend.rs`: Existing `FsBackend` trait pattern
- `fresh-core/src/action.rs`: Example of extracted pure types
