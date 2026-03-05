//! Plugin development workspace for LSP support
//!
//! When a user loads a plugin from a buffer, this module creates a temporary
//! workspace directory containing:
//! - The buffer content as a `.ts` file
//! - A copy of `fresh.d.ts` for type definitions
//! - A `tsconfig.json` configured for the plugin environment
//!
//! This allows `typescript-language-server` to provide autocomplete, type checking,
//! and hover documentation for plugin buffers — including unsaved/unnamed buffers.

use std::path::{Path, PathBuf};

/// Manages a temporary workspace for plugin development LSP support.
pub struct PluginDevWorkspace {
    /// Root directory of the workspace (e.g., `~/.cache/fresh/plugin-dev/{buffer_id}/`)
    dir: PathBuf,
    /// Path to the plugin source file within the workspace
    pub plugin_file: PathBuf,
}

/// The tsconfig.json content for plugin development.
/// - No DOM lib (plugins run in QuickJS, not a browser)
/// - `types: []` prevents picking up @types/node or other ambient types
/// - `skipLibCheck: true` avoids checking fresh.d.ts itself
const TSCONFIG_CONTENT: &str = r#"{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "node",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true,
    "lib": ["ES2020"],
    "types": []
  },
  "files": ["fresh.d.ts", "plugin.ts"]
}
"#;

impl PluginDevWorkspace {
    /// Create a new plugin dev workspace for the given buffer.
    ///
    /// This creates a temp directory, copies `fresh.d.ts` from the source,
    /// writes `tsconfig.json`, and writes the buffer content to `plugin.ts`.
    ///
    /// # Arguments
    /// * `buffer_id` - The buffer ID (used to create a unique directory)
    /// * `content` - The current buffer content
    /// * `fresh_dts_source` - Path to the `fresh.d.ts` file to copy
    pub fn create(
        buffer_id: usize,
        content: &str,
        fresh_dts_source: &Path,
    ) -> Result<Self, String> {
        let cache_dir =
            dirs::cache_dir().ok_or_else(|| "Could not determine cache directory".to_string())?;
        let dir = cache_dir
            .join("fresh")
            .join("plugin-dev")
            .join(buffer_id.to_string());

        // Create directory
        std::fs::create_dir_all(&dir)
            .map_err(|e| format!("Failed to create plugin dev workspace: {}", e))?;

        // Copy fresh.d.ts
        let dest_dts = dir.join("fresh.d.ts");
        std::fs::copy(fresh_dts_source, &dest_dts)
            .map_err(|e| format!("Failed to copy fresh.d.ts: {}", e))?;

        // Write tsconfig.json
        let tsconfig_path = dir.join("tsconfig.json");
        std::fs::write(&tsconfig_path, TSCONFIG_CONTENT)
            .map_err(|e| format!("Failed to write tsconfig.json: {}", e))?;

        // Write buffer content as plugin.ts
        let plugin_file = dir.join("plugin.ts");
        std::fs::write(&plugin_file, content)
            .map_err(|e| format!("Failed to write plugin.ts: {}", e))?;

        tracing::info!(
            "Created plugin dev workspace at {:?} for buffer {}",
            dir,
            buffer_id
        );

        Ok(Self { dir, plugin_file })
    }

    /// Get the path to the workspace directory.
    pub fn dir(&self) -> &Path {
        &self.dir
    }

    /// Clean up the workspace directory.
    pub fn cleanup(&self) {
        if self.dir.exists() {
            if let Err(e) = std::fs::remove_dir_all(&self.dir) {
                tracing::warn!(
                    "Failed to clean up plugin dev workspace {:?}: {}",
                    self.dir,
                    e
                );
            } else {
                tracing::debug!("Cleaned up plugin dev workspace {:?}", self.dir);
            }
        }
    }
}

impl Drop for PluginDevWorkspace {
    fn drop(&mut self) {
        self.cleanup();
    }
}
