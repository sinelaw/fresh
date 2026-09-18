//! What a plugin declares about itself *before it runs*: the
//! `<plugin>.manifest.json` sidecar, read synchronously by the host at
//! discovery, next to the `.i18n.json` and `.schema.json` sidecars.
//!
//! Plugin code runs on the plugin thread after every plugin has loaded, so
//! anything the host needs before the first frame — today, that a plugin
//! fills the left dock, and how wide — has to come from here. A manifest is
//! a declaration, not a command: the plugin still mounts the dock from
//! `ready`, and a column held for one that never arrives is handed back
//! (`Editor::dock_reserved`). Only enabled plugins' manifests count.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use fresh_core::config::PluginConfig;

use crate::view::shell::frame::DockWidthRule;

/// The sidecar's suffix: `orchestrator.manifest.json` beside `orchestrator.ts`.
const MANIFEST_SUFFIX: &str = ".manifest.json";

/// Everything a `<plugin>.manifest.json` can say.
#[derive(Clone, Debug, Default, PartialEq, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PluginManifest {
    /// Chrome the plugin will occupy, laid out by the host before it runs.
    #[serde(default)]
    pub chrome: ChromeManifest,
}

#[derive(Clone, Debug, Default, PartialEq, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ChromeManifest {
    /// The plugin fills the editor-global left dock (`PanelSlot::Dock`).
    #[serde(default)]
    pub dock: Option<DockDeclaration>,
}

/// A plugin's claim on the left dock.
#[derive(Clone, Debug, PartialEq, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DockDeclaration {
    /// Whether the dock opens on a first launch, when nothing is remembered
    /// about it (see `Editor::apply_startup_dock_chrome`).
    #[serde(default = "default_true")]
    pub open: bool,
    /// A boolean in this plugin's settings (`plugins.<name>.settings`) that
    /// switches auto-opening off — the orchestrator's `autoOpenDock`.
    #[serde(default)]
    pub open_setting: Option<String>,
    /// How wide the dock opens before the user drags it.
    #[serde(default)]
    pub width: DockWidthRule,
}

fn default_true() -> bool {
    true
}

/// The manifests of every *enabled* plugin found in `dirs`, keyed by plugin
/// name (the stem before `.manifest.json`, as `plugins.<name>` addresses it).
/// The first directory to name a plugin wins, as with plugin loading; a file
/// that does not parse is logged and skipped.
pub fn read_manifests(
    dirs: &[PathBuf],
    plugins: &HashMap<String, PluginConfig>,
) -> HashMap<String, PluginManifest> {
    let mut out: HashMap<String, PluginManifest> = HashMap::new();
    for dir in dirs {
        let Ok(entries) = std::fs::read_dir(dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            let Some(name) = manifest_plugin_name(&path) else {
                continue;
            };
            if out.contains_key(&name) {
                continue;
            }
            if !plugins.get(&name).is_none_or(|c| c.enabled) {
                tracing::debug!("plugin manifest: {name} is disabled, skipping {path:?}");
                continue;
            }
            match read_manifest(&path) {
                Ok(manifest) => {
                    out.insert(name, manifest);
                }
                Err(e) => tracing::warn!("plugin manifest: skipping {path:?}: {e}"),
            }
        }
    }
    out
}

/// `foo.manifest.json` → `foo`; anything else → `None`.
fn manifest_plugin_name(path: &Path) -> Option<String> {
    let file = path.file_name()?.to_str()?;
    let name = file.strip_suffix(MANIFEST_SUFFIX)?;
    (!name.is_empty()).then(|| name.to_string())
}

fn read_manifest(path: &Path) -> Result<PluginManifest, String> {
    let bytes = std::fs::read(path).map_err(|e| e.to_string())?;
    serde_json::from_slice(&bytes).map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dir_with(files: &[(&str, &str)]) -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        for (name, body) in files {
            std::fs::write(dir.path().join(name), body).unwrap();
        }
        dir
    }

    #[test]
    fn a_dock_declaration_reads_with_its_defaults() {
        let dir = dir_with(&[("orchestrator.manifest.json", r#"{"chrome":{"dock":{}}}"#)]);
        let found = read_manifests(&[dir.path().to_path_buf()], &HashMap::new());
        let dock = found["orchestrator"].chrome.dock.as_ref().expect("a dock");
        assert!(dock.open, "open by default");
        assert_eq!(dock.open_setting, None);
        assert_eq!(dock.width, DockWidthRule::default());
    }

    #[test]
    fn only_manifest_files_are_read_and_only_enabled_plugins_count() {
        let dir = dir_with(&[
            (
                "orchestrator.manifest.json",
                r#"{"chrome":{"dock":{"open":false}}}"#,
            ),
            ("orchestrator.i18n.json", r#"{"not":"a manifest"}"#),
            ("orchestrator.ts", "// code"),
            ("off.manifest.json", r#"{"chrome":{"dock":{}}}"#),
            (".manifest.json", r#"{}"#),
        ]);
        let mut plugins = HashMap::new();
        plugins.insert(
            "off".to_string(),
            PluginConfig {
                enabled: false,
                path: None,
                settings: serde_json::Value::Null,
            },
        );
        let found = read_manifests(&[dir.path().to_path_buf()], &plugins);
        assert_eq!(found.len(), 1, "{found:?}");
        assert!(!found["orchestrator"].chrome.dock.as_ref().unwrap().open);
    }

    #[test]
    fn a_broken_manifest_is_skipped_not_fatal() {
        let dir = dir_with(&[
            ("bad.manifest.json", "{ not json"),
            ("typo.manifest.json", r#"{"chrome":{"dokc":{}}}"#),
            ("good.manifest.json", r#"{}"#),
        ]);
        let found = read_manifests(&[dir.path().to_path_buf()], &HashMap::new());
        assert_eq!(found.keys().collect::<Vec<_>>(), vec!["good"]);
        assert_eq!(found["good"].chrome.dock, None);
    }

    #[test]
    fn the_first_directory_wins() {
        let a = dir_with(&[("p.manifest.json", r#"{"chrome":{"dock":{"open":false}}}"#)]);
        let b = dir_with(&[("p.manifest.json", r#"{"chrome":{"dock":{"open":true}}}"#)]);
        let found = read_manifests(
            &[a.path().to_path_buf(), b.path().to_path_buf()],
            &HashMap::new(),
        );
        assert!(!found["p"].chrome.dock.as_ref().unwrap().open);
    }
}
