//! File openers: plugins that show a file type themselves (an image or PDF
//! viewer, say) instead of the editor's text or byte view.
//!
//! A plugin registers an opener with `registerFileOpener`; the registry is
//! shared with the editor, which asks it at open time which opener, if any,
//! a file goes to ([`FileOpenerRegistry::resolve`]). The registry is written
//! synchronously from the plugin's call rather than through the command
//! queue: an open that follows a registration must see it, including the
//! files a launch opens right after the plugins load.

use crate::api::{FileOpenerOptions, FileOpenerPriority};
use std::path::Path;

/// The value of the `file_openers` setting that opens a file type as text,
/// whatever opener claims it.
pub const TEXT_OPENER: &str = "text";

/// An opener a plugin registered.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileOpener {
    /// The plugin that registered it.
    pub plugin_name: String,
    pub id: String,
    pub label: String,
    /// Lowercase, without the dot.
    pub extensions: Vec<String>,
    pub priority: FileOpenerPriority,
    /// The plugin's handler, called with a `FileOpenRequest`.
    pub handler: String,
}

impl FileOpener {
    fn handles(&self, extension: &str) -> bool {
        self.extensions.iter().any(|e| e == extension)
    }
}

/// Why a registration was refused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegisterError {
    /// Another plugin already registered an opener under this id.
    IdTaken { id: String, owner: String },
    /// An empty id, label, handler or extension list.
    Invalid(&'static str),
}

impl std::fmt::Display for RegisterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IdTaken { id, owner } => {
                write!(
                    f,
                    "file opener '{id}' is already registered by plugin '{owner}'"
                )
            }
            Self::Invalid(what) => write!(f, "file opener needs {what}"),
        }
    }
}

/// Every registered opener, in registration order.
#[derive(Debug, Default)]
pub struct FileOpenerRegistry {
    openers: Vec<FileOpener>,
}

impl FileOpenerRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add `plugin`'s opener. Registering an id the same plugin already
    /// registered replaces it (a reload); another plugin's id is refused.
    pub fn register(
        &mut self,
        plugin: &str,
        options: FileOpenerOptions,
    ) -> Result<(), RegisterError> {
        let extensions: Vec<String> = options
            .extensions
            .iter()
            .map(|e| e.trim_start_matches('.').to_lowercase())
            .filter(|e| !e.is_empty())
            .collect();
        if options.id.is_empty() {
            return Err(RegisterError::Invalid("an id"));
        }
        if options.label.is_empty() {
            return Err(RegisterError::Invalid("a label"));
        }
        if options.handler.is_empty() {
            return Err(RegisterError::Invalid("a handler"));
        }
        if extensions.is_empty() {
            return Err(RegisterError::Invalid("at least one extension"));
        }
        if let Some(existing) = self.openers.iter().find(|o| o.id == options.id) {
            if existing.plugin_name != plugin {
                return Err(RegisterError::IdTaken {
                    id: options.id,
                    owner: existing.plugin_name.clone(),
                });
            }
        }
        // A re-registration goes to the end: the latest registration of a
        // default opener is the one that wins (see `resolve`).
        self.openers.retain(|o| o.id != options.id);
        self.openers.push(FileOpener {
            plugin_name: plugin.to_string(),
            id: options.id,
            label: options.label,
            extensions,
            priority: options.priority.unwrap_or_default(),
            handler: options.handler,
        });
        Ok(())
    }

    /// Drop `plugin`'s opener `id`. Whether there was one.
    pub fn unregister(&mut self, plugin: &str, id: &str) -> bool {
        let len = self.openers.len();
        self.openers
            .retain(|o| !(o.plugin_name == plugin && o.id == id));
        self.openers.len() != len
    }

    /// Drop every opener `plugin` registered; how many there were.
    pub fn unregister_plugin(&mut self, plugin: &str) -> usize {
        let len = self.openers.len();
        self.openers.retain(|o| o.plugin_name != plugin);
        len - self.openers.len()
    }

    /// The opener named `id`.
    pub fn get(&self, id: &str) -> Option<&FileOpener> {
        self.openers.iter().find(|o| o.id == id)
    }

    /// The opener `path` opens with, or `None` for the editor's own view.
    ///
    /// `user_choice` is the `file_openers` setting's value for the file's
    /// extension. [`TEXT_OPENER`] keeps the editor's view; the id of a
    /// registered opener picks that opener, whether or not it lists the
    /// extension. Anything else (no setting, or an opener that is not
    /// installed) falls back to the last-registered `default` opener for
    /// the extension.
    pub fn resolve(&self, path: &Path, user_choice: Option<&str>) -> Option<&FileOpener> {
        match user_choice {
            Some(TEXT_OPENER) => return None,
            Some(id) => {
                if let Some(opener) = self.get(id) {
                    return Some(opener);
                }
            }
            None => {}
        }
        let extension = extension_key(path)?;
        self.openers
            .iter()
            .rev()
            .find(|o| o.priority == FileOpenerPriority::Default && o.handles(&extension))
    }
}

/// The key `path` has in the `file_openers` setting and in openers'
/// extension lists: its extension, lowercase, without the dot.
pub fn extension_key(path: &Path) -> Option<String> {
    path.extension()
        .and_then(|e| e.to_str())
        .filter(|e| !e.is_empty())
        .map(str::to_lowercase)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn options(
        id: &str,
        extensions: &[&str],
        priority: Option<FileOpenerPriority>,
    ) -> FileOpenerOptions {
        FileOpenerOptions {
            id: id.to_string(),
            label: format!("{id} label"),
            extensions: extensions.iter().map(|e| e.to_string()).collect(),
            priority,
            handler: format!("{id}_handler"),
        }
    }

    fn resolved<'a>(
        registry: &'a FileOpenerRegistry,
        path: &str,
        choice: Option<&str>,
    ) -> Option<&'a str> {
        registry
            .resolve(Path::new(path), choice)
            .map(|o| o.id.as_str())
    }

    #[test]
    fn a_default_opener_takes_its_extensions_ignoring_case() {
        let mut registry = FileOpenerRegistry::new();
        registry
            .register("pdf_plugin", options("pdf", &[".PDF"], None))
            .unwrap();
        assert_eq!(resolved(&registry, "/doc/paper.pdf", None), Some("pdf"));
        assert_eq!(resolved(&registry, "/doc/PAPER.Pdf", None), Some("pdf"));
        assert_eq!(resolved(&registry, "/doc/notes.txt", None), None);
        assert_eq!(resolved(&registry, "/doc/Makefile", None), None);
    }

    #[test]
    fn an_option_opener_is_used_only_when_chosen() {
        let mut registry = FileOpenerRegistry::new();
        registry
            .register(
                "p",
                options("hex", &["png"], Some(FileOpenerPriority::Option)),
            )
            .unwrap();
        assert_eq!(resolved(&registry, "a.png", None), None);
        assert_eq!(resolved(&registry, "a.png", Some("hex")), Some("hex"));
    }

    #[test]
    fn the_setting_overrides_the_default_either_way() {
        let mut registry = FileOpenerRegistry::new();
        registry
            .register("a", options("image", &["png"], None))
            .unwrap();
        registry
            .register("b", options("other", &["jpg"], None))
            .unwrap();
        // "text" keeps the editor's own view.
        assert_eq!(resolved(&registry, "a.png", Some(TEXT_OPENER)), None);
        // A registered opener applies even to an extension it does not list.
        assert_eq!(resolved(&registry, "a.png", Some("other")), Some("other"));
        // A missing opener falls back to the default.
        assert_eq!(resolved(&registry, "a.png", Some("gone")), Some("image"));
    }

    #[test]
    fn the_last_default_registered_wins() {
        let mut registry = FileOpenerRegistry::new();
        registry
            .register("a", options("first", &["png"], None))
            .unwrap();
        registry
            .register("b", options("second", &["png"], None))
            .unwrap();
        assert_eq!(resolved(&registry, "a.png", None), Some("second"));
        // Re-registering moves an opener to the end.
        registry
            .register("a", options("first", &["png"], None))
            .unwrap();
        assert_eq!(resolved(&registry, "a.png", None), Some("first"));
    }

    #[test]
    fn another_plugins_id_is_refused_and_bad_options_are_rejected() {
        let mut registry = FileOpenerRegistry::new();
        registry
            .register("a", options("viewer", &["png"], None))
            .unwrap();
        assert_eq!(
            registry.register("b", options("viewer", &["png"], None)),
            Err(RegisterError::IdTaken {
                id: "viewer".into(),
                owner: "a".into()
            })
        );
        assert!(matches!(
            registry.register("a", options("v2", &[], None)),
            Err(RegisterError::Invalid(_))
        ));
        assert!(matches!(
            registry.register("a", options("v3", &["."], None)),
            Err(RegisterError::Invalid(_))
        ));
        assert!(matches!(
            registry.register("a", options("", &["png"], None)),
            Err(RegisterError::Invalid(_))
        ));
    }

    #[test]
    fn unregistering_drops_only_the_plugins_own_openers() {
        let mut registry = FileOpenerRegistry::new();
        registry
            .register("a", options("one", &["png"], None))
            .unwrap();
        registry
            .register("a", options("two", &["jpg"], None))
            .unwrap();
        registry
            .register("b", options("three", &["gif"], None))
            .unwrap();
        assert!(!registry.unregister("b", "one"));
        assert!(registry.unregister("a", "one"));
        assert_eq!(resolved(&registry, "x.png", None), None);
        assert_eq!(registry.unregister_plugin("a"), 1);
        assert_eq!(resolved(&registry, "x.jpg", None), None);
        assert_eq!(resolved(&registry, "x.gif", None), Some("three"));
    }
}
