//! Runtime backend for rust-i18n that loads translations from embedded JSON files.
//!
//! This backend replaces the compile-time macro expansion with runtime JSON parsing,
//! significantly reducing compiler memory usage while maintaining the same functionality.

use once_cell::sync::Lazy;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::RwLock;

/// Embedded locale JSON files (same binary size as macro approach)
const EMBEDDED_LOCALES: &[(&str, &str)] = &[
    (
        "bg",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/bg.json")),
    ),
    (
        "cs",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/cs.json")),
    ),
    (
        "de",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/de.json")),
    ),
    (
        "en",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/en.json")),
    ),
    (
        "es",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/es.json")),
    ),
    (
        "fr",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/fr.json")),
    ),
    (
        "it",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/it.json")),
    ),
    (
        "ja",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/ja.json")),
    ),
    (
        "ko",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/ko.json")),
    ),
    (
        "pt-BR",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/pt-BR.json")),
    ),
    (
        "ru",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/ru.json")),
    ),
    (
        "th",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/th.json")),
    ),
    (
        "uk",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/uk.json")),
    ),
    (
        "vi",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/vi.json")),
    ),
    (
        "zh-CN",
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/locales/zh-CN.json")),
    ),
];

/// Parsed translations storage with leaked strings for 'static lifetime
/// We leak the strings once during parsing to get &'static str references
static TRANSLATIONS: Lazy<RwLock<HashMap<String, HashMap<&'static str, &'static str>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Parse and flatten a locale's JSON, leaking strings for 'static lifetime
fn parse_locale(json_str: &str) -> HashMap<&'static str, &'static str> {
    let value: Value = serde_json::from_str(json_str).expect("Valid JSON");
    let mut flat = HashMap::new();
    flatten_json(&value, String::new(), &mut flat);
    flat
}

/// Recursively flatten nested JSON with dot notation, leaking strings
fn flatten_json(value: &Value, prefix: String, output: &mut HashMap<&'static str, &'static str>) {
    match value {
        Value::Object(map) => {
            for (key, val) in map {
                if key.starts_with('_') {
                    continue; // Skip metadata like _version
                }
                let new_prefix = if prefix.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", prefix, key)
                };
                flatten_json(val, new_prefix, output);
            }
        }
        Value::String(s) => {
            // Leak both key and value for 'static lifetime
            // This is acceptable because:
            // 1. Translations are loaded once per locale
            // 2. They live for the entire program lifetime
            // 3. Total size is ~1.1MB for all locales
            let key_static: &'static str = Box::leak(prefix.into_boxed_str());
            let val_static: &'static str = Box::leak(s.clone().into_boxed_str());
            output.insert(key_static, val_static);
        }
        _ => {}
    }
}

/// Ensure a locale is loaded (lazy loading)
fn ensure_loaded(locale: &str) {
    if TRANSLATIONS.read().unwrap().contains_key(locale) {
        return;
    }

    let mut translations = TRANSLATIONS.write().unwrap();
    // Re-check after acquiring write lock to avoid double-load
    if translations.contains_key(locale) {
        return;
    }

    if let Some((_, json)) = EMBEDDED_LOCALES.iter().find(|(l, _)| *l == locale) {
        translations.insert(locale.to_string(), parse_locale(json));
    }
}

/// Runtime backend for rust-i18n
pub struct RuntimeBackend;

impl RuntimeBackend {
    pub fn new() -> Self {
        Self
    }
}

impl rust_i18n::Backend for RuntimeBackend {
    fn available_locales(&self) -> Vec<&str> {
        EMBEDDED_LOCALES.iter().map(|(l, _)| *l).collect()
    }

    fn translate(&self, locale: &str, key: &str) -> Option<&str> {
        ensure_loaded(locale);
        let translations = TRANSLATIONS.read().unwrap();
        translations.get(locale)?.get(key).copied()
    }
}

impl rust_i18n::BackendExt for RuntimeBackend {}

impl Default for RuntimeBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rust_i18n::Backend;

    #[test]
    fn test_parse_all_locales() {
        for (locale, json) in EMBEDDED_LOCALES {
            let parsed = parse_locale(json);
            assert!(
                !parsed.is_empty(),
                "Locale {} should have translations",
                locale
            );
        }
    }

    #[test]
    fn test_flatten_nested_json() {
        let json = r#"{
            "action": {
                "copy": "Copy",
                "paste": "Paste"
            },
            "simple": "value"
        }"#;
        let parsed = parse_locale(json);
        assert_eq!(parsed.get("action.copy").copied(), Some("Copy"));
        assert_eq!(parsed.get("action.paste").copied(), Some("Paste"));
        assert_eq!(parsed.get("simple").copied(), Some("value"));
    }

    #[test]
    fn test_skip_metadata_keys() {
        let json = r#"{
            "_version": "1.0",
            "key": "value"
        }"#;
        let parsed = parse_locale(json);
        assert!(!parsed.contains_key("_version"));
        assert_eq!(parsed.get("key").copied(), Some("value"));
    }

    #[test]
    fn test_backend_available_locales() {
        let backend = RuntimeBackend::new();
        let locales = backend.available_locales();
        assert_eq!(locales.len(), 14);
        assert!(locales.iter().any(|l| *l == "en"));
        assert!(locales.iter().any(|l| *l == "es"));
    }

    #[test]
    fn test_backend_translate() {
        let backend = RuntimeBackend::new();

        // Test English translation
        let result = backend.translate("en", "action.copy");
        assert!(result.is_some());

        // Test missing key
        let result = backend.translate("en", "nonexistent.key");
        assert!(result.is_none());
    }

    #[test]
    fn test_lazy_loading() {
        let backend = RuntimeBackend::new();

        // First access should load the locale
        backend.translate("en", "action.copy");
        assert!(TRANSLATIONS.read().unwrap().contains_key("en"));

        // Second access should use cached version
        backend.translate("en", "action.paste");
    }
}
