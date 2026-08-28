//! The translation store: registered JSON catalogs, parsed on first use.

use once_cell::sync::Lazy;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::RwLock;

/// Locale consulted when the active locale has no entry for a key.
pub(crate) const FALLBACK_LOCALE: &str = "en";

/// Registered catalogs, sorted by locale code. The JSON source is kept
/// unparsed until the locale is first looked up.
type Sources = Vec<(&'static str, Box<str>)>;
static SOURCES: Lazy<RwLock<Sources>> = Lazy::new(|| RwLock::new(Vec::new()));

/// Flattened translations per locale, populated on demand from `SOURCES`.
/// Keys and values are leaked once so lookups can hand out `&'static str`.
type Catalog = HashMap<&'static str, &'static str>;
static TRANSLATIONS: Lazy<RwLock<HashMap<&'static str, Catalog>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Register translation catalogs as `(locale code, JSON source)` pairs.
///
/// Call this once during startup, before any [`crate::t`]. Re-registering a
/// locale replaces its catalog. The JSON is copied but not parsed here:
/// parsing happens the first time the locale is looked up, so an application
/// that ships fifteen catalogs only pays for the one it displays.
///
/// A catalog that fails to parse is registered empty rather than panicking,
/// so a malformed file degrades to the fallback locale instead of taking the
/// process down from inside an unrelated `t!`.
pub fn register_locales(locales: &[(&str, &str)]) {
    let mut sources = SOURCES.write().unwrap();
    for (code, json) in locales {
        match sources.iter_mut().find(|(c, _)| c == code) {
            Some(slot) => {
                slot.1 = (*json).into();
                TRANSLATIONS.write().unwrap().remove(slot.0);
            }
            None => {
                let code: &'static str = Box::leak(code.to_string().into_boxed_str());
                sources.push((code, (*json).into()));
            }
        }
    }
    sources.sort_by(|a, b| a.0.cmp(b.0));
}

/// All registered locale codes, sorted.
pub fn available_locales() -> Vec<&'static str> {
    SOURCES.read().unwrap().iter().map(|(c, _)| *c).collect()
}

/// Look `key` up in exactly `locale`, with no fallback.
pub fn translate_in(locale: &str, key: &str) -> Option<&'static str> {
    ensure_loaded(locale);
    TRANSLATIONS.read().unwrap().get(locale)?.get(key).copied()
}

/// Parse `locale`'s catalog if it has not been parsed yet.
fn ensure_loaded(locale: &str) {
    if TRANSLATIONS.read().unwrap().contains_key(locale) {
        return;
    }

    // Parse outside the write lock; a concurrent duplicate parse is harmless.
    let parsed = {
        let sources = SOURCES.read().unwrap();
        match sources.iter().find(|(c, _)| *c == locale) {
            Some((code, json)) => (*code, parse_catalog(json)),
            None => return,
        }
    };
    TRANSLATIONS
        .write()
        .unwrap()
        .entry(parsed.0)
        .or_insert(parsed.1);
}

/// Flatten a locale's JSON into dot-separated keys, leaking the strings.
fn parse_catalog(json: &str) -> Catalog {
    let mut flat = HashMap::new();
    if let Ok(value) = serde_json::from_str::<Value>(json) {
        flatten(&value, String::new(), &mut flat);
    }
    flat
}

fn flatten(value: &Value, prefix: String, output: &mut Catalog) {
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
                flatten(val, new_prefix, output);
            }
        }
        Value::String(s) => {
            // Leaked because a catalog is parsed once and lives as long as the
            // process; this is what lets lookups return `&'static str`.
            let key: &'static str = Box::leak(prefix.into_boxed_str());
            let val: &'static str = Box::leak(s.clone().into_boxed_str());
            output.insert(key, val);
        }
        _ => {}
    }
}
