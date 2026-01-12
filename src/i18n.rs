//! Internationalization (i18n) support for Fresh Editor
//!
//! This module provides locale detection and translation support using rust-i18n.
//! Translations are embedded at compile time from JSON files in the `locales/` directory.
//!
//! # Usage
//!
//! ```rust
//! use rust_i18n::t;
//!
//! // Simple translation
//! let msg = t!("search.no_text");
//!
//! // Translation with interpolation
//! let msg = t!("file.saved_as", path = "/path/to/file");
//! ```

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::RwLock;

/// Type alias for the nested plugin strings map.
/// Structure: plugin_name -> locale -> key -> translated_string
type PluginStringsMap = HashMap<String, HashMap<String, HashMap<String, String>>>;

static PLUGIN_STRINGS: Lazy<RwLock<PluginStringsMap>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Register strings for a plugin.
/// format: { "en": { "key": "value" }, "es": { "key": "value" } }
pub fn register_plugin_strings(
    plugin_name: &str,
    strings: HashMap<String, HashMap<String, String>>,
) {
    let mut all_strings = PLUGIN_STRINGS.write().unwrap();
    all_strings.insert(plugin_name.to_string(), strings);
}

/// Translate a string for a plugin using the current locale.
pub fn translate_plugin_string(
    plugin_name: &str,
    key: &str,
    args: &HashMap<String, String>,
) -> String {
    let locale = current_locale();
    let all_strings = PLUGIN_STRINGS.read().unwrap();

    let plugin_map: &HashMap<String, HashMap<String, String>> = match all_strings.get(plugin_name) {
        Some(m) => m,
        None => return key.to_string(),
    };

    // Try current locale, then fallback to English
    let lang_map: Option<&HashMap<String, String>> =
        plugin_map.get(&locale).or_else(|| plugin_map.get("en"));

    let template: &String = match lang_map.and_then(|m| m.get(key)) {
        Some(t) => t,
        None => return key.to_string(),
    };

    // Simple interpolation: %{variable}
    let mut result = template.clone();
    for (k, v) in args {
        result = result.replace(&format!("%{{{}}}", k), v);
    }
    result
}

/// Unregister strings for a plugin.
pub fn unregister_plugin_strings(plugin_name: &str) {
    let mut all_strings = PLUGIN_STRINGS.write().unwrap();
    all_strings.remove(plugin_name);
}

/// Initialize i18n with the user's locale preference.
///
/// This should be called early in application startup. It detects the system
/// locale from environment variables and sets it as the active locale.
///
/// # Locale Detection Order
///
/// 1. `LC_ALL` environment variable
/// 2. `LC_MESSAGES` environment variable
/// 3. `LANG` environment variable
/// 4. Falls back to "en" (English) if none are set
pub fn init() {
    let locale = detect_locale().unwrap_or_else(|| "en".to_string());
    rust_i18n::set_locale(&locale);
}

/// Initialize i18n with a specific locale from user configuration.
///
/// If `config_locale` is `Some`, use that locale. Otherwise, detect from environment.
pub fn init_with_config(config_locale: Option<&str>) {
    let locale = if let Some(req_locale) = config_locale {
        // Try to match the requested locale against available ones
        let supported = available_locales();
        let req_lower = req_locale.replace('_', "-").to_lowercase();

        let mut matched = None;
        for &loc in &supported {
            if loc.to_lowercase() == req_lower {
                matched = Some(loc.to_string());
                break;
            }
        }

        matched.unwrap_or_else(|| req_locale.to_string())
    } else {
        detect_locale().unwrap_or_else(|| "en".to_string())
    };

    rust_i18n::set_locale(&locale);
}

/// Detect the user's preferred locale from environment variables.
///
/// Checks `LC_ALL`, `LC_MESSAGES`, and `LANG` in order, parsing the locale
/// string to extract the language code (e.g., "en_US.UTF-8" -> "en").
///
/// This function also attempts to match region-specific locales supported by Fresh,
/// such as "pt-BR" and "zh-CN".
fn detect_locale() -> Option<String> {
    let env_locale = std::env::var("LC_ALL")
        .or_else(|_| std::env::var("LC_MESSAGES"))
        .or_else(|_| std::env::var("LANG"))
        .ok()?;

    if env_locale.is_empty() || env_locale == "C" || env_locale == "POSIX" {
        return None;
    }

    // First, try exact match with supported region-specific locales
    // e.g. "pt_BR.UTF-8" -> "pt-BR"
    let normalized = env_locale.replace('_', "-").to_lowercase();
    let supported = available_locales();

    for &loc in &supported {
        if normalized.starts_with(&loc.to_lowercase()) {
            return Some(loc.to_string());
        }
    }

    // Fall back to primary language code
    // e.g. "en_US.UTF-8" -> "en"
    let lang = env_locale.split(['_', '-', '.']).next()?;
    if lang.is_empty() || lang == "C" || lang == "POSIX" {
        None
    } else {
        Some(lang.to_lowercase())
    }
}

/// Get the currently active locale.
pub fn current_locale() -> String {
    rust_i18n::locale().to_string()
}

/// Set the locale explicitly.
///
/// This can be used to change the locale at runtime, for example from
/// a settings menu or command palette action.
pub fn set_locale(locale: &str) {
    rust_i18n::set_locale(locale);
}

/// Get a list of all available locales.
///
/// These are the locales that have translation files in the `locales/` directory.
pub fn available_locales() -> Vec<&'static str> {
    rust_i18n::available_locales!()
}

/// Get the display name for a locale code.
///
/// Returns a tuple of (English name, Native name) for display in UI.
/// For example: ("German", "Deutsch") for "de".
/// Returns None if the locale is not recognized.
pub fn locale_display_name(locale: &str) -> Option<(&'static str, &'static str)> {
    match locale {
        "cs" => Some(("Czech", "Čeština")),
        "de" => Some(("German", "Deutsch")),
        "en" => Some(("English", "English")),
        "es" => Some(("Spanish", "Español")),
        "fr" => Some(("French", "Français")),
        "it" => Some(("Italian", "Italiano")),
        "ja" => Some(("Japanese", "日本語")),
        "ko" => Some(("Korean", "한국어")),
        "pt-BR" => Some(("Portuguese (Brazil)", "Português (Brasil)")),
        "ru" => Some(("Russian", "Русский")),
        "th" => Some(("Thai", "ไทย")),
        "uk" => Some(("Ukrainian", "Українська")),
        "zh-CN" => Some(("Chinese (Simplified)", "简体中文")),
        _ => None,
    }
}

/// Get the translated message for "switched to project".
///
/// This is a helper function for use by the binary crate (main.rs) since
/// the t!() macro doesn't work across crate boundaries.
pub fn switched_to_project_message(path: &str) -> String {
    rust_i18n::t!("file.switched_to_project", path = path).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_init_sets_locale() {
        init();
        // Should have some locale set (either detected or fallback)
        let locale = current_locale();
        assert!(!locale.is_empty());
    }

    #[test]
    fn test_set_locale() {
        set_locale("en");
        assert_eq!(current_locale(), "en");
    }

    #[test]
    fn test_locale_changed_interpolation() {
        use rust_i18n::t;
        set_locale("en");

        // Test the locale.changed message interpolation
        // Note: The placeholder is %{locale_name} not %{locale} because "locale"
        // is a reserved parameter in rust_i18n that sets the translation locale.
        let locale_name = "es";
        let msg = t!("locale.changed", locale_name = locale_name).to_string();

        assert_eq!(msg, "Locale changed to es");
    }

    #[test]
    fn test_available_locales_includes_en() {
        let locales = available_locales();
        assert!(
            locales.contains(&"en"),
            "English locale should be available"
        );
    }

    /// Validate that all locale files have the same keys as the English locale.
    /// This ensures translations are complete and no keys are missing.
    #[test]
    fn test_all_locales_have_required_keys() {
        use std::fs;
        use std::path::Path;

        // Read the English locale file as the schema
        let locales_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("locales");
        let en_content =
            fs::read_to_string(locales_dir.join("en.json")).expect("Failed to read en.json");
        let en_json: serde_json::Value =
            serde_json::from_str(&en_content).expect("Failed to parse en.json");

        // Get all keys from English locale (excluding _version)
        let en_keys: HashSet<_> = en_json
            .as_object()
            .expect("en.json should be an object")
            .keys()
            .filter(|k| !k.starts_with('_'))
            .cloned()
            .collect();

        // Check each available locale
        let locales = available_locales();
        assert!(
            locales.len() >= 2,
            "Should have at least 2 locales (en and at least one other)"
        );

        for locale in &locales {
            if *locale == "en" {
                continue; // Skip English, it's the schema
            }

            let locale_file = locales_dir.join(format!("{}.json", locale));
            let content = fs::read_to_string(&locale_file)
                .unwrap_or_else(|_| panic!("Failed to read {}.json", locale));
            let json: serde_json::Value = serde_json::from_str(&content)
                .unwrap_or_else(|_| panic!("Failed to parse {}.json", locale));

            let locale_keys: HashSet<_> = json
                .as_object()
                .unwrap_or_else(|| panic!("{}.json should be an object", locale))
                .keys()
                .filter(|k| !k.starts_with('_'))
                .cloned()
                .collect();

            // Find missing keys
            let missing: Vec<_> = en_keys.difference(&locale_keys).collect();
            if !missing.is_empty() {
                // Sort for consistent error messages
                let mut missing_sorted: Vec<_> = missing.into_iter().collect();
                missing_sorted.sort();
                panic!(
                    "Locale '{}' is missing {} keys: {:?}",
                    locale,
                    missing_sorted.len(),
                    missing_sorted
                );
            }

            // Optionally warn about extra keys (locale has keys not in English)
            let extra: Vec<_> = locale_keys.difference(&en_keys).collect();
            if !extra.is_empty() {
                let mut extra_sorted: Vec<_> = extra.into_iter().collect();
                extra_sorted.sort();
                eprintln!(
                    "Warning: Locale '{}' has {} extra keys not in English: {:?}",
                    locale,
                    extra_sorted.len(),
                    extra_sorted
                );
            }
        }
    }
}
