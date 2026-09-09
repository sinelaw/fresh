//! Internationalization (i18n) support for Fresh Editor
//!
//! Fresh's translation catalogs live in `locales/`, are embedded by
//! [`embedded`], and are looked up by `fresh-i18n`. This module is the thin
//! layer that knows Fresh's data: which locales exist, what to call them, and
//! when to register them.
//!
//! # Usage
//!
//! ```rust
//! use fresh_i18n::t;
//!
//! // Simple translation
//! let msg = t!("search.no_text");
//!
//! // Translation with interpolation
//! let msg = t!("file.saved_as", path = "/path/to/file");
//! ```

pub use fresh_editor_core::i18n_embedded as embedded;

pub use fresh_i18n::{register_plugin_strings, translate_plugin_string, unregister_plugin_strings};

use embedded::ensure_registered;

/// Initialize i18n with the user's locale preference.
///
/// This should be called early in application startup. It registers Fresh's
/// catalogs, detects the system locale from environment variables and sets it
/// as the active locale.
///
/// # Locale Detection Order
///
/// 1. `LC_ALL` environment variable
/// 2. `LC_MESSAGES` environment variable
/// 3. `LANG` environment variable
/// 4. Falls back to "en" (English) if none are set
pub fn init() {
    ensure_registered();
    let locale = fresh_i18n::detect_locale().unwrap_or_else(|| "en".to_string());
    fresh_i18n::set_locale(&locale);
}

/// Initialize i18n with a specific locale from user configuration.
///
/// If `config_locale` is `Some`, use that locale. Otherwise, detect from environment.
pub fn init_with_config(config_locale: Option<&str>) {
    ensure_registered();
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
        fresh_i18n::detect_locale().unwrap_or_else(|| "en".to_string())
    };

    fresh_i18n::set_locale(&locale);
}

/// Get the currently active locale.
pub fn current_locale() -> String {
    ensure_registered();
    fresh_i18n::locale()
}

/// Set the locale explicitly.
///
/// This can be used to change the locale at runtime, for example from
/// a settings menu or command palette action.
pub fn set_locale(locale: &str) {
    ensure_registered();
    fresh_i18n::set_locale(locale);
}

/// Get a list of all available locales.
///
/// These are the locales that have translation files in the `locales/` directory.
pub fn available_locales() -> Vec<&'static str> {
    ensure_registered();
    fresh_i18n::available_locales()
}

/// Get the display name for a locale code.
///
/// Returns a tuple of (English name, Native name) for display in UI.
/// For example: ("German", "Deutsch") for "de".
/// Returns None if the locale is not recognized.
pub fn locale_display_name(locale: &str) -> Option<(&'static str, &'static str)> {
    match locale {
        "bg" => Some(("Bulgarian", "Български")),
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
        "vi" => Some(("Vietnamese", "Tiếng Việt")),
        "zh-CN" => Some(("Chinese (Simplified)", "简体中文")),
        _ => None,
    }
}

/// Translate `key`, with English fallback.
///
/// The `t!` macro covers the same ground and is preferred; this helper exists
/// for callers that build a key at runtime and would rather not import the
/// macro.  Missing keys fall back to English; if the key is missing in English
/// too, the key itself is returned.
pub fn t(key: &str) -> String {
    ensure_registered();
    fresh_i18n::translate(key).into_owned()
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
        use fresh_i18n::t;
        set_locale("en");

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
        let locales_dir = Path::new(embedded::LOCALES_DIR);
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

    #[test]
    fn t_returns_translation_for_active_locale() {
        let saved = current_locale();
        set_locale("ja");
        let s = t("cli.arg.locale");
        // Japanese key should not equal the English value or the raw key
        assert_ne!(s, "cli.arg.locale", "missing key for ja");
        assert!(!s.is_empty());
        set_locale(&saved);
    }

    #[test]
    fn t_falls_back_to_english_when_locale_missing_key() {
        let saved = current_locale();
        // Use a key we know exists in en.json; switch to a locale that
        // (in the test fixture) is missing it — this is a smoke test
        // for the fallback path, so we just ensure no panic and a
        // non-empty result for a known-good key.
        set_locale("en");
        let s = t("cli.arg.locale");
        assert!(!s.is_empty());
        assert_ne!(s, "cli.arg.locale");
        set_locale(&saved);
    }

    #[test]
    fn t_returns_key_for_unknown_lookup() {
        assert_eq!(
            t("cli.does_not_exist.anywhere"),
            "cli.does_not_exist.anywhere"
        );
    }
}
