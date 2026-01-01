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
    let locale = config_locale
        .map(|s| s.to_string())
        .or_else(detect_locale)
        .unwrap_or_else(|| "en".to_string());
    rust_i18n::set_locale(&locale);
}

/// Detect the user's preferred locale from environment variables.
///
/// Checks `LC_ALL`, `LC_MESSAGES`, and `LANG` in order, parsing the locale
/// string to extract the language code (e.g., "en_US.UTF-8" -> "en").
fn detect_locale() -> Option<String> {
    std::env::var("LC_ALL")
        .or_else(|_| std::env::var("LC_MESSAGES"))
        .or_else(|_| std::env::var("LANG"))
        .ok()
        .and_then(|s| {
            // Parse locale string: "en_US.UTF-8" -> "en"
            // Handle both underscore and hyphen separators
            let lang = s.split(|c| c == '_' || c == '-' || c == '.').next()?;
            if lang.is_empty() || lang == "C" || lang == "POSIX" {
                None
            } else {
                Some(lang.to_lowercase())
            }
        })
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
