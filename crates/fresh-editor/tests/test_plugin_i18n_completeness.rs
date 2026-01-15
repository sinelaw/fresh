//! Test that validates all plugin i18n files have complete translations.
//!
//! This test ensures:
//! 1. All plugin .i18n.json files contain all supported locales
//! 2. All locales have the same keys as English (the source of truth)

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::Path;

/// Get all supported locale codes from the locales directory
fn get_supported_locales() -> Vec<String> {
    let locales_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("locales");

    let mut locales: Vec<String> = fs::read_dir(&locales_dir)
        .expect("Failed to read locales directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()? == "json" {
                path.file_stem()?.to_str().map(|s| s.to_string())
            } else {
                None
            }
        })
        .collect();

    locales.sort();
    locales
}

/// Get all plugin i18n files
fn get_plugin_i18n_files() -> Vec<std::path::PathBuf> {
    let plugins_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("plugins");

    let mut files: Vec<_> = fs::read_dir(&plugins_dir)
        .expect("Failed to read plugins directory")
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()?.to_str()? == "json"
                && path.file_name()?.to_str()?.ends_with(".i18n.json")
            {
                Some(path)
            } else {
                None
            }
        })
        .collect();

    files.sort();
    files
}

#[test]
fn test_plugin_i18n_files_have_all_locales() {
    let supported_locales = get_supported_locales();
    let i18n_files = get_plugin_i18n_files();

    assert!(
        !i18n_files.is_empty(),
        "No plugin i18n files found in plugins directory"
    );

    let mut errors: Vec<String> = Vec::new();

    for file_path in &i18n_files {
        let file_name = file_path.file_name().unwrap().to_str().unwrap();

        let content = fs::read_to_string(file_path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {}", file_name, e));

        let i18n: HashMap<String, serde_json::Value> = serde_json::from_str(&content)
            .unwrap_or_else(|e| panic!("Failed to parse {}: {}", file_name, e));

        let file_locales: HashSet<&str> = i18n.keys().map(|s| s.as_str()).collect();

        // Check for missing locales
        for locale in &supported_locales {
            if !file_locales.contains(locale.as_str()) {
                errors.push(format!("{}: missing locale '{}'", file_name, locale));
            }
        }

        // Check for extra locales (not in supported list)
        for locale in &file_locales {
            if !supported_locales.contains(&locale.to_string()) {
                errors.push(format!(
                    "{}: has unsupported locale '{}' (not in locales/*.json)",
                    file_name, locale
                ));
            }
        }
    }

    if !errors.is_empty() {
        panic!(
            "Plugin i18n locale coverage errors ({}):\n  - {}",
            errors.len(),
            errors.join("\n  - ")
        );
    }
}

#[test]
fn test_plugin_i18n_files_have_matching_keys() {
    let i18n_files = get_plugin_i18n_files();

    assert!(
        !i18n_files.is_empty(),
        "No plugin i18n files found in plugins directory"
    );

    let mut errors: Vec<String> = Vec::new();

    for file_path in &i18n_files {
        let file_name = file_path.file_name().unwrap().to_str().unwrap();

        let content = fs::read_to_string(file_path)
            .unwrap_or_else(|e| panic!("Failed to read {}: {}", file_name, e));

        let i18n: HashMap<String, HashMap<String, String>> = serde_json::from_str(&content)
            .unwrap_or_else(|e| panic!("Failed to parse {}: {}", file_name, e));

        // Get English keys as source of truth
        let en_keys: HashSet<&str> = match i18n.get("en") {
            Some(en) => en.keys().map(|s| s.as_str()).collect(),
            None => {
                errors.push(format!(
                    "{}: missing 'en' locale (source of truth)",
                    file_name
                ));
                continue;
            }
        };

        // Check each locale has the same keys as English
        for (locale, translations) in &i18n {
            if locale == "en" {
                continue;
            }

            let locale_keys: HashSet<&str> = translations.keys().map(|s| s.as_str()).collect();

            // Keys in English but missing in this locale
            let missing: Vec<_> = en_keys.difference(&locale_keys).collect();
            for key in &missing {
                errors.push(format!(
                    "{}: locale '{}' missing key '{}'",
                    file_name, locale, key
                ));
            }

            // Keys in this locale but not in English (extra keys)
            let extra: Vec<_> = locale_keys.difference(&en_keys).collect();
            for key in &extra {
                errors.push(format!(
                    "{}: locale '{}' has extra key '{}' not in English",
                    file_name, locale, key
                ));
            }
        }
    }

    if !errors.is_empty() {
        // Sort errors for consistent output
        errors.sort();
        panic!(
            "Plugin i18n key mismatch errors ({}):\n  - {}",
            errors.len(),
            errors.join("\n  - ")
        );
    }
}

#[test]
fn test_plugin_i18n_summary() {
    let supported_locales = get_supported_locales();
    let i18n_files = get_plugin_i18n_files();

    println!(
        "Supported locales ({}): {:?}",
        supported_locales.len(),
        supported_locales
    );
    println!("Plugin i18n files found: {}", i18n_files.len());

    for file_path in &i18n_files {
        let file_name = file_path.file_name().unwrap().to_str().unwrap();
        let content = fs::read_to_string(file_path).unwrap();
        let i18n: HashMap<String, HashMap<String, String>> =
            serde_json::from_str(&content).unwrap();

        let en_key_count = i18n.get("en").map(|e| e.len()).unwrap_or(0);
        let locale_count = i18n.len();

        println!(
            "  {}: {} locales, {} keys (en)",
            file_name, locale_count, en_key_count
        );
    }
}
