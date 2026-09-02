//! Fresh's own translation catalogs, embedded in the binary and handed to
//! `fresh-i18n` at startup.
//!
//! The JSON is embedded rather than read from disk, and `fresh-i18n` parses
//! each catalog only when its locale is first displayed.

use once_cell::sync::Lazy;

/// Absolute path of the directory these catalogs are embedded from.
///
/// Baked in from this crate's manifest directory, so tests that read the
/// locale files off disk do not hard-code a hop from wherever they happen to
/// live. Three of them did, and all three broke the moment `locales/` moved
/// into this crate.
pub const LOCALES_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/locales");

/// Every locale Fresh ships, as `(code, JSON source)`.
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

/// Registration runs once, on the first call from any thread.
static REGISTERED: Lazy<()> = Lazy::new(|| fresh_i18n::register_locales(EMBEDDED_LOCALES));

/// Make Fresh's catalogs available to `fresh_i18n::t!`. Idempotent, and
/// cheap enough to call from every entry point that reads locale state.
pub fn ensure_registered() {
    Lazy::force(&REGISTERED);
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A key the locale-parity test guarantees is present in every catalog.
    const SHARED_KEY: &str = "action.copy";

    #[test]
    fn every_embedded_locale_parses_and_resolves() {
        ensure_registered();
        for (locale, _) in EMBEDDED_LOCALES {
            assert!(
                fresh_i18n::translate_in(locale, SHARED_KEY).is_some(),
                "locale {locale} has no translations"
            );
        }
    }

    #[test]
    fn registration_publishes_every_locale() {
        ensure_registered();
        let available = fresh_i18n::available_locales();
        assert_eq!(available.len(), EMBEDDED_LOCALES.len());
        for (locale, _) in EMBEDDED_LOCALES {
            assert!(available.contains(locale), "{locale} was not registered");
        }
    }

    #[test]
    fn unknown_keys_do_not_resolve() {
        ensure_registered();
        assert!(fresh_i18n::translate_in("en", "nonexistent.key").is_none());
    }
}
