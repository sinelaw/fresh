//! Behavioural tests for the plugin-string registry: a second, separately
//! keyed store so a plugin's translations arrive and leave with the plugin.

mod common;

use common::{distinct_words, locale_guard, Rng};
use fresh_i18n::{
    register_plugin_strings, set_locale, translate_plugin_string, unregister_plugin_strings,
};
use std::collections::HashMap;

fn catalog(pairs: &[(&str, &[(&str, &str)])]) -> HashMap<String, HashMap<String, String>> {
    pairs
        .iter()
        .map(|(locale, entries)| {
            let inner = entries
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect();
            (locale.to_string(), inner)
        })
        .collect()
}

fn args(pairs: &[(&str, &str)]) -> HashMap<String, String> {
    pairs
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

#[test]
fn a_registered_string_is_returned_and_stops_being_returned_once_unregistered() {
    let _guard = locale_guard();
    set_locale("en");
    register_plugin_strings("p-lifecycle", catalog(&[("en", &[("hi", "Hello")])]));

    assert_eq!(
        translate_plugin_string("p-lifecycle", "hi", &args(&[])),
        "Hello"
    );

    unregister_plugin_strings("p-lifecycle");
    assert_eq!(
        translate_plugin_string("p-lifecycle", "hi", &args(&[])),
        "hi"
    );
}

#[test]
fn an_unknown_plugin_or_key_degrades_to_the_key() {
    let _guard = locale_guard();
    set_locale("en");
    register_plugin_strings("p-unknown", catalog(&[("en", &[("hi", "Hello")])]));

    assert_eq!(translate_plugin_string("p-absent", "hi", &args(&[])), "hi");
    assert_eq!(
        translate_plugin_string("p-unknown", "bye", &args(&[])),
        "bye"
    );
    unregister_plugin_strings("p-unknown");
}

#[test]
fn the_active_locale_is_preferred_and_english_is_the_fallback() {
    let _guard = locale_guard();
    register_plugin_strings(
        "p-locale",
        catalog(&[
            ("en", &[("shared", "Hello"), ("only_en", "English")]),
            ("de", &[("shared", "Hallo")]),
        ]),
    );

    set_locale("de");
    assert_eq!(
        translate_plugin_string("p-locale", "shared", &args(&[])),
        "Hallo"
    );
    assert_eq!(
        translate_plugin_string("p-locale", "only_en", &args(&[])),
        "only_en",
        "fallback is per locale table, not per key: `de` exists, so `en` is not consulted"
    );

    set_locale("fr");
    assert_eq!(
        translate_plugin_string("p-locale", "shared", &args(&[])),
        "Hello",
        "a locale the plugin does not ship falls back to English"
    );

    set_locale("en");
    unregister_plugin_strings("p-locale");
}

#[test]
fn re_registering_a_plugin_replaces_its_strings() {
    let _guard = locale_guard();
    set_locale("en");
    register_plugin_strings(
        "p-replace",
        catalog(&[("en", &[("a", "first"), ("b", "gone")])]),
    );
    register_plugin_strings("p-replace", catalog(&[("en", &[("a", "second")])]));

    assert_eq!(
        translate_plugin_string("p-replace", "a", &args(&[])),
        "second"
    );
    assert_eq!(translate_plugin_string("p-replace", "b", &args(&[])), "b");
    unregister_plugin_strings("p-replace");
}

#[test]
fn placeholders_are_substituted_and_unbound_ones_are_left_alone() {
    let _guard = locale_guard();
    set_locale("en");
    register_plugin_strings(
        "p-interp",
        catalog(&[("en", &[("msg", "%{who} opened %{what}")])]),
    );

    assert_eq!(
        translate_plugin_string(
            "p-interp",
            "msg",
            &args(&[("who", "Ada"), ("what", "a file")])
        ),
        "Ada opened a file"
    );
    assert_eq!(
        translate_plugin_string("p-interp", "msg", &args(&[("who", "Ada")])),
        "Ada opened %{what}"
    );
    unregister_plugin_strings("p-interp");
}

/// The same round-trip over generated catalogs: whatever a plugin registers
/// is what comes back, for every key, in every locale it shipped.
#[test]
fn generated_catalogs_round_trip() {
    let _guard = locale_guard();
    let mut rng = Rng::new(0x5eed_1001);

    for round in 0..64 {
        let locale_count = rng.between(1, 3);
        let locales = distinct_words(&mut rng, locale_count, 2, 4);
        let key_count = rng.between(1, 8);
        let keys = distinct_words(&mut rng, key_count, 1, 6);

        let mut expected: HashMap<String, HashMap<String, String>> = HashMap::new();
        for locale in &locales {
            let inner = keys
                .iter()
                .map(|k| (k.clone(), rng.text(0, 16)))
                .collect::<HashMap<_, _>>();
            expected.insert(locale.clone(), inner);
        }

        let plugin = format!("p-gen-{round}");
        register_plugin_strings(&plugin, expected.clone());

        for locale in &locales {
            set_locale(locale);
            for key in &keys {
                assert_eq!(
                    translate_plugin_string(&plugin, key, &args(&[])),
                    expected[locale][key],
                    "{plugin} / {locale} / {key}"
                );
            }
        }
        unregister_plugin_strings(&plugin);
    }
    set_locale("en");
}

#[test]
fn registration_from_many_threads_leaves_every_plugin_readable() {
    let _guard = locale_guard();
    set_locale("en");

    let handles: Vec<_> = (0..8)
        .map(|n| {
            std::thread::spawn(move || {
                for i in 0..32 {
                    let plugin = format!("p-thread-{n}-{i}");
                    register_plugin_strings(&plugin, catalog(&[("en", &[("k", "v")])]));
                    assert_eq!(translate_plugin_string(&plugin, "k", &args(&[])), "v");
                    unregister_plugin_strings(&plugin);
                    assert_eq!(translate_plugin_string(&plugin, "k", &args(&[])), "k");
                }
            })
        })
        .collect();
    for handle in handles {
        handle.join().expect("no thread panicked");
    }
}
