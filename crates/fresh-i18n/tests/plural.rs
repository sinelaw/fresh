//! Count-dependent messages pick the form the locale's grammar needs.

mod common;

use common::locale_guard;
use fresh_i18n::{plural_category, register_locales, set_locale, tn, PluralCategory};

#[test]
fn english_uses_the_singular_for_one_only() {
    assert_eq!(plural_category("en", 0), PluralCategory::Other);
    assert_eq!(plural_category("en", 1), PluralCategory::One);
    assert_eq!(plural_category("en", 2), PluralCategory::Other);
    assert_eq!(plural_category("de", 1), PluralCategory::One);
}

#[test]
fn french_and_portuguese_use_the_singular_for_zero_too() {
    assert_eq!(plural_category("fr", 0), PluralCategory::One);
    assert_eq!(plural_category("fr", 1), PluralCategory::One);
    assert_eq!(plural_category("fr", 2), PluralCategory::Other);
    assert_eq!(plural_category("pt-BR", 0), PluralCategory::One);
    assert_eq!(plural_category("pt-BR", 5), PluralCategory::Other);
}

#[test]
fn east_slavic_rules_follow_the_last_digits() {
    for (n, expected) in [
        (1, PluralCategory::One),
        (21, PluralCategory::One),
        (101, PluralCategory::One),
        (11, PluralCategory::Many),
        (2, PluralCategory::Few),
        (24, PluralCategory::Few),
        (12, PluralCategory::Many),
        (14, PluralCategory::Many),
        (0, PluralCategory::Many),
        (5, PluralCategory::Many),
        (111, PluralCategory::Many),
    ] {
        assert_eq!(plural_category("ru", n), expected, "ru {n}");
        assert_eq!(plural_category("uk", n), expected, "uk {n}");
    }
}

#[test]
fn czech_has_a_few_form_for_two_to_four() {
    assert_eq!(plural_category("cs", 1), PluralCategory::One);
    assert_eq!(plural_category("cs", 3), PluralCategory::Few);
    assert_eq!(plural_category("cs", 5), PluralCategory::Other);
    assert_eq!(plural_category("cs", 0), PluralCategory::Other);
}

#[test]
fn languages_without_grammatical_number_always_use_other() {
    for lang in ["ja", "ko", "zh-CN", "th", "vi"] {
        assert_eq!(plural_category(lang, 1), PluralCategory::Other, "{lang}");
        assert_eq!(plural_category(lang, 7), PluralCategory::Other, "{lang}");
    }
}

#[test]
fn tn_picks_the_form_for_the_count_and_substitutes_it() {
    let _guard = locale_guard();
    register_locales(&[
        (
            "x-pl-en",
            r#"{"files.one": "Saved %{count} file in %{dir}", "files.other": "Saved %{count} files in %{dir}"}"#,
        ),
        (
            "ru",
            r#"{"files.one": "%{count} файл", "files.few": "%{count} файла", "files.many": "%{count} файлов"}"#,
        ),
    ]);
    set_locale("x-pl-en");
    assert_eq!(tn!("files", 1, dir = "a"), "Saved 1 file in a");
    assert_eq!(tn!("files", 3usize, dir = "a"), "Saved 3 files in a");
    assert_eq!(tn!("files", 0, dir = "a"), "Saved 0 files in a");

    set_locale("ru");
    assert_eq!(tn!("files", 1), "1 файл");
    assert_eq!(tn!("files", 22), "22 файла");
    assert_eq!(tn!("files", 11), "11 файлов");
    set_locale("en");
}

#[test]
fn a_locale_without_the_message_falls_back_to_the_english_form_for_the_count() {
    let _guard = locale_guard();
    register_locales(&[
        (
            "en",
            r#"{"x-pl-fallback.one": "%{count} thing", "x-pl-fallback.other": "%{count} things"}"#,
        ),
        ("x-pl-empty", r#"{}"#),
        (
            "x-pl-other-only",
            r#"{"x-pl-fallback.other": "%{count} T"}"#,
        ),
    ]);
    // An unknown language has English's rule, but the point here is that the
    // fallback locale's own rule chooses its form.
    set_locale("x-pl-empty");
    assert_eq!(tn!("x-pl-fallback", 1), "1 thing");
    assert_eq!(tn!("x-pl-fallback", 2), "2 things");

    // A locale that has the message but not the count's form uses its own
    // `other` form rather than English's.
    set_locale("x-pl-other-only");
    assert_eq!(tn!("x-pl-fallback", 1), "1 T");

    // A message nobody has is its own key.
    assert_eq!(tn!("x-pl-nowhere", 1), "x-pl-nowhere");
    set_locale("en");
}
