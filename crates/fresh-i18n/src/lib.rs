//! Runtime translation: a lazily-parsed JSON string store, `%{name}`
//! interpolation and a fallback locale.
//!
//! The crate ships no locale data and knows nothing about which locales an
//! application supports — the application registers its own catalogs once at
//! startup and then translates through the [`t!`] macro:
//!
//! ```
//! fresh_i18n::register_locales(&[("en", r#"{"greeting": "Hello, %{name}!"}"#)]);
//! fresh_i18n::set_locale("en");
//!
//! assert_eq!(fresh_i18n::t!("greeting", name = "world"), "Hello, world!");
//! ```
//!
//! Unlike a macro that expands to a crate-local symbol, [`t!`] resolves
//! through `$crate` and so works from any crate that depends on this one.

use once_cell::sync::Lazy;
use std::borrow::Cow;
use std::sync::RwLock;

mod plugins;
mod store;

pub use plugins::{register_plugin_strings, translate_plugin_string, unregister_plugin_strings};
pub use store::{available_locales, register_locales, translate_in};

use store::FALLBACK_LOCALE;

static CURRENT_LOCALE: Lazy<RwLock<String>> =
    Lazy::new(|| RwLock::new(FALLBACK_LOCALE.to_string()));

/// Set the active locale. Unknown codes are accepted; they simply resolve
/// through the fallback chain.
pub fn set_locale(locale: &str) {
    let mut current = CURRENT_LOCALE.write().unwrap();
    current.clear();
    current.push_str(locale);
}

/// The active locale.
pub fn locale() -> String {
    CURRENT_LOCALE.read().unwrap().clone()
}

/// Detect the preferred locale from the environment, restricted to the
/// locales that have been registered.
///
/// Checks `LC_ALL`, `LC_MESSAGES` and `LANG` in order. A registered
/// region-specific locale wins (`pt_BR.UTF-8` → `pt-BR`); otherwise the
/// primary language subtag is returned (`en_US.UTF-8` → `en`).
pub fn detect_locale() -> Option<String> {
    let env_locale = std::env::var("LC_ALL")
        .or_else(|_| std::env::var("LC_MESSAGES"))
        .or_else(|_| std::env::var("LANG"))
        .ok()?;

    if env_locale.is_empty() || env_locale == "C" || env_locale == "POSIX" {
        return None;
    }

    let normalized = env_locale.replace('_', "-").to_lowercase();
    for loc in available_locales() {
        if normalized.starts_with(&loc.to_lowercase()) {
            return Some(loc.to_string());
        }
    }

    let lang = env_locale.split(['_', '-', '.']).next()?;
    if lang.is_empty() || lang == "C" || lang == "POSIX" {
        None
    } else {
        Some(lang.to_lowercase())
    }
}

/// Translate `key` in the active locale, falling back to progressively
/// shorter language tags, then to the fallback locale, then to `key` itself.
pub fn translate(key: impl AsRef<str>) -> Cow<'static, str> {
    let key = key.as_ref();
    match lookup(key) {
        Some(value) => Cow::Borrowed(value),
        None => Cow::Owned(key.to_string()),
    }
}

/// Translate `key` and substitute `%{name}` placeholders from `args`.
/// Placeholders with no matching argument are left in place.
pub fn translate_with_args(key: impl AsRef<str>, args: &[(&str, String)]) -> Cow<'static, str> {
    let key = key.as_ref();
    let template = lookup(key).unwrap_or(key);
    Cow::Owned(interpolate(template, args))
}

/// Resolve `key` through the fallback chain: the active locale, each shorter
/// tag of it (`zh-Hant-CN` → `zh-Hant` → `zh`), then [`FALLBACK_LOCALE`].
fn lookup(key: &str) -> Option<&'static str> {
    let active = locale();
    let mut current: &str = &active;
    loop {
        if let Some(value) = translate_in(current, key) {
            return Some(value);
        }
        match current.rfind('-') {
            Some(n) => current = current[..n].trim_end_matches("-x"),
            None => break,
        }
    }
    translate_in(FALLBACK_LOCALE, key)
}

/// Replace `%{name}` placeholders in `template` with the matching argument.
pub(crate) fn interpolate(template: &str, args: &[(&str, String)]) -> String {
    let mut out = String::with_capacity(template.len() + 32);
    let mut rest = template;
    while let Some(start) = rest.find("%{") {
        let after = &rest[start + 2..];
        let Some(end) = after.find('}') else { break };
        out.push_str(&rest[..start]);
        match args.iter().find(|(name, _)| *name == &after[..end]) {
            Some((_, value)) => out.push_str(value),
            None => out.push_str(&rest[start..start + 2 + end + 1]),
        }
        rest = &after[end + 1..];
    }
    out.push_str(rest);
    out
}

/// Translate a key in the active locale, returning `Cow<'static, str>`.
///
/// ```
/// # fresh_i18n::register_locales(&[("en", r#"{"hi": "Hi, %{who}"}"#)]);
/// # fresh_i18n::set_locale("en");
/// use fresh_i18n::t;
///
/// t!("hi", who = "you");          // "Hi, you"
/// t!("hi", "who" = "you");        // argument names may be string literals
/// # let key = "hi";
/// t!(key);                        // the key may be any expression
/// ```
///
/// A key with no translation in the active locale or the fallback locale
/// evaluates to the key itself.
#[macro_export]
macro_rules! t {
    ($key:expr $(,)?) => {
        $crate::translate($key)
    };
    ($key:expr, $($name:tt = $value:expr),+ $(,)?) => {
        $crate::translate_with_args(
            $key,
            &[$(($crate::__t_arg_name!($name), ::std::format!("{}", $value))),+],
        )
    };
}

/// Accepts an argument name written either as an identifier or, for names
/// that are Rust keywords, as a string literal.
#[doc(hidden)]
#[macro_export]
macro_rules! __t_arg_name {
    ($name:literal) => {
        $name
    };
    ($name:ident) => {
        ::core::stringify!($name)
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    /// `set_locale` is process-global, so the tests that steer it take turns.
    static LOCALE: Mutex<()> = Mutex::new(());

    const TEST_JSON: &str = r#"{
        "_version": "1.0",
        "action": { "copy": "Copy", "paste": "Paste" },
        "simple": "value",
        "greeting": "Hello, %{name} and %{other}!",
        "typed": "%{type} register"
    }"#;

    #[test]
    fn flattens_nested_keys() {
        register_locales(&[("t-flatten", TEST_JSON)]);
        assert_eq!(translate_in("t-flatten", "action.copy"), Some("Copy"));
        assert_eq!(translate_in("t-flatten", "action.paste"), Some("Paste"));
        assert_eq!(translate_in("t-flatten", "simple"), Some("value"));
    }

    #[test]
    fn skips_metadata_keys() {
        register_locales(&[("t-metadata", TEST_JSON)]);
        assert_eq!(translate_in("t-metadata", "_version"), None);
        assert_eq!(translate_in("t-metadata", "simple"), Some("value"));
    }

    #[test]
    fn malformed_json_registers_an_empty_catalog() {
        register_locales(&[("t-broken", "{ not json")]);
        assert!(available_locales().contains(&"t-broken"));
        assert_eq!(translate_in("t-broken", "simple"), None);
    }

    #[test]
    fn re_registering_a_locale_replaces_its_catalog() {
        register_locales(&[("t-replace", TEST_JSON)]);
        assert_eq!(translate_in("t-replace", "simple"), Some("value"));
        register_locales(&[("t-replace", r#"{"simple": "other"}"#)]);
        assert_eq!(translate_in("t-replace", "simple"), Some("other"));
    }

    #[test]
    fn available_locales_are_sorted_and_deduplicated() {
        register_locales(&[("t-sort-b", TEST_JSON), ("t-sort-a", TEST_JSON)]);
        register_locales(&[("t-sort-b", TEST_JSON)]);
        let locales = available_locales();
        let ours: Vec<_> = locales
            .iter()
            .filter(|l| l.starts_with("t-sort-"))
            .collect();
        assert_eq!(ours, [&"t-sort-a", &"t-sort-b"]);
        assert!(locales.windows(2).all(|w| w[0] <= w[1]));
    }

    #[test]
    fn interpolates_named_arguments() {
        let args = [("name", "a".to_string()), ("other", "b".to_string())];
        assert_eq!(
            interpolate("Hello, %{name} and %{other}!", &args),
            "Hello, a and b!"
        );
    }

    #[test]
    fn leaves_unmatched_placeholders_in_place() {
        let args = [("name", "a".to_string())];
        assert_eq!(interpolate("%{name} %{missing}", &args), "a %{missing}");
        assert_eq!(interpolate("50%{ of it", &args), "50%{ of it");
    }

    #[test]
    fn missing_key_translates_to_itself() {
        assert_eq!(translate("t.no.such.key"), "t.no.such.key");
        assert_eq!(
            translate_with_args("t.no.such.key", &[("name", "x".to_string())]),
            "t.no.such.key"
        );
    }

    #[test]
    fn macro_accepts_every_call_shape() {
        let _guard = LOCALE.lock().unwrap_or_else(|e| e.into_inner());
        register_locales(&[("t-macro", TEST_JSON)]);
        set_locale("t-macro");

        assert_eq!(t!("simple"), "value");
        assert_eq!(t!("greeting", name = "a", other = "b"), "Hello, a and b!");
        assert_eq!(t!("typed", "type" = "digit"), "digit register");
        assert_eq!(t!("greeting", name = 1, other = 2,), "Hello, 1 and 2!");

        let key = String::from("simple");
        assert_eq!(t!(&key), "value");
        assert_eq!(t!(key.as_str()), "value");

        set_locale(FALLBACK_LOCALE);
    }

    #[test]
    fn lookup_falls_back_to_shorter_tags_then_to_english() {
        let _guard = LOCALE.lock().unwrap_or_else(|e| e.into_inner());
        register_locales(&[
            ("t-fb", r#"{"only_base": "base"}"#),
            ("en", r#"{"only_en": "english"}"#),
        ]);
        set_locale("t-fb-XX");

        assert_eq!(t!("only_base"), "base");
        assert_eq!(t!("only_en"), "english");
        assert_eq!(t!("only_nowhere"), "only_nowhere");

        set_locale(FALLBACK_LOCALE);
    }
}
