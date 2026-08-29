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
