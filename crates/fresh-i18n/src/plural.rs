//! Count-dependent messages.
//!
//! A message that states a count ("Saved 1 file" / "Saved 3 files") is
//! stored as one key per CLDR plural category, suffixed with the category:
//!
//! ```json
//! "status.saved_files.one": "Saved %{count} file",
//! "status.saved_files.other": "Saved %{count} files"
//! ```
//!
//! [`crate::tn!`] picks the form the active locale's plural rule gives for the
//! count. A locale lists only the categories its language has: English needs
//! `one` and `other`, Russian `one`, `few` and `many` (plus `other`, which
//! integers never reach there but every lookup falls back to), Japanese only
//! `other`.

use std::borrow::Cow;

/// A CLDR plural category.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PluralCategory {
    Zero,
    One,
    Two,
    Few,
    Many,
    Other,
}

impl PluralCategory {
    /// The suffix a catalog key carries for this category.
    pub fn suffix(self) -> &'static str {
        match self {
            PluralCategory::Zero => "zero",
            PluralCategory::One => "one",
            PluralCategory::Two => "two",
            PluralCategory::Few => "few",
            PluralCategory::Many => "many",
            PluralCategory::Other => "other",
        }
    }
}

/// The CLDR cardinal plural category of the integer `n` in `locale`.
///
/// Only the language subtag matters (`pt-BR` → `pt`). The rules are CLDR's
/// integer rules for the languages Fresh ships; a language not listed here
/// uses English's, the fallback locale's.
pub fn plural_category(locale: &str, n: u64) -> PluralCategory {
    use PluralCategory::*;
    let lang = locale.split(['-', '_']).next().unwrap_or(locale);
    match lang {
        // No grammatical number.
        "ja" | "ko" | "zh" | "th" | "vi" => Other,
        // 0 and 1 take the singular.
        "fr" | "pt" => {
            if n <= 1 {
                One
            } else {
                Other
            }
        }
        "cs" | "sk" => match n {
            1 => One,
            2..=4 => Few,
            _ => Other,
        },
        "ru" | "uk" | "be" => {
            let (m10, m100) = (n % 10, n % 100);
            if m10 == 1 && m100 != 11 {
                One
            } else if (2..=4).contains(&m10) && !(12..=14).contains(&m100) {
                Few
            } else {
                Many
            }
        }
        // en, de, es, it, bg, and anything unknown.
        _ => {
            if n == 1 {
                One
            } else {
                Other
            }
        }
    }
}

/// The template for `key` with count `n` in `locale` exactly: the form for
/// the count's category, else the `other` form.
fn lookup_in(locale: &str, key: &str, n: u64) -> Option<&'static str> {
    let category = plural_category(locale, n);
    crate::translate_in(locale, &format!("{key}.{}", category.suffix()))
        .or_else(|| crate::translate_in(locale, &format!("{key}.other")))
}

/// Translate the count-dependent message `key` for the count `n`, with
/// `%{count}` and `args` substituted.
///
/// Walks the same locale fallback chain as [`crate::translate`], choosing
/// the plural form by each locale's own rule, so a locale missing the
/// message falls back to the fallback locale's correctly-inflected form
/// rather than to a form picked by another language's rule. A key with no
/// translation anywhere evaluates to the key itself.
pub fn translate_plural(key: &str, n: u64, args: &[(&str, String)]) -> Cow<'static, str> {
    let template = crate::locale_chain()
        .into_iter()
        .find_map(|locale| lookup_in(&locale, key, n))
        .unwrap_or(key);
    let mut all: Vec<(&str, String)> = Vec::with_capacity(args.len() + 1);
    all.push(("count", n.to_string()));
    all.extend(args.iter().cloned());
    Cow::Owned(crate::interpolate(template, &all))
}
