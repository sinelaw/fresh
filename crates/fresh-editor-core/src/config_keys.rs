//! Typed keys for the settings that command-palette toggles persist.
//!
//! `Editor::persist_config_change` (in `fresh-editor`) accepts a
//! [`SettingKey<T>`] — never a raw JSON-pointer string. The constructor is
//! private, so the constants below are the only values that exist, and the
//! [`setting_keys!`] macro that declares them generates **one `#[test]` per
//! constant**: a key cannot be added without its validator. The tests run in
//! CI, which makes the check effectively build-time; at runtime a
//! `SettingKey` is a `&'static str` in a `const`, so a call site compiles to
//! exactly what the old string literal did — zero cost.
//!
//! Each generated test pins the constant to what serde actually produces:
//!
//! 1. the pointer must resolve in a serialized [`Config`](crate::config::Config)
//!    — a struct-field rename, an added `#[serde(rename)]`, or a plain typo
//!    breaks this, with serde itself as the oracle rather than a re-derivation
//!    of its naming rules;
//! 2. the value at that pointer must deserialize as the key's `T` — the same
//!    `T` the API forces callers to pass;
//! 3. a document containing only this key must survive a round-trip through
//!    [`PartialConfig`](crate::partial_config::PartialConfig), which is what
//!    `save_changes_to_layer` validates writes against. This is the guard for
//!    the actual failure mode: `set_json_pointer` happily *creates* unknown
//!    keys and serde silently *ignores* them on read-back, so before this
//!    module a typo'd pointer wrote a dead key, returned `Ok`, and the setting
//!    reverted on the next launch — the same silent class as the "four dead
//!    settings" fixed in 0.4.6.

use std::marker::PhantomData;

/// A CI-validated, typed JSON-pointer key into the config.
///
/// The type parameter is the value type the key points at, so
/// `persist_config_change(EDITOR_LINE_WRAP, value)` only compiles when
/// `value` is the `bool` the config field actually holds.
#[derive(Debug, Clone, Copy)]
pub struct SettingKey<T> {
    pointer: &'static str,
    // `fn() -> T` keeps the marker `Copy`/`Send`/`Sync` regardless of `T`.
    _value: PhantomData<fn() -> T>,
}

impl<T> SettingKey<T> {
    /// Private on purpose: a key without a generated validator test must not
    /// exist. Declare new keys through [`setting_keys!`] below.
    const fn new(pointer: &'static str) -> Self {
        Self {
            pointer,
            _value: PhantomData,
        }
    }

    /// The JSON pointer, e.g. `/editor/line_wrap`.
    pub fn pointer(&self) -> &'static str {
        self.pointer
    }
}

/// Declare setting keys and, for each, a same-named `#[test]` that validates
/// the pointer against serde. See the module docs for what the test checks.
macro_rules! setting_keys {
    ($($(#[$attr:meta])* $name:ident: $ty:ty = $pointer:literal;)+) => {
        $(
            $(#[$attr])*
            pub const $name: SettingKey<$ty> = SettingKey::new($pointer);
        )+

        #[cfg(test)]
        mod validation {
            $(
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    super::validate(&super::$name);
                }
            )+
        }
    };
}

setting_keys! {
    EDITOR_LINE_NUMBERS: bool = "/editor/line_numbers";
    EDITOR_LINE_WRAP: bool = "/editor/line_wrap";
    EDITOR_HIGHLIGHT_CURRENT_LINE: bool = "/editor/highlight_current_line";
    EDITOR_HIGHLIGHT_OCCURRENCES: bool = "/editor/highlight_occurrences";
    EDITOR_ENABLE_INLAY_HINTS: bool = "/editor/enable_inlay_hints";
    EDITOR_MOUSE_HOVER_ENABLED: bool = "/editor/mouse_hover_enabled";
    EDITOR_SHOW_MENU_BAR: bool = "/editor/show_menu_bar";
    EDITOR_SHOW_TAB_BAR: bool = "/editor/show_tab_bar";
    EDITOR_SHOW_STATUS_BAR: bool = "/editor/show_status_bar";
    EDITOR_SHOW_PROMPT_LINE: bool = "/editor/show_prompt_line";
    EDITOR_SHOW_VERTICAL_SCROLLBAR: bool = "/editor/show_vertical_scrollbar";
    EDITOR_SHOW_HORIZONTAL_SCROLLBAR: bool = "/editor/show_horizontal_scrollbar";
    FILE_EXPLORER_SHOW_HIDDEN: bool = "/file_explorer/show_hidden";
    FILE_EXPLORER_SHOW_GITIGNORED: bool = "/file_explorer/show_gitignored";
    FILE_EXPLORER_SIDE: crate::config::FileExplorerSide = "/file_explorer/side";
}

#[cfg(test)]
fn validate<T>(key: &SettingKey<T>)
where
    T: serde::Serialize + serde::de::DeserializeOwned,
{
    // 1. The pointer resolves in a serialized Config — serde is the oracle
    //    for spelling, so renames and `#[serde(rename)]` fail here.
    let config =
        serde_json::to_value(crate::config::Config::default()).expect("Config must serialize");
    let value = config.pointer(key.pointer()).cloned().unwrap_or_else(|| {
        panic!(
            "{} does not resolve in a serialized Config: the constant is out \
             of sync with the struct fields / serde attributes",
            key.pointer()
        )
    });

    // 2. The value there is the key's declared type.
    let typed: T = serde_json::from_value(value.clone()).unwrap_or_else(|e| {
        panic!(
            "{} declares the wrong value type: the serialized config holds \
             {value}, which does not deserialize as the key's T: {e}",
            key.pointer()
        )
    });

    // 3. Write-path round trip: a doc containing only this key must survive
    //    PartialConfig, or `save_changes_to_layer` would validate the write
    //    as "fine" while serde drops the key on every load — a dead entry.
    let mut doc = serde_json::Value::Object(Default::default());
    crate::config_io::set_json_pointer(
        &mut doc,
        key.pointer(),
        serde_json::to_value(&typed).expect("T serializes"),
    );
    let partial: crate::partial_config::PartialConfig = serde_json::from_value(doc)
        .unwrap_or_else(|e| panic!("{} is not valid for PartialConfig: {e}", key.pointer()));
    let round = serde_json::to_value(&partial).expect("PartialConfig serializes");
    assert_eq!(
        round.pointer(key.pointer()),
        Some(&value),
        "{} does not survive a PartialConfig round-trip: a write to it would \
         be silently ignored on the next config load",
        key.pointer()
    );
}
