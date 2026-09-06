//! What is left of the settings painter: nothing that paints.
//!
//! The dialog is a description — `view::shell::settings` builds it, box and
//! all, and `view::shell::entry` builds its entry-edit stack. What stayed
//! here are the shapes the tree asks this module for, which are domain
//! knowledge (what a search result reads as, what a category's icon is, what
//! a Delete button is called) rather than paint.

use super::entry_dialog::EntryDialogState;
use super::search::{DeepMatch, SearchResult};

/// Get an icon for a settings category name.
///
/// Two sets are available. The Nerd Font set uses private-use-area
/// codepoints that require a patched "Nerd Font" in the terminal — PUA
/// glyphs have no system-font fallback, so on any other font they
/// render as `?` or empty boxes (issue #2032). The default set uses
/// standard BMP codepoints (default text presentation, width 1) from
/// the same compatibility class as the `▶`/`✓`/`●` glyphs the UI
/// already relies on, so terminal font fallback can always supply
/// them. The Nerd Font set is used only when `editor.nerd_font_icons`
/// is enabled.
pub fn category_icon(name: &str, nerd_fonts: bool) -> &'static str {
    let name = name.to_lowercase();
    if nerd_fonts {
        return match name.as_str() {
            "general" => "\u{f013} ",       //
            "editor" => "\u{f044} ",        //
            "clipboard" => "\u{f328} ",     //
            "file browser" => "\u{f07b} ",  //
            "file explorer" => "\u{f07c} ", //
            "packages" => "\u{f487} ",      //
            "plugins" => "\u{f1e6} ",       //
            "terminal" => "\u{f120} ",      //
            "warnings" => "\u{f071} ",      //
            "keybindings" => "\u{f11c} ",   //
            _ => "\u{f111} ",               //  (dot circle as fallback)
        };
    }
    if name.starts_with("plugin: ") {
        return "\u{271a} "; // ✚ heavy plus (add-on)
    }
    match name.as_str() {
        "general" => "\u{2699} ",       // ⚙ gear
        "editor" => "\u{270e} ",        // ✎ pencil
        "clipboard" => "\u{2702} ",     // ✂ scissors (cut/copy)
        "file browser" => "\u{25a4} ",  // ▤ square with lines (document)
        "file explorer" => "\u{25a6} ", // ▦ square with grid (tree)
        "packages" => "\u{25c6} ",      // ◆ diamond
        "plugins" => "\u{271a} ",       // ✚ heavy plus (add-on)
        "terminal" => "\u{00bb} ",      // » prompt chevron
        "warnings" => "\u{26a0} ",      // ⚠ warning sign
        "keybindings" => "\u{2328} ",   // ⌨ keyboard
        _ => "\u{2022} ",               // • bullet as fallback
    }
}

// **The whole of the settings dialog's chrome is described.** What stood
// here — the two prompts and their button rows, the help overlay, the
// entry-edit stack with its own scroll, its per-field controls and its three
// bottom rows, and the widget adapter each of those controls painted through
// — is `view::shell::settings` and `view::shell::entry`. What is left in this
// file is the shape of one search result, which is domain knowledge rather
// than paint.

/// One search result, as the three rows the tree draws it in.
///
/// **The formatting is domain knowledge and stays here.** What a result's
/// name *is* depends on how it matched — a map's key, a map value with its
/// key as breadcrumb, a text-list item, or the setting itself — and which
/// characters the query hit is the matcher's answer. What went is the
/// painting: three `Paragraph`s, a highlight band drawn row by row, and a
/// rectangle filed per visible card.
pub fn search_result_row(result: &SearchResult) -> crate::view::shell::settings::ResultRow {
    use crate::app::shell_host::shell_theme::attrs;
    use crate::view::shell::settings::ResultRow;

    let (name, desc) = match &result.deep_match {
        Some(DeepMatch::MapKey { key, .. }) => (key.clone(), Some(result.item.name.clone())),
        Some(DeepMatch::MapValue {
            matched_text, key, ..
        }) => (
            matched_text.clone(),
            Some(format!("{} > {}", result.item.name, key)),
        ),
        Some(DeepMatch::TextListItem { text, .. }) => {
            (text.clone(), Some(result.item.name.clone()))
        }
        None => (result.item.name.clone(), result.item.description.clone()),
    };
    ResultRow {
        // The row's own theme carries selection and hover, so the plain runs
        // name no colours of their own — only the matched characters do.
        name: highlight_spans(
            &name,
            &result.name_matches,
            String::new(),
            attrs("diagnostic.warning_fg", "ui.popup_bg", &["bold"]),
        ),
        breadcrumb: format!("{} > {}", result.breadcrumb, result.item.path),
        desc,
    }
}

/// Split `text` at the matched character positions, so the matcher's answer
/// becomes runs the tree can theme.
fn highlight_spans(
    text: &str,
    matches: &[usize],
    plain: String,
    hit: String,
) -> Vec<crate::view::shell::settings::Span> {
    use crate::view::shell::settings::Span as UiSpan;
    if matches.is_empty() {
        return vec![UiSpan::new(text.to_string(), plain)];
    }
    let mut spans = Vec::new();
    let mut current = String::new();
    let mut lit = false;
    for (idx, ch) in text.chars().enumerate() {
        let on = matches.contains(&idx);
        if on != lit {
            if !current.is_empty() {
                spans.push(UiSpan::new(
                    std::mem::take(&mut current),
                    match lit {
                        true => hit.clone(),
                        false => plain.clone(),
                    },
                ));
            }
            lit = on;
        }
        current.push(ch);
    }
    if !current.is_empty() {
        spans.push(UiSpan::new(
            current,
            match lit {
                true => hit,
                false => plain,
            },
        ));
    }
    spans
}

/// Compute the footer Delete-button label for an entry dialog.
///
/// Schema-driven: shows the map key for map entries (e.g.
/// `[ Delete "rust" ]`), a generic "item" for array items (the
/// numeric index isn't meaningful to the user), or a bare fallback
/// when neither is available. The key is truncated so a very long
/// identifier can't blow out the dialog footer.
pub(crate) fn entry_delete_button_label(dialog: &EntryDialogState) -> String {
    const MAX_KEY_IN_LABEL: usize = 24;
    if dialog.is_array_item {
        "[ Delete item ]".to_string()
    } else if dialog.entry_key.is_empty() {
        "[ Delete entry ]".to_string()
    } else {
        let key = if dialog.entry_key.chars().count() > MAX_KEY_IN_LABEL {
            let truncated: String = dialog
                .entry_key
                .chars()
                .take(MAX_KEY_IN_LABEL - 1)
                .collect();
            format!("{}…", truncated)
        } else {
            dialog.entry_key.clone()
        };
        format!("[ Delete \"{}\" ]", key)
    }
}
