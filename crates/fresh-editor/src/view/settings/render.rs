//! What is left of the settings painter.
//!
//! The dialog is a description now — `view::shell::settings` builds it and
//! `view::shell::entry` builds its entry-edit stack. Two things stayed: the
//! box, because it is drawn under the tree's overlay band rather than in it,
//! and the shapes the tree asks this module for, which are domain knowledge
//! (what a search result reads as, what a category's icon is, what a Delete
//! button is called) rather than paint.

use super::entry_dialog::EntryDialogState;
use super::search::{DeepMatch, SearchResult};
use super::state::SettingsState;
use crate::view::theme::Theme;
use ratatui::layout::Rect;
use ratatui::style::Style;
use ratatui::widgets::{Block, BorderType, Borders, Clear, Paragraph};
use ratatui::Frame;

/// Render the settings dialog into the box the tree placed.
///
/// `modal_area` used to be computed here — ninety percent of `area`, capped at
/// 160 columns, centred with `area.x` and `area.y` added back so the dock did
/// not over-draw its left edge — and then filed in `SettingsLayout::modal_area`
/// for the mouse handler to measure every other rectangle from. It is
/// `view::shell::settings`'s now, and this is handed the answer.
///
/// It is not a node itself because it is *under* everything: `Clear`, the
/// popup ground and the border are painted before the tree's overlay band
/// folds over them.
///
/// `area` is still needed for one thing: the too-small message, which is not
/// the dialog and does not go where the dialog would have.
pub fn render_settings(
    frame: &mut Frame,
    area: Rect,
    modal_area: Rect,
    panel_area: Option<Rect>,
    state: &SettingsState,
    theme: &Theme,
) {
    // Minimum size guard — prevent panics from zero-sized layout arithmetic.
    // The tree applies the same guard by placing no box; this is what it looks
    // like when it did.
    if modal_area.width == 0 || modal_area.height == 0 {
        let msg = "[Terminal too small for settings]";
        let x = area.x + area.width.saturating_sub(msg.len() as u16) / 2;
        let y = area.y + area.height / 2;
        if area.width > 0 && area.height > 0 {
            frame.render_widget(
                Paragraph::new(msg).style(Style::default().fg(theme.diagnostic_warning_fg)),
                Rect::new(x, y, msg.len() as u16, 1),
            );
        }
        return;
    }

    // Clear the modal area and draw border
    frame.render_widget(Clear, modal_area);

    let title = if state.has_changes() {
        format!(" Settings [{}] • (modified) ", state.target_layer_name())
    } else {
        format!(" Settings [{}] ", state.target_layer_name())
    };

    let block = Block::default()
        .title(title.as_str())
        .borders(Borders::ALL)
        .border_type(BorderType::Rounded)
        .border_style(Style::default().fg(theme.popup_border_fg))
        .style(Style::default().bg(theme.popup_bg));
    frame.render_widget(block, modal_area);

    // Inner area after border
    let inner_area = Rect::new(
        modal_area.x + 1,
        modal_area.y + 1,
        modal_area.width.saturating_sub(2),
        modal_area.height.saturating_sub(2),
    );

    // **Everything inside the box is the tree's.** The categories (a `List`
    // down the left, or a strip across the top), the page header, the body's
    // cards, the search's results, both footers, all seven dialogs and the
    // entry-edit stack. What is left to paint is the box itself and the one
    // column between its two panes.
    //
    // Narrow below sixty columns — the threshold the tree splits on too, and
    // the only place the two layouts still differ here, because the wide one
    // has a divider and the narrow one does not.
    if inner_area.width >= 60 {
        // `Layout::horizontal([Length(24), Length(1), Min(40)])`'s middle
        // column. The tree lays the same three out and the panel's rectangle
        // is read back rather than split a second time; this is the line
        // between them.
        let x = panel_area
            .map(|p| p.x.saturating_sub(1))
            .unwrap_or(inner_area.x + 24);
        let top = inner_area.y + 2;
        let bottom = inner_area.y + inner_area.height.saturating_sub(2);
        let style = Style::default().fg(theme.split_separator_fg);
        for y in top..bottom {
            frame.render_widget(Paragraph::new("│").style(style), Rect::new(x, y, 1, 1));
        }
    }
}

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
