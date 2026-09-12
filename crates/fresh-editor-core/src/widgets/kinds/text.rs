//! `Text` — unified single-line / multi-line text field, with markdown
//! document mode, completion popups, and the block caret.

use std::collections::HashMap;

use fresh_core::api::{OverlayOptions, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::WidgetInstanceState;
use crate::widgets::render::{
    completion_scrollbar_glyph, ensure_trailing_newline, fit_label, focus_gutter_prefix,
    form_label_width, ratatui_style_to_overlay, render_completion_bottom_border,
    render_completion_dim_separator_overlay, render_completion_item_overlay, render_text_input,
};

pub struct Text;

impl WidgetImpl for Text {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        key: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        use super::KeyDisposition::{Consumed, Pass, PassAfter};
        // The completion popup claims its keys first, and only while
        // showing.
        if matches!(key, "Tab" | "Up" | "Down" | "Enter" | "Escape")
            && completions_open(widget_key, panel)
        {
            return match key {
                "Up" => {
                    move_completion_index(spec, widget_key, panel, -1);
                    Consumed
                }
                "Down" => {
                    move_completion_index(spec, widget_key, panel, 1);
                    Consumed
                }
                "Escape" => {
                    // First Esc only closes the popup — the form stays
                    // open. (A second Esc, with no popup, cancels.)
                    dismiss_completions(widget_key, panel, fx);
                    Consumed
                }
                "Enter" | "Tab" => {
                    if completion_navigated(widget_key, panel) {
                        // The user stepped into the dropdown (↑/↓/wheel)
                        // so a row is highlighted — accept it. The host
                        // does NOT close the popup: directory-descent
                        // flows (the orchestrator's Project Path
                        // accepting `/foo/` re-fetches children) keep it
                        // alive; plugins that want one-shot accept close
                        // it via `setCompletions(key, [])`.
                        if let Some(value) = selected_completion_value(widget_key, panel) {
                            fx.events.push((
                                "completion_accept".into(),
                                serde_json::json!({ "value": value }),
                            ));
                        }
                        return Consumed;
                    }
                    // Not navigated: the popup must not swallow the key.
                    // Close it, then let Enter act on the form (submit /
                    // advance) and Tab advance focus.
                    dismiss_completions(widget_key, panel, fx);
                    PassAfter
                }
                _ => Pass,
            };
        }
        // The editing vocabulary. Caret motion, mutation, selection
        // chords, clipboard, and multi-line paging are the field's
        // own; what stays panel policy is the single-line field's
        // Up/Down (picker forwarding to a sibling list) and Enter
        // (submit / advance) — those Pass.
        let WidgetSpec::Text { rows, .. } = spec else {
            return Pass;
        };
        match key {
            "Up" | "Down" | "PageUp" | "PageDown" if *rows <= 1 => Pass,
            "Up" | "Down" | "Left" | "Right" | "Backspace" | "Delete" | "Home" | "End" | "S-Up"
            | "S-Down" | "S-Left" | "S-Right" | "S-Home" | "S-End" | "C-Left" | "C-Right"
            | "C-S-Left" | "C-S-Right" | "C-Backspace" | "C-Delete" => {
                text_key(spec, widget_key, panel, key, fx);
                Consumed
            }
            "PageUp" | "PageDown" => {
                // Multi-line: page the caret (the viewport follows
                // it), one row of overlap like the lists so the user
                // keeps a visual anchor across pages.
                let page = rows.saturating_sub(1).max(1) as i32;
                let down = key == "PageDown";
                clear_user_scrolled(widget_key, panel);
                apply_edit(spec, widget_key, panel, fx, |editor| {
                    for _ in 0..page.unsigned_abs() {
                        if down {
                            editor.move_down();
                        } else {
                            editor.move_up();
                        }
                    }
                });
                Consumed
            }
            "Enter" => {
                if *rows <= 1 {
                    // Form policy (submit / picker-activate / advance)
                    // belongs to the panel.
                    return Pass;
                }
                text_key(spec, widget_key, panel, "Enter", fx);
                Consumed
            }
            "Space" => {
                insert_str_edit(spec, widget_key, panel, " ", fx);
                Consumed
            }
            "C-c" => {
                // Copy is consumed even with an empty selection so it
                // doesn't fall through to the buffer's copy path.
                if let Some(text) = selected_text(widget_key, panel) {
                    fx.clipboard_copy = Some(text);
                }
                Consumed
            }
            "C-x" => {
                if let Some(text) = selected_text(widget_key, panel) {
                    fx.clipboard_copy = Some(text);
                    // On a read-only / markdown document, Cut degrades
                    // to Copy: the selection reaches the clipboard,
                    // nothing is deleted.
                    if !mode(spec).1 {
                        apply_edit(spec, widget_key, panel, fx, |editor| {
                            editor.delete_selection();
                        });
                    }
                }
                Consumed
            }
            "C-a" => {
                // SelectAll moves the cursor to end-of-value and sets
                // anchor at start; `apply_edit` skips the change event
                // when nothing moved.
                apply_edit(spec, widget_key, panel, fx, |editor| editor.select_all());
                Consumed
            }
            "C-z" => {
                // Engine undo (history lives in the TextEdit itself);
                // routing through apply_edit fires `change` with the
                // restored value so a plugin mirror stays in sync.
                apply_edit(spec, widget_key, panel, fx, |editor| {
                    editor.undo();
                });
                Consumed
            }
            "C-y" => {
                apply_edit(spec, widget_key, panel, fx, |editor| {
                    editor.redo();
                });
                Consumed
            }
            _ => Pass,
        }
    }

    /// Typed text lands in the field at the caret, replacing any
    /// selection; a read-only or markdown field takes none and passes.
    fn on_text(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        text: &str,
        fx: &mut super::KeyFx,
    ) -> super::KeyDisposition {
        if text.is_empty() || mode(spec).1 {
            return super::KeyDisposition::Pass;
        }
        insert_str_edit(spec, widget_key, panel, text, fx);
        super::KeyDisposition::Consumed
    }

    /// Pointer model: a click in the field's editable area moves the
    /// caret to the clicked byte, matching every GUI text input
    /// (#2573). The click-cell → value-byte mapping (and the
    /// markdown-document row variant) is click-path knowledge the
    /// panel doesn't have, so the kind *requests* the placement and
    /// the dispatcher runs the host helper. The recorded `focus`
    /// event still fires — plugins mirror the caret from it.
    fn on_pointer(
        &self,
        _spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        event_type: &str,
        _payload: &serde_json::Value,
        fx: &mut super::PointerFx,
    ) -> super::PointerDisposition {
        if event_type == "focus" {
            fx.place_caret = true;
        }
        super::PointerDisposition::Default
    }

    fn on_wheel(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
        _viewport: super::Viewport,
        delta: i32,
    ) -> bool {
        let WidgetSpec::Text {
            completions_visible_rows,
            ..
        } = spec
        else {
            return false;
        };
        // Only an open completion popup scrolls here — the one window in a
        // described panel the tree does not own, reached through
        // `UiFact::WidgetWheel` by name. The field's own rows are the
        // tree's viewport, which takes its own wheel. Scrolling counts as
        // stepping into the popup: Enter then accepts the highlighted row.
        if let Some(WidgetInstanceState::Text {
            completions,
            completion_scroll_offset,
            completion_navigated,
            ..
        }) = panel.instance_states.get_mut(widget_key)
        {
            if !completions.is_empty() {
                let visible = if *completions_visible_rows == 0 {
                    5u32
                } else {
                    *completions_visible_rows
                };
                *completion_navigated = true;
                let total = completions.len() as u32;
                let max_scroll = total.saturating_sub(visible.min(total));
                let next = (*completion_scroll_offset as i32 + delta).clamp(0, max_scroll as i32);
                *completion_scroll_offset = next as u32;
                return true;
            }
        }
        false
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("text");
        if let WidgetSpec::Text {
            key: Some(k),
            rows,
            markdown,
            ..
        } = spec
        {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
                m.scrollable = *rows > 1;
                // A markdown document view scrolls like a list; plain
                // editable textareas stay excluded (they scroll with
                // their caret and are not picker targets).
                m.picker_scroll_target = *markdown && *rows > 1;
            }
        }
        m
    }
}

/// Resolve the column width a single-line `full_width` text field's
/// bracketed region should occupy: `panel_width` minus the label prefix,
/// the two surrounding `[` / `]` brackets plus the trailing cursor-park
/// column (3), and the focus-marker gutter when the panel opted in.
/// Reserving these unconditionally keeps the rendered width stable
/// across the focus transition so the box never reflows or overflows the
/// enclosing section. Multi-line fields and non-`full_width` fields use
/// the plugin-supplied `field_width` verbatim (`render_text_area`
/// already fills the panel width by default).
#[allow(clippy::too_many_arguments)]
pub fn effective_text_field_width(
    full_width: bool,
    multiline: bool,
    label: &str,
    panel_width: u32,
    field_width: u32,
    marker_gutter: bool,
) -> u32 {
    if !full_width || multiline {
        return field_width;
    }
    let label_overhead = if label.is_empty() {
        0u32
    } else {
        label.chars().count() as u32 + 1
    };
    let marker_reserve = if marker_gutter { 2 } else { 0 };
    panel_width
        .saturating_sub(label_overhead)
        .saturating_sub(3)
        .saturating_sub(marker_reserve)
        .max(1)
}

/// The completion pop-over's rows, in paint order: the dim separator that
/// takes over the enclosing section's bottom border, the windowed candidate
/// rows, and the popup's own bottom border.
pub struct CompletionPopup {
    /// Separator, items, bottom border — one entry per row, each already
    /// carrying its own `│ … │` chrome.
    pub rows: Vec<TextPropertyEntry>,
    /// The scroll offset to persist for the next render.
    pub scroll: u32,
    /// How many candidate rows the window shows, which is the height of the
    /// popup less its two chrome rows.
    pub visible: u32,
}

/// **The completion pop-over: how many rows, which ones, and what they say.**
///
/// `panel_width` is the inner width the wrapping `LabeledSection` handed us
/// (already minus its 4 columns of `│ … │` chrome); the popup widens by 4 so
/// the side borders it paints line up with the section's. That is why the
/// float it becomes has to start two columns *left* of the child — see
/// `view::shell::widgets`'s `Site::escape`.
///
/// Scroll is *forward-only*: when the selection walks past the bottom of the
/// window the view pulls forward to keep it visible, but it is never pulled
/// back if the selection sits above the window — the mouse-wheel handler
/// deliberately diverges scroll from selection, and a back-pull would undo the
/// wheel on the next render.
///
/// Default popup height is 5 visible rows. Plugins override per-widget by
/// setting `completions_visible_rows`; 0 falls back to the default so the
/// orchestrator's existing `text({...})` calls Just Work.
///
/// Pulled out of the collector because the *description* needs the same rows,
/// and a second copy of the windowing would be a second place for it to drift
/// from the scroll offset the collector persists. Pure — no `out`, no
/// `next_state` — which is what lets a description call it.
pub fn completion_popup(
    completions: &[fresh_core::api::CompletionItem],
    completions_visible_rows: u32,
    panel_width: u32,
    selected_idx: usize,
    navigated: bool,
    prev_scroll: u32,
    marker_gutter: bool,
) -> Option<CompletionPopup> {
    if completions.is_empty() {
        return None;
    }
    let visible_rows = if completions_visible_rows == 0 {
        5u32
    } else {
        completions_visible_rows
    };
    let popup_total = (panel_width as usize).saturating_add(4); // re-add section chrome
    let total = completions.len() as u32;
    let visible = visible_rows.max(1).min(total);
    let sel = selected_idx as u32;
    let mut scroll = prev_scroll;
    if sel >= scroll + visible {
        scroll = sel + 1 - visible;
    }
    let max_scroll = total.saturating_sub(visible);
    if scroll > max_scroll {
        scroll = max_scroll;
    }

    let mut rows = Vec::with_capacity(visible as usize + 2);
    rows.push(render_completion_dim_separator_overlay(popup_total));
    let needs_scrollbar = total > visible;
    let end = (scroll + visible).min(total) as usize;
    for (visible_row, i) in (scroll as usize..end).enumerate() {
        let item = &completions[i];
        let thumb = if needs_scrollbar {
            completion_scrollbar_glyph(visible_row as u32, visible, scroll, total)
        } else {
            None
        };
        rows.push(render_completion_item_overlay(
            &item.value,
            item.kind.as_deref(),
            // Only paint a selected-row highlight once the user
            // has stepped into the dropdown (↓/↑). A freshly
            // surfaced popup shows plain suggestions so it's
            // clear Enter acts on the form, not the list.
            navigated && i == selected_idx,
            popup_total,
            thumb,
            marker_gutter,
        ));
    }
    rows.push(render_completion_bottom_border(popup_total));
    Some(CompletionPopup {
        rows,
        scroll,
        visible,
    })
}

#[allow(clippy::too_many_arguments)]
/// Push a one-cell REVERSED overlay at `byte` in `entry` — the block
/// caret used by modal form surfaces (`block_caret` Text widgets and
/// the Number edit cell), where a hardware cursor isn't visible.
/// Clamps to the entry text; a caret at end-of-text reverses the last
/// cell if there is one (renderers reserve a trailing pad cell).
pub fn push_block_caret_overlay(entry: &mut TextPropertyEntry, byte: usize) {
    let text = &entry.text;
    let b = byte.min(text.len());
    let (start, end) = if b < text.len() {
        let ch_len = text[b..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
        (b, b + ch_len)
    } else if !text.is_empty() {
        // End-of-text: reverse the final cell.
        let last_start = text
            .char_indices()
            .last()
            .map(|(i, _)| i)
            .unwrap_or(text.len().saturating_sub(1));
        (last_start, text.len())
    } else {
        return;
    };
    entry.inline_overlays.push(InlineOverlay {
        start,
        end,
        style: OverlayOptions {
            reversed: true,
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
}

/// Markdown-mode multi-line Text: a read-only *document* view.
///
/// The value renders through the shared markdown engine (the same one
/// behind LSP hover docs) and word-wraps to the widget's width; a shadow
/// [`TextEdit`](crate::primitives::text_edit::TextEdit) holds the
/// rendered **plain text** so the caret, selection, and Copy operate on
/// exactly what's on screen — never on markdown markers, and never on
/// the chrome of sibling widgets sharing a merged row. The shadow (and
/// with it the caret) resets whenever the rendered text changes (new
/// value or new width); scroll state and `user_scrolled` follow the
/// List/Tree contract.
#[allow(clippy::too_many_arguments)]
/// The whole markdown document as **one** styled entry: the rendered text with
/// one inline overlay per span, unwrapped.
///
/// **Unwrapped is the point.** `render_markdown_text_area` wraps the parse to a
/// width it is handed and then emits a row per rendered line, which makes a row
/// a fact of the renderer rather than of the layout — so the caller has to
/// re-state the width it laid out at, and the caret, the selection and the
/// scroll all end up in coordinates only a second wrap can produce. Handed over
/// as one run instead, the wrap is `fresh-ui`'s at the width *it* settled, and
/// a byte of this string is the one coordinate everyone shares (L5).
///
/// **The indent is normalised back to spaces.** `parse_markdown` turns leading
/// whitespace into NBSP so the markdown parser does not read an indented line
/// as a code block, and lays list markers behind NBSP for the same reason; a
/// wrapper that treats NBSP as space-like then puts the hanging indent back.
/// `fresh-ui` breaks on `' '` only — correctly, since NBSP exists to *prevent*
/// a break — so a line's own leading run is converted here, where it is known
/// to be indentation. An NBSP the author wrote *inside* a line keeps its
/// meaning.
pub fn markdown_document(
    value: &str,
    md: Option<crate::widgets::MarkdownCtx<'_>>,
) -> TextPropertyEntry {
    use crate::markdown::parse_markdown;
    let lines = match md {
        Some(md) => parse_markdown(value, md.theme, md.grammars),
        // No theme (unit tests, plugin-less hosts): the source, unstyled.
        None => value
            .split('\n')
            .map(|l| {
                let mut sl = crate::markdown::StyledLine::new();
                sl.push(l.to_string(), ratatui::style::Style::default());
                sl
            })
            .collect(),
    };
    markdown_entry_from_lines(&lines)
}

/// [`markdown_document`] with the parse already done: the assembly half, which
/// is where the normalisation and the overlay offsets have to agree.
fn markdown_entry_from_lines(lines: &[crate::markdown::StyledLine]) -> TextPropertyEntry {
    let mut text = String::new();
    let mut overlays: Vec<InlineOverlay> = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        if i > 0 {
            text.push('\n');
        }
        // The line's own leading whitespace, across spans: everything up to its
        // first non-space-like character is indentation.
        let mut leading = true;
        for span in &line.spans {
            let start = text.len();
            match leading {
                true => {
                    let n = span
                        .text
                        .chars()
                        .take_while(|c| crate::markdown::is_space(*c))
                        .count();
                    let head: String = span.text.chars().take(n).map(|_| ' ').collect();
                    text.push_str(&head);
                    text.push_str(
                        &span.text[span
                            .text
                            .char_indices()
                            .nth(n)
                            .map(|(b, _)| b)
                            .unwrap_or(span.text.len())..],
                    );
                    leading = n == span.text.chars().count();
                }
                false => text.push_str(&span.text),
            }
            if let Some(style) = ratatui_style_to_overlay(span.style) {
                overlays.push(InlineOverlay {
                    start,
                    end: text.len(),
                    style,
                    properties: Default::default(),
                    unit: OffsetUnit::Byte,
                });
            }
        }
    }
    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// A `Text`'s state, once the spec and the instance map have been reconciled.
///
/// Host-owned value and cursor (plus scroll, and the completion fields the
/// plugin pushes rather than the spec): instance state if it exists, else
/// seeded from the spec on first render. See `WidgetInstanceState::Text`.
pub struct Resolved {
    /// The editor the row is rendered from — cursor and selection included.
    pub editor: crate::primitives::text_edit::TextEdit,
    /// First visible row (multi-line) or first painted value char
    /// (single-line): the window the previous render left behind.
    pub scroll: u32,
    /// The candidate list the plugin pushed through `SetCompletions`.
    pub completions: Vec<fresh_core::api::CompletionItem>,
    /// The highlighted candidate, clamped into `completions`.
    pub completion_index: usize,
    /// The completion window's first visible candidate.
    pub completion_scroll: u32,
    /// Whether the user has stepped into the list (↑/↓/wheel) — which is what
    /// makes Enter act on the candidate rather than on the form.
    pub completion_navigated: bool,
}

/// **Where a `Text`'s value, cursor and completion list actually come from.**
///
/// Instance state is authoritative once it exists; the spec's `value` /
/// `cursor_byte` are a seed for the first render only. Completions never come
/// from the spec at all — plugins push them through `SetCompletions` — so they
/// are carried across renders verbatim, and the index is clamped **once per
/// render** so a list that shrank host-side (or arrived empty) does not keep a
/// stale out-of-bounds index alive.
///
/// Pulled out of the collector because the *description* needs the same
/// answer, and a second copy of these rules is a second place for them to
/// drift. Pure — it never writes `next_state` — which is what lets a
/// description call it. See `view::shell::widgets`'s single-line `Text` arm.
pub fn resolve(
    value: &str,
    cursor_byte: i32,
    multiline: bool,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
) -> Resolved {
    let mut st = match key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k)) {
        Some(WidgetInstanceState::Text {
            editor,
            scroll,
            completions,
            completion_selected_index,
            completion_scroll_offset,
            completion_navigated,
            ..
        }) => Resolved {
            editor: editor.clone(),
            scroll: *scroll,
            completions: completions.clone(),
            completion_index: *completion_selected_index,
            completion_scroll: *completion_scroll_offset,
            completion_navigated: *completion_navigated,
        },
        _ => {
            let mut editor = if multiline {
                crate::primitives::text_edit::TextEdit::with_text(value)
            } else {
                crate::primitives::text_edit::TextEdit::single_line_with_text(value)
            };
            let seed = if cursor_byte < 0 {
                value.len()
            } else {
                (cursor_byte as usize).min(value.len())
            };
            editor.set_cursor_from_flat(seed);
            Resolved {
                editor,
                scroll: 0,
                completions: Vec::new(),
                completion_index: 0,
                completion_scroll: 0,
                completion_navigated: false,
            }
        }
    };
    st.completion_index = match st.completions.len() {
        0 => 0,
        n => st.completion_index.min(n - 1),
    };
    st
}

/// The byte range the selection band paints over, or `None`.
///
/// Only meaningful for the focused widget — `None` otherwise keeps the
/// no-selection rendering paths unchanged. The editor's own selection wins; a
/// spec-seeded render (stateless surfaces like Settings, which re-emit their
/// model each frame) falls back to the spec's `sel_start`/`sel_end` byte
/// range, clamped into the value.
pub fn selection_of(
    editor: &crate::primitives::text_edit::TextEdit,
    is_focused: bool,
    spec_sel: (i32, i32),
) -> Option<(usize, usize)> {
    if !is_focused {
        return None;
    }
    editor.selection_flat_range().or({
        let (a, b) = spec_sel;
        if a >= 0 && b > a {
            let len = editor.value().len();
            Some(((a as usize).min(len), (b as usize).min(len)))
        } else {
            None
        }
    })
}

/// A single-line `Text`'s one row: what it says, where the caret is in it, and
/// what a press on it means.
pub struct SingleLine {
    /// The rendered row, gutter prepended and block caret (if any) already on
    /// it.
    pub entry: TextPropertyEntry,
    /// Byte offset of the caret within `entry.text`, gutter included; `None`
    /// when the field is unfocused.
    pub caret: Option<usize>,
    /// What a press on the row means, present only for a keyed field: a
    /// `focus` whose payload carries the value-layout breadcrumbs the click
    /// handler needs to reposition the cursor to the clicked column
    /// (`valueInnerStart` is where the value's `<inner>` region begins in
    /// this row's text, after the gutter; the truncation fields translate a
    /// click over a `…`-prefixed tail view back to a value byte). The
    /// description attaches it to the whole row; an *unkeyed* field has
    /// none, because with nothing to name it could not say what it focused.
    pub event: Option<crate::widgets::WidgetEvent>,
    /// The horizontal window `render_text_input` chose — the first painted
    /// value char, to hand back on the next render.
    pub scroll: u32,
}

/// **The single-line field's row: label column, value cell, focus gutter,
/// caret and press.**
///
/// Pulled out of the collector whole, because every one of the rules below is
/// a rule about *this row* rather than about the immediate-mode walk that used
/// to contain it, and the description needs each of them. Pure — no state
/// written — which is what lets a description call it. See `view::shell::widgets`'s single-line `Text` arm.
///
/// `scroll` cannot be written back by a description: it is the horizontal
/// window the *next* render starts from, and only the runtime's own pass owns
/// that write. A described field therefore reads the offset the runtime last
/// persisted, which is the same position the described `Dropdown` is in. 2.1
/// is where that stops being two parties.
#[allow(clippy::too_many_arguments)]
pub fn single_line(
    editor: &crate::primitives::text_edit::TextEdit,
    prev_scroll: u32,
    label: &str,
    placeholder: Option<&str>,
    field_width: u32,
    max_visible_chars: u32,
    full_width: bool,
    block_caret: bool,
    spec_sel: (i32, i32),
    label_width: u32,
    is_focused: bool,
    key: Option<&str>,
    marker_gutter: bool,
    panel_width: u32,
) -> SingleLine {
    let value = editor.value();
    let cursor = if is_focused {
        editor.flat_cursor_byte() as i32
    } else {
        -1
    };
    // Form-column alignment: when `label_width > 0`, pad the label to
    // the column and terminate it with `:` so the value cell's `[` lines
    // up with the sibling Toggle/Number/Dropdown cells (which render
    // `{label}: [..]`). `render_text_input` appends the ` ` + `[`, so the
    // composed label carries only up to the colon. `label_width == 0`
    // keeps the compact `{label} [..]` plugins get by default. This is
    // computed before the field width so the value cell is sized against
    // the *padded* label overhead (else the wider label overflows the
    // control's right edge).
    let composed_label;
    let effective_label: &str = if label_width > 0 && !label.is_empty() {
        let lw = form_label_width(
            label_width,
            focus_gutter_prefix(is_focused, marker_gutter).len(),
            // Reserve the bracketed cell + a couple cells of value so the
            // field opening stays on-screen on a narrow surface.
            "[  ]".len(),
            panel_width,
        );
        composed_label = format!("{}:", fit_label(label, lw));
        &composed_label
    } else {
        label
    };
    let rendered = render_text_input(
        &value,
        cursor,
        selection_of(editor, is_focused, spec_sel),
        is_focused,
        effective_label,
        placeholder,
        max_visible_chars,
        effective_text_field_width(
            full_width,
            false,
            effective_label,
            panel_width,
            field_width,
            marker_gutter,
        ),
        full_width,
        prev_scroll,
    );
    let mut entry = rendered.entry;
    // Lead the single-line input with the focus-marker gutter
    // (`▸ ` when focused, two spaces otherwise) so focus is
    // legible from a plain capture — the hardware cursor lands
    // inside the field too, but a cursor doesn't show up in
    // `tmux capture-pane`. Shift the cursor offset and every
    // inline overlay right by the gutter's byte length so the
    // bracket bg / placeholder / selection spans still line up.
    // The field width was already reduced by the gutter's two
    // columns above, so the box doesn't overflow, and the gutter
    // is present whether or not the field is focused so the
    // layout never shifts.
    let gutter = focus_gutter_prefix(is_focused, marker_gutter);
    let marker_bytes = gutter.len();
    let mut cursor_in_row = rendered.cursor_byte_in_entry;
    if marker_bytes > 0 {
        entry.text.insert_str(0, gutter);
        for ov in entry.inline_overlays.iter_mut() {
            ov.start += marker_bytes;
            ov.end += marker_bytes;
        }
        cursor_in_row = cursor_in_row.map(|c| c + marker_bytes);
    }
    if let Some(byte_in_row) = cursor_in_row {
        // Modal surfaces paint the caret as a REVERSED cell in the
        // row itself (no hardware cursor over a modal).
        if block_caret {
            push_block_caret_overlay(&mut entry, byte_in_row);
        }
    }
    // A click anywhere on the input line focuses the field so a mouse user
    // can type. Text widgets previously emitted no hit area, so clicks fell
    // through and the field stayed unfocused (#2234 item 1). What focuses the
    // field is `deliver_widget_hit`, which every frontend's press goes
    // through — the described row's node, the web's index, the text
    // projection's byte scan; the `focus` event keeps the plugin's focus
    // mirror in step.
    //
    let event = key
        .filter(|k| !k.is_empty())
        .map(|k| crate::widgets::WidgetEvent {
            row_target: false,
            context_click: false,
            widget_key: k.to_string(),
            widget_kind: "text",
            payload: json!({
                "valueInnerStart": marker_bytes + rendered.inner_byte_start,
                "valueDropped": rendered.value_dropped_bytes,
                "ellipsisBytes": rendered.ellipsis_bytes,
                "valueLen": rendered.value_len,
            }),
            event_type: "focus",
            owner_key: None,
        });
    ensure_trailing_newline(&mut entry);
    SingleLine {
        entry,
        caret: cursor_in_row,
        event,
        scroll: rendered.scroll_chars,
    }
}

/// Is this Text widget's completion popup showing?
/// `(markdown, read_only)` for a Text spec. A `markdown` multi-line
/// Text is forcibly read-only; a plain Text honours its `read_only`
/// flag; a non-Text spec is `(false, false)`.
pub fn mode(spec: &WidgetSpec) -> (bool, bool) {
    let WidgetSpec::Text {
        markdown,
        read_only,
        rows,
        ..
    } = spec
    else {
        return (false, false);
    };
    let md = *markdown && *rows > 1;
    (md, md || *read_only)
}

/// Ensure `panel.instance_states[widget_key]` is a seeded
/// `Text { editor, .. }`. If instance state already has the entry,
/// no-op. If not, seeds from the spec's `value` / `cursor_byte` /
/// `rows`. Returns true when the widget is a Text now present in
/// instance state.
///
/// **The one seed a text field has.** Nothing writes a field's state on a
/// render — the description resolves the spec against whatever is stored
/// (`resolve`), and a walk that seeded would be a second authority. The
/// first *handler* that has to hold state seeds it here: an edit, a caret
/// move, or a plugin pushing completions onto a field nobody has typed in.
pub fn ensure_text_state(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
) -> bool {
    if matches!(
        panel.instance_states.get(widget_key),
        Some(WidgetInstanceState::Text { .. })
    ) {
        return true;
    }
    let WidgetSpec::Text {
        value,
        cursor_byte,
        rows,
        ..
    } = spec
    else {
        return false;
    };
    let mut editor = if *rows > 1 {
        crate::primitives::text_edit::TextEdit::with_text(value)
    } else {
        crate::primitives::text_edit::TextEdit::single_line_with_text(value)
    };
    let seed = if *cursor_byte < 0 {
        value.len()
    } else {
        (*cursor_byte as usize).min(value.len())
    };
    editor.set_cursor_from_flat(seed);
    panel.instance_states.insert(
        widget_key.to_string(),
        WidgetInstanceState::Text {
            editor,
            scroll: 0,
            completions: Vec::new(),
            completion_selected_index: 0,
            completion_scroll_offset: 0,
            completion_navigated: false,
            user_scrolled: false,
        },
    );
    true
}

/// Apply a mutating operation to the widget's `TextEdit`. Handles
/// seeding the editor from the spec on first touch, no-op detection
/// (skips the change event), and queueing the `change` event with
/// the post-state. Returns true when the op ran *and* produced a
/// visible change. The single mutation path — the host's
/// `with_focused_text_editor` shell and every key here go through
/// it.
pub fn apply_edit(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    fx: &mut super::KeyFx,
    op: impl FnOnce(&mut crate::primitives::text_edit::TextEdit),
) -> bool {
    if !ensure_text_state(spec, widget_key, panel) {
        return false;
    }
    let Some(WidgetInstanceState::Text { editor, .. }) = panel.instance_states.get_mut(widget_key)
    else {
        return false;
    };
    let (before_value, before_cursor) = (editor.value(), editor.flat_cursor_byte());
    op(editor);
    let (after_value, after_cursor) = (editor.value(), editor.flat_cursor_byte());
    if after_value == before_value && after_cursor == before_cursor {
        return false;
    }
    fx.events.push((
        "change".into(),
        json!({ "value": after_value, "cursorByte": after_cursor as i64, }),
    ));
    true
}

/// Clear the widget's `user_scrolled` flag (re-arming
/// keep-caret-visible). Returns true when the flag was set.
pub fn clear_user_scrolled(widget_key: &str, panel: &mut crate::widgets::WidgetPanelState) -> bool {
    match panel.instance_states.get_mut(widget_key) {
        Some(WidgetInstanceState::Text { user_scrolled, .. }) if *user_scrolled => {
            *user_scrolled = false;
            true
        }
        _ => false,
    }
}

/// The widget's current selection, if its editor holds one.
pub fn selected_text(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> Option<String> {
    match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Text { editor, .. }) => editor.selected_text(),
        _ => None,
    }
}

/// Insert printable / IME-committed text at the cursor (replacing
/// any active selection). Read-only and markdown fields accept no
/// insertion. `TextEdit::insert_str` strips embedded newlines when
/// the editor is single-line.
pub fn insert_str_edit(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    text: &str,
    fx: &mut super::KeyFx,
) {
    if text.is_empty() || mode(spec).1 {
        return;
    }
    apply_edit(spec, widget_key, panel, fx, |editor| {
        editor.insert_str(text);
    });
}

/// Apply a non-printable editing key. Every caret-motion / mutation
/// key routes through the shared
/// [`apply_text_key`](crate::primitives::text_key::apply_text_key)
/// table — the single source of truth the Settings input handler
/// also uses, so the two surfaces can't drift. `Enter` = newline is
/// the one widget-multiline affordance the shared table deliberately
/// leaves as chrome (it means "commit" on other surfaces), so it's
/// handled here: a markdown document has no newline to insert —
/// Enter is its activate gesture (the tour jumps to the step's
/// code).
pub fn text_key(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    key: &str,
    fx: &mut super::KeyFx,
) {
    let (is_markdown, is_read_only) = mode(spec);
    if key == "Enter" {
        if is_markdown {
            fx.events.push(("activate".into(), json!({})));
            return;
        }
        if is_read_only {
            return;
        }
        apply_edit(spec, widget_key, panel, fx, |editor| {
            editor.insert_char('\n');
        });
        return;
    }
    let Some(event) = key_name_to_event(key) else {
        return;
    };
    if is_read_only && key_mutates(&event) {
        return;
    }
    // A key-driven caret move re-arms follow-the-caret: even if the
    // caret was already at a boundary (the op below no-ops), the
    // viewport must snap back from a wheel-scrolled position on the
    // repaint that follows.
    clear_user_scrolled(widget_key, panel);
    apply_edit(spec, widget_key, panel, fx, |editor| {
        crate::primitives::text_key::apply_text_key(
            editor,
            &event,
            crate::primitives::text_key::TextKeyContext::multiline(true),
        );
    });
}

/// Re-hydrate a widget key name back into a `KeyEvent` so text
/// fields can share the editor's text-key table rather than their
/// own dispatch. Only the named keys the router forwards to text
/// fields are recognized; `"Enter"` is handled by the caller.
pub(super) fn key_name_to_event(name: &str) -> Option<crossterm::event::KeyEvent> {
    use crossterm::event::{KeyCode, KeyEvent, KeyModifiers};
    // Peel `C-` / `S-` / `A-` prefixes (in any order) so shift-selection
    // and word-motion chords reach the shared text-key table — a
    // markdown document view needs `S-Down` to extend the selection.
    let mut modifiers = KeyModifiers::NONE;
    let mut rest = name;
    loop {
        if let Some(r) = rest.strip_prefix("C-") {
            modifiers |= KeyModifiers::CONTROL;
            rest = r;
        } else if let Some(r) = rest.strip_prefix("S-") {
            modifiers |= KeyModifiers::SHIFT;
            rest = r;
        } else if let Some(r) = rest.strip_prefix("A-") {
            modifiers |= KeyModifiers::ALT;
            rest = r;
        } else {
            break;
        }
    }
    let code = match rest {
        "Backspace" => KeyCode::Backspace,
        "Delete" => KeyCode::Delete,
        "Left" => KeyCode::Left,
        "Right" => KeyCode::Right,
        "Up" => KeyCode::Up,
        "Down" => KeyCode::Down,
        "Home" => KeyCode::Home,
        "End" => KeyCode::End,
        _ => return None,
    };
    Some(KeyEvent::new(code, modifiers))
}

/// Whether routing `event` through `apply_text_key` would mutate the
/// surface. Everything else in the table is caret motion / selection.
fn key_mutates(event: &crossterm::event::KeyEvent) -> bool {
    use crossterm::event::KeyCode;
    matches!(
        event.code,
        KeyCode::Char(_) | KeyCode::Backspace | KeyCode::Delete
    )
}

pub fn completions_open(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> bool {
    matches!(
        panel.instance_states.get(widget_key),
        Some(WidgetInstanceState::Text { completions, .. }) if !completions.is_empty()
    )
}

/// Has the user explicitly stepped into the popup (↑/↓ / wheel)? Only
/// a *navigated* popup accepts on Enter/Tab — a freshly surfaced one
/// lets the key act on the form instead.
fn completion_navigated(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> bool {
    matches!(
        panel.instance_states.get(widget_key),
        Some(WidgetInstanceState::Text {
            completions,
            completion_navigated,
            ..
        }) if !completions.is_empty() && *completion_navigated
    )
}

/// Move the completion selection by `delta` (clamped, no wraparound —
/// wrap on a popup picker reads as jarring while comparing rows). The
/// first ↑/↓ *enters* the dropdown: it flips `navigated` and selects
/// the current (top) row without moving. Keyboard moves also pull the
/// scroll window back so the selection stays visible (forward-pull is
/// the renderer's job).
fn move_completion_index(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    delta: i32,
) {
    let spec_visible_rows = match spec {
        WidgetSpec::Text {
            completions_visible_rows,
            ..
        } => *completions_visible_rows,
        _ => 0,
    };
    let visible = if spec_visible_rows == 0 {
        5u32
    } else {
        spec_visible_rows
    };
    if let Some(WidgetInstanceState::Text {
        completions,
        completion_selected_index,
        completion_scroll_offset,
        completion_navigated,
        ..
    }) = panel.instance_states.get_mut(widget_key)
    {
        if completions.is_empty() {
            return;
        }
        if !*completion_navigated {
            *completion_navigated = true;
            return;
        }
        let max = (completions.len() - 1) as i32;
        let cur = *completion_selected_index as i32;
        let next = (cur + delta).clamp(0, max);
        *completion_selected_index = next as usize;
        let next_u = next as u32;
        if next_u < *completion_scroll_offset {
            *completion_scroll_offset = next_u;
        } else if next_u >= *completion_scroll_offset + visible {
            *completion_scroll_offset = next_u + 1 - visible;
        }
    }
}

/// Close the popup and queue `completion_dismiss` so the plugin can
/// sync its own state (e.g. invalidate an in-flight fetch token, so a
/// late-arriving result doesn't re-open the popup the user closed).
fn dismiss_completions(
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    fx: &mut super::KeyFx,
) {
    if let Some(WidgetInstanceState::Text {
        completions,
        completion_selected_index,
        ..
    }) = panel.instance_states.get_mut(widget_key)
    {
        if completions.is_empty() {
            return;
        }
        completions.clear();
        *completion_selected_index = 0;
        fx.events
            .push(("completion_dismiss".into(), serde_json::json!({})));
    }
}

/// The currently-highlighted candidate's value, if any.
fn selected_completion_value(
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> Option<String> {
    match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Text {
            completions,
            completion_selected_index,
            ..
        }) if !completions.is_empty() => {
            let idx = (*completion_selected_index).min(completions.len() - 1);
            Some(completions[idx].value.clone())
        }
        _ => None,
    }
}

#[cfg(test)]
mod markdown_document_tests {
    use super::*;

    /// **The indent comes back as spaces.** `parse_markdown` writes NBSP so the
    /// markdown parser does not read an indented line as a code block; a run
    /// handed to `fresh-ui` must break and indent on `' '`, which is the one
    /// thing NBSP exists to prevent.
    #[test]
    fn a_lines_leading_nbsp_is_normalised_and_one_inside_it_is_not() {
        let mut line = crate::markdown::StyledLine::new();
        line.push(
            "\u{00A0}\u{00A0}• ".into(),
            ratatui::style::Style::default(),
        );
        line.push(
            "an item\u{00A0}kept whole".into(),
            ratatui::style::Style::default(),
        );
        let text = document_text_of(&[line]);
        assert_eq!(
            text, "  • an item\u{00A0}kept whole",
            "leading NBSP is indentation; one inside the line is the author's"
        );
    }

    /// The overlays index the text they were built beside — the property
    /// `entry_runs` slices on, and the one the normalisation above could break
    /// by changing byte lengths as it goes.
    #[test]
    fn every_overlay_indexes_the_text_it_was_built_with() {
        let mut line = crate::markdown::StyledLine::new();
        line.push("\u{00A0}\u{00A0}".into(), ratatui::style::Style::default());
        line.push(
            "bold".into(),
            ratatui::style::Style::default().add_modifier(ratatui::style::Modifier::BOLD),
        );
        let e = entry_of(&[line]);
        assert_eq!(e.text, "  bold");
        let o = e
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("the bold span");
        assert_eq!(&e.text[o.start..o.end], "bold");
    }

    /// `markdown_document` with the lines already parsed — the half these tests
    /// are about, without a theme to build.
    fn entry_of(lines: &[crate::markdown::StyledLine]) -> TextPropertyEntry {
        markdown_entry_from_lines(lines)
    }

    fn document_text_of(lines: &[crate::markdown::StyledLine]) -> String {
        entry_of(lines).text
    }
}
