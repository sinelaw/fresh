//! `Text` — unified single-line / multi-line text field, with markdown
//! document mode, completion popups, and the block caret.

use std::collections::HashMap;

use fresh_core::api::{OverlayColorSpec, OverlayOptions, WidgetSpec};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};
use serde_json::json;

use super::WidgetImpl;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use crate::widgets::render::{
    blank_list_row, completion_scrollbar_glyph, ensure_trailing_newline, fit_label,
    focus_gutter_prefix, form_label_width, ratatui_style_to_overlay,
    render_completion_bottom_border, render_completion_dim_separator_overlay,
    render_completion_item_overlay, render_text_area, render_text_input, CollectedOutput,
    FocusCursor, OverlayRow, RenderContext, KEY_COMPLETION_SEL_FG, KEY_TEXT_INPUT_SELECTION_BG,
};

pub(crate) struct Text;

impl WidgetImpl for Text {
    fn on_key(
        &self,
        spec: &WidgetSpec,
        widget_key: &str,
        panel: &mut crate::widgets::WidgetPanelState,
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
            | "C-S-Left" | "C-S-Right" => {
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
        delta: i32,
    ) -> bool {
        let WidgetSpec::Text {
            rows,
            completions_visible_rows,
            ..
        } = spec
        else {
            return false;
        };
        // An open completion popup scrolls first — the wheel reached
        // this widget because the pointer sat on the popup's own box
        // (or the field's). Scrolling counts as stepping into the
        // popup: Enter then accepts the highlighted row.
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
        // Otherwise only a multi-line (document) Text scrolls under
        // the wheel; a single-line field scrolls with its caret and
        // emits no region.
        if *rows <= 1 {
            return false;
        }
        let Some((total, visible)) = panel
            .boxes
            .iter()
            .find(|b| b.key.as_deref() == Some(widget_key))
            .and_then(|b| b.scroll)
            .map(|sc| (sc.total, sc.visible))
        else {
            return false;
        };
        let max_scroll = total.saturating_sub(visible) as i64;
        if max_scroll == 0 {
            return false;
        }
        match panel.instance_states.get_mut(widget_key) {
            Some(WidgetInstanceState::Text {
                scroll,
                user_scrolled,
                ..
            }) => {
                let new = (*scroll as i64 + delta as i64).clamp(0, max_scroll) as u32;
                if new == *scroll {
                    return false;
                }
                *scroll = new;
                *user_scrolled = true;
                true
            }
            _ => false,
        }
    }

    fn box_meta(&self, spec: &WidgetSpec) -> super::BoxMeta {
        let mut m = super::BoxMeta::plain("text");
        if let WidgetSpec::Text {
            key: Some(k), rows, ..
        } = spec
        {
            if !k.is_empty() {
                m.key = Some(k.clone());
                m.focusable = true;
                m.scrollable = *rows > 1;
            }
        }
        m
    }
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput {
        let WidgetSpec::Text {
            value,
            cursor_byte,
            focused,
            label,
            placeholder,
            rows,
            field_width,
            max_visible_chars,
            full_width,
            completions: _,
            completions_visible_rows,
            block_caret,
            sel_start,
            sel_end,
            label_width,
            read_only: _,
            markdown,
            key,
        } = spec
        else {
            return CollectedOutput::default();
        };
        render_widget_text(
            value,
            *cursor_byte,
            *focused,
            label,
            placeholder.as_deref(),
            *rows,
            *field_width,
            *max_visible_chars,
            *full_width,
            *completions_visible_rows,
            *block_caret,
            (*sel_start, *sel_end),
            *label_width,
            *markdown,
            key.as_deref(),
            prev,
            next_state,
            ctx,
            panel_width,
        )
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
fn effective_text_field_width(
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

/// Emit a focused Text widget's completion popup as floating overlay
/// rows on `out`, returning the scroll offset to persist for the next
/// render (0 when there are no completions).
///
/// `panel_width` is the inner width the wrapping `LabeledSection` handed
/// us (already minus its 4 columns of `│ … │` chrome); the popup widens
/// by 4 so the side borders it paints line up with the section's.
///
/// Scroll is *forward-only*: when the selection walks past the bottom of
/// the window the view pulls forward to keep it visible, but it is never
/// pulled back if the selection sits above the window — the mouse-wheel
/// handler deliberately diverges scroll from selection, and a back-pull
/// would undo the wheel on the next render.
///
/// Overlay anchors: 1 = the `LabeledSection`'s bottom border (the dim
/// separator paints over it), 2..N+1 = item rows, N+2 = the popup's own
/// bottom border.
#[allow(clippy::too_many_arguments)]
fn emit_completion_overlays(
    out: &mut CollectedOutput,
    key: Option<&str>,
    completions: &[fresh_core::api::CompletionItem],
    visible_rows: u32,
    panel_width: u32,
    selected_idx: usize,
    navigated: bool,
    prev_scroll: u32,
    marker_gutter: bool,
) -> u32 {
    if completions.is_empty() {
        return 0;
    }
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

    let mut anchor: u32 = 1;
    out.overlays.push(OverlayRow {
        buffer_row: anchor,
        entry: render_completion_dim_separator_overlay(popup_total),
    });
    anchor += 1;
    let needs_scrollbar = total > visible;
    let end = (scroll + visible).min(total) as usize;
    for (visible_row, i) in (scroll as usize..end).enumerate() {
        let item = &completions[i];
        let thumb = if needs_scrollbar {
            completion_scrollbar_glyph(visible_row as u32, visible, scroll, total)
        } else {
            None
        };
        out.overlays.push(OverlayRow {
            buffer_row: anchor,
            entry: render_completion_item_overlay(
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
            ),
        });
        anchor += 1;
    }
    out.overlays.push(OverlayRow {
        buffer_row: anchor,
        entry: render_completion_bottom_border(popup_total),
    });
    // The popup is a real box in the panel's layout tree: one
    // stacking level up, opaque (a click inside it that resolves to
    // nothing must not fall through to the rows it covers), spanning
    // the dim separator (anchor 1), the item rows, and the bottom
    // border. Containers shift it exactly as they shift the overlay
    // rows it describes.
    out.boxes.push({
        let mut b =
            crate::widgets::LayoutBox::plain("text_completions", 1, 0, panel_width, visible + 2);
        b.z = 1;
        b.pointer_opaque = true;
        // Keyed with the field's key and scrollable: a wheel over the
        // popup routes here through the ordinary hit-path bubble and
        // lands in `Text::on_wheel`'s completions branch — no
        // panel-wide absorb, no bespoke popup scroller.
        b.key = key.map(|k| k.to_string());
        b.scrollable = true;
        b
    });
    scroll
}

#[allow(clippy::too_many_arguments)]
/// Push a one-cell REVERSED overlay at `byte` in `entry` — the block
/// caret used by modal form surfaces (`block_caret` Text widgets and
/// the Number edit cell), where a hardware cursor isn't visible.
/// Clamps to the entry text; a caret at end-of-text reverses the last
/// cell if there is one (renderers reserve a trailing pad cell).
fn push_block_caret_overlay(entry: &mut TextPropertyEntry, byte: usize) {
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
fn render_markdown_text_area(
    value: &str,
    rows: u32,
    is_focused: bool,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    use crate::view::markdown::{parse_markdown, wrap_styled_lines, wrap_text_line, StyledLine};
    let mut out = CollectedOutput::default();
    let width = panel_width.max(8) as usize;

    // Render + wrap. Without a theme (unit tests, plugin-less hosts) the
    // source renders as plain wrapped lines — identical layout machinery,
    // no styling.
    let lines: Vec<StyledLine> = match ctx.markdown {
        Some(md) => wrap_styled_lines(&parse_markdown(value, md.theme, md.grammars), width),
        None => value
            .split('\n')
            .flat_map(|l| wrap_text_line(l, width))
            .map(|l| {
                let mut sl = StyledLine::new();
                sl.push(l, ratatui::style::Style::default());
                sl
            })
            .collect(),
    };
    let plain: Vec<String> = lines.iter().map(|l| l.plain_text()).collect();
    let shadow = plain.join("\n");
    // Byte offset of each line's start within `shadow`, for mapping the
    // editor's flat selection range back onto rows.
    let mut line_starts: Vec<usize> = Vec::with_capacity(plain.len());
    let mut off = 0usize;
    for l in &plain {
        line_starts.push(off);
        off += l.len() + 1;
    }

    // Shadow editor: keep caret/selection across renders while the
    // rendered text is unchanged; any change (new step, resize) resets.
    let mut editor;
    let mut scroll: u32 = 0;
    let mut user_scrolled = false;
    match key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k)) {
        Some(WidgetInstanceState::Text {
            editor: prev_editor,
            scroll: prev_scroll,
            user_scrolled: prev_user_scrolled,
            ..
        }) if prev_editor.value() == shadow => {
            editor = prev_editor.clone();
            scroll = *prev_scroll;
            user_scrolled = *prev_user_scrolled;
        }
        _ => {
            editor = crate::primitives::text_edit::TextEdit::with_text(&shadow);
            editor.set_cursor_from_flat(0);
        }
    }

    let total = lines.len() as u32;
    let visible = rows.max(1);
    let max_scroll = total.saturating_sub(visible);
    scroll = scroll.min(max_scroll);
    let cursor_row = editor.cursor_row.min(plain.len().saturating_sub(1));
    if is_focused && !user_scrolled {
        // Follow the caret, List-style: scroll just enough to keep it
        // in the window.
        if (cursor_row as u32) < scroll {
            scroll = cursor_row as u32;
        } else if cursor_row as u32 >= scroll + visible {
            scroll = (cursor_row as u32 + 1)
                .saturating_sub(visible)
                .min(max_scroll);
        }
    }

    let selection = if is_focused {
        editor.selection_flat_range()
    } else {
        None
    };
    for vis in 0..visible {
        let idx = (scroll + vis) as usize;
        if idx >= lines.len() {
            out.entries.push(blank_list_row());
            continue;
        }
        let mut text = String::new();
        let mut overlays: Vec<InlineOverlay> = Vec::new();
        for span in &lines[idx].spans {
            let start = text.len();
            text.push_str(&span.text);
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
        // Selection band: the flat range's intersection with this line,
        // painted over the markdown styling (later overlays win per
        // property, so the band's bg composes with the spans' fg).
        if let Some((sel_start, sel_end)) = selection {
            let ls = line_starts[idx];
            let le = ls + text.len();
            let a = sel_start.max(ls);
            let b = sel_end.min(le);
            if b > a {
                overlays.push(InlineOverlay {
                    start: a - ls,
                    end: b - ls,
                    style: OverlayOptions {
                        fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_FG)),
                        bg: Some(OverlayColorSpec::theme_key(KEY_TEXT_INPUT_SELECTION_BG)),
                        ..Default::default()
                    },
                    properties: Default::default(),
                    unit: OffsetUnit::Byte,
                });
            }
        }
        let mut entry = TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        };
        if is_focused && idx == cursor_row {
            let byte_in_row = editor
                .flat_cursor_byte()
                .saturating_sub(line_starts[idx])
                .min(entry.text.len());
            // The block caret is the document's caret. Deliberately NO
            // `focus_cursor`: publishing one moves the panel *buffer's*
            // real cursor there, and the buffer viewport follows its
            // cursor — caret-down near the bottom then scrolled the whole
            // panel (header off the top, `~` rows below the content).
            push_block_caret_overlay(&mut entry, byte_in_row);
        }
        // One `focus` hit per row. `mdLine` names the rendered line so a
        // click (and a drag) can place the caret; the byte range extends
        // past the text so clicks on the row's padding land at line-end.
        if let Some(k) = key.filter(|k| !k.is_empty()) {
            out.hits.push(HitArea {
                overlay: false,
                widget_key: k.to_string(),
                widget_kind: "text",
                buffer_row: vis,
                byte_start: 0,
                byte_end: entry.text.len() + width,
                payload: json!({ "mdLine": idx }),
                event_type: "focus",
                owner_key: None,
            });
        }
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
    }

    if let Some(k) = key.filter(|k| !k.is_empty()) {
        // Scroll payload on the widget's own box: wheel bounds clamp
        // against it, and the host paints a scrollbar when the
        // document overflows.
        out.self_scroll = Some(crate::widgets::layout_box::BoxScroll {
            total: total as usize,
            visible: visible as usize,
            offset: scroll as usize,
        });
        next_state.insert(
            k.to_string(),
            WidgetInstanceState::Text {
                editor,
                scroll,
                completions: Vec::new(),
                completion_selected_index: 0,
                completion_scroll_offset: 0,
                completion_navigated: false,
                user_scrolled,
            },
        );
    }
    out
}

#[allow(clippy::too_many_arguments)]
fn render_widget_text(
    value: &str,
    cursor_byte: i32,
    focused: bool,
    label: &str,
    placeholder: Option<&str>,
    rows: u32,
    field_width: u32,
    max_visible_chars: u32,
    full_width: bool,
    completions_visible_rows: u32,
    block_caret: bool,
    spec_sel: (i32, i32),
    label_width: u32,
    markdown: bool,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    // Markdown mode is a multi-line document view: rendered through the
    // shared hover-docs engine, forcibly read-only, with the caret /
    // selection / Copy operating on the rendered plain text. It owns its
    // whole render path — none of the input-chrome logic below applies.
    if markdown && rows > 1 {
        let is_focused = if key.is_some_and(|k| !k.is_empty()) {
            ctx.is_focused(key)
        } else {
            focused
        };
        return render_markdown_text_area(
            value,
            rows,
            is_focused,
            key,
            prev,
            next_state,
            ctx,
            panel_width,
        );
    }
    let mut out = CollectedOutput::default();
    // Default popup height: 5 visible rows. Plugins override per-widget
    // by setting `completions_visible_rows`; 0 falls back to the default
    // so the orchestrator's existing `text({...})` calls Just Work.
    let effective_visible_rows = if completions_visible_rows == 0 {
        5u32
    } else {
        completions_visible_rows
    };

    // A keyed widget takes focus from the host's resolved focus key; an
    // unkeyed one falls back to the spec's initial-only `focused` hint.
    let is_focused = if key.is_some_and(|k| !k.is_empty()) {
        ctx.is_focused(key)
    } else {
        focused
    };
    // Host-owned value/cursor (+ scroll, multi-line only):
    // read instance state if it exists; else seed from spec
    // on first render. See WidgetInstanceState::Text doc.
    //
    // `rows == 0` shouldn't happen because of serde's
    // default = 1, but if it slips through (raw struct
    // construction in tests, etc.) treat it as single-line.
    let multiline = rows > 1;
    let mut effective_editor: crate::primitives::text_edit::TextEdit;
    let prev_scroll: u32;
    // Completions + selected index ride along on the
    // Text widget's instance state — neither comes from
    // the spec (plugins push via `SetCompletions`), so we
    // carry them across renders verbatim and clamp the
    // index to the current list size below.
    let mut prev_completions: Vec<fresh_core::api::CompletionItem> = Vec::new();
    let mut prev_completion_idx: usize = 0;
    let mut prev_completion_scroll: u32 = 0;
    let mut prev_completion_navigated = false;
    match key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k)) {
        Some(WidgetInstanceState::Text {
            editor,
            scroll,
            completions,
            completion_selected_index,
            completion_scroll_offset,
            completion_navigated,
            ..
        }) => {
            effective_editor = editor.clone();
            prev_scroll = *scroll;
            prev_completions = completions.clone();
            prev_completion_idx = *completion_selected_index;
            prev_completion_scroll = *completion_scroll_offset;
            prev_completion_navigated = *completion_navigated;
        }
        _ => {
            effective_editor = if multiline {
                crate::primitives::text_edit::TextEdit::with_text(value)
            } else {
                crate::primitives::text_edit::TextEdit::single_line_with_text(value)
            };
            let seed = if cursor_byte < 0 {
                value.len()
            } else {
                (cursor_byte as usize).min(value.len())
            };
            effective_editor.set_cursor_from_flat(seed);
            prev_scroll = 0;
        }
    }
    // Clamp once per render so a list that shrank
    // host-side (or arrived empty) doesn't keep a stale
    // out-of-bounds index alive.
    if !prev_completions.is_empty() {
        prev_completion_idx = prev_completion_idx.min(prev_completions.len() - 1);
    } else {
        prev_completion_idx = 0;
    }
    let effective_value = effective_editor.value();
    let effective_cursor_byte = effective_editor.flat_cursor_byte() as i32;
    let effective_cursor = if is_focused {
        effective_cursor_byte
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
    // control's right edge). Only meaningful for single-line fields.
    let composed_label;
    let effective_label: &str = if label_width > 0 && !label.is_empty() && !multiline {
        let lw = form_label_width(
            label_width,
            focus_gutter_prefix(is_focused, ctx.marker_gutter).len(),
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
    let effective_field_width = effective_text_field_width(
        full_width,
        multiline,
        effective_label,
        panel_width,
        field_width,
        ctx.marker_gutter,
    );
    // Selection overlay is only meaningful for the focused
    // widget — passing `None` otherwise keeps the no-selection
    // rendering paths unchanged. The editor's own selection wins;
    // a spec-seeded render (stateless surfaces like Settings, which
    // re-emit their model each frame) falls back to the spec's
    // `sel_start`/`sel_end` byte range, clamped into the value.
    let selection_for_render = if is_focused {
        effective_editor.selection_flat_range().or({
            let (a, b) = spec_sel;
            if a >= 0 && b > a {
                let len = effective_value.len();
                Some(((a as usize).min(len), (b as usize).min(len)))
            } else {
                None
            }
        })
    } else {
        None
    };
    let new_scroll;
    if multiline {
        let rendered = render_text_area(
            &effective_value,
            effective_cursor,
            selection_for_render,
            is_focused,
            label,
            placeholder,
            rows,
            effective_field_width,
            prev_scroll,
            panel_width,
        );
        new_scroll = rendered.scroll_row;
        if let (Some(buffer_row), Some(byte_in_row)) =
            (rendered.cursor_buffer_row, rendered.cursor_byte_in_row)
        {
            out.focus_cursor = Some(FocusCursor {
                buffer_row,
                byte_in_row: byte_in_row as u32,
            });
        }
        for (row_idx, mut e) in rendered.entries.into_iter().enumerate() {
            // Clicking any rendered row of the text area focuses the field
            // (see the single-line branch / #2234 item 1).
            if let Some(k) = key.filter(|k| !k.is_empty()) {
                out.hits.push(HitArea {
                    overlay: false,
                    widget_key: k.to_string(),
                    widget_kind: "text",
                    buffer_row: row_idx as u32,
                    byte_start: 0,
                    byte_end: e.text.len(),
                    payload: json!({}),
                    event_type: "focus",
                    owner_key: None,
                });
            }
            // Modal surfaces paint the caret as a REVERSED cell in the
            // row itself (no hardware cursor over a modal).
            if block_caret {
                if let Some(fc) = out.focus_cursor {
                    if fc.buffer_row as usize == row_idx {
                        push_block_caret_overlay(&mut e, fc.byte_in_row as usize);
                    }
                }
            }
            ensure_trailing_newline(&mut e);
            out.entries.push(e);
        }
    } else {
        let rendered = render_text_input(
            &effective_value,
            effective_cursor,
            selection_for_render,
            is_focused,
            effective_label,
            placeholder,
            max_visible_chars,
            effective_field_width,
            full_width,
        );
        new_scroll = 0;
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
        let gutter = focus_gutter_prefix(is_focused, ctx.marker_gutter);
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
            out.focus_cursor = Some(FocusCursor {
                buffer_row: 0,
                byte_in_row: byte_in_row as u32,
            });
            // Modal surfaces paint the caret as a REVERSED cell in the
            // row itself (no hardware cursor over a modal).
            if block_caret {
                push_block_caret_overlay(&mut entry, byte_in_row);
            }
        }
        // A click anywhere on the input line focuses the field so a mouse user
        // can type. Text widgets previously emitted no hit area, so clicks fell
        // through and the field stayed unfocused (#2234 item 1). Focusing is
        // driven by the tabbable path in `handle_floating_widget_click`; the
        // `focus` event keeps the plugin's focus mirror in step.
        //
        // The payload carries the value-layout breadcrumbs the click
        // handler needs to reposition the cursor to the clicked column
        // (#2573): `valueInnerStart` is where the value's `<inner>`
        // region begins in this row's text (after the gutter that was
        // just prepended), and the truncation fields translate a click
        // over a `…`-prefixed tail view back to a value byte.
        if let Some(k) = key.filter(|k| !k.is_empty()) {
            let inner_start = marker_bytes + rendered.inner_byte_start;
            out.hits.push(HitArea {
                overlay: false,
                widget_key: k.to_string(),
                widget_kind: "text",
                buffer_row: 0,
                byte_start: 0,
                byte_end: entry.text.len(),
                payload: json!({
                    "valueInnerStart": inner_start,
                    "valueDropped": rendered.value_dropped_bytes,
                    "ellipsisBytes": rendered.ellipsis_bytes,
                    "valueLen": rendered.value_len,
                }),
                event_type: "focus",
                owner_key: None,
            });
        }
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
    }
    // Emit the completion popup (if any) as floating overlay rows so
    // the rest of the form below the input keeps its position and the
    // popup paints on top; persists the forward-only auto-scroll offset.
    prev_completion_scroll = emit_completion_overlays(
        &mut out,
        key,
        &prev_completions,
        effective_visible_rows,
        panel_width,
        prev_completion_idx,
        prev_completion_navigated,
        prev_completion_scroll,
        ctx.marker_gutter,
    );
    // Persist instance state for next render. `editor`
    // already carries the canonical cursor (row/col +
    // selection); `scroll` carries the renderer's
    // auto-clamped first-visible-row for multi-line, or `0`
    // for single-line.
    if let Some(k) = key.filter(|k| !k.is_empty()) {
        next_state.insert(
            k.to_string(),
            WidgetInstanceState::Text {
                editor: effective_editor.clone(),
                scroll: new_scroll,
                completions: prev_completions,
                completion_selected_index: prev_completion_idx,
                completion_scroll_offset: prev_completion_scroll,
                completion_navigated: prev_completion_navigated,
                user_scrolled: false,
            },
        );
    }
    out
}

/// Is this Text widget's completion popup showing?
/// `(markdown, read_only)` for a Text spec. A `markdown` multi-line
/// Text is forcibly read-only; a plain Text honours its `read_only`
/// flag; a non-Text spec is `(false, false)`.
pub(crate) fn mode(spec: &WidgetSpec) -> (bool, bool) {
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
fn ensure_seeded(
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
pub(crate) fn apply_edit(
    spec: &WidgetSpec,
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
    fx: &mut super::KeyFx,
    op: impl FnOnce(&mut crate::primitives::text_edit::TextEdit),
) -> bool {
    if !ensure_seeded(spec, widget_key, panel) {
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
pub(crate) fn clear_user_scrolled(
    widget_key: &str,
    panel: &mut crate::widgets::WidgetPanelState,
) -> bool {
    match panel.instance_states.get_mut(widget_key) {
        Some(WidgetInstanceState::Text { user_scrolled, .. }) if *user_scrolled => {
            *user_scrolled = false;
            true
        }
        _ => false,
    }
}

/// The widget's current selection, if its editor holds one.
pub(crate) fn selected_text(
    widget_key: &str,
    panel: &crate::widgets::WidgetPanelState,
) -> Option<String> {
    match panel.instance_states.get(widget_key) {
        Some(WidgetInstanceState::Text { editor, .. }) => editor.selected_text(),
        _ => None,
    }
}

/// Insert printable / IME-committed text at the cursor (replacing
/// any active selection). Read-only and markdown fields accept no
/// insertion. `TextEdit::insert_str` strips embedded newlines when
/// the editor is single-line.
pub(crate) fn insert_str_edit(
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
pub(crate) fn text_key(
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
fn key_name_to_event(name: &str) -> Option<crossterm::event::KeyEvent> {
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

pub(crate) fn completions_open(widget_key: &str, panel: &crate::widgets::WidgetPanelState) -> bool {
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
