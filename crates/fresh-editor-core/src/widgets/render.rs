//! Render a `WidgetSpec` tree into `Vec<TextPropertyEntry>`.
//!
//! This is the path from declarative spec to the bytes the existing
//! virtual-buffer pipeline already knows how to display. By going
//! through `TextPropertyEntry`, widgets paint via exactly the same
//! renderer that today's `setVirtualBufferContent` uses — no parallel
//! render path. This is what makes the new widget API additive: the
//! buffer mid-bytes are indistinguishable from hand-rolled output.
//!
//! v1 dispatches on four kinds:
//!   * `Row` — children laid out left-to-right within a single line
//!     (the result is one `TextPropertyEntry`).
//!   * `Col` — children stacked vertically (the result is one
//!     `TextPropertyEntry` per child output line).
//!   * `HintBar` — keyboard-hint footer (one `TextPropertyEntry`).
//!   * `Raw` — pass-through (zero interpretation; plugin's entries
//!     flow through unchanged).
//!
//! Future kinds (`Toggle`, `Button`, `TextInput`, `List`, `Tree`,
//! `Layer`, `Transient`, `Table`) extend the dispatch without
//! changing the public function signature.

use crate::widgets::registry::WidgetInstanceState;
use fresh_core::api::{
    DualListOption, HintEntry, OverlayColorSpec, OverlayOptions, TreeNode, WidgetSpec,
};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};
use std::collections::{HashMap, HashSet};

// Theme keys used by the v1 widget renderers. Centralized so future
// "role-based" theming (§7 of the design doc) has one place to
// substitute the role→key mapping.
pub const KEY_HELP_KEY_FG: &str = "ui.help_key_fg";
// Foreground of a checked Toggle's `[v]` glyph. `ui.help_key_fg`
// is the "keyboard-key / highlight on a popup body" theme key —
// every shipped theme picks a colour that contrasts with
// `ui.popup_bg`. The previous choice (`ui.tab_active_fg`) was
// designed to contrast with `tab_active_bg`, not the popup body;
// in `high-contrast` both ended up black so the `[v]` glyph
// vanished on every unfocused toggle. `help_key_fg` keeps the
// emphasis intent (a bright accent colour) while reliably
// surviving the popup background.
pub const KEY_TOGGLE_ON_FG: &str = "ui.help_key_fg";
// Selection/focus highlight for widgets inside floating panels
// (list rows, tree nodes, buttons). Originally pointed at
// `ui.menu_active_{fg,bg}` which defaults to rgb(255,255,255) on
// rgb(60,60,60) — a 30-unit gray-on-gray bump that quantizes flat
// on 256-colour terminals and is hard to see on dark themes (the
// surrounding panel bg is rgb(30,30,30)). `ui.popup_selection_{fg,bg}`
// is the theme key designed for "selected item inside a popup
// surface" — white on rgb(58,79,120) blue, ~6× the perceptual
// contrast — and it's the same key the prompt/palette already uses
// so the cue reads consistently across selection UIs.
pub const KEY_FOCUSED_FG: &str = "ui.popup_selection_fg";
pub const KEY_FOCUSED_BG: &str = "ui.popup_selection_bg";
// Backing band painted under whatever the pointer is on — a button, a
// toggle, a list row, a tree node. `ui.menu_hover_bg` is the editor's
// existing "pointer is here" surface (the menu bar and its dropdowns
// already use it), so a widget panel hovering the same way costs no new
// theme key and reads identically to the rest of the app. Deliberately
// weaker than `KEY_FOCUSED_BG`: hover says "you could act here", focus
// and selection say "you are here", and a hovered row must not be
// mistakable for the selected one.
pub const KEY_HOVER_BG: &str = "ui.menu_hover_bg";
// Leading marker prepended to the *focused* control (button /
// toggle / text input) so "which control is focused" is legible
// from a plain terminal capture — not just from the (theme-
// dependent, capture-invisible) `popup_selection` background or
// the hardware cursor. One glyph + a trailing space = two display
// columns. Only ever applied to the single focused widget, so at
// most one `▸` is on screen at a time; combined with the
// `popup_selection` fg/bg flip it makes focus unmistakable, and
// distinct from a `Primary` button's standing bold accent (which
// carries no marker). See `render_toggle` / `single_line`.
const FOCUS_MARKER: &str = "▸ ";
// The unfocused counterpart to `FOCUS_MARKER`: two spaces, the same
// two display columns the marker occupies, so reserving the gutter
// keeps control widths identical whether or not they're focused.
const FOCUS_GUTTER_BLANK: &str = "  ";
// Display columns the focus-marker gutter occupies (`FOCUS_MARKER` /
// `FOCUS_GUTTER_BLANK`) and the columns a framed button spends on its
// own `[ ` / ` ]` chrome. Both are reserved when stretching a
// `full_width` button so the finished control lands on the panel width
// exactly.
const FOCUS_GUTTER_COLS: usize = 2;

/// The two-column gutter prefix a focusable control leads with when
/// the render reserves the focus-marker gutter
/// (`Ctx::marker_gutter` in the description): `▸ ` for the focused control,
/// two spaces for every other control. Returns `""` when the panel
/// didn't opt into the gutter, so non-marker panels render
/// byte-for-byte as before.
pub fn focus_gutter_prefix(focused: bool, marker_gutter: bool) -> &'static str {
    if !marker_gutter {
        ""
    } else if focused {
        FOCUS_MARKER
    } else {
        FOCUS_GUTTER_BLANK
    }
}

/// Paint the shared hover band across the whole of `entry`, leaving its
/// existing colours alone: the overlay carries a background and nothing
/// else, so a checked toggle's accent glyph and a row's own styling
/// survive underneath the pointer.
pub fn apply_hover_band(entry: &mut TextPropertyEntry) {
    let end = entry.text.len();
    if end == 0 {
        return;
    }
    entry.inline_overlays.push(InlineOverlay {
        start: 0,
        end,
        style: OverlayOptions {
            bg: Some(OverlayColorSpec::theme_key(KEY_HOVER_BG)),
            extend_to_line_end: true,
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
}

// `ui.status_error_indicator_fg` defaults to white (designed as
// the text-on-red status badge), so using it as a standalone fg
// renders invisible against the panel bg. The diagnostic.error_fg
// key is the canonical "red text" theme slot.
pub const KEY_DANGER_FG: &str = "diagnostic.error_fg";
/// Background of an input's bracketed region — what makes it look
/// editable.
///
/// Not `ui.prompt_bg`, which is what this was: several shipped themes
/// set that key to a *foreground* colour (bright green on dracula,
/// olive on nord, yellow on solarized), so a focused field on those
/// themes lit up rather than reading as a well. This is the theme's own
/// "a subtle band lies over the editor background", present everywhere
/// and correct in both polarities — it lifts on the dark themes and
/// recesses on the light one.
pub const KEY_INPUT_BG: &str = "editor.current_line_bg";
// Background tint for the selection span inside a widget Text
// input. Distinct from the buffer's `ui.selection_bg` because
// widget inputs sit on top of the `KEY_INPUT_BG` field-bg overlay
// and the contrast needs to read against that tint, not the
// editor surface.
pub const KEY_TEXT_INPUT_SELECTION_BG: &str = "ui.text_input_selection_bg";
// Placeholder text uses the whitespace-indicator key — a dimmer
// grey than `ui.menu_disabled_fg` (themes ship ~RGB(70,70,70)
// vs ~RGB(100,100,100) for disabled menu items), so hint copy
// reads as background guidance rather than a half-active value.
pub const KEY_PLACEHOLDER_FG: &str = "editor.whitespace_indicator_fg";
// Section-legend tint. `ui.help_key_fg` is the same key the
// hint-bar uses to highlight keys against panel bg, so we know
// it's tuned for readability against the same surface a
// LabeledSection sits on.
pub const KEY_SECTION_LABEL_FG: &str = "ui.help_key_fg";
// Dim separator that replaces the input's bottom border when the
// completion popup is open. `ui.menu_disabled_fg` is the closest
// "muted chrome" key already shipped by every theme (gray-ish in
// dark themes, light gray in light themes) so the separator reads
// as a recessed transition between the active input and the
// candidate list rather than as a hard divider.
pub const KEY_COMPLETION_DIM_FG: &str = "ui.menu_disabled_fg";
// Selected completion row foreground/background. Same keys the
// popup-driven selection highlight uses everywhere else (host
// prompt suggestions, action-popup menu), so themes that
// re-skin one re-skin the other.
pub const KEY_COMPLETION_SEL_FG: &str = "ui.popup_selection_fg";
pub const KEY_COMPLETION_SEL_BG: &str = "ui.popup_selection_bg";
// Foreground for *unselected* completion rows. Without this, the
// row text inherits the terminal's default foreground, which has
// no relationship to the popup's themed `popup_bg` and reads
// poorly on coloured backgrounds.
pub const KEY_COMPLETION_FG: &str = "ui.popup_text_fg";
// Border chrome the popup paints around its own rows (the
// `│ ... │` sides extending below the input + the `╰─...─╯`
// closing border). Distinct theme key from the wrapping
// labeled section's default (unstyled) chrome so the popup
// reads as its own surface — matches the user's "use a theme
// key for the popup border" expectation.
pub const KEY_COMPLETION_BORDER_FG: &str = "ui.popup_border_fg";

/// A panel's screen-level floating pop-over: the open `Dropdown`'s
/// option list, or a plugin `Popup` node with `screen_space: true`.
/// `anchor_row` is the 0-based row within the panel's inner area the
/// box drops from (the host adds `inner.y` to get the screen row and
/// draws the box one row below, flipping above when there's no room).
/// `anchor_col` is the 0-based **display column** within that row (the
/// host adds `inner.x`), so the box drops directly under its trigger
/// instead of at the panel's left edge.
#[derive(Debug, Clone)]
pub struct PanelPopup {
    pub widget_key: String,
    pub anchor_row: u32,
    pub anchor_col: u32,
    /// When true, `anchor_row`/`anchor_col` are already absolute
    /// panel-inner coordinates (a plugin `Popup` with an explicit
    /// `anchor`) and the container merges must NOT shift them by the
    /// node's flow position; false means they're relative to the
    /// producing node's own row (the Dropdown trigger) and shift
    /// with it.
    pub anchor_absolute: bool,
    /// The popup's rows, FULLY RENDERED by the widget renderer —
    /// text, padding, and styling (selection highlight included) as
    /// inline overlays over theme keys, exactly like every other
    /// widget row. The host consumer keeps only screen geometry
    /// (anchor flip/clamp), the border, and painting these entries
    /// verbatim: it knows nothing about options, windows, or
    /// selection.
    pub entries: Vec<fresh_core::text_property::TextPropertyEntry>,
    /// Per-entry click payload: `row_indices[i]` is the absolute
    /// option index a click on row `i` selects. Rows without a
    /// payload (a generic `Popup` child) leave this empty and get no
    /// select hits.
    pub row_indices: Vec<usize>,
}

/// Host resources a `markdown: true` Text widget renders through: the
/// live theme (heading / code / link colours) and, when available, the
/// grammar registry for syntax-highlighted fences. Carried by reference
/// beside the spec — theme state is host state, not spec state. `None`
/// grammar falls back to uniform code styling, exactly like hover docs.
#[derive(Clone, Copy)]
pub struct MarkdownCtx<'a> {
    pub theme: &'a crate::theme::Theme,
    pub grammars: Option<&'a crate::primitives::grammar::GrammarRegistry>,
}

impl std::fmt::Debug for MarkdownCtx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MarkdownCtx").finish_non_exhaustive()
    }
}

/// **What a panel's spec resolves to, with nothing rendered.**
///
/// The three things the host reads back from a panel's spec, and every one
/// of them is a walk of the spec against the previous instance-state map —
/// no geometry, no width, no rows. The text projection used to produce these
/// beside its rows; they were the only outputs a described panel ever
/// consumed, and this is that walk on its own.
pub struct ResolvedPanel {
    /// The instance state the next frame reads: the stored entry for every
    /// keyed node the spec still contains, and nothing else.
    pub instance_states: HashMap<String, WidgetInstanceState>,
    /// The focused widget key, clamped onto a key that exists in this spec.
    pub focus_key: String,
    /// Tabbable widget keys in declaration order.
    pub tabbable: Vec<String>,
}

/// Resolve a panel's spec against its previous state — pure, and width-free.
///
/// **The whole of it was already in the renderer, spread across a pre-pass
/// and one line in every stateful kind.** `collect_tabbable` and the focus
/// clamp run *before* collection (widgets style by focus), so they never
/// needed a rendered row; and what each kind's collector does with the state
/// map is `prev.get(k)` into `next_state`, because `update_side_effects`
/// replaces the whole map and a widget the walk did not mention would lose
/// its state. Neither is a rendering decision, and both are here now.
///
/// **The carry is a carry, not a seed.** Every stateful kind resolves its
/// spec against its stored state on each read (`list::resolve`,
/// `tree::resolve`, `dropdown::resolve`, `text::resolve`), so an absent entry
/// is the spec's own value and storing one would only make this walk a second
/// authority. The one kind whose collector did seed — `Text`, which wrote a
/// `TextEdit` built from the spec on first render — has `text::ensure_text_state`
/// for the first *handler* that needs one, which is the only reader that
/// cannot re-derive it.
pub fn resolve_panel(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    auto_focus_first: bool,
    md: Option<MarkdownCtx<'_>>,
) -> ResolvedPanel {
    let mut tabbable = Vec::new();
    collect_tabbable(spec, &mut tabbable);
    let focus_key = if !prev_focus_key.is_empty() && tabbable.iter().any(|k| k == prev_focus_key) {
        prev_focus_key.to_string()
    } else if auto_focus_first {
        tabbable.first().cloned().unwrap_or_default()
    } else {
        String::new()
    };
    let mut instance_states = HashMap::new();
    carry_instance_states(spec, prev, md, &mut instance_states);
    ResolvedPanel {
        instance_states,
        focus_key,
        tabbable,
    }
}

/// Carry `prev[k]` across for every keyed node in `spec`.
///
/// Keyed, not focusable-and-keyed: a `List` that declined the ring
/// (`focusable: false`) still owns a selection, and `box_meta` names the key
/// for both. A node whose key has no stored entry contributes nothing, which
/// is what makes this the collection step and not a seeding one.
fn carry_instance_states(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    md: Option<MarkdownCtx<'_>>,
    out: &mut HashMap<String, WidgetInstanceState>,
) {
    if let Some(k) = super::kinds::behavior(spec).box_meta(spec).key {
        // **A markdown document's state holds the document.** The kind's
        // renderer used to seed a `TextEdit` over the *reflowed* rows, and a
        // described panel no longer runs that renderer — so the document is
        // seeded here, from the same rendered text the wrapped run displays,
        // and re-seeded when that text changes (a new value): the caret is a
        // byte of this string and a byte into a different string means
        // nothing. A width change is not a text change any more; the wrap is
        // layout's, and the bytes are the same at every width.
        if let WidgetSpec::Text {
            markdown: true,
            rows,
            value,
            ..
        } = spec
        {
            if *rows > 1 {
                let doc = super::kinds::text::markdown_document(value, md).text;
                let doc = doc.trim_end_matches('\n');
                let carried = match prev.get(&k) {
                    Some(WidgetInstanceState::Text { editor, .. }) if editor.value() == doc => {
                        prev.get(&k).cloned()
                    }
                    _ => None,
                };
                out.insert(
                    k,
                    carried.unwrap_or_else(|| WidgetInstanceState::Text {
                        editor: crate::primitives::text_edit::TextEdit::with_text(doc),
                        scroll: 0,
                        completions: Vec::new(),
                        completion_selected_index: 0,
                        completion_scroll_offset: 0,
                        completion_navigated: false,
                        user_scrolled: false,
                    }),
                );
                return;
            }
        }
        if let Some(stored) = prev.get(&k) {
            out.insert(k, stored.clone());
        }
    }
    for c in spec.children() {
        carry_instance_states(c, prev, md, out);
    }
}

/// Strip a trailing `'\n'` from `entry.text` if present (overlays /
/// hits aren't affected because the newline is at the very end and
/// no overlay should span it). Used to prepare an inline-rendered
/// child for Row inline-collapse, where individual newlines would
/// split the merged row across multiple buffer lines.
pub fn strip_trailing_newline(entry: &mut TextPropertyEntry) {
    if entry.text.ends_with('\n') {
        entry.text.pop();
    }
}

/// Append a single trailing newline to `entry.text` if it doesn't
/// already end with one. Each top-level entry needs to end with
/// `\n` so it occupies its own line in the underlying virtual
/// buffer (the buffer's line model is byte-driven; without `\n`
/// adjacent entries concatenate into one logical line).
pub fn ensure_trailing_newline(entry: &mut TextPropertyEntry) {
    if !entry.text.ends_with('\n') {
        entry.text.push('\n');
    }
}

/// Walk a spec tree and append tabbable widget keys (`Toggle`,
/// `Button`, `TextInput`, `List`, `Tree` with a non-empty `key`) in
/// declaration order. Layout containers (`Row`, `Col`) recurse;
/// `Raw`, `Spacer`, `HintBar` skip.
fn collect_tabbable(spec: &WidgetSpec, out: &mut Vec<String>) {
    // One copy of the focusability rules: each kind's `box_meta` is the
    // authority. Focus must resolve *before* collection (widgets style by
    // focus), so this is a walk of the spec and not of anything rendered.
    let meta = super::kinds::behavior(spec).box_meta(spec);
    if meta.focusable {
        if let Some(k) = meta.key {
            out.push(k);
        }
    }
    for c in spec.children() {
        collect_tabbable(c, out);
    }
}

/// Pad (or `…`-truncate) a `full_width` button's label so the finished
/// control spans exactly `panel_width` display columns.
///
/// The chrome the renderer is about to add is reserved here rather than
/// trimmed afterwards, so the band never overshoots the row: a framed
/// button spends 4 columns on `[ ` / ` ]`, plus 2 more on the
/// focus-marker gutter when the panel opted into one. A bare button is
/// all label.
///
/// Padding goes through the shared column helper: menu labels carry
/// `…`, `▾` and box glyphs, and byte-counted padding both misaligns the
/// row and risks slicing a multi-byte char.
pub fn fill_button_label(label: &str, bare: bool, marker_gutter: bool, panel_width: u32) -> String {
    let chrome = if bare {
        0
    } else {
        crate::widgets::frame::Frame::BUTTON.chrome()
            + if marker_gutter { FOCUS_GUTTER_COLS } else { 0 }
    };
    let target = (panel_width as usize).saturating_sub(chrome).max(1);
    let mut filled = label.to_string();
    pad_or_truncate_cols(&mut filled, target);
    filled
}

/// Blank full-height-padding row used to pad a List to its
/// advertised height. Padding rows aren't clickable.
pub fn blank_list_row() -> TextPropertyEntry {
    let mut padding = TextPropertyEntry {
        text: String::new(),
        properties: Default::default(),
        style: None,
        inline_overlays: Vec::new(),
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    ensure_trailing_newline(&mut padding);
    padding
}

/// Style one row of a selected *card* so selection reads in any
/// theme — even when colours are too subtle: a *heavy* box border
/// (colour-independent marker), bold, and an accent fg on the
/// pure-border rows. No background band — it reads garish over a
/// multi-row card and fights theme colours. Every box glyph is 3
/// bytes in both light and heavy forms, so swapping them preserves
/// inline-overlay byte offsets.
pub fn mark_list_card_selected(entry: &mut TextPropertyEntry) {
    entry.text = entry
        .text
        .replace('╭', "┏")
        .replace('╮', "┓")
        .replace('╰', "┗")
        .replace('╯', "┛")
        .replace('─', "━")
        .replace('│', "┃");
    let mut style = entry.style.clone().unwrap_or_default();
    style.bold = true;
    // `trim_start`: tree cards indent nested rows by depth, so the
    // border glyph may sit after leading spaces.
    let head = entry.text.trim_start();
    if head.starts_with('┏') || head.starts_with('┗') {
        // Top / bottom rows are pure border, so a whole-row fg tints
        // the corner-to-corner run.
        style.fg = Some(OverlayColorSpec::theme_key("ui.popup_border_fg"));
        entry.style = Some(style);
    } else {
        // Side rows hold the session text between two vertical border
        // glyphs. A whole-row fg would repaint the name / git text
        // (which only carries an fg overlay when the row is *active*),
        // so tint just the leading and trailing `┃` glyphs with
        // sub-range overlays. This frames the selected card on all
        // four sides instead of only top + bottom.
        entry.style = Some(style);
        let bar = '┃';
        let bar_len = bar.len_utf8();
        let first = entry.text.find(bar);
        let last = entry.text.rfind(bar);
        for pos in [first, last].into_iter().flatten().collect::<HashSet<_>>() {
            entry.inline_overlays.push(InlineOverlay {
                start: pos,
                end: pos + bar_len,
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key("ui.popup_border_fg")),
                    bold: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
    }
}

/// Translate a concrete ratatui `Style` (as produced by the markdown
/// renderer) into widget overlay options. Returns `None` for a style
/// with nothing to say, so unstyled spans don't emit no-op overlays.
pub fn ratatui_style_to_overlay(style: ratatui::style::Style) -> Option<OverlayOptions> {
    use ratatui::style::Modifier;
    let mut o = OverlayOptions::default();
    let mut any = false;
    if let Some(fg) = style.fg.and_then(ratatui_color_to_spec) {
        o.fg = Some(fg);
        any = true;
    }
    if let Some(bg) = style.bg.and_then(ratatui_color_to_spec) {
        o.bg = Some(bg);
        any = true;
    }
    let m = style.add_modifier;
    if m.contains(Modifier::BOLD) {
        o.bold = true;
        any = true;
    }
    if m.contains(Modifier::ITALIC) {
        o.italic = true;
        any = true;
    }
    if m.contains(Modifier::UNDERLINED) {
        o.underline = true;
        any = true;
    }
    if m.contains(Modifier::CROSSED_OUT) {
        o.strikethrough = true;
        any = true;
    }
    any.then_some(o)
}

/// Concrete ratatui colour → overlay colour spec. Named ANSI colours ride
/// the `ThemeKey` slot: the paint-time resolver tries
/// `named_color_from_str` before theme lookup, so `"Cyan"` round-trips to
/// `Color::Cyan` without a theme entry.
fn ratatui_color_to_spec(c: ratatui::style::Color) -> Option<OverlayColorSpec> {
    use ratatui::style::Color;
    let named = |s: &str| Some(OverlayColorSpec::ThemeKey(s.to_string()));
    match c {
        Color::Rgb(r, g, b) => Some(OverlayColorSpec::Rgb(r, g, b)),
        Color::Black => named("Black"),
        Color::Red => named("Red"),
        Color::Green => named("Green"),
        Color::Yellow => named("Yellow"),
        Color::Blue => named("Blue"),
        Color::Magenta => named("Magenta"),
        Color::Cyan => named("Cyan"),
        Color::Gray => named("Gray"),
        Color::DarkGray => named("DarkGray"),
        Color::White => named("White"),
        Color::LightRed => named("LightRed"),
        Color::LightGreen => named("LightGreen"),
        Color::LightYellow => named("LightYellow"),
        Color::LightBlue => named("LightBlue"),
        Color::LightMagenta => named("LightMagenta"),
        Color::LightCyan => named("LightCyan"),
        _ => None,
    }
}

// =========================================================================
// LabeledSection helpers.
// =========================================================================

pub const LEFT_BORDER_PREFIX: &str = "│ ";
const RIGHT_BORDER_SUFFIX: &str = " │";

/// Build the top border row for a `LabeledSection`.
///
/// Output (with label "Session name", total_cols = 30):
///
/// ```text
/// ╭─ Session name ─────────────╮
/// ```
///
/// When `label` is empty the legend separators collapse and the
/// border is one unbroken `─` run.
pub fn render_section_top_border(label: &str, total_cols: usize) -> TextPropertyEntry {
    let mut text = String::new();
    let mut overlays: Vec<InlineOverlay> = Vec::new();
    text.push('╭');
    if label.is_empty() {
        for _ in 0..total_cols.saturating_sub(2) {
            text.push('─');
        }
    } else {
        // `╭─ label ─...─╮`. Capture the byte range of `label`
        // (after the leading `─ ` and before the trailing ` `)
        // so the renderer can paint it in a distinct fg, marking
        // it as the section caption rather than border chrome.
        let label_cols = label.chars().count();
        let used = 1 + 1 + 1 + label_cols + 1; // ╭ ─ ` ` label ` `
        text.push('─');
        text.push(' ');
        let label_byte_start = text.len();
        text.push_str(label);
        let label_byte_end = text.len();
        text.push(' ');
        let remaining = total_cols.saturating_sub(used + 1); // -1 for `╮`
        for _ in 0..remaining {
            text.push('─');
        }
        overlays.push(InlineOverlay {
            start: label_byte_start,
            end: label_byte_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_SECTION_LABEL_FG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    text.push('╮');
    text.push('\n');
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

/// Dim-separator overlay row for the completion popup. Unlike
/// `render_completion_dim_separator` (which targets a child of
/// a `LabeledSection` and lets the section wrap the row with
/// `│ ... │`), this one paints into the FULL panel width
/// directly and supplies its own `│ ... │` chrome — overlay
/// rows skip the wrapping section's per-row wrap and land on
/// the parent col's row directly. `total_cols` is the section's
/// outer width.
pub fn render_completion_dim_separator_overlay(total_cols: usize) -> TextPropertyEntry {
    let inner = total_cols.saturating_sub(2).max(1);
    let mut text = String::with_capacity(total_cols * 4 + 2);
    text.push('│');
    for _ in 0..inner {
        text.push('┄');
    }
    text.push('│');
    text.push('\n');
    // Side `│` chars paint in the popup's border theme key
    // (`ui.popup_border_fg`) so the popup chrome reads as
    // distinct from the wrapping labeled section's default
    // border (per the "use a theme key for the popup border"
    // requirement). The dashed run between them paints in the
    // dim foreground so it reads as a recessed transition
    // rather than chrome.
    let left_border_bytes = "│".len();
    let dash_bytes = "┄".len() * inner;
    let right_border_start = left_border_bytes + dash_bytes;
    let right_border_end = right_border_start + "│".len();
    let inline_overlays = vec![
        InlineOverlay {
            start: 0,
            end: left_border_bytes,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_BORDER_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        },
        InlineOverlay {
            start: left_border_bytes,
            end: left_border_bytes + dash_bytes,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_DIM_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        },
        InlineOverlay {
            start: right_border_start,
            end: right_border_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_BORDER_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        },
    ];
    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Completion-popup bottom border overlay row: `│╰─...─╯│`
/// shape — wait no, the bottom-border row is exactly
/// `╰─...─╯` (the side `│ ... │` columns become the corner
/// glyphs at the very bottom of the popup). Paints at the row
/// right after the last visible candidate, closing the
/// unified box.
pub fn render_completion_bottom_border(total_cols: usize) -> TextPropertyEntry {
    let mut text = String::with_capacity(total_cols * 4 + 2);
    text.push('╰');
    for _ in 0..total_cols.saturating_sub(2).max(1) {
        text.push('─');
    }
    text.push('╯');
    text.push('\n');
    // The whole row is chrome; stamp the popup-border theme key
    // at the entry level so every glyph paints in the same
    // colour (no hard-coded RGB or ratatui `Color` value
    // anywhere in the popup rendering — every fg/bg goes
    // through a `ui.*` theme key).
    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: Some(OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_BORDER_FG)),
            ..Default::default()
        }),
        inline_overlays: Vec::new(),
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Overlay variant of `render_completion_item`. Same body (leading
/// space + candidate text + optional scrollbar glyph + trailing pad),
/// but wrapped with the popup's own `│ ... │` chrome since overlay rows
/// paint at the panel width directly without going through a
/// `LabeledSection`'s row wrapper.
pub fn render_completion_item_overlay(
    item: &str,
    kind: Option<&str>,
    selected: bool,
    total_cols: usize,
    scrollbar: Option<char>,
    marker_gutter: bool,
) -> TextPropertyEntry {
    let inner = total_cols.saturating_sub(2).max(1);
    // Reuse the inline-row builder for the body — same layout
    // rules (2 leading chars, item text, pad-to-(inner-1),
    // scrollbar in the last column).
    let body_entry = render_completion_item(item, kind, selected, inner, scrollbar, marker_gutter);
    // Build the wrapped text: `│` + body content + `│`. We
    // strip the body's trailing newline first so the borders
    // sit on the same line.
    let mut text = String::with_capacity(body_entry.text.len() + 8);
    text.push('│');
    let body_no_nl = body_entry.text.trim_end_matches('\n');
    text.push_str(body_no_nl);
    text.push('│');
    text.push('\n');
    // Selection highlight is emitted as an inline overlay that
    // covers ONLY the body byte range (between the two `│`
    // chars) instead of a row-level `extend_to_line_end` style.
    // A row-level selection style would also cover the border
    // cells, and the per-border fg-only overlay below couldn't
    // paint bg back over them — the right `│` would sit on
    // selection blue. With the highlight scoped to the body
    // range, the borders fall outside the selection's reach
    // and paint with the panel's base bg (`theme.suggestion_bg`,
    // filled in by the painter when no overlay supplies a bg).
    //
    // The body inline overlay covers the leading space, the
    // candidate text, the trailing pad, AND the scrollbar
    // column — so the selection reads as a single solid block
    // across the whole inside of the popup rather than
    // truncating at the end of the candidate text. The
    // scrollbar's own fg-only overlay is appended after the
    // selection overlay so it re-tints the scrollbar glyph's
    // fg (per-property overlay merge keeps the selection bg).
    let left_border_bytes = "│".len();
    let body_no_nl_bytes = body_no_nl.len();
    let right_border_start = left_border_bytes + body_no_nl_bytes;
    let right_border_end = right_border_start + "│".len();
    let mut inline_overlays: Vec<InlineOverlay> = Vec::new();
    if selected {
        inline_overlays.push(InlineOverlay {
            start: left_border_bytes,
            end: right_border_start,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_FG)),
                bg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_BG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    // Shift the body's inline overlays right by one byte
    // (the leading `│`) so the scrollbar tint still lands on
    // the right cell. Then add two more inline overlays for
    // the side `│` chars themselves so they paint in the
    // popup-border theme key — same key the dim separator and
    // bottom border use, so the popup chrome reads as a
    // single themed surface.
    inline_overlays.extend(body_entry.inline_overlays.into_iter().map(|mut io| {
        io.start += left_border_bytes;
        io.end += left_border_bytes;
        io
    }));
    inline_overlays.push(InlineOverlay {
        start: 0,
        end: left_border_bytes,
        style: OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_BORDER_FG)),
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
    inline_overlays.push(InlineOverlay {
        start: right_border_start,
        end: right_border_end,
        style: OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_BORDER_FG)),
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// One completion-candidate row. Renders as two leading spaces
/// followed by the candidate text, padded / truncated by the
/// wrapping `LabeledSection` to `total_cols`. The two leading
/// spaces place the candidate's first character at the same
/// column as the input value's first character: the input
/// row's leading chrome is `│ [` (border + section padding +
/// open bracket) — three columns — and the popup row's leading
/// chrome is `│ ` plus the body's two leading spaces, also
/// three columns. So the popup item's first char sits directly
/// under the value's first char, matching the user's "below
/// the input, aligned with what you typed" expectation.
///
/// `selected` rows paint with the standard popup-selection
/// fg/bg theme keys + `extend_to_line_end` so the highlight
/// runs all the way to the right side border instead of
/// stopping at the end of the candidate text.
///
/// `scrollbar` is `Some(glyph)` when the popup is scrollable
/// AND this row owns a scrollbar character (thumb or track).
/// The glyph paints at the right edge of the row, just inside
/// the wrapping section's `│` border, so the scrollbar lives
/// in the popup's chrome rather than crowding the candidate
/// text. `None` rows leave the column blank — either because
/// the popup fits without scrolling or because every row gets
/// `None` when there's nothing to indicate.
fn render_completion_item(
    item: &str,
    kind: Option<&str>,
    selected: bool,
    total_cols: usize,
    scrollbar: Option<char>,
    marker_gutter: bool,
) -> TextPropertyEntry {
    // Build the row up to `total_cols - 1` so the scrollbar (or
    // a trailing space when there isn't one) lands at exactly
    // `total_cols - 1`. The wrapping section pads/truncates the
    // resulting row to `total_cols`, but we want the scrollbar
    // glyph to keep its position regardless of how long the
    // candidate text is, so we hand-pad rather than relying on
    // entry-level `pad_to_chars`.
    //
    // When the panel reserves the focus-marker gutter, the input's
    // bracketed value is itself shifted right by the two-column gutter
    // (`▸ ` / two spaces, inserted before its `[`). Lead the candidate
    // rows by the same two columns so the candidate text stays directly
    // under the typed value instead of sitting two columns to its left.
    // Zero when the panel didn't opt into the gutter (every other
    // popup), so those render exactly as before.
    let lead = if marker_gutter { 2 } else { 0 };
    // Budget = total_cols - (2 leading chars) - (gutter lead) - (1 scrollbar col).
    // The two leading chars align the item with the bracketed
    // input value (see the function docstring).
    let text_budget = total_cols.saturating_sub(2 + lead).saturating_sub(1);
    let item_chars: Vec<char> = item.chars().collect();
    let (visible_item, truncated): (String, bool) = if item_chars.len() <= text_budget {
        (item.to_string(), false)
    } else {
        // Tail-truncate with `…` so the prefix the user typed
        // stays anchored at the left, which is the common case
        // for path / branch completions (the divergent part is
        // at the end).
        let keep = text_budget.saturating_sub(1);
        let head: String = item_chars.iter().take(keep).collect();
        (format!("{}…", head), true)
    };
    let _ = truncated;
    let scrollbar_ch = scrollbar.unwrap_or(' ');
    let is_history = kind == Some("history");
    // For history rows we replace the second leading space (the
    // column that lines up with the bracketed input's `[`) with
    // a small `↶` marker so the row visibly reads as "from
    // history" at a glance. Regular rows keep two leading
    // spaces. The marker is one display column wide so the
    // item text starts in the same column on both kinds.
    let history_marker: char = '↶';
    let mut text = String::with_capacity(total_cols * 4 + 2);
    // Gutter lead (see `lead` above): keeps the candidate aligned under
    // the gutter-shifted input value. The history `↶` marker and the
    // selection highlight are positioned by byte offsets captured *after*
    // these spaces, so they ride along correctly.
    for _ in 0..lead {
        text.push(' ');
    }
    text.push(' ');
    let marker_start_byte = text.len();
    if is_history {
        text.push(history_marker);
    } else {
        text.push(' ');
    }
    let marker_end_byte = text.len();
    let item_start_byte = text.len();
    text.push_str(&visible_item);
    let item_end_byte = text.len();
    // Pad with spaces between the candidate text and the
    // scrollbar column so all rows have the scrollbar glyph in
    // the same column regardless of candidate length.
    let used_cols = 2 + lead + visible_item.chars().count();
    let pad_cols = total_cols.saturating_sub(used_cols).saturating_sub(1);
    for _ in 0..pad_cols {
        text.push(' ');
    }
    text.push(scrollbar_ch);
    text.push('\n');

    let body_style = if selected {
        Some(OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_FG)),
            bg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_SEL_BG)),
            extend_to_line_end: true,
            fg_on_collision_only: false,
            ..Default::default()
        })
    } else {
        // Stamp the popup's text fg on the whole row so the
        // candidate text reads against `popup_bg` rather than
        // inheriting the terminal's default foreground (which
        // has no relationship to the themed popup surface).
        Some(OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_FG)),
            extend_to_line_end: true,
            fg_on_collision_only: false,
            ..Default::default()
        })
    };
    let mut inline_overlays: Vec<InlineOverlay> = Vec::new();
    // History rows: paint the `↶` marker in the popup-border
    // theme key (so it reads as chrome, not item content) and
    // italicize the item text. Same dim fg key the scrollbar
    // uses so all popup chrome stays in one theme slot.
    if is_history {
        inline_overlays.push(InlineOverlay {
            start: marker_start_byte,
            end: marker_end_byte,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_BORDER_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
        inline_overlays.push(InlineOverlay {
            start: item_start_byte,
            end: item_end_byte,
            style: OverlayOptions {
                italic: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    // Scrollbar glyph paints in the dim theme key so it reads as
    // chrome rather than as part of the candidate text. We do
    // this as an inline overlay over the last visible cell so
    // the selection highlight on selected rows doesn't repaint
    // the scrollbar in white-on-blue.
    if scrollbar.is_some() {
        let total_bytes = text.trim_end_matches('\n').len();
        let scrollbar_byte_len = scrollbar_ch.len_utf8();
        let start = total_bytes - scrollbar_byte_len;
        let end = total_bytes;
        inline_overlays.push(InlineOverlay {
            start,
            end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_COMPLETION_DIM_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: body_style,
        inline_overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Compute the scrollbar glyph for the given visible row
/// position. Returns `Some(...)` for rows that overlap the
/// thumb's vertical extent (rendered as a solid `█`); `None`
/// otherwise (rendered as a blank track cell so the candidate
/// row still aligns with the scrollbar column).
///
/// The thumb size is proportional to `visible / total` and
/// snaps to at least one row. The thumb's top row is
/// `floor(scroll / total * visible)` — first row of the
/// visible window when scrolled to the top, last row when
/// scrolled to the bottom.
pub fn completion_scrollbar_glyph(
    visible_row: u32,
    visible: u32,
    scroll: u32,
    total: u32,
) -> Option<char> {
    if total <= visible || visible == 0 {
        return None;
    }
    // Thumb size: at least 1 row, otherwise proportional. Float
    // math is fine — `total` and `visible` are tiny (popup
    // height capped to a handful of rows).
    let thumb_size = ((visible as f32 * visible as f32) / total as f32).round() as u32;
    let thumb_size = thumb_size.max(1).min(visible);
    let max_scroll = total - visible;
    let thumb_top = if max_scroll == 0 {
        0
    } else {
        // `(scroll / max_scroll) * (visible - thumb_size)` —
        // 0 when at the top, `visible - thumb_size` when at the
        // bottom.
        ((scroll as f32 / max_scroll as f32) * (visible - thumb_size) as f32).round() as u32
    };
    if visible_row >= thumb_top && visible_row < thumb_top + thumb_size {
        Some('█')
    } else {
        None
    }
}

/// Wrap a single child row with `│ ... │` and pad / truncate the
/// child text to fit exactly `inner_width` display columns.
/// Inline overlays are byte-shifted by the left-prefix length so
/// they keep aligning with the right characters.
pub fn wrap_in_side_border(child: TextPropertyEntry, inner_width: usize) -> TextPropertyEntry {
    wrap_entry_between(child, inner_width, LEFT_BORDER_PREFIX, RIGHT_BORDER_SUFFIX)
}

/// Pad/truncate `child` to `inner_width` display columns and sandwich it
/// between `prefix` and `suffix` (side-border chrome), shifting the
/// child's overlays past the prefix. `LabeledSection` uses the padded
/// `"│ "` chrome; the tree's bordered cards use flush `"│"` borders to
/// keep two more content columns on a narrow dock.
pub fn wrap_entry_between(
    mut child: TextPropertyEntry,
    inner_width: usize,
    prefix: &str,
    suffix: &str,
) -> TextPropertyEntry {
    let prefix_bytes = prefix.len();
    // Pad / truncate `child.text` to `inner_width` **display** cols —
    // a wide glyph (`漢`, `😀`) is one char but two columns, and
    // char-counted padding shifted the section's right border out of
    // alignment on every row containing one.
    let cur_cols = crate::primitives::display_width::str_width(&child.text);
    if cur_cols < inner_width {
        for _ in 0..(inner_width - cur_cols) {
            child.text.push(' ');
        }
    } else if cur_cols > inner_width {
        // Tail-truncate at the byte where the display width reaches
        // `inner_width`, then if there's room make the final column an
        // `…` so the cut is visible (mirrors `pad_or_truncate_cols`).
        let byte_cutoff = crate::primitives::display_width::byte_offset_at_visual_column(
            &child.text,
            inner_width,
        );
        child.text.truncate(byte_cutoff);
        if inner_width >= 2 {
            while crate::primitives::display_width::str_width(&child.text)
                > inner_width.saturating_sub(1)
            {
                child.text.pop();
            }
            child.text.push('…');
        }
        let w = crate::primitives::display_width::str_width(&child.text);
        for _ in 0..inner_width.saturating_sub(w) {
            child.text.push(' ');
        }
        let byte_cutoff = child.text.len();
        // Drop any overlay that would now reference past the
        // truncation point; clamp the rest.
        child.inline_overlays.retain_mut(|o| {
            if o.start >= byte_cutoff {
                return false;
            }
            if o.end > byte_cutoff {
                o.end = byte_cutoff;
            }
            true
        });
    }

    // The child is now padded to exactly `inner_width` and flanked by
    // border chrome, so a whole-row `extend_to_line_end` style (a list
    // selection band) has nothing left to fill *inside* the section —
    // all it could reach is whatever lies past the section's right
    // edge: the split's spare columns, or a sibling column once a Row
    // zips this line. Scope the style to the row's own cells so the
    // selection can't flood the screen past the panel border. The same
    // goes for a row-filling *inline* overlay (the hover band): the
    // renderers fill a row's tail from either, so both have to be
    // pinned here or the section leaks.
    if let Some(style) = child.style.as_mut() {
        style.extend_to_line_end = false;
    }
    for overlay in child.inline_overlays.iter_mut() {
        overlay.style.extend_to_line_end = false;
    }

    // Compose final text: `<prefix>` + child + `<suffix>\n`.
    let mut text = String::with_capacity(prefix.len() + child.text.len() + suffix.len() + 1);
    text.push_str(prefix);
    text.push_str(&child.text);
    text.push_str(suffix);
    text.push('\n');

    // Shift child overlays by the left-prefix byte count.
    let overlays: Vec<InlineOverlay> = child
        .inline_overlays
        .into_iter()
        .map(|o| InlineOverlay {
            start: o.start + prefix_bytes,
            end: o.end + prefix_bytes,
            style: o.style,
            properties: o.properties,
            unit: o.unit,
        })
        .collect();

    TextPropertyEntry {
        text,
        properties: child.properties,
        style: child.style,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Render a HintBar into a single `TextPropertyEntry`.
///
/// Layout: `<keys> <label>  <keys> <label>  …`. The key portion of
/// each entry is highlighted with the `ui.help_key_fg` theme key;
/// labels use the buffer's default foreground.
///
/// This replaces the per-plugin hand-rolled footer at e.g.
/// `crates/fresh-editor/plugins/search_replace.ts:535–541`,
/// `audit_mode.ts:1068–1158`, `pkg.ts:2136–2145`.
pub fn render_hint_bar(entries: &[HintEntry]) -> TextPropertyEntry {
    let separator = "  ";
    let mut text = String::new();
    let mut overlays = Vec::new();
    for (i, entry) in entries.iter().enumerate() {
        if i > 0 {
            text.push_str(separator);
        }
        let key_start = text.len();
        text.push_str(&entry.keys);
        let key_end = text.len();
        if key_end > key_start {
            overlays.push(InlineOverlay {
                start: key_start,
                end: key_end,
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
                    bold: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
        if !entry.label.is_empty() {
            text.push(' ');
            text.push_str(&entry.label);
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

/// Render a `Toggle` to a single `TextPropertyEntry`.
///
/// Layout: `[v] label` when checked, `[ ] label` when not. The check
/// glyph is colored via `ui.help_key_fg` when checked (a popup-bg-
/// safe highlight key; no override when unchecked). When focused,
/// the entire entry is given a focused fg/bg pair
/// (`ui.popup_selection_fg`/`ui.popup_selection_bg`) plus bold —
/// matching the prompt / palette's selected-row affordance.
pub fn render_toggle(
    checked: bool,
    label: &str,
    focused: bool,
    marker_gutter: bool,
) -> TextPropertyEntry {
    let glyph = if checked { "[v]" } else { "[ ]" };
    // When the panel reserves the focus-marker gutter, every toggle
    // leads with a two-column gutter — `▸ ` when focused, two spaces
    // otherwise — so focus is capture-legible and the width never
    // changes as focus moves. Panels without the gutter render
    // exactly as before (no prefix).
    let marker = focus_gutter_prefix(focused, marker_gutter);
    let mut text = String::with_capacity(marker.len() + glyph.len() + 1 + label.len());
    text.push_str(marker);
    let glyph_start = text.len();
    text.push_str(glyph);
    text.push(' ');
    text.push_str(label);

    let mut overlays = Vec::new();

    // Check-glyph color (only when checked — leaves default fg
    // when unchecked, which is what plugins do today).
    if checked {
        overlays.push(InlineOverlay {
            start: glyph_start,
            end: glyph_start + glyph.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_TOGGLE_ON_FG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    // Focused: full-entry fg/bg + bold.
    if focused {
        overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
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

/// Format a `Number` widget's value for display.
///
/// `integer` truncates to a whole number; `percent` shows
/// `value * 100` with a `%` suffix (a stored `0.25` → `25%`);
/// otherwise a plain decimal with trailing zeros trimmed.
pub fn format_number_value(value: f64, integer: bool, percent: bool) -> String {
    if percent {
        format!("{}%", (value * 100.0).round() as i64)
    } else if integer {
        format!("{}", value.round() as i64)
    } else {
        // Trim trailing zeros / dot from a fixed-precision render so
        // `3.0` shows as `3` and `3.50` as `3.5`.
        let s = format!("{:.4}", value);
        let trimmed = s.trim_end_matches('0').trim_end_matches('.');
        trimmed.to_string()
    }
}

/// Output of [`render_number`]: the rendered entry plus the byte
/// range of the editable value cell (the text between the brackets)
/// so the caller can emit a click hit area over it.
pub struct RenderedNumber {
    pub entry: TextPropertyEntry,
    /// Byte range of the inner value cell within `entry.text`.
    pub value_range: (usize, usize),
}

/// In-place edit state for a `Number` cell: the buffer being typed
/// plus caret / selection byte offsets within it (`-1` = absent).
pub struct NumberEdit<'a> {
    pub text: &'a str,
    pub cursor: i32,
    pub sel_start: i32,
    pub sel_end: i32,
}

/// Minimum visible width of the digit area (right-aligned). The
/// inner cell is one column wider — a trailing reserved cell holds
/// the block caret at end-of-text so typing doesn't shove the digits
/// leftward as the caret advances. Mirrors the Settings number cell.
const NUMBER_CELL_MIN_WIDTH: usize = 3;

/// Render a `Number` field to a single `TextPropertyEntry`.
///
/// Layout: `{marker}{label}: [{cell}]` — a form-style value cell,
/// not a stepper. Display mode right-aligns the formatted value to
/// [`NUMBER_CELL_MIN_WIDTH`]; edit mode shows the edit buffer with
/// a selection highlight and a REVERSED block caret. The value is
/// changed by typing (click the cell / press Enter to edit), not by
/// increment/decrement glyphs.
#[allow(clippy::too_many_arguments)]
pub fn render_number(
    value: f64,
    integer: bool,
    percent: bool,
    label: &str,
    focused: bool,
    label_width: u32,
    edit: Option<NumberEdit<'_>>,
    marker_gutter: bool,
) -> RenderedNumber {
    let marker = focus_gutter_prefix(focused, marker_gutter);
    let mut text = String::new();
    text.push_str(marker);
    if !label.is_empty() {
        text.push_str(&pad_label(label, label_width as usize));
        text.push_str(": ");
    }
    text.push('[');
    let cell_start = text.len();

    let mut overlays: Vec<InlineOverlay> = Vec::new();
    match &edit {
        None => {
            let value_str = format_number_value(value, integer, percent);
            // Right-align to the minimum cell width plus the trailing
            // reserved caret column so display and edit modes line up.
            text.push_str(&format!(
                "{:>width$} ",
                value_str,
                width = NUMBER_CELL_MIN_WIDTH
            ));
        }
        Some(e) => {
            // Edit mode: the buffer plus a single trailing reserved
            // cell (holds the caret at end-of-text). No min-width pad —
            // the cell hugs the typed digits, exactly like the
            // historical editor (`[8 ]`, not `[8   ]`).
            let buf = e.text;
            text.push_str(buf);
            text.push(' ');
            // Selection highlight over the selected byte range.
            if e.sel_start >= 0 && e.sel_end > e.sel_start {
                let s = cell_start + (e.sel_start as usize).min(buf.len());
                let en = cell_start + (e.sel_end as usize).min(buf.len());
                if en > s {
                    overlays.push(InlineOverlay {
                        start: s,
                        end: en,
                        style: OverlayOptions {
                            bg: Some(OverlayColorSpec::theme_key(KEY_TEXT_INPUT_SELECTION_BG)),
                            ..Default::default()
                        },
                        properties: Default::default(),
                        unit: OffsetUnit::Byte,
                    });
                }
            }
            // Block caret: REVERSED cell at the caret byte (or the
            // reserved trailing cell at end-of-text).
            if e.cursor >= 0 {
                let cur = (e.cursor as usize).min(buf.len());
                let caret_start = cell_start + cur;
                let caret_end = if cur < buf.len() {
                    // Cover the char under the caret.
                    let ch_len = buf[cur..].chars().next().map(|c| c.len_utf8()).unwrap_or(1);
                    caret_start + ch_len
                } else {
                    caret_start + 1 // trailing pad cell (ASCII space)
                };
                overlays.push(InlineOverlay {
                    start: caret_start,
                    end: caret_end,
                    style: OverlayOptions {
                        reversed: true,
                        ..Default::default()
                    },
                    properties: Default::default(),
                    unit: OffsetUnit::Byte,
                });
            }
        }
    }
    let cell_end = text.len();
    text.push(']');

    if focused {
        overlays.insert(
            0,
            InlineOverlay {
                start: 0,
                end: text.len(),
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                    bold: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            },
        );
    }

    let entry = TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    RenderedNumber {
        entry,
        value_range: (cell_start, cell_end),
    }
}

/// Pad `label` with trailing spaces to `width` display columns
/// (never truncates — a long label simply overflows its column).
fn pad_label(label: &str, width: usize) -> String {
    let w = crate::primitives::display_width::str_width(label);
    if w >= width {
        label.to_string()
    } else {
        let mut out = label.to_string();
        out.extend(std::iter::repeat_n(' ', width - w));
        out
    }
}

/// The effective label-column width for a form control (`label: [v]`),
/// clamped so the value cell always stays on-screen. `label_width` is
/// the page-wide alignment column; on a narrow surface it can exceed
/// what's left after the marker + `: ` + value cell, which pushes the
/// cell past the right edge where the painter clips it (the toggle chip
/// "disappearing" on a narrow terminal). Reserve room for the cell and
/// never pad wider than that. `0` panel width (auto-fit / tests) keeps
/// the requested `label_width` unchanged.
pub fn form_label_width(
    label_width: u32,
    marker_cols: usize,
    cell_cols: usize,
    panel_width: u32,
) -> usize {
    let requested = label_width as usize;
    if panel_width == 0 {
        return requested;
    }
    let reserved = marker_cols + ": ".len() + cell_cols;
    let budget = (panel_width as usize).saturating_sub(reserved);
    requested.min(budget)
}

/// Fit `label` into `width` columns: truncate with a trailing `…` when
/// it's too long, otherwise right-pad. Keeps a form control's value cell
/// aligned *and* on-screen even when the label itself overflows the
/// clamped column.
pub fn fit_label(label: &str, width: usize) -> String {
    use crate::primitives::display_width::str_width;
    if width == 0 {
        return String::new();
    }
    if str_width(label) <= width {
        return pad_label(label, width);
    }
    // Truncate to width-1 columns, then append '…'.
    let mut out = String::new();
    let mut used = 0usize;
    for ch in label.chars() {
        let cw = str_width(&ch.to_string());
        if used + cw > width.saturating_sub(1) {
            break;
        }
        out.push(ch);
        used += cw;
    }
    out.push('…');
    used += 1;
    out.extend(std::iter::repeat_n(' ', width.saturating_sub(used)));
    out
}

/// Render a form-layout `Toggle`: `{marker}{label}: [v]` with the
/// chip after the (optionally padded) label. Returns the entry plus
/// the byte range of the `[v]` chip for the click hit area.
/// `indeterminate` renders a neutral `[-]` chip — the value is unset
/// and inherits from a lower layer (issue #2345).
pub fn render_toggle_form(
    checked: bool,
    indeterminate: bool,
    label: &str,
    focused: bool,
    label_width: u32,
    panel_width: u32,
    marker_gutter: bool,
) -> (TextPropertyEntry, (usize, usize)) {
    let glyph = if indeterminate {
        "[-]"
    } else if checked {
        "[v]"
    } else {
        "[ ]"
    };
    let marker = focus_gutter_prefix(focused, marker_gutter);
    // `label_width == 0` means no column alignment: render the label in
    // full (compact). Only pad/truncate to a column when a width is
    // requested; then clamp so the chip stays on-screen on a narrow
    // panel.
    let label_cell = if label_width == 0 {
        label.to_string()
    } else {
        let lw = form_label_width(
            label_width,
            crate::primitives::display_width::str_width(marker),
            glyph.len(),
            panel_width,
        );
        if lw == 0 {
            label.to_string()
        } else {
            fit_label(label, lw)
        }
    };
    let mut text = String::new();
    text.push_str(marker);
    text.push_str(&label_cell);
    text.push_str(": ");
    let chip_start = text.len();
    text.push_str(glyph);
    let chip_end = text.len();

    let mut overlays = Vec::new();
    if checked && !indeterminate {
        overlays.push(InlineOverlay {
            start: chip_start,
            end: chip_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_TOGGLE_ON_FG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    if focused {
        overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    let entry = TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    (entry, (chip_start, chip_end))
}

/// Clamp a `Number` value to its optional `[min, max]` bounds.
pub fn clamp_number(value: f64, min: Option<f64>, max: Option<f64>) -> f64 {
    let mut v = value;
    if let Some(lo) = min {
        if v < lo {
            v = lo;
        }
    }
    if let Some(hi) = max {
        if v > hi {
            v = hi;
        }
    }
    v
}

/// Wrap `index + delta` into `[0, len)`. Returns `0` for an empty
/// option set. Used by the `Dropdown` cycler.
pub fn wrap_index(index: i32, delta: i32, len: usize) -> i32 {
    if len == 0 {
        return 0;
    }
    let n = len as i32;
    (((index + delta) % n) + n) % n
}

/// Output of [`render_dropdown`]: the value-button row plus (when
/// open) one row per visible option, with the byte range of the
/// `[value ▼]` button and each option row's index for hit areas.
pub struct RenderedDropdown {
    /// Row 0: `{label}: [value ▼]`.
    pub entry: TextPropertyEntry,
    /// Byte range of the `[value ▼]` button within `entry.text`.
    pub button_range: (usize, usize),
    /// When open: one entry per visible option row (in screen order)
    /// paired with its absolute option index.
    pub option_rows: Vec<(usize, TextPropertyEntry)>,
    /// First visible option index (clamped scroll offset).
    pub scroll_offset: usize,
}

/// How many option rows an open `Dropdown` shows at once. Matches
/// the Settings control's historical window.
pub const DROPDOWN_VISIBLE_OPTIONS: usize = 8;

/// Render a `Dropdown` to a value button plus (when `open`) an
/// inline option list.
///
/// Layout: `{marker}{label}: [{option padded} ▼]`, `▲` while open;
/// open mode appends one row per visible option below, aligned under
/// the button, with the selected option highlighted.
#[allow(clippy::too_many_arguments)]
pub fn render_dropdown(
    options: &[String],
    selected_index: i32,
    label: &str,
    focused: bool,
    label_width: u32,
    open: bool,
    scroll_offset: u32,
    marker_gutter: bool,
) -> RenderedDropdown {
    let selected = if selected_index >= 0 && (selected_index as usize) < options.len() {
        selected_index as usize
    } else {
        0
    };
    let option = options.get(selected).map(|s| s.as_str()).unwrap_or("");
    // Width the value cell to the widest option so the button doesn't
    // resize as the selection changes (capped like the old control).
    let max_option_len = options
        .iter()
        .map(|s| s.chars().count())
        .max()
        .unwrap_or(10);
    let display_width = max_option_len.max(option.chars().count()).min(20);

    let marker = focus_gutter_prefix(focused, marker_gutter);
    let mut text = String::new();
    text.push_str(marker);
    if !label.is_empty() {
        text.push_str(&pad_label(label, label_width as usize));
        text.push_str(": ");
    }
    let button_start = text.len();
    text.push('[');
    text.push_str(&cell(option, display_width));
    text.push(' ');
    text.push_str(if open { "▲" } else { "▼" });
    text.push(']');
    let button_end = text.len();

    let mut overlays = Vec::new();
    if focused {
        overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    } else {
        // Accent the arrow so the row reads as an openable control.
        let arrow_len = "▼".len() + 1; // arrow + closing bracket
        overlays.push(InlineOverlay {
            start: button_end - arrow_len,
            end: button_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    let entry = TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };

    // Open: option rows aligned under the button, windowed to
    // DROPDOWN_VISIBLE_OPTIONS with the scroll offset clamped so the
    // window never runs past the end.
    let mut option_rows = Vec::new();
    let visible = options.len().min(DROPDOWN_VISIBLE_OPTIONS);
    let max_scroll = options.len().saturating_sub(visible);
    let scroll = (scroll_offset as usize).min(max_scroll);
    if open {
        // Align the option column under the button's value cell using DISPLAY
        // width, never byte length: the focus marker `▸ ` is 4 bytes but only
        // 2 columns, so a byte-length indent pushed the popup two cells right
        // of the value it belongs under.
        use crate::primitives::display_width::str_width;
        let indent = str_width(marker)
            + if label.is_empty() {
                0
            } else {
                str_width(&pad_label(label, label_width as usize)) + 2
            };
        for (row_i, opt) in options.iter().skip(scroll).take(visible).enumerate() {
            let idx = scroll + row_i;
            let mut row_text = String::new();
            row_text.push_str(&" ".repeat(indent));
            row_text.push(' ');
            row_text.push_str(&cell(opt, display_width + 2));
            let mut e = TextPropertyEntry::text(&row_text);
            let style = if idx == selected {
                OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                    bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
                    bold: true,
                    ..Default::default()
                }
            } else {
                OverlayOptions {
                    bg: Some(OverlayColorSpec::theme_key(KEY_INPUT_BG)),
                    ..Default::default()
                }
            };
            e.inline_overlays.push(InlineOverlay {
                start: indent,
                end: row_text.len(),
                style,
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
            option_rows.push((idx, e));
        }
    }

    RenderedDropdown {
        entry,
        button_range: (button_start, button_end),
        option_rows,
        scroll_offset: scroll,
    }
}

// ---- DualList pure model helpers (unit-tested) -------------------

/// Values available to move into the Included column: every option
/// not already included and not owned by a sibling (excluded),
/// preserving the options' declaration order.
pub fn dual_available_values(
    options: &[DualListOption],
    included: &[String],
    excluded: &[String],
) -> Vec<String> {
    options
        .iter()
        .map(|o| &o.value)
        .filter(|v| !included.iter().any(|i| i == *v) && !excluded.iter().any(|e| e == *v))
        .cloned()
        .collect()
}

/// The display label for an option value (falls back to the value
/// when the option isn't found).
pub fn dual_label<'a>(options: &'a [DualListOption], value: &'a str) -> &'a str {
    options
        .iter()
        .find(|o| o.value == value)
        .map(|o| o.label.as_str())
        .unwrap_or(value)
}

/// Drop any included value that isn't a known option — keeps the
/// host-owned included set consistent when the options change.
pub fn dual_sanitize_included(options: &[DualListOption], included: &[String]) -> Vec<String> {
    included
        .iter()
        .filter(|v| options.iter().any(|o| &o.value == *v))
        .cloned()
        .collect()
}

/// Truncate-or-pad a string to exactly `width` display columns
/// (char-approximate; adequate for the ASCII labels DualList shows).
pub fn cell(s: &str, width: usize) -> String {
    let chars: Vec<char> = s.chars().collect();
    if chars.len() >= width {
        chars[..width].iter().collect()
    } else {
        let mut out: String = chars.iter().collect();
        out.extend(std::iter::repeat_n(' ', width - chars.len()));
        out
    }
}

/// Column width used for each DualList column given the panel width.
pub fn dual_col_width(panel_width: u32) -> usize {
    // `u32::MAX` means flex is disabled (tests / unbounded) — fall
    // back to a readable fixed width. Otherwise split the panel in
    // two, reserving each column's cursor gutter plus the gap
    // between them, and clamp to a sane range.
    let width = if panel_width == u32::MAX {
        40
    } else {
        panel_width
    };
    let chrome = (2 * DUAL_GUTTER_W + 2) as u32;
    ((width.saturating_sub(chrome)) / 2).clamp(8, 40) as usize
}

/// Display width of the per-column cursor gutter: one marker glyph
/// plus a separating space.
pub const DUAL_GUTTER_W: usize = 2;
/// Cursor marker for the column the keyboard is currently driving.
/// Filled triangle, matching [`FOCUS_MARKER`].
pub const DUAL_CURSOR_ACTIVE: &str = "▸ ";
/// Cursor marker for the *other* column — where the cursor will land
/// if the user switches columns. Hollow so the two are distinguishable
/// in a monochrome capture, not only by color.
pub const DUAL_CURSOR_IDLE: &str = "▹ ";
/// Marker under the active column's header, pointing down into it.
pub const DUAL_COLUMN_ACTIVE: &str = "▾ ";
/// Blank gutter — the same width as the markers, so rows and headers
/// never reflow as the cursor or the active column moves.
pub const DUAL_GUTTER_BLANK: &str = "  ";

/// The two-column gutter a `DualList` cell leads with: `▸ ` when the
/// cursor is on this cell and its column is the active one, `▹ ` when
/// the cursor is parked here in the idle column, two spaces otherwise.
pub fn dual_cursor_marker(on_cursor: bool, column_active: bool) -> &'static str {
    match (on_cursor, column_active) {
        (true, true) => DUAL_CURSOR_ACTIVE,
        (true, false) => DUAL_CURSOR_IDLE,
        _ => DUAL_GUTTER_BLANK,
    }
}

/// Output of `render_tree_row` — the rendered entry plus the byte
/// range covered by the disclosure glyph (when present) so the
/// caller can emit a separate hit area for click-to-expand.
pub struct RenderedTreeRow {
    pub entry: TextPropertyEntry,
    /// Byte range within `entry.text` of the disclosure glyph
    /// (`▶`/`▼`). `None` for leaf nodes (no glyph rendered).
    pub disclosure_range: Option<(usize, usize)>,
    /// Byte range within `entry.text` of the checkbox glyph
    /// (`[v]` / `[ ]`). `None` when the parent Tree is not
    /// `checkable`, or when this node has `checked: None`. The
    /// caller emits a `toggle` hit area over this range.
    pub checkbox_range: Option<(usize, usize)>,
    /// Continuation rows below the primary entry when the parent Tree
    /// has `item_height > 1`. Already indented to align under the
    /// primary row's body and blank-padded so the card is exactly
    /// `item_height` rows tall. Empty for a single-line tree.
    pub extra_entries: Vec<TextPropertyEntry>,
}

/// Render a single `TreeNode` row.
///
/// Layout: `<indent><disclosure><space>[<checkbox><space>]<node-text>`
/// where:
/// * `indent` = `depth * 2` spaces.
/// * `disclosure` = `▶` (collapsed) / `▼` (expanded) for internal
///   nodes; two spaces (alignment) for leaves.
/// * `checkbox` = `[v]` (checked) / `[ ]` (unchecked) when the
///   parent Tree opted into `checkable: true` *and* this node has
///   `checked: Some(_)`; otherwise omitted entirely.
/// * `<node-text>` is the plugin's pre-rendered row content, with
///   its inline overlays byte-shifted by the prefix length.
///
/// The disclosure glyph is colored with `ui.help_key_fg`; the
/// checkbox glyph reuses `ui.tab_active_fg` (the same key the
/// `Toggle` widget uses for its checked-state glyph) so it reads
/// as a control surface against the row's text.
/// A row body fitted to the columns it has, and where the slice came from.
///
/// The byte offsets are into the *original* body, so the caller can carry the
/// plugin's overlays across the cut: an overlay is clamped into
/// `[slice_start, slice_end)`, rebased to the slice, then shifted by
/// `lead_bytes` (the leading `…`, when one was drawn) and by the row prefix.
pub struct WindowedBody {
    /// The text to draw: the slice, with `…` for each end that was cut.
    pub text: String,
    /// Byte offset in the body where the drawn slice starts.
    pub slice_start: usize,
    /// Byte offset in the body where the drawn slice ends.
    pub slice_end: usize,
    /// Bytes of leading marker drawn before the slice.
    pub lead_bytes: usize,
    /// The pans this row can actually use, as a delta from where it rests.
    ///
    /// The clamp the paint applied, handed back so the *stored* pan can be
    /// held to it. Without this the panel keeps a number no row can reach —
    /// `S-End` overshoots by however far the row rests from its own tail —
    /// and the reader spends that difference pressing `S-Left` at a screen
    /// that does not move. `(0, 0)` for a row that fits.
    pub pan_range: (i32, i32),
}

/// Bytes of the char at char index `n`, or the length past the end.
fn byte_of_char(s: &str, n: usize) -> usize {
    s.char_indices().nth(n).map(|(b, _)| b).unwrap_or(s.len())
}

/// Byte offset `budget` display columns after `from`, never splitting a
/// double-width cell across the edge.
fn byte_after_cols(s: &str, from: usize, budget: usize) -> usize {
    use crate::primitives::display_width::char_width;
    let mut used = 0usize;
    for (i, ch) in s[from..].char_indices() {
        let w = char_width(ch);
        if used + w > budget {
            return from + i;
        }
        used += w;
    }
    s.len()
}

/// The column this row would rest at with no panning: far enough right that
/// the anchor is on screen, and no further.
///
/// The anchor is the span the row exists to show — a search result's own
/// match. When it already fits in the head there is nothing to slide, which
/// is the overwhelmingly common case and leaves ordinary rows exactly as they
/// were. Otherwise a third of the budget is kept as leading context, so the
/// anchor reads in situ rather than flush against the elision marker.
fn resting_start_col(body: &str, budget: usize, anchor: Option<(usize, usize)>) -> usize {
    use crate::primitives::display_width::visual_column_at_byte;
    let Some((start_chars, len_chars)) = anchor else {
        return 0;
    };
    let a_col = visual_column_at_byte(body, byte_of_char(body, start_chars));
    let b_col = visual_column_at_byte(body, byte_of_char(body, start_chars + len_chars));
    // One column is owed to the trailing marker whenever anything is cut.
    if b_col <= budget.saturating_sub(1) {
        return 0;
    }
    a_col.saturating_sub((budget / 3).max(1))
}

/// Fit `body` into `budget` display columns, panned `pan` columns from where
/// the row would rest.
///
/// **Display columns, not codepoints.** Every other width hint in this file
/// counts codepoints, which is the same number for the ASCII a panel is
/// usually full of and wrong by a factor of two for CJK — a `漢`-prefixed row
/// was built half again as wide as the panel and cut by the terminal, taking
/// the very text the row existed to show off the right edge (issue #1580).
/// A pan has to move by what the reader sees, so this measures cells.
///
/// `pan` is a delta from the resting column, shared by every row in a tree:
/// each row starts where its own content needs it to and they slide together.
/// It is clamped per row, so panning right stops at the longest row's tail and
/// panning left brings every row home to column zero.
/// Where a body rests, and how far it can travel each way from there.
///
/// The measuring half of [`window_row_body`], split out because the pan a
/// panel *stores* has to be held to the same limit the paint would apply —
/// and a second copy of this arithmetic is exactly the drift that let
/// `Shift+End` leave a number `Shift+Left` could not walk back.
fn row_travel(body: &str, budget: usize) -> (usize, usize) {
    use crate::primitives::display_width::str_width;
    if budget == 0 {
        return (0, 0);
    }
    let content = body.trim_end_matches(' ');
    let total = str_width(content);
    if total <= budget {
        return (0, 0);
    }
    // The right-hand limit owes the leading marker a column; see below.
    let max_start = total.saturating_sub(budget.saturating_sub(1));
    (max_start, 0)
}

/// The pans `body` can use, as a delta from where it rests.
///
/// `(0, 0)` for a row that fits: there is nothing to its left and nothing to
/// its right, and a row that fits never moves at any pan.
pub fn row_pan_range(body: &str, budget: usize, anchor: Option<(usize, usize)>) -> (i32, i32) {
    let (max_start, _) = row_travel(body, budget);
    if max_start == 0 {
        return (0, 0);
    }
    let content = body.trim_end_matches(' ');
    let rest = resting_start_col(content, budget, anchor).min(max_start);
    let cap = |v: usize| v.min(i32::MAX as usize) as i32;
    (-cap(rest), cap(max_start - rest))
}

pub fn window_row_body(
    body: &str,
    budget: usize,
    anchor: Option<(usize, usize)>,
    pan: i32,
) -> WindowedBody {
    use crate::primitives::display_width::{byte_offset_at_visual_column, str_width};
    const MARKER: &str = "…";

    if budget == 0 {
        return WindowedBody {
            text: String::new(),
            slice_start: 0,
            slice_end: 0,
            lead_bytes: 0,
            pan_range: (0, 0),
        };
    }
    // **Trailing padding is not content.** A row arrives padded to the width
    // its author computed, and that number is arrived at by subtracting a
    // prefix the author has to guess at — so a row that fits can measure a
    // column or two over and pick up a `…` that elides nothing but spaces.
    // Overflow and the right-hand limit are both judged on the real text;
    // the padding still renders when it fits, so a selection band still
    // spans the row.
    let content = body.trim_end_matches(' ');
    let total = str_width(content);
    // Fits, and nobody asked to move: the row is its own text, untouched.
    if total <= budget && pan == 0 {
        return WindowedBody {
            text: body.to_string(),
            slice_start: 0,
            slice_end: body.len(),
            lead_bytes: 0,
            pan_range: (0, 0),
        };
    }

    // **The right-hand limit owes the leading marker a column.** Any window
    // that does not start at zero spends one column on `…`, so stopping at
    // `total - budget` leaves the last column of the line permanently
    // unreachable — panning right ran out one character short of the end.
    let max_start = total.saturating_sub(budget.saturating_sub(1));
    let rest = resting_start_col(content, budget, anchor).min(max_start);
    let start_col = (rest as i64 + pan as i64).clamp(0, max_start as i64) as usize;

    let slice_start = byte_offset_at_visual_column(body, start_col);
    let lead = start_col > 0;
    let lead_cols = usize::from(lead);
    let content_budget = budget - lead_cols;

    // Two passes, because whether a trailing marker is owed is only known
    // once the first pass has seen whether anything is left over.
    //
    // `content_budget == 0` is the one-column panel with a lead marker: the
    // marker *is* the whole window, and a trailing one would draw a second
    // column the row does not have. Say "no tail owed" rather than emit it.
    let full_end = byte_after_cols(body, slice_start, content_budget);
    let trail = content_budget > 0 && full_end < content.len();
    let slice_end = if trail {
        byte_after_cols(body, slice_start, content_budget.saturating_sub(1))
    } else {
        full_end
    };

    let mut text = String::with_capacity(body.len().min(budget * 4) + 8);
    if lead {
        text.push_str(MARKER);
    }
    text.push_str(&body[slice_start..slice_end]);
    if trail {
        text.push_str(MARKER);
    }
    WindowedBody {
        text,
        slice_start,
        slice_end,
        lead_bytes: if lead { MARKER.len() } else { 0 },
        pan_range: row_pan_range(body, budget, anchor),
    }
}

/// Columns of frozen gutter before a tree row's body, and the budget the
/// body is then fitted into.
///
/// Written twice inside `render_tree_row` already — once as the measured
/// width of the prefix it builds, once as the indent its continuation lines
/// take — and [`pan_bounds`] needs the same numbers to say how far a row can
/// travel. One definition, because a bound that disagrees with the paint by
/// even a column is a bound the reader feels as a keypress that does nothing.
fn tree_row_gutter_cols(node: &TreeNode, checkable: bool, indent_cols: usize) -> usize {
    // The disclosure column is two wide whether or not a glyph is drawn
    // (glyph + separator space, or two literal spaces), and a checkbox adds
    // `[v]` and a space. The indent is per level of depth.
    let checkbox = usize::from(checkable && node.checked.is_some()) * 4;
    (node.depth as usize) * indent_cols + 2 + checkbox
}

/// A tree row's text as the paint will see it.
///
/// `TextPropertyEntry::text` is empty until `normalize_widths` concatenates
/// the segments into it, and a row built from segments — which is every
/// styled row a plugin sends — therefore measures as nothing until then.
/// Measuring the unnormalised field said a Search & Replace row had no
/// content and so nowhere to pan.
fn row_text(entry: &TextPropertyEntry) -> std::borrow::Cow<'_, str> {
    if entry.segments.is_empty() {
        std::borrow::Cow::Borrowed(&entry.text)
    } else {
        std::borrow::Cow::Owned(entry.segments.iter().map(|s| s.text.as_str()).collect())
    }
}

/// Columns one pan step moves — one key press, or one wheel notch.
///
/// `less(1)`'s left/right step, the closest thing to a convention a terminal
/// has for panning sideways. The wheel used to move three, mirroring the
/// three *lines* a vertical notch moves, but a screen is three times wider
/// than it is tall: three columns of a hundred-and-twenty is a fifth of what
/// three lines of forty is, and the gesture read as a dead one. One number
/// for both, so a reader who pans with the wheel and a reader who pans with
/// the keyboard are moving the same distance.
pub const PAN_COLUMNS: i32 = 8;

/// How far, in display columns, `widget` can usefully be panned each way.
///
/// The clamp that decides what is *drawn* is per row and lives in
/// [`window_row_body`] — that is what keeps rows of different lengths sliding
/// together instead of drifting apart at the ends. But the pan a panel
/// *stores* is what the next keystroke moves from, so it has to be held to
/// the same limit: `Shift+End` asks for "past the end of the longest row",
/// and a number past what any row can reach is a number the reader spends
/// keystrokes walking back while the screen sits still.
///
/// **Two bounds, because a pan is a delta from where each row rests** — its
/// own match, not column zero. A row whose match sits 259 columns along a 403
/// column line can go 259 columns left and only ~80 right, and one number for
/// both is wrong on whichever side is shorter by the difference.
///
/// `cols` is the width the window was given. Zero means nobody has laid this
/// widget out yet, and the answer is `(0, 0)`: a pan before the first frame
/// has no screen to be measured against, and the first paint will answer it.
///
/// `(0, 0)` also for every kind whose paint does not thread the pan, which is
/// every kind but `Tree`: a widget that cannot show a pan should not
/// accumulate one.
///
/// `row` narrows the question to one node. The whole tree's range is what the
/// stored pan is *clamped* to — it must not exclude a row that still has
/// somewhere to go — but "pan to the end" is a question about the row the
/// reader is on, and answering it with the longest row's travel leaves their
/// row clamped at its own tail with keystrokes still to spend before it
/// moves. Rows of unequal length cannot all be at their end at once; the one
/// under the selection is the one that should be.
pub fn pan_bounds(widget: &WidgetSpec, cols: u32, row: Option<usize>) -> (i32, i32) {
    let WidgetSpec::Tree {
        nodes,
        checkable,
        indent_cols,
        item_height,
        card_borders,
        ..
    } = widget
    else {
        return (0, 0);
    };
    // Bordered cards lay out inside their box rather than being windowed —
    // `render_tree_row` returns before the pan is applied — so they offer a
    // pan nothing.
    if cols == 0 || (*card_borders && *item_height > 1) {
        return (0, 0);
    }
    let (mut left, mut right) = (0i32, 0i32);
    for (i, node) in nodes.iter().enumerate() {
        if row.is_some_and(|r| r != i) {
            continue;
        }
        let r = tree_row_pan_range(node, *checkable, *indent_cols as usize, cols);
        left = left.min(r.0);
        right = right.max(r.1);
    }
    (left, right)
}

/// One tree row's travel, measured the way `render_tree_row` will fit it.
fn tree_row_pan_range(
    node: &TreeNode,
    checkable: bool,
    indent_cols: usize,
    cols: u32,
) -> (i32, i32) {
    let gutter = tree_row_gutter_cols(node, checkable, indent_cols);
    let budget = (cols as usize).saturating_sub(gutter);
    let w = node.window_anchor.unwrap_or_default();
    let text = row_text(&node.text);
    let pinned_bytes = byte_of_char(&text, w.pinned as usize);
    let (pinned, rest) = text.split_at(pinned_bytes);
    let anchor = node.window_anchor.map(|a| {
        (
            (a.start as usize).saturating_sub(a.pinned as usize),
            a.len as usize,
        )
    });
    let body_budget = budget.saturating_sub(crate::primitives::display_width::str_width(pinned));
    let mut r = row_pan_range(rest, body_budget, anchor);
    // Continuation lines pan with the row they continue, so they widen it.
    // They carry no anchor of their own and rest at column zero.
    for line in &node.extra_lines {
        let c = row_pan_range(&row_text(line), budget, None);
        r = (r.0.min(c.0), r.1.max(c.1));
    }
    r
}

/// Carry one overlay across a [`WindowedBody`] cut.
///
/// `None` when the overlay fell entirely outside the drawn slice — a
/// highlight on text that is no longer on screen must not collapse onto the
/// row's first cell.
fn rebase_overlay(o: &InlineOverlay, w: &WindowedBody, shift: usize) -> Option<InlineOverlay> {
    let start = o.start.clamp(w.slice_start, w.slice_end);
    let end = o.end.clamp(w.slice_start, w.slice_end);
    if end <= start {
        return None;
    }
    let mut out = o.clone();
    out.start = start - w.slice_start + w.lead_bytes + shift;
    out.end = end - w.slice_start + w.lead_bytes + shift;
    Some(out)
}

#[allow(clippy::too_many_arguments)]
pub fn render_tree_row(
    node: &TreeNode,
    expanded: bool,
    checkable: bool,
    item_height: u32,
    card_borders: bool,
    panel_width: u32,
    indent_cols: u32,
    h_offset: i32,
) -> RenderedTreeRow {
    // Bordered-card trees: card nodes render inside a rounded box; the
    // other nodes (folder headers) collapse to a plain single row
    // instead of being blank-padded to the card height.
    let item_height = if card_borders && item_height > 1 {
        if tree_node_is_card(node, checkable) {
            return render_tree_card(node, item_height, panel_width);
        }
        1
    } else {
        item_height
    };
    let indent_cols = (node.depth as usize) * (indent_cols as usize);
    let disclosure_glyph: &str = if node.has_children {
        if expanded {
            "▼"
        } else {
            "▶"
        }
    } else {
        // Two spaces — same display width as the glyph plus space,
        // keeping leaf rows aligned with their internal siblings.
        "  "
    };
    // `disclosure_glyph` (▶/▼) is 1 column wide; we want the row
    // text to start at the same column whether or not the row is
    // a leaf. With glyph + one separator space, that's 2 cols. The
    // leaf branch uses two literal spaces for the same width.
    let separator: &str = if node.has_children { " " } else { "" };

    let checkbox_glyph: Option<&'static str> = if checkable {
        match node.checked {
            Some(true) => Some("[v]"),
            Some(false) => Some("[ ]"),
            None => None,
        }
    } else {
        None
    };
    let checkbox_extra = checkbox_glyph.map(|g| g.len() + 1).unwrap_or(0);

    let mut text = String::with_capacity(
        indent_cols
            + disclosure_glyph.len()
            + separator.len()
            + checkbox_extra
            + node.text.text.len(),
    );
    for _ in 0..indent_cols {
        text.push(' ');
    }
    let disc_start = text.len();
    text.push_str(disclosure_glyph);
    let disc_end = text.len();
    text.push_str(separator);
    let checkbox_range = if let Some(g) = checkbox_glyph {
        let cb_start = text.len();
        text.push_str(g);
        let cb_end = text.len();
        text.push(' ');
        Some((cb_start, cb_end))
    } else {
        None
    };
    let body_start = text.len();
    // **The host fits the row, not the plugin.** The prefix above is pinned
    // and only the body slides, which is what makes a pan read as a frozen
    // gutter with the content moving under it — and what lets the row keep
    // its checkbox and disclosure glyph wherever the reader has panned to.
    //
    // Before this, a row longer than the panel was simply drawn past the edge
    // and cut by the terminal, so there was nothing to pan and no marker to
    // say anything had been cut. See `window_row_body`.
    let prefix_cols = crate::primitives::display_width::str_width(&text);
    let budget = (panel_width as usize).saturating_sub(prefix_cols);
    // **The row's own pinned head.** A search result's `path:line` is its
    // identity, not its content: a window that slid it away left rows nobody
    // could tell apart. It stays with the prefix above and the rest of the row
    // slides under it.
    let w = node.window_anchor.unwrap_or_default();
    let pinned_bytes = byte_of_char(&node.text.text, w.pinned as usize);
    let (pinned, rest) = node.text.text.split_at(pinned_bytes);
    let pinned_cols = crate::primitives::display_width::str_width(pinned);
    text.push_str(pinned);
    let rest_start = text.len();
    let anchor = node.window_anchor.map(|a| {
        (
            (a.start as usize).saturating_sub(a.pinned as usize),
            a.len as usize,
        )
    });
    let window = window_row_body(rest, budget.saturating_sub(pinned_cols), anchor, h_offset);
    text.push_str(&window.text);

    // Carry over the plugin's inline overlays. The pinned head keeps its
    // offsets (shifted by the prefix only); everything past it is rebased onto
    // the drawn slice. An overlay straddling the boundary contributes to both,
    // and one wholly outside the slice is dropped rather than clamped — a
    // match highlight on text that panned off screen must not reappear on the
    // row's first cell.
    let mut overlays: Vec<InlineOverlay> = node
        .text
        .inline_overlays
        .iter()
        .flat_map(|o| {
            let head = (o.start < pinned_bytes).then(|| {
                let mut h = o.clone();
                h.start += body_start;
                h.end = o.end.min(pinned_bytes) + body_start;
                h
            });
            let tail = (o.end > pinned_bytes).then(|| {
                let mut t = o.clone();
                t.start = o.start.max(pinned_bytes) - pinned_bytes;
                t.end = o.end - pinned_bytes;
                rebase_overlay(&t, &window, rest_start)
            });
            [head, tail.flatten()]
        })
        .flatten()
        .collect();

    // Disclosure glyph color — only on internal nodes, where the
    // glyph is a real character (not just two spaces).
    if node.has_children {
        overlays.push(InlineOverlay {
            start: disc_start,
            end: disc_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }
    // Checkbox glyph color — bright for checked, dim for unchecked,
    // matching the Toggle widget's convention.
    if let Some((cb_start, cb_end)) = checkbox_range {
        let theme_key = match node.checked {
            Some(true) => KEY_TOGGLE_ON_FG,
            _ => KEY_PLACEHOLDER_FG,
        };
        overlays.push(InlineOverlay {
            start: cb_start,
            end: cb_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(theme_key)),
                bold: matches!(node.checked, Some(true)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    let disclosure_range = if node.has_children {
        Some((disc_start, disc_end))
    } else {
        None
    };
    let entry = TextPropertyEntry {
        text,
        // The plugin's own row-level properties (e.g. file-row
        // metadata) carry through unchanged so existing
        // mouse_click handlers still see them.
        properties: node.text.properties.clone(),
        style: node.text.style.clone(),
        inline_overlays: overlays,
        // segments / pad / truncate hints are consumed by the
        // caller before render_tree_row is invoked (see
        // normalize_widths in the Tree match arm). The output
        // entry's text is already final, so these are cleared.
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };

    // Continuation rows for a fixed-height card (item_height > 1).
    // Each `extra_lines` entry is indented to align under the primary
    // row's body (indent + the 2-col disclosure/leaf gutter + the
    // checkbox column, when present), and the card is blank-padded to
    // exactly `item_height` rows.
    let extra_rows = item_height.saturating_sub(1) as usize;
    let mut extra_entries: Vec<TextPropertyEntry> = Vec::with_capacity(extra_rows);
    if extra_rows > 0 {
        // Disclosure/leaf gutter is 2 columns wide in both cases (glyph
        // + separator space, or two literal spaces). The checkbox, when
        // rendered, adds `[v]` (3 cols) + a trailing space.
        let checkbox_cols = if checkbox_glyph.is_some() { 4 } else { 0 };
        let cont_indent_cols = indent_cols + 2 + checkbox_cols;
        let indent_str = " ".repeat(cont_indent_cols);
        let shift = indent_str.len();
        for i in 0..extra_rows {
            match node.extra_lines.get(i) {
                Some(src) => {
                    // Same fit as the primary row, and the same pan, so a
                    // card's continuation lines slide with the line they
                    // continue. No anchor: only the primary row has a span it
                    // exists to show.
                    let cont_budget = (panel_width as usize).saturating_sub(cont_indent_cols);
                    let cont = window_row_body(&src.text, cont_budget, None, h_offset);
                    let mut line_text = String::with_capacity(shift + cont.text.len());
                    line_text.push_str(&indent_str);
                    line_text.push_str(&cont.text);
                    let shifted: Vec<InlineOverlay> = src
                        .inline_overlays
                        .iter()
                        .filter_map(|o| rebase_overlay(o, &cont, shift))
                        .collect();
                    extra_entries.push(TextPropertyEntry {
                        text: line_text,
                        properties: src.properties.clone(),
                        style: src.style.clone(),
                        inline_overlays: shifted,
                        segments: Vec::new(),
                        pad_to_chars: None,
                        truncate_to_chars: None,
                    });
                }
                // Blank padding row (the node has fewer lines than the
                // card is tall). `extend_to_line_end` on the selection
                // style still fills its background when selected.
                None => extra_entries.push(TextPropertyEntry::text(String::new())),
            }
        }
    }

    RenderedTreeRow {
        entry,
        disclosure_range,
        checkbox_range,
        extra_entries,
    }
}

/// Whether a node renders as a bordered card when the parent Tree has
/// `card_borders`: a leaf carrying continuation lines and no checkbox
/// glyph. Folder headers (`has_children`) and checkable rows keep the
/// plain row layout — the border chrome has nowhere sane to put the
/// disclosure/checkbox hit targets.
pub fn tree_node_is_card(node: &TreeNode, checkable: bool) -> bool {
    !node.extra_lines.is_empty() && !node.has_children && (!checkable || node.checked.is_none())
}

/// Render a card node as a rounded box spanning the panel width:
/// a `╭─…─╮` top border (the primary row — its full-width `select`
/// hit makes the border part of the card's click target), the
/// `item_height` content rows wrapped in `│ … │` side borders
/// (blank-padded so every card is the same height), and a `╰─…─╯`
/// bottom border. All rows are indented by the node's depth so the
/// card nests under its folder. Restores the bordered pill the dock's
/// card density lost in the tree redesign (issue #2703).
fn render_tree_card(node: &TreeNode, item_height: u32, panel_width: u32) -> RenderedTreeRow {
    let indent_cols = (node.depth as usize) * 2;
    let total_cols = (panel_width as usize).saturating_sub(indent_cols).max(4);
    // Flush borders — no inner padding column. A dock card is already
    // narrow, and the extra two columns are what keep a remote card's
    // `user@host` badge prefix visible (the state glyph's own trailing
    // space provides the left breathing room).
    let inner_width = total_cols - 2;
    let indent = " ".repeat(indent_cols);

    let border_row = |left: char, right: char| -> TextPropertyEntry {
        let mut text = String::with_capacity(indent.len() + total_cols * 3);
        text.push_str(&indent);
        text.push(left);
        for _ in 0..total_cols.saturating_sub(2) {
            text.push('─');
        }
        text.push(right);
        TextPropertyEntry::text(text)
    };
    let content_row = |src: TextPropertyEntry| -> TextPropertyEntry {
        let mut src = src;
        // A row carrying the `align: "right"` entry property is padded
        // out to the card's *actual* inner width here, where that width
        // is known exactly — plugin-side padding could only estimate the
        // dock's responsive/dragged width and drifted at other widths.
        // The pad is ASCII spaces (1 byte == 1 char each), so shifting
        // overlay offsets by the pad length is unit-correct for both
        // byte- and char-unit overlays.
        let align = src
            .properties
            .get("align")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        // `align: "between"` splits the row into a left group and a
        // right one flush against the border — the card equivalent of
        // the flex spacer a widget `Row` gets. The split point is a byte
        // offset into the row's own text (`splitByte`), so the plugin
        // says *where* the groups meet and the host, which alone knows
        // the card's real width, decides how much space goes between
        // them. Overflowing rows get a single separating space and fall
        // through to the usual end-truncation.
        let split = if align == "between" {
            src.properties
                .get("splitByte")
                .and_then(|v| v.as_u64())
                .map(|v| v as usize)
                .filter(|&b| b <= src.text.len() && src.text.is_char_boundary(b))
        } else {
            None
        };
        // Where the padding goes: the row's start (right-aligned) or the
        // group boundary (space-between).
        let pad_at = match (align.as_str(), split) {
            ("right", _) => Some(0),
            ("between", Some(b)) => Some(b),
            _ => None,
        };
        if let Some(at) = pad_at {
            let width = src.text.chars().count();
            // A "between" row always keeps at least one space between
            // the groups so they can't run together when the card is too
            // narrow to hold both.
            let pad_cols = inner_width.saturating_sub(width).max(usize::from(at > 0));
            if pad_cols > 0 {
                let pad = " ".repeat(pad_cols);
                src.text.insert_str(at, &pad);
                // The pad is ASCII spaces (1 byte == 1 char each), so
                // shifting the overlays that sit after it is unit-correct
                // for both byte- and char-unit overlays.
                for o in src.inline_overlays.iter_mut().filter(|o| o.start >= at) {
                    o.start += pad.len();
                    o.end += pad.len();
                }
            }
        }
        let mut e = wrap_entry_between(src, inner_width, "│", "│");
        strip_trailing_newline(&mut e);
        if !indent.is_empty() {
            e.text.insert_str(0, &indent);
            for o in e.inline_overlays.iter_mut() {
                o.start += indent.len();
                o.end += indent.len();
            }
        }
        e
    };

    let mut extra_entries: Vec<TextPropertyEntry> = Vec::with_capacity(item_height as usize + 1);
    extra_entries.push(content_row(node.text.clone()));
    for i in 0..(item_height as usize).saturating_sub(1) {
        let src = node
            .extra_lines
            .get(i)
            .cloned()
            .unwrap_or_else(|| TextPropertyEntry::text(String::new()));
        extra_entries.push(content_row(src));
    }
    extra_entries.push(border_row('╰', '╯'));

    RenderedTreeRow {
        entry: border_row('╭', '╮'),
        disclosure_range: None,
        checkbox_range: None,
        extra_entries,
    }
}

/// Output of `render_text_input` — the rendered entry plus the
/// byte offset within `entry.text` where the host should place the
/// hardware cursor when this input is focused.
pub struct RenderedTextInput {
    pub entry: TextPropertyEntry,
    /// Byte offset within `entry.text` where the cursor lands.
    /// When the input is unfocused or has no cursor, `None`.
    pub cursor_byte_in_entry: Option<usize>,
    /// Byte offset within `entry.text` where the value's rendered
    /// `<inner>` region begins (just after the label + `[`). Used to
    /// map a mouse click column back to a value byte for
    /// click-to-position-cursor.
    pub inner_byte_start: usize,
    /// Number of value bytes hidden off the left edge by
    /// head-truncation (the `…`-prefixed tail view). `0` when the
    /// whole value is visible.
    pub value_dropped_bytes: usize,
    /// Byte length of the leading `…` glyph within `<inner>` when the
    /// value is head-truncated; `0` otherwise. A click landing on the
    /// ellipsis maps to the first visible value byte.
    pub ellipsis_bytes: usize,
    /// Total byte length of the (untruncated) value. A click past the
    /// last visible character clamps the cursor here (end-of-value).
    pub value_len: usize,
    /// First value **char** the field painted — the horizontal scroll
    /// window's left edge, to hand back on the next render so the view
    /// only moves when the caret asks it to. `0` when the whole value
    /// fits (or the field has no constant width).
    pub scroll_chars: u32,
}

/// Render a `TextInput`.
///
/// Layout: `Label: [<inner>]` (or `[<inner>]` with no label).
/// `<inner>` is exactly `field_width` chars wide when
/// `field_width > 0` — short values pad with trailing spaces; a long
/// value is shown through a horizontal window that follows the
/// caret, with `…` marking whichever end is cut off. `scroll_chars`
/// is the window's left edge from the previous render (the caller
/// persists the returned `scroll_chars`), so the view holds still
/// while the caret moves inside it and slides only when the caret
/// would leave. With `field_width == 0` the input grows with the
/// value (legacy behaviour, also used by tests).
///
/// Placeholder: when unfocused and empty, the placeholder string
/// is shown in `ui.menu_disabled_fg`. Focused inputs always show
/// their (possibly empty) value, never the placeholder.
///
/// Focused-bg: the bracketed region gets `ui.prompt_bg` so the
/// field visually reads as the active editing target.
///
/// **No cursor overlay**: this renderer does not paint the cursor
/// itself — it returns the byte offset where the host should drop
/// the *real* hardware cursor (the terminal's blinking caret). The
/// dispatcher uses that offset to position
/// `SplitViewState::cursors.primary` and flip `show_cursors=true`
/// on the panel buffer. Result: the cursor is always visible
/// regardless of theme contrast, blinks correctly, and matches
/// every other text-input field in the editor.
#[allow(clippy::too_many_arguments)]
pub fn render_text_input(
    value: &str,
    cursor_byte: i32,
    selection: Option<(usize, usize)>,
    focused: bool,
    label: &str,
    placeholder: Option<&str>,
    max_visible_chars: u32,
    field_width: u32,
    full_width: bool,
    scroll_chars: u32,
) -> RenderedTextInput {
    // Placeholder visibility: the value-empty state, regardless of
    // focus. The placeholder remains in the field until the user
    // types something — a focused-empty input still shows the
    // hint. The cursor (when focused) sits on top of the
    // placeholder's first char, which is the natural way the
    // user "overwrites" the hint as they type.
    let show_placeholder = value.is_empty() && placeholder.is_some();

    // Compute the user-cursor's char position within `value`. We
    // operate in bytes here, which is correct for the cursor on
    // ASCII; multibyte chars resolve via is_char_boundary checks.
    let raw_cursor_byte = if cursor_byte < 0 {
        value.len()
    } else {
        (cursor_byte as usize).min(value.len())
    };

    // Breadcrumbs for mapping a mouse click column back to a value
    // byte (click-to-position-cursor). Set by the head-truncation
    // branch; stay 0 when the whole value is visible.
    let mut value_dropped_bytes = 0usize;
    let mut ellipsis_bytes = 0usize;
    // Window left edge to hand back to the caller. Only the
    // constant-width long-value path moves it off 0.
    let mut scroll_out = 0u32;

    // Build `<inner>` plus the byte offset of the cursor *within*
    // `<inner>` (not yet including `[`/label offsets). This is the
    // single place where field-width truncation/padding lives.
    let (inner, cursor_in_inner) = if show_placeholder && field_width == 0 {
        // No constant width: render the placeholder as-is. Cursor
        // (when focused) parks at byte 0 of the placeholder so
        // the first typed char replaces it.
        let inner = placeholder.unwrap_or("").to_string();
        let cursor = if focused { Some(0usize) } else { None };
        (inner, cursor)
    } else if show_placeholder {
        // Constant-width placeholder: pad / truncate the hint to
        // the same total_inner width the value would occupy, so
        // the bracketed field has a stable visual size whether
        // the user has typed yet or not. Same `pad_extra = 1`
        // rule as the value path (under `full_width`) so the
        // closing bracket doesn't shift on focus.
        let target = field_width as usize;
        let pad_extra = if focused || full_width { 1 } else { 0 };
        let total_inner = target + pad_extra;
        let raw = placeholder.unwrap_or("");
        let raw_chars: Vec<char> = raw.chars().collect();
        let inner = if raw_chars.len() <= total_inner {
            let mut s = raw.to_string();
            while s.chars().count() < total_inner {
                s.push(' ');
            }
            s
        } else {
            // Tail-truncate the placeholder with `…` so a long
            // hint doesn't bleed past the field.
            let keep = total_inner.saturating_sub(1);
            let prefix: String = raw_chars.iter().take(keep).collect();
            format!("{}…", prefix)
        };
        let cursor = if focused { Some(0usize) } else { None };
        (inner, cursor)
    } else if field_width > 0 {
        // Constant-width. Visible value occupies `target` chars;
        // when focused (or when the caller asked for `full_width`,
        // which stabilises the visual width across focus
        // transitions) we add one trailing pad space so the cursor
        // never lands on the closing bracket.
        let target = field_width as usize;
        let pad_extra = if focused || full_width { 1 } else { 0 };
        let total_inner = target + pad_extra;
        let value_chars: Vec<char> = value.chars().collect();
        if value_chars.len() <= target {
            // Short or exact-fit value: pad with trailing spaces
            // to total_inner. Cursor at byte k of value lands at
            // byte k of inner.
            // The deficit is known: counting the string again after every
            // space made this quadratic, and unbounded when a caller asked
            // for an unbounded width.
            let mut padded = value.to_string();
            padded.extend(std::iter::repeat_n(
                ' ',
                total_inner.saturating_sub(value_chars.len()),
            ));
            (padded, Some(raw_cursor_byte))
        } else {
            // Long value: a `target`-wide window slides over it,
            // following the caret, with `…` on whichever end is cut
            // off. Before this the field was pinned to the *tail* and
            // a caret in the hidden head was clamped to the first
            // visible char — so Home / Left / Ctrl+Left walked the
            // caret back through text that was never painted, and the
            // start of a long value could not be seen while editing
            // it. When focused, a trailing pad space follows the
            // window (the caret parks there at end-of-value).
            let n = value_chars.len();
            // Byte offset of every char boundary, so a window edge
            // (a char index) slices the value without re-walking it.
            let mut char_bytes: Vec<usize> = Vec::with_capacity(n + 1);
            let mut acc = 0usize;
            for ch in &value_chars {
                char_bytes.push(acc);
                acc += ch.len_utf8();
            }
            char_bytes.push(acc);
            // The caret as a char index; a byte offset that lands
            // mid-char (a malformed cursor) rounds down to its char.
            let cursor_char = char_bytes
                .iter()
                .rposition(|&b| b <= raw_cursor_byte)
                .unwrap_or(0);
            // Columns a window starting at `start` can spend on value
            // chars, and whether it needs a closing `…`: each cut end
            // costs one column, and the right `…` is only worth
            // painting if a value char still fits beside it.
            let window_at = |start: usize| -> (usize, bool) {
                let budget = target.saturating_sub(usize::from(start > 0));
                if budget >= 2 && start + budget < n {
                    (budget - 1, true)
                } else {
                    (budget, false)
                }
            };
            // Never scroll past the point where the window still ends
            // at the value's end — no blank columns at the tail.
            let max_start = n.saturating_sub(target.saturating_sub(1));
            let mut start = (scroll_chars as usize).min(max_start);
            // Follow the caret: pull the window back when the caret
            // walks off the head, push it on when it walks off the
            // tail, and otherwise leave it exactly where the user left
            // it. Moving `start` can add or drop an `…`, which changes
            // the column budget, so re-check — it settles in two
            // passes.
            for _ in 0..3 {
                let (cap, _) = window_at(start);
                let end = (start + cap).min(n);
                if cursor_char < start {
                    start = cursor_char;
                } else if cursor_char > end || (cursor_char == end && end < n) {
                    // Park the caret on the window's last column.
                    start = (cursor_char + 1).saturating_sub(cap).min(max_start);
                } else {
                    break;
                }
            }
            let (cap, right_ellipsis) = window_at(start);
            let end = (start + cap).min(n);
            let left_ellipsis = start > 0;
            let (start_byte, end_byte) = (char_bytes[start], char_bytes[end]);
            let mut s = String::with_capacity(2 * "…".len() + (end_byte - start_byte) + pad_extra);
            if left_ellipsis {
                s.push('…');
            }
            s.push_str(&value[start_byte..end_byte]);
            if right_ellipsis {
                s.push('…');
            }
            for _ in 0..pad_extra {
                s.push(' ');
            }
            // Cursor: inside the window it translates straight
            // through; outside it (a stale offset the follow loop
            // could not reach, e.g. a 1-column field) it clamps to
            // the nearest visible edge.
            let lead = if left_ellipsis { "…".len() } else { 0 };
            let cursor_in_inner = if raw_cursor_byte <= start_byte {
                lead
            } else if raw_cursor_byte >= end_byte {
                lead + (end_byte - start_byte)
            } else {
                lead + (raw_cursor_byte - start_byte)
            };
            value_dropped_bytes = start_byte;
            ellipsis_bytes = lead;
            scroll_out = start as u32;
            (s, Some(cursor_in_inner))
        }
    } else if max_visible_chars > 0 && value.chars().count() > max_visible_chars as usize {
        // Legacy max_visible_chars path: tail-truncate with `…`
        // (drops the *tail*, not the head — matches the original
        // cursor-invisible v1 behaviour for callers still using it).
        let chars: Vec<char> = value.chars().collect();
        let take = (max_visible_chars as usize).saturating_sub(1);
        let start = chars.len().saturating_sub(take);
        let tail: String = chars[start..].iter().collect();
        let s = format!("…{}", tail);
        (s, Some(raw_cursor_byte.min(value.len())))
    } else {
        // No fixed width and no truncation: render the value as-is.
        // When focused we still need somewhere for the cursor to
        // land at end-of-value — append a trailing space so the
        // cursor sits on it instead of overlapping the closing
        // bracket.
        let mut s = value.to_string();
        if focused {
            s.push(' ');
        }
        (s, Some(raw_cursor_byte))
    };

    // Compose the final text: optional label, `[`, inner, `]`.
    let mut text = String::new();
    if !label.is_empty() {
        text.push_str(label);
        text.push(' ');
    }
    let bracket_open_byte = text.len();
    text.push('[');
    let inner_byte_start = text.len();
    text.push_str(&inner);
    let inner_byte_end = text.len();
    text.push(']');
    let bracket_close_byte = text.len();

    let mut overlays = Vec::new();

    if show_placeholder {
        overlays.push(InlineOverlay {
            start: inner_byte_start,
            end: inner_byte_end,
            style: OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_PLACEHOLDER_FG)),
                italic: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    // A field looks like a field whether or not it is focused.
    //
    // This overlay used to be gated on `focused`, which meant an input
    // nobody had clicked yet was drawn on the panel's own background —
    // and brackets alone are a weak signal, plenty of read-only labels
    // carry them. So the welcome screen's finder, Settings (Terminal ->
    // Command) and the New Agent dialog all opened showing fields that
    // gave no sign they could be typed into. What marks focus instead is
    // the bracket band below — the caret cannot, on a panel mounted into
    // a buffer.
    overlays.push(InlineOverlay {
        start: bracket_open_byte,
        end: bracket_close_byte,
        style: OverlayOptions {
            bg: Some(OverlayColorSpec::theme_key(KEY_INPUT_BG)),
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });

    // ...and a focused field says so on its frame.
    //
    // The field background above is deliberately ungated, so it can no
    // longer be what marks focus. That left the caret as the only mark,
    // which is enough in a floating panel — and nothing at all on a
    // panel mounted into a buffer, where the hardware caret belongs to
    // the *document* and stays in its margin. The welcome screen's
    // finder was the case in point: its focused and unfocused rows came
    // back byte-identical, so the only way to discover you were in the
    // field was to type into it.
    //
    // The brackets are the field's own frame and nothing else on the
    // row is load-bearing, so marking exactly those two cells says
    // "this field has the keys" without touching the well or the value.
    //
    // It takes the `KEY_FOCUSED_FG`/`BG` pair the rest of the widget
    // system already uses for the focused thing, not a foreground alone:
    // the brackets are drawn in an accent at rest, so a fg change is a
    // shade against a shade. The band is a different *kind* of mark, and
    // reads at a glance in either polarity. Pushed after the field
    // background so it layers over it.
    if focused {
        for (start, end) in [
            (bracket_open_byte, bracket_open_byte + 1),
            (bracket_close_byte - 1, bracket_close_byte),
        ] {
            overlays.push(InlineOverlay {
                start,
                end,
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
                    bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
                    bold: true,
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
    }

    // Selection overlay: paint `ui.text_input_selection_bg` over the
    // selected range. Only emitted when focused (matches the cursor
    // visibility rule) and when no per-row truncation is in play —
    // the head-truncated `…` path remaps cursor bytes via
    // `cursor_in_inner`, but a similar remap for an arbitrary
    // range is intricate enough that the v1 widget framework just
    // skips the highlight when the inner is `…`-prefixed. Cursor
    // still renders correctly there.
    let inner_is_truncated = inner.starts_with('…');
    if focused && !inner_is_truncated {
        if let Some((sel_start, sel_end)) = selection {
            // Clamp to the visible value bytes. `inner` may have
            // trailing padding (spaces) when `field_width > 0` —
            // selection never extends into the pad area.
            let visible_value_len = value.len();
            let s = sel_start.min(sel_end).min(visible_value_len);
            let e = sel_start.max(sel_end).min(visible_value_len);
            if e > s {
                overlays.push(InlineOverlay {
                    start: inner_byte_start + s,
                    end: inner_byte_start + e,
                    style: OverlayOptions {
                        bg: Some(OverlayColorSpec::theme_key(KEY_TEXT_INPUT_SELECTION_BG)),
                        ..Default::default()
                    },
                    properties: Default::default(),
                    unit: OffsetUnit::Byte,
                });
            }
        }
    }

    let cursor_byte_in_entry = if focused {
        cursor_in_inner.map(|c| inner_byte_start + c)
    } else {
        None
    };

    RenderedTextInput {
        entry: TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        },
        cursor_byte_in_entry,
        inner_byte_start,
        value_dropped_bytes,
        ellipsis_bytes,
        value_len: value.len(),
        scroll_chars: scroll_out,
    }
}

/// Output of `render_text_area`. One entry per visible row of the
/// editing region, plus optionally one preceding label row.
pub struct RenderedTextArea {
    /// The label row (if any) followed by `visible_rows` rows of
    /// editing content. Empty `value` lines are rendered as blank
    /// padded rows so the widget always occupies its full visual
    /// height.
    pub entries: Vec<TextPropertyEntry>,
    /// Auto-clamped scroll row (first visible line of `value`)
    /// after this render. Persisted into instance state by the
    /// caller.
    pub scroll_row: u32,
    /// Buffer row (within `entries`) where the host should drop
    /// the hardware cursor when focused. `None` when unfocused or
    /// when `value` is empty and the placeholder is showing.
    pub cursor_buffer_row: Option<u32>,
    /// Byte offset within the cursor's row text where the cursor
    /// lands. Pairs with `cursor_buffer_row`.
    pub cursor_byte_in_row: Option<usize>,
}

/// What every row of a text area shares, resolved once from the whole value.
///
/// **A text area does not wrap.** [`render_text_area`] splits `value` on
/// `\n` and pads or tail-truncates each line to the field width, so the row
/// drawn for line `i` is a function of that one line plus the four facts
/// below — none of which depend on which rows are being drawn, or on how
/// many. That is what lets a caller which owns its own window format only
/// the rows it shows, instead of asking for the whole document and windowing
/// the answer it gets back.
///
/// The *markdown* variant is the one that wraps, and it is a different
/// function (`kinds::text::render_markdown_text_area`) for that reason: there
/// a row is a slice of a reflowed document rather than a line of this one.
pub struct TextAreaGeom {
    /// Byte range of each logical line within the value.
    lines: Vec<(usize, usize)>,
    /// Columns every row is padded or truncated to.
    width: usize,
    focused: bool,
    /// The placeholder text, when there is one to draw. It replaces line
    /// zero, which is the only line an empty value has.
    placeholder: Option<String>,
    /// The selection, decomposed onto `((line, byte_in_line), (line,
    /// byte_in_line))`, so a row can band itself without re-scanning the
    /// value.
    selection: Option<((usize, usize), (usize, usize))>,
    /// The line the caret is on, and its byte within that line — computed
    /// whether or not there is a caret to *draw*, because the scroll clamp
    /// needs the line either way.
    cursor_at: (usize, usize),
    /// Whether that caret is drawn.
    caret: bool,
}

impl TextAreaGeom {
    /// How many rows the document has: one per line, always. An empty value
    /// is one empty line, which is what an empty editor shows.
    pub fn rows(&self) -> usize {
        self.lines.len()
    }

    /// The line the caret sits on, whether or not it is drawn. The scroll
    /// clamp is expressed in it.
    pub fn cursor_line(&self) -> usize {
        self.cursor_at.0
    }
}

/// Resolve [`TextAreaGeom`] for one render of `value`.
pub fn text_area_geom(
    value: &str,
    cursor_byte: i32,
    selection: Option<(usize, usize)>,
    focused: bool,
    placeholder: Option<&str>,
    field_width: u32,
    panel_width: u32,
) -> TextAreaGeom {
    // Resolve effective field width: caller's value if set, else
    // `panel_width` (or a small default if the panel is unsized).
    let width: usize = if field_width > 0 {
        field_width as usize
    } else if panel_width != u32::MAX && panel_width > 0 {
        panel_width as usize
    } else {
        40
    };

    // Split value into lines (without the `\n`), as byte ranges rather than
    // slices so the geometry outlives the borrow — a windowing caller keeps
    // it across the whole build and slices `value` per row. `split` always
    // yields at least one piece, so an empty value is one empty line.
    let mut lines: Vec<(usize, usize)> = Vec::new();
    let mut at = 0usize;
    for line in value.split('\n') {
        lines.push((at, at + line.len()));
        at += line.len() + 1;
    }

    // Cursor → (line_index, byte_in_line). When `cursor_byte` is
    // negative (no cursor), we still compute a line for scroll
    // bookkeeping but don't draw one.
    let raw_cursor_byte = if cursor_byte < 0 {
        value.len()
    } else {
        (cursor_byte as usize).min(value.len())
    };
    let cursor_at = byte_to_line_col(value, raw_cursor_byte);

    // Selection decomposed onto (line_start, byte_in_line) →
    // (line_end, byte_in_line) so each visible row can emit its own
    // background overlay. Only meaningful when focused; we trust the
    // caller to pass `None` for unfocused renders.
    let selection = selection.and_then(|(a, b)| {
        let lo = a.min(b);
        let hi = a.max(b);
        if hi <= lo || hi > value.len() {
            return None;
        }
        Some((byte_to_line_col(value, lo), byte_to_line_col(value, hi)))
    });

    let show_placeholder = !focused && value.is_empty();
    TextAreaGeom {
        lines,
        width,
        focused,
        placeholder: placeholder
            .filter(|p| show_placeholder && !p.is_empty())
            .map(str::to_string),
        selection,
        cursor_at,
        caret: focused && cursor_byte >= 0,
    }
}

/// The label row a text area puts above its editing region.
pub fn text_area_label(label: &str) -> TextPropertyEntry {
    let mut text = String::with_capacity(label.len() + 2);
    text.push_str(label);
    text.push(':');
    TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: Vec::new(),
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

// `cfg(debug_assertions)` rather than `cfg(test)`: the test that reads this
// counter lives in `fresh-editor`, so `cfg(test)` would not compile it in.
// Gating on the dev profile keeps release builds free of the increment without
// forking `fresh-editor-core`'s features between `cargo build` and `cargo test`.
#[cfg(debug_assertions)]
thread_local! {
    /// How many rows have been formatted, so a test can pin a windowing caller
    /// to formatting the rows it draws and no others. That difference is
    /// invisible in the cells, which is how "ask for the whole document and
    /// window the answer" survived as long as it did.
    pub static ROWS_FORMATTED: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// One row of a text area's editing region: the row for logical line `line`,
/// and the caret's byte within it when the caret is on this line.
///
/// A `line` past the end of the value is a blank row padded to the field
/// width, which is what keeps the focused block rectangular.
pub fn text_area_row(
    value: &str,
    g: &TextAreaGeom,
    line: usize,
) -> (TextPropertyEntry, Option<usize>) {
    #[cfg(debug_assertions)]
    ROWS_FORMATTED.with(|c| c.set(c.get() + 1));
    let mut overlays: Vec<InlineOverlay> = Vec::new();
    let mut row_text = match g.lines.get(line) {
        Some(&(a, b)) => pad_or_truncate_line(&value[a..b], g.width),
        None => " ".repeat(g.width),
    };

    // Placeholder shows on the first row only — and an empty value, which is
    // the only thing that shows one, has exactly one line.
    if line == 0 {
        if let Some(ph) = g.placeholder.as_deref() {
            row_text = pad_or_truncate_line(ph, g.width);
            overlays.push(InlineOverlay {
                start: 0,
                end: row_text.len(),
                style: OverlayOptions {
                    fg: Some(OverlayColorSpec::theme_key(KEY_PLACEHOLDER_FG)),
                    ..Default::default()
                },
                properties: Default::default(),
                unit: OffsetUnit::Byte,
            });
        }
    }

    // Focused-bg covers the full row width — the editing
    // region reads as a single block.
    if g.focused {
        overlays.push(InlineOverlay {
            start: 0,
            end: row_text.len(),
            style: OverlayOptions {
                bg: Some(OverlayColorSpec::theme_key(KEY_INPUT_BG)),
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    // Selection overlay for this row, clamped to the row's text
    // length. Rows are padded out to the field width; selection
    // never paints into the trailing pad area.
    if g.focused {
        if let Some(((sl, sc), (el, ec))) = g.selection {
            if line >= sl && line <= el {
                let line_text_len = g.lines.get(line).map_or(0, |&(a, b)| b - a);
                let row_start = if line == sl { sc } else { 0 };
                let row_end = if line == el { ec } else { line_text_len };
                let s = row_start.min(line_text_len);
                let e = row_end.min(line_text_len);
                if e > s {
                    overlays.push(InlineOverlay {
                        start: s,
                        end: e,
                        style: OverlayOptions {
                            bg: Some(OverlayColorSpec::theme_key(KEY_TEXT_INPUT_SELECTION_BG)),
                            ..Default::default()
                        },
                        properties: Default::default(),
                        unit: OffsetUnit::Byte,
                    });
                }
            }
        }
    }

    // Drop the cursor on this row if it matches. The cursor's byte column on
    // its line: if the line was truncated, the cursor may have shifted past
    // the visible region — clamp to the last visible byte so the hardware
    // cursor stays in the row.
    let caret = (g.caret && line == g.cursor_at.0).then(|| g.cursor_at.1.min(row_text.len()));

    let entry = TextPropertyEntry {
        text: row_text,
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    (entry, caret)
}

/// Render a multi-line `TextArea`.
///
/// Layout:
/// * If `label` is non-empty, one `Label:` row precedes the editing
///   region.
/// * Then exactly `visible_rows` rows of editing content. Lines of
///   `value` between `[scroll_row, scroll_row + visible_rows)` are
///   rendered; rows beyond the value are blanks (padded so the
///   editing region's input-bg block keeps its rectangular shape).
/// * The editing region uses `field_width` columns when set; `0`
///   means "use up to `panel_width`". Long lines are truncated with
///   `…` at the right when they exceed the field width — this is
///   different from `TextInput`'s head-truncation, because the
///   cursor is no longer pinned to end-of-value (it can be
///   anywhere within multi-line content).
/// * When focused, every visible content row gets the
///   `ui.prompt_bg` overlay extended to the field width so the
///   editing region reads as a single block.
/// * Placeholder: shown on the *first* row only when unfocused and
///   `value` is empty.
///
/// Cursor: returns the visible row index (relative to `entries`)
/// and byte offset within that row's text. The auto-clamp policy:
/// keep the cursor's line in view by adjusting `scroll_row` when
/// the cursor's line falls outside `[scroll_row, scroll_row +
/// visible_rows)`.
///
/// **This is the windowing half only.** Everything about what a row *says*
/// is [`text_area_geom`] and [`text_area_row`], which a caller that owns its
/// own window calls directly — see `view::shell::widgets`' multi-line `Text`
/// arm. Both callers therefore share one answer per row rather than two that
/// can drift.
#[allow(clippy::too_many_arguments)]
pub fn render_text_area(
    value: &str,
    cursor_byte: i32,
    selection: Option<(usize, usize)>,
    focused: bool,
    label: &str,
    placeholder: Option<&str>,
    visible_rows: u32,
    field_width: u32,
    prev_scroll: u32,
    panel_width: u32,
) -> RenderedTextArea {
    let g = text_area_geom(
        value,
        cursor_byte,
        selection,
        focused,
        placeholder,
        field_width,
        panel_width,
    );

    // Auto-clamp scroll: keep cursor's line in [scroll_row,
    // scroll_row + visible_rows). On first render, prev_scroll == 0.
    let visible_rows_usize = visible_rows.max(1) as usize;
    let cursor_line = g.cursor_line();
    let mut scroll_row = prev_scroll as usize;
    if cursor_line < scroll_row {
        scroll_row = cursor_line;
    } else if cursor_line >= scroll_row + visible_rows_usize {
        scroll_row = cursor_line + 1 - visible_rows_usize;
    }
    // Don't scroll past the last line.
    let max_scroll = g.rows().saturating_sub(visible_rows_usize);
    if scroll_row > max_scroll {
        scroll_row = max_scroll;
    }

    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut cursor_buffer_row: Option<u32> = None;
    let mut cursor_byte_in_row: Option<usize> = None;

    if !label.is_empty() {
        entries.push(text_area_label(label));
    }
    let label_offset: u32 = entries.len() as u32;

    for row_in_view in 0..visible_rows_usize {
        let (entry, caret) = text_area_row(value, &g, scroll_row + row_in_view);
        if let Some(col_in_line) = caret {
            cursor_buffer_row = Some(label_offset + row_in_view as u32);
            cursor_byte_in_row = Some(col_in_line);
        }
        entries.push(entry);
    }

    RenderedTextArea {
        entries,
        scroll_row: scroll_row as u32,
        cursor_buffer_row,
        cursor_byte_in_row,
    }
}

/// Translate a byte offset in `value` to (line_index, byte_in_line).
fn byte_to_line_col(value: &str, byte: usize) -> (usize, usize) {
    let byte = byte.min(value.len());
    let mut line = 0usize;
    let mut line_start = 0usize;
    for (i, &b) in value.as_bytes().iter().enumerate().take(byte) {
        if b == b'\n' {
            line += 1;
            line_start = i + 1;
        }
    }
    (line, byte - line_start)
}

/// Pad `line` with trailing spaces to `target` chars, or
/// tail-truncate with `…` if it overflows. Operates on chars to keep
/// the visual width predictable for ASCII; multibyte chars count as
/// one char each (terminal column width != char count for CJK, but
/// that's an acceptable v1 limitation matching `TextInput`).
fn pad_or_truncate_line(line: &str, target: usize) -> String {
    let chars: Vec<char> = line.chars().collect();
    if chars.len() <= target {
        let mut out = line.to_string();
        let pad = target - chars.len();
        for _ in 0..pad {
            out.push(' ');
        }
        out
    } else {
        let keep = target.saturating_sub(1);
        let mut out: String = chars.iter().take(keep).collect();
        out.push('…');
        out
    }
}

/// Pad / truncate `text` to exactly `cols` display columns, in
/// place. Uses char count as the display-width approximation —
/// good for ASCII; wide-char-aware width would need
/// `unicode-width`, but no current caller relies on that.
///
/// When truncating, the final visible column is replaced with `…`
/// so the cut is visually distinguishable from a value that
/// happens to be exactly `cols` long. Degenerate `cols == 0` and
/// `cols == 1` (no room for the ellipsis itself) fall back to a
/// plain cut.
pub fn pad_or_truncate_cols(text: &mut String, cols: usize) {
    // Measure in display columns, not chars: a `漢` or `😀` is one char
    // but two columns, and char-counted padding pushed every border to
    // the right of a wide glyph out of alignment.
    let cur = crate::primitives::display_width::str_width(text);
    if cur < cols {
        for _ in 0..(cols - cur) {
            text.push(' ');
        }
    } else if cur > cols {
        // Cut at the byte where the display width reaches `cols`, then
        // if we have room make the last column an `…` so the truncation
        // is visible. A wide glyph straddling the cut is dropped whole,
        // leaving a one-column gap the pad below fills.
        let cutoff = crate::primitives::display_width::byte_offset_at_visual_column(text, cols);
        text.truncate(cutoff);
        if cols >= 2 {
            while crate::primitives::display_width::str_width(text) > cols.saturating_sub(1) {
                text.pop();
            }
            text.push('…');
        }
        let w = crate::primitives::display_width::str_width(text);
        for _ in 0..cols.saturating_sub(w) {
            text.push(' ');
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn form_toggle_chip_stays_visible_on_narrow_panel() {
        // A page-wide label_width larger than the narrow panel must not
        // push the `[v]` chip past the right edge: the label is clamped
        // (and truncated if needed) so the chip always fits. Regression:
        // Editor toggles' chips vanished off-screen on a narrow terminal.
        let panel = 34u32; // narrow content width
        let (entry, chip) = render_toggle_form(
            true,
            false,
            "Highlight Matching Brackets",
            false,
            40, // requested label column wider than the panel
            panel,
            false,
        );
        let w = crate::primitives::display_width::str_width(&entry.text);
        assert!(
            w <= panel as usize,
            "row must fit the panel ({w} > {panel}): {:?}",
            entry.text
        );
        // The chip byte range is inside the text and reads `[v]`.
        assert_eq!(&entry.text[chip.0..chip.1], "[v]");
    }

    #[test]
    fn form_label_width_zero_panel_keeps_request() {
        // Auto-fit / tests (panel_width == 0) leave the requested width.
        assert_eq!(form_label_width(20, 2, 3, 0), 20);
    }

    use crate::primitives::display_width::str_width;

    /// A row that fits is its own text, markers and all absent.
    #[test]
    fn window_row_body_leaves_a_fitting_row_alone() {
        let w = window_row_body("hello", 20, None, 0);
        assert_eq!(w.text, "hello");
        assert_eq!((w.slice_start, w.slice_end, w.lead_bytes), (0, 5, 0));
    }

    /// Trailing padding is not content: a row padded past the budget by
    /// spaces alone does not pick up a marker that elides nothing.
    #[test]
    fn window_row_body_ignores_trailing_padding() {
        let padded = format!("abc{}", " ".repeat(40));
        let w = window_row_body(&padded, 10, None, 0);
        assert_eq!(w.text, padded, "padding is not something to elide");
    }

    /// With no anchor the window rests at the head and marks the cut tail.
    #[test]
    fn window_row_body_marks_a_cut_tail() {
        let w = window_row_body("abcdefghij", 5, None, 0);
        assert_eq!(w.text, "abcd…");
        assert_eq!(str_width(&w.text), 5);
    }

    /// With an anchor past the budget the window slides to bring it in, and
    /// marks both ends.
    #[test]
    fn window_row_body_rests_on_its_anchor() {
        let body = format!("{}MATCH{}", "x".repeat(60), "y".repeat(60));
        let w = window_row_body(&body, 20, Some((60, 5)), 0);
        assert!(
            w.text.contains("MATCH"),
            "window missed its anchor: {}",
            w.text
        );
        assert!(
            w.text.starts_with('…') && w.text.ends_with('…'),
            "{}",
            w.text
        );
        assert_eq!(str_width(&w.text), 20);
    }

    /// The pan is a delta from that resting column: negative reaches the head
    /// of the line, positive its tail, and each end clamps.
    #[test]
    fn window_row_body_pans_from_the_resting_column() {
        let body = format!("HEAD{}MATCH{}TAIL", "x".repeat(60), "y".repeat(60));
        let anchor = Some((64, 5));
        assert!(!window_row_body(&body, 20, anchor, 0).text.contains("HEAD"));

        let left = window_row_body(&body, 20, anchor, -1000);
        assert!(left.text.starts_with("HEAD"), "{}", left.text);
        assert_eq!(left.slice_start, 0, "clamped to the head, not past it");

        let right = window_row_body(&body, 20, anchor, 1000);
        assert!(right.text.ends_with("TAIL"), "{}", right.text);
        assert_eq!(right.slice_end, body.len(), "clamped to the tail");
    }

    /// Columns, not codepoints. A CJK body is half as many characters as it
    /// is cells, and the window has to measure what the reader sees — this is
    /// the case an ASCII-only test cannot fail on.
    #[test]
    fn window_row_body_measures_display_columns() {
        let body = format!("{}MATCH", "漢".repeat(30));
        // 30 wide chars are 60 columns: a codepoint-counting window would
        // think this fits in 40 and leave the row overflowing.
        let w = window_row_body(&body, 40, Some((30, 5)), 0);
        assert!(w.text.contains("MATCH"), "{}", w.text);
        assert!(
            str_width(&w.text) <= 40,
            "{} cols: {}",
            str_width(&w.text),
            w.text
        );
        // And a pan of N moves N columns, not N characters.
        let panned = window_row_body(&body, 40, Some((30, 5)), -8);
        assert_eq!(
            str_width(&w.text),
            str_width(&panned.text),
            "both windows fill the budget"
        );
        assert!(str_width(&panned.text) <= 40);
    }

    /// A window never splits a double-width cell across its edge.
    #[test]
    fn window_row_body_never_splits_a_wide_cell() {
        // Panned as well as at rest. A window that has been panned owes a
        // leading marker, which spends a column the resting window does not,
        // so the budgets where a row can overrun are only reachable with a
        // non-zero pan — at `budget == 1` the marker *is* the whole window
        // and a trailing one would draw a second column.
        for body in ["漢".repeat(20), "abcdefghij".to_string()] {
            for budget in 1..=40usize {
                for pan in [-40, -1, 0, 1, 5, 40] {
                    let w = window_row_body(&body, budget, None, pan);
                    assert!(
                        str_width(&w.text) <= budget,
                        "budget {budget}, pan {pan}: {:?} is {} columns",
                        w.text,
                        str_width(&w.text)
                    );
                    assert!(body.is_char_boundary(w.slice_start));
                    assert!(body.is_char_boundary(w.slice_end));
                }
            }
        }
    }

    /// An overlay on text that panned off screen is dropped, not clamped onto
    /// the row's first cell.
    #[test]
    fn tree_row_overlay_outside_the_window_is_dropped() {
        let body = format!("{}MATCH{}", "x".repeat(60), "y".repeat(60));
        let mut node = tnode(&body, 0, false);
        node.text.inline_overlays.push(InlineOverlay {
            start: 60,
            end: 65,
            style: OverlayOptions::default(),
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
        // Panned hard right: the overlay's text is no longer drawn.
        let r = render_tree_row(&node, false, false, 1, false, 30, 2, i32::MAX / 4);
        assert!(
            !r.entry.text.contains("MATCH"),
            "precondition: panned past the overlay. Row: {:?}",
            r.entry.text
        );
        assert!(
            r.entry.inline_overlays.is_empty(),
            "an overlay outside the drawn slice must be dropped, not clamped"
        );
    }

    /// The pinned head stays put while the rest of the row slides under it.
    #[test]
    fn tree_row_pins_its_declared_head() {
        let body = format!("path.rs:1 - {}MATCH{}", "x".repeat(60), "y".repeat(60));
        let mut node = tnode(&body, 0, false);
        node.window_anchor = Some(fresh_core::api::TextWindowAnchor {
            pinned: 12,
            start: 72,
            len: 5,
        });
        for pan in [0, 20, i32::MAX / 4] {
            let r = render_tree_row(&node, false, false, 1, false, 40, 2, pan);
            assert!(
                r.entry.text.contains("path.rs:1 - "),
                "pan {pan} slid the pinned head away: {:?}",
                r.entry.text
            );
        }
    }

    #[test]
    fn fit_label_truncates_with_ellipsis() {
        // Too long → truncated to width with a trailing `…`.
        let out = fit_label("VeryLongLanguageName", 8);
        assert_eq!(crate::primitives::display_width::str_width(&out), 8);
        assert!(out.ends_with('…'), "expected ellipsis: {out:?}");
        // Fits → right-padded to width.
        assert_eq!(fit_label("Go", 5), "Go   ");
    }

    #[test]
    fn hint_bar_renders_entries_with_key_overlays() {
        let entries = vec![
            HintEntry {
                keys: "Tab".into(),
                label: "next".into(),
            },
            HintEntry {
                keys: "Esc".into(),
                label: "close".into(),
            },
        ];
        let entry = render_hint_bar(&entries);
        assert_eq!(entry.text, "Tab next  Esc close");
        assert_eq!(entry.inline_overlays.len(), 2);
        // First overlay covers "Tab" (bytes 0..3).
        assert_eq!(entry.inline_overlays[0].start, 0);
        assert_eq!(entry.inline_overlays[0].end, 3);
        // Second overlay covers "Esc" (bytes 10..13).
        assert_eq!(entry.inline_overlays[1].start, 10);
        assert_eq!(entry.inline_overlays[1].end, 13);
    }

    #[test]
    fn hint_bar_omits_label_when_empty() {
        let entries = vec![HintEntry {
            keys: "?".into(),
            label: "".into(),
        }];
        let entry = render_hint_bar(&entries);
        assert_eq!(entry.text, "?");
    }

    #[test]
    fn toggle_checked_emits_glyph_overlay() {
        let entry = render_toggle(true, "Case", false, false);
        assert_eq!(entry.text, "[v] Case");
        // One overlay for the glyph, no focused overlay.
        assert_eq!(entry.inline_overlays.len(), 1);
        assert_eq!(entry.inline_overlays[0].start, 0);
        assert_eq!(entry.inline_overlays[0].end, 3);
    }

    #[test]
    fn toggle_unchecked_no_glyph_overlay() {
        let entry = render_toggle(false, "Case", false, false);
        assert_eq!(entry.text, "[ ] Case");
        assert_eq!(entry.inline_overlays.len(), 0);
    }

    #[test]
    fn toggle_focused_adds_full_entry_overlay() {
        let entry = render_toggle(true, "Case", true, false);
        // Glyph overlay + focused overlay.
        assert_eq!(entry.inline_overlays.len(), 2);
        // Focused overlay spans the full entry.
        assert_eq!(entry.inline_overlays[1].start, 0);
        assert_eq!(entry.inline_overlays[1].end, entry.text.len());
        assert!(entry.inline_overlays[1].style.bold);
    }

    // -------------------------------------------------------------
    // Hit-area tests
    // -------------------------------------------------------------

    // -------------------------------------------------------------
    // Focus management
    // -------------------------------------------------------------

    // -------------------------------------------------------------
    // List
    // -------------------------------------------------------------

    pub fn make_list(selected: i32, visible: u32, total: usize, key: Option<&str>) -> WidgetSpec {
        let items = (0..total)
            .map(|i| TextPropertyEntry::text(format!("row{}", i)))
            .collect();
        let item_keys = (0..total).map(|i| format!("k{}", i)).collect();
        WidgetSpec::List {
            items,
            item_specs: vec![],
            item_keys,
            selected_index: selected,
            visible_rows: Some(visible),
            focusable: true,
            key: key.map(|s| s.to_string()),
        }
    }

    // -------------------------------------------------------------
    // TextInput
    // -------------------------------------------------------------

    #[test]
    fn text_input_renders_value_in_brackets() {
        let entry = render_text_input("hello", -1, None, false, "", None, 0, 0, false, 0).entry;
        assert_eq!(entry.text, "[hello]");
        // Unfocused still carries the field background — that is the
        // whole point of it: an input has to look editable before
        // anyone has clicked it. The only overlay is that background.
        assert_eq!(entry.inline_overlays.len(), 1);
        let bg = entry.inline_overlays[0].style.bg.as_ref().unwrap();
        assert_eq!(bg.as_theme_key(), Some("editor.current_line_bg"));
    }

    #[test]
    fn text_input_with_label_prefixes_with_label_space() {
        let entry =
            render_text_input("foo", -1, None, false, "Search:", None, 0, 0, false, 0).entry;
        assert_eq!(entry.text, "Search: [foo]");
    }

    /// Overlays on `entry` whose background is `key`.
    fn overlays_with_bg<'e>(entry: &'e TextPropertyEntry, key: &str) -> Vec<&'e InlineOverlay> {
        entry
            .inline_overlays
            .iter()
            .filter(|o| o.style.bg.as_ref().and_then(|c| c.as_theme_key()) == Some(key))
            .collect()
    }

    #[test]
    fn text_input_adds_input_bg_overlay_regardless_of_focus() {
        // The field background is not a focus indicator — it is what
        // makes an input look editable before anyone has clicked it —
        // so it is present either way. It was focus-gated, which left
        // every unfocused input looking like inert text.
        //
        // Counted by theme key rather than by total overlay count: a
        // focused field carries the bracket band too, and asserting
        // `len() == 1` for both made this test a tripwire on the very
        // fix that put the band there.
        for focused in [true, false] {
            let entry = render_text_input("x", -1, None, focused, "", None, 0, 0, false, 0).entry;
            assert_eq!(
                overlays_with_bg(&entry, KEY_INPUT_BG).len(),
                1,
                "field background, focused={focused}"
            );
        }
    }

    #[test]
    fn text_input_focused_bands_its_brackets() {
        // ...and because that background is ungated it can no longer be
        // what marks focus. The caret cannot stand in on a panel mounted
        // into a buffer — the hardware cursor belongs to the document
        // and stays in its margin — so a focused field marks its own
        // frame: the two bracket cells, and nothing else on the row.
        let entry = render_text_input("x", -1, None, true, "", None, 0, 0, false, 0).entry;
        let bands = overlays_with_bg(&entry, KEY_FOCUSED_BG);
        assert_eq!(bands.len(), 2, "one band per bracket");
        let n = entry.text.len();
        assert!(entry.text.starts_with('[') && entry.text.ends_with(']'));
        assert_eq!((bands[0].start, bands[0].end), (0, 1), "opening bracket");
        assert_eq!(
            (bands[1].start, bands[1].end),
            (n - 1, n),
            "closing bracket"
        );

        let unfocused = render_text_input("x", -1, None, false, "", None, 0, 0, false, 0).entry;
        assert!(
            overlays_with_bg(&unfocused, KEY_FOCUSED_BG).is_empty(),
            "an unfocused field carries no band"
        );
    }

    #[test]
    fn text_input_focused_with_selection_adds_selection_bg_overlay() {
        // Focused + selection range → input-bg overlay AND a
        // selection-bg overlay scoped to the selected bytes.
        let entry = render_text_input(
            "hello world",
            5,
            Some((0, 5)),
            true,
            "",
            None,
            0,
            0,
            false,
            0,
        )
        .entry;
        // First char is at byte 1 (after `[`); selection over
        // bytes 0..5 of value → entry bytes 1..6.
        let sel = entry
            .inline_overlays
            .iter()
            .find(|o| {
                o.style.bg.as_ref().and_then(|c| c.as_theme_key())
                    == Some("ui.text_input_selection_bg")
            })
            .expect("selection overlay present");
        assert_eq!(sel.start, 1);
        assert_eq!(sel.end, 6);
    }

    #[test]
    fn text_input_unfocused_skips_selection_overlay() {
        // Selection only paints when focused — an inactive widget
        // shows no highlight.
        let entry =
            render_text_input("hello", -1, Some((0, 5)), false, "", None, 0, 0, false, 0).entry;
        let has_sel_overlay = entry.inline_overlays.iter().any(|o| {
            o.style.bg.as_ref().and_then(|c| c.as_theme_key()) == Some("ui.text_input_selection_bg")
        });
        assert!(!has_sel_overlay);
    }

    #[test]
    fn text_area_focused_with_selection_emits_per_row_overlays() {
        // Multi-line selection from line 0 col 2 to line 1 col 3.
        // Each visible row gets its own selection overlay clamped
        // to that row's content bytes.
        let r = render_text_area("abcd\nefgh", 8, Some((2, 8)), true, "", None, 2, 0, 0, 80);
        // Row 0 (line 0): selection from byte 2..4 (last 2 chars of "abcd").
        // Row 1 (line 1): selection from byte 0..3 (first 3 chars of "efgh").
        let row0 = &r.entries[0];
        let row1 = &r.entries[1];
        let sel0 = row0
            .inline_overlays
            .iter()
            .find(|o| {
                o.style.bg.as_ref().and_then(|c| c.as_theme_key())
                    == Some("ui.text_input_selection_bg")
            })
            .expect("row 0 selection overlay");
        assert_eq!((sel0.start, sel0.end), (2, 4));
        let sel1 = row1
            .inline_overlays
            .iter()
            .find(|o| {
                o.style.bg.as_ref().and_then(|c| c.as_theme_key())
                    == Some("ui.text_input_selection_bg")
            })
            .expect("row 1 selection overlay");
        assert_eq!((sel1.start, sel1.end), (0, 3));
    }

    #[test]
    fn text_input_cursor_byte_in_entry_at_value_position() {
        // Cursor mid-value: returned byte points at the position
        // *within entry.text*. text = "[abc ]" (focused → trailing
        // pad space). 'a' at byte 1, 'b' at 2, 'c' at 3 — so a
        // cursor at value-byte 1 lands at entry-byte 2.
        let r = render_text_input("abc", 1, None, true, "", None, 0, 0, false, 0);
        assert_eq!(r.cursor_byte_in_entry, Some(2));
    }

    #[test]
    fn text_input_cursor_at_end_lands_on_padding_space_not_bracket() {
        // Cursor at end-of-value: with focused + no field_width,
        // a trailing pad space is appended so the cursor never
        // overlaps the closing bracket. text = "[ab ]" → cursor
        // at value-byte 2 lands at entry-byte 3 (the space), not
        // at byte 4 (the `]`).
        let r = render_text_input("ab", 2, None, true, "", None, 0, 0, false, 0);
        assert_eq!(r.entry.text, "[ab ]");
        assert_eq!(r.cursor_byte_in_entry, Some(3));
        assert_ne!(r.cursor_byte_in_entry, Some(4), "must not overlap ]");
    }

    #[test]
    fn text_input_unfocused_empty_shows_placeholder_in_muted() {
        let entry =
            render_text_input("", -1, None, false, "", Some("type here"), 0, 0, false, 0).entry;
        assert_eq!(entry.text, "[type here]");
        // Placeholder gets a muted-fg italic overlay.
        let placeholder_overlay = entry
            .inline_overlays
            .iter()
            .find(|o| o.style.fg.as_ref().and_then(|c| c.as_theme_key()).is_some())
            .expect("placeholder fg overlay");
        let fg = placeholder_overlay.style.fg.as_ref().unwrap();
        assert_eq!(fg.as_theme_key(), Some("editor.whitespace_indicator_fg"));
        assert!(placeholder_overlay.style.italic);
    }

    #[test]
    fn text_input_focused_empty_still_shows_placeholder() {
        // New behaviour: placeholder remains visible while focused
        // until the user types something. Cursor parks at byte 0
        // of the placeholder so the first keystroke replaces it.
        let r = render_text_input("", -1, None, true, "", Some("type here"), 0, 0, false, 0);
        assert_eq!(r.entry.text, "[type here]");
        assert_eq!(r.cursor_byte_in_entry, Some(1));
    }

    #[test]
    fn text_input_field_width_pads_short_value_unfocused() {
        // field_width=10, unfocused, not full_width → inner is 10
        // chars (no extra cursor-park pad).
        let r = render_text_input("hi", 2, None, false, "", None, 0, 10, false, 0);
        assert_eq!(r.entry.text, "[hi        ]");
    }

    #[test]
    fn text_input_field_width_focused_adds_cursor_park_space() {
        // field_width=10, focused, value fills exactly 10 → inner
        // is 11 chars (10 + 1 cursor-park space) so the cursor at
        // end-of-value never lands on `]`.
        let r = render_text_input("0123456789", 10, None, true, "", None, 0, 10, false, 0);
        assert_eq!(r.entry.text, "[0123456789 ]");
        // Cursor at byte 10 of value → byte 10 of inner → byte 11
        // of entry.text (after `[`). That's the cursor-park space,
        // not `]` (which lives at byte 12).
        assert_eq!(r.cursor_byte_in_entry, Some(11));
        assert_ne!(r.cursor_byte_in_entry, Some(12), "must not land on ]");
    }

    #[test]
    fn text_input_field_width_full_width_pads_to_same_size_when_unfocused() {
        // full_width=true makes the inner reserve the cursor-park
        // space whether or not the input is focused, so the field
        // doesn't "jump" wider on focus.
        let r = render_text_input("hi", -1, None, false, "", None, 0, 10, true, 0);
        assert_eq!(r.entry.text, "[hi         ]"); // 10 + 1 trailing pad
    }

    #[test]
    fn text_input_field_width_head_truncates_long_value() {
        // 30-char value, field_width=10, unfocused → keep last 9
        // chars + `…`; no pad space.
        let r = render_text_input(
            "0123456789abcdefghijklmnopqrst",
            30,
            None,
            false,
            "",
            None,
            0,
            10,
            false,
            0,
        );
        assert!(r.entry.text.contains("…lmnopqrst"));
    }

    #[test]
    fn text_input_field_width_window_follows_caret_to_the_value_start() {
        // Long value, field_width=5, focused, caret at byte 0: the
        // window scrolls home so the caret sits on the value's first
        // char — it used to clamp to just right of a `…` that never
        // moved, leaving the head of the value unreachable on screen.
        let r = render_text_input("abcdefghij", 0, None, true, "", None, 0, 5, false, 9);
        // Inner = `abcd…` (4 value chars + the cut-tail marker) plus
        // the focused pad space; `[` precedes it in `entry.text`.
        assert_eq!(r.entry.text, "[abcd… ]");
        assert_eq!(r.cursor_byte_in_entry, Some(1));
        assert_eq!(r.scroll_chars, 0);
        // Nothing is hidden off the left, so a click maps straight
        // through (no leading ellipsis to step over).
        assert_eq!(r.value_dropped_bytes, 0);
        assert_eq!(r.ellipsis_bytes, 0);
    }

    #[test]
    fn text_input_field_width_window_holds_still_while_the_caret_moves_inside_it() {
        // Window parked at char 5 (`fghi` + markers). A caret inside
        // it moves without dragging the view along.
        let r = render_text_input("abcdefghijklmn", 7, None, true, "", None, 0, 6, false, 5);
        assert_eq!(r.entry.text, "[…fghi… ]");
        assert_eq!(r.scroll_chars, 5);
        // Caret on 'h' = value byte 7 = 2 chars past the window start,
        // after `[` and the leading `…`.
        assert_eq!(r.cursor_byte_in_entry, Some(1 + "…".len() + 2));
        assert_eq!(r.value_dropped_bytes, 5);
        assert_eq!(r.ellipsis_bytes, "…".len());
    }

    #[test]
    fn text_input_field_width_window_slides_one_char_when_the_caret_steps_out() {
        // Caret one char left of the window start pulls the window
        // back by exactly that much — a Left-key walk scrolls the
        // value smoothly instead of stopping dead.
        let r = render_text_input("abcdefghijklmn", 4, None, true, "", None, 0, 6, false, 5);
        assert_eq!(r.scroll_chars, 4);
        assert_eq!(r.entry.text, "[…efgh… ]");
        assert_eq!(r.cursor_byte_in_entry, Some(1 + "…".len()));
    }

    #[test]
    fn text_input_field_width_window_pushes_right_when_the_caret_passes_its_end() {
        // Caret past the window's last column pushes the window on so
        // the caret stays painted (typing at the tail, End, a click
        // beyond the view).
        let r = render_text_input("abcdefghijklmn", 14, None, true, "", None, 0, 6, false, 0);
        // Window at the far end: `…jklmn` + the caret's pad space.
        assert_eq!(r.entry.text, "[…jklmn ]");
        assert_eq!(r.scroll_chars, 9);
        assert_eq!(r.cursor_byte_in_entry, Some(1 + "…".len() + 5));
    }

    #[test]
    fn text_input_field_width_window_resets_once_the_value_fits_again() {
        // Value trimmed back under the field width: the window snaps
        // home and the persisted offset clears, so the field doesn't
        // stay scrolled off a value it can show whole.
        let r = render_text_input("abc", 3, None, true, "", None, 0, 6, false, 9);
        assert_eq!(r.entry.text, "[abc    ]");
        assert_eq!(r.scroll_chars, 0);
    }

    #[test]
    fn text_input_field_width_window_handles_multibyte_values() {
        // Multi-byte chars: the window is measured in chars, sliced on
        // char boundaries, and the caret byte maps through both `…`
        // and the wide chars before it.
        let value = "αβγδεζηθικλμ";
        let caret = value.char_indices().nth(6).unwrap().0; // before 'η'
        let r = render_text_input(value, caret as i32, None, true, "", None, 0, 5, false, 8);
        assert_eq!(r.scroll_chars, 6);
        assert_eq!(r.entry.text, "[…ηθι… ]");
        assert_eq!(r.cursor_byte_in_entry, Some(1 + "…".len()));
        assert_eq!(r.value_dropped_bytes, "αβγδεζ".len());
    }

    #[test]
    fn text_input_truncates_long_value_keeping_tail_visible() {
        let value: String = "0123456789abcdefghij".to_string();
        let entry = render_text_input(&value, -1, None, false, "", None, 6, 0, false, 0).entry;
        // Tail-truncated to "…fghij" (max=6, take=5 chars).
        assert_eq!(entry.text, "[…fghij]");
    }

    // -------------------------------------------------------------
    // Tree
    // -------------------------------------------------------------

    fn tnode(text: &str, depth: u32, has_children: bool) -> TreeNode {
        TreeNode {
            text: TextPropertyEntry::text(text),
            depth,
            has_children,
            checked: None,
            extra_lines: Vec::new(),
            window_anchor: None,
        }
    }

    fn make_tree(
        nodes: Vec<TreeNode>,
        item_keys: Vec<&str>,
        selected: i32,
        visible: u32,
        expanded: Vec<&str>,
        key: Option<&str>,
    ) -> WidgetSpec {
        WidgetSpec::Tree {
            nodes,
            item_keys: item_keys.iter().map(|s| s.to_string()).collect(),
            selected_index: selected,
            visible_rows: Some(visible),
            expanded_keys: expanded.iter().map(|s| s.to_string()).collect(),
            checkable: false,
            item_height: 1,
            card_borders: false,
            indent_cols: 2,
            key: key.map(|s| s.to_string()),
        }
    }

    /// A pan is a delta from where each row *rests*, so the two directions
    /// are bounded by different numbers: a row whose match is far along a long
    /// line has a great deal to its left and little to its right.
    ///
    /// One number for both is what let `S-End` store a value `S-Left` could
    /// not walk back.
    #[test]
    fn pan_bounds_are_measured_from_where_rows_rest() {
        let mut node = tnode(&"x".repeat(400), 0, false);
        node.window_anchor = Some(fresh_core::api::TextWindowAnchor {
            pinned: 0,
            start: 380,
            len: 5,
        });
        let tree = make_tree(vec![node], vec!["a"], 0, 10, vec![], Some("t"));
        let (left, right) = pan_bounds(&tree, 100, None);
        assert!(
            (0..20).contains(&right),
            "a match 380 columns along a 400 column row, in a 100 column \
             window, has almost nothing to its right — the bound says {right}"
        );
        assert!(
            (-360..-280).contains(&left),
            "the head of that row is roughly 320 columns to its left — the \
             bound says {left}"
        );
    }

    /// "Pan to the end" is a question about the row the reader is on.
    ///
    /// The pan is shared, so rows of unequal length cannot all sit at their
    /// tail at once. Answering with the longest row's travel leaves the
    /// selected row clamped at its own tail with keystrokes still to spend
    /// before it moves — which is what a reader reports as "Shift+arrow does
    /// nothing". The whole tree's range still bounds the *stored* value, so
    /// panning right does not stop early for the longer row.
    #[test]
    fn pan_bounds_can_be_asked_about_one_row() {
        let long = {
            let mut n = tnode(
                &format!("{}M{}", "x".repeat(100), "y".repeat(300)),
                0,
                false,
            );
            n.window_anchor = Some(fresh_core::api::TextWindowAnchor {
                pinned: 0,
                start: 100,
                len: 1,
            });
            n
        };
        let short = {
            let mut n = tnode(&format!("{}M{}", "x".repeat(100), "y".repeat(60)), 0, false);
            n.window_anchor = Some(fresh_core::api::TextWindowAnchor {
                pinned: 0,
                start: 100,
                len: 1,
            });
            n
        };
        let tree = make_tree(
            vec![short.clone(), long.clone()],
            vec!["a", "b"],
            0,
            10,
            vec![],
            Some("t"),
        );
        let cols = 60u32;
        let whole = pan_bounds(&tree, cols, None).1;
        let just_short = pan_bounds(&tree, cols, Some(0)).1;
        let just_long = pan_bounds(&tree, cols, Some(1)).1;
        assert!(
            just_short < whole,
            "the short row reaches its tail before the tree does ({just_short} vs {whole})"
        );
        assert_eq!(
            just_long, whole,
            "the long row is what sets the tree's own bound"
        );
        // And the row-scoped answer is the row's real clamp: panning by it
        // lands on the tail, one step more shows nothing new.
        let at = |pan: i32| {
            render_tree_row(&short, false, false, 1, false, cols, 0, pan)
                .entry
                .text
        };
        assert_ne!(at(just_short), at(just_short - PAN_COLUMNS));
        assert_eq!(
            at(just_short),
            at(whole),
            "past its own tail is still its tail"
        );
    }

    /// The bound is the clamp the paint would apply, not an estimate of it:
    /// panning by exactly it lands on the last column the row can show, and
    /// one more column changes nothing.
    #[test]
    fn pan_bounds_agree_with_what_the_window_draws() {
        let body = format!("{}MATCH{}", "x".repeat(200), "y".repeat(200));
        let mut node = tnode(&body, 0, false);
        node.window_anchor = Some(fresh_core::api::TextWindowAnchor {
            pinned: 0,
            start: 200,
            len: 5,
        });
        let tree = make_tree(vec![node.clone()], vec!["a"], 0, 10, vec![], Some("t"));
        let cols = 60u32;
        let (left, right) = pan_bounds(&tree, cols, None);
        let at = |pan: i32| {
            render_tree_row(&node, false, false, 1, false, cols, 0, pan)
                .entry
                .text
        };
        assert_eq!(
            at(right),
            at(right + 40),
            "past the right bound is not further right"
        );
        assert_ne!(at(right), at(right - 8), "the right bound is reachable");
        assert_eq!(
            at(left),
            at(left - 40),
            "past the left bound is not further left"
        );
        assert_ne!(at(left), at(left + 8), "the left bound is reachable");
        assert!(
            at(left).contains('x'),
            "the left bound shows the head: {}",
            at(left)
        );
        assert!(
            at(right).ends_with('y'),
            "the right bound shows the tail: {}",
            at(right)
        );
    }

    /// A row with no anchor rests at column zero: nothing to its left, its
    /// whole length to its right. A kind whose paint cannot show a pan does
    /// not accumulate one, and neither does a widget nothing has laid out.
    #[test]
    fn pan_bounds_without_an_anchor_a_tree_or_a_width() {
        let tree = make_tree(
            vec![tnode(&"x".repeat(400), 0, false)],
            vec!["a"],
            0,
            10,
            vec![],
            Some("t"),
        );
        assert_eq!(
            pan_bounds(&tree, 100, None).0,
            0,
            "nothing to the left of column zero"
        );
        assert!(pan_bounds(&tree, 100, None).1 > 300);
        assert_eq!(
            pan_bounds(&tree, 0, None),
            (0, 0),
            "a widget nothing has laid out has no width to be panned against"
        );
        assert_eq!(
            pan_bounds(
                &WidgetSpec::Spacer {
                    cols: 1,
                    flex: false,
                    key: None,
                },
                100,
                None
            ),
            (0, 0),
            "a kind that never threads the pan has nothing to pan"
        );
    }

    /// Trailing padding is not content on this path either: a row padded out
    /// to the panel's width must not claim a pan that would show only spaces.
    #[test]
    fn pan_bounds_ignore_trailing_padding() {
        let node = tnode(&format!("{}{}", "x".repeat(40), " ".repeat(360)), 0, false);
        assert_eq!(
            pan_bounds(
                &make_tree(vec![node], vec!["a"], 0, 10, vec![], Some("t")),
                100,
                None
            ),
            (0, 0)
        );
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_collapsed() {
        let r = render_tree_row(
            &tnode("file.txt", 0, true),
            false,
            false,
            1,
            false,
            80,
            2,
            0,
        );
        assert!(r.entry.text.starts_with('\u{25B6}'), "starts with ▶");
        assert!(r.entry.text.contains("file.txt"));
        assert!(r.disclosure_range.is_some());
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_expanded() {
        let r = render_tree_row(&tnode("file.txt", 0, true), true, false, 1, false, 80, 2, 0);
        assert!(r.entry.text.starts_with('\u{25BC}'), "starts with ▼");
    }

    #[test]
    fn tree_row_leaf_uses_two_spaces_no_disclosure_hit() {
        let r = render_tree_row(&tnode("match", 0, false), false, false, 1, false, 80, 2, 0);
        // No glyph, just spaces for alignment.
        assert!(r.entry.text.starts_with("  "));
        assert!(r.entry.text.contains("match"));
        assert!(r.disclosure_range.is_none());
    }

    #[test]
    fn tree_row_indents_by_depth_times_two() {
        let r = render_tree_row(&tnode("nested", 2, false), false, false, 1, false, 80, 2, 0);
        // depth=2 → 4 leading spaces, then 2 alignment spaces, then "nested".
        assert!(r.entry.text.starts_with("      nested"));
    }

    #[test]
    fn tree_row_shifts_plugin_overlays_by_prefix() {
        let mut node = tnode("hello", 1, false);
        node.text.inline_overlays.push(InlineOverlay {
            start: 0,
            end: 5,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
        let r = render_tree_row(&node, false, false, 1, false, 80, 2, 0);
        // depth=1 → 2 indent + 2 alignment = 4 prefix bytes (ASCII).
        // The plugin's [0..5] becomes [4..9].
        let plugin_overlay = r
            .entry
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("bold overlay carried through");
        assert_eq!(plugin_overlay.start, 4);
        assert_eq!(plugin_overlay.end, 9);
    }

    #[test]
    fn tree_row_omits_checkbox_when_not_checkable() {
        // Even with `checked: Some(_)`, no glyph if `checkable: false`.
        let mut node = tnode("file.rs", 0, false);
        node.checked = Some(true);
        let r = render_tree_row(&node, false, false, 1, false, 80, 2, 0);
        assert!(r.checkbox_range.is_none());
        assert!(!r.entry.text.contains("[v]"));
        assert!(!r.entry.text.contains("[ ]"));
    }

    #[test]
    fn tree_row_omits_checkbox_when_checked_is_none() {
        // `checkable: true` but `checked: None` → still no glyph.
        // Lets a checkable tree mix non-checkbox-bearing nodes
        // (e.g. a separator or header) with checkbox rows.
        let node = tnode("section", 0, false);
        let r = render_tree_row(&node, false, true, 1, false, 80, 2, 0);
        assert!(r.checkbox_range.is_none());
        assert!(!r.entry.text.contains("[v]"));
        assert!(!r.entry.text.contains("[ ]"));
    }

    #[test]
    fn tree_row_renders_checked_glyph_after_disclosure() {
        let mut node = tnode("file.rs", 0, true);
        node.checked = Some(true);
        let r = render_tree_row(&node, true, true, 1, false, 80, 2, 0);
        assert!(r.checkbox_range.is_some(), "checkbox range emitted");
        let (cb_start, cb_end) = r.checkbox_range.unwrap();
        // Layout: ▼(3 bytes UTF-8) + " " + [v] + " " + body
        assert_eq!(&r.entry.text[cb_start..cb_end], "[v]");
        assert!(r.entry.text.contains("[v] file.rs"));
    }

    #[test]
    fn tree_row_renders_unchecked_glyph_for_leaf() {
        let mut node = tnode("match-row", 1, false);
        node.checked = Some(false);
        let r = render_tree_row(&node, false, true, 1, false, 80, 2, 0);
        let (cb_start, cb_end) = r
            .checkbox_range
            .expect("checkbox range for leaf with checked: Some");
        assert_eq!(&r.entry.text[cb_start..cb_end], "[ ]");
        // depth=1 → 2-space indent; leaf-alignment → 2 spaces; then `[ ]` + " ".
        assert!(r.entry.text.starts_with("    [ ] match-row"));
    }

    #[test]
    fn tree_row_checkbox_glyph_byte_range_addresses_correct_text() {
        // Sanity: byte_start..byte_end must extract the glyph
        // verbatim (no UTF-8 boundary issues from the disclosure).
        let mut node = tnode("path/with/é", 0, true);
        node.checked = Some(true);
        let r = render_tree_row(&node, false, true, 1, false, 80, 2, 0);
        let (cb_start, cb_end) = r.checkbox_range.unwrap();
        assert!(r.entry.text.is_char_boundary(cb_start));
        assert!(r.entry.text.is_char_boundary(cb_end));
        assert_eq!(&r.entry.text[cb_start..cb_end], "[v]");
    }

    #[test]
    fn tree_tabbable_keys_include_tree_with_key() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "T".into(),
                    focused: false,
                    key: Some("toggle".into()),
                },
                make_tree(
                    vec![tnode("a", 0, false)],
                    vec!["a"],
                    -1,
                    10,
                    vec![],
                    Some("tree"),
                ),
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["toggle", "tree"]);
    }

    // -------------------------------------------------------------
    // TextArea
    // -------------------------------------------------------------

    fn make_text_area(
        value: &str,
        cursor_byte: i32,
        focused: bool,
        rows: u32,
        field_width: u32,
        key: Option<&str>,
    ) -> WidgetSpec {
        WidgetSpec::Text {
            sel_start: -1,
            sel_end: -1,
            block_caret: false,
            label_width: 0,
            value: value.into(),
            cursor_byte,
            focused,
            label: String::new(),
            placeholder: None,
            // Force multi-line behaviour even when the test passes
            // `rows: 1` — the previous TextArea-specific tests
            // exercise the multi-line code path through this
            // helper.
            rows: rows.max(2),
            field_width,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            read_only: false,
            markdown: false,
            key: key.map(|s| s.into()),
        }
    }

    #[test]
    fn text_area_unfocused_empty_shows_placeholder_in_first_row() {
        // Test the renderer directly (focused=false). Host-owned
        // focus would otherwise auto-focus the only tabbable
        // widget — see `text_area_publishes_focus_cursor_at_value_position`
        // for the focused path.
        let r = render_text_area("", -1, None, false, "", Some("write here"), 2, 12, 0, 80);
        assert!(r.entries[0].text.starts_with("write here"));
        // Placeholder uses the muted-fg overlay.
        let fg = r.entries[0]
            .inline_overlays
            .iter()
            .find_map(|o| o.style.fg.as_ref())
            .and_then(|c| c.as_theme_key());
        assert_eq!(fg, Some("editor.whitespace_indicator_fg"));
    }

    #[test]
    fn text_area_tabbable_keys_include_text_area_with_key() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "T".into(),
                    focused: false,
                    key: Some("toggle".into()),
                },
                make_text_area("", -1, false, 3, 10, Some("note")),
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["toggle", "note"]);
    }

    // -------------------------------------------------------------
    // LabeledSection
    // -------------------------------------------------------------

    pub fn make_text_input(
        value: &str,
        cursor_byte: i32,
        focused: bool,
        full_width: bool,
        field_width: u32,
        key: Option<&str>,
    ) -> WidgetSpec {
        WidgetSpec::Text {
            sel_start: -1,
            sel_end: -1,
            block_caret: false,
            label_width: 0,
            value: value.into(),
            cursor_byte,
            focused,
            label: String::new(),
            placeholder: None,
            rows: 1,
            field_width,
            max_visible_chars: 0,
            full_width,
            completions: Vec::new(),
            completions_visible_rows: 0,
            read_only: false,
            markdown: false,
            key: key.map(|s| s.into()),
        }
    }

    #[test]
    fn labeled_section_includes_child_in_tabbable() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::LabeledSection {
                    label: "Name".into(),
                    child: Box::new(make_text_input("", -1, false, false, 0, Some("n"))),
                    width_cols: None,
                    width_pct: None,
                    key: None,
                    hover_style: None,
                },
                WidgetSpec::LabeledSection {
                    label: "Cmd".into(),
                    child: Box::new(make_text_input("", -1, false, false, 0, Some("c"))),
                    width_cols: None,
                    width_pct: None,
                    key: None,
                    hover_style: None,
                },
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["n", "c"]);
    }

    fn make_number(value: f64, key: Option<&str>) -> WidgetSpec {
        WidgetSpec::Number {
            label_width: 0,
            value,
            min: None,
            max: None,
            step: 1.0,
            integer: false,
            percent: false,
            label: String::new(),
            focused: false,
            key: key.map(|k| k.to_string()),
        }
    }

    #[test]
    fn format_number_value_variants() {
        assert_eq!(format_number_value(3.0, false, false), "3");
        assert_eq!(format_number_value(3.5, false, false), "3.5");
        assert_eq!(format_number_value(3.7, true, false), "4");
        assert_eq!(format_number_value(0.25, false, true), "25%");
    }

    #[test]
    fn clamp_number_respects_bounds() {
        assert_eq!(clamp_number(5.0, Some(0.0), Some(10.0)), 5.0);
        assert_eq!(clamp_number(-1.0, Some(0.0), Some(10.0)), 0.0);
        assert_eq!(clamp_number(99.0, Some(0.0), Some(10.0)), 10.0);
        assert_eq!(clamp_number(99.0, None, None), 99.0);
    }

    #[test]
    fn number_renders_form_cell_and_value() {
        let r = render_number(3.0, true, false, "Size", false, 0, None, false);
        assert_eq!(r.entry.text, "Size: [  3 ]");
        // The value range covers the inner cell.
        assert_eq!(&r.entry.text[r.value_range.0..r.value_range.1], "  3 ");
    }

    #[test]
    fn number_editing_shows_buffer_selection_and_caret() {
        let r = render_number(
            3.0,
            true,
            false,
            "Size",
            false,
            0,
            Some(NumberEdit {
                text: "750",
                cursor: 3,
                sel_start: 0,
                sel_end: 3,
            }),
            false,
        );
        assert_eq!(r.entry.text, "Size: [750 ]");
        // Selection bg over the digits + a REVERSED caret cell.
        assert!(r
            .entry
            .inline_overlays
            .iter()
            .any(|o| o.style.bg.is_some() && !o.style.reversed));
        assert!(r.entry.inline_overlays.iter().any(|o| o.style.reversed));
    }

    #[test]
    fn number_is_tabbable() {
        let spec = make_number(0.0, Some("n"));
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["n"]);
    }

    fn make_dropdown(options: &[&str], selected: i32, key: Option<&str>) -> WidgetSpec {
        WidgetSpec::Dropdown {
            label_width: 0,
            open: false,
            scroll_offset: 0,
            options: options.iter().map(|s| s.to_string()).collect(),
            selected_index: selected,
            label: String::new(),
            focused: false,
            key: key.map(|k| k.to_string()),
        }
    }

    #[test]
    fn wrap_index_wraps_both_directions() {
        assert_eq!(wrap_index(0, -1, 3), 2);
        assert_eq!(wrap_index(2, 1, 3), 0);
        assert_eq!(wrap_index(1, 1, 3), 2);
        assert_eq!(wrap_index(0, 1, 0), 0); // empty
    }

    #[test]
    fn dropdown_renders_selected_option_with_arrow() {
        let r = render_dropdown(
            &["Red".into(), "Green".into(), "Blue".into()],
            1,
            "Color",
            false,
            0,
            false,
            0,
            false,
        );
        assert_eq!(r.entry.text, "Color: [Green ▼]");
        assert!(r.option_rows.is_empty());
    }

    #[test]
    fn dropdown_open_renders_inline_option_rows() {
        let r = render_dropdown(
            &["Red".into(), "Green".into(), "Blue".into()],
            1,
            "Color",
            true,
            0,
            true,
            0,
            false,
        );
        assert!(r.entry.text.contains("▲"));
        assert_eq!(r.option_rows.len(), 3);
        assert!(r.option_rows[0].1.text.contains("Red"));
        assert_eq!(r.option_rows[2].0, 2);
    }

    #[test]
    fn dropdown_is_tabbable() {
        let spec = make_dropdown(&["a"], 0, Some("d"));
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["d"]);
    }

    fn opts(pairs: &[(&str, &str)]) -> Vec<DualListOption> {
        pairs
            .iter()
            .map(|(v, l)| DualListOption {
                value: v.to_string(),
                label: l.to_string(),
            })
            .collect()
    }

    #[test]
    fn dual_available_excludes_included_and_excluded() {
        let o = opts(&[("a", "A"), ("b", "B"), ("c", "C"), ("d", "D")]);
        let avail = dual_available_values(&o, &["b".into()], &["d".into()]);
        // b is included, d is excluded → only a, c remain (in order).
        assert_eq!(avail, vec!["a".to_string(), "c".to_string()]);
    }

    #[test]
    fn dual_sanitize_drops_unknown_values() {
        let o = opts(&[("a", "A"), ("b", "B")]);
        let clean = dual_sanitize_included(&o, &["b".into(), "zzz".into(), "a".into()]);
        assert_eq!(clean, vec!["b".to_string(), "a".to_string()]);
    }

    #[test]
    fn dual_label_falls_back_to_value() {
        let o = opts(&[("a", "Apple")]);
        assert_eq!(dual_label(&o, "a"), "Apple");
        assert_eq!(dual_label(&o, "missing"), "missing");
    }

    fn make_dual(options: &[(&str, &str)], included: &[&str], key: Option<&str>) -> WidgetSpec {
        WidgetSpec::DualList {
            options: opts(options),
            included: included.iter().map(|s| s.to_string()).collect(),
            excluded: Vec::new(),
            label: "Elements".into(),
            focused: false,
            active_included: false,
            available_cursor: 0,
            included_cursor: 0,
            hint: String::new(),
            visible_rows: 3,
            key: key.map(|k| k.to_string()),
        }
    }

    #[test]
    fn dual_list_is_tabbable() {
        let spec = make_dual(&[("a", "A")], &[], Some("d"));
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        assert_eq!(tabbable, vec!["d"]);
    }
    // -------------------------------------------------------------
    // Layout-box tree (phase 3 substrate)
    // -------------------------------------------------------------

    // -------------------------------------------------------------
    // WidgetImpl::on_wheel (phase 4 dispatch)
    // -------------------------------------------------------------
}
