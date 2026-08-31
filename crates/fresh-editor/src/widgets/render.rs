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

use crate::widgets::layout_box::LayoutBox;
use crate::widgets::registry::{HitArea, WidgetInstanceState};
use fresh_core::api::{
    ButtonKind, DualListOption, HintEntry, OverlayColorSpec, OverlayOptions, TreeNode, WidgetSpec,
};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};
use std::collections::{HashMap, HashSet};

// Theme keys used by the v1 widget renderers. Centralized so future
// "role-based" theming (§7 of the design doc) has one place to
// substitute the role→key mapping.
pub(crate) const KEY_HELP_KEY_FG: &str = "ui.help_key_fg";
// Foreground of a checked Toggle's `[v]` glyph. `ui.help_key_fg`
// is the "keyboard-key / highlight on a popup body" theme key —
// every shipped theme picks a colour that contrasts with
// `ui.popup_bg`. The previous choice (`ui.tab_active_fg`) was
// designed to contrast with `tab_active_bg`, not the popup body;
// in `high-contrast` both ended up black so the `[v]` glyph
// vanished on every unfocused toggle. `help_key_fg` keeps the
// emphasis intent (a bright accent colour) while reliably
// surviving the popup background.
pub(crate) const KEY_TOGGLE_ON_FG: &str = "ui.help_key_fg";
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
pub(crate) const KEY_FOCUSED_FG: &str = "ui.popup_selection_fg";
pub(crate) const KEY_FOCUSED_BG: &str = "ui.popup_selection_bg";
// Backing band painted under whatever the pointer is on — a button, a
// toggle, a list row, a tree node. `ui.menu_hover_bg` is the editor's
// existing "pointer is here" surface (the menu bar and its dropdowns
// already use it), so a widget panel hovering the same way costs no new
// theme key and reads identically to the rest of the app. Deliberately
// weaker than `KEY_FOCUSED_BG`: hover says "you could act here", focus
// and selection say "you are here", and a hovered row must not be
// mistakable for the selected one.
pub(crate) const KEY_HOVER_BG: &str = "ui.menu_hover_bg";
// Leading marker prepended to the *focused* control (button /
// toggle / text input) so "which control is focused" is legible
// from a plain terminal capture — not just from the (theme-
// dependent, capture-invisible) `popup_selection` background or
// the hardware cursor. One glyph + a trailing space = two display
// columns. Only ever applied to the single focused widget, so at
// most one `▸` is on screen at a time; combined with the
// `popup_selection` fg/bg flip it makes focus unmistakable, and
// distinct from a `Primary` button's standing bold accent (which
// carries no marker). See `render_button` / `render_toggle` /
// `render_widget_text`.
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
const FRAMED_BUTTON_CHROME_COLS: usize = 4;

/// The two-column gutter prefix a focusable control leads with when
/// the render reserves the focus-marker gutter
/// ([`RenderContext::marker_gutter`]): `▸ ` for the focused control,
/// two spaces for every other control. Returns `""` when the panel
/// didn't opt into the gutter, so non-marker panels render
/// byte-for-byte as before.
pub(crate) fn focus_gutter_prefix(focused: bool, marker_gutter: bool) -> &'static str {
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
pub(crate) const KEY_DANGER_FG: &str = "diagnostic.error_fg";
pub(crate) const KEY_INPUT_BG: &str = "ui.prompt_bg";
// Background tint for the selection span inside a widget Text
// input. Distinct from the buffer's `ui.selection_bg` because
// widget inputs sit on top of the `ui.prompt_bg` field-bg overlay
// and the contrast needs to read against that tint, not the
// editor surface.
pub(crate) const KEY_TEXT_INPUT_SELECTION_BG: &str = "ui.text_input_selection_bg";
// Placeholder text uses the whitespace-indicator key — a dimmer
// grey than `ui.menu_disabled_fg` (themes ship ~RGB(70,70,70)
// vs ~RGB(100,100,100) for disabled menu items), so hint copy
// reads as background guidance rather than a half-active value.
pub(crate) const KEY_PLACEHOLDER_FG: &str = "editor.whitespace_indicator_fg";
// Section-legend tint. `ui.help_key_fg` is the same key the
// hint-bar uses to highlight keys against panel bg, so we know
// it's tuned for readability against the same surface a
// LabeledSection sits on.
pub(crate) const KEY_SECTION_LABEL_FG: &str = "ui.help_key_fg";
// Dim separator that replaces the input's bottom border when the
// completion popup is open. `ui.menu_disabled_fg` is the closest
// "muted chrome" key already shipped by every theme (gray-ish in
// dark themes, light gray in light themes) so the separator reads
// as a recessed transition between the active input and the
// candidate list rather than as a hard divider.
pub(crate) const KEY_COMPLETION_DIM_FG: &str = "ui.menu_disabled_fg";
// Selected completion row foreground/background. Same keys the
// popup-driven selection highlight uses everywhere else (host
// prompt suggestions, action-popup menu), so themes that
// re-skin one re-skin the other.
pub(crate) const KEY_COMPLETION_SEL_FG: &str = "ui.popup_selection_fg";
pub(crate) const KEY_COMPLETION_SEL_BG: &str = "ui.popup_selection_bg";
// Foreground for *unselected* completion rows. Without this, the
// row text inherits the terminal's default foreground, which has
// no relationship to the popup's themed `popup_bg` and reads
// poorly on coloured backgrounds.
pub(crate) const KEY_COMPLETION_FG: &str = "ui.popup_text_fg";
// Border chrome the popup paints around its own rows (the
// `│ ... │` sides extending below the input + the `╰─...─╯`
// closing border). Distinct theme key from the wrapping
// labeled section's default (unstyled) chrome so the popup
// reads as its own surface — matches the user's "use a theme
// key for the popup border" expectation.
pub(crate) const KEY_COMPLETION_BORDER_FG: &str = "ui.popup_border_fg";

/// Where the host should place the buffer's hardware cursor — the
/// terminal's blinking caret — when a `TextInput` is focused. Built
/// by the renderer; the dispatcher translates `(buffer_row,
/// byte_in_row)` to an absolute byte position in the virtual buffer
/// and sets the panel buffer's primary cursor there. When a
/// non-text widget is focused (Toggle / Button / List) or the
/// panel has no tabbable widgets, this is `None` and the host
/// hides the cursor entirely.
#[derive(Debug, Clone, Copy)]
pub struct FocusCursor {
    pub buffer_row: u32,
    pub byte_in_row: u32,
}

/// What a single render of a `WidgetSpec` produces.
///
/// * `entries` — the bytes for `set_virtual_buffer_content`.
/// * `hits` — click rectangles for the `WidgetRegistry` so a later
///   `mouse_click` dispatches a semantic `widget_event`.
/// * `instance_states` — next-tick widget instance state (List
///   scroll offsets / selection, TextInput value+cursor, …).
/// * `focus_key` — currently focused widget key, clamped to a
///   tabbable that exists in the spec (or `""` when there are no
///   tabbables).
/// * `tabbable` — focusable widget keys collected in declaration
///   order. The Tab-cycle command finds the current `focus_key`'s
///   index in this list to advance it.
/// * `focus_cursor` — when a `TextInput` is focused, where the
///   terminal cursor should land. Replaces the previous
///   "overlay-as-cursor" hack — the actual hardware cursor blinks
///   at the right byte, with no theme-color guesswork.
pub struct RenderOutput {
    pub entries: Vec<TextPropertyEntry>,
    pub hits: Vec<HitArea>,
    pub instance_states: HashMap<String, WidgetInstanceState>,
    pub focus_key: String,
    pub tabbable: Vec<String>,
    pub focus_cursor: Option<FocusCursor>,
    /// Rectangles reserved by `WindowEmbed` widgets. Each entry
    /// names a window id and the cell range (relative to the
    /// rendered panel's inner area) the host should paint that
    /// window into after laying down the regular entries.
    pub embeds: Vec<EmbedRect>,
    /// Rows produced by `WidgetSpec::Overlay` children. Each
    /// row carries its anchor `buffer_row` (relative to the
    /// rendered panel's inner area) and is painted by the host
    /// AFTER the main `entries`, on top of whatever is at that
    /// row. Used for dropdown completions, tooltips, hover
    /// popups — anything that should appear next to a focused
    /// widget without reflowing the rest of the layout when it
    /// shows or hides.
    pub overlays: Vec<OverlayRow>,
    /// The open `Dropdown`'s option list, surfaced for a screen-level
    /// floating pop-over instead of inline panel rows. `Some` only when a
    /// keyed Dropdown is open; the panel `entries` then hold just the
    /// compact `[value ▼]` trigger. The host draws this as a bordered box
    /// anchored to the trigger's screen row, clipped to the terminal (not
    /// the panel), so the list extends past the panel/modal frame. Only
    /// one can be open at a time (the focused widget). See [`PanelPopup`].
    pub popup: Option<PanelPopup>,
    /// Effective rows each keyed `List`/`Tree` actually windowed to
    /// this render — spec value, or the auto-size height budget, or
    /// the legacy fallback. Stored on the panel so key/mouse handlers
    /// compute scroll bounds against what was really painted (an
    /// auto-sized widget's spec carries no number at all).
    pub effective_rows: HashMap<String, u32>,
    /// The panel's layout-box tree (root-last arena; see
    /// [`crate::widgets::layout_box`]). One box per widget with its
    /// panel-relative rectangle, stacking level, and dispatch flags —
    /// the geometry substrate hit-tested event routing and the derived
    /// focus ring are built on.
    pub boxes: Vec<LayoutBox>,
}

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

/// One row produced by an `Overlay` widget. `buffer_row` is the
/// 0-based row inside the panel's inner area where the entry
/// should be painted; the host's paint pass writes overlay rows
/// after the main entries so they sit on top.
#[derive(Debug, Clone)]
pub struct OverlayRow {
    pub buffer_row: u32,
    pub entry: TextPropertyEntry,
}

/// A rectangle reserved by a `WindowEmbed` widget. All
/// coordinates are in display **columns** (not bytes), so the
/// host can map straight to screen cells via `inner.x +
/// col_in_row`. `width_cols` is the column count; `height_rows`
/// matches the spec's `rows`. The host's floating-panel render
/// walks these and invokes the per-window paint path scoped to
/// the rect.
#[derive(Debug, Clone, Copy)]
pub struct EmbedRect {
    pub window_id: u32,
    pub buffer_row: u32,
    pub col_in_row: u32,
    pub width_cols: u32,
    pub height_rows: u32,
}

/// Output of a single [`render_collected`] call (or one of the
/// standalone arm helpers). Replaces the six-element tuple that was
/// the previous return type, giving call sites named fields instead
/// of positional slots.
#[derive(Default)]
pub(crate) struct CollectedOutput {
    pub(crate) entries: Vec<TextPropertyEntry>,
    pub(crate) hits: Vec<HitArea>,
    pub(crate) focus_cursor: Option<FocusCursor>,
    pub(crate) embeds: Vec<EmbedRect>,
    pub(crate) overlays: Vec<OverlayRow>,
    /// Scroll payload for THIS node's own box (a keyed List/Tree or
    /// multi-line Text writes it in `collect`); `push_self_box` moves
    /// it onto the box. Never set by containers.
    pub(crate) self_scroll: Option<crate::widgets::layout_box::BoxScroll>,
    /// Open-Dropdown pop-overs, each anchored to its trigger row. Shifted
    /// through Col/Row/Section collapse exactly like `overlays`'
    /// `buffer_row`, then collapsed to `RenderOutput::popup`
    /// (only one Dropdown is open at a time — the focused one).
    pub(crate) popups: Vec<PanelPopup>,
    /// True when a descendant `List`/`Tree` omitted `visible_rows`
    /// (wants auto-sizing) but no height budget reached it. A `Col`
    /// with a real `avail_height` resolves this by re-rendering that
    /// child with the leftover budget; unresolved it bubbles to the
    /// caller (harmless — the widget used the legacy fallback rows).
    pub(crate) wants_fill: bool,
    /// Effective rows each keyed `List`/`Tree` actually windowed to
    /// this render (spec value, height budget, or legacy fallback).
    /// The renderer writes it; host key/mouse handlers read it for
    /// scroll bounds — same contract as instance-state `item_height`.
    pub(crate) effective_rows: HashMap<String, u32>,
    /// The layout-box arena for this subtree (root-last; see
    /// [`crate::widgets::layout_box`]). Containers shift child box
    /// rectangles alongside the other column-addressed side channels
    /// and re-parent subtree roots onto their own box.
    pub(crate) boxes: Vec<LayoutBox>,
}

impl CollectedOutput {
    /// Fold a child subtree's entire output into this accumulator at
    /// the current column cursor: every geometry channel — hits,
    /// focus cursor, embeds, scroll regions, dropdown anchors,
    /// overlays, boxes, entries — shifts down by `row_offset`
    /// together. Containers MUST use this (or the overlay promotion
    /// variant) instead of shifting channels by hand: a container
    /// that shifts two of three column-addressed channels compiles
    /// fine and mis-routes the wheel, which is exactly the drift
    /// class the box tree exists to kill.
    ///
    /// `promote_overlay` = the child is an `Overlay` in a `Col`: its
    /// entries become overlay rows anchored at the cursor (occupying
    /// no column height), its hits are stamped `overlay`, and its
    /// whole box subtree moves up one stacking level.
    /// Translate EVERY geometry channel by one origin shift: `rows`
    /// down, `display_cols` right for the column-addressed channels
    /// (boxes, embeds), `bytes` right for the byte-addressed channels
    /// (hits, the focus cursor). Flow-anchored popups ride the row
    /// shift; absolute anchors name their own panel row and stay put.
    /// ONE method so a container cannot shift some channels and forget
    /// others — the labeled section used to spell this translation six
    /// times, synced only by prose (the byte-vs-column unit split is
    /// exactly where a hand-copied shift drifts).
    pub(crate) fn shift_channels(&mut self, rows: u32, display_cols: u32, bytes: usize) {
        for b in &mut self.boxes {
            b.row += rows;
            b.col += display_cols;
        }
        for o in &mut self.overlays {
            o.buffer_row += rows;
        }
        for dp in &mut self.popups {
            if !dp.anchor_absolute {
                dp.anchor_row += rows;
            }
        }
        for h in &mut self.hits {
            h.buffer_row += rows;
            h.byte_start += bytes;
            h.byte_end += bytes;
        }
        if let Some(fc) = &mut self.focus_cursor {
            fc.buffer_row += rows;
            fc.byte_in_row += bytes as u32;
        }
        for emb in &mut self.embeds {
            emb.buffer_row += rows;
            emb.col_in_row += display_cols;
        }
    }

    pub(crate) fn absorb_child(
        &mut self,
        mut child: CollectedOutput,
        row_offset: u32,
        promote_overlay: bool,
    ) {
        self.wants_fill |= child.wants_fill;
        self.effective_rows
            .extend(std::mem::take(&mut child.effective_rows));
        let base = self.boxes.len();
        for mut b in child.boxes {
            b.parent = b.parent.map(|pi| pi + base);
            b.row += row_offset;
            if promote_overlay {
                b.z = b.z.saturating_add(1);
            }
            self.boxes.push(b);
        }
        if let Some(mut fc) = child.focus_cursor {
            fc.buffer_row += row_offset;
            self.focus_cursor = Some(fc);
        }
        for mut emb in child.embeds {
            emb.buffer_row += row_offset;
            self.embeds.push(emb);
        }
        for mut dp in child.popups {
            if !dp.anchor_absolute {
                dp.anchor_row += row_offset;
            }
            self.popups.push(dp);
        }
        if promote_overlay {
            for (i, e) in child.entries.into_iter().enumerate() {
                self.overlays.push(OverlayRow {
                    buffer_row: row_offset + i as u32,
                    entry: e,
                });
            }
            for mut h in child.hits {
                h.buffer_row += row_offset;
                // Byte ranges are measured against the overlay's row
                // text — the covered row's text is invisible and must
                // not resolve clicks.
                h.overlay = true;
                self.hits.push(h);
            }
            // Nested overlays are already anchored.
            self.overlays.extend(child.overlays);
        } else {
            for mut h in child.hits {
                h.buffer_row += row_offset;
                self.hits.push(h);
            }
            self.overlays
                .extend(child.overlays.into_iter().map(|mut o| {
                    o.buffer_row += row_offset;
                    o
                }));
            self.entries.extend(child.entries);
        }
    }

    /// Append this subtree's own root box: rectangle covering the rows
    /// the subtree emitted at full `panel_width`, every parentless box
    /// so far re-parented onto it. Leaf kinds call this on an output
    /// with no boxes; containers call it after merging children.
    pub(crate) fn push_self_box(&mut self, mut own: LayoutBox, panel_width: u32) {
        own.width = panel_width;
        own.height = self.entries.len() as u32;
        own.scroll = self.self_scroll.take();
        let idx = self.boxes.len();
        for b in &mut self.boxes {
            if b.parent.is_none() {
                b.parent = Some(idx);
            }
        }
        self.boxes.push(own);
    }
}

/// Everything a render pass needs that isn't in the spec itself.
///
/// Focus, hover, and the marker gutter are all *host* state — they
/// change without the plugin re-sending its spec — so they travel
/// beside the spec rather than in it. Bundling them into one `Copy`
/// context is what keeps the ~18 recursive `collect_*` signatures from
/// growing a parameter every time the host learns to track something
/// new; `marker_gutter` used to ride a thread-local for exactly that
/// reason, and hover would have been the second.
/// Host resources a `markdown: true` Text widget renders through: the
/// live theme (heading / code / link colours) and, when available, the
/// grammar registry for syntax-highlighted fences. Carried by reference
/// beside the spec — theme state is host state, not spec state. `None`
/// grammar falls back to uniform code styling, exactly like hover docs.
#[derive(Clone, Copy)]
pub struct MarkdownCtx<'a> {
    pub theme: &'a crate::view::theme::Theme,
    pub grammars: Option<&'a crate::primitives::grammar::GrammarRegistry>,
}

impl std::fmt::Debug for MarkdownCtx<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MarkdownCtx").finish_non_exhaustive()
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct RenderContext<'a> {
    /// Widget key that owns the keyboard, or `""` for none. Resolved
    /// from the caller's `prev_focus_key` against the new spec's
    /// tabbables before the entry pass begins.
    pub focus_key: &'a str,
    /// Widget key the pointer is over, or `""` for none.
    pub hover_key: &'a str,
    /// Item key of the `List` / `Tree` row the pointer is over, or `""`.
    /// Every row of one list shares that list's `hover_key`, so the row
    /// identity has to travel separately for a per-row highlight.
    pub hover_item_key: &'a str,
    /// The open dropdown pop-over's hovered option, as a decimal index, or
    /// `""`. Separate from `hover_item_key` because a pop-over's rows are not
    /// panel rows: the runtime's own hover probe walks the panel's entries and
    /// a pop-over floats beside them, so this arrives from the tree, which
    /// lays the rows out and knows which one the pointer is on.
    pub hover_popup_row: &'a str,
    /// Theme + grammars for `markdown: true` Text widgets. `None`
    /// (tests, callers without a theme in hand) renders the markdown
    /// source as plain unstyled lines — layout identical, colours
    /// absent.
    pub markdown: Option<MarkdownCtx<'a>>,
    /// Reserve a leading two-column gutter on every focusable control
    /// for the `▸ ` focus marker: the focused control leads with `▸ `
    /// and every other focusable control with two spaces, so focus is
    /// legible from a plain terminal capture AND the layout never
    /// shifts as focus moves. Off for panels that predate it, which
    /// then render byte-for-byte as before.
    pub marker_gutter: bool,
    /// Row budget for auto-sized (`visible_rows: None`) `List`/`Tree`
    /// widgets, threaded down like `panel_width`. At the panel root
    /// this is the surface's inner height (when the host knows it);
    /// `Col` resolves it to "height minus the rows every sibling
    /// occupies" before handing it to the one auto child (see
    /// `collect_col`'s fill pass). `None` = no budget: auto widgets
    /// fall back to the legacy default and report `wants_fill`.
    pub avail_height: Option<u32>,
}

impl RenderContext<'_> {
    /// Whether `key` names the focused widget. Empty keys never match.
    pub(crate) fn is_focused(&self, key: Option<&str>) -> bool {
        matches!(key, Some(k) if !k.is_empty() && k == self.focus_key)
    }

    /// Whether `key` names the widget under the pointer. Empty keys
    /// never match, so an unkeyed widget can't be "hovered" by an empty
    /// hover key.
    pub(crate) fn is_hovered(&self, key: Option<&str>) -> bool {
        matches!(key, Some(k) if !k.is_empty() && k == self.hover_key)
    }

    /// Whether `item_key` names the list/tree row under the pointer, given
    /// that `key` names the list itself. Both halves must match: an empty
    /// item key (a row the plugin didn't key) never lights up, and a row
    /// key that collides across two lists only counts inside the hovered
    /// one.
    pub(crate) fn is_row_hovered(&self, key: Option<&str>, item_key: &str) -> bool {
        !item_key.is_empty() && item_key == self.hover_item_key && self.is_hovered(key)
    }
}

/// What the host asks of one render, beyond the spec and its previous
/// instance state. Separate from [`RenderContext`] because these are the
/// caller's *inputs* — `prev_focus_key` is a request that the resolved
/// context may override when it names a widget the new spec dropped.
#[derive(Debug, Clone, Copy, Default)]
pub struct RenderOptions<'a> {
    /// Previous render's focus key (or `""`). Kept if it still matches a
    /// tabbable in the new spec; otherwise see `auto_focus_first`.
    pub prev_focus_key: &'a str,
    /// Widget key the pointer is over (or `""`).
    pub hover_key: &'a str,
    /// See [`RenderContext::hover_item_key`].
    pub hover_item_key: &'a str,
    /// See [`RenderContext::hover_popup_row`].
    pub hover_popup_row: &'a str,
    /// See [`RenderContext::marker_gutter`].
    pub marker_gutter: bool,
    /// Fall back to the first tabbable when `prev_focus_key` matches
    /// none. Hosts that own their own focus ring — and for which "no
    /// widget focused" is a real state, e.g. the search overlay, where
    /// focus can rest on the input with no toolbar control highlighted —
    /// set this `false`.
    pub auto_focus_first: bool,
    /// See [`RenderContext::markdown`].
    pub markdown: Option<MarkdownCtx<'a>>,
    /// See [`RenderContext::avail_height`] — the surface's inner
    /// height in rows, when the host knows it. `None` keeps auto-sized
    /// `List`/`Tree` widgets on the legacy fallback.
    pub avail_height: Option<u32>,
}

/// Render a spec to a [`RenderOutput`] under explicit [`RenderOptions`].
///
/// The `render_spec*` helpers below are the common presets over this;
/// call this directly when you need to combine options they don't
/// (notably a hover key, which only the live host tracks).
///
/// `panel_width` is the buffer's column width — used by `Row` to size
/// flex `Spacer`s. Pass `u32::MAX` to disable flex (children won't be
/// padded).
pub fn render_spec_with_options(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    panel_width: u32,
    opts: RenderOptions<'_>,
) -> RenderOutput {
    // Walk the spec to collect tabbable keys, then resolve the
    // active focus key. This must happen before the entry pass so
    // that widget arms know whether they're focused.
    let mut tabbable = Vec::new();
    collect_tabbable(spec, &mut tabbable);
    let focus_key =
        if !opts.prev_focus_key.is_empty() && tabbable.iter().any(|k| k == opts.prev_focus_key) {
            opts.prev_focus_key.to_string()
        } else if opts.auto_focus_first {
            tabbable.first().cloned().unwrap_or_default()
        } else {
            String::new()
        };

    let ctx = RenderContext {
        focus_key: &focus_key,
        hover_key: opts.hover_key,
        hover_item_key: opts.hover_item_key,
        hover_popup_row: "",
        markdown: opts.markdown,
        marker_gutter: opts.marker_gutter,
        avail_height: opts.avail_height,
    };
    let mut next_state = HashMap::new();
    let collected = render_collected(spec, prev, &mut next_state, ctx, panel_width);
    // The box tree is the focus authority: publish the ring derived
    // from it (focusable boxes in document order). The spec-walk ring
    // computed above exists because focus must resolve *before*
    // collection (widgets style by focus); both rings now ask the same
    // `box_meta` impls, so they cannot diverge on rules — this assert
    // guards arena construction (container merge order) until the
    // pre-pass ring is retired with the constraint-layout phase.
    let derived_tabbable = crate::widgets::layout_box::focus_ring(&collected.boxes);
    debug_assert_eq!(
        derived_tabbable, tabbable,
        "box-tree focus ring diverged from collect_tabbable"
    );
    RenderOutput {
        entries: collected.entries,
        hits: collected.hits,
        instance_states: next_state,
        focus_key,
        tabbable: derived_tabbable,
        focus_cursor: collected.focus_cursor,
        embeds: collected.embeds,
        overlays: collected.overlays,
        // At most one Dropdown is open at a time (the focused one); take
        // the first if the spec somehow produced several.
        popup: collected.popups.into_iter().next(),
        effective_rows: collected.effective_rows,
        boxes: collected.boxes,
    }
}

/// Render a spec with the default options: keyboard focus only, no
/// hover, no marker gutter, auto-focusing the first tabbable when
/// `prev_focus_key` matches nothing.
pub fn render_spec(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    render_spec_with_options(
        spec,
        prev,
        panel_width,
        RenderOptions {
            prev_focus_key,
            auto_focus_first: true,
            ..Default::default()
        },
    )
}

/// Like [`render_spec`], but reserves the `▸ ` focus-marker gutter on
/// every focusable control (see [`RenderContext::marker_gutter`]).
/// Panels that want capture-legible, layout-stable focus (the
/// Orchestrator New Session form) render through this entry point;
/// everything else uses [`render_spec`] and is unaffected.
pub fn render_spec_with_marker(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    render_spec_with_options(
        spec,
        prev,
        panel_width,
        RenderOptions {
            prev_focus_key,
            marker_gutter: true,
            auto_focus_first: true,
            ..Default::default()
        },
    )
}

/// Like [`render_spec`] but does **not** fall back to focusing the first
/// tabbable widget when `focus_key` matches none. See
/// [`RenderOptions::auto_focus_first`]. Pass `""` for no focus.
pub fn render_spec_no_autofocus(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    render_spec_with_options(
        spec,
        prev,
        panel_width,
        RenderOptions {
            prev_focus_key: focus_key,
            ..Default::default()
        },
    )
}

/// Strip a trailing `'\n'` from `entry.text` if present (overlays /
/// hits aren't affected because the newline is at the very end and
/// no overlay should span it). Used to prepare an inline-rendered
/// child for Row inline-collapse, where individual newlines would
/// split the merged row across multiple buffer lines.
pub(crate) fn strip_trailing_newline(entry: &mut TextPropertyEntry) {
    if entry.text.ends_with('\n') {
        entry.text.pop();
    }
}

/// Append a single trailing newline to `entry.text` if it doesn't
/// already end with one. Each top-level entry needs to end with
/// `\n` so it occupies its own line in the underlying virtual
/// buffer (the buffer's line model is byte-driven; without `\n`
/// adjacent entries concatenate into one logical line).
pub(crate) fn ensure_trailing_newline(entry: &mut TextPropertyEntry) {
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
    // authority (it also builds the layout-box tree the published ring
    // derives from). This walk exists only because focus must resolve
    // *before* collection builds the tree; it asks the same impls the
    // tree does, so the two rings cannot diverge on rules — only an
    // arena-construction bug could split them, which the debug assert
    // in `render_spec_with_options` still guards.
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

/// Internal renderer. Returns the entries and the hit areas
/// produced by `spec` *as if* it were rendered at row 0; callers
/// (Col, Row block path) shift `buffer_row` upward by their own
/// row offset before forwarding. `prev` is read-only previous
/// instance state; `next_state` accumulates the post-render state
/// the host should persist. `focus_key` is the panel's currently
/// focused widget key — widget arms compare against their own
/// `key` to decide whether to render with focus styling, ignoring
/// the spec's `focused` field. (Plugin-passed `focused` is the
/// initial-only hint that becomes redundant once the host's focus
/// key takes over.)
pub(crate) fn render_collected(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    ctx: RenderContext<'_>,
    panel_width: u32,
) -> CollectedOutput {
    // Every kind's behaviour lives in `widgets::kinds` behind the
    // `WidgetImpl` trait (widget-framework-v2-review.md §4.3); the single
    // kind-dispatch is `kinds::behavior`.
    let behavior = super::kinds::behavior(spec);
    let mut out = behavior.collect(spec, prev, next_state, ctx, panel_width);
    // Cap the subtree with its own layout box (rectangle = the rows it
    // just emitted at this width), re-parenting child-subtree roots.
    // Done here, once, so `collect` impls never see box bookkeeping
    // beyond the container merge helpers.
    let meta = behavior.box_meta(spec);
    let mut own = LayoutBox::plain(meta.kind, 0, 0, 0, 0);
    own.key = meta.key;
    own.focusable = meta.focusable;
    own.scrollable = meta.scrollable;
    own.pointer_opaque = meta.pointer_opaque;
    own.focus_trap = meta.focus_trap;
    out.push_self_box(own, panel_width);
    out
}

// =========================================================================
// Standalone arm helpers — extracted from the render_collected match to keep
// that function navigable. Each returns a CollectedOutput the caller folds
// back into its local accumulators.
// =========================================================================

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
        FRAMED_BUTTON_CHROME_COLS + if marker_gutter { FOCUS_GUTTER_COLS } else { 0 }
    };
    let target = (panel_width as usize).saturating_sub(chrome).max(1);
    let mut filled = label.to_string();
    pad_or_truncate_cols(&mut filled, target);
    filled
}

/// Blank full-height-padding row used to pad a List to its
/// advertised height. Padding rows aren't clickable.
pub(crate) fn blank_list_row() -> TextPropertyEntry {
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

/// Style a classic (one-row-per-item) list row as the selected
/// item: a highlight band that runs to line end behind the text.
pub(crate) fn mark_list_row_selected(entry: &mut TextPropertyEntry) {
    let mut style = entry.style.clone().unwrap_or_default();
    style.bg = Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG));
    style.extend_to_line_end = true;
    entry.style = Some(style);
}

/// Style one row of a selected *card* so selection reads in any
/// theme — even when colours are too subtle: a *heavy* box border
/// (colour-independent marker), bold, and an accent fg on the
/// pure-border rows. No background band — it reads garish over a
/// multi-row card and fights theme colours. Every box glyph is 3
/// bytes in both light and heavy forms, so swapping them preserves
/// inline-overlay byte offsets.
pub(crate) fn mark_list_card_selected(entry: &mut TextPropertyEntry) {
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
pub(crate) fn ratatui_style_to_overlay(style: ratatui::style::Style) -> Option<OverlayOptions> {
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

pub(crate) const LEFT_BORDER_PREFIX: &str = "│ ";
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
pub(crate) fn render_section_top_border(label: &str, total_cols: usize) -> TextPropertyEntry {
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

/// Build the bottom border row: `╰──...──╯` spanning `total_cols`
/// display columns.
pub(crate) fn render_section_bottom_border(total_cols: usize) -> TextPropertyEntry {
    let mut text = String::new();
    text.push('╰');
    for _ in 0..total_cols.saturating_sub(2) {
        text.push('─');
    }
    text.push('╯');
    text.push('\n');
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

/// Dim-separator overlay row for the completion popup. Unlike
/// `render_completion_dim_separator` (which targets a child of
/// a `LabeledSection` and lets the section wrap the row with
/// `│ ... │`), this one paints into the FULL panel width
/// directly and supplies its own `│ ... │` chrome — overlay
/// rows skip the wrapping section's per-row wrap and land on
/// the parent col's row directly. `total_cols` is the section's
/// outer width.
pub(crate) fn render_completion_dim_separator_overlay(total_cols: usize) -> TextPropertyEntry {
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
pub(crate) fn render_completion_bottom_border(total_cols: usize) -> TextPropertyEntry {
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
pub(crate) fn render_completion_item_overlay(
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
pub(crate) fn completion_scrollbar_glyph(
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
pub(crate) fn wrap_in_side_border(
    child: TextPropertyEntry,
    inner_width: usize,
) -> TextPropertyEntry {
    wrap_entry_between(child, inner_width, LEFT_BORDER_PREFIX, RIGHT_BORDER_SUFFIX)
}

/// Pad/truncate `child` to `inner_width` display columns and sandwich it
/// between `prefix` and `suffix` (side-border chrome), shifting the
/// child's overlays past the prefix. `LabeledSection` uses the padded
/// `"│ "` chrome; the tree's bordered cards use flush `"│"` borders to
/// keep two more content columns on a narrow dock.
pub(crate) fn wrap_entry_between(
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
pub(crate) fn form_label_width(
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
pub(crate) fn fit_label(label: &str, width: usize) -> String {
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
pub(crate) fn cell(s: &str, width: usize) -> String {
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
pub(crate) fn dual_col_width(panel_width: u32) -> usize {
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
pub(crate) const DUAL_GUTTER_W: usize = 2;
/// Cursor marker for the column the keyboard is currently driving.
/// Filled triangle, matching [`FOCUS_MARKER`].
pub(crate) const DUAL_CURSOR_ACTIVE: &str = "▸ ";
/// Cursor marker for the *other* column — where the cursor will land
/// if the user switches columns. Hollow so the two are distinguishable
/// in a monochrome capture, not only by color.
pub(crate) const DUAL_CURSOR_IDLE: &str = "▹ ";
/// Marker under the active column's header, pointing down into it.
pub(crate) const DUAL_COLUMN_ACTIVE: &str = "▾ ";
/// Blank gutter — the same width as the markers, so rows and headers
/// never reflow as the cursor or the active column moves.
pub(crate) const DUAL_GUTTER_BLANK: &str = "  ";

/// The two-column gutter a `DualList` cell leads with: `▸ ` when the
/// cursor is on this cell and its column is the active one, `▹ ` when
/// the cursor is parked here in the idle column, two spaces otherwise.
pub(crate) fn dual_cursor_marker(on_cursor: bool, column_active: bool) -> &'static str {
    match (on_cursor, column_active) {
        (true, true) => DUAL_CURSOR_ACTIVE,
        (true, false) => DUAL_CURSOR_IDLE,
        _ => DUAL_GUTTER_BLANK,
    }
}

/// Render a `Button` to a single `TextPropertyEntry`.
///
/// Layout: `[ Label ]` (with explicit space padding so the label
/// is visually inset from the brackets), or the bare label when
/// `bare` — see [`render_bare_button`]. Styling depends on `kind`
/// and `focused`:
///
/// * `Normal`  — default fg; focused → fg/bg flip + bold.
/// * `Primary` — bold; focused → fg/bg flip.
/// * `Danger`  — red fg (theme `ui.status_error_indicator_fg`);
///   focused → bold.
///
/// `hovered` is whether the pointer is on this button. With no explicit
/// `hover` style it paints the shared [`KEY_HOVER_BG`] band under the
/// button's own colours — every framed button answers the pointer, with
/// no per-call-site opt-in. An explicit `hover` still wins outright.
pub fn render_button(
    label: &str,
    focused: bool,
    kind: ButtonKind,
    disabled: bool,
    marker_gutter: bool,
    hover: Option<&OverlayOptions>,
    hovered: bool,
) -> TextPropertyEntry {
    // In a marker-gutter panel, focused buttons lead with `▸ ` and
    // every other button with two spaces. This is the cue that
    // distinguishes "focused" from "Primary": a Primary button keeps
    // its standing bold accent whether or not it's focused, so
    // without the marker (and the focused bg flip) `[ Create Session ]`
    // looked permanently selected. The marker rides only on the one
    // focused control, so exactly one button reads as focused — and
    // because the gutter is always reserved, the row never reflows as
    // focus moves between buttons.
    let marker = focus_gutter_prefix(focused && !disabled, marker_gutter);
    let text = format!("{}[ {} ]", marker, label);
    let mut overlays = Vec::new();

    // Disabled overrides intent: a "Delete" button that isn't
    // available should not still scream red — the muted-grey of
    // `ui.menu_disabled_fg` is the canonical "this control is
    // present but inert" cue across the editor. Focus is also
    // forced off (the caller already gates focus on `!disabled`,
    // but bake it in here so a stale `focused: true` from the spec
    // can't paint the focused bg over a disabled button).
    let base_style = if disabled {
        OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key("ui.menu_disabled_fg")),
            ..Default::default()
        }
    } else {
        match kind {
            ButtonKind::Normal => OverlayOptions::default(),
            // Primary marks the affirmative action with a bold,
            // strong fg drawn directly on the surrounding surface —
            // no opinionated bg. Focus is the only state that paints
            // a backing color (handled below).
            ButtonKind::Primary => OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
                bold: true,
                ..Default::default()
            },
            // Danger gets the error fg, bold, on the surrounding
            // surface — same fg-only treatment as Primary.
            ButtonKind::Danger => OverlayOptions {
                fg: Some(OverlayColorSpec::theme_key(KEY_DANGER_FG)),
                bold: true,
                ..Default::default()
            },
        }
    };

    // Hover outranks focus: the pointer is the more immediate signal.
    // A spec-declared `hover_style` replaces the look outright; without
    // one, hover paints the shared band *under* the button's own intent
    // colours, so a Danger button stays red while answering the pointer.
    let style = if let Some(hover) = hover.filter(|_| !disabled) {
        hover.clone()
    } else if hovered && !disabled {
        OverlayOptions {
            bg: Some(OverlayColorSpec::theme_key(KEY_HOVER_BG)),
            ..base_style
        }
    } else if focused && !disabled {
        OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
            bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
            bold: true,
            ..base_style
        }
    } else {
        base_style
    };

    // Only emit an overlay if the style is non-default — keeps the
    // serialized entry tight.
    if style.fg.is_some()
        || style.bg.is_some()
        || style.bold
        || style.italic
        || style.underline
        || style.strikethrough
    {
        overlays.push(InlineOverlay {
            start: 0,
            end: text.len(),
            style,
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

/// Render a `bare` Button — an icon affordance such as a `×` close
/// glyph — as its label and nothing else: no `[ ]` frame and no
/// focus-marker gutter, because both exist to give a *word* the shape
/// of a control and a glyph already has one.
///
/// `hover` is the spec's `hover_style`, passed in only while the pointer
/// is on this button. It outranks focus styling: the pointer is the more
/// immediate signal, and the one the user is actively driving. `hovered`
/// without a declared style falls back to the shared hover band, so even
/// a glyph affordance lights up under the pointer.
pub fn render_bare_button(
    label: &str,
    focused: bool,
    kind: ButtonKind,
    disabled: bool,
    hover: Option<&OverlayOptions>,
    hovered: bool,
) -> TextPropertyEntry {
    let base = match kind {
        ButtonKind::Normal => OverlayOptions::default(),
        ButtonKind::Primary => OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_HELP_KEY_FG)),
            bold: true,
            ..Default::default()
        },
        ButtonKind::Danger => OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_DANGER_FG)),
            bold: true,
            ..Default::default()
        },
    };
    let style = if disabled {
        OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key("ui.menu_disabled_fg")),
            ..Default::default()
        }
    } else if let Some(hover) = hover {
        hover.clone()
    } else if hovered {
        OverlayOptions {
            bg: Some(OverlayColorSpec::theme_key(KEY_HOVER_BG)),
            ..base
        }
    } else if focused {
        OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_FG)),
            bg: Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG)),
            bold: true,
            ..Default::default()
        }
    } else {
        base
    };

    let mut overlays = Vec::new();
    if style_paints_anything(&style) {
        overlays.push(InlineOverlay {
            start: 0,
            end: label.len(),
            style,
            properties: Default::default(),
            unit: OffsetUnit::Byte,
        });
    }

    TextPropertyEntry {
        text: label.to_string(),
        properties: Default::default(),
        style: None,
        inline_overlays: overlays,
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    }
}

/// Whether an `OverlayOptions` would change any cell it covers. Used to
/// keep a serialized entry tight by skipping a no-op overlay.
fn style_paints_anything(style: &OverlayOptions) -> bool {
    style.fg.is_some()
        || style.bg.is_some()
        || style.bold
        || style.italic
        || style.underline
        || style.strikethrough
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
pub fn render_tree_row(
    node: &TreeNode,
    expanded: bool,
    checkable: bool,
    item_height: u32,
    card_borders: bool,
    panel_width: u32,
    indent_cols: u32,
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
    text.push_str(&node.text.text);

    // Carry over the plugin's inline overlays, shifted right by
    // `body_start` so they land on the correct bytes after the
    // prefix.
    let mut overlays: Vec<InlineOverlay> = node
        .text
        .inline_overlays
        .iter()
        .map(|o| {
            let mut shifted = o.clone();
            shifted.start += body_start;
            shifted.end += body_start;
            shifted
        })
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
                    let mut line_text = String::with_capacity(shift + src.text.len());
                    line_text.push_str(&indent_str);
                    line_text.push_str(&src.text);
                    let shifted: Vec<InlineOverlay> = src
                        .inline_overlays
                        .iter()
                        .map(|o| {
                            let mut s = o.clone();
                            s.start += shift;
                            s.end += shift;
                            s
                        })
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
pub(crate) fn tree_node_is_card(node: &TreeNode, checkable: bool) -> bool {
    !node.extra_lines.is_empty() && !node.has_children && (!checkable || node.checked.is_none())
}

/// Screen rows one node occupies. Fixed `item_height` bands normally;
/// with `card_borders`, card nodes gain a top + bottom border row and
/// non-card nodes collapse to a single row.
pub(crate) fn tree_node_rows(
    node: &TreeNode,
    checkable: bool,
    item_height: u32,
    card_borders: bool,
) -> u32 {
    if item_height <= 1 {
        return 1;
    }
    if !card_borders {
        return item_height;
    }
    if tree_node_is_card(node, checkable) {
        item_height + 2
    } else {
        1
    }
}

/// Largest useful *row* scroll for a tree whose visible nodes occupy
/// `heights` rows each: the offset at which the last viewport-full of
/// rows sits flush with the bottom (`0` when everything fits). Shared
/// by the renderer and the mouse-wheel handler so the wheel's clamp
/// can't disagree with what the renderer will actually show.
pub(crate) fn tree_max_scroll(heights: &[u32], visible_rows: u32) -> u32 {
    heights.iter().sum::<u32>().saturating_sub(visible_rows)
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

    if focused {
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
    // Resolve effective field width: caller's value if set, else
    // `panel_width` (or a small default if the panel is unsized).
    let target_width: usize = if field_width > 0 {
        field_width as usize
    } else if panel_width != u32::MAX && panel_width > 0 {
        panel_width as usize
    } else {
        40
    };

    // Split value into lines (without the `\n`). Empty value still
    // produces one (empty) line — matching how a single-line
    // editor would treat an empty buffer.
    let mut lines: Vec<&str> = value.split('\n').collect();
    if lines.is_empty() {
        lines.push("");
    }

    // Cursor → (line_index, byte_in_line). When `cursor_byte` is
    // negative (no cursor), we still compute a line for scroll
    // bookkeeping but don't emit a focus_cursor.
    let raw_cursor_byte = if cursor_byte < 0 {
        value.len()
    } else {
        (cursor_byte as usize).min(value.len())
    };
    let (cursor_line, cursor_col) = byte_to_line_col(value, raw_cursor_byte);

    // Selection decomposed onto (line_start, byte_in_line) →
    // (line_end, byte_in_line) so each visible row can emit its own
    // background overlay. Only meaningful when focused; we trust the
    // caller to pass `None` for unfocused renders.
    let selection_lc: Option<((usize, usize), (usize, usize))> = selection.and_then(|(a, b)| {
        let lo = a.min(b);
        let hi = a.max(b);
        if hi <= lo || hi > value.len() {
            return None;
        }
        Some((byte_to_line_col(value, lo), byte_to_line_col(value, hi)))
    });

    // Auto-clamp scroll: keep cursor's line in [scroll_row,
    // scroll_row + visible_rows). On first render, prev_scroll == 0.
    let visible_rows_usize = visible_rows.max(1) as usize;
    let mut scroll_row = prev_scroll as usize;
    if cursor_line < scroll_row {
        scroll_row = cursor_line;
    } else if cursor_line >= scroll_row + visible_rows_usize {
        scroll_row = cursor_line + 1 - visible_rows_usize;
    }
    // Don't scroll past the last line.
    let max_scroll = lines.len().saturating_sub(visible_rows_usize);
    if scroll_row > max_scroll {
        scroll_row = max_scroll;
    }

    let show_placeholder =
        !focused && value.is_empty() && placeholder.is_some() && !placeholder.unwrap().is_empty();

    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut cursor_buffer_row: Option<u32> = None;
    let mut cursor_byte_in_row: Option<usize> = None;

    if !label.is_empty() {
        let mut text = String::with_capacity(label.len() + 2);
        text.push_str(label);
        text.push(':');
        entries.push(TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: Vec::new(),
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
    }
    let label_offset: u32 = entries.len() as u32;

    for row_in_view in 0..visible_rows_usize {
        let line_idx = scroll_row + row_in_view;
        let mut row_text;
        let mut overlays: Vec<InlineOverlay> = Vec::new();

        if line_idx < lines.len() {
            row_text = pad_or_truncate_line(lines[line_idx], target_width);
        } else {
            row_text = " ".repeat(target_width);
        }

        // Placeholder shows on the first row only.
        if show_placeholder && row_in_view == 0 {
            let ph = placeholder.unwrap();
            row_text = pad_or_truncate_line(ph, target_width);
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

        // Focused-bg covers the full row width — the editing
        // region reads as a single block.
        if focused {
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
        // length. Rows are padded out to `target_width`; selection
        // never paints into the trailing pad area.
        if focused {
            if let Some(((sl, sc), (el, ec))) = selection_lc {
                if line_idx >= sl && line_idx <= el {
                    let line_text_len = if line_idx < lines.len() {
                        lines[line_idx].len()
                    } else {
                        0
                    };
                    let row_start = if line_idx == sl { sc } else { 0 };
                    let row_end = if line_idx == el { ec } else { line_text_len };
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

        // Drop the cursor on this row if it matches.
        if focused && line_idx == cursor_line && cursor_byte >= 0 {
            // The cursor's byte column on its line. If the line was
            // truncated, the cursor may have shifted past the
            // visible region — clamp to the last visible byte so
            // the hardware cursor stays in the row.
            let col_in_line = cursor_col.min(row_text.len());
            cursor_buffer_row = Some(label_offset + row_in_view as u32);
            cursor_byte_in_row = Some(col_in_line);
        }

        entries.push(TextPropertyEntry {
            text: row_text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
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
pub(crate) fn pad_or_truncate_cols(text: &mut String, cols: usize) {
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

/// Clamp `idx` to `s.len()`, then walk it down to the nearest
/// char boundary. Byte-unit inline overlays computed against a
/// pre-truncation line must pass through this after the line is
/// column-truncated, so they can never index inside a multi-byte
/// char (the panic the span splitter raises on `text[a..b]`).
pub(crate) fn snap_down_to_char_boundary(s: &str, idx: usize) -> usize {
    let mut i = idx.min(s.len());
    while i > 0 && !s.is_char_boundary(i) {
        i -= 1;
    }
    i
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use serde_json::json;

    /// Most existing tests don't care about the new focus_key /
    /// tabbable fields. Wrap the no-focus-needed render path so
    /// they keep destructuring a 3-tuple; new tests destructure
    /// `RenderOutput` directly.
    fn render_no_focus(
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
    ) -> (
        Vec<TextPropertyEntry>,
        Vec<HitArea>,
        HashMap<String, WidgetInstanceState>,
    ) {
        // u32::MAX disables flex sizing (no leftover to distribute).
        let out = render_spec(spec, prev, "", u32::MAX);
        (out.entries, out.hits, out.instance_states)
    }

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
    fn text_field_label_width_aligns_value_cell() {
        // A `label_width`-set single-line Text pads the label to the
        // column and terminates it with `: ` so its `[` aligns with the
        // sibling toggles' chips. Regression: the entry-dialog Grammar
        // field opened `Grammar [value]` instead of the aligned column.
        let spec = WidgetSpec::Text {
            value: "PowerShell".into(),
            cursor_byte: -1,
            focused: false,
            label: "Grammar".into(),
            placeholder: None,
            rows: 1,
            field_width: 0,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 18,
            read_only: false,
            markdown: false,
            key: None,
        };
        let (entries, _, _) = render_no_focus(&spec, &HashMap::new());
        let text = entries[0].text.trim_end_matches('\n');
        use crate::primitives::display_width::str_width;
        // The label is padded to the 18-col column, then `: [` opens the
        // value cell — so everything up to `[` is exactly the marker +
        // 18 + ": ".
        let bracket = text.find('[').expect("value cell bracket");
        let prefix = &text[..bracket];
        assert!(
            prefix.starts_with("Grammar") && prefix.trim_end().ends_with(':'),
            "padded label then colon: {prefix:?}"
        );
        assert_eq!(
            str_width(prefix),
            str_width(focus_gutter_prefix(false, false)) + 18 + ": ".len(),
            "value cell opens at the aligned column: {text:?}"
        );
    }

    #[test]
    fn text_field_no_label_width_is_compact() {
        // label_width == 0 keeps the plugin-default compact form.
        let spec = WidgetSpec::Text {
            value: "x".into(),
            cursor_byte: -1,
            focused: false,
            label: "Name".into(),
            placeholder: None,
            rows: 1,
            field_width: 0,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: false,
            key: None,
        };
        let (entries, _, _) = render_no_focus(&spec, &HashMap::new());
        let text = entries[0].text.trim_end_matches('\n');
        assert!(
            text.contains("Name [") && !text.contains("Name :"),
            "compact form keeps `label [value]`: {text:?}"
        );
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
    fn col_stacks_children_top_to_bottom() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "A".into(),
                        label: "alpha".into(),
                    }],
                    key: None,
                },
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "B".into(),
                        label: "beta".into(),
                    }],
                    key: None,
                },
            ],
            key: None,
        };
        let (out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 2);
        assert_eq!(out[0].text, "A alpha\n");
        assert_eq!(out[1].text, "B beta\n");
        assert!(hits.is_empty(), "HintBar emits no hit areas in v1");
    }

    #[test]
    fn raw_passes_through_unchanged() {
        let spec = WidgetSpec::Raw {
            entries: vec![TextPropertyEntry::text("hello")],
            key: None,
        };
        let (out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text, "hello\n");
        assert!(hits.is_empty());
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

    #[test]
    fn button_normal_unfocused_has_no_overlay() {
        let entry = render_button(
            "Replace All",
            false,
            ButtonKind::Normal,
            false,
            false,
            None,
            false,
        );
        assert_eq!(entry.text, "[ Replace All ]");
        assert!(entry.inline_overlays.is_empty());
    }

    #[test]
    fn button_primary_unfocused_is_bold_help_key_fg_with_no_bg() {
        // Primary marks the "good" action with a bold, strong fg
        // on the surrounding surface. Only the focused state
        // paints a backing colour — verified in
        // `button_focused_overrides_with_menu_active_keys`.
        let entry = render_button(
            "Submit",
            false,
            ButtonKind::Primary,
            false,
            false,
            None,
            false,
        );
        assert_eq!(entry.inline_overlays.len(), 1);
        let style = &entry.inline_overlays[0].style;
        assert!(style.bold);
        assert_eq!(
            style.fg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.help_key_fg"),
        );
        assert!(style.bg.is_none(), "unfocused primary must not paint a bg");
    }

    #[test]
    fn button_danger_uses_error_theme_key() {
        let entry = render_button(
            "Delete",
            false,
            ButtonKind::Danger,
            false,
            false,
            None,
            false,
        );
        assert_eq!(entry.inline_overlays.len(), 1);
        let fg = entry.inline_overlays[0].style.fg.as_ref().unwrap();
        assert_eq!(fg.as_theme_key(), Some("diagnostic.error_fg"));
        assert!(entry.inline_overlays[0].style.bold);
    }

    #[test]
    fn button_focused_overrides_with_popup_selection_keys() {
        // Picker / palette / list / button focus now resolves through
        // `ui.popup_selection_{fg,bg}` (white-on-blue) instead of
        // `ui.menu_active_{fg,bg}` (white-on-rgb(60,60,60)) — the
        // former has ~6× the perceptual contrast against the popup
        // bg and is the same key the prompt already uses. See the
        // `KEY_FOCUSED_FG/BG` const comment.
        let entry = render_button("OK", true, ButtonKind::Normal, false, false, None, false);
        let style = &entry.inline_overlays[0].style;
        assert_eq!(
            style.fg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.popup_selection_fg")
        );
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.popup_selection_bg")
        );
        assert!(style.bold);
    }

    #[test]
    fn flex_spacer_fills_remaining_row_width() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 0,
                    flex: true,
                    key: None,
                },
                WidgetSpec::Button {
                    label: "B".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: None,
                    disabled: false,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                },
            ],
            key: None,
        };
        // Toggle "[ ] A" = 5 bytes; Button "[ B ]" = 5 bytes;
        // panel_width = 30 → flex fills 20 spaces. Plus a trailing
        // newline added by the Row's terminator.
        let out = render_spec(&spec, &HashMap::new(), "", 30);
        assert_eq!(out.entries.len(), 1);
        let text = &out.entries[0].text;
        assert_eq!(text.len(), 31);
        assert!(text.starts_with("[ ] A"));
        assert!(text.ends_with("[ B ]\n"));
        let button_hit = out.hits.iter().find(|h| h.widget_kind == "button").unwrap();
        assert_eq!(button_hit.byte_start, 25);
        assert_eq!(button_hit.byte_end, 30);
    }

    #[test]
    fn flex_spacer_with_no_leftover_collapses_to_zero() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 0,
                    flex: true,
                    key: None,
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: None,
                },
            ],
            key: None,
        };
        // Both toggles use 5+5=10 bytes; panel_width=10 → flex=0.
        let out = render_spec(&spec, &HashMap::new(), "", 10);
        assert_eq!(out.entries[0].text, "[ ] A[ ] B\n");
    }

    #[test]
    fn spacer_in_row_pads_with_spaces() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: None,
                },
                WidgetSpec::Spacer {
                    cols: 4,
                    flex: false,
                    key: None,
                },
                WidgetSpec::Button {
                    label: "Go".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: None,
                    disabled: false,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                },
            ],
            key: None,
        };
        let (out, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text, "[ ] A    [ Go ]\n");
    }

    #[test]
    fn row_collapses_inline_children_with_shifted_overlays() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "Tab".into(),
                        label: "x".into(),
                    }],
                    key: None,
                },
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "Esc".into(),
                        label: "y".into(),
                    }],
                    key: None,
                },
            ],
            key: None,
        };
        let (out, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(out.len(), 1);
        // Two adjacent HintBars are concatenated; the second's overlay shifts.
        assert_eq!(out[0].text, "Tab xEsc y\n");
        assert_eq!(out[0].inline_overlays.len(), 2);
        assert_eq!(out[0].inline_overlays[1].start, 5);
        assert_eq!(out[0].inline_overlays[1].end, 8);
    }

    // -------------------------------------------------------------
    // Hit-area tests
    // -------------------------------------------------------------

    #[test]
    fn toggle_emits_hit_area_with_toggle_payload() {
        let spec = WidgetSpec::Toggle {
            indeterminate: false,
            label_first: false,
            label_width: 0,
            checked: false,
            label: "Case".into(),
            focused: false,
            key: Some("case".into()),
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 1);
        let h = &hits[0];
        assert_eq!(h.widget_key, "case");
        assert_eq!(h.widget_kind, "toggle");
        assert_eq!(h.event_type, "toggle");
        assert_eq!(h.buffer_row, 0);
        assert_eq!(h.byte_start, 0);
        assert_eq!(h.byte_end, "[ ] Case".len());
        assert_eq!(h.payload, json!({"checked": true}));
    }

    #[test]
    fn button_emits_hit_area_with_activate_payload() {
        let spec = WidgetSpec::Button {
            label: "Replace All".into(),
            focused: false,
            intent: ButtonKind::Primary,
            key: Some("replace".into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 1);
        let h = &hits[0];
        assert_eq!(h.widget_key, "replace");
        assert_eq!(h.widget_kind, "button");
        assert_eq!(h.event_type, "activate");
        assert_eq!(h.byte_end, "[ Replace All ]".len());
        assert_eq!(h.payload, json!({}));
    }

    #[test]
    fn disabled_button_omits_hit_area_and_skips_tabbable() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Button {
                    label: "Archive".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: Some("archive".into()),
                    disabled: true,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                },
                WidgetSpec::Button {
                    label: "Cancel".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: Some("cancel".into()),
                    disabled: false,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 30);
        assert_eq!(
            out.hits
                .iter()
                .filter(|h| h.widget_kind == "button")
                .count(),
            1,
            "disabled button should not emit a hit area"
        );
        assert_eq!(
            out.tabbable,
            vec!["cancel".to_string()],
            "disabled button must drop out of the Tab cycle"
        );
    }

    #[test]
    fn disabled_button_uses_menu_disabled_fg_overlay() {
        let entry = render_button(
            "Archive",
            false,
            ButtonKind::Danger,
            true,
            false,
            None,
            false,
        );
        assert_eq!(entry.inline_overlays.len(), 1);
        let style = &entry.inline_overlays[0].style;
        assert_eq!(
            style.fg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.menu_disabled_fg"),
            "disabled overrides Danger fg with the muted theme key"
        );
        assert!(
            !style.bold,
            "disabled buttons drop the intent's bold emphasis"
        );
        assert!(style.bg.is_none(), "disabled buttons paint no bg");
    }

    #[test]
    fn hovered_button_paints_the_shared_hover_band() {
        let entry = render_button(
            "Delete",
            false,
            ButtonKind::Danger,
            false,
            false,
            None,
            true,
        );
        let style = &entry.inline_overlays[0].style;
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.menu_hover_bg"),
            "the pointer paints the shared hover background"
        );
        assert_eq!(
            style.fg.as_ref().and_then(|c| c.as_theme_key()),
            Some("diagnostic.error_fg"),
            "hover sits *under* the intent's own colour rather than replacing it"
        );
    }

    #[test]
    fn hovered_disabled_button_stays_inert() {
        let entry = render_button(
            "Archive",
            false,
            ButtonKind::Danger,
            true,
            false,
            None,
            true,
        );
        let style = &entry.inline_overlays[0].style;
        assert!(
            style.bg.is_none(),
            "an inert control must not advertise itself as actionable under the pointer"
        );
    }

    #[test]
    fn hovered_tree_row_lights_only_the_row_under_the_pointer() {
        let node = |text: &str| TreeNode {
            text: TextPropertyEntry::text(text),
            depth: 0,
            has_children: false,
            checked: None,
            extra_lines: Vec::new(),
        };
        let nodes = vec![node("alpha"), node("beta")];
        let spec = WidgetSpec::Tree {
            nodes,
            item_keys: vec!["a".to_string(), "b".to_string()],
            selected_index: -1,
            visible_rows: Some(4),
            expanded_keys: Vec::new(),
            checkable: false,
            item_height: 1,
            card_borders: false,
            indent_cols: 2,
            key: Some("sessions".to_string()),
        };
        let out = render_spec_with_options(
            &spec,
            &HashMap::new(),
            40,
            RenderOptions {
                // Every row of a tree shares the tree's widget key, so the
                // row identity has to come from `hover_item_key`.
                hover_key: "sessions",
                hover_item_key: "b",
                hover_popup_row: "",
                ..Default::default()
            },
        );
        let band = |row: usize| {
            out.entries[row].inline_overlays.iter().any(|o| {
                o.style.bg.as_ref().and_then(|c| c.as_theme_key()) == Some("ui.menu_hover_bg")
            })
        };
        assert!(!band(0), "the row the pointer is not on stays unpainted");
        assert!(band(1), "the row under the pointer takes the hover band");
    }

    #[test]
    fn row_inline_collapse_shifts_hit_byte_offsets() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: true,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Spacer {
                    cols: 2,
                    flex: false,
                    key: None,
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        // One merged row with text "[v] A  [ ] B"
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].text, "[v] A  [ ] B\n");
        assert_eq!(hits.len(), 2);
        assert_eq!(hits[0].widget_key, "a");
        assert_eq!(hits[0].buffer_row, 0);
        assert_eq!(hits[0].byte_start, 0);
        assert_eq!(hits[0].byte_end, 5); // "[v] A".len()
                                         // Second toggle shifts past first toggle ("[v] A".len() = 5)
                                         // + spacer ("  ".len() = 2) = 7.
        assert_eq!(hits[1].widget_key, "b");
        assert_eq!(hits[1].buffer_row, 0);
        assert_eq!(hits[1].byte_start, 7);
        assert_eq!(hits[1].byte_end, 12);
    }

    #[test]
    fn col_stacks_hit_rows() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "row0".into(),
                    focused: false,
                    key: Some("k0".into()),
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: true,
                    label: "row1".into(),
                    focused: false,
                    key: Some("k1".into()),
                },
            ],
            key: None,
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 2);
        assert_eq!(hits[0].buffer_row, 0);
        assert_eq!(hits[1].buffer_row, 1);
    }

    // -------------------------------------------------------------
    // Focus management
    // -------------------------------------------------------------

    #[test]
    fn collect_tabbable_visits_widgets_with_keys_in_declaration_order() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![],
                    key: Some("hb".into()),
                },
                WidgetSpec::Row {
                    wrap: false,
                    children: vec![
                        WidgetSpec::Toggle {
                            indeterminate: false,
                            label_first: false,
                            label_width: 0,
                            checked: false,
                            label: "T".into(),
                            focused: false,
                            key: Some("t".into()),
                        },
                        WidgetSpec::Spacer {
                            cols: 1,
                            flex: false,
                            key: None,
                        },
                        WidgetSpec::Button {
                            label: "B".into(),
                            focused: false,
                            intent: ButtonKind::Normal,
                            key: Some("b".into()),
                            disabled: false,
                            focusable: true,
                            bare: false,
                            full_width: false,
                            hover_style: None,
                        },
                    ],
                    key: None,
                },
                WidgetSpec::Text {
                    sel_start: -1,
                    sel_end: -1,
                    block_caret: false,
                    label_width: 0,
                    value: "".into(),
                    cursor_byte: -1,
                    focused: false,
                    label: "".into(),
                    placeholder: None,
                    rows: 1,
                    field_width: 0,
                    max_visible_chars: 0,
                    full_width: false,
                    completions: Vec::new(),
                    completions_visible_rows: 0,
                    read_only: false,
                    markdown: false,
                    key: Some("ti".into()),
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "no key".into(),
                    focused: false,
                    key: None,
                },
            ],
            key: None,
        };
        let mut tabbable = Vec::new();
        collect_tabbable(&spec, &mut tabbable);
        // HintBar without a key isn't tabbable; tabbables are
        // Toggle/Button/TextInput/List with non-empty keys.
        assert_eq!(tabbable, vec!["t", "b", "ti"]);
    }

    #[test]
    fn first_render_focuses_first_tabbable() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", u32::MAX);
        assert_eq!(out.focus_key, "a");
        assert_eq!(out.tabbable, vec!["a", "b"]);
    }

    #[test]
    fn render_preserves_focus_key_across_re_renders() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "b", u32::MAX);
        assert_eq!(out.focus_key, "b");
    }

    #[test]
    fn render_clamps_stale_focus_key_to_first_tabbable() {
        // Previous render focused "stale", but the new spec doesn't
        // have any widget with that key — fall back to the first
        // tabbable.
        let spec = WidgetSpec::Toggle {
            indeterminate: false,
            label_first: false,
            label_width: 0,
            checked: false,
            label: "Only".into(),
            focused: false,
            key: Some("only".into()),
        };
        let out = render_spec(&spec, &HashMap::new(), "stale", u32::MAX);
        assert_eq!(out.focus_key, "only");
    }

    #[test]
    fn focused_widget_renders_with_focused_styling() {
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "A".into(),
                    focused: false,
                    key: Some("a".into()),
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "B".into(),
                    focused: false,
                    key: Some("b".into()),
                },
            ],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "b", u32::MAX);
        assert_eq!(out.entries.len(), 1, "row collapses inline");
        // Two overlays expected from the focused B: one for B's
        // glyph (none, since unchecked) — actually unchecked emits
        // no glyph overlay. So only the focused-style overlay.
        // Find the focused overlay by its popup_selection_bg key
        // (white-on-blue; see KEY_FOCUSED_BG).
        let entry = &out.entries[0];
        let focused_overlay = entry
            .inline_overlays
            .iter()
            .find(|o| {
                o.style.bg.as_ref().and_then(|c| c.as_theme_key()) == Some("ui.popup_selection_bg")
            })
            .expect("focused overlay present on B");
        // B's text is "[ ] B", starting after "[ ] A".len()==5 + spacer 0 (no spacer here).
        // Inline collapse: A is "[ ] A" then immediately "[ ] B" = 10 bytes.
        assert_eq!(focused_overlay.start, 5);
        assert_eq!(focused_overlay.end, 10);
    }

    #[test]
    fn no_tabbables_yields_empty_focus_key() {
        let spec = WidgetSpec::Col {
            children: vec![WidgetSpec::HintBar {
                entries: vec![],
                key: None,
            }],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", u32::MAX);
        assert_eq!(out.focus_key, "");
        assert!(out.tabbable.is_empty());
    }

    // -------------------------------------------------------------
    // List
    // -------------------------------------------------------------

    #[test]
    fn list_emits_one_entry_and_one_hit_per_item() {
        let spec = WidgetSpec::List {
            items: vec![
                TextPropertyEntry::text("alpha"),
                TextPropertyEntry::text("beta"),
                TextPropertyEntry::text("gamma"),
            ],
            item_specs: vec![],
            item_keys: vec!["a".into(), "b".into(), "c".into()],
            selected_index: -1,
            visible_rows: Some(10),
            focusable: true,
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        // 3 real items + 7 blank padding rows to fill `visible_rows=10`.
        // Padding ensures the labeledSection that wraps a List stays
        // the height it advertises, so a sibling pane lands its
        // bottom border on the matching row (orchestrator picker
        // depends on this).
        assert_eq!(entries.len(), 10);
        // Real items still produce exactly one hit each; padded rows
        // are intentionally not clickable.
        assert_eq!(hits.len(), 3);
        for (i, h) in hits.iter().enumerate() {
            assert_eq!(h.buffer_row, i as u32);
            assert_eq!(h.widget_kind, "list");
            assert_eq!(h.event_type, "select");
            assert_eq!(h.payload["index"], i);
        }
        assert_eq!(hits[0].widget_key, "a");
        assert_eq!(hits[2].widget_key, "c");
    }

    #[test]
    fn list_item_specs_render_multirow_cards_in_item_units() {
        // Two cards, each a LabeledSection (rounded box) wrapping one
        // body row ⇒ 3 rows tall (top border, body, bottom border).
        let card = |body: &str| WidgetSpec::LabeledSection {
            label: String::new(),
            child: Box::new(WidgetSpec::Raw {
                entries: vec![TextPropertyEntry::text(body)],
                key: None,
            }),
            width_pct: None,
            key: None,
        };
        let spec = WidgetSpec::List {
            items: vec![],
            item_specs: vec![card("aaa"), card("bbb")],
            item_keys: vec!["a".into(), "b".into()],
            selected_index: 1,
            // 12 rows available: 2 cards * 3 rows = 6, padded to 12.
            visible_rows: Some(12),
            focusable: true,
            key: Some("cards".into()),
        };
        // Finite panel width (cards draw borders sized to it; the
        // u32::MAX `render_no_focus` uses would loop drawing `─`).
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let (entries, hits) = (out.entries, out.hits);
        // Fills the advertised height.
        assert_eq!(entries.len(), 12);
        // Card height is 3 rows; both cards render → 6 hit rows, all
        // mapping back to their item index (whole card is clickable).
        assert_eq!(hits.len(), 6, "3 rows per card * 2 cards");
        assert!(hits[0..3]
            .iter()
            .all(|h| h.payload["index"] == 0 && h.widget_key == "a"));
        assert!(hits[3..6]
            .iter()
            .all(|h| h.payload["index"] == 1 && h.widget_key == "b"));
        // The selected card (index 1, rows 3..6) is marked by a heavy
        // box border + bold — NOT a background band (which read garish
        // over a multi-row card). The unselected card (rows 0..3) keeps
        // the light rounded border and no bold.
        for r in 0..3 {
            assert!(
                !entries[r].text.contains('┓') && !entries[r].text.contains('┃'),
                "unselected card row {r} should keep the light border"
            );
            assert!(entries[r].style.as_ref().map_or(true, |s| s.bg.is_none()));
        }
        // Heavy border glyphs appear somewhere in the selected card, and
        // its rows are bold, with no background band.
        let heavy = (3..6).any(|r| {
            entries[r].text.contains('┏')
                || entries[r].text.contains('┗')
                || entries[r].text.contains('┃')
        });
        assert!(heavy, "selected card should use a heavy box border");
        for r in 3..6 {
            let style = entries[r].style.as_ref();
            assert!(
                style.map(|s| s.bold).unwrap_or(false),
                "row {r} of the selected card should be bold"
            );
            assert!(
                style.and_then(|s| s.bg.as_ref()).is_none(),
                "row {r} of the selected card should NOT use a background band"
            );
        }
        // Rounded corners survived the per-item render.
        assert!(entries[0].text.starts_with('╭'));
        assert!(entries[2].text.starts_with('╰'));
    }

    #[test]
    fn selected_card_accent_frames_all_four_sides() {
        // A selected multi-row card frames itself with a heavy accent
        // border. Regression: the accent fg was applied only to the
        // top/bottom border rows, leaving the vertical `┃` glyphs on the
        // body rows uncoloured — so the highlight framed only two sides.
        // The fix tints the side `┃` glyphs via sub-range overlays without
        // repainting the body text between them.
        let card = |body: &str| WidgetSpec::LabeledSection {
            label: String::new(),
            child: Box::new(WidgetSpec::Raw {
                entries: vec![TextPropertyEntry::text(body)],
                key: None,
            }),
            width_pct: None,
            key: None,
        };
        let spec = WidgetSpec::List {
            items: vec![],
            item_specs: vec![card("aaa"), card("bbb")],
            item_keys: vec!["a".into(), "b".into()],
            selected_index: 1,
            visible_rows: Some(12),
            focusable: true,
            key: Some("cards".into()),
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let entries = out.entries;
        // Selected card is index 1 → rows 3 (top), 4 (body/side), 5 (bottom).
        let accent_is = |c: &OverlayColorSpec| matches!(c, OverlayColorSpec::ThemeKey(k) if k == "ui.popup_border_fg");
        // Top + bottom carry the accent as a whole-row fg (entire row is border).
        for r in [3usize, 5] {
            let fg = entries[r].style.as_ref().and_then(|s| s.fg.as_ref());
            assert!(
                fg.map(accent_is).unwrap_or(false),
                "row {r} (top/bottom border) should carry the accent fg"
            );
        }
        // The body row keeps heavy side borders but must NOT set a
        // whole-row fg (that would repaint the session text). Its vertical
        // `┃` glyphs are tinted via sub-range overlays instead.
        let body = &entries[4];
        assert!(
            body.text.contains('┃'),
            "selected card body row should have heavy side borders: {:?}",
            body.text
        );
        assert!(
            body.style.as_ref().and_then(|s| s.fg.as_ref()).is_none(),
            "body row must not set a whole-row fg (would repaint the text)"
        );
        let bar_overlays: Vec<_> = body
            .inline_overlays
            .iter()
            .filter(|o| o.style.fg.as_ref().map(accent_is).unwrap_or(false))
            .collect();
        assert_eq!(
            bar_overlays.len(),
            2,
            "both the leading and trailing ┃ should be accent-tinted: {:?}",
            body.inline_overlays
        );
        // Each accent overlay covers exactly one `┃` glyph.
        for o in bar_overlays {
            assert_eq!(o.end - o.start, '┃'.len_utf8());
            assert_eq!(&body.text[o.start..o.end], "┃");
        }
    }

    #[test]
    fn list_applies_selection_bg_to_selected_row() {
        let spec = WidgetSpec::List {
            items: vec![
                TextPropertyEntry::text("first"),
                TextPropertyEntry::text("second"),
            ],
            item_specs: vec![],
            item_keys: vec!["x".into(), "y".into()],
            selected_index: 1,
            visible_rows: Some(10),
            focusable: true,
            key: None,
        };
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert!(entries[0].style.is_none(), "unselected row keeps no style");
        let style = entries[1].style.as_ref().expect("selected row gets style");
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.popup_selection_bg"),
        );
        assert!(style.extend_to_line_end);
    }

    #[test]
    fn list_inside_col_offsets_hit_rows_by_preceding_lines() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::HintBar {
                    entries: vec![HintEntry {
                        keys: "h".into(),
                        label: "header".into(),
                    }],
                    key: None,
                },
                WidgetSpec::List {
                    items: vec![
                        TextPropertyEntry::text("row0"),
                        TextPropertyEntry::text("row1"),
                    ],
                    item_specs: vec![],
                    item_keys: vec!["a".into(), "b".into()],
                    selected_index: -1,
                    visible_rows: Some(10),
                    key: None,
                    focusable: true,
                },
            ],
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        // HintBar (1 row) + List items (2) + padding rows (8) to fill
        // `visible_rows=10` = 11 total entries.
        assert_eq!(entries.len(), 11);
        // Real list rows still produce one hit each; padding is not
        // clickable.
        assert_eq!(hits.len(), 2);
        // List rows land at buffer_row 1 and 2 (after the HintBar).
        assert_eq!(hits[0].buffer_row, 1);
        assert_eq!(hits[1].buffer_row, 2);
    }

    #[test]
    fn list_payload_includes_absolute_index_and_key() {
        let spec = WidgetSpec::List {
            items: vec![TextPropertyEntry::text("only")],
            item_specs: vec![],
            item_keys: vec!["match:42".into()],
            selected_index: 0,
            visible_rows: Some(10),
            focusable: true,
            key: None,
        };
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].payload["index"], 0);
        assert_eq!(hits[0].payload["key"], "match:42");
    }

    #[test]
    fn list_hit_payload_carries_list_key() {
        // The click handler needs the List's *spec* key to update the
        // host-owned selection (instance state is keyed by it) and to
        // report a `widget_key` consistent with keyboard nav. The
        // per-item key alone (in `payload.key`) can't identify the
        // widget, so every list hit must carry `list_key`.
        let spec = make_list(-1, 10, 2, Some("mylist"));
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 2);
        assert_eq!(hits[0].payload["list_key"], "mylist");
        assert_eq!(hits[1].payload["list_key"], "mylist");
    }

    #[test]
    fn list_hit_payload_list_key_is_null_when_keyless() {
        // A keyless List has no instance state to update, so the click
        // handler must be able to tell (null) and skip the sync.
        let spec = make_list(-1, 10, 1, None);
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert!(hits[0].payload["list_key"].is_null());
    }

    #[test]
    fn list_with_missing_key_emits_empty_widget_key() {
        let spec = WidgetSpec::List {
            items: vec![TextPropertyEntry::text("a"), TextPropertyEntry::text("b")],
            // Only one key for two items — second hit gets an empty key.
            item_specs: vec![],
            item_keys: vec!["only".into()],
            selected_index: -1,
            visible_rows: Some(10),
            focusable: true,
            key: None,
        };
        let (_, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].widget_key, "only");
        assert_eq!(hits[1].widget_key, "");
    }

    pub(crate) fn make_list(
        selected: i32,
        visible: u32,
        total: usize,
        key: Option<&str>,
    ) -> WidgetSpec {
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

    #[test]
    fn list_renders_only_visible_window() {
        let spec = make_list(-1, 3, 10, Some("L"));
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 3);
        assert_eq!(hits.len(), 3);
        // First three items, absolute indices 0..2.
        assert_eq!(hits[0].payload["index"], 0);
        assert_eq!(hits[2].payload["index"], 2);
    }

    #[test]
    fn list_scrolls_to_keep_selected_below_window_in_view() {
        // 10 items, visible=3, select index 5: scroll should be 3
        // (so selected lands at the bottom of the window). On
        // *first* render (empty prev), the spec's selected_index
        // seeds instance state.
        let spec = make_list(5, 3, 10, Some("L"));
        let (_entries, hits, state) = render_no_focus(&spec, &HashMap::new());
        // Visible window is items 3..6 → hits index 3, 4, 5.
        assert_eq!(hits.len(), 3);
        assert_eq!(hits[0].payload["index"], 3);
        assert_eq!(hits[2].payload["index"], 5);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 3);
    }

    #[test]
    fn list_scrolls_to_keep_selected_above_window_in_view() {
        // Previous render scrolled to 5 with selection at 5; user
        // pressed Up enough times that select_move set instance
        // state's selection to 1; renderer should scroll back up
        // to 1. (Spec's selected_index is initial-only; instance
        // state is authoritative once present.)
        let mut prev = HashMap::new();
        prev.insert(
            "L".into(),
            WidgetInstanceState::List {
                scroll_offset: 5,
                selected_index: 1,
                item_height: 1,
                user_scrolled: false,
            },
        );
        // Spec's selected_index doesn't matter (instance state wins).
        let spec = make_list(99, 3, 10, Some("L"));
        let (_entries, hits, state) = render_no_focus(&spec, &prev);
        assert_eq!(hits[0].payload["index"], 1);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 1);
    }

    #[test]
    fn list_scroll_preserved_when_selection_remains_in_view() {
        // Previous render scrolled to 4 with selection at 4; user
        // moved selection to 5 (still in window 4..6); scroll stays.
        let mut prev = HashMap::new();
        prev.insert(
            "L".into(),
            WidgetInstanceState::List {
                scroll_offset: 4,
                selected_index: 5,
                item_height: 1,
                user_scrolled: false,
            },
        );
        let spec = make_list(99, 3, 10, Some("L"));
        let (_entries, hits, state) = render_no_focus(&spec, &prev);
        assert_eq!(hits[0].payload["index"], 4);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 4);
    }

    #[test]
    fn list_clamps_scroll_to_max_when_dataset_is_smaller_than_old_offset() {
        // Previous scroll past the end of a now-shorter dataset
        // clamps to max_scroll = total - visible.
        let mut prev = HashMap::new();
        prev.insert(
            "L".into(),
            WidgetInstanceState::List {
                scroll_offset: 8,
                selected_index: -1,
                item_height: 1,
                user_scrolled: false,
            },
        );
        let spec = make_list(-1, 3, 5, Some("L"));
        let (entries, _hits, state) = render_no_focus(&spec, &prev);
        assert_eq!(entries.len(), 3);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        // total=5, visible=3 → max=2.
        assert_eq!(scroll, 2);
    }

    #[test]
    fn list_does_not_scroll_when_total_smaller_than_visible() {
        let spec = make_list(-1, 10, 3, Some("L"));
        let (entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        // 3 items + 7 blank padding rows to fill `visible_rows=10`.
        // The labeledSection wrapping a List keeps the height it
        // advertises so a sibling pane (orchestrator picker's
        // preview) can match.
        assert_eq!(entries.len(), 10);
        let scroll = match state.get("L").unwrap() {
            WidgetInstanceState::List { scroll_offset, .. } => *scroll_offset,
            _ => unreachable!(),
        };
        assert_eq!(scroll, 0);
    }

    #[test]
    fn list_without_key_does_not_persist_state() {
        let spec = make_list(5, 3, 10, None);
        let (_entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        assert!(
            state.is_empty(),
            "Lists without a `key` opt out of state preservation"
        );
    }

    // -------------------------------------------------------------
    // TextInput
    // -------------------------------------------------------------

    #[test]
    fn text_input_renders_value_in_brackets() {
        let entry = render_text_input("hello", -1, None, false, "", None, 0, 0, false, 0).entry;
        assert_eq!(entry.text, "[hello]");
        assert!(entry.inline_overlays.is_empty());
    }

    #[test]
    fn text_input_with_label_prefixes_with_label_space() {
        let entry =
            render_text_input("foo", -1, None, false, "Search:", None, 0, 0, false, 0).entry;
        assert_eq!(entry.text, "Search: [foo]");
    }

    #[test]
    fn text_input_focused_adds_input_bg_overlay() {
        let entry = render_text_input("x", -1, None, true, "", None, 0, 0, false, 0).entry;
        // Focused → input-bg overlay (no cursor since cursor_byte < 0).
        assert_eq!(entry.inline_overlays.len(), 1);
        let bg = entry.inline_overlays[0].style.bg.as_ref().unwrap();
        assert_eq!(bg.as_theme_key(), Some("ui.prompt_bg"));
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

    #[test]
    fn raw_inside_col_offsets_following_hits() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![
                        TextPropertyEntry::text("line0"),
                        TextPropertyEntry::text("line1"),
                        TextPropertyEntry::text("line2"),
                    ],
                    key: None,
                },
                WidgetSpec::Toggle {
                    indeterminate: false,
                    label_first: false,
                    label_width: 0,
                    checked: false,
                    label: "after raw".into(),
                    focused: false,
                    key: Some("post".into()),
                },
            ],
            key: None,
        };
        let (entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 4);
        assert_eq!(hits.len(), 1);
        assert_eq!(hits[0].buffer_row, 3);
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

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_collapsed() {
        let r = render_tree_row(&tnode("file.txt", 0, true), false, false, 1, false, 80, 2);
        assert!(r.entry.text.starts_with('\u{25B6}'), "starts with ▶");
        assert!(r.entry.text.contains("file.txt"));
        assert!(r.disclosure_range.is_some());
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_expanded() {
        let r = render_tree_row(&tnode("file.txt", 0, true), true, false, 1, false, 80, 2);
        assert!(r.entry.text.starts_with('\u{25BC}'), "starts with ▼");
    }

    #[test]
    fn tree_row_leaf_uses_two_spaces_no_disclosure_hit() {
        let r = render_tree_row(&tnode("match", 0, false), false, false, 1, false, 80, 2);
        // No glyph, just spaces for alignment.
        assert!(r.entry.text.starts_with("  "));
        assert!(r.entry.text.contains("match"));
        assert!(r.disclosure_range.is_none());
    }

    #[test]
    fn tree_row_indents_by_depth_times_two() {
        let r = render_tree_row(&tnode("nested", 2, false), false, false, 1, false, 80, 2);
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
        let r = render_tree_row(&node, false, false, 1, false, 80, 2);
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
        let r = render_tree_row(&node, false, false, 1, false, 80, 2);
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
        let r = render_tree_row(&node, false, true, 1, false, 80, 2);
        assert!(r.checkbox_range.is_none());
        assert!(!r.entry.text.contains("[v]"));
        assert!(!r.entry.text.contains("[ ]"));
    }

    #[test]
    fn tree_row_renders_checked_glyph_after_disclosure() {
        let mut node = tnode("file.rs", 0, true);
        node.checked = Some(true);
        let r = render_tree_row(&node, true, true, 1, false, 80, 2);
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
        let r = render_tree_row(&node, false, true, 1, false, 80, 2);
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
        let r = render_tree_row(&node, false, true, 1, false, 80, 2);
        let (cb_start, cb_end) = r.checkbox_range.unwrap();
        assert!(r.entry.text.is_char_boundary(cb_start));
        assert!(r.entry.text.is_char_boundary(cb_end));
        assert_eq!(&r.entry.text[cb_start..cb_end], "[v]");
    }

    #[test]
    fn tree_node_pad_to_chars_pads_text_before_prefix_offset_shift() {
        // depth=0 prefix is "▶ " (1 codepoint glyph + 1 space).
        // Plugin sends body "x" with pad_to_chars=5; renderer pads
        // body to "x    " then prepends prefix.
        let mut node = tnode("x", 0, true);
        node.text.pad_to_chars = Some(5);
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec!["x"], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(entries.len(), 1);
        // The full row is prefix + padded body + trailing newline.
        // Body region must be "x    " (5 columns).
        let trimmed = entries[0].text.trim_end_matches('\n');
        assert!(
            trimmed.ends_with("x    "),
            "row should end with the padded body, got {trimmed:?}"
        );
    }

    #[test]
    fn tree_node_truncate_to_chars_cuts_body_before_prefix_offset_shift() {
        let mut node = tnode("abcdefghij", 0, false);
        node.text.truncate_to_chars = Some(6);
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        // With budget=6, truncation produces "abc..." (3 head chars
        // + ellipsis), then prefix is prepended.
        assert!(
            trimmed.ends_with("abc..."),
            "row should end with truncated body, got {trimmed:?}"
        );
    }

    #[test]
    fn tree_node_char_unit_overlay_resolves_against_padded_text_and_shifts_by_prefix() {
        // Body text "x" padded to 5 codepoints — the host pads to
        // "x    " before resolving overlays. A char-unit overlay at
        // [0..5] must end up covering the full padded body in bytes,
        // shifted right by the prefix length.
        let mut node = tnode("x", 0, false);
        node.text.pad_to_chars = Some(5);
        node.text.inline_overlays.push(InlineOverlay {
            start: 0,
            end: 5,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Char,
        });
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let entry = &entries[0];
        let bold = entry
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("bold overlay carried through");
        // depth=0, leaf → prefix is two spaces (no glyph). Body
        // starts at byte 2 and is 5 bytes (ASCII pad), so [2..7].
        assert_eq!(bold.start, 2);
        assert_eq!(bold.end, 7);
    }

    #[test]
    fn tree_node_char_unit_overlay_with_multibyte_body_resolves_correctly() {
        // Body text "éxé" — 3 codepoints, 5 bytes. A char-unit
        // overlay at [1..2] (just the "x") becomes byte [3..4]
        // within the body, then shifted by leaf prefix (2 bytes).
        let mut node = tnode("éxé", 0, false);
        node.text.inline_overlays.push(InlineOverlay {
            start: 1,
            end: 2,
            style: OverlayOptions {
                bold: true,
                ..Default::default()
            },
            properties: Default::default(),
            unit: OffsetUnit::Char,
        });
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let entry = &entries[0];
        let bold = entry
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("bold overlay carried through");
        // Prefix is 2 bytes (two ASCII spaces), char→byte [1..2]
        // resolves to body byte [2..3], then shift +2 → [4..5].
        let trimmed = entry.text.trim_end_matches('\n');
        assert_eq!(bold.start, 4);
        assert_eq!(bold.end, 5);
        assert_eq!(&trimmed[bold.start..bold.end], "x");
    }

    #[test]
    fn tree_node_segments_concatenate_into_row_text_with_per_segment_overlays() {
        let mut node = tnode("", 0, false);
        node.text.segments = vec![
            fresh_core::text_property::StyledSegment {
                text: "AB".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: " ".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: "CD".to_string(),
                style: Some(OverlayOptions {
                    bold: true,
                    ..Default::default()
                }),
                overlays: vec![],
            },
        ];
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        // Leaf row: 2-space prefix + concatenated segments.
        assert!(
            trimmed.ends_with("AB CD"),
            "row should end with concatenated segments, got {trimmed:?}"
        );
        let bold = entries[0]
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("styled segment overlay carried through");
        // Bold covers the third segment only ("CD" at byte 5..7
        // after 2-byte prefix + "AB " = 3 bytes).
        assert_eq!(&trimmed[bold.start..bold.end], "CD");
    }

    #[test]
    fn tree_node_segment_nested_overlay_shifts_to_segment_position() {
        // Build a row whose third segment carries a nested overlay
        // covering chars [0..3] within itself ("CDE"). The host
        // shifts those by the segment's start in the entry; final
        // bytes resolve against the assembled text.
        let mut node = tnode("", 0, false);
        node.text.segments = vec![
            fresh_core::text_property::StyledSegment {
                text: "AB".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: " - ".to_string(),
                style: None,
                overlays: vec![],
            },
            fresh_core::text_property::StyledSegment {
                text: "CDEFG".to_string(),
                style: None,
                overlays: vec![InlineOverlay {
                    start: 0,
                    end: 3,
                    style: OverlayOptions {
                        bold: true,
                        ..Default::default()
                    },
                    properties: Default::default(),
                    unit: OffsetUnit::Char,
                }],
            },
        ];
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        let bold = entries[0]
            .inline_overlays
            .iter()
            .find(|o| o.style.bold)
            .expect("nested overlay carried through");
        assert_eq!(&trimmed[bold.start..bold.end], "CDE");
    }

    #[test]
    fn tree_node_segments_with_pad_pad_after_concatenation() {
        let mut node = tnode("", 0, false);
        node.text.segments = vec![fresh_core::text_property::StyledSegment {
            text: "ab".to_string(),
            style: None,
            overlays: vec![],
        }];
        node.text.pad_to_chars = Some(5);
        let spec = make_tree(vec![node], vec!["x"], -1, 10, vec![], Some("T"));
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        let trimmed = entries[0].text.trim_end_matches('\n');
        // Two-space leaf prefix + "ab" + three padding spaces = "  ab   ".
        assert!(
            trimmed.ends_with("ab   "),
            "row should be padded after segment concat, got {trimmed:?}"
        );
    }

    /// One `align: "between"` card row: `left` and `right` groups meet
    /// at `splitByte`, rendered in a card `width` columns wide.
    fn between_card_row(left: &str, right: &str, width: u32) -> String {
        let mut node = tnode("name", 0, false);
        let mut line = TextPropertyEntry::text(format!("{left}{right}"));
        line.properties.insert(
            "align".to_string(),
            serde_json::Value::String("between".to_string()),
        );
        line.properties.insert(
            "splitByte".to_string(),
            serde_json::Value::Number((left.len() as u64).into()),
        );
        node.extra_lines = vec![line];
        let spec = WidgetSpec::Tree {
            nodes: vec![node],
            item_keys: vec!["x".to_string()],
            selected_index: -1,
            visible_rows: Some(10),
            expanded_keys: vec![],
            checkable: false,
            item_height: 2,
            card_borders: true,
            indent_cols: 2,
            key: Some("T".to_string()),
        };
        let out = render_spec(&spec, &HashMap::new(), "", width);
        // Top border, name row, the split row, bottom border.
        out.entries[2].text.trim_end_matches('\n').to_string()
    }

    /// A card row (the orchestrator dock's workspace cards) can ask for
    /// its right-hand group to sit flush against the card border while
    /// the left group starts at the left one — `align: "between"` with
    /// the group boundary as a byte offset. Only the host knows the
    /// card's real width (the dock is resizable), so it owns the gap.
    #[test]
    fn tree_card_between_alignment_pushes_the_right_group_to_the_border() {
        let row = between_card_row("branch", "PR #7", 30);
        assert!(
            row.starts_with("│branch") && row.ends_with("PR #7│"),
            "left group hugs the left border and the right group the right one, got {row:?}"
        );
        // Padding only between them — not a plugin-side guess that
        // leaves both groups floating mid-card.
        let inner = row.trim_start_matches('│').trim_end_matches('│');
        assert!(
            inner["branch".len()..inner.len() - "PR #7".len()]
                .chars()
                .all(|c| c == ' '),
            "the two groups are separated by padding only, got {inner:?}"
        );
    }

    /// The groups still get a separating space when the card has no room
    /// to spare — they must never run together into one unreadable word,
    /// even at the exact width where they would just barely both fit.
    #[test]
    fn tree_card_between_alignment_keeps_a_gap_when_the_row_is_full() {
        // Inner width 15 = exactly "abcdefghij" + "PR #7".
        let row = between_card_row("abcdefghij", "PR #7", 17);
        assert!(
            !row.contains("ijPR"),
            "a full row still separates the groups, got {row:?}"
        );
    }

    #[test]
    fn tree_renders_only_top_level_when_nothing_expanded() {
        let spec = make_tree(
            vec![
                tnode("a", 0, true),
                tnode("a.0", 1, false),
                tnode("a.1", 1, false),
                tnode("b", 0, true),
                tnode("b.0", 1, false),
            ],
            vec!["a", "a.0", "a.1", "b", "b.0"],
            -1,
            10,
            vec![], // none expanded
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        // Only the two top-level nodes are visible.
        assert_eq!(entries.len(), 2);
        assert!(entries[0].text.contains('a'));
        assert!(entries[1].text.contains('b'));
    }

    #[test]
    fn tree_renders_children_of_expanded_nodes() {
        let spec = make_tree(
            vec![
                tnode("a", 0, true),
                tnode("a.0", 1, false),
                tnode("a.1", 1, false),
                tnode("b", 0, true),
                tnode("b.0", 1, false),
            ],
            vec!["a", "a.0", "a.1", "b", "b.0"],
            -1,
            10,
            vec!["a"],
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        // a, a.0, a.1, b — b's child stays hidden.
        assert_eq!(entries.len(), 4);
    }

    #[test]
    fn tree_emits_two_hits_per_internal_row_one_per_leaf() {
        // a (internal, expanded) + a.0 (leaf) → 2 hits for a (disclosure + body)
        // and 1 hit for a.0 (body only).
        let spec = make_tree(
            vec![tnode("a", 0, true), tnode("a.0", 1, false)],
            vec!["a", "a.0"],
            -1,
            10,
            vec!["a"],
            Some("T"),
        );
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits.len(), 3);
        // First hit: disclosure on the internal node.
        assert_eq!(hits[0].event_type, "expand");
        assert_eq!(hits[0].widget_kind, "tree");
        assert_eq!(hits[1].event_type, "select");
        assert_eq!(hits[2].event_type, "select");
    }

    #[test]
    fn tree_hits_carry_tree_spec_key_and_per_item_key_in_payload() {
        let spec = make_tree(
            vec![tnode("only", 0, false)],
            vec!["only-key"],
            -1,
            10,
            vec![],
            Some("matchTree"),
        );
        let (_entries, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].widget_key, "matchTree");
        assert_eq!(hits[0].payload["key"], "only-key");
        assert_eq!(hits[0].payload["index"], 0);
    }

    #[test]
    fn tree_persists_expanded_keys_in_instance_state() {
        let spec = make_tree(
            vec![tnode("a", 0, true), tnode("a.0", 1, false)],
            vec!["a", "a.0"],
            -1,
            10,
            vec!["a"],
            Some("T"),
        );
        let (_, _, state) = render_no_focus(&spec, &HashMap::new());
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { expanded_keys, .. } => {
                assert!(expanded_keys.contains("a"));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn tree_instance_state_overrides_spec_expanded_keys() {
        // Previous instance state has b expanded but spec says a.
        // Instance state wins (spec is initial-only after first render).
        let mut prev = HashMap::new();
        prev.insert(
            "T".into(),
            WidgetInstanceState::Tree {
                scroll_offset: 0,
                selected_index: -1,
                expanded_keys: ["b".to_string()].iter().cloned().collect(),
                user_scrolled: false,
            },
        );
        let spec = make_tree(
            vec![
                tnode("a", 0, true),
                tnode("a.0", 1, false),
                tnode("b", 0, true),
                tnode("b.0", 1, false),
            ],
            vec!["a", "a.0", "b", "b.0"],
            -1,
            10,
            vec!["a"], // initial-only — ignored after first render
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &prev);
        // Should render: a (collapsed), b, b.0 — three rows. a.0 hidden.
        assert_eq!(entries.len(), 3);
    }

    #[test]
    fn tree_selected_row_gets_focused_bg() {
        let spec = make_tree(
            vec![tnode("a", 0, false), tnode("b", 0, false)],
            vec!["a", "b"],
            1,
            10,
            vec![],
            Some("T"),
        );
        let (entries, _hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert!(entries[0].style.is_none());
        let style = entries[1].style.as_ref().expect("selected gets style");
        assert_eq!(
            style.bg.as_ref().and_then(|c| c.as_theme_key()),
            Some("ui.popup_selection_bg")
        );
        assert!(style.extend_to_line_end);
    }

    #[test]
    fn tree_clamps_selection_to_visible_when_selected_node_is_hidden() {
        // selected_index = 1 (a.0), but `a` is collapsed → a.0 hidden.
        // The renderer falls back to the nearest earlier visible
        // node (a, idx 0).
        let spec = make_tree(
            vec![tnode("a", 0, true), tnode("a.0", 1, false)],
            vec!["a", "a.0"],
            1,
            10,
            vec![], // a not expanded
            Some("T"),
        );
        let (_entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { selected_index, .. } => {
                assert_eq!(*selected_index, 0);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn tree_scrolls_to_keep_selection_in_visible_window() {
        // 6 visible rows total, visible_rows=3, selected at flat
        // position 4 → scroll should be 2 (so selected lands at the
        // bottom of the window).
        let spec = make_tree(
            vec![
                tnode("0", 0, false),
                tnode("1", 0, false),
                tnode("2", 0, false),
                tnode("3", 0, false),
                tnode("4", 0, false),
                tnode("5", 0, false),
            ],
            vec!["k0", "k1", "k2", "k3", "k4", "k5"],
            4,
            3,
            vec![],
            Some("T"),
        );
        let (entries, _hits, state) = render_no_focus(&spec, &HashMap::new());
        // Visible window: items 2..5 → 3 rows.
        assert_eq!(entries.len(), 3);
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { scroll_offset, .. } => assert_eq!(*scroll_offset, 2),
            _ => unreachable!(),
        }
    }

    /// A mouse-scrolled tree (`user_scrolled`) keeps its offset even
    /// though the selected node is scrolled out of view. Without the
    /// flag, any re-render — e.g. the orchestrator dock's async
    /// probe-poll refresh re-pinning the same selection — snapped the
    /// wheel-scrolled view back to the selected card (the flaky
    /// `dock_card_tree_wheel_scrolls_when_overflowing` hang).
    #[test]
    fn tree_user_scroll_is_not_snapped_back_to_selection() {
        let mut prev = HashMap::new();
        prev.insert(
            "T".to_string(),
            WidgetInstanceState::Tree {
                scroll_offset: 3,
                selected_index: 0,
                expanded_keys: HashSet::new(),
                user_scrolled: true,
            },
        );
        let spec = make_tree(
            vec![
                tnode("n0", 0, false),
                tnode("n1", 0, false),
                tnode("n2", 0, false),
                tnode("n3", 0, false),
                tnode("n4", 0, false),
                tnode("n5", 0, false),
            ],
            vec!["k0", "k1", "k2", "k3", "k4", "k5"],
            0,
            2,
            vec![],
            Some("T"),
        );
        let (entries, _hits, state) = render_no_focus(&spec, &prev);
        // Window stays at the user's offset (n3, n4) — not snapped back
        // to the selected n0.
        assert!(
            entries[0].text.contains("n3"),
            "window must start at the user's scroll offset, got: {:?}",
            entries.iter().map(|e| e.text.trim()).collect::<Vec<_>>()
        );
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree {
                scroll_offset,
                user_scrolled,
                ..
            } => {
                assert_eq!(*scroll_offset, 3);
                assert!(*user_scrolled, "the flag must persist across renders");
            }
            _ => unreachable!(),
        }
    }

    /// Row-granular scrolling: a scroll offset landing *inside* a
    /// bordered card clips the card's top rows instead of snapping to a
    /// node boundary, and the clipped rows' hits are dropped/shifted so
    /// nothing hidden stays clickable.
    #[test]
    fn tree_row_scroll_clips_partial_cards_at_the_edges() {
        // Two bordered cards, item_height 3 → 5 rows each (10 total).
        let card = |name: &str| {
            let mut n = tnode(name, 0, false);
            n.extra_lines = vec![
                TextPropertyEntry::text(format!("{name}-l2")),
                TextPropertyEntry::text(format!("{name}-l3")),
            ];
            n
        };
        let mut prev = HashMap::new();
        prev.insert(
            "T".to_string(),
            WidgetInstanceState::Tree {
                // Row 2 of card A: its border + name rows are clipped
                // off; the window (6 rows) ends inside card B.
                scroll_offset: 2,
                selected_index: -1,
                expanded_keys: HashSet::new(),
                user_scrolled: true,
            },
        );
        let spec = WidgetSpec::Tree {
            nodes: vec![card("aa"), card("bb")],
            item_keys: vec!["ka".into(), "kb".into()],
            selected_index: -1,
            visible_rows: Some(6),
            expanded_keys: vec![],
            checkable: false,
            item_height: 3,
            card_borders: true,
            indent_cols: 2,
            key: Some("T".into()),
        };
        // A finite panel width: bordered cards draw `─` runs across the
        // full width, so the `u32::MAX` no-flex width `render_no_focus`
        // uses would try to build a 4-billion-char border string.
        let out = render_spec(&spec, &prev, "", 40);
        let (entries, hits) = (out.entries, out.hits);
        // Window = rows 2..8 of [A0 A1 A2 A3 A4 B0 B1 B2 B3 B4]:
        // A's l2 content row first, B's l2 row last; 6 rows exactly.
        assert_eq!(entries.len(), 6, "{:?}", texts(&entries));
        assert!(
            entries[0].text.contains("aa-l2"),
            "first row must be card A clipped mid-card: {:?}",
            texts(&entries)
        );
        assert!(
            entries[5].text.contains("bb-l2"),
            "last row must clip card B at the window bottom: {:?}",
            texts(&entries)
        );
        // No hit may point outside the emitted rows, and card A's
        // clipped-off name row must not have left a stale hit behind.
        assert!(
            hits.iter().all(|h| (h.buffer_row as usize) < entries.len()),
            "hits must be clipped/shifted with the rows: {:?}",
            hits.iter().map(|h| h.buffer_row).collect::<Vec<_>>()
        );
    }

    fn texts(entries: &[TextPropertyEntry]) -> Vec<&str> {
        entries.iter().map(|e| e.text.trim_end()).collect()
    }

    /// The inverse: once the flag clears (a deliberate selection move —
    /// keyboard nav, click, or a plugin `SetSelectedIndex` to a new
    /// index), keep-selection-visible re-engages and the window snaps
    /// to the selection again.
    #[test]
    fn tree_selection_move_re_arms_scroll_follow() {
        let mut prev = HashMap::new();
        prev.insert(
            "T".to_string(),
            WidgetInstanceState::Tree {
                scroll_offset: 3,
                selected_index: 0,
                expanded_keys: HashSet::new(),
                user_scrolled: false,
            },
        );
        let spec = make_tree(
            vec![
                tnode("n0", 0, false),
                tnode("n1", 0, false),
                tnode("n2", 0, false),
                tnode("n3", 0, false),
                tnode("n4", 0, false),
                tnode("n5", 0, false),
            ],
            vec!["k0", "k1", "k2", "k3", "k4", "k5"],
            0,
            2,
            vec![],
            Some("T"),
        );
        let (entries, _hits, state) = render_no_focus(&spec, &prev);
        assert!(
            entries[0].text.contains("n0"),
            "window must follow the selection when user_scrolled is clear, got: {:?}",
            entries.iter().map(|e| e.text.trim()).collect::<Vec<_>>()
        );
        match state.get("T").unwrap() {
            WidgetInstanceState::Tree { scroll_offset, .. } => assert_eq!(*scroll_offset, 0),
            _ => unreachable!(),
        }
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
    fn text_area_renders_visible_rows_count() {
        // Single line value, but rows=3 → 3 entries (line + 2
        // blanks).
        let spec = make_text_area("hi", -1, false, 3, 10, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 80);
        assert_eq!(out.entries.len(), 3);
    }

    #[test]
    fn text_area_pads_short_lines_to_field_width() {
        let spec = make_text_area("hi", -1, false, 1, 6, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 80);
        // First (only visible) row: "hi" padded to 6 chars → "hi    \n"
        let first = &out.entries[0];
        assert_eq!(first.text, "hi    \n");
    }

    #[test]
    fn text_area_truncates_long_line_with_ellipsis() {
        let spec = make_text_area("abcdefghi", -1, false, 1, 5, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 80);
        // 9 chars trimmed to 5 → "abcd…\n".
        assert_eq!(out.entries[0].text, "abcd…\n");
    }

    #[test]
    fn text_area_focused_adds_input_bg_overlay_per_row() {
        let spec = make_text_area("a\nb", -1, true, 3, 4, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        for entry in &out.entries {
            let has_bg = entry.inline_overlays.iter().any(|o| {
                o.style
                    .bg
                    .as_ref()
                    .and_then(|c| c.as_theme_key())
                    .map(|k| k == "ui.prompt_bg")
                    .unwrap_or(false)
            });
            assert!(has_bg, "every focused row gets input-bg");
        }
    }

    #[test]
    fn text_area_publishes_focus_cursor_at_value_position() {
        // value="ab\ncd", cursor at byte 4 (col 1 on line 1, char
        // 'd' position).
        let spec = make_text_area("ab\ncd", 4, true, 3, 6, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        let fc = out.focus_cursor.expect("focused → cursor published");
        // Line 1 is the second visible row → buffer_row 1.
        assert_eq!(fc.buffer_row, 1);
        // Col 1 on the rendered row.
        assert_eq!(fc.byte_in_row, 1);
    }

    #[test]
    fn text_area_label_offsets_cursor_buffer_row() {
        // With a label, the editing region starts on row 1, so a
        // cursor on line 0 of the value lands on row 1 of the
        // buffer.
        let spec = WidgetSpec::Text {
            sel_start: -1,
            sel_end: -1,
            block_caret: false,
            label_width: 0,
            value: "hi".into(),
            cursor_byte: 1,
            focused: true,
            label: "Note".into(),
            placeholder: None,
            rows: 2,
            field_width: 6,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            read_only: false,
            markdown: false,
            key: Some("ta".into()),
        };
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        // entries[0] is the label row, entries[1..] are content.
        assert!(out.entries[0].text.starts_with("Note:"));
        let fc = out.focus_cursor.unwrap();
        assert_eq!(fc.buffer_row, 1);
    }

    /// A single-line field whose value outgrows it keeps its
    /// horizontal window in instance state, so the window survives
    /// the renders that happen between keystrokes: the caret walking
    /// into the hidden head drags the view back with it (the field
    /// used to paint the tail unconditionally, stranding the start of
    /// a long value off-screen for good), and a caret moving *inside*
    /// the window leaves the view alone.
    #[test]
    fn single_line_field_window_follows_the_caret_across_renders() {
        let value = "0123456789abcdefghij";
        let field = |cursor: usize, scroll: u32| {
            let spec = WidgetSpec::Text {
                sel_start: -1,
                sel_end: -1,
                block_caret: false,
                label_width: 0,
                value: value.into(),
                cursor_byte: cursor as i32,
                focused: true,
                label: String::new(),
                placeholder: None,
                rows: 1,
                field_width: 8,
                max_visible_chars: 0,
                full_width: false,
                completions: Vec::new(),
                completions_visible_rows: 0,
                read_only: false,
                markdown: false,
                key: Some("f".into()),
            };
            let mut editor = crate::primitives::text_edit::TextEdit::single_line_with_text(value);
            editor.set_cursor_from_flat(cursor);
            let mut prev = HashMap::new();
            prev.insert(
                "f".into(),
                WidgetInstanceState::Text {
                    editor,
                    scroll,
                    completions: Vec::new(),
                    completion_selected_index: 0,
                    completion_scroll_offset: 0,
                    completion_navigated: false,
                    user_scrolled: false,
                },
            );
            let out = render_spec(&spec, &prev, "f", 80);
            let row = out.entries[0].text.trim_end_matches('\n').to_string();
            let scroll = match out.instance_states.get("f") {
                Some(WidgetInstanceState::Text { scroll, .. }) => *scroll,
                other => panic!("expected Text instance state, got {:?}", other),
            };
            (row, scroll)
        };

        // Caret at end: the window sits at the tail, as it always did.
        let (row, scroll) = field(value.len(), 0);
        assert!(row.contains("…defghij"), "row: {row}");
        assert_eq!(scroll, 13);

        // Caret walked home: the window comes back to the value's
        // start and the head is painted.
        let (row, scroll) = field(0, 13);
        assert!(row.contains("[0123456…"), "row: {row}");
        assert_eq!(scroll, 0);

        // Caret one step right of the window start: the view holds
        // still rather than re-anchoring on the caret.
        let (row, scroll) = field(1, 0);
        assert!(row.contains("[0123456…"), "row: {row}");
        assert_eq!(scroll, 0);
    }

    #[test]
    fn text_area_persists_value_and_cursor_in_instance_state() {
        let spec = make_text_area("abc", 2, true, 2, 8, Some("ta"));
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        match out.instance_states.get("ta") {
            Some(WidgetInstanceState::Text { editor, .. }) => {
                assert_eq!(editor.value(), "abc");
                assert_eq!(editor.flat_cursor_byte(), 2);
            }
            other => panic!("expected Text instance state, got {:?}", other),
        }
    }

    #[test]
    fn text_area_instance_state_overrides_spec_value() {
        // Plugin's spec says "old" but instance state has "new" —
        // the renderer reads from instance state.
        let spec = make_text_area("old", 0, true, 2, 8, Some("ta"));
        let mut prev = HashMap::new();
        let mut editor = crate::primitives::text_edit::TextEdit::with_text("new");
        editor.set_cursor_from_flat(3);
        prev.insert(
            "ta".into(),
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
        let out = render_spec(&spec, &prev, "ta", 80);
        // The first row should now read "new" (not "old").
        assert!(out.entries[0].text.starts_with("new"));
    }

    #[test]
    fn text_area_scroll_clamps_to_keep_cursor_visible() {
        // 5-line value, rows=2. Cursor on line 4 (last). On first
        // render the renderer should auto-scroll so line 4 is
        // visible.
        let spec = make_text_area("a\nb\nc\nd\ne", 8, true, 2, 4, Some("ta"));
        // byte 8 is on the 5th line (line index 4).
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        match out.instance_states.get("ta") {
            Some(WidgetInstanceState::Text { scroll, .. }) => {
                assert_eq!(*scroll, 3, "scroll so lines 3..5 are visible");
            }
            _ => panic!("expected Text instance state"),
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

    pub(crate) fn make_text_input(
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
    fn labeled_section_renders_three_rows_with_legend() {
        let spec = WidgetSpec::LabeledSection {
            label: "Name".into(),
            child: Box::new(make_text_input("hi", -1, false, false, 4, Some("n"))),
            width_pct: None,
            key: None,
        };
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 20);
        // 3 lines: top border, content, bottom border.
        assert_eq!(out.entries.len(), 3);
        // Top border has legend.
        assert!(out.entries[0].text.starts_with("╭─ Name "));
        assert!(out.entries[0].text.ends_with("╮\n"));
        // Content wrapped with side borders.
        assert!(out.entries[1].text.starts_with("│ "));
        assert!(out.entries[1].text.ends_with(" │\n"));
        // Bottom border is a plain run.
        assert!(out.entries[2].text.starts_with("╰"));
        assert!(out.entries[2].text.ends_with("╯\n"));
    }

    #[test]
    fn markdown_text_renders_document_rows_with_region_and_shadow() {
        // A markdown Text (no theme in a bare render context → plain
        // line fallback, same layout machinery) renders one row per
        // wrapped line, padded to `rows`, emits its geometry region,
        // and shadows the rendered plain text into a TextEdit so
        // selection/copy operate on exactly what's shown.
        let spec = WidgetSpec::Text {
            value: "alpha\nbeta\ngamma\ndelta\nepsilon".into(),
            cursor_byte: -1,
            focused: false,
            label: String::new(),
            placeholder: None,
            rows: 3,
            field_width: 0,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: true,
            key: Some("doc".into()),
        };
        let out = render_spec(&spec, &HashMap::new(), "", 30);
        assert_eq!(out.entries.len(), 3, "visible window is `rows` tall");
        assert!(out.entries[0].text.starts_with("alpha"));
        // The widget's box carries the scroll payload: every content
        // line counted, not just the window.
        let sc = out
            .boxes
            .iter()
            .find(|b| b.key.as_deref() == Some("doc"))
            .and_then(|b| b.scroll)
            .expect("scroll payload on the doc box");
        assert_eq!((sc.total, sc.visible), (5, 3));
        // The shadow editor holds the rendered plain text.
        match out.instance_states.get("doc") {
            Some(WidgetInstanceState::Text { editor, .. }) => {
                assert_eq!(editor.value(), "alpha\nbeta\ngamma\ndelta\nepsilon");
            }
            other => panic!("expected Text instance state, got {other:?}"),
        }
        // Every row is a caret target.
        assert_eq!(out.hits.len(), 3);
        assert!(out.hits.iter().all(|h| h.event_type == "focus"));
    }

    #[test]
    fn markdown_text_caret_follows_focus_and_paints_block_caret() {
        let spec = WidgetSpec::Text {
            value: "one\ntwo".into(),
            cursor_byte: -1,
            focused: false,
            label: String::new(),
            placeholder: None,
            rows: 2,
            field_width: 0,
            max_visible_chars: 0,
            full_width: false,
            completions: Vec::new(),
            completions_visible_rows: 0,
            block_caret: false,
            sel_start: -1,
            sel_end: -1,
            label_width: 0,
            read_only: false,
            markdown: true,
            key: Some("doc".into()),
        };
        // The doc is the only tabbable → auto-focused; the caret paints
        // as a reversed block cell on the first row. Deliberately NO
        // `focus_cursor`: publishing one would move the panel buffer's
        // real cursor, and the buffer viewport following it scrolled the
        // whole panel when the caret neared the bottom.
        let out = render_spec(&spec, &HashMap::new(), "", 30);
        assert!(
            out.focus_cursor.is_none(),
            "a markdown document must not publish a hardware cursor"
        );
        assert!(
            out.entries[0]
                .inline_overlays
                .iter()
                .any(|o| o.style.reversed),
            "caret renders as a reversed block cell"
        );
    }

    #[test]
    fn lists_emit_scroll_regions_even_when_they_fit() {
        // Wheel routing hit-tests the pointer against every keyed list's
        // region — a list that fits must still claim its geometry, or a
        // wheel over it gets rerouted to a scrollable sibling.
        let fits = make_list(-1, 10, 3, Some("fits"));
        let overflows = make_list(-1, 3, 10, Some("overflows"));
        let spec = WidgetSpec::Col {
            children: vec![fits, overflows],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let keys: Vec<(&str, bool)> = out
            .boxes
            .iter()
            .filter(|b| b.scroll.is_some())
            .map(|b| {
                let sc = b.scroll.unwrap();
                (b.key.as_deref().unwrap_or(""), sc.total > sc.visible)
            })
            .collect();
        assert_eq!(
            keys,
            vec![("fits", false), ("overflows", true)],
            "every keyed list surfaces a region; only the overflowing one scrolls"
        );
    }

    #[test]
    fn labeled_section_keeps_border_aligned_with_wide_glyphs() {
        // `漢` / `😀` are one char but two display columns. Char-counted
        // padding shifted the section's right border on every row that
        // contained one — pad in display columns so all rows line up.
        let wide = TextPropertyEntry::text("wide 漢😀 row");
        let narrow = TextPropertyEntry::text("narrow row");
        let spec = WidgetSpec::LabeledSection {
            label: "".into(),
            child: Box::new(WidgetSpec::Raw {
                entries: vec![wide, narrow],
                key: None,
            }),
            width_pct: None,
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 30);
        let widths: Vec<usize> = out
            .entries
            .iter()
            .map(|e| crate::primitives::display_width::str_width(e.text.trim_end_matches('\n')))
            .collect();
        assert!(
            widths.iter().all(|w| *w == widths[0]),
            "every section row must span the same display width: {widths:?}"
        );
    }

    #[test]
    fn labeled_section_pads_child_to_inner_width() {
        let spec = WidgetSpec::LabeledSection {
            label: "".into(),
            child: Box::new(make_text_input("hi", -1, false, false, 4, Some("n"))),
            width_pct: None,
            key: None,
        };
        let prev = HashMap::new();
        // panel_width = 16 → inner_width = 12 → middle row is
        // "│ " + 12 cols + " │".
        let out = render_spec(&spec, &prev, "", 16);
        let middle = &out.entries[1];
        // Count display columns including the borders + spaces.
        assert_eq!(middle.text.chars().count(), 16 + 1 /* \n */);
    }

    #[test]
    fn labeled_section_text_full_width_fills_inner_area() {
        // Inner width = 16 - 4 = 12. With no label on the input,
        // 3 cols of overhead (brackets + focus park) →
        // effective field_width = 9. The widget is the only
        // tabbable so the renderer marks it focused, padding the
        // inner region to field_width + 1 = 10 chars.
        let spec = WidgetSpec::LabeledSection {
            label: "".into(),
            child: Box::new(make_text_input("ab", -1, false, true, 0, Some("n"))),
            width_pct: None,
            key: None,
        };
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "", 16);
        let middle = &out.entries[1];
        // Middle row should be `│ [ab        ] │\n` — 17 chars
        // total (16 visible cols + trailing newline). When the
        // child fits exactly, the `]` is preserved.
        assert_eq!(middle.text.chars().count(), 17, "actual: {:?}", middle.text);
        assert!(
            middle.text.contains("[ab        ]"),
            "actual: {:?}",
            middle.text
        );
    }

    #[test]
    fn labeled_section_propagates_focus_cursor_with_offsets() {
        let spec = WidgetSpec::LabeledSection {
            label: "".into(),
            child: Box::new(make_text_input("abc", 3, true, false, 4, Some("n"))),
            width_pct: None,
            key: None,
        };
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "n", 20);
        let fc = out.focus_cursor.expect("focused child publishes cursor");
        // Child renders on the second row (top border = row 0).
        assert_eq!(fc.buffer_row, 1);
        // Cursor offset includes the left-prefix "│ " byte count
        // plus the child's own offset (1 for the opening bracket
        // + 3 for "abc"). "│" is 3 bytes in UTF-8 → prefix = 4.
        let prefix_bytes = LEFT_BORDER_PREFIX.len() as u32;
        assert_eq!(fc.byte_in_row, prefix_bytes + 1 + 3);
    }

    #[test]
    fn labeled_section_includes_child_in_tabbable() {
        let spec = WidgetSpec::Col {
            children: vec![
                WidgetSpec::LabeledSection {
                    label: "Name".into(),
                    child: Box::new(make_text_input("", -1, false, false, 0, Some("n"))),
                    width_pct: None,
                    key: None,
                },
                WidgetSpec::LabeledSection {
                    label: "Cmd".into(),
                    child: Box::new(make_text_input("", -1, false, false, 0, Some("c"))),
                    width_pct: None,
                    key: None,
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
            edit_text: None,
            edit_cursor: -1,
            edit_sel_start: -1,
            edit_sel_end: -1,
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
    fn number_emits_value_cell_hit_area() {
        let spec = make_number(2.0, Some("size"));
        let (_out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        let cells: Vec<_> = hits.iter().filter(|h| h.widget_kind == "number").collect();
        assert_eq!(cells.len(), 1, "one value-cell hit");
        assert_eq!(cells[0].event_type, "number_value");
    }

    #[test]
    fn a_number_render_clamps_without_recording_it() {
        let spec = WidgetSpec::Number {
            label_width: 0,
            edit_text: None,
            edit_cursor: -1,
            edit_sel_start: -1,
            edit_sel_end: -1,
            value: 42.0,
            min: Some(0.0),
            max: Some(10.0),
            step: 1.0,
            integer: true,
            percent: false,
            label: String::new(),
            focused: false,
            key: Some("n".into()),
        };
        let (out, _hits, state) = render_no_focus(&spec, &HashMap::new());
        // Spec value 42 clamps to max 10 in what is drawn — and is not written
        // down. The clamp is a derivation applied on every read, so persisting
        // it stored nothing a reader could not work out, while making the
        // render walk a second writer of a field the key and pointer handlers
        // own. See `kinds::number`.
        assert!(
            out[0].text.contains("10"),
            "the out-of-range value clamps to max: {:?}",
            out[0].text
        );
        assert!(
            state.get("n").is_none(),
            "and nothing is recorded: {:?}",
            state.get("n")
        );
    }

    #[test]
    fn number_instance_state_overrides_spec_value() {
        let spec = make_number(1.0, Some("n"));
        let mut prev = HashMap::new();
        prev.insert("n".to_string(), WidgetInstanceState::Number { value: 7.0 });
        let r = render_spec(&spec, &prev, "", u32::MAX);
        // The rendered value reflects instance state (7), not spec (1).
        assert!(
            r.entries[0].text.contains(" 7 "),
            "instance value should win: {:?}",
            r.entries[0].text
        );
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
    fn dropdown_open_emits_toggle_hit_and_floating_popup() {
        let spec = WidgetSpec::Dropdown {
            label_width: 0,
            open: true,
            scroll_offset: 0,
            options: vec!["a".into(), "b".into()],
            selected_index: 0,
            label: String::new(),
            focused: true,
            key: Some("d".into()),
        };
        let out = render_spec(&spec, &HashMap::new(), "d", u32::MAX);
        let toggles = out
            .hits
            .iter()
            .filter(|h| h.event_type == "dropdown_toggle")
            .count();
        assert_eq!(toggles, 1, "the trigger button stays a toggle hit");
        // Options no longer render inline — they surface on the floating
        // pop-over instead, so the panel has NO `dropdown_select` hits.
        assert!(
            !out.hits.iter().any(|h| h.event_type == "dropdown_select"),
            "open dropdown must not emit inline option hits"
        );
        let dp = out
            .popup
            .expect("an open dropdown surfaces a floating pop-over");
        assert_eq!(dp.widget_key, "d");
        assert_eq!(
            dp.entries
                .iter()
                .map(|e| e.text.as_str())
                .collect::<Vec<_>>(),
            vec![" a ", " b "],
            "rows arrive fully rendered (padded) with their indices"
        );
        assert_eq!(dp.row_indices, vec![0, 1]);
        assert_eq!(dp.anchor_row, 0, "trigger is the panel's row 0");
    }

    /// **Rendering clamps; it does not write the clamp down.**
    ///
    /// An out-of-range spec index still selects the last option — the clamp is
    /// applied on every read by `kinds::dropdown::resolve`, which is what both
    /// the painter and the description call. What changed is that the walk no
    /// longer *persists* the clamped value: a derived answer is not storage,
    /// and writing it back made the render walk an authority on state that the
    /// key and pointer handlers also write.
    ///
    /// So a spec that has never been interacted with contributes no entry at
    /// all. That is what "the spec is the seed until a handler makes a
    /// decision" means, and the web frontend depends on it — an absent entry
    /// is how it knows to show the spec's own value.
    #[test]
    fn a_render_clamps_the_selection_without_recording_it() {
        let spec = make_dropdown(&["a", "b", "c"], 9, Some("d"));
        let (out, _hits, state) = render_no_focus(&spec, &HashMap::new());
        assert!(
            out[0].text.contains("[c "),
            "the out-of-range index clamps to the last option: {:?}",
            out[0].text
        );
        assert!(
            state.get("d").is_none(),
            "and nothing is written down: {:?}",
            state.get("d")
        );
    }

    /// **What the walk still does for state is carry it.**
    ///
    /// `update_side_effects` replaces the whole map, so a widget the walk does
    /// not mention loses its state — which is the collection that drops the
    /// state of widgets a new spec no longer contains. A stored entry
    /// therefore has to survive a render that decides nothing, verbatim.
    #[test]
    fn a_render_carries_a_stored_selection_through_unchanged() {
        let spec = make_dropdown(&["a", "b", "c"], 0, Some("d"));
        let mut prev = HashMap::new();
        prev.insert(
            "d".to_string(),
            WidgetInstanceState::Dropdown {
                selected_index: 2,
                open: true,
            },
        );
        let (_out, _hits, state) = render_no_focus(&spec, &prev);
        match state.get("d") {
            Some(WidgetInstanceState::Dropdown {
                selected_index,
                open,
            }) => {
                assert_eq!(*selected_index, 2, "the stored index, not the spec's");
                assert!(
                    *open,
                    "and the stored flag verbatim — the focus gate is applied \
                     by every reader, not baked in here"
                );
            }
            other => panic!("expected the stored entry to survive, got {other:?}"),
        }
    }

    #[test]
    fn dropdown_instance_state_overrides_spec() {
        let spec = make_dropdown(&["a", "b", "c"], 0, Some("d"));
        let mut prev = HashMap::new();
        prev.insert(
            "d".to_string(),
            WidgetInstanceState::Dropdown {
                selected_index: 2,
                open: false,
            },
        );
        let r = render_spec(&spec, &prev, "", u32::MAX);
        assert!(
            r.entries[0].text.contains("[c "),
            "instance selection should win: {:?}",
            r.entries[0].text
        );
    }

    #[test]
    fn dropdown_open_surfaces_popup_not_inline_rows() {
        let spec = make_dropdown(&["a", "b", "c"], 1, Some("d"));
        // Focused + open in instance state → the option list floats as a
        // screen-level pop-over; the panel keeps only the compact trigger.
        let mut prev = HashMap::new();
        prev.insert(
            "d".to_string(),
            WidgetInstanceState::Dropdown {
                selected_index: 1,
                open: true,
            },
        );
        let out = render_spec(&spec, &prev, "d", u32::MAX);
        assert_eq!(
            out.entries.len(),
            1,
            "open dropdown keeps only the trigger row (no inline options)"
        );
        assert!(
            !out.hits.iter().any(|h| h.event_type == "dropdown_select"),
            "options moved to the pop-over — no inline select hits"
        );
        let dp = out.popup.expect("open dropdown surfaces a popup");
        assert_eq!(
            dp.entries
                .iter()
                .map(|e| e.text.as_str())
                .collect::<Vec<_>>(),
            vec![" a ", " b ", " c "]
        );
        assert_eq!(dp.row_indices, vec![0, 1, 2]);
        // The selected row carries its highlight as a rendered overlay
        // (bg set); unselected rows have fg-only styling.
        let has_bg = |i: usize| {
            dp.entries[i]
                .inline_overlays
                .iter()
                .any(|o| o.style.bg.is_some())
        };
        assert!(!has_bg(0) && has_bg(1) && !has_bg(2));
        assert_eq!(dp.anchor_row, 0);
    }

    #[test]
    fn popup_closes_when_unfocused() {
        let spec = make_dropdown(&["a", "b"], 0, Some("d"));
        let mut prev = HashMap::new();
        prev.insert(
            "d".to_string(),
            WidgetInstanceState::Dropdown {
                selected_index: 0,
                open: true,
            },
        );
        // Not the focused widget → popup suppressed, state closed.
        // (no-autofocus so the sole tabbable isn't auto-selected).
        let out = render_spec_no_autofocus(&spec, &prev, "", u32::MAX);
        assert!(out.overlays.is_empty());
        assert!(
            out.popup.is_none(),
            "an unfocused (closed) dropdown surfaces no pop-over"
        );
        match out.instance_states.get("d") {
            Some(WidgetInstanceState::Dropdown { open, .. }) => assert!(!open),
            other => panic!("expected Dropdown state, got {other:?}"),
        }
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

    /// Focused dual list seeded straight from the spec — the shape
    /// Settings renders, where the host owns cursor + active column and
    /// re-supplies them every frame.
    fn make_dual_focused(
        options: &[(&str, &str)],
        included: &[&str],
        active_included: bool,
        available_cursor: u32,
        included_cursor: u32,
        hint: &str,
    ) -> WidgetSpec {
        WidgetSpec::DualList {
            options: opts(options),
            included: included.iter().map(|s| s.to_string()).collect(),
            excluded: Vec::new(),
            label: "Elements".into(),
            focused: true,
            active_included,
            available_cursor,
            included_cursor,
            hint: hint.to_string(),
            visible_rows: 3,
            key: None,
        }
    }

    /// Rows the picker paints for a spec-seeded, focused dual list.
    fn dual_rows(spec: &WidgetSpec) -> Vec<String> {
        let (out, _hits, _state) = render_no_focus(spec, &HashMap::new());
        out.iter()
            .map(|e| e.text.trim_end_matches('\n').to_string())
            .collect()
    }

    #[test]
    fn dual_list_marks_cursor_and_active_column_with_glyphs() {
        // Cursor on "Beta" in Available; Included holds "Gamma".
        let spec = make_dual_focused(
            &[("a", "Alpha"), ("b", "Beta"), ("g", "Gamma")],
            &["g"],
            false,
            1,
            0,
            "",
        );
        let rows = dual_rows(&spec);
        // Header marks the Available column, not Included.
        let header = &rows[1];
        assert!(
            header.contains("▾ Available"),
            "active column unmarked: {header:?}"
        );
        assert!(
            !header.contains("▾ Included"),
            "idle column marked active: {header:?}"
        );
        // Body: filled marker on the Available cursor row, hollow one
        // on the idle Included cursor.
        assert!(rows[2].contains("  Alpha"), "row 0: {:?}", rows[2]);
        assert!(rows[2].contains("▹ Gamma"), "idle cursor: {:?}", rows[2]);
        assert!(rows[3].contains("▸ Beta"), "active cursor: {:?}", rows[3]);
    }

    #[test]
    fn dual_list_cursor_glyphs_follow_the_active_column() {
        let spec = make_dual_focused(
            &[("a", "Alpha"), ("b", "Beta"), ("g", "Gamma")],
            &["g"],
            true,
            1,
            0,
            "",
        );
        let rows = dual_rows(&spec);
        assert!(rows[1].contains("▾ Included"), "header: {:?}", rows[1]);
        // Filled marker moved to Included; Available keeps the hollow one.
        assert!(rows[2].contains("▸ Gamma"), "active cursor: {:?}", rows[2]);
        assert!(rows[3].contains("▹ Beta"), "idle cursor: {:?}", rows[3]);
    }

    #[test]
    fn dual_list_unfocused_renders_no_cursor_glyphs() {
        let spec = make_dual(&[("a", "Alpha")], &[], None);
        let joined = dual_rows(&spec).join("\n");
        assert!(!joined.contains('▸'), "{joined:?}");
        assert!(!joined.contains('▹'), "{joined:?}");
        assert!(!joined.contains('▾'), "{joined:?}");
    }

    #[test]
    fn dual_list_appends_hint_row_when_supplied() {
        let hint = "↑↓ Select  Shift+←→ Move item";
        let spec = make_dual_focused(&[("a", "Alpha")], &[], false, 0, 0, hint);
        let rows = dual_rows(&spec);
        assert_eq!(rows.last().map(|r| r.trim()), Some(hint));
        // ...and nothing extra when the host supplies no hint.
        let bare = make_dual_focused(&[("a", "Alpha")], &[], false, 0, 0, "");
        assert_eq!(dual_rows(&bare).len(), rows.len() - 1);
    }

    #[test]
    fn dual_list_cell_hits_cover_the_cursor_gutter() {
        // The gutter is part of the cell, so clicking the marker (or
        // the blank column reserved for it) selects that row.
        let spec = make_dual(&[("a", "Alpha"), ("b", "Beta")], &["b"], None);
        let (out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        let h = hits
            .iter()
            .find(|h| h.payload["column"] == "available")
            .expect("available cell hit");
        let row = &out[h.buffer_row as usize].text;
        let cell = &row[h.byte_start..h.byte_end];
        assert_eq!(h.byte_start, 0, "cell should start at the gutter");
        assert!(cell.starts_with("  "), "gutter not in the hit: {cell:?}");
        assert!(cell.contains("Alpha"), "label not in the hit: {cell:?}");
    }

    #[test]
    fn dual_list_renders_header_and_columns() {
        let spec = make_dual(&[("a", "Alpha"), ("b", "Beta")], &["b"], Some("d"));
        let (out, _hits, state) = render_no_focus(&spec, &HashMap::new());
        // Label + header + >=1 body rows.
        assert_eq!(out[0].text.trim_end(), "Elements");
        assert!(out[1].text.contains("Available"));
        assert!(out[1].text.contains("Included"));
        // Body shows Alpha in the available column and Beta in included.
        let body: String = out[2..].iter().map(|e| e.text.clone()).collect();
        assert!(body.contains("Alpha"), "available col: {body:?}");
        assert!(body.contains("Beta"), "included col: {body:?}");
        // Instance state seeded from spec.
        match state.get("d") {
            Some(WidgetInstanceState::DualList { included, .. }) => {
                assert_eq!(included, &vec!["b".to_string()]);
            }
            other => panic!("expected DualList state, got {other:?}"),
        }
    }

    #[test]
    fn dual_list_emits_cell_hit_areas() {
        let spec = make_dual(&[("a", "Alpha"), ("b", "Beta")], &["b"], Some("d"));
        let (_out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        let cells: Vec<_> = hits
            .iter()
            .filter(|h| h.widget_kind == "dual_list")
            .collect();
        // One available cell (a) + one included cell (b).
        assert_eq!(cells.len(), 2);
        assert!(cells.iter().any(|h| h.payload["column"] == "available"));
        assert!(cells.iter().any(|h| h.payload["column"] == "included"));
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

    fn boxed_list(key: &str, n: usize, visible: u32) -> WidgetSpec {
        WidgetSpec::List {
            items: (0..n)
                .map(|i| TextPropertyEntry::text(&format!("item{i}")))
                .collect(),
            item_specs: vec![],
            item_keys: (0..n).map(|i| format!("{key}{i}")).collect(),
            selected_index: -1,
            visible_rows: Some(visible),
            focusable: true,
            key: Some(key.to_string()),
        }
    }

    #[test]
    fn box_tree_mirrors_structure_rows_and_focus_ring() {
        use crate::widgets::layout_box::{focus_ring, hit_path};
        let spec = WidgetSpec::Col {
            key: None,
            children: vec![
                WidgetSpec::Button {
                    label: "Go".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: Some("b".into()),
                    disabled: false,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                },
                WidgetSpec::LabeledSection {
                    label: "Files".into(),
                    child: Box::new(boxed_list("l", 3, 5)),
                    width_pct: None,
                    key: None,
                },
            ],
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let boxes = &out.boxes;
        // Root-last arena: the outer Col caps the vec and spans the
        // whole surface.
        let root = boxes.last().expect("root box");
        assert_eq!(root.kind, "col");
        assert_eq!(root.parent, None);
        assert_eq!((root.row, root.col), (0, 0));
        assert_eq!(root.width, 40);
        assert_eq!(root.height as usize, out.entries.len());

        let find = |kind: &str| {
            boxes
                .iter()
                .position(|b| b.kind == kind)
                .unwrap_or_else(|| panic!("no {kind} box"))
        };
        let button = &boxes[find("button")];
        assert_eq!((button.row, button.height), (0, 1));
        assert!(button.focusable);
        let section = &boxes[find("labeled_section")];
        // Below the button: top border + 5 list rows + bottom border.
        assert_eq!((section.row, section.height), (1, 7));
        let list = &boxes[find("list")];
        // Inside the section: down past the top border, right past the
        // "| " border prefix; the section rendered it at width - 4,
        // then widened the scrollable box +2 through the right border
        // (wheel over the border scrolls the list, matching the
        // scroll-region widening).
        assert_eq!((list.row, list.col), (2, 2));
        assert_eq!((list.width, list.height), (38, 5));
        assert!(list.scrollable && list.focusable);
        assert_eq!(boxes[list.parent.unwrap()].kind, "labeled_section");

        // The derived focus ring reproduces the collected tabbable
        // list order-for-order — the invariant the phase-5 focus
        // unification stands on.
        assert_eq!(focus_ring(boxes), out.tabbable);
        assert_eq!(out.tabbable, vec!["b".to_string(), "l".to_string()]);

        // Hit-testing resolves through the structure: a point inside
        // the list's rows lands on the list via col -> section -> list.
        let path = hit_path(boxes, 3, 10);
        let kinds: Vec<&str> = path.iter().map(|&i| boxes[i].kind).collect();
        assert_eq!(kinds, vec!["col", "labeled_section", "list"]);
        // The button row resolves to the button.
        let path = hit_path(boxes, 0, 1);
        assert_eq!(*path.last().unwrap(), find("button"));
    }

    #[test]
    fn box_tree_row_zip_offsets_block_columns() {
        let spec = WidgetSpec::Row {
            key: None,
            children: vec![boxed_list("left", 2, 3), boxed_list("right", 2, 3)],
            wrap: false,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let lists: Vec<&LayoutBox> = out.boxes.iter().filter(|b| b.kind == "list").collect();
        assert_eq!(lists.len(), 2);
        // Two zip blocks split the width; the right one starts at the
        // left one's column budget.
        assert_eq!((lists[0].row, lists[0].col, lists[0].width), (0, 0, 20));
        assert_eq!((lists[1].row, lists[1].col, lists[1].width), (0, 20, 20));
        // Side-by-side hit-tests pick the correct list.
        use crate::widgets::layout_box::hit_path;
        let left_hit = hit_path(&out.boxes, 1, 5);
        let right_hit = hit_path(&out.boxes, 1, 25);
        assert_eq!(
            out.boxes[*left_hit.last().unwrap()].key.as_deref(),
            Some("left")
        );
        assert_eq!(
            out.boxes[*right_hit.last().unwrap()].key.as_deref(),
            Some("right")
        );
    }

    #[test]
    fn box_tree_overlay_promotion_bumps_z_and_wins_hits() {
        use crate::widgets::layout_box::hit_path;
        let spec = WidgetSpec::Col {
            key: None,
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![
                        TextPropertyEntry::text("base0"),
                        TextPropertyEntry::text("base1"),
                        TextPropertyEntry::text("base2"),
                    ],
                    key: None,
                },
                WidgetSpec::Overlay {
                    key: None,
                    child: Box::new(WidgetSpec::Raw {
                        entries: vec![TextPropertyEntry::text("popup")],
                        key: None,
                    }),
                },
            ],
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let overlay = out
            .boxes
            .iter()
            .find(|b| b.kind == "overlay")
            .expect("overlay box");
        // Promoted: anchored where the col cursor stood (after 3 raw
        // rows), one stacking level up, opaque to fall-through.
        assert_eq!(overlay.z, 1);
        assert!(overlay.pointer_opaque);
        assert_eq!(overlay.row, 3);
        // Its raw child rides along at the same z.
        let raws: Vec<&LayoutBox> = out.boxes.iter().filter(|b| b.kind == "raw").collect();
        assert!(raws.iter().any(|b| b.z == 1 && b.row == 3));
        // A hit at the overlay's anchor row resolves to the promoted
        // subtree, not the base surface.
        let path = hit_path(&out.boxes, 3, 2);
        assert_eq!(out.boxes[*path.last().unwrap()].z, 1);
    }
    // -------------------------------------------------------------
    // WidgetImpl::on_wheel (phase 4 dispatch)
    // -------------------------------------------------------------

    fn wheel_panel(spec: &WidgetSpec) -> crate::widgets::WidgetPanelState {
        let out = render_spec(spec, &HashMap::new(), "", 40);
        crate::widgets::WidgetPanelState {
            buffer_id: crate::model::event::BufferId(1),
            spec: spec.clone(),
            hits: out.hits,
            instance_states: out.instance_states,
            focus_key: out.focus_key,
            tabbable: out.tabbable,
            effective_rows: out.effective_rows,
            boxes: out.boxes,
        }
    }

    #[test]
    fn list_on_wheel_consumes_until_bound_then_chains() {
        use crate::widgets::kinds::behavior;
        // 6 items, 3 visible → max_scroll 3.
        let spec = boxed_list("l", 6, 3);
        let mut panel = wheel_panel(&spec);
        // Consume 3 notches down…
        for i in 1..=3 {
            assert!(
                behavior(&spec).on_wheel(&spec, "l", &mut panel, 1),
                "notch {i} should scroll"
            );
        }
        // …then the bound is hit: the wheel is NOT consumed, so the
        // dispatcher keeps bubbling (scroll chaining) instead of the
        // event going dead on a maxed-out list.
        assert!(!behavior(&spec).on_wheel(&spec, "l", &mut panel, 1));
        // Back up consumes again.
        assert!(behavior(&spec).on_wheel(&spec, "l", &mut panel, -1));
    }

    #[test]
    fn fitting_list_on_wheel_never_consumes() {
        use crate::widgets::kinds::behavior;
        // Everything visible (Git Log shape): nothing to scroll, the
        // wheel must fall through to the enclosing pane.
        let spec = boxed_list("l", 3, 10);
        let mut panel = wheel_panel(&spec);
        assert!(!behavior(&spec).on_wheel(&spec, "l", &mut panel, 1));
        assert!(!behavior(&spec).on_wheel(&spec, "l", &mut panel, -1));
    }
    #[test]
    fn box_tree_carries_popup_pseudo_boxes() {
        // Completion popup: a focused Text with candidates in instance
        // state renders overlay rows AND a z=1 opaque box spanning
        // them (separator + items + bottom border).
        let spec = make_text_area("qu", 2, true, 1, 40, Some("q"));
        let mut prev = HashMap::new();
        prev.insert(
            "q".into(),
            WidgetInstanceState::Text {
                editor: crate::primitives::text_edit::TextEdit::with_text("qu"),
                scroll: 0,
                completions: vec!["quick".to_string().into(), "quiet".to_string().into()],
                completion_selected_index: 0,
                completion_scroll_offset: 0,
                completion_navigated: false,
                user_scrolled: false,
            },
        );
        let out = render_spec(&spec, &prev, "q", 40);
        let popup = out
            .boxes
            .iter()
            .find(|b| b.kind == "text_completions")
            .expect("completion popup box");
        assert_eq!(popup.z, 1);
        assert!(popup.pointer_opaque);
        // Anchored one row below the field: separator + 2 items + border.
        assert_eq!((popup.row, popup.height), (1, 4));
        assert!(!out.overlays.is_empty(), "popup rows exist");
        // Its parent is the Text box itself — the popup belongs to the
        // field, deeper in the tree, which is what lets depth-first
        // dispatch reach it without a short-circuit.
        assert_eq!(out.boxes[popup.parent.unwrap()].kind, "text");

        // Dropdown pop-over: open state renders the screen-space box.
        let spec = WidgetSpec::Dropdown {
            options: vec!["a".into(), "b".into()],
            selected_index: 0,
            label: "Pick".into(),
            focused: true,
            label_width: 0,
            open: false,
            scroll_offset: 0,
            key: Some("dd".into()),
        };
        let mut prev = HashMap::new();
        prev.insert(
            "dd".into(),
            WidgetInstanceState::Dropdown {
                selected_index: 0,
                open: true,
            },
        );
        let out = render_spec(&spec, &prev, "dd", 40);
        let popup = out
            .boxes
            .iter()
            .find(|b| b.kind == "dropdown_popup")
            .expect("dropdown popup box");
        assert!(popup.screen_space);
        assert_eq!(popup.z, 2);
        assert!(out.popup.is_some(), "side channel still feeds paint");
        assert_eq!(out.boxes[popup.parent.unwrap()].kind, "dropdown");
    }

    #[test]
    fn screen_space_popup_rides_the_popup_channel() {
        // A plugin `Popup { screen_space: true }` contributes no inline
        // rows; its child renders through the generalized PanelPopup
        // channel (the one the Dropdown pop-over rides) with no
        // click-routing indices, and an explicit anchor is absolute —
        // the enclosing Col must not shift it with the flow.
        let spec = WidgetSpec::Col {
            key: None,
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![TextPropertyEntry::text("row0")],
                    key: None,
                },
                WidgetSpec::Popup {
                    child: Box::new(WidgetSpec::Raw {
                        entries: vec![TextPropertyEntry::text("float me")],
                        key: None,
                    }),
                    key: Some("pp".into()),
                    anchor: Some([2, 3]),
                    screen_space: true,
                },
            ],
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        let dp = out.popup.as_ref().expect("screen-space popup surfaces");
        assert_eq!(dp.widget_key, "pp");
        assert_eq!((dp.anchor_row, dp.anchor_col), (2, 3), "anchor absolute");
        assert_eq!(dp.entries.len(), 1);
        assert!(dp.entries[0].text.starts_with("float me"));
        assert!(dp.row_indices.is_empty(), "generic rows get no select hits");
        // Only the sibling Raw row flows inline.
        assert_eq!(out.entries.len(), 1);
        let pb = out
            .boxes
            .iter()
            .find(|b| b.kind == "panel_popup")
            .expect("screen-space popup box");
        assert!(pb.screen_space);
    }

    /// A plugin `Popup { screen_space: false }` documents itself as
    /// riding the promoted-overlay path: its rows must FLOAT (overlay
    /// channel, not inline column flow), its hits are stamped overlay,
    /// and its boxes get the overlay z bump — which is what arms its
    /// `pointer_opaque` box, since the panel opacity probe requires
    /// z > 0. The Col promotion match once listed only `Overlay`,
    /// leaving all three unwired for this kind.
    #[test]
    fn panel_clipped_popup_promotes_like_overlay() {
        let spec = WidgetSpec::Col {
            key: None,
            children: vec![
                WidgetSpec::Raw {
                    entries: vec![TextPropertyEntry::text("row0")],
                    key: None,
                },
                WidgetSpec::Popup {
                    child: Box::new(WidgetSpec::Raw {
                        entries: vec![TextPropertyEntry::text("float me")],
                        key: None,
                    }),
                    key: Some("pp".into()),
                    anchor: None,
                    screen_space: false,
                },
            ],
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        // Only the sibling Raw row flows inline; the popup's row floats.
        assert_eq!(
            out.entries.len(),
            1,
            "panel-clipped popup rows must not consume column flow"
        );
        assert!(
            out.overlays
                .iter()
                .any(|o| o.entry.text.starts_with("float me")),
            "panel-clipped popup row rides the overlay channel"
        );
        let pb = out
            .boxes
            .iter()
            .find(|b| b.kind == "popup")
            .expect("panel-clipped popup box collected");
        assert!(pb.pointer_opaque, "popup box is opaque");
        assert!(pb.z > 0, "promotion bumps z so the opacity probe sees it");
    }

    #[test]
    fn component_is_a_transparent_focus_trap() {
        use crate::widgets::layout_box::focus_ring_scoped;
        let btn = |k: &str| WidgetSpec::Button {
            label: k.to_uppercase(),
            focused: false,
            intent: ButtonKind::Normal,
            key: Some(k.into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
        };
        let spec = WidgetSpec::Col {
            key: None,
            children: vec![
                btn("outside"),
                WidgetSpec::Component {
                    key: Some("dialog".into()),
                    child: Box::new(WidgetSpec::Col {
                        key: None,
                        children: vec![btn("ok"), btn("cancel")],
                    }),
                },
            ],
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        // Transparent: rows identical to rendering without the wrapper,
        // and the published ring still sees every focusable.
        assert_eq!(out.tabbable, vec!["outside", "ok", "cancel"]);
        let comp = out
            .boxes
            .iter()
            .find(|b| b.kind == "component")
            .expect("component box");
        assert!(comp.focus_trap);
        assert_eq!(comp.key.as_deref(), Some("dialog"));
        // Tab cycling from inside the component stays inside it; from
        // outside, the whole panel ring applies.
        assert_eq!(focus_ring_scoped(&out.boxes, "ok"), vec!["ok", "cancel"]);
        assert_eq!(
            focus_ring_scoped(&out.boxes, "outside"),
            vec!["outside", "ok", "cancel"]
        );
    }
    #[test]
    fn col_flex_spacer_absorbs_leftover_height() {
        // col(button, flexSpacer, button) with a 6-row budget: the
        // spacer stretches so the second button lands on the last row
        // — the "pin to bottom" pattern without plugin arithmetic.
        let btn = |k: &str| WidgetSpec::Button {
            label: k.to_uppercase(),
            focused: false,
            intent: ButtonKind::Normal,
            key: Some(k.into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
        };
        let spec = WidgetSpec::Col {
            key: None,
            children: vec![
                btn("top"),
                WidgetSpec::Spacer {
                    cols: 0,
                    flex: true,
                    key: None,
                },
                btn("bottom"),
            ],
        };
        let out = render_spec_with_options(
            &spec,
            &HashMap::new(),
            40,
            RenderOptions {
                avail_height: Some(6),
                ..Default::default()
            },
        );
        assert_eq!(out.entries.len(), 6, "col fills its budget");
        assert!(
            out.entries[5].text.contains("BOTTOM"),
            "second button pinned to the last row: {:?}",
            out.entries
                .iter()
                .map(|e| e.text.as_str())
                .collect::<Vec<_>>()
        );
        // Without a budget the spacer stays its natural 1-row self.
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        assert_eq!(out.entries.len(), 3);
    }
}
