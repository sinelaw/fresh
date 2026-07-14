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

use crate::widgets::registry::{HitArea, WidgetInstanceState};
use fresh_core::api::{
    ButtonKind, DualListOption, HintEntry, OverlayColorSpec, OverlayOptions, TreeNode, WidgetSpec,
};
use fresh_core::text_property::{InlineOverlay, OffsetUnit, TextPropertyEntry};
use serde_json::json;
use std::collections::{HashMap, HashSet};

// Theme keys used by the v1 widget renderers. Centralized so future
// "role-based" theming (§7 of the design doc) has one place to
// substitute the role→key mapping.
const KEY_HELP_KEY_FG: &str = "ui.help_key_fg";
// Foreground of a checked Toggle's `[v]` glyph. `ui.help_key_fg`
// is the "keyboard-key / highlight on a popup body" theme key —
// every shipped theme picks a colour that contrasts with
// `ui.popup_bg`. The previous choice (`ui.tab_active_fg`) was
// designed to contrast with `tab_active_bg`, not the popup body;
// in `high-contrast` both ended up black so the `[v]` glyph
// vanished on every unfocused toggle. `help_key_fg` keeps the
// emphasis intent (a bright accent colour) while reliably
// surviving the popup background.
const KEY_TOGGLE_ON_FG: &str = "ui.help_key_fg";
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
const KEY_FOCUSED_FG: &str = "ui.popup_selection_fg";
const KEY_FOCUSED_BG: &str = "ui.popup_selection_bg";
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

/// The two-column gutter prefix a focusable control leads with when
/// the current render reserves the focus-marker gutter
/// ([`MARKER_GUTTER`]): `▸ ` for the focused control, two spaces for
/// every other control. Returns `""` when the panel didn't opt into
/// the gutter, so non-marker panels render byte-for-byte as before.
fn focus_gutter_prefix(focused: bool) -> &'static str {
    if !marker_gutter_enabled() {
        ""
    } else if focused {
        FOCUS_MARKER
    } else {
        FOCUS_GUTTER_BLANK
    }
}
// `ui.status_error_indicator_fg` defaults to white (designed as
// the text-on-red status badge), so using it as a standalone fg
// renders invisible against the panel bg. The diagnostic.error_fg
// key is the canonical "red text" theme slot.
const KEY_DANGER_FG: &str = "diagnostic.error_fg";
const KEY_INPUT_BG: &str = "ui.prompt_bg";
// Background tint for the selection span inside a widget Text
// input. Distinct from the buffer's `ui.selection_bg` because
// widget inputs sit on top of the `ui.prompt_bg` field-bg overlay
// and the contrast needs to read against that tint, not the
// editor surface.
const KEY_TEXT_INPUT_SELECTION_BG: &str = "ui.text_input_selection_bg";
// Placeholder text uses the whitespace-indicator key — a dimmer
// grey than `ui.menu_disabled_fg` (themes ship ~RGB(70,70,70)
// vs ~RGB(100,100,100) for disabled menu items), so hint copy
// reads as background guidance rather than a half-active value.
const KEY_PLACEHOLDER_FG: &str = "editor.whitespace_indicator_fg";
// Section-legend tint. `ui.help_key_fg` is the same key the
// hint-bar uses to highlight keys against panel bg, so we know
// it's tuned for readability against the same surface a
// LabeledSection sits on.
const KEY_SECTION_LABEL_FG: &str = "ui.help_key_fg";
// Dim separator that replaces the input's bottom border when the
// completion popup is open. `ui.menu_disabled_fg` is the closest
// "muted chrome" key already shipped by every theme (gray-ish in
// dark themes, light gray in light themes) so the separator reads
// as a recessed transition between the active input and the
// candidate list rather than as a hard divider.
const KEY_COMPLETION_DIM_FG: &str = "ui.menu_disabled_fg";
// Selected completion row foreground/background. Same keys the
// popup-driven selection highlight uses everywhere else (host
// prompt suggestions, action-popup menu), so themes that
// re-skin one re-skin the other.
const KEY_COMPLETION_SEL_FG: &str = "ui.popup_selection_fg";
const KEY_COMPLETION_SEL_BG: &str = "ui.popup_selection_bg";
// Foreground for *unselected* completion rows. Without this, the
// row text inherits the terminal's default foreground, which has
// no relationship to the popup's themed `popup_bg` and reads
// poorly on coloured backgrounds.
const KEY_COMPLETION_FG: &str = "ui.popup_text_fg";
// Border chrome the popup paints around its own rows (the
// `│ ... │` sides extending below the input + the `╰─...─╯`
// closing border). Distinct theme key from the wrapping
// labeled section's default (unstyled) chrome so the popup
// reads as its own surface — matches the user's "use a theme
// key for the popup border" expectation.
const KEY_COMPLETION_BORDER_FG: &str = "ui.popup_border_fg";

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
    /// Scrollable `List` widgets that overflowed their visible height,
    /// with the geometry + state the host needs to paint and drag a
    /// scrollbar. Empty for lists that fit.
    pub scroll_regions: Vec<ScrollRegion>,
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

/// A scrollable `List` widget's geometry + scroll state, surfaced so
/// the host can paint a draggable scrollbar over the list's rightmost
/// column and hit-test mouse press/drag against it. Threaded through
/// the compositor (Row/Col/Section) identically to [`EmbedRect`] —
/// `buffer_row`/`col_in_row` are panel-relative display coordinates.
/// `width_cols` spans the list's column so `col_in_row + width_cols -
/// 1` is the scrollbar column; `height_rows` is the visible track
/// height. `total`/`visible`/`scroll` feed `ScrollbarState`.
#[derive(Debug, Clone)]
pub struct ScrollRegion {
    pub list_key: String,
    pub buffer_row: u32,
    pub col_in_row: u32,
    pub width_cols: u32,
    pub height_rows: u32,
    pub total: usize,
    pub visible: usize,
    pub scroll: usize,
}

/// Output of a single [`render_collected`] call (or one of the
/// standalone arm helpers). Replaces the six-element tuple that was
/// the previous return type, giving call sites named fields instead
/// of positional slots.
#[derive(Default)]
struct CollectedOutput {
    entries: Vec<TextPropertyEntry>,
    hits: Vec<HitArea>,
    focus_cursor: Option<FocusCursor>,
    embeds: Vec<EmbedRect>,
    overlays: Vec<OverlayRow>,
    scroll_regions: Vec<ScrollRegion>,
}

/// Render a spec to a [`RenderOutput`].
///
/// `prev` is the previous render's instance state (or empty on
/// first mount). `prev_focus_key` is the previous render's focus
/// key (or `""`); the renderer keeps it if it matches a tabbable in
/// the new spec, otherwise falls back to the first tabbable.
/// `panel_width` is the buffer's column width — used by `Row` to
/// size flex `Spacer`s. Pass `u32::MAX` to disable flex (children
/// won't be padded).
pub fn render_spec(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    let _guard = MarkerGutterGuard::set(false);
    render_spec_inner(spec, prev, prev_focus_key, panel_width, true)
}

// Whether the *current* render reserves a leading two-column gutter
// on every focusable control for the `▸ ` focus marker. Opt-in per
// panel (see `render_spec_with_marker`): when on, the focused
// control leads with `▸ ` and every other focusable control leads
// with two spaces, so focus is legible from a plain capture AND the
// layout never shifts as focus moves (the gutter is always present,
// only its glyph changes). When off — the default for every existing
// panel — controls render exactly as before (no gutter, no marker),
// so other dialogs are byte-for-byte unchanged. A thread-local keeps
// the flag out of the ~dozen recursive `collect_*` signatures; it's
// read only by the three leaf renderers (`render_button`,
// `render_toggle`, `render_widget_text`). Rendering is synchronous
// and non-re-entrant, so a thread-local with a restore guard is
// sufficient.
thread_local! {
    static MARKER_GUTTER: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

fn marker_gutter_enabled() -> bool {
    MARKER_GUTTER.with(|c| c.get())
}

/// RAII guard that sets the marker-gutter thread-local for the
/// duration of one render and restores the previous value on drop —
/// so a direct `render_button` call after a marker render doesn't
/// observe a stale `true`.
struct MarkerGutterGuard(bool);
impl MarkerGutterGuard {
    fn set(enabled: bool) -> Self {
        let prev = MARKER_GUTTER.with(|c| c.replace(enabled));
        MarkerGutterGuard(prev)
    }
}
impl Drop for MarkerGutterGuard {
    fn drop(&mut self) {
        MARKER_GUTTER.with(|c| c.set(self.0));
    }
}

/// Like [`render_spec`], but reserves the `▸ ` focus-marker gutter on
/// every focusable control (see [`MARKER_GUTTER`]). Panels that want
/// capture-legible, layout-stable focus (the Orchestrator New Session
/// form) render through this entry point; everything else uses
/// [`render_spec`] and is unaffected.
pub fn render_spec_with_marker(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    let _guard = MarkerGutterGuard::set(true);
    render_spec_inner(spec, prev, prev_focus_key, panel_width, true)
}

/// Like [`render_spec`] but does **not** fall back to focusing the first
/// tabbable widget when `focus_key` matches none. Use this when the host owns
/// the focus ring and a state of "no widget focused" is meaningful — e.g. the
/// search overlay, where focus can rest on the input (no toggle highlighted)
/// rather than always on a toolbar control. Pass `""` for no focus.
pub fn render_spec_no_autofocus(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> RenderOutput {
    let _guard = MarkerGutterGuard::set(false);
    render_spec_inner(spec, prev, focus_key, panel_width, false)
}

fn render_spec_inner(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    prev_focus_key: &str,
    panel_width: u32,
    auto_focus_first: bool,
) -> RenderOutput {
    // Walk the spec to collect tabbable keys, then resolve the
    // active focus key. This must happen before the entry pass so
    // that widget arms know whether they're focused.
    let mut tabbable = Vec::new();
    collect_tabbable(spec, &mut tabbable);
    let focus_key = if !prev_focus_key.is_empty() && tabbable.iter().any(|k| k == prev_focus_key) {
        prev_focus_key.to_string()
    } else if auto_focus_first {
        tabbable.first().cloned().unwrap_or_default()
    } else {
        String::new()
    };

    let mut next_state = HashMap::new();
    let collected = render_collected(spec, prev, &mut next_state, &focus_key, panel_width);
    RenderOutput {
        entries: collected.entries,
        hits: collected.hits,
        instance_states: next_state,
        focus_key,
        tabbable,
        focus_cursor: collected.focus_cursor,
        embeds: collected.embeds,
        overlays: collected.overlays,
        scroll_regions: collected.scroll_regions,
    }
}

/// Predict whether a `WidgetSpec` will render as a multi-line
/// (Block) child of a Row, without doing the actual render. The
/// Row's layout uses this up-front to decide whether a child
/// should get its full `panel_width` (inline path) or a smaller
/// per-column budget (horizontal-zip path).
///
/// Slightly conservative — a `Col` with one inline child is
/// predicted inline (matches its actual one-line render); a `Row`
/// containing any block descendant is predicted block (so nested
/// rows participate in the zip correctly).
/// Extract the `width_pct` declaration of a Row child, if any
/// and in-range (1..=100). Currently only `LabeledSection`
/// carries this — other block kinds (Col, Tree, List,
/// multi-line Text, Raw) participate in the equal-split path.
/// Out-of-range (0, > 100, or unset) collapses to `None` so
/// callers don't have to re-check.
fn labeled_section_width_pct(spec: &WidgetSpec) -> Option<u32> {
    let WidgetSpec::LabeledSection { width_pct, .. } = spec else {
        return None;
    };
    width_pct.filter(|pct| (1..=100).contains(pct))
}

fn predicts_block(spec: &WidgetSpec) -> bool {
    match spec {
        WidgetSpec::Col { children, .. } => {
            if children.len() > 1 {
                return true;
            }
            children.first().map(predicts_block).unwrap_or(false)
        }
        WidgetSpec::LabeledSection { .. } => true,
        WidgetSpec::Tree { .. } => true,
        WidgetSpec::List { .. } => true,
        WidgetSpec::Text { rows, .. } => *rows > 1,
        WidgetSpec::WindowEmbed { rows, .. } => *rows > 1,
        WidgetSpec::Raw { entries, .. } => entries.len() > 1,
        WidgetSpec::Row { children, .. } => children.iter().any(predicts_block),
        _ => false,
    }
}

/// One position in a Row's two-pass layout. Used internally to
/// defer flex-spacer sizing until after we know all the inline
/// children's natural widths.
enum RowPiece {
    Inline {
        entry: TextPropertyEntry,
        hits: Vec<HitArea>,
        /// Some when this inline child was a focused TextInput.
        /// `byte_in_row` is the cursor's offset within the *child's*
        /// text — the Row collapse pass shifts it by the merged
        /// inline_shift before publishing.
        focus_cursor: Option<FocusCursor>,
        /// Embed rects propagated up from this inline child.
        /// Inlines collapse to row 0, so embeds inside them are
        /// pinned to that row. Rare but worth carrying through
        /// rather than dropping.
        embeds: Vec<EmbedRect>,
        /// Scroll regions propagated up from this inline child.
        scroll_regions: Vec<ScrollRegion>,
    },
    Block {
        /// Allocated column width for the zip path. May differ
        /// from the entries' natural widths (each block was
        /// rendered with this as its `panel_width`, so the
        /// entries should already fit).
        column_width: u32,
        entries: Vec<TextPropertyEntry>,
        hits: Vec<HitArea>,
        focus_cursor: Option<FocusCursor>,
        /// Embed rects propagated up from this block child.
        /// Their `buffer_row` is already relative to the block's
        /// own row 0; the zip pass shifts row by `starting_row`
        /// and byte_in_row by the block's `byte_shift`.
        embeds: Vec<EmbedRect>,
        /// Scroll regions propagated up from this block child,
        /// shifted by the zip pass identically to `embeds`.
        scroll_regions: Vec<ScrollRegion>,
    },
    Flex,
}

/// Strip a trailing `'\n'` from `entry.text` if present (overlays /
/// hits aren't affected because the newline is at the very end and
/// no overlay should span it). Used to prepare an inline-rendered
/// child for Row inline-collapse, where individual newlines would
/// split the merged row across multiple buffer lines.
fn strip_trailing_newline(entry: &mut TextPropertyEntry) {
    if entry.text.ends_with('\n') {
        entry.text.pop();
    }
}

/// Append a single trailing newline to `entry.text` if it doesn't
/// already end with one. Each top-level entry needs to end with
/// `\n` so it occupies its own line in the underlying virtual
/// buffer (the buffer's line model is byte-driven; without `\n`
/// adjacent entries concatenate into one logical line).
fn ensure_trailing_newline(entry: &mut TextPropertyEntry) {
    if !entry.text.ends_with('\n') {
        entry.text.push('\n');
    }
}

/// Walk a spec tree and append tabbable widget keys (`Toggle`,
/// `Button`, `TextInput`, `List`, `Tree` with a non-empty `key`) in
/// declaration order. Layout containers (`Row`, `Col`) recurse;
/// `Raw`, `Spacer`, `HintBar` skip.
fn collect_tabbable(spec: &WidgetSpec, out: &mut Vec<String>) {
    match spec {
        WidgetSpec::Button {
            key: Some(k),
            disabled,
            focusable,
            ..
        } if !k.is_empty() && !*disabled && *focusable => {
            out.push(k.clone());
        }
        WidgetSpec::Toggle { key: Some(k), .. }
        | WidgetSpec::Number { key: Some(k), .. }
        | WidgetSpec::Dropdown { key: Some(k), .. }
        | WidgetSpec::DualList { key: Some(k), .. }
        | WidgetSpec::Text { key: Some(k), .. }
        | WidgetSpec::Tree { key: Some(k), .. }
            if !k.is_empty() =>
        {
            out.push(k.clone());
        }
        WidgetSpec::List {
            key: Some(k),
            focusable,
            ..
        } if !k.is_empty() && *focusable => {
            out.push(k.clone());
        }
        _ => {}
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
fn render_collected(
    spec: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    match spec {
        WidgetSpec::Row { children, wrap, .. } => {
            collect_row(children, *wrap, prev, next_state, focus_key, panel_width)
        }
        WidgetSpec::Col { children, .. } => {
            collect_col(children, prev, next_state, focus_key, panel_width)
        }
        WidgetSpec::HintBar { entries, .. } => collect_hint_bar(entries),
        WidgetSpec::Toggle {
            checked,
            label,
            focused,
            indeterminate,
            label_first,
            label_width,
            key,
        } => collect_toggle(
            *checked,
            label,
            *focused,
            *indeterminate,
            *label_first,
            *label_width,
            key.as_deref(),
            focus_key,
            panel_width,
        ),
        WidgetSpec::Number {
            value,
            min,
            max,
            integer,
            percent,
            label,
            focused,
            label_width,
            edit_text,
            edit_cursor,
            edit_sel_start,
            edit_sel_end,
            key,
            ..
        } => collect_number(
            *value,
            *min,
            *max,
            *integer,
            *percent,
            label,
            *focused,
            *label_width,
            edit_text.as_deref().map(|t| NumberEdit {
                text: t,
                cursor: *edit_cursor,
                sel_start: *edit_sel_start,
                sel_end: *edit_sel_end,
            }),
            key.as_deref(),
            prev,
            next_state,
            focus_key,
        ),
        WidgetSpec::Dropdown {
            options,
            selected_index,
            label,
            focused,
            label_width,
            open,
            scroll_offset,
            key,
        } => collect_dropdown(
            options,
            *selected_index,
            label,
            *focused,
            *label_width,
            *open,
            *scroll_offset,
            key.as_deref(),
            prev,
            next_state,
            focus_key,
        ),
        WidgetSpec::DualList {
            options,
            included,
            excluded,
            label,
            focused,
            visible_rows,
            key,
        } => collect_dual_list(
            options,
            included,
            excluded,
            label,
            *focused,
            *visible_rows,
            key.as_deref(),
            prev,
            next_state,
            focus_key,
            panel_width,
        ),
        WidgetSpec::Button {
            label,
            focused,
            intent,
            key,
            disabled,
            ..
        } => collect_button(
            label,
            *focused,
            *intent,
            key.as_deref(),
            *disabled,
            focus_key,
        ),
        WidgetSpec::Spacer { cols, .. } => collect_spacer(*cols),
        WidgetSpec::Divider { ch, style, .. } => collect_divider(ch, style.as_ref(), panel_width),
        WidgetSpec::List {
            items,
            item_specs,
            item_keys,
            selected_index,
            visible_rows,
            key: list_key,
            ..
        } => collect_list(
            items,
            item_specs,
            item_keys,
            *selected_index,
            *visible_rows,
            list_key.as_deref(),
            prev,
            next_state,
            focus_key,
            panel_width,
        ),
        WidgetSpec::Tree {
            nodes,
            item_keys,
            selected_index,
            visible_rows,
            expanded_keys,
            checkable,
            item_height,
            key: tree_key,
        } => render_widget_tree(
            nodes,
            item_keys,
            *selected_index,
            *visible_rows,
            expanded_keys,
            *checkable,
            *item_height,
            tree_key.as_deref(),
            prev,
            next_state,
            panel_width,
        ),
        WidgetSpec::Text {
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
            key,
        } => render_widget_text(
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
            key.as_deref(),
            prev,
            next_state,
            focus_key,
            panel_width,
        ),
        WidgetSpec::LabeledSection { label, child, .. } => {
            collect_labeled_section(label, child, prev, next_state, focus_key, panel_width)
        }
        WidgetSpec::WindowEmbed {
            window_id, rows, ..
        } => collect_window_embed(*window_id, *rows, panel_width),
        WidgetSpec::Raw { entries, .. } => collect_raw(entries),
        WidgetSpec::Overlay { child, .. } => {
            collect_overlay(child, prev, next_state, focus_key, panel_width)
        }
    }
}

// =========================================================================
// Standalone arm helpers — extracted from the render_collected match to keep
// that function navigable. Each returns a CollectedOutput the caller folds
// back into its local accumulators.
// =========================================================================

#[allow(clippy::too_many_arguments)]
fn collect_row(
    children: &[WidgetSpec],
    wrap: bool,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut focus_cursor: Option<FocusCursor> = None;
    let mut embeds: Vec<EmbedRect> = Vec::new();
    let mut overlays: Vec<OverlayRow> = Vec::new();
    let mut scroll_regions: Vec<ScrollRegion> = Vec::new();

    // Two-pass layout for Row:
    //  1. Walk children, render each. Track flex spacers
    //     by index in the accumulator; their text starts
    //     empty and grows in pass 2.
    //  2. Compute leftover width = panel_width - sum of
    //     non-flex widths; distribute evenly across flex
    //     slots; expand each flex spacer's text + shift
    //     subsequent overlays / hits accordingly.
    //
    // When ≥1 child is multi-line (a `Block`), the
    // assembly switches to a per-line zip instead of
    // the inline-collapse path — each block gets a
    // column budget and the layout walks block lines
    // left-to-right. See [the Phase 1b note in
    // docs/internal/orchestrator-open-dialog-and-lifecycle.md]
    // for the rationale.
    //
    // Width allocation for the zip path: blocks share
    // `panel_width`. Children with a `width_pct`
    // declaration get their explicit share first
    // (`panel_width * pct / 100`); the remainder splits
    // equally among blocks without an explicit width.
    // Inline children render at full `panel_width` (they
    // collapse to a single line so width is a soft cap).
    let per_child_width = allocate_row_child_widths(children, panel_width);
    let mut row_pieces: Vec<RowPiece> = Vec::new();
    for (idx, child) in children.iter().enumerate() {
        if let WidgetSpec::Spacer { flex: true, .. } = child {
            row_pieces.push(RowPiece::Flex);
            continue;
        }
        let child_panel_width = per_child_width[idx];
        let child_out = render_collected(child, prev, next_state, focus_key, child_panel_width);
        // Rows can host overlays in principle (e.g. a
        // tooltip on a button); forward them up without
        // a row-offset adjustment — Row pieces all sit
        // on the same buffer-row as the merged row.
        overlays.extend(child_out.overlays);
        if child_out.entries.is_empty() {
            debug_assert!(child_out.hits.is_empty(), "empty children produce no hits");
            continue;
        }
        if child_out.entries.len() == 1 {
            let mut entry = child_out.entries.into_iter().next().unwrap();
            // Inline children can't carry their own newlines
            // — that would split the merged Row across
            // buffer lines. The Row's final merged entry
            // gets exactly one newline appended below.
            strip_trailing_newline(&mut entry);
            row_pieces.push(RowPiece::Inline {
                entry,
                hits: child_out.hits,
                focus_cursor: child_out.focus_cursor,
                embeds: child_out.embeds,
                scroll_regions: child_out.scroll_regions,
            });
        } else {
            row_pieces.push(RowPiece::Block {
                column_width: child_panel_width,
                entries: child_out.entries,
                hits: child_out.hits,
                focus_cursor: child_out.focus_cursor,
                embeds: child_out.embeds,
                scroll_regions: child_out.scroll_regions,
            });
        }
    }
    // If any Block pieces survived classification, take
    // the horizontal-zip path; otherwise fall through to
    // the original inline-collapse assembly.
    let has_blocks = row_pieces
        .iter()
        .any(|p| matches!(p, RowPiece::Block { .. }));
    if has_blocks {
        zip_row_blocks(
            row_pieces,
            panel_width,
            &mut entries,
            &mut hits,
            &mut focus_cursor,
            &mut embeds,
            &mut scroll_regions,
        );
    } else if wrap {
        // Wrapping path: greedily pack inline pieces onto lines no
        // wider than `panel_width`; a piece that doesn't fit starts a
        // new line (pieces are never split). Each piece's hits get
        // their byte offset shifted by the line-so-far and their
        // `buffer_row` set to the line index.
        assemble_wrapped_row(
            row_pieces,
            panel_width,
            &mut entries,
            &mut hits,
            &mut focus_cursor,
        );
    } else {
        assemble_inline_row(
            row_pieces,
            panel_width,
            &mut entries,
            &mut hits,
            &mut focus_cursor,
            &mut embeds,
            &mut scroll_regions,
        );
    }

    CollectedOutput {
        entries,
        hits,
        focus_cursor,
        embeds,
        overlays,
        scroll_regions,
    }
}

/// Allocate a per-child column budget for a `Row`, aligned index-for-
/// index with `children`. Block children (those that render multi-line,
/// e.g. a `LabeledSection`) share `panel_width`: a child with an
/// explicit `width_pct` takes its declared share first, and the
/// remainder splits equally among the blocks without one. Non-block
/// children get the full `panel_width` (a soft cap — they collapse to a
/// single line, so width doesn't truncate them).
fn allocate_row_child_widths(children: &[WidgetSpec], panel_width: u32) -> Vec<u32> {
    let block_indices: Vec<usize> = children
        .iter()
        .enumerate()
        .filter(|(_, c)| predicts_block(c))
        .map(|(i, _)| i)
        .collect();
    let block_count = block_indices.len();
    let mut per_child_width: Vec<u32> = children.iter().map(|_| panel_width).collect();
    if block_count == 0 {
        return per_child_width;
    }
    let mut explicit_total: u32 = 0;
    let mut explicit_count: u32 = 0;
    for &idx in &block_indices {
        if let Some(pct) = labeled_section_width_pct(&children[idx]) {
            let w = (panel_width as u64 * pct as u64 / 100) as u32;
            per_child_width[idx] = w.max(1);
            explicit_total = explicit_total.saturating_add(w);
            explicit_count += 1;
        }
    }
    let remaining = panel_width.saturating_sub(explicit_total);
    let implicit_count = (block_count as u32).saturating_sub(explicit_count).max(1);
    let each_implicit = (remaining / implicit_count).max(1);
    for &idx in &block_indices {
        if labeled_section_width_pct(&children[idx]).is_none() {
            per_child_width[idx] = each_implicit;
        }
    }
    per_child_width
}

/// Assemble a `Row` of purely inline pieces (no multi-line `Block`s)
/// into a single merged entry. Flex spacers expand to fill the leftover
/// width (`panel_width` minus the natural inline width, measured in
/// display columns); child hits / focus / embeds / scroll regions are
/// shifted by the running byte offset so they stay aligned in the
/// merged row. The inline-only counterpart to [`zip_row_blocks`] and
/// [`assemble_wrapped_row`].
fn assemble_inline_row(
    pieces: Vec<RowPiece>,
    panel_width: u32,
    entries: &mut Vec<TextPropertyEntry>,
    hits: &mut Vec<HitArea>,
    focus_cursor: &mut Option<FocusCursor>,
    embeds: &mut Vec<EmbedRect>,
    scroll_regions: &mut Vec<ScrollRegion>,
) {
    // Compute flex sizing. Width is measured in display columns
    // (`str_width`) to match `panel_width`; using the raw byte length
    // would over-count multi-byte glyphs (▣ · ▸ ↑ − …) and under-size
    // the flex spacer, leaving a right-aligned group floating short of
    // the edge.
    let inline_natural: usize = pieces
        .iter()
        .filter_map(|p| match p {
            RowPiece::Inline { entry, .. } => {
                Some(crate::primitives::display_width::str_width(&entry.text))
            }
            _ => None,
        })
        .sum();
    let flex_count = pieces
        .iter()
        .filter(|p| matches!(p, RowPiece::Flex))
        .count();
    let flex_total = (panel_width as usize).saturating_sub(inline_natural);
    // Distribute leftover evenly. With multiple flex slots,
    // the leftover bytes spread as evenly as possible (any
    // remainder lands in the first slot).
    let (flex_each, flex_extra) = match flex_total.checked_div(flex_count) {
        Some(each) => (each, flex_total % flex_count),
        None => (0, 0),
    };

    // Pass 2: assemble. Accumulate inline pieces (with
    // collapsed flex spacers) into one entry; flush block
    // pieces. Track byte-shift so child hits' offsets stay
    // correct.
    let mut acc: Option<TextPropertyEntry> = None;
    let mut flex_seen = 0usize;
    for piece in pieces {
        match piece {
            RowPiece::Inline {
                mut entry,
                hits: child_hits,
                focus_cursor: child_focus,
                embeds: child_embeds,
                scroll_regions: child_scroll,
            } => {
                let inline_shift = match acc.as_ref() {
                    Some(e) => e.text.len(),
                    None => 0,
                };
                for mut h in child_hits {
                    h.byte_start += inline_shift;
                    h.byte_end += inline_shift;
                    hits.push(h);
                }
                if let Some(mut fc) = child_focus {
                    // buffer_row stays 0 — caller shifts.
                    fc.byte_in_row += inline_shift as u32;
                    *focus_cursor = Some(fc);
                }
                for mut emb in child_embeds {
                    // Inline shift is in bytes; for ASCII
                    // inline content this matches columns,
                    // which is the only case that lands here
                    // in practice (single-row embeds are
                    // rare).
                    emb.col_in_row += inline_shift as u32;
                    embeds.push(emb);
                }
                for mut sr in child_scroll {
                    sr.col_in_row += inline_shift as u32;
                    scroll_regions.push(sr);
                }
                match acc.as_mut() {
                    Some(merged) => merge_inline(merged, &mut entry),
                    None => acc = Some(entry),
                }
            }
            RowPiece::Flex => {
                // Materialize the flex spacer as N spaces.
                let n = flex_each + if flex_seen < flex_extra { 1 } else { 0 };
                flex_seen += 1;
                if n > 0 {
                    let mut text = String::with_capacity(n);
                    for _ in 0..n {
                        text.push(' ');
                    }
                    let entry = TextPropertyEntry {
                        text,
                        properties: Default::default(),
                        style: None,
                        inline_overlays: Vec::new(),
                        segments: Vec::new(),
                        pad_to_chars: None,
                        truncate_to_chars: None,
                    };
                    match acc.as_mut() {
                        Some(merged) => {
                            let mut e = entry;
                            merge_inline(merged, &mut e);
                        }
                        None => acc = Some(entry),
                    }
                }
            }
            RowPiece::Block { .. } => {
                // Unreachable in the inline-only path —
                // `has_blocks` was false here.
                debug_assert!(false, "block piece in inline-only Row path");
            }
        }
    }
    if let Some(mut merged) = acc {
        ensure_trailing_newline(&mut merged);
        entries.push(merged);
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_col(
    children: &[WidgetSpec],
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut focus_cursor: Option<FocusCursor> = None;
    let mut embeds: Vec<EmbedRect> = Vec::new();
    let mut overlays: Vec<OverlayRow> = Vec::new();
    let mut scroll_regions: Vec<ScrollRegion> = Vec::new();

    for child in children {
        // Overlay children DO NOT contribute vertical
        // space to the col. Render them, but stash the
        // produced entries as overlays anchored at the
        // current `entries.len()` (the row they would
        // have occupied) — they get painted on top
        // afterwards without pushing the rest of the
        // col downward.
        let is_overlay = matches!(child, WidgetSpec::Overlay { .. });
        let child_out = render_collected(child, prev, next_state, focus_key, panel_width);
        let row_offset = entries.len() as u32;
        if is_overlay {
            // Promote the overlay child's regular
            // entries to overlay rows anchored at the
            // current col cursor (`row_offset`). Hits
            // for those entries are shifted to the same
            // anchor row so click-to-pick targets the
            // painted row.
            for (i, e) in child_out.entries.into_iter().enumerate() {
                overlays.push(OverlayRow {
                    buffer_row: row_offset + i as u32,
                    entry: e,
                });
            }
            for mut h in child_out.hits {
                h.buffer_row += row_offset;
                hits.push(h);
            }
            // Focus cursor inside an overlay (rare but
            // legal) anchors at the same row; without
            // this shift Up/Down + cursor placement
            // would land on the col's "natural" row.
            if let Some(mut fc) = child_out.focus_cursor {
                fc.buffer_row += row_offset;
                focus_cursor = Some(fc);
            }
            // Forward nested overlays without further
            // adjustment (already anchored).
            overlays.extend(child_out.overlays);
            // Embeds inside an overlay don't make sense
            // today (a window-embed below a popup would
            // be confusing) — propagate at the same
            // anchor row so behaviour is well-defined
            // if someone tries it.
            for mut emb in child_out.embeds {
                emb.buffer_row += row_offset;
                embeds.push(emb);
            }
            for mut sr in child_out.scroll_regions {
                sr.buffer_row += row_offset;
                scroll_regions.push(sr);
            }
            continue;
        }
        for mut h in child_out.hits {
            h.buffer_row += row_offset;
            hits.push(h);
        }
        if let Some(mut fc) = child_out.focus_cursor {
            fc.buffer_row += row_offset;
            focus_cursor = Some(fc);
        }
        for mut emb in child_out.embeds {
            emb.buffer_row += row_offset;
            embeds.push(emb);
        }
        for mut sr in child_out.scroll_regions {
            sr.buffer_row += row_offset;
            scroll_regions.push(sr);
        }
        overlays.extend(child_out.overlays.into_iter().map(|mut o| {
            o.buffer_row += row_offset;
            o
        }));
        entries.extend(child_out.entries);
    }

    CollectedOutput {
        entries,
        hits,
        focus_cursor,
        embeds,
        overlays,
        scroll_regions,
    }
}

fn collect_hint_bar(entries: &[HintEntry]) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    let mut entry = render_hint_bar(entries);
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    // No hits — HintBar is read-only in v1. (When the
    // keymap layer arrives, individual entries become
    // clickable command targets.)
    out
}

#[allow(clippy::too_many_arguments)]
fn collect_toggle(
    checked: bool,
    label: &str,
    focused: bool,
    indeterminate: bool,
    label_first: bool,
    label_width: u32,
    key: Option<&str>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Host-managed focus overrides the spec's `focused`
    // when this widget has a key and is the panel's focused
    // widget. Plugin-passed `focused` is ignored when the
    // host owns focus (i.e. the panel has any tabbable
    // widgets); without it, the renderer falls back to the
    // spec value (legacy path).
    let is_focused = match key {
        Some(k) if !k.is_empty() => k == focus_key,
        _ => focused,
    };
    // Form layout (`label: [v]`) restricts the hit to the chip so a
    // click on the label doesn't flip the value (the settings dialog's
    // long-standing contract); the default chip-first layout keeps the
    // whole row clickable, which is what plugin panels expect.
    let (mut entry, chip_range) = if label_first {
        render_toggle_form(
            checked,
            indeterminate,
            label,
            is_focused,
            label_width,
            panel_width,
        )
    } else {
        let entry = render_toggle(checked, label, is_focused);
        let end = entry.text.len();
        (entry, (0, end))
    };
    out.hits.push(HitArea {
        widget_key: key.unwrap_or("").to_string(),
        widget_kind: "toggle",
        buffer_row: 0,
        byte_start: chip_range.0,
        byte_end: chip_range.1,
        payload: json!({ "checked": !checked }),
        event_type: "toggle",
    });
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}

#[allow(clippy::too_many_arguments)]
fn collect_number(
    spec_value: f64,
    min: Option<f64>,
    max: Option<f64>,
    integer: bool,
    percent: bool,
    label: &str,
    focused: bool,
    label_width: u32,
    edit: Option<NumberEdit<'_>>,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    let is_focused = match key {
        Some(k) if !k.is_empty() => k == focus_key,
        _ => focused,
    };
    // Instance state is authoritative once the widget has rendered;
    // the spec's `value` is a seed only. Read prior value by key,
    // clamp, and persist for the next render.
    let cur = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Number { value }) => *value,
            _ => spec_value,
        },
        _ => spec_value,
    };
    let cur = clamp_number(cur, min, max);
    if let Some(k) = key {
        if !k.is_empty() {
            next_state.insert(k.to_string(), WidgetInstanceState::Number { value: cur });
        }
    }

    let rendered = render_number(cur, integer, percent, label, is_focused, label_width, edit);
    let RenderedNumber {
        mut entry,
        value_range,
    } = rendered;
    // A click on the value cell begins in-place editing host-side
    // (see `deliver_widget_hit`'s `number_value` special case).
    out.hits.push(HitArea {
        widget_key: key.unwrap_or("").to_string(),
        widget_kind: "number",
        buffer_row: 0,
        byte_start: value_range.0,
        byte_end: value_range.1,
        payload: json!({}),
        event_type: "number_value",
    });
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}

#[allow(clippy::too_many_arguments)]
fn collect_dropdown(
    options: &[String],
    spec_selected: i32,
    label: &str,
    focused: bool,
    label_width: u32,
    spec_open: bool,
    spec_scroll: u32,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    let is_focused = match key {
        Some(k) if !k.is_empty() => k == focus_key,
        _ => focused,
    };
    // Instance state is authoritative after first render; clamp the
    // selected index into the current option set and persist. A panel
    // that renders statelessly (no prior instance state — e.g. the
    // Settings dialog re-emitting its model each frame) falls back to
    // the spec's `open`/`scroll_offset`: the host model drives the
    // expansion directly, so the spec's `open` is honored as-is (no
    // focus gate — the surface's own focus model already decided).
    let (cur, state_open) = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::Dropdown {
                selected_index,
                open,
            }) => (*selected_index, Some(*open)),
            _ => (spec_selected, None),
        },
        _ => (spec_selected, None),
    };
    let cur = if options.is_empty() {
        0
    } else {
        cur.clamp(0, options.len() as i32 - 1)
    };
    // Instance-state open only persists while the widget is focused —
    // a blur (Tab away, click elsewhere) closes it.
    let open = match state_open {
        Some(o) => o && is_focused,
        None => spec_open,
    } && !options.is_empty();
    if let Some(k) = key {
        if !k.is_empty() {
            next_state.insert(
                k.to_string(),
                WidgetInstanceState::Dropdown {
                    selected_index: cur,
                    open,
                },
            );
        }
    }

    let RenderedDropdown {
        mut entry,
        button_range,
        option_rows,
        scroll_offset,
    } = render_dropdown(
        options,
        cur,
        label,
        is_focused,
        label_width,
        open,
        spec_scroll,
    );
    let widget_key = key.unwrap_or("").to_string();
    // A click on the `[value ▼]` button toggles the option list open
    // (see `deliver_widget_hit`'s `dropdown_toggle` special case).
    out.hits.push(HitArea {
        widget_key: widget_key.clone(),
        widget_kind: "dropdown",
        buffer_row: 0,
        byte_start: button_range.0,
        byte_end: button_range.1,
        payload: json!({}),
        event_type: "dropdown_toggle",
    });
    let _ = scroll_offset;
    for (row_i, (idx, mut opt_entry)) in option_rows.into_iter().enumerate() {
        // Each visible option row is a full-width click target that
        // selects that option and closes the list.
        let row_len = opt_entry.text.len();
        out.hits.push(HitArea {
            widget_key: widget_key.clone(),
            widget_kind: "dropdown",
            buffer_row: (1 + row_i) as u32,
            byte_start: 0,
            byte_end: row_len,
            payload: json!({ "index": idx }),
            event_type: "dropdown_select",
        });
        ensure_trailing_newline(&mut opt_entry);
        out.entries.push(opt_entry);
    }
    ensure_trailing_newline(&mut entry);
    out.entries.insert(0, entry);
    out
}

#[allow(clippy::too_many_arguments)]
fn collect_button(
    label: &str,
    focused: bool,
    intent: ButtonKind,
    key: Option<&str>,
    disabled: bool,
    focus_key: &str,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    let is_focused = match key {
        Some(k) if !k.is_empty() && !disabled => k == focus_key,
        _ => !disabled && focused,
    };
    let mut entry = render_button(label, is_focused, intent, disabled);
    // Disabled buttons skip the hit area entirely — clicks on
    // them are no-ops, matching the non-tabbable behavior in
    // `collect_tabbable`. Without this, a stray click would
    // still focus + activate a button whose handler is
    // already gated by the same disabled condition the
    // plugin computed.
    if !disabled {
        let byte_end = entry.text.len();
        out.hits.push(HitArea {
            widget_key: key.unwrap_or("").to_string(),
            widget_kind: "button",
            buffer_row: 0,
            byte_start: 0,
            byte_end,
            payload: json!({}),
            event_type: "activate",
        });
    }
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}

fn collect_spacer(cols: u32) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Top-level / Col context: flex Spacers don't fill at
    // this level (no Row to absorb their flexibility), so
    // they fall back to `cols`. Row uses a separate code
    // path that sees the Spacer spec directly and handles
    // flex sizing — see RowPiece::Flex.
    let cols = cols.min(4096) as usize;
    let mut text = String::with_capacity(cols + 1);
    for _ in 0..cols {
        text.push(' ');
    }
    let mut entry = TextPropertyEntry {
        text,
        properties: Default::default(),
        style: None,
        inline_overlays: Vec::new(),
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}

fn collect_divider(ch: &str, style: Option<&OverlayOptions>, panel_width: u32) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Draw the rule at the host's authoritative inner width, so it
    // always spans the panel exactly — no plugin-side width guess.
    // One column per glyph (the default `─` is a single cell); an
    // empty `ch` falls back to a space so a stray empty divider
    // still occupies its row instead of collapsing.
    let glyph = if ch.is_empty() { " " } else { ch };
    let cols = (panel_width as usize).min(4096);
    let mut text = String::with_capacity(cols * glyph.len() + 1);
    for _ in 0..cols {
        text.push_str(glyph);
    }
    let mut entry = TextPropertyEntry {
        text,
        properties: Default::default(),
        style: style.cloned(),
        inline_overlays: Vec::new(),
        segments: Vec::new(),
        pad_to_chars: None,
        truncate_to_chars: None,
    };
    ensure_trailing_newline(&mut entry);
    out.entries.push(entry);
    out
}

/// Pre-render every card item-spec into its own block of entries at the
/// given column width. Returns the rendered cards alongside the uniform
/// card height (the tallest card, minimum 1) that the list's selection
/// and scroll math are expressed in.
fn render_list_cards(
    item_specs: &[WidgetSpec],
    prev: &HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    width: u32,
) -> (Vec<Vec<TextPropertyEntry>>, u32) {
    let mut rendered_cards: Vec<Vec<TextPropertyEntry>> = Vec::with_capacity(item_specs.len());
    let mut item_height: u32 = 1;
    for item_spec in item_specs.iter() {
        let mut scratch = HashMap::new();
        let card_entries =
            render_collected(item_spec, prev, &mut scratch, focus_key, width).entries;
        item_height = item_height.max((card_entries.len() as u32).max(1));
        rendered_cards.push(card_entries);
    }
    (rendered_cards, item_height)
}

/// Blank full-height-padding row used to pad a List to its
/// advertised height. Padding rows aren't clickable.
fn blank_list_row() -> TextPropertyEntry {
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
fn mark_list_row_selected(entry: &mut TextPropertyEntry) {
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
fn mark_list_card_selected(entry: &mut TextPropertyEntry) {
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
    if entry.text.starts_with('┏') || entry.text.starts_with('┗') {
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

/// Resolved geometry for one [`collect_list`] render: where the
/// scroll window sits and how items map to rows. All selection /
/// scroll values are in *item* units; the card path maps each item
/// to a fixed band of `item_height` rows.
struct ListLayout {
    /// Total number of items (classic rows or card specs).
    total: u32,
    /// Selection clamped to the current dataset (-1 when none / empty).
    effective_sel: i32,
    /// First visible item index.
    scroll: u32,
    /// Number of items that fit in the available height.
    visible_items: u32,
    /// Uniform card height in rows (1 for the classic path).
    item_height: u32,
    /// Pre-rendered card blocks (empty for the classic path).
    rendered_cards: Vec<Vec<TextPropertyEntry>>,
    /// Whether the host last scrolled by mouse (suppresses follow).
    user_scrolled: bool,
}

/// Resolve the prior host-owned scroll/selection state, (re-)render
/// any card blocks, and compute the visible window for a List. Pure
/// bookkeeping — it neither emits rows nor persists state.
#[allow(clippy::too_many_arguments)]
fn plan_list_layout(
    items_len: usize,
    item_specs: &[WidgetSpec],
    selected_index: i32,
    visible_rows: u32,
    list_key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> ListLayout {
    let use_specs = !item_specs.is_empty();
    let total = if use_specs {
        item_specs.len() as u32
    } else {
        items_len as u32
    };
    // Available height, in terminal rows.
    let avail_rows = visible_rows.max(1);

    // Look up host-owned scroll + selected index from prev state
    // (becomes authoritative after first render). The spec's
    // `selected_index` is initial-only on first mount.
    let (prev_scroll, prev_sel, user_scrolled) = list_key
        .and_then(|k| prev.get(k))
        .and_then(|s| match s {
            WidgetInstanceState::List {
                scroll_offset,
                selected_index,
                user_scrolled,
                ..
            } => Some((*scroll_offset, *selected_index, *user_scrolled)),
            _ => None,
        })
        .unwrap_or((0, selected_index, false));
    // Clamp the previous selection to the current dataset size —
    // items may have shrunk between renders. Out-of-range selections
    // collapse to the last item, or -1 if the list is now empty.
    let effective_sel = if prev_sel < 0 || total == 0 {
        -1
    } else if (prev_sel as u32) >= total {
        (total - 1) as i32
    } else {
        prev_sel
    };

    // Pre-render the card blocks (if any) so we know the uniform card
    // height; the visible-item count and all the scroll math derive
    // from it. Nested hits/embeds/overlays/scroll are dropped: a card
    // is a single `select` target (interactive widgets nested in a
    // card aren't routed yet).
    let mut rendered_cards: Vec<Vec<TextPropertyEntry>> = Vec::new();
    let mut item_height: u32 = 1;
    if use_specs {
        (rendered_cards, item_height) = render_list_cards(item_specs, prev, focus_key, panel_width);
    }
    // How many items fit, and the per-item scroll window.
    let visible_items = if use_specs {
        (avail_rows / item_height).max(1)
    } else {
        avail_rows
    };

    // When the card list overflows, the host paints a scrollbar in the
    // rightmost column — which would sit on top of each card's right
    // border. Re-render the cards one column narrower so they leave
    // that column free. (Row count is width-independent, so
    // `item_height` stays valid.)
    if use_specs && total > visible_items && panel_width > 1 {
        (rendered_cards, _) = render_list_cards(item_specs, prev, focus_key, panel_width - 1);
    }

    // Compute scroll. Normally we auto-clamp to keep the selection in
    // view, but once the user has scrolled by mouse (`user_scrolled`)
    // we respect their offset as-is so the selected card can sit
    // off-screen — only the range clamp below still applies. Selection
    // moves (keyboard/click/plugin) clear `user_scrolled`, re-arming
    // this follow behaviour.
    let mut scroll = prev_scroll;
    if effective_sel >= 0 && !user_scrolled {
        let sel = effective_sel as u32;
        if sel < scroll {
            scroll = sel;
        }
        if sel >= scroll + visible_items {
            scroll = sel + 1 - visible_items;
        }
    }
    let max_scroll = total.saturating_sub(visible_items);
    if scroll > max_scroll {
        scroll = max_scroll;
    }

    ListLayout {
        total,
        effective_sel,
        scroll,
        visible_items,
        item_height,
        rendered_cards,
        user_scrolled,
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_list(
    items: &[TextPropertyEntry],
    item_specs: &[WidgetSpec],
    item_keys: &[String],
    selected_index: i32,
    visible_rows: u32,
    list_key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut scroll_regions: Vec<ScrollRegion> = Vec::new();

    // Two layouts share one selection/scroll model:
    //   * classic — one `items` `TextPropertyEntry` per row;
    //   * cards    — one `item_specs` `WidgetSpec` per item,
    //                each rendered into a multi-row block (a
    //                rounded `LabeledSection` "pill", say).
    // Selection, scroll, `visible_rows`, and clicks are always
    // in *item* units; the card path just maps an item to a
    // fixed band of `item_height` rows instead of one row.
    let use_specs = !item_specs.is_empty();
    // Available height, in terminal rows.
    let avail_rows = visible_rows.max(1);
    let ListLayout {
        total,
        effective_sel,
        scroll,
        visible_items,
        item_height,
        rendered_cards,
        user_scrolled,
    } = plan_list_layout(
        items.len(),
        item_specs,
        selected_index,
        visible_rows,
        list_key,
        prev,
        focus_key,
        panel_width,
    );

    // Persist scroll + selection for the next render.
    // Lists without a `key` lose state across updates.
    if let Some(k) = list_key {
        next_state.insert(
            k.to_string(),
            WidgetInstanceState::List {
                scroll_offset: scroll,
                selected_index: effective_sel,
                item_height,
                user_scrolled,
            },
        );
    }

    let start = scroll as usize;
    let end = ((scroll + visible_items) as usize).min(total as usize);

    let rows_emitted: u32 = if use_specs {
        // Each item occupies a band of `item_height` rows; shorter
        // cards pad within their band so every card lines up. A
        // `select` hit covers every row, so a click anywhere on
        // the card selects it. When the list height isn't a whole
        // multiple of the card height, the next item below the
        // fold is rendered *partially* into the leftover rows
        // (rather than a blank gap) so it's clear there's more to
        // scroll.
        let mut emitted = 0u32;
        let last = if end < total as usize { end + 1 } else { end };
        'cards: for (offset, card) in rendered_cards[start..last].iter().enumerate() {
            let i = start + offset;
            let is_selected = i as i32 == effective_sel;
            let item_key = item_keys.get(i).cloned().unwrap_or_default();
            for r in 0..item_height as usize {
                if emitted >= avail_rows {
                    break 'cards;
                }
                let mut entry = card.get(r).cloned().unwrap_or_else(blank_list_row);
                entry.normalize_widths();
                if is_selected {
                    mark_list_card_selected(&mut entry);
                }
                let byte_end = entry.text.len();
                ensure_trailing_newline(&mut entry);
                let hit_row = entries.len() as u32;
                entries.push(entry);
                hits.push(HitArea {
                    widget_key: item_key.clone(),
                    widget_kind: "list",
                    buffer_row: hit_row,
                    byte_start: 0,
                    byte_end,
                    payload: json!({
                        "index": i as i64,
                        "key": item_key,
                        "list_key": list_key,
                    }),
                    event_type: "select",
                });
                emitted += 1;
            }
        }
        emitted
    } else {
        // Classic one-row-per-item path.
        for (offset, item) in items[start..end.min(items.len())].iter().enumerate() {
            let i = start + offset;
            let mut entry = item.clone();
            entry.normalize_widths();
            if i as i32 == effective_sel {
                mark_list_row_selected(&mut entry);
            }
            let byte_end = entry.text.len();
            ensure_trailing_newline(&mut entry);
            entries.push(entry);
            let item_key = item_keys.get(i).cloned().unwrap_or_default();
            let hit_row = (entries.len() - 1) as u32;
            hits.push(HitArea {
                widget_key: item_key.clone(),
                widget_kind: "list",
                buffer_row: hit_row,
                byte_start: 0,
                byte_end,
                payload: json!({
                    "index": i as i64,
                    "key": item_key,
                    // The List's own spec key, so a click handler can
                    // update the host-owned selection instance state
                    // (keyed by this) — the item key in `key` is not
                    // enough to find the widget. Null for keyless lists.
                    "list_key": list_key,
                }),
                event_type: "select",
            });
        }
        (end - start) as u32
    };

    // Pad to the advertised height with blank rows so the List
    // occupies its full `visible_rows` (keeps a sibling pane's
    // bottom border aligned). Padding rows aren't clickable.
    for _ in rows_emitted..avail_rows {
        entries.push(blank_list_row());
    }

    // Surface a scroll region for the host to paint a draggable
    // scrollbar when the list overflows. Totals are in items;
    // height_rows is the painted band so the thumb spans it.
    if total > visible_items {
        if let Some(k) = list_key {
            scroll_regions.push(ScrollRegion {
                list_key: k.to_string(),
                buffer_row: 0,
                col_in_row: 0,
                width_cols: panel_width,
                height_rows: avail_rows,
                total: total as usize,
                visible: visible_items as usize,
                scroll: scroll as usize,
            });
        }
    }

    CollectedOutput {
        entries,
        hits,
        focus_cursor: None,
        embeds: Vec::new(),
        overlays: Vec::new(),
        scroll_regions,
    }
}

#[allow(clippy::too_many_arguments)]
fn collect_labeled_section(
    label: &str,
    child: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut entries: Vec<TextPropertyEntry> = Vec::new();
    let mut hits: Vec<HitArea> = Vec::new();
    let mut focus_cursor: Option<FocusCursor> = None;
    let mut embeds: Vec<EmbedRect> = Vec::new();
    let mut overlays: Vec<OverlayRow> = Vec::new();
    let mut scroll_regions: Vec<ScrollRegion> = Vec::new();

    // Inner area: 1 column of border + 1 column of
    // padding on each side ⇒ 4 columns of chrome.
    let inner_width = panel_width.saturating_sub(4).max(1);
    let child_out = render_collected(child, prev, next_state, focus_key, inner_width);
    // Shift child overlays by 1 to account for the top
    // border row this section emits — the child authored
    // its anchors relative to its own row 0 (e.g. anchor 1
    // = "one row below me"), so an unshifted forward
    // would land them one row earlier than intended. The
    // Text widget's completion-popup overlays rely on
    // this: anchor 1 lands on the section's bottom
    // border row (replacing it visually with the dim
    // separator), anchor 2+ lands below the section.
    overlays.extend(child_out.overlays.into_iter().map(|mut o| {
        o.buffer_row += 1;
        o
    }));

    // Render the top border with the label embedded as a
    // legend: `╭─ <label> ─...─╮`. When the label is empty,
    // produce a plain `╭─...─╮` bar.
    let total_cols = panel_width.max(2) as usize;
    entries.push(render_section_top_border(label, total_cols));

    // Render each child row wrapped with the side borders
    // and one column of padding. Pad/truncate the child
    // text to exactly `inner_width` so the right border
    // lines up regardless of the child's natural width.
    for mut child_entry in child_out.entries {
        strip_trailing_newline(&mut child_entry);
        let wrapped = wrap_in_side_border(child_entry, inner_width as usize);
        let row_offset = entries.len() as u32;
        // Shift hits/focus emitted by the child by 1 row
        // (top border) and by the left-border prefix
        // ("│ " — 4 bytes for the box-drawing char + 1
        // for the space).
        let _ = row_offset;
        entries.push(wrapped);
    }

    // The child's hit areas were rendered with row 0 at
    // the *first child line*; shift them by 1 (top
    // border) and by the left-border byte prefix.
    let prefix_bytes = LEFT_BORDER_PREFIX.len();
    for mut h in child_out.hits {
        h.buffer_row += 1;
        h.byte_start += prefix_bytes;
        h.byte_end += prefix_bytes;
        hits.push(h);
    }
    if let Some(mut fc) = child_out.focus_cursor {
        fc.buffer_row += 1;
        fc.byte_in_row += prefix_bytes as u32;
        focus_cursor = Some(fc);
    }
    // Embeds are column-addressed; the `│ ` prefix is
    // 4 UTF-8 bytes but only 2 display columns wide.
    let prefix_cols = LEFT_BORDER_PREFIX.chars().count() as u32;
    for mut emb in child_out.embeds {
        emb.buffer_row += 1;
        emb.col_in_row += prefix_cols;
        embeds.push(emb);
    }
    for mut sr in child_out.scroll_regions {
        sr.buffer_row += 1;
        sr.col_in_row += prefix_cols;
        // The section padded the child to `inner_width`, so the
        // scroll region's usable width is the inner width (not
        // the child's requested width).
        sr.width_cols = inner_width;
        scroll_regions.push(sr);
    }

    entries.push(render_section_bottom_border(total_cols));

    CollectedOutput {
        entries,
        hits,
        focus_cursor,
        embeds,
        overlays,
        scroll_regions,
    }
}

fn collect_window_embed(window_id: u32, embed_rows: u32, panel_width: u32) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Emit `rows` blank lines of `panel_width` width so
    // layout reserves the rectangle. The host paint
    // path overlays the native window render on top of
    // these blanks after the rest of the panel paints.
    let cols = panel_width.max(1) as usize;
    for _ in 0..embed_rows {
        let mut text = String::with_capacity(cols + 1);
        for _ in 0..cols {
            text.push(' ');
        }
        text.push('\n');
        out.entries.push(TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: Vec::new(),
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
    }
    out.embeds.push(EmbedRect {
        window_id,
        buffer_row: 0,
        col_in_row: 0,
        width_cols: panel_width,
        height_rows: embed_rows,
    });
    out
}

fn collect_raw(raw_entries: &[TextPropertyEntry]) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Raw is the migration escape hatch: the plugin's own
    // bytes flow through unchanged. The plugin still owns
    // mouse clicks within Raw regions (via the existing
    // `mouse_click` hook); the widget runtime intentionally
    // emits no hit areas here. We *do* ensure each Raw
    // entry ends with a newline so it occupies its own
    // buffer line — plugins that already include `\n` are
    // unaffected.
    for raw_entry in raw_entries {
        let mut e = raw_entry.clone();
        e.normalize_widths();
        ensure_trailing_newline(&mut e);
        out.entries.push(e);
    }
    out
}

#[allow(clippy::too_many_arguments)]
fn collect_overlay(
    child: &WidgetSpec,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    // Renders the child normally; the parent (`Col`)
    // is what decides to promote the resulting entries
    // into the overlay set instead of consuming
    // vertical space. Outside of a `Col`, an Overlay
    // behaves like a transparent wrapper — entries
    // flow through unchanged. This keeps the
    // Overlay-as-root case (no enclosing Col) sane:
    // it just renders inline.
    let child_out = render_collected(child, prev, next_state, focus_key, panel_width);
    CollectedOutput {
        entries: child_out.entries,
        hits: child_out.hits,
        focus_cursor: child_out.focus_cursor,
        embeds: child_out.embeds,
        overlays: child_out.overlays,
        scroll_regions: child_out.scroll_regions,
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
fn effective_text_field_width(
    full_width: bool,
    multiline: bool,
    label: &str,
    panel_width: u32,
    field_width: u32,
) -> u32 {
    if !full_width || multiline {
        return field_width;
    }
    let label_overhead = if label.is_empty() {
        0u32
    } else {
        label.chars().count() as u32 + 1
    };
    let marker_reserve = if marker_gutter_enabled() { 2 } else { 0 };
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
fn emit_completion_overlays(
    out: &mut CollectedOutput,
    completions: &[fresh_core::api::CompletionItem],
    visible_rows: u32,
    panel_width: u32,
    selected_idx: usize,
    navigated: bool,
    prev_scroll: u32,
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
            ),
        });
        anchor += 1;
    }
    out.overlays.push(OverlayRow {
        buffer_row: anchor,
        entry: render_completion_bottom_border(popup_total),
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
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Default popup height: 5 visible rows. Plugins override per-widget
    // by setting `completions_visible_rows`; 0 falls back to the default
    // so the orchestrator's existing `text({...})` calls Just Work.
    let effective_visible_rows = if completions_visible_rows == 0 {
        5u32
    } else {
        completions_visible_rows
    };

    let is_focused = match key.filter(|k| !k.is_empty()) {
        Some(k) => k == focus_key,
        None => focused,
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
            focus_gutter_prefix(is_focused).len(),
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
                    widget_key: k.to_string(),
                    widget_kind: "text",
                    buffer_row: row_idx as u32,
                    byte_start: 0,
                    byte_end: e.text.len(),
                    payload: json!({}),
                    event_type: "focus",
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
        let gutter = focus_gutter_prefix(is_focused);
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
        &prev_completions,
        effective_visible_rows,
        panel_width,
        prev_completion_idx,
        prev_completion_navigated,
        prev_completion_scroll,
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
            },
        );
    }
    out
}

#[allow(clippy::too_many_arguments)]
fn render_widget_tree(
    nodes: &[TreeNode],
    item_keys: &[String],
    selected_index: i32,
    visible_rows: u32,
    expanded_keys: &[String],
    checkable: bool,
    item_height: u32,
    tree_key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    panel_width: u32,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    // Fixed rows per node. `1` is the classic single-line tree; a
    // larger value renders every node as a card of this many rows.
    // Windowing/scroll stay in *node* units — the node budget is just
    // `visible_rows / item_height` — so single-line trees (the vast
    // majority) are wholly unaffected.
    let item_height = item_height.max(1);
    // Look up host-owned instance state (scroll, selection,
    // expanded set). Spec values are initial-only.
    let prev_state = tree_key.filter(|k| !k.is_empty()).and_then(|k| prev.get(k));
    let (prev_scroll, prev_sel, prev_expanded) = match prev_state {
        Some(WidgetInstanceState::Tree {
            scroll_offset,
            selected_index,
            expanded_keys,
        }) => (*scroll_offset, *selected_index, expanded_keys.clone()),
        _ => {
            // First render: seed expanded_keys from spec.
            let seeded: HashSet<String> = expanded_keys.iter().cloned().collect();
            (0, selected_index, seeded)
        }
    };

    // Compute the visible (un-collapsed) flat slice of the
    // full `nodes` list. A node at depth d is visible iff
    // every ancestor (the most recent earlier node at depth
    // d-1, that node's most recent earlier at d-2, etc.) is
    // expanded. Walk linearly tracking ancestor expansion at
    // each depth — set ancestor[d] = is_expanded(node) when
    // we visit a node at depth d, and consider a node
    // visible iff ancestor[0..node.depth] are all true.
    //
    // O(N * max_depth) — fine; trees in this editor are
    // shallow (filesystem trees, search-results trees).
    let mut ancestor_open: Vec<bool> = Vec::new();
    let mut visible_indices: Vec<usize> = Vec::with_capacity(nodes.len());
    for (i, node) in nodes.iter().enumerate() {
        let depth = node.depth as usize;
        // Truncate the ancestor stack to this node's depth.
        ancestor_open.truncate(depth);
        let visible = ancestor_open.iter().all(|open| *open);
        if visible {
            visible_indices.push(i);
        }
        // Push this node's own openness onto the stack so
        // descendants see it. The node is "open" iff it has
        // children AND its key is in expanded_keys; leaves
        // act like open nodes (their nonexistent descendants
        // can't be hidden anyway).
        let key = item_keys.get(i).cloned().unwrap_or_default();
        let is_open = if node.has_children {
            !key.is_empty() && prev_expanded.contains(&key)
        } else {
            true
        };
        ancestor_open.push(is_open);
    }

    // Clamp the previous selection to a visible index. The
    // selected_index in the spec/instance state references
    // the *absolute* `nodes` index; if that node is now
    // hidden (parent collapsed), find the closest visible
    // node at-or-before it. If no visible nodes, -1.
    let total_visible = visible_indices.len() as u32;
    // Node budget: how many fixed-height cards fit in the row budget.
    // Everything below counts *nodes*, so item_height == 1 reduces to
    // the original single-line behaviour exactly.
    let visible = (visible_rows / item_height).max(1);
    let clamp_to_visible = |abs: i32| -> i32 {
        if abs < 0 || nodes.is_empty() {
            return -1;
        }
        let abs = abs.min((nodes.len() as i32) - 1) as usize;
        if let Ok(_pos) = visible_indices.binary_search(&abs) {
            return abs as i32;
        }
        // Not visible — fall back to the nearest earlier
        // visible node, else the first visible node, else -1.
        let earlier = visible_indices.iter().rev().find(|&&v| v <= abs);
        if let Some(&v) = earlier {
            return v as i32;
        }
        visible_indices.first().map(|&v| v as i32).unwrap_or(-1)
    };
    let effective_sel_abs = clamp_to_visible(prev_sel);
    // Find the position of the selected absolute index in
    // visible_indices — that's its "visible-window position"
    // used for scroll math.
    let sel_visible_pos: i32 = if effective_sel_abs < 0 {
        -1
    } else {
        visible_indices
            .iter()
            .position(|&v| v == effective_sel_abs as usize)
            .map(|p| p as i32)
            .unwrap_or(-1)
    };

    // Compute scroll: same auto-clamp logic as List, but
    // operating on the visible-windowed indices.
    let mut scroll = prev_scroll;
    if sel_visible_pos >= 0 {
        let sel = sel_visible_pos as u32;
        if sel < scroll {
            scroll = sel;
        }
        if sel >= scroll + visible {
            scroll = sel + 1 - visible;
        }
    }
    let max_scroll = total_visible.saturating_sub(visible);
    if scroll > max_scroll {
        scroll = max_scroll;
    }

    // Persist instance state.
    if let Some(k) = tree_key.filter(|k| !k.is_empty()) {
        next_state.insert(
            k.to_string(),
            WidgetInstanceState::Tree {
                scroll_offset: scroll,
                selected_index: effective_sel_abs,
                expanded_keys: prev_expanded.clone(),
            },
        );
    }

    // Render the visible window.
    let start = scroll as usize;
    let end = ((scroll + visible) as usize).min(visible_indices.len());
    for &abs_idx in &visible_indices[start..end] {
        // Apply pad/truncate hints and convert any char-unit
        // overlays to byte offsets *before* the disclosure
        // prefix is prepended; render_tree_row then byte-shifts
        // the (now byte-unit) overlays uniformly.
        let mut node = nodes[abs_idx].clone();
        node.text.normalize_widths();
        for line in node.extra_lines.iter_mut() {
            line.normalize_widths();
        }
        let item_key = item_keys.get(abs_idx).cloned().unwrap_or_default();
        let is_expanded =
            node.has_children && !item_key.is_empty() && prev_expanded.contains(&item_key);
        let rendered = render_tree_row(&node, is_expanded, checkable, item_height);
        let mut entry = rendered.entry;
        let is_selected = abs_idx as i32 == effective_sel_abs;
        // The selection highlight fills the whole card (every row),
        // so a multi-row card reads as one selected block.
        let select_style = |e: &mut TextPropertyEntry| {
            let mut style = e.style.clone().unwrap_or_default();
            style.bg = Some(OverlayColorSpec::theme_key(KEY_FOCUSED_BG));
            style.extend_to_line_end = true;
            e.style = Some(style);
        };
        if is_selected {
            select_style(&mut entry);
        }
        let row_byte_end = entry.text.len();
        ensure_trailing_newline(&mut entry);
        out.entries.push(entry);
        let hit_row = (out.entries.len() - 1) as u32;
        // Continuation rows of a card (item_height > 1). They carry no
        // hit areas of their own — the primary row owns select/expand/
        // toggle — but they do take the selection highlight so the card
        // highlights as a unit.
        for mut extra in rendered.extra_entries {
            if is_selected {
                select_style(&mut extra);
            }
            ensure_trailing_newline(&mut extra);
            out.entries.push(extra);
        }
        // Disclosure hit (only when has_children) — fires
        // `expand`. The host toggles instance-state
        // `expanded_keys` and re-renders before firing the
        // event; the plugin only listens if it cares about
        // expansion changes.
        // Tree hits use the *tree's* spec key for
        // `widget_key` (so click-to-focus works the same
        // as Toggle/Button — the tree is tabbable). The
        // per-row key travels in the payload.
        let tree_spec_key = tree_key.unwrap_or("").to_string();
        if let Some(disc_range) = rendered.disclosure_range {
            out.hits.push(HitArea {
                widget_key: tree_spec_key.clone(),
                widget_kind: "tree",
                buffer_row: hit_row,
                byte_start: disc_range.0,
                byte_end: disc_range.1,
                payload: json!({
                    "index": abs_idx as i64,
                    "key": item_key.clone(),
                    "expanded": !is_expanded,
                }),
                event_type: "expand",
            });
        }
        // Checkbox hit (when the parent Tree is checkable
        // *and* this node has Some(_) checked) — fires
        // `toggle` with the *new* checked value. The host
        // does not mutate the spec; the plugin owns the
        // truth and pushes the new state back via
        // `WidgetMutation::SetCheckedKeys`.
        if let Some(cb_range) = rendered.checkbox_range {
            let new_checked = !nodes[abs_idx].checked.unwrap_or(false);
            out.hits.push(HitArea {
                widget_key: tree_spec_key.clone(),
                widget_kind: "tree",
                buffer_row: hit_row,
                byte_start: cb_range.0,
                byte_end: cb_range.1,
                payload: json!({
                    "index": abs_idx as i64,
                    "key": item_key.clone(),
                    "checked": new_checked,
                }),
                event_type: "toggle",
            });
        }
        // Row body hit — fires `select`. Spans whatever's
        // left of the row text after the disclosure +
        // checkbox prefix.
        let body_start = match (rendered.checkbox_range, rendered.disclosure_range) {
            (Some((_, end)), _) => end + 1, // +1 for the trailing space after [v]
            (None, Some((_, end))) => end,
            (None, None) => 0,
        };
        if body_start < row_byte_end {
            out.hits.push(HitArea {
                widget_key: tree_spec_key,
                widget_kind: "tree",
                buffer_row: hit_row,
                byte_start: body_start,
                byte_end: row_byte_end,
                payload: json!({
                    "index": abs_idx as i64,
                    "key": item_key,
                }),
                event_type: "select",
            });
        }
    }

    // Surface a scroll region so the host paints a draggable overlay
    // scrollbar when the tree overflows — mirroring the List path, so the
    // dock's session tree gets the same hover scrollbar the card list had.
    // Totals are in visible (un-collapsed) rows.
    if total_visible > visible {
        if let Some(k) = tree_key.filter(|k| !k.is_empty()) {
            out.scroll_regions.push(ScrollRegion {
                list_key: k.to_string(),
                buffer_row: 0,
                col_in_row: 0,
                width_cols: panel_width,
                // The thumb is sized in node units (total/visible/scroll),
                // but the track spans the painted screen rows.
                height_rows: visible * item_height,
                total: total_visible as usize,
                visible: visible as usize,
                scroll: scroll as usize,
            });
        }
    }

    out
}

// =========================================================================
// LabeledSection helpers.
// =========================================================================

const LEFT_BORDER_PREFIX: &str = "│ ";
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
fn render_section_top_border(label: &str, total_cols: usize) -> TextPropertyEntry {
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
fn render_section_bottom_border(total_cols: usize) -> TextPropertyEntry {
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
fn render_completion_dim_separator_overlay(total_cols: usize) -> TextPropertyEntry {
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
fn render_completion_bottom_border(total_cols: usize) -> TextPropertyEntry {
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
fn render_completion_item_overlay(
    item: &str,
    kind: Option<&str>,
    selected: bool,
    total_cols: usize,
    scrollbar: Option<char>,
) -> TextPropertyEntry {
    let inner = total_cols.saturating_sub(2).max(1);
    // Reuse the inline-row builder for the body — same layout
    // rules (2 leading chars, item text, pad-to-(inner-1),
    // scrollbar in the last column).
    let body_entry = render_completion_item(item, kind, selected, inner, scrollbar);
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
    let lead = if marker_gutter_enabled() { 2 } else { 0 };
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
fn completion_scrollbar_glyph(
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
fn wrap_in_side_border(mut child: TextPropertyEntry, inner_width: usize) -> TextPropertyEntry {
    let prefix_bytes = LEFT_BORDER_PREFIX.len();
    // Pad / truncate `child.text` to `inner_width` display cols.
    let cur_cols = child.text.chars().count();
    if cur_cols < inner_width {
        for _ in 0..(inner_width - cur_cols) {
            child.text.push(' ');
        }
    } else if cur_cols > inner_width {
        // Tail-truncate at the codepoint boundary corresponding
        // to `inner_width` chars, then if there's room replace
        // the final visible char with `…` so the cut is visible
        // (mirrors `pad_or_truncate_cols`).
        let indices: Vec<usize> = child.text.char_indices().map(|(i, _)| i).collect();
        let byte_cutoff = indices
            .get(inner_width)
            .copied()
            .unwrap_or(child.text.len());
        child.text.truncate(byte_cutoff);
        if inner_width >= 2 {
            // Replace the last visible char with `…`. `pop()` walks
            // codepoint boundaries so multi-byte tails are handled
            // correctly. We then update `byte_cutoff` to the new
            // string length so overlay clamping below uses the
            // post-ellipsis boundary.
            child.text.pop();
            child.text.push('…');
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

    // Compose final text: `│ ` + child + ` │\n`.
    let mut text = String::with_capacity(
        LEFT_BORDER_PREFIX.len() + child.text.len() + RIGHT_BORDER_SUFFIX.len() + 1,
    );
    text.push_str(LEFT_BORDER_PREFIX);
    text.push_str(&child.text);
    text.push_str(RIGHT_BORDER_SUFFIX);
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
pub fn render_toggle(checked: bool, label: &str, focused: bool) -> TextPropertyEntry {
    let glyph = if checked { "[v]" } else { "[ ]" };
    // When the panel reserves the focus-marker gutter, every toggle
    // leads with a two-column gutter — `▸ ` when focused, two spaces
    // otherwise — so focus is capture-legible and the width never
    // changes as focus moves. Panels without the gutter render
    // exactly as before (no prefix).
    let marker = focus_gutter_prefix(focused);
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
pub fn render_number(
    value: f64,
    integer: bool,
    percent: bool,
    label: &str,
    focused: bool,
    label_width: u32,
    edit: Option<NumberEdit<'_>>,
) -> RenderedNumber {
    let marker = focus_gutter_prefix(focused);
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
fn form_label_width(
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
fn fit_label(label: &str, width: usize) -> String {
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
) -> (TextPropertyEntry, (usize, usize)) {
    let glyph = if indeterminate {
        "[-]"
    } else if checked {
        "[v]"
    } else {
        "[ ]"
    };
    let marker = focus_gutter_prefix(focused);
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
pub fn render_dropdown(
    options: &[String],
    selected_index: i32,
    label: &str,
    focused: bool,
    label_width: u32,
    open: bool,
    scroll_offset: u32,
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

    let marker = focus_gutter_prefix(focused);
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
        let indent = marker.len()
            + if label.is_empty() {
                0
            } else {
                pad_label(label, label_width as usize).len() + 2
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
fn cell(s: &str, width: usize) -> String {
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
fn dual_col_width(panel_width: u32) -> usize {
    // `u32::MAX` means flex is disabled (tests / unbounded) — fall
    // back to a readable fixed width. Otherwise split the panel in
    // two with a two-column gap, clamped to a sane range.
    let width = if panel_width == u32::MAX {
        40
    } else {
        panel_width
    };
    ((width.saturating_sub(4)) / 2).clamp(8, 40) as usize
}

#[allow(clippy::too_many_arguments)]
fn collect_dual_list(
    options: &[DualListOption],
    spec_included: &[String],
    excluded: &[String],
    label: &str,
    focused: bool,
    visible_rows: u32,
    key: Option<&str>,
    prev: &HashMap<String, WidgetInstanceState>,
    next_state: &mut HashMap<String, WidgetInstanceState>,
    focus_key: &str,
    panel_width: u32,
) -> CollectedOutput {
    let mut out = CollectedOutput::default();
    let is_focused = match key {
        Some(k) if !k.is_empty() => k == focus_key,
        _ => focused,
    };
    // Instance state is authoritative after first render.
    let (included, active_included, mut avail_cur, mut incl_cur) = match key {
        Some(k) if !k.is_empty() => match prev.get(k) {
            Some(WidgetInstanceState::DualList {
                included,
                active_included,
                available_cursor,
                included_cursor,
            }) => (
                included.clone(),
                *active_included,
                *available_cursor as usize,
                *included_cursor as usize,
            ),
            _ => (spec_included.to_vec(), false, 0, 0),
        },
        _ => (spec_included.to_vec(), false, 0, 0),
    };
    let included = dual_sanitize_included(options, &included);
    let available = dual_available_values(options, &included, excluded);
    // Clamp cursors into their columns.
    if !available.is_empty() {
        avail_cur = avail_cur.min(available.len() - 1);
    } else {
        avail_cur = 0;
    }
    if !included.is_empty() {
        incl_cur = incl_cur.min(included.len() - 1);
    } else {
        incl_cur = 0;
    }
    if let Some(k) = key {
        if !k.is_empty() {
            next_state.insert(
                k.to_string(),
                WidgetInstanceState::DualList {
                    included: included.clone(),
                    active_included,
                    available_cursor: avail_cur as u32,
                    included_cursor: incl_cur as u32,
                },
            );
        }
    }

    let col_w = dual_col_width(panel_width);
    let widget_key = key.unwrap_or("").to_string();

    // Optional label row.
    if !label.is_empty() {
        let mut e = TextPropertyEntry::text(label);
        ensure_trailing_newline(&mut e);
        out.entries.push(e);
    }
    // Header row.
    let header = format!("{}  {}", cell("Available", col_w), cell("Included", col_w));
    let mut header_entry = TextPropertyEntry::text(&header);
    header_entry.inline_overlays.push(InlineOverlay {
        start: 0,
        end: header.len(),
        style: OverlayOptions {
            fg: Some(OverlayColorSpec::theme_key(KEY_SECTION_LABEL_FG)),
            ..Default::default()
        },
        properties: Default::default(),
        unit: OffsetUnit::Byte,
    });
    ensure_trailing_newline(&mut header_entry);
    let header_row = out.entries.len() as u32;
    out.entries.push(header_entry);

    // Body rows — one per max(available, included), at least
    // `visible_rows`.
    let body_rows = available
        .len()
        .max(included.len())
        .max(visible_rows as usize);
    for i in 0..body_rows {
        let left_val = available.get(i);
        let right_val = included.get(i);
        let left = left_val.map(|v| dual_label(options, v)).unwrap_or("");
        let right = right_val.map(|v| dual_label(options, v)).unwrap_or("");
        let left_cell = cell(left, col_w);
        let right_cell = cell(right, col_w);
        let text = format!("{}  {}", left_cell, right_cell);
        let left_start = 0usize;
        let left_end = left_cell.len();
        let right_start = left_end + 2;
        let right_end = right_start + right_cell.len();

        let mut entry = TextPropertyEntry::text(&text);
        // Cursor highlight on the active column's cursor row (only
        // when the widget is focused).
        if is_focused {
            let (hs, he) = if active_included {
                if right_val.is_some() && i == incl_cur {
                    (right_start, right_end)
                } else {
                    (0, 0)
                }
            } else if left_val.is_some() && i == avail_cur {
                (left_start, left_end)
            } else {
                (0, 0)
            };
            if he > hs {
                entry.inline_overlays.push(InlineOverlay {
                    start: hs,
                    end: he,
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
        ensure_trailing_newline(&mut entry);
        let row = header_row + 1 + i as u32;
        // Click hit areas: clicking a cell focuses that column +
        // cursor row.
        if left_val.is_some() {
            out.hits.push(HitArea {
                widget_key: widget_key.clone(),
                widget_kind: "dual_list",
                buffer_row: row,
                byte_start: left_start,
                byte_end: left_end,
                payload: json!({ "column": "available", "index": i }),
                event_type: "dual_focus",
            });
        }
        if right_val.is_some() {
            out.hits.push(HitArea {
                widget_key: widget_key.clone(),
                widget_kind: "dual_list",
                buffer_row: row,
                byte_start: right_start,
                byte_end: right_end,
                payload: json!({ "column": "included", "index": i }),
                event_type: "dual_focus",
            });
        }
        out.entries.push(entry);
    }
    out
}

/// Render a `Button` to a single `TextPropertyEntry`.
///
/// Layout: `[ Label ]` (with explicit space padding so the label
/// is visually inset from the brackets). Styling depends on `kind`
/// and `focused`:
///
/// * `Normal`  — default fg; focused → fg/bg flip + bold.
/// * `Primary` — bold; focused → fg/bg flip.
/// * `Danger`  — red fg (theme `ui.status_error_indicator_fg`);
///   focused → bold.
pub fn render_button(
    label: &str,
    focused: bool,
    kind: ButtonKind,
    disabled: bool,
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
    let marker = focus_gutter_prefix(focused && !disabled);
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

    let style = if focused && !disabled {
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
) -> RenderedTreeRow {
    let indent_cols = (node.depth as usize) * 2;
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
}

/// Render a `TextInput`.
///
/// Layout: `Label: [<inner>]` (or `[<inner>]` with no label).
/// `<inner>` is exactly `field_width` chars wide when
/// `field_width > 0` — short values pad with trailing spaces, long
/// values head-truncate with `…` so the cursor (typically near the
/// tail) stays visible. With `field_width == 0` the input grows
/// with the value (legacy behaviour, also used by tests).
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
            let mut padded = value.to_string();
            while padded.chars().count() < total_inner {
                padded.push(' ');
            }
            (padded, Some(raw_cursor_byte))
        } else {
            // Long value: head-truncate to fit `target - 1` value
            // chars + 1 ellipsis. When focused, append a trailing
            // pad space (cursor parks there at end-of-value).
            let keep = target - 1;
            let drop_chars = value_chars.len() - keep;
            let mut dropped_bytes = 0usize;
            for ch in value_chars.iter().take(drop_chars) {
                dropped_bytes += ch.len_utf8();
            }
            let tail = &value[dropped_bytes..];
            let mut s = String::with_capacity("…".len() + tail.len() + pad_extra);
            s.push('…');
            s.push_str(tail);
            for _ in 0..pad_extra {
                s.push(' ');
            }
            // Cursor: if it sits in the dropped prefix, clamp to
            // right after the `…` glyph; otherwise translate
            // through the truncation.
            let cursor_in_inner = if raw_cursor_byte < dropped_bytes {
                "…".len()
            } else {
                "…".len() + (raw_cursor_byte - dropped_bytes)
            };
            value_dropped_bytes = dropped_bytes;
            ellipsis_bytes = "…".len();
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

/// Assemble a wrapping Row: pack inline pieces onto lines no wider than
/// `panel_width` (display columns), starting a new line when the next piece
/// would overflow. Pieces are never split, so wrap logical groups in a
/// nested non-wrapping Row to keep them intact. A whitespace-only piece (a
/// separator spacer) at the start of a fresh line is dropped so wrapped lines
/// don't begin with stray indentation. `Flex` spacers are ignored in the
/// wrap path (flex distribution is meaningless across reflowed lines).
fn assemble_wrapped_row(
    pieces: Vec<RowPiece>,
    panel_width: u32,
    entries: &mut Vec<TextPropertyEntry>,
    hits: &mut Vec<HitArea>,
    focus_cursor: &mut Option<FocusCursor>,
) {
    use crate::primitives::display_width::str_width;
    let max_w = panel_width as usize;
    let mut acc: Option<TextPropertyEntry> = None;
    let mut row: u32 = 0;
    // Hits for the current (not-yet-flushed) line, with byte offsets already
    // shifted but buffer_row not yet stamped (set when the line is started).
    let flush = |acc: &mut Option<TextPropertyEntry>, entries: &mut Vec<TextPropertyEntry>| {
        if let Some(mut merged) = acc.take() {
            ensure_trailing_newline(&mut merged);
            entries.push(merged);
        }
    };
    for piece in pieces {
        let RowPiece::Inline {
            mut entry,
            hits: child_hits,
            focus_cursor: piece_fc,
            ..
        } = piece
        else {
            // Flex / Block: ignored in the wrap path.
            continue;
        };
        let is_blank = entry.text.trim().is_empty();
        let piece_w = str_width(&entry.text);
        let acc_w = acc.as_ref().map(|e| str_width(&e.text)).unwrap_or(0);
        // Overflow → start a new line first.
        if acc.is_some() && acc_w + piece_w > max_w {
            flush(&mut acc, entries);
            row += 1;
        }
        // Drop a separator spacer that would lead a fresh line.
        if acc.is_none() && is_blank {
            continue;
        }
        let shift = acc.as_ref().map(|e| e.text.len()).unwrap_or(0);
        for mut h in child_hits {
            h.byte_start += shift;
            h.byte_end += shift;
            h.buffer_row = row;
            hits.push(h);
        }
        // A focused piece (e.g. the search TextInput) reports its caret;
        // shift it by the line-so-far and stamp the wrapped line index so
        // the host draws the cursor on the right row.
        if let Some(mut fc) = piece_fc {
            fc.byte_in_row += shift as u32;
            fc.buffer_row = row;
            *focus_cursor = Some(fc);
        }
        match acc.as_mut() {
            Some(merged) => merge_inline(merged, &mut entry),
            None => acc = Some(entry),
        }
    }
    flush(&mut acc, entries);
}

/// Merge `next` into `merged` for the inline-row collapse path.
/// `next`'s overlays are byte-shifted to account for the merged
/// text length so far.
fn merge_inline(merged: &mut TextPropertyEntry, next: &mut TextPropertyEntry) {
    let shift = merged.text.len();
    merged.text.push_str(&next.text);
    for overlay in next.inline_overlays.drain(..) {
        merged.inline_overlays.push(InlineOverlay {
            start: overlay.start + shift,
            end: overlay.end + shift,
            style: overlay.style,
            properties: overlay.properties,
            unit: overlay.unit,
        });
    }
    // `style` and `properties` from `next` are dropped — Row inline
    // collapse only preserves inline_overlays. Whole-entry style on
    // an inline-row child has no meaningful semantics here; if a
    // plugin needs whole-line styling it should produce a Col with
    // the styled child as its sole element.
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
fn pad_or_truncate_cols(text: &mut String, cols: usize) {
    let cur = text.chars().count();
    if cur < cols {
        for _ in 0..(cols - cur) {
            text.push(' ');
        }
    } else if cur > cols {
        // Cut to `cols` chars, then if we have room replace the
        // last char with `…` so the truncation is visible.
        let cutoff = text
            .char_indices()
            .nth(cols)
            .map(|(i, _)| i)
            .unwrap_or(text.len());
        text.truncate(cutoff);
        if cols >= 2 {
            // Drop the last char and append the ellipsis. We pop a
            // char (not a byte) so multi-byte tails stay intact.
            text.pop();
            text.push('…');
        }
    }
}

/// Clamp `idx` to `s.len()`, then walk it down to the nearest
/// char boundary. Byte-unit inline overlays computed against a
/// pre-truncation line must pass through this after the line is
/// column-truncated, so they can never index inside a multi-byte
/// char (the panic the span splitter raises on `text[a..b]`).
fn snap_down_to_char_boundary(s: &str, idx: usize) -> usize {
    let mut i = idx.min(s.len());
    while i > 0 && !s.is_char_boundary(i) {
        i -= 1;
    }
    i
}

/// Horizontal-zip pass for a Row that contains ≥1 multi-line
/// (Block) child. Each block has already been rendered with its
/// per-column budget (`block_width`); this helper walks the
/// row's pieces left-to-right per visual row and stitches them
/// into one merged line at a time.
///
/// Layout rules:
///   * Inline pieces sit at row 0 and become `chars().count()`
///     spaces on subsequent rows (so the right-hand block stays
///     aligned with its column).
///   * Block pieces contribute their `entries[row]` (or a blank
///     row of `block_width` spaces past their height).
///   * Flex pieces are intentionally a no-op in the block path —
///     `row(block, flexSpacer(), block)` is a rare shape and we
///     skip honouring flex here to keep the budget arithmetic
///     simple. Plugins that need a fixed gap should use
///     `spacer(n)` instead.
///
/// Hits and focus cursors get shifted by both the buffer-row
/// offset (which output line we're on) and the per-piece
/// byte-column offset (where in the merged text the piece
/// starts).
fn zip_row_blocks(
    pieces: Vec<RowPiece>,
    panel_width: u32,
    out_entries: &mut Vec<TextPropertyEntry>,
    out_hits: &mut Vec<HitArea>,
    out_focus_cursor: &mut Option<FocusCursor>,
    out_embeds: &mut Vec<EmbedRect>,
    out_scroll: &mut Vec<ScrollRegion>,
) {
    let starting_row = out_entries.len() as u32;
    let _ = panel_width;

    // Compute the merged height = max(block.entries.len()).
    let max_height = pieces
        .iter()
        .filter_map(|p| match p {
            RowPiece::Block { entries, .. } => Some(entries.len()),
            _ => None,
        })
        .max()
        .unwrap_or(0);
    if max_height == 0 {
        return;
    }

    for row_idx in 0..max_height {
        let mut text = String::new();
        let mut overlays: Vec<InlineOverlay> = Vec::new();
        for piece in &pieces {
            match piece {
                RowPiece::Inline {
                    entry,
                    hits,
                    focus_cursor,
                    embeds: inline_embeds,
                    scroll_regions: inline_scroll,
                } => {
                    let inline_cols = entry.text.chars().count();
                    let byte_shift = text.len();
                    // Cumulative column width to the left of this
                    // piece, for embed positioning. Embeds are
                    // column-addressed, not byte-addressed.
                    let col_shift = text.chars().count() as u32;
                    if row_idx == 0 {
                        text.push_str(&entry.text);
                        for emb in inline_embeds {
                            out_embeds.push(EmbedRect {
                                window_id: emb.window_id,
                                buffer_row: starting_row + emb.buffer_row,
                                col_in_row: emb.col_in_row + col_shift,
                                width_cols: emb.width_cols,
                                height_rows: emb.height_rows,
                            });
                        }
                        for sr in inline_scroll {
                            let mut sr = sr.clone();
                            sr.buffer_row += starting_row;
                            sr.col_in_row += col_shift;
                            out_scroll.push(sr);
                        }
                        for overlay in &entry.inline_overlays {
                            overlays.push(InlineOverlay {
                                start: overlay.start + byte_shift,
                                end: overlay.end + byte_shift,
                                style: overlay.style.clone(),
                                properties: overlay.properties.clone(),
                                unit: overlay.unit,
                            });
                        }
                        for h in hits {
                            let mut h = h.clone();
                            h.byte_start += byte_shift;
                            h.byte_end += byte_shift;
                            h.buffer_row = starting_row;
                            out_hits.push(h);
                        }
                        if let Some(fc) = focus_cursor {
                            *out_focus_cursor = Some(FocusCursor {
                                buffer_row: starting_row,
                                byte_in_row: fc.byte_in_row + byte_shift as u32,
                            });
                        }
                    } else {
                        for _ in 0..inline_cols {
                            text.push(' ');
                        }
                    }
                }
                RowPiece::Flex => {
                    // Skipped — see fn doc.
                }
                RowPiece::Block {
                    column_width,
                    entries,
                    hits,
                    focus_cursor,
                    embeds: block_embeds,
                    scroll_regions: block_scroll,
                } => {
                    let block_w = *column_width as usize;
                    let byte_shift = text.len();
                    // Cumulative column width to the left of this
                    // block, for embed positioning.
                    let col_shift = text.chars().count() as u32;
                    // Emit each embed exactly once, on the row
                    // where its top edge lands. The embed's
                    // buffer_row is relative to the block's row
                    // 0; absolute = starting_row + that.
                    if row_idx == 0 {
                        for emb in block_embeds {
                            out_embeds.push(EmbedRect {
                                window_id: emb.window_id,
                                buffer_row: starting_row + emb.buffer_row,
                                col_in_row: emb.col_in_row + col_shift,
                                width_cols: emb.width_cols,
                                height_rows: emb.height_rows,
                            });
                        }
                        for sr in block_scroll {
                            let mut sr = sr.clone();
                            sr.buffer_row += starting_row;
                            sr.col_in_row += col_shift;
                            out_scroll.push(sr);
                        }
                    }
                    if let Some(line) = entries.get(row_idx) {
                        let mut line_text = line.text.clone();
                        // Strip the entry's trailing newline so it
                        // doesn't split our merged line.
                        if line_text.ends_with('\n') {
                            line_text.pop();
                        }
                        pad_or_truncate_cols(&mut line_text, block_w);
                        let padded_byte_len = line_text.len();
                        text.push_str(&line_text);
                        // Convert the entry's whole-line `style`
                        // into an inline overlay covering the
                        // block's column in the merged row. This is
                        // what carries through the list widget's
                        // selected-row bg (and any other
                        // whole-entry styling on individual block
                        // lines) — without it, the picker's
                        // selection highlight disappears in the
                        // zipped output.
                        if let Some(line_style) = &line.style {
                            overlays.push(InlineOverlay {
                                start: byte_shift,
                                end: byte_shift + padded_byte_len,
                                style: line_style.clone(),
                                properties: Default::default(),
                                unit: OffsetUnit::Byte,
                            });
                        }
                        for overlay in &line.inline_overlays {
                            // `pad_or_truncate_cols` may have cut the
                            // line (and appended a multi-byte `…`), so
                            // an overlay computed against the original
                            // line can now point past — or *inside* — a
                            // char of the truncated text. Clamp both
                            // ends to the truncated length and snap to a
                            // char boundary; otherwise the downstream
                            // span splitter slices mid-char and panics.
                            let start = snap_down_to_char_boundary(&line_text, overlay.start);
                            let end = snap_down_to_char_boundary(&line_text, overlay.end);
                            if start >= end {
                                continue;
                            }
                            overlays.push(InlineOverlay {
                                start: start + byte_shift,
                                end: end + byte_shift,
                                style: overlay.style.clone(),
                                properties: overlay.properties.clone(),
                                unit: overlay.unit,
                            });
                        }
                        for h in hits {
                            if h.buffer_row != row_idx as u32 {
                                continue;
                            }
                            let mut h = h.clone();
                            h.byte_start += byte_shift;
                            h.byte_end += byte_shift;
                            h.buffer_row = starting_row + row_idx as u32;
                            out_hits.push(h);
                        }
                        if let Some(fc) = focus_cursor {
                            if fc.buffer_row == row_idx as u32 {
                                *out_focus_cursor = Some(FocusCursor {
                                    buffer_row: starting_row + row_idx as u32,
                                    byte_in_row: fc.byte_in_row + byte_shift as u32,
                                });
                            }
                        }
                    } else {
                        // Past this block's height — emit a blank
                        // column of `block_w` spaces.
                        for _ in 0..block_w {
                            text.push(' ');
                        }
                    }
                }
            }
        }
        text.push('\n');
        out_entries.push(TextPropertyEntry {
            text,
            properties: Default::default(),
            style: None,
            inline_overlays: overlays,
            segments: Vec::new(),
            pad_to_chars: None,
            truncate_to_chars: None,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            str_width(focus_gutter_prefix(false)) + 18 + ": ".len(),
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
        let entry = render_toggle(true, "Case", false);
        assert_eq!(entry.text, "[v] Case");
        // One overlay for the glyph, no focused overlay.
        assert_eq!(entry.inline_overlays.len(), 1);
        assert_eq!(entry.inline_overlays[0].start, 0);
        assert_eq!(entry.inline_overlays[0].end, 3);
    }

    #[test]
    fn toggle_unchecked_no_glyph_overlay() {
        let entry = render_toggle(false, "Case", false);
        assert_eq!(entry.text, "[ ] Case");
        assert_eq!(entry.inline_overlays.len(), 0);
    }

    #[test]
    fn toggle_focused_adds_full_entry_overlay() {
        let entry = render_toggle(true, "Case", true);
        // Glyph overlay + focused overlay.
        assert_eq!(entry.inline_overlays.len(), 2);
        // Focused overlay spans the full entry.
        assert_eq!(entry.inline_overlays[1].start, 0);
        assert_eq!(entry.inline_overlays[1].end, entry.text.len());
        assert!(entry.inline_overlays[1].style.bold);
    }

    #[test]
    fn button_normal_unfocused_has_no_overlay() {
        let entry = render_button("Replace All", false, ButtonKind::Normal, false);
        assert_eq!(entry.text, "[ Replace All ]");
        assert!(entry.inline_overlays.is_empty());
    }

    #[test]
    fn button_primary_unfocused_is_bold_help_key_fg_with_no_bg() {
        // Primary marks the "good" action with a bold, strong fg
        // on the surrounding surface. Only the focused state
        // paints a backing colour — verified in
        // `button_focused_overrides_with_menu_active_keys`.
        let entry = render_button("Submit", false, ButtonKind::Primary, false);
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
        let entry = render_button("Delete", false, ButtonKind::Danger, false);
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
        let entry = render_button("OK", true, ButtonKind::Normal, false);
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
                },
                WidgetSpec::Button {
                    label: "Cancel".into(),
                    focused: false,
                    intent: ButtonKind::Normal,
                    key: Some("cancel".into()),
                    disabled: false,
                    focusable: true,
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
        let entry = render_button("Archive", false, ButtonKind::Danger, true);
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
            visible_rows: 10,
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
            visible_rows: 12,
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
            visible_rows: 12,
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
            visible_rows: 10,
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
                    visible_rows: 10,
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
            visible_rows: 10,
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
            visible_rows: 10,
            focusable: true,
            key: None,
        };
        let (_, hits, _state) = render_no_focus(&spec, &HashMap::new());
        assert_eq!(hits[0].widget_key, "only");
        assert_eq!(hits[1].widget_key, "");
    }

    fn make_list(selected: i32, visible: u32, total: usize, key: Option<&str>) -> WidgetSpec {
        let items = (0..total)
            .map(|i| TextPropertyEntry::text(format!("row{}", i)))
            .collect();
        let item_keys = (0..total).map(|i| format!("k{}", i)).collect();
        WidgetSpec::List {
            items,
            item_specs: vec![],
            item_keys,
            selected_index: selected,
            visible_rows: visible,
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
        let entry = render_text_input("hello", -1, None, false, "", None, 0, 0, false).entry;
        assert_eq!(entry.text, "[hello]");
        assert!(entry.inline_overlays.is_empty());
    }

    #[test]
    fn text_input_with_label_prefixes_with_label_space() {
        let entry = render_text_input("foo", -1, None, false, "Search:", None, 0, 0, false).entry;
        assert_eq!(entry.text, "Search: [foo]");
    }

    #[test]
    fn text_input_focused_adds_input_bg_overlay() {
        let entry = render_text_input("x", -1, None, true, "", None, 0, 0, false).entry;
        // Focused → input-bg overlay (no cursor since cursor_byte < 0).
        assert_eq!(entry.inline_overlays.len(), 1);
        let bg = entry.inline_overlays[0].style.bg.as_ref().unwrap();
        assert_eq!(bg.as_theme_key(), Some("ui.prompt_bg"));
    }

    #[test]
    fn text_input_focused_with_selection_adds_selection_bg_overlay() {
        // Focused + selection range → input-bg overlay AND a
        // selection-bg overlay scoped to the selected bytes.
        let entry =
            render_text_input("hello world", 5, Some((0, 5)), true, "", None, 0, 0, false).entry;
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
            render_text_input("hello", -1, Some((0, 5)), false, "", None, 0, 0, false).entry;
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
        let r = render_text_input("abc", 1, None, true, "", None, 0, 0, false);
        assert_eq!(r.cursor_byte_in_entry, Some(2));
    }

    #[test]
    fn text_input_cursor_at_end_lands_on_padding_space_not_bracket() {
        // Cursor at end-of-value: with focused + no field_width,
        // a trailing pad space is appended so the cursor never
        // overlaps the closing bracket. text = "[ab ]" → cursor
        // at value-byte 2 lands at entry-byte 3 (the space), not
        // at byte 4 (the `]`).
        let r = render_text_input("ab", 2, None, true, "", None, 0, 0, false);
        assert_eq!(r.entry.text, "[ab ]");
        assert_eq!(r.cursor_byte_in_entry, Some(3));
        assert_ne!(r.cursor_byte_in_entry, Some(4), "must not overlap ]");
    }

    #[test]
    fn text_input_unfocused_empty_shows_placeholder_in_muted() {
        let entry =
            render_text_input("", -1, None, false, "", Some("type here"), 0, 0, false).entry;
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
        let r = render_text_input("", -1, None, true, "", Some("type here"), 0, 0, false);
        assert_eq!(r.entry.text, "[type here]");
        assert_eq!(r.cursor_byte_in_entry, Some(1));
    }

    #[test]
    fn text_input_field_width_pads_short_value_unfocused() {
        // field_width=10, unfocused, not full_width → inner is 10
        // chars (no extra cursor-park pad).
        let r = render_text_input("hi", 2, None, false, "", None, 0, 10, false);
        assert_eq!(r.entry.text, "[hi        ]");
    }

    #[test]
    fn text_input_field_width_focused_adds_cursor_park_space() {
        // field_width=10, focused, value fills exactly 10 → inner
        // is 11 chars (10 + 1 cursor-park space) so the cursor at
        // end-of-value never lands on `]`.
        let r = render_text_input("0123456789", 10, None, true, "", None, 0, 10, false);
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
        let r = render_text_input("hi", -1, None, false, "", None, 0, 10, true);
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
        );
        assert!(r.entry.text.contains("…lmnopqrst"));
    }

    #[test]
    fn text_input_field_width_clamps_cursor_in_dropped_prefix() {
        // Long value, field_width=5, focused, cursor at byte 0 (in
        // dropped prefix) → clamped to right after the `…`.
        let r = render_text_input("abcdefghij", 0, None, true, "", None, 0, 5, false);
        // Inner = `…fghij ` (1 ellipsis + 4 tail chars + 1 pad).
        // Cursor at "right after `…`" = byte 3 of inner (3 = `…`'s
        // UTF-8 byte length). entry.text has `[` before, so
        // absolute byte = 1 + 3 = 4.
        assert_eq!(r.cursor_byte_in_entry, Some(1 + "…".len()));
    }

    #[test]
    fn text_input_truncates_long_value_keeping_tail_visible() {
        let value: String = "0123456789abcdefghij".to_string();
        let entry = render_text_input(&value, -1, None, false, "", None, 6, 0, false).entry;
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
            visible_rows: visible,
            expanded_keys: expanded.iter().map(|s| s.to_string()).collect(),
            checkable: false,
            item_height: 1,
            key: key.map(|s| s.to_string()),
        }
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_collapsed() {
        let r = render_tree_row(&tnode("file.txt", 0, true), false, false, 1);
        assert!(r.entry.text.starts_with('\u{25B6}'), "starts with ▶");
        assert!(r.entry.text.contains("file.txt"));
        assert!(r.disclosure_range.is_some());
    }

    #[test]
    fn tree_row_renders_disclosure_glyph_for_internal_expanded() {
        let r = render_tree_row(&tnode("file.txt", 0, true), true, false, 1);
        assert!(r.entry.text.starts_with('\u{25BC}'), "starts with ▼");
    }

    #[test]
    fn tree_row_leaf_uses_two_spaces_no_disclosure_hit() {
        let r = render_tree_row(&tnode("match", 0, false), false, false, 1);
        // No glyph, just spaces for alignment.
        assert!(r.entry.text.starts_with("  "));
        assert!(r.entry.text.contains("match"));
        assert!(r.disclosure_range.is_none());
    }

    #[test]
    fn tree_row_indents_by_depth_times_two() {
        let r = render_tree_row(&tnode("nested", 2, false), false, false, 1);
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
        let r = render_tree_row(&node, false, false, 1);
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
        let r = render_tree_row(&node, false, false, 1);
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
        let r = render_tree_row(&node, false, true, 1);
        assert!(r.checkbox_range.is_none());
        assert!(!r.entry.text.contains("[v]"));
        assert!(!r.entry.text.contains("[ ]"));
    }

    #[test]
    fn tree_row_renders_checked_glyph_after_disclosure() {
        let mut node = tnode("file.rs", 0, true);
        node.checked = Some(true);
        let r = render_tree_row(&node, true, true, 1);
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
        let r = render_tree_row(&node, false, true, 1);
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
        let r = render_tree_row(&node, false, true, 1);
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
            key: Some("ta".into()),
        };
        let prev = HashMap::new();
        let out = render_spec(&spec, &prev, "ta", 80);
        // entries[0] is the label row, entries[1..] are content.
        assert!(out.entries[0].text.starts_with("Note:"));
        let fc = out.focus_cursor.unwrap();
        assert_eq!(fc.buffer_row, 1);
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

    fn make_text_input(
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
    fn zip_row_blocks_keeps_overlays_on_char_boundaries() {
        // Regression for the orchestrator picker panic: a two-pane
        // `row(labeledSection, labeledSection)` whose left label is
        // long and contains a multi-byte `·`. The column is narrow
        // enough that `pad_or_truncate_cols` cuts the label and
        // appends a multi-byte `…`. Before the fix, the label's
        // byte-unit overlay end was clamped to the *pre*-truncation
        // length, leaving it pointing inside the `…` — and the app
        // span splitter then sliced `text[a..b]` mid-char and
        // panicked. Every emitted overlay offset must land on a char
        // boundary of its row text.
        let left = WidgetSpec::LabeledSection {
            label: "alpha/beta · this project (2)".into(),
            child: Box::new(make_text_input("x", -1, false, false, 4, Some("a"))),
            width_pct: Some(40),
            key: None,
        };
        let right = WidgetSpec::LabeledSection {
            label: "preview".into(),
            child: Box::new(make_text_input("y", -1, false, false, 4, Some("b"))),
            width_pct: None,
            key: None,
        };
        let spec = WidgetSpec::Row {
            wrap: false,
            children: vec![left, right],
            key: None,
        };
        let out = render_spec(&spec, &HashMap::new(), "", 40);
        for e in &out.entries {
            for o in &e.inline_overlays {
                assert!(
                    e.text.is_char_boundary(o.start.min(e.text.len())),
                    "overlay start {} not on a char boundary of {:?}",
                    o.start,
                    e.text,
                );
                assert!(
                    e.text.is_char_boundary(o.end.min(e.text.len())),
                    "overlay end {} not on a char boundary of {:?}",
                    o.end,
                    e.text,
                );
            }
        }
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
        let r = render_number(3.0, true, false, "Size", false, 0, None);
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
    fn number_seeds_and_clamps_instance_state() {
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
        let (_out, _hits, state) = render_no_focus(&spec, &HashMap::new());
        // Spec value 42 clamps to max 10 and persists as instance state.
        match state.get("n") {
            Some(WidgetInstanceState::Number { value }) => assert_eq!(*value, 10.0),
            other => panic!("expected Number instance state, got {other:?}"),
        }
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
        );
        assert!(r.entry.text.contains("▲"));
        assert_eq!(r.option_rows.len(), 3);
        assert!(r.option_rows[0].1.text.contains("Red"));
        assert_eq!(r.option_rows[2].0, 2);
    }

    #[test]
    fn dropdown_emits_button_and_option_hit_areas() {
        let spec = WidgetSpec::Dropdown {
            label_width: 0,
            open: true,
            scroll_offset: 0,
            options: vec!["a".into(), "b".into()],
            selected_index: 0,
            label: String::new(),
            focused: true,
            key: None,
        };
        let (_out, hits, _state) = render_no_focus(&spec, &HashMap::new());
        let toggles: Vec<_> = hits
            .iter()
            .filter(|h| h.event_type == "dropdown_toggle")
            .collect();
        let selects: Vec<_> = hits
            .iter()
            .filter(|h| h.event_type == "dropdown_select")
            .collect();
        assert_eq!(toggles.len(), 1);
        assert_eq!(selects.len(), 2);
        assert_eq!(selects[1].payload["index"], 1);
    }

    #[test]
    fn dropdown_seeds_and_clamps_instance_state() {
        // Out-of-range spec index clamps into the option set.
        let spec = make_dropdown(&["a", "b", "c"], 9, Some("d"));
        let (_out, _hits, state) = render_no_focus(&spec, &HashMap::new());
        match state.get("d") {
            Some(WidgetInstanceState::Dropdown { selected_index, .. }) => {
                assert_eq!(*selected_index, 2)
            }
            other => panic!("expected Dropdown instance state, got {other:?}"),
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
    fn dropdown_open_emits_inline_option_rows() {
        let spec = make_dropdown(&["a", "b", "c"], 1, Some("d"));
        // Focused + open in instance state → inline option rows below
        // the value button.
        let mut prev = HashMap::new();
        prev.insert(
            "d".to_string(),
            WidgetInstanceState::Dropdown {
                selected_index: 1,
                open: true,
            },
        );
        let out = render_spec(&spec, &prev, "d", u32::MAX);
        assert_eq!(out.entries.len(), 4, "button row + one row per option");
        assert!(out.entries[2].text.contains('b'));
        // Each option row is a full-width select hit.
        let selects: Vec<_> = out
            .hits
            .iter()
            .filter(|h| h.event_type == "dropdown_select")
            .collect();
        assert_eq!(selects.len(), 3);
    }

    #[test]
    fn dropdown_popup_closes_when_unfocused() {
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
            visible_rows: 3,
            key: key.map(|k| k.to_string()),
        }
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
}
