//! Resolution of the bg style painted into the trailing columns of
//! a visual row (`extend_to_line_end` overlay fill + virtual-line bg
//! fallback).
//!
//! Pulled out of `render_view_lines` so the cell loop's post-content
//! handling reads as three named operations:
//!
//! ```text
//! let fill = resolve_tail_fill(TailFillInput { ... });
//! if let Some(fill) = fill { paint_remaining_cols(fill); }
//! if is_on_cursor_line { paint_cursor_line_bg(); }
//! ```
//!
//! …instead of the old ~100-line block where the overlay-face match
//! and the virtual-line fallback sat next to each other but consumed
//! different inputs and tripped different guards.

use crate::view::overlay::Overlay;
use crate::view::theme::{Theme, TokenColorExt};
use crate::view::ui::view_pipeline::{LineStart, ViewLine};
use ratatui::style::Style;

pub(super) struct TailFillInput<'a> {
    pub current_view_line: &'a ViewLine,
    pub theme: &'a Theme,
    /// Output of `OverlayActiveSet::fill_overlay()` — the highest
    /// priority `extend_to_line_end` overlay that touched this row.
    /// `None` when no such overlay covered the row.
    pub overlay_fill: Option<&'a Overlay>,
    /// Set by the cell loop when a syntect span scoped to this row
    /// carries a bg AND its category's `bg_extends_to_line_end()` is
    /// true (diff Inserted / Deleted / Changed). Lower priority than
    /// `overlay_fill` so plugin overlays still win, higher priority
    /// than the virtual-line fallback.
    pub syntax_extend_bg: Option<ratatui::style::Color>,
    /// `Some` iff at least one cell on this row mapped to a source
    /// byte. Used to suppress the overlay-fill path on rows that
    /// contributed no bytes (virtual / empty lines fall through to
    /// the virtual-line fallback instead).
    pub first_line_byte_pos: Option<usize>,
    pub last_line_byte_pos: Option<usize>,
}

pub(super) struct TailFillResult {
    pub style: Style,
    /// Source byte to attach to the fill span. Mirrors the original
    /// inline behaviour: `None` for virtual lines (so visual-line
    /// motion keeps skipping them), the line's start byte for empty
    /// source lines (so `char_source_bytes` has at least one `Some`
    /// entry and the navigable check doesn't treat the row as a
    /// plugin-injected decoration to skip).
    pub source_byte: Option<usize>,
}

/// Pick the bg style for the row's trailing fill, or `None` to leave
/// it untouched.
///
/// Composes two policies in priority order:
///
/// 1. **Overlay-driven** — when the row had at least one source byte
///    AND an `extend_to_line_end` overlay covered it, paint with that
///    overlay's resolved bg.
/// 2. **Virtual-line fallback** — when (1) didn't fire AND the row
///    is `LineStart::AfterInjectedNewline`, paint the virtual line's
///    own bg (set by the virtual-line builder), so plugins can paint
///    full-row stripes by setting bg on the virtual line itself even
///    when the line text is empty.
pub(super) fn resolve_tail_fill(input: TailFillInput<'_>) -> Option<TailFillResult> {
    let TailFillInput {
        current_view_line,
        theme,
        overlay_fill,
        syntax_extend_bg,
        first_line_byte_pos,
        last_line_byte_pos,
    } = input;

    let row_had_source_bytes = first_line_byte_pos.is_some() && last_line_byte_pos.is_some();
    let overlay_style = if row_had_source_bytes {
        overlay_fill.and_then(|overlay| overlay_bg_style(overlay, theme))
    } else {
        None
    };

    // Diff-syntax row-bg wash — slots between the overlay layer (so
    // plugin overlays still win) and the virtual-line fallback. Set
    // fg = bg so terminals that suppress empty-bg ANSI sequences
    // still emit the colour, mirroring `overlay_bg_style`.
    let syntax_style = if row_had_source_bytes {
        syntax_extend_bg.map(|bg| Style::default().fg(bg).bg(bg))
    } else {
        None
    };

    let style = overlay_style
        .or(syntax_style)
        .or_else(|| virtual_line_fallback_style(current_view_line, theme))?;

    // Virtual lines stay None so visual-line motion keeps skipping
    // them; non-virtual rows carry the line's start byte (so empty
    // source lines aren't mistaken for decorations).
    let source_byte = if current_view_line.line_start == LineStart::AfterInjectedNewline {
        None
    } else {
        current_view_line.source_start_byte
    };

    Some(TailFillResult { style, source_byte })
}

/// Resolve the fill bg an `extend_to_line_end` overlay paints with.
/// Also used by the blank-line indentation-guide synthesis: guide
/// cells render before the tail fill and count as "rendered", so
/// without carrying this bg themselves they'd punch holes in the
/// overlay's full-width band on empty rows.
pub(super) fn overlay_bg_style(overlay: &Overlay, theme: &Theme) -> Option<Style> {
    use crate::view::overlay::OverlayFace;

    // Set fg = bg so terminals that suppress empty-bg ANSI sequences
    // still emit the colour for the fill cells.
    let bg = match &overlay.face {
        OverlayFace::Background { color } => Some(*color),
        OverlayFace::Style { style } => style.bg,
        OverlayFace::ThemedStyle {
            fallback_style,
            bg_theme,
            ..
        } => bg_theme
            .as_ref()
            .and_then(|key| theme.resolve_theme_key(key))
            .or(fallback_style.bg),
        _ => None,
    }?;
    Some(Style::default().fg(bg).bg(bg))
}

fn virtual_line_fallback_style(view_line: &ViewLine, theme: &Theme) -> Option<Style> {
    if view_line.line_start != LineStart::AfterInjectedNewline {
        return None;
    }
    // The virtual-line builder always sets `virtual_line_style`; older
    // virtual entries that didn't go through it fall back to the first
    // char's style (covers empty deletion virtual rows where there's
    // no first char at all — those rely on `virtual_line_style`).
    let token_style = view_line
        .virtual_line_style
        .as_ref()
        .or_else(|| view_line.char_styles.first().and_then(|s| s.as_ref()))?;
    let bg = token_style.bg.as_ref()?.to_ratatui(theme);
    Some(Style::default().fg(bg).bg(bg))
}
