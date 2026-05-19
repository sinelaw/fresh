//! Per-character style computation.
//!
//! Given a description of a single cell (byte position, syntax highlight,
//! overlays, cursor / selection state, theme) this module returns the final
//! ratatui `Style` together with theme-key provenance used by the theme
//! inspector. The input and output structs are private to this module so they
//! never leak to callers outside `split_rendering`.

use crate::view::overlay::{Overlay, OverlayFace};
use crate::view::theme::{Theme, TokenColorExt};
use fresh_core::api::ViewTokenStyle;
use ratatui::style::{Color, Modifier, Style};

/// Context for computing the style of a single character.
pub(super) struct CharStyleContext<'a> {
    pub byte_pos: Option<usize>,
    pub token_style: Option<&'a ViewTokenStyle>,
    pub ansi_style: Style,
    pub is_cursor: bool,
    pub is_selected: bool,
    pub theme: &'a Theme,
    /// Pre-resolved syntax highlight color for this byte position.
    pub highlight_color: Option<Color>,
    /// Theme key for the syntax highlight category (e.g. "syntax.keyword").
    pub highlight_theme_key: Option<&'static str>,
    /// Pre-resolved syntax highlight background colour for this byte
    /// position. `Some(..)` only for diff categories (Inserted /
    /// Deleted / Changed); `None` keeps the existing fg-only path.
    pub highlight_bg: Option<Color>,
    /// Theme key for the bg above, when set. Surfaced to the theme
    /// inspector.
    pub highlight_bg_theme_key: Option<&'static str>,
    /// Pre-resolved semantic token color for this byte position.
    pub semantic_token_color: Option<Color>,
    /// Overlays currently active at `byte_pos`, already in priority-ascending
    /// order ("last write wins"). Empty when `byte_pos` is `None`.
    pub active_overlays: &'a [&'a Overlay],
    pub primary_cursor_position: usize,
    pub is_active: bool,
    /// Skip REVERSED style on the primary cursor cell. True when a hardware
    /// cursor is available (not software_cursor_only), or in session mode.
    pub skip_primary_cursor_reverse: bool,
    /// Whether this character is on the cursor line and current-line
    /// highlighting is enabled.
    pub is_cursor_line_highlighted: bool,
    /// Background color for the current line.
    pub current_line_bg: Color,
}

/// Output from [`compute_char_style`].
pub(super) struct CharStyleOutput {
    pub style: Style,
    pub is_secondary_cursor: bool,
    /// Theme key for the foreground color used on this cell.
    pub fg_theme_key: Option<&'static str>,
    /// Theme key for the background color used on this cell.
    pub bg_theme_key: Option<&'static str>,
    /// Region label for this cell.
    pub region: &'static str,
}

/// Compute the style for a character by layering:
/// token -> ANSI -> syntax -> semantic -> overlays -> selection -> cursor.
/// Also tracks which theme keys produced the final fg/bg colors.
pub(super) fn compute_char_style(ctx: &CharStyleContext) -> CharStyleOutput {
    let highlight_color = ctx.highlight_color;

    // Track theme key provenance alongside style
    let mut fg_theme_key: Option<&'static str> = None;
    let mut bg_theme_key: Option<&'static str> = Some("editor.bg");
    let mut region: &'static str = "Editor Content";

    // Start with token style if present (for injected content like annotation headers)
    // Otherwise use ANSI/syntax/theme default
    let mut style = if let Some(ts) = ctx.token_style {
        let mut s = Style::default();
        if let Some(ref fg) = ts.fg {
            s = s.fg(fg.to_ratatui(ctx.theme));
        } else {
            s = s.fg(ctx.theme.editor_fg);
            fg_theme_key = Some("editor.fg");
        }
        if let Some(ref bg) = ts.bg {
            s = s.bg(bg.to_ratatui(ctx.theme));
        }
        if ts.bold {
            s = s.add_modifier(Modifier::BOLD);
        }
        if ts.italic {
            s = s.add_modifier(Modifier::ITALIC);
        }
        if ts.underline {
            s = s.add_modifier(Modifier::UNDERLINED);
        }
        region = "Plugin Token";
        s
    } else if ctx.ansi_style.fg.is_some()
        || ctx.ansi_style.bg.is_some()
        || !ctx.ansi_style.add_modifier.is_empty()
    {
        // Apply ANSI styling from escape codes
        let mut s = Style::default();
        if let Some(fg) = ctx.ansi_style.fg {
            s = s.fg(fg);
        } else {
            s = s.fg(ctx.theme.editor_fg);
            fg_theme_key = Some("editor.fg");
        }
        if let Some(bg) = ctx.ansi_style.bg {
            s = s.bg(bg);
            bg_theme_key = None; // ANSI bg, not from theme
        }
        s = s.add_modifier(ctx.ansi_style.add_modifier);
        region = "ANSI Escape";
        s
    } else if let Some(color) = highlight_color {
        // Apply syntax highlighting
        fg_theme_key = ctx.highlight_theme_key;
        Style::default().fg(color)
    } else {
        // Default color from theme
        fg_theme_key = Some("editor.fg");
        Style::default().fg(ctx.theme.editor_fg)
    };

    // If we have ANSI style but also syntax highlighting, syntax takes precedence for color
    // (unless ANSI has explicit color which we already applied above)
    if let Some(color) = highlight_color {
        if ctx.ansi_style.fg.is_none()
            && (ctx.ansi_style.bg.is_some() || !ctx.ansi_style.add_modifier.is_empty())
        {
            style = style.fg(color);
            fg_theme_key = ctx.highlight_theme_key;
        }
    }

    // Syntax-driven background: diff categories (markup.inserted /
    // markup.deleted / meta.diff.range) carry a bg the renderer
    // applies as a row wash. Slots BELOW overlays (so an
    // `addOverlay` from a plugin can still paint over the diff
    // colour) but ABOVE ANSI bg (so the syntax intent wins).
    if let Some(bg) = ctx.highlight_bg {
        style = style.bg(bg);
        if let Some(key) = ctx.highlight_bg_theme_key {
            bg_theme_key = Some(key);
        }
    }

    // Apply LSP semantic token foreground color when no custom token style is set.
    if ctx.token_style.is_none() {
        if let Some(color) = ctx.semantic_token_color {
            style = style.fg(color);
            // Semantic tokens don't have a single static key; leave fg_theme_key as-is
            // (the syntax highlight key is a reasonable approximation)
        }
    }

    // Apply overlay styles — last overlay wins for each attribute
    for overlay in ctx.active_overlays {
        match &overlay.face {
            OverlayFace::Underline {
                color,
                style: _underline_style,
            } => {
                style = style.add_modifier(Modifier::UNDERLINED).fg(*color);
                if let Some(key) = overlay.theme_key {
                    fg_theme_key = Some(key);
                }
            }
            OverlayFace::Background { color } => {
                style = style.bg(*color);
                if let Some(key) = overlay.theme_key {
                    bg_theme_key = Some(key);
                    // Pick up any SGR modifier the theme associates with
                    // this bg slot (e.g. terminal-adaptive themes ship
                    // `Reversed` for `ui.semantic_highlight_bg`).
                    let m = ctx.theme.modifier_for_bg_key(key);
                    if !m.is_empty() {
                        style = style.add_modifier(m);
                    }
                }
            }
            OverlayFace::Foreground { color } => {
                style = style.fg(*color);
                if let Some(key) = overlay.theme_key {
                    fg_theme_key = Some(key);
                }
            }
            OverlayFace::Style {
                style: overlay_style,
            } => {
                style = style.patch(*overlay_style);
                if let Some(key) = overlay.theme_key {
                    if overlay_style.bg.is_some() {
                        bg_theme_key = Some(key);
                    }
                    if overlay_style.fg.is_some() {
                        fg_theme_key = Some(key);
                    }
                }
            }
            OverlayFace::ThemedStyle {
                fallback_style,
                fg_theme,
                bg_theme,
                fg_on_collision_only,
            } => {
                let mut themed_style = *fallback_style;
                // Resolve bg first so the collision check below sees it.
                if let Some(bg_key) = bg_theme {
                    if let Some(color) = ctx.theme.resolve_theme_key(bg_key) {
                        themed_style = themed_style.bg(color);
                    }
                    let m = ctx.theme.modifier_for_bg_key(bg_key);
                    if !m.is_empty() {
                        themed_style = themed_style.add_modifier(m);
                    }
                }
                if let Some(fg_key) = fg_theme {
                    if let Some(color) = ctx.theme.resolve_theme_key(fg_key) {
                        let apply = if *fg_on_collision_only {
                            let new_bg = themed_style.bg.or(style.bg);
                            matches!((style.fg, new_bg), (Some(f), Some(b)) if f == b)
                        } else {
                            true
                        };
                        if apply {
                            themed_style = themed_style.fg(color);
                        }
                    }
                }
                style = style.patch(themed_style);
            }
        }
    }

    // Apply current line background highlight (before selection, so selection overrides it)
    if ctx.is_cursor_line_highlighted && !ctx.is_selected && style.bg.is_none() {
        style = style.bg(ctx.current_line_bg);
    }

    // Apply selection highlighting (preserve fg/syntax colors, only change bg).
    // Themes may also opt into SGR text attributes here (e.g. `Reversed`)
    // so a native-palette theme can swap fg/bg via the terminal instead
    // of relying on a fixed bg color — see `Theme::selection_modifier`.
    if ctx.is_selected {
        style = style.bg(ctx.theme.selection_bg);
        if !ctx.theme.selection_modifier.is_empty() {
            style = style.add_modifier(ctx.theme.selection_modifier);
        }
        bg_theme_key = Some("editor.selection_bg");
        region = "Selection";
    }

    // Apply cursor styling.
    let is_secondary_cursor = ctx.is_cursor && ctx.byte_pos != Some(ctx.primary_cursor_position);
    if ctx.is_active {
        if ctx.is_cursor {
            if ctx.skip_primary_cursor_reverse {
                if is_secondary_cursor {
                    style = style.add_modifier(Modifier::REVERSED);
                }
            } else {
                style = style.add_modifier(Modifier::REVERSED);
            }
            region = "Cursor";
        }
    } else if ctx.is_cursor {
        style = style.fg(ctx.theme.editor_fg).bg(ctx.theme.inactive_cursor);
        fg_theme_key = Some("editor.fg");
        bg_theme_key = Some("editor.inactive_cursor");
        region = "Inactive Cursor";
    }

    CharStyleOutput {
        style,
        is_secondary_cursor,
        fg_theme_key,
        bg_theme_key,
        region,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::marker::MarkerList;
    use crate::view::overlay::Overlay;
    use crate::view::theme::{Theme, THEME_TERMINAL};
    use ratatui::style::Color;

    fn themed_overlay(
        marker_list: &mut MarkerList,
        fg_key: &str,
        bg_key: &str,
        fg_on_collision_only: bool,
    ) -> Overlay {
        Overlay::new(
            marker_list,
            0..10,
            OverlayFace::ThemedStyle {
                fallback_style: Style::default(),
                fg_theme: Some(fg_key.to_string()),
                bg_theme: Some(bg_key.to_string()),
                fg_on_collision_only,
            },
        )
    }

    fn run(theme: &Theme, overlay: &Overlay, existing_fg: Option<Color>) -> CharStyleOutput {
        let overlays: Vec<&Overlay> = vec![overlay];
        compute_char_style(&CharStyleContext {
            byte_pos: Some(0),
            token_style: None,
            ansi_style: Style::default(),
            is_cursor: false,
            is_selected: false,
            theme,
            highlight_color: existing_fg,
            highlight_theme_key: None,
            highlight_bg: None,
            highlight_bg_theme_key: None,
            semantic_token_color: None,
            active_overlays: &overlays,
            primary_cursor_position: 0,
            is_active: true,
            skip_primary_cursor_reverse: true,
            is_cursor_line_highlighted: false,
            current_line_bg: theme.current_line_bg,
        })
    }

    fn diff_add(marker_list: &mut MarkerList) -> Overlay {
        themed_overlay(
            marker_list,
            "editor.diff_add_collision_fg",
            "editor.diff_add_bg",
            true,
        )
    }

    #[test]
    fn collision_only_preserves_non_matching_fg() {
        // Red keyword on Green diff-add bg: no collision → keep Red.
        let theme = Theme::load_builtin(THEME_TERMINAL).unwrap();
        let mut ml = MarkerList::new();
        ml.set_buffer_size(100);
        let o = diff_add(&mut ml);

        let out = run(&theme, &o, Some(Color::Red));

        assert_eq!(out.style.bg, Some(Color::Green));
        assert_eq!(out.style.fg, Some(Color::Red));
    }

    #[test]
    fn collision_only_applies_on_same_colour() {
        // Green string on Green diff-add bg: collision → Black.
        let theme = Theme::load_builtin(THEME_TERMINAL).unwrap();
        let mut ml = MarkerList::new();
        ml.set_buffer_size(100);
        let o = diff_add(&mut ml);

        let out = run(&theme, &o, Some(Color::Green));

        assert_eq!(out.style.bg, Some(Color::Green));
        assert_eq!(out.style.fg, Some(Color::Black));
    }

    #[test]
    fn default_themed_overlay_always_repaints_fg() {
        // Without the flag, ThemedStyle fg overrides existing fg —
        // matches the behaviour search-match and similar overlays
        // rely on.
        let theme = Theme::load_builtin(THEME_TERMINAL).unwrap();
        let mut ml = MarkerList::new();
        ml.set_buffer_size(100);
        let o = themed_overlay(&mut ml, "search.match_fg", "search.match_bg", false);

        let out = run(&theme, &o, Some(Color::Blue));

        assert_eq!(out.style.bg, Some(Color::Yellow));
        assert_eq!(out.style.fg, Some(Color::Black));
    }

    #[test]
    fn collision_only_leaves_reset_fg_alone() {
        // Color::Reset (no syntax token) is not equal to any concrete
        // bg, so the override stays off — the terminal renders its
        // own default fg on the diff bg.
        let theme = Theme::load_builtin(THEME_TERMINAL).unwrap();
        let mut ml = MarkerList::new();
        ml.set_buffer_size(100);
        let o = diff_add(&mut ml);

        let out = run(&theme, &o, None);

        assert_eq!(out.style.bg, Some(Color::Green));
        assert_eq!(out.style.fg, Some(Color::Reset));
    }
}
