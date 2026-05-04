//! Per-character style computation.
//!
//! Given a description of a single cell (byte position, syntax highlight,
//! overlays, cursor / selection state, theme) this module returns the final
//! ratatui `Style` together with theme-key provenance used by the theme
//! inspector. The input and output structs are private to this module so they
//! never leak to callers outside `split_rendering`.

use crate::view::overlay::{Overlay, OverlayFace};
use crate::view::theme::Theme;
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
        if let Some((r, g, b)) = ts.fg {
            s = s.fg(Color::Rgb(r, g, b));
        } else {
            s = s.fg(ctx.theme.editor_fg);
            fg_theme_key = Some("editor.fg");
        }
        if let Some((r, g, b)) = ts.bg {
            s = s.bg(Color::Rgb(r, g, b));
        }
        if ts.bold {
            s = s.add_modifier(Modifier::BOLD);
        }
        if ts.italic {
            s = s.add_modifier(Modifier::ITALIC);
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
            } => {
                let mut themed_style = *fallback_style;
                if let Some(fg_key) = fg_theme {
                    if let Some(color) = ctx.theme.resolve_theme_key(fg_key) {
                        themed_style = themed_style.fg(color);
                    }
                }
                if let Some(bg_key) = bg_theme {
                    if let Some(color) = ctx.theme.resolve_theme_key(bg_key) {
                        themed_style = themed_style.bg(color);
                    }
                    let m = ctx.theme.modifier_for_bg_key(bg_key);
                    if !m.is_empty() {
                        themed_style = themed_style.add_modifier(m);
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
