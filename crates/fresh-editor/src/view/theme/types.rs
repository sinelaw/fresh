//! Pure theme types without I/O operations.
//!
//! This module contains all theme-related data structures that can be used
//! without filesystem access. This enables WASM compatibility and easier testing.

use ratatui::style::{Color, Modifier};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

pub const THEME_DARK: &str = "dark";
pub const THEME_LIGHT: &str = "light";
pub const THEME_HIGH_CONTRAST: &str = "high-contrast";
pub const THEME_NOSTALGIA: &str = "nostalgia";
pub const THEME_DRACULA: &str = "dracula";
pub const THEME_NORD: &str = "nord";
pub const THEME_SOLARIZED_DARK: &str = "solarized-dark";
/// Theme that defers to the host terminal's palette and background
/// (uses `Default` and named ANSI colors for everything visual), so
/// fresh inherits whatever colorscheme the terminal already has.
pub const THEME_TERMINAL: &str = "terminal";

/// A builtin theme with its name, pack, and embedded JSON content.
pub struct BuiltinTheme {
    pub name: &'static str,
    /// Pack name (subdirectory path, empty for root themes)
    pub pack: &'static str,
    pub json: &'static str,
}

// Include the auto-generated BUILTIN_THEMES array from build.rs
include!(concat!(env!("OUT_DIR"), "/builtin_themes.rs"));

/// Information about an available theme.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThemeInfo {
    /// Theme display name (e.g., "dark", "adwaita-dark")
    pub name: String,
    /// Pack name (subdirectory path, empty for root themes)
    pub pack: String,
    /// Unique key used as the registry identifier.
    ///
    /// Derivation priority:
    /// 1. Package themes: `{repository_url}#{theme_name}`
    /// 2. User-saved themes (theme editor): `file://{absolute_path}`
    /// 3. Loose user themes: `{pack}/{name}` or just `{name}` if pack is empty
    /// 4. Builtins: just the name
    pub key: String,
}

impl ThemeInfo {
    /// Create a new ThemeInfo. The key defaults to `pack/name` (or just `name`
    /// when pack is empty).
    pub fn new(name: impl Into<String>, pack: impl Into<String>) -> Self {
        let name = name.into();
        let pack = pack.into();
        let key = if pack.is_empty() {
            name.clone()
        } else {
            format!("{}/{}", pack, name)
        };
        Self { name, pack, key }
    }

    /// Create a ThemeInfo with an explicit key (e.g. a repository URL).
    pub fn with_key(
        name: impl Into<String>,
        pack: impl Into<String>,
        key: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            pack: pack.into(),
            key: key.into(),
        }
    }

    /// Get display name showing pack if present
    pub fn display_name(&self) -> String {
        if self.pack.is_empty() {
            self.name.clone()
        } else {
            format!("{} ({})", self.name, self.pack)
        }
    }
}

/// Convert a ratatui Color to RGB values.
/// Returns None for Reset or Indexed colors.
pub fn color_to_rgb(color: Color) -> Option<(u8, u8, u8)> {
    match color {
        Color::Rgb(r, g, b) => Some((r, g, b)),
        Color::White => Some((255, 255, 255)),
        Color::Black => Some((0, 0, 0)),
        Color::Red => Some((205, 0, 0)),
        Color::Green => Some((0, 205, 0)),
        Color::Blue => Some((0, 0, 238)),
        Color::Yellow => Some((205, 205, 0)),
        Color::Magenta => Some((205, 0, 205)),
        Color::Cyan => Some((0, 205, 205)),
        Color::Gray => Some((229, 229, 229)),
        Color::DarkGray => Some((127, 127, 127)),
        Color::LightRed => Some((255, 0, 0)),
        Color::LightGreen => Some((0, 255, 0)),
        Color::LightBlue => Some((92, 92, 255)),
        Color::LightYellow => Some((255, 255, 0)),
        Color::LightMagenta => Some((255, 0, 255)),
        Color::LightCyan => Some((0, 255, 255)),
        Color::Reset | Color::Indexed(_) => None,
    }
}

/// Brighten a color by adding an amount to each RGB component.
/// Clamps values to 255.
pub fn brighten_color(color: Color, amount: u8) -> Color {
    if let Some((r, g, b)) = color_to_rgb(color) {
        Color::Rgb(
            r.saturating_add(amount),
            g.saturating_add(amount),
            b.saturating_add(amount),
        )
    } else {
        color
    }
}

/// Shift an RGB color a small amount toward the opposite end of the
/// brightness spectrum: dark colors become slightly brighter, light colors
/// slightly darker. Non-RGB colors are returned unchanged.
///
/// Used to derive subtle visual cues (e.g. post-EOF background shade) from
/// a theme's editor background without requiring theme authors to pick an
/// explicit color.
pub fn shade_toward_contrast(color: Color, amount: u8) -> Color {
    if let Some((r, g, b)) = color_to_rgb(color) {
        let avg = (u16::from(r) + u16::from(g) + u16::from(b)) / 3;
        if avg < 128 {
            Color::Rgb(
                r.saturating_add(amount),
                g.saturating_add(amount),
                b.saturating_add(amount),
            )
        } else {
            Color::Rgb(
                r.saturating_sub(amount),
                g.saturating_sub(amount),
                b.saturating_sub(amount),
            )
        }
    } else {
        color
    }
}

/// Serializable color representation
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum ColorDef {
    /// RGB color as [r, g, b]
    Rgb(u8, u8, u8),
    /// Named color
    Named(String),
}

impl From<ColorDef> for Color {
    fn from(def: ColorDef) -> Self {
        match def {
            ColorDef::Rgb(r, g, b) => Color::Rgb(r, g, b),
            ColorDef::Named(name) => match name.as_str() {
                "Black" => Color::Black,
                "Red" => Color::Red,
                "Green" => Color::Green,
                "Yellow" => Color::Yellow,
                "Blue" => Color::Blue,
                "Magenta" => Color::Magenta,
                "Cyan" => Color::Cyan,
                "Gray" => Color::Gray,
                "DarkGray" => Color::DarkGray,
                "LightRed" => Color::LightRed,
                "LightGreen" => Color::LightGreen,
                "LightYellow" => Color::LightYellow,
                "LightBlue" => Color::LightBlue,
                "LightMagenta" => Color::LightMagenta,
                "LightCyan" => Color::LightCyan,
                "White" => Color::White,
                // Default/Reset uses the terminal's default color (preserves transparency)
                "Default" | "Reset" => Color::Reset,
                _ => Color::White, // Default fallback
            },
        }
    }
}

/// Serializable text-attribute modifier list.
///
/// Lets a theme specify SGR text attributes (reverse video, bold,
/// italic, underline, dim) on top of fg/bg colors. Designed for
/// terminal-adaptive themes that want to use `["reversed"]` on the
/// visual selection — the canonical pattern documented for native-
/// palette themes (vim/neovim Visual mode, helix term16, htop, less)
/// because reverse video automatically inverts the terminal's
/// current fg/bg and so adapts to both light and dark backgrounds
/// without a separate variant.
///
/// JSON form: `["reversed"]` or `["bold", "underlined"]`. Unknown
/// strings are silently dropped so a typo can't crash a render.
#[derive(Debug, Clone, Default, Serialize, Deserialize, JsonSchema)]
#[serde(transparent)]
pub struct ModifierDef(pub Vec<String>);

impl From<&ModifierDef> for Modifier {
    fn from(def: &ModifierDef) -> Self {
        let mut m = Modifier::empty();
        for s in &def.0 {
            match s.as_str() {
                "reversed" | "reverse" => m |= Modifier::REVERSED,
                "bold" => m |= Modifier::BOLD,
                "italic" => m |= Modifier::ITALIC,
                "underlined" | "underline" => m |= Modifier::UNDERLINED,
                "dim" => m |= Modifier::DIM,
                _ => {}
            }
        }
        m
    }
}

impl From<ModifierDef> for Modifier {
    fn from(def: ModifierDef) -> Self {
        Modifier::from(&def)
    }
}

impl From<Modifier> for ModifierDef {
    fn from(m: Modifier) -> Self {
        // Order matches the canonical order in the parser, so a
        // round-trip Theme -> ThemeFile -> Theme yields the same set.
        let mut out = Vec::new();
        if m.contains(Modifier::REVERSED) {
            out.push("reversed".to_string());
        }
        if m.contains(Modifier::BOLD) {
            out.push("bold".to_string());
        }
        if m.contains(Modifier::ITALIC) {
            out.push("italic".to_string());
        }
        if m.contains(Modifier::UNDERLINED) {
            out.push("underlined".to_string());
        }
        if m.contains(Modifier::DIM) {
            out.push("dim".to_string());
        }
        ModifierDef(out)
    }
}

/// Convert a named color string (e.g. "Yellow", "Red") to a ratatui Color.
/// Returns None if the string is not a recognized named color.
pub fn named_color_from_str(name: &str) -> Option<Color> {
    match name {
        "Black" => Some(Color::Black),
        "Red" => Some(Color::Red),
        "Green" => Some(Color::Green),
        "Yellow" => Some(Color::Yellow),
        "Blue" => Some(Color::Blue),
        "Magenta" => Some(Color::Magenta),
        "Cyan" => Some(Color::Cyan),
        "Gray" => Some(Color::Gray),
        "DarkGray" => Some(Color::DarkGray),
        "LightRed" => Some(Color::LightRed),
        "LightGreen" => Some(Color::LightGreen),
        "LightYellow" => Some(Color::LightYellow),
        "LightBlue" => Some(Color::LightBlue),
        "LightMagenta" => Some(Color::LightMagenta),
        "LightCyan" => Some(Color::LightCyan),
        "White" => Some(Color::White),
        "Default" | "Reset" => Some(Color::Reset),
        _ => None,
    }
}

/// Convert a ratatui `Color` into the lossless `TokenColor::Named`
/// string form used by `ViewTokenStyle` (for everything except
/// `Color::Rgb`, which uses the array variant). The corresponding
/// inverse lives on [`TokenColorExt::to_ratatui`].
fn token_color_named_from_ratatui(color: Color) -> &'static str {
    match color {
        Color::Black => "Black",
        Color::Red => "Red",
        Color::Green => "Green",
        Color::Yellow => "Yellow",
        Color::Blue => "Blue",
        Color::Magenta => "Magenta",
        Color::Cyan => "Cyan",
        Color::Gray => "Gray",
        Color::DarkGray => "DarkGray",
        Color::LightRed => "LightRed",
        Color::LightGreen => "LightGreen",
        Color::LightYellow => "LightYellow",
        Color::LightBlue => "LightBlue",
        Color::LightMagenta => "LightMagenta",
        Color::LightCyan => "LightCyan",
        Color::White => "White",
        Color::Reset => "Default",
        // Rgb and Indexed are handled by callers; this fn is for the
        // named-only set above.
        _ => "Default",
    }
}

/// Resolve a `TokenColor` (the lossless RGB-or-named color carried by
/// `ViewTokenStyle`) and produce a ratatui `Color` ready for the
/// renderer. Named strings try (in order) an ANSI name, then
/// `"Indexed:N"` for 256-color values, then a theme-key lookup
/// against `theme`. Unknown strings fall through to `Color::Reset`
/// so a typo in a plugin can't make text disappear.
pub trait TokenColorExt {
    fn to_ratatui(&self, theme: &Theme) -> Color;
    fn from_ratatui(color: Color) -> Option<fresh_core::api::TokenColor>;
}

impl TokenColorExt for fresh_core::api::TokenColor {
    fn to_ratatui(&self, theme: &Theme) -> Color {
        use fresh_core::api::TokenColor;
        match self {
            TokenColor::Rgb(r, g, b) => Color::Rgb(*r, *g, *b),
            TokenColor::Named(name) => {
                if let Some(c) = named_color_from_str(name) {
                    return c;
                }
                if let Some(rest) = name.strip_prefix("Indexed:") {
                    if let Ok(n) = rest.parse::<u8>() {
                        return Color::Indexed(n);
                    }
                }
                theme.resolve_theme_key(name).unwrap_or(Color::Reset)
            }
        }
    }

    fn from_ratatui(color: Color) -> Option<fresh_core::api::TokenColor> {
        use fresh_core::api::TokenColor;
        match color {
            Color::Rgb(r, g, b) => Some(TokenColor::Rgb(r, g, b)),
            Color::Indexed(n) => Some(TokenColor::Named(format!("Indexed:{n}"))),
            other => Some(TokenColor::Named(
                token_color_named_from_ratatui(other).to_string(),
            )),
        }
    }
}

impl From<Color> for ColorDef {
    fn from(color: Color) -> Self {
        match color {
            Color::Rgb(r, g, b) => ColorDef::Rgb(r, g, b),
            Color::White => ColorDef::Named("White".to_string()),
            Color::Black => ColorDef::Named("Black".to_string()),
            Color::Red => ColorDef::Named("Red".to_string()),
            Color::Green => ColorDef::Named("Green".to_string()),
            Color::Blue => ColorDef::Named("Blue".to_string()),
            Color::Yellow => ColorDef::Named("Yellow".to_string()),
            Color::Magenta => ColorDef::Named("Magenta".to_string()),
            Color::Cyan => ColorDef::Named("Cyan".to_string()),
            Color::Gray => ColorDef::Named("Gray".to_string()),
            Color::DarkGray => ColorDef::Named("DarkGray".to_string()),
            Color::LightRed => ColorDef::Named("LightRed".to_string()),
            Color::LightGreen => ColorDef::Named("LightGreen".to_string()),
            Color::LightBlue => ColorDef::Named("LightBlue".to_string()),
            Color::LightYellow => ColorDef::Named("LightYellow".to_string()),
            Color::LightMagenta => ColorDef::Named("LightMagenta".to_string()),
            Color::LightCyan => ColorDef::Named("LightCyan".to_string()),
            Color::Reset => ColorDef::Named("Default".to_string()),
            Color::Indexed(_) => {
                // Fallback for indexed colors
                if let Some((r, g, b)) = color_to_rgb(color) {
                    ColorDef::Rgb(r, g, b)
                } else {
                    ColorDef::Named("Default".to_string())
                }
            }
        }
    }
}

/// Serializable theme definition (matches JSON structure)
///
/// The five color sections (`editor`, `ui`, `search`, `diagnostic`, `syntax`)
/// are all optional. Every leaf field within each section already has a
/// `#[serde(default = "…")]` fallback, so a theme JSON only needs to specify
/// the colors it cares about. This matches the minimal example shipped in
/// `docs/features/themes.md` and unblocks user-authored themes that override
/// just `editor`/`syntax` (issue #1281).
///
/// **Inheritance**: when a theme omits whole sections, the unset fields are
/// resolved against a *base* theme rather than against the per-field hardcoded
/// fallback. The base is chosen in this order:
///
/// 1. Explicit `extends` field (`"builtin://light"`, `"dark"`, etc.).
/// 2. If `editor.bg` is provided, the relative-luminance of that color picks
///    `builtin://light` or `builtin://dark` automatically — so a user theme
///    that sets a cream background gets light UI chrome without any extra
///    configuration.
/// 3. Otherwise, fall through to the per-field hardcoded defaults.
///
/// Only built-in themes are valid `extends` targets in this version. Chained
/// inheritance across user themes is intentionally out of scope here.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ThemeFile {
    /// Theme name
    pub name: String,
    /// Optional base theme to inherit from. Accepts `"builtin://NAME"` or a
    /// bare built-in name (e.g. `"dark"`, `"light"`, `"high-contrast"`).
    /// When set, every field this theme does not specify is taken from the
    /// base; explicit fields override the base. See [`ThemeFile`] for the
    /// full inheritance resolution order.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub extends: Option<String>,
    /// Editor area colors
    #[serde(default = "default_editor_colors")]
    pub editor: EditorColors,
    /// UI element colors (tabs, menus, status bar, etc.)
    #[serde(default = "default_ui_colors")]
    pub ui: UiColors,
    /// Search result highlighting colors
    #[serde(default = "default_search_colors")]
    pub search: SearchColors,
    /// LSP diagnostic colors (errors, warnings, etc.)
    #[serde(default = "default_diagnostic_colors")]
    pub diagnostic: DiagnosticColors,
    /// Syntax highlighting colors
    #[serde(default = "default_syntax_colors")]
    pub syntax: SyntaxColors,
}

// Per-section defaults piggyback on the field-level `#[serde(default = "…")]`
// already declared on every leaf — deserializing an empty object materializes
// an all-defaults section without us having to restate every field here, and
// keeps the section default in lock-step with its field defaults.
fn default_section<T: serde::de::DeserializeOwned>(section: &'static str) -> T {
    serde_json::from_str("{}").unwrap_or_else(|e| {
        panic!(
            "theme section `{}` must be default-constructible from `{{}}` \
             (every field needs `#[serde(default = ...)]`): {}",
            section, e
        )
    })
}

fn default_editor_colors() -> EditorColors {
    default_section("editor")
}

fn default_ui_colors() -> UiColors {
    default_section("ui")
}

fn default_search_colors() -> SearchColors {
    default_section("search")
}

fn default_diagnostic_colors() -> DiagnosticColors {
    default_section("diagnostic")
}

fn default_syntax_colors() -> SyntaxColors {
    default_section("syntax")
}

/// Editor area colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct EditorColors {
    /// Editor background color
    #[serde(default = "default_editor_bg")]
    pub bg: ColorDef,
    /// Default text color
    #[serde(default = "default_editor_fg")]
    pub fg: ColorDef,
    /// Cursor color
    #[serde(default = "default_cursor")]
    pub cursor: ColorDef,
    /// Cursor color in unfocused splits
    #[serde(default = "default_inactive_cursor")]
    pub inactive_cursor: ColorDef,
    /// Selected text background
    #[serde(default = "default_selection_bg")]
    pub selection_bg: ColorDef,
    /// Optional text-attribute modifiers (e.g. `["reversed"]`) layered
    /// on top of `selection_bg`. Themes that want a terminal-adaptive
    /// visual selection (the canonical pattern for native-palette
    /// themes — vim/neovim Visual, helix term16, htop, less) set
    /// `["reversed"]` here; the renderer ORs `Modifier::REVERSED` into
    /// the selected cells, which works on any terminal because it
    /// inverts whatever fg/bg the terminal already uses.
    #[serde(default)]
    pub selection_modifier: Option<ModifierDef>,
    /// Background of the line containing cursor
    #[serde(default = "default_current_line_bg")]
    pub current_line_bg: ColorDef,
    /// Line number text color
    #[serde(default = "default_line_number_fg")]
    pub line_number_fg: ColorDef,
    /// Line number gutter background
    #[serde(default = "default_line_number_bg")]
    pub line_number_bg: ColorDef,
    /// Diff added line background
    #[serde(default = "default_diff_add_bg")]
    pub diff_add_bg: ColorDef,
    /// Diff removed line background
    #[serde(default = "default_diff_remove_bg")]
    pub diff_remove_bg: ColorDef,
    /// Diff added word-level highlight background (optional override)
    /// When not set, computed by brightening diff_add_bg
    #[serde(default)]
    pub diff_add_highlight_bg: Option<ColorDef>,
    /// Diff removed word-level highlight background (optional override)
    /// When not set, computed by brightening diff_remove_bg
    #[serde(default)]
    pub diff_remove_highlight_bg: Option<ColorDef>,
    /// Diff modified line background
    #[serde(default = "default_diff_modify_bg")]
    pub diff_modify_bg: ColorDef,
    /// Fallback fg for cells whose existing fg matches `diff_add_bg`
    /// (e.g. ANSI Green-on-Green). Only applied on collision; other
    /// tokens keep their syntax colour.
    #[serde(default)]
    pub diff_add_collision_fg: Option<ColorDef>,
    /// Collision-only fallback fg for `diff_remove_bg`.
    #[serde(default)]
    pub diff_remove_collision_fg: Option<ColorDef>,
    /// Collision-only fallback fg for `diff_modify_bg`.
    #[serde(default)]
    pub diff_modify_collision_fg: Option<ColorDef>,
    /// Vertical ruler background color
    #[serde(default = "default_ruler_bg")]
    pub ruler_bg: ColorDef,
    /// Whitespace indicator foreground color (for tab arrows and space dots)
    #[serde(default = "default_whitespace_indicator_fg")]
    pub whitespace_indicator_fg: ColorDef,
    /// Bracket match highlight color (used when rainbow is disabled)
    #[serde(default = "default_bracket_match_fg")]
    pub bracket_match_fg: ColorDef,
    /// Rainbow bracket color (nesting level 1)
    #[serde(default = "default_bracket_rainbow_1")]
    pub bracket_rainbow_1: ColorDef,
    /// Rainbow bracket color (nesting level 2)
    #[serde(default = "default_bracket_rainbow_2")]
    pub bracket_rainbow_2: ColorDef,
    /// Rainbow bracket color (nesting level 3)
    #[serde(default = "default_bracket_rainbow_3")]
    pub bracket_rainbow_3: ColorDef,
    /// Rainbow bracket color (nesting level 4)
    #[serde(default = "default_bracket_rainbow_4")]
    pub bracket_rainbow_4: ColorDef,
    /// Rainbow bracket color (nesting level 5)
    #[serde(default = "default_bracket_rainbow_5")]
    pub bracket_rainbow_5: ColorDef,
    /// Rainbow bracket color (nesting level 6)
    #[serde(default = "default_bracket_rainbow_6")]
    pub bracket_rainbow_6: ColorDef,
    /// Background color for lines after end-of-file (optional override).
    /// When not set, computed as a slightly contrasting shade of `bg`
    /// (lighter for dark themes, darker for light themes) to give post-EOF
    /// rows a subtle visual separation from the buffer content.
    #[serde(default)]
    pub after_eof_bg: Option<ColorDef>,
}

// Default editor colors (for minimal themes)
fn default_editor_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_editor_fg() -> ColorDef {
    ColorDef::Rgb(212, 212, 212)
}
fn default_cursor() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_inactive_cursor() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_selection_bg() -> ColorDef {
    ColorDef::Rgb(38, 79, 120)
}
fn default_current_line_bg() -> ColorDef {
    ColorDef::Rgb(40, 40, 40)
}
fn default_line_number_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100)
}
fn default_line_number_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_diff_add_bg() -> ColorDef {
    ColorDef::Rgb(35, 60, 35) // Dark green
}
fn default_diff_remove_bg() -> ColorDef {
    ColorDef::Rgb(70, 35, 35) // Dark red
}
fn default_diff_modify_bg() -> ColorDef {
    ColorDef::Rgb(40, 38, 30) // Very subtle yellow tint, close to dark bg
}
fn default_ruler_bg() -> ColorDef {
    ColorDef::Rgb(50, 50, 50) // Subtle dark gray, slightly lighter than default editor bg
}
fn default_whitespace_indicator_fg() -> ColorDef {
    ColorDef::Rgb(70, 70, 70) // Subdued dark gray, subtle but visible
}
fn default_bracket_match_fg() -> ColorDef {
    ColorDef::Rgb(255, 215, 0) // Gold
}
fn default_bracket_rainbow_1() -> ColorDef {
    ColorDef::Rgb(255, 215, 0) // Gold
}
fn default_bracket_rainbow_2() -> ColorDef {
    ColorDef::Rgb(218, 112, 214) // Orchid
}
fn default_bracket_rainbow_3() -> ColorDef {
    ColorDef::Rgb(50, 205, 50) // Lime Green
}
fn default_bracket_rainbow_4() -> ColorDef {
    ColorDef::Rgb(30, 144, 255) // Dodger Blue
}
fn default_bracket_rainbow_5() -> ColorDef {
    ColorDef::Rgb(255, 127, 80) // Coral
}
fn default_bracket_rainbow_6() -> ColorDef {
    ColorDef::Rgb(147, 112, 219) // Medium Purple
}

/// UI element colors (tabs, menus, status bar, etc.)
///
/// Naming convention: every `*_bg` key that has text drawn on top of
/// it MUST have a matching `*_fg` key, and renderers must pair them
/// (never borrow a foreground from an unrelated surface — doing so is
/// how text ends up invisible when a theme's borrowed fg matches the
/// real bg). `popup_text_fg` is the foreground for `popup_bg` (kept
/// under its historical name with a `popup_fg` serde alias).
///
/// `*_bg` keys WITHOUT a matching `*_fg` are intentional — they don't
/// draw their own text and inherit the surrounding foreground:
///   - lines / borders / separators: `tab_separator_bg`,
///     `popup_border_fg`, `menu_border_fg`, `menu_separator_fg`,
///     `split_separator_fg`, `scrollbar_*`, `tab_drop_zone_border`
///   - selection / hover / highlight tints layered over a surface that
///     keeps its base fg: `*_selection_bg`, `*_hover_bg`,
///     `suggestion_selected_bg`, `text_input_selection_bg`,
///     `semantic_highlight_bg`, `compose_margin_bg`, `inline_code_bg`
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct UiColors {
    /// Active tab text color
    #[serde(default = "default_tab_active_fg")]
    pub tab_active_fg: ColorDef,
    /// Active tab background color
    #[serde(default = "default_tab_active_bg")]
    pub tab_active_bg: ColorDef,
    /// Inactive tab text color
    #[serde(default = "default_tab_inactive_fg")]
    pub tab_inactive_fg: ColorDef,
    /// Inactive tab background color
    #[serde(default = "default_tab_inactive_bg")]
    pub tab_inactive_bg: ColorDef,
    /// Tab bar separator color
    #[serde(default = "default_tab_separator_bg")]
    pub tab_separator_bg: ColorDef,
    /// Tab close button hover color
    #[serde(default = "default_tab_close_hover_fg")]
    pub tab_close_hover_fg: ColorDef,
    /// Tab hover background color
    #[serde(default = "default_tab_hover_bg")]
    pub tab_hover_bg: ColorDef,
    /// Menu bar background
    #[serde(default = "default_menu_bg")]
    pub menu_bg: ColorDef,
    /// Menu bar text color
    #[serde(default = "default_menu_fg")]
    pub menu_fg: ColorDef,
    /// Active menu item background
    #[serde(default = "default_menu_active_bg")]
    pub menu_active_bg: ColorDef,
    /// Active menu item text color
    #[serde(default = "default_menu_active_fg")]
    pub menu_active_fg: ColorDef,
    /// Dropdown menu background
    #[serde(default = "default_menu_dropdown_bg")]
    pub menu_dropdown_bg: ColorDef,
    /// Dropdown menu text color
    #[serde(default = "default_menu_dropdown_fg")]
    pub menu_dropdown_fg: ColorDef,
    /// Highlighted menu item background
    #[serde(default = "default_menu_highlight_bg")]
    pub menu_highlight_bg: ColorDef,
    /// Highlighted menu item text color
    #[serde(default = "default_menu_highlight_fg")]
    pub menu_highlight_fg: ColorDef,
    /// Menu border color
    #[serde(default = "default_menu_border_fg")]
    pub menu_border_fg: ColorDef,
    /// Menu separator line color
    #[serde(default = "default_menu_separator_fg")]
    pub menu_separator_fg: ColorDef,
    /// Menu item hover background
    #[serde(default = "default_menu_hover_bg")]
    pub menu_hover_bg: ColorDef,
    /// Menu item hover text color
    #[serde(default = "default_menu_hover_fg")]
    pub menu_hover_fg: ColorDef,
    /// Disabled menu item text color
    #[serde(default = "default_menu_disabled_fg")]
    pub menu_disabled_fg: ColorDef,
    /// Disabled menu item background
    #[serde(default = "default_menu_disabled_bg")]
    pub menu_disabled_bg: ColorDef,
    /// Status bar text color
    #[serde(default = "default_status_bar_fg")]
    pub status_bar_fg: ColorDef,
    /// Status bar background color
    #[serde(default = "default_status_bar_bg")]
    pub status_bar_bg: ColorDef,
    /// Command palette shortcut hint text color in status bar (falls back to status_bar_fg)
    #[serde(default)]
    pub status_palette_fg: Option<ColorDef>,
    /// Command palette shortcut hint background in status bar (falls back to status_bar_bg)
    #[serde(default)]
    pub status_palette_bg: Option<ColorDef>,
    /// Status bar LSP indicator text color when LSP is running (falls back to status_bar_fg)
    #[serde(default)]
    pub status_lsp_on_fg: Option<ColorDef>,
    /// Status bar LSP indicator background when LSP is running (falls back to status_bar_bg)
    #[serde(default)]
    pub status_lsp_on_bg: Option<ColorDef>,
    /// Status bar LSP indicator text color when LSP options are available
    /// to act on (configured-but-not-running). Drawn prominently to signal
    /// "click here to enable". Falls back to `status_warning_indicator_fg`.
    #[serde(default)]
    pub status_lsp_actionable_fg: Option<ColorDef>,
    /// Status bar LSP indicator background when LSP options are available
    /// to act on. Falls back to `status_warning_indicator_bg`.
    #[serde(default)]
    pub status_lsp_actionable_bg: Option<ColorDef>,
    /// Command prompt text color
    #[serde(default = "default_prompt_fg")]
    pub prompt_fg: ColorDef,
    /// Command prompt background
    #[serde(default = "default_prompt_bg")]
    pub prompt_bg: ColorDef,
    /// Prompt selected text color
    #[serde(default = "default_prompt_selection_fg")]
    pub prompt_selection_fg: ColorDef,
    /// Prompt selection background
    #[serde(default = "default_prompt_selection_bg")]
    pub prompt_selection_bg: ColorDef,
    /// Popup window border color
    #[serde(default = "default_popup_border_fg")]
    pub popup_border_fg: ColorDef,
    /// Popup window background
    #[serde(default = "default_popup_bg")]
    pub popup_bg: ColorDef,
    /// Popup selected item background
    #[serde(default = "default_popup_selection_bg")]
    pub popup_selection_bg: ColorDef,
    /// Selection background inside a widget Text input. Reads
    /// against `prompt_bg`, so it needs higher contrast against
    /// that tint than `editor.selection_bg` (which targets the
    /// editor surface). Defaults to the same `popup_selection_bg`
    /// blue used everywhere "selected item inside a chrome
    /// surface" is shown — same key the prompt selection uses, so
    /// the cue reads consistently across selection UIs.
    #[serde(default = "default_text_input_selection_bg")]
    pub text_input_selection_bg: ColorDef,
    /// Popup selected item text color
    #[serde(default = "default_popup_selection_fg")]
    pub popup_selection_fg: ColorDef,
    /// Popup window text color. Per the `*_bg`/`*_fg` convention this
    /// is the foreground for `popup_bg`; `popup_fg` is accepted as an
    /// alias so theme JSON can use the convention-consistent name.
    #[serde(default = "default_popup_text_fg", alias = "popup_fg")]
    pub popup_text_fg: ColorDef,
    /// Autocomplete suggestion background
    #[serde(default = "default_suggestion_bg")]
    pub suggestion_bg: ColorDef,
    /// Text color for content drawn on `suggestion_bg` (autocomplete
    /// items, the overlay-prompt title/input field). Falls back to
    /// `popup_text_fg` so existing themes need no change.
    #[serde(default)]
    pub suggestion_fg: Option<ColorDef>,
    /// Selected suggestion background
    #[serde(default = "default_suggestion_selected_bg")]
    pub suggestion_selected_bg: ColorDef,
    /// Help panel background
    #[serde(default = "default_help_bg")]
    pub help_bg: ColorDef,
    /// Help panel text color
    #[serde(default = "default_help_fg")]
    pub help_fg: ColorDef,
    /// Help keybinding text color
    #[serde(default = "default_help_key_fg")]
    pub help_key_fg: ColorDef,
    /// Help panel separator color
    #[serde(default = "default_help_separator_fg")]
    pub help_separator_fg: ColorDef,
    /// Help indicator text color
    #[serde(default = "default_help_indicator_fg")]
    pub help_indicator_fg: ColorDef,
    /// Help indicator background
    #[serde(default = "default_help_indicator_bg")]
    pub help_indicator_bg: ColorDef,
    /// Inline code block background
    #[serde(default = "default_inline_code_bg")]
    pub inline_code_bg: ColorDef,
    /// Split pane separator color
    #[serde(default = "default_split_separator_fg")]
    pub split_separator_fg: ColorDef,
    /// Split separator hover color
    #[serde(default = "default_split_separator_hover_fg")]
    pub split_separator_hover_fg: ColorDef,
    /// Scrollbar track color
    #[serde(default = "default_scrollbar_track_fg")]
    pub scrollbar_track_fg: ColorDef,
    /// Scrollbar thumb color
    #[serde(default = "default_scrollbar_thumb_fg")]
    pub scrollbar_thumb_fg: ColorDef,
    /// Scrollbar track hover color
    #[serde(default = "default_scrollbar_track_hover_fg")]
    pub scrollbar_track_hover_fg: ColorDef,
    /// Scrollbar thumb hover color
    #[serde(default = "default_scrollbar_thumb_hover_fg")]
    pub scrollbar_thumb_hover_fg: ColorDef,
    /// Compose mode margin background
    #[serde(default = "default_compose_margin_bg")]
    pub compose_margin_bg: ColorDef,
    /// Word under cursor highlight
    #[serde(default = "default_semantic_highlight_bg")]
    pub semantic_highlight_bg: ColorDef,
    /// Optional text-attribute modifiers (e.g. `["bold"]` or
    /// `["reversed"]`) layered on top of `semantic_highlight_bg`.
    /// Per the canonical native-palette pattern, current-word
    /// highlights are often shown via `Bold` (so the word stands
    /// out against other variables without altering its color slot)
    /// or `Reversed`. See `EditorColors::selection_modifier`.
    #[serde(default)]
    pub semantic_highlight_modifier: Option<ModifierDef>,
    /// Embedded terminal background (use Default for transparency)
    #[serde(default = "default_terminal_bg")]
    pub terminal_bg: ColorDef,
    /// Embedded terminal default text color
    #[serde(default = "default_terminal_fg")]
    pub terminal_fg: ColorDef,
    /// Warning indicator background in status bar
    #[serde(default = "default_status_warning_indicator_bg")]
    pub status_warning_indicator_bg: ColorDef,
    /// Warning indicator text color in status bar
    #[serde(default = "default_status_warning_indicator_fg")]
    pub status_warning_indicator_fg: ColorDef,
    /// Error indicator background in status bar
    #[serde(default = "default_status_error_indicator_bg")]
    pub status_error_indicator_bg: ColorDef,
    /// Error indicator text color in status bar
    #[serde(default = "default_status_error_indicator_fg")]
    pub status_error_indicator_fg: ColorDef,
    /// Warning indicator hover background
    #[serde(default = "default_status_warning_indicator_hover_bg")]
    pub status_warning_indicator_hover_bg: ColorDef,
    /// Warning indicator hover text color
    #[serde(default = "default_status_warning_indicator_hover_fg")]
    pub status_warning_indicator_hover_fg: ColorDef,
    /// Error indicator hover background
    #[serde(default = "default_status_error_indicator_hover_bg")]
    pub status_error_indicator_hover_bg: ColorDef,
    /// Error indicator hover text color
    #[serde(default = "default_status_error_indicator_hover_fg")]
    pub status_error_indicator_hover_fg: ColorDef,
    /// Tab drop zone background during drag
    #[serde(default = "default_tab_drop_zone_bg")]
    pub tab_drop_zone_bg: ColorDef,
    /// Tab drop zone border during drag
    #[serde(default = "default_tab_drop_zone_border")]
    pub tab_drop_zone_border: ColorDef,
    /// Settings UI selected item background
    #[serde(default = "default_settings_selected_bg")]
    pub settings_selected_bg: ColorDef,
    /// Settings UI selected item foreground (text on selected background)
    #[serde(default = "default_settings_selected_fg")]
    pub settings_selected_fg: ColorDef,
    /// File status: added file color in file explorer (falls back to diagnostic.info_fg)
    #[serde(default)]
    pub file_status_added_fg: Option<ColorDef>,
    /// File status: modified file color in file explorer (falls back to diagnostic.warning_fg)
    #[serde(default)]
    pub file_status_modified_fg: Option<ColorDef>,
    /// File status: deleted file color in file explorer (falls back to diagnostic.error_fg)
    #[serde(default)]
    pub file_status_deleted_fg: Option<ColorDef>,
    /// File status: renamed file color in file explorer (falls back to diagnostic.info_fg)
    #[serde(default)]
    pub file_status_renamed_fg: Option<ColorDef>,
    /// File status: untracked file color in file explorer (falls back to diagnostic.hint_fg)
    #[serde(default)]
    pub file_status_untracked_fg: Option<ColorDef>,
    /// File status: conflicted file color in file explorer (falls back to diagnostic.error_fg)
    #[serde(default)]
    pub file_status_conflicted_fg: Option<ColorDef>,
}

// Default tab close hover color (for backward compatibility with existing themes)
// Default tab colors (for minimal themes)
fn default_tab_active_fg() -> ColorDef {
    ColorDef::Named("Yellow".to_string())
}
fn default_tab_active_bg() -> ColorDef {
    ColorDef::Named("Blue".to_string())
}
fn default_tab_inactive_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_tab_inactive_bg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_tab_separator_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}
fn default_tab_close_hover_fg() -> ColorDef {
    ColorDef::Rgb(255, 100, 100) // Red-ish color for close button hover
}
fn default_tab_hover_bg() -> ColorDef {
    ColorDef::Rgb(70, 70, 75) // Slightly lighter than inactive tab bg for hover
}

// Default menu colors (for backward compatibility with existing themes)
fn default_menu_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 65)
}
fn default_menu_fg() -> ColorDef {
    ColorDef::Rgb(220, 220, 220)
}
fn default_menu_active_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 60)
}
fn default_menu_active_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_menu_dropdown_bg() -> ColorDef {
    ColorDef::Rgb(50, 50, 50)
}
fn default_menu_dropdown_fg() -> ColorDef {
    ColorDef::Rgb(220, 220, 220)
}
fn default_menu_highlight_bg() -> ColorDef {
    ColorDef::Rgb(70, 130, 180)
}
fn default_menu_highlight_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_menu_border_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100)
}
fn default_menu_separator_fg() -> ColorDef {
    ColorDef::Rgb(80, 80, 80)
}
fn default_menu_hover_bg() -> ColorDef {
    ColorDef::Rgb(55, 55, 55)
}
fn default_menu_hover_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
fn default_menu_disabled_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100) // Gray for disabled items
}
fn default_menu_disabled_bg() -> ColorDef {
    ColorDef::Rgb(50, 50, 50) // Same as dropdown bg
}
// Default status bar colors
fn default_status_bar_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_status_bar_bg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}

// Default prompt colors
fn default_prompt_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_prompt_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}
fn default_prompt_selection_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_prompt_selection_bg() -> ColorDef {
    ColorDef::Rgb(58, 79, 120)
}

// Default popup colors
fn default_popup_border_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_popup_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_popup_selection_bg() -> ColorDef {
    ColorDef::Rgb(58, 79, 120)
}
fn default_text_input_selection_bg() -> ColorDef {
    // Match the popup-selection blue. Widget Text inputs sit on a
    // `prompt_bg` field tint, so the selection needs the same
    // "selected on chrome" contrast that popups + prompts use.
    ColorDef::Rgb(58, 79, 120)
}
fn default_popup_selection_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255) // White text on selected popup item
}
fn default_popup_text_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}

// Default suggestion colors
fn default_suggestion_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}
fn default_suggestion_selected_bg() -> ColorDef {
    ColorDef::Rgb(58, 79, 120)
}

// Default help colors
fn default_help_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}
fn default_help_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_help_key_fg() -> ColorDef {
    ColorDef::Named("Cyan".to_string())
}
fn default_help_separator_fg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_help_indicator_fg() -> ColorDef {
    ColorDef::Named("Red".to_string())
}
fn default_help_indicator_bg() -> ColorDef {
    ColorDef::Named("Black".to_string())
}

fn default_inline_code_bg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}

// Default split separator colors
fn default_split_separator_fg() -> ColorDef {
    ColorDef::Rgb(100, 100, 100)
}
fn default_split_separator_hover_fg() -> ColorDef {
    ColorDef::Rgb(100, 149, 237) // Cornflower blue for visibility
}
fn default_scrollbar_track_fg() -> ColorDef {
    ColorDef::Named("DarkGray".to_string())
}
fn default_scrollbar_thumb_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_scrollbar_track_hover_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_scrollbar_thumb_hover_fg() -> ColorDef {
    ColorDef::Named("White".to_string())
}
fn default_compose_margin_bg() -> ColorDef {
    ColorDef::Rgb(18, 18, 18) // Darker than editor_bg for "desk" effect
}
fn default_semantic_highlight_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 80) // Subtle dark highlight for word occurrences
}
fn default_terminal_bg() -> ColorDef {
    ColorDef::Named("Default".to_string()) // Use terminal's default background (preserves transparency)
}
fn default_terminal_fg() -> ColorDef {
    ColorDef::Named("Default".to_string()) // Use terminal's default foreground
}
fn default_status_warning_indicator_bg() -> ColorDef {
    ColorDef::Rgb(181, 137, 0) // Solarized yellow/amber - noticeable but not harsh
}
fn default_status_warning_indicator_fg() -> ColorDef {
    ColorDef::Rgb(0, 0, 0) // Black text on amber background
}
fn default_status_error_indicator_bg() -> ColorDef {
    ColorDef::Rgb(220, 50, 47) // Solarized red - clearly an error
}
fn default_status_error_indicator_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255) // White text on red background
}
fn default_status_warning_indicator_hover_bg() -> ColorDef {
    ColorDef::Rgb(211, 167, 30) // Lighter amber for hover
}
fn default_status_warning_indicator_hover_fg() -> ColorDef {
    ColorDef::Rgb(0, 0, 0) // Black text on hover
}
fn default_status_error_indicator_hover_bg() -> ColorDef {
    ColorDef::Rgb(250, 80, 77) // Lighter red for hover
}
fn default_status_error_indicator_hover_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255) // White text on hover
}
fn default_tab_drop_zone_bg() -> ColorDef {
    ColorDef::Rgb(70, 130, 180) // Steel blue with transparency effect
}
fn default_tab_drop_zone_border() -> ColorDef {
    ColorDef::Rgb(100, 149, 237) // Cornflower blue for border
}
fn default_settings_selected_bg() -> ColorDef {
    ColorDef::Rgb(60, 60, 70) // Subtle highlight for selected settings item
}
fn default_settings_selected_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255) // White text on selected background
}
/// Search result highlighting colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SearchColors {
    /// Search match background color
    #[serde(default = "default_search_match_bg")]
    pub match_bg: ColorDef,
    /// Search match text color
    #[serde(default = "default_search_match_fg")]
    pub match_fg: ColorDef,
    /// Background color for jump labels (e.g. flash plugin labels).
    /// Should be visually distinct from `match_bg` so labels stand
    /// out against highlighted matches.  Default: bright magenta.
    #[serde(default = "default_search_label_bg")]
    pub label_bg: ColorDef,
    /// Foreground color for jump labels.  Should be high contrast
    /// against `label_bg` so the single label letter is unambiguous
    /// even on small terminal cells.  Default: white.
    #[serde(default = "default_search_label_fg")]
    pub label_fg: ColorDef,
}

// Default search colors
fn default_search_match_bg() -> ColorDef {
    ColorDef::Rgb(100, 100, 20)
}
fn default_search_match_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}
// Mirrors flash.nvim's default FlashLabel (links to Substitute, which
// is a magenta-family colour in most colorschemes).  The pairing is
// chosen so labels pop visually distinct from `search.match_bg`
// (typically yellow / orange).
fn default_search_label_bg() -> ColorDef {
    ColorDef::Rgb(199, 78, 189)
}
fn default_search_label_fg() -> ColorDef {
    ColorDef::Rgb(255, 255, 255)
}

/// LSP diagnostic colors (errors, warnings, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct DiagnosticColors {
    /// Error message text color
    #[serde(default = "default_diagnostic_error_fg")]
    pub error_fg: ColorDef,
    /// Error highlight background
    #[serde(default = "default_diagnostic_error_bg")]
    pub error_bg: ColorDef,
    /// Warning message text color
    #[serde(default = "default_diagnostic_warning_fg")]
    pub warning_fg: ColorDef,
    /// Warning highlight background
    #[serde(default = "default_diagnostic_warning_bg")]
    pub warning_bg: ColorDef,
    /// Info message text color
    #[serde(default = "default_diagnostic_info_fg")]
    pub info_fg: ColorDef,
    /// Info highlight background
    #[serde(default = "default_diagnostic_info_bg")]
    pub info_bg: ColorDef,
    /// Hint message text color
    #[serde(default = "default_diagnostic_hint_fg")]
    pub hint_fg: ColorDef,
    /// Hint highlight background
    #[serde(default = "default_diagnostic_hint_bg")]
    pub hint_bg: ColorDef,
}

// Default diagnostic colors
fn default_diagnostic_error_fg() -> ColorDef {
    ColorDef::Named("Red".to_string())
}
fn default_diagnostic_error_bg() -> ColorDef {
    ColorDef::Rgb(60, 20, 20)
}
fn default_diagnostic_warning_fg() -> ColorDef {
    ColorDef::Named("Yellow".to_string())
}
fn default_diagnostic_warning_bg() -> ColorDef {
    ColorDef::Rgb(60, 50, 0)
}
fn default_diagnostic_info_fg() -> ColorDef {
    ColorDef::Named("Blue".to_string())
}
fn default_diagnostic_info_bg() -> ColorDef {
    ColorDef::Rgb(0, 30, 60)
}
fn default_diagnostic_hint_fg() -> ColorDef {
    ColorDef::Named("Gray".to_string())
}
fn default_diagnostic_hint_bg() -> ColorDef {
    ColorDef::Rgb(30, 30, 30)
}

/// Syntax highlighting colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SyntaxColors {
    /// Language keywords (if, for, fn, etc.)
    #[serde(default = "default_syntax_keyword")]
    pub keyword: ColorDef,
    /// String literals
    #[serde(default = "default_syntax_string")]
    pub string: ColorDef,
    /// Code comments
    #[serde(default = "default_syntax_comment")]
    pub comment: ColorDef,
    /// Function names
    #[serde(default = "default_syntax_function")]
    pub function: ColorDef,
    /// Type names
    #[serde(rename = "type", default = "default_syntax_type")]
    pub type_: ColorDef,
    /// Variable names
    #[serde(default = "default_syntax_variable")]
    pub variable: ColorDef,
    /// Built-in language variables (self, this, super, etc.)
    #[serde(default = "default_syntax_variable_builtin")]
    pub variable_builtin: ColorDef,
    /// Constants and literals
    #[serde(default = "default_syntax_constant")]
    pub constant: ColorDef,
    /// Operators (+, -, =, etc.)
    #[serde(default = "default_syntax_operator")]
    pub operator: ColorDef,
    /// Punctuation brackets ({, }, (, ), [, ])
    #[serde(default = "default_syntax_punctuation_bracket")]
    pub punctuation_bracket: ColorDef,
    /// Punctuation delimiters (;, ,, .)
    #[serde(default = "default_syntax_punctuation_delimiter")]
    pub punctuation_delimiter: ColorDef,
}

// Default syntax colors (VSCode Dark+ inspired)
fn default_syntax_keyword() -> ColorDef {
    ColorDef::Rgb(86, 156, 214)
}
fn default_syntax_string() -> ColorDef {
    ColorDef::Rgb(206, 145, 120)
}
fn default_syntax_comment() -> ColorDef {
    ColorDef::Rgb(106, 153, 85)
}
fn default_syntax_function() -> ColorDef {
    ColorDef::Rgb(220, 220, 170)
}
fn default_syntax_type() -> ColorDef {
    ColorDef::Rgb(78, 201, 176)
}
fn default_syntax_variable() -> ColorDef {
    ColorDef::Rgb(156, 220, 254)
}
fn default_syntax_variable_builtin() -> ColorDef {
    ColorDef::Rgb(86, 156, 214) // same as keyword — self/this/super are language-defined
}
fn default_syntax_constant() -> ColorDef {
    ColorDef::Rgb(79, 193, 255)
}
fn default_syntax_operator() -> ColorDef {
    ColorDef::Rgb(212, 212, 212)
}
fn default_syntax_punctuation_bracket() -> ColorDef {
    ColorDef::Rgb(212, 212, 212) // default foreground — brackets blend with text
}
fn default_syntax_punctuation_delimiter() -> ColorDef {
    ColorDef::Rgb(212, 212, 212) // default foreground — delimiters blend with text
}

/// Comprehensive theme structure with all UI colors
#[derive(Debug, Clone)]
pub struct Theme {
    /// Theme name (e.g., "dark", "light", "high-contrast")
    pub name: String,

    // Editor colors
    pub editor_bg: Color,
    pub editor_fg: Color,
    pub cursor: Color,
    pub inactive_cursor: Color,
    pub selection_bg: Color,
    /// SGR text attributes layered onto selected cells. Empty for
    /// traditional themes; native-palette themes set
    /// `Modifier::REVERSED` so the selection inverts the terminal's
    /// current fg/bg (vim/neovim Visual, helix term16, htop, less).
    pub selection_modifier: Modifier,
    pub current_line_bg: Color,
    pub line_number_fg: Color,
    pub line_number_bg: Color,

    /// Background color for rows past end-of-file
    pub after_eof_bg: Color,

    // Vertical ruler color
    pub ruler_bg: Color,

    // Whitespace indicator color (tab arrows, space dots)
    pub whitespace_indicator_fg: Color,

    // Bracket matching colors
    pub bracket_match_fg: Color,
    pub bracket_rainbow_1: Color,
    pub bracket_rainbow_2: Color,
    pub bracket_rainbow_3: Color,
    pub bracket_rainbow_4: Color,
    pub bracket_rainbow_5: Color,
    pub bracket_rainbow_6: Color,

    // Diff highlighting colors
    pub diff_add_bg: Color,
    pub diff_remove_bg: Color,
    pub diff_modify_bg: Color,
    /// Brighter background for inline diff highlighting on added content
    pub diff_add_highlight_bg: Color,
    /// Brighter background for inline diff highlighting on removed content
    pub diff_remove_highlight_bg: Color,
    /// Collision-only fg fallback for cells whose existing fg matches
    /// `diff_*_bg`. `None` keeps the cell's original fg; overlays opt
    /// into the override via `fg_on_collision_only`.
    pub diff_add_collision_fg: Option<Color>,
    pub diff_remove_collision_fg: Option<Color>,
    pub diff_modify_collision_fg: Option<Color>,

    // UI element colors
    pub tab_active_fg: Color,
    pub tab_active_bg: Color,
    pub tab_inactive_fg: Color,
    pub tab_inactive_bg: Color,
    pub tab_separator_bg: Color,
    pub tab_close_hover_fg: Color,
    pub tab_hover_bg: Color,

    // Menu bar colors
    pub menu_bg: Color,
    pub menu_fg: Color,
    pub menu_active_bg: Color,
    pub menu_active_fg: Color,
    pub menu_dropdown_bg: Color,
    pub menu_dropdown_fg: Color,
    pub menu_highlight_bg: Color,
    pub menu_highlight_fg: Color,
    pub menu_border_fg: Color,
    pub menu_separator_fg: Color,
    pub menu_hover_bg: Color,
    pub menu_hover_fg: Color,
    pub menu_disabled_fg: Color,
    pub menu_disabled_bg: Color,

    pub status_bar_fg: Color,
    pub status_bar_bg: Color,
    /// Status bar palette shortcut hint colors (default: same as status bar)
    pub status_palette_fg: Color,
    pub status_palette_bg: Color,
    /// Status bar LSP indicator colors when running (default: same as status bar)
    pub status_lsp_on_fg: Color,
    pub status_lsp_on_bg: Color,
    /// Status bar LSP indicator colors when actionable options are available
    /// (configured-but-not-running). Default: same as status warning indicator.
    pub status_lsp_actionable_fg: Color,
    pub status_lsp_actionable_bg: Color,
    pub prompt_fg: Color,
    pub prompt_bg: Color,
    pub prompt_selection_fg: Color,
    pub prompt_selection_bg: Color,

    pub popup_border_fg: Color,
    pub popup_bg: Color,
    pub popup_selection_bg: Color,
    pub popup_selection_fg: Color,
    pub popup_text_fg: Color,
    /// Background for the selection span inside a widget Text
    /// input. See the file-format field doc for why this isn't
    /// just `editor.selection_bg`.
    pub text_input_selection_bg: Color,

    pub suggestion_bg: Color,
    pub suggestion_fg: Color,
    pub suggestion_selected_bg: Color,

    pub help_bg: Color,
    pub help_fg: Color,
    pub help_key_fg: Color,
    pub help_separator_fg: Color,

    pub help_indicator_fg: Color,
    pub help_indicator_bg: Color,

    /// Background color for inline code in help popups
    pub inline_code_bg: Color,

    pub split_separator_fg: Color,
    pub split_separator_hover_fg: Color,

    // Scrollbar colors
    pub scrollbar_track_fg: Color,
    pub scrollbar_thumb_fg: Color,
    pub scrollbar_track_hover_fg: Color,
    pub scrollbar_thumb_hover_fg: Color,

    // Compose mode colors
    pub compose_margin_bg: Color,

    // Semantic highlighting (word under cursor)
    pub semantic_highlight_bg: Color,
    /// SGR text attributes layered onto current-word-highlight cells.
    /// Native-palette themes typically set `Modifier::BOLD` (so the
    /// word stands out without altering its color slot) or
    /// `Modifier::REVERSED`.
    pub semantic_highlight_modifier: Modifier,

    // Terminal colors (for embedded terminal buffers)
    pub terminal_bg: Color,
    pub terminal_fg: Color,

    // Status bar warning/error indicator colors
    pub status_warning_indicator_bg: Color,
    pub status_warning_indicator_fg: Color,
    pub status_error_indicator_bg: Color,
    pub status_error_indicator_fg: Color,
    pub status_warning_indicator_hover_bg: Color,
    pub status_warning_indicator_hover_fg: Color,
    pub status_error_indicator_hover_bg: Color,
    pub status_error_indicator_hover_fg: Color,

    // Tab drag-and-drop colors
    pub tab_drop_zone_bg: Color,
    pub tab_drop_zone_border: Color,

    // Settings UI colors
    pub settings_selected_bg: Color,
    pub settings_selected_fg: Color,

    // File status colors (git status indicators in file explorer)
    pub file_status_added_fg: Color,
    pub file_status_modified_fg: Color,
    pub file_status_deleted_fg: Color,
    pub file_status_renamed_fg: Color,
    pub file_status_untracked_fg: Color,
    pub file_status_conflicted_fg: Color,

    // Search colors
    pub search_match_bg: Color,
    pub search_match_fg: Color,
    pub search_label_bg: Color,
    pub search_label_fg: Color,

    // Diagnostic colors
    pub diagnostic_error_fg: Color,
    pub diagnostic_error_bg: Color,
    pub diagnostic_warning_fg: Color,
    pub diagnostic_warning_bg: Color,
    pub diagnostic_info_fg: Color,
    pub diagnostic_info_bg: Color,
    pub diagnostic_hint_fg: Color,
    pub diagnostic_hint_bg: Color,

    // Syntax highlighting colors
    pub syntax_keyword: Color,
    pub syntax_string: Color,
    pub syntax_comment: Color,
    pub syntax_function: Color,
    pub syntax_type: Color,
    pub syntax_variable: Color,
    pub syntax_variable_builtin: Color,
    pub syntax_constant: Color,
    pub syntax_operator: Color,
    pub syntax_punctuation_bracket: Color,
    pub syntax_punctuation_delimiter: Color,
}

impl From<ThemeFile> for Theme {
    fn from(file: ThemeFile) -> Self {
        Self {
            name: file.name,
            editor_bg: file.editor.bg.clone().into(),
            editor_fg: file.editor.fg.into(),
            cursor: file.editor.cursor.into(),
            inactive_cursor: file.editor.inactive_cursor.into(),
            selection_bg: file.editor.selection_bg.into(),
            selection_modifier: file
                .editor
                .selection_modifier
                .as_ref()
                .map(Modifier::from)
                .unwrap_or(Modifier::empty()),
            current_line_bg: file.editor.current_line_bg.into(),
            line_number_fg: file.editor.line_number_fg.into(),
            line_number_bg: file.editor.line_number_bg.into(),
            // Use explicit override if provided, otherwise derive a subtle
            // contrasting shade from the editor background.
            after_eof_bg: file
                .editor
                .after_eof_bg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| shade_toward_contrast(file.editor.bg.clone().into(), 10)),
            ruler_bg: file.editor.ruler_bg.into(),
            whitespace_indicator_fg: file.editor.whitespace_indicator_fg.into(),
            bracket_match_fg: file.editor.bracket_match_fg.into(),
            bracket_rainbow_1: file.editor.bracket_rainbow_1.into(),
            bracket_rainbow_2: file.editor.bracket_rainbow_2.into(),
            bracket_rainbow_3: file.editor.bracket_rainbow_3.into(),
            bracket_rainbow_4: file.editor.bracket_rainbow_4.into(),
            bracket_rainbow_5: file.editor.bracket_rainbow_5.into(),
            bracket_rainbow_6: file.editor.bracket_rainbow_6.into(),
            diff_add_bg: file.editor.diff_add_bg.clone().into(),
            diff_remove_bg: file.editor.diff_remove_bg.clone().into(),
            diff_modify_bg: file.editor.diff_modify_bg.into(),
            // Use explicit override if provided, otherwise brighten from base
            diff_add_highlight_bg: file
                .editor
                .diff_add_highlight_bg
                .map(|c| c.into())
                .unwrap_or_else(|| brighten_color(file.editor.diff_add_bg.into(), 40)),
            diff_remove_highlight_bg: file
                .editor
                .diff_remove_highlight_bg
                .map(|c| c.into())
                .unwrap_or_else(|| brighten_color(file.editor.diff_remove_bg.into(), 40)),
            diff_add_collision_fg: file.editor.diff_add_collision_fg.clone().map(|c| c.into()),
            diff_remove_collision_fg: file
                .editor
                .diff_remove_collision_fg
                .clone()
                .map(|c| c.into()),
            diff_modify_collision_fg: file
                .editor
                .diff_modify_collision_fg
                .clone()
                .map(|c| c.into()),
            tab_active_fg: file.ui.tab_active_fg.into(),
            tab_active_bg: file.ui.tab_active_bg.into(),
            tab_inactive_fg: file.ui.tab_inactive_fg.into(),
            tab_inactive_bg: file.ui.tab_inactive_bg.into(),
            tab_separator_bg: file.ui.tab_separator_bg.into(),
            tab_close_hover_fg: file.ui.tab_close_hover_fg.into(),
            tab_hover_bg: file.ui.tab_hover_bg.into(),
            menu_bg: file.ui.menu_bg.into(),
            menu_fg: file.ui.menu_fg.into(),
            menu_active_bg: file.ui.menu_active_bg.into(),
            menu_active_fg: file.ui.menu_active_fg.into(),
            menu_dropdown_bg: file.ui.menu_dropdown_bg.into(),
            menu_dropdown_fg: file.ui.menu_dropdown_fg.into(),
            menu_highlight_bg: file.ui.menu_highlight_bg.into(),
            menu_highlight_fg: file.ui.menu_highlight_fg.into(),
            menu_border_fg: file.ui.menu_border_fg.into(),
            menu_separator_fg: file.ui.menu_separator_fg.into(),
            menu_hover_bg: file.ui.menu_hover_bg.into(),
            menu_hover_fg: file.ui.menu_hover_fg.into(),
            menu_disabled_fg: file.ui.menu_disabled_fg.into(),
            menu_disabled_bg: file.ui.menu_disabled_bg.into(),
            status_bar_fg: file.ui.status_bar_fg.clone().into(),
            status_bar_bg: file.ui.status_bar_bg.clone().into(),
            status_palette_fg: file
                .ui
                .status_palette_fg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.status_bar_fg.clone().into()),
            status_palette_bg: file
                .ui
                .status_palette_bg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.status_bar_bg.clone().into()),
            status_lsp_on_fg: file
                .ui
                .status_lsp_on_fg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.status_bar_fg.clone().into()),
            status_lsp_on_bg: file
                .ui
                .status_lsp_on_bg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.status_bar_bg.clone().into()),
            status_lsp_actionable_fg: file
                .ui
                .status_lsp_actionable_fg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.status_warning_indicator_fg.clone().into()),
            status_lsp_actionable_bg: file
                .ui
                .status_lsp_actionable_bg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.status_warning_indicator_bg.clone().into()),
            prompt_fg: file.ui.prompt_fg.into(),
            prompt_bg: file.ui.prompt_bg.into(),
            prompt_selection_fg: file.ui.prompt_selection_fg.into(),
            prompt_selection_bg: file.ui.prompt_selection_bg.into(),
            popup_border_fg: file.ui.popup_border_fg.into(),
            popup_bg: file.ui.popup_bg.into(),
            popup_selection_bg: file.ui.popup_selection_bg.into(),
            popup_selection_fg: file.ui.popup_selection_fg.into(),
            popup_text_fg: file.ui.popup_text_fg.clone().into(),
            text_input_selection_bg: file.ui.text_input_selection_bg.into(),
            suggestion_bg: file.ui.suggestion_bg.into(),
            suggestion_fg: file
                .ui
                .suggestion_fg
                .clone()
                .map(|c| c.into())
                .unwrap_or_else(|| file.ui.popup_text_fg.clone().into()),
            suggestion_selected_bg: file.ui.suggestion_selected_bg.into(),
            help_bg: file.ui.help_bg.into(),
            help_fg: file.ui.help_fg.into(),
            help_key_fg: file.ui.help_key_fg.into(),
            help_separator_fg: file.ui.help_separator_fg.into(),
            help_indicator_fg: file.ui.help_indicator_fg.into(),
            help_indicator_bg: file.ui.help_indicator_bg.into(),
            inline_code_bg: file.ui.inline_code_bg.into(),
            split_separator_fg: file.ui.split_separator_fg.into(),
            split_separator_hover_fg: file.ui.split_separator_hover_fg.into(),
            scrollbar_track_fg: file.ui.scrollbar_track_fg.into(),
            scrollbar_thumb_fg: file.ui.scrollbar_thumb_fg.into(),
            scrollbar_track_hover_fg: file.ui.scrollbar_track_hover_fg.into(),
            scrollbar_thumb_hover_fg: file.ui.scrollbar_thumb_hover_fg.into(),
            compose_margin_bg: file.ui.compose_margin_bg.into(),
            semantic_highlight_bg: file.ui.semantic_highlight_bg.into(),
            semantic_highlight_modifier: file
                .ui
                .semantic_highlight_modifier
                .as_ref()
                .map(Modifier::from)
                .unwrap_or(Modifier::empty()),
            terminal_bg: file.ui.terminal_bg.into(),
            terminal_fg: file.ui.terminal_fg.into(),
            status_warning_indicator_bg: file.ui.status_warning_indicator_bg.into(),
            status_warning_indicator_fg: file.ui.status_warning_indicator_fg.into(),
            status_error_indicator_bg: file.ui.status_error_indicator_bg.into(),
            status_error_indicator_fg: file.ui.status_error_indicator_fg.into(),
            status_warning_indicator_hover_bg: file.ui.status_warning_indicator_hover_bg.into(),
            status_warning_indicator_hover_fg: file.ui.status_warning_indicator_hover_fg.into(),
            status_error_indicator_hover_bg: file.ui.status_error_indicator_hover_bg.into(),
            status_error_indicator_hover_fg: file.ui.status_error_indicator_hover_fg.into(),
            tab_drop_zone_bg: file.ui.tab_drop_zone_bg.into(),
            tab_drop_zone_border: file.ui.tab_drop_zone_border.into(),
            settings_selected_bg: file.ui.settings_selected_bg.into(),
            settings_selected_fg: file.ui.settings_selected_fg.into(),
            file_status_added_fg: file
                .ui
                .file_status_added_fg
                .map(|c| c.into())
                .unwrap_or_else(|| file.diagnostic.info_fg.clone().into()),
            file_status_modified_fg: file
                .ui
                .file_status_modified_fg
                .map(|c| c.into())
                .unwrap_or_else(|| file.diagnostic.warning_fg.clone().into()),
            file_status_deleted_fg: file
                .ui
                .file_status_deleted_fg
                .map(|c| c.into())
                .unwrap_or_else(|| file.diagnostic.error_fg.clone().into()),
            file_status_renamed_fg: file
                .ui
                .file_status_renamed_fg
                .map(|c| c.into())
                .unwrap_or_else(|| file.diagnostic.info_fg.clone().into()),
            file_status_untracked_fg: file
                .ui
                .file_status_untracked_fg
                .map(|c| c.into())
                .unwrap_or_else(|| file.diagnostic.hint_fg.clone().into()),
            file_status_conflicted_fg: file
                .ui
                .file_status_conflicted_fg
                .map(|c| c.into())
                .unwrap_or_else(|| file.diagnostic.error_fg.clone().into()),
            search_match_bg: file.search.match_bg.into(),
            search_match_fg: file.search.match_fg.into(),
            search_label_bg: file.search.label_bg.into(),
            search_label_fg: file.search.label_fg.into(),
            diagnostic_error_fg: file.diagnostic.error_fg.into(),
            diagnostic_error_bg: file.diagnostic.error_bg.into(),
            diagnostic_warning_fg: file.diagnostic.warning_fg.into(),
            diagnostic_warning_bg: file.diagnostic.warning_bg.into(),
            diagnostic_info_fg: file.diagnostic.info_fg.into(),
            diagnostic_info_bg: file.diagnostic.info_bg.into(),
            diagnostic_hint_fg: file.diagnostic.hint_fg.into(),
            diagnostic_hint_bg: file.diagnostic.hint_bg.into(),
            syntax_keyword: file.syntax.keyword.into(),
            syntax_string: file.syntax.string.into(),
            syntax_comment: file.syntax.comment.into(),
            syntax_function: file.syntax.function.into(),
            syntax_type: file.syntax.type_.into(),
            syntax_variable: file.syntax.variable.into(),
            syntax_variable_builtin: file.syntax.variable_builtin.into(),
            syntax_constant: file.syntax.constant.into(),
            syntax_operator: file.syntax.operator.into(),
            syntax_punctuation_bracket: file.syntax.punctuation_bracket.into(),
            syntax_punctuation_delimiter: file.syntax.punctuation_delimiter.into(),
        }
    }
}

impl From<Theme> for ThemeFile {
    fn from(theme: Theme) -> Self {
        Self {
            name: theme.name,
            // A round-tripped `Theme` is already fully resolved — no further
            // inheritance is needed when serializing back out.
            extends: None,
            editor: EditorColors {
                bg: theme.editor_bg.into(),
                fg: theme.editor_fg.into(),
                cursor: theme.cursor.into(),
                inactive_cursor: theme.inactive_cursor.into(),
                selection_bg: theme.selection_bg.into(),
                selection_modifier: if theme.selection_modifier.is_empty() {
                    None
                } else {
                    Some(theme.selection_modifier.into())
                },
                current_line_bg: theme.current_line_bg.into(),
                line_number_fg: theme.line_number_fg.into(),
                line_number_bg: theme.line_number_bg.into(),
                diff_add_bg: theme.diff_add_bg.into(),
                diff_remove_bg: theme.diff_remove_bg.into(),
                diff_add_highlight_bg: Some(theme.diff_add_highlight_bg.into()),
                diff_remove_highlight_bg: Some(theme.diff_remove_highlight_bg.into()),
                diff_modify_bg: theme.diff_modify_bg.into(),
                diff_add_collision_fg: theme.diff_add_collision_fg.map(|c| c.into()),
                diff_remove_collision_fg: theme.diff_remove_collision_fg.map(|c| c.into()),
                diff_modify_collision_fg: theme.diff_modify_collision_fg.map(|c| c.into()),
                ruler_bg: theme.ruler_bg.into(),
                whitespace_indicator_fg: theme.whitespace_indicator_fg.into(),
                bracket_match_fg: theme.bracket_match_fg.into(),
                bracket_rainbow_1: theme.bracket_rainbow_1.into(),
                bracket_rainbow_2: theme.bracket_rainbow_2.into(),
                bracket_rainbow_3: theme.bracket_rainbow_3.into(),
                bracket_rainbow_4: theme.bracket_rainbow_4.into(),
                bracket_rainbow_5: theme.bracket_rainbow_5.into(),
                bracket_rainbow_6: theme.bracket_rainbow_6.into(),
                after_eof_bg: Some(theme.after_eof_bg.into()),
            },
            ui: UiColors {
                tab_active_fg: theme.tab_active_fg.into(),
                tab_active_bg: theme.tab_active_bg.into(),
                tab_inactive_fg: theme.tab_inactive_fg.into(),
                tab_inactive_bg: theme.tab_inactive_bg.into(),
                tab_separator_bg: theme.tab_separator_bg.into(),
                tab_close_hover_fg: theme.tab_close_hover_fg.into(),
                tab_hover_bg: theme.tab_hover_bg.into(),
                menu_bg: theme.menu_bg.into(),
                menu_fg: theme.menu_fg.into(),
                menu_active_bg: theme.menu_active_bg.into(),
                menu_active_fg: theme.menu_active_fg.into(),
                menu_dropdown_bg: theme.menu_dropdown_bg.into(),
                menu_dropdown_fg: theme.menu_dropdown_fg.into(),
                menu_highlight_bg: theme.menu_highlight_bg.into(),
                menu_highlight_fg: theme.menu_highlight_fg.into(),
                menu_border_fg: theme.menu_border_fg.into(),
                menu_separator_fg: theme.menu_separator_fg.into(),
                menu_hover_bg: theme.menu_hover_bg.into(),
                menu_hover_fg: theme.menu_hover_fg.into(),
                menu_disabled_fg: theme.menu_disabled_fg.into(),
                menu_disabled_bg: theme.menu_disabled_bg.into(),
                status_bar_fg: theme.status_bar_fg.into(),
                status_bar_bg: theme.status_bar_bg.into(),
                status_palette_fg: Some(theme.status_palette_fg.into()),
                status_palette_bg: Some(theme.status_palette_bg.into()),
                status_lsp_on_fg: Some(theme.status_lsp_on_fg.into()),
                status_lsp_on_bg: Some(theme.status_lsp_on_bg.into()),
                status_lsp_actionable_fg: Some(theme.status_lsp_actionable_fg.into()),
                status_lsp_actionable_bg: Some(theme.status_lsp_actionable_bg.into()),
                prompt_fg: theme.prompt_fg.into(),
                prompt_bg: theme.prompt_bg.into(),
                prompt_selection_fg: theme.prompt_selection_fg.into(),
                prompt_selection_bg: theme.prompt_selection_bg.into(),
                popup_border_fg: theme.popup_border_fg.into(),
                popup_bg: theme.popup_bg.into(),
                popup_selection_bg: theme.popup_selection_bg.into(),
                popup_selection_fg: theme.popup_selection_fg.into(),
                popup_text_fg: theme.popup_text_fg.into(),
                text_input_selection_bg: theme.text_input_selection_bg.into(),
                suggestion_bg: theme.suggestion_bg.into(),
                suggestion_fg: Some(theme.suggestion_fg.into()),
                suggestion_selected_bg: theme.suggestion_selected_bg.into(),
                help_bg: theme.help_bg.into(),
                help_fg: theme.help_fg.into(),
                help_key_fg: theme.help_key_fg.into(),
                help_separator_fg: theme.help_separator_fg.into(),
                help_indicator_fg: theme.help_indicator_fg.into(),
                help_indicator_bg: theme.help_indicator_bg.into(),
                inline_code_bg: theme.inline_code_bg.into(),
                split_separator_fg: theme.split_separator_fg.into(),
                split_separator_hover_fg: theme.split_separator_hover_fg.into(),
                scrollbar_track_fg: theme.scrollbar_track_fg.into(),
                scrollbar_thumb_fg: theme.scrollbar_thumb_fg.into(),
                scrollbar_track_hover_fg: theme.scrollbar_track_hover_fg.into(),
                scrollbar_thumb_hover_fg: theme.scrollbar_thumb_hover_fg.into(),
                compose_margin_bg: theme.compose_margin_bg.into(),
                semantic_highlight_bg: theme.semantic_highlight_bg.into(),
                semantic_highlight_modifier: if theme.semantic_highlight_modifier.is_empty() {
                    None
                } else {
                    Some(theme.semantic_highlight_modifier.into())
                },
                terminal_bg: theme.terminal_bg.into(),
                terminal_fg: theme.terminal_fg.into(),
                status_warning_indicator_bg: theme.status_warning_indicator_bg.into(),
                status_warning_indicator_fg: theme.status_warning_indicator_fg.into(),
                status_error_indicator_bg: theme.status_error_indicator_bg.into(),
                status_error_indicator_fg: theme.status_error_indicator_fg.into(),
                status_warning_indicator_hover_bg: theme.status_warning_indicator_hover_bg.into(),
                status_warning_indicator_hover_fg: theme.status_warning_indicator_hover_fg.into(),
                status_error_indicator_hover_bg: theme.status_error_indicator_hover_bg.into(),
                status_error_indicator_hover_fg: theme.status_error_indicator_hover_fg.into(),
                tab_drop_zone_bg: theme.tab_drop_zone_bg.into(),
                tab_drop_zone_border: theme.tab_drop_zone_border.into(),
                settings_selected_bg: theme.settings_selected_bg.into(),
                settings_selected_fg: theme.settings_selected_fg.into(),
                file_status_added_fg: Some(theme.file_status_added_fg.into()),
                file_status_modified_fg: Some(theme.file_status_modified_fg.into()),
                file_status_deleted_fg: Some(theme.file_status_deleted_fg.into()),
                file_status_renamed_fg: Some(theme.file_status_renamed_fg.into()),
                file_status_untracked_fg: Some(theme.file_status_untracked_fg.into()),
                file_status_conflicted_fg: Some(theme.file_status_conflicted_fg.into()),
            },
            search: SearchColors {
                match_bg: theme.search_match_bg.into(),
                match_fg: theme.search_match_fg.into(),
                label_bg: theme.search_label_bg.into(),
                label_fg: theme.search_label_fg.into(),
            },
            diagnostic: DiagnosticColors {
                error_fg: theme.diagnostic_error_fg.into(),
                error_bg: theme.diagnostic_error_bg.into(),
                warning_fg: theme.diagnostic_warning_fg.into(),
                warning_bg: theme.diagnostic_warning_bg.into(),
                info_fg: theme.diagnostic_info_fg.into(),
                info_bg: theme.diagnostic_info_bg.into(),
                hint_fg: theme.diagnostic_hint_fg.into(),
                hint_bg: theme.diagnostic_hint_bg.into(),
            },
            syntax: SyntaxColors {
                keyword: theme.syntax_keyword.into(),
                string: theme.syntax_string.into(),
                comment: theme.syntax_comment.into(),
                function: theme.syntax_function.into(),
                type_: theme.syntax_type.into(),
                variable: theme.syntax_variable.into(),
                variable_builtin: theme.syntax_variable_builtin.into(),
                constant: theme.syntax_constant.into(),
                operator: theme.syntax_operator.into(),
                punctuation_bracket: theme.syntax_punctuation_bracket.into(),
                punctuation_delimiter: theme.syntax_punctuation_delimiter.into(),
            },
        }
    }
}

/// Resolve the base theme that a parsed `ThemeFile` should be layered on top of.
///
/// See [`ThemeFile`] for the resolution order. Returns an error only when
/// `extends` references a base that does not exist; the no-info-at-all case
/// quietly falls through to the per-field hardcoded defaults so a theme of
/// `{"name": "x"}` keeps working.
fn resolve_base_theme(theme_file: &ThemeFile, raw: &serde_json::Value) -> Result<Theme, String> {
    // 1. Explicit `extends`.
    if let Some(extends) = theme_file.extends.as_deref() {
        let name = extends.strip_prefix("builtin://").unwrap_or(extends);
        return Theme::load_builtin(name).ok_or_else(|| {
            let available: Vec<&str> = BUILTIN_THEMES.iter().map(|t| t.name).collect();
            format!(
                "theme `extends: {:?}` does not match any built-in theme. \
                 Available: {}. \
                 Inheriting from other user themes is not yet supported.",
                extends,
                available.join(", ")
            )
        });
    }

    // 2. Auto-infer from explicit `editor.bg` luminance. We deliberately read
    //    the *raw* JSON here instead of `theme_file.editor.bg` — the typed
    //    struct fills in a default for `bg` even when the user didn't write
    //    one, and inferring a base from a default we ourselves invented would
    //    be circular.
    if let Some(bg) = raw
        .get("editor")
        .and_then(|e| e.get("bg"))
        .cloned()
        .and_then(|v| serde_json::from_value::<ColorDef>(v).ok())
    {
        let color: Color = bg.into();
        if let Some((r, g, b)) = color_to_rgb(color) {
            let lum = relative_luminance(r, g, b);
            let base_name = if lum > 0.5 { THEME_LIGHT } else { THEME_DARK };
            if let Some(base) = Theme::load_builtin(base_name) {
                return Ok(base);
            }
        }
    }

    // 3. Fallback: per-field hardcoded defaults via the existing typed path.
    Ok(theme_file.clone().into())
}

/// Compute sRGB relative luminance (ITU-R BT.709) for an RGB triple in 0..=255.
/// Used for picking a light vs dark base when the user didn't ask for one.
fn relative_luminance(r: u8, g: u8, b: u8) -> f64 {
    0.2126 * (r as f64 / 255.0) + 0.7152 * (g as f64 / 255.0) + 0.0722 * (b as f64 / 255.0)
}

/// Walk the user-supplied JSON and overlay every explicitly-set leaf onto the
/// base theme. Reuses [`Theme::resolve_theme_key_mut`] so the override surface
/// is exactly the surface the rest of the editor already knows how to address;
/// unknown keys are silently ignored, matching `override_colors` semantics.
fn apply_theme_overrides(theme: &mut Theme, theme_file: &ThemeFile, raw: &serde_json::Value) {
    // Name always comes from the user file — that's the theme's identity.
    theme.name = theme_file.name.clone();

    for section in ["editor", "ui", "search", "diagnostic", "syntax"] {
        let Some(obj) = raw.get(section).and_then(|v| v.as_object()) else {
            continue;
        };
        for (field, value) in obj {
            // Optional `Option<ColorDef>` fields encode `null` as JSON null.
            // Treat that as "no override," not "set to default."
            if value.is_null() {
                continue;
            }
            let key = format!("{}.{}", section, field);
            if let Ok(color_def) = serde_json::from_value::<ColorDef>(value.clone()) {
                if let Some(slot) = theme.resolve_theme_key_mut(&key) {
                    *slot = color_def.into();
                }
            }
        }
    }
}

impl Theme {
    /// Returns `true` when the theme has a light background.
    ///
    /// Uses the relative luminance of `editor_bg` (perceived brightness).
    /// A threshold of 0.5 separates dark from light; for `Color::Reset` or
    /// unresolvable colors, falls back to `false` (dark).
    pub fn is_light(&self) -> bool {
        color_to_rgb(self.editor_bg)
            .map(|(r, g, b)| relative_luminance(r, g, b) > 0.5)
            .unwrap_or(false)
    }

    /// Load a builtin theme by name (no I/O, uses embedded JSON).
    pub fn load_builtin(name: &str) -> Option<Self> {
        BUILTIN_THEMES
            .iter()
            .find(|t| t.name == name)
            .and_then(|t| serde_json::from_str::<ThemeFile>(t.json).ok())
            .map(|tf| tf.into())
    }

    /// Parse theme from JSON string (no I/O).
    ///
    /// Supports the inheritance model documented on [`ThemeFile`]: an explicit
    /// `extends` chooses the base; otherwise the relative luminance of an
    /// explicit `editor.bg` picks `builtin://light` vs `builtin://dark`;
    /// otherwise the per-field hardcoded defaults apply. Every leaf the user
    /// JSON specifies overrides the corresponding field on the base — the
    /// override walk uses the same `resolve_theme_key_mut` machinery as
    /// `override_colors`, so the supported set of keys stays in lock-step.
    pub fn from_json(json: &str) -> Result<Self, String> {
        // Dual-parse: the typed `ThemeFile` validates the schema and gives us
        // `name` / `extends` cheaply; the raw `Value` tells us *which* fields
        // the user actually specified, which we cannot recover from the typed
        // struct because every field has a serde default.
        let raw: serde_json::Value =
            serde_json::from_str(json).map_err(|e| format!("Failed to parse theme JSON: {}", e))?;
        let theme_file: ThemeFile = serde_json::from_value(raw.clone())
            .map_err(|e| format!("Failed to parse theme: {}", e))?;

        let mut theme = resolve_base_theme(&theme_file, &raw)?;
        apply_theme_overrides(&mut theme, &theme_file, &raw);
        Ok(theme)
    }

    /// SGR text-attribute modifier associated with a bg theme key.
    ///
    /// Lets overlay-driven highlights (e.g. word-under-cursor via
    /// `ui.semantic_highlight_bg`, selection via `editor.selection_bg`)
    /// pick up the same modifier the theme would apply directly when
    /// painting that region, so a `terminal` theme's `["reversed"]`
    /// selection works whether the cells go through `char_style` or
    /// the overlay pipeline. Unknown keys return `Modifier::empty()`.
    pub fn modifier_for_bg_key(&self, key: &str) -> Modifier {
        match key {
            "editor.selection_bg" => self.selection_modifier,
            "ui.semantic_highlight_bg" => self.semantic_highlight_modifier,
            _ => Modifier::empty(),
        }
    }

    /// Resolve a theme key to a Color.
    ///
    /// Theme keys use dot notation: "section.field"
    /// Examples:
    /// - "ui.status_bar_fg" -> status_bar_fg
    /// - "editor.selection_bg" -> selection_bg
    /// - "syntax.keyword" -> syntax_keyword
    /// - "diagnostic.error_fg" -> diagnostic_error_fg
    ///
    /// Returns None if the key is not recognized.
    pub fn resolve_theme_key(&self, key: &str) -> Option<Color> {
        // Parse "section.field" format
        let parts: Vec<&str> = key.split('.').collect();
        if parts.len() != 2 {
            return None;
        }

        let (section, field) = (parts[0], parts[1]);

        match section {
            "editor" => match field {
                "after_eof_bg" => Some(self.after_eof_bg),
                "bg" => Some(self.editor_bg),
                "current_line_bg" => Some(self.current_line_bg),
                "cursor" => Some(self.cursor),
                "diff_add_bg" => Some(self.diff_add_bg),
                "diff_add_collision_fg" => self.diff_add_collision_fg,
                "diff_add_highlight_bg" => Some(self.diff_add_highlight_bg),
                "diff_modify_bg" => Some(self.diff_modify_bg),
                "diff_modify_collision_fg" => self.diff_modify_collision_fg,
                "diff_remove_bg" => Some(self.diff_remove_bg),
                "diff_remove_collision_fg" => self.diff_remove_collision_fg,
                "diff_remove_highlight_bg" => Some(self.diff_remove_highlight_bg),
                "fg" => Some(self.editor_fg),
                "inactive_cursor" => Some(self.inactive_cursor),
                "line_number_bg" => Some(self.line_number_bg),
                "line_number_fg" => Some(self.line_number_fg),
                "ruler_bg" => Some(self.ruler_bg),
                "selection_bg" => Some(self.selection_bg),
                "whitespace_indicator_fg" => Some(self.whitespace_indicator_fg),
                "bracket_match_fg" => Some(self.bracket_match_fg),
                "bracket_rainbow_1" => Some(self.bracket_rainbow_1),
                "bracket_rainbow_2" => Some(self.bracket_rainbow_2),
                "bracket_rainbow_3" => Some(self.bracket_rainbow_3),
                "bracket_rainbow_4" => Some(self.bracket_rainbow_4),
                "bracket_rainbow_5" => Some(self.bracket_rainbow_5),
                "bracket_rainbow_6" => Some(self.bracket_rainbow_6),
                _ => None,
            },
            "ui" => match field {
                "compose_margin_bg" => Some(self.compose_margin_bg),
                "file_status_added_fg" => Some(self.file_status_added_fg),
                "file_status_conflicted_fg" => Some(self.file_status_conflicted_fg),
                "file_status_deleted_fg" => Some(self.file_status_deleted_fg),
                "file_status_modified_fg" => Some(self.file_status_modified_fg),
                "file_status_renamed_fg" => Some(self.file_status_renamed_fg),
                "file_status_untracked_fg" => Some(self.file_status_untracked_fg),
                "help_bg" => Some(self.help_bg),
                "help_fg" => Some(self.help_fg),
                "help_indicator_bg" => Some(self.help_indicator_bg),
                "help_indicator_fg" => Some(self.help_indicator_fg),
                "help_key_fg" => Some(self.help_key_fg),
                "help_separator_fg" => Some(self.help_separator_fg),
                "inline_code_bg" => Some(self.inline_code_bg),
                "menu_active_bg" => Some(self.menu_active_bg),
                "menu_active_fg" => Some(self.menu_active_fg),
                "menu_bg" => Some(self.menu_bg),
                "menu_border_fg" => Some(self.menu_border_fg),
                "menu_disabled_bg" => Some(self.menu_disabled_bg),
                "menu_disabled_fg" => Some(self.menu_disabled_fg),
                "menu_dropdown_bg" => Some(self.menu_dropdown_bg),
                "menu_dropdown_fg" => Some(self.menu_dropdown_fg),
                "menu_fg" => Some(self.menu_fg),
                "menu_highlight_bg" => Some(self.menu_highlight_bg),
                "menu_highlight_fg" => Some(self.menu_highlight_fg),
                "menu_hover_bg" => Some(self.menu_hover_bg),
                "menu_hover_fg" => Some(self.menu_hover_fg),
                "menu_separator_fg" => Some(self.menu_separator_fg),
                "popup_bg" => Some(self.popup_bg),
                "popup_border_fg" => Some(self.popup_border_fg),
                "popup_selection_bg" => Some(self.popup_selection_bg),
                "popup_selection_fg" => Some(self.popup_selection_fg),
                "popup_text_fg" => Some(self.popup_text_fg),
                "prompt_bg" => Some(self.prompt_bg),
                "prompt_fg" => Some(self.prompt_fg),
                "prompt_selection_bg" => Some(self.prompt_selection_bg),
                "prompt_selection_fg" => Some(self.prompt_selection_fg),
                "scrollbar_thumb_fg" => Some(self.scrollbar_thumb_fg),
                "scrollbar_thumb_hover_fg" => Some(self.scrollbar_thumb_hover_fg),
                "scrollbar_track_fg" => Some(self.scrollbar_track_fg),
                "scrollbar_track_hover_fg" => Some(self.scrollbar_track_hover_fg),
                "semantic_highlight_bg" => Some(self.semantic_highlight_bg),
                "settings_selected_bg" => Some(self.settings_selected_bg),
                "settings_selected_fg" => Some(self.settings_selected_fg),
                "split_separator_fg" => Some(self.split_separator_fg),
                "split_separator_hover_fg" => Some(self.split_separator_hover_fg),
                "status_bar_bg" => Some(self.status_bar_bg),
                "status_bar_fg" => Some(self.status_bar_fg),
                "status_error_indicator_bg" => Some(self.status_error_indicator_bg),
                "status_error_indicator_fg" => Some(self.status_error_indicator_fg),
                "status_error_indicator_hover_bg" => Some(self.status_error_indicator_hover_bg),
                "status_error_indicator_hover_fg" => Some(self.status_error_indicator_hover_fg),
                "status_lsp_actionable_bg" => Some(self.status_lsp_actionable_bg),
                "status_lsp_actionable_fg" => Some(self.status_lsp_actionable_fg),
                "status_lsp_on_bg" => Some(self.status_lsp_on_bg),
                "status_lsp_on_fg" => Some(self.status_lsp_on_fg),
                "status_palette_bg" => Some(self.status_palette_bg),
                "status_palette_fg" => Some(self.status_palette_fg),
                "status_warning_indicator_bg" => Some(self.status_warning_indicator_bg),
                "status_warning_indicator_fg" => Some(self.status_warning_indicator_fg),
                "status_warning_indicator_hover_bg" => Some(self.status_warning_indicator_hover_bg),
                "status_warning_indicator_hover_fg" => Some(self.status_warning_indicator_hover_fg),
                "suggestion_bg" => Some(self.suggestion_bg),
                "suggestion_fg" => Some(self.suggestion_fg),
                "suggestion_selected_bg" => Some(self.suggestion_selected_bg),
                "tab_active_bg" => Some(self.tab_active_bg),
                "tab_active_fg" => Some(self.tab_active_fg),
                "tab_close_hover_fg" => Some(self.tab_close_hover_fg),
                "tab_drop_zone_bg" => Some(self.tab_drop_zone_bg),
                "tab_drop_zone_border" => Some(self.tab_drop_zone_border),
                "tab_hover_bg" => Some(self.tab_hover_bg),
                "tab_inactive_bg" => Some(self.tab_inactive_bg),
                "tab_inactive_fg" => Some(self.tab_inactive_fg),
                "tab_separator_bg" => Some(self.tab_separator_bg),
                "terminal_bg" => Some(self.terminal_bg),
                "terminal_fg" => Some(self.terminal_fg),
                "text_input_selection_bg" => Some(self.text_input_selection_bg),
                _ => None,
            },
            "syntax" => match field {
                "comment" => Some(self.syntax_comment),
                "constant" => Some(self.syntax_constant),
                "function" => Some(self.syntax_function),
                "keyword" => Some(self.syntax_keyword),
                "operator" => Some(self.syntax_operator),
                "punctuation_bracket" => Some(self.syntax_punctuation_bracket),
                "punctuation_delimiter" => Some(self.syntax_punctuation_delimiter),
                "string" => Some(self.syntax_string),
                "type" => Some(self.syntax_type),
                "variable" => Some(self.syntax_variable),
                "variable_builtin" => Some(self.syntax_variable_builtin),
                _ => None,
            },
            "diagnostic" => match field {
                "error_bg" => Some(self.diagnostic_error_bg),
                "error_fg" => Some(self.diagnostic_error_fg),
                "hint_bg" => Some(self.diagnostic_hint_bg),
                "hint_fg" => Some(self.diagnostic_hint_fg),
                "info_bg" => Some(self.diagnostic_info_bg),
                "info_fg" => Some(self.diagnostic_info_fg),
                "warning_bg" => Some(self.diagnostic_warning_bg),
                "warning_fg" => Some(self.diagnostic_warning_fg),
                _ => None,
            },
            "search" => match field {
                "label_bg" => Some(self.search_label_bg),
                "label_fg" => Some(self.search_label_fg),
                "match_bg" => Some(self.search_match_bg),
                "match_fg" => Some(self.search_match_fg),
                _ => None,
            },
            _ => None,
        }
    }

    /// Mutable companion to [`resolve_theme_key`]. Keep the two matches in
    /// lock-step: any key readable by `resolve_theme_key` should also be
    /// writable here, and vice versa.
    pub fn resolve_theme_key_mut(&mut self, key: &str) -> Option<&mut Color> {
        let parts: Vec<&str> = key.split('.').collect();
        if parts.len() != 2 {
            return None;
        }
        let (section, field) = (parts[0], parts[1]);
        match section {
            "editor" => match field {
                "bg" => Some(&mut self.editor_bg),
                "fg" => Some(&mut self.editor_fg),
                "cursor" => Some(&mut self.cursor),
                "inactive_cursor" => Some(&mut self.inactive_cursor),
                "selection_bg" => Some(&mut self.selection_bg),
                "current_line_bg" => Some(&mut self.current_line_bg),
                "line_number_fg" => Some(&mut self.line_number_fg),
                "line_number_bg" => Some(&mut self.line_number_bg),
                "diff_add_bg" => Some(&mut self.diff_add_bg),
                "diff_remove_bg" => Some(&mut self.diff_remove_bg),
                "diff_modify_bg" => Some(&mut self.diff_modify_bg),
                // `Option<Color>` — only addressable for mutation
                // when already set in the theme JSON (lock-step with
                // `resolve_theme_key`).
                "diff_add_collision_fg" => self.diff_add_collision_fg.as_mut(),
                "diff_remove_collision_fg" => self.diff_remove_collision_fg.as_mut(),
                "diff_modify_collision_fg" => self.diff_modify_collision_fg.as_mut(),
                "ruler_bg" => Some(&mut self.ruler_bg),
                "whitespace_indicator_fg" => Some(&mut self.whitespace_indicator_fg),
                "bracket_match_fg" => Some(&mut self.bracket_match_fg),
                "bracket_rainbow_1" => Some(&mut self.bracket_rainbow_1),
                "bracket_rainbow_2" => Some(&mut self.bracket_rainbow_2),
                "bracket_rainbow_3" => Some(&mut self.bracket_rainbow_3),
                "bracket_rainbow_4" => Some(&mut self.bracket_rainbow_4),
                "bracket_rainbow_5" => Some(&mut self.bracket_rainbow_5),
                "bracket_rainbow_6" => Some(&mut self.bracket_rainbow_6),
                "diff_add_highlight_bg" => Some(&mut self.diff_add_highlight_bg),
                "diff_remove_highlight_bg" => Some(&mut self.diff_remove_highlight_bg),
                "after_eof_bg" => Some(&mut self.after_eof_bg),
                _ => None,
            },
            "ui" => match field {
                "tab_active_fg" => Some(&mut self.tab_active_fg),
                "tab_active_bg" => Some(&mut self.tab_active_bg),
                "tab_inactive_fg" => Some(&mut self.tab_inactive_fg),
                "tab_inactive_bg" => Some(&mut self.tab_inactive_bg),
                "status_bar_fg" => Some(&mut self.status_bar_fg),
                "status_bar_bg" => Some(&mut self.status_bar_bg),
                "status_palette_fg" => Some(&mut self.status_palette_fg),
                "status_palette_bg" => Some(&mut self.status_palette_bg),
                "status_lsp_on_fg" => Some(&mut self.status_lsp_on_fg),
                "status_lsp_on_bg" => Some(&mut self.status_lsp_on_bg),
                "status_lsp_actionable_fg" => Some(&mut self.status_lsp_actionable_fg),
                "status_lsp_actionable_bg" => Some(&mut self.status_lsp_actionable_bg),
                "prompt_fg" => Some(&mut self.prompt_fg),
                "prompt_bg" => Some(&mut self.prompt_bg),
                "prompt_selection_fg" => Some(&mut self.prompt_selection_fg),
                "prompt_selection_bg" => Some(&mut self.prompt_selection_bg),
                "popup_bg" => Some(&mut self.popup_bg),
                "popup_border_fg" => Some(&mut self.popup_border_fg),
                "popup_selection_bg" => Some(&mut self.popup_selection_bg),
                "popup_selection_fg" => Some(&mut self.popup_selection_fg),
                "popup_text_fg" => Some(&mut self.popup_text_fg),
                "text_input_selection_bg" => Some(&mut self.text_input_selection_bg),
                "menu_bg" => Some(&mut self.menu_bg),
                "menu_fg" => Some(&mut self.menu_fg),
                "menu_active_bg" => Some(&mut self.menu_active_bg),
                "menu_active_fg" => Some(&mut self.menu_active_fg),
                "menu_disabled_fg" => Some(&mut self.menu_disabled_fg),
                "menu_disabled_bg" => Some(&mut self.menu_disabled_bg),
                "help_bg" => Some(&mut self.help_bg),
                "help_fg" => Some(&mut self.help_fg),
                "help_key_fg" => Some(&mut self.help_key_fg),
                "split_separator_fg" => Some(&mut self.split_separator_fg),
                "scrollbar_track_fg" => Some(&mut self.scrollbar_track_fg),
                "scrollbar_thumb_fg" => Some(&mut self.scrollbar_thumb_fg),
                "scrollbar_track_hover_fg" => Some(&mut self.scrollbar_track_hover_fg),
                "scrollbar_thumb_hover_fg" => Some(&mut self.scrollbar_thumb_hover_fg),
                "semantic_highlight_bg" => Some(&mut self.semantic_highlight_bg),
                "file_status_added_fg" => Some(&mut self.file_status_added_fg),
                "file_status_modified_fg" => Some(&mut self.file_status_modified_fg),
                "file_status_deleted_fg" => Some(&mut self.file_status_deleted_fg),
                "file_status_renamed_fg" => Some(&mut self.file_status_renamed_fg),
                "file_status_untracked_fg" => Some(&mut self.file_status_untracked_fg),
                "file_status_conflicted_fg" => Some(&mut self.file_status_conflicted_fg),
                "menu_dropdown_bg" => Some(&mut self.menu_dropdown_bg),
                "menu_dropdown_fg" => Some(&mut self.menu_dropdown_fg),
                "menu_highlight_bg" => Some(&mut self.menu_highlight_bg),
                "menu_highlight_fg" => Some(&mut self.menu_highlight_fg),
                "menu_border_fg" => Some(&mut self.menu_border_fg),
                "menu_separator_fg" => Some(&mut self.menu_separator_fg),
                "menu_hover_bg" => Some(&mut self.menu_hover_bg),
                "menu_hover_fg" => Some(&mut self.menu_hover_fg),
                "tab_separator_bg" => Some(&mut self.tab_separator_bg),
                "tab_close_hover_fg" => Some(&mut self.tab_close_hover_fg),
                "tab_hover_bg" => Some(&mut self.tab_hover_bg),
                "inline_code_bg" => Some(&mut self.inline_code_bg),
                "split_separator_hover_fg" => Some(&mut self.split_separator_hover_fg),
                "compose_margin_bg" => Some(&mut self.compose_margin_bg),
                "terminal_bg" => Some(&mut self.terminal_bg),
                "terminal_fg" => Some(&mut self.terminal_fg),
                "status_warning_indicator_bg" => Some(&mut self.status_warning_indicator_bg),
                "status_warning_indicator_fg" => Some(&mut self.status_warning_indicator_fg),
                "status_error_indicator_bg" => Some(&mut self.status_error_indicator_bg),
                "status_error_indicator_fg" => Some(&mut self.status_error_indicator_fg),
                "status_warning_indicator_hover_bg" => {
                    Some(&mut self.status_warning_indicator_hover_bg)
                }
                "status_warning_indicator_hover_fg" => {
                    Some(&mut self.status_warning_indicator_hover_fg)
                }
                "status_error_indicator_hover_bg" => {
                    Some(&mut self.status_error_indicator_hover_bg)
                }
                "status_error_indicator_hover_fg" => {
                    Some(&mut self.status_error_indicator_hover_fg)
                }
                "tab_drop_zone_bg" => Some(&mut self.tab_drop_zone_bg),
                "tab_drop_zone_border" => Some(&mut self.tab_drop_zone_border),
                "settings_selected_bg" => Some(&mut self.settings_selected_bg),
                "settings_selected_fg" => Some(&mut self.settings_selected_fg),
                "suggestion_bg" => Some(&mut self.suggestion_bg),
                "suggestion_fg" => Some(&mut self.suggestion_fg),
                "suggestion_selected_bg" => Some(&mut self.suggestion_selected_bg),
                "help_separator_fg" => Some(&mut self.help_separator_fg),
                "help_indicator_fg" => Some(&mut self.help_indicator_fg),
                "help_indicator_bg" => Some(&mut self.help_indicator_bg),
                _ => None,
            },
            "syntax" => match field {
                "keyword" => Some(&mut self.syntax_keyword),
                "string" => Some(&mut self.syntax_string),
                "comment" => Some(&mut self.syntax_comment),
                "function" => Some(&mut self.syntax_function),
                "type" => Some(&mut self.syntax_type),
                "variable" => Some(&mut self.syntax_variable),
                "variable_builtin" => Some(&mut self.syntax_variable_builtin),
                "constant" => Some(&mut self.syntax_constant),
                "operator" => Some(&mut self.syntax_operator),
                "punctuation_bracket" => Some(&mut self.syntax_punctuation_bracket),
                "punctuation_delimiter" => Some(&mut self.syntax_punctuation_delimiter),
                _ => None,
            },
            "diagnostic" => match field {
                "error_fg" => Some(&mut self.diagnostic_error_fg),
                "error_bg" => Some(&mut self.diagnostic_error_bg),
                "warning_fg" => Some(&mut self.diagnostic_warning_fg),
                "warning_bg" => Some(&mut self.diagnostic_warning_bg),
                "info_fg" => Some(&mut self.diagnostic_info_fg),
                "info_bg" => Some(&mut self.diagnostic_info_bg),
                "hint_fg" => Some(&mut self.diagnostic_hint_fg),
                "hint_bg" => Some(&mut self.diagnostic_hint_bg),
                _ => None,
            },
            "search" => match field {
                "match_bg" => Some(&mut self.search_match_bg),
                "match_fg" => Some(&mut self.search_match_fg),
                "label_bg" => Some(&mut self.search_label_bg),
                "label_fg" => Some(&mut self.search_label_fg),
                _ => None,
            },
            _ => None,
        }
    }

    /// Apply a map of `"section.field" -> Color` overrides to the running
    /// theme in-place. Returns the number of keys that matched a known
    /// theme field. Unknown keys are silently dropped so a typo in a fast
    /// animation loop doesn't crash the caller.
    pub fn override_colors<I, K>(&mut self, overrides: I) -> usize
    where
        I: IntoIterator<Item = (K, Color)>,
        K: AsRef<str>,
    {
        let mut applied = 0;
        for (key, color) in overrides {
            if let Some(slot) = self.resolve_theme_key_mut(key.as_ref()) {
                *slot = color;
                applied += 1;
            }
        }
        applied
    }
}

// =============================================================================
// Theme Schema Generation for Plugin API
// =============================================================================

/// Returns the raw JSON Schema for ThemeFile, generated by schemars.
/// The schema uses standard JSON Schema format with $ref for type references.
/// Plugins are responsible for parsing and resolving $ref references.
pub fn get_theme_schema() -> serde_json::Value {
    use schemars::schema_for;
    let schema = schema_for!(ThemeFile);
    serde_json::to_value(&schema).unwrap_or_default()
}

/// Returns a map of built-in theme names to their JSON content.
pub fn get_builtin_themes() -> serde_json::Value {
    let mut map = serde_json::Map::new();
    for theme in BUILTIN_THEMES {
        map.insert(
            theme.name.to_string(),
            serde_json::Value::String(theme.json.to_string()),
        );
    }
    serde_json::Value::Object(map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_builtin_theme() {
        let dark = Theme::load_builtin(THEME_DARK).expect("Dark theme must exist");
        assert_eq!(dark.name, THEME_DARK);

        let light = Theme::load_builtin(THEME_LIGHT).expect("Light theme must exist");
        assert_eq!(light.name, THEME_LIGHT);

        let high_contrast =
            Theme::load_builtin(THEME_HIGH_CONTRAST).expect("High contrast theme must exist");
        assert_eq!(high_contrast.name, THEME_HIGH_CONTRAST);

        let terminal = Theme::load_builtin(THEME_TERMINAL).expect("Terminal theme must exist");
        assert_eq!(terminal.name, THEME_TERMINAL);
        // The terminal theme defers to the host palette: backgrounds and
        // primary text use Color::Reset so the terminal's own colors
        // (including transparency) show through.
        assert_eq!(terminal.editor_bg, Color::Reset);
        assert_eq!(terminal.editor_fg, Color::Reset);
        assert_eq!(terminal.terminal_bg, Color::Reset);
        // Adaptive accents use SGR text attributes so they invert/emphasise
        // against whatever fg/bg the terminal already has.
        assert!(terminal.selection_modifier.contains(Modifier::REVERSED));
        assert!(terminal
            .semantic_highlight_modifier
            .contains(Modifier::BOLD));
    }

    #[test]
    fn test_suggestion_fg_falls_back_and_contrasts() {
        // Regression: the overlay prompt (Live Grep) drew its title/input
        // on `suggestion_bg`/`editor_bg` but borrowed `prompt_fg` for the
        // text. Dracula's `prompt_fg` equals its editor background, so the
        // text was invisible. `suggestion_fg` is the dedicated foreground
        // for that surface; when a theme omits it, it falls back to
        // `popup_text_fg` (never `prompt_fg`).
        let dracula = Theme::load_builtin(THEME_DRACULA).expect("Dracula theme must exist");
        assert_eq!(
            dracula.suggestion_fg, dracula.popup_text_fg,
            "suggestion_fg should fall back to popup_text_fg when unset"
        );
        assert_ne!(
            dracula.suggestion_fg, dracula.suggestion_bg,
            "suggestion_fg must contrast with suggestion_bg, not vanish into it"
        );
    }

    #[test]
    fn test_modifier_def_round_trip() {
        let cases = [
            (vec!["reversed"], Modifier::REVERSED),
            (
                vec!["bold", "underlined"],
                Modifier::BOLD | Modifier::UNDERLINED,
            ),
            (vec!["italic", "dim"], Modifier::ITALIC | Modifier::DIM),
            (vec!["reverse"], Modifier::REVERSED),     // alias
            (vec!["underline"], Modifier::UNDERLINED), // alias
        ];
        for (strs, expected) in cases {
            let def = ModifierDef(strs.iter().map(|s| s.to_string()).collect());
            let m: Modifier = (&def).into();
            assert_eq!(m, expected, "ModifierDef({:?}) -> Modifier", strs);
        }
    }

    #[test]
    fn test_modifier_def_unknown_strings_are_dropped() {
        // A typo in a theme JSON shouldn't crash a render — unknown
        // modifier names are silently dropped.
        let def = ModifierDef(vec!["reversed".into(), "wibble".into(), "bold".into()]);
        let m: Modifier = (&def).into();
        assert_eq!(m, Modifier::REVERSED | Modifier::BOLD);
    }

    #[test]
    fn test_themes_without_modifier_default_to_empty() {
        // Existing themes (no `*_modifier` keys in their JSON) must
        // resolve to Modifier::empty() — i.e. the new fields are
        // backward compatible and don't change rendering for old
        // themes.
        let dark = Theme::load_builtin(THEME_DARK).expect("Dark theme must exist");
        assert!(dark.selection_modifier.is_empty());
        assert!(dark.semantic_highlight_modifier.is_empty());
    }

    #[test]
    fn test_modifier_for_bg_key_lookup() {
        let terminal = Theme::load_builtin(THEME_TERMINAL).expect("Terminal theme must exist");
        // Overlay-driven highlights pick up the same modifier the
        // direct-paint path uses, keyed by bg theme key.
        assert!(terminal
            .modifier_for_bg_key("editor.selection_bg")
            .contains(Modifier::REVERSED));
        assert!(terminal
            .modifier_for_bg_key("ui.semantic_highlight_bg")
            .contains(Modifier::BOLD));
        // Unknown / unmapped keys yield empty so we don't accidentally
        // tint other UI regions.
        assert!(terminal
            .modifier_for_bg_key("ui.popup_selection_bg")
            .is_empty());
        assert!(terminal.modifier_for_bg_key("nonsense").is_empty());
    }

    #[test]
    fn test_modifier_round_trip_via_theme_file() {
        // Theme -> ThemeFile -> Theme preserves modifiers.
        let original = Theme::load_builtin(THEME_TERMINAL).expect("Terminal theme must exist");
        let file: ThemeFile = original.clone().into();
        let json = serde_json::to_string(&file).expect("serialize");
        let parsed: ThemeFile = serde_json::from_str(&json).expect("parse");
        let round_tripped: Theme = parsed.into();
        assert_eq!(
            round_tripped.selection_modifier,
            original.selection_modifier
        );
        assert_eq!(
            round_tripped.semantic_highlight_modifier,
            original.semantic_highlight_modifier
        );
    }

    #[test]
    fn test_builtin_themes_match_schema() {
        for theme in BUILTIN_THEMES {
            let _: ThemeFile = serde_json::from_str(theme.json)
                .unwrap_or_else(|_| panic!("Theme '{}' does not match schema", theme.name));
        }
    }

    #[test]
    fn test_from_json() {
        let json = r#"{"name":"test","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#;
        let theme = Theme::from_json(json).expect("Should parse minimal theme");
        assert_eq!(theme.name, "test");
    }

    /// Regression test for #1281: a user theme that follows the minimal example
    /// in `docs/features/themes.md` (only `name`, `editor`, `syntax` — no `ui`,
    /// `search`, or `diagnostic` sections) must load successfully. Before the
    /// fix, `serde_json::from_str::<ThemeFile>` errored with `missing field
    /// `ui``, the loader silently dropped the theme, and the user saw
    /// "Failed to load theme" in the status bar.
    ///
    /// Beyond loading, this also pins the auto-inheritance behavior: with a
    /// cream `editor.bg`, the unspecified UI/diagnostic colors must come from
    /// `builtin://light` (so the theme reads coherently end-to-end), not from
    /// the dark-flavored hardcoded fallbacks.
    #[test]
    fn test_minimal_user_theme_from_issue_1281_loads() {
        // Verbatim from https://github.com/sinelaw/fresh/issues/1281
        let json = r#"{
  "name": "gruvbox-light-orange",
  "editor": {
    "bg": [251, 241, 199],
    "fg": [60, 56, 54],
    "cursor": [254, 128, 25],
    "selection_bg": [213, 196, 161]
  },
  "syntax": {
    "keyword": [175, 58, 3],
    "string": [152, 151, 26],
    "comment": [146, 131, 116]
  }
}"#;
        let theme = Theme::from_json(json)
            .expect("Theme from issue #1281 should parse without `ui`/`search`/`diagnostic`");
        assert_eq!(theme.name, "gruvbox-light-orange");

        // Explicit fields land where expected.
        assert_eq!(theme.editor_bg, Color::Rgb(251, 241, 199));
        assert_eq!(theme.editor_fg, Color::Rgb(60, 56, 54));
        assert_eq!(theme.cursor, Color::Rgb(254, 128, 25));
        assert_eq!(theme.selection_bg, Color::Rgb(213, 196, 161));
        assert_eq!(theme.syntax_keyword, Color::Rgb(175, 58, 3));
        assert_eq!(theme.syntax_string, Color::Rgb(152, 151, 26));
        assert_eq!(theme.syntax_comment, Color::Rgb(146, 131, 116));

        // Auto-inheritance: cream bg → `builtin://light` is the base. The
        // unspecified UI/diagnostic colors should match the light builtin's
        // values — not the dark-flavored hardcoded fallbacks.
        let light = Theme::load_builtin(THEME_LIGHT).expect("light builtin");
        assert_eq!(
            theme.status_bar_fg, light.status_bar_fg,
            "ui.status_bar_fg should inherit from builtin://light when bg is bright"
        );
        assert_eq!(
            theme.diagnostic_error_fg, light.diagnostic_error_fg,
            "diagnostic.error_fg should inherit from builtin://light when bg is bright"
        );
        assert_eq!(
            theme.menu_bg, light.menu_bg,
            "ui.menu_bg should inherit from builtin://light when bg is bright"
        );
    }

    /// A user theme with an explicit `extends` must inherit from that base —
    /// even when auto-inference would have picked something different.
    #[test]
    fn test_extends_explicit_builtin_wins_over_auto_infer() {
        // `editor.bg` is dark (would auto-infer `dark`), but `extends` asks
        // for `light`. The explicit choice must win.
        let json = r#"{
            "name": "explicit-light",
            "extends": "builtin://light",
            "editor": { "bg": [0, 0, 0] }
        }"#;
        let theme = Theme::from_json(json).expect("extends should resolve");
        let light = Theme::load_builtin(THEME_LIGHT).expect("light builtin");

        // Override applied.
        assert_eq!(theme.editor_bg, Color::Rgb(0, 0, 0));
        // Unspecified fields come from the explicit base, not from auto-infer.
        assert_eq!(theme.menu_bg, light.menu_bg);
        assert_eq!(theme.tab_active_bg, light.tab_active_bg);
        assert_eq!(theme.diagnostic_warning_fg, light.diagnostic_warning_fg);
    }

    /// Bare-name `extends` (e.g. `"dark"`) is the legacy form accepted by the
    /// rest of the registry (`ThemeRegistry::resolve_key`), so we accept it
    /// here too — being strict about a `builtin://` prefix would just be a
    /// papercut for users hand-writing a theme JSON.
    #[test]
    fn test_extends_bare_builtin_name_works() {
        let json = r#"{ "name": "x", "extends": "high-contrast" }"#;
        let theme = Theme::from_json(json).expect("bare-name extends should resolve");
        let hc = Theme::load_builtin("high-contrast").expect("hc builtin");
        assert_eq!(theme.menu_bg, hc.menu_bg);
    }

    /// An unknown `extends` target must produce a clear error that names what
    /// went wrong and lists the valid alternatives — anything less leaves the
    /// user staring at the same opaque "Failed to load theme" message that
    /// motivated #1281 in the first place.
    #[test]
    fn test_extends_unknown_builtin_errors_with_helpful_message() {
        let json = r#"{ "name": "x", "extends": "builtin://no-such-theme" }"#;
        let err = Theme::from_json(json).expect_err("unknown extends must error");
        assert!(
            err.contains("no-such-theme"),
            "error should quote the bad value, got: {}",
            err
        );
        assert!(
            err.contains("dark") && err.contains("light"),
            "error should list available builtins, got: {}",
            err
        );
    }

    /// Auto-inference picks `dark` for a clearly-dark `editor.bg`. Mirrors
    /// the light path tested in the #1281 regression so both branches stay
    /// honest.
    #[test]
    fn test_auto_infer_dark_base_from_dark_bg() {
        let json = r#"{ "name": "x", "editor": { "bg": [20, 20, 30] } }"#;
        let theme = Theme::from_json(json).expect("should parse");
        let dark = Theme::load_builtin(THEME_DARK).expect("dark builtin");
        assert_eq!(theme.menu_bg, dark.menu_bg);
        assert_eq!(theme.diagnostic_error_fg, dark.diagnostic_error_fg);
    }

    /// With neither `extends` nor an explicit `editor.bg`, there's nothing to
    /// infer from — the theme should still load and use the per-field
    /// hardcoded defaults rather than failing or picking an arbitrary builtin.
    #[test]
    fn test_no_inheritance_signal_uses_hardcoded_defaults() {
        let json = r#"{ "name": "x" }"#;
        let theme = Theme::from_json(json).expect("should parse");
        // The hardcoded `default_editor_bg` is `Rgb(30, 30, 30)`. Pin that so
        // a future change to the default prompts a deliberate test update.
        assert_eq!(theme.editor_bg, Color::Rgb(30, 30, 30));
    }

    /// `name` remains the only truly required top-level field. A theme JSON
    /// missing `name` should still be rejected with a clear error so users
    /// don't end up with an unidentifiable theme in the registry.
    #[test]
    fn test_theme_without_name_still_errors() {
        let json = r#"{ "editor": {} }"#;
        let err = Theme::from_json(json).expect_err("missing `name` must be an error");
        assert!(
            err.contains("name"),
            "error should mention the missing `name` field, got: {}",
            err
        );
    }

    /// Overriding a single nested field on top of an explicit `extends` must
    /// only touch that field — every sibling stays at the base's value. This
    /// is the surgical-tweak workflow ("I love `dark` but want a different
    /// cursor color"), and the override walk must not bleed into other fields.
    #[test]
    fn test_extends_overrides_compose_field_by_field() {
        let json = r#"{
            "name": "dark-with-pink-cursor",
            "extends": "builtin://dark",
            "editor": { "cursor": [255, 105, 180] }
        }"#;
        let theme = Theme::from_json(json).expect("should parse");
        let dark = Theme::load_builtin(THEME_DARK).expect("dark builtin");

        // Cursor was overridden.
        assert_eq!(theme.cursor, Color::Rgb(255, 105, 180));
        // Every other editor field comes from the base verbatim.
        assert_eq!(theme.editor_bg, dark.editor_bg);
        assert_eq!(theme.editor_fg, dark.editor_fg);
        assert_eq!(theme.selection_bg, dark.selection_bg);
        // And so do the other sections.
        assert_eq!(theme.menu_bg, dark.menu_bg);
        assert_eq!(theme.syntax_keyword, dark.syntax_keyword);
    }

    #[test]
    fn test_default_reset_color() {
        // Test that "Default" maps to Color::Reset
        let color: Color = ColorDef::Named("Default".to_string()).into();
        assert_eq!(color, Color::Reset);

        // Test that "Reset" also maps to Color::Reset
        let color: Color = ColorDef::Named("Reset".to_string()).into();
        assert_eq!(color, Color::Reset);
    }

    #[test]
    fn test_file_status_colors_fall_back_to_diagnostic_colors() {
        // A theme with NO file_status_* keys should inherit from diagnostic colors
        let json = r#"{
            "name": "test-fallback",
            "editor": {},
            "ui": {},
            "search": {},
            "diagnostic": {
                "error_fg": [220, 50, 47],
                "warning_fg": [181, 137, 0],
                "info_fg": [38, 139, 210],
                "hint_fg": [101, 123, 131]
            },
            "syntax": {}
        }"#;
        let theme = Theme::from_json(json).expect("Should parse theme without file_status keys");

        // Verify fallback: added/renamed -> info_fg
        assert_eq!(theme.file_status_added_fg, Color::Rgb(38, 139, 210));
        assert_eq!(theme.file_status_renamed_fg, Color::Rgb(38, 139, 210));
        // modified -> warning_fg
        assert_eq!(theme.file_status_modified_fg, Color::Rgb(181, 137, 0));
        // deleted/conflicted -> error_fg
        assert_eq!(theme.file_status_deleted_fg, Color::Rgb(220, 50, 47));
        assert_eq!(theme.file_status_conflicted_fg, Color::Rgb(220, 50, 47));
        // untracked -> hint_fg
        assert_eq!(theme.file_status_untracked_fg, Color::Rgb(101, 123, 131));
    }

    #[test]
    fn test_file_status_colors_explicit_override() {
        // A theme WITH explicit file_status keys should use those, not the fallback
        let json = r#"{
            "name": "test-override",
            "editor": {},
            "ui": {
                "file_status_added_fg": [80, 250, 123],
                "file_status_modified_fg": [255, 184, 108]
            },
            "search": {},
            "diagnostic": {
                "info_fg": [38, 139, 210],
                "warning_fg": [181, 137, 0]
            },
            "syntax": {}
        }"#;
        let theme = Theme::from_json(json).expect("Should parse theme with file_status overrides");

        // Explicit overrides should win
        assert_eq!(theme.file_status_added_fg, Color::Rgb(80, 250, 123));
        assert_eq!(theme.file_status_modified_fg, Color::Rgb(255, 184, 108));
        // Non-overridden should still fall back
        assert_eq!(theme.file_status_renamed_fg, Color::Rgb(38, 139, 210));
    }

    #[test]
    fn test_file_status_colors_resolve_via_theme_key() {
        let json = r#"{
            "name": "test-resolve",
            "editor": {},
            "ui": {
                "file_status_added_fg": [80, 250, 123]
            },
            "search": {},
            "diagnostic": {
                "warning_fg": [181, 137, 0]
            },
            "syntax": {}
        }"#;
        let theme = Theme::from_json(json).expect("Should parse theme");

        // Theme key resolution should work for file_status keys
        assert_eq!(
            theme.resolve_theme_key("ui.file_status_added_fg"),
            Some(Color::Rgb(80, 250, 123))
        );
        assert_eq!(
            theme.resolve_theme_key("ui.file_status_modified_fg"),
            Some(Color::Rgb(181, 137, 0))
        );
    }

    #[test]
    fn override_colors_writes_known_keys_and_drops_unknowns() {
        let mut theme = Theme::load_builtin(THEME_DARK).expect("dark builtin");
        let applied = theme.override_colors([
            ("editor.bg".to_string(), Color::Rgb(10, 20, 30)),
            ("ui.status_bar_fg".to_string(), Color::Rgb(1, 2, 3)),
            ("does.not_exist".to_string(), Color::Rgb(9, 9, 9)),
            ("garbage_no_dot".to_string(), Color::Rgb(9, 9, 9)),
        ]);
        assert_eq!(applied, 2, "only the two valid keys should be applied");
        assert_eq!(
            theme.resolve_theme_key("editor.bg"),
            Some(Color::Rgb(10, 20, 30))
        );
        assert_eq!(
            theme.resolve_theme_key("ui.status_bar_fg"),
            Some(Color::Rgb(1, 2, 3))
        );
    }

    #[test]
    fn resolve_theme_key_mut_matches_resolve_theme_key_domain() {
        // If a key resolves readably, it must also resolve as a mutable
        // slot — the two matches must stay in lock-step.
        let mut theme = Theme::load_builtin(THEME_DARK).expect("dark builtin");
        let probe = [
            "editor.bg",
            "editor.fg",
            "ui.status_bar_fg",
            "ui.tab_active_bg",
            "syntax.keyword",
            "diagnostic.error_fg",
            "search.match_bg",
        ];
        for key in probe {
            assert!(
                theme.resolve_theme_key(key).is_some(),
                "reader lost key {key}"
            );
            assert!(
                theme.resolve_theme_key_mut(key).is_some(),
                "mutator missing key {key}"
            );
        }
    }

    /// The set of `(section, field)` color keys that the theme's JSON
    /// surface — and therefore the plugin schema — exposes. Derived by
    /// taking a fully resolved builtin back to a `ThemeFile`, so every
    /// optional color slot is materialized as `Some`. Non-color leaves
    /// (text-attribute modifiers, `name`/`extends`) are filtered by shape.
    ///
    /// This is the single authority the resolvers/conversions are checked
    /// against: there is no hand-maintained key list to drift.
    fn schema_color_keys() -> Vec<(String, String)> {
        let theme = Theme::load_builtin(THEME_DARK).expect("dark builtin");
        let file: ThemeFile = theme.into();
        let value = serde_json::to_value(&file).expect("ThemeFile serializes");
        let obj = value.as_object().expect("ThemeFile is a JSON object");

        let mut keys = Vec::new();
        for section in ["editor", "ui", "search", "diagnostic", "syntax"] {
            let fields = obj
                .get(section)
                .and_then(|v| v.as_object())
                .unwrap_or_else(|| panic!("section `{section}` missing from serialized ThemeFile"));
            for (field, val) in fields {
                if is_color_leaf(val) {
                    keys.push((section.to_string(), field.clone()));
                }
            }
        }
        assert!(
            keys.len() >= 100,
            "expected the theme to expose at least ~100 color keys, found {} — \
             has the serialization shape changed?",
            keys.len()
        );
        keys
    }

    /// A `ColorDef` JSON leaf is either a named-color string or an
    /// `[r, g, b]` array of three numbers. Text-attribute modifiers
    /// serialize as arrays of *strings*, so this excludes them.
    fn is_color_leaf(v: &serde_json::Value) -> bool {
        v.is_string()
            || v.as_array()
                .is_some_and(|a| a.len() == 3 && a.iter().all(serde_json::Value::is_number))
    }

    /// Distinct, round-trip-stable sentinel color for index `i`. Always an
    /// RGB triple, which `ColorDef` passes through unchanged (only the named
    /// `Color` variants get folded into `ColorDef::Named`).
    fn sentinel(i: usize) -> Color {
        Color::Rgb((i >> 8) as u8, (i & 0xff) as u8, 0x5a)
    }

    #[test]
    fn every_exposed_color_key_resolves_in_both_directions() {
        // Every color the JSON/schema surface exposes must be addressable by
        // BOTH resolvers, under the SAME section it appears in. A key the
        // schema advertises but a resolver drops is silently un-overridable
        // (the drift bug behind #2079): plugin overrides and theme-JSON loads
        // both go through `resolve_theme_key_mut`, the inspector through
        // `resolve_theme_key`.
        let mut theme = Theme::load_builtin(THEME_DARK).expect("dark builtin");
        let mut missing_reader = Vec::new();
        let mut missing_mutator = Vec::new();
        for (section, field) in schema_color_keys() {
            let key = format!("{section}.{field}");
            if theme.resolve_theme_key(&key).is_none() {
                missing_reader.push(key.clone());
            }
            if theme.resolve_theme_key_mut(&key).is_none() {
                missing_mutator.push(key);
            }
        }
        assert!(
            missing_reader.is_empty() && missing_mutator.is_empty(),
            "theme color keys exposed by the JSON schema but dropped by a resolver:\n  \
             resolve_theme_key:     {missing_reader:?}\n  \
             resolve_theme_key_mut: {missing_mutator:?}"
        );
    }

    #[test]
    fn color_keys_round_trip_through_the_same_field_and_section() {
        // Assign every exposed key a distinct color, then push it all the way
        // around the loop and back, checking the value lands in the same slot
        // at every hop:
        //   write    via resolve_theme_key_mut   (string key   -> Theme field)
        //   read     via resolve_theme_key        (Theme field  -> string key)
        //   serialize via From<Theme> for ThemeFile (Theme field -> section.field)
        //   reload   via from_json                 (section.field -> Theme field)
        // If field name OR section disagree between any of these four paths, a
        // sentinel lands in the wrong field and an assert fires — this is what
        // pins names and categories together in all directions.
        let keys = schema_color_keys();
        let mut theme = Theme::load_builtin(THEME_DARK).expect("dark builtin");

        let pairs: Vec<(String, Color)> = keys
            .iter()
            .enumerate()
            .map(|(i, (s, f))| (format!("{s}.{f}"), sentinel(i)))
            .collect();
        let applied = theme.override_colors(pairs.iter().map(|(k, c)| (k.as_str(), *c)));
        assert_eq!(
            applied,
            keys.len(),
            "override_colors should write every exposed key via resolve_theme_key_mut"
        );

        // reader agrees with mutator on which field each key addresses.
        for (i, (s, f)) in keys.iter().enumerate() {
            let key = format!("{s}.{f}");
            assert_eq!(
                theme.resolve_theme_key(&key),
                Some(sentinel(i)),
                "reader and mutator disagree on the field `{key}` addresses"
            );
        }

        // reverse conversion serializes each sentinel back under the SAME
        // section.field — proves field name + category wiring in From<Theme>.
        let file: ThemeFile = theme.into();
        let value = serde_json::to_value(&file).expect("ThemeFile serializes");
        let obj = value.as_object().expect("ThemeFile is a JSON object");
        for (i, (s, f)) in keys.iter().enumerate() {
            let leaf = obj
                .get(s)
                .and_then(|sec| sec.get(f))
                .unwrap_or_else(|| panic!("`{s}.{f}` vanished from serialized ThemeFile"));
            let color: Color = serde_json::from_value::<ColorDef>(leaf.clone())
                .expect("color leaf parses as ColorDef")
                .into();
            assert_eq!(
                color,
                sentinel(i),
                "`{s}.{f}` serialized back to the wrong field or section"
            );
        }

        // forward conversion (from_json) routes each section.field leaf back to
        // the field the reader reads — proves From<ThemeFile> for Theme wiring.
        let reloaded = Theme::from_json(&value.to_string()).expect("from_json round-trips");
        for (i, (s, f)) in keys.iter().enumerate() {
            let key = format!("{s}.{f}");
            assert_eq!(
                reloaded.resolve_theme_key(&key),
                Some(sentinel(i)),
                "`{key}` did not survive ThemeFile -> JSON -> from_json"
            );
        }
    }

    #[test]
    fn test_all_builtin_themes_set_prominent_palette_indicator() {
        // Issue #1711: the Ctrl+P palette hint should be a *prominent*
        // accent drawn from each theme's own palette, not the neutral
        // status-bar colors. The fallback to status_bar_* exists for
        // user themes that don't opt in, but every shipped theme must
        // set explicit values that differ from the bar so the hint
        // pops as intended.
        for builtin in BUILTIN_THEMES {
            let theme = Theme::from_json(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' failed to parse: {}", builtin.name, e));
            assert!(
                theme.status_palette_fg != theme.status_bar_fg
                    || theme.status_palette_bg != theme.status_bar_bg,
                "Theme '{}' must set status_palette_fg/bg to a prominent \
                 accent distinct from status_bar_fg/bg",
                builtin.name
            );
        }
    }

    #[test]
    fn test_all_builtin_themes_have_file_status_colors() {
        // Every builtin theme must produce valid file_status colors (via fallback or explicit)
        for builtin in BUILTIN_THEMES {
            let theme = Theme::from_json(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' failed to parse: {}", builtin.name, e));

            // All six keys must resolve to Some via resolve_theme_key
            for key in &[
                "ui.file_status_added_fg",
                "ui.file_status_modified_fg",
                "ui.file_status_deleted_fg",
                "ui.file_status_renamed_fg",
                "ui.file_status_untracked_fg",
                "ui.file_status_conflicted_fg",
            ] {
                assert!(
                    theme.resolve_theme_key(key).is_some(),
                    "Theme '{}' missing resolution for '{}'",
                    builtin.name,
                    key
                );
            }
        }
    }
}
