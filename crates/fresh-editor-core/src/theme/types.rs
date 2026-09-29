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

/// A syntax color that may carry text attributes.
///
/// A theme key is either a bare color (`[156, 220, 254]` or `"Blue"`) or an
/// object bundling that color with a modifier list
/// (`{"color": [156, 220, 254], "modifier": ["bold"]}`). Bundling keeps the
/// color and its attributes in one value, so nothing has to declare a separate
/// `*_modifier` sibling key or map one to the other. A bare color carries no
/// attributes.
///
/// `Plain` must precede `Styled`: untagged deserialization tries variants in
/// order, and only an object can match `Styled`, so an array/string settles on
/// `Plain` and an object falls through to `Styled`.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(untagged)]
pub enum StyledColorDef {
    /// A color with no text attributes.
    Plain(ColorDef),
    /// A color bundled with the text attributes to render it with.
    Styled {
        color: ColorDef,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        modifier: Option<ModifierDef>,
    },
}

impl StyledColorDef {
    /// The color component, regardless of whether attributes are present.
    pub fn color(&self) -> &ColorDef {
        match self {
            StyledColorDef::Plain(color) => color,
            StyledColorDef::Styled { color, .. } => color,
        }
    }

    /// The text attributes, or `Modifier::empty()` for a bare color.
    pub fn modifier(&self) -> Modifier {
        match self {
            StyledColorDef::Plain(_) => Modifier::empty(),
            StyledColorDef::Styled { modifier, .. } => {
                modifier.as_ref().map(Modifier::from).unwrap_or_default()
            }
        }
    }

    /// Build the serialized form from a resolved color + modifier pair,
    /// collapsing to a bare color when there are no attributes so unchanged
    /// themes round-trip to their original compact JSON.
    pub fn from_parts(color: Color, modifier: Modifier) -> Self {
        if modifier.is_empty() {
            StyledColorDef::Plain(color.into())
        } else {
            StyledColorDef::Styled {
                color: color.into(),
                modifier: Some(modifier.into()),
            }
        }
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
pub fn token_color_named_from_ratatui(color: Color) -> &'static str {
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
/// Every color key is optional in the file; a key the file leaves out is
/// resolved from other keys, never from a color written into the code:
///
/// 1. **`extends`**: with a base theme (`"builtin://light"`, `"dark"`, …),
///    every key the theme leaves out is the base's.
/// 2. **Standalone**: a theme without `extends` that names every *required*
///    key (see [`Theme::is_required_key`], the keys theme files have had since
///    the first theme format) stands on its own. Every other key it leaves out
///    takes the style of its fallback key ([`Theme::fallback_key`]), following
///    the chain to the first key the theme names.
/// 3. **Partial**: a theme without `extends` that leaves out a required key
///    gets an implicit base, as if it extended one: the relative luminance of
///    its `editor.bg` picks `builtin://light` or `builtin://dark`, and with no
///    `editor.bg` it extends `builtin://dark`. This keeps minimal themes that
///    override just `editor`/`syntax` working (issue #1281).
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
    #[serde(default)]
    pub bg: Option<ColorDef>,
    /// Default text color
    #[serde(default)]
    pub fg: Option<ColorDef>,
    /// Cursor color
    #[serde(default)]
    pub cursor: Option<ColorDef>,
    /// Cursor color in unfocused splits
    #[serde(default)]
    pub inactive_cursor: Option<ColorDef>,
    /// Selected text background
    #[serde(default)]
    pub selection_bg: Option<ColorDef>,
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
    #[serde(default)]
    pub current_line_bg: Option<ColorDef>,
    /// Line number text color
    #[serde(default)]
    pub line_number_fg: Option<ColorDef>,
    /// Line number gutter background
    #[serde(default)]
    pub line_number_bg: Option<ColorDef>,
    /// Diff added line background
    #[serde(default)]
    pub diff_add_bg: Option<ColorDef>,
    /// Diff removed line background
    #[serde(default)]
    pub diff_remove_bg: Option<ColorDef>,
    /// Diff added word-level highlight background (optional override)
    /// When not set, computed by brightening diff_add_bg
    #[serde(default)]
    pub diff_add_highlight_bg: Option<ColorDef>,
    /// Diff removed word-level highlight background (optional override)
    /// When not set, computed by brightening diff_remove_bg
    #[serde(default)]
    pub diff_remove_highlight_bg: Option<ColorDef>,
    /// Diff modified line background
    #[serde(default)]
    pub diff_modify_bg: Option<ColorDef>,
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
    #[serde(default)]
    pub ruler_bg: Option<ColorDef>,
    /// Indentation guide foreground color. When omitted, inherits
    /// `whitespace_indicator_fg` so guides remain subtle in both dark and
    /// light themes while still allowing a dedicated override.
    #[serde(default)]
    pub indentation_guide_fg: Option<ColorDef>,
    /// Rainbow indentation-guide color (nesting level 1)
    #[serde(default)]
    pub indent_rainbow_1: Option<ColorDef>,
    /// Rainbow indentation-guide color (nesting level 2)
    #[serde(default)]
    pub indent_rainbow_2: Option<ColorDef>,
    /// Rainbow indentation-guide color (nesting level 3)
    #[serde(default)]
    pub indent_rainbow_3: Option<ColorDef>,
    /// Rainbow indentation-guide color (nesting level 4)
    #[serde(default)]
    pub indent_rainbow_4: Option<ColorDef>,
    /// Rainbow indentation-guide color (nesting level 5)
    #[serde(default)]
    pub indent_rainbow_5: Option<ColorDef>,
    /// Rainbow indentation-guide color (nesting level 6)
    #[serde(default)]
    pub indent_rainbow_6: Option<ColorDef>,
    /// Whitespace indicator foreground color (for tab arrows and space dots)
    #[serde(default)]
    pub whitespace_indicator_fg: Option<ColorDef>,
    /// Whitespace indicator foreground color *inside a selection*. Selected
    /// cells keep their own foreground, so the plain
    /// `whitespace_indicator_fg` — picked to sit on the editor background —
    /// is often invisible on `selection_bg` (several themes use the very
    /// same color for both). When omitted, this is derived from
    /// `selection_bg` by shifting it toward contrast: legible against the
    /// selection, still clearly dimmer than the selected text.
    #[serde(default)]
    pub whitespace_indicator_selected_fg: Option<ColorDef>,
    /// Bracket match highlight color (used when rainbow is disabled)
    #[serde(default)]
    pub bracket_match_fg: Option<ColorDef>,
    /// Text attributes that mark the matched bracket pair when rainbow
    /// brackets are on. The pair keeps its depth color there, so the match
    /// is shown by these attributes instead. Default `["bold", "underlined"]`.
    #[serde(default = "default_bracket_rainbow_match_modifier")]
    pub bracket_rainbow_match_modifier: ModifierDef,
    /// Rainbow bracket color (nesting level 1)
    #[serde(default)]
    pub bracket_rainbow_1: Option<ColorDef>,
    /// Rainbow bracket color (nesting level 2)
    #[serde(default)]
    pub bracket_rainbow_2: Option<ColorDef>,
    /// Rainbow bracket color (nesting level 3)
    #[serde(default)]
    pub bracket_rainbow_3: Option<ColorDef>,
    /// Rainbow bracket color (nesting level 4)
    #[serde(default)]
    pub bracket_rainbow_4: Option<ColorDef>,
    /// Rainbow bracket color (nesting level 5)
    #[serde(default)]
    pub bracket_rainbow_5: Option<ColorDef>,
    /// Rainbow bracket color (nesting level 6)
    #[serde(default)]
    pub bracket_rainbow_6: Option<ColorDef>,
    /// Background color for lines after end-of-file (optional override).
    /// When not set, post-EOF rows keep the theme's editor `bg`, so the
    /// space below a short buffer reads as part of the same surface as the
    /// text. Themes that want the post-EOF area called out (issue #779) can
    /// name a shade here; `~` markers (`editor.show_tilde`) mark the end of
    /// the buffer either way.
    #[serde(default)]
    pub after_eof_bg: Option<ColorDef>,
}

fn default_bracket_rainbow_match_modifier() -> ModifierDef {
    ModifierDef::from(Modifier::BOLD | Modifier::UNDERLINED)
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
    #[serde(default)]
    pub tab_active_fg: Option<ColorDef>,
    /// Active tab background color
    #[serde(default)]
    pub tab_active_bg: Option<ColorDef>,
    /// Inactive tab text color
    #[serde(default)]
    pub tab_inactive_fg: Option<ColorDef>,
    /// Inactive tab background color
    #[serde(default)]
    pub tab_inactive_bg: Option<ColorDef>,
    /// Tab bar separator color
    #[serde(default)]
    pub tab_separator_bg: Option<ColorDef>,
    /// Tab close button hover color
    #[serde(default)]
    pub tab_close_hover_fg: Option<ColorDef>,
    /// Tab hover background color
    #[serde(default)]
    pub tab_hover_bg: Option<ColorDef>,
    /// Menu bar background
    #[serde(default)]
    pub menu_bg: Option<ColorDef>,
    /// Menu bar text color
    #[serde(default)]
    pub menu_fg: Option<ColorDef>,
    /// Active menu item background
    #[serde(default)]
    pub menu_active_bg: Option<ColorDef>,
    /// Active menu item text color
    #[serde(default)]
    pub menu_active_fg: Option<ColorDef>,
    /// Dropdown menu background
    #[serde(default)]
    pub menu_dropdown_bg: Option<ColorDef>,
    /// Dropdown menu text color
    #[serde(default)]
    pub menu_dropdown_fg: Option<ColorDef>,
    /// Highlighted menu item background
    #[serde(default)]
    pub menu_highlight_bg: Option<ColorDef>,
    /// Highlighted menu item text color
    #[serde(default)]
    pub menu_highlight_fg: Option<ColorDef>,
    /// Menu border color
    #[serde(default)]
    pub menu_border_fg: Option<ColorDef>,
    /// Menu separator line color
    #[serde(default)]
    pub menu_separator_fg: Option<ColorDef>,
    /// Menu item hover background
    #[serde(default)]
    pub menu_hover_bg: Option<ColorDef>,
    /// Menu item hover text color
    #[serde(default)]
    pub menu_hover_fg: Option<ColorDef>,
    /// Disabled menu item text color
    #[serde(default)]
    pub menu_disabled_fg: Option<ColorDef>,
    /// Disabled menu item background
    #[serde(default)]
    pub menu_disabled_bg: Option<ColorDef>,
    /// Status bar text color
    #[serde(default)]
    pub status_bar_fg: Option<ColorDef>,
    /// Status bar background color
    #[serde(default)]
    pub status_bar_bg: Option<ColorDef>,
    /// Command palette shortcut hint text color in status bar (falls back to status_bar_fg)
    #[serde(default)]
    pub status_palette_fg: Option<ColorDef>,
    /// Command palette shortcut hint background in status bar (falls back to status_bar_bg)
    #[serde(default)]
    pub status_palette_bg: Option<ColorDef>,
    /// Status bar separator glyph text color (falls back to status_bar_fg)
    #[serde(default)]
    pub status_separator_fg: Option<ColorDef>,
    /// Status bar separator glyph background (falls back to status_bar_bg)
    #[serde(default)]
    pub status_separator_bg: Option<ColorDef>,
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
    #[serde(default)]
    pub prompt_fg: Option<ColorDef>,
    /// Command prompt background
    #[serde(default)]
    pub prompt_bg: Option<ColorDef>,
    /// Prompt selected text color
    #[serde(default)]
    pub prompt_selection_fg: Option<ColorDef>,
    /// Prompt selection background
    #[serde(default)]
    pub prompt_selection_bg: Option<ColorDef>,
    /// Popup window border color
    #[serde(default)]
    pub popup_border_fg: Option<ColorDef>,
    /// Popup window background
    #[serde(default)]
    pub popup_bg: Option<ColorDef>,
    /// Popup selected item background
    #[serde(default)]
    pub popup_selection_bg: Option<ColorDef>,
    /// Selection background inside a widget Text input. Reads
    /// against `prompt_bg`, so it needs higher contrast against
    /// that tint than `editor.selection_bg` (which targets the
    /// editor surface). Defaults to the same `popup_selection_bg`
    /// blue used everywhere "selected item inside a chrome
    /// surface" is shown — same key the prompt selection uses, so
    /// the cue reads consistently across selection UIs.
    #[serde(default)]
    pub text_input_selection_bg: Option<ColorDef>,
    /// Popup selected item text color
    #[serde(default)]
    pub popup_selection_fg: Option<ColorDef>,
    /// Popup window text color. Per the `*_bg`/`*_fg` convention this
    /// is the foreground for `popup_bg`; `popup_fg` is accepted as an
    /// alias so theme JSON can use the convention-consistent name.
    #[serde(default, alias = "popup_fg")]
    pub popup_text_fg: Option<ColorDef>,
    /// Autocomplete suggestion background
    #[serde(default)]
    pub suggestion_bg: Option<ColorDef>,
    /// Text color for content drawn on `suggestion_bg` (autocomplete
    /// items, the overlay-prompt title/input field). Falls back to
    /// `popup_text_fg` so existing themes need no change.
    #[serde(default)]
    pub suggestion_fg: Option<ColorDef>,
    /// Selected suggestion background
    #[serde(default)]
    pub suggestion_selected_bg: Option<ColorDef>,
    /// Help panel background
    #[serde(default)]
    pub help_bg: Option<ColorDef>,
    /// Help panel text color
    #[serde(default)]
    pub help_fg: Option<ColorDef>,
    /// Help keybinding text color
    #[serde(default)]
    pub help_key_fg: Option<ColorDef>,
    /// Help panel separator color
    #[serde(default)]
    pub help_separator_fg: Option<ColorDef>,
    /// Help indicator text color
    #[serde(default)]
    pub help_indicator_fg: Option<ColorDef>,
    /// Help indicator background
    #[serde(default)]
    pub help_indicator_bg: Option<ColorDef>,
    /// Inline code block background
    #[serde(default)]
    pub inline_code_bg: Option<ColorDef>,
    /// Split pane separator color
    #[serde(default)]
    pub split_separator_fg: Option<ColorDef>,
    /// Split separator hover color
    #[serde(default)]
    pub split_separator_hover_fg: Option<ColorDef>,
    /// Scrollbar track color
    #[serde(default)]
    pub scrollbar_track_fg: Option<ColorDef>,
    /// Scrollbar thumb color
    #[serde(default)]
    pub scrollbar_thumb_fg: Option<ColorDef>,
    /// Scrollbar track hover color
    #[serde(default)]
    pub scrollbar_track_hover_fg: Option<ColorDef>,
    /// Scrollbar thumb hover color
    #[serde(default)]
    pub scrollbar_thumb_hover_fg: Option<ColorDef>,
    /// Compose mode margin background
    #[serde(default)]
    pub compose_margin_bg: Option<ColorDef>,
    /// Text color of a git-blame block header band. Falls back to
    /// `ui.menu_fg` — see `blame_header_bg` for why that pair.
    #[serde(default)]
    pub blame_header_fg: Option<ColorDef>,
    /// Background of a git-blame block header band. Falls back to
    /// `ui.menu_bg` (and the text to its `ui.menu_fg`): a menu is the
    /// closest thing a theme already names to what a blame header is — a
    /// chrome band carrying its own text over the editor — so an
    /// unspecified header borrows a surface a theme has already made
    /// legible rather than a color computed from another one.
    ///
    /// Every shipped theme names both keys explicitly (a test enforces it),
    /// so this fallback only ever serves user themes. It is exact, not
    /// derived: a theme whose menu shares the editor background hands the
    /// header that background too, and the fix is for it to name the keys.
    ///
    /// The band used to borrow `ui.status_bar_bg`, which several themes set
    /// to their editor background (`dark` sets exactly it; `high-contrast`
    /// pairs `[20, 20, 20]` with a black editor) — so the header that is
    /// meant to separate one commit's block from the next drew on an
    /// effectively invisible background.
    #[serde(default)]
    pub blame_header_bg: Option<ColorDef>,
    /// Occurrence highlight (word under cursor, or the selected text)
    #[serde(default)]
    pub semantic_highlight_bg: Option<ColorDef>,
    /// Optional text-attribute modifiers (e.g. `["bold"]` or
    /// `["reversed"]`) layered on top of `semantic_highlight_bg`.
    /// Per the canonical native-palette pattern, current-word
    /// highlights are often shown via `Bold` (so the word stands
    /// out against other variables without altering its color slot)
    /// or `Reversed`. See `EditorColors::selection_modifier`.
    #[serde(default)]
    pub semantic_highlight_modifier: Option<ModifierDef>,
    /// Code tour step band background
    #[serde(default)]
    pub tour_step_bg: Option<ColorDef>,
    /// Embedded terminal background (use Default for transparency)
    #[serde(default)]
    pub terminal_bg: Option<ColorDef>,
    /// Embedded terminal default text color
    #[serde(default)]
    pub terminal_fg: Option<ColorDef>,
    /// Warning indicator background in status bar
    #[serde(default)]
    pub status_warning_indicator_bg: Option<ColorDef>,
    /// Warning indicator text color in status bar
    #[serde(default)]
    pub status_warning_indicator_fg: Option<ColorDef>,
    /// Error indicator background in status bar
    #[serde(default)]
    pub status_error_indicator_bg: Option<ColorDef>,
    /// Error indicator text color in status bar
    #[serde(default)]
    pub status_error_indicator_fg: Option<ColorDef>,
    /// Warning indicator hover background
    #[serde(default)]
    pub status_warning_indicator_hover_bg: Option<ColorDef>,
    /// Warning indicator hover text color
    #[serde(default)]
    pub status_warning_indicator_hover_fg: Option<ColorDef>,
    /// Error indicator hover background
    #[serde(default)]
    pub status_error_indicator_hover_bg: Option<ColorDef>,
    /// Error indicator hover text color
    #[serde(default)]
    pub status_error_indicator_hover_fg: Option<ColorDef>,
    /// Tab drop zone background during drag
    #[serde(default)]
    pub tab_drop_zone_bg: Option<ColorDef>,
    /// Tab drop zone border during drag
    #[serde(default)]
    pub tab_drop_zone_border: Option<ColorDef>,
    /// A list row a drag would drop into (the Orchestrator dock's target
    /// folder). Must read apart from the selection and hover bands, which
    /// are on screen at the same time.
    #[serde(default)]
    pub list_drop_target_bg: Option<ColorDef>,
    /// Settings UI selected item background
    #[serde(default)]
    pub settings_selected_bg: Option<ColorDef>,
    /// Settings UI selected item foreground (text on selected background)
    #[serde(default)]
    pub settings_selected_fg: Option<ColorDef>,
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

/// Search result highlighting colors
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SearchColors {
    /// Search match background color
    #[serde(default)]
    pub match_bg: Option<ColorDef>,
    /// Search match text color
    #[serde(default)]
    pub match_fg: Option<ColorDef>,
    /// Background of the *current* search match: the one Find Next / Find
    /// Previous just landed on, or the one Query Replace is asking about.
    /// Should stand out from `match_bg` so the current match is obvious
    /// among the other highlighted matches.
    #[serde(default)]
    pub current_match_bg: Option<ColorDef>,
    /// Text color of the current search match, optionally bundled with text
    /// attributes (`{"color": [255, 255, 255], "modifier": ["bold"]}`).
    #[serde(default)]
    pub current_match_fg: Option<StyledColorDef>,
    /// Background color for jump labels (e.g. flash plugin labels).
    /// Should be visually distinct from `match_bg` so labels stand
    /// out against highlighted matches.  Default: bright magenta.
    #[serde(default)]
    pub label_bg: Option<ColorDef>,
    /// Foreground color for jump labels.  Should be high contrast
    /// against `label_bg` so the single label letter is unambiguous
    /// even on small terminal cells.  Default: white.
    #[serde(default)]
    pub label_fg: Option<ColorDef>,
}

// Mirrors flash.nvim's default FlashLabel (links to Substitute, which
// is a magenta-family colour in most colorschemes).  The pairing is
// chosen so labels pop visually distinct from `search.match_bg`
// (typically yellow / orange).

/// LSP diagnostic colors (errors, warnings, etc.)
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct DiagnosticColors {
    /// Error message text color
    #[serde(default)]
    pub error_fg: Option<ColorDef>,
    /// Error highlight background
    #[serde(default)]
    pub error_bg: Option<ColorDef>,
    /// Warning message text color
    #[serde(default)]
    pub warning_fg: Option<ColorDef>,
    /// Warning highlight background
    #[serde(default)]
    pub warning_bg: Option<ColorDef>,
    /// Info message text color
    #[serde(default)]
    pub info_fg: Option<ColorDef>,
    /// Info highlight background
    #[serde(default)]
    pub info_bg: Option<ColorDef>,
    /// Hint message text color
    #[serde(default)]
    pub hint_fg: Option<ColorDef>,
    /// Hint highlight background
    #[serde(default)]
    pub hint_bg: Option<ColorDef>,
}

/// Syntax highlighting colors.
///
/// Each field is a [`StyledColorDef`]: a bare color, or a color bundled with
/// the text attributes to render it with. The bundle keeps a category's color
/// and its attributes in one value — no parallel `*_modifier` keys.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SyntaxColors {
    /// Language keywords (if, for, fn, etc.)
    #[serde(default)]
    pub keyword: Option<StyledColorDef>,
    /// String literals
    #[serde(default)]
    pub string: Option<StyledColorDef>,
    /// Code comments
    #[serde(default)]
    pub comment: Option<StyledColorDef>,
    /// Function names
    #[serde(default)]
    pub function: Option<StyledColorDef>,
    /// Type names
    #[serde(rename = "type", default)]
    pub type_: Option<StyledColorDef>,
    /// Variable names
    #[serde(default)]
    pub variable: Option<StyledColorDef>,
    /// Built-in language variables (self, this, super, etc.)
    #[serde(default)]
    pub variable_builtin: Option<StyledColorDef>,
    /// Constants and literals
    #[serde(default)]
    pub constant: Option<StyledColorDef>,
    /// Operators (+, -, =, etc.)
    #[serde(default)]
    pub operator: Option<StyledColorDef>,
    /// Punctuation brackets ({, }, (, ), [, ])
    #[serde(default)]
    pub punctuation_bracket: Option<StyledColorDef>,
    /// Punctuation delimiters (;, ,, .)
    #[serde(default)]
    pub punctuation_delimiter: Option<StyledColorDef>,
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

    // Indentation guide color
    pub indentation_guide_fg: Color,
    pub indent_rainbow_1: Color,
    pub indent_rainbow_2: Color,
    pub indent_rainbow_3: Color,
    pub indent_rainbow_4: Color,
    pub indent_rainbow_5: Color,
    pub indent_rainbow_6: Color,

    // Whitespace indicator color (tab arrows, space dots)
    pub whitespace_indicator_fg: Color,
    /// Whitespace indicator color for cells inside a selection.
    pub whitespace_indicator_selected_fg: Color,

    // Bracket matching colors
    pub bracket_match_fg: Color,
    /// The attributes that mark the matched pair with rainbow brackets on.
    pub bracket_rainbow_match_modifier: Modifier,
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
    /// Status bar separator glyph colors (default: same as status bar)
    pub status_separator_fg: Color,
    pub status_separator_bg: Color,
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

    /// Foreground / background of a git-blame block header band
    /// (`ui.blame_header_fg` / `ui.blame_header_bg`), falling back to the
    /// menu surface's own pair when a theme names neither.
    pub blame_header_fg: Color,
    pub blame_header_bg: Color,

    // Occurrence highlighting (word under cursor, or the selected text)
    pub semantic_highlight_bg: Color,
    /// SGR text attributes layered onto occurrence-highlight cells.
    /// Native-palette themes typically set `Modifier::BOLD` (so the
    /// word stands out without altering its color slot) or
    /// `Modifier::REVERSED`.
    pub semantic_highlight_modifier: Modifier,

    /// Background of a code tour's step band (`ui.tour_step_bg`).
    pub tour_step_bg: Color,

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
    pub list_drop_target_bg: Color,

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
    pub search_current_match_bg: Color,
    pub search_current_match_fg: Color,
    pub search_current_match_modifier: Modifier,
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
    pub syntax_keyword_modifier: Modifier,
    pub syntax_string: Color,
    pub syntax_string_modifier: Modifier,
    pub syntax_comment: Color,
    pub syntax_comment_modifier: Modifier,
    pub syntax_function: Color,
    pub syntax_function_modifier: Modifier,
    pub syntax_type: Color,
    pub syntax_type_modifier: Modifier,
    pub syntax_variable: Color,
    pub syntax_variable_modifier: Modifier,
    pub syntax_variable_builtin: Color,
    pub syntax_variable_builtin_modifier: Modifier,
    pub syntax_constant: Color,
    pub syntax_constant_modifier: Modifier,
    pub syntax_operator: Color,
    pub syntax_operator_modifier: Modifier,
    pub syntax_punctuation_bracket: Color,
    pub syntax_punctuation_bracket_modifier: Modifier,
    pub syntax_punctuation_delimiter: Color,
    pub syntax_punctuation_delimiter_modifier: Modifier,
}

/// The color of a key a theme file may leave out, or a placeholder when it
/// does. The placeholder never survives loading: [`Theme::fill_fallbacks`]
/// replaces every key the file leaves out with its fallback key's color.
fn placeholder(def: Option<ColorDef>) -> Color {
    def.map(Color::from).unwrap_or(Color::Reset)
}

/// The color part of a styled key a theme file may leave out (see
/// [`placeholder`]).
fn styled_color(def: &Option<StyledColorDef>) -> Color {
    placeholder(def.as_ref().map(|d| d.color().clone()))
}

/// The text attributes of a styled key; none when the file leaves it out.
fn styled_modifier(def: &Option<StyledColorDef>) -> Modifier {
    def.as_ref()
        .map(StyledColorDef::modifier)
        .unwrap_or_default()
}

/// Converts the keys a theme file names. Keys it leaves out hold serde
/// defaults or [`placeholder`]s until [`Theme::fill_fallbacks`] runs, which
/// every loading path does ([`Theme::from_json`], [`Theme::load_builtin`]).
impl From<ThemeFile> for Theme {
    fn from(file: ThemeFile) -> Self {
        Self {
            name: file.name,
            editor_bg: placeholder(file.editor.bg.clone()),
            editor_fg: placeholder(file.editor.fg.clone()),
            cursor: placeholder(file.editor.cursor.clone()),
            inactive_cursor: placeholder(file.editor.inactive_cursor.clone()),
            selection_bg: placeholder(file.editor.selection_bg.clone()),
            selection_modifier: file
                .editor
                .selection_modifier
                .as_ref()
                .map(Modifier::from)
                .unwrap_or(Modifier::empty()),
            current_line_bg: placeholder(file.editor.current_line_bg.clone()),
            line_number_fg: placeholder(file.editor.line_number_fg.clone()),
            line_number_bg: placeholder(file.editor.line_number_bg.clone()),
            after_eof_bg: placeholder(file.editor.after_eof_bg.clone()),
            ruler_bg: placeholder(file.editor.ruler_bg.clone()),
            indentation_guide_fg: placeholder(file.editor.indentation_guide_fg.clone()),
            indent_rainbow_1: placeholder(file.editor.indent_rainbow_1.clone()),
            indent_rainbow_2: placeholder(file.editor.indent_rainbow_2.clone()),
            indent_rainbow_3: placeholder(file.editor.indent_rainbow_3.clone()),
            indent_rainbow_4: placeholder(file.editor.indent_rainbow_4.clone()),
            indent_rainbow_5: placeholder(file.editor.indent_rainbow_5.clone()),
            indent_rainbow_6: placeholder(file.editor.indent_rainbow_6.clone()),
            whitespace_indicator_fg: placeholder(file.editor.whitespace_indicator_fg.clone()),
            whitespace_indicator_selected_fg: placeholder(
                file.editor.whitespace_indicator_selected_fg.clone(),
            ),
            bracket_match_fg: placeholder(file.editor.bracket_match_fg.clone()),
            bracket_rainbow_match_modifier: Modifier::from(
                &file.editor.bracket_rainbow_match_modifier,
            ),
            bracket_rainbow_1: placeholder(file.editor.bracket_rainbow_1.clone()),
            bracket_rainbow_2: placeholder(file.editor.bracket_rainbow_2.clone()),
            bracket_rainbow_3: placeholder(file.editor.bracket_rainbow_3.clone()),
            bracket_rainbow_4: placeholder(file.editor.bracket_rainbow_4.clone()),
            bracket_rainbow_5: placeholder(file.editor.bracket_rainbow_5.clone()),
            bracket_rainbow_6: placeholder(file.editor.bracket_rainbow_6.clone()),
            diff_add_bg: placeholder(file.editor.diff_add_bg.clone()),
            diff_remove_bg: placeholder(file.editor.diff_remove_bg.clone()),
            diff_modify_bg: placeholder(file.editor.diff_modify_bg.clone()),
            diff_add_highlight_bg: placeholder(file.editor.diff_add_highlight_bg.clone()),
            diff_remove_highlight_bg: placeholder(file.editor.diff_remove_highlight_bg.clone()),
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
            tab_active_fg: placeholder(file.ui.tab_active_fg.clone()),
            tab_active_bg: placeholder(file.ui.tab_active_bg.clone()),
            tab_inactive_fg: placeholder(file.ui.tab_inactive_fg.clone()),
            tab_inactive_bg: placeholder(file.ui.tab_inactive_bg.clone()),
            tab_separator_bg: placeholder(file.ui.tab_separator_bg.clone()),
            tab_close_hover_fg: placeholder(file.ui.tab_close_hover_fg.clone()),
            tab_hover_bg: placeholder(file.ui.tab_hover_bg.clone()),
            menu_bg: placeholder(file.ui.menu_bg.clone()),
            menu_fg: placeholder(file.ui.menu_fg.clone()),
            menu_active_bg: placeholder(file.ui.menu_active_bg.clone()),
            menu_active_fg: placeholder(file.ui.menu_active_fg.clone()),
            menu_dropdown_bg: placeholder(file.ui.menu_dropdown_bg.clone()),
            menu_dropdown_fg: placeholder(file.ui.menu_dropdown_fg.clone()),
            menu_highlight_bg: placeholder(file.ui.menu_highlight_bg.clone()),
            menu_highlight_fg: placeholder(file.ui.menu_highlight_fg.clone()),
            menu_border_fg: placeholder(file.ui.menu_border_fg.clone()),
            menu_separator_fg: placeholder(file.ui.menu_separator_fg.clone()),
            menu_hover_bg: placeholder(file.ui.menu_hover_bg.clone()),
            menu_hover_fg: placeholder(file.ui.menu_hover_fg.clone()),
            menu_disabled_fg: placeholder(file.ui.menu_disabled_fg.clone()),
            menu_disabled_bg: placeholder(file.ui.menu_disabled_bg.clone()),
            status_bar_fg: placeholder(file.ui.status_bar_fg.clone()),
            status_bar_bg: placeholder(file.ui.status_bar_bg.clone()),
            status_palette_fg: placeholder(file.ui.status_palette_fg.clone()),
            status_palette_bg: placeholder(file.ui.status_palette_bg.clone()),
            status_separator_fg: placeholder(file.ui.status_separator_fg.clone()),
            status_separator_bg: placeholder(file.ui.status_separator_bg.clone()),
            status_lsp_on_fg: placeholder(file.ui.status_lsp_on_fg.clone()),
            status_lsp_on_bg: placeholder(file.ui.status_lsp_on_bg.clone()),
            status_lsp_actionable_fg: placeholder(file.ui.status_lsp_actionable_fg.clone()),
            status_lsp_actionable_bg: placeholder(file.ui.status_lsp_actionable_bg.clone()),
            prompt_fg: placeholder(file.ui.prompt_fg.clone()),
            prompt_bg: placeholder(file.ui.prompt_bg.clone()),
            prompt_selection_fg: placeholder(file.ui.prompt_selection_fg.clone()),
            prompt_selection_bg: placeholder(file.ui.prompt_selection_bg.clone()),
            popup_border_fg: placeholder(file.ui.popup_border_fg.clone()),
            popup_bg: placeholder(file.ui.popup_bg.clone()),
            popup_selection_bg: placeholder(file.ui.popup_selection_bg.clone()),
            popup_selection_fg: placeholder(file.ui.popup_selection_fg.clone()),
            popup_text_fg: placeholder(file.ui.popup_text_fg.clone()),
            text_input_selection_bg: placeholder(file.ui.text_input_selection_bg.clone()),
            suggestion_bg: placeholder(file.ui.suggestion_bg.clone()),
            suggestion_fg: placeholder(file.ui.suggestion_fg.clone()),
            suggestion_selected_bg: placeholder(file.ui.suggestion_selected_bg.clone()),
            help_bg: placeholder(file.ui.help_bg.clone()),
            help_fg: placeholder(file.ui.help_fg.clone()),
            help_key_fg: placeholder(file.ui.help_key_fg.clone()),
            help_separator_fg: placeholder(file.ui.help_separator_fg.clone()),
            help_indicator_fg: placeholder(file.ui.help_indicator_fg.clone()),
            help_indicator_bg: placeholder(file.ui.help_indicator_bg.clone()),
            inline_code_bg: placeholder(file.ui.inline_code_bg.clone()),
            split_separator_fg: placeholder(file.ui.split_separator_fg.clone()),
            split_separator_hover_fg: placeholder(file.ui.split_separator_hover_fg.clone()),
            scrollbar_track_fg: placeholder(file.ui.scrollbar_track_fg.clone()),
            scrollbar_thumb_fg: placeholder(file.ui.scrollbar_thumb_fg.clone()),
            scrollbar_track_hover_fg: placeholder(file.ui.scrollbar_track_hover_fg.clone()),
            scrollbar_thumb_hover_fg: placeholder(file.ui.scrollbar_thumb_hover_fg.clone()),
            compose_margin_bg: placeholder(file.ui.compose_margin_bg.clone()),
            blame_header_fg: placeholder(file.ui.blame_header_fg.clone()),
            blame_header_bg: placeholder(file.ui.blame_header_bg.clone()),
            semantic_highlight_bg: placeholder(file.ui.semantic_highlight_bg.clone()),
            semantic_highlight_modifier: file
                .ui
                .semantic_highlight_modifier
                .as_ref()
                .map(Modifier::from)
                .unwrap_or(Modifier::empty()),
            tour_step_bg: placeholder(file.ui.tour_step_bg.clone()),
            terminal_bg: placeholder(file.ui.terminal_bg.clone()),
            terminal_fg: placeholder(file.ui.terminal_fg.clone()),
            status_warning_indicator_bg: placeholder(file.ui.status_warning_indicator_bg.clone()),
            status_warning_indicator_fg: placeholder(file.ui.status_warning_indicator_fg.clone()),
            status_error_indicator_bg: placeholder(file.ui.status_error_indicator_bg.clone()),
            status_error_indicator_fg: placeholder(file.ui.status_error_indicator_fg.clone()),
            status_warning_indicator_hover_bg: placeholder(
                file.ui.status_warning_indicator_hover_bg.clone(),
            ),
            status_warning_indicator_hover_fg: placeholder(
                file.ui.status_warning_indicator_hover_fg.clone(),
            ),
            status_error_indicator_hover_bg: placeholder(
                file.ui.status_error_indicator_hover_bg.clone(),
            ),
            status_error_indicator_hover_fg: placeholder(
                file.ui.status_error_indicator_hover_fg.clone(),
            ),
            tab_drop_zone_bg: placeholder(file.ui.tab_drop_zone_bg.clone()),
            tab_drop_zone_border: placeholder(file.ui.tab_drop_zone_border.clone()),
            list_drop_target_bg: placeholder(file.ui.list_drop_target_bg.clone()),
            settings_selected_bg: placeholder(file.ui.settings_selected_bg.clone()),
            settings_selected_fg: placeholder(file.ui.settings_selected_fg.clone()),
            file_status_added_fg: placeholder(file.ui.file_status_added_fg.clone()),
            file_status_modified_fg: placeholder(file.ui.file_status_modified_fg.clone()),
            file_status_deleted_fg: placeholder(file.ui.file_status_deleted_fg.clone()),
            file_status_renamed_fg: placeholder(file.ui.file_status_renamed_fg.clone()),
            file_status_untracked_fg: placeholder(file.ui.file_status_untracked_fg.clone()),
            file_status_conflicted_fg: placeholder(file.ui.file_status_conflicted_fg.clone()),
            search_match_bg: placeholder(file.search.match_bg.clone()),
            search_match_fg: placeholder(file.search.match_fg.clone()),
            search_current_match_bg: placeholder(file.search.current_match_bg.clone()),
            search_current_match_fg: styled_color(&file.search.current_match_fg),
            search_current_match_modifier: styled_modifier(&file.search.current_match_fg),
            search_label_bg: placeholder(file.search.label_bg.clone()),
            search_label_fg: placeholder(file.search.label_fg.clone()),
            diagnostic_error_fg: placeholder(file.diagnostic.error_fg.clone()),
            diagnostic_error_bg: placeholder(file.diagnostic.error_bg.clone()),
            diagnostic_warning_fg: placeholder(file.diagnostic.warning_fg.clone()),
            diagnostic_warning_bg: placeholder(file.diagnostic.warning_bg.clone()),
            diagnostic_info_fg: placeholder(file.diagnostic.info_fg.clone()),
            diagnostic_info_bg: placeholder(file.diagnostic.info_bg.clone()),
            diagnostic_hint_fg: placeholder(file.diagnostic.hint_fg.clone()),
            diagnostic_hint_bg: placeholder(file.diagnostic.hint_bg.clone()),
            syntax_keyword: styled_color(&file.syntax.keyword),
            syntax_keyword_modifier: styled_modifier(&file.syntax.keyword),
            syntax_string: styled_color(&file.syntax.string),
            syntax_string_modifier: styled_modifier(&file.syntax.string),
            syntax_comment: styled_color(&file.syntax.comment),
            syntax_comment_modifier: styled_modifier(&file.syntax.comment),
            syntax_function: styled_color(&file.syntax.function),
            syntax_function_modifier: styled_modifier(&file.syntax.function),
            syntax_type: styled_color(&file.syntax.type_),
            syntax_type_modifier: styled_modifier(&file.syntax.type_),
            syntax_variable: styled_color(&file.syntax.variable),
            syntax_variable_modifier: styled_modifier(&file.syntax.variable),
            syntax_variable_builtin: styled_color(&file.syntax.variable_builtin),
            syntax_variable_builtin_modifier: styled_modifier(&file.syntax.variable_builtin),
            syntax_constant: styled_color(&file.syntax.constant),
            syntax_constant_modifier: styled_modifier(&file.syntax.constant),
            syntax_operator: styled_color(&file.syntax.operator),
            syntax_operator_modifier: styled_modifier(&file.syntax.operator),
            syntax_punctuation_bracket: styled_color(&file.syntax.punctuation_bracket),
            syntax_punctuation_bracket_modifier: styled_modifier(&file.syntax.punctuation_bracket),
            syntax_punctuation_delimiter: styled_color(&file.syntax.punctuation_delimiter),
            syntax_punctuation_delimiter_modifier: styled_modifier(
                &file.syntax.punctuation_delimiter,
            ),
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
                bg: Some(theme.editor_bg.into()),
                fg: Some(theme.editor_fg.into()),
                cursor: Some(theme.cursor.into()),
                inactive_cursor: Some(theme.inactive_cursor.into()),
                selection_bg: Some(theme.selection_bg.into()),
                selection_modifier: if theme.selection_modifier.is_empty() {
                    None
                } else {
                    Some(theme.selection_modifier.into())
                },
                current_line_bg: Some(theme.current_line_bg.into()),
                line_number_fg: Some(theme.line_number_fg.into()),
                line_number_bg: Some(theme.line_number_bg.into()),
                diff_add_bg: Some(theme.diff_add_bg.into()),
                diff_remove_bg: Some(theme.diff_remove_bg.into()),
                diff_add_highlight_bg: Some(theme.diff_add_highlight_bg.into()),
                diff_remove_highlight_bg: Some(theme.diff_remove_highlight_bg.into()),
                diff_modify_bg: Some(theme.diff_modify_bg.into()),
                diff_add_collision_fg: theme.diff_add_collision_fg.map(|c| c.into()),
                diff_remove_collision_fg: theme.diff_remove_collision_fg.map(|c| c.into()),
                diff_modify_collision_fg: theme.diff_modify_collision_fg.map(|c| c.into()),
                ruler_bg: Some(theme.ruler_bg.into()),
                indentation_guide_fg: Some(theme.indentation_guide_fg.into()),
                indent_rainbow_1: Some(theme.indent_rainbow_1.into()),
                indent_rainbow_2: Some(theme.indent_rainbow_2.into()),
                indent_rainbow_3: Some(theme.indent_rainbow_3.into()),
                indent_rainbow_4: Some(theme.indent_rainbow_4.into()),
                indent_rainbow_5: Some(theme.indent_rainbow_5.into()),
                indent_rainbow_6: Some(theme.indent_rainbow_6.into()),
                whitespace_indicator_fg: Some(theme.whitespace_indicator_fg.into()),
                whitespace_indicator_selected_fg: Some(
                    theme.whitespace_indicator_selected_fg.into(),
                ),
                bracket_match_fg: Some(theme.bracket_match_fg.into()),
                bracket_rainbow_match_modifier: theme.bracket_rainbow_match_modifier.into(),
                bracket_rainbow_1: Some(theme.bracket_rainbow_1.into()),
                bracket_rainbow_2: Some(theme.bracket_rainbow_2.into()),
                bracket_rainbow_3: Some(theme.bracket_rainbow_3.into()),
                bracket_rainbow_4: Some(theme.bracket_rainbow_4.into()),
                bracket_rainbow_5: Some(theme.bracket_rainbow_5.into()),
                bracket_rainbow_6: Some(theme.bracket_rainbow_6.into()),
                after_eof_bg: Some(theme.after_eof_bg.into()),
            },
            ui: UiColors {
                tab_active_fg: Some(theme.tab_active_fg.into()),
                tab_active_bg: Some(theme.tab_active_bg.into()),
                tab_inactive_fg: Some(theme.tab_inactive_fg.into()),
                tab_inactive_bg: Some(theme.tab_inactive_bg.into()),
                tab_separator_bg: Some(theme.tab_separator_bg.into()),
                tab_close_hover_fg: Some(theme.tab_close_hover_fg.into()),
                tab_hover_bg: Some(theme.tab_hover_bg.into()),
                menu_bg: Some(theme.menu_bg.into()),
                menu_fg: Some(theme.menu_fg.into()),
                menu_active_bg: Some(theme.menu_active_bg.into()),
                menu_active_fg: Some(theme.menu_active_fg.into()),
                menu_dropdown_bg: Some(theme.menu_dropdown_bg.into()),
                menu_dropdown_fg: Some(theme.menu_dropdown_fg.into()),
                menu_highlight_bg: Some(theme.menu_highlight_bg.into()),
                menu_highlight_fg: Some(theme.menu_highlight_fg.into()),
                menu_border_fg: Some(theme.menu_border_fg.into()),
                menu_separator_fg: Some(theme.menu_separator_fg.into()),
                menu_hover_bg: Some(theme.menu_hover_bg.into()),
                menu_hover_fg: Some(theme.menu_hover_fg.into()),
                menu_disabled_fg: Some(theme.menu_disabled_fg.into()),
                menu_disabled_bg: Some(theme.menu_disabled_bg.into()),
                status_bar_fg: Some(theme.status_bar_fg.into()),
                status_bar_bg: Some(theme.status_bar_bg.into()),
                status_palette_fg: Some(theme.status_palette_fg.into()),
                status_palette_bg: Some(theme.status_palette_bg.into()),
                status_separator_fg: Some(theme.status_separator_fg.into()),
                status_separator_bg: Some(theme.status_separator_bg.into()),
                status_lsp_on_fg: Some(theme.status_lsp_on_fg.into()),
                status_lsp_on_bg: Some(theme.status_lsp_on_bg.into()),
                status_lsp_actionable_fg: Some(theme.status_lsp_actionable_fg.into()),
                status_lsp_actionable_bg: Some(theme.status_lsp_actionable_bg.into()),
                prompt_fg: Some(theme.prompt_fg.into()),
                prompt_bg: Some(theme.prompt_bg.into()),
                prompt_selection_fg: Some(theme.prompt_selection_fg.into()),
                prompt_selection_bg: Some(theme.prompt_selection_bg.into()),
                popup_border_fg: Some(theme.popup_border_fg.into()),
                popup_bg: Some(theme.popup_bg.into()),
                popup_selection_bg: Some(theme.popup_selection_bg.into()),
                popup_selection_fg: Some(theme.popup_selection_fg.into()),
                popup_text_fg: Some(theme.popup_text_fg.into()),
                text_input_selection_bg: Some(theme.text_input_selection_bg.into()),
                suggestion_bg: Some(theme.suggestion_bg.into()),
                suggestion_fg: Some(theme.suggestion_fg.into()),
                suggestion_selected_bg: Some(theme.suggestion_selected_bg.into()),
                help_bg: Some(theme.help_bg.into()),
                help_fg: Some(theme.help_fg.into()),
                help_key_fg: Some(theme.help_key_fg.into()),
                help_separator_fg: Some(theme.help_separator_fg.into()),
                help_indicator_fg: Some(theme.help_indicator_fg.into()),
                help_indicator_bg: Some(theme.help_indicator_bg.into()),
                inline_code_bg: Some(theme.inline_code_bg.into()),
                split_separator_fg: Some(theme.split_separator_fg.into()),
                split_separator_hover_fg: Some(theme.split_separator_hover_fg.into()),
                scrollbar_track_fg: Some(theme.scrollbar_track_fg.into()),
                scrollbar_thumb_fg: Some(theme.scrollbar_thumb_fg.into()),
                scrollbar_track_hover_fg: Some(theme.scrollbar_track_hover_fg.into()),
                scrollbar_thumb_hover_fg: Some(theme.scrollbar_thumb_hover_fg.into()),
                compose_margin_bg: Some(theme.compose_margin_bg.into()),
                blame_header_fg: Some(theme.blame_header_fg.into()),
                blame_header_bg: Some(theme.blame_header_bg.into()),
                semantic_highlight_bg: Some(theme.semantic_highlight_bg.into()),
                semantic_highlight_modifier: if theme.semantic_highlight_modifier.is_empty() {
                    None
                } else {
                    Some(theme.semantic_highlight_modifier.into())
                },
                tour_step_bg: Some(theme.tour_step_bg.into()),
                terminal_bg: Some(theme.terminal_bg.into()),
                terminal_fg: Some(theme.terminal_fg.into()),
                status_warning_indicator_bg: Some(theme.status_warning_indicator_bg.into()),
                status_warning_indicator_fg: Some(theme.status_warning_indicator_fg.into()),
                status_error_indicator_bg: Some(theme.status_error_indicator_bg.into()),
                status_error_indicator_fg: Some(theme.status_error_indicator_fg.into()),
                status_warning_indicator_hover_bg: Some(
                    theme.status_warning_indicator_hover_bg.into(),
                ),
                status_warning_indicator_hover_fg: Some(
                    theme.status_warning_indicator_hover_fg.into(),
                ),
                status_error_indicator_hover_bg: Some(theme.status_error_indicator_hover_bg.into()),
                status_error_indicator_hover_fg: Some(theme.status_error_indicator_hover_fg.into()),
                tab_drop_zone_bg: Some(theme.tab_drop_zone_bg.into()),
                tab_drop_zone_border: Some(theme.tab_drop_zone_border.into()),
                list_drop_target_bg: Some(theme.list_drop_target_bg.into()),
                settings_selected_bg: Some(theme.settings_selected_bg.into()),
                settings_selected_fg: Some(theme.settings_selected_fg.into()),
                file_status_added_fg: Some(theme.file_status_added_fg.into()),
                file_status_modified_fg: Some(theme.file_status_modified_fg.into()),
                file_status_deleted_fg: Some(theme.file_status_deleted_fg.into()),
                file_status_renamed_fg: Some(theme.file_status_renamed_fg.into()),
                file_status_untracked_fg: Some(theme.file_status_untracked_fg.into()),
                file_status_conflicted_fg: Some(theme.file_status_conflicted_fg.into()),
            },
            search: SearchColors {
                match_bg: Some(theme.search_match_bg.into()),
                match_fg: Some(theme.search_match_fg.into()),
                current_match_bg: Some(theme.search_current_match_bg.into()),
                current_match_fg: Some(StyledColorDef::from_parts(
                    theme.search_current_match_fg,
                    theme.search_current_match_modifier,
                )),
                label_bg: Some(theme.search_label_bg.into()),
                label_fg: Some(theme.search_label_fg.into()),
            },
            diagnostic: DiagnosticColors {
                error_fg: Some(theme.diagnostic_error_fg.into()),
                error_bg: Some(theme.diagnostic_error_bg.into()),
                warning_fg: Some(theme.diagnostic_warning_fg.into()),
                warning_bg: Some(theme.diagnostic_warning_bg.into()),
                info_fg: Some(theme.diagnostic_info_fg.into()),
                info_bg: Some(theme.diagnostic_info_bg.into()),
                hint_fg: Some(theme.diagnostic_hint_fg.into()),
                hint_bg: Some(theme.diagnostic_hint_bg.into()),
            },
            syntax: SyntaxColors {
                keyword: Some(StyledColorDef::from_parts(
                    theme.syntax_keyword,
                    theme.syntax_keyword_modifier,
                )),
                string: Some(StyledColorDef::from_parts(
                    theme.syntax_string,
                    theme.syntax_string_modifier,
                )),
                comment: Some(StyledColorDef::from_parts(
                    theme.syntax_comment,
                    theme.syntax_comment_modifier,
                )),
                function: Some(StyledColorDef::from_parts(
                    theme.syntax_function,
                    theme.syntax_function_modifier,
                )),
                type_: Some(StyledColorDef::from_parts(
                    theme.syntax_type,
                    theme.syntax_type_modifier,
                )),
                variable: Some(StyledColorDef::from_parts(
                    theme.syntax_variable,
                    theme.syntax_variable_modifier,
                )),
                variable_builtin: Some(StyledColorDef::from_parts(
                    theme.syntax_variable_builtin,
                    theme.syntax_variable_builtin_modifier,
                )),
                constant: Some(StyledColorDef::from_parts(
                    theme.syntax_constant,
                    theme.syntax_constant_modifier,
                )),
                operator: Some(StyledColorDef::from_parts(
                    theme.syntax_operator,
                    theme.syntax_operator_modifier,
                )),
                punctuation_bracket: Some(StyledColorDef::from_parts(
                    theme.syntax_punctuation_bracket,
                    theme.syntax_punctuation_bracket_modifier,
                )),
                punctuation_delimiter: Some(StyledColorDef::from_parts(
                    theme.syntax_punctuation_delimiter,
                    theme.syntax_punctuation_delimiter_modifier,
                )),
            },
        }
    }
}

/// Whether the theme JSON `raw` names the color key `"section.field"`
/// (a JSON `null` counts as leaving it out).
fn names_key(raw: &serde_json::Value, key: &str) -> bool {
    split_theme_key(key).is_some_and(|(section, field)| {
        raw.get(section)
            .and_then(|s| s.get(field))
            .is_some_and(|v| !v.is_null())
    })
}

/// The base theme a parsed `ThemeFile` is layered on, or `None` for a
/// standalone theme. See [`ThemeFile`] for the rules. Errors only when
/// `extends` names a base that does not exist.
fn resolve_base_theme(
    theme_file: &ThemeFile,
    raw: &serde_json::Value,
) -> Result<Option<Theme>, String> {
    // 1. Explicit `extends`.
    if let Some(extends) = theme_file.extends.as_deref() {
        let name = extends.strip_prefix("builtin://").unwrap_or(extends);
        return Theme::load_builtin(name).map(Some).ok_or_else(|| {
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

    // 2. Standalone: names every required key.
    if Theme::COLOR_KEYS
        .iter()
        .filter(|key| Theme::is_required_key(key))
        .all(|key| names_key(raw, key))
    {
        return Ok(None);
    }

    // 3. Partial: an implicit base, light or dark by `editor.bg`'s luminance.
    let bg = raw
        .get("editor")
        .and_then(|e| e.get("bg"))
        .cloned()
        .and_then(|v| serde_json::from_value::<ColorDef>(v).ok())
        .and_then(|bg| color_to_rgb(bg.into()));
    let base_name = match bg {
        Some((r, g, b)) if relative_luminance(r, g, b) > 0.5 => THEME_LIGHT,
        _ => THEME_DARK,
    };
    Ok(Theme::load_builtin(base_name))
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
            // A key that is only text attributes takes a modifier list.
            if let Some(slot) = theme.resolve_modifier_only_key_mut(&key) {
                if let Ok(def) = serde_json::from_value::<ModifierDef>(value.clone()) {
                    *slot = Modifier::from(&def);
                }
                continue;
            }
            // A value is a bare color or a `{color, modifier}` bundle; the
            // richer form also accepts a bare color (empty modifier). The
            // whole value defines the style, so a bare color clears any
            // modifier the base theme set for this key.
            let Ok(styled) = serde_json::from_value::<StyledColorDef>(value.clone()) else {
                continue;
            };
            if let Some(slot) = theme.resolve_theme_key_mut(&key) {
                *slot = styled.color().clone().into();
            }
            if let Some(slot) = theme.resolve_modifier_key_mut(&key) {
                *slot = styled.modifier();
            }
        }
    }
}

impl Theme {
    /// Background to paint rows past end-of-file on, given the effective
    /// editor background of the split being drawn.
    ///
    /// `effective_editor_bg` is `editor_bg` normally and `Color::Reset` when
    /// `editor.use_terminal_bg` puts the terminal's own background behind the
    /// buffer. A theme that does not name `after_eof_bg` resolves it to
    /// `editor_bg`, and those rows have to follow whatever the content rows
    /// are actually painted on — otherwise `use_terminal_bg` leaves an opaque
    /// band below the last line, which is the thing the derived shade was
    /// removed for. A theme that does name a distinct post-EOF color keeps
    /// it: an explicit color is a choice, not a fallback.
    pub fn post_eof_bg(&self, effective_editor_bg: Color) -> Color {
        if self.after_eof_bg == self.editor_bg {
            effective_editor_bg
        } else {
            self.after_eof_bg
        }
    }

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
        // Built-in themes are standalone: each names every required key.
        let json = BUILTIN_THEMES.iter().find(|t| t.name == name)?.json;
        let raw: serde_json::Value = serde_json::from_str(json).ok()?;
        let theme_file: ThemeFile = serde_json::from_value(raw.clone()).ok()?;
        let mut theme: Theme = theme_file.into();
        theme.fill_fallbacks(&raw);
        Some(theme)
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
        // `name` / `extends` cheaply; the raw `Value` tells us *which* keys
        // the theme names, including modifier-only keys and `null`s.
        let raw: serde_json::Value =
            serde_json::from_str(json).map_err(|e| format!("Failed to parse theme JSON: {}", e))?;
        let theme_file: ThemeFile = serde_json::from_value(raw.clone())
            .map_err(|e| format!("Failed to parse theme: {}", e))?;

        match resolve_base_theme(&theme_file, &raw)? {
            Some(mut theme) => {
                apply_theme_overrides(&mut theme, &theme_file, &raw);
                Ok(theme)
            }
            None => {
                let mut theme: Theme = theme_file.into();
                theme.fill_fallbacks(&raw);
                Ok(theme)
            }
        }
    }

    /// Give every color key the theme JSON `raw` leaves out the style of its
    /// fallback key: the first key along its fallback chain that `raw` names,
    /// or the chain's required key.
    fn fill_fallbacks(&mut self, raw: &serde_json::Value) {
        for &key in Self::COLOR_KEYS {
            if names_key(raw, key) {
                continue;
            }
            let mut source = key;
            while let Some(next) = Self::fallback_key(source) {
                source = next;
                if names_key(raw, source) {
                    break;
                }
            }
            if source == key {
                continue; // a required or optional key: nothing to fall back to
            }
            let modifier = self.resolve_modifier_key(source);
            if let (Some(color), Some(slot)) = (
                self.resolve_theme_key(source),
                self.resolve_theme_key_mut(key),
            ) {
                *slot = color;
            }
            if let Some(slot) = self.resolve_modifier_key_mut(key) {
                *slot = modifier;
            }
        }
    }

    /// The slot for a key that names text attributes alone, with no color
    /// of its own (`editor.selection_modifier`, …). The color table cannot
    /// hold these, so an override of one in a theme with a base is applied
    /// through here.
    pub fn resolve_modifier_only_key_mut(&mut self, key: &str) -> Option<&mut Modifier> {
        match key {
            "editor.selection_modifier" => Some(&mut self.selection_modifier),
            "ui.semantic_highlight_modifier" => Some(&mut self.semantic_highlight_modifier),
            "editor.bracket_rainbow_match_modifier" => {
                Some(&mut self.bracket_rainbow_match_modifier)
            }
            _ => None,
        }
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
}

/// Split a `"section.field"` theme key into its two components, returning
/// `None` when it is not exactly two dot-separated parts.
fn split_theme_key(key: &str) -> Option<(&str, &str)> {
    let (section, field) = key.split_once('.')?;
    if field.contains('.') {
        return None;
    }
    Some((section, field))
}

/// Generate the read and write theme-key resolvers from a single key -> field
/// table.
///
/// [`Theme::resolve_theme_key`] (by value) and [`Theme::resolve_theme_key_mut`]
/// (by `&mut`) map exactly the same `"section.field"` keys to the same `Theme`
/// fields. They used to be two hand-maintained ~150-line `match`es that had to
/// be kept "in lock-step" by eye — and had already drifted into different
/// orderings. Deriving both from one table makes divergence impossible by
/// construction. `color` entries are plain `Color` fields; `opt` entries are
/// `Option<Color>` fields, which are only writable once the theme JSON has set
/// them (so the mutable resolver yields `None` while they are unset).
macro_rules! theme_color_keys {
    (
        $(
            $section:literal => {
                $( $field_key:literal => $kind:tt $field:ident $(modifier $mod:ident)? $(fallback $fb:literal)? ),* $(,)?
            }
        ),* $(,)?
    ) => {
        impl Theme {
            /// Resolve a `"section.field"` theme key to its `Color` value.
            ///
            /// Returns `None` for an unrecognized key, or a malformed key that
            /// is not exactly two dot-separated parts.
            pub fn resolve_theme_key(&self, key: &str) -> Option<Color> {
                let (section, field) = split_theme_key(key)?;
                match section {
                    $(
                        $section => match field {
                            $( $field_key => theme_color_keys!(@get self, $kind $field), )*
                            _ => None,
                        },
                    )*
                    _ => None,
                }
            }

            /// The `'static` spelling of a theme key, if it is one.
            ///
            /// Generated from the same table as [`Theme::resolve_theme_key`],
            /// so validating a key and getting a name that outlives the caller
            /// are one step. Provenance wants both — a
            /// `ThemeRun` (in `fresh-editor`) borrows for `'static`
            /// — and a key parsed out of a run at paint time is a `&str` with
            /// a frame's lifetime until it comes through here.
            pub fn static_theme_key(key: &str) -> Option<&'static str> {
                let (section, field) = split_theme_key(key)?;
                match section {
                    $(
                        $section => match field {
                            $( $field_key => Some(concat!($section, ".", $field_key)), )*
                            _ => None,
                        },
                    )*
                    _ => None,
                }
            }

            /// Mutable companion to [`Theme::resolve_theme_key`]. Generated
            /// from the same table, so the readable and writable key sets stay
            /// identical by construction.
            pub fn resolve_theme_key_mut(&mut self, key: &str) -> Option<&mut Color> {
                let (section, field) = split_theme_key(key)?;
                match section {
                    $(
                        $section => match field {
                            $( $field_key => theme_color_keys!(@get_mut self, $kind $field), )*
                            _ => None,
                        },
                    )*
                    _ => None,
                }
            }

            /// Text-attribute [`Modifier`] a `"section.field"` color key opts
            /// into, if any. The color key and its modifier field are declared
            /// on the same table row, so the relation lives in one place instead
            /// of a hand-maintained parallel `match`. Keys with no declared
            /// modifier (and unknown keys) return `Modifier::empty()`.
            pub fn resolve_modifier_key(&self, key: &str) -> Modifier {
                let Some((section, field)) = split_theme_key(key) else {
                    return Modifier::empty();
                };
                match section {
                    $(
                        $section => match field {
                            $( $field_key => theme_color_keys!(@mod self $(, $mod)?), )*
                            _ => Modifier::empty(),
                        },
                    )*
                    _ => Modifier::empty(),
                }
            }

            /// Mutable companion to [`Theme::resolve_modifier_key`]; yields the
            /// modifier slot for a color key that declares one.
            pub fn resolve_modifier_key_mut(&mut self, key: &str) -> Option<&mut Modifier> {
                let (section, field) = split_theme_key(key)?;
                match section {
                    $(
                        $section => match field {
                            $( $field_key => theme_color_keys!(@mod_mut self $(, $mod)?), )*
                            _ => None,
                        },
                    )*
                    _ => None,
                }
            }

            /// Every color key, as `"section.field"`.
            pub const COLOR_KEYS: &'static [&'static str] = &[
                $( $( concat!($section, ".", $field_key), )* )*
            ];

            /// The key a color key takes its style from when a theme without
            /// a base leaves it out. `None` for a required key (see
            /// [`Theme::is_required_key`]) and for an optional `opt` key.
            pub fn fallback_key(key: &str) -> Option<&'static str> {
                let (section, field) = split_theme_key(key)?;
                match section {
                    $(
                        $section => match field {
                            $( $field_key => theme_color_keys!(@fallback $($fb)?), )*
                            _ => None,
                        },
                    )*
                    _ => None,
                }
            }

            /// Whether every theme without a base must name this key: a color
            /// key with no fallback.
            pub fn is_required_key(key: &str) -> bool {
                let Some((section, field)) = split_theme_key(key) else {
                    return false;
                };
                match section {
                    $(
                        $section => match field {
                            $( $field_key => theme_color_keys!(@required $kind $($fb)?), )*
                            _ => false,
                        },
                    )*
                    _ => false,
                }
            }
        }
    };

    // Per-field accessors. `color` fields are `Color`; `opt` fields are
    // `Option<Color>` and only resolve to a slot once set.
    (@get $self:ident, color $field:ident) => { Some($self.$field) };
    (@get $self:ident, opt $field:ident) => { $self.$field };
    (@get_mut $self:ident, color $field:ident) => { Some(&mut $self.$field) };
    (@get_mut $self:ident, opt $field:ident) => { $self.$field.as_mut() };

    // Modifier accessors. A row without `modifier <field>` contributes no
    // attribute; one with it resolves to that `Modifier` field.
    (@mod $self:ident) => { Modifier::empty() };
    (@mod $self:ident, $mod:ident) => { $self.$mod };
    (@mod_mut $self:ident) => { None };
    (@mod_mut $self:ident, $mod:ident) => { Some(&mut $self.$mod) };

    // Fallbacks. A `color` row without `fallback "<key>"` is required.
    (@fallback) => { None };
    (@fallback $fb:literal) => { Some($fb) };
    (@required color) => { true };
    (@required $kind:tt $($fb:literal)?) => { false };
}

theme_color_keys! {
    "editor" => {
        "after_eof_bg" => color after_eof_bg fallback "editor.bg",
        "bg" => color editor_bg,
        "current_line_bg" => color current_line_bg,
        "cursor" => color cursor,
        "diff_add_bg" => color diff_add_bg fallback "diagnostic.info_bg",
        "diff_add_collision_fg" => opt diff_add_collision_fg,
        "diff_add_highlight_bg" => color diff_add_highlight_bg fallback "editor.diff_add_bg",
        "diff_modify_bg" => color diff_modify_bg fallback "diagnostic.warning_bg",
        "diff_modify_collision_fg" => opt diff_modify_collision_fg,
        "diff_remove_bg" => color diff_remove_bg fallback "diagnostic.error_bg",
        "diff_remove_collision_fg" => opt diff_remove_collision_fg,
        "diff_remove_highlight_bg" => color diff_remove_highlight_bg fallback "editor.diff_remove_bg",
        "fg" => color editor_fg,
        "inactive_cursor" => color inactive_cursor fallback "editor.line_number_fg",
        "indentation_guide_fg" => color indentation_guide_fg fallback "editor.whitespace_indicator_fg",
        "indent_rainbow_1" => color indent_rainbow_1 fallback "editor.indentation_guide_fg",
        "indent_rainbow_2" => color indent_rainbow_2 fallback "editor.indentation_guide_fg",
        "indent_rainbow_3" => color indent_rainbow_3 fallback "editor.indentation_guide_fg",
        "indent_rainbow_4" => color indent_rainbow_4 fallback "editor.indentation_guide_fg",
        "indent_rainbow_5" => color indent_rainbow_5 fallback "editor.indentation_guide_fg",
        "indent_rainbow_6" => color indent_rainbow_6 fallback "editor.indentation_guide_fg",
        "line_number_bg" => color line_number_bg,
        "line_number_fg" => color line_number_fg,
        "ruler_bg" => color ruler_bg fallback "editor.current_line_bg",
        "selection_bg" => color selection_bg,
        "whitespace_indicator_fg" => color whitespace_indicator_fg fallback "editor.line_number_fg",
        "whitespace_indicator_selected_fg" => color whitespace_indicator_selected_fg fallback "editor.whitespace_indicator_fg",
        "bracket_match_fg" => color bracket_match_fg fallback "editor.cursor",
        "bracket_rainbow_1" => color bracket_rainbow_1 fallback "syntax.keyword",
        "bracket_rainbow_2" => color bracket_rainbow_2 fallback "syntax.function",
        "bracket_rainbow_3" => color bracket_rainbow_3 fallback "syntax.type",
        "bracket_rainbow_4" => color bracket_rainbow_4 fallback "syntax.string",
        "bracket_rainbow_5" => color bracket_rainbow_5 fallback "syntax.constant",
        "bracket_rainbow_6" => color bracket_rainbow_6 fallback "syntax.variable",
    },
    "ui" => {
        "blame_header_bg" => color blame_header_bg fallback "ui.menu_bg",
        "blame_header_fg" => color blame_header_fg fallback "ui.menu_fg",
        "compose_margin_bg" => color compose_margin_bg fallback "editor.after_eof_bg",
        "file_status_added_fg" => color file_status_added_fg fallback "diagnostic.info_fg",
        "file_status_conflicted_fg" => color file_status_conflicted_fg fallback "diagnostic.error_fg",
        "file_status_deleted_fg" => color file_status_deleted_fg fallback "diagnostic.error_fg",
        "file_status_modified_fg" => color file_status_modified_fg fallback "diagnostic.warning_fg",
        "file_status_renamed_fg" => color file_status_renamed_fg fallback "diagnostic.info_fg",
        "file_status_untracked_fg" => color file_status_untracked_fg fallback "diagnostic.hint_fg",
        "help_bg" => color help_bg,
        "help_fg" => color help_fg,
        "help_indicator_bg" => color help_indicator_bg,
        "help_indicator_fg" => color help_indicator_fg,
        "help_key_fg" => color help_key_fg,
        "help_separator_fg" => color help_separator_fg,
        "inline_code_bg" => color inline_code_bg fallback "editor.current_line_bg",
        "list_drop_target_bg" => color list_drop_target_bg fallback "ui.tab_drop_zone_bg",
        "menu_active_bg" => color menu_active_bg fallback "ui.popup_selection_bg",
        "menu_active_fg" => color menu_active_fg fallback "ui.popup_selection_fg",
        "menu_bg" => color menu_bg fallback "ui.tab_inactive_bg",
        "menu_border_fg" => color menu_border_fg fallback "ui.popup_border_fg",
        "menu_disabled_bg" => color menu_disabled_bg fallback "ui.menu_dropdown_bg",
        "menu_disabled_fg" => color menu_disabled_fg fallback "editor.line_number_fg",
        "menu_dropdown_bg" => color menu_dropdown_bg fallback "ui.popup_bg",
        "menu_dropdown_fg" => color menu_dropdown_fg fallback "ui.popup_text_fg",
        "menu_fg" => color menu_fg fallback "ui.tab_inactive_fg",
        "menu_highlight_bg" => color menu_highlight_bg fallback "ui.popup_selection_bg",
        "menu_highlight_fg" => color menu_highlight_fg fallback "ui.popup_selection_fg",
        "menu_hover_bg" => color menu_hover_bg fallback "ui.menu_highlight_bg",
        "menu_hover_fg" => color menu_hover_fg fallback "ui.menu_highlight_fg",
        "menu_separator_fg" => color menu_separator_fg fallback "ui.menu_border_fg",
        "popup_bg" => color popup_bg,
        "popup_border_fg" => color popup_border_fg,
        "popup_selection_bg" => color popup_selection_bg,
        "popup_selection_fg" => color popup_selection_fg fallback "ui.popup_text_fg",
        "popup_text_fg" => color popup_text_fg,
        "prompt_bg" => color prompt_bg,
        "prompt_fg" => color prompt_fg,
        "prompt_selection_bg" => color prompt_selection_bg,
        "prompt_selection_fg" => color prompt_selection_fg,
        "scrollbar_thumb_fg" => color scrollbar_thumb_fg fallback "editor.line_number_fg",
        "scrollbar_thumb_hover_fg" => color scrollbar_thumb_hover_fg fallback "ui.scrollbar_thumb_fg",
        "scrollbar_track_fg" => color scrollbar_track_fg fallback "editor.current_line_bg",
        "scrollbar_track_hover_fg" => color scrollbar_track_hover_fg fallback "ui.scrollbar_track_fg",
        "semantic_highlight_bg" => color semantic_highlight_bg fallback "editor.current_line_bg",
        "settings_selected_bg" => color settings_selected_bg fallback "ui.popup_selection_bg",
        "settings_selected_fg" => color settings_selected_fg fallback "ui.popup_selection_fg",
        "split_separator_fg" => color split_separator_fg,
        "split_separator_hover_fg" => color split_separator_hover_fg fallback "ui.split_separator_fg",
        "status_bar_bg" => color status_bar_bg,
        "status_bar_fg" => color status_bar_fg,
        "status_error_indicator_bg" => color status_error_indicator_bg fallback "diagnostic.error_bg",
        "status_error_indicator_fg" => color status_error_indicator_fg fallback "diagnostic.error_fg",
        "status_error_indicator_hover_bg" => color status_error_indicator_hover_bg fallback "ui.status_error_indicator_bg",
        "status_error_indicator_hover_fg" => color status_error_indicator_hover_fg fallback "ui.status_error_indicator_fg",
        "status_lsp_actionable_bg" => color status_lsp_actionable_bg fallback "ui.status_warning_indicator_bg",
        "status_lsp_actionable_fg" => color status_lsp_actionable_fg fallback "ui.status_warning_indicator_fg",
        "status_lsp_on_bg" => color status_lsp_on_bg fallback "ui.status_bar_bg",
        "status_lsp_on_fg" => color status_lsp_on_fg fallback "ui.status_bar_fg",
        "status_palette_bg" => color status_palette_bg fallback "ui.status_bar_bg",
        "status_palette_fg" => color status_palette_fg fallback "ui.status_bar_fg",
        "status_separator_bg" => color status_separator_bg fallback "ui.status_bar_bg",
        "status_separator_fg" => color status_separator_fg fallback "ui.status_bar_fg",
        "status_warning_indicator_bg" => color status_warning_indicator_bg fallback "diagnostic.warning_bg",
        "status_warning_indicator_fg" => color status_warning_indicator_fg fallback "diagnostic.warning_fg",
        "status_warning_indicator_hover_bg" => color status_warning_indicator_hover_bg fallback "ui.status_warning_indicator_bg",
        "status_warning_indicator_hover_fg" => color status_warning_indicator_hover_fg fallback "ui.status_warning_indicator_fg",
        "suggestion_bg" => color suggestion_bg,
        "suggestion_fg" => color suggestion_fg fallback "ui.popup_text_fg",
        "suggestion_selected_bg" => color suggestion_selected_bg,
        "tab_active_bg" => color tab_active_bg,
        "tab_active_fg" => color tab_active_fg,
        "tab_close_hover_fg" => color tab_close_hover_fg fallback "diagnostic.error_fg",
        "tab_drop_zone_bg" => color tab_drop_zone_bg fallback "editor.selection_bg",
        "tab_drop_zone_border" => color tab_drop_zone_border fallback "ui.tab_active_fg",
        "tab_hover_bg" => color tab_hover_bg fallback "ui.tab_inactive_bg",
        "tab_inactive_bg" => color tab_inactive_bg,
        "tab_inactive_fg" => color tab_inactive_fg,
        "tab_separator_bg" => color tab_separator_bg,
        "terminal_bg" => color terminal_bg fallback "editor.bg",
        "terminal_fg" => color terminal_fg fallback "editor.fg",
        "text_input_selection_bg" => color text_input_selection_bg fallback "ui.popup_selection_bg",
        "tour_step_bg" => color tour_step_bg fallback "ui.popup_selection_bg",
    },
    "syntax" => {
        "comment" => color syntax_comment modifier syntax_comment_modifier,
        "constant" => color syntax_constant modifier syntax_constant_modifier,
        "function" => color syntax_function modifier syntax_function_modifier,
        "keyword" => color syntax_keyword modifier syntax_keyword_modifier,
        "operator" => color syntax_operator modifier syntax_operator_modifier,
        "punctuation_bracket" => color syntax_punctuation_bracket modifier syntax_punctuation_bracket_modifier fallback "syntax.operator",
        "punctuation_delimiter" => color syntax_punctuation_delimiter modifier syntax_punctuation_delimiter_modifier fallback "syntax.operator",
        "string" => color syntax_string modifier syntax_string_modifier,
        "type" => color syntax_type modifier syntax_type_modifier,
        "variable" => color syntax_variable modifier syntax_variable_modifier,
        "variable_builtin" => color syntax_variable_builtin modifier syntax_variable_builtin_modifier fallback "syntax.keyword",
    },
    "diagnostic" => {
        "error_bg" => color diagnostic_error_bg,
        "error_fg" => color diagnostic_error_fg,
        "hint_bg" => color diagnostic_hint_bg,
        "hint_fg" => color diagnostic_hint_fg,
        "info_bg" => color diagnostic_info_bg,
        "info_fg" => color diagnostic_info_fg,
        "warning_bg" => color diagnostic_warning_bg,
        "warning_fg" => color diagnostic_warning_fg,
    },
    "search" => {
        "current_match_bg" => color search_current_match_bg fallback "editor.selection_bg",
        "current_match_fg" => color search_current_match_fg modifier search_current_match_modifier fallback "editor.fg",
        "label_bg" => color search_label_bg fallback "syntax.keyword",
        "label_fg" => color search_label_fg fallback "editor.bg",
        "match_bg" => color search_match_bg,
        "match_fg" => color search_match_fg,
    },
}

impl Theme {
    /// Return the independently configurable indentation-guide color for a
    /// zero-based nesting depth, cycling after the sixth level.
    pub fn indent_rainbow_color(&self, depth: usize) -> Color {
        match depth % 6 {
            0 => self.indent_rainbow_1,
            1 => self.indent_rainbow_2,
            2 => self.indent_rainbow_3,
            3 => self.indent_rainbow_4,
            4 => self.indent_rainbow_5,
            _ => self.indent_rainbow_6,
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
    fn syntax_modifiers_parse_and_map_to_theme_keys() {
        // A syntax key is either a bare color or a `{color, modifier}` bundle.
        let theme = Theme::from_json(
            r#"{
                "name": "syntax-attributes",
                "syntax": {
                    "keyword": { "color": [1, 2, 3], "modifier": ["bold", "underlined"] },
                    "comment": { "color": [4, 5, 6], "modifier": ["italic", "dim"] },
                    "string": [7, 8, 9]
                }
            }"#,
        )
        .unwrap();

        // The bundle sets both the color and its attributes.
        assert_eq!(theme.syntax_keyword, Color::Rgb(1, 2, 3));
        assert_eq!(
            theme.resolve_modifier_key("syntax.keyword"),
            Modifier::BOLD | Modifier::UNDERLINED
        );
        assert_eq!(theme.syntax_comment, Color::Rgb(4, 5, 6));
        assert_eq!(
            theme.resolve_modifier_key("syntax.comment"),
            Modifier::ITALIC | Modifier::DIM
        );
        // A bare color carries no attributes.
        assert_eq!(theme.syntax_string, Color::Rgb(7, 8, 9));
        assert!(theme.resolve_modifier_key("syntax.string").is_empty());
    }

    #[test]
    fn bare_color_override_clears_base_modifier() {
        // The value fully defines the style: overriding a keyword that the
        // base theme rendered bold with a bare color drops the bold.
        let theme = Theme::from_json(
            r#"{
                "name": "clears-modifier",
                "extends": "builtin://dark",
                "syntax": {
                    "keyword": { "color": [1, 2, 3], "modifier": ["bold"] }
                }
            }"#,
        )
        .unwrap();
        assert_eq!(theme.resolve_modifier_key("syntax.keyword"), Modifier::BOLD);

        let cleared = Theme::from_json(
            r#"{
                "name": "clears-modifier",
                "extends": "builtin://dark",
                "syntax": { "keyword": [1, 2, 3] }
            }"#,
        )
        .unwrap();
        assert!(cleared.resolve_modifier_key("syntax.keyword").is_empty());
    }

    #[test]
    fn current_search_match_style_comes_from_the_theme() {
        // Bundled themes name the keys, and draw the current match bold.
        let dark = Theme::load_builtin(THEME_DARK).unwrap();
        assert_eq!(dark.search_current_match_bg, Color::Rgb(64, 170, 230));
        assert_eq!(dark.search_current_match_modifier, Modifier::BOLD);
        assert_eq!(
            dark.resolve_modifier_key("search.current_match_fg"),
            Modifier::BOLD
        );

        // A theme can pick other attributes, or none with a bare color.
        let underlined = Theme::from_json(
            r#"{
                "name": "current-match-underlined",
                "extends": "builtin://dark",
                "search": {
                    "current_match_fg": { "color": [1, 2, 3], "modifier": ["underlined"] }
                }
            }"#,
        )
        .unwrap();
        assert_eq!(underlined.search_current_match_fg, Color::Rgb(1, 2, 3));
        assert_eq!(
            underlined.search_current_match_modifier,
            Modifier::UNDERLINED
        );

        let plain = Theme::from_json(
            r#"{
                "name": "current-match-plain",
                "extends": "builtin://dark",
                "search": { "current_match_fg": [1, 2, 3] }
            }"#,
        )
        .unwrap();
        assert!(plain.search_current_match_modifier.is_empty());
    }

    #[test]
    fn current_search_match_falls_back_to_the_selection() {
        let theme = standalone(serde_json::json!({
            "editor": { "selection_bg": [40, 50, 60], "fg": [10, 20, 30] }
        }));
        assert_eq!(theme.search_current_match_bg, Color::Rgb(40, 50, 60));
        assert_eq!(theme.search_current_match_fg, Color::Rgb(10, 20, 30));
        assert!(theme.search_current_match_modifier.is_empty());
    }

    /// A standalone theme: names every required key (all `[1, 1, 1]`), plus
    /// the keys in `extra` (`{"section": {"key": value}}`).
    fn standalone(extra: serde_json::Value) -> Theme {
        let mut raw = serde_json::json!({ "name": "standalone" });
        for key in Theme::COLOR_KEYS
            .iter()
            .filter(|k| Theme::is_required_key(k))
        {
            let (section, field) = split_theme_key(key).unwrap();
            raw[section][field] = serde_json::json!([1, 1, 1]);
        }
        for (section, keys) in extra.as_object().unwrap() {
            for (field, value) in keys.as_object().unwrap() {
                raw[section][field] = value.clone();
            }
        }
        Theme::from_json(&raw.to_string()).unwrap()
    }

    /// The required keys are the keys of the first theme file format; every
    /// other color key falls back, along a chain with no cycles, to one.
    #[test]
    fn every_fallback_chain_ends_at_a_required_key() {
        let required: Vec<_> = Theme::COLOR_KEYS
            .iter()
            .filter(|k| Theme::is_required_key(k))
            .collect();
        assert_eq!(required.len(), 49);
        for &key in Theme::COLOR_KEYS {
            let mut current = key;
            let mut steps = 0;
            while let Some(next) = Theme::fallback_key(current) {
                assert!(
                    Theme::COLOR_KEYS.contains(&next),
                    "{key}: unknown fallback {next}"
                );
                current = next;
                steps += 1;
                assert!(steps < Theme::COLOR_KEYS.len(), "{key}: fallback cycle");
            }
            let optional = Theme::fallback_key(key).is_none() && !Theme::is_required_key(key);
            assert!(
                optional || Theme::is_required_key(current),
                "{key}: chain ends at {current}, which is not required"
            );
        }
    }

    /// Built-in themes are standalone: each names every required key.
    #[test]
    fn builtin_themes_name_every_required_key() {
        for builtin in BUILTIN_THEMES {
            let raw: serde_json::Value = serde_json::from_str(builtin.json).unwrap();
            for key in Theme::COLOR_KEYS
                .iter()
                .filter(|k| Theme::is_required_key(k))
            {
                assert!(names_key(&raw, key), "{}: missing {key}", builtin.name);
            }
        }
    }

    /// A key a standalone theme leaves out follows its chain to the first key
    /// the theme names.
    #[test]
    fn standalone_theme_follows_the_fallback_chain() {
        // menu_hover_bg -> menu_highlight_bg -> popup_selection_bg (required).
        let theme = standalone(serde_json::json!({
            "ui": { "status_bar_fg": [7, 7, 7], "popup_selection_bg": [8, 8, 8] }
        }));
        assert_eq!(theme.status_separator_fg, Color::Rgb(7, 7, 7));
        assert_eq!(theme.menu_hover_bg, Color::Rgb(8, 8, 8));

        let named_midway = standalone(serde_json::json!({
            "ui": { "popup_selection_bg": [8, 8, 8], "menu_highlight_bg": [9, 9, 9] }
        }));
        assert_eq!(named_midway.menu_hover_bg, Color::Rgb(9, 9, 9));
    }

    /// A styled key that falls back to another styled key takes its text
    /// attributes along with its color.
    #[test]
    fn fallback_carries_text_attributes() {
        let theme = standalone(serde_json::json!({
            "syntax": { "keyword": { "color": [5, 5, 5], "modifier": ["bold"] } }
        }));
        assert_eq!(theme.syntax_variable_builtin, Color::Rgb(5, 5, 5));
        assert_eq!(theme.syntax_variable_builtin_modifier, Modifier::BOLD);
    }

    #[test]
    fn syntax_modifiers_survive_theme_round_trip() {
        let mut theme = Theme::load_builtin(THEME_DARK).unwrap();
        theme.syntax_function_modifier = Modifier::BOLD | Modifier::ITALIC;

        let file: ThemeFile = theme.into();
        let round_tripped: Theme = file.into();

        assert_eq!(
            round_tripped.syntax_function_modifier,
            Modifier::BOLD | Modifier::ITALIC
        );
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
        assert!(dark.syntax_keyword_modifier.is_empty());
        assert!(dark.syntax_comment_modifier.is_empty());
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

    /// With neither `extends` nor the required keys, the theme extends
    /// `builtin://dark` — no color comes from the code itself.
    #[test]
    fn test_no_inheritance_signal_extends_dark() {
        let theme = Theme::from_json(r#"{ "name": "x" }"#).expect("should parse");
        let dark = Theme::load_builtin(THEME_DARK).expect("dark builtin");
        assert_eq!(theme.editor_bg, dark.editor_bg);
        assert_eq!(theme.menu_bg, dark.menu_bg);
    }

    #[test]
    fn test_indentation_guide_fg_falls_back_to_whitespace_indicator_fg() {
        let theme = standalone(serde_json::json!({
            "editor": { "whitespace_indicator_fg": [12, 34, 56] }
        }));
        assert_eq!(theme.indentation_guide_fg, Color::Rgb(12, 34, 56));
    }

    /// Every built-in theme must give in-selection whitespace indicators a
    /// color that is actually distinguishable from the selection background —
    /// otherwise the marks vanish exactly where they are needed. Several
    /// themes use one color for both `selection_bg` and
    /// `whitespace_indicator_fg` (Dracula does), so those name a
    /// `whitespace_indicator_selected_fg` of their own.
    #[test]
    fn test_builtin_themes_have_a_visible_selected_indicator_color() {
        for builtin in BUILTIN_THEMES {
            let theme = Theme::load_builtin(builtin.name).expect("builtin theme loads");
            let Some(bg) = color_to_rgb(theme.selection_bg) else {
                // Terminal-palette selections have no RGB value to contrast
                // against; those fall back to the plain indicator color.
                assert_eq!(
                    theme.whitespace_indicator_selected_fg, theme.whitespace_indicator_fg,
                    "{}: non-RGB selection background falls back to the plain indicator color",
                    builtin.name
                );
                continue;
            };
            let fg = color_to_rgb(theme.whitespace_indicator_selected_fg)
                .unwrap_or_else(|| panic!("{}: derived indicator color is RGB", builtin.name));
            assert_ne!(
                fg, bg,
                "{}: selected whitespace indicators would be invisible on the selection",
                builtin.name
            );
        }
    }

    /// `extends` takes precedence over fallbacks: a key the theme leaves out is
    /// the base's, even when the theme restyles that key's fallback.
    #[test]
    fn test_extends_takes_precedence_over_fallbacks() {
        let theme = Theme::from_json(
            r#"{
                "name": "x",
                "extends": "builtin://dark",
                "editor": { "selection_bg": [20, 20, 20] },
                "ui": { "status_bar_fg": [1, 2, 3] }
            }"#,
        )
        .expect("should parse");
        let dark = Theme::load_builtin(THEME_DARK).unwrap();

        assert_eq!(theme.selection_bg, Color::Rgb(20, 20, 20));
        assert_eq!(
            theme.whitespace_indicator_selected_fg,
            dark.whitespace_indicator_selected_fg
        );
        assert_eq!(theme.status_bar_fg, Color::Rgb(1, 2, 3));
        assert_eq!(theme.status_separator_fg, dark.status_separator_fg);
        assert_eq!(theme.search_current_match_bg, dark.search_current_match_bg);
    }

    #[test]
    fn test_selected_indicator_fg_honours_an_explicit_theme_value() {
        let json = r#"{
            "name": "x",
            "extends": "builtin://dark",
            "editor": {
                "selection_bg": [20, 20, 20],
                "whitespace_indicator_selected_fg": [9, 8, 7]
            }
        }"#;
        let theme = Theme::from_json(json).expect("should parse");

        assert_eq!(theme.whitespace_indicator_selected_fg, Color::Rgb(9, 8, 7));
    }

    #[test]
    fn test_indent_rainbow_colors_are_independent_from_bracket_colors() {
        let json = r#"{
            "name": "independent-indent-rainbow",
            "editor": {
                "indent_rainbow_1": [1, 2, 3],
                "indent_rainbow_2": [4, 5, 6],
                "bracket_rainbow_1": [200, 201, 202]
            }
        }"#;
        let theme = Theme::from_json(json).expect("theme should parse");

        assert_eq!(theme.indent_rainbow_1, Color::Rgb(1, 2, 3));
        assert_eq!(theme.indent_rainbow_2, Color::Rgb(4, 5, 6));
        assert_eq!(theme.bracket_rainbow_1, Color::Rgb(200, 201, 202));
        assert_eq!(theme.indent_rainbow_color(6), Color::Rgb(1, 2, 3));
    }

    #[test]
    fn test_explicit_indentation_guide_fg_overrides_whitespace_fallback() {
        let json = r#"{
            "name": "x",
            "editor": {
                "whitespace_indicator_fg": [12, 34, 56],
                "indentation_guide_fg": [65, 67, 69]
            }
        }"#;
        let theme = Theme::from_json(json).expect("should parse");

        assert_eq!(theme.whitespace_indicator_fg, Color::Rgb(12, 34, 56));
        assert_eq!(theme.indentation_guide_fg, Color::Rgb(65, 67, 69));
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
        // A standalone theme with no file_status_* keys takes diagnostic colors.
        let theme = standalone(serde_json::json!({
            "diagnostic": {
                "error_fg": [220, 50, 47],
                "warning_fg": [181, 137, 0],
                "info_fg": [38, 139, 210],
                "hint_fg": [101, 123, 131]
            }
        }));

        // added/renamed -> info_fg
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
        // Explicit file_status keys win over the fallback.
        let theme = standalone(serde_json::json!({
            "ui": {
                "file_status_added_fg": [80, 250, 123],
                "file_status_modified_fg": [255, 184, 108]
            },
            "diagnostic": { "info_fg": [38, 139, 210], "warning_fg": [181, 137, 0] }
        }));

        assert_eq!(theme.file_status_added_fg, Color::Rgb(80, 250, 123));
        assert_eq!(theme.file_status_modified_fg, Color::Rgb(255, 184, 108));
        // Non-overridden still fall back
        assert_eq!(theme.file_status_renamed_fg, Color::Rgb(38, 139, 210));
    }

    #[test]
    fn test_file_status_colors_resolve_via_theme_key() {
        let theme = standalone(serde_json::json!({
            "ui": { "file_status_added_fg": [80, 250, 123] },
            "diagnostic": { "warning_fg": [181, 137, 0] }
        }));

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

    /// Relative luminance (WCAG 2.1) of an 8-bit RGB colour.
    ///
    /// Deliberately not named `relative_luminance`: this module already has
    /// one of those via `use super::*`, a plain linear weighting that
    /// `brightness` builds on, and a same-named helper here shadows it for
    /// the whole module rather than only its own callers. This one applies
    /// the sRGB gamma decode WCAG contrast requires, which is a different
    /// quantity — shadowing silently changed what `brightness` measured and
    /// broke the occurrence-highlight test (#3011) two screens away.
    fn wcag_relative_luminance(r: u8, g: u8, b: u8) -> f64 {
        fn channel(v: u8) -> f64 {
            let v = f64::from(v) / 255.0;
            if v <= 0.03928 {
                v / 12.92
            } else {
                ((v + 0.055) / 1.055).powf(2.4)
            }
        }
        0.2126 * channel(r) + 0.7152 * channel(g) + 0.0722 * channel(b)
    }

    /// WCAG contrast ratio between two colours, or `None` when either defers
    /// to the terminal palette (`terminal` names its colours, and what the
    /// emulator paints them is not knowable here).
    fn contrast_ratio(a: Color, b: Color) -> Option<f64> {
        let (Color::Rgb(ar, ag, ab), Color::Rgb(br, bg, bb)) = (a, b) else {
            return None;
        };
        let (la, lb) = (
            wcag_relative_luminance(ar, ag, ab),
            wcag_relative_luminance(br, bg, bb),
        );
        let (hi, lo) = if la > lb { (la, lb) } else { (lb, la) };
        Some((hi + 0.05) / (lo + 0.05))
    }

    /// A git-blame header band separates one commit's block from the next,
    /// so it must not be drawn on the editor's own background. The band used
    /// to borrow `ui.status_bar_bg`, which `dark` sets to exactly `editor.bg`
    /// and `high-contrast` sets a hair off black — the header was there and
    /// invisible.
    ///
    /// "Not equal" turned out to be far too weak a bar: `light` separated by
    /// a ratio of 1.17 and `dark`, `dracula` and `nord` by 1.45–1.56, all of
    /// which read as one flat surface. So the thresholds are contrast ratios,
    /// not inequality — the band has to be a step off the body, and its text
    /// has to clear WCAG AA against the band it sits on.
    ///
    /// The fallback (`ui.menu_bg` / `ui.menu_fg`) is an exact borrow, not a
    /// computed shade, so it cannot guarantee any of this on its own. Every
    /// shipped theme therefore names both keys itself, which this also pins.
    #[test]
    fn test_all_builtin_themes_draw_blame_headers_off_the_editor_background() {
        /// A visible step between the band and the code around it.
        const MIN_BAND_VS_BODY: f64 = 2.0;
        /// WCAG AA for normal-size text.
        const MIN_TEXT_VS_BAND: f64 = 4.5;

        for builtin in BUILTIN_THEMES {
            let raw: serde_json::Value = serde_json::from_str(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' is not valid JSON: {}", builtin.name, e));
            let ui = raw.get("ui").and_then(|u| u.as_object());
            for key in ["blame_header_bg", "blame_header_fg"] {
                assert!(
                    ui.is_some_and(|u| u.contains_key(key)),
                    "Theme '{}' must name `ui.{}` explicitly instead of leaning on the \
                     `ui.menu_*` fallback, which is an exact borrow and cannot promise a \
                     band that separates from this theme's editor background",
                    builtin.name,
                    key
                );
            }

            let theme = Theme::from_json(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' failed to parse: {}", builtin.name, e));

            if let Some(ratio) = contrast_ratio(theme.blame_header_bg, theme.editor_bg) {
                assert!(
                    ratio >= MIN_BAND_VS_BODY,
                    "Theme '{}': the git-blame header band contrasts with the editor \
                     background by only {:.2}, under the {:.2} needed for the band to read \
                     as separating one commit's block from the next",
                    builtin.name,
                    ratio,
                    MIN_BAND_VS_BODY
                );
            }

            if let Some(ratio) = contrast_ratio(theme.blame_header_fg, theme.blame_header_bg) {
                assert!(
                    ratio >= MIN_TEXT_VS_BAND,
                    "Theme '{}': the git-blame header text contrasts with its own band by \
                     only {:.2}, under the WCAG AA bar of {:.2}",
                    builtin.name,
                    ratio,
                    MIN_TEXT_VS_BAND
                );
            }
        }
    }

    /// A standalone theme that names neither blame-header key borrows the menu
    /// surface's colors *exactly* — the header is not tinted, shaded, or otherwise
    /// computed from another color, so what a theme author reads off
    /// `ui.menu_bg` is what the band is painted with.
    #[test]
    fn blame_header_falls_back_to_the_exact_menu_colors() {
        let theme = standalone(serde_json::json!({
            "ui": { "menu_bg": [11, 22, 33], "menu_fg": [44, 55, 66] }
        }));
        assert_eq!(theme.blame_header_bg, Color::Rgb(11, 22, 33));
        assert_eq!(theme.blame_header_fg, Color::Rgb(44, 55, 66));
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

    /// Regression for #2312: occurrence highlighting must use a
    /// theme-appropriate background in *every* shipped theme. A theme that
    /// omits `ui.semantic_highlight_bg` silently inherits the global default
    /// (a fixed dark color), which is invisible on dark high-contrast themes
    /// and an inverted block on light themes. Each builtin must therefore
    /// either define its own highlight color or opt into a modifier-only
    /// highlight (e.g. `reversed`/`bold`), and the resulting highlight must be
    /// visibly distinct from the editor background.
    #[test]
    fn test_all_builtin_themes_define_visible_occurrence_highlight() {
        for builtin in BUILTIN_THEMES {
            let raw: serde_json::Value = serde_json::from_str(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' is not valid JSON: {}", builtin.name, e));
            let ui = raw.get("ui").and_then(|u| u.as_object());
            let has_bg = ui.is_some_and(|u| u.contains_key("semantic_highlight_bg"));
            let has_modifier = ui.is_some_and(|u| u.contains_key("semantic_highlight_modifier"));
            assert!(
                has_bg || has_modifier,
                "Theme '{}' must explicitly define `ui.semantic_highlight_bg` (or a \
                 `semantic_highlight_modifier`) so occurrence highlighting is theme-appropriate \
                 instead of falling back to the hard-wired global default (#2312)",
                builtin.name
            );

            let theme = Theme::from_json(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' failed to parse: {}", builtin.name, e));
            // When the highlight relies purely on a color (no SGR modifier such
            // as reversed/bold to make it stand out), that color must differ
            // from the editor background, otherwise the highlight is invisible.
            if theme
                .modifier_for_bg_key("ui.semantic_highlight_bg")
                .is_empty()
            {
                assert_ne!(
                    theme.semantic_highlight_bg, theme.editor_bg,
                    "Theme '{}': occurrence-highlight background equals the editor background \
                     (highlight would be invisible) and no modifier compensates (#2312)",
                    builtin.name
                );
            }
        }
    }

    /// Perceived brightness (ITU-R BT.709 luma) on the 0..255 scale.
    ///
    /// The same weights [`relative_luminance`] uses for picking a light-vs-dark
    /// theme base, rescaled — one definition of "how bright is this", not two.
    fn brightness((r, g, b): (u8, u8, u8)) -> f64 {
        relative_luminance(r, g, b) * 255.0
    }

    /// Backgrounds closer than this in perceived brightness read as "the same
    /// tint" when they sit next to each other on screen.
    const MIN_BRIGHTNESS_DELTA: f64 = 20.0;

    /// Regression for #3011, and for the 256-color invisibility that predates
    /// it: the occurrence highlight must actually be *seen*.
    ///
    /// Two separations are required of every builtin, and neither is waivable
    /// by a modifier — a modifier is an extra cue, never a substitute for a
    /// visible background, and on terminals that render underlines faintly it
    /// is no cue at all:
    ///
    /// 1. against `editor.bg`, or the highlight is invisible outright;
    /// 2. against `editor.selection_bg`, or a selection and its highlighted
    ///    matches cannot be told apart while both are on screen.
    ///
    /// Both are checked **after 256-color quantization** as well as in
    /// truecolor, because quantization is where this actually broke for users:
    /// on master `high-contrast`'s highlight `[0,25,55]` and its `editor.bg`
    /// `[0,0,0]` both quantize to palette index 16, so on a plain
    /// `xterm-256color`/tmux session the highlight painted the background
    /// colour and vanished. `solarized-dark` did the same; `dracula` and `nord`
    /// came within 6.9 luma of it. `painted_rgb` models exactly what the
    /// terminal is sent, so the assertion sees what the user sees.
    #[test]
    fn test_builtin_highlight_is_visible_and_distinct() {
        use crate::color_support::{painted_rgb, ColorCapability};

        for builtin in BUILTIN_THEMES {
            let theme = Theme::from_json(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' failed to parse: {}", builtin.name, e));

            // A modifier the selection does not itself carry is the only way a
            // theme that owns no highlight colour at all (the terminal-palette
            // themes) can be distinguished — and even then only from the
            // selection, never from the editor background.
            let distinguishing_modifier = theme
                .modifier_for_bg_key("ui.semantic_highlight_bg")
                .difference(theme.selection_modifier);

            for capability in [ColorCapability::TrueColor, ColorCapability::Color256] {
                let Some(highlight) = painted_rgb(theme.semantic_highlight_bg, capability) else {
                    // No colour of its own: the modifier has to carry it.
                    assert!(
                        !distinguishing_modifier.is_empty(),
                        "Theme '{}': the occurrence highlight resolves to a terminal-palette \
                         colour and carries no modifier the selection lacks, so nothing marks \
                         it at all (#3011)",
                        builtin.name
                    );
                    continue;
                };

                for (name, other) in [
                    ("editor background", theme.editor_bg),
                    ("selection background", theme.selection_bg),
                ] {
                    let Some(other_rgb) = painted_rgb(other, capability) else {
                        continue;
                    };
                    let delta = (brightness(highlight) - brightness(other_rgb)).abs();
                    assert!(
                        delta >= MIN_BRIGHTNESS_DELTA,
                        "Theme '{}' at {:?}: the occurrence highlight {:?} is only {:.1} apart \
                         in perceived brightness from the {} {:?} (need {:.1}) — it has to be \
                         visible as a background in its own right, and no modifier substitutes \
                         for that (#3011)",
                        builtin.name,
                        capability,
                        highlight,
                        delta,
                        name,
                        other_rgb,
                        MIN_BRIGHTNESS_DELTA
                    );
                }
            }
        }
    }

    /// The counterpart to the visibility rule above: a highlight far enough
    /// from `editor.bg` to be seen necessarily sits where syntax foregrounds
    /// live, so some cells collide. `repaired_fg` repairs those, and this
    /// asserts the repair actually lands — every syntax colour of every
    /// builtin ends up clearing `MIN_CONTRAST_RATIO` over the highlight, either
    /// on its own or after repair.
    ///
    /// Without the repair this is unsatisfiable: an RGB-cube search over the
    /// values that meet the visibility rule tops out at 1.42:1 for dracula and
    /// 2.67:1 for solarized-dark, both below the 3.0:1 bar.
    #[test]
    fn test_builtin_highlight_never_erases_syntax_colors() {
        use crate::color_support::{
            contrast_ratio, painted_rgb, repaired_fg, ColorCapability, MIN_CONTRAST_RATIO,
        };

        for builtin in BUILTIN_THEMES {
            let theme = Theme::from_json(builtin.json)
                .unwrap_or_else(|e| panic!("Theme '{}' failed to parse: {}", builtin.name, e));

            let bg = theme.semantic_highlight_bg;
            let Some(bg_rgb) = painted_rgb(bg, ColorCapability::TrueColor) else {
                continue; // Terminal-palette highlight: the RGB isn't ours to know.
            };

            for (name, fg) in [
                ("editor.fg", theme.editor_fg),
                ("syntax.comment", theme.syntax_comment),
                ("syntax.constant", theme.syntax_constant),
                ("syntax.function", theme.syntax_function),
                ("syntax.keyword", theme.syntax_keyword),
                ("syntax.operator", theme.syntax_operator),
                ("syntax.string", theme.syntax_string),
                ("syntax.type", theme.syntax_type),
                ("syntax.variable", theme.syntax_variable),
                ("syntax.variable_builtin", theme.syntax_variable_builtin),
            ] {
                let painted = repaired_fg(fg, bg).unwrap_or(fg);
                let Some(fg_rgb) = painted_rgb(painted, ColorCapability::TrueColor) else {
                    continue;
                };
                let ratio = contrast_ratio(fg_rgb, bg_rgb);
                assert!(
                    ratio >= MIN_CONTRAST_RATIO,
                    "Theme '{}': {} ends up at {:?} over the occurrence highlight {:?}, a \
                     contrast ratio of {:.2}:1 (need {:.1}:1) — marking a word must not erase it",
                    builtin.name,
                    name,
                    fg_rgb,
                    bg_rgb,
                    ratio,
                    MIN_CONTRAST_RATIO
                );
            }
        }
    }
}
