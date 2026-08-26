/// Lightweight per-cell theme key provenance recorded during rendering.
/// Stored in `ChromeLayout::cell_theme_map` so the theme inspector popup
/// can look up the exact keys used for any screen position.
///
/// Keys are `Cow<'static, str>` so the hot editor/chrome paths store cheap
/// borrowed `&'static str` literals while plugin-driven surfaces (the
/// orchestrator dock) can record the runtime key strings their text
/// properties carry.
#[derive(Debug, Clone, Default)]
pub struct CellThemeInfo {
    /// Foreground theme key (e.g. "syntax.keyword", "editor.fg")
    pub fg_key: Option<std::borrow::Cow<'static, str>>,
    /// Background theme key (e.g. "editor.bg", "diagnostic.warning_bg")
    pub bg_key: Option<std::borrow::Cow<'static, str>>,
    /// Short region label (e.g. "Line Numbers", "Editor Content")
    pub region: std::borrow::Cow<'static, str>,
    /// Dynamic region suffix (e.g. syntax category display name appended to "Syntax: ")
    pub syntax_category: Option<std::borrow::Cow<'static, str>>,
}

/// One horizontal run of cells a chrome renderer painted with a known set of
/// theme keys, captured *as it paints*. Renderers collect these into a fresh
/// `Vec` (sidestepping any borrow of the per-cell map / the window) and the
/// caller applies them via [`ChromeLayout::apply_theme_runs`] once its own
/// borrows are released. Keys are `&'static str` (all chrome keys are literals).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ThemeRun {
    pub x: u16,
    pub y: u16,
    pub w: u16,
    pub fg_key: Option<&'static str>,
    pub bg_key: Option<&'static str>,
    pub region: &'static str,
}

/// Write theme-key runs into a flat per-cell map of the given `width` (rows
/// indexed `row * width + col`). Cells outside the frame are skipped. Used by
/// both [`super::layout::ChromeLayout::apply_theme_runs`] and the split
/// rendering pipeline, which holds the map directly.
pub fn apply_theme_runs(map: &mut [CellThemeInfo], width: u16, runs: &[ThemeRun]) {
    use std::borrow::Cow;
    if width == 0 {
        return;
    }
    let stride = width as usize;
    for r in runs {
        for col in r.x..r.x.saturating_add(r.w) {
            if col >= width {
                break;
            }
            let idx = r.y as usize * stride + col as usize;
            if let Some(cell) = map.get_mut(idx) {
                cell.fg_key = r.fg_key.map(Cow::Borrowed);
                cell.bg_key = r.bg_key.map(Cow::Borrowed);
                cell.region = Cow::Borrowed(r.region);
                cell.syntax_category = None;
            }
        }
    }
}

/// Collects [`ThemeRun`]s during a chrome region's paint. Threaded as
/// `Option<&mut CellThemeRecorder>` so recording is opt-in (the inspector
/// wants it; offscreen/test renders pass `None`).
pub struct CellThemeRecorder<'a> {
    runs: &'a mut Vec<ThemeRun>,
}

impl<'a> CellThemeRecorder<'a> {
    pub fn new(runs: &'a mut Vec<ThemeRun>) -> Self {
        Self { runs }
    }

    /// Record a horizontal run of `w` cells starting at screen `(x, y)`.
    pub fn run(
        &mut self,
        x: u16,
        y: u16,
        w: u16,
        fg_key: Option<&'static str>,
        bg_key: Option<&'static str>,
        region: &'static str,
    ) {
        if w == 0 {
            return;
        }
        self.runs.push(ThemeRun {
            x,
            y,
            w,
            fg_key,
            bg_key,
            region,
        });
    }
}

/// Information about which theme key(s) style a specific screen position.
/// Used by the Ctrl+Right-Click theme inspector popup.
#[derive(Debug, Clone)]
pub struct ThemeKeyInfo {
    /// The foreground theme key path (e.g., "syntax.keyword", "editor.fg")
    pub fg_key: Option<String>,
    /// The background theme key path (e.g., "editor.bg", "editor.selection_bg")
    pub bg_key: Option<String>,
    /// Human-readable description of the UI region
    pub region: String,
    /// The actual foreground color value currently applied
    pub fg_color: Option<ratatui::style::Color>,
    /// The actual background color value currently applied
    pub bg_color: Option<ratatui::style::Color>,
    /// For syntax highlights: the HighlightCategory display name
    pub syntax_category: Option<String>,
}

/// State for the theme inspector popup (Ctrl+Right-Click). The
/// button-hover highlight is not state here — paint derives it from
/// `HoverTarget::ThemeInfoButton` (the chrome hover walk's target).
#[derive(Debug, Clone)]
pub struct ThemeInfoPopup {
    /// Screen position where popup appears (x, y)
    pub position: (u16, u16),
    /// Resolved theme key information
    pub info: ThemeKeyInfo,
}
