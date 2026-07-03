//! Central glyph set for UI chrome (settings editor, file tree, popups,
//! prompts, dialog borders).
//!
//! Some terminals/fonts render the decorative Unicode glyphs the chrome uses
//! (`▶`, `→`, `╭╮╰╯`, `✓`, and especially the Nerd-Font private-use-area
//! category icons) as literal `?` or blank boxes — see issue #2032. Routing
//! every chrome glyph through this module lets the `editor.ascii_ui` config
//! flag swap the whole set for plain ASCII in one place.
//!
//! Scope is UI **chrome only**. Buffer text, whitespace indicators
//! (`·`, `→`) and indentation guides have their own dedicated config and are
//! intentionally left alone.

use ratatui::symbols::border;

/// A resolved set of chrome glyphs. Every field is `&'static str` so the two
/// concrete sets ([`UNICODE`] and [`ASCII`]) live as `static` values and
/// [`glyphs`] can hand out cheap `&'static` references.
#[derive(Debug, Clone, Copy)]
pub struct Glyphs {
    /// `true` for the ASCII set — lets callers branch on mode when composing
    /// multi-glyph strings.
    pub ascii: bool,

    // ── Disclosure / tree ────────────────────────────────────────────────
    /// Expanded node marker (`▼`).
    pub chevron_expanded: &'static str,
    /// Collapsed node marker (`▶`).
    pub chevron_collapsed: &'static str,
    /// Upward triangle used by dropdowns/scroll affordances (`▲`).
    pub triangle_up: &'static str,
    /// Downward triangle used by dropdowns (`▼`).
    pub triangle_down: &'static str,

    // ── Arrows ───────────────────────────────────────────────────────────
    pub arrow_right: &'static str,
    pub arrow_left: &'static str,
    pub arrow_up: &'static str,
    pub arrow_down: &'static str,

    // ── Indicators ───────────────────────────────────────────────────────
    /// Filled dot used for "modified"/selected markers (`●`).
    pub bullet: &'static str,
    /// Active/enabled toggle marker (`✓`).
    pub check: &'static str,
    /// Left edge accent bar for highlighted rows (`▎`).
    pub bar: &'static str,

    // ── Box drawing ──────────────────────────────────────────────────────
    pub h: &'static str,
    pub v: &'static str,
    pub corner_tl: &'static str,
    pub corner_tr: &'static str,
    pub corner_bl: &'static str,
    pub corner_br: &'static str,
    pub tee_right: &'static str,
    pub tee_left: &'static str,
}

impl Glyphs {
    /// The pure-ASCII border symbol set (`+`/`-`/`|`).
    fn ascii_border_set(&self) -> border::Set<'static> {
        border::Set {
            top_left: self.corner_tl,
            top_right: self.corner_tr,
            bottom_left: self.corner_bl,
            bottom_right: self.corner_br,
            vertical_left: self.v,
            vertical_right: self.v,
            horizontal_top: self.h,
            horizontal_bottom: self.h,
        }
    }

    /// Border symbol set for rounded-corner blocks. Replaces
    /// `.border_type(BorderType::Rounded)` so the flag reaches block borders;
    /// falls back to ASCII when `ascii_ui` is enabled.
    pub fn border_set(&self) -> border::Set<'static> {
        if self.ascii {
            self.ascii_border_set()
        } else {
            border::ROUNDED
        }
    }

    /// Border symbol set for plain (square-corner) blocks — the ratatui
    /// default. Identical to the default in Unicode mode (no visual change),
    /// ASCII when `ascii_ui` is enabled. Use on `Borders::ALL` blocks that
    /// don't set an explicit border type.
    pub fn plain_border_set(&self) -> border::Set<'static> {
        if self.ascii {
            self.ascii_border_set()
        } else {
            border::PLAIN
        }
    }

    /// Combined vertical scroll indicator (`↑`, `↓`, `↑↓`, or empty) for the
    /// given can-scroll-up / can-scroll-down state. Includes a leading space
    /// when non-empty, matching the previous inline call sites.
    pub fn scroll_indicator(&self, up: bool, down: bool) -> String {
        match (up, down) {
            (true, true) => format!(" {}{}", self.arrow_up, self.arrow_down),
            (true, false) => format!(" {}", self.arrow_up),
            (false, true) => format!(" {}", self.arrow_down),
            (false, false) => String::new(),
        }
    }

    /// Icon shown before a settings category name. Unicode mode uses Nerd-Font
    /// private-use-area icons (unchanged from before); ASCII mode uses a plain
    /// two-cell marker so alignment is preserved without needing a patched
    /// font.
    pub fn category_icon(&self, name: &str) -> &'static str {
        if self.ascii {
            return "- ";
        }
        match name.to_lowercase().as_str() {
            "general" => "\u{f013} ",       //
            "editor" => "\u{f044} ",        //
            "clipboard" => "\u{f328} ",     //
            "file browser" => "\u{f07b} ",  //
            "file explorer" => "\u{f07c} ", //
            "packages" => "\u{f487} ",      //
            "plugins" => "\u{f1e6} ",       //
            "terminal" => "\u{f120} ",      //
            "warnings" => "\u{f071} ",      //
            "keybindings" => "\u{f11c} ",   //
            _ => "\u{f111} ",               //  (dot circle as fallback)
        }
    }
}

/// The default (Unicode) chrome glyphs — current appearance, unchanged.
pub static UNICODE: Glyphs = Glyphs {
    ascii: false,
    chevron_expanded: "▼",
    chevron_collapsed: "▶",
    triangle_up: "▲",
    triangle_down: "▼",
    arrow_right: "→",
    arrow_left: "←",
    arrow_up: "↑",
    arrow_down: "↓",
    bullet: "●",
    check: "✓",
    bar: "▎",
    h: "─",
    v: "│",
    corner_tl: "╭",
    corner_tr: "╮",
    corner_bl: "╰",
    corner_br: "╯",
    tee_right: "├",
    tee_left: "┤",
};

/// The ASCII fallback chrome glyphs, used when `editor.ascii_ui` is enabled.
pub static ASCII: Glyphs = Glyphs {
    ascii: true,
    chevron_expanded: "v",
    chevron_collapsed: ">",
    triangle_up: "^",
    triangle_down: "v",
    arrow_right: "->",
    arrow_left: "<-",
    arrow_up: "^",
    arrow_down: "v",
    bullet: "*",
    check: "x",
    bar: "|",
    h: "-",
    v: "|",
    corner_tl: "+",
    corner_tr: "+",
    corner_bl: "+",
    corner_br: "+",
    tee_right: "+",
    tee_left: "+",
};

use std::cell::Cell;

thread_local! {
    /// Per-thread ASCII-mode flag. `set_ascii_ui` and every `glyphs()` read
    /// happen synchronously on one thread within a single `App::render` frame,
    /// so a thread-local is both correct for production and keeps tests that
    /// render on separate threads fully isolated from one another.
    static ASCII_MODE: Cell<bool> = const { Cell::new(false) };
}

/// Update the active glyph mode. Called once per frame from the render pass so
/// it always reflects the live config, no matter which path swapped the config.
pub fn set_ascii_ui(on: bool) {
    ASCII_MODE.with(|m| m.set(on));
}

/// The active chrome glyph set.
#[inline]
pub fn glyphs() -> &'static Glyphs {
    if ASCII_MODE.with(Cell::get) {
        &ASCII
    } else {
        &UNICODE
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascii_glyphs_are_pure_ascii() {
        let g = &ASCII;
        for s in [
            g.chevron_expanded,
            g.chevron_collapsed,
            g.triangle_up,
            g.triangle_down,
            g.arrow_right,
            g.arrow_left,
            g.arrow_up,
            g.arrow_down,
            g.bullet,
            g.check,
            g.bar,
            g.h,
            g.v,
            g.corner_tl,
            g.corner_tr,
            g.corner_bl,
            g.corner_br,
            g.tee_right,
            g.tee_left,
            g.category_icon("general"),
            g.category_icon("does-not-exist"),
        ] {
            assert!(s.is_ascii(), "expected ASCII, got {s:?}");
        }
        assert!(g.scroll_indicator(true, true).is_ascii());
    }

    #[test]
    fn mode_toggles_the_active_set() {
        set_ascii_ui(false);
        assert!(!glyphs().ascii);
        assert_eq!(glyphs().chevron_collapsed, "▶");
        set_ascii_ui(true);
        assert!(glyphs().ascii);
        assert_eq!(glyphs().chevron_collapsed, ">");
        // restore default so global state doesn't leak across tests
        set_ascii_ui(false);
    }

    #[test]
    fn scroll_indicator_matches_states() {
        let g = &UNICODE;
        assert_eq!(g.scroll_indicator(false, false), "");
        assert_eq!(g.scroll_indicator(true, false), " ↑");
        assert_eq!(g.scroll_indicator(false, true), " ↓");
        assert_eq!(g.scroll_indicator(true, true), " ↑↓");
    }

    #[test]
    fn ascii_border_set_is_ascii() {
        let set = ASCII.border_set();
        for s in [
            set.top_left,
            set.top_right,
            set.bottom_left,
            set.bottom_right,
            set.vertical_left,
            set.vertical_right,
            set.horizontal_top,
            set.horizontal_bottom,
        ] {
            assert!(s.is_ascii(), "expected ASCII border cell, got {s:?}");
        }
    }
}
