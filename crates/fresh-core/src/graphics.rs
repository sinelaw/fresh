//! Terminal raster-graphics capability detection.
//!
//! Shared by the editor (which gates inline-image placement on it) and the
//! plugin runtime (which exposes it to plugins via
//! `editor.getGraphicsCapability()`), so both sides always agree on what the
//! terminal supports. Detection is purely environment-based.

/// Terminal raster-graphics capability.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GraphicsCapability {
    /// No known raster-graphics support; images fall back to text.
    None,
    /// Kitty graphics protocol (kitty, WezTerm, Ghostty, recent Konsole).
    Kitty,
}

impl GraphicsCapability {
    /// Detect graphics capability from the environment. Override with the
    /// `FRESH_GRAPHICS` env var (`kitty` / `none`).
    pub fn detect() -> Self {
        Self::detect_from(|name| std::env::var(name).ok())
    }

    /// Detection core, parameterized over an environment lookup so it can be
    /// unit-tested without touching the process environment.
    pub fn detect_from(get: impl Fn(&str) -> Option<String>) -> Self {
        if let Some(v) = get("FRESH_GRAPHICS") {
            match v.to_lowercase().as_str() {
                "kitty" | "on" | "1" | "true" => return GraphicsCapability::Kitty,
                "none" | "off" | "0" | "false" => return GraphicsCapability::None,
                _ => {}
            }
        }

        // kitty sets KITTY_WINDOW_ID for every window.
        if get("KITTY_WINDOW_ID").is_some() {
            return GraphicsCapability::Kitty;
        }

        // WezTerm / Ghostty advertise themselves and both speak the kitty
        // graphics protocol.
        if let Some(tp) = get("TERM_PROGRAM") {
            let t = tp.to_lowercase();
            if t.contains("wezterm") || t.contains("ghostty") {
                return GraphicsCapability::Kitty;
            }
        }
        if get("GHOSTTY_RESOURCES_DIR").is_some() || get("GHOSTTY_BIN_DIR").is_some() {
            return GraphicsCapability::Kitty;
        }

        if let Some(term) = get("TERM") {
            let t = term.to_lowercase();
            if t.contains("kitty") || t.contains("ghostty") || t.contains("wezterm") {
                return GraphicsCapability::Kitty;
            }
        }

        GraphicsCapability::None
    }

    pub fn supports_images(self) -> bool {
        matches!(self, GraphicsCapability::Kitty)
    }

    /// Stable string form exposed to plugins (`"kitty"` / `"none"`).
    pub fn as_str(self) -> &'static str {
        match self {
            GraphicsCapability::None => "none",
            GraphicsCapability::Kitty => "kitty",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_respects_override() {
        let kitty = GraphicsCapability::detect_from(|n| {
            if n == "FRESH_GRAPHICS" {
                Some("kitty".to_string())
            } else {
                None
            }
        });
        assert_eq!(kitty, GraphicsCapability::Kitty);

        let off = GraphicsCapability::detect_from(|n| {
            if n == "FRESH_GRAPHICS" {
                Some("none".to_string())
            } else {
                None
            }
        });
        assert_eq!(off, GraphicsCapability::None);
    }

    #[test]
    fn detect_recognizes_kitty_and_wezterm() {
        let kitty = GraphicsCapability::detect_from(|n| {
            if n == "KITTY_WINDOW_ID" {
                Some("1".to_string())
            } else {
                None
            }
        });
        assert_eq!(kitty, GraphicsCapability::Kitty);

        let wez = GraphicsCapability::detect_from(|n| {
            if n == "TERM_PROGRAM" {
                Some("WezTerm".to_string())
            } else {
                None
            }
        });
        assert_eq!(wez, GraphicsCapability::Kitty);

        let plain = GraphicsCapability::detect_from(|_| None);
        assert_eq!(plain, GraphicsCapability::None);
    }

    #[test]
    fn as_str_round_trips() {
        assert_eq!(GraphicsCapability::Kitty.as_str(), "kitty");
        assert_eq!(GraphicsCapability::None.as_str(), "none");
    }
}
