//! Wrap-configuration primitive.
//!
//! Historically this module held `wrap_line` (a char-width hard-wrap
//! implementation) and `char_position_to_segment` (a cursor →
//! wrapped-segment mapping).  Both were the scroll-math side of a
//! second wrap implementation that drifted from the renderer's
//! word-boundary wrap (`apply_wrapping_transform`).  They are gone.
//!
//! The surviving type here is [`WrapConfig`], used by a handful of
//! call sites to carry "first-line width + gutter + scrollbar"
//! geometry. Callers that need the full wrapped layout go through
//! [`crate::view::line_wrap_cache`], which caches the renderer's
//! own pipeline output per logical line — a single source of truth
//! shared by the renderer, scroll math, cursor navigation, and
//! scrollbar thumb sizing.
//!
//! See `docs/internal/line-wrap-cache-plan.md` for the design.

/// Configuration for line wrapping geometry.
///
/// Carries the widths that callers feed into the renderer's wrap.
/// The widths account for the gutter (line-number column) and
/// optionally a scrollbar column.  `content_area_width` is the total
/// area width *before* these deductions.
#[derive(Debug, Clone)]
pub struct WrapConfig {
    /// Width available for text on the first line
    /// (`content_area_width` - scrollbar - gutter).
    pub first_line_width: usize,
    /// Width available for text on continuation lines.  Same as
    /// `first_line_width`; continuation lines get visual indent via
    /// rendering, not reduced width.
    pub continuation_line_width: usize,
    /// Width of the line-number gutter.  Stored so callers can
    /// recover it (and derive `effective_width = first_line_width +
    /// gutter_width` for the renderer's wrap).
    pub gutter_width: usize,
    /// Whether continuation lines should visually align with the
    /// first line's leading whitespace (hanging indent).
    pub hanging_indent: bool,
}

impl WrapConfig {
    /// Create a new wrap configuration.
    ///
    /// # Arguments
    /// * `content_area_width` — total width before any deductions
    ///   (the area a buffer renders into, including gutter and
    ///   scrollbar columns).
    /// * `gutter_width` — line-number gutter column count.
    /// * `has_scrollbar` — whether to reserve one column for the
    ///   vertical scrollbar.
    /// * `hanging_indent` — whether continuation lines align to the
    ///   first line's leading whitespace.
    pub fn new(
        content_area_width: usize,
        gutter_width: usize,
        has_scrollbar: bool,
        hanging_indent: bool,
    ) -> Self {
        let scrollbar_width = usize::from(has_scrollbar);
        let text_area_width = content_area_width
            .saturating_sub(scrollbar_width)
            .saturating_sub(gutter_width);

        Self {
            first_line_width: text_area_width,
            continuation_line_width: text_area_width,
            gutter_width,
            hanging_indent,
        }
    }

    /// Create a "no wrap" configuration (infinite widths).
    pub fn no_wrap(gutter_width: usize) -> Self {
        Self {
            first_line_width: usize::MAX,
            continuation_line_width: usize::MAX,
            gutter_width,
            hanging_indent: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_subtracts_scrollbar_and_gutter() {
        let cfg = WrapConfig::new(100, 6, true, false);
        assert_eq!(cfg.first_line_width, 100 - 1 - 6);
        assert_eq!(cfg.continuation_line_width, 100 - 1 - 6);
        assert_eq!(cfg.gutter_width, 6);
        assert!(!cfg.hanging_indent);
    }

    #[test]
    fn new_without_scrollbar_omits_its_column() {
        let cfg = WrapConfig::new(100, 6, false, true);
        assert_eq!(cfg.first_line_width, 100 - 6);
        assert!(cfg.hanging_indent);
    }

    #[test]
    fn new_clamps_to_zero_on_oversize_deductions() {
        // Saturating — no underflow.
        let cfg = WrapConfig::new(3, 6, true, false);
        assert_eq!(cfg.first_line_width, 0);
    }

    #[test]
    fn no_wrap_returns_max_widths() {
        let cfg = WrapConfig::no_wrap(6);
        assert_eq!(cfg.first_line_width, usize::MAX);
        assert_eq!(cfg.continuation_line_width, usize::MAX);
    }
}
