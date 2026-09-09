//! A single-line overlay that follows a buffer's cursor.
//!
//! A plugin that wants a magit-style "you are here" bar used to paint it
//! itself, from the `cursor_moved` hook: the host moved the cursor, fired
//! the hook, and painted the frame; the plugin's `addOverlay` only landed
//! on the *next* one. Holding an arrow key then showed the bar one row
//! behind the caret for as long as the key repeated — the highlight can
//! never catch up, because every frame it is answering the previous
//! frame's cursor.
//!
//! Declaring the bar once and letting the host place it removes the round
//! trip entirely: the range is derived from the cursor at paint time, in
//! the same pass that draws the caret, so the two cannot disagree.
//!
//! The plugin still owns the *appearance* — it hands over the same
//! `OverlayOptions` it would have passed to `addOverlay`, so the bar looks
//! exactly as it did when the plugin painted it by hand.

use crate::model::buffer::Buffer;
use crate::model::marker::MarkerList;
use crate::view::overlay::{Overlay, OverlayFace, OverlayManager, OverlayNamespace};
use fresh_core::api::OverlayOptions;
use std::ops::Range;

/// Namespace owning the cursor-line overlay. Host-managed: a plugin never
/// adds to or clears this namespace itself.
pub fn cursor_line_namespace() -> OverlayNamespace {
    OverlayNamespace::from_string("cursor-line".to_string())
}

/// Per-buffer state for the cursor-following line overlay.
#[derive(Debug, Clone, Default)]
pub struct CursorLineOverlay {
    /// Appearance the owning plugin asked for. `None` disables the overlay.
    spec: Option<OverlayOptions>,
    /// Byte range currently painted, so an update that would repaint the
    /// same row does nothing (the common case: any cursor move within a
    /// line, and every frame that redraws without one).
    painted: Option<Range<usize>>,
    /// Set when the spec changed, so the next update repaints even onto
    /// the row already painted — the colours under it may be different.
    spec_dirty: bool,
}

impl CursorLineOverlay {
    pub fn new() -> Self {
        Self::default()
    }

    /// Declare (or, with `None`, withdraw) the bar. The next update paints
    /// it; the paint itself is deferred so this stays cheap to call from a
    /// command handler that has no buffer to work with yet.
    pub fn set_spec(&mut self, spec: Option<OverlayOptions>) {
        self.spec = spec;
        self.spec_dirty = true;
    }

    pub fn is_enabled(&self) -> bool {
        self.spec.is_some()
    }

    /// Re-place the bar on the line holding `cursor_position`. Returns
    /// whether anything changed.
    pub fn update(
        &mut self,
        buffer: &Buffer,
        overlays: &mut OverlayManager,
        marker_list: &mut MarkerList,
        cursor_position: usize,
    ) -> bool {
        let ns = cursor_line_namespace();
        self.spec_dirty &= self.spec.is_some();
        let Some(spec) = self.spec.clone() else {
            if self.painted.take().is_some() {
                overlays.clear_namespace(&ns, marker_list);
                return true;
            }
            return false;
        };

        let Some(range) = cursor_line_range(buffer, cursor_position) else {
            if self.painted.take().is_some() {
                overlays.clear_namespace(&ns, marker_list);
                return true;
            }
            return false;
        };

        if self.painted.as_ref() == Some(&range) && !self.spec_dirty {
            return false;
        }
        self.spec_dirty = false;

        overlays.clear_namespace(&ns, marker_list);
        let face = OverlayFace::from_options(&spec);
        let overlay = Overlay::with_namespace(marker_list, range.clone(), face, ns)
            // Same priority `addOverlay` gives a plugin's own overlays, so
            // stacking against the rest of the buffer's decoration is
            // unchanged from when the plugin painted this itself.
            .with_priority_value(10)
            .with_extend_to_line_end(spec.extend_to_line_end);
        overlays.add(overlay);
        self.painted = Some(range);
        true
    }
}

/// Byte range of the line holding `position`, newline included so the bar
/// reaches the row's end. `None` for an empty buffer, where there is no
/// row to paint.
fn cursor_line_range(buffer: &Buffer, position: usize) -> Option<Range<usize>> {
    if buffer.is_empty() {
        return None;
    }
    let position = position.min(buffer.len());
    let line = buffer.get_line_number(position);
    let start = buffer.line_start_offset(line)?;
    let end = buffer
        .line_start_offset(line + 1)
        .unwrap_or_else(|| buffer.len());
    if end <= start {
        return None;
    }
    Some(start..end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_core::api::OverlayColorSpec;

    fn spec() -> OverlayOptions {
        OverlayOptions {
            bg: Some(OverlayColorSpec::Rgb(1, 2, 3)),
            extend_to_line_end: true,
            ..Default::default()
        }
    }

    fn buffer() -> Buffer {
        Buffer::from_str_test("alpha\nbeta\ngamma\n")
    }

    #[test]
    fn paints_the_line_holding_the_cursor() {
        let buf = buffer();
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut cl = CursorLineOverlay::new();
        cl.set_spec(Some(spec()));

        assert!(cl.update(&buf, &mut overlays, &mut markers, 8));
        assert_eq!(overlays.all().len(), 1, "one bar, on one line");
        // "beta\n" is bytes 6..11.
        assert_eq!(cl.painted, Some(6..11));
    }

    #[test]
    fn moving_within_a_line_does_not_repaint() {
        let buf = buffer();
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut cl = CursorLineOverlay::new();
        cl.set_spec(Some(spec()));

        assert!(cl.update(&buf, &mut overlays, &mut markers, 6));
        assert!(!cl.update(&buf, &mut overlays, &mut markers, 9));
        assert_eq!(overlays.all().len(), 1);
    }

    #[test]
    fn moving_to_another_line_moves_the_bar() {
        let buf = buffer();
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut cl = CursorLineOverlay::new();
        cl.set_spec(Some(spec()));

        cl.update(&buf, &mut overlays, &mut markers, 0);
        assert_eq!(cl.painted, Some(0..6));
        assert!(cl.update(&buf, &mut overlays, &mut markers, 12));
        assert_eq!(cl.painted, Some(11..17));
        assert_eq!(overlays.all().len(), 1, "the old bar is gone");
    }

    #[test]
    fn withdrawing_the_spec_clears_the_bar() {
        let buf = buffer();
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut cl = CursorLineOverlay::new();
        cl.set_spec(Some(spec()));
        cl.update(&buf, &mut overlays, &mut markers, 0);

        cl.set_spec(None);
        assert!(cl.update(&buf, &mut overlays, &mut markers, 0));
        assert_eq!(overlays.all().len(), 0);
    }

    #[test]
    fn an_empty_buffer_has_no_line_to_paint() {
        let buf = Buffer::from_str_test("");
        let mut overlays = OverlayManager::new();
        let mut markers = MarkerList::new();
        let mut cl = CursorLineOverlay::new();
        cl.set_spec(Some(spec()));

        assert!(!cl.update(&buf, &mut overlays, &mut markers, 0));
        assert_eq!(overlays.all().len(), 0);
    }
}
