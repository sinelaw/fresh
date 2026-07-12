//! Mapping a mouse click on a rendered text widget to a caret position.
//!
//! Every text input the widget runtime draws — plugin panels *and* the
//! Settings controls, which render through the same [`render_spec`] path —
//! emits a `focus` [`HitArea`] whose payload carries the value-layout
//! breadcrumbs needed to turn a click into a byte offset in the field's
//! *value*:
//!
//! * `valueInnerStart` — byte where the value's `<inner>` region begins in
//!   the rendered row (after the gutter / label / `[`).
//! * `valueDropped` / `ellipsisBytes` — for a single-line field whose
//!   value is head-truncated to a `…`-prefixed tail view, the bytes hidden
//!   off the left and the width of the `…`.
//! * `valueLen` — the value's byte length, used to clamp.
//!
//! [`WidgetTextClickGeometry`] is a snapshot of that hit plus the row text
//! and the screen column the row was painted at — everything required to
//! answer "the user clicked screen column X; what value byte is that?".
//! The mounted-panel path reaches the same data live through
//! [`WidgetRegistry::hit_test`](super::WidgetRegistry::hit_test); surfaces
//! that render widgets *without* mounting them (the Settings UI, today)
//! stamp this geometry at render time and read it back on click. When
//! Settings controls are eventually mounted as real panels, this snapshot
//! and its stamping become redundant with the registry hit path.
//!
//! [`render_spec`]: super::render_spec

use super::RenderOutput;
use crate::primitives::display_width::grapheme_byte_at_visual_column;

/// Translate a byte offset into a rendered widget row back to a byte
/// offset into the field's *value*, undoing the field's layout: the
/// label/`[` prefix (`byte_start` + `inner_start`) and single-line
/// head-truncation (a `…`-prefixed tail view, `ellipsis`/`dropped`).
///
/// Shared by every click-to-position-cursor path — the mounted widget hit
/// handler and the Settings UI — so the truncation arithmetic lives in one
/// place.
pub fn row_byte_to_value_byte(
    row_byte: usize,
    byte_start: usize,
    inner_start: usize,
    dropped: usize,
    ellipsis: usize,
    value_len: usize,
) -> usize {
    let offset_in_field = row_byte.saturating_sub(byte_start);
    // A click left of the value (label / `[` / gutter) clamps to the
    // start; a click on the `…` ellipsis maps to the first visible byte;
    // a click past the last character clamps to end-of-value.
    let rel = offset_in_field.saturating_sub(inner_start);
    if ellipsis > 0 {
        if rel < ellipsis {
            dropped
        } else {
            dropped + (rel - ellipsis)
        }
    } else {
        rel
    }
    .min(value_len)
}

/// A snapshot of one rendered text field's geometry, sufficient to map a
/// screen-column click to a value byte after the fact. Built from the
/// widget [`RenderOutput`] the field was drawn from; see the module docs.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct WidgetTextClickGeometry {
    /// Screen column where the field's row was painted (its byte 0).
    pub origin_col: u16,
    /// The rendered row's text (label + `[` + visible value + `]`).
    pub row_text: String,
    /// `valueInnerStart` from the field's `focus` hit payload.
    pub inner_start: usize,
    /// `valueDropped` — bytes hidden off the left when head-truncated.
    pub dropped: usize,
    /// `ellipsisBytes` — width of the leading `…`, or 0 when not truncated.
    pub ellipsis: usize,
    /// `valueLen` — the value's byte length.
    pub value_len: usize,
}

impl WidgetTextClickGeometry {
    /// Build the geometry from a rendered field's [`RenderOutput`] and the
    /// screen column its row was painted at. Returns `None` when the output
    /// has no single-line text field (`focus` hit of kind `"text"`), e.g.
    /// a non-text control or an unfocusable field with an empty key.
    pub fn from_render_output(out: &RenderOutput, origin_col: u16) -> Option<Self> {
        let hit = out
            .hits
            .iter()
            .find(|h| h.widget_kind == "text" && h.event_type == "focus")?;
        let entry = out.entries.get(hit.buffer_row as usize)?;
        let row_text = entry.text.trim_end_matches('\n').to_string();
        let field = |k: &str| hit.payload.get(k).and_then(|v| v.as_u64()).unwrap_or(0) as usize;
        // The `focus` hit spans the whole row (`byte_start == 0`), so the
        // payload offsets are already row-relative; `byte_start` is folded
        // in here for uniformity with the mounted hit path.
        Some(Self {
            origin_col,
            row_text,
            inner_start: field("valueInnerStart").saturating_sub(hit.byte_start),
            dropped: field("valueDropped"),
            ellipsis: field("ellipsisBytes"),
            value_len: field("valueLen"),
        })
    }

    /// Map an absolute screen column to a byte offset in the field's value
    /// (grapheme-boundary aligned, clamped to the value). A click left of
    /// the value yields 0; a click past its end yields `value_len`.
    pub fn value_byte_at(&self, screen_col: u16) -> usize {
        let col_in_row = screen_col.saturating_sub(self.origin_col) as usize;
        let row_byte = grapheme_byte_at_visual_column(&self.row_text, col_in_row);
        row_byte_to_value_byte(
            row_byte,
            0,
            self.inner_start,
            self.dropped,
            self.ellipsis,
            self.value_len,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn row_byte_to_value_byte_untruncated() {
        // value "abcdef" begins 10 bytes into the row (label + `[`).
        assert_eq!(row_byte_to_value_byte(10, 0, 10, 0, 0, 6), 0); // at value start
        assert_eq!(row_byte_to_value_byte(13, 0, 10, 0, 0, 6), 3); // mid value
        assert_eq!(row_byte_to_value_byte(3, 0, 10, 0, 0, 6), 0); // in the label → clamp 0
        assert_eq!(row_byte_to_value_byte(99, 0, 10, 0, 0, 6), 6); // past end → value_len
    }

    #[test]
    fn row_byte_to_value_byte_truncated() {
        // Head-truncated `…tail`: inner_start=10, ellipsis=3 ("…"),
        // dropped=4 bytes hidden off the left, value_len=20.
        // Click on the ellipsis → first visible byte (== dropped).
        assert_eq!(row_byte_to_value_byte(10, 0, 10, 4, 3, 20), 4);
        assert_eq!(row_byte_to_value_byte(12, 0, 10, 4, 3, 20), 4); // still on `…`
                                                                    // First real char after `…` (row byte 13 = inner_start+ellipsis).
        assert_eq!(row_byte_to_value_byte(13, 0, 10, 4, 3, 20), 4);
        assert_eq!(row_byte_to_value_byte(15, 0, 10, 4, 3, 20), 6); // 2 past the tail start
    }

    #[test]
    fn value_byte_at_maps_screen_column() {
        // Row "Name: [abcdef]" painted at screen col 4. Value "abcdef"
        // begins at row byte 7 ("Name: [" == 7 chars, all ASCII).
        let g = WidgetTextClickGeometry {
            origin_col: 4,
            row_text: "Name: [abcdef]".to_string(),
            inner_start: 7,
            dropped: 0,
            ellipsis: 0,
            value_len: 6,
        };
        // Screen col 4 → row col 0 (the 'N') → left of value → 0.
        assert_eq!(g.value_byte_at(4), 0);
        // Screen col 11 → row col 7 → value byte 0 ('a').
        assert_eq!(g.value_byte_at(11), 0);
        // Screen col 14 → row col 10 → value byte 3 ('d').
        assert_eq!(g.value_byte_at(14), 3);
        // Far right → clamps to value_len.
        assert_eq!(g.value_byte_at(200), 6);
    }

    #[test]
    fn value_byte_at_wide_and_grapheme() {
        // Value "中b" (中 is 3 bytes / 2 cols) begins at row byte 1
        // (after "["), painted at origin 0. Row "[中b]".
        let g = WidgetTextClickGeometry {
            origin_col: 0,
            row_text: "[中b]".to_string(),
            inner_start: 1,
            dropped: 0,
            ellipsis: 0,
            value_len: 4, // 中(3) + b(1)
        };
        assert_eq!(g.value_byte_at(1), 0); // left half of 中 → its start
        assert_eq!(g.value_byte_at(2), 0); // right half of 中 → its start
        assert_eq!(g.value_byte_at(3), 3); // the 'b' (row col 3)
    }
}
