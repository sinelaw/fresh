//! Mapping a press on a rendered text widget to a caret position.
//!
//! Every text input the widget runtime draws — plugin panels *and* the
//! Settings controls, which render through the same [`render_spec`] path —
//! emits a `focus` [`HitArea`] whose payload carries the value-layout
//! breadcrumbs needed to turn a byte in the rendered row into a byte in the
//! field's *value*:
//!
//! * `valueInnerStart` — byte where the value's `<inner>` region begins in
//!   the rendered row (after the gutter / label / `[`).
//! * `valueDropped` / `ellipsisBytes` — for a single-line field whose
//!   value is head-truncated to a `…`-prefixed tail view, the bytes hidden
//!   off the left and the width of the `…`.
//! * `valueLen` — the value's byte length, used to clamp.
//!
//! **What used to be here was a snapshot, and it was here because the press
//! arrived as a column.** `WidgetTextClickGeometry` held a rendered row and
//! the screen column it was painted at, so that "the user clicked column X"
//! could be answered later — and the two Settings click paths built one by
//! rendering the control a second time, on the click, at a width read back
//! off the tree. None of that was avoidable while a column was all the press
//! carried: turning one into a byte means knowing where every grapheme
//! landed, which means laying the text out.
//!
//! `fresh_ui::Event::text_byte` reports the byte, from the shaping that drew
//! the row. So the snapshot is gone and what is left is the arithmetic that
//! was always the real work: undo the field's own layout.
//!
//! [`render_spec`]: super::render_spec

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

/// The byte in a field's *value* under a press, from the press's own byte in
/// the rendered row.
///
/// **The library reports where the press landed; this undoes the field's
/// layout.** `fresh_ui::Event::text_byte` gives the byte inside the piece the
/// gesture sits on, `HitArea::byte_start` says where that piece begins in the
/// entry's row, and the `focus` hit's payload carries the breadcrumbs the
/// renderer stamped. Nothing here re-renders anything, which is the point: the
/// two Settings click paths used to render the whole control again and measure
/// the row they produced, because a *column* cannot be turned into a byte
/// without laying the text out. A byte can.
///
/// `None` for a hit with no layout payload — a non-text widget, or a text
/// field an older render path produced.
pub fn value_byte_from_hit(hit: &super::HitArea, byte_in_piece: usize) -> Option<usize> {
    let field = |k: &str| {
        hit.payload
            .get(k)
            .and_then(|v| v.as_u64())
            .map(|v| v as usize)
    };
    let inner_start = field("valueInnerStart")?;
    let row_byte = hit.byte_start.saturating_add(byte_in_piece);
    Some(row_byte_to_value_byte(
        row_byte,
        hit.byte_start,
        inner_start.saturating_sub(hit.byte_start),
        field("valueDropped").unwrap_or(0),
        field("ellipsisBytes").unwrap_or(0),
        field("valueLen").unwrap_or(0),
    ))
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

    fn text_hit(
        byte_start: usize,
        inner_start: usize,
        value_len: usize,
    ) -> crate::widgets::HitArea {
        crate::widgets::HitArea {
            widget_key: "field".into(),
            widget_kind: "text",
            buffer_row: 0,
            byte_start,
            byte_end: byte_start + 64,
            payload: serde_json::json!({
                "valueInnerStart": inner_start,
                "valueDropped": 0,
                "ellipsisBytes": 0,
                "valueLen": value_len,
            }),
            event_type: "focus",
            owner_key: None,
            overlay: false,
            row_target: false,
            context_click: false,
        }
    }

    /// The press's byte, through the field's layout, is the value's byte.
    ///
    /// Row `"Name: [abcdef]"`: the value begins at row byte 7, so the press's
    /// byte inside the piece maps straight through once the label is undone.
    #[test]
    fn value_byte_from_hit_undoes_the_label() {
        let hit = text_hit(0, 7, 6);
        assert_eq!(value_byte_from_hit(&hit, 0), Some(0)); // on the 'N' → clamp
        assert_eq!(value_byte_from_hit(&hit, 7), Some(0)); // the 'a'
        assert_eq!(value_byte_from_hit(&hit, 10), Some(3)); // the 'd'
        assert_eq!(value_byte_from_hit(&hit, 200), Some(6)); // past the end → clamp
    }

    /// **The case a column got wrong.** Row `"[中b]"`, value `"中b"` — `中` is
    /// three bytes and two cells, so the byte and the column part company at
    /// the second character. A press on the `b` is at column 3 and byte 4; if
    /// the column were passed here it would answer 2, in the middle of `中`.
    #[test]
    fn value_byte_from_hit_is_bytes_not_cells() {
        let hit = text_hit(0, 1, 4);
        assert_eq!(value_byte_from_hit(&hit, 1), Some(0), "the start of 中");
        assert_eq!(value_byte_from_hit(&hit, 4), Some(3), "the 'b' after it");
        assert_ne!(
            value_byte_from_hit(&hit, 3),
            Some(3),
            "byte 3 is not the 'b'; a column of 3 would have been"
        );
    }

    /// A hit whose payload has no layout breadcrumbs is not a text field.
    #[test]
    fn value_byte_from_hit_is_absent_without_the_payload() {
        let mut hit = text_hit(0, 0, 0);
        hit.payload = serde_json::json!({});
        assert_eq!(value_byte_from_hit(&hit, 3), None);
    }

    /// The field's own offset within a composed row is rebased away — Search
    /// and Replace share one line, so `byte_start` is not always zero.
    #[test]
    fn value_byte_from_hit_rebases_a_composed_row() {
        // The field starts 20 bytes into the row; its value 7 bytes further.
        let hit = text_hit(20, 27, 6);
        assert_eq!(value_byte_from_hit(&hit, 7), Some(0));
        assert_eq!(value_byte_from_hit(&hit, 10), Some(3));
    }
}
