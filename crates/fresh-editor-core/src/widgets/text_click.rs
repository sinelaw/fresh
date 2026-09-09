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
/// the field's rendered row.
///
/// **The library reports where the press landed; this undoes the field's
/// layout.** `fresh_ui::Event::text_byte` gives the byte inside the piece the
/// gesture sits on, and the `focus` event's payload carries the breadcrumbs the
/// renderer stamped. Nothing here re-renders anything, which is the point: the
/// two Settings click paths used to render the whole control again and measure
/// the row they produced, because a *column* cannot be turned into a byte
/// without laying the text out. A byte can.
///
/// **One coordinate space, and it is the field's own.** `valueInnerStart` is
/// stamped by `kinds::text::single_line` against the row *that field* built,
/// and the container pass that composes two fields onto one line
/// (`kinds::containers`) shifts the `HitArea`'s byte range without shifting
/// the payload — so the value origin never moves and the caller must hand a
/// byte already measured from the field's own row start. A press on a
/// described field is one already: the field is its own node, and its piece
/// begins where the field does.
///
/// This used to take a `HitArea` and subtract `byte_start` from
/// `valueInnerStart`, which double-counted a composed row's offset. It could
/// not be observed — every producer of a text `focus` hit sets `byte_start`
/// to 0, and the composing pass runs only in the text projection, which does
/// not reach here — but the arithmetic disagreed with the other consumer of
/// the same payload (`Editor::reposition_widget_text_cursor_from_click`,
/// which rebases the click rather than the origin). A `WidgetEvent` cannot
/// state the disagreement.
///
/// `None` for an event with no layout payload — a non-text widget, or a text
/// field an older render path produced.
pub fn value_byte_from_hit(event: &super::WidgetEvent, byte_in_field: usize) -> Option<usize> {
    let field = |k: &str| {
        event
            .payload
            .get(k)
            .and_then(|v| v.as_u64())
            .map(|v| v as usize)
    };
    Some(row_byte_to_value_byte(
        byte_in_field,
        0,
        field("valueInnerStart")?,
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

    fn text_event(inner_start: usize, value_len: usize) -> crate::widgets::WidgetEvent {
        crate::widgets::WidgetEvent {
            widget_key: "field".into(),
            widget_kind: "text",
            payload: serde_json::json!({
                "valueInnerStart": inner_start,
                "valueDropped": 0,
                "ellipsisBytes": 0,
                "valueLen": value_len,
            }),
            event_type: "focus",
            owner_key: None,
            row_target: false,
            context_click: false,
        }
    }

    /// The press's byte, through the field's layout, is the value's byte.
    ///
    /// Row `"Name: [abcdef]"`: the value begins at row byte 7, so the press's
    /// byte inside the field maps straight through once the label is undone.
    #[test]
    fn value_byte_from_hit_undoes_the_label() {
        let ev = text_event(7, 6);
        assert_eq!(value_byte_from_hit(&ev, 0), Some(0)); // on the 'N' → clamp
        assert_eq!(value_byte_from_hit(&ev, 7), Some(0)); // the 'a'
        assert_eq!(value_byte_from_hit(&ev, 10), Some(3)); // the 'd'
        assert_eq!(value_byte_from_hit(&ev, 200), Some(6)); // past the end → clamp
    }

    /// **The case a column got wrong.** Row `"[中b]"`, value `"中b"` — `中` is
    /// three bytes and two cells, so the byte and the column part company at
    /// the second character. A press on the `b` is at column 3 and byte 4; if
    /// the column were passed here it would answer 2, in the middle of `中`.
    #[test]
    fn value_byte_from_hit_is_bytes_not_cells() {
        let ev = text_event(1, 4);
        assert_eq!(value_byte_from_hit(&ev, 1), Some(0), "the start of 中");
        assert_eq!(value_byte_from_hit(&ev, 4), Some(3), "the 'b' after it");
        assert_ne!(
            value_byte_from_hit(&ev, 3),
            Some(3),
            "byte 3 is not the 'b'; a column of 3 would have been"
        );
    }

    /// An event whose payload has no layout breadcrumbs is not a text field.
    #[test]
    fn value_byte_from_hit_is_absent_without_the_payload() {
        let mut ev = text_event(0, 0);
        ev.payload = serde_json::json!({});
        assert_eq!(value_byte_from_hit(&ev, 3), None);
    }

    /// **The value origin is the field's own row, and there is nothing else
    /// to rebase against.**
    ///
    /// Search and Replace share one line in the text projection, and the
    /// container pass shifts the composed `HitArea`'s byte range by the
    /// line-so-far — but not the payload it carries, whose `valueInnerStart`
    /// stays measured from the field's own text. So the press's byte must
    /// already be field-relative when it arrives, and this answers the same
    /// value byte whatever the field's offset in a composed row was. The
    /// version that took a `HitArea` subtracted `byte_start` from the origin
    /// as well and drifted by exactly that offset; a `WidgetEvent` has no
    /// `byte_start` to subtract.
    #[test]
    fn the_value_origin_is_the_field_s_own_row() {
        let ev = text_event(7, 6);
        assert_eq!(value_byte_from_hit(&ev, 7), Some(0));
        assert_eq!(value_byte_from_hit(&ev, 10), Some(3));
    }
}
