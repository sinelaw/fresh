//! Pure decision logic for the Home key on soft-wrapped lines.
//!
//! `smart_home_target` is a pure function of the cursor position, the
//! current visual-row boundary, and the first non-whitespace offset on
//! the row. The caller on `Editor` does the layout lookups and line-
//! content scan, then calls this function to decide where the cursor
//! should move. The caller handles the `PreviousVisualRowStart` case by
//! issuing another layout query — this module deliberately does not know
//! about the cached layout.

/// Where the Home key should move the cursor on the current visual row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SmartHomeTarget {
    /// Move to this exact byte offset.
    At(usize),
    /// The cursor was already at the visual-row start of a wrapped
    /// continuation row; the caller must look up and move to the start of
    /// the *previous* visual row.
    PreviousVisualRowStart,
}

/// Compute what Home should do on the current visual row.
///
/// On the **first** visual row of a physical line the cursor toggles
/// between the first non-whitespace character and position 0 (standard
/// smart-home).
///
/// On a **continuation** (wrapped) row the cursor moves to the visual
/// row start; if already there it advances to the previous visual row's
/// start so repeated Home presses walk all the way back to position 0.
///
/// `first_non_ws_in_visual` should equal `visual_start` when the line is
/// all-whitespace within this visual row (the caller's scan will have
/// found no non-whitespace character).
pub(crate) fn smart_home_target(
    cursor_pos: usize,
    visual_start: usize,
    is_first_visual_row: bool,
    first_non_ws_in_visual: usize,
) -> SmartHomeTarget {
    if is_first_visual_row {
        // Toggle first-non-ws ↔ physical line start.
        SmartHomeTarget::At(if cursor_pos == first_non_ws_in_visual {
            visual_start
        } else {
            first_non_ws_in_visual
        })
    } else if cursor_pos == visual_start {
        SmartHomeTarget::PreviousVisualRowStart
    } else {
        SmartHomeTarget::At(visual_start)
    }
}

/// Byte offset of the first non-whitespace character in `text`, looking
/// only at characters that start before `limit` and before the first `\n`.
///
/// Both smart-home paths add the result to a byte position (the physical
/// line start, the visual row start), so it is a byte offset rather than a
/// character count: the two differ as soon as the indent holds anything
/// but ASCII, such as U+3000 IDEOGRAPHIC SPACE. Keeping the scan in one
/// place keeps the two paths from disagreeing about that again.
///
/// `limit` is the length of the region the caller is scanning (a visual
/// row). A character is counted if it *starts* inside it; visual rows break
/// on character boundaries, so such a character also ends inside it.
pub(crate) fn first_non_whitespace_offset(text: &str, limit: usize) -> Option<usize> {
    text.char_indices()
        .take_while(|(offset, c)| *offset < limit && *c != '\n')
        .find(|(_, c)| !c.is_whitespace())
        .map(|(offset, _)| offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_non_whitespace_offset_is_in_bytes() {
        // U+3000 is one character but three bytes.
        assert_eq!(
            first_non_whitespace_offset("\u{3000}abc", usize::MAX),
            Some(3)
        );
        assert_eq!(first_non_whitespace_offset("  \tx", usize::MAX), Some(3));
        assert_eq!(first_non_whitespace_offset("x", usize::MAX), Some(0));
    }

    #[test]
    fn first_non_whitespace_offset_stops_at_newline_and_limit() {
        assert_eq!(first_non_whitespace_offset("   \nabc", usize::MAX), None);
        assert_eq!(first_non_whitespace_offset("    abc", 4), None);
        assert_eq!(first_non_whitespace_offset("    abc", 5), Some(4));
        assert_eq!(first_non_whitespace_offset("", usize::MAX), None);
    }

    #[test]
    fn first_row_at_line_start_jumps_to_first_non_ws() {
        // Cursor at 0 (== visual_start), non-ws at 4.
        assert_eq!(smart_home_target(0, 0, true, 4), SmartHomeTarget::At(4));
    }

    #[test]
    fn first_row_at_first_non_ws_toggles_back_to_visual_start() {
        // Cursor at 4 (== first_non_ws), toggle back to 0.
        assert_eq!(smart_home_target(4, 0, true, 4), SmartHomeTarget::At(0));
    }

    #[test]
    fn first_row_in_middle_jumps_to_first_non_ws() {
        // Cursor at 7, first_non_ws at 4 — not at either toggle point, so
        // jump to first_non_ws.
        assert_eq!(smart_home_target(7, 0, true, 4), SmartHomeTarget::At(4));
    }

    #[test]
    fn first_row_all_whitespace_stays_at_visual_start() {
        // When line is all-whitespace the caller passes first_non_ws == visual_start.
        // Cursor anywhere maps to visual_start (either the toggle branch or
        // equal-to-non-ws branch lands there).
        assert_eq!(smart_home_target(3, 0, true, 0), SmartHomeTarget::At(0));
    }

    #[test]
    fn continuation_row_middle_moves_to_visual_start() {
        // Soft-wrap: visual_start = 80, cursor somewhere on the row.
        // first_non_ws_in_visual is ignored on continuation rows.
        assert_eq!(
            smart_home_target(95, 80, false, 80),
            SmartHomeTarget::At(80)
        );
    }

    #[test]
    fn continuation_row_at_start_advances_to_previous_row() {
        // Repeated-Home climb: cursor == visual_start on a continuation row.
        assert_eq!(
            smart_home_target(80, 80, false, 80),
            SmartHomeTarget::PreviousVisualRowStart
        );
    }
}
