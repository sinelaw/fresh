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

#[cfg(test)]
mod tests {
    use super::*;

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
