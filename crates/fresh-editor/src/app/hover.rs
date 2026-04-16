//! Self-contained hover state.
//!
//! Owns five fields previously scattered on `Editor`:
//!
//! - In-flight LSP hover request: `(request_id, lsp_position)`. Used to
//!   ignore stale responses from earlier mouse moves.
//! - Highlighted-symbol range and its overlay handle. The handle is kept
//!   so we can remove the old overlay before drawing a new one, or when
//!   focus is lost.
//! - Cached mouse screen position. Set at request time so the popup can
//!   be placed under the same cell the user last hovered, even if they
//!   have since moved the mouse elsewhere.
//!
//! All cross-cutting effects — sending the LSP request, applying the
//! `RemoveOverlay` event to the buffer after `take_symbol_overlay`,
//! positioning the popup — stay on `Editor` as orchestrators. This module
//! is pure data with no `Editor` or I/O access.

use crate::view::overlay::OverlayHandle;

/// Owner of in-flight hover request and highlighted-symbol tracking.
#[derive(Debug, Default)]
pub(crate) struct HoverState {
    /// LSP request id of the in-flight hover request, if any.
    pending_request: Option<u64>,
    /// LSP position `(line, character)` of the in-flight request. Retained
    /// so the response handler can correlate diagnostics with the hover
    /// point and fuse them into the hover card.
    pending_position: Option<(u32, u32)>,
    /// Byte range `(start, end)` of the currently-highlighted symbol.
    /// Used by mouse-move handlers to detect "still on same symbol" and
    /// skip re-querying.
    symbol_range: Option<(usize, usize)>,
    /// Overlay handle for the symbol highlight, so the caller can remove
    /// the previous highlight via `RemoveOverlay` before adding a new one.
    symbol_overlay: Option<OverlayHandle>,
    /// Screen cell `(col, row)` where the popup should be placed. Set
    /// when a mouse-triggered hover request is fired; consumed when the
    /// popup is rendered.
    screen_position: Option<(u16, u16)>,
}

impl HoverState {
    // ---- Pending-request correlation --------------------------------------

    /// Record that a hover request with `request_id` was sent at LSP
    /// position `(line, character)`.
    pub(crate) fn record_request(&mut self, request_id: u64, line: u32, character: u32) {
        self.pending_request = Some(request_id);
        self.pending_position = Some((line, character));
    }

    /// Claim a response as matching the in-flight request. If it matches,
    /// both `pending_request` and `pending_position` are cleared, and the
    /// position is returned for the caller's use (diagnostic correlation).
    ///
    /// Returns `None` if the response is stale — the caller should drop it.
    pub(crate) fn claim_pending(&mut self, request_id: u64) -> Option<(u32, u32)> {
        if self.pending_request != Some(request_id) {
            return None;
        }
        self.pending_request = None;
        self.pending_position.take()
    }

    /// Clear any in-flight request without consuming a position — used
    /// when focus is lost or the user cancels hover.
    pub(crate) fn clear_pending(&mut self) {
        self.pending_request = None;
        self.pending_position = None;
    }

    // ---- Symbol range -----------------------------------------------------

    pub(crate) fn symbol_range(&self) -> Option<(usize, usize)> {
        self.symbol_range
    }

    pub(crate) fn set_symbol_range(&mut self, range: Option<(usize, usize)>) {
        self.symbol_range = range;
    }

    // ---- Symbol overlay handle --------------------------------------------

    /// Take the current overlay handle (if any) so the caller can apply a
    /// `RemoveOverlay` event to the buffer before adding a new overlay.
    pub(crate) fn take_symbol_overlay(&mut self) -> Option<OverlayHandle> {
        self.symbol_overlay.take()
    }

    pub(crate) fn set_symbol_overlay(&mut self, handle: OverlayHandle) {
        self.symbol_overlay = Some(handle);
    }

    // ---- Screen position --------------------------------------------------

    pub(crate) fn set_screen_position(&mut self, pos: (u16, u16)) {
        self.screen_position = Some(pos);
    }

    pub(crate) fn take_screen_position(&mut self) -> Option<(u16, u16)> {
        self.screen_position.take()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_is_empty() {
        let mut h = HoverState::default();
        assert_eq!(h.symbol_range(), None);
        assert_eq!(h.claim_pending(42), None);
    }

    #[test]
    fn claim_pending_returns_position_and_clears_state() {
        let mut h = HoverState::default();
        h.record_request(7, 10, 20);
        assert_eq!(h.claim_pending(7), Some((10, 20)));
        // Subsequent claim for the same id returns None — position drained.
        assert_eq!(h.claim_pending(7), None);
    }

    #[test]
    fn claim_pending_rejects_stale_response() {
        let mut h = HoverState::default();
        h.record_request(7, 10, 20);
        // Older response arrives after a newer request went out.
        assert_eq!(h.claim_pending(3), None);
        // Correct one still works.
        assert_eq!(h.claim_pending(7), Some((10, 20)));
    }

    #[test]
    fn record_request_overwrites_previous_pending() {
        let mut h = HoverState::default();
        h.record_request(1, 0, 0);
        h.record_request(2, 5, 5);
        assert_eq!(h.claim_pending(1), None);
        assert_eq!(h.claim_pending(2), Some((5, 5)));
    }

    #[test]
    fn clear_pending_drops_without_returning_position() {
        let mut h = HoverState::default();
        h.record_request(7, 10, 20);
        h.clear_pending();
        assert_eq!(h.claim_pending(7), None);
    }

    #[test]
    fn symbol_range_roundtrips() {
        let mut h = HoverState::default();
        assert_eq!(h.symbol_range(), None);
        h.set_symbol_range(Some((10, 20)));
        assert_eq!(h.symbol_range(), Some((10, 20)));
        h.set_symbol_range(None);
        assert_eq!(h.symbol_range(), None);
    }

    #[test]
    fn take_screen_position_drains_on_first_call() {
        let mut h = HoverState::default();
        h.set_screen_position((15, 8));
        assert_eq!(h.take_screen_position(), Some((15, 8)));
        assert_eq!(h.take_screen_position(), None);
    }
}
