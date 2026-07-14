//! Cursor-dependent activation for conceals and soft breaks.
//!
//! A decoration tagged with a [`ScopedActivation`] is filtered at *query*
//! time against the rendering split's cursor positions, instead of being
//! deleted and recreated by the plugin on every cursor move. This is what
//! lets cursor movement leave the marker set — and therefore the
//! `LineWrapCache` / `VisualRowIndex` versions — completely untouched.
//!
//! The scope is stored **relative to the decoration's own marker
//! position** (`before` bytes ahead of it, `len` bytes long). Both the
//! decoration and its scope live on the same line, so an edit anywhere
//! before that line shifts the marker and the scope stays aligned without
//! any bookkeeping; an edit *inside* the line re-fires `lines_changed`
//! for it and the plugin rebuilds the decoration anyway.

use fresh_core::api::MarkerActivation;

/// Cursor-scope rule stored on a conceal range or soft break.
/// `None` on the owning entry means "always active".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopedActivation {
    /// `true`: active only while a cursor IS in scope (`if-cursor-in`).
    /// `false`: active only while NO cursor is in scope (`unless-cursor-in`).
    pub if_cursor_in: bool,
    /// Scope starts `before` bytes ahead of the decoration's anchor
    /// position (the conceal's start marker / the soft break's marker).
    pub before: u32,
    /// Scope byte length (half-open range).
    pub len: u32,
}

impl ScopedActivation {
    /// Convert a wire-format rule (absolute scope bytes) into the
    /// marker-relative form, anchored at `anchor` (the decoration's
    /// position at emission time). A scope starting after the anchor is
    /// clamped to it — plugins scope a decoration to its own span or
    /// line, both of which start at or before the decoration.
    pub fn from_absolute(rule: &MarkerActivation, anchor: usize) -> Self {
        let scope_start = rule.scope_start.min(anchor);
        Self {
            if_cursor_in: rule.if_cursor_in,
            before: (anchor - scope_start) as u32,
            len: rule.scope_end.saturating_sub(scope_start) as u32,
        }
    }

    /// Whether the decoration is active given its current anchor
    /// position and the rendering split's cursor byte positions.
    #[inline]
    pub fn is_active(&self, anchor: usize, cursors: &[usize]) -> bool {
        let scope_start = anchor.saturating_sub(self.before as usize);
        let scope_end = scope_start + self.len as usize;
        let cursor_in = cursors.iter().any(|&p| p >= scope_start && p < scope_end);
        cursor_in == self.if_cursor_in
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(if_cursor_in: bool, start: usize, end: usize) -> MarkerActivation {
        MarkerActivation {
            if_cursor_in,
            scope_start: start,
            scope_end: end,
        }
    }

    #[test]
    fn unless_cursor_in_active_without_cursor() {
        let a = ScopedActivation::from_absolute(&rule(false, 10, 20), 12);
        assert!(a.is_active(12, &[]));
        assert!(a.is_active(12, &[5, 25]));
        assert!(!a.is_active(12, &[10]));
        assert!(!a.is_active(12, &[19]));
        assert!(a.is_active(12, &[20])); // end is exclusive
    }

    #[test]
    fn if_cursor_in_active_only_with_cursor() {
        let a = ScopedActivation::from_absolute(&rule(true, 10, 20), 12);
        assert!(!a.is_active(12, &[]));
        assert!(a.is_active(12, &[15]));
        assert!(!a.is_active(12, &[20]));
    }

    #[test]
    fn scope_shifts_with_anchor() {
        // Emitted with anchor 12, scope [10, 20). After an edit shifts the
        // line by +5 the anchor resolves to 17 and the scope follows.
        let a = ScopedActivation::from_absolute(&rule(false, 10, 20), 12);
        assert!(!a.is_active(17, &[15])); // shifted scope [15, 25)
        assert!(a.is_active(17, &[10])); // now outside
    }
}
