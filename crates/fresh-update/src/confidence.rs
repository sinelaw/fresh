//! How sure we are about the resolved provenance.
//!
//! Confidence gates behaviour: only `Embedded` and above may trigger an
//! automatic in-place binary swap; `Heuristic` results are shown as a
//! suggestion but never acted on destructively.

/// Confidence level of a resolved [`crate::Provenance`], ordered from least
/// to most trustworthy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Confidence {
    /// Nothing told us anything; provenance is a blank guess.
    Unknown,
    /// Inferred from the executable path. Best-effort only.
    Heuristic,
    /// Baked into the binary at compile time (`FRESH_BUILD_CHANNEL`).
    Embedded,
    /// Read from an install receipt written by the installer.
    Authoritative,
    /// Forced by the `FRESH_INSTALL_CHANNEL` runtime override.
    Overridden,
}

impl Confidence {
    /// Numeric rank for ordering (higher = more trustworthy).
    pub const fn rank(self) -> u8 {
        match self {
            Confidence::Unknown => 0,
            Confidence::Heuristic => 1,
            Confidence::Embedded => 2,
            Confidence::Authoritative => 3,
            Confidence::Overridden => 4,
        }
    }

    /// Whether provenance at this confidence is trustworthy enough to perform
    /// an automatic, unattended in-place binary swap.
    pub const fn allows_self_swap(self) -> bool {
        self.rank() >= Confidence::Embedded.rank()
    }
}

impl PartialOrd for Confidence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Confidence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.rank().cmp(&other.rank())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ordering_is_monotonic() {
        assert!(Confidence::Overridden > Confidence::Authoritative);
        assert!(Confidence::Authoritative > Confidence::Embedded);
        assert!(Confidence::Embedded > Confidence::Heuristic);
        assert!(Confidence::Heuristic > Confidence::Unknown);
    }

    #[test]
    fn self_swap_gate() {
        assert!(Confidence::Overridden.allows_self_swap());
        assert!(Confidence::Authoritative.allows_self_swap());
        assert!(Confidence::Embedded.allows_self_swap());
        assert!(!Confidence::Heuristic.allows_self_swap());
        assert!(!Confidence::Unknown.allows_self_swap());
    }
}
