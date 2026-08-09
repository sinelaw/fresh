//! How sure we are about the resolved provenance.
//!
//! Every level here is something that was *recorded* — by the installer, by
//! the build, or by the user. There is no rung for a guess.
//!
//! There used to be. `Heuristic` sat between `Unknown` and `Embedded` and held
//! the result of pattern-matching the executable's path: `~/.cargo/bin` meant
//! cargo, `/opt/homebrew` meant brew, `/usr/bin` on Arch meant the AUR. It was
//! wrong whenever a binary was moved, copied or symlinked, it could not
//! distinguish apt from dnf from a file someone dropped in `/usr/bin`, and
//! being *nearly* right is worse than being unknown: it produced a confident
//! sentence about an update route that might not exist. The cases it actually
//! covered are now recorded facts — `cargo` is detected at build time from the
//! registry checkout it was compiled from (see `build.rs`), and every other
//! channel writes a receipt. What is left over is honestly [`Unknown`], which
//! routes to the releases page and says so.
//!
//! [`Unknown`]: Confidence::Unknown

/// Confidence level of a resolved [`crate::Provenance`], ordered from least
/// to most trustworthy.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Confidence {
    /// Nothing recorded how this copy was installed.
    Unknown,
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
            Confidence::Embedded => 1,
            Confidence::Authoritative => 2,
            Confidence::Overridden => 3,
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
        assert!(Confidence::Embedded > Confidence::Unknown);
    }

    #[test]
    fn self_swap_gate() {
        assert!(Confidence::Overridden.allows_self_swap());
        assert!(Confidence::Authoritative.allows_self_swap());
        assert!(Confidence::Embedded.allows_self_swap());
        assert!(!Confidence::Unknown.allows_self_swap());
    }

    /// The gate is "was this recorded", and the only unrecorded level is
    /// `Unknown`. If a rung is ever added below `Embedded` it must not silently
    /// inherit permission to overwrite the user's binary.
    #[test]
    fn only_unknown_is_barred_from_self_swap() {
        for c in [
            Confidence::Unknown,
            Confidence::Embedded,
            Confidence::Authoritative,
            Confidence::Overridden,
        ] {
            assert_eq!(
                c.allows_self_swap(),
                c != Confidence::Unknown,
                "{c:?} self-swap gate"
            );
        }
    }
}
