//! Version comparison and GitHub release-tag parsing.
//!
//! Kept self-contained (no `semver` dependency) and matching the historical
//! `release_checker` behaviour so it can back that module without changing
//! observable results.

/// A parsed version, ordered by semver precedence.
///
/// The fourth field is the pre-release rank: semver says a version carrying a
/// pre-release suffix precedes the same numeric triple without one, so a plain
/// release ranks `1` and a pre-release ranks `0`. Deriving `Ord` on the fields
/// in this order gives exactly that, and falls back to comparing the
/// pre-release text when two pre-releases share a triple.
///
/// That text comparison is lexicographic rather than semver's
/// identifier-by-identifier rule, so `rc10` sorts before `rc2`. Living with it
/// is deliberate: we publish `-rc1`-style tags at most, and the alternative is
/// a semver dependency for a comparison that runs once per launch.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Version {
    major: u32,
    minor: u32,
    patch: u32,
    release_rank: u8,
    pre: String,
}

/// Parse a dotted version, tolerating a missing patch (treated as 0), a
/// pre-release suffix (`0.4.4-rc1`) and build metadata (`0.4.4+deadbeef`).
fn parse(v: &str) -> Option<Version> {
    let v = v.trim().trim_start_matches('v');
    let (numeric, pre) = match v.split_once('-') {
        Some((numeric, pre)) => (numeric, pre.split('+').next().unwrap_or("").to_string()),
        None => (v.split('+').next()?, String::new()),
    };
    let parts: Vec<&str> = numeric.split('.').collect();
    let (major, minor, patch) = match parts.as_slice() {
        [major, minor, patch, ..] => (
            major.parse().ok()?,
            minor.parse().ok()?,
            patch.parse().ok()?,
        ),
        [major, minor] => (major.parse().ok()?, minor.parse().ok()?, 0),
        _ => return None,
    };
    Some(Version {
        major,
        minor,
        patch,
        release_rank: u8::from(pre.is_empty()),
        pre,
    })
}

/// `true` if `latest` is strictly newer than `current`.
pub fn is_newer(current: &str, latest: &str) -> bool {
    match (parse(current), parse(latest)) {
        (Some(c), Some(l)) => l > c,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn newer_comparison() {
        let cases = [
            ("0.4.4", "1.0.0", true),
            ("0.4.4", "0.5.0", true),
            ("0.4.4", "0.4.5", true),
            ("0.4.4", "0.4.4", false),
            ("0.4.4", "0.4.3", false),
            ("0.5.0", "0.4.9", false),
            // A pre-release precedes its own release (semver §11.3), so
            // someone running an rc *is* offered the final build. Treating
            // these as equal stranded every pre-release user on the rc.
            ("0.4.4-alpha", "0.4.4", true),
            ("0.4.4", "0.4.4-alpha", false),
            ("0.4.4-alpha", "0.4.4-beta", true),
            ("0.4.4-rc1", "0.4.4-rc1", false),
            ("0.4.4+build7", "0.4.4", false), // build metadata is not precedence
            ("0.4.4", "0.4.5-beta", true),
            ("v0.4.4", "v0.4.5", true), // leading v tolerated
            ("0.4", "0.4.1", true),     // missing patch -> 0
        ];
        for (cur, lat, want) in cases {
            assert_eq!(is_newer(cur, lat), want, "is_newer({cur:?}, {lat:?})");
        }
    }
}
