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

/// Extract the `tag_name` value from a GitHub releases API JSON body and strip
/// a leading `v`. Dependency-free string scan, matching the existing checker.
pub fn parse_tag_name(json: &str) -> Option<String> {
    let key = "\"tag_name\"";
    let start = json.find(key)?;
    let after = &json[start + key.len()..];
    let q1 = after.find('"')?;
    let rest = &after[q1 + 1..];
    let q2 = rest.find('"')?;
    let tag = &rest[..q2];
    Some(tag.strip_prefix('v').unwrap_or(tag).to_string())
}

/// Find the download URL of the release asset whose filename ends with
/// `extension` and contains `arch` — e.g. (`.deb`, `amd64`) picks
/// `fresh-editor_0.4.7-1_amd64.deb` out of the release feed.
///
/// Reading the name off the feed rather than templating it keeps the updater
/// working when a packaging tool changes how it spells a version or release
/// number. Dependency-free scan, matching [`parse_tag_name`].
pub fn find_asset_url(json: &str, extension: &str, arch: &str) -> Option<String> {
    const KEY: &str = "\"browser_download_url\"";
    let mut rest = json;
    while let Some(start) = rest.find(KEY) {
        let after = &rest[start + KEY.len()..];
        // Step past the `:` and the opening quote of the value.
        let q1 = after.find('"')?;
        let tail = &after[q1 + 1..];
        let q2 = tail.find('"')?;
        let url = &tail[..q2];
        let name = url.rsplit('/').next().unwrap_or(url);
        if name.ends_with(extension) && name.contains(arch) {
            return Some(url.to_string());
        }
        rest = &tail[q2..];
    }
    None
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

    #[test]
    fn tag_name_parsing() {
        assert_eq!(
            parse_tag_name(r#"{"tag_name": "v0.4.5"}"#).as_deref(),
            Some("0.4.5")
        );
        assert_eq!(
            parse_tag_name(r#"{"tag_name":"0.4.5"}"#).as_deref(),
            Some("0.4.5")
        );
        assert_eq!(
            parse_tag_name(r#"{"name":"x","tag_name": "v1.2.3", "draft": false}"#).as_deref(),
            Some("1.2.3")
        );
        assert_eq!(parse_tag_name(r#"{"no_tag": "here"}"#), None);
    }

    /// A trimmed release feed with the assets that actually ship, in the order
    /// GitHub returns them.
    const FEED: &str = r#"{"tag_name":"v0.4.7","assets":[
        {"browser_download_url":"https://gh/v0.4.7/fresh-editor-x86_64-unknown-linux-gnu.tar.xz"},
        {"browser_download_url":"https://gh/v0.4.7/fresh-editor_0.4.7-1_arm64.deb"},
        {"browser_download_url":"https://gh/v0.4.7/fresh-editor_0.4.7-1_amd64.deb"},
        {"browser_download_url":"https://gh/v0.4.7/fresh-editor-0.4.7-1.x86_64.rpm"},
        {"browser_download_url":"https://gh/v0.4.7/fresh-editor-0.4.7-x86_64.flatpak"},
        {"browser_download_url":"https://gh/v0.4.7/fresh-editor-0.4.7-x86_64.flatpak.sha256"}
    ]}"#;

    #[test]
    fn asset_lookup_picks_the_matching_artifact() {
        let cases = [
            (".deb", "amd64", "fresh-editor_0.4.7-1_amd64.deb"),
            (".deb", "arm64", "fresh-editor_0.4.7-1_arm64.deb"),
            (".rpm", "x86_64", "fresh-editor-0.4.7-1.x86_64.rpm"),
            (".flatpak", "x86_64", "fresh-editor-0.4.7-x86_64.flatpak"),
        ];
        for (ext, arch, want) in cases {
            let url = find_asset_url(FEED, ext, arch).expect("{ext} {arch}");
            assert_eq!(url.rsplit('/').next().unwrap(), want, "{ext} {arch}");
        }
    }

    #[test]
    fn asset_lookup_declines_when_absent() {
        // No aarch64 rpm in this feed, and the checksum sidecar must not be
        // mistaken for the bundle it describes.
        assert_eq!(find_asset_url(FEED, ".rpm", "aarch64"), None);
        assert_eq!(find_asset_url(FEED, ".AppImage", "x86_64"), None);
        assert_eq!(find_asset_url(r#"{"assets":[]}"#, ".deb", "amd64"), None);
    }
}
