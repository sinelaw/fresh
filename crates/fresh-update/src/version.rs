//! Version comparison and GitHub release-tag parsing.
//!
//! Kept self-contained (no `semver` dependency) and matching the historical
//! `release_checker` behaviour so it can back that module without changing
//! observable results.

/// Parse a dotted version into `(major, minor, patch)`, tolerating a missing
/// patch (treated as 0) and a pre-release suffix on the patch (`0.4.4-rc1`).
fn parse(v: &str) -> Option<(u32, u32, u32)> {
    let v = v.trim().trim_start_matches('v');
    let parts: Vec<&str> = v.split('.').collect();
    match parts.as_slice() {
        [major, minor, rest, ..] => Some((
            major.parse().ok()?,
            minor.parse().ok()?,
            rest.split(['-', '+']).next()?.parse().ok()?,
        )),
        [major, minor] => Some((major.parse().ok()?, minor.parse().ok()?, 0)),
        _ => None,
    }
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
            ("0.4.4-alpha", "0.4.4", false), // same numeric triple
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
}
