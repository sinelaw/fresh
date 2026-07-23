//! Release-check evaluation: given the current version and a GitHub releases
//! API JSON body, decide whether an update is available and resolve how this
//! copy was installed. This is the pure, dependency-free core that used to live
//! inline in the editor's `release_checker`; the editor keeps only the HTTP
//! fetch and the background-thread/debounce plumbing around it.

use crate::provenance::{self, Provenance};
use crate::version;

/// The outcome of a release check.
#[derive(Debug, Clone)]
pub struct ReleaseCheck {
    /// The latest version advertised by the release feed (no leading `v`).
    pub latest_version: String,
    /// Whether `latest_version` is newer than the current version.
    pub update_available: bool,
    /// How this copy of `fresh` was installed (drives the update command).
    pub provenance: Provenance,
}

/// Evaluate a release-feed body against `current_version`, resolving provenance
/// from the live environment. Returns an error string if the body has no
/// parseable `tag_name`.
pub fn evaluate(current_version: &str, release_json: &str) -> Result<ReleaseCheck, String> {
    let latest_version = version::parse_tag_name(release_json)
        .ok_or_else(|| "tag_name not found in response".to_string())?;
    let update_available = version::is_newer(current_version, &latest_version);
    Ok(ReleaseCheck {
        latest_version,
        update_available,
        provenance: provenance::resolve(),
    })
}

/// Like [`evaluate`] but with an explicit provenance (for tests / callers that
/// already resolved it).
pub fn evaluate_with(
    current_version: &str,
    release_json: &str,
    provenance: Provenance,
) -> Result<ReleaseCheck, String> {
    let latest_version = version::parse_tag_name(release_json)
        .ok_or_else(|| "tag_name not found in response".to_string())?;
    let update_available = version::is_newer(current_version, &latest_version);
    Ok(ReleaseCheck {
        latest_version,
        update_available,
        provenance,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::channel::Channel;
    use crate::confidence::Confidence;

    #[test]
    fn evaluate_detects_update() {
        let c = evaluate_with(
            "0.4.4",
            r#"{"tag_name": "v0.4.5"}"#,
            Provenance::for_channel(Channel::Tarball, Confidence::Authoritative),
        )
        .unwrap();
        assert_eq!(c.latest_version, "0.4.5");
        assert!(c.update_available);
    }

    #[test]
    fn evaluate_no_update_when_current() {
        let c = evaluate_with(
            "0.4.5",
            r#"{"tag_name": "v0.4.5"}"#,
            Provenance::unknown(),
        )
        .unwrap();
        assert!(!c.update_available);
    }

    #[test]
    fn evaluate_errors_without_tag() {
        assert!(evaluate("0.4.4", r#"{"nope": true}"#).is_err());
    }
}
