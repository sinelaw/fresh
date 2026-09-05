//! Cross-origin verification: does GitHub attest to the bytes we downloaded?
//!
//! The `.sha256` sidecar comes from the same origin as the artifact, so
//! whoever can substitute one can substitute the other — it catches
//! corruption, nothing else. The release pipeline also publishes build
//! attestations: in-toto statements naming each asset and its digest, served
//! from `api.github.com`, an origin pinned separately. Asking there whether
//! this digest is published under this name means both origins have to fall
//! rather than one. GitHub indexes by digest, so a tampered artifact has no
//! attestation to find.
//!
//! This is deliberately not full Sigstore. The bundle's DSSE envelope is
//! signed by a Fulcio certificate, but checking that signature buys nothing
//! without validating the chain to a pinned root: whoever can forge the
//! `api.github.com` response forges the certificate with it. GitHub ships that
//! root over TUF, whose traversal is a client in its own right, and pinning it
//! without that machinery trades an outage at every root rotation for the
//! assurance gained. The anchor here is TLS to a second origin, shaped so
//! chain validation can slot in above it later. Two origins, not a signature.
//!
//! Fail-closed: every asset the engine downloads is attested, so a missing
//! attestation means something is wrong, not something is old. An overridden
//! endpoint skips the check — a test server has no attestations and never
//! could — and the engine already refuses to elevate with bytes from one.

use crate::net::Transport;
use base64::engine::general_purpose::STANDARD as BASE64;
use base64::Engine as _;

/// Cap on the attestation document. Bundles run to a few tens of KB — one DSSE
/// envelope listing every asset in the release — so this is far above real
/// sizes and far below anything that would hurt.
pub const ATTESTATION_MAX_BYTES: u64 = 4 * 1024 * 1024;

/// Why an artifact could not be shown to be attested.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttestationError {
    /// The attestation endpoint could not be reached, or refused the request.
    Fetch(String),
    /// GitHub holds no attestation covering this digest.
    NotAttested { asset: String, digest: String },
    /// The response was not an attestation document we could read.
    Malformed(String),
    /// Attestations exist for this digest, but none records it under the asset
    /// name we asked for.
    NameMismatch { asset: String, digest: String },
    /// The attestation endpoint refused because GitHub's API rate limit for
    /// this address is spent. Distinct from [`AttestationError::Fetch`]
    /// because it is temporary and has remedies nothing else here has.
    RateLimited(String),
}

impl std::fmt::Display for AttestationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AttestationError::Fetch(e) => {
                write!(f, "could not check the release attestation: {e}")
            }
            AttestationError::NotAttested { asset, digest } => write!(
                f,
                "{asset} is not covered by any release attestation \
                 (sha256:{digest}); refusing to install it"
            ),
            AttestationError::Malformed(e) => {
                write!(f, "could not read the release attestation: {e}")
            }
            AttestationError::NameMismatch { asset, digest } => write!(
                f,
                "the release attestation for sha256:{digest} does not record it \
                 as {asset}; refusing to install it"
            ),
            // Fail-closed is the whole design (see the module note), so this
            // reports a stop, not a warning — and then says how to get past it.
            // The download itself is fine: it matched its checksum. What could
            // not happen is the second origin agreeing, and installing without
            // that is the thing this module exists to refuse.
            AttestationError::RateLimited(detail) => write!(
                f,
                "{detail}\n\nThe download matched its checksum, but the release \
                 attestation could not be checked, and fresh does not install \
                 unverified bytes. Re-run once the limit resets, or set a token as \
                 above and re-run now."
            ),
        }
    }
}

impl std::error::Error for AttestationError {}

/// Where the attestations for `digest_hex` live.
///
/// `api.github.com` is in [`crate::endpoint::ALLOWED_HOSTS`], so this URL
/// passes the same host check as everything else the updater fetches.
pub fn attestation_url(repo: &str, digest_hex: &str) -> String {
    format!("https://api.github.com/repos/{repo}/attestations/sha256:{digest_hex}")
}

/// Confirm GitHub attests to `digest_hex` under the name `asset`.
///
/// `digest_hex` is the SHA-256 of the bytes actually downloaded, not a value
/// read from anywhere on the network.
pub fn verify(
    transport: &Transport,
    repo: &str,
    asset: &str,
    digest_hex: &str,
) -> Result<(), AttestationError> {
    let digest = digest_hex.to_ascii_lowercase();
    let url = attestation_url(repo, &digest);

    // The attestation origin is pinned exactly like the asset origin; a
    // redirect away from it would defeat the point of asking a second host.
    crate::endpoint::check(&url).map_err(|e| AttestationError::Fetch(e.to_string()))?;

    let body = match transport.get_text_optional(&url, ATTESTATION_MAX_BYTES) {
        Ok(body) => body,
        Err(e) if e.is_rate_limited() => return Err(AttestationError::RateLimited(e.to_string())),
        Err(e) => return Err(AttestationError::Fetch(e.to_string())),
    }
    .ok_or_else(|| AttestationError::NotAttested {
        asset: asset.to_string(),
        digest: digest.clone(),
    })?;

    if attests(&body, asset, &digest)? {
        Ok(())
    } else {
        Err(AttestationError::NameMismatch {
            asset: asset.to_string(),
            digest,
        })
    }
}

/// `true` if `body` contains an attestation whose in-toto statement lists
/// `asset` with SHA-256 `digest`.
///
/// Split out from [`verify`] so the document handling is testable without a
/// network: this is the part that has to be right about a real GitHub payload.
pub fn attests(body: &str, asset: &str, digest: &str) -> Result<bool, AttestationError> {
    let doc: serde_json::Value =
        serde_json::from_str(body).map_err(|e| AttestationError::Malformed(e.to_string()))?;

    let attestations = doc
        .get("attestations")
        .and_then(|a| a.as_array())
        .ok_or_else(|| AttestationError::Malformed("no `attestations` array".to_string()))?;

    // An empty array is GitHub saying "nothing here" with a 200 rather than a
    // 404; it must not read as success.
    for attestation in attestations {
        let payload_b64 = attestation
            .pointer("/bundle/dsseEnvelope/payload")
            .and_then(|p| p.as_str());
        let Some(payload_b64) = payload_b64 else {
            continue;
        };
        let raw = BASE64
            .decode(payload_b64)
            .map_err(|e| AttestationError::Malformed(format!("DSSE payload is not base64: {e}")))?;
        let statement: serde_json::Value = serde_json::from_slice(&raw)
            .map_err(|e| AttestationError::Malformed(format!("DSSE payload: {e}")))?;

        let Some(subjects) = statement.get("subject").and_then(|s| s.as_array()) else {
            continue;
        };
        for subject in subjects {
            let name = subject.get("name").and_then(|n| n.as_str());
            let sha = subject
                .pointer("/digest/sha256")
                .and_then(|d| d.as_str())
                .map(str::to_ascii_lowercase);
            // Both must match: the digest alone would accept a genuine
            // attestation for a different artifact that we were handed under
            // this name, and the name alone would accept anything at all.
            if name == Some(asset) && sha.as_deref() == Some(digest) {
                return Ok(true);
            }
        }
    }
    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Shape of a real `api.github.com/.../attestations/sha256:…` response,
    /// trimmed to the fields this module reads.
    fn document(asset: &str, digest: &str) -> String {
        let statement = format!(
            r#"{{"_type":"https://in-toto.io/Statement/v1",
                 "predicateType":"https://in-toto.io/attestation/release/v0.2",
                 "subject":[
                   {{"uri":"pkg:github/sinelaw/fresh@v0.4.7","digest":{{"sha1":"94bcfd0"}}}},
                   {{"name":"other-asset.tar.xz","digest":{{"sha256":"{other}"}}}},
                   {{"name":"{asset}","digest":{{"sha256":"{digest}"}}}}
                 ]}}"#,
            other = "b".repeat(64),
        );
        let payload = BASE64.encode(statement.as_bytes());
        format!(
            r#"{{"attestations":[{{"bundle":{{
                 "mediaType":"application/vnd.dev.sigstore.bundle.v0.3+json",
                 "dsseEnvelope":{{"payloadType":"application/vnd.in-toto+json",
                                  "payload":"{payload}",
                                  "signatures":[{{"sig":"MEUCIQ=="}}]}}}}}}]}}"#
        )
    }

    /// The real thing: an unedited response from
    /// `api.github.com/repos/sinelaw/fresh/attestations/sha256:…` for the
    /// v0.4.7 release.
    ///
    /// The synthetic documents above only prove the matching logic is right
    /// about a payload this file wrote. This one proves it is right about a
    /// payload GitHub wrote — the DSSE nesting, the base64 of a 55-subject
    /// statement, and the leading `uri`/`sha1` subject that carries no `name`
    /// at all and must not derail the scan.
    const REAL: &str = include_str!("../tests/fixtures/github-release-attestation.json");

    /// Digest of `fresh-editor-x86_64-unknown-linux-gnu.tar.xz` at v0.4.7, as
    /// published in that release's `.sha256` sidecar.
    const REAL_DIGEST: &str = "c2d161b2686d1e5c3c09db2591746402993d33f33ff77bf4fe67b575472ea87d";
    const REAL_ASSET: &str = "fresh-editor-x86_64-unknown-linux-gnu.tar.xz";

    #[test]
    fn accepts_a_real_github_attestation() {
        assert_eq!(attests(REAL, REAL_ASSET, REAL_DIGEST), Ok(true));
    }

    /// Every channel the engine downloads for must be covered, or fail-closed
    /// verification would break that channel's updates the day it ships.
    #[test]
    fn a_real_attestation_covers_every_downloaded_asset_kind() {
        for asset in [
            "fresh-editor_0.4.7-1_amd64.deb",
            "fresh-editor-0.4.7-1.x86_64.rpm",
            "fresh-editor-0.4.7-x86_64.flatpak",
            "fresh-editor-0.4.7-x86_64.AppImage",
            "fresh-editor-x86_64-pc-windows-msvc.zip",
            "fresh-editor-x86_64-unknown-linux-musl.tar.gz",
        ] {
            let doc: serde_json::Value = serde_json::from_str(REAL).unwrap();
            let payload = doc
                .pointer("/attestations/0/bundle/dsseEnvelope/payload")
                .and_then(|p| p.as_str())
                .unwrap();
            let statement: serde_json::Value =
                serde_json::from_slice(&BASE64.decode(payload).unwrap()).unwrap();
            let named = statement["subject"]
                .as_array()
                .unwrap()
                .iter()
                .find(|s| s.get("name").and_then(|n| n.as_str()) == Some(asset))
                .unwrap_or_else(|| panic!("{asset} is not attested"));
            let digest = named["digest"]["sha256"].as_str().unwrap();
            assert_eq!(attests(REAL, asset, digest), Ok(true));
        }
    }

    /// One flipped byte in the artifact means a digest that appears nowhere in
    /// the statement — the tampering case, on real data.
    #[test]
    fn a_real_attestation_rejects_an_unlisted_digest() {
        let tampered = format!("{}0", &REAL_DIGEST[..REAL_DIGEST.len() - 1]);
        assert_eq!(attests(REAL, REAL_ASSET, &tampered), Ok(false));
    }

    /// Right bytes, wrong name: a real asset's digest offered as some other
    /// asset must not verify.
    #[test]
    fn a_real_attestation_binds_digest_to_name() {
        assert_eq!(
            attests(
                REAL,
                "fresh-editor-aarch64-unknown-linux-gnu.tar.xz",
                REAL_DIGEST
            ),
            Ok(false)
        );
    }

    #[test]
    fn accepts_matching_name_and_digest() {
        let digest = "a".repeat(64);
        let doc = document("fresh-editor-x86_64-unknown-linux-musl.tar.xz", &digest);
        assert_eq!(
            attests(
                &doc,
                "fresh-editor-x86_64-unknown-linux-musl.tar.xz",
                &digest
            ),
            Ok(true)
        );
    }

    /// A genuine attestation for a *different* artifact must not launder a
    /// substituted one: the digest is in the document, but under another name.
    #[test]
    fn rejects_digest_recorded_under_another_name() {
        let digest = "b".repeat(64);
        let doc = document(
            "fresh-editor-x86_64-unknown-linux-musl.tar.xz",
            &"a".repeat(64),
        );
        assert_eq!(
            attests(
                &doc,
                "fresh-editor-x86_64-unknown-linux-musl.tar.xz",
                &digest
            ),
            Ok(false)
        );
    }

    #[test]
    fn rejects_name_with_a_different_digest() {
        let doc = document(
            "fresh-editor-x86_64-unknown-linux-musl.tar.xz",
            &"a".repeat(64),
        );
        assert_eq!(
            attests(
                &doc,
                "fresh-editor-x86_64-unknown-linux-musl.tar.xz",
                &"c".repeat(64)
            ),
            Ok(false)
        );
    }

    #[test]
    fn empty_attestation_list_is_not_success() {
        assert_eq!(
            attests(r#"{"attestations":[]}"#, "a.tar.xz", "d"),
            Ok(false)
        );
    }

    #[test]
    fn missing_array_is_malformed() {
        assert!(matches!(
            attests("{}", "a.tar.xz", "d"),
            Err(AttestationError::Malformed(_))
        ));
    }

    #[test]
    fn digest_comparison_ignores_hex_case() {
        let digest = "a".repeat(64);
        let doc = document("asset.tar.xz", &digest);
        assert_eq!(
            attests(&doc, "asset.tar.xz", &"A".repeat(64).to_ascii_lowercase()),
            Ok(true)
        );
    }

    /// The one API request an update still makes is this one, so a rate limit
    /// here is the one that can stop an install. It has to say that: what was
    /// verified, what was not, and that re-running is the fix — not "403".
    #[test]
    fn a_rate_limited_lookup_explains_the_stop_rather_than_the_status() {
        let limited = crate::net::FetchError::RateLimited {
            url: attestation_url("sinelaw/fresh", &"a".repeat(64)),
            wait: Some(std::time::Duration::from_secs(600)),
            authenticated: false,
        };
        assert!(limited.is_rate_limited());
        let message = AttestationError::RateLimited(limited.to_string()).to_string();
        assert!(message.contains("10 minutes"), "{message}");
        assert!(message.contains("GITHUB_TOKEN"), "{message}");
        assert!(
            message.contains("matched its checksum"),
            "the message must say what did verify: {message}"
        );
        assert!(
            message.contains("does not install unverified bytes"),
            "the message must say the install stopped: {message}"
        );
    }

    #[test]
    fn url_is_on_the_pinned_api_host() {
        let url = attestation_url("sinelaw/fresh", &"a".repeat(64));
        assert!(crate::endpoint::is_trusted(&url), "{url} is not pinned");
        assert_eq!(
            crate::endpoint::host_of(&url).as_deref(),
            Some("api.github.com")
        );
    }
}
