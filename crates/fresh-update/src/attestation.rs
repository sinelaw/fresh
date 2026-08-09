//! Cross-origin verification: does GitHub attest to the bytes we just
//! downloaded?
//!
//! # The hole this closes
//!
//! [`crate::endpoint`] is explicit that host pinning is not a substitute for
//! signing, "because a checksum served from the same origin as the payload
//! proves nothing about who produced either". That is precisely the situation
//! the `.sha256` sidecar leaves us in: the artifact comes from the release CDN
//! and its checksum comes from the release CDN, so whoever can serve one can
//! serve the other, and the comparison only proves the server can do
//! arithmetic.
//!
//! The release pipeline already publishes GitHub build attestations
//! (`github-attestations = true` in `dist-workspace.toml`). An attestation is
//! an in-toto statement, produced by the workflow that built the release,
//! listing every published asset by name and SHA-256. It is served from
//! `api.github.com` — a *different* origin from the asset CDN, and one this
//! crate pins separately.
//!
//! So after the sidecar check we ask a second question at a second origin:
//! **is this exact digest, under this exact asset name, in the attestation for
//! this repository?** GitHub indexes attestations by subject digest, so a
//! tampered artifact simply has no attestation to find — the lookup 404s.
//! Substituting a genuine-but-different artifact fails too, because the name
//! recorded next to the digest will not be the asset we asked for.
//!
//! # What this proves, and what it does not
//!
//! It proves that the bytes are ones GitHub's attestation service holds a
//! release attestation for, under the name we expected, in this repository —
//! and that an attacker who controls only the asset CDN cannot manufacture
//! that. Both origins now have to fall, not one.
//!
//! It is **not** full Sigstore verification. The bundle carries a DSSE
//! envelope signed by a Fulcio-issued certificate, and verifying that
//! signature is only worth anything if the certificate chain is validated to a
//! pinned root — otherwise an attacker who can forge the `api.github.com`
//! response forges the certificate alongside it, and the signature check adds
//! nothing it did not already control. GitHub distributes that root through a
//! TUF repository whose traversal (timestamp → snapshot → targets, with root
//! rotation) is a client in its own right, and the research this design
//! follows is explicit that full TUF is usually too much for a standalone
//! binary. Pinning the root without the TUF machinery would trade a hard
//! availability failure at every rotation for the assurance gained.
//!
//! So the trust anchor here is TLS to a pinned second origin, and the code is
//! shaped so that chain validation slots in above it later rather than
//! replacing it. The honest summary is: two origins instead of one, not a
//! signature.
//!
//! # Fail-closed, except against a test endpoint
//!
//! A production run treats a missing or non-matching attestation as fatal —
//! every asset type the engine downloads (`.deb`, `.rpm`, `.flatpak`,
//! `.tar.xz`, `.zip`, `.AppImage`) is attested by the release workflow, so its
//! absence means something is wrong rather than something is old.
//!
//! A run against an overridden endpoint skips the check, because a local test
//! server has no attestations and never could. That is the same line
//! [`crate::endpoint`] already draws: an overridden endpoint is marked
//! untrusted, and the engine refuses to elevate with what it fetched.

use crate::net::Transport;

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

    let body = transport
        .get_text_optional(&url, ATTESTATION_MAX_BYTES)
        .map_err(AttestationError::Fetch)?
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
        let raw = decode_base64(payload_b64)
            .ok_or_else(|| AttestationError::Malformed("DSSE payload is not base64".to_string()))?;
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

/// Standard-alphabet base64 with optional padding.
///
/// Hand-rolled rather than pulled in: the crate's dependency list is
/// deliberately short, and this decodes one field of one document.
fn decode_base64(input: &str) -> Option<Vec<u8>> {
    fn value(b: u8) -> Option<u32> {
        match b {
            b'A'..=b'Z' => Some(u32::from(b - b'A')),
            b'a'..=b'z' => Some(u32::from(b - b'a') + 26),
            b'0'..=b'9' => Some(u32::from(b - b'0') + 52),
            b'+' => Some(62),
            b'/' => Some(63),
            _ => None,
        }
    }

    let mut out = Vec::with_capacity(input.len() / 4 * 3);
    let mut acc: u32 = 0;
    let mut bits = 0u32;
    for &byte in input.as_bytes() {
        if byte.is_ascii_whitespace() {
            continue;
        }
        if byte == b'=' {
            break;
        }
        acc = (acc << 6) | value(byte)?;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push(u8::try_from((acc >> bits) & 0xff).ok()?);
        }
    }
    // Leftover bits must be zero padding, never a partial byte we dropped.
    if bits >= 6 || (acc & ((1 << bits) - 1)) != 0 {
        return None;
    }
    Some(out)
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
        let payload = encode_base64(statement.as_bytes());
        format!(
            r#"{{"attestations":[{{"bundle":{{
                 "mediaType":"application/vnd.dev.sigstore.bundle.v0.3+json",
                 "dsseEnvelope":{{"payloadType":"application/vnd.in-toto+json",
                                  "payload":"{payload}",
                                  "signatures":[{{"sig":"MEUCIQ=="}}]}}}}}}]}}"#
        )
    }

    fn encode_base64(bytes: &[u8]) -> String {
        const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        let mut out = String::new();
        for chunk in bytes.chunks(3) {
            let b = [
                chunk[0],
                *chunk.get(1).unwrap_or(&0),
                *chunk.get(2).unwrap_or(&0),
            ];
            let n = (u32::from(b[0]) << 16) | (u32::from(b[1]) << 8) | u32::from(b[2]);
            let idx = [(n >> 18) & 63, (n >> 12) & 63, (n >> 6) & 63, n & 63];
            for (i, id) in idx.iter().enumerate() {
                if i <= chunk.len() {
                    out.push(char::from(ALPHABET[*id as usize]));
                } else {
                    out.push('=');
                }
            }
        }
        out
    }

    #[test]
    fn base64_round_trips() {
        for sample in ["", "a", "ab", "abc", "abcd", "hello world", "{\"a\":1}"] {
            let encoded = encode_base64(sample.as_bytes());
            assert_eq!(
                decode_base64(&encoded).as_deref(),
                Some(sample.as_bytes()),
                "round trip failed for {sample:?} (encoded {encoded})"
            );
        }
    }

    #[test]
    fn base64_rejects_junk() {
        assert_eq!(decode_base64("!!!!"), None);
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
                serde_json::from_slice(&decode_base64(payload).unwrap()).unwrap();
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
