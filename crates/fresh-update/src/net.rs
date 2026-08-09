//! The one HTTP client the updater uses.
//!
//! Feed, checksum sidecar and artifact all go through here so they share one
//! TLS configuration, redirect policy and set of size limits. That sharing is
//! the point: the previous arrangement fetched the artifact with the platform
//! verifier and the checksum with bundled webpki roots, so behind a
//! TLS-intercepting proxy the download succeeded and its checksum fetch
//! failed, which — verification being fail-closed — made the updater unusable.
//! Verifying an artifact and the statement about it against two different
//! trust anchors is also not one coherent decision.
//!
//! `ureq` is built with `http_status_as_error(false)`, so a 404 arrives as a
//! successful call yielding an error page and every status is checked here.
//! Bodies are capped: an unbounded read of a response we did not produce is
//! TUF's *endless data* attack.
//!
//! A redirect between two https hosts is still followed, because GitHub's
//! asset CDN varies. Transport strictness is not the answer to "who produced
//! these bytes"; see [`crate::attestation`].

use crate::endpoint::Endpoints;
use std::path::Path;
use std::time::Duration;

/// Cap on the release-metadata document. GitHub's release JSON runs to a few
/// hundred KB with long changelogs; 8 MiB is far above that and far below
/// anything that would hurt.
pub const FEED_MAX_BYTES: u64 = 8 * 1024 * 1024;

/// Cap on a `.sha256` sidecar. It holds one hex digest and maybe a filename.
pub const SIDECAR_MAX_BYTES: u64 = 8 * 1024;

/// Cap on a downloaded release artifact.
pub const ASSET_MAX_BYTES: u64 = 512 * 1024 * 1024;

/// Total time budget for a single request, including redirects.
const TIMEOUT: Duration = Duration::from_secs(60);

/// Redirects to follow. GitHub uses one hop from `github.com` to the asset CDN;
/// anything past a handful is a loop or a redirect chain being used to walk us
/// somewhere.
const MAX_REDIRECTS: u32 = 4;

const USER_AGENT: &str = "fresh-editor-updater";

/// An HTTP client configured for one set of release endpoints.
pub struct Transport {
    agent: ureq::Agent,
}

impl Transport {
    /// Build a client for `endpoints`.
    ///
    /// `https_only` tracks [`Endpoints::trusted`]: a production run refuses
    /// plaintext outright, including across redirects. A run against an
    /// overridden endpoint has already been marked untrusted — and the engine
    /// will not perform a privileged install with what it fetches — so it is
    /// permitted to speak http to a local test server.
    pub fn new(endpoints: &Endpoints) -> Self {
        let tls_config = ureq::tls::TlsConfig::builder()
            .root_certs(ureq::tls::RootCerts::PlatformVerifier)
            .build();

        let agent = ureq::Agent::config_builder()
            .timeout_global(Some(TIMEOUT))
            .https_only(endpoints.trusted)
            .max_redirects(MAX_REDIRECTS)
            // A non-2xx must reach us as a status, not as an `Err` that hides
            // which status it was; `status_checked` turns it back into an
            // error with the code in the message.
            .http_status_as_error(false)
            .tls_config(tls_config)
            .build()
            .new_agent();

        Transport { agent }
    }

    /// GET `url` and return the body as text, refusing anything over `limit`.
    pub fn get_text(&self, url: &str, limit: u64) -> Result<String, String> {
        let response = self.call(url)?;
        response
            .into_body()
            .into_with_config()
            .limit(limit)
            .read_to_string()
            .map_err(|e| format!("reading {url}: {e}"))
    }

    /// GET `url` and stream the body into `target`, refusing anything over
    /// `limit`.
    pub fn download(&self, url: &str, target: &Path, limit: u64) -> Result<(), String> {
        let response = self.call(url)?;
        let mut file = std::fs::File::create(target)
            .map_err(|e| format!("creating {}: {e}", target.display()))?;
        let mut reader = response
            .into_body()
            .into_with_config()
            .limit(limit)
            .reader();
        std::io::copy(&mut reader, &mut file)
            .map_err(|e| format!("writing {}: {e}", target.display()))?;
        Ok(())
    }

    /// GET `url` and return the body as text, treating a 404 as `Ok(None)`
    /// rather than an error.
    ///
    /// Only for endpoints where "absent" is a real answer the caller must act
    /// on rather than a failure to report — the attestation lookup, where a
    /// 404 means GitHub holds no attestation for that digest and the caller
    /// turns it into a specific, actionable refusal.
    pub fn get_text_optional(&self, url: &str, limit: u64) -> Result<Option<String>, String> {
        let response = self.send(url)?;
        let status = response.status().as_u16();
        if status == 404 {
            return Ok(None);
        }
        if !(200..300).contains(&status) {
            return Err(format!("HTTP {status} fetching {url}"));
        }
        response
            .into_body()
            .into_with_config()
            .limit(limit)
            .read_to_string()
            .map(Some)
            .map_err(|e| format!("reading {url}: {e}"))
    }

    fn call(&self, url: &str) -> Result<ureq::http::Response<ureq::Body>, String> {
        let response = self.send(url)?;
        let status = response.status().as_u16();
        if !(200..300).contains(&status) {
            return Err(format!("HTTP {status} fetching {url}"));
        }
        Ok(response)
    }

    /// The request itself, with no opinion about the status code.
    fn send(&self, url: &str) -> Result<ureq::http::Response<ureq::Body>, String> {
        self.agent
            .get(url)
            .header("User-Agent", USER_AGENT)
            .header("Accept", "application/vnd.github.v3+json")
            .call()
            .map_err(|e| format!("fetching {url}: {e}"))
    }
}
