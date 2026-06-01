//! All outbound HTTP(S) for the editor, in one place.
//!
//! Three callers need the network: the update/release checker
//! ([`get_release_json`]), anonymous open-count telemetry
//! ([`post_telemetry`]), and the `editor.httpFetch` plugin API
//! ([`download_to_file`]). Confining every `ureq`/TLS reference to this
//! module means the entire TLS stack (`ureq` + `rustls` + `ring` +
//! `webpki`) can be dropped from the binary by building without the
//! `http` feature — the rest of the editor calls these functions
//! unconditionally and gets the no-op / error stubs below.

use std::path::Path;

/// Maximum size of a body downloaded via `editor.httpFetch`. 64 MB is well
/// above any reasonable theme/plugin asset (themes are tens of KB) while
/// still capping a misbehaving server's blast radius.
#[cfg(feature = "http")]
const HTTP_FETCH_MAX_BYTES: u64 = 64 * 1024 * 1024;

#[cfg(feature = "http")]
mod imp {
    use super::*;
    use std::time::Duration;

    /// GET a release-metadata JSON document (GitHub API shape) and return the
    /// raw body. Used by the update checker.
    pub fn get_release_json(url: &str) -> Result<String, String> {
        let agent = ureq::Agent::config_builder()
            .timeout_global(Some(Duration::from_secs(15)))
            .build()
            .new_agent();
        let response = agent
            .get(url)
            .header("User-Agent", "fresh-editor-update-checker")
            .header("Accept", "application/vnd.github.v3+json")
            .call()
            .map_err(|e| {
                tracing::debug!("HTTP request failed: {}", e);
                format!("HTTP request failed: {}", e)
            })?;

        response
            .into_body()
            .read_to_string()
            .map_err(|e| format!("Failed to read response body: {}", e))
    }

    /// Best-effort, fire-and-forget JSON POST used for anonymous telemetry.
    /// Spawns a detached thread and never reports failures.
    pub fn post_telemetry(url: &'static str, body: String) {
        #[allow(clippy::let_underscore_must_use)]
        let _ = std::thread::spawn(move || {
            let agent = ureq::Agent::config_builder()
                .timeout_global(Some(Duration::from_secs(5)))
                .build()
                .new_agent();
            #[allow(clippy::let_underscore_must_use)]
            let _ = agent
                .post(url)
                .header("Content-Type", "application/json")
                .send(body.as_bytes());
        });
    }

    /// Fetch a URL over HTTP(S) and stream the response body into `target`.
    ///
    /// Returns the HTTP status code on success. Non-2xx responses are returned
    /// as their status code without writing to the target file. Transport
    /// errors (DNS, TLS, timeout, …) are returned as `Err`.
    pub fn download_to_file(url: &str, target: &Path) -> Result<u16, String> {
        // Use the platform's native certificate verifier so requests work in
        // environments with TLS-intercepting proxies or custom enterprise root
        // CAs that aren't in Mozilla's bundled webpki-roots.
        let tls_config = ureq::tls::TlsConfig::builder()
            .root_certs(ureq::tls::RootCerts::PlatformVerifier)
            .build();

        let agent = ureq::Agent::config_builder()
            .timeout_global(Some(Duration::from_secs(30)))
            .http_status_as_error(false)
            .tls_config(tls_config)
            .build()
            .new_agent();

        let response = agent
            .get(url)
            .header("User-Agent", "fresh-editor")
            .call()
            .map_err(|e| format!("HTTP request failed: {}", e))?;

        let status = response.status().as_u16();
        if !(200..300).contains(&status) {
            return Ok(status);
        }

        let mut file = std::fs::File::create(target)
            .map_err(|e| format!("failed to create {}: {}", target.display(), e))?;

        let mut reader = response
            .into_body()
            .into_with_config()
            .limit(HTTP_FETCH_MAX_BYTES)
            .reader();

        std::io::copy(&mut reader, &mut file)
            .map_err(|e| format!("failed to write response body: {}", e))?;

        Ok(status)
    }
}

#[cfg(not(feature = "http"))]
mod imp {
    use super::*;

    const DISABLED: &str = "HTTP support not compiled in (built without the `http` feature)";

    pub fn get_release_json(_url: &str) -> Result<String, String> {
        Err(DISABLED.to_string())
    }

    pub fn post_telemetry(_url: &'static str, _body: String) {}

    pub fn download_to_file(_url: &str, _target: &Path) -> Result<u16, String> {
        Err(DISABLED.to_string())
    }
}

pub use imp::{download_to_file, get_release_json, post_telemetry};
