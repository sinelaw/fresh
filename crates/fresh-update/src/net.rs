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
//!
//! # Rate limiting is a distinct outcome, not a failure like any other
//!
//! `api.github.com` allows an unauthenticated client 60 requests an hour *per
//! source address*, a budget shared by everyone behind the same NAT — so "403"
//! from that host usually means someone else spent it. [`crate::fetch`] keeps
//! the updater off the API for everything but the attestation lookup, and what
//! is left is reported as [`FetchError::RateLimited`] rather than a bare
//! status, because the answer ("wait, or bring a token") is nothing like the
//! answer to a real 403.
//!
//! A token from the environment is attached to `api.github.com` requests only.
//! `ureq`'s default is to drop `Authorization` across a redirect, so a hop to
//! the asset CDN cannot carry it either.

use crate::endpoint::{self, Endpoints};
use std::path::Path;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

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

/// The host whose requests count against GitHub's API rate limit — and the
/// only host a token is ever sent to.
const API_HOST: &str = "api.github.com";

/// Environment variables consulted for a GitHub API token, in order.
///
/// The fresh-specific name comes first so a user can raise the updater's limit
/// without handing it whatever token the rest of their shell is carrying; the
/// two conventional names follow because most machines that have a token
/// already have it under one of them.
pub const TOKEN_ENVS: &[&str] = &["FRESH_GITHUB_TOKEN", "GITHUB_TOKEN", "GH_TOKEN"];

/// Why a request did not produce bytes we can use.
///
/// Rate limiting is called out as its own variant: it is the one failure here
/// that is neither the user's fault nor a sign that anything is wrong, it
/// clears by itself, and both remedies (wait, or set a token) are things only
/// the message can convey.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FetchError {
    /// GitHub refused the request because the API budget for this address —
    /// or for this token — is spent.
    RateLimited {
        /// The request that was refused.
        url: String,
        /// How long until the budget resets, when GitHub said.
        wait: Option<Duration>,
        /// Whether a token was attached. Without one the remedy is to bring
        /// one; with one, the limit is already the higher one and waiting is
        /// all that is left.
        authenticated: bool,
    },
    /// Any other non-2xx status.
    Status {
        /// The request that was refused.
        url: String,
        /// The status code returned.
        status: u16,
    },
    /// The request never completed: DNS, TLS, timeout, connection reset.
    Transport {
        /// The request that failed.
        url: String,
        /// The transport's own description.
        detail: String,
    },
    /// A response arrived but its body could not be read — most often the
    /// size cap above.
    Body {
        /// The request whose body could not be read.
        url: String,
        /// What went wrong reading or writing it.
        detail: String,
    },
}

impl FetchError {
    /// Whether this is GitHub's rate limit rather than a real failure.
    ///
    /// Callers with a route around the API — [`crate::fetch`] reading the
    /// version from the release redirect instead — key off this.
    pub fn is_rate_limited(&self) -> bool {
        matches!(self, FetchError::RateLimited { .. })
    }
}

impl std::fmt::Display for FetchError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FetchError::RateLimited {
                url,
                wait,
                authenticated,
            } => {
                write!(
                    f,
                    "GitHub's API rate limit is used up, so {url} was refused"
                )?;
                if let Some(wait) = wait {
                    write!(f, "; it resets in about {}", humanize(*wait))?;
                }
                write!(f, ".")?;
                if !authenticated {
                    write!(
                        f,
                        " Unauthenticated callers get 60 requests an hour per IP address, \
                         shared with everyone else behind it. Set {} to a personal access \
                         token — it needs no scopes for a public repository — to get 5000.",
                        TOKEN_ENVS.join(" or ")
                    )?;
                }
                Ok(())
            }
            FetchError::Status { url, status } => write!(f, "HTTP {status} fetching {url}"),
            FetchError::Transport { url, detail } => write!(f, "fetching {url}: {detail}"),
            FetchError::Body { url, detail } => write!(f, "reading {url}: {detail}"),
        }
    }
}

impl std::error::Error for FetchError {}

impl From<FetchError> for String {
    fn from(e: FetchError) -> String {
        e.to_string()
    }
}

/// A duration as a person would say it, for "try again in …".
fn humanize(d: Duration) -> String {
    let secs = d.as_secs();
    if secs < 90 {
        return format!("{secs} seconds");
    }
    let mins = secs.div_ceil(60);
    if mins < 90 {
        format!("{mins} minutes")
    } else {
        format!("{} hours", mins.div_ceil(60))
    }
}

/// An HTTP client configured for one set of release endpoints.
pub struct Transport {
    agent: ureq::Agent,
    /// A GitHub token from the environment, if any. Sent to [`API_HOST`] only.
    token: Option<String>,
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
            // which status it was; `classify` turns it back into an error with
            // the code — or the rate limit — in the message.
            .http_status_as_error(false)
            .tls_config(tls_config)
            .build()
            .new_agent();

        Transport {
            agent,
            token: token_from_env(),
        }
    }

    /// Whether a token was found, and so which rate limit applies.
    pub fn authenticated(&self) -> bool {
        self.token.is_some()
    }

    /// The same client with no token, whatever the environment holds.
    ///
    /// Tests of the unauthenticated path must not depend on whether the
    /// machine running them happens to export a `GITHUB_TOKEN` — on CI it
    /// usually does, and the two paths report different things.
    #[cfg(test)]
    pub(crate) fn without_token(mut self) -> Self {
        self.token = None;
        self
    }

    /// GET `url` and return the body as text, refusing anything over `limit`.
    pub fn get_text(&self, url: &str, limit: u64) -> Result<String, FetchError> {
        let response = self.call(url)?;
        read_text(url, response, limit)
    }

    /// GET `url` and stream the body into `target`, refusing anything over
    /// `limit`.
    pub fn download(&self, url: &str, target: &Path, limit: u64) -> Result<(), FetchError> {
        let response = self.call(url)?;
        let mut file = std::fs::File::create(target).map_err(|e| FetchError::Body {
            url: url.to_string(),
            detail: format!("creating {}: {e}", target.display()),
        })?;
        let mut reader = response
            .into_body()
            .into_with_config()
            .limit(limit)
            .reader();
        std::io::copy(&mut reader, &mut file).map_err(|e| FetchError::Body {
            url: url.to_string(),
            detail: format!("writing {}: {e}", target.display()),
        })?;
        Ok(())
    }

    /// GET `url` and return the body as text, treating a 404 as `Ok(None)`
    /// rather than an error.
    ///
    /// Only for endpoints where "absent" is a real answer the caller must act
    /// on rather than a failure to report — the attestation lookup, where a
    /// 404 means GitHub holds no attestation for that digest and the caller
    /// turns it into a specific, actionable refusal.
    pub fn get_text_optional(&self, url: &str, limit: u64) -> Result<Option<String>, FetchError> {
        let response = self.send(url)?;
        if response.status().as_u16() == 404 {
            return Ok(None);
        }
        if let Some(e) = self.classify(url, &response) {
            return Err(e);
        }
        read_text(url, response, limit).map(Some)
    }

    /// GET `url` **without** following redirects, returning where it points.
    ///
    /// `Ok(None)` means the response was not a redirect. This exists for
    /// [`crate::fetch`]: `github.com/<repo>/releases/latest` names the newest
    /// release in its `Location` header, which answers "what is the latest
    /// version?" without spending any of the API budget the feed spends.
    pub fn redirect_location(&self, url: &str) -> Result<Option<String>, FetchError> {
        let response = self
            .request(url)
            .config()
            // 0 means "return the redirect rather than following it", which is
            // the whole point: we want the header, not the page.
            .max_redirects(0)
            .build()
            .call()
            .map_err(|e| FetchError::Transport {
                url: url.to_string(),
                detail: e.to_string(),
            })?;
        let status = response.status().as_u16();
        if !(300..400).contains(&status) {
            if let Some(e) = self.classify(url, &response) {
                return Err(e);
            }
            return Ok(None);
        }
        Ok(response
            .headers()
            .get("location")
            .and_then(|v| v.to_str().ok())
            .map(|v| v.trim().to_string())
            .filter(|v| !v.is_empty()))
    }

    fn call(&self, url: &str) -> Result<ureq::http::Response<ureq::Body>, FetchError> {
        let response = self.send(url)?;
        match self.classify(url, &response) {
            Some(e) => Err(e),
            None => Ok(response),
        }
    }

    /// The request itself, with no opinion about the status code.
    fn send(&self, url: &str) -> Result<ureq::http::Response<ureq::Body>, FetchError> {
        self.request(url).call().map_err(|e| FetchError::Transport {
            url: url.to_string(),
            detail: e.to_string(),
        })
    }

    /// The common request shape: our headers, plus a token when — and only
    /// when — the request is going to the API host over https.
    fn request(&self, url: &str) -> ureq::RequestBuilder<ureq::typestate::WithoutBody> {
        let request = self
            .agent
            .get(url)
            .header("User-Agent", USER_AGENT)
            .header("Accept", "application/vnd.github.v3+json");
        match self.token_for(url) {
            Some(token) => request.header("Authorization", &format!("Bearer {token}")),
            None => request,
        }
    }

    /// The token to send with `url`, if any.
    ///
    /// Pinned to the API host over https: a token is a credential, and the
    /// asset CDN, a mirror named by `--releases-url`, or a plaintext test
    /// server have no business seeing one. `ureq` drops `Authorization` across
    /// redirects by default, so a hop off this host cannot carry it either.
    fn token_for(&self, url: &str) -> Option<&str> {
        let host = endpoint::host_of(url)?;
        if host == API_HOST && url.starts_with("https://") {
            self.token.as_deref()
        } else {
            None
        }
    }

    /// The error this response represents, or `None` if it is a 2xx.
    fn classify(
        &self,
        url: &str,
        response: &ureq::http::Response<ureq::Body>,
    ) -> Option<FetchError> {
        let status = response.status().as_u16();
        if (200..300).contains(&status) {
            return None;
        }
        if let Some(wait) = rate_limit_wait(status, response.headers()) {
            return Some(FetchError::RateLimited {
                url: url.to_string(),
                wait,
                authenticated: self.authenticated(),
            });
        }
        Some(FetchError::Status {
            url: url.to_string(),
            status,
        })
    }
}

fn read_text(
    url: &str,
    response: ureq::http::Response<ureq::Body>,
    limit: u64,
) -> Result<String, FetchError> {
    response
        .into_body()
        .into_with_config()
        .limit(limit)
        .read_to_string()
        .map_err(|e| FetchError::Body {
            url: url.to_string(),
            detail: e.to_string(),
        })
}

/// `Some(wait)` when this response is GitHub saying "you have had enough".
///
/// Both of GitHub's limits are reported as 403 (older) or 429 (newer), so the
/// status alone does not distinguish a rate limit from a genuine refusal. The
/// headers do: the primary limit sets `x-ratelimit-remaining: 0` and a reset
/// timestamp, and the secondary limit sets `retry-after`. Requiring one of
/// those keeps a real 403 — a private repository, a bad token — reported as
/// what it is.
fn rate_limit_wait(status: u16, headers: &ureq::http::HeaderMap) -> Option<Option<Duration>> {
    if status != 403 && status != 429 {
        return None;
    }
    let header = |name: &str| {
        headers
            .get(name)
            .and_then(|v| v.to_str().ok())
            .map(str::trim)
            .filter(|v| !v.is_empty())
    };

    let exhausted = header("x-ratelimit-remaining") == Some("0");
    let retry_after = header("retry-after").and_then(|v| v.parse::<u64>().ok());
    if !exhausted && retry_after.is_none() {
        return None;
    }

    let wait = retry_after
        .map(Duration::from_secs)
        .or_else(|| {
            let reset: u64 = header("x-ratelimit-reset")?.parse().ok()?;
            let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs();
            Some(Duration::from_secs(reset.saturating_sub(now)))
        })
        // A reset already in the past says nothing useful, and "resets in
        // about 0 seconds" reads as a bug rather than as advice.
        .filter(|d| !d.is_zero());
    Some(wait)
}

/// The first non-empty token in [`TOKEN_ENVS`].
fn token_from_env() -> Option<String> {
    TOKEN_ENVS.iter().find_map(|key| {
        std::env::var(key)
            .ok()
            .map(|v| v.trim().to_string())
            .filter(|v| !v.is_empty())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn headers(pairs: &[(&str, &str)]) -> ureq::http::HeaderMap {
        let mut map = ureq::http::HeaderMap::new();
        for (name, value) in pairs {
            map.insert(
                ureq::http::HeaderName::from_bytes(name.as_bytes()).expect("header name"),
                ureq::http::HeaderValue::from_str(value).expect("header value"),
            );
        }
        map
    }

    /// GitHub reports both of its limits as 403/429, so the status alone must
    /// not decide: a genuine refusal (a private repository, a bad token) has
    /// to stay reported as a refusal.
    #[test]
    fn only_a_403_with_the_rate_limit_headers_is_a_rate_limit() {
        assert_eq!(
            rate_limit_wait(403, &headers(&[("x-ratelimit-remaining", "0")])),
            Some(None),
            "the primary limit is remaining=0"
        );
        assert_eq!(
            rate_limit_wait(429, &headers(&[("retry-after", "120")])),
            Some(Some(Duration::from_secs(120))),
            "the secondary limit is retry-after"
        );
        assert_eq!(
            rate_limit_wait(403, &headers(&[("x-ratelimit-remaining", "57")])),
            None,
            "a 403 with budget left is a real refusal"
        );
        assert_eq!(rate_limit_wait(403, &headers(&[])), None);
        assert_eq!(
            rate_limit_wait(404, &headers(&[("retry-after", "60")])),
            None
        );
    }

    #[test]
    fn a_reset_timestamp_becomes_a_wait() {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock")
            .as_secs();
        let wait = rate_limit_wait(
            403,
            &headers(&[
                ("x-ratelimit-remaining", "0"),
                ("x-ratelimit-reset", &(now + 600).to_string()),
            ]),
        )
        .expect("rate limited")
        .expect("a wait");
        assert!(
            wait <= Duration::from_secs(600) && wait > Duration::from_secs(590),
            "got {wait:?}"
        );
        // A reset in the past is no advice at all.
        assert_eq!(
            rate_limit_wait(
                403,
                &headers(&[("x-ratelimit-remaining", "0"), ("x-ratelimit-reset", "1"),]),
            ),
            Some(None)
        );
    }

    /// A token is a credential. It goes to the API host over https and
    /// nowhere else — not to the asset CDN a download redirects to, not to a
    /// mirror named by `--releases-url`, not to a plaintext test server.
    #[test]
    fn a_token_is_only_ever_sent_to_the_api_host() {
        let mut transport = Transport::new(&Endpoints::production());
        transport.token = Some("secret".to_string());

        assert_eq!(
            transport.token_for("https://api.github.com/repos/x/y/releases/latest"),
            Some("secret")
        );
        assert_eq!(
            transport.token_for("https://github.com/x/y/releases/download/v1/a"),
            None
        );
        assert_eq!(
            transport.token_for("https://objects.githubusercontent.com/a"),
            None
        );
        assert_eq!(transport.token_for("http://api.github.com/repos/x/y"), None);
        // Userinfo must not smuggle the host past the check.
        assert_eq!(
            transport.token_for("https://api.github.com@evil.example/repos"),
            None
        );
    }

    #[test]
    fn a_wait_is_phrased_the_way_a_person_would_say_it() {
        assert_eq!(humanize(Duration::from_secs(30)), "30 seconds");
        assert_eq!(humanize(Duration::from_secs(600)), "10 minutes");
        assert_eq!(humanize(Duration::from_secs(3600)), "60 minutes");
        assert_eq!(humanize(Duration::from_secs(3600 * 5)), "5 hours");
    }
}
