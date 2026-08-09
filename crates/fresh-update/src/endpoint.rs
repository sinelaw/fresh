//! Where release metadata and release artifacts are allowed to come from.
//!
//! Everything the updater downloads is eventually executed — a package handed
//! to `apt-get`/`dnf` under `sudo`, or a binary swapped over the running
//! executable. So the set of hosts we will fetch from is a security boundary,
//! not a configuration convenience.
//!
//! Two rules, both enforced here rather than at each call site:
//!
//! 1. **https only, host pinned.** A production build fetches from
//!    [`ALLOWED_HOSTS`] over TLS or not at all. This holds after redirects too
//!    ([`crate::net`] re-checks the effective URL), because GitHub 302s release
//!    assets to a CDN and a redirect that downgraded to `http://` would
//!    otherwise be followed silently.
//!
//! 2. **An override is never trusted.** `$FRESH_RELEASES_URL` /
//!    `$FRESH_DOWNLOAD_BASE` exist so the update path can be exercised without
//!    cutting a release. They are also, if left unconstrained, a way to turn
//!    "can set an env var" into "can run code as root": point the base at a
//!    server you control, serve a package *and* the `.sha256` that matches it,
//!    and the checksum proves only that the attacker can do arithmetic. So an
//!    overridden endpoint sets [`Endpoints::trusted`] to `false`, and the
//!    engine refuses to elevate for an untrusted endpoint — it prints the
//!    command instead. The override stays useful for testing; it stops being a
//!    path to `sudo`.
//!
//! Note what this does *not* claim. Pinning the host means an attacker needs
//! GitHub rather than any host; it is not a substitute for signing the
//! artifacts, because a checksum served from the same origin as the payload
//! proves nothing about who produced either. [`crate::attestation`] narrows
//! that gap by asking a *second* pinned origin — `api.github.com` — whether the
//! bytes we got were published under the name we expected, so the asset CDN
//! alone is no longer enough to substitute an artifact. It is still not a
//! signature check; that module says exactly where the line falls.

/// Hosts a production build will fetch release metadata or artifacts from.
///
/// `github.com` issues the download redirect; the `*.githubusercontent.com`
/// hosts are where release assets actually live and are what the redirect
/// lands on.
pub const ALLOWED_HOSTS: &[&str] = &[
    "github.com",
    "api.github.com",
    "objects.githubusercontent.com",
    "release-assets.githubusercontent.com",
];

/// Environment override for the release-metadata URL.
pub const RELEASES_URL_ENV: &str = "FRESH_RELEASES_URL";

/// Environment override for the release-asset base URL.
pub const DOWNLOAD_BASE_ENV: &str = "FRESH_DOWNLOAD_BASE";

/// The GitHub repository releases are published to.
pub const REPO: &str = "sinelaw/fresh";

/// The default release-metadata URL.
pub const DEFAULT_RELEASES_URL: &str = "https://api.github.com/repos/sinelaw/fresh/releases/latest";

/// Why an endpoint was rejected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EndpointError {
    /// The URL did not start with `https://`.
    NotHttps { url: String },
    /// The host is not in [`ALLOWED_HOSTS`].
    HostNotAllowed { host: String },
    /// The URL had no host at all.
    Malformed { url: String },
}

impl std::fmt::Display for EndpointError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EndpointError::NotHttps { url } => {
                write!(f, "refusing a non-https update endpoint: {url}")
            }
            EndpointError::HostNotAllowed { host } => write!(
                f,
                "refusing an update endpoint on {host}; releases are only fetched from {}",
                ALLOWED_HOSTS.join(", ")
            ),
            EndpointError::Malformed { url } => write!(f, "not a usable URL: {url}"),
        }
    }
}

impl std::error::Error for EndpointError {}

/// The host part of an `https://host[:port]/path` URL, lowercased.
///
/// Deliberately minimal rather than pulling in a URL crate: we only ever need
/// the authority, and only to compare it against a fixed list.
pub fn host_of(url: &str) -> Option<String> {
    let rest = url
        .strip_prefix("https://")
        .or_else(|| url.strip_prefix("http://"))?;
    // Strip any userinfo — `https://evil.com@github.com/` has authority
    // `github.com`, but `https://github.com@evil.com/` has authority
    // `evil.com`, and reading left-to-right gets that backwards.
    let authority = rest.split(['/', '?', '#']).next()?;
    let host = authority.rsplit('@').next()?;
    let host = host.split(':').next()?;
    if host.is_empty() {
        None
    } else {
        Some(host.to_ascii_lowercase())
    }
}

/// `true` if `url` is https on an allowlisted host.
pub fn is_trusted(url: &str) -> bool {
    check(url).is_ok()
}

/// Validate a URL against the production policy.
pub fn check(url: &str) -> Result<(), EndpointError> {
    if !url.starts_with("https://") {
        return Err(EndpointError::NotHttps {
            url: url.to_string(),
        });
    }
    let host = host_of(url).ok_or_else(|| EndpointError::Malformed {
        url: url.to_string(),
    })?;
    if ALLOWED_HOSTS.contains(&host.as_str()) {
        Ok(())
    } else {
        Err(EndpointError::HostNotAllowed { host })
    }
}

/// The resolved release endpoints for this run, and whether they are the
/// pinned production ones.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Endpoints {
    /// Where the release metadata JSON is fetched from.
    pub releases_url: String,
    /// Base URL that release asset names are appended to.
    pub download_base: String,
    /// `false` when either endpoint was overridden away from the pinned
    /// defaults. An untrusted endpoint may still be used — that is the point
    /// of the override — but the engine will not run a privileged install with
    /// bytes that came from one.
    pub trusted: bool,
}

impl Default for Endpoints {
    fn default() -> Self {
        Endpoints::production()
    }
}

impl Endpoints {
    /// The pinned defaults: GitHub, over TLS.
    pub fn production() -> Self {
        Endpoints {
            releases_url: DEFAULT_RELEASES_URL.to_string(),
            download_base: format!("https://github.com/{REPO}/releases/download"),
            trusted: true,
        }
    }

    /// Resolve from the environment, applying the policy above.
    ///
    /// An override that satisfies the production policy (https, allowlisted
    /// host) stays trusted — pointing at a different path on `github.com` is
    /// not a downgrade. Anything else is accepted only when the build opted in
    /// via the `insecure-endpoints` feature or is a debug build, and is marked
    /// untrusted either way. In a release build without that feature an
    /// out-of-policy override is refused outright, so a poisoned environment
    /// cannot even redirect the *download*, let alone reach `sudo`.
    pub fn from_env() -> Result<Self, EndpointError> {
        let mut ep = Endpoints::production();
        if let Some(url) = env_override(RELEASES_URL_ENV) {
            ep.trusted &= accept(&url)?;
            ep.releases_url = url;
        }
        if let Some(base) = env_override(DOWNLOAD_BASE_ENV) {
            ep.trusted &= accept(&base)?;
            ep.download_base = base;
        }
        Ok(ep)
    }

    /// The URL of a release asset, given its filename.
    pub fn asset_url(&self, version: &str, file_name: &str) -> String {
        let base = self.download_base.trim_end_matches('/');
        format!("{base}/v{version}/{file_name}")
    }
}

/// Decide whether an overridden endpoint may be used at all.
///
/// `Ok(true)` — within the production policy, still trusted.
/// `Ok(false)` — outside it, but this build permits unpinned endpoints; usable,
/// no longer trusted.
/// `Err` — outside it, and this build does not permit that.
fn accept(url: &str) -> Result<bool, EndpointError> {
    match check(url) {
        Ok(()) => Ok(true),
        Err(e) if overrides_permitted() => {
            tracing::warn!(
                error = %e,
                "using an unpinned update endpoint; privileged installs are disabled for this run"
            );
            Ok(false)
        }
        Err(e) => Err(e),
    }
}

/// `true` when this build permits endpoints outside the production policy.
const fn overrides_permitted() -> bool {
    cfg!(feature = "insecure-endpoints") || cfg!(debug_assertions)
}

fn env_override(key: &str) -> Option<String> {
    std::env::var(key)
        .ok()
        .map(|v| v.trim().to_string())
        .filter(|v| !v.is_empty())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn host_is_read_from_the_authority_not_the_first_thing_that_looks_like_one() {
        assert_eq!(
            host_of("https://github.com/x/y").as_deref(),
            Some("github.com")
        );
        assert_eq!(
            host_of("https://api.github.com:443/repos").as_deref(),
            Some("api.github.com")
        );
        // Userinfo must not be mistaken for the host: this URL fetches from
        // evil.com, and reading up to the first `@` would say `github.com`.
        assert_eq!(
            host_of("https://github.com@evil.com/asset.deb").as_deref(),
            Some("evil.com")
        );
        assert_eq!(host_of("https://").as_deref(), None);
        assert_eq!(host_of("file:///etc/passwd").as_deref(), None);
    }

    #[test]
    fn only_https_on_a_pinned_host_is_trusted() {
        assert!(is_trusted(DEFAULT_RELEASES_URL));
        assert!(is_trusted(
            "https://objects.githubusercontent.com/release/x.deb"
        ));
        // Plain http, even to an allowed host: a redirect that downgrades is
        // the exact case this stops.
        assert!(!is_trusted("http://github.com/sinelaw/fresh"));
        assert!(!is_trusted("https://evil.example/fresh.deb"));
        // Not a prefix match: `github.com.evil.example` is a different host.
        assert!(!is_trusted("https://github.com.evil.example/fresh.deb"));
    }

    #[test]
    fn production_endpoints_satisfy_their_own_policy() {
        let ep = Endpoints::production();
        assert!(ep.trusted);
        check(&ep.releases_url).expect("default releases url must be pinned");
        check(&ep.download_base).expect("default download base must be pinned");
        check(&ep.asset_url("0.4.7", "fresh-editor_0.4.7-1_amd64.deb"))
            .expect("asset urls must stay on a pinned host");
    }

    #[test]
    fn asset_urls_do_not_double_the_separator() {
        let ep = Endpoints {
            releases_url: DEFAULT_RELEASES_URL.to_string(),
            download_base: "https://github.com/x/releases/download/".to_string(),
            trusted: true,
        };
        assert_eq!(
            ep.asset_url("1.0.0", "a.deb"),
            "https://github.com/x/releases/download/v1.0.0/a.deb"
        );
    }
}
