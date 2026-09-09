//! Where release metadata and artifacts are allowed to come from.
//!
//! Everything the updater downloads is eventually executed, so the set of
//! hosts it will fetch from is a security boundary rather than a convenience.
//! Two rules, enforced here instead of at each call site.
//!
//! **https only, host pinned.** Checked again after redirects by
//! [`crate::net`], because GitHub 302s assets to a CDN and a redirect that
//! downgraded to `http://` would otherwise be followed silently.
//!
//! **An override off the pinned hosts is never trusted.** The overrides exist
//! so the update path can be exercised without cutting a release.
//! Unconstrained they would turn "can set an env var" into "can run code as
//! root": point the base at your own server, serve a package and a matching
//! `.sha256`, and the checksum proves only that you can do arithmetic. So an
//! override that leaves the allowlist marks the endpoints untrusted and the
//! engine prints the command rather than elevating. One that stays on it —
//! another path on `api.github.com`, say — is not a downgrade and keeps its
//! trust, whether it arrived by env var or by flag.
//!
//! Pinning means an attacker needs GitHub rather than any host. It is not a
//! substitute for signing — [`crate::attestation`] narrows that gap with a
//! second origin, and says where the remaining line falls.

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

/// How many releases to ask for when listing. Enough that the newest
/// pre-release is always on the page, small enough that the document stays
/// far below the feed size cap as releases accumulate.
const RELEASE_PAGE: u32 = 10;

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
    /// Where the `releases/latest` redirect that names the newest release
    /// lives — the route that costs no API budget (see [`crate::fetch`]).
    ///
    /// Derived from [`Endpoints::download_base`], because that is where it
    /// sits: GitHub serves `…/releases/latest` beside `…/releases/download`,
    /// and so does anything mirroring that layout. `None` once a feed has been
    /// named explicitly — a caller who points `--releases-url` at a document
    /// chose where versions come from, and resolving them somewhere else would
    /// ignore that — or when the base has a shape we do not recognise.
    pub redirect_url: Option<String>,
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
        let download_base = format!("https://github.com/{REPO}/releases/download");
        Endpoints {
            releases_url: DEFAULT_RELEASES_URL.to_string(),
            redirect_url: redirect_beside(&download_base),
            download_base,
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
            ep.set_releases_url(url)?;
        }
        if let Some(base) = env_override(DOWNLOAD_BASE_ENV) {
            ep.set_download_base(base)?;
        }
        Ok(ep)
    }

    /// Point the release feed elsewhere, under the policy above.
    ///
    /// The `--releases-url` flag lands here so it is judged by where it points
    /// rather than by which channel set it: another path on an allowlisted host
    /// is not a downgrade and stays trusted, and anything outside the policy is
    /// refused in a release build exactly as an env override would be.
    pub fn set_releases_url(&mut self, url: String) -> Result<(), EndpointError> {
        self.trusted &= accept(&url)?;
        self.releases_url = url;
        // A named feed is the answer to "which release?", so the redirect is
        // no longer consulted — whichever order the two overrides arrive in.
        self.redirect_url = None;
        Ok(())
    }

    /// Point asset downloads elsewhere, under the same policy.
    ///
    /// The redirect moves with them: it lives beside the downloads, so a base
    /// that keeps GitHub's layout keeps the cheap route to the version, and one
    /// that does not falls back to the feed.
    pub fn set_download_base(&mut self, base: String) -> Result<(), EndpointError> {
        self.trusted &= accept(&base)?;
        if self.redirect_url.is_some() {
            self.redirect_url = redirect_beside(&base);
        }
        self.download_base = base;
        Ok(())
    }

    /// The same repository's release list, which carries pre-releases too.
    ///
    /// One list, one flag per entry — `/releases/latest` is just GitHub
    /// pre-filtering it. Opting in therefore changes the endpoint, not the
    /// source.
    ///
    /// Bounded deliberately. The default page is 30 releases, which for this
    /// project is already ~2.9 MB and grows by ~95 KB per release, so an
    /// unbounded list would meet [`crate::net::FEED_MAX_BYTES`] after roughly
    /// 85 releases and start failing for no reason a user could see. Only the
    /// newest few can win the comparison anyway.
    ///
    /// An overridden URL is returned untouched: it may already be a tag
    /// endpoint or a mirror's own document, and rewriting it would be guessing
    /// at a shape we were handed.
    pub fn list_url(&self) -> String {
        match self.releases_url.strip_suffix("/latest") {
            Some(base) if self.trusted => format!("{base}?per_page={RELEASE_PAGE}"),
            _ => self.releases_url.clone(),
        }
    }

    /// The web redirect that names the latest release without spending any of
    /// the API's rate-limit budget: `…/releases/latest` answers 302 with that
    /// release's page, and the tag is in the `Location` header.
    ///
    /// See [`Endpoints::redirect_url`] for when there is one, and
    /// [`crate::fetch`] for what it can and cannot answer.
    pub fn latest_redirect_url(&self) -> Option<String> {
        self.redirect_url.clone()
    }

    /// The URL of a release asset, given its filename.
    pub fn asset_url(&self, version: &str, file_name: &str) -> String {
        let base = self.download_base.trim_end_matches('/');
        format!("{base}/v{version}/{file_name}")
    }
}

/// The `releases/latest` redirect that sits beside a `releases/download` base.
///
/// One rule, applied to whatever base is in effect: replace the final
/// `download` segment with `latest`. It yields GitHub's own redirect for the
/// pinned base, the matching one for a mirror that keeps that layout, and
/// `None` for a base shaped some other way — which reads as "ask the feed".
fn redirect_beside(download_base: &str) -> Option<String> {
    let base = download_base.trim_end_matches('/');
    base.strip_suffix("/download")
        .map(|stem| format!("{stem}/latest"))
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
mod endpoint_list_tests {
    use super::*;

    #[test]
    fn the_pinned_default_yields_the_list_endpoint() {
        let ep = Endpoints::production();
        assert_eq!(
            ep.list_url(),
            format!("https://api.github.com/repos/{REPO}/releases?per_page={RELEASE_PAGE}")
        );
        assert!(is_trusted(&ep.list_url()));
    }

    /// An override may already be a tag endpoint or a mirror's own document;
    /// rewriting it would guess at a shape we were handed.
    #[test]
    fn an_override_is_left_alone() {
        let mut ep = Endpoints::production();
        ep.releases_url = "https://example.invalid/feed/latest".to_string();
        ep.trusted = false;
        assert_eq!(ep.list_url(), "https://example.invalid/feed/latest");
    }
}

#[cfg(test)]
mod redirect_tests {
    use super::*;

    #[test]
    fn the_pinned_default_resolves_versions_through_the_web_redirect() {
        let url = Endpoints::production()
            .latest_redirect_url()
            .expect("the pinned endpoints have one");
        assert_eq!(url, format!("https://github.com/{REPO}/releases/latest"));
        check(&url).expect("the redirect must satisfy the same policy");
    }

    /// A named feed is the caller's answer to "which release?", whichever
    /// order the two overrides arrive in.
    #[test]
    fn naming_a_feed_takes_the_redirect_out_of_play() {
        let mut ep = Endpoints::production();
        ep.set_releases_url(format!(
            "https://api.github.com/repos/{REPO}/releases/tags/v0.4.7"
        ))
        .expect("an allowlisted host is within policy");
        assert_eq!(ep.latest_redirect_url(), None);

        // …and the download base moving afterwards does not bring it back.
        ep.set_download_base(format!("https://github.com/{REPO}/releases/download"))
            .expect("within policy");
        assert_eq!(ep.latest_redirect_url(), None);
    }

    /// The redirect lives beside the downloads, so it follows them: a mirror
    /// keeping GitHub's layout keeps the route that costs no API budget.
    #[test]
    fn the_redirect_follows_the_download_base() {
        let mut ep = Endpoints::production();
        ep.set_download_base("https://github.com/other/repo/releases/download".to_string())
            .expect("an allowlisted host is within policy");
        assert_eq!(
            ep.latest_redirect_url().as_deref(),
            Some("https://github.com/other/repo/releases/latest")
        );

        // A base shaped some other way has no redirect beside it to guess at.
        let mut ep = Endpoints::production();
        match ep.set_download_base("https://github.com/other/repo/dl".to_string()) {
            Ok(()) => assert_eq!(ep.latest_redirect_url(), None),
            Err(e) => panic!("an allowlisted host should be accepted: {e}"),
        }
    }

    #[test]
    fn a_trailing_slash_does_not_hide_the_redirect() {
        assert_eq!(
            redirect_beside("https://github.com/a/b/releases/download/").as_deref(),
            Some("https://github.com/a/b/releases/latest")
        );
        assert_eq!(redirect_beside("https://example.invalid/files"), None);
    }
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

    /// Trust follows where an override points, not which channel set it — the
    /// CLI flags resolve through the same `accept` as the env vars, so a feed
    /// on an allowlisted host still gets the attestation check.
    #[test]
    fn an_override_within_policy_stays_trusted() {
        let mut ep = Endpoints::production();
        ep.set_releases_url(format!(
            "https://api.github.com/repos/{REPO}/releases/tags/v0.4.7"
        ))
        .expect("an allowlisted host is within policy");
        assert!(ep.trusted);

        // And one that leaves the allowlist does not, whether this build
        // refuses it outright or merely demotes it.
        let mut ep = Endpoints::production();
        match ep.set_download_base("https://evil.example/dl".to_string()) {
            Ok(()) => assert!(!ep.trusted, "an out-of-policy base must not stay trusted"),
            Err(e) => assert!(matches!(e, EndpointError::HostNotAllowed { .. })),
        }
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
            redirect_url: None,
            trusted: true,
        };
        assert_eq!(
            ep.asset_url("1.0.0", "a.deb"),
            "https://github.com/x/releases/download/v1.0.0/a.deb"
        );
    }
}
