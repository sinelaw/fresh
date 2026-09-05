//! Which release to offer, resolved the way the install script resolves it.
//!
//! `scripts/install.sh` never touches `api.github.com`. It asks
//! `github.com/<repo>/releases/latest`, which answers 302 with the newest
//! release's page in its `Location` header, and builds every asset URL from
//! the tag it reads there. That is why `curl … install.sh | sh` succeeds on a
//! machine where `fresh --cmd update` was just refused with a 403: the API
//! gives an unauthenticated caller 60 requests an hour *per source address*,
//! shared with everyone behind the same NAT, and the web host has no such
//! budget.
//!
//! So the updater resolves the version the same way: one redirect, no API
//! budget, same host allowlist, still https. `--check`, the editor's daily
//! background check and a self-contained update therefore spend nothing at all
//! on the API, and cannot be refused by a limit somebody else used up.
//!
//! # What the redirect cannot answer, and what happens then
//!
//! It names a tag and nothing else. Two things need more, and both fall back
//! to the feed on `api.github.com`:
//!
//! * **`--pre`.** The redirect points at the newest *full* release by
//!   definition, so a pre-release can only come from the list endpoint. This
//!   is opt-in and rare.
//! * **A package whose filename we cannot construct.** `.deb`/`.rpm`/`.flatpak`
//!   names come out of [`crate::feed::package_file_name`], the same
//!   construction the install script does — and [`crate::engine`] falls back to
//!   the feed if the release does not actually publish that name.
//!
//! An overridden endpoint (`--releases-url`, a mirror, a test server) keeps
//! using its feed: it named a document, and reinterpreting that as "ask GitHub
//! instead" would ignore what the caller asked for.
//!
//! The one request that has no second route is the attestation lookup
//! ([`crate::attestation`]), which is a *different origin* on purpose — that is
//! the entire value it adds over the checksum sidecar. It is spent once, when
//! an update actually installs, and never by a check.
//!
//! # The guarantee the flag used to carry
//!
//! [`crate::feed::select`] refuses a pre-release unless asked, rather than
//! relying on `/releases/latest` to omit one. A tag carries no such flag, so
//! that check is made against the version itself: a tag that parses as a
//! pre-release is refused here exactly as the flag would be. Drafts need no
//! equivalent — they are not public, so no redirect reaches one.

use crate::endpoint::{self, Endpoints};
use crate::feed::Release;
use crate::net::{self, Transport};

/// Where the release we are about to act on came from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Source {
    /// The release-metadata feed: full detail, assets included.
    Api,
    /// The `releases/latest` redirect: the version, and nothing else.
    ReleaseRedirect,
}

impl Source {
    /// Whether this source lists the release's published assets.
    pub fn lists_assets(self) -> bool {
        matches!(self, Source::Api)
    }
}

/// A release, and how much we know about it.
#[derive(Debug, Clone)]
pub struct Fetched {
    /// The release to offer.
    pub release: Release,
    /// Where it came from — see [`Source::lists_assets`].
    pub source: Source,
}

/// The release to offer.
///
/// Tries the redirect first on the pinned endpoints, and the feed when that is
/// not available (an override, `--pre`) or did not work. Failing over in that
/// direction matters: the redirect is one header on a host that also serves
/// the download, so if it stops answering, the API is still there.
pub fn latest(
    transport: &Transport,
    endpoints: &Endpoints,
    allow_prerelease: bool,
) -> Result<Fetched, String> {
    if !allow_prerelease {
        if let Some(redirect) = endpoints.latest_redirect_url() {
            match latest_tag(transport, &redirect) {
                Ok(tag) => return from_redirect_tag(tag),
                // Not fatal: the feed answers the same question, and is what
                // every overridden endpoint uses anyway.
                Err(e) => {
                    tracing::warn!(error = %e, "release redirect unusable; asking the API feed")
                }
            }
        }
    }

    Ok(Fetched {
        release: from_feed(transport, endpoints, allow_prerelease)?,
        source: Source::Api,
    })
}

/// The release a redirect tag names, with the guarantee the feed's flag used
/// to carry.
///
/// GitHub should never point `releases/latest` at a pre-release, so this is
/// the same defence `feed::select` makes against a feed that does: the refusal
/// is ours, derived from the version, rather than a behaviour we are trusting.
fn from_redirect_tag(tag: String) -> Result<Fetched, String> {
    if crate::version::is_prerelease(&tag) {
        return Err(format!(
            "the newest release is {}, a pre-release; pass --pre to install it",
            tag.trim_start_matches('v')
        ));
    }
    Ok(Fetched {
        release: Release::from_tag(tag),
        source: Source::ReleaseRedirect,
    })
}

/// The release the API feed describes, assets and all.
///
/// This is the request that spends rate-limit budget, so it is called only
/// when something actually needs what the redirect cannot give: `--pre`, an
/// overridden endpoint, or a package filename that turned out not to be the
/// published one.
pub fn from_feed(
    transport: &Transport,
    endpoints: &Endpoints,
    allow_prerelease: bool,
) -> Result<Release, String> {
    // Pre-releases are absent from `/releases/latest`, so opting in means
    // asking the list endpoint; without the flag this is the pinned default.
    let feed_url = if allow_prerelease {
        endpoints.list_url()
    } else {
        endpoints.releases_url.clone()
    };
    let body = transport.get_text(&feed_url, net::FEED_MAX_BYTES)?;
    crate::feed::select(&body, allow_prerelease)
}

/// The tag the `releases/latest` redirect points at.
///
/// Exposed for tests and for callers that want the version without the feed.
pub fn latest_tag(transport: &Transport, url: &str) -> Result<String, String> {
    let location = transport
        .redirect_location(url)?
        .ok_or_else(|| format!("{url} did not redirect to a release"))?;
    let target = absolutize(url, &location)
        .ok_or_else(|| format!("{url} redirected somewhere unusable: {location}"))?;

    // A pinned request must land on a pinned host: a redirect is exactly the
    // move that would otherwise walk us off the allowlist, and this one is
    // read for a version we then build a download URL from. A request that was
    // already off the allowlist (a test server) is left alone, as it is for
    // asset URLs that come out of an overridden feed.
    if endpoint::is_trusted(url) {
        endpoint::check(&target).map_err(|e| e.to_string())?;
    }

    tag_of(&target).ok_or_else(|| format!("{url} redirected to {target}, which names no release"))
}

/// The tag in a `…/releases/tag/<tag>` URL, if it is one and the tag looks
/// like a version rather than arbitrary text we would go on to put in a URL.
fn tag_of(url: &str) -> Option<String> {
    let tag = url.split("/releases/tag/").nth(1)?;
    let tag = tag.split(['?', '#', '/']).next()?.trim();
    let plausible = !tag.is_empty()
        && tag.chars().any(|c| c.is_ascii_digit())
        && tag
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '-' | '+' | '_'));
    plausible.then(|| tag.to_string())
}

/// Resolve a `Location` against the URL it came from.
///
/// GitHub answers with an absolute URL; a root-relative one is resolved
/// against the request's own origin. Anything else — a path-relative hop, a
/// `data:` URL — is refused rather than guessed at.
fn absolutize(base: &str, location: &str) -> Option<String> {
    if location.starts_with("https://") || location.starts_with("http://") {
        return Some(location.to_string());
    }
    let rest = location.strip_prefix('/')?;
    let scheme_end = base.find("://")? + 3;
    let authority_len = base[scheme_end..]
        .find('/')
        .unwrap_or(base.len() - scheme_end);
    Some(format!("{}/{rest}", &base[..scheme_end + authority_len]))
}

#[cfg(test)]
mod server_tests {
    use super::*;
    use std::sync::mpsc;

    /// One canned answer: the path prefix it matches, its status, and the
    /// headers that carry the part under test.
    type Route = (&'static str, u16, Vec<(&'static str, String)>);

    /// Serve one canned answer per path, and hand back the base URL.
    fn serve(routes: Vec<Route>) -> String {
        let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
        let port = server.server_addr().to_ip().expect("ip").port();
        let (ready, started) = mpsc::channel();
        std::thread::spawn(move || {
            ready.send(()).expect("signal");
            for request in server.incoming_requests() {
                let url = request.url().to_string();
                let route = routes
                    .iter()
                    .find(|(path, _, _)| url.starts_with(path))
                    .cloned();
                let response = match route {
                    Some((_, code, headers)) => {
                        let mut response = tiny_http::Response::from_string("")
                            .with_status_code(tiny_http::StatusCode(code));
                        for (name, value) in headers {
                            response.add_header(
                                tiny_http::Header::from_bytes(name.as_bytes(), value.as_bytes())
                                    .expect("header"),
                            );
                        }
                        response
                    }
                    None => tiny_http::Response::from_string("").with_status_code(404),
                };
                let _ = request.respond(response);
            }
        });
        started.recv().expect("server start");
        format!("http://127.0.0.1:{port}")
    }

    fn endpoints_for(base: &str) -> Endpoints {
        Endpoints {
            releases_url: format!("{base}/releases/latest"),
            download_base: format!("{base}/dl"),
            trusted: false,
        }
    }

    #[test]
    fn the_redirect_yields_the_tag_without_following_it() {
        let base = serve(vec![(
            "/releases/latest",
            302,
            vec![(
                "Location",
                "https://github.com/sinelaw/fresh/releases/tag/v9.9.9".to_string(),
            )],
        )]);
        let transport = Transport::new(&endpoints_for(&base));
        assert_eq!(
            latest_tag(&transport, &format!("{base}/releases/latest")).unwrap(),
            "v9.9.9"
        );
    }

    /// A `Location` that names no release is a failure, not a version we make
    /// up: whatever came back would otherwise be pasted into a download URL.
    #[test]
    fn a_redirect_that_names_no_release_is_refused() {
        let base = serve(vec![(
            "/releases/latest",
            302,
            vec![("Location", "https://github.com/sinelaw/fresh".to_string())],
        )]);
        let transport = Transport::new(&endpoints_for(&base));
        let err = latest_tag(&transport, &format!("{base}/releases/latest")).unwrap_err();
        assert!(err.contains("names no release"), "got: {err}");
    }

    /// The regression this module exists for: `api.github.com` answering 403
    /// because the address's hourly budget is gone must be reported as the
    /// rate limit it is, with the remedy in the message.
    #[test]
    fn a_rate_limited_feed_says_so() {
        let base = serve(vec![(
            "/releases",
            403,
            vec![
                ("x-ratelimit-remaining", "0".to_string()),
                ("retry-after", "600".to_string()),
            ],
        )]);
        let endpoints = endpoints_for(&base);
        let transport = Transport::new(&endpoints).without_token();
        // An overridden feed has no redirect to fall back to — a mirror's
        // `latest` is not this repository's — so the rate limit is reported.
        assert_eq!(endpoints.latest_redirect_url(), None);
        let err = latest(&transport, &endpoints, false).unwrap_err();
        assert!(err.contains("rate limit"), "got: {err}");
        assert!(err.contains("10 minutes"), "the wait must be named: {err}");
        assert!(
            err.contains("FRESH_GITHUB_TOKEN"),
            "the message must name the way out: {err}"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The redirect carries no `prerelease` flag, so a stable install keeps
    /// its guarantee only if the tag itself is read — this is that check.
    #[test]
    fn a_pre_release_tag_is_refused_without_the_flag() {
        let err = from_redirect_tag("v0.4.8-rc.1".to_string()).unwrap_err();
        assert!(err.contains("0.4.8-rc.1"), "got: {err}");
        assert!(
            err.contains("--pre"),
            "the refusal must name the flag: {err}"
        );

        let fetched = from_redirect_tag("v0.4.8".to_string()).expect("a full release");
        assert_eq!(fetched.release.version(), "0.4.8");
        assert_eq!(fetched.source, Source::ReleaseRedirect);
        assert!(
            !fetched.source.lists_assets(),
            "a tag names no assets, and callers key off that"
        );
    }

    #[test]
    fn the_tag_comes_out_of_a_release_url() {
        assert_eq!(
            tag_of("https://github.com/sinelaw/fresh/releases/tag/v0.4.7").as_deref(),
            Some("v0.4.7")
        );
        // Query strings and trailing segments are not part of the tag.
        assert_eq!(
            tag_of("https://github.com/sinelaw/fresh/releases/tag/v0.4.7?a=b").as_deref(),
            Some("v0.4.7")
        );
    }

    /// The tag is pasted into a download URL, so it may not be arbitrary text.
    #[test]
    fn a_tag_that_is_not_a_version_is_refused() {
        assert_eq!(tag_of("https://github.com/sinelaw/fresh/releases"), None);
        assert_eq!(
            tag_of("https://github.com/sinelaw/fresh/releases/tag/"),
            None
        );
        assert_eq!(
            tag_of("https://github.com/sinelaw/fresh/releases/tag/latest"),
            None,
            "a tag with no digit in it is not a version"
        );
        assert_eq!(
            tag_of("https://github.com/x/y/releases/tag/v1.0%2F..%2Fevil"),
            None
        );
    }

    #[test]
    fn a_relative_location_resolves_against_the_request() {
        assert_eq!(
            absolutize(
                "https://github.com/sinelaw/fresh/releases/latest",
                "/sinelaw/fresh/releases/tag/v0.4.7"
            )
            .as_deref(),
            Some("https://github.com/sinelaw/fresh/releases/tag/v0.4.7")
        );
        assert_eq!(
            absolutize("https://github.com/a", "https://elsewhere.example/b").as_deref(),
            Some("https://elsewhere.example/b")
        );
        // Not a hop we understand, so not a hop we follow.
        assert_eq!(absolutize("https://github.com/a", "../b"), None);
    }
}
