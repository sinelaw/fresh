//! Getting the release we would offer — and what to do when GitHub's API says
//! no.
//!
//! The release feed lives on `api.github.com`, which gives an unauthenticated
//! caller 60 requests an hour *per source address*. That is a budget shared
//! with everything else on the same NAT, so `fresh --cmd update` can be
//! refused with a 403 on a machine where `curl … install.sh | sh` works fine
//! seconds later: the install script never touches the API at all. It asks
//! `github.com` for the release *asset*, and GitHub serves that from the web
//! host with no such budget.
//!
//! So when the API refuses for that reason — and only that reason — the
//! version is read the way the install script effectively reads it:
//! `github.com/<repo>/releases/latest` is a redirect to the newest release's
//! page, and the tag is in the `Location` header. One request, no API budget,
//! same host allowlist, still https.
//!
//! What the redirect cannot give is the release's **asset list**, which is why
//! the source is reported alongside the release rather than hidden. Channels
//! whose artifact is named by the feed ([`crate::registry::UpdateKind::DownloadPackage`])
//! need that list and must say so plainly; the self-contained channels build
//! the asset name from their own target triple and need nothing but the
//! version. The former is the case the redirect cannot rescue, and pretending
//! otherwise would surface as "release 0.4.8 publishes no .deb for amd64" —
//! a sentence that is both false and unactionable.
//!
//! The redirect is *not* used in place of the feed by default. It carries no
//! `prerelease` or `draft` flag, so `select`'s guarantees would become
//! GitHub's web behaviour again, which is the arrangement
//! [`crate::feed::select`] exists to end.

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

/// The release to offer, from the feed if the API will serve it.
///
/// Falls back to the redirect only when the API refused for rate limiting:
/// every other failure is reported as itself, because a 404 or a TLS error is
/// not something a second request to a different path would fix, and quietly
/// answering a broken feed with a version from somewhere else would hide it.
pub fn latest(
    transport: &Transport,
    endpoints: &Endpoints,
    allow_prerelease: bool,
) -> Result<Fetched, String> {
    // Pre-releases are absent from `/releases/latest`, so opting in means
    // asking the list endpoint; without the flag this is the pinned default.
    let feed_url = if allow_prerelease {
        endpoints.list_url()
    } else {
        endpoints.releases_url.clone()
    };

    let error = match transport.get_text(&feed_url, net::FEED_MAX_BYTES) {
        Ok(body) => {
            return Ok(Fetched {
                release: crate::feed::select(&body, allow_prerelease)?,
                source: Source::Api,
            })
        }
        Err(e) => e,
    };

    // The redirect names the latest *stable* release, so it cannot answer a
    // `--pre` run, and it only means anything against the pinned endpoints.
    let redirect = match (error.is_rate_limited(), allow_prerelease) {
        (true, false) => endpoints.latest_redirect_url(),
        _ => None,
    };
    let Some(redirect) = redirect else {
        return Err(error.to_string());
    };

    tracing::warn!(%error, "release feed rate-limited; reading the version from the release redirect");
    match latest_tag(transport, &redirect) {
        Ok(tag) => Ok(Fetched {
            release: Release::from_tag(tag),
            source: Source::ReleaseRedirect,
        }),
        // Both routes failed: report the rate limit, which is the actionable
        // half, and name the second attempt so it is clear one was made.
        Err(second) => Err(format!("{error}\n\nReading {redirect} instead: {second}")),
    }
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
