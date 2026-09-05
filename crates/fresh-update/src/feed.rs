//! Parsing the release metadata document.
//!
//! This used to be a string scan: find `"browser_download_url"`, take the next
//! quoted run, keep the first one whose basename contained the architecture.
//! Two things were wrong with that. A release body is free text written by
//! whoever cut the release, so a *value* containing `"browser_download_url":
//! "..."` was indistinguishable from real structure — and "first match wins"
//! made the answer depend on field ordering, which no API guarantees. Since the
//! URL that comes out is downloaded and then handed to a package manager, that
//! is a bigger promise than a substring search can keep.
//!
//! So: parse the JSON, look only inside the `assets` array, and require the
//! match to be **unique**. If two assets could both be the one we want, that is
//! a packaging mistake or an attempt to shadow the real artifact; either way
//! guessing is worse than stopping.

use serde::Deserialize;

/// The package name every published artifact starts with. Note this is the
/// *package* name (`fresh-editor`), not the binary name (`fresh`) — the two
/// differ, which is the distinction that broke receipt lookup once already.
pub const PACKAGE_PREFIX: &str = "fresh-editor";

/// One published release asset.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Asset {
    /// Filename as published, e.g. `fresh-editor_0.4.7-1_amd64.deb`.
    pub name: String,
    /// Direct download URL.
    pub browser_download_url: String,
}

/// A release, as described by the release-metadata document.
///
/// Unknown fields are ignored: the real GitHub document carries dozens we do
/// not care about, and a test fixture carries almost none. Both must parse.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
pub struct Release {
    /// The git tag, usually `v`-prefixed.
    pub tag_name: String,
    /// Published artifacts. Absent in a minimal fixture, hence `default`.
    #[serde(default)]
    pub assets: Vec<Asset>,
    /// GitHub's prerelease flag. Defaulting to `false` is the safe direction:
    /// a feed that omits it is treated as a normal release, which is what a
    /// mirror serving a plain document means.
    #[serde(default)]
    pub prerelease: bool,
    /// Drafts are never installable — their assets are not publicly served.
    #[serde(default)]
    pub draft: bool,
}

impl Release {
    /// A release known only by its tag.
    ///
    /// This is what [`crate::fetch`] can recover when the API refuses to serve
    /// the feed: the `releases/latest` redirect names the release and nothing
    /// else. The empty asset list is *unknown*, not *empty*, and callers must
    /// keep the two apart — [`crate::fetch::Source::lists_assets`] is how.
    pub fn from_tag(tag_name: impl Into<String>) -> Self {
        Release {
            tag_name: tag_name.into(),
            assets: Vec::new(),
            prerelease: false,
            draft: false,
        }
    }

    /// Parse a release-metadata document.
    pub fn parse(json: &str) -> Result<Self, String> {
        serde_json::from_str(json).map_err(|e| format!("could not read the release feed: {e}"))
    }

    /// Parse a document that is either one release or a list of them.
    ///
    /// `/releases/latest` and a tag endpoint return one object; `/releases`
    /// returns an array. Accepting both means the caller picks an endpoint
    /// without the parser needing to be told which.
    pub fn parse_any(json: &str) -> Result<Vec<Self>, String> {
        if let Ok(list) = serde_json::from_str::<Vec<Self>>(json) {
            return Ok(list);
        }
        Self::parse(json).map(|r| vec![r])
    }

    /// The version, with any leading `v` stripped.
    pub fn version(&self) -> &str {
        self.tag_name
            .trim()
            .strip_prefix('v')
            .unwrap_or_else(|| self.tag_name.trim())
    }

    /// The single published package matching `extension` and `arch`.
    ///
    /// Matching stays a predicate rather than an exact filename because the
    /// name we would have to construct embeds a packaging release number
    /// (`0.4.7-1`) that the version alone does not give us. It is tightened in
    /// three ways over a bare substring test: the name must start with the
    /// package prefix, the architecture must appear in the position the
    /// packaging tool puts it (`_amd64.deb`, `.x86_64.rpm`), and the match must
    /// be unique.
    pub fn find_package(&self, extension: &str, arch: &str) -> Result<&Asset, String> {
        let mut matches = self
            .assets
            .iter()
            .filter(|a| is_package_named(&a.name, extension, arch));

        let first = matches.next().ok_or_else(|| {
            format!(
                "release {} publishes no {extension} for {arch}",
                self.version()
            )
        })?;
        if let Some(second) = matches.next() {
            return Err(format!(
                "release {} publishes more than one {extension} for {arch} ({} and {}); \
                 refusing to guess which is the update",
                self.version(),
                first.name,
                second.name
            ));
        }
        Ok(first)
    }
}

/// Choose the release to offer, or say why there is none.
///
/// Drafts are never offered. A prerelease is offered only when explicitly
/// asked for: stable installs are protected today only because
/// `/releases/latest` omits prereleases, which is GitHub's behaviour rather
/// than a decision this code makes — so a mirror, a tag endpoint or a change
/// at GitHub would silently start handing users release candidates. Refusing
/// here makes it ours.
///
/// The refusal names the version and the flag, because "no update available"
/// when a release plainly exists is the kind of answer people debug for an
/// hour.
pub fn select(json: &str, allow_prerelease: bool) -> Result<Release, String> {
    let all = Release::parse_any(json)?;
    if all.is_empty() {
        return Err("the release feed lists no releases".to_string());
    }

    let mut best: Option<Release> = None;
    let mut skipped_pre: Option<String> = None;
    for release in all {
        if release.draft {
            continue;
        }
        if release.prerelease && !allow_prerelease {
            let version = release.version().to_string();
            // Report the newest one skipped, so the hint names what the user
            // can actually see on the releases page.
            if skipped_pre
                .as_deref()
                .is_none_or(|s| crate::version::is_newer(s, &version))
            {
                skipped_pre = Some(version);
            }
            continue;
        }
        let better = best
            .as_ref()
            .is_none_or(|b| crate::version::is_newer(b.version(), release.version()));
        if better {
            best = Some(release);
        }
    }

    match (best, skipped_pre) {
        (Some(r), _) => Ok(r),
        (None, Some(pre)) => Err(format!(
            "the newest release is {pre}, a pre-release; pass --pre to install it"
        )),
        (None, None) => Err("the release feed lists no installable releases".to_string()),
    }
}

/// Whether `name` is our package artifact for `extension` and `arch`.
///
/// The separator before the architecture matters: dpkg writes
/// `fresh-editor_0.4.7-1_amd64.deb` and rpm writes
/// `fresh-editor-0.4.7-1.x86_64.rpm`, so accepting either `_` or `.` covers
/// both while still refusing a name that merely happens to contain the arch
/// somewhere in the middle.
fn is_package_named(name: &str, extension: &str, arch: &str) -> bool {
    if !name.starts_with(PACKAGE_PREFIX) {
        return false;
    }
    let Some(stem) = name.strip_suffix(extension) else {
        return false;
    };
    for sep in ['_', '.', '-'] {
        if let Some(rest) = stem.strip_suffix(arch) {
            if rest.ends_with(sep) {
                return true;
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    const FEED: &str = r#"{
        "tag_name": "v0.4.7",
        "body": "See https://example.invalid for notes. \"browser_download_url\": \"https://evil.example/fresh-editor_0.4.7-1_amd64.deb\"",
        "assets": [
            { "name": "fresh-editor_0.4.7-1_amd64.deb",
              "browser_download_url": "https://github.com/sinelaw/fresh/releases/download/v0.4.7/fresh-editor_0.4.7-1_amd64.deb" },
            { "name": "fresh-editor-0.4.7-1.x86_64.rpm",
              "browser_download_url": "https://github.com/sinelaw/fresh/releases/download/v0.4.7/fresh-editor-0.4.7-1.x86_64.rpm" },
            { "name": "fresh-editor-x86_64-unknown-linux-gnu.tar.xz",
              "browser_download_url": "https://github.com/sinelaw/fresh/releases/download/v0.4.7/fresh-editor-x86_64-unknown-linux-gnu.tar.xz" }
        ]
    }"#;

    #[test]
    fn version_strips_the_tag_prefix() {
        assert_eq!(Release::parse(FEED).unwrap().version(), "0.4.7");
        let bare = Release::parse(r#"{"tag_name":"1.2.3"}"#).unwrap();
        assert_eq!(bare.version(), "1.2.3");
    }

    /// The regression this module exists for: a URL written into the release
    /// *body* must never be mistaken for a published asset. The old string
    /// scan returned the first `"browser_download_url"` in the document, and
    /// in this feed that is the one inside the prose.
    #[test]
    fn a_url_in_the_release_body_is_not_an_asset() {
        let release = Release::parse(FEED).unwrap();
        let asset = release.find_package(".deb", "amd64").unwrap();
        assert!(
            asset
                .browser_download_url
                .starts_with("https://github.com/"),
            "resolved {} from the release body instead of the assets array",
            asset.browser_download_url
        );
    }

    #[test]
    fn each_packaging_tool_s_arch_spelling_resolves() {
        let release = Release::parse(FEED).unwrap();
        assert_eq!(
            release.find_package(".deb", "amd64").unwrap().name,
            "fresh-editor_0.4.7-1_amd64.deb"
        );
        assert_eq!(
            release.find_package(".rpm", "x86_64").unwrap().name,
            "fresh-editor-0.4.7-1.x86_64.rpm"
        );
    }

    #[test]
    fn a_missing_package_is_an_error_not_a_wrong_answer() {
        let release = Release::parse(FEED).unwrap();
        // arm64 is not published in this feed; the amd64 .deb must not be
        // offered as a near-enough substitute.
        assert!(release.find_package(".deb", "arm64").is_err());
        assert!(release.find_package(".flatpak", "x86_64").is_err());
    }

    #[test]
    fn the_architecture_must_sit_where_the_packaging_tool_puts_it() {
        // `contains(arch)` would accept this: the arch appears, but as part of
        // another word rather than as the architecture field.
        assert!(!is_package_named(
            "fresh-editor-notamd64-extra.deb",
            ".deb",
            "amd64"
        ));
        assert!(is_package_named(
            "fresh-editor_0.4.7-1_amd64.deb",
            ".deb",
            "amd64"
        ));
        // A different project's artifact in the same release must not match.
        assert!(!is_package_named(
            "other-tool_1.0_amd64.deb",
            ".deb",
            "amd64"
        ));
    }

    #[test]
    fn an_ambiguous_release_is_refused_rather_than_guessed() {
        let feed = r#"{
            "tag_name": "v0.4.7",
            "assets": [
                { "name": "fresh-editor_0.4.7-1_amd64.deb", "browser_download_url": "https://github.com/a.deb" },
                { "name": "fresh-editor_0.4.7-2_amd64.deb", "browser_download_url": "https://github.com/b.deb" }
            ]
        }"#;
        let err = Release::parse(feed)
            .unwrap()
            .find_package(".deb", "amd64")
            .unwrap_err();
        assert!(err.contains("more than one"), "got: {err}");
    }

    /// The guard this exists for: a stable install must refuse an rc even when
    /// one is served, because `/releases/latest` omitting them is GitHub's
    /// behaviour and not something this code controls.
    #[test]
    fn a_prerelease_is_refused_unless_asked_for() {
        let feed = r#"{"tag_name":"v0.4.8-rc.1","prerelease":true}"#;
        let err = select(feed, false).unwrap_err();
        assert!(err.contains("0.4.8-rc.1"), "got: {err}");
        assert!(
            err.contains("--pre"),
            "the refusal must name the flag: {err}"
        );
        assert_eq!(select(feed, true).unwrap().version(), "0.4.8-rc.1");
    }

    #[test]
    fn a_list_picks_the_newest_and_skips_prereleases_by_default() {
        let feed = r#"[
            {"tag_name":"v0.4.8-rc.2","prerelease":true},
            {"tag_name":"v0.4.7","prerelease":false},
            {"tag_name":"v0.4.6","prerelease":false}
        ]"#;
        assert_eq!(select(feed, false).unwrap().version(), "0.4.7");
        assert_eq!(select(feed, true).unwrap().version(), "0.4.8-rc.2");
    }

    /// GitHub returns releases newest-first, but nothing promises that, and a
    /// mirror need not. Order must come from precedence, not position.
    #[test]
    fn selection_does_not_trust_feed_order() {
        let feed = r#"[
            {"tag_name":"v0.4.6"},
            {"tag_name":"v0.4.9"},
            {"tag_name":"v0.4.7"}
        ]"#;
        assert_eq!(select(feed, false).unwrap().version(), "0.4.9");
    }

    /// Drafts are listed by the API but their assets are not publicly served,
    /// so offering one produces a download that 404s.
    #[test]
    fn drafts_are_never_offered() {
        let feed = r#"[
            {"tag_name":"v0.5.0","draft":true},
            {"tag_name":"v0.4.7"}
        ]"#;
        assert_eq!(select(feed, false).unwrap().version(), "0.4.7");
        assert_eq!(select(feed, true).unwrap().version(), "0.4.7");
    }

    /// A mirror serving a plain document omits the flag entirely; that must
    /// read as a normal release rather than defaulting to "pre-release".
    #[test]
    fn a_document_without_the_flag_is_a_normal_release() {
        let r = Release::parse(r#"{"tag_name":"v1.2.3"}"#).unwrap();
        assert!(!r.prerelease && !r.draft);
        assert_eq!(
            select(r#"{"tag_name":"v1.2.3"}"#, false).unwrap().version(),
            "1.2.3"
        );
    }

    #[test]
    fn a_feed_that_is_not_json_fails_loudly() {
        assert!(Release::parse("<html>404</html>").is_err());
        assert!(Release::parse("").is_err());
    }
}
