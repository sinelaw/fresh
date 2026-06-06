use std::path::{Path, PathBuf};

/// Convert a file path to an `lsp_types::Uri`.
pub fn file_path_to_lsp_uri(path: &Path) -> Option<lsp_types::Uri> {
    fresh_core::file_uri::path_to_lsp_uri(path)
}

/// LSP-facing URI: a URI as it appears on the wire to or from a
/// language server. This is a newtype around `lsp_types::Uri`. The
/// type-system point is to force every URI that crosses the
/// editor↔LSP boundary through one of the two checked constructors:
///
///   * [`LspUri::from_host_path`] — given a host path and the active
///     authority's host↔remote translation, produces an `LspUri` that
///     carries the in-container path on container authorities (and
///     the host path everywhere else).
///   * [`LspUri::from_wire`] — wraps a raw `lsp_types::Uri` that was
///     received from the LSP server. The wrapped URI is "remote-side"
///     under a container authority and must be passed back through
///     [`LspUri::to_host_path`] before any filesystem-facing code
///     sees it.
///
/// Conversely, the only ways to extract a path are:
///
///   * [`LspUri::to_host_path`] — applies remote→host translation
///     symmetrically with `from_host_path`. This is the host-side
///     `PathBuf` filesystem APIs accept. Untranslated extraction
///     (`as_uri().path()`) is intentionally not exposed as a method —
///     callers that genuinely want the wire-side path string read
///     `as_str()` and document why a host-path interpretation isn't
///     wanted.
///
/// Storing buffer URIs in [`BufferMetadata`] as `LspUri` (not
/// `lsp_types::Uri`) keeps the cached form already translated for the
/// active authority, so the dozens of `metadata.file_uri()` call
/// sites can't accidentally ship a host URI to a container LSP.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LspUri(lsp_types::Uri);

impl LspUri {
    /// Build an LSP-facing URI from a host path, applying the
    /// authority's host→remote translation when one is set. Returns
    /// `None` for relative paths (matches the pre-newtype helper).
    pub fn from_host_path(
        path: &Path,
        translation: Option<&crate::services::authority::PathTranslation>,
    ) -> Option<Self> {
        let mapped = translation
            .and_then(|t| t.host_to_remote(path))
            .unwrap_or_else(|| path.to_path_buf());
        fresh_core::file_uri::path_to_lsp_uri(&mapped).map(Self)
    }

    /// Wrap a raw URI received from the LSP wire. The caller must
    /// subsequently translate via [`Self::to_host_path`] before
    /// opening the file or comparing with host paths — that's the
    /// whole point of having the newtype.
    pub fn from_wire(uri: lsp_types::Uri) -> Self {
        Self(uri)
    }

    /// Borrow the underlying raw URI for serialization to the LSP
    /// wire (e.g. into JSON-RPC params). Only the LSP transport layer
    /// should call this; editor-level code never sees a bare
    /// `lsp_types::Uri`.
    pub fn as_uri(&self) -> &lsp_types::Uri {
        &self.0
    }

    /// String form, for log messages and equality comparisons against
    /// other URI strings (e.g. when matching a buffer against an
    /// incoming notification's URI). Does not strip the
    /// host-vs-container ambiguity — comparisons must be between two
    /// `LspUri`s, not between a wire URI and a host URI.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Decode this URI to a host path, applying the authority's
    /// remote→host translation when one is set. Returns `None` for
    /// non-`file://` URIs.
    pub fn to_host_path(
        &self,
        translation: Option<&crate::services::authority::PathTranslation>,
    ) -> Option<PathBuf> {
        let raw = fresh_core::file_uri::lsp_uri_to_path(&self.0)?;
        Some(
            translation
                .and_then(|t| t.remote_to_host(&raw))
                .unwrap_or(raw),
        )
    }

    /// Consume `self` and return the raw `lsp_types::Uri`. Reserved
    /// for the wire layer (LSP transport, lsp_types interop). Editor
    /// code uses [`Self::as_uri`] when it just needs to borrow.
    pub fn into_inner(self) -> lsp_types::Uri {
        self.0
    }
}

/// Build the LSP-facing URI for a host-side `path`, applying the
/// authority's host→remote translation when one is set.
///
/// Thin shim around [`LspUri::from_host_path`] that returns the
/// inner [`lsp_types::Uri`] for the few callers (root_uri building
/// inside `LspManager`, code-action workspace folder hand-off) that
/// have to feed a raw `Uri` into a third-party API. New code should
/// prefer `LspUri::from_host_path` directly so the host-vs-LSP side
/// stays type-checked.
pub fn file_path_to_lsp_uri_with_translation(
    path: &Path,
    translation: Option<&crate::services::authority::PathTranslation>,
) -> Option<lsp_types::Uri> {
    LspUri::from_host_path(path, translation).map(|u| u.into_inner())
}

// `LspUri` translation algebra works on any platform but the unit-test
// fixtures use POSIX-shaped paths (the only side that ever exists for a
// container's interior) and a Linux-style URI without a drive letter.
// On Windows `lsp_types::Uri::parse(\"file:///workspaces/...\")` returns
// `None` for lack of a drive letter, which would make these tests fail
// for reasons unrelated to the algebra they're verifying. Gate to Unix
// — the cross-platform URI encoding is covered separately by
// `uri_encoding_tests`.
#[cfg(all(test, unix))]
mod lsp_uri_tests {
    use super::*;
    use crate::services::authority::PathTranslation;

    fn translation() -> PathTranslation {
        PathTranslation {
            host_root: PathBuf::from("/tmp/.tmpA1B2"),
            remote_root: PathBuf::from("/workspaces/proj"),
        }
    }

    #[test]
    fn from_host_path_under_workspace_translates_to_remote_uri() {
        let host = PathBuf::from("/tmp/.tmpA1B2/src/util.py");
        let lsp_uri = LspUri::from_host_path(&host, Some(&translation())).expect("absolute path");
        assert_eq!(lsp_uri.as_str(), "file:///workspaces/proj/src/util.py");
    }

    #[test]
    fn from_host_path_outside_workspace_passes_through() {
        // System headers / library sources sit outside the mounted
        // workspace; translation returns `None` and the host URI is
        // shipped to the LSP unchanged. The point of the newtype is
        // just to make the decision explicit.
        let host = PathBuf::from("/usr/include/stdio.h");
        let lsp_uri = LspUri::from_host_path(&host, Some(&translation())).expect("absolute path");
        assert_eq!(lsp_uri.as_str(), "file:///usr/include/stdio.h");
    }

    #[test]
    fn to_host_path_under_remote_root_translates_back() {
        let wire: lsp_types::Uri = "file:///workspaces/proj/src/util.py".parse().unwrap();
        let host = LspUri::from_wire(wire)
            .to_host_path(Some(&translation()))
            .expect("file:// URI");
        assert_eq!(host, PathBuf::from("/tmp/.tmpA1B2/src/util.py"));
    }

    #[test]
    fn to_host_path_outside_remote_root_passes_through() {
        let wire: lsp_types::Uri = "file:///usr/include/stdio.h".parse().unwrap();
        let host = LspUri::from_wire(wire)
            .to_host_path(Some(&translation()))
            .expect("file:// URI");
        assert_eq!(host, PathBuf::from("/usr/include/stdio.h"));
    }

    #[test]
    fn round_trip_host_to_wire_to_host_under_workspace() {
        // The whole point of the symmetry: anything that goes out
        // through `from_host_path` must come back through
        // `to_host_path` byte-identical. This is the property the
        // editor relies on so a buffer's host file_path matches the
        // path resolved from a server-returned `Location`.
        let host = PathBuf::from("/tmp/.tmpA1B2/main.py");
        let lsp_uri = LspUri::from_host_path(&host, Some(&translation())).unwrap();
        let back = lsp_uri.to_host_path(Some(&translation())).unwrap();
        assert_eq!(back, host);
    }

    #[test]
    fn no_translation_is_identity() {
        let host = PathBuf::from("/some/host/path/file.rs");
        let lsp_uri = LspUri::from_host_path(&host, None).unwrap();
        assert_eq!(lsp_uri.as_str(), "file:///some/host/path/file.rs");
        let back = lsp_uri.to_host_path(None).unwrap();
        assert_eq!(back, host);
    }
}

#[cfg(test)]
mod uri_encoding_tests {
    use super::*;

    /// Helper to get a platform-appropriate absolute path for testing.
    fn abs_path(suffix: &str) -> PathBuf {
        std::env::temp_dir().join(suffix)
    }

    #[test]
    fn test_brackets_in_path() {
        let path = abs_path("MY_PROJECTS [temp]/gogame/main.go");
        let uri = file_path_to_lsp_uri(&path);
        assert!(
            uri.is_some(),
            "URI should be computed for path with brackets"
        );
        let uri = uri.unwrap();
        assert!(
            uri.as_str().contains("%5Btemp%5D"),
            "Brackets should be percent-encoded: {}",
            uri.as_str()
        );
    }

    #[test]
    fn test_spaces_in_path() {
        let path = abs_path("My Projects/src/main.go");
        let uri = file_path_to_lsp_uri(&path);
        assert!(uri.is_some(), "URI should be computed for path with spaces");
    }

    #[test]
    fn test_normal_path() {
        let path = abs_path("project/main.go");
        let uri = file_path_to_lsp_uri(&path);
        assert!(uri.is_some(), "URI should be computed for normal path");
        let s = uri.unwrap().as_str().to_string();
        assert!(s.starts_with("file:///"), "Should be a file URI: {}", s);
        assert!(
            s.ends_with("project/main.go"),
            "Should end with the path: {}",
            s
        );
    }

    #[test]
    fn test_relative_path_returns_none() {
        let path = PathBuf::from("main.go");
        assert!(file_path_to_lsp_uri(&path).is_none());
    }

    #[test]
    fn test_all_special_chars() {
        let path = abs_path("a[b]c{d}e^g`h/file.rs");
        let uri = file_path_to_lsp_uri(&path);
        assert!(uri.is_some(), "Should handle all special characters");
        let s = uri.unwrap().as_str().to_string();
        assert!(!s.contains('['), "[ should be encoded in {}", s);
        assert!(!s.contains(']'), "] should be encoded in {}", s);
        assert!(!s.contains('{'), "{{ should be encoded in {}", s);
        assert!(!s.contains('}'), "}} should be encoded in {}", s);
        assert!(!s.contains('^'), "^ should be encoded in {}", s);
        assert!(!s.contains('`'), "` should be encoded in {}", s);
    }
}
