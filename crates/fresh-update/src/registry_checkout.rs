// Is a source tree a cargo registry checkout?
//
// One predicate, in its own file because it is needed in two places that
// cannot share code the usual way: `build.rs` (which decides at compile time
// whether to stamp `FRESH_BUILD_CHANNEL=cargo`) and the library (so the
// decision is covered by tests that actually run — a `#[cfg(test)]` module
// inside a build script is never executed by `cargo test`). The build script
// `include!`s this file, which is also why the header above is `//` rather
// than `//!`: an included file cannot carry inner doc comments.

use std::path::Path;

/// `true` if `path` sits under a `registry/src/` pair — the layout cargo uses
/// for sources it unpacked from crates.io, i.e.
/// `$CARGO_HOME/registry/src/<index>/<crate>-<version>/`.
///
/// This is how a `cargo install fresh-editor` is recognised at build time,
/// replacing the runtime guess that used to read `~/.cargo/bin` off the
/// executable path.
///
/// Matching the adjacent pair rather than either name alone is what keeps an
/// unrelated directory called `registry` from qualifying.
pub fn is_registry_checkout(path: &Path) -> bool {
    let parts: Vec<_> = path.components().map(|c| c.as_os_str()).collect();
    parts
        .windows(2)
        .any(|w| w[0] == "registry" && w[1] == "src")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognises_a_registry_checkout() {
        assert!(is_registry_checkout(Path::new(
            "/root/.cargo/registry/src/index.crates.io-1949cf8c6b5b557f/fresh-update-0.4.7"
        )));
        // A relocated CARGO_HOME is still a registry checkout.
        assert!(is_registry_checkout(Path::new(
            "/opt/cargo-home/registry/src/index.crates.io-abc/fresh-update-0.4.7"
        )));
    }

    #[test]
    fn a_workspace_or_git_build_is_not_one() {
        assert!(!is_registry_checkout(Path::new(
            "/home/u/src/fresh/crates/fresh-update"
        )));
        assert!(!is_registry_checkout(Path::new("/home/u/src/fresh")));
    }

    /// `registry` and `src` must be adjacent and in that order, or any project
    /// with a directory called `registry` would claim to be a cargo install.
    #[test]
    fn a_directory_merely_named_registry_is_not_one() {
        assert!(!is_registry_checkout(Path::new("/srv/registry/fresh")));
        assert!(!is_registry_checkout(Path::new("/home/u/src/registry")));
        assert!(!is_registry_checkout(Path::new("/srv/src/registry/fresh")));
        assert!(!is_registry_checkout(Path::new(
            "/srv/registry/vendor/src/fresh"
        )));
    }
}
