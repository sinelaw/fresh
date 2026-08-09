// One predicate, in its own file because `build.rs` `include!`s it: the
// decision is made at compile time, but a `#[cfg(test)]` module inside a build
// script never runs under `cargo test`. An included file cannot carry inner
// doc comments, hence `//` above.

use std::path::Path;

/// `true` if `path` is a source tree cargo unpacked from a registry `.crate` —
/// i.e. this build is a `cargo install` from crates.io.
///
/// Both markers are written by cargo itself, which is the point: the earlier
/// version matched `registry/src` in the path, and any directory can be named
/// that. `.cargo-ok` is written after cargo finishes unpacking and is not in
/// the published tarball; `Cargo.toml.orig` is written by `cargo package` and
/// so is absent from a git checkout. Requiring both means "unpacked by cargo,
/// from something published" — a git or `--path` build satisfies neither.
///
/// Prefer setting `FRESH_BUILD_CHANNEL` explicitly. This exists only for the
/// one channel that cannot: crates.io builds on the user's machine, so there
/// is no packaging step of ours to inject into.
pub fn is_registry_checkout(path: &Path) -> bool {
    path.join(".cargo-ok").is_file() && path.join("Cargo.toml.orig").is_file()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn tree(markers: &[&str]) -> (tempfile::TempDir, PathBuf) {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        std::fs::write(root.join("Cargo.toml"), "[package]\n").unwrap();
        for m in markers {
            std::fs::write(root.join(m), "{\"v\":1}").unwrap();
        }
        (dir, root)
    }

    #[test]
    fn recognises_an_unpacked_registry_crate() {
        let (_d, root) = tree(&[".cargo-ok", "Cargo.toml.orig"]);
        assert!(is_registry_checkout(&root));
    }

    #[test]
    fn a_workspace_or_git_build_is_not_one() {
        let (_d, root) = tree(&[]);
        assert!(!is_registry_checkout(&root));
    }

    /// `cargo install --git` also gets a `.cargo-ok`, but the checkout is the
    /// repository, so `cargo package` never ran and there is no `.orig`.
    #[test]
    fn a_git_install_is_not_a_registry_install() {
        let (_d, root) = tree(&[".cargo-ok"]);
        assert!(!is_registry_checkout(&root));
    }

    /// A `.crate` untarred by hand is not something cargo is managing.
    #[test]
    fn an_unpacked_tarball_alone_is_not_one() {
        let (_d, root) = tree(&["Cargo.toml.orig"]);
        assert!(!is_registry_checkout(&root));
    }

    /// The path is no longer consulted at all — this is the case the old
    /// name-matching version got wrong.
    #[test]
    fn a_directory_merely_named_registry_is_not_one() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().join("registry").join("src").join("fresh-0.1.0");
        std::fs::create_dir_all(&root).unwrap();
        assert!(!is_registry_checkout(&root));
    }
}
