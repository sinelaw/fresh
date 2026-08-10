//! Does the updater ask for the file the release pipeline actually publishes?
//!
//! This is a contract with YAML in another directory, which is precisely why it
//! needs its own test. The `.tar.gz` / `.tar.xz` mismatch that shipped on this
//! branch passed every unit test and the end-to-end test alike, because each
//! derived the asset name from the same function it was checking —
//! self-consistent, and blind to that function being wrong. A test can only
//! hold this seam by reading the other side of it.

// `archive_ext` lives behind the engine feature; without this the file fails to
// compile under a plain `cargo test -p fresh-update`.
#![cfg(feature = "engine")]

use std::path::{Path, PathBuf};

/// Does [`archive_ext`] agree with what the release workflows *actually publish*?
///
/// Skipped when the workflows are absent, which means "not a git checkout" —
/// a packaged crate has no `.github`. In CI, the place this matters, it runs.
///
/// [`archive_ext`]: fresh_update::engine::archive_ext
#[test]
fn archive_ext_matches_the_release_workflows() {
    let Some(workflows) = workflow_dir() else {
        eprintln!("skipping: no .github/workflows (not a source checkout)");
        return;
    };

    // musl-builds.yml publishes the universal archive. The name it uploads is
    // the name the engine must ask for.
    let musl = std::fs::read_to_string(workflows.join("musl-builds.yml")).unwrap();
    let musl_ext = fresh_update::engine::archive_ext("x86_64-unknown-linux-musl");
    let uploaded = format!("fresh-editor-${{{{ matrix.target }}}}.{musl_ext}");
    assert!(
        musl.contains(&uploaded),
        "engine asks for `.{musl_ext}` on musl, but musl-builds.yml does not upload `{uploaded}`.\n\
         The engine and the workflow have to name the same file or self-update 404s."
    );
    assert!(
        musl.contains(&format!("{uploaded}.sha256")),
        "musl-builds.yml uploads `{uploaded}` without its .sha256 sidecar; \
         verification is fail-closed, so the update would abort."
    );

    // The upload check above is satisfied by *any* published extension, which
    // is too weak while the workflow also ships a transition `.tar.xz` for
    // binaries built before `archive_ext` existed: the engine could ask for the
    // wrong one and still find a file. So pin the canonical artifact too — the
    // name the workflow records in the install receipt, which is that job's own
    // statement of what it considers its archive. When the shim is dropped this
    // stays correct; while it exists, this is what catches a wrong mapping.
    let recorded = format!("asset=fresh-editor-${{{{ matrix.target }}}}.{musl_ext}");
    assert!(
        musl.contains(&recorded),
        "engine derives `.{musl_ext}` for musl, but musl-builds.yml records a \
         different canonical asset in its install receipt (expected `{recorded}`).\n\
         These must agree, or a self-update asks for the wrong file the moment \
         the compatibility archive is dropped."
    );

    // release.yml drives the dist targets from an `archive_ext:` matrix key.
    // Every value it lists must be one the engine can ask for and extract.
    let release = std::fs::read_to_string(workflows.join("release.yml")).unwrap();
    let declared: std::collections::BTreeSet<&str> = release
        .lines()
        .filter_map(|l| l.trim().strip_prefix("archive_ext:"))
        .map(str::trim)
        .collect();
    assert!(
        !declared.is_empty(),
        "release.yml declares no archive_ext; this test is no longer reading it correctly"
    );
    for ext in &declared {
        assert!(
            matches!(*ext, "tar.xz" | "tar.gz" | "zip"),
            "release.yml publishes `{ext}`, which the engine cannot extract"
        );
    }
    // The non-musl unix targets are the ones release.yml builds, so whatever it
    // declares for them must be what the engine derives.
    let gnu_ext = fresh_update::engine::archive_ext("x86_64-unknown-linux-gnu");
    assert!(
        declared.contains(gnu_ext),
        "engine asks for `.{gnu_ext}` on gnu targets, but release.yml declares only {declared:?}"
    );
    let win_ext = fresh_update::engine::archive_ext("x86_64-pc-windows-msvc");
    assert!(
        declared.contains(win_ext),
        "engine asks for `.{win_ext}` on windows, but release.yml declares only {declared:?}"
    );
}

/// `.github/workflows`, found relative to this crate rather than the cwd.
fn workflow_dir() -> Option<PathBuf> {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .map(|a| a.join(".github/workflows"))
        .find(|p| p.is_dir())?;
    dir.join("release.yml").is_file().then_some(dir)
}
