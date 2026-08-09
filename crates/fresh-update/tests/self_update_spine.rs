//! The self-update spine, end to end, against nothing real.
//!
//! # Why this exists
//!
//! Everything below the engine is unit-tested, and every piece passed while the
//! whole was broken twice on the branch that added this file: the musl archive
//! was published as `.tar.gz` while the engine asked for `.tar.xz`, and an
//! installer receipt made a binary offer an update it could not fetch. Both are
//! seam defects — each side correct, the contract between them wrong — and no
//! unit test can see a seam. Only running the whole chain can.
//!
//! So this drives the real engine through: read the feed → compare versions →
//! resolve provenance from a receipt → derive the asset name from the
//! compile-time target triple → download → verify the checksum → extract →
//! atomically replace the running binary.
//!
//! # No live GitHub
//!
//! The release is fabricated. `tiny_http` serves a feed announcing a version
//! that does not exist, a tarball built here from bytes chosen by the test, and
//! a `.sha256` computed over it; `FRESH_RELEASES_URL` / `FRESH_DOWNLOAD_BASE`
//! point the harness at that server. Nothing reaches the network, no release
//! has to be cut, and the "new version" is whatever string the test likes —
//! which is what makes an upgrade testable without building a second binary at
//! a different version.
//!
//! The attestation check is skipped here, and deliberately: an overridden
//! endpoint is marked untrusted, and a local server has no attestations and
//! never could. That is the same line the engine already draws for privilege.
//! It does mean this test covers everything on the path *except* the
//! attestation gate, which is covered against a real captured GitHub payload in
//! `attestation`'s own tests.
//!
//! # Shape
//!
//! The thing being updated has to be a real process, because the engine
//! replaces `current_exe()`. `src/bin/update-harness.rs` is that process.

#![cfg(all(unix, feature = "engine", feature = "insecure-endpoints"))]

use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::Command;

/// The version the fabricated release announces. Far above anything real, so
/// the comparison can only go one way.
const NEW_VERSION: &str = "99.9.9";

/// What the harness claims to be before updating.
const OLD_VERSION: &str = "0.0.1";

/// Contents of the "new binary" inside the fabricated archive. A script rather
/// than an ELF: the engine extracts an entry by name and writes the bytes, it
/// does not inspect them, so this both proves the swap and stays runnable to
/// prove the executable bit survived.
const NEW_BINARY: &[u8] = b"#!/bin/sh\necho updated-ok\n";

struct Fixture {
    _dir: tempfile::TempDir,
    /// The binary that will update itself.
    exe: PathBuf,
    /// Directory served over HTTP.
    served: PathBuf,
    /// Name of the asset the engine will ask for.
    asset: String,
}

/// Lay out an install: the harness binary, plus the receipt that makes it
/// resolve as a self-updating tarball install.
fn install(dir: tempfile::TempDir) -> Fixture {
    let root = dir.path().to_path_buf();
    let install_dir = root.join("install");
    let served = root.join("served");
    std::fs::create_dir_all(&install_dir).unwrap();
    std::fs::create_dir_all(&served).unwrap();

    let exe = install_dir.join("fresh");
    std::fs::copy(env!("CARGO_BIN_EXE_update-harness"), &exe).unwrap();
    std::fs::set_permissions(&exe, std::fs::Permissions::from_mode(0o755)).unwrap();

    // Exactly what `install.sh` writes, and what the musl archive ships.
    std::fs::write(
        install_dir.join("install-receipt.toml"),
        "schema = 1\n\
         channel = \"tarball\"\n\
         package_name = \"fresh-editor\"\n\
         managed = false\n\
         self_update = true\n",
    )
    .unwrap();

    // Derived from the engine, not chosen here, so the test follows whatever
    // this host's triple maps to. Note what that does *not* prove: since the
    // archive is then built under the same derived name, this test is
    // self-consistent and cannot see a *wrong* mapping. That contract — engine
    // vs. what the workflows actually publish — is held by
    // `archive_ext_matches_the_release_workflows` below and by the unit test
    // pinning the table.
    let asset = format!(
        "fresh-editor-{}.{}",
        fresh_update::TARGET_TRIPLE,
        fresh_update::engine::archive_ext(fresh_update::TARGET_TRIPLE)
    );

    Fixture {
        _dir: dir,
        exe,
        served,
        asset,
    }
}

/// Build the release archive in whichever format `archive_ext` chose, using the
/// system `tar` — the same tool the release workflows use, so the bytes under
/// test are shaped like the real ones rather than like this crate's encoder.
fn build_archive(fixture: &Fixture) -> Vec<u8> {
    let staging = fixture.served.join("staging");
    let inner = staging.join("fresh-editor-x");
    std::fs::create_dir_all(&inner).unwrap();
    std::fs::write(inner.join("fresh"), NEW_BINARY).unwrap();

    let out = fixture.served.join(&fixture.asset);
    let flag = if fixture.asset.ends_with(".tar.gz") {
        "-czf"
    } else {
        "-cJf"
    };
    let status = Command::new("tar")
        .arg(flag)
        .arg(&out)
        .arg("-C")
        .arg(&staging)
        .arg("fresh-editor-x")
        .status()
        .expect("run tar");
    assert!(status.success(), "tar failed building {}", fixture.asset);

    std::fs::read(&out).unwrap()
}

/// Serve the fabricated release: feed, asset, checksum sidecar. Returns the
/// base URL. The server lives for the process; the test is short.
fn serve(archive: Vec<u8>, asset: String, sha256_line: String) -> String {
    let server = tiny_http::Server::http("127.0.0.1:0").expect("bind");
    let port = server.server_addr().to_ip().unwrap().port();
    let base = format!("http://127.0.0.1:{port}");

    let feed = format!(
        r#"{{"tag_name":"v{NEW_VERSION}","assets":[
             {{"name":"{asset}","browser_download_url":"{base}/dl/v{NEW_VERSION}/{asset}"}}
           ]}}"#
    );

    std::thread::spawn(move || {
        for request in server.incoming_requests() {
            let url = request.url().to_string();
            let (body, ok): (Vec<u8>, bool) = if url.starts_with("/releases") {
                (feed.clone().into_bytes(), true)
            } else if url.ends_with(&format!("{asset}.sha256")) {
                (sha256_line.clone().into_bytes(), true)
            } else if url.ends_with(&asset) {
                (archive.clone(), true)
            } else {
                (b"not found".to_vec(), false)
            };
            let response =
                tiny_http::Response::from_data(body).with_status_code(if ok { 200 } else { 404 });
            let _ = request.respond(response);
        }
    });

    base
}

/// Run the harness against the fabricated release.
fn run_update(fixture: &Fixture, base: &str) -> std::process::Output {
    Command::new(&fixture.exe)
        .env("HARNESS_CURRENT_VERSION", OLD_VERSION)
        .env("FRESH_RELEASES_URL", format!("{base}/releases/latest"))
        .env("FRESH_DOWNLOAD_BASE", format!("{base}/dl"))
        // The receipt beside the binary must be what decides this; make sure a
        // stray value in the ambient environment cannot be what makes it pass.
        .env_remove("FRESH_INSTALL_CHANNEL")
        .output()
        .expect("run harness")
}

fn sha256_line(archive: &[u8], asset: &str) -> String {
    format!(
        "{}  {asset}\n",
        fresh_update::self_update::sha256_hex(archive)
    )
}

fn report(what: &str, out: &std::process::Output) -> String {
    format!(
        "{what}\n--- stdout ---\n{}\n--- stderr ---\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

/// The whole point: an install that recorded itself as a tarball fetches the
/// announced release and replaces its own binary with it.
#[test]
fn a_tarball_install_updates_itself_in_place() {
    let fixture = install(tempfile::tempdir().unwrap());
    let archive = build_archive(&fixture);
    let line = sha256_line(&archive, &fixture.asset);
    let base = serve(archive, fixture.asset.clone(), line);

    let before = std::fs::read(&fixture.exe).unwrap();
    let out = run_update(&fixture, &base);

    assert!(out.status.success(), "{}", report("update failed", &out));

    let after = std::fs::read(&fixture.exe).unwrap();
    assert_ne!(before, after, "binary was not replaced");
    assert_eq!(after, NEW_BINARY, "binary is not the payload we published");

    // The swap must leave something runnable, not just correct bytes.
    assert!(
        is_executable(&fixture.exe),
        "replacement lost the executable bit"
    );
    let ran = Command::new(&fixture.exe)
        .output()
        .expect("run replacement");
    assert_eq!(String::from_utf8_lossy(&ran.stdout).trim(), "updated-ok");
}

/// Fail-closed: a checksum that does not match the payload must abort the
/// update and leave the installed binary exactly as it was. This is the
/// property that makes the whole path safe to run unattended, so it is asserted
/// on the real engine rather than on `verify_sha256` alone.
#[test]
fn a_bad_checksum_aborts_and_leaves_the_binary_untouched() {
    let fixture = install(tempfile::tempdir().unwrap());
    let archive = build_archive(&fixture);
    let wrong = format!("{}  {}\n", "0".repeat(64), fixture.asset);
    let base = serve(archive, fixture.asset.clone(), wrong);

    let before = std::fs::read(&fixture.exe).unwrap();
    let out = run_update(&fixture, &base);

    assert!(
        !out.status.success(),
        "{}",
        report("a bad checksum was accepted", &out)
    );
    assert_eq!(
        std::fs::read(&fixture.exe).unwrap(),
        before,
        "binary was modified despite a failed checksum"
    );
}

/// A release that publishes nothing under the name this build asks for must
/// fail loudly and change nothing. This is the *symptom* of an asset-name
/// mismatch; the mismatch itself is caught by
/// `archive_ext_matches_the_release_workflows`.
#[test]
fn a_missing_asset_fails_without_touching_the_binary() {
    let fixture = install(tempfile::tempdir().unwrap());
    let archive = build_archive(&fixture);
    let line = sha256_line(&archive, &fixture.asset);
    // Serve it under a name this build will never request.
    let base = serve(
        archive,
        "fresh-editor-some-other-target.tar.gz".to_string(),
        line,
    );

    let before = std::fs::read(&fixture.exe).unwrap();
    let out = run_update(&fixture, &base);

    assert!(
        !out.status.success(),
        "{}",
        report("a missing asset was treated as success", &out)
    );
    assert_eq!(std::fs::read(&fixture.exe).unwrap(), before);
}

/// Without a receipt the install recorded nothing, so it must not swap its own
/// binary — it should report that a step is needed instead. This is the
/// `Unknown` half of dropping the path heuristic: the binary sits in a
/// directory that looks like a tarball install, and that must not be enough.
#[test]
fn an_unrecorded_install_refuses_to_swap_itself() {
    let fixture = install(tempfile::tempdir().unwrap());
    std::fs::remove_file(fixture.exe.parent().unwrap().join("install-receipt.toml")).unwrap();

    let archive = build_archive(&fixture);
    let line = sha256_line(&archive, &fixture.asset);
    let base = serve(archive, fixture.asset.clone(), line);

    let before = std::fs::read(&fixture.exe).unwrap();
    let out = run_update(&fixture, &base);

    assert_eq!(
        out.status.code(),
        Some(fresh_update::EXIT_ACTION_REQUIRED),
        "{}",
        report("expected ActionRequired for an unrecorded install", &out)
    );
    assert_eq!(
        std::fs::read(&fixture.exe).unwrap(),
        before,
        "an unrecorded install swapped its own binary"
    );
}

/// The seam the spine test above cannot see: does [`archive_ext`] agree with
/// what the release workflows *actually publish*?
///
/// This is the defect that shipped on this branch. Every unit test passed and
/// the end-to-end test passed, because both derived the asset name from the
/// same function they were checking — self-consistent, and blind to the
/// function being wrong. What was wrong was the contract with a YAML file in
/// another directory, so the only test that can hold it is one that reads that
/// file.
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

fn is_executable(path: &Path) -> bool {
    std::fs::metadata(path)
        .map(|m| m.permissions().mode() & 0o111 != 0)
        .unwrap_or(false)
}

// Silence the unused-import warning on the non-unix cfg path.
#[allow(dead_code)]
fn _unused(mut w: impl Write) {
    let _ = w.write_all(b"");
}
