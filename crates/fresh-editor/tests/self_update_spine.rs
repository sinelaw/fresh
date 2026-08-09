//! The self-update spine, driven against the **real `fresh` binary**.
//!
//! # Why the real binary
//!
//! An earlier version of this test drove a purpose-built stand-in that called
//! the engine directly. It proved the engine worked and could not prove that
//! `fresh` did: everything between the command line and the engine — flag
//! parsing, the endpoint overrides, the feature gate, the exit-code mapping the
//! editor's update indicator keys off — was untested, and a stand-in that
//! duplicates the caller is a second implementation free to drift from the one
//! that ships. So this spawns `CARGO_BIN_EXE_fresh` and drives it exactly as a
//! user would.
//!
//! # No test-only surface
//!
//! Nothing here needs a flag that exists for testing. `--releases-url` /
//! `--download-base` already ship, for air-gapped and enterprise mirrors, and
//! pointing them anywhere marks the endpoints untrusted — which is what makes
//! this safe: an untrusted endpoint never reaches `sudo`, and the attestation
//! check is skipped because a local server has no attestations and never could.
//!
//! The *version* is faked without any override at all: the fabricated feed
//! announces something far above anything real, so the binary's own
//! `CURRENT_VERSION` is genuinely older and the upgrade path is entered for the
//! real reason. That is the whole trick — no build at a second version, and no
//! way for a test affordance to leak into a release.
//!
//! # Hermetic
//!
//! `tiny_http` serves a feed, an archive built here, and a `.sha256` over it.
//! Nothing reaches the network, and `HOME` / `XDG_*` point into the temp dir so
//! a developer's real config and receipts cannot influence the result.
//!
//! Not covered here: the attestation gate (skipped for an overridden endpoint —
//! it is tested against a real captured GitHub payload in `fresh-update`), and
//! whether the engine asks for the extension the pipelines publish (held by
//! `fresh-update`'s `release_contract` test, which reads the workflow YAML).

#![cfg(all(unix, feature = "self-update"))]

use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Announced by the fabricated release. Far above anything real, so the
/// comparison against the binary's own version can only go one way.
const NEW_VERSION: &str = "99.9.9";

/// Payload published as the "new binary". A script rather than an ELF: the
/// engine extracts the entry by name and writes the bytes without inspecting
/// them, so this proves the swap landed *and* stays runnable, which proves the
/// executable bit survived it.
const NEW_BINARY: &[u8] = b"#!/bin/sh\necho updated-ok\n";

struct Install {
    _dir: tempfile::TempDir,
    root: PathBuf,
    /// The copy of `fresh` that will replace itself.
    exe: PathBuf,
    /// Name of the asset this build will ask the release for.
    asset: String,
}

/// Lay out an install of the real binary, with the receipt that makes it
/// resolve as a self-updating tarball install — byte-for-byte what `install.sh`
/// writes and what the musl archive ships.
fn install(with_receipt: bool) -> Install {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path().to_path_buf();
    let install_dir = root.join("install");
    std::fs::create_dir_all(&install_dir).unwrap();
    std::fs::create_dir_all(root.join("home")).unwrap();
    std::fs::create_dir_all(root.join("served")).unwrap();

    let exe = install_dir.join("fresh");
    place_binary(Path::new(env!("CARGO_BIN_EXE_fresh")), &exe);
    std::fs::set_permissions(&exe, std::fs::Permissions::from_mode(0o755)).unwrap();

    if with_receipt {
        std::fs::write(
            install_dir.join("install-receipt.toml"),
            "schema = 1\n\
             channel = \"tarball\"\n\
             package_name = \"fresh-editor\"\n\
             managed = false\n\
             self_update = true\n",
        )
        .unwrap();
    }

    let asset = format!(
        "fresh-editor-{}.{}",
        fresh_update::TARGET_TRIPLE,
        fresh_update::engine::archive_ext(fresh_update::TARGET_TRIPLE)
    );

    Install {
        _dir: dir,
        root,
        exe,
        asset,
    }
}

/// Hard-link the binary if we can, copy if we cannot.
///
/// A debug `fresh` is large and each test needs its own copy. Linking is safe
/// because the swap is a rename over the path, not a write through the inode,
/// so the build output cannot be clobbered — but `/tmp` is often a different
/// filesystem, hence the fallback.
fn place_binary(from: &Path, to: &Path) {
    if std::fs::hard_link(from, to).is_err() {
        std::fs::copy(from, to).expect("copy fresh binary");
    }
}

/// Build the release archive in whichever format this target publishes, using
/// the system `tar` — the same tool the release workflows use, so the bytes
/// under test are shaped like real ones rather than like this crate's encoder.
fn build_archive(install: &Install) -> Vec<u8> {
    let staging = install.root.join("staging");
    let inner = staging.join("fresh-editor-x");
    std::fs::create_dir_all(&inner).unwrap();
    std::fs::write(inner.join("fresh"), NEW_BINARY).unwrap();

    let out = install.root.join("served").join(&install.asset);
    let flag = if install.asset.ends_with(".tar.gz") {
        "-czf"
    } else {
        "-cJf"
    };
    let status = Command::new("tar")
        .args([flag, out.to_str().unwrap(), "-C", staging.to_str().unwrap()])
        .arg("fresh-editor-x")
        .status()
        .expect("run tar");
    assert!(status.success(), "tar failed building {}", install.asset);
    std::fs::read(&out).unwrap()
}

/// Serve the fabricated release. Returns the base URL.
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
            let (body, code) = if url.starts_with("/releases") {
                (feed.clone().into_bytes(), 200)
            } else if url.ends_with(&format!("{asset}.sha256")) {
                (sha256_line.clone().into_bytes(), 200)
            } else if url.ends_with(&asset) {
                (archive.clone(), 200)
            } else {
                (b"not found".to_vec(), 404)
            };
            let _ = request.respond(tiny_http::Response::from_data(body).with_status_code(code));
        }
    });

    base
}

/// Run the installed binary exactly as a user would, pointed at the fake
/// release. `extra` carries the mode flags under test.
fn run_update(install: &Install, base: &str, extra: &[&str]) -> Output {
    let mut cmd = Command::new(&install.exe);
    cmd.arg("--cmd")
        .arg("update")
        .arg("--releases-url")
        .arg(format!("{base}/releases/latest"))
        .arg("--download-base")
        .arg(format!("{base}/dl"))
        .args(extra)
        // Hermetic: a developer's real config, data dir or receipt must not be
        // able to decide the outcome.
        .env("HOME", install.root.join("home"))
        .env("XDG_DATA_HOME", install.root.join("home"))
        .env("XDG_CONFIG_HOME", install.root.join("home"))
        .env_remove("FRESH_INSTALL_CHANNEL")
        .env_remove("FRESH_RELEASES_URL")
        .env_remove("FRESH_DOWNLOAD_BASE");
    cmd.output().expect("run fresh --cmd update")
}

fn sha256_line(archive: &[u8], asset: &str) -> String {
    format!(
        "{}  {asset}\n",
        fresh_update::self_update::sha256_hex(archive)
    )
}

fn report(what: &str, out: &Output) -> String {
    format!(
        "{what} (exit {:?})\n--- stdout ---\n{}\n--- stderr ---\n{}",
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

/// Set up an install serving a well-formed release, ready to update.
fn ready(with_receipt: bool) -> (Install, String) {
    let install = install(with_receipt);
    let archive = build_archive(&install);
    let line = sha256_line(&archive, &install.asset);
    let base = serve(archive, install.asset.clone(), line);
    (install, base)
}

/// The whole point: a recorded tarball install fetches the announced release
/// and replaces its own binary, through the real command-line entry point.
#[test]
fn fresh_updates_itself_in_place() {
    let (install, base) = ready(true);
    let before = std::fs::read(&install.exe).unwrap();

    let out = run_update(&install, &base, &["--yes"]);
    assert!(out.status.success(), "{}", report("update failed", &out));

    let after = std::fs::read(&install.exe).unwrap();
    assert_ne!(before, after, "binary was not replaced");
    assert_eq!(after, NEW_BINARY, "binary is not the payload we published");
    assert!(
        std::fs::metadata(&install.exe)
            .unwrap()
            .permissions()
            .mode()
            & 0o111
            != 0,
        "replacement lost the executable bit"
    );

    // The replaced file has to be a working program, not just correct bytes.
    let ran = Command::new(&install.exe)
        .output()
        .expect("run replacement");
    assert_eq!(String::from_utf8_lossy(&ran.stdout).trim(), "updated-ok");
}

/// `--check` reports and stops. The binary must be untouched, because this is
/// the mode the editor's background check uses.
#[test]
fn check_reports_without_replacing_anything() {
    let (install, base) = ready(true);
    let before = std::fs::read(&install.exe).unwrap();

    let out = run_update(&install, &base, &["--check"]);
    assert!(out.status.success(), "{}", report("--check failed", &out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains(NEW_VERSION),
        "{}",
        report("--check did not report the available version", &out)
    );
    assert_eq!(
        std::fs::read(&install.exe).unwrap(),
        before,
        "--check replaced the binary"
    );
}

/// Without `--yes` the swap is described, not performed, and the exit code is
/// the distinct ActionRequired one the update indicator keys off.
#[test]
fn without_yes_it_describes_the_swap_and_stops() {
    let (install, base) = ready(true);
    let before = std::fs::read(&install.exe).unwrap();

    let out = run_update(&install, &base, &[]);
    assert_eq!(
        out.status.code(),
        Some(fresh_update::EXIT_ACTION_REQUIRED),
        "{}",
        report("expected ActionRequired without --yes", &out)
    );
    assert_eq!(std::fs::read(&install.exe).unwrap(), before);
}

/// Fail-closed: a checksum that does not match must abort and leave the
/// installed binary byte-identical. This is what makes the path safe to run
/// unattended, so it is asserted on the shipped binary, not on `verify_sha256`.
#[test]
fn a_bad_checksum_aborts_and_leaves_the_binary_untouched() {
    let install = install(true);
    let archive = build_archive(&install);
    let wrong = format!("{}  {}\n", "0".repeat(64), install.asset);
    let base = serve(archive, install.asset.clone(), wrong);

    let before = std::fs::read(&install.exe).unwrap();
    let out = run_update(&install, &base, &["--yes"]);

    assert!(
        !out.status.success(),
        "{}",
        report("a bad checksum was accepted", &out)
    );
    assert_eq!(
        std::fs::read(&install.exe).unwrap(),
        before,
        "binary was modified despite a failed checksum"
    );
}

/// A release publishing nothing under the name this build asks for must fail
/// loudly and change nothing — the symptom of an asset-name mismatch. The
/// mismatch itself is caught by `fresh-update`'s release_contract test.
#[test]
fn a_missing_asset_fails_without_touching_the_binary() {
    let install = install(true);
    let archive = build_archive(&install);
    let line = sha256_line(&archive, &install.asset);
    let base = serve(
        archive,
        "fresh-editor-some-other-target.tar.gz".into(),
        line,
    );

    let before = std::fs::read(&install.exe).unwrap();
    let out = run_update(&install, &base, &["--yes"]);

    assert!(
        !out.status.success(),
        "{}",
        report("a missing asset was treated as success", &out)
    );
    assert_eq!(std::fs::read(&install.exe).unwrap(), before);
}

/// No receipt means nothing recorded how this copy was installed, so it must
/// refuse to overwrite itself however much the surroundings look like a tarball
/// install. This is the `Unknown` half of dropping the path heuristic.
#[test]
fn an_unrecorded_install_refuses_to_swap_itself() {
    let (install, base) = ready(false);
    let before = std::fs::read(&install.exe).unwrap();

    let out = run_update(&install, &base, &["--yes"]);
    assert_ne!(
        out.status.code(),
        Some(0),
        "{}",
        report("an unrecorded install performed a swap", &out)
    );
    assert_eq!(
        std::fs::read(&install.exe).unwrap(),
        before,
        "an unrecorded install swapped its own binary"
    );
}
