//! The self-update spine, driven against the real `fresh` binary.
//!
//! An earlier version drove a purpose-built stand-in that called the engine
//! directly. That proved the engine worked, not that `fresh` did: flag
//! parsing, endpoint overrides, the feature gate and the exit-code mapping the
//! update indicator keys off were all untested, and a stand-in duplicating the
//! caller is free to drift from the one that ships.
//!
//! No test-only surface. `--releases-url` / `--download-base` already ship for
//! air-gapped mirrors, and pointing them anywhere marks the endpoints
//! untrusted — which is what makes this safe. The *version* is faked with no
//! override at all: the fabricated feed announces something far above anything
//! real, so the upgrade path is entered for the real reason, with no build at
//! a second version and no affordance that could leak into a release.
//!
//! Not covered here: the attestation gate (skipped for an overridden endpoint;
//! tested against a captured GitHub payload in `fresh-update`) and whether the
//! engine asks for the extension the pipelines publish (`release_contract`).

#![cfg(all(unix, feature = "self-update"))]

use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Announced by the fabricated release. Far above anything real, so the
/// comparison against the binary's own version can only go one way.
const NEW_VERSION: &str = "99.9.9";

/// A script rather than an ELF: the engine writes the entry without
/// inspecting it, so running the result proves both the swap and that the
/// executable bit survived.
const NEW_BINARY: &[u8] = b"#!/bin/sh\necho updated-ok\n";

/// The triple this build asks the release for.
const TRIPLE: &str = fresh_update::TARGET_TRIPLE;

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
    // Inside `target/`, not `/tmp`: each case needs its own copy of the binary,
    // and `place_binary` hard-links to avoid the bytes — which only works within
    // one filesystem. `CARGO_TARGET_TMPDIR` is guaranteed to sit beside the
    // build output, so the link succeeds instead of falling back to a copy.
    let dir = tempfile::Builder::new()
        .prefix("self-update-spine-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir");
    let root = dir.path().to_path_buf();
    let install_dir = root.join("install");
    std::fs::create_dir_all(&install_dir).unwrap();
    std::fs::create_dir_all(root.join("home")).unwrap();
    std::fs::create_dir_all(root.join("served")).unwrap();

    let exe = install_dir.join("fresh");
    place_binary(&exe);
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
        TRIPLE,
        fresh_update::engine::archive_ext(TRIPLE)
    );

    Install {
        _dir: dir,
        root,
        exe,
        asset,
    }
}

/// A debug `fresh` is ~500 MB and every case needs its own copy to overwrite,
/// so it is stripped once into a cache (~90 MB) that cases hard-link.
///
/// Linking is safe because the swap renames over the path rather than writing
/// through the inode. The cache exists because stripping a hard link would
/// strip the build output itself.
fn place_binary(to: &Path) {
    let source = stripped_binary();
    if std::fs::hard_link(&source, to).is_err() {
        std::fs::copy(&source, to).expect("copy fresh binary");
    }
}

/// nextest gives each case its own process, so several may strip at once: each
/// writes a private name and renames into place, so the losers of the race
/// overwrite identical content and any reader holds a complete file.
///
/// Falls back to the unstripped binary if `strip` is unavailable.
fn stripped_binary() -> PathBuf {
    let original = PathBuf::from(env!("CARGO_BIN_EXE_fresh"));
    let cache_dir = Path::new(env!("CARGO_TARGET_TMPDIR"));
    let cached = cache_dir.join("fresh-stripped");
    if cached.is_file() {
        return cached;
    }

    let staging = cache_dir.join(format!("fresh-stripped.{}", std::process::id()));
    let stripped = Command::new("strip")
        .arg("-o")
        .arg(&staging)
        .arg(&original)
        .status()
        .map(|s| s.success())
        .unwrap_or(false);

    if stripped && staging.is_file() && std::fs::rename(&staging, &cached).is_ok() {
        return cached;
    }
    let _ = std::fs::remove_file(&staging);
    original
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

/// Put a sentinel-writing stand-in for `name` on `PATH`. If `fresh` ever
/// spawns it, the file it writes proves the call happened — which is the only
/// way to assert a *negative* about execution.
fn sabotage(install: &Install, name: &str) -> PathBuf {
    let bin = install.root.join("fakebin");
    std::fs::create_dir_all(&bin).unwrap();
    let sentinel = install.root.join(format!("{name}.was-run"));
    let script = format!("#!/bin/sh\ntouch '{}'\nexit 0\n", sentinel.display());
    let path = bin.join(name);
    std::fs::write(&path, script).unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    sentinel
}

/// Run with the sabotaged directory first on `PATH`.
fn run_update_with_fakebin(install: &Install, base: &str, extra: &[&str]) -> Output {
    let bin = install.root.join("fakebin");
    let path = format!(
        "{}:{}",
        bin.display(),
        std::env::var("PATH").unwrap_or_default()
    );
    let mut cmd = Command::new(&install.exe);
    cmd.arg("--cmd")
        .arg("update")
        .arg("--releases-url")
        .arg(format!("{base}/releases/latest"))
        .arg("--download-base")
        .arg(format!("{base}/dl"))
        .args(extra)
        .env("PATH", path)
        .env("HOME", install.root.join("home"))
        .env("XDG_DATA_HOME", install.root.join("home"))
        .env("XDG_CONFIG_HOME", install.root.join("home"))
        .env_remove("FRESH_INSTALL_CHANNEL");
    cmd.output().expect("run fresh --cmd update")
}

/// Write a receipt for an arbitrary channel, replacing whatever is there.
fn record_channel(install: &Install, channel: &str) {
    std::fs::write(
        install.exe.parent().unwrap().join("install-receipt.toml"),
        format!("schema = 1\nchannel = \"{channel}\"\npackage_name = \"fresh-editor\"\n"),
    )
    .unwrap();
}

/// A channel someone else owns is *named*, never run — even with `--yes`, and
/// even though the tool is right there on `PATH`. This is the rule: `fresh`
/// writes files it owns and nothing else.
#[test]
fn a_delegated_channel_prints_its_command_and_runs_nothing() {
    let (install, base) = ready(true);
    record_channel(&install, "homebrew");
    let brew_ran = sabotage(&install, "brew");
    let before = std::fs::read(&install.exe).unwrap();

    let out = run_update_with_fakebin(&install, &base, &["--yes"]);

    assert_eq!(
        out.status.code(),
        Some(fresh_update::EXIT_ACTION_REQUIRED),
        "{}",
        report("a delegated channel did not stop at ActionRequired", &out)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("brew upgrade"),
        "{}",
        report("the command the user must run was never named", &out)
    );
    assert!(
        !brew_ran.exists(),
        "{}",
        report("fresh executed `brew` on the user's behalf", &out)
    );
    assert_eq!(std::fs::read(&install.exe).unwrap(), before);
}

/// A release package is still fetched and checksum-verified for the user —
/// that is the only verification they would get — but installing it belongs to
/// dpkg, so neither `dpkg` nor `sudo` may be spawned.
#[test]
fn a_release_package_is_verified_but_never_installed() {
    let install = install(true);
    record_channel(&install, "apt");

    let spec = fresh_update::registry::package_asset(fresh_update::Channel::Apt, TRIPLE)
        .expect("apt publishes a package for this target");
    let deb_name = format!(
        "fresh-editor_{NEW_VERSION}-1_{}{}",
        spec.arch, spec.extension
    );
    let deb = b"not really a package, but the checksum will match".to_vec();
    let line = sha256_line(&deb, &deb_name);
    let base = serve(deb, deb_name.clone(), line);

    let dpkg_ran = sabotage(&install, "dpkg");
    let sudo_ran = sabotage(&install, "sudo");
    let before = std::fs::read(&install.exe).unwrap();

    let out = run_update_with_fakebin(&install, &base, &["--yes"]);
    let stdout = String::from_utf8_lossy(&out.stdout);

    assert_eq!(
        out.status.code(),
        Some(fresh_update::EXIT_ACTION_REQUIRED),
        "{}",
        report("a release package did not stop at ActionRequired", &out)
    );
    assert!(
        stdout.contains("Downloaded to"),
        "{}",
        report("the package was not fetched, so nothing verified it", &out)
    );
    assert!(
        !dpkg_ran.exists() && !sudo_ran.exists(),
        "{}",
        report(
            "fresh installed a package instead of naming the command",
            &out
        )
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
