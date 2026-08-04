//! The `fresh update` engine (behind the `self-update` feature).
//!
//! Ties together the pieces: resolve provenance (`fresh_update`), fetch the
//! release feed (`services::http`), then take one of three routes — **delegate**
//! to the owning package manager, **download the release package** and hand it
//! to the local package tool (`.deb`/`.rpm`/`.flatpak`, which ship as release
//! artifacts rather than through a repository), or perform a verified **in-place
//! swap** for self-contained installs (raw tarball / AppImage).
//!
//! Extraction (`tar`/`xz`/`zip`) and networking live here so the `fresh-update`
//! crate stays dependency-light; the crate owns the parts that must be correct
//! (checksum verification + the atomic swap).

// This module does a lot of best-effort filesystem cleanup (removing staged
// temp files / rollback dirs) where a failed unlink is genuinely ignorable.
// The crate denies `let_underscore_must_use` at the root; allow it here for
// those intentional fire-and-forget calls (same convention as release_checker).
#![allow(clippy::let_underscore_must_use)]

use super::http;
use super::release_checker::CURRENT_VERSION;
use fresh_update::registry::UpdateKind;
use fresh_update::{self_update, Channel, Provenance};
use std::io::Read;
use std::path::{Path, PathBuf};

const REPO: &str = "sinelaw/fresh";

/// Options parsed from the `fresh update` command line.
#[derive(Debug, Clone)]
pub struct UpdateOptions {
    /// Only report status; make no changes.
    pub check_only: bool,
    /// Run the update without an interactive confirmation.
    pub yes: bool,
    /// Permit "updating" to the same or an older version.
    pub allow_downgrade: bool,
    /// Run the update even when the current version is already the latest.
    /// Distinct from `allow_downgrade`, which is about *which* versions are
    /// acceptable; this is about running the install path at all. Reinstalls
    /// the current version, which is how the packaging containers exercise the
    /// real download → verify → install flow without waiting for a new release.
    pub force: bool,
    /// Print the command that would install the update and stop, instead of
    /// running it. The escape hatch behind the popup's "Show the command"
    /// choice, for users who would rather run it themselves.
    pub print_command: bool,
    /// The releases API URL (overridable for testing).
    pub releases_url: String,
    /// Base URL for release asset downloads (overridable for testing).
    pub download_base: String,
}

impl Default for UpdateOptions {
    fn default() -> Self {
        UpdateOptions {
            check_only: false,
            yes: false,
            allow_downgrade: false,
            print_command: false,
            force: false,
            // Same env overrides the editor's background check reads, so the
            // child the popup spawns inherits them without argv plumbing.
            releases_url: super::release_checker::releases_url(),
            download_base: std::env::var(super::release_checker::DOWNLOAD_BASE_ENV)
                .ok()
                .filter(|v| !v.trim().is_empty())
                .unwrap_or_else(|| format!("https://github.com/{REPO}/releases/download")),
        }
    }
}

/// The outcome of a successful [`run`], for callers that map it to a process
/// exit code.
pub enum UpdateStatus {
    /// The new version is installed (in-place swap, or a package-manager
    /// command we ran to completion), or `--check` reported status. Nothing
    /// more to do — exit success.
    Done,
    /// The update was **not** applied, and finishing it needs a step we won't
    /// take for the user: the package was downloaded and verified but
    /// installing it needs root, the owning package manager's command was
    /// printed rather than run, or there is no in-place mechanism at all
    /// (unknown / source).
    ///
    /// `run` already printed what to do, so the caller exits
    /// [`fresh_update::EXIT_ACTION_REQUIRED`] and prints nothing further — this
    /// is not an error. Reporting it as success would have the editor's
    /// indicator claim an update that never landed.
    ActionRequired,
}

/// Run `fresh update`. Prints human-readable progress; returns an error string
/// on genuine failure, or an [`UpdateStatus`] otherwise. Never escalates
/// privilege itself.
pub fn run(opts: &UpdateOptions) -> Result<UpdateStatus, String> {
    // Same entry point the popup used, so the command we run is the one the
    // user was shown.
    let prov = super::release_checker::detect_provenance();
    println!(
        "Installed via: {} (confidence: {:?})",
        prov.channel.label(),
        prov.confidence
    );

    let body = http::get_release_json(&opts.releases_url)?;
    let check = fresh_update::check::evaluate_with(CURRENT_VERSION, &body, prov.clone())?;
    let latest = check.latest_version.clone();

    println!("Current version: {CURRENT_VERSION}");
    println!("Latest version:  {latest}");

    if !check.update_available && !opts.allow_downgrade && !opts.force {
        println!("You are already on the latest version.");
        return Ok(UpdateStatus::Done);
    }
    if !check.update_available && opts.force {
        println!("Already on {latest}; --force given, reinstalling it anyway.");
    }

    let plan = fresh_update::plan(&prov);

    match plan.kind {
        UpdateKind::SelfContained => {
            if opts.check_only {
                println!(
                    "An update is available. Run `fresh --cmd update` to install it in place."
                );
                return Ok(UpdateStatus::Done);
            }
            self_contained_update(&prov, &latest, opts).map(|()| UpdateStatus::Done)
        }
        UpdateKind::DownloadPackage => {
            if opts.check_only {
                println!("An update is available. Run `fresh --cmd update` to fetch it.");
                return Ok(UpdateStatus::Done);
            }
            package_update(&prov, &latest, &body, opts)
        }
        UpdateKind::Delegated | UpdateKind::Toolchain => {
            let cmd = plan.command.clone().unwrap_or_default();
            if opts.check_only {
                println!("An update is available. Update with: {}", plan.human);
                return Ok(UpdateStatus::Done);
            }
            if opts.print_command || !opts.yes {
                // The user asked to see it rather than run it (or this is an
                // unattended CLI invocation without --yes). Printing a command
                // is not performing it, so this is ActionRequired, not Done.
                show_command(&elevated(&cmd, plan.needs_privilege));
                return Ok(UpdateStatus::ActionRequired);
            }
            run_install(&cmd, plan.needs_privilege).map(|()| UpdateStatus::Done)
        }
        UpdateKind::Manual => {
            let url = format!("https://github.com/{REPO}/releases/tag/v{latest}");
            if opts.check_only {
                // `--check` only reports availability; a manual install still
                // "has an update", so this is informational, not a failure.
                println!("A new version of Fresh is available: {CURRENT_VERSION} → {latest}");
                println!("Download it from: {url}");
                Ok(UpdateStatus::Done)
            } else {
                // Unknown / source install: no in-place mechanism. Friendly
                // guidance — deliberately not phrased as an error — but the
                // caller still exits non-zero so the editor's update indicator
                // doesn't report a phantom "Updated" that never happened.
                println!("A new version of Fresh is available: {CURRENT_VERSION} → {latest}");
                println!();
                println!("This copy of Fresh was installed manually, so it can't update itself.");
                println!("To update, download the latest release:");
                println!();
                println!("    {url}");
                println!();
                println!("and replace your current fresh binary with the new one.");
                Ok(UpdateStatus::ActionRequired)
            }
        }
    }
}

/// Run a delegated package-manager command (e.g. `brew upgrade fresh-editor`).
fn run_delegated(cmd: &[String]) -> Result<(), String> {
    println!("Running: {}", cmd.join(" "));
    let status = std::process::Command::new(&cmd[0])
        .args(&cmd[1..])
        .status()
        .map_err(|e| format!("failed to run `{}`: {e}", cmd[0]))?;
    if status.success() {
        println!("Update complete.");
        Ok(())
    } else {
        Err(format!("`{}` exited with {status}", cmd.join(" ")))
    }
}

/// Fetch and install the release artifact for a channel whose package manager
/// has no repository to upgrade from (`.deb`, `.rpm`, `.flatpak`).
///
/// The binary is owned by dpkg/rpm/flatpak, so an in-place swap is off the
/// table — it would leave the package database describing a file that no longer
/// exists. Instead we download and checksum-verify the new package and hand it
/// to the local package tool, elevating with `sudo`/`doas` when the tool needs
/// root — we run in an interactive PTY, so the password prompt works and the
/// update finishes in one step. `--print-command` (the popup's "Show the
/// command" choice) stops after the download and prints instead, reporting
/// [`UpdateStatus::ActionRequired`] so the indicator doesn't claim an update
/// that hasn't happened.
fn package_update(
    prov: &Provenance,
    latest: &str,
    release_json: &str,
    opts: &UpdateOptions,
) -> Result<UpdateStatus, String> {
    let channel = prov.channel;
    // Prefer the arch the installer recorded over one re-derived from this
    // build's target triple.
    let asset = fresh_update::registry::package_asset_with(
        channel,
        fresh_update::TARGET_TRIPLE,
        prov.hints.pkg_arch.as_deref(),
    )
    .ok_or_else(|| format!("no release package is published for {}", channel.label()))?;

    // Always resolved from the release feed. Package filenames carry a packaging
    // release number (`0.4.7-1`) that the version alone doesn't give us, and a
    // name recorded at install time describes the version we already have — so
    // the feed is the only source that can be right, and it is the only one
    // consulted.
    let _ = latest;
    let url = fresh_update::version::find_asset_url(release_json, asset.extension, &asset.arch)
        .ok_or_else(|| {
            format!(
                "the latest release has no {} package for {}",
                asset.extension, asset.arch
            )
        })?;

    let bytes = fetch_and_verify(&url)?;

    let file_name = url.rsplit('/').next().unwrap_or("fresh-update-package");
    let path = std::env::temp_dir().join(file_name);
    std::fs::write(&path, &bytes).map_err(|e| format!("write {}: {e}", path.display()))?;
    println!("Downloaded to {}", path.display());

    // `--force` reinstalls the version already present, which rpm rejects
    // unless told otherwise.
    let cmd = fresh_update::registry::install_command_with(channel, &path, opts.force)
        .ok_or_else(|| format!("no install command for {}", channel.label()))?;
    let needs_privilege = fresh_update::plan(prov).needs_privilege;

    if opts.print_command || !opts.yes {
        println!();
        show_command(&elevated(&cmd, needs_privilege));
        return Ok(UpdateStatus::ActionRequired);
    }

    println!();
    run_install(&cmd, needs_privilege).map(|()| UpdateStatus::Done)
}

/// Print a command for the user to run, rather than running it.
fn show_command(cmd: &[String]) {
    println!("Install it with:");
    println!();
    println!("    {}", cmd.join(" "));
}

/// The argv to actually execute, elevated when the install needs root.
///
/// One mechanism: `sudo`. Probing for `doas`, or falling back to running the
/// command unprivileged when neither is present, would mean the same provenance
/// class installed itself differently depending on the machine — and the
/// unprivileged fallback in particular fails in a way that looks like a
/// packaging bug rather than a missing `sudo`. `sudo` run as root is a no-op,
/// so this is correct for a root shell too; a system without it gets a plain
/// "command not found" naming exactly what is missing.
fn elevated(cmd: &[String], needs_privilege: bool) -> Vec<String> {
    if !needs_privilege {
        return cmd.to_vec();
    }
    let mut argv = vec!["sudo".to_string()];
    argv.extend_from_slice(cmd);
    argv
}

/// Run the install command, elevating it when it needs root.
///
/// `fresh --cmd update` runs in an interactive PTY (the editor launches it in a
/// local terminal buffer), so `sudo` can prompt for a password right there.
/// That is what lets a `.deb`/`.rpm` update finish in one step instead of
/// handing the user a command to go and run somewhere else.
fn run_install(cmd: &[String], needs_privilege: bool) -> Result<(), String> {
    run_delegated(&elevated(cmd, needs_privilege))
}

/// Download `url` and check it against its `.sha256` sidecar.
fn fetch_and_verify(url: &str) -> Result<Vec<u8>, String> {
    println!("Downloading {url} ...");
    let bytes = download(url)?;
    verify(&bytes, &format!("{url}.sha256"))?;
    Ok(bytes)
}

/// Perform a verified in-place update for a self-contained install.
fn self_contained_update(
    prov: &Provenance,
    latest: &str,
    opts: &UpdateOptions,
) -> Result<(), String> {
    let target = fresh_update::TARGET_TRIPLE;

    if prov.channel == Channel::Appimage {
        return appimage_update(prov, latest, opts, target);
    }

    // Tarball / prebuilt: download the archive, verify, extract the inner
    // binary, and atomically swap the running executable.
    // Computed from the target triple this binary was compiled for — a
    // compile-time fact, not a guess, and the same answer every time. A name
    // recorded in the receipt describes the version already installed, so it
    // is not consulted.
    let ext = if cfg!(windows) { "zip" } else { "tar.xz" };
    let asset = format!("fresh-editor-{target}.{ext}");
    let url = format!("{}/v{latest}/{asset}", opts.download_base);

    let bin_name = if cfg!(windows) { "fresh.exe" } else { "fresh" };
    let binary = fetch_and_extract_binary(&url, bin_name)?;

    let exe = std::env::current_exe().map_err(|e| format!("cannot find current exe: {e}"))?;
    println!("Installing to {} ...", exe.display());
    self_update::atomic_replace(&exe, &binary).map_err(|e| e.to_string())?;

    println!("Updated to {latest}. Restart fresh to use the new version.");
    Ok(())
}

/// AppImage self-update: download the new AppImage, verify, extract its
/// squashfs, and atomically replace the install root created by install.sh.
/// Unix-only (AppImages don't exist on Windows/macOS).
fn appimage_update(
    prov: &Provenance,
    latest: &str,
    opts: &UpdateOptions,
    target: &str,
) -> Result<(), String> {
    let arch = target.split('-').next().unwrap_or("x86_64");
    let asset = format!("fresh-editor-{latest}-{arch}.AppImage");
    let url = format!("{}/v{latest}/{asset}", opts.download_base);

    let install_root = prov.hints.install_root.as_deref().ok_or_else(|| {
        "AppImage install has no recorded install_root; reinstall via install.sh".to_string()
    })?;

    let bytes = fetch_and_verify(&url)?;

    // Stage the AppImage next to the install root and extract it.
    let root = PathBuf::from(install_root);
    let parent = root.parent().unwrap_or_else(|| Path::new("."));
    let staged = parent.join(format!(".fresh-update-{}.AppImage", std::process::id()));
    std::fs::write(&staged, &bytes).map_err(|e| format!("write staged AppImage: {e}"))?;
    make_executable(&staged)?;

    let workdir = parent.join(format!(".fresh-update-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&workdir);
    std::fs::create_dir_all(&workdir).map_err(|e| format!("create workdir: {e}"))?;
    let status = std::process::Command::new(&staged)
        .arg("--appimage-extract")
        .current_dir(&workdir)
        .status()
        .map_err(|e| format!("failed to extract AppImage: {e}"))?;
    let _ = std::fs::remove_file(&staged);
    if !status.success() {
        let _ = std::fs::remove_dir_all(&workdir);
        return Err("AppImage extraction failed".to_string());
    }

    let new_root = workdir.join("squashfs-root");
    if !new_root.is_dir() {
        let _ = std::fs::remove_dir_all(&workdir);
        return Err("AppImage extraction produced no squashfs-root".to_string());
    }

    // Swap the install root: move the old aside, move the new in.
    let backup = parent.join(format!(".{}-old", file_name(&root)));
    let _ = std::fs::remove_dir_all(&backup);
    if root.exists() {
        std::fs::rename(&root, &backup).map_err(|e| format!("move old install aside: {e}"))?;
    }
    match std::fs::rename(&new_root, &root) {
        Ok(()) => {
            let _ = std::fs::remove_dir_all(&backup);
            let _ = std::fs::remove_dir_all(&workdir);
            println!("Updated to {latest}. Restart fresh to use the new version.");
            Ok(())
        }
        Err(e) => {
            // Roll back.
            let _ = std::fs::rename(&backup, &root);
            let _ = std::fs::remove_dir_all(&workdir);
            Err(format!("failed to install new AppImage payload: {e}"))
        }
    }
}

fn file_name(p: &Path) -> String {
    p.file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "fresh-editor".to_string())
}

/// Download an archive, verify its SHA-256 sidecar (`<url>.sha256`), and
/// extract the named inner binary. Shared by the real update path and tests;
/// deliberately does *not* touch the running executable.
fn fetch_and_extract_binary(url: &str, bin_name: &str) -> Result<Vec<u8>, String> {
    let bytes = fetch_and_verify(url)?;
    if url.ends_with(".zip") {
        extract_from_zip(&bytes, bin_name)
    } else {
        extract_from_tar_xz(&bytes, bin_name)
    }
}

/// Download a URL fully into memory (self-update assets are a few MB).
fn download(url: &str) -> Result<Vec<u8>, String> {
    let dir = std::env::temp_dir();
    let tmp = dir.join(format!("fresh-update-dl-{}", std::process::id()));
    http::download_to_file(url, &tmp)?;
    let bytes = std::fs::read(&tmp).map_err(|e| format!("read download: {e}"));
    let _ = std::fs::remove_file(&tmp);
    bytes
}

/// Fetch the `.sha256` sidecar and verify `bytes` against it. Fail-closed.
fn verify(bytes: &[u8], sha_url: &str) -> Result<(), String> {
    println!("Verifying checksum ...");
    let expected = http::get_release_json(sha_url)
        .map_err(|e| format!("could not fetch checksum ({sha_url}): {e}"))?;
    self_update::verify_sha256(bytes, expected.trim()).map_err(|e| e.to_string())
}

/// Extract a named file from a `.tar.xz` archive held in memory.
fn extract_from_tar_xz(bytes: &[u8], name: &str) -> Result<Vec<u8>, String> {
    let decoder = xz2::read::XzDecoder::new(bytes);
    let mut archive = tar::Archive::new(decoder);
    let entries = archive.entries().map_err(|e| format!("read tar: {e}"))?;
    for entry in entries {
        let mut entry = entry.map_err(|e| format!("read tar entry: {e}"))?;
        let is_match = entry
            .path()
            .ok()
            .and_then(|p| p.file_name().map(|n| n == name))
            .unwrap_or(false);
        if is_match {
            let mut buf = Vec::new();
            entry
                .read_to_end(&mut buf)
                .map_err(|e| format!("read binary from tar: {e}"))?;
            return Ok(buf);
        }
    }
    Err(format!("`{name}` not found in archive"))
}

/// Extract a named file from a `.zip` archive held in memory.
fn extract_from_zip(bytes: &[u8], name: &str) -> Result<Vec<u8>, String> {
    let reader = std::io::Cursor::new(bytes);
    let mut zip = zip::ZipArchive::new(reader).map_err(|e| format!("open zip: {e}"))?;
    for i in 0..zip.len() {
        let mut file = zip
            .by_index(i)
            .map_err(|e| format!("read zip entry: {e}"))?;
        let matches = Path::new(file.name())
            .file_name()
            .map(|n| n == name)
            .unwrap_or(false);
        if matches {
            let mut buf = Vec::new();
            file.read_to_end(&mut buf)
                .map_err(|e| format!("read binary from zip: {e}"))?;
            return Ok(buf);
        }
    }
    Err(format!("`{name}` not found in archive"))
}

#[cfg(unix)]
fn make_executable(path: &Path) -> Result<(), String> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(path)
        .map_err(|e| e.to_string())?
        .permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(path, perms).map_err(|e| e.to_string())
}

#[cfg(not(unix))]
fn make_executable(_path: &Path) -> Result<(), String> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    /// Build a `.tar.xz` in memory containing a single `fresh` entry.
    fn make_tar_xz(bin: &[u8]) -> Vec<u8> {
        let mut tar_bytes = Vec::new();
        {
            let mut builder = tar::Builder::new(&mut tar_bytes);
            let mut header = tar::Header::new_gnu();
            header.set_size(bin.len() as u64);
            header.set_mode(0o755);
            header.set_cksum();
            builder
                .append_data(&mut header, "fresh-editor-x/fresh", bin)
                .unwrap();
            builder.finish().unwrap();
        }
        let mut xz = Vec::new();
        {
            let mut enc = xz2::write::XzEncoder::new(&mut xz, 6);
            enc.write_all(&tar_bytes).unwrap();
            enc.finish().unwrap();
        }
        xz
    }

    #[test]
    fn extract_tar_xz_finds_binary() {
        let archive = make_tar_xz(b"#!/bin/sh\necho fresh\n");
        let got = extract_from_tar_xz(&archive, "fresh").unwrap();
        assert_eq!(got, b"#!/bin/sh\necho fresh\n");
    }

    #[test]
    fn extract_tar_xz_missing_binary_errors() {
        let archive = make_tar_xz(b"x");
        assert!(extract_from_tar_xz(&archive, "nope").is_err());
    }

    #[test]
    fn extract_zip_finds_binary() {
        let mut buf = Vec::new();
        {
            let mut w = zip::ZipWriter::new(std::io::Cursor::new(&mut buf));
            let opts: zip::write::FileOptions<'_, ()> = zip::write::FileOptions::default()
                .compression_method(zip::CompressionMethod::Deflated);
            w.start_file("fresh-editor-x/fresh.exe", opts).unwrap();
            w.write_all(b"MZ fake exe").unwrap();
            w.finish().unwrap();
        }
        let got = extract_from_zip(&buf, "fresh.exe").unwrap();
        assert_eq!(got, b"MZ fake exe");
    }

    /// End-to-end (download -> verify -> extract) against a local mock server.
    /// Exercises the real HTTP + checksum + extraction path without touching
    /// the running executable. `http` feature only (needs the ureq client).
    #[cfg(feature = "http")]
    #[test]
    fn fetch_verify_extract_end_to_end() {
        use std::time::Duration;

        let bin = b"#!/bin/sh\necho fresh 9.9.9\n";
        let archive = make_tar_xz(bin);
        let digest = self_update::sha256_hex(&archive);

        // Serve `/asset.tar.xz` and `/asset.tar.xz.sha256`.
        let server = tiny_http::Server::http("127.0.0.1:0").unwrap();
        let port = server.server_addr().to_ip().unwrap().port();
        let archive_for_thread = archive.clone();
        let sha_line = format!("{digest}  asset.tar.xz");
        let (stop_tx, stop_rx) = std::sync::mpsc::channel::<()>();
        let handle = std::thread::spawn(move || loop {
            if stop_rx.try_recv().is_ok() {
                break;
            }
            if let Ok(Some(req)) = server.recv_timeout(Duration::from_millis(100)) {
                if req.url().ends_with(".sha256") {
                    let _ = req.respond(tiny_http::Response::from_string(sha_line.clone()));
                } else {
                    let _ = req.respond(tiny_http::Response::from_data(archive_for_thread.clone()));
                }
            }
        });

        let url = format!("http://127.0.0.1:{port}/asset.tar.xz");
        let got = fetch_and_extract_binary(&url, "fresh").expect("update fetch failed");
        assert_eq!(got, bin);

        // A tampered payload must fail the checksum closed.
        assert!(
            verify(b"corrupted", &format!("{url}.sha256")).is_err(),
            "verification should reject a mismatch"
        );

        let _ = stop_tx.send(());
        let _ = handle.join();
    }
}
