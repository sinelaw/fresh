//! `fresh --cmd update`, end to end.
//!
//! Resolve provenance, fetch the release feed, then take one of four routes
//! decided entirely by [`UpdateKind`]:
//!
//! * **Delegated / Toolchain** — run the owning package manager's own command.
//! * **DownloadPackage** — fetch the release artifact and hand it to the local
//!   package tool. Never an in-place swap: the file is owned by dpkg/rpm, and
//!   replacing it behind the package database's back leaves the database
//!   describing a file that no longer exists.
//! * **SelfContained** — we own the bits, so download, verify and swap.
//! * **Manual** — nothing we can run here; say what to run instead.
//!
//! This used to live in `fresh-editor`. It is here so that the decision (which
//! command, which artifact, which privilege) sits next to the provenance that
//! justifies it, and so it can be tested without building an editor.
//!
//! # Privilege
//!
//! A privileged install runs only when the bytes came from a trusted endpoint
//! ([`Endpoints::trusted`]). With an override in effect we still download and
//! verify — that is what makes the override useful for testing — but we print
//! the command rather than running it under `sudo`, because a checksum fetched
//! from the same origin as the payload proves nothing about who produced it.

#![allow(clippy::let_underscore_must_use)]

use crate::endpoint::Endpoints;
use crate::feed::Release;
use crate::net::{self, Transport};
use crate::provenance::Provenance;
use crate::registry::UpdateKind;
use crate::{self_update, Channel};
use std::path::{Path, PathBuf};

/// Options for one run of the engine.
#[derive(Debug, Clone)]
pub struct UpdateOptions {
    /// Only report status; make no changes.
    pub check_only: bool,
    /// Run the update without an interactive confirmation.
    pub yes: bool,
    /// Permit "updating" to the same or an older version.
    pub allow_downgrade: bool,
    /// Run the install path even when already on the latest version.
    ///
    /// Distinct from `allow_downgrade`, which is about *which* versions are
    /// acceptable; this is about running the install at all. It is how the
    /// packaging containers exercise download → verify → install without
    /// waiting for a new release.
    pub force: bool,
    /// How far to go: install, fetch-and-stop, or print-and-stop.
    pub execution: Execution,
    /// Where releases come from, and whether that is the pinned production
    /// location.
    pub endpoints: Endpoints,
}

impl Default for UpdateOptions {
    fn default() -> Self {
        UpdateOptions {
            check_only: false,
            yes: false,
            allow_downgrade: false,
            force: false,
            execution: Execution::Install,
            // An out-of-policy override is refused outright in a release build,
            // so falling back to the pinned defaults here would silently ignore
            // what the user asked for. Better to start from production and let
            // the caller surface the error.
            endpoints: Endpoints::from_env().unwrap_or_else(|e| {
                tracing::warn!(error = %e, "ignoring an unusable update endpoint override");
                Endpoints::production()
            }),
        }
    }
}

/// How much of the update to actually perform.
///
/// Three rungs rather than two, because for a release package the work splits
/// at a natural seam: fetching it needs the network, installing it needs root.
/// A user can reasonably want us to do the first and keep the second.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Execution {
    /// Fetch, verify, install.
    #[default]
    Install,
    /// Fetch and verify, then stop and print the install command against the
    /// file on disk. Only differs from [`Execution::PrintOnly`] where there is
    /// something for *us* to fetch.
    DownloadOnly,
    /// Touch nothing: no request for an artifact, nothing written to disk.
    /// Print what would happen.
    ///
    /// The release feed is still read — that is how the version being offered
    /// is known at all, and it is the same request the background check makes
    /// once a day.
    PrintOnly,
}

impl Execution {
    /// Whether the artifact may be fetched.
    const fn may_download(self) -> bool {
        matches!(self, Execution::Install | Execution::DownloadOnly)
    }

    /// Whether the install command may be run.
    const fn may_install(self) -> bool {
        matches!(self, Execution::Install)
    }
}

/// The outcome of a successful [`run`], for callers mapping it to an exit code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateStatus {
    /// Installed, or `--check` reported status. Nothing more to do.
    Done,
    /// Not applied, and finishing it needs a step we will not take: the package
    /// is downloaded and verified but installing it needs root, the owning
    /// manager's command was printed rather than run, or there is no mechanism
    /// at all.
    ///
    /// [`run`] has already said what to do, so the caller exits
    /// [`crate::EXIT_ACTION_REQUIRED`] and prints nothing further. Reporting
    /// this as success would have the editor's indicator claim an update that
    /// never landed.
    ActionRequired,
}

/// Run the update.
pub fn run(current_version: &str, opts: &UpdateOptions) -> Result<UpdateStatus, String> {
    let prov = crate::resolve();
    println!(
        "Installed via: {} (confidence: {:?})",
        prov.channel.label(),
        prov.confidence
    );

    let transport = Transport::new(&opts.endpoints);
    let body = transport.get_text(&opts.endpoints.releases_url, net::FEED_MAX_BYTES)?;
    let release = Release::parse(&body)?;
    let latest = release.version().to_string();

    println!("Current version: {current_version}");
    println!("Latest version:  {latest}");

    let newer = crate::version::is_newer(current_version, &latest);
    if !newer && !opts.allow_downgrade && !opts.force {
        println!("You are already on the latest version.");
        return Ok(UpdateStatus::Done);
    }
    if !newer && opts.force {
        println!("Already on {latest}; --force given, reinstalling it anyway.");
    }

    let plan = crate::plan(&prov);

    match plan.kind {
        UpdateKind::SelfContained => {
            if opts.check_only {
                println!(
                    "An update is available. Run `fresh --cmd update` to install it in place."
                );
                return Ok(UpdateStatus::Done);
            }
            if !opts.execution.may_install() || !opts.yes {
                // There is no command to hand over — the swap is ours to do —
                // so the honest answer is to say what would happen and stop,
                // rather than replacing the running binary for someone who
                // asked to see it first.
                println!("This copy updates in place: fresh would download the");
                println!("{latest} release archive, verify it, and replace");
                match std::env::current_exe() {
                    Ok(p) => println!("{}.", p.display()),
                    Err(_) => println!("the running binary."),
                }
                return Ok(UpdateStatus::ActionRequired);
            }
            self_contained(&prov, &latest, &transport, opts).map(|()| UpdateStatus::Done)
        }
        UpdateKind::DownloadPackage => {
            if opts.check_only {
                println!("An update is available. Run `fresh --cmd update` to fetch it.");
                return Ok(UpdateStatus::Done);
            }
            package(&prov, &release, &transport, opts)
        }
        UpdateKind::Delegated | UpdateKind::Toolchain => {
            let cmd = plan.command.clone().unwrap_or_default();
            if opts.check_only {
                println!("An update is available. Update with: {}", plan.human);
                return Ok(UpdateStatus::Done);
            }
            // Nothing here is ours to download — the package manager does its
            // own fetching — so "download only" and "show the command" are the
            // same thing for these channels.
            if !opts.execution.may_install() || !opts.yes {
                // Printing a command is not performing it, so this is
                // ActionRequired rather than Done.
                show_command(&crate::elevate::elevated(&cmd, plan.needs_privilege));
                return Ok(UpdateStatus::ActionRequired);
            }
            run_install(&cmd, plan.needs_privilege).map(|()| UpdateStatus::Done)
        }
        UpdateKind::Manual => {
            println!("A new version of Fresh is available: {current_version} → {latest}");
            println!();
            println!("{}", plan.human);
            if opts.check_only {
                // `--check` only reports availability; a manual install still
                // has an update, so this is informational rather than a
                // failure.
                Ok(UpdateStatus::Done)
            } else {
                Ok(UpdateStatus::ActionRequired)
            }
        }
    }
}

/// Fetch the release artifact for a channel whose package manager has no
/// repository to upgrade from, and hand it to the local package tool.
fn package(
    prov: &Provenance,
    release: &Release,
    transport: &Transport,
    opts: &UpdateOptions,
) -> Result<UpdateStatus, String> {
    let channel = prov.channel;
    // The architecture the installer *recorded* beats one re-derived from this
    // build's target triple: deriving assumes the package was built for the
    // triple we were compiled for, and that we spell the arch the way the
    // packaging tool does. The pipeline knew both for certain.
    let asset_spec = crate::registry::package_asset_with(
        channel,
        crate::TARGET_TRIPLE,
        prov.hints.pkg_arch.as_deref(),
    )
    .ok_or_else(|| format!("no release package is published for {}", channel.label()))?;

    let asset = release.find_package(asset_spec.extension, &asset_spec.arch)?;
    crate::endpoint::check(&asset.browser_download_url).map_err(|e| e.to_string())?;

    let needs_privilege = crate::plan(prov).needs_privilege;
    // Downloading from an unpinned endpoint is fine; installing what came back
    // as root is not. Degrade to staging-and-printing rather than refusing, so
    // a test run still exercises download and verification.
    let may_install =
        opts.execution.may_install() && opts.yes && (opts.endpoints.trusted || !needs_privilege);

    if !opts.execution.may_download() {
        // "Show the command" means exactly that: no request for the artifact,
        // nothing written to disk. The two commands are still nameable, because
        // the release feed — already fetched, and the same request the daily
        // background check makes — carries the asset's URL and filename.
        let cmd = crate::registry::install_command_with(
            channel,
            Path::new(&format!("./{}", asset.name)),
            opts.force,
        )
        .ok_or_else(|| format!("no install command for {}", channel.label()))?;
        println!();
        println!("Download it from:");
        println!();
        println!("    {}", asset.browser_download_url);
        println!();
        show_command(&crate::elevate::elevated(&cmd, needs_privilege));
        return Ok(UpdateStatus::ActionRequired);
    }

    let bytes = fetch_and_verify(
        transport,
        &asset.browser_download_url,
        opts.endpoints.trusted,
    )?;

    // A package the user installs by hand, later, needs a directory nothing
    // sweeps; one we are about to install ourselves should disappear after.
    let (dir, cleanup) = if may_install {
        let scratch = crate::staging::ephemeral()?;
        (scratch.path().to_path_buf(), Some(scratch))
    } else {
        (crate::staging::durable()?, None)
    };
    let path = dir.join(&asset.name);
    std::fs::write(&path, &bytes).map_err(|e| format!("write {}: {e}", path.display()))?;
    println!("Downloaded to {}", path.display());

    let cmd = crate::registry::install_command_with(channel, &path, opts.force)
        .ok_or_else(|| format!("no install command for {}", channel.label()))?;

    if !may_install {
        if opts.execution.may_install() && opts.yes {
            // Wanted to install, was not allowed to.
            println!();
            println!(
                "The release endpoint is overridden, so this package was not \
                 installed automatically."
            );
        }
        println!();
        show_command(&crate::elevate::elevated(&cmd, needs_privilege));
        return Ok(UpdateStatus::ActionRequired);
    }

    println!();
    let result = run_install(&cmd, needs_privilege).map(|()| UpdateStatus::Done);
    drop(cleanup);
    result
}

/// Verified in-place update for a self-contained install.
fn self_contained(
    prov: &Provenance,
    latest: &str,
    transport: &Transport,
    opts: &UpdateOptions,
) -> Result<(), String> {
    if prov.channel == Channel::Appimage {
        return appimage(prov, latest, transport, opts);
    }

    // Computed from the triple this binary was compiled for — a compile-time
    // fact, not a guess. A name recorded in the receipt describes the version
    // already installed, so it is not consulted.
    let target = crate::TARGET_TRIPLE;
    let ext = if cfg!(windows) { "zip" } else { "tar.xz" };
    let asset = format!("fresh-editor-{target}.{ext}");
    let url = opts.endpoints.asset_url(latest, &asset);

    let bin_name = if cfg!(windows) { "fresh.exe" } else { "fresh" };
    let archive = fetch_and_verify(transport, &url, opts.endpoints.trusted)?;
    let binary = if url.ends_with(".zip") {
        crate::archive::from_zip(&archive, bin_name)?
    } else {
        crate::archive::from_tar_xz(&archive, bin_name)?
    };

    let exe = std::env::current_exe().map_err(|e| format!("cannot find current exe: {e}"))?;
    println!("Installing to {} ...", exe.display());
    self_update::atomic_replace(&exe, &binary).map_err(|e| e.to_string())?;

    println!("Updated to {latest}. Restart fresh to use the new version.");
    Ok(())
}

/// AppImage self-update: download the new AppImage, verify, extract its
/// squashfs, and replace the install root created by `install.sh`.
fn appimage(
    prov: &Provenance,
    latest: &str,
    transport: &Transport,
    opts: &UpdateOptions,
) -> Result<(), String> {
    let arch = crate::TARGET_TRIPLE.split('-').next().unwrap_or("x86_64");
    let asset = format!("fresh-editor-{latest}-{arch}.AppImage");
    let url = opts.endpoints.asset_url(latest, &asset);

    let install_root = prov.hints.install_root.as_deref().ok_or_else(|| {
        "AppImage install has no recorded install_root; reinstall via install.sh".to_string()
    })?;

    let bytes = fetch_and_verify(transport, &url, opts.endpoints.trusted)?;

    // Everything happens inside one private directory next to the install root
    // — same filesystem, so the final rename is atomic, and private so nothing
    // can substitute the AppImage between `chmod +x` and running it.
    let root = PathBuf::from(install_root);
    let parent = root.parent().unwrap_or_else(|| Path::new("."));
    let work = crate::staging::ephemeral_in(parent)?;
    let staged = work.path().join("fresh.AppImage");
    std::fs::write(&staged, &bytes).map_err(|e| format!("write staged AppImage: {e}"))?;
    make_executable(&staged)?;

    let status = std::process::Command::new(&staged)
        .arg("--appimage-extract")
        .current_dir(work.path())
        .status()
        .map_err(|e| format!("failed to extract AppImage: {e}"))?;
    if !status.success() {
        return Err("AppImage extraction failed".to_string());
    }

    let new_root = work.path().join("squashfs-root");
    if !new_root.is_dir() {
        return Err("AppImage extraction produced no squashfs-root".to_string());
    }

    // Swap the install root: move the old aside, move the new in, and only
    // then discard the old one.
    let backup = parent.join(format!(".{}-old", file_name(&root)));
    let _ = std::fs::remove_dir_all(&backup);
    if root.exists() {
        std::fs::rename(&root, &backup).map_err(|e| format!("move old install aside: {e}"))?;
    }
    match std::fs::rename(&new_root, &root) {
        Ok(()) => {
            let _ = std::fs::remove_dir_all(&backup);
            println!("Updated to {latest}. Restart fresh to use the new version.");
            Ok(())
        }
        Err(e) => {
            let _ = std::fs::rename(&backup, &root);
            Err(format!("failed to install new AppImage payload: {e}"))
        }
    }
}

fn file_name(p: &Path) -> String {
    p.file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "fresh-editor".to_string())
}

/// Download `url` and refuse to return the bytes unless they check out.
///
/// Two checks at two origins, because one origin is not enough. The `.sha256`
/// sidecar catches corruption but nothing else: it is served from the same
/// place as the payload, so anyone who can substitute one can substitute the
/// other. The attestation lookup asks `api.github.com` whether these exact
/// bytes were published under this exact asset name, which an attacker holding
/// only the asset CDN cannot arrange. See [`crate::attestation`] for what that
/// does and does not prove.
///
/// `trusted` tracks [`Endpoints::trusted`]: a test server has no attestations
/// and never could, so an overridden endpoint skips the second check — and the
/// engine already refuses to elevate with bytes from one.
fn fetch_and_verify(transport: &Transport, url: &str, trusted: bool) -> Result<Vec<u8>, String> {
    println!("Downloading {url} ...");
    let scratch = crate::staging::ephemeral()?;
    let tmp = scratch.path().join("payload");
    transport.download(url, &tmp, net::ASSET_MAX_BYTES)?;
    let bytes = std::fs::read(&tmp).map_err(|e| format!("read download: {e}"))?;

    println!("Verifying checksum ...");
    let sha_url = format!("{url}.sha256");
    let expected = transport
        .get_text(&sha_url, net::SIDECAR_MAX_BYTES)
        .map_err(|e| format!("could not fetch checksum ({sha_url}): {e}"))?;
    self_update::verify_sha256(&bytes, expected.trim()).map_err(|e| e.to_string())?;

    if trusted {
        println!("Verifying release attestation ...");
        // The digest is computed from the bytes on disk, never read from the
        // network — otherwise the second origin would be checking the first
        // origin's claim about itself.
        let digest = self_update::sha256_hex(&bytes);
        let asset = url.rsplit('/').next().unwrap_or(url);
        crate::attestation::verify(transport, crate::endpoint::REPO, asset, &digest)
            .map_err(|e| e.to_string())?;
    } else {
        println!("Release endpoint overridden — skipping the attestation check.");
    }

    Ok(bytes)
}

/// Print a command for the user to run, rather than running it.
fn show_command(cmd: &[String]) {
    println!("Install it with:");
    println!();
    println!("    {}", cmd.join(" "));
}

/// Run the install command, elevating it when it needs root and we are not it.
fn run_install(cmd: &[String], needs_privilege: bool) -> Result<(), String> {
    let argv = crate::elevate::elevated(cmd, needs_privilege);
    println!("Running: {}", argv.join(" "));
    let status = std::process::Command::new(&argv[0])
        .args(&argv[1..])
        .status()
        .map_err(|e| format!("failed to run `{}`: {e}", argv[0]))?;
    if status.success() {
        println!("Update complete.");
        Ok(())
    } else {
        Err(format!("`{}` exited with {status}", argv.join(" ")))
    }
}

#[cfg(unix)]
fn make_executable(path: &Path) -> Result<(), String> {
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755))
        .map_err(|e| e.to_string())
}

#[cfg(not(unix))]
fn make_executable(_path: &Path) -> Result<(), String> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::confidence::Confidence;

    /// An untrusted endpoint must never reach a privileged install, and must
    /// still be usable for everything up to that point — that is the whole
    /// reason the override exists.
    #[test]
    fn an_overridden_endpoint_cannot_reach_a_privileged_install() {
        let untrusted = Endpoints {
            releases_url: "http://127.0.0.1:9/release.json".to_string(),
            download_base: "http://127.0.0.1:9".to_string(),
            trusted: false,
        };
        for channel in [Channel::Apt, Channel::Dnf, Channel::Zypper] {
            let prov = Provenance::for_channel(channel, Confidence::Authoritative);
            let needs_privilege = crate::plan(&prov).needs_privilege;
            assert!(needs_privilege, "{channel} should need root");
            assert!(
                !(untrusted.trusted || !needs_privilege),
                "{channel} would install unpinned bytes as root"
            );
        }
        // A channel that needs no privilege is unaffected: there is no
        // escalation to withhold.
        let prov = Provenance::for_channel(Channel::Tarball, Confidence::Authoritative);
        assert!(!crate::plan(&prov).needs_privilege);
    }

    #[test]
    fn the_production_endpoint_permits_installing() {
        let ep = Endpoints::production();
        assert!(ep.trusted);
        let prov = Provenance::for_channel(Channel::Apt, Confidence::Authoritative);
        let needs_privilege = crate::plan(&prov).needs_privilege;
        assert!(ep.trusted || !needs_privilege);
    }

    /// The point of "Show the command": a user who asks to see it first has
    /// asked for nothing to happen yet. It previously downloaded and staged
    /// the package anyway, which is a surprising amount of work — and a file
    /// on disk — for a request to read something.
    #[test]
    fn only_show_the_command_declines_to_fetch() {
        assert!(!Execution::PrintOnly.may_download());
        assert!(!Execution::PrintOnly.may_install());
        // The middle rung does the network half and stops at the root half.
        assert!(Execution::DownloadOnly.may_download());
        assert!(!Execution::DownloadOnly.may_install());
        assert!(Execution::Install.may_download());
        assert!(Execution::Install.may_install());
    }

    /// The rungs are ordered, and each one permits everything the one below it
    /// does. A mode that installed without being allowed to download, or that
    /// downloaded while claiming to touch nothing, would be a contradiction.
    #[test]
    fn each_rung_permits_at_least_what_the_one_below_does() {
        for mode in [
            Execution::PrintOnly,
            Execution::DownloadOnly,
            Execution::Install,
        ] {
            if mode.may_install() {
                assert!(mode.may_download(), "{mode:?} installs without fetching");
            }
        }
    }

    /// The middle rung is offered exactly where the work splits at a seam the
    /// user could stand on: we fetch, a package manager installs. Anywhere
    /// else it would be a row that does the same thing as its neighbour.
    #[test]
    fn download_only_is_offered_only_where_there_is_something_to_download() {
        use crate::offer::{offer_for, UpdateChoice, UpdateOffer};
        for channel in [Channel::Apt, Channel::Dnf, Channel::Zypper] {
            let plan = crate::plan(&Provenance::for_channel(channel, Confidence::Authoritative));
            assert_eq!(offer_for(&plan), UpdateOffer::DownloadPackage);
            assert!(
                offer_for(&plan)
                    .choices()
                    .contains(&UpdateChoice::DownloadOnly),
                "{channel} fetches its own package but offers no download-only rung"
            );
        }
        // The package manager does its own downloading here, so the rung would
        // be indistinguishable from "show the command".
        for channel in [
            Channel::Homebrew,
            Channel::Npm,
            Channel::Winget,
            Channel::Nix,
        ] {
            let plan = crate::plan(&Provenance::for_channel(channel, Confidence::Authoritative));
            assert!(
                !offer_for(&plan)
                    .choices()
                    .contains(&UpdateChoice::DownloadOnly),
                "{channel} offers a download-only rung with nothing to download"
            );
        }
    }

    #[test]
    fn default_options_do_not_act_without_being_asked() {
        let opts = UpdateOptions::default();
        assert!(!opts.yes, "a bare `fresh --cmd update` must not install");
        assert_eq!(opts.execution, Execution::Install);
        assert!(!opts.force);
        assert!(!opts.allow_downgrade);
    }
}
