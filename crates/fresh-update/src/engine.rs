//! `fresh --cmd update`, end to end.
//!
//! Resolve provenance, resolve the release (through the `releases/latest`
//! redirect, not the API — see [`crate::fetch`]), then take one of four routes
//! decided entirely by [`UpdateKind`]:
//!
//! * **Delegated / Toolchain** — print the owning package manager's own
//!   command, for the user to run.
//! * **DownloadPackage** — fetch the release artifact, verify it, and print the
//!   command that installs it. Never an in-place swap: the file is owned by
//!   dpkg/rpm, and replacing it behind the package database's back leaves the
//!   database describing a file that no longer exists.
//! * **SelfContained** — we own the bits, so download, verify and swap.
//! * **Manual** — nothing to name here; say where to go instead.
//!
//! This used to live in `fresh-editor`. It is here so that the decision (which
//! command, which artifact, which privilege) sits next to the provenance that
//! justifies it, and so it can be tested without building an editor.
//!
//! # One rule: we only write files we own
//!
//! `fresh` replaces its own binary when — and only when — it owns that binary.
//! It never runs anyone else's package manager. Not `brew upgrade`, not
//! `winget upgrade`, not `cargo install`, and above all not `sudo dpkg -i`.
//! For every channel but [`UpdateKind::SelfContained`] the outcome is the exact
//! command, printed, and [`UpdateStatus::ActionRequired`].
//!
//! This used to be graded — user-scoped managers were run, root ones were run
//! after a confirmation, `sudo` was prepended in the update terminal. The
//! grading is what made it hard to reason about: whether an update *executed*
//! depended on the channel, on `--yes`, on whether the endpoint was trusted and
//! on whether the tool needed root, and the most dangerous path in the whole
//! updater — spawning a package installer as root — sat at the end of that
//! chain. Deleting the execution deletes the class of bug rather than an
//! instance of it, and a rule with no exceptions is one nobody has to remember.
//!
//! What it does **not** delete is verification. Channels whose packages we host
//! but nobody serves from a repository ([`UpdateKind::DownloadPackage`]) are
//! still fetched, checksum-verified and attestation-checked before the command
//! is printed, because a user who downloads a `.deb` by hand verifies nothing.
//! Stopping before `dpkg` costs a keystroke; stopping before the download would
//! cost the only verification those users get.

#![allow(clippy::let_underscore_must_use)]

/// The flag that turns the attestation check off, named once so the hint in a
/// failure and the CLI that parses it cannot drift apart.
pub const SKIP_ATTESTATION_FLAG: &str = "--skip-attestation";

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
    /// Consider releases GitHub marks as pre-releases.
    ///
    /// Off by default, and the refusal is enforced in [`crate::feed::select`]
    /// rather than left to the endpoint: see the note there.
    pub allow_prerelease: bool,
    /// Do not check the release attestation before installing.
    ///
    /// Lowers verification to the checksum sidecar alone — the same bar
    /// `install.sh` sets, and a weaker one than this engine's default: the
    /// sidecar is served from the same origin as the artifact, so it catches
    /// corruption but not a release that was tampered with at the source. It
    /// exists because the attestation lookup is the one request an update makes
    /// to `api.github.com`, and a rate limit there is otherwise an hour's wait
    /// with no way through.
    ///
    /// Deliberately a flag and not an environment variable: this is a decision
    /// to be taken per run, not one to leave set in a shell profile where it
    /// silently applies to every future update.
    pub skip_attestation: bool,
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
            allow_prerelease: false,
            skip_attestation: false,
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
/// The split that matters is fetching versus not fetching. Installing is no
/// longer a rung: the only thing we ever install is our own binary, and that is
/// decided by provenance rather than by this.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Execution {
    /// Do the work: swap in place for a self-contained install, fetch and
    /// verify for a release package, print the command for everything else.
    #[default]
    Install,
    /// Fetch and verify, then stop and print the install command against the
    /// file on disk. For [`UpdateKind::DownloadPackage`] this is now what
    /// [`Execution::Install`] does too; it stays as an explicit way to ask for
    /// it, and as the flag the editor's popup dispatches.
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
    /// Not applied, and finishing it is the user's step to take: the package is
    /// downloaded and verified but installing it belongs to dpkg/rpm, the
    /// owning manager's command was printed, or there is no mechanism at all.
    ///
    /// Every channel except [`UpdateKind::SelfContained`] ends here, by design
    /// — see the module note on only writing files we own.
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
    // Resolves through the `releases/latest` redirect on the pinned endpoints —
    // no API budget spent, so a `--check` cannot be refused by a rate limit
    // somebody else used up. See `crate::fetch`.
    let fetched = crate::fetch::latest(&transport, &opts.endpoints, opts.allow_prerelease)?;
    let latest = fetched.release.version().to_string();

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
            package(&prov, &fetched, &transport, opts)
        }
        UpdateKind::Delegated | UpdateKind::Toolchain => {
            let cmd = plan.command.clone().unwrap_or_default();
            if opts.check_only {
                println!("An update is available. Update with: {}", plan.human);
                return Ok(UpdateStatus::Done);
            }
            // Always printed, never run — see the note on `run` about not
            // driving other people's package managers. Nothing here is ours to
            // download either (the manager fetches its own), so every rung of
            // `Execution` lands in the same place for these channels.
            show_command(&crate::elevate::elevated(&cmd, plan.needs_privilege));
            Ok(UpdateStatus::ActionRequired)
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
///
/// The artifact is named the way `scripts/install.sh` names it — from the
/// version, with no request for the release's asset list — so a `.deb`/`.rpm`
/// update spends no API budget either. That name is a prediction, so it is
/// checked by using it: if the release does not publish it, the feed is asked
/// for the real one, which costs the one request the construction was there to
/// avoid and only when it was actually wrong.
fn package(
    prov: &Provenance,
    fetched: &crate::fetch::Fetched,
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

    let mut asset = match constructed_asset(fetched, &asset_spec, opts) {
        Some(asset) => asset,
        // Either the feed is already in hand, or this extension has no naming
        // we encode. Both mean: read the name off the release.
        None => feed_asset(&fetched.release, &asset_spec, opts)?,
    };

    // Display only: whether the printed command needs a `sudo` in front of it.
    let needs_privilege = crate::plan(prov).needs_privilege;

    if !opts.execution.may_download() {
        // "Show the command" means exactly that: no request for the artifact,
        // nothing written to disk.
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

    let bytes = match fetch_and_verify(
        transport,
        &asset.browser_download_url,
        opts.endpoints.trusted,
        opts.skip_attestation,
    ) {
        Ok(bytes) => bytes,
        // The name we built is not what this release published — a packaging
        // change, or a second revision of the same version. The feed knows the
        // real one; ask it, and only now.
        Err(DownloadError::Missing(_))
            if fetched.source == crate::fetch::Source::ReleaseRedirect =>
        {
            tracing::warn!(
                asset = %asset.name,
                "constructed package name is not published; asking the release feed"
            );
            let release =
                crate::fetch::from_feed(transport, &opts.endpoints, opts.allow_prerelease)?;
            asset = feed_asset(&release, &asset_spec, opts)?;
            fetch_and_verify(
                transport,
                &asset.browser_download_url,
                opts.endpoints.trusted,
                opts.skip_attestation,
            )?
        }
        Err(e) => return Err(e.into()),
    };

    // The user installs this by hand, later, so it goes somewhere nothing
    // sweeps out from under them. (This used to be an ephemeral directory when
    // we were about to run the installer ourselves; we no longer do.)
    let dir = crate::staging::durable()?;
    let path = dir.join(&asset.name);
    std::fs::write(&path, &bytes).map_err(|e| format!("write {}: {e}", path.display()))?;
    println!("Downloaded to {}", path.display());

    let cmd = crate::registry::install_command_with(channel, &path, opts.force)
        .ok_or_else(|| format!("no install command for {}", channel.label()))?;

    // Verified, and that is where we stop: installing it belongs to the tool
    // that owns these files, run by the person who owns the machine.
    println!();
    show_command(&crate::elevate::elevated(&cmd, needs_privilege));
    Ok(UpdateStatus::ActionRequired)
}

/// The package this release should publish, named from its version and hosted
/// under the pinned download base.
///
/// `None` when the release came from the feed (whose names are facts, so there
/// is nothing to predict), when the endpoint is overridden (a mirror serves its
/// own layout, and only its feed knows it), or when the extension has no naming
/// we encode.
fn constructed_asset(
    fetched: &crate::fetch::Fetched,
    spec: &crate::registry::PackageAsset,
    opts: &UpdateOptions,
) -> Option<crate::feed::Asset> {
    if fetched.source.lists_assets() || !opts.endpoints.trusted {
        return None;
    }
    let version = fetched.release.version();
    let name = crate::feed::package_file_name(version, spec.extension, &spec.arch)?;
    let browser_download_url = opts.endpoints.asset_url(version, &name);
    Some(crate::feed::Asset {
        name,
        browser_download_url,
    })
}

/// The package this release *does* publish, read off the feed.
fn feed_asset(
    release: &Release,
    spec: &crate::registry::PackageAsset,
    opts: &UpdateOptions,
) -> Result<crate::feed::Asset, String> {
    let asset = release.find_package(spec.extension, &spec.arch)?;
    // This URL comes out of the feed rather than being built from a pinned
    // base, so on the production endpoint it gets the full host check — a feed
    // we did not author must not be able to point the download anywhere.
    //
    // An overridden endpoint is the one case where that check is wrong rather
    // than strict: a mirror's feed necessarily names assets *on the mirror*, so
    // enforcing the GitHub allowlist there rejects every asset and makes
    // `--releases-url` unusable for the air-gapped case it exists for. The
    // endpoint is already marked untrusted, and nothing is installed from it.
    if opts.endpoints.trusted {
        crate::endpoint::check(&asset.browser_download_url).map_err(|e| e.to_string())?;
    }
    Ok(asset.clone())
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
    let asset = format!("fresh-editor-{target}.{}", archive_ext(target));
    let url = opts.endpoints.asset_url(latest, &asset);

    let bin_name = if cfg!(windows) { "fresh.exe" } else { "fresh" };
    let archive = fetch_and_verify(
        transport,
        &url,
        opts.endpoints.trusted,
        opts.skip_attestation,
    )?;
    let binary = if asset.ends_with(".zip") {
        crate::archive::from_zip(&archive, bin_name)?
    } else if asset.ends_with(".tar.gz") {
        crate::archive::from_tar_gz(&archive, bin_name)?
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

    let bytes = fetch_and_verify(
        transport,
        &url,
        opts.endpoints.trusted,
        opts.skip_attestation,
    )?;

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

/// The archive extension the release publishes for `target`.
///
/// A pure function of the compile-time target triple, so it stays a fact
/// rather than a lookup: Windows ships `.zip`; the musl (universal) archive
/// ships `.tar.gz` because `install.sh` unpacks it with the system `tar` and
/// `.tar.xz` needs the xz binary that minimal images often lack; everything
/// else ships `.tar.xz`, matching what the dist pipeline produces.
///
/// Keep in step with `.github/workflows/musl-builds.yml` and the
/// `archive_ext` matrix in `release.yml` — a mismatch here is a 404 at update
/// time, which is why the test below pins every target we publish.
pub fn archive_ext(target: &str) -> &'static str {
    if target.contains("-windows-") {
        "zip"
    } else if target.contains("-musl") {
        "tar.gz"
    } else {
        "tar.xz"
    }
}

/// A download that did not produce verified bytes.
///
/// "That asset does not exist" is kept apart from every other failure because
/// one caller can act on it: [`package`] builds a filename from a convention,
/// and a 404 is the release telling it the convention did not hold. Everything
/// else — a checksum mismatch, a missing attestation, a broken connection — is
/// a failure to report, and collapsing the two would turn a tampered download
/// into a retry against the feed.
enum DownloadError {
    /// The asset is not published under that name.
    Missing(String),
    /// Anything else, already phrased for the user.
    Failed(String),
}

impl From<crate::net::FetchError> for DownloadError {
    fn from(e: crate::net::FetchError) -> Self {
        match e {
            crate::net::FetchError::Status { status: 404, .. } => {
                DownloadError::Missing(e.to_string())
            }
            other => DownloadError::Failed(other.to_string()),
        }
    }
}

impl From<DownloadError> for String {
    fn from(e: DownloadError) -> String {
        match e {
            DownloadError::Missing(detail) | DownloadError::Failed(detail) => detail,
        }
    }
}

/// Download `url`, returning the bytes only if they check out at both origins.
///
/// The sidecar shares an origin with the payload, so it catches corruption and
/// nothing else; the attestation lookup asks a second, separately pinned origin
/// whether these bytes were published under this name. See
/// [`crate::attestation`] for what that does and does not prove.
///
/// `trusted` tracks [`Endpoints::trusted`] — an overridden endpoint has no
/// attestations to find, so the second check is skipped.
fn fetch_and_verify(
    transport: &Transport,
    url: &str,
    trusted: bool,
    skip_attestation: bool,
) -> Result<Vec<u8>, DownloadError> {
    println!("Downloading {url} ...");
    let scratch = crate::staging::ephemeral().map_err(DownloadError::Failed)?;
    let tmp = scratch.path().join("payload");
    transport.download(url, &tmp, net::ASSET_MAX_BYTES)?;
    let bytes =
        std::fs::read(&tmp).map_err(|e| DownloadError::Failed(format!("read download: {e}")))?;

    println!("Verifying checksum ...");
    let sha_url = format!("{url}.sha256");
    let expected = transport
        .get_text(&sha_url, net::SIDECAR_MAX_BYTES)
        .map_err(|e| DownloadError::Failed(format!("could not fetch checksum ({sha_url}): {e}")))?;
    self_update::verify_sha256(&bytes, expected.trim())
        .map_err(|e| DownloadError::Failed(e.to_string()))?;

    if skip_attestation {
        // Said plainly, every time, because the user gave up the check that
        // separates "these bytes are intact" from "GitHub published them".
        println!("Skipping the release attestation check ({SKIP_ATTESTATION_FLAG}).");
        println!("These bytes are verified only against a checksum served from the same");
        println!("origin as the artifact, which catches corruption but not tampering.");
    } else if trusted {
        println!("Verifying release attestation ...");
        // The digest is computed from the bytes on disk, never read from the
        // network — otherwise the second origin would be checking the first
        // origin's claim about itself.
        let digest = self_update::sha256_hex(&bytes);
        let asset = url.rsplit('/').next().unwrap_or(url);
        crate::attestation::verify(transport, crate::endpoint::REPO, asset, &digest)
            .map_err(|e| DownloadError::Failed(attestation_failure(&e)))?;
    } else {
        println!("Release endpoint overridden — skipping the attestation check.");
    }

    Ok(bytes)
}

/// What to tell the user when the attestation could not be confirmed.
///
/// The bypass is named for a rate limit and nothing else. Every other failure
/// here is either "GitHub does not attest to these bytes" or "something got in
/// the way of asking" — and pointing at a flag that skips the check would be
/// suggesting the bypass in precisely the cases the check is for. A rate limit
/// is different in kind: GitHub answered, over TLS, that it will not answer
/// again for a while.
fn attestation_failure(e: &crate::attestation::AttestationError) -> String {
    let bypass = matches!(e, crate::attestation::AttestationError::RateLimited(_));
    if bypass {
        format!(
            "{e}\n\nIf a checksum-only install is acceptable — the verification \
             `install.sh` does, and no more — re-run with {SKIP_ATTESTATION_FLAG}."
        )
    } else {
        e.to_string()
    }
}

/// Print a command for the user to run, rather than running it.
fn show_command(cmd: &[String]) {
    println!("Install it with:");
    println!();
    println!("    {}", cmd.join(" "));
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

    /// Every target the release pipeline publishes, with the extension it
    /// actually produces. A mismatch is a 404 at update time on a channel
    /// nobody exercises until a user tries to update, so it is pinned here
    /// rather than discovered in the wild.
    #[test]
    fn archive_ext_matches_what_the_pipelines_publish() {
        let cases = [
            // musl-builds.yml: gzip, so `install.sh` can unpack it with a
            // stock tar on a machine without xz-utils.
            ("x86_64-unknown-linux-musl", "tar.gz"),
            ("aarch64-unknown-linux-musl", "tar.gz"),
            // release.yml `archive_ext: tar.xz`.
            ("x86_64-unknown-linux-gnu", "tar.xz"),
            ("aarch64-unknown-linux-gnu", "tar.xz"),
            ("x86_64-apple-darwin", "tar.xz"),
            ("aarch64-apple-darwin", "tar.xz"),
            // release.yml `archive_ext: zip`.
            ("x86_64-pc-windows-msvc", "zip"),
            ("aarch64-pc-windows-msvc", "zip"),
        ];
        for (target, expected) in cases {
            assert_eq!(archive_ext(target), expected, "archive_ext({target})");
        }
    }

    fn redirect_fetched(version: &str) -> crate::fetch::Fetched {
        crate::fetch::Fetched {
            release: Release::from_tag(format!("v{version}")),
            source: crate::fetch::Source::ReleaseRedirect,
        }
    }

    /// The point of the construction: a `.deb`/`.rpm` update names its
    /// artifact from the version, so it needs no request to `api.github.com`
    /// and cannot be refused by a rate limit somebody else used up. The URL
    /// must land on the pinned host, exactly where `install.sh` fetches it.
    #[test]
    fn a_package_is_named_from_the_version_without_the_feed() {
        let opts = UpdateOptions {
            endpoints: Endpoints::production(),
            ..UpdateOptions::default()
        };
        let spec = crate::registry::PackageAsset {
            extension: ".deb",
            arch: "amd64".to_string(),
        };
        let asset = constructed_asset(&redirect_fetched("0.4.10"), &spec, &opts)
            .expect("a .deb name is one we encode");
        assert_eq!(asset.name, "fresh-editor_0.4.10-1_amd64.deb");
        assert_eq!(
            asset.browser_download_url,
            format!(
                "https://github.com/{}/releases/download/v0.4.10/{}",
                crate::endpoint::REPO,
                asset.name
            )
        );
        crate::endpoint::check(&asset.browser_download_url)
            .expect("a constructed asset URL stays on a pinned host");
    }

    /// Two cases where predicting the name would be wrong rather than merely
    /// unnecessary: the feed is already in hand and states the real names, or
    /// the endpoint is a mirror whose layout we do not get to assume.
    #[test]
    fn a_name_is_never_predicted_when_the_release_can_be_asked() {
        let spec = crate::registry::PackageAsset {
            extension: ".deb",
            arch: "amd64".to_string(),
        };

        let from_feed = crate::fetch::Fetched {
            release: Release::from_tag("v0.4.10"),
            source: crate::fetch::Source::Api,
        };
        let opts = UpdateOptions {
            endpoints: Endpoints::production(),
            ..UpdateOptions::default()
        };
        assert!(constructed_asset(&from_feed, &spec, &opts).is_none());

        let opts = UpdateOptions {
            endpoints: Endpoints {
                releases_url: "http://127.0.0.1:9/release.json".to_string(),
                download_base: "http://127.0.0.1:9".to_string(),
                redirect_url: None,
                trusted: false,
            },
            ..UpdateOptions::default()
        };
        assert!(constructed_asset(&redirect_fetched("0.4.10"), &spec, &opts).is_none());
    }

    /// The bypass is offered for a rate limit and for nothing else: naming it
    /// when GitHub says these bytes are *not* attested would be suggesting the
    /// way around the check in the one case the check exists for.
    #[test]
    fn only_a_rate_limit_is_told_how_to_go_around_the_attestation() {
        use crate::attestation::AttestationError;

        let limited = AttestationError::RateLimited("rate limited".to_string());
        let message = attestation_failure(&limited);
        assert!(
            message.contains(SKIP_ATTESTATION_FLAG),
            "a rate limit must name the way through: {message}"
        );

        for e in [
            AttestationError::NotAttested {
                asset: "fresh-editor.tar.gz".to_string(),
                digest: "a".repeat(64),
            },
            AttestationError::NameMismatch {
                asset: "fresh-editor.tar.gz".to_string(),
                digest: "a".repeat(64),
            },
            AttestationError::Malformed("not json".to_string()),
            AttestationError::Fetch("connection reset".to_string()),
        ] {
            let message = attestation_failure(&e);
            assert!(
                !message.contains(SKIP_ATTESTATION_FLAG),
                "{e:?} must not advertise the bypass: {message}"
            );
        }
    }

    /// A 404 is the release saying the constructed name was wrong, and only
    /// that failure may send the caller back to the feed. A checksum mismatch
    /// or a missing attestation must never be retried as if it were a naming
    /// problem.
    #[test]
    fn only_a_missing_asset_is_a_naming_problem() {
        let missing: DownloadError = crate::net::FetchError::Status {
            url: "https://github.com/x.deb".to_string(),
            status: 404,
        }
        .into();
        assert!(matches!(missing, DownloadError::Missing(_)));

        for status in [403, 500, 502] {
            let other: DownloadError = crate::net::FetchError::Status {
                url: "https://github.com/x.deb".to_string(),
                status,
            }
            .into();
            assert!(
                matches!(other, DownloadError::Failed(_)),
                "HTTP {status} must not read as a naming problem"
            );
        }
        let limited: DownloadError = crate::net::FetchError::RateLimited {
            url: "https://api.github.com/x".to_string(),
            wait: None,
            authenticated: false,
        }
        .into();
        assert!(matches!(limited, DownloadError::Failed(_)));
    }

    /// Windows wins over the musl check, so a hypothetical
    /// `*-windows-musl` cannot be handed a tarball extension.
    #[test]
    fn windows_takes_precedence_over_musl() {
        assert_eq!(archive_ext("x86_64-pc-windows-musl"), "zip");
    }

    /// An untrusted endpoint must never reach a privileged install, and must
    /// still be usable for everything up to that point — that is the whole
    /// reason the override exists.
    #[test]
    fn an_overridden_endpoint_cannot_reach_a_privileged_install() {
        let untrusted = Endpoints {
            releases_url: "http://127.0.0.1:9/release.json".to_string(),
            download_base: "http://127.0.0.1:9".to_string(),
            redirect_url: None,
            trusted: false,
        };
        for channel in [Channel::Apt, Channel::Dnf, Channel::Zypper] {
            let prov = Provenance::for_channel(channel, Confidence::Authoritative);
            let needs_privilege = crate::plan(&prov).needs_privilege;
            assert!(needs_privilege, "{channel} should need root");
            assert!(
                !untrusted.trusted && needs_privilege,
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
