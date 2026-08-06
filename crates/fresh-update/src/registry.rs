//! The channel → update-strategy registry: the single place that encodes
//! "how do we update channel X". See `docs/internal/packaging-self-update.md`
//! §6.

use crate::channel::Channel;
use crate::provenance::Provenance;
use std::path::Path;

/// The tool ref mise installs `fresh` under, matching the README's
/// `mise use github:sinelaw/fresh`.
const MISE_TOOL: &str = "github:sinelaw/fresh";

/// The broad category of update mechanism for a channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateKind {
    /// An external OS/package manager owns updates — run (or print) its
    /// command; never swap the binary ourselves.
    Delegated,
    /// A user-scoped toolchain manager (cargo/npm/mise) — delegate, no sudo.
    Toolchain,
    /// We own the bits — download the release asset, verify, swap in place.
    SelfContained,
    /// A package manager owns the installed files, but no repository serves
    /// them: fetch the release artifact (`.deb`/`.rpm`/`.flatpak`) from GitHub
    /// and hand it to the local package tool. Never an in-place binary swap —
    /// that would desync the package database.
    DownloadPackage,
    /// Unknown/source — point the user at instructions only.
    Manual,
}

impl UpdateKind {
    /// Default `managed` flag for this kind (an external manager owns it).
    pub const fn managed(self) -> bool {
        matches!(
            self,
            UpdateKind::Delegated | UpdateKind::Toolchain | UpdateKind::DownloadPackage
        )
    }

    /// Default `self_update` capability for this kind.
    pub const fn self_update(self) -> bool {
        matches!(self, UpdateKind::SelfContained)
    }
}

/// A concrete, ready-to-present update action for a resolved provenance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UpdatePlan {
    /// The mechanism category.
    pub kind: UpdateKind,
    /// The command to run, as an argv vector. `None` for `SelfContained`
    /// (handled by the self-update engine) and `Manual`.
    pub command: Option<Vec<String>>,
    /// Whether the command needs elevated privileges (we print rather than
    /// run these ourselves).
    pub needs_privilege: bool,
    /// A human-readable one-liner for the UI / notification.
    pub human: String,
}

/// Whether `pkg` is a plausible AUR package name. The name is interpolated into
/// a shell command, so a receipt carrying metacharacters is refused outright.
fn is_safe_pkg_name(pkg: &str) -> bool {
    !pkg.is_empty()
        && pkg.len() <= 64
        && pkg
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_' | b'.' | b'+'))
}

/// The argv that updates an AUR package: clone the AUR repo and build it.
///
/// One command, for every Arch user. `makepkg` ships in `base-devel`, which is
/// a prerequisite for having installed an AUR package at all, so this works
/// everywhere without depending on anything the channel does not guarantee.
///
/// Deliberately *not* helper-aware. Preferring `yay`/`paru` when present would
/// mean this provenance class had two different upgrade mechanisms depending on
/// what happened to be installed — the update would differ machine to machine
/// and could not be reasoned about from the receipt alone. An AUR receipt means
/// `pacman` + `makepkg`; that is what we use.
pub fn aur_command(pkg: &str) -> Vec<String> {
    if !is_safe_pkg_name(pkg) {
        // A corrupt receipt is an error, not a reason to try something else.
        return vec![
            "sh".to_string(),
            "-c".to_string(),
            "echo 'refusing to build: implausible AUR package name in install receipt' >&2; exit 1"
                .to_string(),
        ];
    }
    let script = format!(
        "set -eu; \
dir=$(mktemp -d); \
trap 'rm -rf \"$dir\"' EXIT; \
git clone --depth 1 https://aur.archlinux.org/{pkg}.git \"$dir/{pkg}\"; \
cd \"$dir/{pkg}\"; \
makepkg --syncdeps --install"
    );
    vec!["sh".to_string(), "-c".to_string(), script]
}

/// The update kind for a channel.
pub const fn kind_for(channel: Channel) -> UpdateKind {
    match channel {
        // We publish the .deb/.rpm/.flatpak as release artifacts only — no apt
        // repo, no dnf repo, no Flathub remote — so their package managers have
        // nothing to upgrade *from*. Fetch the artifact and install it locally.
        // openSUSE has no OBS project either: a zypper user installed the
        // release .rpm by hand, so the next version arrives the same way.
        Channel::Apt | Channel::Dnf | Channel::Zypper => UpdateKind::DownloadPackage,
        // Flatpak used to be here, and could not work. A Flatpak build of
        // fresh runs inside `org.freedesktop.Platform`, which contains no
        // `flatpak` binary — so the `flatpak install` we generated could not
        // execute in the process that generated it. The user was shown a
        // command, clicked it, and got "No such file or directory".
        //
        // The escape hatch, `flatpak-spawn --host`, needs
        // `--talk-name=org.freedesktop.Flatpak` in the manifest, which is a
        // total sandbox escape rather than a scoped permission and is not
        // something to ship for an editor's update button. The sanctioned
        // in-sandbox route is the `org.freedesktop.portal.Flatpak`
        // `UpdateMonitor` — but it pulls from the app's remote, and we
        // distribute a bundle with no usable remote, so it has nothing to pull
        // from either. Publishing to Flathub is what unlocks that, and until
        // then the honest answer is to say so and name the host command.
        Channel::Flatpak => UpdateKind::Manual,
        // Arch ships through the AUR only, so `pacman` continues via the AUR
        // helper — that is how an Arch user got this build in the first place.
        Channel::Homebrew
        | Channel::Aur
        | Channel::AurBin
        | Channel::Pacman
        | Channel::Winget
        | Channel::Nix
        | Channel::FreebsdPkg => UpdateKind::Delegated,
        Channel::Cargo | Channel::CargoBinstall | Channel::Npm | Channel::Mise => {
            UpdateKind::Toolchain
        }
        Channel::Appimage | Channel::Tarball | Channel::Prebuilt => UpdateKind::SelfContained,
        // Nothing in this repository ships through snap, scoop or chocolatey,
        // and we publish no artifact those tools could install, so there is no
        // same-channel continuation to offer. Nothing writes these receipts
        // either, so they are unreachable in practice; they stay in `Channel`
        // because the ids are receipt wire format.
        Channel::Snap
        | Channel::Scoop
        | Channel::Chocolatey
        | Channel::Source
        | Channel::Unknown => UpdateKind::Manual,
    }
}

/// What to tell a user whose channel has no command we can run for them.
///
/// The rule is that this never dead-ends at the releases page for a channel
/// that actually installed fresh. Someone who installed through a package
/// manager should carry on through that package manager; sending them to
/// download a tarball would leave two copies on the machine, one of them
/// unknown to the tool that owns `/usr/bin/fresh`. The releases page is only
/// right when nothing else is true — an unknown or hand-placed binary.
fn manual_instructions(channel: Channel) -> String {
    match channel {
        Channel::Source => "git pull && cargo install --path crates/fresh-editor".to_string(),
        // Runs on the host, not in the sandbox — see `kind_for`. The app id is
        // what `flatpak update` matches.
        Channel::Flatpak => "run `flatpak update io.github.sinelaw.fresh` on the host, \
             or update through GNOME Software / KDE Discover"
            .to_string(),
        // snapd already refreshes snaps about four times a day and cannot be
        // told not to, so the honest message is that this will happen by
        // itself — with the command for someone who does not want to wait.
        Channel::Snap => "snapd refreshes snaps automatically; to do it now run \
             `sudo snap refresh fresh`"
            .to_string(),
        // Nothing is published to these, so there is no same-channel
        // continuation to name — see `kind_for`.
        Channel::Scoop | Channel::Chocolatey => "no package is published for this tool; see \
             https://github.com/sinelaw/fresh/releases"
            .to_string(),
        _ => "see https://github.com/sinelaw/fresh/releases".to_string(),
    }
}

/// Whether the delegated command for a channel needs root/admin.
const fn needs_privilege(channel: Channel) -> bool {
    // AUR helpers and `makepkg -si` invoke sudo themselves for the pacman step,
    // so we must not wrap them in another elevation.
    matches!(
        channel,
        Channel::Apt | Channel::Dnf | Channel::Zypper | Channel::FreebsdPkg
    )
}

/// Build the concrete [`UpdatePlan`] for a resolved provenance, templating the
/// command with the receipt's hints (falling back to project defaults).
pub fn plan(prov: &Provenance) -> UpdatePlan {
    let channel = prov.channel;
    let kind = kind_for(channel);
    let h = &prov.hints;

    let pkg = prov
        .package_name
        .clone()
        .unwrap_or_else(|| "fresh-editor".to_string());
    // The tap ships `Formula/fresh.rb` (`class Fresh`), so the formula is
    // `fresh` — *not* the `fresh-editor` package name. Defaulting to the
    // package gave `brew upgrade fresh-editor`, which errors with
    // "No available formula with the name".
    let formula = h.formula.clone().unwrap_or_else(|| "fresh".to_string());
    let winget_id = h
        .winget_id
        .clone()
        .unwrap_or_else(|| "sinelaw.fresh-editor".to_string());
    let npm_pkg = h
        .npm_pkg
        .clone()
        .unwrap_or_else(|| "@fresh-editor/fresh-editor".to_string());
    let npm_latest = format!("{npm_pkg}@latest");
    let aur_pkg = h.aur_pkg.clone().unwrap_or_else(|| match channel {
        Channel::AurBin => "fresh-editor-bin".to_string(),
        _ => "fresh-editor".to_string(),
    });
    let aur_argv = aur_command(&aur_pkg);

    let argv: Option<Vec<&str>> = match channel {
        Channel::Homebrew => Some(vec!["brew", "upgrade", "--formula", &formula]),
        // `--exact` so a prefix match can't select another package, and
        // `--include-unknown` because we ship a .zip: winget often cannot read
        // an installed version out of a portable/archive package, and without
        // this it answers "No applicable upgrade found" forever.
        Channel::Winget => Some(vec![
            "winget",
            "upgrade",
            "--id",
            &winget_id,
            "--exact",
            "--include-unknown",
            "--silent",
            "--accept-package-agreements",
            "--accept-source-agreements",
            "--disable-interactivity",
        ]),
        // `flake.nix` builds with `pname = "fresh"`, which is the name a
        // profile element gets, so this is what `nix profile upgrade` matches.
        Channel::Nix => Some(vec!["nix", "profile", "upgrade", "fresh"]),
        Channel::FreebsdPkg => Some(vec!["pkg", "upgrade", "fresh"]),
        Channel::Cargo => Some(vec!["cargo", "install", "--locked", &pkg]),
        Channel::CargoBinstall => Some(vec!["cargo", "binstall", &pkg]),
        // `npm update -g` is the obvious command and the wrong one. npm's own
        // v10 docs say a global install is treated as if it carried a caret
        // range, and `^0.4.6` does not admit `0.5.0` — so every npm user would
        // be pinned inside the current minor until fresh reaches 1.0. npm 10
        // no longer behaves that way in practice, which is worse rather than
        // better: the behaviour differs by npm version, and a rule that forbids
        // runtime branching cannot rest on which npm the user happens to have.
        // `install @latest` means the same thing on npm 6, 10 and 11.
        Channel::Npm => Some(vec!["npm", "install", "-g", &npm_latest]),
        // mise installs this as a github backend tool (README:
        // `mise use github:sinelaw/fresh`), and that flake-style ref *is* the
        // tool's name in mise. A bare `fresh` matches nothing.
        //
        // `--bump` because plain `mise upgrade` keeps the range recorded in
        // mise.toml: a user who pinned `@0.4.6` would get exit 0 and no
        // upgrade. It rewrites their mise.toml, which the confirmation names.
        Channel::Mise => Some(vec!["mise", "upgrade", "--bump", MISE_TOOL]),
        // Built separately (owned, helper-dependent) — see `aur_argv`.
        Channel::Aur | Channel::AurBin | Channel::Pacman => None,
        // Resolved only once the release artifact is downloaded; see
        // `package_asset` / `install_command`.
        Channel::Apt | Channel::Dnf | Channel::Zypper | Channel::Flatpak => None,
        Channel::Appimage | Channel::Tarball | Channel::Prebuilt => None,
        // Manual: nothing to name (see `kind_for`).
        Channel::Snap
        | Channel::Scoop
        | Channel::Chocolatey
        | Channel::Source
        | Channel::Unknown => None,
    };

    let command = match channel {
        // Arch ships through the AUR only, so a `pacman` receipt continues the
        // same way. Built separately because it is owned, not borrowed.
        Channel::Aur | Channel::AurBin | Channel::Pacman => Some(aur_argv),
        _ => argv
            .as_ref()
            .map(|v| v.iter().map(|s| s.to_string()).collect::<Vec<_>>()),
    };

    let human = match kind {
        UpdateKind::SelfContained => {
            "download the latest release and replace in place (fresh --cmd update)".to_string()
        }
        UpdateKind::DownloadPackage => format!(
            "download the latest {} from the GitHub release and install it (fresh --cmd update)",
            package_extension(channel).unwrap_or("package")
        ),
        UpdateKind::Manual => manual_instructions(channel),
        _ => command
            .as_ref()
            .map(|c| c.join(" "))
            .unwrap_or_else(|| "see https://github.com/sinelaw/fresh/releases".to_string()),
    };

    UpdatePlan {
        kind,
        command,
        needs_privilege: needs_privilege(channel),
        human,
    }
}

/// How to recognise a [`UpdateKind::DownloadPackage`] channel's artifact among
/// the release assets: the filename ends with `extension` and contains `arch`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageAsset {
    /// Filename extension, including the dot (e.g. `.deb`).
    pub extension: &'static str,
    /// Architecture token as it appears in the filename (e.g. `amd64`).
    pub arch: String,
}

/// The artifact extension for a download-and-install channel.
const fn package_extension(channel: Channel) -> Option<&'static str> {
    match channel {
        Channel::Apt => Some(".deb"),
        // openSUSE installs the same .rpm we build for Fedora/RHEL.
        Channel::Dnf | Channel::Zypper => Some(".rpm"),
        Channel::Flatpak => Some(".flatpak"),
        _ => None,
    }
}

/// The release asset to fetch for `channel` on `target_triple`. `None` for
/// channels that are not [`UpdateKind::DownloadPackage`].
///
/// Each packaging tool spells the architecture its own way: dpkg uses Debian
/// arch names, rpm and flatpak use the CPU part of the target triple.
pub fn package_asset(channel: Channel, target_triple: &str) -> Option<PackageAsset> {
    package_asset_with(channel, target_triple, None)
}

/// [`package_asset`], preferring the architecture the installer *recorded* over
/// one derived from the target triple.
///
/// Deriving is a guess: it assumes this build's triple is the one the package
/// was built for and that we spell the arch the way the packaging tool does.
/// The pipeline that built the package knows both for certain, so when the
/// receipt carries `hints.pkg_arch` it wins outright — that is the whole point
/// of recording provenance instead of inferring it.
pub fn package_asset_with(
    channel: Channel,
    target_triple: &str,
    recorded_arch: Option<&str>,
) -> Option<PackageAsset> {
    let extension = package_extension(channel)?;
    if let Some(arch) = recorded_arch.filter(|a| !a.is_empty()) {
        return Some(PackageAsset {
            extension,
            arch: arch.to_string(),
        });
    }
    let cpu = target_triple.split('-').next().unwrap_or("x86_64");
    let arch = match channel {
        Channel::Apt if cpu == "aarch64" => "arm64",
        Channel::Apt => "amd64",
        Channel::Dnf | Channel::Zypper | Channel::Flatpak => cpu,
        _ => return None,
    };
    Some(PackageAsset {
        extension,
        arch: arch.to_string(),
    })
}

/// The argv that installs an already-downloaded release artifact. Root is
/// required for `dpkg`/`rpm` (see [`UpdatePlan::needs_privilege`]); the flatpak
/// bundle installs into the per-user installation, so it needs none.
pub fn install_command(channel: Channel, file: &Path) -> Option<Vec<String>> {
    install_command_with(channel, file, false)
}

/// [`install_command`], optionally permitting reinstallation of a version that
/// is already installed.
///
/// `rpm -U` treats "already installed" as an error where `dpkg -i` simply
/// unpacks over the top, so `--force` (which reinstalls the current version)
/// needs `--replacepkgs` to mean the same thing on both. Off by default: a
/// normal upgrade must not silently reinstall.
pub fn install_command_with(channel: Channel, file: &Path, reinstall: bool) -> Option<Vec<String>> {
    let path = file.to_string_lossy().into_owned();
    let mut argv: Vec<String> = match channel {
        // `apt-get install <file>`, not `dpkg -i <file>`. dpkg is, in its own
        // man page's words, "a medium-level tool": it unpacks and configures
        // but resolves no dependencies, so the first release that adds a
        // `Depends:` the system does not already satisfy would leave the
        // package half-configured and every later apt run complaining. apt-get
        // takes a local file, enters it into the transaction as a synthetic
        // repository entry, and resolves against the configured repos.
        //
        // `env DEBIAN_FRONTEND=noninteractive` because a maintainer script is
        // entitled to prompt via debconf, and there is nobody to answer inside
        // an update we launched.
        Channel::Apt => {
            let mut a: Vec<String> = vec![
                "env".into(),
                "DEBIAN_FRONTEND=noninteractive".into(),
                "apt-get".into(),
                "install".into(),
                "-y".into(),
            ];
            // apt reports "already the newest version" and exits 0 rather than
            // reinstalling, so --force needs to say so explicitly.
            if reinstall {
                a.push("--reinstall".into());
            }
            a
        }
        // `dnf install <file>`, not `rpm -U <file>`, for the same reason: rpm
        // resolves nothing from the configured repositories. dnf also handles
        // the same-version and older-version cases itself, where rpm errors
        // out and needs --replacepkgs / --oldpackage.
        //
        // `--nogpgcheck` covers this one unsigned local file. Signing the
        // release rpms would let it go away; see the follow-up issue.
        Channel::Dnf if reinstall => vec![
            "dnf".into(),
            "reinstall".into(),
            "-y".into(),
            "--nogpgcheck".into(),
        ],
        Channel::Dnf => vec![
            "dnf".into(),
            "install".into(),
            "-y".into(),
            "--nogpgcheck".into(),
        ],
        // The release .rpm is unsigned as far as zypper is concerned, and it
        // refuses a local file without this.
        Channel::Zypper => {
            // `--allow-unsigned-rpm` covers the one unsigned local file being
            // installed. `--no-gpg-checks` was here too and is deliberately
            // gone: it is a *global* option, so under `sudo --non-interactive`
            // it would also disable GPG verification of repository metadata and
            // of any dependencies zypper resolves in the same transaction —
            // far more than this one file needs.
            let mut z: Vec<String> = vec![
                "zypper".into(),
                "--non-interactive".into(),
                "install".into(),
                "--allow-unsigned-rpm".into(),
            ];
            if reinstall {
                z.push("--force".into());
            }
            z
        }
        _ => return None,
    };
    // apt-get reads a bare filename as a *package name* and fails with
    // "Unable to locate package fresh-editor_0.4.7-1_amd64.deb"; it only treats
    // an argument as a file when it looks like a path. Everything downloaded is
    // staged under an absolute path, so this is belt-and-braces — but the
    // failure it prevents is confusing enough to be worth two lines.
    // `is_absolute` alone is wrong here: on Windows a POSIX-style `/tmp/x.rpm`
    // has a root but no drive prefix, so it is not "absolute" and would get a
    // `./` glued on. `has_root` is the property that actually matters — the
    // package tool needs the argument to *look* like a path rather than a
    // package name, and these tools only ever run on Unix anyway.
    let path = if Path::new(&path).has_root() {
        path
    } else {
        format!("./{path}")
    };
    argv.push(path);
    Some(argv)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::confidence::Confidence;
    use crate::receipt::Hints;

    fn prov(channel: Channel) -> Provenance {
        Provenance::for_channel(channel, Confidence::Authoritative)
    }

    #[test]
    fn delegated_commands_template_defaults() {
        // The tap's formula is `fresh` (Formula/fresh.rb), not the package name.
        assert_eq!(
            plan(&prov(Channel::Homebrew)).human,
            "brew upgrade --formula fresh"
        );
        // `--include-unknown` is not optional for us: winget frequently cannot
        // read an installed version out of a portable/archive package, and
        // without it every upgrade answers "No applicable upgrade found".
        let winget = plan(&prov(Channel::Winget)).human;
        assert!(winget.starts_with("winget upgrade --id sinelaw.fresh-editor"));
        for flag in ["--exact", "--include-unknown", "--disable-interactivity"] {
            assert!(winget.contains(flag), "winget is missing {flag}: {winget}");
        }
        assert!(plan(&prov(Channel::AurBin)).human.contains("makepkg"));
    }

    /// §6's invariant: a channel only names an external command when a
    /// distribution actually exists for that command to act on. Inventing one
    /// produces a row that looks helpful and does nothing — `snap refresh
    /// fresh` for a snap that was never published, `zypper update fresh-editor`
    /// against an OBS project that does not exist.
    ///
    /// Both lists are exhaustive over the channels that name a package manager,
    /// so adding a channel forces a deliberate choice between them.
    #[test]
    fn only_channels_with_a_real_distribution_name_a_command() {
        // Each of these ships from something in this repository or a
        // repository we publish to.
        let distributed = [
            (
                Channel::Homebrew,
                "tap sinelaw/homebrew-fresh (release.yml)",
            ),
            (Channel::Aur, "AUR fresh-editor"),
            (Channel::AurBin, "AUR fresh-editor-bin"),
            (Channel::Winget, "winget-pkgs sinelaw.fresh-editor"),
            (Channel::Nix, "flake.nix"),
            (Channel::FreebsdPkg, "FreeBSD ports"),
            // Arch is AUR-only, so a pacman receipt continues through the AUR.
            (Channel::Pacman, "AUR (Arch has no official repo package)"),
            (Channel::Cargo, "crates.io"),
            (Channel::CargoBinstall, "crates.io + release archives"),
            (Channel::Npm, "@fresh-editor/fresh-editor"),
            (Channel::Mise, "github:sinelaw/fresh"),
            (Channel::AurBin, "AUR fresh-editor-bin"),
        ];
        // We ship no artifact snap, scoop or chocolatey could install, and no
        // pipeline writes their receipts, so there is no same-channel
        // continuation to name. (zypper and pacman are *not* here: openSUSE
        // continues via the release .rpm and Arch via the AUR — see
        // `every_reachable_channel_has_a_same_channel_continuation`.)
        let undistributed = [Channel::Snap, Channel::Scoop, Channel::Chocolatey];

        for (channel, source) in distributed {
            let p = plan(&prov(channel));
            assert!(
                matches!(p.kind, UpdateKind::Delegated | UpdateKind::Toolchain),
                "{channel} ships from {source} but is not delegated"
            );
            assert!(
                p.command.as_ref().is_some_and(|c| !c.is_empty()),
                "{channel} ships from {source} but names no command"
            );
        }

        for channel in undistributed {
            let p = plan(&prov(channel));
            assert_eq!(
                p.kind,
                UpdateKind::Manual,
                "{channel} has no distribution, so it must not delegate"
            );
            assert!(
                p.command.is_none(),
                "{channel} has no distribution but names a command: {:?}",
                p.command
            );
        }
    }

    /// A user who installed through one of our channels must be able to keep
    /// updating through that same channel. Sending them to a web page to
    /// download a file by hand is the failure this whole mechanism exists to
    /// avoid, so no channel that anything can actually resolve to may do it.
    #[test]
    fn every_reachable_channel_has_a_same_channel_continuation() {
        // Everything a receipt, an embedded channel or the path heuristic can
        // produce. `Unknown` is excluded: it is the one honest "we have no idea"
        // and has nothing to continue.
        let reachable = [
            Channel::Homebrew,
            Channel::Npm,
            Channel::Cargo,
            Channel::CargoBinstall,
            Channel::Aur,
            Channel::AurBin,
            Channel::Apt,
            Channel::Dnf,
            Channel::Zypper,
            Channel::Pacman,
            Channel::Flatpak,
            Channel::Appimage,
            Channel::Winget,
            Channel::Nix,
            Channel::FreebsdPkg,
            Channel::Mise,
            Channel::Tarball,
            Channel::Prebuilt,
        ];
        for channel in reachable {
            let p = plan(&prov(channel));
            // The invariant is about where the user is sent, not about the
            // kind. Flatpak is `Manual` because nothing we could emit would
            // run inside its sandbox — but it still names flatpak rather than
            // a tarball, so the user continues through the tool that
            // installed fresh.
            assert!(
                !p.human.contains("releases"),
                "{channel} dead-ends at the releases page instead of continuing through {channel}: {}",
                p.human
            );
            let has_route = matches!(
                p.kind,
                UpdateKind::SelfContained | UpdateKind::DownloadPackage | UpdateKind::Manual
            ) || p.command.as_ref().is_some_and(|c| !c.is_empty());
            assert!(has_route, "{channel} names no way to get the next version");
        }
    }

    /// The two package names that are not `fresh-editor` and not the binary
    /// name either, so a plausible-looking guess is wrong in both cases.
    #[test]
    fn tool_refs_match_how_each_tool_actually_installed_it() {
        // README: `mise use github:sinelaw/fresh` — the flake-style ref is the
        // tool's name in mise, so a bare `fresh` matches nothing.
        assert_eq!(
            plan(&prov(Channel::Mise)).human,
            "mise upgrade --bump github:sinelaw/fresh"
        );
        // flake.nix sets `pname = "fresh"`, not the `fresh-editor` package name
        // the receipt carries.
        assert_eq!(plan(&prov(Channel::Nix)).human, "nix profile upgrade fresh");
        // release.yml installs the tap formula as `Formula/fresh.rb`
        // (`class Fresh`), so the formula is `fresh`. Naming the package
        // instead gave `brew upgrade fresh-editor`, which errors with
        // "No available formula with the name".
        assert_eq!(
            plan(&prov(Channel::Homebrew)).human,
            "brew upgrade --formula fresh"
        );
    }

    #[test]
    fn privilege_flags() {
        assert!(plan(&prov(Channel::Apt)).needs_privilege);
        assert!(plan(&prov(Channel::Dnf)).needs_privilege);
        assert!(!plan(&prov(Channel::Homebrew)).needs_privilege);
        assert!(!plan(&prov(Channel::Npm)).needs_privilege);
    }

    #[test]
    fn hints_override_defaults() {
        let mut p = prov(Channel::AurBin);
        p.hints = Hints {
            aur_pkg: Some("fresh-editor-custom".to_string()),
            ..Hints::default()
        };
        assert!(plan(&p).human.contains("fresh-editor-custom.git"));
    }

    /// One AUR command, the same on every machine. Preferring a helper when
    /// one happened to be installed made the upgrade mechanism a property of
    /// the machine rather than of the proved provenance — two users with
    /// identical receipts would update by different routes, and neither route
    /// could be predicted from the receipt.
    #[test]
    fn aur_uses_one_command_everywhere() {
        let cmd = aur_command("fresh-editor-bin");
        assert_eq!(cmd[0], "sh", "expected the makepkg route, got {cmd:?}");
        let script = &cmd[2];
        assert!(script.contains("git clone"), "{script}");
        assert!(script.contains("makepkg --syncdeps --install"), "{script}");
        assert!(
            script.contains("aur.archlinux.org/fresh-editor-bin.git"),
            "{script}"
        );
        // No helper is named, whatever is installed on the machine.
        for helper in ["yay", "paru", "pikaur", "trizen"] {
            assert!(!script.contains(helper), "names {helper}: {script}");
        }
    }

    /// The package name is interpolated into a shell command, so a receipt
    /// carrying metacharacters is refused rather than run.
    #[test]
    fn aur_refuses_an_implausible_package_name() {
        for bad in ["fresh; rm -rf /", "$(whoami)", "a b", "", "pkg`id`"] {
            let cmd = aur_command(bad);
            assert!(
                !cmd[2].contains("git clone"),
                "built a clone command for {bad:?}: {cmd:?}"
            );
        }
    }

    /// `makepkg -si` calls sudo itself for the pacman step; wrapping it in
    /// another elevation would nest password prompts.
    #[test]
    fn aur_is_not_separately_elevated() {
        for channel in [Channel::Aur, Channel::AurBin, Channel::Pacman] {
            assert!(
                !plan(&prov(channel)).needs_privilege,
                "{channel} should not be externally elevated"
            );
        }
    }

    #[test]
    fn self_contained_has_no_command() {
        let p = prov(Channel::Tarball);
        let plan = plan(&p);
        assert_eq!(plan.kind, UpdateKind::SelfContained);
        assert!(plan.command.is_none());
    }

    /// The .deb/.rpm/.flatpak ship as release artifacts only — there is no apt
    /// repo, dnf repo or Flathub remote to upgrade from, so these channels must
    /// never emit a package-manager upgrade command.
    #[test]
    fn release_artifact_channels_do_not_delegate_to_a_repo() {
        for channel in [Channel::Apt, Channel::Dnf, Channel::Zypper] {
            let p = plan(&prov(channel));
            assert_eq!(p.kind, UpdateKind::DownloadPackage, "{channel}");
            assert!(
                p.command.is_none(),
                "{channel} must not have a repo command"
            );
            assert!(p.human.contains("GitHub release"), "{channel}: {}", p.human);
            // Still externally managed: never swap the packaged binary in place.
            assert!(p.kind.managed() && !p.kind.self_update(), "{channel}");
        }
    }

    #[test]
    fn package_assets_use_each_tool_s_arch_spelling() {
        let cases = [
            (Channel::Apt, "x86_64-unknown-linux-gnu", ".deb", "amd64"),
            (Channel::Apt, "aarch64-unknown-linux-gnu", ".deb", "arm64"),
            (Channel::Dnf, "x86_64-unknown-linux-gnu", ".rpm", "x86_64"),
            (Channel::Dnf, "aarch64-unknown-linux-gnu", ".rpm", "aarch64"),
            (
                Channel::Flatpak,
                "x86_64-unknown-linux-gnu",
                ".flatpak",
                "x86_64",
            ),
        ];
        for (channel, triple, ext, arch) in cases {
            let a = package_asset(channel, triple).expect("asset");
            assert_eq!((a.extension, a.arch.as_str()), (ext, arch), "{channel}");
        }
        assert!(package_asset(Channel::Homebrew, "x86_64-unknown-linux-gnu").is_none());
    }

    /// `--force` reinstalls the version already present. `dpkg -i` unpacks
    /// over the top happily; `rpm -U` calls that an error unless told
    /// otherwise, so the flag has to mean the same thing on both.
    #[test]
    fn reinstall_is_permitted_only_when_asked_for() {
        let rpm = Path::new("/tmp/x.rpm");
        assert_eq!(
            install_command_with(Channel::Dnf, rpm, true).unwrap(),
            vec!["dnf", "reinstall", "-y", "--nogpgcheck", "/tmp/x.rpm"]
        );
        // A normal upgrade must not silently reinstall.
        assert_eq!(
            install_command_with(Channel::Dnf, rpm, false).unwrap(),
            vec!["dnf", "install", "-y", "--nogpgcheck", "/tmp/x.rpm"]
        );
        assert_eq!(
            install_command(Channel::Dnf, rpm).unwrap(),
            install_command_with(Channel::Dnf, rpm, false).unwrap()
        );

        assert!(install_command_with(Channel::Zypper, rpm, true)
            .unwrap()
            .contains(&"--force".to_string()));
        assert!(!install_command_with(Channel::Zypper, rpm, false)
            .unwrap()
            .contains(&"--force".to_string()));

        // dpkg needs no extra flag either way.
        // apt-get reports "already the newest version" and exits 0 rather
        // than reinstalling, so unlike the old `dpkg -i` these two differ.
        let deb = Path::new("/tmp/x.deb");
        assert!(install_command_with(Channel::Apt, deb, true)
            .unwrap()
            .contains(&"--reinstall".to_string()));
        assert!(!install_command_with(Channel::Apt, deb, false)
            .unwrap()
            .contains(&"--reinstall".to_string()));
    }

    #[test]
    fn install_commands_take_the_downloaded_file() {
        let f = Path::new("/tmp/fresh-editor_0.4.7-1_amd64.deb");
        assert_eq!(
            install_command(Channel::Apt, f).unwrap(),
            vec![
                "env",
                "DEBIAN_FRONTEND=noninteractive",
                "apt-get",
                "install",
                "-y",
                "/tmp/fresh-editor_0.4.7-1_amd64.deb"
            ]
        );
        assert_eq!(
            install_command(Channel::Dnf, Path::new("/tmp/x.rpm")).unwrap(),
            vec!["dnf", "install", "-y", "--nogpgcheck", "/tmp/x.rpm"]
        );
        // Flatpak no longer has an install command at all: it could never
        // have run in the sandbox that would have issued it.
        assert!(install_command(Channel::Flatpak, Path::new("/tmp/x.flatpak")).is_none());
        // apt-get reads a bare filename as a package name, so a relative path
        // has to be made to look like one.
        assert!(install_command(Channel::Apt, Path::new("x.deb"))
            .unwrap()
            .contains(&"./x.deb".to_string()));
        assert!(!plan(&prov(Channel::Flatpak)).needs_privilege);
        assert!(install_command(Channel::Homebrew, f).is_none());
    }

    #[test]
    fn toolchain_commands() {
        assert_eq!(
            plan(&prov(Channel::Cargo)).human,
            "cargo install --locked fresh-editor"
        );
        assert_eq!(
            plan(&prov(Channel::Npm)).human,
            "npm install -g @fresh-editor/fresh-editor@latest"
        );
    }
}
