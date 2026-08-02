//! The channel → update-strategy registry: the single place that encodes
//! "how do we update channel X". See `docs/internal/packaging-self-update.md`
//! §6.

use crate::channel::Channel;
use crate::provenance::Provenance;
use std::path::Path;

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

/// The update kind for a channel.
pub const fn kind_for(channel: Channel) -> UpdateKind {
    match channel {
        // We publish the .deb/.rpm/.flatpak as release artifacts only — no apt
        // repo, no dnf repo, no Flathub remote — so their package managers have
        // nothing to upgrade *from*. Fetch the artifact and install it locally.
        Channel::Apt | Channel::Dnf | Channel::Flatpak => UpdateKind::DownloadPackage,
        Channel::Homebrew
        | Channel::Zypper
        | Channel::Pacman
        | Channel::Aur
        | Channel::AurBin
        | Channel::Snap
        | Channel::Winget
        | Channel::Scoop
        | Channel::Chocolatey
        | Channel::Nix
        | Channel::FreebsdPkg => UpdateKind::Delegated,
        Channel::Cargo | Channel::CargoBinstall | Channel::Npm | Channel::Mise => {
            UpdateKind::Toolchain
        }
        Channel::Appimage | Channel::Tarball | Channel::Prebuilt => UpdateKind::SelfContained,
        Channel::Source | Channel::Unknown => UpdateKind::Manual,
    }
}

/// Whether the delegated command for a channel needs root/admin.
const fn needs_privilege(channel: Channel) -> bool {
    matches!(
        channel,
        Channel::Apt
            | Channel::Dnf
            | Channel::Zypper
            | Channel::Pacman
            | Channel::FreebsdPkg
            | Channel::Chocolatey
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
    let formula = h.formula.clone().unwrap_or_else(|| pkg.clone());
    let winget_id = h
        .winget_id
        .clone()
        .unwrap_or_else(|| "sinelaw.fresh-editor".to_string());
    let npm_pkg = h
        .npm_pkg
        .clone()
        .unwrap_or_else(|| "@fresh-editor/fresh-editor".to_string());
    let aur_helper = h.aur_helper.clone().unwrap_or_else(|| "yay".to_string());
    let aur_pkg = h.aur_pkg.clone().unwrap_or_else(|| match channel {
        Channel::AurBin => "fresh-editor-bin".to_string(),
        _ => "fresh-editor".to_string(),
    });

    let argv: Option<Vec<&str>> = match channel {
        Channel::Homebrew => Some(vec!["brew", "upgrade", &formula]),
        Channel::Zypper => Some(vec!["zypper", "update", &pkg]),
        Channel::Pacman => Some(vec!["pacman", "-Syu", &pkg]),
        Channel::Aur | Channel::AurBin => Some(vec![&aur_helper, "-S", &aur_pkg]),
        Channel::Winget => Some(vec!["winget", "upgrade", "--id", &winget_id]),
        Channel::Scoop => Some(vec!["scoop", "update", "fresh"]),
        Channel::Chocolatey => Some(vec!["choco", "upgrade", "fresh"]),
        Channel::Snap => Some(vec!["snap", "refresh", "fresh"]),
        Channel::Nix => Some(vec!["nix", "profile", "upgrade", "fresh"]),
        Channel::FreebsdPkg => Some(vec!["pkg", "upgrade", "fresh"]),
        Channel::Cargo => Some(vec!["cargo", "install", "--locked", &pkg]),
        Channel::CargoBinstall => Some(vec!["cargo", "binstall", &pkg]),
        Channel::Npm => Some(vec!["npm", "update", "-g", &npm_pkg]),
        Channel::Mise => Some(vec!["mise", "upgrade", "fresh"]),
        // Resolved only once the release artifact is downloaded; see
        // `package_asset` / `install_command`.
        Channel::Apt | Channel::Dnf | Channel::Flatpak => None,
        Channel::Appimage | Channel::Tarball | Channel::Prebuilt => None,
        Channel::Source => None,
        Channel::Unknown => None,
    };

    let command = argv
        .as_ref()
        .map(|v| v.iter().map(|s| s.to_string()).collect::<Vec<_>>());

    let human = match kind {
        UpdateKind::SelfContained => {
            "download the latest release and replace in place (fresh --cmd update)".to_string()
        }
        UpdateKind::DownloadPackage => format!(
            "download the latest {} from the GitHub release and install it (fresh --cmd update)",
            package_extension(channel).unwrap_or("package")
        ),
        UpdateKind::Manual if channel == Channel::Source => {
            "git pull && cargo install --path crates/fresh-editor".to_string()
        }
        UpdateKind::Manual => "see https://github.com/sinelaw/fresh/releases".to_string(),
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
        Channel::Dnf => Some(".rpm"),
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
    let cpu = target_triple.split('-').next().unwrap_or("x86_64");
    let arch = match channel {
        Channel::Apt if cpu == "aarch64" => "arm64",
        Channel::Apt => "amd64",
        Channel::Dnf | Channel::Flatpak => cpu,
        _ => return None,
    };
    Some(PackageAsset {
        extension: package_extension(channel)?,
        arch: arch.to_string(),
    })
}

/// The argv that installs an already-downloaded release artifact. Root is
/// required for `dpkg`/`rpm` (see [`UpdatePlan::needs_privilege`]); the flatpak
/// bundle installs into the per-user installation, so it needs none.
pub fn install_command(channel: Channel, file: &Path) -> Option<Vec<String>> {
    let path = file.to_string_lossy().into_owned();
    let mut argv: Vec<String> = match channel {
        Channel::Apt => vec!["dpkg".into(), "-i".into()],
        Channel::Dnf => vec!["rpm".into(), "-U".into()],
        Channel::Flatpak => vec![
            "flatpak".into(),
            "install".into(),
            "--user".into(),
            "--or-update".into(),
            "--noninteractive".into(),
        ],
        _ => return None,
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
        assert_eq!(
            plan(&prov(Channel::Homebrew)).human,
            "brew upgrade fresh-editor"
        );
        assert_eq!(
            plan(&prov(Channel::Winget)).human,
            "winget upgrade --id sinelaw.fresh-editor"
        );
        assert_eq!(
            plan(&prov(Channel::AurBin)).human,
            "yay -S fresh-editor-bin"
        );
        assert_eq!(plan(&prov(Channel::Aur)).human, "yay -S fresh-editor");
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
            aur_helper: Some("paru".to_string()),
            aur_pkg: Some("fresh-editor-bin".to_string()),
            ..Hints::default()
        };
        assert_eq!(plan(&p).human, "paru -S fresh-editor-bin");
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
        for channel in [Channel::Apt, Channel::Dnf, Channel::Flatpak] {
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

    #[test]
    fn install_commands_take_the_downloaded_file() {
        let f = Path::new("/tmp/fresh-editor_0.4.7-1_amd64.deb");
        assert_eq!(
            install_command(Channel::Apt, f).unwrap(),
            vec!["dpkg", "-i", "/tmp/fresh-editor_0.4.7-1_amd64.deb"]
        );
        assert_eq!(
            install_command(Channel::Dnf, Path::new("/tmp/x.rpm")).unwrap(),
            vec!["rpm", "-U", "/tmp/x.rpm"]
        );
        // The flatpak bundle installs per-user, so it needs no privilege.
        assert!(
            install_command(Channel::Flatpak, Path::new("/tmp/x.flatpak"))
                .unwrap()
                .contains(&"--user".to_string())
        );
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
            "npm update -g @fresh-editor/fresh-editor"
        );
    }
}
