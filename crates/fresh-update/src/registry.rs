//! The channel → update-strategy registry: the single place that encodes
//! "how do we update channel X". See `docs/internal/packaging-self-update.md`
//! §6.

use crate::channel::Channel;
use crate::provenance::Provenance;

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
    /// Unknown/source — point the user at instructions only.
    Manual,
}

impl UpdateKind {
    /// Default `managed` flag for this kind (an external manager owns it).
    pub const fn managed(self) -> bool {
        matches!(self, UpdateKind::Delegated | UpdateKind::Toolchain)
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
        Channel::Homebrew
        | Channel::Apt
        | Channel::Dnf
        | Channel::Zypper
        | Channel::Pacman
        | Channel::Aur
        | Channel::AurBin
        | Channel::Flatpak
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
    let flatpak_ref = h
        .flatpak_ref
        .clone()
        .unwrap_or_else(|| "io.github.sinelaw.fresh".to_string());
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
        Channel::Apt => Some(vec!["apt-get", "install", "--only-upgrade", &pkg]),
        Channel::Dnf => Some(vec!["dnf", "upgrade", &pkg]),
        Channel::Zypper => Some(vec!["zypper", "update", &pkg]),
        Channel::Pacman => Some(vec!["pacman", "-Syu", &pkg]),
        Channel::Aur | Channel::AurBin => Some(vec![&aur_helper, "-S", &aur_pkg]),
        Channel::Winget => Some(vec!["winget", "upgrade", "--id", &winget_id]),
        Channel::Scoop => Some(vec!["scoop", "update", "fresh"]),
        Channel::Chocolatey => Some(vec!["choco", "upgrade", "fresh"]),
        Channel::Flatpak => Some(vec!["flatpak", "update", &flatpak_ref]),
        Channel::Snap => Some(vec!["snap", "refresh", "fresh"]),
        Channel::Nix => Some(vec!["nix", "profile", "upgrade", "fresh"]),
        Channel::FreebsdPkg => Some(vec!["pkg", "upgrade", "fresh"]),
        Channel::Cargo => Some(vec!["cargo", "install", "--locked", &pkg]),
        Channel::CargoBinstall => Some(vec!["cargo", "binstall", &pkg]),
        Channel::Npm => Some(vec!["npm", "update", "-g", &npm_pkg]),
        Channel::Mise => Some(vec!["mise", "upgrade", "fresh"]),
        Channel::Appimage | Channel::Tarball | Channel::Prebuilt => None,
        Channel::Source => None,
        Channel::Unknown => None,
    };

    let command = argv
        .as_ref()
        .map(|v| v.iter().map(|s| s.to_string()).collect::<Vec<_>>());

    let human = match kind {
        UpdateKind::SelfContained => {
            "download the latest release and replace in place (fresh update)".to_string()
        }
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
            plan(&prov(Channel::Flatpak)).human,
            "flatpak update io.github.sinelaw.fresh"
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
