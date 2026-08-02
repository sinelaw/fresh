//! What the update confirmation should say and offer, derived from the
//! resolved [`UpdatePlan`].
//!
//! "Update to vX" means something different in each channel: an in-place swap
//! we perform, a download plus a root command we deliberately leave to the
//! user, a per-user install we can run, a command we will only print, or
//! nothing we can do at all. A single row with one label described all five and
//! was accurate for none of them.
//!
//! This is the pure mapping from plan to offer; the popup in
//! `app::popup_dialogs` turns an [`UpdateOffer`] into rows and body text so the
//! locale keys stay literal at the `t!` call sites.

use fresh_update::{UpdateKind, UpdatePlan};

/// What confirming the update will actually do. One variant per distinct
/// promise we can honestly make to the user.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateOffer {
    /// We own the bits: download, verify, and replace the binary in place.
    SelfContained,
    /// Download the release package and install it — no privilege needed
    /// (the Flatpak bundle installs into the per-user installation).
    DownloadPackage,
    /// Download and verify the release package, then stop: installing it is a
    /// root `dpkg -i` / `rpm -U` we will not run for the user.
    DownloadPackagePrivileged,
    /// A package manager owns this install and its command needs no privilege,
    /// so we can run it.
    RunCommand,
    /// A package manager owns this install and its command needs root, so we
    /// show the command rather than running it.
    ShowCommand,
    /// No update mechanism at all (unknown provenance, or a source build).
    Manual,
}

impl UpdateOffer {
    /// Whether taking this offer leaves the user a command to run themselves.
    /// These are the offers that end in [`SelfUpdatePhase::ActionRequired`]
    /// rather than an installed update, and the confirmation says so up front.
    ///
    /// [`SelfUpdatePhase::ActionRequired`]:
    ///     crate::services::release_checker::SelfUpdatePhase::ActionRequired
    pub fn leaves_command_for_user(self) -> bool {
        matches!(
            self,
            UpdateOffer::DownloadPackagePrivileged | UpdateOffer::ShowCommand | UpdateOffer::Manual
        )
    }
}

/// Map a resolved update plan to the offer the confirmation should present.
pub fn offer_for(plan: &UpdatePlan) -> UpdateOffer {
    match plan.kind {
        UpdateKind::SelfContained => UpdateOffer::SelfContained,
        UpdateKind::DownloadPackage if plan.needs_privilege => {
            UpdateOffer::DownloadPackagePrivileged
        }
        UpdateKind::DownloadPackage => UpdateOffer::DownloadPackage,
        UpdateKind::Delegated | UpdateKind::Toolchain if plan.needs_privilege => {
            UpdateOffer::ShowCommand
        }
        // A delegated command we cannot actually name is no better than manual
        // guidance, so don't promise to run one.
        UpdateKind::Delegated | UpdateKind::Toolchain => match &plan.command {
            Some(cmd) if !cmd.is_empty() => UpdateOffer::RunCommand,
            _ => UpdateOffer::Manual,
        },
        UpdateKind::Manual => UpdateOffer::Manual,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_update::{Channel, Confidence, Provenance};

    fn offer(channel: Channel) -> UpdateOffer {
        let prov = Provenance::for_channel(channel, Confidence::Authoritative);
        offer_for(&fresh_update::plan(&prov))
    }

    #[test]
    fn each_channel_gets_the_offer_matching_what_we_will_do() {
        let cases = [
            // We own the bits.
            (Channel::Tarball, UpdateOffer::SelfContained),
            (Channel::Appimage, UpdateOffer::SelfContained),
            // Package manager owns the files, nothing hosts them.
            (Channel::Apt, UpdateOffer::DownloadPackagePrivileged),
            (Channel::Dnf, UpdateOffer::DownloadPackagePrivileged),
            (Channel::Flatpak, UpdateOffer::DownloadPackage),
            // Delegated, runnable without root.
            (Channel::Homebrew, UpdateOffer::RunCommand),
            (Channel::Winget, UpdateOffer::RunCommand),
            (Channel::Npm, UpdateOffer::RunCommand),
            (Channel::Cargo, UpdateOffer::RunCommand),
            // Delegated, needs root — we only ever print these.
            (Channel::FreebsdPkg, UpdateOffer::ShowCommand),
            // Nothing we can do.
            (Channel::Unknown, UpdateOffer::Manual),
            (Channel::Source, UpdateOffer::Manual),
            // Channels with no distribution at all: the registry routes them to
            // Manual rather than naming an invented command, so the popup must
            // not offer to run one.
            (Channel::Pacman, UpdateOffer::Manual),
            (Channel::Zypper, UpdateOffer::Manual),
            (Channel::Snap, UpdateOffer::Manual),
        ];
        for (channel, expected) in cases {
            assert_eq!(offer(channel), expected, "offer for {}", channel.id());
        }
    }

    /// The distinction the confirmation has to make up front: which offers end
    /// with the user still holding a command to run.
    #[test]
    fn privileged_and_manual_offers_leave_work_for_the_user() {
        assert!(UpdateOffer::DownloadPackagePrivileged.leaves_command_for_user());
        assert!(UpdateOffer::ShowCommand.leaves_command_for_user());
        assert!(UpdateOffer::Manual.leaves_command_for_user());

        assert!(!UpdateOffer::SelfContained.leaves_command_for_user());
        assert!(!UpdateOffer::DownloadPackage.leaves_command_for_user());
        assert!(!UpdateOffer::RunCommand.leaves_command_for_user());
    }

    /// A delegated plan whose command we cannot build must not offer to run it.
    #[test]
    fn delegated_without_a_command_falls_back_to_manual() {
        let plan = UpdatePlan {
            kind: UpdateKind::Delegated,
            command: None,
            needs_privilege: false,
            human: "see the releases page".to_string(),
        };
        assert_eq!(offer_for(&plan), UpdateOffer::Manual);
    }
}
