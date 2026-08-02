//! What the update confirmation should say and offer, derived from the
//! resolved [`UpdatePlan`].
//!
//! "Update to vX" means something different in each channel: an in-place swap,
//! a release package fetched and handed to dpkg/rpm/zypper/flatpak, or the
//! owning manager's own upgrade command. A single row with one label described
//! all of them and was accurate for none.
//!
//! Two rules hold across every variant. The update **completes** — whatever it
//! needs downloaded gets downloaded, and whatever needs root prompts for a
//! password in the update terminal rather than being handed back as a chore.
//! And the user **chooses**: every offer with a nameable command also offers to
//! show it instead of running it.
//!
//! This is the pure mapping from plan to offer; the popup in
//! `app::popup_dialogs` turns an [`UpdateOffer`] into rows and body text so the
//! locale keys stay literal at the `t!` call sites.

use fresh_update::{UpdateKind, UpdatePlan};

/// What "Update now" will actually do. One variant per distinct promise we can
/// honestly make to the user — but in every case except [`UpdateOffer::Manual`]
/// the promise is that the update *completes*, with no follow-up chore.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateOffer {
    /// We own the bits: download, verify, and replace the binary in place.
    SelfContained,
    /// Download and verify the release package, then install it with the local
    /// package tool (elevating if that tool needs root).
    DownloadPackage,
    /// Run the owning package manager's own upgrade command.
    RunCommand,
    /// No update mechanism at all (unknown provenance, or a source build).
    Manual,
}

/// A row offered in the update popup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateChoice {
    /// Do it — download and install, prompting for a password if the install
    /// needs root. Runs `fresh --cmd update --yes`.
    UpdateNow,
    /// Print the command that would be run and stop, so the user can inspect
    /// it or run it themselves. Runs `fresh --cmd update --yes --print-command`.
    ShowCommand,
}

impl UpdateChoice {
    /// The popup action key this choice dispatches on.
    pub fn action_key(self) -> &'static str {
        match self {
            UpdateChoice::UpdateNow => "update",
            UpdateChoice::ShowCommand => "show_command",
        }
    }
}

impl UpdateOffer {
    /// The rows to offer, in order. "Update now" comes first everywhere it is
    /// possible, because completing the update is the point; "Show the command"
    /// rides along wherever there is a concrete command to show, so a user who
    /// would rather drive it themselves keeps that control.
    pub fn choices(self) -> &'static [UpdateChoice] {
        match self {
            UpdateOffer::SelfContained => &[UpdateChoice::UpdateNow],
            UpdateOffer::DownloadPackage | UpdateOffer::RunCommand => {
                &[UpdateChoice::UpdateNow, UpdateChoice::ShowCommand]
            }
            // Nothing to run and nothing to name.
            UpdateOffer::Manual => &[UpdateChoice::ShowCommand],
        }
    }
}

/// Map a resolved update plan to the offer the confirmation should present.
pub fn offer_for(plan: &UpdatePlan) -> UpdateOffer {
    match plan.kind {
        UpdateKind::SelfContained => UpdateOffer::SelfContained,
        UpdateKind::DownloadPackage => UpdateOffer::DownloadPackage,
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

    fn plan_for(channel: Channel) -> UpdatePlan {
        fresh_update::plan(&Provenance::for_channel(channel, Confidence::Authoritative))
    }

    fn offer(channel: Channel) -> UpdateOffer {
        offer_for(&plan_for(channel))
    }

    #[test]
    fn each_channel_gets_the_offer_matching_what_we_will_do() {
        let cases = [
            // We own the bits.
            (Channel::Tarball, UpdateOffer::SelfContained),
            (Channel::Appimage, UpdateOffer::SelfContained),
            // Package manager owns the files, nothing hosts them: fetch the
            // artifact and install it (elevating only where the tool needs it).
            (Channel::Apt, UpdateOffer::DownloadPackage),
            (Channel::Dnf, UpdateOffer::DownloadPackage),
            (Channel::Zypper, UpdateOffer::DownloadPackage),
            (Channel::Flatpak, UpdateOffer::DownloadPackage),
            // The owning manager's own command.
            (Channel::Homebrew, UpdateOffer::RunCommand),
            (Channel::Winget, UpdateOffer::RunCommand),
            (Channel::Npm, UpdateOffer::RunCommand),
            (Channel::Cargo, UpdateOffer::RunCommand),
            (Channel::FreebsdPkg, UpdateOffer::RunCommand),
            (Channel::Aur, UpdateOffer::RunCommand),
            (Channel::Pacman, UpdateOffer::RunCommand),
            // Nothing we can do.
            (Channel::Unknown, UpdateOffer::Manual),
            (Channel::Source, UpdateOffer::Manual),
        ];
        for (channel, expected) in cases {
            assert_eq!(offer(channel), expected, "offer for {}", channel.id());
        }
    }

    /// Every channel a user can actually be on offers to complete the update,
    /// and every one with a nameable command also offers to show it instead —
    /// the user picks, we don't decide for them.
    #[test]
    fn reachable_channels_offer_to_finish_it_and_to_show_the_command() {
        let reachable = [
            Channel::Tarball,
            Channel::Appimage,
            Channel::Apt,
            Channel::Dnf,
            Channel::Zypper,
            Channel::Flatpak,
            Channel::Homebrew,
            Channel::Winget,
            Channel::Npm,
            Channel::Cargo,
            Channel::CargoBinstall,
            Channel::Mise,
            Channel::Nix,
            Channel::FreebsdPkg,
            Channel::Aur,
            Channel::AurBin,
            Channel::Pacman,
        ];
        for channel in reachable {
            let choices = offer(channel).choices();
            assert!(
                choices.contains(&UpdateChoice::UpdateNow),
                "{channel} does not offer to complete the update"
            );
            if offer(channel) != UpdateOffer::SelfContained {
                assert!(
                    choices.contains(&UpdateChoice::ShowCommand),
                    "{channel} gives the user no way to inspect the command first"
                );
            }
        }
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

    #[test]
    fn action_keys_are_stable() {
        assert_eq!(UpdateChoice::UpdateNow.action_key(), "update");
        assert_eq!(UpdateChoice::ShowCommand.action_key(), "show_command");
    }
}
