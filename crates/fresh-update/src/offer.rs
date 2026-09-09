//! What the update confirmation should say and offer, derived from the
//! resolved [`UpdatePlan`].
//!
//! "Update to vX" means something different in each channel: an in-place swap,
//! a release package fetched and handed to dpkg/rpm/zypper/flatpak, or the
//! owning manager's own upgrade command. A single row with one label described
//! all of them and was accurate for none.
//!
//! Three rules hold across every variant. We **only finish what we own**: the
//! sole offer that completes an update is the in-place swap of our own binary.
//! Everywhere else the offer names the command and stops, because `fresh` does
//! not run another tool on the user's behalf — see the module note on `engine`.
//! The user is **told what is left to do**: every offer with a nameable command
//! also offers to show it. And the offer follows from the resolved **kind
//! alone** — one mechanism per provenance class, never a different route
//! because of what happens to be installed on this particular machine.
//!
//! This is the pure mapping from plan to offer; the popup in
//! `app::popup_dialogs` turns an [`UpdateOffer`] into rows and body text so the
//! locale keys stay literal at the `t!` call sites.

use crate::{UpdateKind, UpdatePlan};

/// What the popup can honestly promise. One variant per distinct outcome —
/// and only [`UpdateOffer::SelfContained`] promises the update *completes*,
/// because it is the only one where we do the finishing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateOffer {
    /// We own the bits: download, verify, and replace the binary in place. The
    /// only offer that finishes the job.
    SelfContained,
    /// Download and verify the release package, then hand the user the command
    /// that installs it.
    DownloadPackage,
    /// Name the owning package manager's own upgrade command, for the user to
    /// run.
    RunCommand,
    /// No update mechanism at all (unknown provenance, or a source build).
    Manual,
}

/// A row offered in the update popup.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpdateChoice {
    /// Do it — download, verify and replace our own binary. Never needs a
    /// password, because it only ever writes a file the user already owns.
    /// Runs `fresh --cmd update --yes`.
    UpdateNow,
    /// Fetch and verify the package, then stop and print the install command
    /// against the file on disk. The part that needs the network is done, the
    /// part that needs root is the user's. Runs
    /// `fresh --cmd update --yes --download-only`.
    DownloadOnly,
    /// Touch nothing. Print the commands that would fetch and install the
    /// update, for a user who wants to read before anything happens. Runs
    /// `fresh --cmd update --yes --print-command`.
    ShowCommand,
}

impl UpdateChoice {
    /// The popup action key this choice dispatches on.
    pub fn action_key(self) -> &'static str {
        match self {
            UpdateChoice::UpdateNow => "update",
            UpdateChoice::DownloadOnly => "download_only",
            UpdateChoice::ShowCommand => "show_command",
        }
    }
}

impl UpdateOffer {
    /// The rows to offer, in order, from most automatic to least.
    ///
    /// "Update now" appears only for a self-contained install, because that is
    /// the only case where accepting it finishes the job. Offering it anywhere
    /// else would promise something `fresh` deliberately does not do: run
    /// another tool on the user's behalf.
    ///
    /// "Download only" exists for one case and would be meaningless anywhere
    /// else: a release package that we fetch and a package manager installs.
    /// There the work splits cleanly in two — the half that needs the network
    /// and the half that needs root — and we do the first. The delegated
    /// channels have no such split (the manager does its own downloading), and
    /// a self-contained swap has nothing to hand over.
    ///
    /// "Show the command" rides along wherever there is a concrete command to
    /// name, and is exactly what it says: nothing fetched, nothing written.
    pub fn choices(self) -> &'static [UpdateChoice] {
        match self {
            // The only rung where "now" is ours to offer, because it is the
            // only place we are replacing a file we own.
            UpdateOffer::SelfContained => &[UpdateChoice::UpdateNow],
            // We fetch and verify; installing it belongs to dpkg/rpm. There is
            // no "update now" to offer, so the top rung is the fetch.
            UpdateOffer::DownloadPackage => {
                &[UpdateChoice::DownloadOnly, UpdateChoice::ShowCommand]
            }
            // The owning manager's command, named. We do not run it — see the
            // module note on `engine` — so naming it is the whole offer.
            UpdateOffer::RunCommand => &[UpdateChoice::ShowCommand],
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
        UpdateKind::Delegated | UpdateKind::Toolchain => UpdateOffer::RunCommand,
        UpdateKind::Manual => UpdateOffer::Manual,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Channel, Confidence, Provenance};

    fn plan_for(channel: Channel) -> UpdatePlan {
        crate::plan(&Provenance::for_channel(channel, Confidence::Authoritative))
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
            // The owning manager's own command.
            (Channel::Homebrew, UpdateOffer::RunCommand),
            (Channel::Winget, UpdateOffer::RunCommand),
            (Channel::Npm, UpdateOffer::RunCommand),
            (Channel::Cargo, UpdateOffer::RunCommand),
            (Channel::FreebsdPkg, UpdateOffer::RunCommand),
            (Channel::Aur, UpdateOffer::RunCommand),
            (Channel::Pacman, UpdateOffer::RunCommand),
            // Nothing we can do *here*. Flatpak is the interesting one: a
            // Flatpak build runs in a sandbox with no `flatpak` binary, so
            // any command we emitted could not execute in the process that
            // emitted it. The popup names the host command instead of
            // offering a button that fails.
            (Channel::Flatpak, UpdateOffer::Manual),
            (Channel::Snap, UpdateOffer::Manual),
            (Channel::Unknown, UpdateOffer::Manual),
            (Channel::Source, UpdateOffer::Manual),
        ];
        for (channel, expected) in cases {
            assert_eq!(offer(channel), expected, "offer for {}", channel.id());
        }
    }

    /// Exactly one offer promises to finish the job, and it is the one where
    /// finishing it means writing a file we own. Everywhere else the promise is
    /// to name the command — `fresh` does not run another tool on the user's
    /// behalf, so an "Update now" row there would be a button that lies.
    ///
    /// Flatpak is absent for a second, older reason: nothing we could run would
    /// work inside its sandbox at all.
    #[test]
    fn only_a_self_contained_install_offers_to_finish_it() {
        let reachable = [
            Channel::Tarball,
            Channel::Appimage,
            Channel::Apt,
            Channel::Dnf,
            Channel::Zypper,
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
            let offer = offer(channel);
            let choices = offer.choices();
            let promises_completion = choices.contains(&UpdateChoice::UpdateNow);
            assert_eq!(
                promises_completion,
                offer == UpdateOffer::SelfContained,
                "{channel} offers UpdateNow: {promises_completion}, but it is \
                 {offer:?} — only a self-contained install may promise to finish"
            );
            assert!(
                !choices.is_empty(),
                "{channel} offers the user nothing at all"
            );
            if offer != UpdateOffer::SelfContained {
                assert!(
                    choices.contains(&UpdateChoice::ShowCommand),
                    "{channel} never names the command the user has to run"
                );
            }
        }
    }

    /// A package we host but nobody serves from a repository is still fetched
    /// and verified for the user — stopping before `dpkg` costs a keystroke,
    /// stopping before the download would cost them the only verification they
    /// would ever get.
    #[test]
    fn release_packages_still_offer_the_verified_download() {
        for channel in [Channel::Apt, Channel::Dnf, Channel::Zypper] {
            assert!(
                offer(channel)
                    .choices()
                    .contains(&UpdateChoice::DownloadOnly),
                "{channel} no longer offers to fetch and verify the package"
            );
        }
    }

    /// The offer follows the plan's kind and nothing else — one mechanism per
    /// provenance class, with no machine-dependent alternates.
    #[test]
    fn the_offer_is_determined_by_the_kind_alone() {
        for (kind, expected) in [
            (UpdateKind::SelfContained, UpdateOffer::SelfContained),
            (UpdateKind::DownloadPackage, UpdateOffer::DownloadPackage),
            (UpdateKind::Delegated, UpdateOffer::RunCommand),
            (UpdateKind::Toolchain, UpdateOffer::RunCommand),
            (UpdateKind::Manual, UpdateOffer::Manual),
        ] {
            for needs_privilege in [false, true] {
                let plan = UpdatePlan {
                    kind,
                    command: Some(vec!["x".to_string()]),
                    needs_privilege,
                    human: "x".to_string(),
                };
                assert_eq!(offer_for(&plan), expected, "{kind:?}");
            }
        }
    }

    #[test]
    fn action_keys_are_stable() {
        assert_eq!(UpdateChoice::UpdateNow.action_key(), "update");
        assert_eq!(UpdateChoice::ShowCommand.action_key(), "show_command");
    }
}
