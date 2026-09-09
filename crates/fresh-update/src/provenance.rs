//! Layered provenance resolution.
//!
//! Answers "how was *this* copy of `fresh` installed?" by combining, in strict
//! precedence order (highest first):
//!
//!   A. `FRESH_INSTALL_CHANNEL` runtime override   → [`Confidence::Overridden`]
//!   B. an install receipt written by the installer → [`Confidence::Authoritative`]
//!   C. the compile-time `FRESH_BUILD_CHANNEL`       → [`Confidence::Embedded`]
//!
//! …and nothing else. If none of the three says anything, the answer is
//! [`Channel::Unknown`], not a guess.
//!
//! There was a fourth layer: pattern-matching the executable's path
//! (`~/.cargo/bin` → cargo, `/opt/homebrew` → brew, `/usr/bin` on Arch → AUR).
//! It is gone. Every layer above records something at the moment it is true —
//! the installer writes the receipt, the build stamps the channel — whereas a
//! path is read long afterwards and says only where a file currently sits.
//! Copy the binary elsewhere and the guess changes; it never could separate
//! apt from dnf from a hand-dropped file. The one channel that genuinely
//! relied on it, crates.io, is now recorded at build time instead: `build.rs`
//! detects a cargo registry checkout and stamps `cargo`, which is a fact about
//! the source and survives the binary being moved.
//!
//! What is left over resolves to Unknown, which routes to the releases page
//! and says plainly that it does not know. That is worse UX than a lucky guess
//! and better behaviour than a confident wrong one, because the failure is
//! visible instead of being a command that quietly updates nothing.
//!
//! The pure core is [`resolve_from`] (takes all inputs explicitly, no
//! environment or filesystem access) so it is fully unit-testable. [`resolve`]
//! is the thin convenience wrapper that gathers the real environment.

use crate::channel::Channel;
use crate::confidence::Confidence;
use crate::receipt::{self, Hints, InstallReceipt};
use crate::registry;
use std::path::PathBuf;

/// The resolved provenance of the running binary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Provenance {
    /// The channel that installed this copy.
    pub channel: Channel,
    /// How sure we are.
    pub confidence: Confidence,
    /// Whether an external manager owns updates (do not self-swap).
    pub managed: bool,
    /// Whether `fresh update` may swap the binary in place.
    pub self_update: bool,
    /// Package name (defaults to `fresh-editor` when building commands).
    pub package_name: Option<String>,
    /// Channel-specific update hints.
    pub hints: Hints,
    /// Human-readable note on *why* we resolved this (receipt path,
    /// "embedded", the override), for `config paths` / debugging.
    pub detail: Option<String>,
}

impl Provenance {
    /// Build a provenance for `channel` at `confidence`, filling `managed` /
    /// `self_update` from the registry defaults and leaving hints empty.
    pub fn for_channel(channel: Channel, confidence: Confidence) -> Self {
        let kind = registry::kind_for(channel);
        Provenance {
            channel,
            confidence,
            managed: kind.managed(),
            self_update: kind.self_update(),
            package_name: Some("fresh-editor".to_string()),
            hints: Hints::default(),
            detail: None,
        }
    }

    /// A completely unknown provenance.
    pub fn unknown() -> Self {
        Provenance::for_channel(Channel::Unknown, Confidence::Unknown)
    }

    /// The concrete update plan for this provenance (see [`registry::plan`]).
    pub fn update_plan(&self) -> registry::UpdatePlan {
        registry::plan(self)
    }
}

/// Explicit inputs to the pure resolver. Any field may be absent.
#[derive(Debug, Default)]
pub struct ResolveInputs<'a> {
    /// `FRESH_INSTALL_CHANNEL` value, if set.
    pub override_channel: Option<String>,
    /// A receipt already located on disk, if any.
    pub receipt: Option<InstallReceipt>,
    /// `FRESH_BUILD_CHANNEL` compile-time value, if set.
    pub embedded_channel: Option<&'a str>,
}

/// The pure resolution core. Deterministic in its inputs.
pub fn resolve_from(inputs: ResolveInputs<'_>) -> Provenance {
    // Layer A — runtime override.
    if let Some(raw) = inputs.override_channel.as_deref() {
        if let Some(channel) = Channel::from_id(raw) {
            let mut p = Provenance::for_channel(channel, Confidence::Overridden);
            p.detail = Some(format!("override FRESH_INSTALL_CHANNEL={raw}"));
            return p;
        }
        tracing::warn!(value = %raw, "ignoring unrecognised FRESH_INSTALL_CHANNEL");
    }

    // Layer B — install receipt (authoritative).
    if let Some(r) = inputs.receipt {
        let channel = Channel::from_id(&r.channel).unwrap_or(Channel::Unknown);
        let kind = registry::kind_for(channel);
        return Provenance {
            channel,
            confidence: Confidence::Authoritative,
            // Receipt flags win over defaults when present.
            managed: r.managed.unwrap_or_else(|| kind.managed()),
            self_update: r.self_update.unwrap_or_else(|| kind.self_update()),
            package_name: r
                .package_name
                .clone()
                .or_else(|| Some("fresh-editor".to_string())),
            hints: r.hints,
            detail: Some("install-receipt.toml".to_string()),
        };
    }

    // Layer C — compile-time embedded channel. "prebuilt"/empty means "the
    // shared release archive", which is not itself a channel: such a build
    // could have been unpacked by any of the wrapper installers, and the one
    // that did will have left a receipt. With no receipt it is genuinely
    // unknown.
    if let Some(raw) = inputs.embedded_channel {
        if !raw.is_empty() && raw != "prebuilt" {
            if let Some(channel) = Channel::from_id(raw) {
                let mut p = Provenance::for_channel(channel, Confidence::Embedded);
                p.detail = Some(format!("embedded FRESH_BUILD_CHANNEL={raw}"));
                return p;
            }
            tracing::warn!(value = %raw, "ignoring unrecognised FRESH_BUILD_CHANNEL");
        }
    }

    Provenance::unknown()
}

/// Resolve provenance from the real runtime environment.
pub fn resolve() -> Provenance {
    let override_channel = std::env::var("FRESH_INSTALL_CHANNEL").ok();
    let exe_path = std::env::current_exe().ok();
    let data_dir = default_data_dir();

    let receipt = match (&exe_path, &data_dir) {
        (Some(exe), Some(data)) => receipt::find(exe, data).map(|(_, r)| r),
        _ => None,
    };

    resolve_from(ResolveInputs {
        override_channel,
        receipt,
        embedded_channel: embedded_channel(),
    })
}

/// The channel embedded at compile time via `FRESH_BUILD_CHANNEL`, if any.
pub fn embedded_channel() -> Option<&'static str> {
    option_env!("FRESH_BUILD_CHANNEL")
}

/// The per-user data directory where a sidecar receipt may live as a last
/// resort. Mirrors the platform conventions used elsewhere in `fresh`.
pub fn default_data_dir() -> Option<PathBuf> {
    #[cfg(target_os = "windows")]
    {
        std::env::var_os("LOCALAPPDATA").map(PathBuf::from)
    }
    #[cfg(target_os = "macos")]
    {
        std::env::var_os("HOME")
            .map(|h| PathBuf::from(h).join("Library").join("Application Support"))
    }
    #[cfg(not(any(target_os = "windows", target_os = "macos")))]
    {
        if let Some(xdg) = std::env::var_os("XDG_DATA_HOME") {
            Some(PathBuf::from(xdg))
        } else {
            std::env::var_os("HOME").map(|h| PathBuf::from(h).join(".local").join("share"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn override_wins_over_everything() {
        let mut r = InstallReceipt::new("homebrew");
        r.managed = Some(true);
        let p = resolve_from(ResolveInputs {
            override_channel: Some("flatpak".to_string()),
            receipt: Some(r),
            embedded_channel: Some("cargo"),
        });
        assert_eq!(p.channel, Channel::Flatpak);
        assert_eq!(p.confidence, Confidence::Overridden);
    }

    #[test]
    fn receipt_beats_embedded() {
        let p = resolve_from(ResolveInputs {
            override_channel: None,
            receipt: Some(InstallReceipt::new("winget")),
            embedded_channel: Some("cargo"),
        });
        assert_eq!(p.channel, Channel::Winget);
        assert_eq!(p.confidence, Confidence::Authoritative);
        assert!(p.managed);
        assert!(!p.self_update);
    }

    #[test]
    fn receipt_flags_override_registry_defaults() {
        // A tarball is SelfContained by default, but a receipt can pin it.
        let mut r = InstallReceipt::new("tarball");
        r.self_update = Some(false);
        r.managed = Some(true);
        let p = resolve_from(ResolveInputs {
            receipt: Some(r),
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Tarball);
        assert!(!p.self_update);
        assert!(p.managed);
    }

    #[test]
    fn embedded_is_used_when_there_is_no_receipt() {
        let p = resolve_from(ResolveInputs {
            embedded_channel: Some("cargo"),
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Cargo);
        assert_eq!(p.confidence, Confidence::Embedded);
    }

    /// `prebuilt` names the shared release archive, not a channel. Any wrapper
    /// could have unpacked it, and whichever did left a receipt — so with no
    /// receipt this is unknown rather than an invitation to guess.
    #[test]
    fn prebuilt_embedded_is_not_a_channel() {
        let p = resolve_from(ResolveInputs {
            embedded_channel: Some("prebuilt"),
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Unknown);
        assert_eq!(p.confidence, Confidence::Unknown);
    }

    #[test]
    fn nothing_resolves_to_unknown() {
        let p = resolve_from(ResolveInputs::default());
        assert_eq!(p.channel, Channel::Unknown);
        assert_eq!(p.confidence, Confidence::Unknown);
    }

    /// Provenance is recorded, never inferred. Nothing about the machine —
    /// where the binary sits, which distro is underneath — may influence the
    /// answer, because those are properties of the host at read time rather
    /// than facts about the install. The old path heuristic read exactly those
    /// and is gone; this pins that it stays gone.
    #[test]
    fn resolution_depends_only_on_recorded_inputs() {
        // Paths that the retired heuristic would each have claimed as a
        // different channel. The resolver cannot see them at all now.
        for _ in [
            "/home/u/.cargo/bin/fresh",
            "/opt/homebrew/bin/fresh",
            "/usr/bin/fresh",
            "/usr/local/lib/node_modules/fresh-editor/bin/fresh",
        ] {
            let p = resolve_from(ResolveInputs::default());
            assert_eq!(p.channel, Channel::Unknown);
            assert_eq!(p.confidence, Confidence::Unknown);
        }
    }

    /// An unknown install must still say something useful, and must never be
    /// talked into overwriting the binary.
    #[test]
    fn unknown_is_manual_and_never_self_swaps() {
        let p = resolve_from(ResolveInputs::default());
        assert!(!p.confidence.allows_self_swap());
        assert!(!crate::self_update::can_self_update(&p));
        assert_eq!(p.update_plan().kind, registry::UpdateKind::Manual);
    }

    /// The whole point of the receipt, over the layout a real `.deb`/`.rpm`
    /// install produces: `/usr/bin/fresh` plus
    /// `/usr/share/fresh-editor/install-receipt.toml`. This resolved to
    /// `Unknown`/`Manual` while the search order only covered `share/fresh/`:
    /// layer B missed the file and layer C is unset for these packages. There
    /// is no layer below to rescue it — which is the point. Locating the
    /// receipt is the whole mechanism.
    #[test]
    fn packaged_deb_layout_resolves_authoritatively() {
        let dir = tempfile::tempdir().unwrap();
        let usr = dir.path().join("usr");
        std::fs::create_dir_all(usr.join("bin")).unwrap();
        let exe = usr.join("bin").join("fresh");
        std::fs::write(&exe, b"not really a binary").unwrap();

        let share = usr.join("share").join(receipt::PACKAGE_DIR_NAME);
        std::fs::create_dir_all(&share).unwrap();
        std::fs::write(
            share.join(receipt::RECEIPT_FILE_NAME),
            "schema = 1\nchannel = \"apt\"\nmanaged = true\nself_update = false\n",
        )
        .unwrap();

        let found = receipt::find(&exe, Path::new("/nonexistent")).map(|(_, r)| r);
        assert!(found.is_some(), "packaged receipt not located");

        let p = resolve_from(ResolveInputs {
            receipt: found,
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Apt);
        assert_eq!(p.confidence, Confidence::Authoritative);
        assert!(p.managed);
        assert!(!p.self_update);
        // …and it gets a real update route, not the releases-page fallback.
        assert_eq!(p.update_plan().kind, registry::UpdateKind::DownloadPackage);
    }

    #[test]
    fn unparseable_override_is_ignored() {
        let p = resolve_from(ResolveInputs {
            override_channel: Some("banana".to_string()),
            embedded_channel: Some("cargo"),
            ..Default::default()
        });
        // Falls through to embedded.
        assert_eq!(p.channel, Channel::Cargo);
        assert_eq!(p.confidence, Confidence::Embedded);
    }
}
