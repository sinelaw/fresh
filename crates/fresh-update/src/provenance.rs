//! Layered provenance resolution.
//!
//! Answers "how was *this* copy of `fresh` installed?" by combining, in strict
//! precedence order (highest first):
//!
//!   A. `FRESH_INSTALL_CHANNEL` runtime override   → [`Confidence::Overridden`]
//!   B. an install receipt written by the installer → [`Confidence::Authoritative`]
//!   C. the compile-time `FRESH_BUILD_CHANNEL`       → [`Confidence::Embedded`]
//!   D. the executable-path heuristic                → [`Confidence::Heuristic`]
//!
//! The pure core is [`resolve_from`] (takes all inputs explicitly, no
//! environment or filesystem access) so it is fully unit-testable. [`resolve`]
//! is the thin convenience wrapper that gathers the real environment.

use crate::channel::Channel;
use crate::confidence::Confidence;
use crate::heuristic;
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
    /// Human-readable note on *why* we resolved this (receipt path, "embedded",
    /// `heuristic:<exe path>`), for `config paths` / debugging.
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
    /// Path to the running executable, for the heuristic fallback.
    pub exe_path: Option<PathBuf>,
    /// Whether the host is Arch Linux (only consulted by the heuristic).
    pub is_arch_linux: bool,
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

    // Layer C — compile-time embedded channel. "prebuilt"/empty means "no
    // specific embedded channel"; fall through to the heuristic in that case.
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

    // Layer D — executable-path heuristic (low confidence).
    if let Some(exe) = inputs.exe_path.as_deref() {
        let channel = heuristic::detect_from_path(exe, inputs.is_arch_linux);
        if channel != Channel::Unknown {
            let mut p = Provenance::for_channel(channel, Confidence::Heuristic);
            p.detail = Some(format!("heuristic:{}", exe.display()));
            return p;
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
        exe_path,
        is_arch_linux: heuristic::host_is_arch_linux(),
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

    #[test]
    fn override_wins_over_everything() {
        let mut r = InstallReceipt::new("homebrew");
        r.managed = Some(true);
        let p = resolve_from(ResolveInputs {
            override_channel: Some("flatpak".to_string()),
            receipt: Some(r),
            embedded_channel: Some("cargo"),
            exe_path: Some(PathBuf::from("/home/u/.cargo/bin/fresh")),
            is_arch_linux: false,
        });
        assert_eq!(p.channel, Channel::Flatpak);
        assert_eq!(p.confidence, Confidence::Overridden);
    }

    #[test]
    fn receipt_beats_embedded_and_heuristic() {
        let p = resolve_from(ResolveInputs {
            override_channel: None,
            receipt: Some(InstallReceipt::new("winget")),
            embedded_channel: Some("cargo"),
            exe_path: Some(PathBuf::from("/home/u/.cargo/bin/fresh")),
            is_arch_linux: false,
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
    fn embedded_beats_heuristic() {
        let p = resolve_from(ResolveInputs {
            embedded_channel: Some("cargo"),
            exe_path: Some(PathBuf::from("/opt/homebrew/bin/fresh")),
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Cargo);
        assert_eq!(p.confidence, Confidence::Embedded);
    }

    #[test]
    fn prebuilt_embedded_falls_through_to_heuristic() {
        let p = resolve_from(ResolveInputs {
            embedded_channel: Some("prebuilt"),
            exe_path: Some(PathBuf::from("/opt/homebrew/bin/fresh")),
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Homebrew);
        assert_eq!(p.confidence, Confidence::Heuristic);
    }

    #[test]
    fn heuristic_last_resort() {
        let p = resolve_from(ResolveInputs {
            exe_path: Some(PathBuf::from("/home/u/.cargo/bin/fresh")),
            ..Default::default()
        });
        assert_eq!(p.channel, Channel::Cargo);
        assert_eq!(p.confidence, Confidence::Heuristic);
    }

    #[test]
    fn nothing_resolves_to_unknown() {
        let p = resolve_from(ResolveInputs::default());
        assert_eq!(p.channel, Channel::Unknown);
        assert_eq!(p.confidence, Confidence::Unknown);
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
