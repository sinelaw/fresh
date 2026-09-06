//! `fresh-update` — deterministic install provenance and self-update for the
//! `fresh` editor.
//!
//! The problem this crate solves: at runtime, `fresh` must know **for sure**
//! how it was installed (Homebrew vs AUR vs winget vs a raw tarball vs …) so it
//! can update through the *same* mechanism instead of guessing from the
//! executable path. See `docs/internal/packaging-self-update.md` for the full
//! design.
//!
//! # Shape
//!
//! * [`Channel`] — the canonical set of distribution channels, each with a
//!   stable string id used on the wire.
//! * [`InstallReceipt`] — the `install-receipt.toml` an installer writes to
//!   record provenance authoritatively.
//! * [`Provenance`] / [`resolve`] — layered resolution (override → receipt →
//!   embedded) with a [`Confidence`] level. There is no path-guessing layer:
//!   an install that recorded nothing resolves to `Unknown`, not a guess.
//! * [`registry::plan`] — the channel → update-command table.
//! * [`self_update`] — checksum verification and the atomic in-place binary
//!   swap for self-contained channels.
//!
//! # Typical use
//!
//! ```no_run
//! let prov = fresh_update::resolve();
//! let plan = prov.update_plan();
//! if fresh_update::self_update::can_self_update(&prov) {
//!     // fetch + verify + fresh_update::self_update::atomic_replace(...)
//! } else if let Some(cmd) = &plan.command {
//!     println!("To update, run: {}", cmd.join(" "));
//! } else {
//!     println!("{}", plan.human);
//! }
//! ```

pub mod channel;
pub mod check;
pub mod confidence;
pub mod elevate;
pub mod endpoint;
pub mod feed;
pub mod provenance;
pub mod receipt;
pub mod registry;
pub mod registry_checkout;
pub mod self_update;
pub mod staging;
pub mod version;

#[cfg(feature = "archive")]
pub mod archive;
#[cfg(feature = "net")]
pub mod attestation;
#[cfg(feature = "engine")]
pub mod engine;
#[cfg(feature = "net")]
pub mod fetch;
#[cfg(feature = "net")]
pub mod net;
pub mod offer;

#[cfg(feature = "engine")]
pub use engine::{UpdateOptions, UpdateStatus};
pub use offer::{offer_for, UpdateChoice, UpdateOffer};

pub use channel::{Channel, ParseChannelError};
pub use check::{evaluate, ReleaseCheck};
pub use confidence::Confidence;
pub use provenance::{resolve, resolve_from, Provenance, ResolveInputs};
pub use receipt::{Hints, InstallReceipt};
pub use registry::{kind_for, plan, UpdateKind, UpdatePlan};

/// The target triple this build was compiled for (e.g.
/// `x86_64-unknown-linux-gnu`), captured by `build.rs`. Used to pick the
/// matching release asset during self-update.
pub const TARGET_TRIPLE: &str = env!("FRESH_UPDATE_TARGET");

/// Exit code from `fresh --cmd update` meaning: the update was **not** applied,
/// and finishing it needs a step we will not take on the user's behalf — a
/// privileged `dpkg -i`/`rpm -U`, a package-manager command we only print, or a
/// manual download.
///
/// This is a third outcome, distinct from success (`0`) and failure (`1`).
/// Nothing went wrong, so reporting it as a failure is a lie; nothing was
/// installed either, so reporting success is the opposite lie. The editor's
/// update indicator keys its `ActionRequired` state off this code.
pub const EXIT_ACTION_REQUIRED: i32 = 2;

/// The flag that turns the release-attestation check off, lowering
/// verification to the checksum sidecar alone.
///
/// Lives here rather than in [`engine`] for the same reason
/// [`EXIT_ACTION_REQUIRED`] does: it is part of the CLI's contract, and the
/// argument parser reads it in builds compiled without the engine.
pub const SKIP_ATTESTATION_FLAG: &str = "--skip-attestation";

/// The build-time install channel embedded via `FRESH_BUILD_CHANNEL`, if any.
/// `None` for the shared prebuilt archive and ordinary developer builds.
pub fn embedded_channel() -> Option<&'static str> {
    provenance::embedded_channel()
}
