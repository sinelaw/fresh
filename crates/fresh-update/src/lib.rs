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
//!   embedded → heuristic) with a [`Confidence`] level.
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
pub mod heuristic;
pub mod provenance;
pub mod receipt;
pub mod registry;
pub mod self_update;
pub mod version;

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

/// The build-time install channel embedded via `FRESH_BUILD_CHANNEL`, if any.
/// `None` for the shared prebuilt archive and ordinary developer builds.
pub fn embedded_channel() -> Option<&'static str> {
    provenance::embedded_channel()
}
