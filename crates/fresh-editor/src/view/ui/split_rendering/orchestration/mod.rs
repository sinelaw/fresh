//! Orchestration layer.
//!
//! This subdirectory holds the code that depends on the shared
//! [`SelectionContext`](contexts::SelectionContext) and
//! [`DecorationContext`](contexts::DecorationContext) carriers. Everything
//! *outside* this directory is self-contained and has no such dependency —
//! the quarantine is intentional.

pub(super) mod contexts;
pub(super) mod overlays;
pub(super) mod render_line;
