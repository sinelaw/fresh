//! The latest release's notes, compiled in from `CHANGELOG.md`.
//!
//! `build.rs` runs [`parse::latest_release`] over the changelog and writes
//! the result to `$OUT_DIR/release_notes.rs`, so each release's welcome
//! screen lists that release's changes with no manual step. A build without
//! the changelog (a source tree that lacks it) compiles `LATEST = None`, and
//! the welcome screen leaves its "What's new" section out.
//!
//! Plugins read it through `editor.releaseNotes()`.

// The parser is compiled into the editor only for its tests; the build
// script is its real caller.
#[cfg(test)]
mod parse;

include!(concat!(env!("OUT_DIR"), "/release_notes.rs"));

/// The latest release's version and its short change bullets, if the build
/// had a changelog to read them from.
pub fn latest() -> Option<(&'static str, &'static [&'static str])> {
    LATEST
}
