//! Build script for `fresh-update`.
//!
//! Emits two compile-time values consumed by `src/lib.rs`:
//!
//!  * `FRESH_UPDATE_TARGET` — the Rust target triple this binary is built
//!    for (e.g. `x86_64-unknown-linux-gnu`). `TARGET` is only visible to
//!    build scripts, so we forward it into a `rustc-env` the library can
//!    read with `env!`. The self-update engine uses it to pick the matching
//!    release asset.
//!
//!  * `FRESH_BUILD_CHANNEL` — the *compile-time* install channel (Layer C in
//!    the provenance resolver). Source-building channels (AUR-source, Nix, the
//!    .deb/.rpm builders) set this env var explicitly and it is passed
//!    through untouched. When it is *not* set we detect one case ourselves:
//!    a build from a crates.io registry checkout. See below.

// The predicate lives in the library so its tests actually run: a
// `#[cfg(test)]` module inside a build script is never executed by
// `cargo test`, so keeping it here would have meant tests that only looked
// like coverage.
include!("src/registry_checkout.rs");

fn main() {
    let target = std::env::var("TARGET").unwrap_or_else(|_| "unknown".to_string());
    println!("cargo:rustc-env=FRESH_UPDATE_TARGET={target}");

    // An explicit value always wins: a packager who says what they are
    // building is never second-guessed.
    let channel = std::env::var("FRESH_BUILD_CHANNEL")
        .ok()
        .filter(|c| !c.is_empty())
        .or_else(|| built_from_registry().then(|| "cargo".to_string()));

    if let Some(channel) = channel {
        println!("cargo:rustc-env=FRESH_BUILD_CHANNEL={channel}");
    }

    println!("cargo:rerun-if-env-changed=FRESH_BUILD_CHANNEL");
    println!("cargo:rerun-if-env-changed=TARGET");
}

/// Whether cargo unpacked these sources from crates.io.
///
/// `cargo install fresh-editor` is otherwise invisible: the user runs cargo,
/// not our release pipeline, so nothing sets `FRESH_BUILD_CHANNEL` and there is
/// no packaging step to write a receipt. Provenance used to fall back to a
/// runtime guess at the executable path, which any move or copy invalidated.
/// Where the source came from is instead a fact, fixed at the only moment it
/// is knowable.
fn built_from_registry() -> bool {
    let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") else {
        return false;
    };
    is_registry_checkout(Path::new(&manifest_dir))
}
