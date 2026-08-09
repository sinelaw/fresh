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

/// Whether this crate is being compiled from a cargo *registry checkout* —
/// i.e. sources cargo unpacked from crates.io, at
/// `$CARGO_HOME/registry/src/<index>/<crate>-<version>/`.
///
/// This exists because `cargo install fresh-editor` is otherwise invisible.
/// Nothing sets `FRESH_BUILD_CHANNEL` for it (the user runs cargo, not our
/// release pipeline), so provenance used to fall through to a runtime guess
/// at the executable path: `~/.cargo/bin` therefore cargo. That guess was
/// wrong the moment anyone moved, copied or symlinked the binary, and it could
/// not tell a cargo install from a file someone happened to put there.
///
/// Where the *source* came from is a fact rather than a guess, it is known at
/// the only time it is knowable, and moving the resulting binary cannot
/// invalidate it. A workspace or `--path` build is not a registry checkout and
/// correctly does not match; a git checkout does not either.
fn built_from_registry() -> bool {
    let Ok(manifest_dir) = std::env::var("CARGO_MANIFEST_DIR") else {
        return false;
    };
    is_registry_checkout(Path::new(&manifest_dir))
}
