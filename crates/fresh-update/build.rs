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
//!  * The passthrough of `FRESH_BUILD_CHANNEL` — the *compile-time* install
//!    channel (Layer C in the provenance resolver). Source-building channels
//!    (crates.io, AUR-source, Nix, the .deb/.rpm builders) set this env var
//!    at build time; the library reads it with `option_env!`. We only need to
//!    declare the rerun dependency here so a changed value triggers a rebuild.

fn main() {
    let target = std::env::var("TARGET").unwrap_or_else(|_| "unknown".to_string());
    println!("cargo:rustc-env=FRESH_UPDATE_TARGET={target}");

    // The embedded channel is read directly via `option_env!` in lib.rs;
    // declaring the rerun-if-changed keeps rebuilds correct when a packager
    // flips the value between builds.
    println!("cargo:rerun-if-env-changed=FRESH_BUILD_CHANNEL");
    println!("cargo:rerun-if-env-changed=TARGET");
}
