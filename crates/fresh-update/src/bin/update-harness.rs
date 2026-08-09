//! A stand-in for `fresh` that runs the real update engine against itself.
//!
//! Exists so the self-update spine can be tested end to end without building
//! the editor and without touching the network. The engine replaces
//! `current_exe()`, so the thing being updated has to be a real process on
//! disk — a unit test cannot stand in for it without either replacing the test
//! runner's own binary or refactoring the swap target to be injectable, and
//! the second option would mean the test no longer exercises the code path
//! that actually ships.
//!
//! Everything here is deliberately thin: resolve provenance from the receipt
//! beside the binary, read the endpoints from the environment, run the engine.
//! The test drives it by placing a receipt, serving a fake release, and
//! pointing `FRESH_RELEASES_URL` / `FRESH_DOWNLOAD_BASE` at a local server.
//!
//! `required-features = ["engine", "insecure-endpoints"]` keeps this out of
//! published builds: `insecure-endpoints` is what permits a plain-http local
//! endpoint, and it is never enabled for a release.

fn main() {
    // The version the harness claims to be. The test serves a feed announcing
    // something newer, which is how "fake version numbers" enter the flow —
    // no rebuild at a different version is needed to exercise an upgrade.
    let current = std::env::var("HARNESS_CURRENT_VERSION").unwrap_or_else(|_| "0.0.1".to_string());

    let opts = fresh_update::UpdateOptions {
        // Non-interactive: the test is the confirmation.
        yes: true,
        ..Default::default()
    };

    match fresh_update::engine::run(&current, &opts) {
        Ok(fresh_update::UpdateStatus::Done) => std::process::exit(0),
        Ok(fresh_update::UpdateStatus::ActionRequired) => {
            std::process::exit(fresh_update::EXIT_ACTION_REQUIRED)
        }
        Err(e) => {
            eprintln!("update failed: {e}");
            std::process::exit(1);
        }
    }
}
