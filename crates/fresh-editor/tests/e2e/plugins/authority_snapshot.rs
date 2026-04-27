//! Regression test for the devcontainer-restart re-prompt bug.
//!
//! Repro: after the user picks "Attach" in the devcontainer popup, the
//! plugin calls `setAuthority`, which triggers the editor-restart flow.
//! `main.rs` constructs a fresh `Editor` (with `Authority::local()` and
//! empty `display_label`), then calls `set_boot_authority(real)` and
//! finally fires the `plugins_loaded` hook so plugins can finish
//! initializing.
//!
//! The devcontainer plugin's "am I already attached?" check reads
//! `editor.getAuthorityLabel()`, which is served from the plugin state
//! snapshot. Before the fix, `set_boot_authority` didn't refresh that
//! snapshot, so the plugin saw the stale empty label that the
//! `Authority::local()` construction-time default seeded, and re-showed
//! the attach popup — even though the status bar correctly reflected the
//! new container authority.
//!
//! This test pins the invariant the plugin relies on: **after
//! `set_boot_authority`, a read from the plugin state snapshot returns
//! the real authority's `display_label`, not the construction-time
//! default.** That's sufficient — the JS plugin end of the flow is
//! straightforward string propagation.

use crate::common::harness::EditorTestHarness;
use fresh::services::authority::{
    Authority, AuthorityPayload, FilesystemSpec, SpawnerSpec, TerminalWrapperSpec,
};

fn devcontainer_authority_payload(label: &str) -> AuthorityPayload {
    // Filesystem/spawner kinds match the minimal local-fs + host-spawner
    // combination so the harness doesn't need docker; what matters for
    // the snapshot propagation test is the `display_label` round-trip.
    AuthorityPayload {
        filesystem: FilesystemSpec::Local,
        spawner: SpawnerSpec::Local,
        terminal_wrapper: TerminalWrapperSpec::HostShell,
        display_label: label.to_string(),
        path_translation: None,
    }
}

/// Directly observes the plugin state snapshot the plugin runtime reads
/// from. A failing assertion here means `editor.getAuthorityLabel()`
/// returns "" from JS — which is what makes the devcontainer popup come
/// back after restart.
#[test]
fn set_boot_authority_refreshes_plugin_state_snapshot() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    // Sanity: a freshly-constructed Editor has `Authority::local()` with
    // an empty label, and that is what the snapshot carries.
    let snapshot_handle = harness
        .editor()
        .plugin_manager()
        .state_snapshot_handle()
        .expect("plugins enabled in the default test harness");
    let initial = snapshot_handle.read().unwrap().authority_label.clone();
    assert_eq!(
        initial, "",
        "Construction seeds an empty authority_label in the plugin snapshot"
    );

    // Install the container authority the same way main.rs does after
    // editor construction during the post-attach restart.
    let authority =
        Authority::from_plugin_payload(devcontainer_authority_payload("Container:deadbeef"))
            .unwrap();
    harness.editor_mut().set_boot_authority(authority);

    // The plugin state snapshot must now reflect the container
    // authority. Before the fix this stayed empty and the devcontainer
    // plugin's `getAuthorityLabel()` check on `plugins_loaded` still
    // read "", re-triggering the attach popup.
    let after = snapshot_handle.read().unwrap().authority_label.clone();
    assert_eq!(
        after, "Container:deadbeef",
        "set_boot_authority must refresh the plugin state snapshot so \
         `plugins_loaded` hook handlers read the installed label"
    );
}

/// Guard the other side of the same invariant: after `clear_authority`
/// (the `devcontainer_detach` path) flows through, the snapshot returns
/// to the empty label so "am I already attached?" goes back to "no".
#[test]
fn set_boot_authority_back_to_local_clears_plugin_state_snapshot_label() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let snapshot_handle = harness
        .editor()
        .plugin_manager()
        .state_snapshot_handle()
        .expect("plugins enabled in the default test harness");

    // Swap to a container authority, then back to local. Each transition
    // must push through the snapshot, otherwise the plugin's decision
    // will lag by one restart.
    let container =
        Authority::from_plugin_payload(devcontainer_authority_payload("Container:feedface"))
            .unwrap();
    harness.editor_mut().set_boot_authority(container);
    assert_eq!(
        snapshot_handle.read().unwrap().authority_label,
        "Container:feedface"
    );

    harness.editor_mut().set_boot_authority(Authority::local());
    assert_eq!(
        snapshot_handle.read().unwrap().authority_label,
        "",
        "Swapping back to local must clear the label in the snapshot"
    );
}
