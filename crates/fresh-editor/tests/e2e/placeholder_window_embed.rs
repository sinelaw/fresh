//! Issue #1971: a `windowEmbed` naming a window that does not exist froze
//! the panel it lived in.
//!
//! `WidgetSpec::WindowEmbed::window_id` is a `u32`, and its own contract
//! says an unknown id renders placeholder blanks. But a plugin whose row
//! has no window *yet* — the orchestrator gives a workspace being created
//! a synthetic id below every real one — sent a value that could not
//! deserialise at all, and serde fails the **whole spec**: the host logged
//! `updateFloatingWidget: invalid spec: invalid value: integer -1000000,
//! expected u32` and dropped the update. The panel then kept painting the
//! last spec that *had* parsed. In the orchestrator's picker that was the
//! "Archiving… / Waiting for git…" card, still up long after the worktree
//! had moved to `.archived/` and the manifest had been written, and the
//! picker took no further keys.
//!
//! `windowEmbed()` now normalises an id that names no window to 0 — the
//! value the widget already documents for exactly this state — so the
//! placeholder renders as blanks and the panel keeps updating.
//!
//! Per CONTRIBUTING.md §2 the assertion is on rendered output only: mount
//! a panel carrying a placeholder embed and marker V1, replace it with
//! V2, and require V2 on screen. Before the fix neither spec crossed the
//! boundary, so neither marker ever appeared.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

const MARKER_V1: &str = "PLACEHOLDER-EMBED-V1";
const MARKER_V2: &str = "PLACEHOLDER-EMBED-V2";

fn install_plugin(project_root: &std::path::Path) {
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("create plugins dir");
    copy_plugin_lib(&plugins_dir);

    const SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_placeholder_embed.ts"
    ));
    let dst = plugins_dir.join("test_placeholder_embed.ts");
    fs::write(&dst, SRC)
        .unwrap_or_else(|e| panic!("write test_placeholder_embed.ts to {dst:?}: {e}"));
}

fn run_command_and_wait(harness: &mut EditorTestHarness, name: &str, ack: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(ack))
        .unwrap();
}

#[test]
fn a_placeholder_window_embed_does_not_freeze_its_panel() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    install_plugin(&project_root);

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    run_command_and_wait(&mut harness, "TestEmbed: Mount", "TestEmbed: MOUNTED");
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(MARKER_V1))
        .unwrap();

    // The update is the half that froze: with the embed's id rejected, the
    // host dropped this spec and left V1 on screen indefinitely.
    run_command_and_wait(&mut harness, "TestEmbed: Update", "TestEmbed: UPDATED");
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(MARKER_V2))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(MARKER_V2),
        "issue #1971: an embed naming no window must not stop the panel \
         from taking its next spec — screen was:\n{screen}"
    );
    assert!(
        !screen.contains(MARKER_V1),
        "the replaced spec must be gone, not painted under the new one \
         — screen was:\n{screen}"
    );
}
