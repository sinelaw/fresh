//! E2E tests for the frame-buffer animation layer.
//!
//! These exercise the Editor-level animations that run independent of
//! the plugin system (tab switches in particular). Plugin-driven
//! dashboard animations live in `e2e/plugins/dashboard.rs`.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

/// Cycling to the next tab fires a slide-in effect over the active
/// split's content area. We don't assert the direction of the slide
/// from the rendered frame (direction is a runner-internal decision
/// encoded in the effect's `from` edge); instead we wait for
/// `animations.is_active()` to flip true, which proves the Editor
/// actually kicked the animation off. Then we wait for it to settle
/// and verify the post-animation frame shows the new active buffer.
///
/// Animations are off by default in the test harness (see the comment
/// in common/harness.rs); this test opts them back on via an explicit
/// Config::default().
#[test]
fn next_buffer_kicks_off_a_slide_animation() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Config::default()).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Two files with distinctive content so the post-settle frame
    // assertion can target one or the other.
    let file_a = project_dir.join("alpha.txt");
    let file_b = project_dir.join("bravo.txt");
    std::fs::write(&file_a, "ALPHA_BUFFER_CONTENT").unwrap();
    std::fs::write(&file_b, "BRAVO_BUFFER_CONTENT").unwrap();

    harness.open_file(&file_a).unwrap();
    harness.render().unwrap();
    harness.open_file(&file_b).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("BRAVO_BUFFER_CONTENT"))
        .unwrap();

    // Baseline: no animation in flight at steady state.
    assert!(!harness.editor().animations.is_active());

    // Switch to the previous tab. The Editor should start a
    // horizontal slide (prev → from the left).
    harness.editor_mut().prev_buffer();

    // is_active flips true within a couple of ticks; wait for it
    // semantically rather than polling on a timer.
    harness
        .wait_until(|h| h.editor().animations.is_active())
        .unwrap();

    // Settle, then confirm the alpha buffer is now the active one.
    harness
        .wait_until(|h| !h.editor().animations.is_active())
        .unwrap();
    assert!(
        harness.screen_to_string().contains("ALPHA_BUFFER_CONTENT"),
        "after tab-switch animation settles, alpha buffer should be visible — screen:\n{}",
        harness.screen_to_string()
    );
}

/// Reproduces a bug reported during development: cycling from a
/// buffer-group tab (e.g. `*Git Log*`, created via createBufferGroup)
/// to a plain file buffer silently skipped the tab-switch animation.
/// The reverse direction (file → group) already animated. Root cause
/// was that `animate_tab_switch` looks up the split's content Rect in
/// cached_layout.split_areas by the OUTER split id — but when a group
/// is active, the split_areas entries are keyed by the group's INNER
/// leaf ids (each panel is its own entry), and no entry exists for
/// the outer split id. The lookup missed and the animation silently
/// returned.
///
/// This test exercises the buggy path: open a file, activate a group,
/// cycle away. Before the fix the assertion `is_active()` never flips
/// true and the test hangs (external nextest timeout surfaces the
/// regression). After the fix the animation fires as expected.
#[test]
fn tab_switch_from_group_to_file_animates() {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Drop the tiny test_buffer_groups plugin next to the test so we
    // can create a group with deterministic panels without pulling in
    // git_log (which needs a real repo).
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    const PLUGIN_SRC: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/plugins/test_buffer_groups.ts"
    ));
    fs::write(plugins_dir.join("test_buffer_groups.ts"), PLUGIN_SRC).unwrap();

    // Write a file so we have a real file buffer to cycle to.
    let file_path = project_root.join("somefile.txt");
    fs::write(&file_path, "FILE_BUFFER_CONTENT").unwrap();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Config::default(), project_root)
            .unwrap();
    harness.render().unwrap();

    // Open the file first, then the buffer-group, so `open_buffers`
    // has both targets and we can cycle between them.
    harness.open_file(&file_path).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("FILE_BUFFER_CONTENT"))
        .unwrap();

    // Trigger the group via the palette, then wait for its markers.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("TestBG: Create").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("LEFT") && s.contains("RIGHT")
        })
        .unwrap();

    // Wait for any open-time animation to settle so is_active is a
    // clean false baseline.
    harness
        .wait_until(|h| !h.editor().animations.is_active())
        .unwrap();

    // Cycle to the previous tab: group → file. Before the fix,
    // is_active stayed false forever and this wait never returned.
    harness.editor_mut().prev_buffer();
    harness
        .wait_until(|h| h.editor().animations.is_active())
        .unwrap();

    harness
        .wait_until(|h| !h.editor().animations.is_active())
        .unwrap();
    assert!(
        harness.screen_to_string().contains("FILE_BUFFER_CONTENT"),
        "after tab-switch animation settles, file buffer should be visible — screen:\n{}",
        harness.screen_to_string()
    );
}

/// Reproducer for the "stuck mid-slide" bug: rapidly cycling
/// buffers kicks a new slide while the previous one is still in
/// flight. Without a replacement rule the new effect snapshots
/// the old effect's mid-slide pixels as its "after" frame, and
/// once both finish the buffer ends up frozen at an intermediate
/// state (half the old content, half blank). The assert on the
/// final screen catches that — after all animations settle, the
/// target buffer's content must be fully visible.
#[test]
fn rapid_tab_switches_settle_on_target_content() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Config::default()).unwrap();
    let project_dir = harness.project_dir().unwrap();

    // Three files so we can bounce between them multiple times and
    // reliably land back on a predictable one at the end.
    let file_a = project_dir.join("alpha.txt");
    let file_b = project_dir.join("bravo.txt");
    let file_c = project_dir.join("charlie.txt");
    std::fs::write(&file_a, "ALPHA_BUFFER_CONTENT").unwrap();
    std::fs::write(&file_b, "BRAVO_BUFFER_CONTENT").unwrap();
    std::fs::write(&file_c, "CHARLIE_BUFFER_CONTENT").unwrap();

    harness.open_file(&file_a).unwrap();
    harness.open_file(&file_b).unwrap();
    harness.open_file(&file_c).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CHARLIE_BUFFER_CONTENT"))
        .unwrap();
    // Let the post-open animation settle so the rapid-switch
    // sequence starts from a clean baseline.
    harness
        .wait_until(|h| !h.editor().animations.is_active())
        .unwrap();

    // Fire four switches back-to-back without waiting for any to
    // settle. Net motion lands on charlie: prev/prev/next/next from
    // charlie → bravo → alpha → bravo → charlie.
    harness.editor_mut().prev_buffer();
    harness.editor_mut().prev_buffer();
    harness.editor_mut().next_buffer();
    harness.editor_mut().next_buffer();

    // Wait for everything to settle, then confirm the target is the
    // only buffer content visible on screen.
    harness
        .wait_until(|h| !h.editor().animations.is_active())
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("CHARLIE_BUFFER_CONTENT"),
        "after rapid switches settle, charlie should be visible — screen:\n{}",
        screen
    );
    // No residue from the bouncing switches should remain.
    assert!(
        !screen.contains("ALPHA_BUFFER_CONTENT"),
        "alpha must not linger after the animations finish — screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("BRAVO_BUFFER_CONTENT"),
        "bravo must not linger after the animations finish — screen:\n{}",
        screen
    );
}
