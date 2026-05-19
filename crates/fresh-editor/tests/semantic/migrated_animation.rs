//! Migration of `tests/e2e/animation.rs` — Editor-level animations
//! (tab-switch slide effects, cursor-jump trail) that run independent
//! of the plugin system.
//!
//! Load-bearing claims preserved here:
//!
//!   1. **Tab switch kicks off a slide animation.** `prev_buffer()`
//!      (or `next_buffer()`) must increment the per-window
//!      `animations.total_started()` counter, and the target
//!      buffer's content must be visible after everything settles.
//!
//!   2. **Tab switch from a buffer-group to a plain file animates.**
//!      Regression for the `animate_tab_switch` lookup that missed
//!      when the active split was a buffer-group (outer split id
//!      vs. inner panel ids). Before the fix the animation never
//!      fired and `wait_until` hung.
//!
//!   3. **Rapid switches settle on the target content.** Stacking
//!      multiple slide kick-offs while the previous one is still in
//!      flight must NOT freeze the buffer at an intermediate state.
//!      After everything settles, only the target buffer's marker
//!      should appear on screen.
//!
//!   4. **`cursor_jump_animation = true` fires for long Ctrl+End
//!      jumps.** The dedicated toggle gates the cursor-jump trail
//!      effect for long vertical moves (dy > 2).
//!
//!   5. **`cursor_jump_animation = false` suppresses the trail.**
//!      Even with master `editor.animations = true`, the dedicated
//!      toggle being off must prevent any cursor-jump effect from
//!      starting.
//!
//! ## Harness-direct pattern
//!
//! The animation machinery (`active_window().animations.total_started()`,
//! `is_active()`) and `prev_buffer()`/`next_buffer()` live on
//! `Editor` directly with no `EditorTestApi` projection — they're
//! per-window runner state, not abstract editor observables. These
//! tests therefore use the harness-direct pattern (same pattern
//! `migrated_redraw_screen.rs` uses for the full-redraw flag).
//!
//! Animations are off by default in the test harness (see the comment
//! in `common/harness.rs`); these tests opt them back on via an
//! explicit `Config::default()` (which enables `editor.animations`)
//! or an explicit `editor.animations = true`.
//!
//! Source: `tests/e2e/animation.rs` (5 tests migrated; no tests
//! deferred). The `tab_switch_from_group_to_file_animates` test
//! depends on the `test_buffer_groups.ts` plugin and runs only when
//! the `plugins` feature is enabled, matching the e2e routing.

use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Cycling to the next/prev tab fires a slide-in effect over the
/// active split's content area. We assert via the monotonic
/// `total_started()` counter so a delayed poll under CI load can't
/// straddle the entire ~260 ms animation window and miss the flip.
#[test]
fn migrated_next_buffer_kicks_off_a_slide_animation() {
    // Original: `next_buffer_kicks_off_a_slide_animation`.
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
    // Baseline: any open-time animation has settled.
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    let baseline = harness
        .editor()
        .active_window()
        .animations
        .total_started();

    // Switch to the previous tab. The Editor should start a
    // horizontal slide (prev → from the left).
    harness.editor_mut().prev_buffer();

    // Monotonic counter catches the kick-off even if the animation
    // already finished by the time we poll.
    harness
        .wait_until(|h| h.editor().active_window().animations.total_started() > baseline)
        .unwrap();

    // Settle, then confirm the alpha buffer is now active.
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    assert!(
        harness
            .screen_to_string()
            .contains("ALPHA_BUFFER_CONTENT"),
        "after tab-switch animation settles, alpha buffer should be \
         visible — screen:\n{}",
        harness.screen_to_string()
    );
}

/// Reproducer for "stuck mid-slide": rapidly cycling buffers kicks a
/// new slide while the previous is still in flight. Without a
/// replacement rule the new effect snapshots the old effect's
/// mid-slide pixels as its "after" frame, and once both finish the
/// buffer ends up frozen at an intermediate state. The assert on the
/// final screen catches that — after all animations settle, the
/// target buffer's content must be fully visible.
#[test]
fn migrated_rapid_tab_switches_settle_on_target_content() {
    // Original: `rapid_tab_switches_settle_on_target_content`.
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
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();

    // Fire four switches back-to-back without waiting for any to
    // settle. Net motion lands on charlie:
    // prev/prev/next/next from charlie → bravo → alpha → bravo → charlie.
    harness.editor_mut().prev_buffer();
    harness.editor_mut().prev_buffer();
    harness.editor_mut().next_buffer();
    harness.editor_mut().next_buffer();

    // Wait for everything to settle, then confirm the target is the
    // only buffer content visible.
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("CHARLIE_BUFFER_CONTENT"),
        "after rapid switches settle, charlie should be visible — \
         screen:\n{}",
        screen
    );
    // No residue from the bouncing switches should remain.
    assert!(
        !screen.contains("ALPHA_BUFFER_CONTENT"),
        "alpha must not linger after the animations finish — \
         screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("BRAVO_BUFFER_CONTENT"),
        "bravo must not linger after the animations finish — \
         screen:\n{}",
        screen
    );
}

/// Reproducer for the buffer-group → file animation skip: opening a
/// file, activating a buffer-group, then cycling away to the file
/// must animate. Before the fix `animate_tab_switch` keyed the
/// split's content Rect lookup by the OUTER split id, but the group
/// stores entries under its INNER leaf ids, so the lookup missed and
/// the animation silently returned. Without the fix, `total_started`
/// never increments and `wait_until` hangs.
#[cfg(feature = "plugins")]
#[test]
fn migrated_tab_switch_from_group_to_file_animates() {
    // Original: `tab_switch_from_group_to_file_animates`.
    use crate::common::harness::copy_plugin_lib;
    use std::fs;

    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Drop the tiny test_buffer_groups plugin next to the test so we
    // can create a group with deterministic panels without pulling
    // in git_log (which needs a real repo).
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
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    let baseline = harness
        .editor()
        .active_window()
        .animations
        .total_started();

    // Cycle to the previous tab: group → file. Before the fix,
    // total_started never incremented and the wait never returned.
    harness.editor_mut().prev_buffer();
    harness
        .wait_until(|h| h.editor().active_window().animations.total_started() > baseline)
        .unwrap();

    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    assert!(
        harness
            .screen_to_string()
            .contains("FILE_BUFFER_CONTENT"),
        "after tab-switch animation settles, file buffer should be \
         visible — screen:\n{}",
        harness.screen_to_string()
    );
}

/// Drive a long vertical cursor move (`Ctrl+End`, dy ≫ 2 — well past
/// the cursor-jump threshold) and return how many animations started.
/// Used by the two toggle-gated tests below.
fn cursor_jump_long_move_test(cursor_jump_enabled: bool) -> u64 {
    let mut config = Config::default();
    config.editor.animations = true;
    config.editor.cursor_jump_animation = cursor_jump_enabled;

    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.new_buffer().unwrap();

    // Twenty short lines: enough that Ctrl+End jumps the cursor far
    // past the dy > 2 threshold for the cursor-jump animation.
    for i in 1..=20 {
        harness.type_text(&format!("line {}", i)).unwrap();
        if i < 20 {
            harness
                .send_key(KeyCode::Enter, KeyModifiers::empty())
                .unwrap();
        }
    }
    // Park the cursor at the top so the next jump is unambiguous.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();

    let baseline = harness
        .editor()
        .active_window()
        .animations
        .total_started();

    // Long jump: top → end of buffer.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Give the runner one extra render tick so the jump observed
    // in this frame can call `start()` if it's going to.
    harness.render().unwrap();

    harness.editor().active_window().animations.total_started() - baseline
}

#[test]
fn migrated_cursor_jump_animation_runs_when_toggle_enabled() {
    // Original: `cursor_jump_animation_runs_when_toggle_enabled`.
    let started = cursor_jump_long_move_test(true);
    assert!(
        started >= 1,
        "long Ctrl+End jump should kick off a cursor-jump effect \
         when editor.cursor_jump_animation = true (started: {})",
        started,
    );
}

#[test]
fn migrated_cursor_jump_animation_suppressed_when_toggle_disabled() {
    // Original: `cursor_jump_animation_suppressed_when_toggle_disabled`.
    let started = cursor_jump_long_move_test(false);
    assert_eq!(
        started, 0,
        "long Ctrl+End jump must not start any animation when \
         editor.cursor_jump_animation = false even though \
         editor.animations = true",
    );
}

/// Anti-test: drop the `prev_buffer()` call. Without the tab switch,
/// no slide animation can be kicked off — `total_started()` must
/// stay at the baseline, proving the positive
/// `migrated_next_buffer_kicks_off_a_slide_animation` claim is gated
/// on the actual `prev_buffer()` invocation, not on incidental
/// harness or open-time animation activity.
#[test]
fn anti_no_tab_switch_means_total_started_stays_at_baseline() {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(100, 24, Config::default()).unwrap();
    let project_dir = harness.project_dir().unwrap();
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
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();
    let baseline = harness
        .editor()
        .active_window()
        .animations
        .total_started();

    // No prev_buffer() / next_buffer() here — that's the
    // load-bearing step we drop.
    harness.render().unwrap();
    harness.render().unwrap();

    let after = harness
        .editor()
        .active_window()
        .animations
        .total_started();
    assert_eq!(
        after, baseline,
        "anti: without a prev_buffer()/next_buffer() call, \
         animations.total_started must not advance \
         (baseline={baseline}, after={after}). The positive \
         tab-switch test depends on that call actually happening."
    );
}

/// Anti-test: drop the long `Ctrl+End` jump from the cursor-jump
/// path. With the cursor still parked at the top and only a
/// settle-wait afterward, `total_started()` must not advance — proves
/// the positive `migrated_cursor_jump_animation_runs_when_toggle_enabled`
/// claim is gated on the actual long jump, not on harness init or
/// `Ctrl+Home` parking incidentally bumping the counter.
#[test]
fn anti_cursor_jump_without_long_move_does_not_advance_counter() {
    let mut config = Config::default();
    config.editor.animations = true;
    config.editor.cursor_jump_animation = true;

    let mut harness = EditorTestHarness::with_config(80, 30, config).unwrap();
    harness.new_buffer().unwrap();

    for i in 1..=20 {
        harness.type_text(&format!("line {}", i)).unwrap();
        if i < 20 {
            harness
                .send_key(KeyCode::Enter, KeyModifiers::empty())
                .unwrap();
        }
    }
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| !h.editor().active_window().animations.is_active())
        .unwrap();

    let baseline = harness
        .editor()
        .active_window()
        .animations
        .total_started();

    // No Ctrl+End jump here — only two render ticks.
    harness.render().unwrap();
    harness.render().unwrap();

    let after = harness
        .editor()
        .active_window()
        .animations
        .total_started();
    assert_eq!(
        after, baseline,
        "anti: without the long Ctrl+End jump, the cursor-jump \
         animation must not start (baseline={baseline}, \
         after={after}). The positive cursor-jump test depends on \
         that long move actually happening."
    );
}
