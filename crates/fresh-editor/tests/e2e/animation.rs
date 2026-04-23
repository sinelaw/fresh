//! E2E tests for the frame-buffer animation layer.
//!
//! These exercise the Editor-level animations that run independent of
//! the plugin system (tab switches in particular). Plugin-driven
//! dashboard animations live in `e2e/plugins/dashboard.rs`.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;

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
