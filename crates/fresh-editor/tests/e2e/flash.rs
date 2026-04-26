//! End-to-end tests for the `flash` plugin (label-based jump
//! navigation).  The plugin lives at
//! `crates/fresh-editor/plugins/flash.ts` and these tests load it via
//! the same `copy_plugin` mechanism vi_mode tests use, so they
//! exercise the actual production plugin source.
//!
//! These are also the production-code regression test for plugin
//! API #1 (`editor.getNextKey()`) when used by a plugin that does
//! NOT also use `defineMode` bindings.

use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::input::keybindings::Action::PluginAction;
use std::fs;

/// Build a harness with the `flash` plugin loaded into an isolated
/// per-test project directory.
fn flash_harness(width: u16, height: u16) -> (EditorTestHarness, tempfile::TempDir) {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "flash");
    copy_plugin_lib(&plugins_dir);

    let harness = EditorTestHarness::with_config_and_working_dir(
        width,
        height,
        Default::default(),
        project_root,
    )
    .unwrap();
    (harness, temp_dir)
}

/// Open the command palette, type `Flash: Jump`, press Enter, and
/// wait for the plugin's mode + status to be set.  Mirrors
/// `enable_vi_mode` in vi_mode tests.
fn arm_flash(harness: &mut EditorTestHarness) {
    // Wait for the plugin's command to be registered.
    harness
        .wait_until(|h| {
            let commands = h.editor().command_registry().read().unwrap().get_all();
            commands
                .iter()
                .any(|c| c.action == PluginAction("flash_jump".to_string()))
        })
        .unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Flash: Jump").unwrap();
    harness.wait_for_screen_contains("Flash: Jump").unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the plugin to enter flash mode AND post its initial
    // empty-pattern status.  Both signals together prove the plugin
    // has armed its first `getNextKey` and is ready for the next key.
    harness
        .wait_until(|h| {
            h.editor().editor_mode() == Some("flash".to_string())
                && h.screen_to_string().contains("Flash[]")
        })
        .unwrap();
}

/// Type a pattern one character at a time, waiting after each char
/// for the plugin to acknowledge by updating its status banner to
/// `Flash[<pattern-so-far>]`.  This avoids the natural race between
/// the harness's synchronous key dispatch and the plugin's async
/// `getNextKey` re-arm — without it, fast `type_text` can outrun the
/// plugin and chars 2+ fall through into the buffer.
fn type_pattern(harness: &mut EditorTestHarness, pattern: &str) {
    let mut so_far = String::new();
    for c in pattern.chars() {
        so_far.push(c);
        let needle = format!("Flash[{}]", so_far);
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains(&needle))
            .unwrap();
    }
}

#[test]
fn flash_jumps_to_label() {
    // Three "hello" lines; cursor at byte 0.  Distances 0/12/24
    // → labels a/s/d in distance order.  Pressing 's' jumps to
    // byte 12 (start of "hello there").
    let (mut harness, _temp) = flash_harness(120, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nhello there\nhello again\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    let initial = harness.cursor_position();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "hello");
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.editor().editor_mode() != Some("flash".to_string()))
        .unwrap();

    let landed = harness.cursor_position();
    assert_ne!(landed, initial, "cursor should have moved");
    assert_eq!(
        landed, 12,
        "expected cursor at start of second match (byte 12), got {}",
        landed,
    );
}

#[test]
fn flash_escape_cancels_no_movement() {
    let (mut harness, _temp) = flash_harness(120, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nhello there\nhello again\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    let initial = harness.cursor_position();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "hello");
    harness.render().unwrap();

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    harness
        .wait_until(|h| h.editor().editor_mode() != Some("flash".to_string()))
        .unwrap();

    assert_eq!(
        harness.cursor_position(),
        initial,
        "Escape must not move the cursor",
    );
}

#[test]
fn flash_backspace_shrinks_pattern() {
    // After Backspace the prior label set should be re-assigned.
    // Verify by typing a too-narrow pattern first ("there"), then
    // Backspacing back to a multi-match prefix and pressing a label.
    let (mut harness, _temp) = flash_harness(120, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nhello there\nhello again\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "there"); // 1 match (line 2)
    for n in (0..5).rev() {
        let needle = format!("Flash[{}]", &"there"[..n]);
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
        harness
            .wait_until(|h| h.screen_to_string().contains(&needle))
            .unwrap();
    }
    type_pattern(&mut harness, "hello"); // 3 matches again
    harness.render().unwrap();

    // Press the label for the third (farthest) match.
    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.editor().editor_mode() != Some("flash".to_string()))
        .unwrap();

    assert_eq!(
        harness.cursor_position(),
        24,
        "after backspace+retype, label 'd' must reach line 3 (byte 24)",
    );
}

#[test]
fn flash_enter_jumps_to_closest() {
    let (mut harness, _temp) = flash_harness(120, 24);
    let fixture = TestFixture::new("test.txt", "hello world\nhello there\nhello again\n").unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    arm_flash(&mut harness);
    type_pattern(&mut harness, "hello");
    harness.render().unwrap();

    // Closest match is at cursor position (byte 0); Enter should
    // exit cleanly without changing position.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| h.editor().editor_mode() != Some("flash".to_string()))
        .unwrap();

    assert_eq!(harness.cursor_position(), 0);
}
