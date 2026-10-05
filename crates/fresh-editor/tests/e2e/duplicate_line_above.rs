//! Native above duplication is reachable from the command palette.
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
fn duplicate_line_above_palette_keeps_typing_on_copy() {
    let mut harness = EditorTestHarness::with_temp_project_no_plugins(80, 24).unwrap();
    harness.type_text("hello\nworld").unwrap();
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.run_palette_command("Duplicate Line Above").unwrap();
    harness.type_text("X").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    let copied = screen
        .lines()
        .position(|line| line.contains("heXllo"))
        .unwrap();
    let original = screen
        .lines()
        .position(|line| line.contains("hello"))
        .unwrap();
    let following = screen
        .lines()
        .position(|line| line.contains("world"))
        .unwrap();
    assert!(copied < original && original < following, "{screen}");
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert_eq!(
        screen.lines().filter(|line| line.contains("hello")).count(),
        1,
        "{screen}"
    );
    harness.assert_screen_contains("world");
}
