// Test the EditorTestHarness itself

mod common;

use common::harness::EditorTestHarness;

#[test]
fn test_harness_creation() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    assert!(!harness.should_quit());
}

#[test]
fn test_harness_render() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(!screen.is_empty());
}

#[test]
fn test_buffer_content() {
    let harness = EditorTestHarness::new(80, 24).unwrap();
    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, ""); // New buffer is empty
}

#[test]
fn test_screen_contains() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();

    // Should show status bar with "[No Name]"
    harness.assert_screen_contains("[No Name]");
}

// ─────────────────────────────────────────────────────────────────────────
// Phase 1 smoke: EditorTestApi seam.
//
// Verifies that the test_api module wires through to a working Editor
// without going through send_key / render. Not a feature test — just
// proof that the seam compiles and runs. The PoC theorem tests (Phase 2)
// will live under tests/semantic/ and use the same surface.
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn test_api_smoke_dispatch_and_observe() {
    use fresh::input::keybindings::Action;

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let _fix = harness.load_buffer_from_text("hello world").unwrap();

    let api = harness.api_mut();

    // Initial caret is at byte 0 with no selection.
    assert_eq!(api.primary_caret().position, 0);
    assert_eq!(api.primary_caret().anchor, None);
    assert_eq!(api.buffer_text(), "hello world");

    // Move 5 right with selection (Shift+Right ×5) using semantic actions.
    api.dispatch_seq(&[
        Action::SelectRight,
        Action::SelectRight,
        Action::SelectRight,
        Action::SelectRight,
        Action::SelectRight,
    ]);

    let primary = api.primary_caret();
    assert_eq!(primary.position, 5);
    assert_eq!(primary.anchor, Some(0));
    assert_eq!(api.selection_text(), "hello");

    // Apply ToUpperCase.
    api.dispatch(Action::ToUpperCase);
    assert_eq!(api.buffer_text(), "HELLO world");
}
