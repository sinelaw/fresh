//! Tests for the "Suspend Process" action (issue #1316).
//!
//! The action exposes Unix job-control suspend (SIGTSTP) so users can bind a
//! key and drop back to their shell with `fg` to resume. The actual signal
//! raise happens in the outer event loop (main.rs), so here we only verify
//! that dispatching the action arms the editor-side flag and that the
//! command is discoverable from the command palette.

use crate::common::harness::EditorTestHarness;
use fresh::input::keybindings::Action;

/// Dispatching `Action::SuspendProcess` must set the editor's suspend-requested
/// flag so the event loop can tear down the TUI and raise SIGTSTP.
#[test]
fn test_suspend_process_action_requests_suspend() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();

    assert!(
        !harness.editor_mut().take_suspend_request(),
        "fresh editor should not have a pending suspend request"
    );

    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::SuspendProcess);

    assert!(
        harness.editor_mut().take_suspend_request(),
        "SuspendProcess action should arm the suspend-requested flag"
    );

    // The flag is consumed on read — a second read must return false.
    assert!(
        !harness.editor_mut().take_suspend_request(),
        "suspend request flag must be one-shot"
    );
}

/// The "Suspend Process" entry must be discoverable from the command palette
/// so users can bind a key (Ctrl+Z is already undo, so this is deliberately
/// keybind-only by default).
#[test]
fn test_suspend_process_visible_in_command_palette() {
    use crossterm::event::{KeyCode, KeyModifiers};
    let mut harness = EditorTestHarness::new(100, 24).unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("suspend").unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Suspend Process");
}
