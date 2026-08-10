//! Regression tests: editing actions must respect the buffer's
//! `editing_disabled` (read-only) flag.
//!
//! Originally surfaced by Replace / Query Replace, which opened their
//! prompt and then mutated the buffer without consulting
//! `is_editing_disabled()`. Once the buffer was modified the
//! `editing_disabled` guard on undo/redo also kicked in, leaving the
//! user with a corrupted buffer they could not roll back. These tests
//! pin the contract for every entry point that takes a similar
//! shortcut around the catch-all check in `apply_action_as_events`.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::test_api::Action;

/// Mark the active buffer as read-only (sets `editing_disabled = true`).
fn make_active_buffer_read_only(harness: &mut EditorTestHarness) {
    let buffer_id = harness.editor().active_buffer();
    harness
        .editor_mut()
        .active_window_mut()
        .mark_buffer_read_only(buffer_id, true);
}

/// The "Editing disabled in this buffer" status message gets truncated
/// in the status bar at normal terminal widths, so substring matching
/// against the screen is brittle. Assert on the raw status message
/// instead.
fn assert_editing_disabled_status(harness: &EditorTestHarness) {
    let msg = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        msg.contains("Editing disabled"),
        "expected the 'Editing disabled' status to be set, got: {msg:?}"
    );
}

/// Basic `Replace` (Ctrl+R) on a read-only buffer must not mutate it
/// and must surface the "Editing disabled" status message.
#[test]
fn test_replace_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("hello world hello").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    // Trigger basic Replace via Ctrl+R.
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // The Replace prompt must not appear on a read-only buffer; the
    // editor should refuse the action and post the standard status.
    harness.assert_screen_not_contains("Replace:");
    assert_editing_disabled_status(&harness);

    // Even if the user has somehow advanced past the prompt, the
    // buffer must remain byte-identical to the original.
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "Replace on a read-only buffer must not mutate its content"
    );
}

/// `Query Replace` (Ctrl+Alt+R) on a read-only buffer must not mutate
/// it and must surface the "Editing disabled" status message.
#[test]
fn test_query_replace_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("hello world hello").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    // Trigger Query Replace via Ctrl+Alt+R.
    harness
        .send_key(
            KeyCode::Char('r'),
            KeyModifiers::CONTROL | KeyModifiers::ALT,
        )
        .unwrap();
    harness.render().unwrap();

    harness.assert_screen_not_contains("Query replace:");
    assert_editing_disabled_status(&harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "Query Replace on a read-only buffer must not mutate its content"
    );
}

/// `SortLines` is dispatched through the `apply_action_as_events`
/// catch-all. The historical `is_editing_action` allowlist did not
/// include it, so it slipped past the read-only guard. The fix moved
/// the check to the event level (Insert/Delete) so this — and any
/// future action that emits buffer mutations — is covered.
#[test]
fn test_sort_lines_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("banana\napple\ncherry").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    // Select all so SortLines has a range to operate on, then invoke it.
    harness.api_mut().dispatch(Action::SelectAll);
    harness.api_mut().dispatch(Action::SortLines);
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "SortLines on a read-only buffer must not reorder its content"
    );
    assert_editing_disabled_status(&harness);
}

/// `OpenLine` (Emacs C-o) emits an `Event::Insert` for the new line
/// ending. Same class of bug as SortLines — caught by the event-level
/// check, not the action-level allowlist.
#[test]
fn test_open_line_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("line one").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::OpenLine);
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "OpenLine on a read-only buffer must not insert a newline"
    );
}

/// `FormatBuffer` calls an external formatter and rewrites the buffer.
/// It has its own action arm in `handle_action`, so the event-level
/// check in `apply_action_as_events` does not cover it — it needs its
/// own guard.
#[test]
fn test_format_buffer_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    // Use deliberately ugly formatting so any real formatter run would
    // be detectable. We never expect the formatter to actually execute
    // here (the test asserts it was refused), so the lack of an
    // associated tool on disk is fine.
    harness.type_text("fn  main(){  }").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::FormatBuffer);
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "FormatBuffer on a read-only buffer must not rewrite its content"
    );
    assert_editing_disabled_status(&harness);
}

/// `TrimTrailingWhitespace` walks the buffer and replaces trailing
/// spaces in every line. It bypasses the event-level guard and needs
/// its own check.
#[test]
fn test_trim_trailing_whitespace_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("hello   \nworld   ").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::TrimTrailingWhitespace);
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "TrimTrailingWhitespace on a read-only buffer must not strip its content"
    );
    assert_editing_disabled_status(&harness);
}

/// `EnsureFinalNewline` appends a trailing newline if missing. Same
/// class as TrimTrailingWhitespace — needs an entry-level guard.
#[test]
fn test_ensure_final_newline_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("no trailing newline").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::EnsureFinalNewline);
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "EnsureFinalNewline on a read-only buffer must not append a newline"
    );
    assert_editing_disabled_status(&harness);
}

/// `DabbrevExpand` (Alt+/) inserts and deletes text directly via
/// `log_and_apply_event`, bypassing `apply_action_as_events` and its
/// event-level guard. Needs its own entry-level check.
#[test]
fn test_dabbrev_expand_on_readonly_buffer_does_not_modify() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    // Type a candidate word, then a space, then the prefix to expand.
    harness.type_text("helloworld hel").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::DabbrevExpand);
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "DabbrevExpand on a read-only buffer must not insert any completion"
    );
    assert_editing_disabled_status(&harness);
}

/// `ShellCommandReplace` pipes the buffer through a shell command and
/// replaces its contents with the output. It opens a prompt first, so
/// the guard goes at the prompt entry point.
#[test]
fn test_shell_command_replace_on_readonly_buffer_does_not_open_prompt() {
    let mut harness = EditorTestHarness::new(100, 30).unwrap();
    harness.type_text("some content").unwrap();
    let original = harness.get_buffer_content().unwrap();
    make_active_buffer_read_only(&mut harness);
    harness.render().unwrap();

    harness.api_mut().dispatch(Action::ShellCommandReplace);
    harness.render().unwrap();

    assert_editing_disabled_status(&harness);
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        original,
        "ShellCommandReplace on a read-only buffer must not mutate its content"
    );
}
