//! E2E regression test for issue #1901.
//!
//! When the completion popup is showing (because Completion Popup Auto Show +
//! Quick Suggestions are enabled) and there are multiple cursors, typing a
//! word-character, pressing Backspace, *or accepting a completion suggestion*
//! must operate on every cursor — not just the primary one.
//!
//! Before the fix, all three of those paths bypassed the action pipeline and
//! only touched the primary cursor — secondary cursors silently drifted out
//! of sync after the popup appeared.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{
    Event, PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
};

/// Show a minimal completion popup with one filterable item so that
/// type-to-filter routes through the popup's input handler.
fn show_completion_popup(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    let items = vec![lsp_types::CompletionItem {
        label: "value".to_string(),
        kind: Some(lsp_types::CompletionItemKind::VARIABLE),
        insert_text: Some("value".to_string()),
        ..Default::default()
    }];
    harness.editor_mut().set_completion_items(items);

    harness.apply_event(Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Completion,
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "value".to_string(),
                    detail: None,
                    icon: Some("v".to_string()),
                    data: Some("value".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    })?;
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "completion popup must be visible after setup"
    );
    Ok(())
}

/// Typing a word character with a completion popup open and multiple cursors
/// must insert the character at every cursor position.
#[test]
fn test_multicursor_type_char_with_completion_popup() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    harness.type_text("aaa\nbbb\nccc")?;
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();
    assert_eq!(
        harness.editor().active_cursors().iter().count(),
        3,
        "test precondition: three cursors set up"
    );

    show_completion_popup(&mut harness)?;

    harness.send_key(KeyCode::Char('x'), KeyModifiers::NONE)?;
    harness.render()?;

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "xaaa\nxbbb\nxccc",
        "type-to-filter inside the completion popup must apply to every cursor"
    );
    Ok(())
}

/// Backspace with a completion popup open and multiple cursors must delete
/// one character behind every cursor.
#[test]
fn test_multicursor_backspace_with_completion_popup() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Place three cursors at column 0 of three lines, then advance each by
    // one character so they sit *after* the first letter. Using the existing
    // `add_cursor_below` pattern keeps cursor placement orthogonal to the
    // bug under test (`add_cursor_above` has its own column-offset quirks).
    harness.type_text("aXa\nbXb\ncXc")?;
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();
    assert_eq!(
        harness.editor().active_cursors().iter().count(),
        3,
        "test precondition: three cursors set up"
    );
    // Move every cursor past the leading letter and the 'X' so each one sits
    // right after an 'X'. MoveRight is multi-cursor aware in the normal path.
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;

    show_completion_popup(&mut harness)?;

    harness.send_key(KeyCode::Backspace, KeyModifiers::NONE)?;
    harness.render()?;

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "aa\nbb\ncc",
        "Backspace inside the completion popup must apply to every cursor"
    );
    Ok(())
}

/// Helper: show a completion popup whose accepted item replaces a `hel` prefix
/// with `hello`. The data field carries the LSP `insert_text` so PopupConfirm
/// routes through `insert_completion_text`.
fn show_hello_completion_popup(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    let items = vec![lsp_types::CompletionItem {
        label: "hello".to_string(),
        kind: Some(lsp_types::CompletionItemKind::VARIABLE),
        insert_text: Some("hello".to_string()),
        ..Default::default()
    }];
    harness.editor_mut().set_completion_items(items);

    harness.apply_event(Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Completion,
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![PopupListItemData {
                    text: "hello".to_string(),
                    detail: None,
                    icon: Some("v".to_string()),
                    data: Some("hello".to_string()),
                }],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 30,
            max_height: 10,
            bordered: true,
        },
    })?;
    harness.render()?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "completion popup must be visible after setup"
    );
    Ok(())
}

/// Accepting a completion (Tab) with multiple cursors must replace the word
/// prefix at every cursor — not just the primary one. Each cursor's own
/// prefix is replaced so cursors with different prefixes still align after
/// the swap.
#[test]
fn test_multicursor_accept_completion_with_popup() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Three lines each containing the prefix `hel`. Cursors start at the end
    // of each `hel` so the completion-accept path sees `hel` as the word to
    // replace at every cursor.
    harness.type_text("hel\nhel\nhel")?;
    harness.send_key(KeyCode::Home, KeyModifiers::CONTROL)?;
    harness.editor_mut().add_cursor_below();
    harness.editor_mut().add_cursor_below();
    assert_eq!(
        harness.editor().active_cursors().iter().count(),
        3,
        "test precondition: three cursors set up"
    );
    // Move every cursor to the end of `hel` (col 3).
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;

    show_hello_completion_popup(&mut harness)?;

    // Tab is bound to completion_accept in the Completion key context.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;
    harness.render()?;

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "hello\nhello\nhello",
        "Accepting the completion must replace the prefix at every cursor"
    );
    Ok(())
}
