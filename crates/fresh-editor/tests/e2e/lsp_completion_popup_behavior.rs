//! E2E tests for LSP completion popup behavior issues.
//!
//! These tests reproduce issues where the completion popup incorrectly handles
//! various key inputs: swallowing arrow keys, treating non-word characters as
//! type-to-filter, consuming Ctrl+key combos, etc.
//!
//! See LSP_COMPLETION_ISSUES.md for the full list of issues.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{
    Event, PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
};

/// Helper: set up an editor with a completion popup showing three "calculate_*" items.
/// The buffer will contain the given `prefix` text, and the popup will be configured
/// with the matching LSP completion items for type-to-filter support.
fn setup_completion_popup(prefix: &str) -> anyhow::Result<EditorTestHarness> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type the initial prefix
    harness.type_text(prefix)?;
    harness.render()?;

    // Set up LSP completion items for re-filtering
    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "calculate_difference".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            detail: Some("fn calculate_difference(a: i32, b: i32) -> i32".to_string()),
            insert_text: Some("calculate_difference".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "calculate_product".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            detail: Some("fn calculate_product(a: i32, b: i32) -> i32".to_string()),
            insert_text: Some("calculate_product".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "calculate_sum".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            detail: Some("fn calculate_sum(a: i32, b: i32) -> i32".to_string()),
            insert_text: Some("calculate_sum".to_string()),
            ..Default::default()
        },
    ];
    harness.editor_mut().set_completion_items(completion_items);

    // Show completion popup
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Completion,
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "calculate_difference".to_string(),
                        detail: Some("fn calculate_difference(a: i32, b: i32) -> i32".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("calculate_difference".to_string()),
                    },
                    PopupListItemData {
                        text: "calculate_product".to_string(),
                        detail: Some("fn calculate_product(a: i32, b: i32) -> i32".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("calculate_product".to_string()),
                    },
                    PopupListItemData {
                        text: "calculate_sum".to_string(),
                        detail: Some("fn calculate_sum(a: i32, b: i32) -> i32".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("calculate_sum".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });

    harness.render()?;

    // Verify popup is visible
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Completion popup should be visible after setup"
    );

    Ok(harness)
}

// ============================================================================
// Issue 1: Left/Right arrow keys are swallowed
// ============================================================================

/// Left arrow should close the completion popup and move the cursor left.
/// Currently the key is silently consumed (swallowed).
#[test]
fn test_completion_left_arrow_closes_popup() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    let buffer_before = harness.get_buffer_content().unwrap();
    assert_eq!(buffer_before, "calc");

    // Press Left arrow
    harness.send_key(KeyCode::Left, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Left arrow should close the completion popup"
    );

    // Buffer should be unchanged
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calc", "Left arrow should not modify the buffer");

    Ok(())
}

/// Right arrow should close the completion popup and move the cursor right.
/// Currently the key is silently consumed (swallowed).
#[test]
fn test_completion_right_arrow_closes_popup() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press Right arrow
    harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Right arrow should close the completion popup"
    );

    // Buffer should be unchanged
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calc", "Right arrow should not modify the buffer");

    Ok(())
}

// ============================================================================
// Issue 2: Space is treated as type-to-filter instead of closing the popup
// ============================================================================

/// Space should close the completion popup and insert a space character.
/// Currently space is treated as a type-to-filter character, keeping the popup
/// open and resetting the filter prefix.
#[test]
fn test_completion_space_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press Space
    harness.send_key(KeyCode::Char(' '), KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Space should close the completion popup"
    );

    // Space should be inserted into the buffer
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "calc ",
        "Space should be inserted after the typed prefix"
    );

    Ok(())
}

// ============================================================================
// Issue 3: Non-word characters treated as type-to-filter
// ============================================================================

/// Semicolon should close the completion popup and insert ';'.
/// Currently it's treated as a type-to-filter character.
#[test]
fn test_completion_semicolon_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press semicolon
    harness.send_key(KeyCode::Char(';'), KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Semicolon should close the completion popup"
    );

    // Semicolon should be inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "calc;",
        "Semicolon should be inserted into the buffer"
    );

    Ok(())
}

/// Open parenthesis should close the completion popup and insert '('.
/// Currently it's treated as a type-to-filter character.
/// Note: Ideally '(' should accept the completion and insert '(' (commit character behavior),
/// but at minimum it should not leave the popup open with a corrupted filter state.
#[test]
fn test_completion_open_paren_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press open paren
    harness.send_key(KeyCode::Char('('), KeyModifiers::SHIFT)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Open parenthesis should close the completion popup"
    );

    // The character should be inserted into the buffer
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains('('),
        "Open parenthesis should be inserted into the buffer, got: {buffer}"
    );

    Ok(())
}

/// Equals sign should close the completion popup and insert '='.
/// Currently it's treated as a type-to-filter character.
#[test]
fn test_completion_equals_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press equals
    harness.send_key(KeyCode::Char('='), KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Equals sign should close the completion popup"
    );

    // The character should be inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calc=", "Equals should be inserted into the buffer");

    Ok(())
}

/// Close brace should close the completion popup and insert '}'.
/// Currently it's treated as a type-to-filter character.
#[test]
fn test_completion_close_brace_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press close brace
    harness.send_key(KeyCode::Char('}'), KeyModifiers::SHIFT)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Close brace should close the completion popup"
    );

    // The character should be inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains('}'),
        "Close brace should be inserted into the buffer, got: {buffer}"
    );

    Ok(())
}

/// Comma should close the completion popup and insert ','.
/// Currently it's treated as a type-to-filter character.
#[test]
fn test_completion_comma_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press comma
    harness.send_key(KeyCode::Char(','), KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Comma should close the completion popup"
    );

    // The character should be inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calc,", "Comma should be inserted into the buffer");

    Ok(())
}

/// Plus sign should close the completion popup and insert '+'.
/// Currently it's treated as a type-to-filter character.
#[test]
fn test_completion_plus_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press plus (requires Shift on most keyboards)
    harness.send_key(KeyCode::Char('+'), KeyModifiers::SHIFT)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Plus sign should close the completion popup"
    );

    // The character should be inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains('+'),
        "Plus sign should be inserted into the buffer, got: {buffer}"
    );

    Ok(())
}

/// Close parenthesis should close the completion popup and insert ')'.
#[test]
fn test_completion_close_paren_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press close paren
    harness.send_key(KeyCode::Char(')'), KeyModifiers::SHIFT)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Close parenthesis should close the completion popup"
    );

    let buffer = harness.get_buffer_content().unwrap();
    assert!(
        buffer.contains(')'),
        "Close parenthesis should be inserted into the buffer, got: {buffer}"
    );

    Ok(())
}

/// Dot/period should close the completion popup and insert '.'.
#[test]
fn test_completion_dot_closes_popup_and_inserts() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press dot
    harness.send_key(KeyCode::Char('.'), KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed (or re-triggered for member access, but not
    // kept open with the old items and a corrupted filter state)
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Dot should close the completion popup (may re-trigger a new one later)"
    );

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calc.", "Dot should be inserted into the buffer");

    Ok(())
}

// ============================================================================
// Issue 4: Ctrl+key combinations are swallowed
// ============================================================================

/// Ctrl+P (command palette) should close the popup and open the command palette.
/// Currently the key is silently consumed.
#[test]
fn test_completion_ctrl_p_closes_popup() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press Ctrl+P (command palette)
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Ctrl+P should close the completion popup"
    );

    // Buffer should not be modified
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "calc",
        "Ctrl+P should not modify the buffer content"
    );

    Ok(())
}

/// Ctrl+S (save) should close the popup and save the file.
/// Currently the key is silently consumed.
#[test]
fn test_completion_ctrl_s_closes_popup() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press Ctrl+S
    harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Ctrl+S should close the completion popup"
    );

    // Buffer should not be modified
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "calc",
        "Ctrl+S should not modify the buffer content"
    );

    Ok(())
}

/// Ctrl+F (find) should close the popup and open the search.
/// Currently the key is silently consumed.
#[test]
fn test_completion_ctrl_f_closes_popup() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press Ctrl+F
    harness.send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Ctrl+F should close the completion popup"
    );

    // Buffer should not be modified
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "calc",
        "Ctrl+F should not modify the buffer content"
    );

    Ok(())
}

// ============================================================================
// Issue 5: Delete key is swallowed
// ============================================================================

/// Delete key should close the completion popup.
/// Currently the key is silently consumed.
#[test]
fn test_completion_delete_key_closes_popup() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Press Delete
    harness.send_key(KeyCode::Delete, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Delete key should close the completion popup"
    );

    Ok(())
}

// ============================================================================
// Issue 6: Shift+Tab is swallowed
// ============================================================================

/// Shift+Tab should not be silently consumed. It should either navigate to the
/// previous item or close the popup.
/// Currently the key is silently consumed (no effect).
#[test]
fn test_completion_shift_tab_not_swallowed() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Move to second item first
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;

    // Press Shift+Tab
    harness.send_key(KeyCode::Tab, KeyModifiers::SHIFT)?;
    harness.render()?;

    // The key should have some effect - either the popup closes or the
    // selection moves. It should NOT be silently consumed.
    // At minimum, the popup should close:
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Shift+Tab should not be silently consumed - popup should close"
    );

    Ok(())
}

// ============================================================================
// Verify that word characters still correctly type-to-filter
// (regression guards - these should continue to work)
// ============================================================================

/// Word characters (letters, digits, underscore) should continue to work as
/// type-to-filter while the popup is open.
#[test]
fn test_completion_word_chars_still_filter() -> anyhow::Result<()> {
    let mut harness = setup_completion_popup("calc")?;

    // Type 'u' to filter to "calcu*" items
    harness.send_key(KeyCode::Char('u'), KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should still be visible (items match "calcu")
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should remain visible when word chars match completions"
    );

    // Buffer should have the typed char
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calcu", "Character should be inserted into buffer");

    // The screen should still show matching completions
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("calculate_difference"),
        "Matching completions should still be visible"
    );

    Ok(())
}

/// Underscore should be treated as a word character for type-to-filter.
#[test]
fn test_completion_underscore_filters() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;

    // Type "calculate" as prefix
    harness.type_text("calculate")?;
    harness.render()?;

    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "calculate_sum".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            insert_text: Some("calculate_sum".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "calculated".to_string(),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            insert_text: Some("calculated".to_string()),
            ..Default::default()
        },
    ];
    harness.editor_mut().set_completion_items(completion_items);

    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Completion,
            title: Some("Completion".to_string()),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "calculate_sum".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("calculate_sum".to_string()),
                    },
                    PopupListItemData {
                        text: "calculated".to_string(),
                        detail: None,
                        icon: None,
                        data: Some("calculated".to_string()),
                    },
                ],
                selected: 0,
            },
            position: PopupPositionData::BelowCursor,
            width: 50,
            max_height: 15,
            bordered: true,
        },
    });
    harness.render()?;

    // Type underscore - should filter to only "calculate_sum"
    harness.send_key(KeyCode::Char('_'), KeyModifiers::SHIFT)?;
    harness.render()?;

    // Popup should still be visible
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Underscore should act as type-to-filter (word character)"
    );

    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "calculate_");

    // Only calculate_sum should remain (calculated doesn't match "calculate_")
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("calculate_sum"),
        "calculate_sum should still be visible"
    );
    assert!(
        !screen.contains("calculated"),
        "calculated should be filtered out (doesn't start with 'calculate_')"
    );

    Ok(())
}
