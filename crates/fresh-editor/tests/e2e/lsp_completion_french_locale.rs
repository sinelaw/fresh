//! E2E tests for LSP completion popup with non-English locales.
//!
//! When the locale is set to French (or any non-English locale), the completion
//! popup title is translated (e.g., "Complétion" instead of "Completion").
//! However, the refilter path in popup_actions.rs uses a hardcoded English title
//! "Completion", causing the popup to be reclassified as a generic List popup
//! after the first typed character. The List popup handler consumes ALL key
//! presses (modal behavior), making the editor appear stuck.
//!
//! Similarly, handle_popup_confirm checks for the hardcoded English title
//! "Completion", so pressing Enter/Tab to accept a completion does nothing
//! when using a non-English locale.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, LocaleName};
use fresh::model::event::{
    Event, PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
};
/// The French translation of "lsp.popup_completion" from locales/fr.json.
/// This is what t!("lsp.popup_completion") returns when the locale is "fr".
const FRENCH_COMPLETION_TITLE: &str = "Compl\u{00e9}tion";

/// Helper: set up an editor with French locale and a completion popup.
/// Uses the translated popup title to match what the real LSP code does.
fn setup_french_completion_popup(prefix: &str) -> anyhow::Result<EditorTestHarness> {
    let config = Config {
        locale: LocaleName(Some("fr".to_string())),
        ..Default::default()
    };
    let mut harness = EditorTestHarness::with_config(80, 24, config)?;

    // Type the initial prefix
    harness.type_text(prefix)?;
    harness.render()?;

    // Set up LSP completion items for re-filtering
    let completion_items = vec![
        lsp_types::CompletionItem {
            label: "test_function".to_string(),
            kind: Some(lsp_types::CompletionItemKind::FUNCTION),
            detail: Some("fn test_function()".to_string()),
            insert_text: Some("test_function".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "test_variable".to_string(),
            kind: Some(lsp_types::CompletionItemKind::VARIABLE),
            detail: Some("let test_variable".to_string()),
            insert_text: Some("test_variable".to_string()),
            ..Default::default()
        },
        lsp_types::CompletionItem {
            label: "test_struct".to_string(),
            kind: Some(lsp_types::CompletionItemKind::STRUCT),
            detail: Some("struct TestStruct".to_string()),
            insert_text: Some("test_struct".to_string()),
            ..Default::default()
        },
    ];
    harness.editor_mut().set_completion_items(completion_items);

    // Use the French-translated title, matching what lsp_requests.rs produces
    // when t!("lsp.popup_completion") is called with locale set to "fr".
    let completion_title = FRENCH_COMPLETION_TITLE.to_string();

    // Show completion popup with the translated (French) title
    let state = harness.editor_mut().active_state_mut();
    state.apply(&Event::ShowPopup {
        popup: PopupData {
            kind: PopupKindHint::Completion,
            title: Some(completion_title),
            description: None,
            transient: false,
            content: PopupContentData::List {
                items: vec![
                    PopupListItemData {
                        text: "test_function".to_string(),
                        detail: Some("fn test_function()".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some("test_function".to_string()),
                    },
                    PopupListItemData {
                        text: "test_variable".to_string(),
                        detail: Some("let test_variable".to_string()),
                        icon: Some("v".to_string()),
                        data: Some("test_variable".to_string()),
                    },
                    PopupListItemData {
                        text: "test_struct".to_string(),
                        detail: Some("struct TestStruct".to_string()),
                        icon: Some("S".to_string()),
                        data: Some("test_struct".to_string()),
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

/// After typing a character to filter the completion list with French locale,
/// the popup should remain a Completion popup (not degrade to a List popup).
/// Further typing should continue to work as type-to-filter.
///
/// BUG: refilter_completion_popup() uses hardcoded "Completion" title instead of
/// t!("lsp.popup_completion"), causing the refiltered popup to be classified as
/// PopupKind::List. The List handler's catch-all consumes ALL keys, making the
/// editor appear stuck.
#[test]
fn test_french_locale_completion_typing_not_stuck_after_refilter() -> anyhow::Result<()> {
    let mut harness = setup_french_completion_popup("test")?;

    // Type '_' to filter to "test_*" items - this triggers refilter_completion_popup
    harness.send_key(KeyCode::Char('_'), KeyModifiers::SHIFT)?;
    harness.render()?;

    // The popup should still be visible (items match "test_")
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should remain visible when items match the filter"
    );

    // Buffer should have the typed character
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(buffer, "test_", "Character should be inserted into buffer");

    // Now type 'f' to further filter - this is where the bug manifests.
    // If the popup was reclassified as List, this key will be consumed
    // by the action handler and never reach the buffer.
    harness.send_key(KeyCode::Char('f'), KeyModifiers::NONE)?;
    harness.render()?;

    // The character should appear in the buffer
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "test_f",
        "Second character should be inserted into buffer (not consumed by List handler)"
    );

    // The popup should still be visible with matching items
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Popup should still be visible with matching completions"
    );

    // Should show test_function in the completion list
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("test_function"),
        "test_function should still be visible in completions"
    );

    Ok(())
}

/// Pressing Enter to confirm a completion should work with French locale.
///
/// BUG: handle_popup_confirm() checks `title == "Completion"` (hardcoded English)
/// instead of using t!("lsp.popup_completion"). With French locale, the title is
/// "Complétion" so the check fails and the completion is never inserted.
#[test]
fn test_french_locale_completion_enter_confirms() -> anyhow::Result<()> {
    let mut harness = setup_french_completion_popup("test")?;

    // Press Enter to confirm the first completion item ("test_function")
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Popup should be closed after Enter"
    );

    // The completion text should have been inserted (replacing the prefix)
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "test_function",
        "Enter should insert the selected completion text"
    );

    Ok(())
}

/// Tab should also confirm a completion with French locale.
#[test]
fn test_french_locale_completion_tab_confirms() -> anyhow::Result<()> {
    let mut harness = setup_french_completion_popup("test")?;

    // Press Tab to confirm the first completion item ("test_function")
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;
    harness.render()?;

    // Popup should be closed
    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "Popup should be closed after Tab"
    );

    // The completion text should have been inserted
    let buffer = harness.get_buffer_content().unwrap();
    assert_eq!(
        buffer, "test_function",
        "Tab should insert the selected completion text"
    );

    Ok(())
}
