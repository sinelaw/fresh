//! Reproduces the focus-grab regression that surfaces when an LSP popup
//! pops up over the active buffer: keystrokes used to be silently
//! swallowed by the popup as soon as it became visible, even if the
//! user was in the middle of typing or navigating with the cursor. The
//! LSP auto-prompt that fires on `open_file` is the user-visible
//! example, but the same focus-routing rule applies to every popup
//! that pops up *under* the user's cursor without an explicit user
//! gesture (hover docs, signature help, plugin Text overlays, …).
//!
//! The new contract:
//!   1. Such popups are shown *unfocused*. Subsequent keystrokes drive
//!      the buffer / explorer / etc., not the popup.
//!   2. The popup title carries an `[Alt+T to focus]` hint so the user
//!      knows how to grab it with the keyboard.
//!   3. Pressing the configured popup-focus key (`Alt+T` by default)
//!      transfers focus to the popup. The hint disappears once the
//!      popup has focus.
//!
//! Per CONTRIBUTING.md §2 the assertions only inspect rendered screen
//! output (popup title text + buffer cursor line), never internal
//! state.
//!
//! Same `apply_event(Event::ShowPopup{...})` injection path as
//! `preview_lsp_popup_focus.rs`. We use `PopupKindHint::Text` here so
//! the popup is unfocused on show — Completion popups are a separate
//! beast (user-invoked, type-to-filter requires the popup to be the
//! keyboard target on show) and are exercised by the dedicated
//! `lsp_completion_popup_behavior` suite.
//!
//! Reproducing the bug on baseline:
//!   - Without the fix the unfocused-by-default flag does not exist, so
//!     `[Alt+T to focus]` never appears in the popup title and assertion
//!     #1 fails immediately.
//!   - The `popup_focus` action and `Alt+T` global binding don't exist
//!     either, so assertion #3 (focus transfer dismisses the hint) also
//!     fails.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{Event, PopupContentData, PopupData, PopupKindHint, PopupPositionData};

const FOCUS_HINT: &str = "Alt+T to focus";
const POPUP_TITLE: &str = "Hover Docs";
const POPUP_BODY: &str = "auto_shown_overlay_text";

fn setup_buffer_with_lines(line_count: usize) -> EditorTestHarness {
    let mut harness = EditorTestHarness::new(120, 30).unwrap();
    let body: String = (1..=line_count)
        .map(|n| format!("line_{:02}\n", n))
        .collect();
    harness.type_text(&body).unwrap();
    // Rewind so Down arrows have somewhere to go.
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness
}

/// Inject an auto-shown text popup of the same shape an LSP hover/auto-
/// prompt would build. `PopupKindHint::Text` makes the converter mark
/// the popup unfocused by default.
fn show_auto_popup(harness: &mut EditorTestHarness) {
    harness
        .apply_event(Event::ShowPopup {
            popup: PopupData {
                kind: PopupKindHint::Text,
                title: Some(POPUP_TITLE.to_string()),
                description: None,
                transient: false,
                content: PopupContentData::Text(vec![POPUP_BODY.to_string()]),
                position: PopupPositionData::BelowCursor,
                width: 40,
                max_height: 6,
                bordered: true,
            },
        })
        .unwrap();
    harness.render().unwrap();
}

/// Helper: extract the line of the screen that contains the popup title,
/// so the assertion error message points right at the bordered title row
/// rather than dumping the whole frame for every failure.
fn title_row(harness: &EditorTestHarness) -> Option<String> {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .find(|l| l.contains(POPUP_TITLE))
        .map(str::to_string)
}

#[test]
fn unfocused_popup_advertises_focus_key_in_title() {
    let mut harness = setup_buffer_with_lines(8);

    show_auto_popup(&mut harness);

    let title_line = title_row(&harness).unwrap_or_else(|| {
        panic!(
            "popup title should be visible:\n{}",
            harness.screen_to_string()
        )
    });
    assert!(
        title_line.contains(FOCUS_HINT),
        "Unfocused popup must advertise its focus key in the title; \
         expected \"{FOCUS_HINT}\" on title row:\n{title_line}",
    );
    // Body should still render so this is unambiguously a popup, not
    // some other widget chrome that happened to contain the title text.
    assert!(
        harness.screen_to_string().contains(POPUP_BODY),
        "popup body should be visible alongside the title hint;\nscreen:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn arrows_drive_buffer_when_popup_is_unfocused() {
    let mut harness = setup_buffer_with_lines(8);

    show_auto_popup(&mut harness);

    // Sanity check: cursor is on row 1 of the buffer (after Ctrl+Home).
    // The status bar reports "Ln 1, Col 1"; we use that as the
    // pre-condition observable.
    assert!(
        harness.screen_to_string().contains("Ln 1"),
        "precondition: cursor should be on line 1; screen:\n{}",
        harness.screen_to_string()
    );

    // Down arrow with an unfocused popup must drive the buffer cursor.
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Ln 2"),
        "Down arrow must move the buffer cursor to line 2 when the popup is unfocused; \
         screen:\n{screen}",
    );
}

/// Esc still dismisses an unfocused popup. Without this, the popup
/// would have to be focused (Alt+T) before the user could close it,
/// which doesn't match either the LSP-auto-prompt UX or the
/// `popup_cancel` keybinding contract.
#[test]
fn popup_cancel_key_dismisses_unfocused_popup() {
    let mut harness = setup_buffer_with_lines(8);

    show_auto_popup(&mut harness);
    assert!(
        harness.screen_to_string().contains(POPUP_BODY),
        "precondition: popup must be visible before Esc"
    );

    harness
        .send_key(KeyCode::Esc, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(POPUP_BODY),
        "Esc on an unfocused popup should dismiss it; popup body still on screen:\n{screen}",
    );
    assert!(
        !screen.contains(POPUP_TITLE),
        "Esc should also remove the popup title from the screen:\n{screen}",
    );
}

#[test]
fn alt_t_focuses_popup_and_clears_focus_hint() {
    let mut harness = setup_buffer_with_lines(8);

    show_auto_popup(&mut harness);

    // Sanity precondition: the hint is shown before focusing.
    let initial_title = title_row(&harness).expect("popup must be visible");
    assert!(
        initial_title.contains(FOCUS_HINT),
        "precondition: focus-key hint must be visible while popup is unfocused;\ntitle row:\n{initial_title}",
    );

    // Press Alt+T → focus moves to the popup.
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    let title_after = title_row(&harness).unwrap_or_else(|| {
        panic!(
            "popup must remain visible after Alt+T;\nscreen:\n{}",
            harness.screen_to_string()
        )
    });
    assert!(
        !title_after.contains(FOCUS_HINT),
        "Once the popup is focused the focus-key hint must disappear from the title;\n\
         title row:\n{title_after}",
    );
    assert!(
        title_after.contains(POPUP_TITLE),
        "Popup title text itself should still render after focus transfer;\ntitle row:\n{title_after}",
    );
}
