//! Regression test for issue #2268.
//!
//! A terminal-initiated bracketed paste (`Event::Paste`) while a Settings
//! text input is focused must land in that field — not in the editor buffer
//! obscured behind the dialog. The test also guards the inverse: once the
//! dialog is closed, a bracketed paste must reach the buffer again. The
//! `settings_state` is only *hidden* on close (not dropped), so the router
//! must gate on visibility or it would keep swallowing buffer pastes.
//!
//! Mirrors the manual repro: paste into the buffer, open Settings, go to
//! Terminal -> Command and paste, close Settings, then paste again.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// A label only rendered while the Terminal settings panel is shown. Used
/// both to detect that the Terminal category is selected and to confirm the
/// dialog has closed.
const TERMINAL_PANEL_MARKER: &str = "Jump To End On Output";

/// Arrow down through the category list until the right panel renders
/// `marker`, i.e. the desired category is selected.
fn select_category_showing(harness: &mut EditorTestHarness, marker: &str) {
    for _ in 0..40 {
        if harness.screen_to_string().contains(marker) {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    panic!(
        "category showing {marker:?} never selected; screen:\n{}",
        harness.screen_to_string()
    );
}

/// Arrow down through the focused settings panel until `label` is the
/// focused row. The focus marker is ">  " (unchanged) or ">● " (changed),
/// matching the renderer (see settings_paste.rs).
fn focus_setting_row(harness: &mut EditorTestHarness, label: &str) {
    let unchanged = format!(">  {label}");
    let changed = format!(">● {label}");
    for _ in 0..40 {
        let screen = harness.screen_to_string();
        if screen.contains(&unchanged) || screen.contains(&changed) {
            return;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    panic!(
        "settings row {label:?} never focused; screen:\n{}",
        harness.screen_to_string()
    );
}

#[test]
fn test_bracketed_paste_routing_with_settings_dialog() {
    let mut harness = EditorTestHarness::new(150, 45).unwrap();

    // Step 1: bracketed paste into the editor buffer.
    harness.send_paste("BUF1").unwrap();
    harness.assert_buffer_content("BUF1");

    // Step 2: open the Settings UI.
    harness.open_settings().unwrap();

    // Step 3: select the Terminal category, move focus into the settings
    // panel, and land on the Command text input.
    select_category_showing(&mut harness, TERMINAL_PANEL_MARKER);
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    focus_setting_row(&mut harness, "Command");
    // Enter edit mode on the focused Command field.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Step 4: bracketed paste into the focused Command field.
    harness.send_paste("CMD2").unwrap();
    // It landed in the field...
    harness.assert_screen_contains("CMD2");
    // ...and NOT in the buffer hidden behind the dialog.
    harness.assert_buffer_content("BUF1");

    // Step 5: close the Settings UI (Esc exits edit mode, Esc closes).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.assert_screen_not_contains(TERMINAL_PANEL_MARKER);

    // Step 6: with the dialog closed, a bracketed paste must reach the
    // buffer again (cursor sits at the end of "BUF1").
    harness.send_paste("BUF3").unwrap();
    harness.assert_buffer_content("BUF1BUF3");
}
