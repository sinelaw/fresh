//! Reproduces a focus-grab bug where an LSP popup that's still on screen
//! while the user is browsing the file explorer (single-clicks or up/down
//! arrows) silently swallows their next keystroke / blocks the next preview
//! switch.
//!
//! Why these tests live in their own file: the bug is at the seam between
//! `preview_tabs` and the LSP popup stack — if either feature regresses in
//! isolation the failure mode here would be misleading. Keeping the
//! reproduction self-contained makes the regression obvious.
//!
//! Two invariants under test:
//!   1. Keyboard nav in the explorer (Down/Up) must continue to drive the
//!      preview tab even when an LSP popup is visible. The popup must NOT
//!      silently consume the keystroke. Today it does, because
//!      `get_key_context()` returns `KeyContext::Popup` whenever any popup
//!      is visible, regardless of which pane the user is interacting with.
//!   2. Switching the preview to a different file (via mouse single-click
//!      or arrow nav) must dismiss the LSP popup. Otherwise the popup
//!      anchored to the previous buffer's cursor lingers over an unrelated
//!      file and continues to grab focus.
//!
//! Per CONTRIBUTING.md §2 ("E2E Tests Observe, Not Inspect") the assertions
//! only inspect rendered screen output (tab bar text + popup label text),
//! never internal state.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::model::event::{
    Event, PopupContentData, PopupData, PopupKindHint, PopupListItemData, PopupPositionData,
};
use std::fs;
use std::time::Duration;

/// Same row constants as `preview_tabs.rs` — the harness layout is shared.
const TAB_BAR_ROW: u16 = 1;
const EXPLORER_CLICK_COL: u16 = 10;
const FIRST_EXPLORER_ROW: usize = 2;

/// A short, recognizable string we put inside the simulated LSP popup so
/// the test can assert "popup is on screen" / "popup is gone" purely from
/// rendered output. Chosen to be unlikely to collide with editor chrome,
/// filenames, or status messages elsewhere on the screen.
const POPUP_MARKER: &str = "calculate_difference";

fn setup_explorer_with_files(filenames: &[&str]) -> EditorTestHarness {
    let mut harness = EditorTestHarness::with_temp_project(120, 40).unwrap();
    let project = harness.project_dir().unwrap();
    for name in filenames {
        // Pad each file with a few lines so a hypothetical popup anchored
        // at the cursor has room to render below row 1 (tab bar).
        let body = format!("{name}\nline 2\nline 3\nline 4\nline 5\n");
        fs::write(project.join(name), body).unwrap();
    }

    harness
        .send_key(KeyCode::Char('e'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_file_explorer().unwrap();
    for name in filenames {
        harness.wait_for_file_explorer_item(name).unwrap();
    }
    harness
}

/// Find the screen row that renders `name` in the explorer tree, skipping
/// the menu/tab bar so a tab-bar match (the preview indicator puts the
/// filename in the tab bar) doesn't shadow the explorer entry.
fn explorer_row_for(harness: &EditorTestHarness, name: &str) -> u16 {
    let screen = harness.screen_to_string();
    for (row, line) in screen.lines().enumerate().skip(FIRST_EXPLORER_ROW) {
        let prefix: String = line.chars().take(40).collect();
        if prefix.contains(name) {
            return row as u16;
        }
    }
    panic!("file {name} not found in file explorer;\nscreen:\n{screen}");
}

fn single_click_file(harness: &mut EditorTestHarness, name: &str) {
    // Reset the double-click window — see `preview_tabs.rs` for the same
    // reasoning. The harness uses a mocked clock; real `sleep` is a no-op.
    let dc_window = Duration::from_millis(
        harness
            .config()
            .editor
            .double_click_time_ms
            .saturating_mul(2),
    );
    harness.advance_time(dc_window);
    let row = explorer_row_for(harness, name);
    harness.mouse_click(EXPLORER_CLICK_COL, row).unwrap();
}

fn tab_bar(harness: &EditorTestHarness) -> String {
    harness.screen_row_text(TAB_BAR_ROW)
}

/// Inject a popup that mimics the real LSP completion popup. We mirror
/// `build_completion_popup_from_items` exactly: `kind = Completion`,
/// `transient = false`. The non-transient flag is what makes this bug
/// observable — transient popups (hover, signature help) are auto-
/// dismissed on the next key press by `Editor::handle_key`, so they
/// surface only as a one-frame flicker. The completion popup, by
/// contrast, sticks around and steals subsequent keystrokes.
fn show_lsp_popup(harness: &mut EditorTestHarness) {
    harness
        .apply_event(Event::ShowPopup {
            popup: PopupData {
                kind: PopupKindHint::Completion,
                title: None,
                description: None,
                transient: false,
                content: PopupContentData::List {
                    items: vec![PopupListItemData {
                        id: "t114_33".into(),
                        text: POPUP_MARKER.to_string(),
                        detail: Some("fn calculate_difference(a: i32, b: i32) -> i32".to_string()),
                        icon: Some("λ".to_string()),
                        data: Some(POPUP_MARKER.to_string()),
                    }],
                    selected: 0,
                },
                position: PopupPositionData::BelowCursor,
                width: 50,
                max_height: 10,
                bordered: true,
            },
        })
        .unwrap();
    harness.render().unwrap();
}

/// Down arrow in the focused file explorer must continue to update the
/// preview tab even when an LSP popup happens to be on screen — the popup
/// must not silently consume the keystroke. The popup must also be
/// dismissed so the next preview can render in peace.
#[test]
fn down_arrow_drives_preview_when_lsp_popup_is_visible() {
    let mut harness = setup_explorer_with_files(&["alpha.txt", "beta.txt", "gamma.txt"]);

    // First Down: select alpha.txt and open it as a preview.
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();
    assert!(
        tab_bar(&harness).contains("alpha.txt"),
        "first Down should preview alpha.txt; tab bar:\n{}",
        tab_bar(&harness)
    );

    // Simulate an LSP popup that arrived for the previewed buffer
    // (e.g. signature help that fired from a previous interaction).
    show_lsp_popup(&mut harness);
    assert!(
        harness.screen_to_string().contains(POPUP_MARKER),
        "precondition: popup should be visible on screen before Down\n{}",
        harness.screen_to_string()
    );

    // Second Down: this is what the user expects to advance the preview
    // to beta.txt. The popup must NOT swallow it.
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let bar = tab_bar(&harness);
    assert!(
        bar.contains("beta.txt"),
        "Down arrow must advance the preview to beta.txt even with an LSP popup visible; \
         tab bar:\n{}",
        bar
    );
    assert!(
        !bar.contains("alpha.txt"),
        "previous preview alpha.txt should have been replaced; tab bar:\n{}",
        bar
    );

    // The popup was anchored to the previous buffer's cursor; switching the
    // preview must dismiss it so it doesn't continue to grab keystrokes for
    // the new file.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(POPUP_MARKER),
        "LSP popup should be dismissed when the preview buffer is replaced; screen:\n{}",
        screen
    );
}

/// Up arrow has the same contract as Down: it should drive the preview
/// even when an LSP popup is visible, and dismiss the popup on the switch.
#[test]
fn up_arrow_drives_preview_when_lsp_popup_is_visible() {
    let mut harness = setup_explorer_with_files(&["alpha.txt", "beta.txt"]);

    // Down twice → preview beta.txt.
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness
        .send_key(KeyCode::Down, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();
    assert!(
        tab_bar(&harness).contains("beta.txt"),
        "precondition: beta.txt should be the active preview; tab bar:\n{}",
        tab_bar(&harness)
    );

    show_lsp_popup(&mut harness);

    // Up should walk preview back to alpha.txt.
    harness
        .send_key(KeyCode::Up, KeyModifiers::empty())
        .unwrap();
    harness.render().unwrap();

    let bar = tab_bar(&harness);
    assert!(
        bar.contains("alpha.txt"),
        "Up arrow must walk the preview back to alpha.txt even with an LSP popup visible; \
         tab bar:\n{}",
        bar
    );
    assert!(
        !bar.contains("beta.txt"),
        "previous preview beta.txt should have been replaced; tab bar:\n{}",
        bar
    );

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(POPUP_MARKER),
        "LSP popup should be dismissed when the preview buffer is replaced; screen:\n{}",
        screen
    );
}

/// Mouse-click flow: single-clicking a different file in the explorer must
/// swap the preview *and* dismiss the lingering LSP popup. The mouse path
/// goes through `handle_file_explorer_click`, distinct from the keyboard
/// path, so the dismissal contract is exercised separately here.
#[test]
fn click_other_file_dismisses_lsp_popup_and_switches_preview() {
    let mut harness = setup_explorer_with_files(&["alpha.txt", "beta.txt"]);

    single_click_file(&mut harness, "alpha.txt");
    assert!(
        tab_bar(&harness).contains("alpha.txt"),
        "precondition: alpha.txt should be the active preview; tab bar:\n{}",
        tab_bar(&harness)
    );

    show_lsp_popup(&mut harness);
    assert!(
        harness.screen_to_string().contains(POPUP_MARKER),
        "precondition: popup should be visible before second click"
    );

    // Click beta.txt — the preview must swap, and the popup that was tied
    // to alpha.txt's cursor must go away.
    single_click_file(&mut harness, "beta.txt");

    let bar = tab_bar(&harness);
    assert!(
        bar.contains("beta.txt"),
        "single-click on beta.txt should swap the preview; tab bar:\n{}",
        bar
    );
    assert!(
        !bar.contains("alpha.txt"),
        "previous preview alpha.txt should have been replaced; tab bar:\n{}",
        bar
    );

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(POPUP_MARKER),
        "LSP popup should be dismissed when the preview buffer is swapped via click; \
         screen:\n{}",
        screen
    );
}
