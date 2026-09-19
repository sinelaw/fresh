//! The Everything dialog scans through the agent-sessions plugin hub.
//!
//! Covers the chain from palette command to rendered rows. What a scan finds
//! depends on the home directory the test runs against, so the assertions are
//! about the dialog answering, not about any tool being installed.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

const WIDTH: u16 = 160;
const HEIGHT: u16 = 40;

fn run_palette(harness: &mut EditorTestHarness, command_name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(command_name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn discover_dialog_scans_through_the_plugin_hub() {
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();

    run_palette(&mut harness, "Everything");
    let opened = harness.screen_to_string();
    assert!(
        opened.contains("Everything"),
        "the dialog opens; screen was:\n{opened}"
    );
    assert!(
        opened.contains("Machine"),
        "with the machine picker; screen was:\n{opened}"
    );
    assert!(
        opened.contains("Pick a machine and choose Scan"),
        "and the pre-scan hint; screen was:\n{opened}"
    );

    // Focus starts on Scan. The scan runs off the editor thread, so tick until it settles.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let mut settled = String::new();
    for _ in 0..400 {
        harness.tick_and_render().unwrap();
        let screen = harness.screen_to_string();
        if !screen.contains("Pick a machine and choose Scan")
            && !screen.contains("Scanning for agent sessions")
        {
            settled = screen;
            break;
        }
    }
    assert!(
        !settled.is_empty(),
        "the scan settled rather than hanging on the pending line"
    );
    assert!(
        settled.contains("Everything"),
        "the dialog is still up with its answer; screen was:\n{settled}"
    );
    // A missing hub, a rejected `openMachine` spec or a throw in the chain surfaces here.
    assert!(
        !settled.contains("Session discovery failed"),
        "the scan completed rather than failing; screen was:\n{settled}"
    );

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let closed = harness.screen_to_string();
    assert!(
        !closed.contains("Pick a machine and choose Scan"),
        "the dialog closed; screen was:\n{closed}"
    );
    assert!(!harness.editor().should_quit(), "and the editor is fine");
}
