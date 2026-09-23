//! The Import sessions dialog scans through the agent-sessions plugin hub.
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

    run_palette(&mut harness, "Import sessions");
    let opened = harness.screen_to_string();
    assert!(
        opened.contains("Import sessions"),
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
        settled.contains("Import sessions"),
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

/// **A `~/.ssh/config` host is a machine to scan.** The Machines dialog lists
/// config hosts beside the saved machines; the Machine picker here must offer
/// the same hosts, or a host visible there cannot be scanned from here.
#[test]
fn discover_machine_picker_lists_ssh_config_hosts() {
    let data_home = tempfile::tempdir().unwrap();
    let ssh = data_home.path().join("home").join(".ssh");
    std::fs::create_dir_all(&ssh).unwrap();
    std::fs::write(
        ssh.join("config"),
        "Host plantedbox\n  HostName 10.0.0.9\n  User deploy\n",
    )
    .unwrap();
    let dir_context = fresh::config_io::DirectoryContext::for_testing(data_home.path());
    let mut harness = EditorTestHarness::create(
        WIDTH,
        HEIGHT,
        crate::common::harness::HarnessOptions::new()
            .with_project_root()
            .with_shared_dir_context(dir_context),
    )
    .unwrap();
    harness.tick_and_render().unwrap();

    run_palette(&mut harness, "Import sessions");
    harness.assert_screen_contains("Pick a machine and choose Scan");

    // Focus starts on Scan; Shift+Tab reaches the Machine picker and Enter
    // drops its list open.
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("plantedbox"),
        "the config host is offered as a machine to scan; screen was:\n{screen}"
    );
}
