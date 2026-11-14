//! The Import sessions dialog scans through the agent-sessions plugin hub.
//!
//! Covers the chain from palette command to rendered rows. The assertions are
//! about the dialog answering, not about any tool being installed: the scan
//! runs against a home directory of this test's own, so it finds nothing and
//! says so, whatever the machine running the suite has in its real one.

use crate::common::global_state::pin_env;
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
    // The scan resolves the stores it looks in (`~/.claude/projects`,
    // `~/.codex/sessions`, ...) out of `$HOME`, and it reads it through the
    // plugin API's `getEnv`, which is `std::env::var` rather than the
    // harness's `DirectoryContext`. Unpinned, this test scans the home
    // directory of whoever runs the suite and asserts on what is in it
    // (CONTRIBUTING.md Testing §4). An empty one of its own is the fixture
    // the assertions below actually want.
    //
    // `$HOME` only, deliberately. The scan reads `getEnv("HOME") ??
    // getEnv("USERPROFILE")`, so pinning `HOME` alone redirects it on every
    // platform — and `USERPROFILE` must be left as it is, because Windows
    // resolves the cache directory from it. Pinning that too pointed the
    // cache at a temp directory with no `AppData\Local`, embedded plugin
    // extraction failed with "Could not determine cache directory", and the
    // palette then had no "Import sessions" command to open at all.
    let home = tempfile::tempdir().unwrap();
    let _env = pin_env(&[("HOME", home.path())]);

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

    // Focus starts on Scan. The scan runs off the editor thread, so wait for
    // it to leave the pending line. A fixed tick budget here is a timeout
    // inside the test (CONTRIBUTING.md Testing §3): 400 ticks is plenty on an
    // idle machine and not always enough under a loaded parallel run, which
    // turns contention into "the scan never settled". `wait_until` waits on
    // the state instead, and gives up out loud with the screen it was on.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Pick a machine and choose Scan")
                && !screen.contains("Scanning for agent sessions")
        })
        .unwrap_or_else(|e| panic!("the scan hung on the pending line: {e}"));
    let settled = harness.screen_to_string();
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
