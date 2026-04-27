//! Bug from interactive walkthrough: clicking on the rendered
//! status message at the bottom should open the status log file
//! (mirror of the warning-badge click → opens warning log). The
//! handler `Action::ShowStatusLog` is wired and dispatched, but
//! interactively in tmux nothing happens.
//!
//! This test pins the click → action → file-open chain by:
//!   1. Setting a real status log path on the editor.
//!   2. Setting a status message so the `Messages` element
//!      renders (without it `render_element` returns None and
//!      the layout cache never tracks the area).
//!   3. Rendering, finding the message-area cell coords.
//!   4. Clicking that cell.
//!   5. Asserting the status log file has been opened as a
//!      buffer.

use crate::common::harness::EditorTestHarness;
use std::path::PathBuf;

#[test]
fn click_on_status_message_opens_status_log_buffer() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::new(120, 30)?;

    // Wire a status log path that points at a real file with
    // some content — open_status_log routes through
    // open_local_file, which fails if the path is missing.
    let temp = tempfile::NamedTempFile::new()?;
    std::fs::write(temp.path(), "test status log line\n")?;
    let log_path: PathBuf = temp.path().to_path_buf();
    harness.editor_mut().set_status_log_path(log_path.clone());

    // Set a status message so the `Messages` element renders
    // somewhere on the bottom row (and registers an area in
    // the layout cache that mouse_input.rs uses for routing
    // clicks to `Action::ShowStatusLog`).
    let marker = "hello-from-status-bar";
    harness.editor_mut().set_status_message(marker.to_string());
    harness.render()?;

    // Locate the rendered message text on screen and click on
    // its first cell. find_text_on_screen returns (col, row).
    let (col, row) = harness.find_text_on_screen(marker).ok_or_else(|| {
        anyhow::anyhow!(
            "status message marker must be visible on screen after render; \
             screen:\n{}",
            harness.screen_to_string()
        )
    })?;
    let click_col = col;
    let click_row = row;

    harness.mouse_click(click_col, click_row)?;
    harness.render()?;

    // After the click the active buffer should be the status
    // log — `open_status_log` calls `open_local_file` then
    // `mark_buffer_read_only`, which routes the focus there.
    // Verify by reading the active buffer's content.
    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("test status log line"),
        "clicking on the status message must open the status log as the active buffer; \
         active buffer content was: {content:?}\nclick was at ({click_col},{click_row})\n\
         screen:\n{}",
        harness.screen_to_string()
    );
    Ok(())
}

/// `set_status_message` (the Rust-side path) is the simpler
/// case. Plugins set status via `editor.setStatus(...)` which
/// routes to `plugin_status_message` instead of `status_message`.
/// Verify the click works for that path too — same `Messages`
/// element renders both, but different fields populate them.
#[test]
fn click_on_plugin_status_message_opens_status_log() -> anyhow::Result<()> {
    use fresh_core::api::PluginCommand;

    let mut harness = EditorTestHarness::new(120, 30)?;

    let temp = tempfile::NamedTempFile::new()?;
    std::fs::write(temp.path(), "plugin-path log line\n")?;
    harness
        .editor_mut()
        .set_status_log_path(temp.path().to_path_buf());

    // Use the same path real plugins use: send a SetStatus
    // command through the plugin command channel.
    let marker = "plugin-set-status-marker";
    let _ = harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetStatus {
            message: marker.to_string(),
        });
    harness.render()?;

    let (col, row) = harness.find_text_on_screen(marker).ok_or_else(|| {
        anyhow::anyhow!(
            "plugin status marker must be visible after handle_plugin_command(SetStatus); \
             screen:\n{}",
            harness.screen_to_string()
        )
    })?;

    harness.mouse_click(col, row)?;
    harness.render()?;

    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("plugin-path log line"),
        "clicking on a plugin-set status message must open the status log; \
         active buffer content was: {content:?}\nclick=({col},{row})\nscreen:\n{}",
        harness.screen_to_string()
    );
    Ok(())
}

/// `set_status_log_path` survives a destructive editor rebuild.
///
/// Production drops the old editor and constructs a new one on every
/// authority swap (devcontainer attach, SSH connect, …). The status
/// log path used to be wired up only in `handle_first_run_setup`, so
/// after the swap the new editor had no log path and clicking the
/// status bar surfaced "Status log not available". `main.rs` now
/// captures the path once and re-binds it to every editor — this test
/// pins the bare invariant that re-applying the path on a fresh editor
/// gets clicks routed back to the log.
#[test]
fn status_log_path_can_be_rebound_after_editor_rebuild() -> anyhow::Result<()> {
    use crate::common::harness::HarnessOptions;

    let temp = tempfile::NamedTempFile::new()?;
    std::fs::write(temp.path(), "post-restart status log line\n")?;
    let log_path: PathBuf = temp.path().to_path_buf();

    // First editor: configure the path, then drop it without
    // touching the channel — simulates the pre-restart editor going
    // out of scope when `setAuthority` triggers a rebuild.
    {
        let mut harness = EditorTestHarness::create(120, 30, HarnessOptions::new())?;
        harness.editor_mut().set_status_log_path(log_path.clone());
    }

    // Second editor: brand-new instance, no first-run setup, just
    // the explicit re-bind that the new main-loop wiring performs.
    let mut harness = EditorTestHarness::create(120, 30, HarnessOptions::new())?;
    harness.editor_mut().set_status_log_path(log_path.clone());

    let marker = "post-restart-status-marker";
    harness.editor_mut().set_status_message(marker.to_string());
    harness.render()?;

    let (col, row) = harness.find_text_on_screen(marker).ok_or_else(|| {
        anyhow::anyhow!(
            "post-restart status marker must be visible; screen:\n{}",
            harness.screen_to_string()
        )
    })?;
    harness.mouse_click(col, row)?;
    harness.render()?;

    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        content.contains("post-restart status log line"),
        "rebound status log path must open the log buffer; \
         active buffer content was: {content:?}\nscreen:\n{}",
        harness.screen_to_string()
    );
    Ok(())
}

/// `take_warning_log` returns the path that was last set, and a
/// freshly-created editor without `set_warning_log` returns None.
/// Pins the round-trip used by `main.rs` to forward the warning
/// channel across editor restarts.
#[test]
fn take_warning_log_returns_set_value() -> anyhow::Result<()> {
    let mut harness = EditorTestHarness::new(80, 24)?;
    assert!(
        harness.editor_mut().take_warning_log().is_none(),
        "new editor has no warning log channel installed"
    );

    let (_tx, rx) = std::sync::mpsc::channel::<()>();
    let path = std::path::PathBuf::from("/tmp/fake-warning-log");
    harness.editor_mut().set_warning_log(rx, path.clone());

    let taken = harness
        .editor_mut()
        .take_warning_log()
        .expect("warning log was set, must be returned by take");
    assert_eq!(taken.1, path);
    assert!(
        harness.editor_mut().take_warning_log().is_none(),
        "subsequent take returns None — single-consumer semantics"
    );
    Ok(())
}
