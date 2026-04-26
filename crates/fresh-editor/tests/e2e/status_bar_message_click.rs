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
