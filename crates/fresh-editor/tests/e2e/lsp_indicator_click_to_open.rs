//! E2E tests: the LSP status popup never auto-shows; the indicator
//! is the only entry point.
//!
//! Replaces the old auto-prompt behaviour where opening a file with a
//! configured-but-dormant LSP would pop the status dialog under the
//! user's cursor. The new UX:
//!
//!   * Opening such a file shows only the `LSP (off)` indicator on the
//!     status bar — no popup is attached to the buffer.
//!   * The indicator is drawn in a visually prominent palette (the
//!     `status_lsp_actionable_*` theme keys, defaulting to the
//!     warning-indicator palette) so the user sees there's something
//!     to act on without a dialog stealing focus.
//!   * Clicking the indicator opens the LSP status popup explicitly,
//!     same as the existing `ShowLspStatus` action.

use crate::common::harness::{EditorTestHarness, HarnessOptions};

fn make_config_with_dormant_rust_lsp() -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![],
            enabled: true,
            auto_start: false,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("rust-analyzer".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );
    config
}

/// Opening a file whose language has a dormant LSP server must NOT
/// auto-show the LSP status popup. The previous behaviour swallowed
/// the user's next keystrokes; the new behaviour leaves the buffer
/// focused and the keyboard live.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_no_popup_on_open_for_dormant_lsp() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    // Wait for the indicator to appear so the post-file-open async
    // bookkeeping (config probe, language detection) has settled
    // before we assert "no popup".
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // The status bar surfaces the dormant indicator …
    assert!(
        harness.get_status_bar().contains("LSP (off)"),
        "expected dormant LSP indicator on status bar; got: {}",
        harness.get_status_bar()
    );
    // … but no popup should be on the screen. Dialog labels from the
    // status popup ("Start rust-analyzer", "Disable LSP for rust",
    // etc.) would all have to surface from a popup — none of them
    // should be visible.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Start rust-analyzer"),
        "no auto-popup should appear on file open. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("LSP Servers (rust)"),
        "no LSP status popup title should appear on file open. Screen:\n{}",
        screen
    );
    Ok(())
}

/// Clicking the LSP indicator on the status bar opens the LSP status
/// popup. This is the only entry point now that the auto-prompt is
/// gone, so a regression here would strand the user with no way to
/// reach the start/stop/install actions.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_click_indicator_opens_popup() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;

    harness.open_file(&file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // Locate the LSP indicator on the rendered status bar by scanning
    // for "LSP" on the status row. The indicator is centered in an
    // 11-cell pill, and `get_status_bar` returns the trimmed status
    // row; we approximate the click target by finding the column of
    // the "L" character in the raw screen capture.
    let screen = harness.screen_to_string();
    let status_row_idx = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("LSP (off)"))
        .map(|(i, _)| i as u16)
        .expect("status row with LSP indicator must be present");
    let status_row = screen.lines().nth(status_row_idx as usize).unwrap();
    let lsp_col = status_row
        .find("LSP")
        .expect("status row should contain LSP segment") as u16;

    harness.mouse_click(lsp_col, status_row_idx)?;

    // After click, the popup's title appears on screen.
    let after = harness.screen_to_string();
    assert!(
        after.contains("LSP Servers (rust)"),
        "clicking the LSP indicator should open the status popup. Screen:\n{}",
        after
    );
    Ok(())
}

/// Find the row containing the popup's bottom border (`└`) and the
/// row containing the status bar (`LSP (off)` indicator). Returns
/// `(popup_bottom_row, status_row)` as 0-indexed display rows.
///
/// The popup uses unicode box-drawing characters; `└` is unique to the
/// popup's bottom-left corner so it's a reliable anchor.
fn popup_bottom_and_status_rows(screen: &str) -> (usize, usize) {
    let popup_bottom_row = screen
        .lines()
        .position(|l| l.contains('└'))
        .expect("popup must render a bottom border");
    let status_row = screen
        .lines()
        .position(|l| l.contains("LSP (off)"))
        .expect("status bar with LSP indicator must be present");
    (popup_bottom_row, status_row)
}

/// Find the column of the popup's right border (`┐` on the popup top
/// row, or `│` and `┘` on subsequent rows). Picks the right-most box
/// border on the row so we hit the popup's right edge specifically.
fn popup_right_border_col(screen: &str) -> usize {
    let top = screen
        .lines()
        .find(|l| l.contains('┐'))
        .expect("popup must render a top-right corner");
    top.char_indices()
        .filter(|(_, c)| *c == '┐')
        .last()
        .map(|(i, _)| top[..i].chars().count())
        .expect("right corner must be locatable")
}

/// Regression: the LSP popup's bottom border must sit one row above the
/// status bar with no gap, in BOTH prompt-visible and prompt-auto-hide
/// modes. Before the fix, the popup positioning hard-coded a 2-row
/// reservation at the bottom (status + prompt), so when the prompt
/// auto-hid, the popup floated one row too high and left a visible gap
/// between its bottom border and the status bar.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_hugs_status_bar_in_both_prompt_modes() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;

    // Mode 1: prompt line visible (harness default). Open the popup,
    // assert no gap between popup bottom and status bar.
    assert!(harness.editor().prompt_line_visible());
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;
    let screen = harness.screen_to_string();
    let (popup_bottom, status_row) = popup_bottom_and_status_rows(&screen);
    assert_eq!(
        popup_bottom + 1,
        status_row,
        "with prompt-line visible, popup bottom (row {}) must be \
         immediately above status bar (row {}). Screen:\n{}",
        popup_bottom,
        status_row,
        screen
    );

    // Close the popup and toggle prompt-line off (auto-hide mode).
    // Render after the toggle so the cached layout (which the popup
    // builder reads to find the status row) reflects the new layout
    // before the popup is rebuilt — the same ordering happens in the
    // real user flow because a status-bar click triggers a render
    // before dispatching `ShowLspStatus`.
    harness.editor_mut().show_lsp_status_popup(); // toggles closed
    harness.editor_mut().toggle_prompt_line();
    assert!(!harness.editor().prompt_line_visible());
    harness.render()?;

    // Mode 2: prompt line hidden. Same invariant.
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;
    let screen = harness.screen_to_string();
    let (popup_bottom, status_row) = popup_bottom_and_status_rows(&screen);
    assert_eq!(
        popup_bottom + 1,
        status_row,
        "with prompt-line auto-hidden, popup bottom (row {}) must be \
         immediately above status bar (row {}); the previous bug left \
         a 1-row gap here. Screen:\n{}",
        popup_bottom,
        status_row,
        screen
    );

    Ok(())
}

/// Regression: the LSP popup's right border must not paint on top of
/// the editor scrollbar (the rightmost column of the split). Before
/// the fix, the popup's right-edge clamp ran flush to
/// `terminal_area.width`, so the right border `┐` landed in the same
/// cell as the scrollbar.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_popup_right_border_does_not_overlap_scrollbar() -> anyhow::Result<()> {
    let temp = tempfile::tempdir()?;
    let file = temp.path().join("hello.rs");
    // Long enough to force the scrollbar to render.
    let mut body = String::new();
    for i in 0..200 {
        body.push_str(&format!("    let x_{} = {};\n", i, i));
    }
    body.push_str("fn main() {}\n");
    std::fs::write(&file, body)?;

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(make_config_with_dormant_rust_lsp())
            .with_working_dir(temp.path().to_path_buf()),
    )?;
    harness.open_file(&file)?;
    harness.wait_until(|h| h.get_status_bar().contains("LSP (off)"))?;
    harness.editor_mut().show_lsp_status_popup();
    harness.render()?;

    let screen = harness.screen_to_string();
    let right_border_col = popup_right_border_col(&screen);
    // Width is 120 (cols 0..=119). The scrollbar lives in col 119, so
    // the popup's rightmost cell must be at most col 118.
    assert!(
        right_border_col <= 118,
        "popup right border at col {} must not overlap the scrollbar \
         at col 119. Screen:\n{}",
        right_border_col,
        screen
    );

    Ok(())
}

/// Sanity: the default `status_lsp_actionable_*` theme keys must
/// differ from the muted status-bar palette. Without this distinction,
/// the dormant indicator would visually disappear into the bar and
/// the user would lose the only on-screen signal that there's
/// something to click (now that the auto-popup is gone).
///
/// The actual mapping of `LspIndicatorState::Off` to these keys is
/// covered by `view::ui::status_bar` unit tests; this test pins the
/// theme-level invariant the click-to-open UX relies on.
#[test]
fn test_actionable_palette_default_is_distinct_from_status_bar() {
    let theme = fresh::view::theme::Theme::from_json(
        r#"{"name":"t","editor":{},"ui":{},"search":{},"diagnostic":{},"syntax":{}}"#,
    )
    .expect("minimal theme should parse");
    assert_ne!(
        theme.status_lsp_actionable_fg, theme.status_bar_fg,
        "actionable LSP indicator fg must default to a value distinct \
         from status-bar fg"
    );
    assert_ne!(
        theme.status_lsp_actionable_bg, theme.status_bar_bg,
        "actionable LSP indicator bg must default to a value distinct \
         from status-bar bg"
    );
}
