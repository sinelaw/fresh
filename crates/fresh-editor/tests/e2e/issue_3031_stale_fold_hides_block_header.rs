//! E2E tests for issue #3031: opening a Niri KDL config, the line
//! `debug {` renders as a bare `...` — as if the editor had swallowed the
//! word. The file on disk is fine (`nano` shows it), and nothing about KDL
//! is special here: what the reporter is looking at is a *collapsed fold*
//! whose header landed on the blank line above `debug {`, so the block
//! opener itself is inside the hidden range.
//!
//! Two independent defects conspire, and each gets a test here:
//!
//! 1. Session restore accepted a stale fold. A session file written before
//!    `header_text` was recorded (issue #1568) carries only line numbers,
//!    which restore trusts blindly. Edit the file between sessions and the
//!    saved header slides onto a blank line, putting the real block opener
//!    (`debug {`) into the folded — hidden — range.
//!
//! 2. Rendering then hid the evidence. The placeholder `apply_folding`
//!    appends lands in front of a blank header row's only source character,
//!    which reads as an injected row: no line number, and no `▸` arrow to
//!    click. All that is left on screen is `...` where a line of the user's
//!    config used to be.
//!
//! <https://github.com/sinelaw/fresh/issues/3031>

use crate::common::fixtures::TestFixture;
use crate::common::harness::{layout, EditorTestHarness, HarnessOptions};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use fresh::workspace::Workspace;
use tempfile::TempDir;

/// A trimmed-down Niri config with the reporter's `debug` block. Line
/// numbers below are 0-indexed: 7 is the blank line, 8 is `debug {`.
const NIRI_CONFIG: &str = "\
input {
    keyboard {
        xkb {
            layout \"us\"
        }
    }
}

debug {
  // Allows notification actions and window activation from Noctalia.
  honor-xdg-activation-with-invalid-serial
}
";

const BLANK_LINE: usize = 7;
const DEBUG_HEADER_LINE: usize = 8;

/// Rows carrying a collapsed-fold indicator in the gutter's first column.
fn collapsed_fold_rows(harness: &EditorTestHarness) -> Vec<usize> {
    let (start, end) = harness.content_area_rows();
    (start..=end)
        .filter(|row| {
            harness
                .get_cell(0, *row as u16)
                .as_deref()
                .map(|cell| cell == "▸")
                .unwrap_or(false)
        })
        .collect()
}

/// Byte range covering exactly `line` (its text plus its newline).
fn line_byte_range(harness: &mut EditorTestHarness, line: usize) -> (usize, usize) {
    let buffer = &mut harness.editor_mut().active_state_mut().buffer;
    let start = buffer.line_start_offset(line).expect("line exists");
    let end = buffer
        .line_start_offset(line + 1)
        .unwrap_or_else(|| buffer.len());
    (start, end)
}

/// Age the session Fresh just wrote into the shape an older Fresh wrote:
/// line numbers only, no `header_text` — the field only arrived with the
/// issue #1568 fix, so every session file written before it looks like
/// this. The header is also slid one line up, onto the blank line, which is
/// where an external edit that dropped a line above the block leaves it.
///
/// Returns how many folds were aged, so the test can tell a real repro
/// from a session that never recorded one.
fn age_session_folds_to_blank_header(project_dir: &std::path::Path) -> usize {
    let mut workspace = Workspace::load(project_dir)
        .expect("session 1 wrote a workspace")
        .expect("session 1 wrote a workspace");
    let mut aged = 0;
    for split_state in workspace.split_states.values_mut() {
        for file_state in split_state.file_states.values_mut() {
            for fold in file_state.folds.iter_mut() {
                fold.header_text = None;
                fold.header_line = BLANK_LINE;
                fold.end_line = DEBUG_HEADER_LINE;
                aged += 1;
            }
        }
    }
    workspace.save().expect("aged workspace is writable");
    aged
}

/// A fold restored onto a blank line is stale by construction — no
/// fold-creation path ever puts a header there — and restoring it hides the
/// block opener that follows. It must be dropped, leaving `debug {` on
/// screen.
#[test]
fn test_stale_legacy_fold_on_blank_line_is_dropped_on_restore() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("niri");
    std::fs::create_dir(&project_dir).unwrap();

    let file_path = project_dir.join("config.kdl");
    std::fs::write(&file_path, NIRI_CONFIG).unwrap();

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // --- Session 1: fold the `debug` block so the session records a fold.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();
        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        let buffer_id = harness.editor().active_buffer();
        harness
            .editor_mut()
            .active_window_mut()
            .toggle_fold_at_line(buffer_id, DEBUG_HEADER_LINE);
        harness.render().unwrap();

        assert_eq!(
            collapsed_fold_rows(&harness).len(),
            1,
            "Precondition: the `debug` block should be collapsed. Screen:\n{}",
            harness.screen_to_string()
        );

        harness.shutdown(true).unwrap();
    }

    // --- Between sessions: age the session file into the pre-#1568 shape
    //     and slide its header onto the blank line above `debug {`.
    let aged = age_session_folds_to_blank_header(&project_dir);
    assert_eq!(
        aged, 1,
        "Precondition: session 1 must have persisted exactly the one fold \
         this test ages"
    );

    // --- Session 2: restore. The stale fold must not swallow `debug {`.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap();

        let _ = harness.startup(true, &[]).unwrap();
        harness.open_file(&file_path).unwrap();
        harness.render().unwrap();

        let screen = harness.screen_to_string();
        assert!(
            screen.contains("debug {"),
            "Issue #3031: `debug {{` disappeared from the buffer — a stale \
             fold restored onto the blank line above it hid the block \
             opener behind a placeholder. Screen:\n{}",
            screen
        );
        assert!(
            collapsed_fold_rows(&harness).is_empty(),
            "Issue #3031: the stale fold should have been dropped, not \
             restored. Screen:\n{}",
            screen
        );
    }
}

/// Defence in depth for the same symptom: whatever put a collapsed fold on
/// a blank header line (a stale session, marker drift after an edit, a
/// plugin calling `addFold`), the header row must still say so — line
/// number in the gutter and a `▸` to click — instead of showing a bare
/// `...` that reads as vanished text.
#[test]
fn test_collapsed_fold_on_blank_header_keeps_its_line_number_and_arrow() {
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    let fixture = TestFixture::new("config.kdl", NIRI_CONFIG).unwrap();
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Plant a fold that hides exactly the `debug {` line, which makes the
    // blank line above it the fold's header.
    let (start, end) = line_byte_range(&mut harness, DEBUG_HEADER_LINE);
    let buffer_id = harness.editor().active_buffer();
    assert!(harness
        .editor_mut()
        .active_window_mut()
        .add_fold(buffer_id, start, end, None));
    harness.render().unwrap();

    let header_row = layout::CONTENT_START_ROW + BLANK_LINE;
    let row_text = harness.get_row_text(header_row as u16);
    let screen = harness.screen_to_string();

    assert!(
        row_text.contains("..."),
        "Precondition: the fold placeholder should render on the blank \
         header row {}. Screen:\n{}",
        header_row,
        screen
    );
    assert!(
        row_text.contains(&(BLANK_LINE + 1).to_string()),
        "Issue #3031: the collapsed fold's header row lost its line \
         number, so the hidden line reads as text the editor replaced with \
         `...`. Row {} text: {:?}. Screen:\n{}",
        header_row,
        row_text,
        screen
    );
    assert_eq!(
        harness.get_cell(0, header_row as u16).as_deref(),
        Some("▸"),
        "Issue #3031: the collapsed fold's header row lost its fold arrow, \
         leaving no way to see — or click open — the fold hiding \
         `debug {{`. Screen:\n{}",
        screen
    );
}
