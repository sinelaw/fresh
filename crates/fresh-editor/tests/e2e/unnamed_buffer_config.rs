//! Every unnamed buffer must follow the buffer-level `editor.*` settings, not
//! only the one Ctrl+N creates.
//!
//! `EditorState::new` takes no config: the state it returns highlights
//! occurrences and carries `BufferSettings::default()` (tab size, auto-close,
//! whitespace indicators, …) until something stamps the config onto it. A file
//! is stamped as it opens, and `new_buffer` stamped Ctrl+N's buffer, but the
//! other paths that create an unnamed buffer stamped only part of it or none:
//!
//! - an unnamed buffer restored by hot exit (`create_unnamed_recovery_buffer`)
//!   got neither the occurrence-highlight setting nor the buffer settings;
//! - the boot-time scratch buffer (`Editor::new`) got the buffer settings but
//!   not the occurrence-highlight setting;
//! - a later window's scratch buffer (`build_fresh_layout_if_needed`,
//!   `Window::seed_initial_layout`) got neither.
//!
//! #3426 fixed the same miss for the *view* state of these buffers
//! (`BufferViewState`: current-line highlight, line numbers, rulers); this is
//! its buffer-state half.
//!
//! Each setting is flipped away from the built-in default, so a buffer that
//! missed the stamp is unmistakable. What is checked is what the user sees:
//! the "(Current Buffer)" occurrence toggle reports the state it flips *to*, so
//! with `highlight_occurrences: false` a stamped buffer answers "enabled" and
//! an unstamped one "disabled"; and with `auto_close: false` typing `(` leaves
//! no `)` behind it.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, LineEndingOption};
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

/// Wide enough for the toggle's status message to be drawn in full.
const WIDTH: u16 = 200;

/// Both settings away from what `EditorState::new` leaves.
fn config() -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.editor.highlight_occurrences = false;
    config.editor.auto_close = false;
    config
}

fn run_command(harness: &mut EditorTestHarness, name: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text(name).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// The active buffer follows both settings, as seen on screen. Both are
/// checked before either is asserted, so a buffer that misses the whole stamp
/// reports both settings rather than only the first.
fn assert_active_buffer_follows_config(harness: &mut EditorTestHarness, which: &str) {
    // `auto_close: false`: the bracket stays unpaired. Typed at the end of the
    // buffer, where auto-close would otherwise pair it.
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text(" call(").unwrap();
    harness.render().unwrap();
    let typed = harness.screen_to_string();
    let bracket_unpaired = typed.contains(" call(") && !typed.contains(" call()");

    // `highlight_occurrences: false`: the per-buffer toggle turns it *on*.
    run_command(harness, "Toggle Occurrence Highlight (Current Buffer)");
    let toggled = harness.screen_to_string();
    let highlight_was_off = toggled.contains("Occurrence highlight enabled (this buffer)");

    assert!(
        bracket_unpaired && highlight_was_off,
        "{which}:\n\
         - auto_close: false leaves `(` unpaired: {bracket_unpaired}\n\
         - highlight_occurrences: false (the toggle turns it on): {highlight_was_off}\n\
         Screen after typing:\n{typed}\nScreen after the toggle:\n{toggled}"
    );
}

#[test]
fn restored_unnamed_buffer_follows_buffer_config() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir).unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());
    let harness_for = || {
        EditorTestHarness::create(
            WIDTH,
            24,
            HarnessOptions::new()
                .with_config(config())
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )
        .unwrap()
    };

    // Session 1: an unnamed buffer with content, kept for hot exit.
    {
        let mut harness = harness_for();
        harness.editor_mut().set_session_mode(true);
        harness.new_buffer().unwrap();
        harness.type_text("scratch notes").unwrap();
        harness.render().unwrap();
        harness.shutdown(true).unwrap();
    }

    // Session 2: the restored buffer follows the config.
    {
        let mut harness = harness_for();
        assert!(
            harness.startup(true, &[]).unwrap(),
            "the saved workspace should restore"
        );
        harness.render().unwrap();
        harness.assert_screen_contains("scratch notes");
        assert_active_buffer_follows_config(&mut harness, "restored unnamed buffer");
    }
}

#[test]
fn initial_scratch_buffer_follows_buffer_config() {
    let mut harness = EditorTestHarness::with_config(WIDTH, 24, config()).unwrap();
    harness.render().unwrap();
    assert_active_buffer_follows_config(&mut harness, "initial scratch buffer");
}

#[test]
fn new_window_scratch_buffer_follows_buffer_config() {
    let mut harness = EditorTestHarness::with_config(WIDTH, 24, config()).unwrap();

    // A window at a root with no saved workspace: the restore never runs, so
    // the fresh layout's seed is what the user sees.
    let extra_root = tempfile::tempdir().unwrap();
    let second = harness
        .editor_mut()
        .create_window_at(extra_root.path().to_path_buf(), "second".to_string());
    harness.editor_mut().set_active_window(second);
    harness.render().unwrap();
    assert_active_buffer_follows_config(&mut harness, "new window's scratch buffer");
}

/// The boot-time scratch buffer also takes `editor.default_line_ending`, as
/// Ctrl+N's buffer and every other unnamed buffer already did: `Editor::new`
/// stamped line numbers and the buffer settings but not the line ending, so
/// the first buffer of a session was LF whatever the config said.
#[test]
fn initial_scratch_buffer_takes_the_default_line_ending() {
    let mut config = Config::default();
    config.editor.default_line_ending = LineEndingOption::Crlf;
    let mut harness = EditorTestHarness::with_config(WIDTH, 24, config).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("CRLF");
}
