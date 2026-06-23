// E2E tests for the `editor.auto_read_only` config option (issue #2048).
//
// By default, files that are not writable on disk and files in library/vendor
// directories (node_modules, rustup toolchains, ...) open in read-only mode.
// Setting `auto_read_only: false` disables that automatic detection so such
// files always open editable. Binary files open read-only regardless.
//
// Read-only state is asserted through rendered behavior: keystrokes into a
// read-only buffer are dropped and surface the "Editing disabled in this
// buffer" status message; in an editable buffer the typed text renders.

use crate::common::harness::EditorTestHarness;
use fresh::config::Config;
use tempfile::TempDir;

#[cfg(unix)]
use std::fs::Permissions;
#[cfg(unix)]
use std::os::unix::fs::PermissionsExt;

const EDITING_DISABLED_MSG: &str = "Editing disabled in this buffer";

/// Persistent read-only indicator rendered in the status bar (issue #2309).
const READ_ONLY_INDICATOR: &str = "[RO]";

/// A file without write permission opens read-only by default: keystrokes are
/// dropped and the "editing disabled" status message appears.
#[test]
#[cfg(unix)]
fn test_unwritable_file_opens_read_only_by_default() {
    // Root (uid 0) bypasses Unix file permission checks, so writability
    // detection is meaningless when running as root.
    if unsafe { libc::getuid() } == 0 {
        eprintln!("Skipping test: root bypasses file permission checks");
        return;
    }

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("readonly.txt");
    std::fs::write(&file_path, "original content\n").unwrap();
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o444)).unwrap();

    let mut harness = EditorTestHarness::with_config(160, 24, Config::default()).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("original content");

    // Typing must be dropped: the marker never appears, and the status bar
    // surfaces the editing-disabled message.
    harness.type_text("ZZTYPEDZZ").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Restore permissions before assertions (cleanup)
    let _ = std::fs::set_permissions(&file_path, Permissions::from_mode(0o644));

    assert!(
        !screen.contains("ZZTYPEDZZ"),
        "Editing should be disabled for unwritable files by default. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains(EDITING_DISABLED_MSG),
        "Blocked keystroke should surface the editing-disabled message. Screen:\n{}",
        screen
    );
}

/// With `auto_read_only: false`, a file without write permission opens
/// editable: keystrokes land in the buffer.
#[test]
#[cfg(unix)]
fn test_auto_read_only_disabled_opens_unwritable_file_editable() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("readonly.txt");
    std::fs::write(&file_path, "original content\n").unwrap();
    std::fs::set_permissions(&file_path, Permissions::from_mode(0o444)).unwrap();

    let mut config = Config::default();
    config.editor.auto_read_only = false;

    let mut harness = EditorTestHarness::with_config(160, 24, config).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("original content");

    harness.type_text("ZZTYPEDZZ").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();

    // Restore permissions before assertions (cleanup)
    let _ = std::fs::set_permissions(&file_path, Permissions::from_mode(0o644));

    assert!(
        screen.contains("ZZTYPEDZZ"),
        "Typing should work when auto_read_only is off. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains(EDITING_DISABLED_MSG),
        "No editing-disabled message expected when auto_read_only is off. Screen:\n{}",
        screen
    );
}

/// A read-only buffer shows the persistent `[RO]` status-bar indicator, while
/// an editable buffer does not (issue #2309). The indicator is its own status
/// segment — it must appear in the default layout, which omits `{filename}`.
#[test]
fn test_read_only_buffer_shows_ro_status_indicator() {
    let temp_dir = TempDir::new().unwrap();

    // Library file: opens read-only by default.
    let lib_dir = temp_dir.path().join("node_modules").join("somelib");
    std::fs::create_dir_all(&lib_dir).unwrap();
    let ro_file = lib_dir.join("index.js");
    std::fs::write(&ro_file, "library content\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        24,
        Config::default(),
        temp_dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(&ro_file).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(READ_ONLY_INDICATOR),
        "Read-only buffer should show a persistent [RO] status indicator. Screen:\n{}",
        screen
    );
}

/// An editable buffer shows no `[RO]` indicator (issue #2309): the segment is
/// only present while the buffer is actually read-only.
#[test]
fn test_editable_buffer_has_no_ro_status_indicator() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("editable.txt");
    std::fs::write(&file_path, "editable content\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        24,
        Config::default(),
        temp_dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(READ_ONLY_INDICATOR),
        "Editable buffer should not show the [RO] status indicator. Screen:\n{}",
        screen
    );
}

/// Locate a substring on the rendered screen, returning `(col, row)` of its
/// first character. Columns are counted as grapheme cells (the status bar is
/// ASCII here, so char count == display column).
fn find_on_screen(screen: &str, needle: &str) -> Option<(u16, u16)> {
    for (row, line) in screen.lines().enumerate() {
        if let Some(byte_idx) = line.find(needle) {
            let col = line[..byte_idx].chars().count();
            return Some((col as u16, row as u16));
        }
    }
    None
}

/// Clicking the `[RO]` indicator opens the read-only menu, and choosing
/// "Enable editing" makes the buffer editable (issue #2309 follow-up). Drives
/// a real mouse click + keypress and asserts only on rendered output.
#[test]
fn test_read_only_indicator_click_opens_menu_and_enables_editing() {
    use crossterm::event::{KeyCode, KeyModifiers};

    let temp_dir = TempDir::new().unwrap();
    let lib_dir = temp_dir.path().join("node_modules").join("somelib");
    std::fs::create_dir_all(&lib_dir).unwrap();
    let ro_file = lib_dir.join("index.js");
    std::fs::write(&ro_file, "library content\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        24,
        Config::default(),
        temp_dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(&ro_file).unwrap();
    harness.render().unwrap();

    // The indicator is on the status bar; find it and click it.
    let screen = harness.screen_to_string();
    let (ro_col, ro_row) =
        find_on_screen(&screen, READ_ONLY_INDICATOR).expect("[RO] indicator should be visible");
    harness.mouse_click(ro_col + 1, ro_row).unwrap();
    harness.render().unwrap();

    // The read-only menu should be open with an "Enable editing" action.
    harness.assert_screen_contains("Enable editing");

    // Confirm the (pre-selected) "Enable editing" row.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Editing is now enabled: the [RO] indicator is gone and typed text lands.
    harness.type_text("ZZTYPEDZZ").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("ZZTYPEDZZ"),
        "After enabling editing from the [RO] menu, typing should work. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains(EDITING_DISABLED_MSG),
        "Editing should no longer be disabled after the menu enabled it. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains(READ_ONLY_INDICATOR),
        "[RO] indicator should disappear once editing is enabled. Screen:\n{}",
        screen
    );
}

/// A file in a library directory (node_modules) opens read-only by default.
#[test]
fn test_library_file_opens_read_only_by_default() {
    let temp_dir = TempDir::new().unwrap();
    let lib_dir = temp_dir.path().join("node_modules").join("somelib");
    std::fs::create_dir_all(&lib_dir).unwrap();
    let file_path = lib_dir.join("index.js");
    std::fs::write(&file_path, "library content\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        24,
        Config::default(),
        temp_dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("library content");

    harness.type_text("ZZTYPEDZZ").unwrap();
    harness.render().unwrap();
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("ZZTYPEDZZ"),
        "Editing should be disabled for library files by default. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains(EDITING_DISABLED_MSG),
        "Blocked keystroke should surface the editing-disabled message. Screen:\n{}",
        screen
    );
}

/// With `auto_read_only: false`, a file in a library directory opens editable.
#[test]
fn test_auto_read_only_disabled_opens_library_file_editable() {
    let temp_dir = TempDir::new().unwrap();
    let lib_dir = temp_dir.path().join("node_modules").join("somelib");
    std::fs::create_dir_all(&lib_dir).unwrap();
    let file_path = lib_dir.join("index.js");
    std::fs::write(&file_path, "library content\n").unwrap();

    let mut config = Config::default();
    config.editor.auto_read_only = false;

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        24,
        config,
        temp_dir.path().to_path_buf(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("library content");

    harness.type_text("ZZTYPEDZZ").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("ZZTYPEDZZ");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(EDITING_DISABLED_MSG),
        "No editing-disabled message expected when auto_read_only is off. Screen:\n{}",
        screen
    );
}
