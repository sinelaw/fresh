//! E2E tests for the GUI backend.
//!
//! These tests exercise the GUI module's file-location parsing, coordinate
//! helpers, and headless binary launch without requiring direct construction
//! of winit-internal types.  Input-translation tests that need
//! `winit::event::KeyEvent` live in the unit-test module inside
//! `gui/mod.rs` (where the private `platform_specific` field is accessible).

use crate::common::harness::EditorTestHarness;
use std::path::PathBuf;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// File-location parsing
// ---------------------------------------------------------------------------

#[test]
fn test_parse_file_location_plain_path() {
    let (path, line, col) = fresh::gui::parse_file_location("src/main.rs");
    assert_eq!(path, PathBuf::from("src/main.rs"));
    assert_eq!(line, None);
    assert_eq!(col, None);
}

#[test]
fn test_parse_file_location_with_line() {
    let (path, line, col) = fresh::gui::parse_file_location("src/main.rs:42");
    assert_eq!(path, PathBuf::from("src/main.rs"));
    assert_eq!(line, Some(42));
    assert_eq!(col, None);
}

#[test]
fn test_parse_file_location_with_line_and_col() {
    let (path, line, col) = fresh::gui::parse_file_location("src/main.rs:42:10");
    assert_eq!(path, PathBuf::from("src/main.rs"));
    assert_eq!(line, Some(42));
    assert_eq!(col, Some(10));
}

#[test]
fn test_parse_file_location_non_numeric_suffix() {
    // "foo:bar" — "bar" is not a number, so the whole thing is a plain path
    let (path, line, col) = fresh::gui::parse_file_location("foo:bar");
    assert_eq!(path, PathBuf::from("foo:bar"));
    assert_eq!(line, None);
    assert_eq!(col, None);
}

#[test]
fn test_parse_file_location_windows_drive() {
    // Windows-style path: C:\file.rs:10:5
    // rsplitn(3, ':') splits from the right: "5", "10", "C:\file.rs"
    let (path, line, col) = fresh::gui::parse_file_location(r"C:\file.rs:10:5");
    assert_eq!(path, PathBuf::from(r"C:\file.rs"));
    assert_eq!(line, Some(10));
    assert_eq!(col, Some(5));
}

// ---------------------------------------------------------------------------
// GUI-style editor initialization (uses same Editor::with_working_dir path)
// ---------------------------------------------------------------------------

#[test]
fn test_gui_editor_opens_file_and_renders() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("hello.rs");
    std::fs::write(&file_path, "fn main() {}").unwrap();

    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    harness.assert_buffer_content("fn main() {}");
    harness.assert_screen_contains("hello.rs");
}

#[test]
fn test_gui_editor_file_explorer_on_empty_args() {
    // When no files are passed, the GUI shows the file explorer.
    // Verify the harness editor renders without crashing.
    let mut harness = EditorTestHarness::new(80, 24).unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("[No Name]");
}

// ---------------------------------------------------------------------------
// pixel_to_cell / cell_dimensions_to_grid (integration-level)
// ---------------------------------------------------------------------------

#[test]
fn test_pixel_to_cell_typical_font() {
    // 24px font: cell_width ≈ 14.4, cell_height ≈ 28.8
    let cell_size = (14.4, 28.8);
    // Click at pixel (150, 60) → col 10, row 2
    let (col, row) = fresh::gui::pixel_to_cell((150.0, 60.0), cell_size);
    assert_eq!(col, 10);
    assert_eq!(row, 2);
}

#[test]
fn test_grid_dimensions_for_default_window() {
    let cell_size = (14.4, 28.8);
    let (cols, rows) = fresh::gui::cell_dimensions_to_grid(1280.0, 800.0, cell_size);
    // 1280/14.4 ≈ 88, 800/28.8 ≈ 27
    assert_eq!(cols, 88);
    assert_eq!(rows, 27);
}

// ---------------------------------------------------------------------------
// Headless GUI binary launch (requires Xvfb + mesa-vulkan-drivers in CI)
// ---------------------------------------------------------------------------

/// Launch the real `fresh --gui` binary headlessly, open a file, and verify
/// it starts and shuts down cleanly.  On CI this runs under `xvfb-run` with
/// lavapipe (software Vulkan).  Locally it needs a `DISPLAY` (or Wayland).
///
/// The test is skipped when no display server is available.
#[test]
#[cfg(unix)]
fn test_gui_headless_launch_and_quit() {
    use std::process::Command;

    // Skip if no display and not running under xvfb-run
    if std::env::var("DISPLAY").is_err() && std::env::var("WAYLAND_DISPLAY").is_err() {
        eprintln!("SKIP: no DISPLAY/WAYLAND_DISPLAY — run under xvfb-run to enable");
        return;
    }

    let binary = env!("CARGO_BIN_EXE_fresh");

    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test_gui.txt");
    std::fs::write(&file_path, "hello gui").unwrap();

    // Launch the GUI, opening our test file with --no-session to avoid workspace state
    let mut child = Command::new(binary)
        .args(["--gui", "--no-session", file_path.to_str().unwrap()])
        .env("WGPU_BACKEND", "vulkan")
        .env("LIBGL_ALWAYS_SOFTWARE", "1")
        .spawn()
        .expect("failed to launch fresh --gui");

    // Give the window time to create and render a frame
    std::thread::sleep(std::time::Duration::from_secs(3));

    // The process should still be alive (it's waiting for events)
    assert!(
        child.try_wait().unwrap().is_none(),
        "GUI process exited prematurely"
    );

    // Send SIGTERM for a clean shutdown
    unsafe {
        libc::kill(child.id() as i32, libc::SIGTERM);
    }

    let status = child.wait().expect("failed to wait on GUI process");
    // SIGTERM produces an exit-by-signal which is not "success" on Unix,
    // but is a normal termination.
    use std::os::unix::process::ExitStatusExt;
    let signal = status.signal();
    // Accept: clean exit (code 0) OR terminated by SIGTERM (15)
    assert!(
        status.success() || signal == Some(libc::SIGTERM),
        "GUI process terminated abnormally: {:?}",
        status
    );
}
