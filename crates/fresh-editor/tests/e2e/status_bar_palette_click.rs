//! E2E tests: clicking the "Palette: <shortcut>" hint on the status bar
//! opens Quick Open, same as pressing the shortcut (#3250).

use crate::common::harness::EditorTestHarness;
use tempfile::TempDir;

#[test]
fn test_palette_hint_click_opens_palette() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    std::fs::write(&file_path, "hello world").unwrap();

    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Before the click, Quick Open's hint line must not be on screen, so
    // the assertion below can only pass because of the click.
    let before = harness.screen_to_string();
    assert!(
        !before.contains(">command"),
        "Quick Open should not be open before the click. Screen:\n{}",
        before
    );

    let (col, row) = harness
        .find_text_on_screen("Palette:")
        .expect("Palette hint should be visible in status bar");
    harness.mouse_click(col, row).unwrap();

    // Quick Open shows its ">command" prefix hint once it is open.
    harness.assert_screen_contains(">command");
}
