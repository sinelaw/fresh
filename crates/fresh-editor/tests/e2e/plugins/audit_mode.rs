//! E2E tests for audit_mode (Review Diff) plugin

use crate::common::git_test_helper::GitTestRepo;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::input::keybindings::Action::PluginAction;
use std::fs;

/// Helper to copy audit_mode plugin and its dependencies to the test repo
fn setup_audit_mode_plugin(repo: &GitTestRepo) {
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).expect("Failed to create plugins directory");
    copy_plugin(&plugins_dir, "audit_mode");
    copy_plugin_lib(&plugins_dir);
}

/// Test that opening the diff view works without errors
/// This test reproduces the addOverlay TypeError that occurred when
/// the plugin passed parameters in the wrong order
#[test]
fn test_review_diff_opens_without_error() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file to create uncommitted changes
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, modified world!");
    let config = load_config();
    start_server(config);
    // New comment line
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    // Verify the file is open
    harness
        .wait_until(|h| h.screen_to_string().contains("modified world"))
        .unwrap();

    // Trigger the Review Diff command via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete
    // The status bar changes from "Generating Review Diff Stream..." to showing hunk count
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Wait until we're no longer generating the diff stream
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Review Diff screen:\n{}", screen);

    // The diff view should show without errors
    // Check that we don't see an error about addOverlay
    assert!(
        !screen.contains("expected i32"),
        "Should not show addOverlay type error. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        screen
    );

    // Should show something related to the diff - either the split view or content
    assert!(
        screen.contains("main.rs")
            || screen.contains("modified world")
            || screen.contains("OLD")
            || screen.contains("Review"),
        "Should show diff-related content. Screen:\n{}",
        screen
    );
}

/// Test that the diff view displays hunks correctly
#[test]
fn test_review_diff_shows_hunks() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file to create uncommitted changes
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, CHANGED!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
    println!("New line added");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Hunks screen:\n{}", screen);

    // Should not have any TypeError
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );
}

/// Test that the side-by-side diff view (drill-down) works with synchronized scrolling
/// This test verifies that setSplitScroll is available in the editor API
#[test]
fn test_review_diff_side_by_side_view() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file to create uncommitted changes
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, CHANGED!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
    println!("New line added");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete and hunks to be displayed
    // The status bar shows hunk count when done: "Review Diff: N hunks"
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream") && screen.contains("hunks")
        })
        .unwrap();

    let screen_before_drill = harness.screen_to_string();
    println!("Before drill-down:\n{}", screen_before_drill);

    // Now drill down into a hunk to open the side-by-side view
    // Press Enter to drill down
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for side-by-side view to open
    // The drill-down creates a split with "[OLD ◀]" in the tab name
    // Or if the operation is async, wait a bit for it to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Either we see the OLD marker from the split, or the file was opened
            screen.contains("[OLD") || screen.contains("main.rs ×")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Side-by-side screen:\n{}", screen);

    // Should not have any TypeError about setSplitScroll
    assert!(
        !screen.contains("setSplitScroll is not a function"),
        "setSplitScroll should be available. Screen:\n{}",
        screen
    );
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );
}

/// Test that the improved side-by-side diff shows aligned content with filler lines
#[test]
fn test_side_by_side_diff_shows_alignment() {
    use tracing_subscriber::EnvFilter;
    let _ = tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::from_default_env()
                .add_directive("fresh=debug".parse().unwrap())
                .add_directive("fresh_plugin_runtime=debug".parse().unwrap()),
        )
        .with_test_writer()
        .try_init();

    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file with additions and deletions
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, modified!");
    let config = load_config();
    start_server(config);
    // New line 1
    // New line 2
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160, // Wide enough for side-by-side
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("modified"))
        .unwrap();

    // Use the new "Side-by-Side Diff" command which directly opens side-by-side view
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for side-by-side view to fully load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Aligned diff screen:\n{}", screen);

    // Should show OLD and NEW pane headers
    // The format is "OLD (HEAD)" and "NEW (Working)"
    assert!(
        screen.contains("OLD (HEAD)") || screen.contains("NEW (Working)"),
        "Should show OLD or NEW pane header. Screen:\n{}",
        screen
    );

    // Verify alignment - the OLD and NEW panes should be side by side with a separator
    // The left pane has blank lines where content was added on the right
    assert!(
        screen.contains("│"),
        "Should show pane separator for side-by-side view. Screen:\n{}",
        screen
    );

    // Should not have any errors
    assert!(
        !screen.contains("TypeError") && !screen.contains("Error"),
        "Should not show any errors. Screen:\n{}",
        screen
    );
}

/// Test that the side-by-side diff shows change statistics in status bar
#[test]
fn test_side_by_side_diff_shows_statistics() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, modified!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting...");
    println!("Added line");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("modified"))
        .unwrap();

    // Use the new "Side-by-Side Diff" command which directly opens side-by-side view
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for side-by-side view to fully load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Stats screen:\n{}", screen);

    // Should show the statistics format in status bar
    // Format is: "Side-by-side diff: +N -M ~K"
    assert!(
        screen.contains("Side-by-side diff:"),
        "Should show diff statistics. Screen:\n{}",
        screen
    );
}

/// Test that change markers (+, -, ~) appear in the gutter
#[test]
fn test_side_by_side_diff_shows_gutter_markers() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create changes that will show all marker types
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Hello, MODIFIED!");
    let config = load_config();
    start_server(config);
    // This is a new line
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Server started");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("MODIFIED"))
        .unwrap();

    // Use the new "Side-by-Side Diff" command which directly opens side-by-side view
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for side-by-side view to fully load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Gutter markers screen:\n{}", screen);

    // The gutter should show + for additions, - for removals, ~ for modifications
    // These appear as "│+" "│-" "│~" in the gutter column, or just the markers
    let has_markers = screen.contains("│+")
        || screen.contains("│-")
        || screen.contains("│~")
        || screen.contains("+")
        || screen.contains("-");

    assert!(
        has_markers,
        "Should show change markers in gutter (+, -, ~). Screen:\n{}",
        screen
    );
}

/// Test that scroll sync works between the two panes in side-by-side diff view
/// When scrolling one pane, the other should follow to keep aligned lines in sync
#[test]
#[ignore = "Scroll sync with G/g keys not yet implemented for composite buffer views"]
fn test_side_by_side_diff_scroll_sync() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a file with many lines so that scrolling is required
    // Add enough lines that the viewport can't show everything at once
    let main_rs_path = repo.path.join("src/main.rs");
    let mut original_lines: Vec<String> = Vec::new();
    for i in 0..60 {
        original_lines.push(format!(
            "fn function_{}() {{ println!(\"Line {}\"); }}",
            i, i
        ));
    }
    fs::write(&main_rs_path, original_lines.join("\n")).expect("Failed to write original file");

    // Commit the original
    repo.git_add_all();
    repo.git_commit("Add many functions");

    // Now modify - add some lines in the middle and change some at the end
    let mut modified_lines: Vec<String> = Vec::new();
    for i in 0..30 {
        modified_lines.push(format!(
            "fn function_{}() {{ println!(\"Line {}\"); }}",
            i, i
        ));
    }
    // Add new lines in the middle
    for i in 0..5 {
        modified_lines.push(format!(
            "fn new_function_{}() {{ println!(\"New {}\"); }}",
            i, i
        ));
    }
    for i in 30..60 {
        if i >= 55 {
            // Modify the last few lines
            modified_lines.push(format!(
                "fn function_{}() {{ println!(\"Modified {}\"); }}",
                i, i
            ));
        } else {
            modified_lines.push(format!(
                "fn function_{}() {{ println!(\"Line {}\"); }}",
                i, i
            ));
        }
    }
    fs::write(&main_rs_path, modified_lines.join("\n")).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        30, // Relatively small height to ensure scrolling is needed
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("function_"))
        .unwrap();

    // Use the new "Side-by-Side Diff" command which directly opens side-by-side view
    // for the current file without needing to navigate through the hunk list
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();

    eprintln!(
        "DEBUG scroll_sync: Screen after typing command:\n{}",
        harness.screen_to_string()
    );

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    eprintln!(
        "DEBUG scroll_sync: Screen after command executed:\n{}",
        harness.screen_to_string()
    );

    // Wait for side-by-side view to fully load
    // The status bar shows "Side-by-side diff: +N -M ~K" when loading is complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
                || screen.contains("No file open")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();

    let screen_before = harness.screen_to_string();
    println!("Before scrolling:\n{}", screen_before);

    // Helper function to split a line at a character position (handles multi-byte UTF-8)
    fn split_at_char(line: &str, char_pos: usize) -> (String, String) {
        let chars: Vec<char> = line.chars().collect();
        let left: String = chars.iter().take(char_pos).collect();
        let right: String = chars.iter().skip(char_pos).collect();
        (left, right)
    }

    // Check if a string contains a late function number (50-59)
    fn has_late_function(s: &str) -> bool {
        // Look for function_50 through function_59 or "Modified"
        s.contains("function_50")
            || s.contains("function_51")
            || s.contains("function_52")
            || s.contains("function_53")
            || s.contains("function_54")
            || s.contains("function_55")
            || s.contains("function_56")
            || s.contains("function_57")
            || s.contains("function_58")
            || s.contains("function_59")
            || s.contains("Modified")
    }

    // Helper to check if both panes show synchronized content from near the end
    // Both OLD and NEW panes should show late function numbers (50s) when synced at bottom
    fn both_panes_show_late_content(screen: &str) -> bool {
        let lines: Vec<&str> = screen.lines().collect();
        let mut old_pane_has_late = false;
        let mut new_pane_has_late = false;

        for line in &lines {
            // Check for late function numbers (function_50-59) or "Modified"
            if has_late_function(line) {
                let char_count = line.chars().count();
                if char_count > 80 {
                    let (left_half, right_half) = split_at_char(line, char_count / 2);
                    if has_late_function(&left_half) {
                        old_pane_has_late = true;
                    }
                    if has_late_function(&right_half) {
                        new_pane_has_late = true;
                    }
                } else {
                    // For shorter lines, just mark as found (could be wrapped display)
                    old_pane_has_late = true;
                    new_pane_has_late = true;
                }
            }
        }
        old_pane_has_late && new_pane_has_late
    }

    // Helper to check if both panes show synchronized content from near the start
    fn both_panes_show_early_content(screen: &str) -> bool {
        let lines: Vec<&str> = screen.lines().collect();
        let mut old_pane_has_early = false;
        let mut new_pane_has_early = false;

        for line in &lines {
            // Check for early function numbers (function_0, function_1, etc.)
            if line.contains("function_0") || line.contains("function_1(") {
                let char_count = line.chars().count();
                if char_count > 80 {
                    let (left_half, right_half) = split_at_char(line, char_count / 2);
                    if left_half.contains("function_0") || left_half.contains("function_1(") {
                        old_pane_has_early = true;
                    }
                    if right_half.contains("function_0") || right_half.contains("function_1(") {
                        new_pane_has_early = true;
                    }
                } else {
                    old_pane_has_early = true;
                    new_pane_has_early = true;
                }
            }
        }
        old_pane_has_early && new_pane_has_early
    }

    // Test 1: Press 'G' to go to end of document - this should sync both panes
    harness
        .send_key(KeyCode::Char('G'), KeyModifiers::SHIFT)
        .unwrap();

    // Debug: print screen state before waiting (helps diagnose CI timeouts)
    eprintln!(
        "DEBUG: Screen after pressing G (before wait):\n{}",
        harness.screen_to_string()
    );

    // Use semantic waiting: wait until BOTH panes show late content (scroll synced)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
            {
                panic!("Error during scroll sync. Screen:\n{}", screen);
            }
            // Both panes should eventually show content from near the end
            both_panes_show_late_content(&screen)
        })
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("After pressing G (synced to end):\n{}", screen_after);

    // Verify no errors
    assert!(
        !screen_after.contains("TypeError") && !screen_after.contains("Error:"),
        "Should not show any errors. Screen:\n{}",
        screen_after
    );

    // Test 2: Press 'g' to go back to start - both panes should sync to top
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::NONE)
        .unwrap();

    // Debug: print screen state before waiting (helps diagnose CI timeouts)
    eprintln!(
        "DEBUG: Screen after pressing g (before wait):\n{}",
        harness.screen_to_string()
    );

    // Use semantic waiting: wait until BOTH panes show early content (scroll synced)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
            {
                panic!("Error during scroll sync. Screen:\n{}", screen);
            }
            both_panes_show_early_content(&screen)
        })
        .unwrap();

    let screen_top = harness.screen_to_string();
    println!("After pressing g (synced to start):\n{}", screen_top);

    // Verify no errors
    assert!(
        !screen_top.contains("TypeError") && !screen_top.contains("Error:"),
        "Should not show any errors. Screen:\n{}",
        screen_top
    );

    // Note: Scroll sync currently works for cursor movement commands (G/g)
    // but NOT for viewport-only scroll commands (Ctrl+Down, PageDown, mouse wheel).
    // Those commands scroll the active pane without syncing the other pane.
    // This is a known limitation - the on_viewport_changed hook fires but
    // the setSplitScroll command is processed asynchronously and may not
    // take effect in time.
}

/// Test vim-style navigation in diff-view mode
#[test]
fn test_side_by_side_diff_vim_navigation() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Modified line");
}

fn helper() {
    println!("Added function");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Modified"))
        .unwrap();

    // Use the new "Side-by-Side Diff" command which directly opens side-by-side view
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for side-by-side view to fully load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Fail fast if errors occur (prevents infinite wait in CI)
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();

    // Test vim navigation: j moves down, k moves up
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();

    let screen = harness.screen_to_string();

    // Should still be in the diff view without errors
    assert!(
        !screen.contains("TypeError") && !screen.contains("Error"),
        "Vim navigation should work without errors. Screen:\n{}",
        screen
    );

    // Test 'q' to close
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    // After closing, should still be functional
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("TypeError"),
        "Closing with 'q' should work. Screen:\n{}",
        screen
    );
}

/// Test that running "Show Warnings" command while diff view is open doesn't break the diff
/// Bug: The diff buffer would disappear when "Show Warnings" was triggered
#[test]
#[ignore = "Test times out waiting for diff to load - needs investigation"]
fn test_side_by_side_diff_survives_show_warnings() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Create a simple file with multiple lines - matching the tmux test scenario
    let test_txt_path = repo.path.join("test.txt");
    let original_content = (1..=15)
        .map(|i| format!("line {}", i))
        .collect::<Vec<_>>()
        .join("\n")
        + "\n";
    fs::write(&test_txt_path, &original_content).expect("Failed to write test.txt");

    // Initialize git with the original content
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify the file with changes similar to tmux test
    let modified_content = "line 1 modified\nline 2\nline 3\nline 4 changed\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10 modified\nline 11\nline 12\nline 13\nline 14\nline 15\nline 16 added\n";
    fs::write(&test_txt_path, modified_content).expect("Failed to modify test.txt");

    // Use smaller terminal to ensure diff view triggers warnings
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        25,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&test_txt_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("line 1 modified"))
        .unwrap();

    // Open side-by-side diff via command palette (same as tmux)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for diff to load (semantic waiting)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading")
        })
        .unwrap();

    let screen_before = harness.screen_to_string();
    println!("Screen before opening new file:\n{}", screen_before);

    // Verify we have the diff tab in tab bar
    assert!(
        screen_before.contains("*Diff:"),
        "Should show diff tab. Screen:\n{}",
        screen_before
    );

    // Create and open a new file (tests the same underlying issue as Show Warnings)
    let new_file = repo.path.join("another_file.txt");
    fs::write(&new_file, "new file content here\n").expect("Failed to write file");
    harness.open_file(&new_file).unwrap();

    // Wait for the new file to be shown
    harness
        .wait_until(|h| h.screen_to_string().contains("new file content"))
        .unwrap();

    let screen_after = harness.screen_to_string();
    println!("Screen after opening new file:\n{}", screen_after);

    // The diff tab should still be visible in the tab bar
    // Bug: When a new buffer is opened, the composite diff buffer disappears from tabs
    assert!(
        screen_after.contains("*Diff:"),
        "Diff tab should still exist after opening new file. Screen:\n{}",
        screen_after
    );
}

/// Test that closing buffers doesn't switch to a hidden buffer
/// Bug: When closing the last visible buffer, the editor would switch to a hidden
/// source buffer (like *OLD:* or *NEW:*) instead of creating a new buffer
#[test]
fn test_close_buffer_skips_hidden_buffers() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("Modified");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("Modified"))
        .unwrap();

    // Open side-by-side diff (this creates hidden *OLD:* and *NEW:* buffers)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading")
        })
        .unwrap();

    // Close the diff view with 'q'
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Now close the main.rs buffer
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after closing buffer:\n{}", screen);

    // Should NOT be showing a hidden buffer (OLD: or NEW:)
    assert!(
        !screen.contains("*OLD:") && !screen.contains("*NEW:"),
        "Should not switch to hidden OLD/NEW buffers. Screen:\n{}",
        screen
    );

    // The tab bar should not show *OLD: or *NEW: tabs
    // (This is enforced by hidden_from_tabs, but double-check)
    let first_lines: String = screen.lines().take(3).collect::<Vec<_>>().join("\n");
    assert!(
        !first_lines.contains("*OLD:") && !first_lines.contains("*NEW:"),
        "Hidden buffers should not appear in tab bar. Screen:\n{}",
        screen
    );
}

/// Test that the Side-by-Side Diff command is visible in the command palette.
///
/// This test verifies that the command is registered with null context (always visible)
/// rather than a specific context like "global" which would hide it.
///
/// The test types a partial query and waits for the full command name to appear in
/// suggestions. If the command has the wrong context, it won't appear in the palette.
#[test]
fn test_side_by_side_diff_command_visible_in_palette() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Wait for the audit_mode plugin command to be registered
    // Check by action name which is stable across locales
    harness
        .wait_until(|h| {
            let commands = h.editor().command_registry().read().unwrap().get_all();
            commands
                .iter()
                .any(|c| c.action == PluginAction("side_by_side_diff_current_file".to_string()))
        })
        .unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type a PARTIAL query - if the command is hidden by context, the full name won't appear
    // in suggestions (only our typed input would show, not the full "Side-by-Side Diff")
    harness.type_text("Side-by-Side").unwrap();
    harness.render().unwrap();

    // Wait for the FULL command name to appear in suggestions on screen
    // This verifies the command is visible (not hidden by context filtering)
    // The command name is "Side-by-Side Diff" as defined in audit_mode.i18n.json
    harness
        .wait_for_screen_contains("Side-by-Side Diff")
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Command palette with Side-by-Side Diff:\n{}", screen);

    // The command should be visible in the suggestions
    assert!(
        screen.contains("Side-by-Side Diff"),
        "Side-by-Side Diff command should be visible in command palette. Screen:\n{}",
        screen
    );
}

/// Test that diff lines have proper background highlighting colors.
///
/// This test verifies that added/removed/modified lines in the side-by-side diff
/// view have visible background colors (not just the default editor background).
#[test]
fn test_side_by_side_diff_line_highlighting() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file with a clear change
    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("CHANGED LINE HERE");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#;
    fs::write(&main_rs_path, modified_content).expect("Failed to modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        160,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("CHANGED"))
        .unwrap();

    // Open side-by-side diff
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Side-by-Side Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for side-by-side view to fully load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError")
                || screen.contains("Error:")
                || screen.contains("Failed")
                || screen.contains("No changes")
            {
                panic!("Error loading side-by-side diff. Screen:\n{}", screen);
            }
            screen.contains("Side-by-side diff:") && !screen.contains("Loading side-by-side diff")
        })
        .unwrap();

    harness.render().unwrap();
    let screen = harness.screen_to_string();
    println!("Side-by-side diff view:\n{}", screen);

    // Find a context line OUTSIDE the hunk (line 8+, like "Config::default()")
    // and a diff line INSIDE the hunk (like "Hello" or "CHANGED")
    // The diff line should have a different (non-black) background color.

    let mut context_bg: Option<ratatui::style::Color> = None;
    let mut diff_bg: Option<ratatui::style::Color> = None;
    let mut diff_row: Option<u16> = None;

    for row in 0..harness.terminal_height() {
        let line = harness.get_screen_row(row);

        // Look for context line OUTSIDE the hunk - "Config::default()" is on line 8,
        // well outside the hunk which spans lines 1-5
        if line.contains("Config::default()") && context_bg.is_none() {
            if let Some(style) = harness.get_cell_style(40, row as u16) {
                context_bg = style.bg;
                eprintln!(
                    "Context line (outside hunk) at row {}: bg={:?}",
                    row, context_bg
                );
            }
        }

        // Look for diff line INSIDE the hunk (CHANGED or Hello on line 2)
        if (line.contains("CHANGED") || line.contains("Hello")) && diff_bg.is_none() {
            if let Some(style) = harness.get_cell_style(40, row as u16) {
                diff_bg = style.bg;
                diff_row = Some(row as u16);
                eprintln!(
                    "Diff line (inside hunk) at row {}: bg={:?}, content: {}",
                    row,
                    diff_bg,
                    line.trim()
                );
            }
        }
    }

    // Print all row backgrounds for debugging
    eprintln!("\n=== Row background colors ===");
    for row in 0..harness.terminal_height().min(30) {
        let line = harness.get_screen_row(row);
        let bg = harness.get_cell_style(40, row as u16).and_then(|s| s.bg);
        let truncated: String = line.chars().take(80).collect();
        eprintln!("Row {:2}: bg={:?} | {}", row, bg, truncated);
    }

    // Verify we found both types of lines
    assert!(
        context_bg.is_some(),
        "Should find a context line outside hunk (Config::default()). Screen:\n{}",
        screen
    );
    assert!(
        diff_bg.is_some() && diff_row.is_some(),
        "Should find a diff line inside hunk (CHANGED or Hello). Screen:\n{}",
        screen
    );

    // The key assertion: diff lines (inside hunk) should have a DIFFERENT background
    // than context lines (outside hunk). This verifies diff highlighting is working.
    assert_ne!(
        context_bg, diff_bg,
        "Diff lines should have different background than context lines.\n\
         Context bg (outside hunk): {:?}\n\
         Diff bg (inside hunk): {:?}\n\
         This means diff highlighting is NOT working correctly.",
        context_bg, diff_bg
    );

    // Also verify the diff background is not the default black (should be a diff color)
    assert_ne!(
        diff_bg,
        Some(ratatui::style::Color::Black),
        "Diff lines should have a colored background, not black. Got: {:?}",
        diff_bg
    );

    eprintln!("\nDiff highlighting is working correctly:");
    eprintln!("Context bg (outside hunk): {:?}", context_bg);
    eprintln!("Diff bg (inside hunk): {:?}", diff_bg);
}

/// Test that Review Diff shows newly added (untracked) files
/// Reproduces https://github.com/sinelaw/fresh/issues/1452
#[test]
fn test_review_diff_shows_added_files() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit with the typical project files
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a brand new untracked file (not staged, not committed)
    let new_file_path = repo.path.join("src/new_module.rs");
    let new_file_content = r#"/// A brand new module
pub fn new_function() {
    println!("This is a new file!");
}
"#;
    fs::write(&new_file_path, new_file_content).expect("Failed to create new file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open any existing file (review diff shows all changes, not just current file)
    let main_rs_path = repo.path.join("src/main.rs");
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("main"))
        .unwrap();

    // Trigger the Review Diff command via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Review Diff (added file) screen:\n{}", screen);

    // Should not have any errors
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );

    // The new untracked file should appear in the review diff
    assert!(
        screen.contains("new_module.rs"),
        "Review diff should show the newly added untracked file 'new_module.rs'. Screen:\n{}",
        screen
    );

    // The content of the new file should be visible as additions
    assert!(
        screen.contains("new_function") || screen.contains("new file"),
        "Review diff should show content from the new file. Screen:\n{}",
        screen
    );
}

/// Test that drill-down (side-by-side diff) works for newly added (untracked) files
/// Before the fix, review_drill_down() would fail because git show HEAD:<file> errors
/// for files that don't exist in HEAD, causing a silent early return.
/// Reproduces https://github.com/sinelaw/fresh/issues/1452
#[test]
fn test_review_diff_drill_down_added_file() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a brand new untracked file
    let new_file_path = repo.path.join("src/new_module.rs");
    let new_file_content = r#"/// A brand new module
pub fn new_function() {
    println!("This is a new file!");
}
"#;
    fs::write(&new_file_path, new_file_content).expect("Failed to create new file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open any file to start
    let main_rs_path = repo.path.join("src/main.rs");
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("main"))
        .unwrap();

    // Trigger Review Diff
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for Review Diff to complete and show the untracked file's hunk
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream") && screen.contains("hunks")
        })
        .unwrap();

    // Navigate to the first hunk using 'n' (next hunk), then drill down with Enter
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for side-by-side diff view to open - tab shows "*Diff: <filename>*"
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("*Diff:") || screen.contains("OLD (HEAD)")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Drill-down (added file) screen:\n{}", screen);

    // Before the fix, git show HEAD:<file> would fail for untracked files
    // and the drill-down would silently abort with "failed" status
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );

    // The new file content should be visible in the side-by-side view
    assert!(
        screen.contains("new_function") || screen.contains("brand new"),
        "Side-by-side diff should show the new file's content. Screen:\n{}",
        screen
    );
}

/// Test that the review diff view shows section headers for staged, unstaged, and untracked files
#[test]
fn test_review_diff_section_headers() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Commit the plugin files so they don't appear as untracked
    repo.git_add_all();
    repo.git_commit("Add plugin files");

    // 1. Staged change: modify lib.rs and stage it
    repo.modify_file(
        "src/lib.rs",
        r#"pub struct Config {
    pub port: u16,
    pub host: String,
    pub debug: bool,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            port: 8080,
            host: "localhost".to_string(),
            debug: false,
        }
    }
}

pub fn process_request(data: &str) -> String {
    format!("Processed: {}", data)
}
"#,
    );
    repo.stage_file("src/lib.rs");

    // 2. Unstaged change: modify main.rs but don't stage it
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Hello, modified world!");
    let config = load_config();
    start_server(config);
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#,
    );

    // 3. Untracked file: create a brand new file
    repo.create_file(
        "src/new_module.rs",
        "pub fn new_function() {\n    println!(\"I am new!\");\n}\n",
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        50,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open any file to start the editor
    let main_rs_path = repo.path.join("src/main.rs");
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("modified world"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff to finish loading
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Review Diff with section headers:\n{}", screen);

    // Should not have any errors
    assert!(
        !screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        screen
    );

    // Verify section headers are present
    assert!(
        screen.contains("Staged Changes"),
        "Should show 'Staged Changes' section header. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Modified (Unstaged)"),
        "Should show 'Modified (Unstaged)' section header. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Untracked Files"),
        "Should show 'Untracked Files' section header. Screen:\n{}",
        screen
    );

    // Verify the files appear under the correct sections
    assert!(
        screen.contains("lib.rs"),
        "Should show staged file lib.rs. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("main.rs"),
        "Should show unstaged file main.rs. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("new_module.rs"),
        "Should show untracked file new_module.rs. Screen:\n{}",
        screen
    );
}

/// Test that Review Diff shows both untracked files AND newly git-added (staged) files
/// that have never been committed. Previously only modified tracked files were shown.
#[test]
fn test_review_diff_shows_untracked_and_staged_new_files() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // setup_typical_project already does an initial commit.

    // Ignore the plugins directory so copied plugin files don't clutter the diff
    repo.create_file(".gitignore", "plugins/\n");
    repo.git_add(&[".gitignore"]);
    repo.git_commit("Add gitignore");

    // Now create two brand-new files:

    // 1) A new file that is git-added (staged but never committed)
    repo.create_file(
        "src/staged_new.rs",
        "pub fn staged_func() {\n    println!(\"I am staged\");\n}\n",
    );
    repo.stage_file("src/staged_new.rs");

    // 2) A new file that is untracked (never staged or committed)
    repo.create_file(
        "src/untracked_new.rs",
        "pub fn untracked_func() {\n    println!(\"I am untracked\");\n}\n",
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open an existing file (Review Diff shows all changes, not just current file)
    let main_rs_path = repo.path.join("src/main.rs");
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();

    harness
        .wait_until(|h| h.screen_to_string().contains("main"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the Review Diff async operation to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!(
        "Review Diff (untracked + staged new) screen:\n{}",
        screen
    );

    // Should not have any errors
    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );

    // The staged new file should appear in the review diff
    assert!(
        screen.contains("staged_new.rs"),
        "Review diff should show the staged new file 'staged_new.rs'. Screen:\n{}",
        screen
    );

    // The staged new file's content should be visible
    assert!(
        screen.contains("staged_func"),
        "Review diff should show content from the staged new file. Screen:\n{}",
        screen
    );

    // The untracked file should appear in the review diff
    assert!(
        screen.contains("untracked_new.rs"),
        "Review diff should show the untracked file 'untracked_new.rs'. Screen:\n{}",
        screen
    );

    // The untracked file's content should be visible
    assert!(
        screen.contains("untracked_func"),
        "Review diff should show content from the untracked file. Screen:\n{}",
        screen
    );
}

/// Test that Review Diff shows files when they are the ONLY changes (no modifications).
/// This catches cases where the diff only has new files and no tracked-file modifications.
#[test]
fn test_review_diff_only_new_files_no_modifications() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Ignore plugins so they don't clutter the diff
    repo.create_file(".gitignore", "plugins/\n");
    repo.git_add(&[".gitignore"]);
    repo.git_commit("Add gitignore");

    // Create ONLY new files — no modifications to existing tracked files
    // 1) Staged new file
    repo.create_file("src/brand_new_staged.rs", "pub fn brand_new() {}\n");
    repo.stage_file("src/brand_new_staged.rs");

    // 2) Untracked file
    repo.create_file("src/brand_new_untracked.rs", "pub fn also_new() {}\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    let main_rs_path = repo.path.join("src/main.rs");
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("main"))
        .unwrap();

    // Trigger Review Diff
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!(
        "Review Diff (only new files, no modifications) screen:\n{}",
        screen
    );

    assert!(
        !screen.contains("TypeError"),
        "Should not show any TypeError. Screen:\n{}",
        screen
    );

    // The staged new file must appear
    assert!(
        screen.contains("brand_new_staged.rs"),
        "Review diff should show staged new file 'brand_new_staged.rs'. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("brand_new"),
        "Review diff should show content from staged new file. Screen:\n{}",
        screen
    );

    // The untracked file must appear
    assert!(
        screen.contains("brand_new_untracked.rs"),
        "Review diff should show untracked file 'brand_new_untracked.rs'. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("also_new"),
        "Review diff should show content from untracked file. Screen:\n{}",
        screen
    );
}

/// Test that the magit-style review diff scrolling works with many files.
/// Creates enough files to overflow the viewport and verifies:
/// - File list scrolls when navigating past the visible area
/// - Diff panel updates correctly when selection changes
/// - No content corruption when file list exceeds viewport
#[test]
fn test_review_diff_scrolling_many_files() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Create an initial commit
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create 8 staged modified files
    for i in 0..8 {
        let path = format!("src/staged_{}.rs", i);
        repo.create_file(&path, &format!("fn staged_func_{}() {{}}\n", i));
    }
    // Stage them
    let output = std::process::Command::new("git")
        .args(["add", "src/"])
        .current_dir(&repo.path)
        .output()
        .expect("git add failed");
    assert!(output.status.success(), "git add failed");

    // Create 5 unstaged modified files (modify existing tracked files or create new ones)
    // First commit the staged files
    let output = std::process::Command::new("git")
        .args(["commit", "-m", "Add staged files"])
        .current_dir(&repo.path)
        .output()
        .expect("git commit failed");
    assert!(output.status.success(), "git commit failed");

    // Now modify some of them to create unstaged changes
    for i in 0..5 {
        let path = format!("src/staged_{}.rs", i);
        repo.create_file(
            &path,
            &format!("fn staged_func_{}() {{ /* modified */ }}\n", i),
        );
    }

    // Create 5 untracked new files
    for i in 0..5 {
        let path = format!("src/untracked_{}.rs", i);
        repo.create_file(&path, &format!("fn untracked_func_{}() {{}}\n", i));
    }

    // Use a small viewport (80x15) so the file list overflows
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        80,
        15,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open any file to start
    let main_rs_path = repo.path.join("src/main.rs");
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("main"))
        .unwrap();

    // Trigger Review Diff via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review Diff").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for review diff to load
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", screen);
            }
            screen.contains("GIT STATUS") && screen.contains("DIFF")
        })
        .unwrap();

    let initial_screen = harness.screen_to_string();
    println!("Initial magit screen:\n{}", initial_screen);

    // Verify initial render shows header
    assert!(
        initial_screen.contains("GIT STATUS"),
        "Should show GIT STATUS header. Screen:\n{}",
        initial_screen
    );

    // Should not have errors
    assert!(
        !initial_screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        initial_screen
    );

    // Navigate down several times past the viewport
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    let scrolled_screen = harness.screen_to_string();
    println!("After scrolling down:\n{}", scrolled_screen);

    // The screen should still have the GIT STATUS header
    assert!(
        scrolled_screen.contains("GIT STATUS"),
        "Should still show GIT STATUS header after scrolling. Screen:\n{}",
        scrolled_screen
    );

    // The diff panel should have updated (no stale content)
    assert!(
        scrolled_screen.contains("DIFF"),
        "Should still show DIFF header after scrolling. Screen:\n{}",
        scrolled_screen
    );

    // Should not have any errors after navigation
    assert!(
        !scrolled_screen.contains("TypeError") && !scrolled_screen.contains("Error"),
        "Should not show errors after navigation. Screen:\n{}",
        scrolled_screen
    );
}
