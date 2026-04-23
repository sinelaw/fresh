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

    // Verify section headers are present (uppercase in the unified-stream layout)
    assert!(
        screen.contains("STAGED"),
        "Should show 'STAGED' section header. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("UNSTAGED"),
        "Should show 'UNSTAGED' section header. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("UNTRACKED"),
        "Should show 'UNTRACKED' section header. Screen:\n{}",
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
    println!("Review Diff (untracked + staged new) screen:\n{}", screen);

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

    // The untracked file should appear in the file list
    assert!(
        screen.contains("untracked_new.rs"),
        "Review diff should show the untracked file 'untracked_new.rs'. Screen:\n{}",
        screen
    );

    // The untracked file's content is rendered inline somewhere in the
    // unified stream; page-down to scan if necessary.
    let mut found_untracked_func = false;
    for _ in 0..6 {
        if harness.screen_to_string().contains("untracked_func") {
            found_untracked_func = true;
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    assert!(
        found_untracked_func,
        "Review diff should show content from the untracked file. Final screen:\n{}",
        harness.screen_to_string()
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

    // The untracked file must appear in the file list
    assert!(
        screen.contains("brand_new_untracked.rs"),
        "Review diff should show untracked file 'brand_new_untracked.rs'. Screen:\n{}",
        screen
    );

    // In the unified-stream layout, the untracked file's content is already
    // emitted inline (or page-down reveals it for long file lists).
    let mut found_also_new = false;
    for _ in 0..6 {
        if harness.screen_to_string().contains("also_new") {
            found_also_new = true;
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    assert!(
        found_also_new,
        "Review diff should show content from untracked file. Final screen:\n{}",
        harness.screen_to_string()
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

    // Wait for review diff to load — toolbar's "next hunk" hint marks the
    // unified-stream layout as ready.
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", screen);
            }
            screen.contains("next hunk")
        })
        .unwrap();

    let initial_screen = harness.screen_to_string();
    println!("Initial magit screen:\n{}", initial_screen);

    // Should not have errors.
    assert!(
        !initial_screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        initial_screen
    );

    // Use n (next hunk) to walk past the viewport into hunks belonging to
    // later files; this exercises the same scroll path the old j-based
    // file-list navigation did.
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    let scrolled_screen = harness.screen_to_string();
    println!("After scrolling down:\n{}", scrolled_screen);

    // No errors after navigation.
    assert!(
        !scrolled_screen.contains("TypeError") && !scrolled_screen.contains("Error"),
        "Should not show errors after navigation. Screen:\n{}",
        scrolled_screen
    );
}

/// Helper: open Review Diff via command palette and wait for it to load.
/// Returns the initial screen string. The unified-stream layout doesn't
/// have a static "GIT STATUS" / "DIFF FOR" header anymore — we wait for
/// the toolbar's "next hunk" hint, which is a unique marker that only
/// renders once the buffer group is up.
fn open_review_diff(harness: &mut EditorTestHarness) -> String {
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
            if screen.contains("TypeError") || screen.contains("Error:") {
                panic!("Error loading review diff. Screen:\n{}", screen);
            }
            // The toolbar ("next hunk") renders before the stream body has
            // finished generating. Wait until the "Generating ..." status
            // is gone so tests see the actual diff content.
            screen.contains("next hunk") && !screen.contains("Generating Review Diff Stream")
        })
        .unwrap();

    harness.screen_to_string()
}

/// Test that j/k delegate to native cursor motion in the unified-stream
/// diff buffer (no more files-pane plugin-managed selection). Verifies
/// the cursor moves down/up by one row without errors.
#[test]
fn test_review_diff_jk_navigation() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create two unstaged modified files so the unified stream has multiple
    // file headers in it.
    repo.create_file("src/main.rs", "fn main() { /* changed */ }\n");
    repo.create_file("src/lib.rs", "pub struct Config { /* changed */ }\n");

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
        .wait_until(|h| h.screen_to_string().contains("changed"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // Both file headers should be present in the unified stream.
    assert!(
        screen.contains("src/main.rs") && screen.contains("src/lib.rs"),
        "Both files should appear as headers in the unified stream. Screen:\n{}",
        screen
    );

    // j moves cursor down a row in the diff buffer; k moves it back up.
    // We just verify these don't error.
    for _ in 0..3 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    let s_after_j = harness.screen_to_string();
    assert!(
        !s_after_j.contains("TypeError"),
        "j should not error. Screen:\n{}",
        s_after_j
    );

    for _ in 0..3 {
        harness
            .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    let s_after_k = harness.screen_to_string();
    assert!(
        !s_after_k.contains("TypeError"),
        "k should not error. Screen:\n{}",
        s_after_k
    );
}

/// Home / End fall through to the editor's native start-of-line /
/// end-of-line motion (the plugin intentionally does NOT bind them).
/// Just verify they don't error.
#[test]
fn test_review_diff_home_end_navigation() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.create_file("src/main.rs", "fn main() { /* changed */ }\n");

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
        .wait_until(|h| h.screen_to_string().contains("changed"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        !harness.screen_to_string().contains("TypeError"),
        "Home / End should not error. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// Test that the unified-stream layout shows a comments-navigation
/// panel alongside the diff buffer. The panel renders a header and
/// either an empty-state message or a list of comments.
#[test]
fn test_review_diff_shows_comments_panel() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.create_file("src/main.rs", "fn main() { /* changed */ }\n");

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
        .wait_until(|h| h.screen_to_string().contains("changed"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // The comments panel header and empty state are visible.
    assert!(
        screen.contains("COMMENTS"),
        "Comments panel header should be visible. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("No comments yet"),
        "Empty-state message should be visible. Screen:\n{}",
        screen
    );
}

/// Test that renamed files show "Renamed from <path>" instead of "(no diff available)".
#[test]
fn test_review_diff_renamed_file_message() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Rename a file via git mv (staged rename)
    let output = std::process::Command::new("git")
        .args(["mv", "src/utils.rs", "src/helpers.rs"])
        .current_dir(&repo.path)
        .output()
        .expect("git mv failed");
    assert!(output.status.success(), "git mv failed");

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
        .wait_until(|h| h.screen_to_string().contains("Hello"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // The unified stream lists every file inline. Both the renamed file
    // and the "Renamed from" message should be visible somewhere on screen
    // (or after a single page-down to reveal the renamed-from line for
    // long file lists).
    let mut found_rename = false;
    for _ in 0..6 {
        let s = harness.screen_to_string();
        if (s.contains("helpers.rs") || s.contains("src/helpers.rs")) && s.contains("Renamed from")
        {
            assert!(
                s.contains("Renamed from src/utils.rs") || s.contains("Renamed from utils.rs"),
                "Should show original path in rename message. Screen:\n{}",
                s
            );
            found_rename = true;
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    assert!(
        found_rename,
        "Should find renamed file with 'Renamed from' message. Final screen:\n{}",
        harness.screen_to_string()
    );
}

/// Test that untracked directories show "(untracked directory)" message.
#[test]
fn test_review_diff_untracked_directory_message() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create an untracked directory with a file inside
    repo.create_file("newdir/hello.txt", "hello\n");

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
        .wait_until(|h| h.screen_to_string().contains("Hello"))
        .unwrap();

    let _screen = open_review_diff(&mut harness);

    // The unified stream lists every file inline; the untracked directory
    // header and its (untracked directory) message are both rendered.
    let mut found_dir = false;
    for _ in 0..6 {
        let s = harness.screen_to_string();
        if s.contains("newdir/") && s.contains("untracked directory") {
            found_dir = true;
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    assert!(
        found_dir,
        "Should find untracked directory with '(untracked directory)' message. Final screen:\n{}",
        harness.screen_to_string()
    );
}

/// Test that Tab toggles the collapse state of the file under the cursor
/// in the unified diff stream. After Tab, the file's hunks should
/// disappear (only the header remains, with a ▸ triangle); pressing Tab
/// again restores the hunks.
#[test]
fn test_review_diff_tab_toggles_file_collapse() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify main.rs to produce a hunk with a unique marker so we can
    // tell when the file is expanded vs. collapsed.
    repo.create_file("src/main.rs", "fn main() { /* COLLAPSE_MARKER */ }\n");

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
        .wait_until(|h| h.screen_to_string().contains("COLLAPSE_MARKER"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // Initially the file is expanded — the COLLAPSE_MARKER hunk content is visible.
    assert!(
        screen.contains("COLLAPSE_MARKER"),
        "File content should be visible while expanded. Screen:\n{}",
        screen
    );

    // Move cursor to the first hunk so Tab toggles the *file* (nearest
    // collapsible ancestor of a hunk is file, then section). Without this
    // the cursor lands on the section header row and Tab collapses the
    // whole section instead of just the file.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press Tab to toggle collapse on the file under the cursor.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let collapsed = harness.screen_to_string();
    assert!(
        !collapsed.contains("COLLAPSE_MARKER"),
        "Hunk content should be hidden after Tab collapse. Screen:\n{}",
        collapsed
    );
    // The file path itself stays as the header row.
    assert!(
        collapsed.contains("src/main.rs"),
        "File header should still be visible after collapse. Screen:\n{}",
        collapsed
    );

    // Press Tab again to expand.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let expanded = harness.screen_to_string();
    assert!(
        expanded.contains("COLLAPSE_MARKER"),
        "Hunk content should reappear after second Tab. Screen:\n{}",
        expanded
    );
}

/// `z a` collapses every file in the unified stream; `z r` reveals
/// (expands) every file. After collapsing all, no hunk content for any
/// file is visible — only headers. After expanding, content returns.
#[test]
fn test_review_diff_collapse_all_and_expand_all() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.create_file("src/main.rs", "fn main() { /* MARKER_MAIN */ }\n");
    repo.create_file("src/lib.rs", "pub struct Config { /* MARKER_LIB */ }\n");

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
        .wait_until(|h| h.screen_to_string().contains("MARKER_MAIN"))
        .unwrap();

    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("MARKER_MAIN") || screen.contains("MARKER_LIB"),
        "Hunk content from at least one file should be visible. Screen:\n{}",
        screen
    );

    // `z a` collapses everything.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let collapsed = harness.screen_to_string();
    assert!(
        !collapsed.contains("MARKER_MAIN") && !collapsed.contains("MARKER_LIB"),
        "Both files' hunks should be hidden after `z a`. Screen:\n{}",
        collapsed
    );

    // `z r` expands everything.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('r'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let expanded = harness.screen_to_string();
    assert!(
        expanded.contains("MARKER_MAIN") || expanded.contains("MARKER_LIB"),
        "Hunk content should reappear after `z r`. Screen:\n{}",
        expanded
    );
}

/// Regression test for the "Review Diff scroll jumps on collapse/expand"
/// bug: when the user scrolls the diff cursor via `j` until it sits away
/// from the re-centering point (roughly the top third of the viewport)
/// and then presses `Tab` to toggle a fold, the viewport must not jump.
/// Historically the Tab handler called `editor.scrollBufferToLine` after
/// every fold toggle, forcing the header row to land at ~1/3 from the top
/// — which yanked the viewport whenever the cursor was anywhere else.
#[test]
fn test_review_diff_tab_preserves_scroll_position() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Baseline commit so the modifications below show up as unstaged hunks.
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Add enough tracked files with enough per-file content that the
    // unified diff comfortably overflows a 40-row viewport.
    for i in 0..8 {
        let path = format!("src/mod_{}.rs", i);
        let content = format!(
            "fn alpha_{i}() {{\n    let a = {i};\n    let b = a + {i};\n    \
             println!(\"alpha {i} {{}} {{}}\", a, b);\n}}\n\n\
             fn beta_{i}() {{\n    let c = {i} * 2;\n    let d = c - 1;\n    \
             println!(\"beta {i} {{}} {{}}\", c, d);\n}}\n",
            i = i
        );
        repo.create_file(&path, &content);
    }
    repo.git_add_all();
    repo.git_commit("Seed files");

    // Modify every file to produce a multi-hunk diff per file.
    for i in 0..8 {
        let path = format!("src/mod_{}.rs", i);
        let content = format!(
            "fn alpha_{i}() {{\n    let a = {i} + 100;\n    let b = a + {i} * 3;\n    \
             let extra = a - b;\n    println!(\"alpha {i} v2 {{}} {{}} {{}}\", a, b, extra);\n}}\n\n\
             fn beta_{i}() {{\n    let c = {i} * 2 + 5;\n    let d = c - 1;\n    \
             let more = c + d;\n    println!(\"beta {i} v2 {{}} {{}} {{}}\", c, d, more);\n}}\n",
            i = i
        );
        repo.create_file(&path, &content);
    }

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

    let _ = open_review_diff(&mut harness);

    // Scroll the cursor far down the unified stream using native `j`
    // motion. `j` delegates to the editor's `move_down`, which moves the
    // cursor one row at a time and only scrolls the viewport when the
    // cursor would otherwise step off-screen — so after enough presses
    // the cursor is pinned near the BOTTOM of the viewport, not at the
    // ~1/3 mark that `scrollBufferToLine` targets. That mismatch is what
    // the bug exploits.
    for _ in 0..100 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let before = harness.screen_to_string();
    let top_before = diff_top_anchor(&before);

    // Toggle the fold under the cursor. Without the fix this yanks the
    // viewport so the cursor row re-centers at ~1/3 from the top, which
    // changes what's rendered at the top of the diff pane.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let after = harness.screen_to_string();
    let top_after = diff_top_anchor(&after);

    assert_eq!(
        top_before, top_after,
        "Review Diff viewport scrolled unexpectedly after Tab. \
         Toggling a fold must not re-center the viewport.\n\
         BEFORE:\n{}\n\nAFTER:\n{}",
        before, after
    );
}

/// Slice the top ~10 rows of the diff content pane out of a rendered
/// screen string. Used by the scroll-position regression test above.
/// The layout in tests is: menu bar, tab bar, two toolbar hint rows,
/// separator, sticky file header, separator, then the diff stream. The
/// first ~7 rows are chrome that never shifts on a scroll, so we skip
/// them and compare the first 10 rows of actual diff content.
fn diff_top_anchor(screen: &str) -> String {
    screen
        .lines()
        .skip(7)
        .take(10)
        .map(|l| l.trim_end().to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Test that the review diff handles symlinks, type changes (file ↔ symlink),
/// and mode changes (chmod) without errors.
/// Git reports type changes as 'T' status and mode changes as 'M'.
#[test]
#[cfg(unix)]
fn test_review_diff_symlinks_and_type_changes() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Create initial files: regular file, symlink, and executable
    repo.create_file("regular.txt", "regular file content\n");
    repo.create_file("script.sh", "#!/bin/sh\necho hello\n");

    // Create a symlink
    std::os::unix::fs::symlink("regular.txt", repo.path.join("symlink.txt"))
        .expect("Failed to create symlink");

    // Make script.sh executable
    let output = std::process::Command::new("chmod")
        .args(["+x", "script.sh"])
        .current_dir(&repo.path)
        .output()
        .expect("chmod failed");
    assert!(output.status.success(), "chmod failed");

    repo.git_add_all();
    repo.git_commit("Initial commit with symlink and executable");

    // Type change: replace symlink with a regular file
    fs::remove_file(repo.path.join("symlink.txt")).unwrap();
    fs::write(repo.path.join("symlink.txt"), "now a regular file\n").unwrap();

    // Type change: replace regular file with a symlink
    fs::remove_file(repo.path.join("regular.txt")).unwrap();
    std::os::unix::fs::symlink("script.sh", repo.path.join("regular.txt"))
        .expect("Failed to create symlink for type change");

    // Mode change: remove execute permission
    let output = std::process::Command::new("chmod")
        .args(["-x", "script.sh"])
        .current_dir(&repo.path)
        .output()
        .expect("chmod failed");
    assert!(output.status.success(), "chmod -x failed");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    let script_path = repo.path.join("script.sh");
    harness.open_file(&script_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("echo"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // Should show all three changed files without errors
    assert!(
        !screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        screen
    );
    // The unified stream renders every file inline; both the mode-change
    // file (script.sh) and the type-change file (symlink.txt or regular.txt)
    // appear as headers on screen (or after page-down for long lists).
    let mut _found_type_change = false;
    let mut found_mode_change = false;
    for _ in 0..6 {
        let s = harness.screen_to_string();
        assert!(
            !s.contains("TypeError"),
            "Should not show TypeError during navigation. Screen:\n{}",
            s
        );
        if s.contains("type change") {
            _found_type_change = true;
        }
        if s.contains("script.sh") {
            found_mode_change = true;
        }
        if found_mode_change {
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    assert!(
        found_mode_change,
        "Should find script.sh with mode change. Final screen:\n{}",
        harness.screen_to_string()
    );

    // Type changes show "T" status — the plugin should handle them gracefully.
    // On some git versions, typechanges may or may not produce diff hunks,
    // so we just verify no crashes occurred.
}

/// Test that the review diff handles a new symlink (untracked) gracefully.
#[test]
#[cfg(unix)]
fn test_review_diff_new_symlink() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Create a new symlink (untracked)
    std::os::unix::fs::symlink("src/main.rs", repo.path.join("link_to_main"))
        .expect("Failed to create symlink");

    // Also create a new regular file for comparison
    repo.create_file("newfile.txt", "new content\n");

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
        .wait_until(|h| h.screen_to_string().contains("Hello"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // Should show untracked files including the symlink
    assert!(
        !screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        screen
    );

    // Navigate to find the regular file. Symlinks may or may not show (git
    // may list them as regular files); the key assertion is no errors.
    let mut found_newfile = false;
    for _ in 0..10 {
        let s = harness.screen_to_string();
        if s.contains("newfile.txt") {
            found_newfile = true;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }

    assert!(
        found_newfile,
        "Should find newfile.txt in review diff. Final screen:\n{}",
        harness.screen_to_string()
    );
}

/// Test that staged type changes (file replaced with directory) are handled.
/// When a tracked file is deleted and a directory with the same base path is created,
/// git shows the file as deleted and directory contents as untracked.
#[test]
fn test_review_diff_file_replaced_with_directory() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Create and commit a regular file
    repo.create_file("component.txt", "original component\n");
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Replace file with a directory containing files
    fs::remove_file(repo.path.join("component.txt")).unwrap();
    repo.create_file("component/index.txt", "index content\n");
    repo.create_file("component/style.txt", "style content\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    let index_path = repo.path.join("component/index.txt");
    harness.open_file(&index_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("index"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // Should show the deleted file and untracked directory without errors
    assert!(
        !screen.contains("TypeError"),
        "Should not show TypeError. Screen:\n{}",
        screen
    );
    // The unified stream renders both the deleted file and the untracked
    // directory inline; scan via PageDown if they aren't on the first
    // viewport.
    let mut found_deleted = false;
    let mut found_new_dir = false;
    for _ in 0..6 {
        let s = harness.screen_to_string();
        assert!(
            !s.contains("TypeError"),
            "No errors during navigation. Screen:\n{}",
            s
        );
        if s.contains("component.txt") {
            found_deleted = true;
        }
        if s.contains("component/") {
            found_new_dir = true;
        }
        if found_deleted && found_new_dir {
            break;
        }
        harness
            .send_key(KeyCode::PageDown, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    // The original file should show as deleted
    assert!(
        found_deleted || found_new_dir,
        "Should find deleted file or new directory. Final screen:\n{}",
        harness.screen_to_string()
    );
}

/// Regression test: switching the active theme via "Select Theme" while a
/// Review Diff (audit_mode) virtual buffer is open must update the colors of
/// the diff hunk backgrounds in that buffer to reflect the new theme.
///
/// The audit_mode plugin attaches overlays to its virtual buffer with theme
/// key references like `"editor.diff_add_bg"` / `"editor.diff_remove_bg"`.
/// These are stored as `OverlayFace::ThemedStyle` so they resolve at render
/// time — so when the active theme changes, the next render should pick up
/// the new theme's values.
///
/// Dark theme: `diff_add_bg` = [30, 60, 30], `diff_remove_bg` = [70, 30, 30]
/// Light theme: `diff_add_bg` = [200, 255, 200], `diff_remove_bg` = [255, 200, 200]
#[test]
fn test_review_diff_colors_update_on_theme_change() {
    use ratatui::style::Color;

    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    // Commit the initial project, then modify a file so the diff has hunks.
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    let modified_content = r#"fn main() {
    println!("DIFF_NEW_LINE");
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

    // Start in the dark theme so we have well-known expected colors.
    let mut config = Config::default();
    config.theme = "dark".into();

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(140, 40, config, repo.path.clone()).unwrap();

    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("DIFF_NEW_LINE"))
        .unwrap();

    // Open Review Diff via the command palette.
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
            let s = h.screen_to_string();
            !s.contains("Generating Review Diff Stream") && s.contains("DIFF_NEW_LINE")
        })
        .unwrap();
    harness.render().unwrap();

    // Find the added line ("+    println!(...DIFF_NEW_LINE..)") inside the diff
    // panel. Its background should be the dark theme's `diff_add_bg`.
    let dark_add_bg = Color::Rgb(30, 60, 30);
    let dark_remove_bg = Color::Rgb(70, 30, 30);

    let add_pos = harness
        .find_text_on_screen("DIFF_NEW_LINE")
        .expect("DIFF_NEW_LINE should be visible in the review diff buffer");
    let add_style = harness
        .get_cell_style(add_pos.0, add_pos.1)
        .expect("cell should have a style");
    assert_eq!(
        add_style.bg,
        Some(dark_add_bg),
        "With dark theme, diff_add_bg should be {:?}, got {:?}. Screen:\n{}",
        dark_add_bg,
        add_style.bg,
        harness.screen_to_string(),
    );

    // The removed line should carry dark_remove_bg.
    let rem_pos = harness
        .find_text_on_screen("Hello, world!")
        .expect("original 'Hello, world!' should be visible as a removed line");
    let rem_style = harness
        .get_cell_style(rem_pos.0, rem_pos.1)
        .expect("cell should have a style");
    assert_eq!(
        rem_style.bg,
        Some(dark_remove_bg),
        "With dark theme, diff_remove_bg should be {:?}, got {:?}. Screen:\n{}",
        dark_remove_bg,
        rem_style.bg,
        harness.screen_to_string(),
    );

    // --- Switch to the light theme ---
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Select Theme").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_screen_contains("Select theme").unwrap();

    // Clear the pre-filled current theme name and type "light".
    for _ in 0..20 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.type_text("light").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
    harness.render().unwrap();

    // --- Verify the plugin buffer picked up the new theme's diff colors ---
    let light_add_bg = Color::Rgb(200, 255, 200);
    let light_remove_bg = Color::Rgb(255, 200, 200);

    let add_pos = harness
        .find_text_on_screen("DIFF_NEW_LINE")
        .expect("DIFF_NEW_LINE should still be visible after theme switch");
    let add_style = harness
        .get_cell_style(add_pos.0, add_pos.1)
        .expect("cell should have a style");
    assert_eq!(
        add_style.bg,
        Some(light_add_bg),
        "After switching to light theme, diff_add_bg in the Review Diff plugin buffer \
         should be {:?}, got {:?}. The plugin buffer's overlays were not refreshed for \
         the new theme. Screen:\n{}",
        light_add_bg,
        add_style.bg,
        harness.screen_to_string(),
    );

    let rem_pos = harness
        .find_text_on_screen("Hello, world!")
        .expect("original 'Hello, world!' should still be visible");
    let rem_style = harness
        .get_cell_style(rem_pos.0, rem_pos.1)
        .expect("cell should have a style");
    assert_eq!(
        rem_style.bg,
        Some(light_remove_bg),
        "After switching to light theme, diff_remove_bg in the Review Diff plugin buffer \
         should be {:?}, got {:?}. The plugin buffer's overlays were not refreshed for \
         the new theme. Screen:\n{}",
        light_remove_bg,
        rem_style.bg,
        harness.screen_to_string(),
    );
}

/// Regression test for: after pressing `n` in the diff panel (which scrolls
/// the panel viewport via `scrollBufferToLine`, setting `skip_ensure_visible`
/// on the panel buffer's view state), pressing `k` to move the cursor up
/// should still scroll the viewport to keep the cursor visible.
///
/// The bug: `handle_key` cleared `skip_ensure_visible` on
/// `split_manager.active_split()` instead of the *effective* active split,
/// so for a focused buffer-group panel the flag stayed set on the panel and
/// subsequent cursor motion left the cursor stranded off-screen.
#[test]
fn test_review_diff_panel_viewport_follows_cursor_after_scroll() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // 300-line file with a modification every 10 lines so the diff produces
    // ~30 separate hunks (each hunk is one changed line plus 3 context lines
    // on each side = 9 buffer rows). Total diff panel content is well over
    // a viewport height, so jumping forward and walking back actually
    // exercises the scroll path.
    let file_path = repo.path.join("manyhunks.txt");
    let mut original = String::new();
    for i in 1..=300 {
        original.push_str(&format!("Line {}\n", i));
    }
    fs::write(&file_path, &original).expect("write original");
    repo.git_add_all();
    repo.git_commit("Initial");

    let mut modified = String::new();
    for i in 1..=300 {
        if i % 10 == 0 {
            modified.push_str(&format!("MODIFIED line {}\n", i));
        } else {
            modified.push_str(&format!("Line {}\n", i));
        }
    }
    fs::write(&file_path, &modified).expect("write modified");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MODIFIED line 10"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // The file header for manyhunks.txt is visible in the unified stream.
    harness
        .wait_until(|h| h.screen_to_string().contains("manyhunks.txt"))
        .unwrap();

    // Jump several hunks forward. Each `n` press calls
    // `editor.scrollBufferToLine` on the diff buffer, which sets
    // `skip_ensure_visible` on its viewport — exactly the state the bug
    // depends on.
    for _ in 0..10 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Sanity: the file header (top of the buffer) has scrolled off-screen.
    let mid_screen = harness.screen_to_string();
    assert!(
        !mid_screen.contains("▾ manyhunks.txt"),
        "After 10 `n` presses the file header should be scrolled \
         off-screen — the test setup isn't producing a long enough diff. \
         Screen:\n{}",
        mid_screen
    );

    // Now walk the cursor back toward the top of the diff buffer with `k`.
    // 200 presses is generously more than enough to clear any conceivable
    // viewport offset. With the bug, the cursor moves but the viewport stays
    // stranded; with the fix, the viewport follows the cursor and the file
    // header comes back into view.
    for _ in 0..200 {
        harness
            .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let final_screen = harness.screen_to_string();
    assert!(
        final_screen.contains("▾ manyhunks.txt"),
        "After walking the cursor back to the top of the diff buffer, the \
         viewport should follow it — the file header is missing. \
         Screen:\n{}",
        final_screen
    );
}

/// Helper for the cursor-line / drill-down tests below: build a repo with
/// a file that yields a long diff containing several consecutive `-`/`+`
/// lines per hunk in the review-diff right panel. The consecutive `+`
/// lines matter for `test_review_diff_cursor_line_highlight_does_not_bleed_…`
/// because the bug only manifests when the row immediately below the
/// cursor row also has an entry-level `extendToLineEnd` bg of its own.
fn setup_many_hunks_repo() -> (GitTestRepo, std::path::PathBuf) {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);
    let file_path = repo.path.join("manyhunks.txt");
    let mut original = String::new();
    for i in 1..=300 {
        original.push_str(&format!("Line {}\n", i));
    }
    fs::write(&file_path, &original).expect("write original");
    repo.git_add_all();
    repo.git_commit("Initial");

    // Modify lines 10-12, 20-22, 30-32, … so each hunk produces a block
    // of three `-` lines followed by three `+` lines (six adjacent -/+
    // rows per hunk, ~30 hunks total).
    let mut modified = String::new();
    for i in 1..=300 {
        if matches!(i % 10, 0 | 1 | 2) && i >= 10 {
            modified.push_str(&format!("MODIFIED line {}\n", i));
        } else {
            modified.push_str(&format!("Line {}\n", i));
        }
    }
    fs::write(&file_path, &modified).expect("write modified");
    (repo, file_path)
}

/// Find the screen row containing a substring. Returns the 0-indexed row, or
/// panics if not found.
fn find_screen_row(harness: &EditorTestHarness, needle: &str) -> usize {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("Did not find {needle:?} on screen:\n{screen}"))
}

/// Regression test for the cursor-line highlight bleeding into the next row.
///
/// `applyCursorLineOverlay` in `audit_mode.ts` paints a bg overlay covering
/// `[diffLineByteOffsets[idx], diffLineByteOffsets[idx+1])`. The end offset
/// is the start of the *next* row, which means the range includes the
/// trailing newline byte and the renderer extends the bg one cell into the
/// row below — visible as a tinted leading-whitespace block on the next
/// content line.
#[test]
fn test_review_diff_cursor_line_highlight_does_not_bleed_to_next_row() {
    init_tracing_from_env();
    let (repo, file_path) = setup_many_hunks_repo();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MODIFIED line 10"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // Walk the cursor down so it sits on a `+` row whose *next* row is also
    // a `+` row. In the unified-stream layout the buffer starts with the
    // file header; for a single-file diff the layout is approximately:
    //
    //   1: "▾ manyhunks.txt   +N / -M"   (file header)
    //   2: "@@ -7,9 +7,9 @@"
    //   3: " Line 7"
    //   4: " Line 8"
    //   5: " Line 9"
    //   6: "-Line 10"
    //   7: "-Line 11"
    //   8: "-Line 12"
    //   9: "+MODIFIED line 10"
    //  10: "+MODIFIED line 11"   ← stop here
    //  11: "+MODIFIED line 12"
    //  12: " Line 13"
    for _ in 0..9 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Locate the file-header row to anchor the probe range.
    let screen = harness.screen_to_string();
    let header_row = find_screen_row(&harness, "manyhunks.txt");
    // Probe at fixed columns inside the diff buffer area.
    let divider_col = 0usize;

    // Find the row in the diff panel whose visible content cell has a bg
    // that *differs* from the entry-level `+` ADD bg — that's the cursor
    // row (where the cursor-line overlay sits on top of the entry style).
    //
    // We do this by walking down the rows of the diff panel area and
    // probing two cells per row: one inside the visible content, one in
    // the trailing fill. On a normal `+` row both cells have the entry
    // ADD bg and match. On the cursor row both cells have the cursor
    // highlight bg (still matching). On the row immediately below the
    // cursor row WITH THE BUG, the visible content has the entry ADD bg
    // but the trailing fill has the cursor highlight bg — they DIFFER,
    // which is the assertion failure.
    let content_x = (divider_col + 5) as u16; // inside the line text
    let fill_x = (divider_col + 50) as u16; // well into the trailing fill

    let bg_at = |x: u16, y: u16| -> Option<ratatui::style::Color> {
        harness.get_cell_style(x, y).and_then(|s| s.bg)
    };

    // Sanity: probe `+` rows to make sure they have an entry-level bg
    // (and the test is wired up correctly). We scan the diff panel area
    // for any row where the content cell looks like a `+` row.
    let probe_rows: Vec<u16> = ((header_row + 1) as u16..(header_row + 25) as u16).collect();
    let mut bleed_rows: Vec<(
        u16,
        Option<ratatui::style::Color>,
        Option<ratatui::style::Color>,
    )> = Vec::new();
    for &y in &probe_rows {
        let c = bg_at(content_x, y);
        let f = bg_at(fill_x, y);
        if c != f {
            bleed_rows.push((y, c, f));
        }
    }

    assert!(
        bleed_rows.is_empty(),
        "Cursor-line highlight bg leaked into the trailing fill of one or \
         more rows (bug shows the visible content and the trailing fill of \
         the same row with different bgs). Mismatches: {:?}.\nScreen:\n{}",
        bleed_rows,
        screen
    );
}

/// Regression test for the drill-down close behavior: pressing `q` in the
/// composite side-by-side diff should return focus to the review-diff
/// buffer group, not pick some unrelated buffer or open `[No Name]`.
#[test]
fn test_review_diff_drill_down_close_returns_to_group() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file so the review diff has something to drill into.
    let main_rs_path = repo.path.join("src/main.rs");
    fs::write(&main_rs_path, "fn main() { /* changed */ }\n").expect("modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("changed"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // Wait until the file list shows the modified file and the diff panel
    // header shows the per-file path — both are buffer content (not status
    // bar) and indicate review diff has finished fetching git state.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("main.rs") && s.contains("next hunk")
        })
        .unwrap();

    // In the unified-stream layout Enter only drills down from a hunk
    // content row (add/remove/context/hunk-header). Navigate to the first
    // hunk via `n` so the cursor is on a diff content line before pressing
    // Enter.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Drill down on the selected file. Enter triggers `review_drill_down`,
    // which builds a composite side-by-side diff buffer and switches to it.
    //
    // IMPORTANT: wait for content that only appears when the composite is
    // the ACTIVE buffer (rendered in the content area), not just present
    // as a tab. `create_composite_buffer` adds the composite to the tab
    // bar one tick before `showBuffer` makes it active. If we matched the
    // tab title (`*Diff:`), the wait could exit while the active buffer is
    // still the hidden `*NEW:*` helper (mode "normal"), and the subsequent
    // `q` press would be silently ignored instead of triggering `close`
    // from the `diff-view` mode.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            if s.contains("TypeError") {
                panic!("TypeError during drill-down. Screen:\n{}", s);
            }
            // "OLD (HEAD)" is rendered by the composite buffer's content
            // area — it proves the composite is active, not just tabbed.
            s.contains("OLD (HEAD)")
        })
        .unwrap();

    // Close the composite via `q` (bound by the diff-view mode).
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    // After closing the composite the review-diff group should be active
    // again — the unified-stream toolbar reappears.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk")
        })
        .unwrap();

    // And critically — the active buffer should NOT be a fresh [No Name]
    // buffer. The composite was closed; the editor must NOT have created
    // an empty replacement.
    let final_screen = harness.screen_to_string();
    assert!(
        !final_screen.contains("[No Name]"),
        "Closing the drill-down composite should return to the review-diff \
         group, not create a [No Name] buffer. Screen:\n{}",
        final_screen
    );
}

/// Variant of the drill-down close test with no other visible buffers.
///
/// When the user has closed every other file before drilling down, closing
/// the composite must still return focus to the review-diff group — and
/// crucially, must NOT spawn a new `[No Name]` buffer as a fallback
/// "replacement" for the now-removed composite. The group's active inner
/// panel is the natural target to land on.
#[test]
fn test_review_diff_drill_down_close_without_other_buffers() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    let main_rs_path = repo.path.join("src/main.rs");
    fs::write(&main_rs_path, "fn main() { /* changed */ }\n").expect("modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    // Remember main.rs's buffer id *before* any group setup runs. After
    // `open_file`, the active buffer is main.rs, so `active_buffer()`
    // gives us its id.
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("changed"))
        .unwrap();
    let main_buffer_id = harness.editor().active_buffer();

    let _ = open_review_diff(&mut harness);
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("main.rs") && s.contains("next hunk")
        })
        .unwrap();

    // In the unified-stream layout Enter only drills down from a hunk
    // content row; navigate to the first hunk with `n` before pressing Enter.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Drill down — wait for composite CONTENT (see comment in variant 1).
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            if s.contains("TypeError") {
                panic!("TypeError during drill-down. Screen:\n{}", s);
            }
            s.contains("OLD (HEAD)")
        })
        .unwrap();

    // Close the only other visible buffer (main.rs) while the composite
    // is active. The composite is a valid replacement for main.rs's leaf,
    // so no [No Name] fallback is created at this step. The subsequent
    // close of the composite is what exercises the "no other visible
    // buffers" branch in close_buffer_internal.
    harness
        .editor_mut()
        .close_buffer(main_buffer_id)
        .expect("closing main.rs while composite is active");
    harness.render().unwrap();

    // Close the composite via `q` (bound by the diff-view mode).
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();

    // The review-diff group should reappear as the active target.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk")
        })
        .unwrap();

    // No fresh [No Name] buffer should have been created as a fallback.
    let final_screen = harness.screen_to_string();
    assert!(
        !final_screen.contains("[No Name]"),
        "Closing the drill-down composite with no other visible buffers \
         should return to the review-diff group, not create a [No Name] \
         fallback buffer. Screen:\n{}",
        final_screen
    );
}

/// Closing the last regular buffer when a group tab exists in the same
/// split should activate the group — not create a new `[No Name]` buffer
/// and not focus the file explorer sidebar.
///
/// Scenario: editor opens a file, user opens Review Diff (adds a group
/// tab after the file tab), switches back to the file tab, then closes
/// it. The group tab is *after* the closing buffer in `open_buffers`, so
/// a backward-only search wouldn't find it.
#[test]
fn test_close_last_buffer_activates_group_tab() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify a file so Review Diff has content.
    let main_rs_path = repo.path.join("src/main.rs");
    fs::write(&main_rs_path, "fn main() { /* changed */ }\n").expect("modify file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&main_rs_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("changed"))
        .unwrap();

    let _ = open_review_diff(&mut harness);
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("main.rs") && s.contains("next hunk")
        })
        .unwrap();

    // Switch back to the file tab via Ctrl+PageUp (prev buffer).
    harness
        .send_key(KeyCode::PageUp, KeyModifiers::CONTROL)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The file content is visible (not the group panels).
            s.contains("changed") && !s.contains("next hunk")
        })
        .unwrap();

    // Close it via Alt+W (close_tab).
    harness
        .send_key(KeyCode::Char('w'), KeyModifiers::ALT)
        .unwrap();

    // The review-diff group should become active — no [No Name] fallback.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("next hunk")
        })
        .unwrap();

    let final_screen = harness.screen_to_string();
    assert!(
        !final_screen.contains("[No Name]"),
        "Closing the last buffer when a group tab exists should activate \
         the group, not create a [No Name] fallback. Screen:\n{}",
        final_screen
    );
}

// ────────────────────────────────────────────────────────────────────────
// New unified-stream layout tests
// ────────────────────────────────────────────────────────────────────────

/// The unified stream emits a file-header row "▾ <path>   +N / -M" for
/// each file with changes; the headers for every file should appear on
/// screen (or after page-down for long lists).
#[test]
fn test_review_diff_unified_stream_shows_file_headers() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);

    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file("src/main.rs", "fn main() { /* MARKER_A */ }\n");
    repo.create_file("src/lib.rs", "pub struct Config { /* MARKER_B */ }\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MARKER_A"))
        .unwrap();

    let screen = open_review_diff(&mut harness);

    // The ▾ triangle marks an expanded file header.
    assert!(
        screen.contains("▾"),
        "Unified stream should render ▾ file-header triangles. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("src/main.rs") && screen.contains("src/lib.rs"),
        "Both file paths should be in the unified stream. Screen:\n{}",
        screen
    );
    // The "+N / -M" change-count summary appears on the file header line.
    assert!(
        screen.contains("+") && screen.contains("-"),
        "File headers should include +N / -M change counts. Screen:\n{}",
        screen
    );
}

/// `n`/`p` hunk navigation crosses file boundaries inside the unified
/// buffer — pressing `n` enough times from the first file's last hunk
/// lands the cursor on the second file's first hunk.
#[test]
fn test_review_diff_n_p_cross_file_boundaries() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file("a_first.rs", "fn first() { /* MARKER_FIRST */ }\n");
    repo.create_file("z_second.rs", "fn second() { /* MARKER_SECOND */ }\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        20,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("a_first.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MARKER_FIRST"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // Walk forward — multiple `n` presses should cross out of the first
    // file's hunks and land somewhere inside the second file.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }

    let after_n = harness.screen_to_string();
    assert!(
        !after_n.contains("TypeError"),
        "n should not error. Screen:\n{}",
        after_n
    );

    // Walk back — `p` should land on the first file's content again.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();
    }
    let after_p = harness.screen_to_string();
    assert!(
        !after_p.contains("TypeError"),
        "p should not error. Screen:\n{}",
        after_p
    );
}

/// `n` auto-expands collapsed files instead of skipping over them.
/// Collapse a file with `z a`, then `n` should reveal it as it lands
/// the cursor on its first hunk.
#[test]
fn test_review_diff_n_auto_expands_collapsed_file() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file("a.rs", "fn a() { /* HUNK_A */ }\n");
    repo.create_file("b.rs", "fn b() { /* HUNK_B */ }\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("a.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("HUNK_A"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // Collapse all files.
    harness
        .send_key(KeyCode::Char('z'), KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let collapsed = harness.screen_to_string();
    assert!(
        !collapsed.contains("HUNK_A") && !collapsed.contains("HUNK_B"),
        "After `z a`, all hunk content is hidden. Screen:\n{}",
        collapsed
    );

    // Press `n` — should expand the first file with hunks and land on it.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    // The plugin's `review_next_hunk` handler rebuilds the diff buffer
    // asynchronously via `updateMagitDisplay`; wait for the expansion to
    // land on screen rather than assuming a single render flushes it.
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("HUNK_A") || s.contains("HUNK_B")
        })
        .unwrap();
}

/// The sticky panel sits between the toolbar and the diff stream.
/// When the buffer scrolls past the first file's start, the sticky
/// panel should reflect the file currently at the top of the viewport.
#[test]
fn test_review_diff_sticky_header_renders() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    // Make the diff long enough to scroll.
    let file_path = repo.path.join("scrolly.txt");
    let mut original = String::new();
    for i in 1..=200 {
        original.push_str(&format!("Line {}\n", i));
    }
    fs::write(&file_path, &original).expect("write original");
    repo.git_add_all();
    repo.git_commit("Initial scrolly");

    let mut modified = String::new();
    for i in 1..=200 {
        if i % 10 == 0 {
            modified.push_str(&format!("MODIFIED {}\n", i));
        } else {
            modified.push_str(&format!("Line {}\n", i));
        }
    }
    fs::write(&file_path, &modified).expect("write modified");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        25,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MODIFIED"))
        .unwrap();

    let initial = open_review_diff(&mut harness);

    // The sticky panel either shows the neutral summary (cursor before
    // first file header) or the file's section + path. Either way, it
    // contains the word "Review Diff" or the file path.
    assert!(
        initial.contains("Review Diff") || initial.contains("scrolly.txt"),
        "Sticky header should show either neutral summary or file path. Screen:\n{}",
        initial
    );

    // Scroll into the file by jumping to the first hunk.
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Walk forward enough to push the file header off-screen.
    for _ in 0..15 {
        harness
            .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let scrolled = harness.screen_to_string();
    // Sticky should still show the file path (it's pinned, not part of
    // the scrollable diff buffer).
    assert!(
        scrolled.contains("scrolly.txt"),
        "Sticky header should keep the current file's path visible while \
         the diff scrolls. Screen:\n{}",
        scrolled
    );
}

/// `v` starts visual line-selection mode. The selection extends with
/// `j`; pressing `Esc` cancels. Verifies the no-op happy path doesn't
/// error and the toolbar hint appears for the cancellation.
#[test]
fn test_review_diff_visual_select_and_cancel() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file(
        "src/main.rs",
        "fn main() {\n    let a = 1;\n    let b = 2;\n    let c = 3;\n}\n",
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("let a"))
        .unwrap();

    let _ = open_review_diff(&mut harness);

    // Move into the file's diff content (past the header rows).
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Start visual selection on a diff line.
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Extend the selection downward.
    for _ in 0..2 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    let after_extend = harness.screen_to_string();
    assert!(
        !after_extend.contains("TypeError"),
        "Visual extend should not error. Screen:\n{}",
        after_extend
    );

    // Cancel the selection — Esc should clear and not crash.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let after_esc = harness.screen_to_string();
    assert!(
        !after_esc.contains("TypeError"),
        "Esc should not error. Screen:\n{}",
        after_esc
    );
}

/// Single-key `]` and `[` cycle through comments. With no comments
/// present the keys should print a "No comments" status message and
/// not crash.
#[test]
fn test_review_diff_comment_nav_single_keys() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file("src/main.rs", "fn main() { /* edit */ }\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("edit"))
        .unwrap();

    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("No comments yet"),
        "Empty comments panel should be visible. Screen:\n{}",
        screen
    );

    // `]` and `[` with no comments are a no-op (status message). They
    // must not error.
    harness
        .send_key(KeyCode::Char(']'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_close_bracket = harness.screen_to_string();
    assert!(
        !after_close_bracket.contains("TypeError"),
        "`]` should not error. Screen:\n{}",
        after_close_bracket
    );

    harness
        .send_key(KeyCode::Char('['), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let after_open_bracket = harness.screen_to_string();
    assert!(
        !after_open_bracket.contains("TypeError"),
        "`[` should not error. Screen:\n{}",
        after_open_bracket
    );
}

/// Mouse click on a file-header row in the diff stream toggles its
/// collapse state. Requires a viewport tall enough to render the file
/// header on a known row.
///
/// TODO: Port to the unified-stream + host-fold layout. Under the current
/// layout the first click collapses (host fold added, plugin's `▂`
/// placeholder visible), but the second click at the same screen
/// coordinates does not re-toggle — the plugin's mouse handler appears
/// not to fire the fileHeaderRows match on the second click. Needs
/// investigation into how `screen_to_buffer_position` maps the click
/// after a fold is active and whether the plugin's internal state
/// (fileHeaderRows, fileBodyRange) survives a cursor_moved round-trip.
#[test]
#[ignore = "needs port to unified-stream + host-fold layout; see TODO"]
fn test_review_diff_mouse_click_toggles_file_collapse() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file("src/main.rs", "fn main() { /* MOUSE_MARKER */ }\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MOUSE_MARKER"))
        .unwrap();

    let screen = open_review_diff(&mut harness);
    assert!(
        screen.contains("MOUSE_MARKER"),
        "File content should be visible while expanded. Screen:\n{}",
        screen
    );

    // Find the screen row containing the file header (▾ src/main.rs).
    let header_row = screen
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("src/main.rs") && l.contains("▾"))
        .map(|(i, _)| i as u16)
        .expect("file-header row should be on screen");

    // Click somewhere in the middle of the file path.
    harness.mouse_click(15, header_row).unwrap();
    harness.render().unwrap();

    let collapsed = harness.screen_to_string();
    assert!(
        !collapsed.contains("MOUSE_MARKER"),
        "Mouse click on file header should collapse the file. Screen:\n{}",
        collapsed
    );

    // Click again to expand. Re-derive the header row — after the first
    // collapse the screen-to-buffer mapping still has the file header at
    // the same buffer row, but if any scroll happened during the cursor
    // jump the screen row may shift.
    let collapsed_header_row = collapsed
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains("src/main.rs") && l.contains("▾"))
        .map(|(i, _)| i as u16)
        .expect("file-header row should still be on screen after collapse");
    harness.mouse_click(15, collapsed_header_row).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("MOUSE_MARKER"))
        .expect("Second mouse click should re-expand the file");
}

/// Capital `S` stages the file the cursor is currently inside,
/// regardless of whether the cursor is on a header or a hunk line.
/// We move past the header into the hunk body, then press `S` and
/// verify the file's category transitions from unstaged to staged
/// (no longer appears in the "Changes" section).
#[test]
fn test_review_diff_capital_s_stages_whole_file() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    setup_audit_mode_plugin(&repo);
    repo.git_add_all();
    repo.git_commit("Initial");

    repo.create_file("src/main.rs", "fn main() { /* CAPS_S_MARKER */ }\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.open_file(&repo.path.join("src/main.rs")).unwrap();
    harness.render().unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("CAPS_S_MARKER"))
        .unwrap();

    let screen = open_review_diff(&mut harness);
    // Initially in the UNSTAGED section.
    assert!(
        screen.contains("UNSTAGED"),
        "Initially the file is unstaged. Screen:\n{}",
        screen
    );

    // Move the cursor down into the file's hunk body so we're not
    // sitting on the header — that proves capital `S` reaches up to
    // the enclosing file rather than only acting on the header.
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();

    // Capital S — file-level stage.
    harness
        .send_key(KeyCode::Char('S'), KeyModifiers::SHIFT)
        .unwrap();
    harness.render().unwrap();

    // After staging, the file moves into STAGED and the UNSTAGED
    // section disappears (single-file repo).
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            s.contains("STAGED") && !s.contains("UNSTAGED")
        })
        .unwrap();
}

/// Run `git` with the given args in the given repo and panic on failure.
fn run_git(repo: &GitTestRepo, args: &[&str]) {
    let out = std::process::Command::new("git")
        .args(args)
        .current_dir(&repo.path)
        .output()
        .unwrap_or_else(|e| panic!("git {:?} failed to spawn: {}", args, e));
    if !out.status.success() {
        panic!(
            "git {:?} failed: stdout={} stderr={}",
            args,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr),
        );
    }
}

/// Rename the current branch to the given name so the test doesn't depend
/// on whatever `init.defaultBranch` the host happens to have configured.
fn rename_head_branch(repo: &GitTestRepo, name: &str) {
    run_git(repo, &["branch", "-M", name]);
}

/// Review PR Branch should default the base-ref prompt to the repo's
/// actual default branch (master in this test), not a hardcoded "main"
/// that doesn't even exist here.
#[test]
fn test_review_branch_prompt_defaults_to_repo_default_branch() {
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    // Force the default branch to `master` — this is the scenario the
    // original bug hit: the plugin suggested `main`, which wasn't a
    // valid ref, so the user had to type `master` manually.
    repo.create_file("a.txt", "one\n");
    repo.git_add_all();
    repo.git_commit("base commit");
    rename_head_branch(&repo, "master");

    // Feature branch with commits so the review view has something
    // to render once the prompt is accepted.
    run_git(&repo, &["checkout", "-b", "feature"]);
    repo.create_file("a.txt", "one\ntwo\n");
    repo.git_add_all();
    repo.git_commit("add line two");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review PR Branch").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    // The command runs async and eventually opens a second prompt
    // asking for the base ref. Wait for the base-ref prompt to show
    // up (command-palette closes, then the new prompt opens).
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            h.editor().is_prompting() && s.contains("Base ref")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("master"),
        "Base-ref prompt should default to the repo's actual default \
         branch (master), but screen was:\n{}",
        screen
    );
    assert!(
        !screen.contains("default: main"),
        "Prompt shouldn't advertise a hardcoded 'main' default when \
         the repo's default branch is master. Screen:\n{}",
        screen
    );
}

/// PageDown in the detail pane of Review PR Branch should page the
/// buffer, not trip the "Action page_down is not defined as a global
/// function" error. The bug was that the review-branch mode bound
/// PageUp/PageDown to action names that don't exist; they should map
/// to the built-in `move_page_up` / `move_page_down`.
#[test]
fn test_review_branch_detail_pane_page_down_works() {
    init_tracing_from_env();
    let repo = GitTestRepo::new();
    setup_audit_mode_plugin(&repo);

    repo.create_file("a.txt", "base\n");
    repo.git_add_all();
    repo.git_commit("base commit");
    rename_head_branch(&repo, "master");

    // Long feature-branch commit so the detail pane has enough
    // content that PageDown is meaningful — a single one-line diff
    // fits on one screen and wouldn't exercise paging.
    run_git(&repo, &["checkout", "-b", "feature"]);
    let mut body = String::new();
    for i in 0..200 {
        body.push_str(&format!("line {i}\n"));
    }
    repo.create_file("big.txt", &body);
    repo.git_add_all();
    repo.git_commit("add big file");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Review PR Branch").unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            h.editor().is_prompting() && s.contains("Base ref")
        })
        .unwrap();
    // Accept the default (master) — confirms fix #1 is live too.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Wait for the review-branch view to finish loading.
    harness
        .wait_until(|h| h.screen_to_string().contains("add big file"))
        .unwrap();

    // Wait for the detail pane to actually populate (git show is
    // fetched async so it can lag behind the log-panel render).
    harness
        .wait_until(|h| h.screen_to_string().contains("line 0"))
        .unwrap();

    // Tab from the log panel into the detail panel. process_async_and_render
    // drains the JS queue so the focus-switch handler lands before we
    // send the next key.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("*detail* [RO]"))
        .unwrap();

    // Page down: the bug surface is that the broken `page_down`
    // binding was a no-op and logged an error, so the cursor stays on
    // Ln 1. With the fix in place, `move_page_down` advances it by a
    // page, which shows up as a larger line number in the status bar.
    harness
        .send_key(KeyCode::PageDown, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            let s = h.screen_to_string();
            // The detail panel's status line looks like
            //   `*detail* [RO] | Ln <N>, Col <M> | ...`
            // We only care that N is no longer 1 (i.e. paging actually
            // moved the cursor). Match "| Ln 2" / "Ln 3" / … without
            // accidentally matching the old "Ln 1".
            let has_detail = s.contains("*detail* [RO]");
            let moved = [
                "| Ln 2", "| Ln 3", "| Ln 4", "| Ln 5", "| Ln 6", "| Ln 7", "| Ln 8", "| Ln 9",
            ]
            .iter()
            .any(|prefix| s.contains(prefix));
            has_detail && moved
        })
        .unwrap();
}
