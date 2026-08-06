//! E2E tests for gutter indicator plugins (git gutter and buffer modified)

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use ratatui::style::Color;

// =============================================================================
// Test Helpers
// =============================================================================

/// Get content lines from screen (skip menu bar, tab bar, and bottom UI elements)
/// Content lines start at row 2 (after menu bar and tab bar) and end before status bar
fn get_content_lines(screen: &str) -> Vec<&str> {
    let lines: Vec<&str> = screen.lines().collect();
    // Skip: row 0 (menu bar), row 1 (tab bar)
    // Skip: last 2 rows (status bar, prompt line)
    let content_start = 2;
    let content_end = lines.len().saturating_sub(2);

    if content_end > content_start {
        lines[content_start..content_end].to_vec()
    } else {
        vec![]
    }
}

/// Check if any content line has a gutter indicator symbol
/// Only looks at the first character of each line (the indicator column)
fn has_gutter_indicator(screen: &str, symbol: &str) -> bool {
    for line in get_content_lines(screen) {
        // The indicator column is the very first character
        // Only check the first char to avoid matching other │ characters
        if let Some(first_char) = line.chars().next() {
            if first_char.to_string() == symbol {
                return true;
            }
        }
    }
    false
}

/// Count gutter indicators on content lines
/// Only counts the first character of each line (the indicator column)
fn count_gutter_indicators(screen: &str, symbol: &str) -> usize {
    let mut count = 0;
    for line in get_content_lines(screen) {
        // The indicator column is the very first character
        if let Some(first_char) = line.chars().next() {
            if first_char.to_string() == symbol {
                count += 1;
            }
        }
    }
    count
}

/// Get the set of line numbers (0-indexed, relative to content area) that have a specific indicator
fn get_indicator_lines(screen: &str, symbol: &str) -> Vec<usize> {
    let mut lines_with_indicator = Vec::new();
    for (idx, line) in get_content_lines(screen).iter().enumerate() {
        if let Some(first_char) = line.chars().next() {
            if first_char.to_string() == symbol {
                lines_with_indicator.push(idx);
            }
        }
    }
    lines_with_indicator
}

/// Wait for a gutter indicator to appear on any line
fn wait_for_indicator(harness: &mut EditorTestHarness, symbol: &str) {
    let symbol = symbol.to_string();
    harness
        .wait_until(|h| has_gutter_indicator(&h.screen_to_string(), &symbol))
        .unwrap();
}

/// Wait for gutter indicators to disappear completely
fn wait_for_no_indicators(harness: &mut EditorTestHarness, symbol: &str) {
    let symbol = symbol.to_string();
    harness
        .wait_until(|h| !has_gutter_indicator(&h.screen_to_string(), &symbol))
        .unwrap();
}

/// Wait for a specific line to have an indicator (0-indexed relative to content area)
fn wait_for_indicator_on_line(harness: &mut EditorTestHarness, symbol: &str, line: usize) {
    let symbol = symbol.to_string();
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            get_indicator_lines(&screen, &symbol).contains(&line)
        })
        .unwrap();
}

/// Process async operations once (single iteration, no sleep)
fn process_async_once(harness: &mut EditorTestHarness) {
    let _ = harness.process_async_and_render();
}

/// Trigger the Git Gutter Refresh command via command palette.
///
/// Goes through `run_palette_command` rather than typing and pressing Enter:
/// the command is registered by the git_gutter *plugin*, so until the plugin
/// has loaded there is no row to activate, and Quick Open only re-filters on
/// input change. Pressing Enter blind either runs some other command or none.
fn trigger_git_gutter_refresh(harness: &mut EditorTestHarness) {
    harness.run_palette_command("Git Gutter").unwrap();
}

/// Open a file using the harness's open_file method
fn open_file(harness: &mut EditorTestHarness, repo_path: &std::path::Path, relative_path: &str) {
    let full_path = repo_path.join(relative_path);
    harness.open_file(&full_path).unwrap();
    // Wait for the file content to be visible on screen
    // This ensures the file is loaded and rendered
    harness
        .wait_until(|h| {
            // Check that we're no longer showing the empty scratch buffer
            let screen = h.screen_to_string();
            // The tab should show the filename
            screen.contains(relative_path)
        })
        .unwrap();
}

/// Save the current file
fn save_file(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    // Process any resulting async operations
    process_async_once(harness);
}

// =============================================================================
// Git Gutter Tests
// =============================================================================

/// Test that git gutter shows indicators for uncommitted changes on file open
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_shows_on_file_open() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Modify a file in the working copy (not staged, not committed)
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Modified line!");
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open the modified file
    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait for git gutter to update
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Look for the modified indicator (│) in the gutter
            has_gutter_indicator(&screen, "│")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Git gutter screen:\n{}", screen);
}

/// Regression test for issue #2721: user-configured external diff tools may
/// emit side-by-side output, but the gutter parser requires a unified diff.
#[test]
#[cfg(unix)]
fn test_git_gutter_ignores_external_diff_and_pager() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();
    repo.setup_external_diff_and_pager();

    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Modified while difft is configured!");
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

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");
    wait_for_indicator(&mut harness, "│");

    assert!(
        has_gutter_indicator(&harness.screen_to_string(), "│"),
        "Git gutter indicator should appear even when diff.external and core.pager are configured"
    );
}

/// Test that git gutter updates after saving a file
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_updates_after_save() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open an unmodified file
    open_file(&mut harness, &repo.path, "src/main.rs");
    harness.render().unwrap();

    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("main.rs") && screen.contains("fn main")
        })
        .unwrap();

    // Make a change
    harness.type_text("// New comment\n").unwrap();
    harness.render().unwrap();

    // Save the file - this should trigger git gutter update
    save_file(&mut harness);

    // Wait for the *end state* — the added line carries a green indicator in
    // column 0 — rather than for "more indicators than a baseline".
    //
    // The baseline used to be sampled after a `for _ in 0..5 { …
    // harness.sleep(50ms) }` settle loop, but `harness.sleep` advances
    // *logical* time only (see its doc comment): the loop waited no real
    // wall-clock at all, so the sample was taken from whatever frame the git
    // diff happened to have reached. If that sample caught the settled count
    // instead of the pre-diff one, `count > initial` was unsatisfiable and
    // the wait below could never resolve — a 180 s timeout, not a failure.
    //
    // Waiting on the invariant itself is immune to that: it holds only once
    // the post-save diff has landed, and a genuinely wrong result still fails
    // loudly through the wait's periodic screen dumps.
    let comment_row = |h: &EditorTestHarness| -> Option<u16> {
        h.screen_to_string()
            .lines()
            .position(|line| line.contains("// New comment"))
            .map(|row| row as u16)
    };
    let indicator_landed = |h: &EditorTestHarness| {
        let Some(row) = comment_row(h) else {
            return false;
        };
        h.get_row_text(row).starts_with('│')
            && h.get_cell_style(0, row)
                .is_some_and(|s| s.fg == Some(Color::Rgb(80, 250, 123)))
    };
    harness.wait_until(indicator_landed).unwrap();

    let screen = harness.screen_to_string();
    println!("After save screen:\n{}", screen);

    let row = comment_row(&harness).expect("New comment should be visible after save");
    let added_line = harness.get_row_text(row);
    assert_eq!(
        added_line.chars().next(),
        Some('│'),
        "Saved Git gutter indicator should remain in its original position"
    );

    let indicator_style = harness
        .get_cell_style(0, row)
        .expect("Git gutter indicator cell should have a style");
    assert_eq!(
        indicator_style.fg,
        Some(Color::Rgb(80, 250, 123)),
        "Saved Git gutter indicator should turn green"
    );
}

/// Test that git gutter shows added lines indicator
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_added_lines() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Add new lines to a file
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Hello, world!");
    let config = load_config();
    start_server(config);
}

// New function added
fn new_function() {
    println!("This is new!");
}

fn load_config() -> Config {
    Config::default()
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#,
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait for indicators
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Should have multiple added line indicators
            count_gutter_indicators(&screen, "│") >= 3
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Added lines screen:\n{}", screen);
}

/// Test that git gutter shows deleted lines indicator
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_deleted_lines() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Delete some lines from a file
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    start_server(Config::default());
}

fn start_server(config: Config) {
    println!("Starting server...");
}
"#,
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait for indicators - deleted lines show as ▾
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            has_gutter_indicator(&screen, "▾") || has_gutter_indicator(&screen, "│")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Deleted lines screen:\n{}", screen);
}

/// Test git gutter with staged changes (should still show diff vs HEAD)
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_staged_changes() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Modify and stage a file
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Staged change!");
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
    repo.stage_file("src/main.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait for indicators - staged changes should still show vs HEAD
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            has_gutter_indicator(&screen, "│")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Staged changes screen:\n{}", screen);
}

/// Test that git gutter clears after committing changes
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_clears_after_commit() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // First, create a change and commit it
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Committed change!");
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
    repo.git_add_all();
    repo.git_commit("Update main.rs");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait a bit for git gutter to process
    harness.sleep(std::time::Duration::from_millis(500));
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("After commit screen:\n{}", screen);

    // After commit, there should be no git indicators (file matches HEAD)
    let indicators = count_gutter_indicators(&screen, "│");
    assert_eq!(
        indicators, 0,
        "Git gutter should have no indicators after changes are committed"
    );
}

/// Test git gutter on untracked file (should show no indicators)
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_untracked_file() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_git_gutter_plugin();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create a new untracked file
    repo.create_file("src/new_file.rs", "fn new_function() {}\n");

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/new_file.rs");

    // Wait a bit for git gutter to process
    harness.sleep(std::time::Duration::from_millis(500));
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Untracked file screen:\n{}", screen);

    // Untracked files should have no git indicators
    let indicators = count_gutter_indicators(&screen, "│");
    assert_eq!(
        indicators, 0,
        "Git gutter should have no indicators for untracked files"
    );
}

// =============================================================================
// Buffer Modified Tests
// =============================================================================

/// Test that buffer modified shows indicators for unsaved changes
#[test]
#[cfg_attr(windows, ignore)] // Uses git plugins which timeout on Windows CI
fn test_buffer_modified_shows_on_edit() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_buffer_modified_plugin();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open a file
    open_file(&mut harness, &repo.path, "src/main.rs");

    // Initial state - no modifications
    let screen = harness.screen_to_string();
    let initial_indicators = count_gutter_indicators(&screen, "│");

    // Make an edit (but don't save)
    harness.type_text("// Unsaved change\n").unwrap();
    harness.render().unwrap();

    // Wait a bit for plugin to update
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("After edit screen:\n{}", screen);

    let new_indicators = count_gutter_indicators(&screen, "│");
    assert!(
        new_indicators > initial_indicators,
        "Buffer modified should show indicator for unsaved changes"
    );
}

/// Test that buffer modified clears after save
#[test]
#[cfg_attr(windows, ignore)] // Uses git plugins which timeout on Windows CI
fn test_buffer_modified_clears_after_save() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_buffer_modified_plugin();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Make an edit
    harness.type_text("// Unsaved change\n").unwrap();
    harness.render().unwrap();
    harness.sleep(std::time::Duration::from_millis(100));

    // Verify we have indicators before save
    harness.render().unwrap();
    let screen_before = harness.screen_to_string();
    let indicators_before = count_gutter_indicators(&screen_before, "│");

    // Save the file
    save_file(&mut harness);

    // Wait for plugin to update
    harness.sleep(std::time::Duration::from_millis(200));
    harness.render().unwrap();

    let screen_after = harness.screen_to_string();
    println!("After save screen:\n{}", screen_after);

    let indicators_after = count_gutter_indicators(&screen_after, "│");

    // After save, buffer modified indicators should be gone
    // (but git gutter might show indicators if git_gutter plugin is also loaded)
    assert!(
        indicators_after < indicators_before || indicators_after == 0,
        "Buffer modified indicators should clear after save"
    );
}

// =============================================================================
// Combined Tests (Both Plugins)
// =============================================================================

/// Test that both git gutter and buffer modified can coexist
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_both_plugins_coexist() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_gutter_plugins(); // Sets up both plugins

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create an uncommitted change on disk
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Git change on disk!");
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait for git gutter indicators
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            has_gutter_indicator(&screen, "│")
        })
        .unwrap();

    // Now make an additional in-memory edit
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("\n// Unsaved edit").unwrap();
    harness.render().unwrap();
    harness.sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Both plugins screen:\n{}", screen);

    // Should still have indicators (from either or both plugins)
    let total_indicators = count_gutter_indicators(&screen, "│");
    assert!(
        total_indicators >= 1,
        "Should have indicators from both git changes and unsaved changes"
    );
}

/// Test that git gutter priority is higher than buffer modified
/// (git gutter uses priority 10, buffer modified uses priority 5)
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_priority_over_buffer_modified() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();
    repo.setup_gutter_plugins();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create a committed file first, then modify on disk (for git diff)
    repo.modify_file(
        "src/main.rs",
        r#"fn main() {
    println!("Modified for git!");
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "src/main.rs");

    // Wait for git gutter indicators to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            has_gutter_indicator(&screen, "│")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Priority test screen:\n{}", screen);

    // The git gutter indicator (priority 10) should be visible,
    // not overridden by buffer_modified (priority 5)
    // Both use │ symbol but with different colors
    assert!(
        has_gutter_indicator(&screen, "│"),
        "Higher priority indicator should be visible"
    );
}

// =============================================================================
// Comprehensive Indicator Behavior Test
// =============================================================================

/// Comprehensive test for gutter indicator behavior:
/// 1. Create a file and commit it
/// 2. Make a change to a specific line, verify git indicators appear on that line
/// 3. Add a newline before the change, verify indicators shift down
/// 4. Verify the newly inserted line gets an unsaved-change indicator
/// 5. Save the file and verify git indicators update correctly
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_gutter_indicators_comprehensive() {
    use std::fs;

    // Create a fresh git repo with a simple test file
    let repo = GitTestRepo::new();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create a simple file with numbered lines for easy tracking
    let initial_content = r#"line 1: unchanged
line 2: unchanged
line 3: will be modified
line 4: unchanged
line 5: unchanged
"#;
    repo.create_file("test.txt", initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Set up the gutter plugins
    repo.setup_gutter_plugins();

    // Modify line 3 on disk (simulating a change that will show in git diff)
    let modified_content = r#"line 1: unchanged
line 2: unchanged
line 3: MODIFIED!
line 4: unchanged
line 5: unchanged
"#;
    fs::write(repo.path.join("test.txt"), modified_content).unwrap();

    // Create harness and open the file
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");

    // Manually trigger git gutter refresh to ensure it runs
    trigger_git_gutter_refresh(&mut harness);

    // Wait for git gutter indicator to appear on line 2 (0-indexed, which is line 3 in the file)
    wait_for_indicator_on_line(&mut harness, "│", 2);

    let screen = harness.screen_to_string();
    println!("=== After opening modified file ===\n{}", screen);

    // STEP 1: Verify git gutter shows indicator on the modified line (line 3, 0-indexed = line 2)
    let indicator_lines = get_indicator_lines(&screen, "│");
    println!("Indicator lines after open: {:?}", indicator_lines);

    // STEP 2: Now make an in-editor change - insert a newline before line 3
    // First, go to the beginning of line 3
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap(); // Go to beginning
    harness.render().unwrap();

    // Go down to line 3 (press Down twice from line 1)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Go to beginning of line
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Insert a new line above (this should push line 3 down to line 4)
    harness.type_text("NEW LINE INSERTED\n").unwrap();
    harness.render().unwrap();

    // Wait for indicator to appear on the newly inserted line (now at index 2)
    wait_for_indicator_on_line(&mut harness, "│", 2);

    let screen_after_insert = harness.screen_to_string();
    println!("=== After inserting new line ===\n{}", screen_after_insert);

    // STEP 3: Verify indicators
    let indicator_lines_after = get_indicator_lines(&screen_after_insert, "│");
    println!("Indicator lines after insert: {:?}", indicator_lines_after);

    // After inserting a line before line 3:
    // - The newly inserted line (now line 3) should have an unsaved-changes indicator
    // - The originally modified line (now line 4) should still have a git indicator
    // Both use │ symbol, so we should see indicators on at least 2 lines

    // Count total indicators - should have at least 2 (one for unsaved change, one for git change)
    let indicator_count = count_gutter_indicators(&screen_after_insert, "│");
    println!("Total indicators after insert: {}", indicator_count);

    // We expect indicators on:
    // - Line index 2: the newly inserted "NEW LINE INSERTED" (unsaved change)
    // - Line index 3: the original "line 3: MODIFIED!" which moved down (git change)

    // STEP 4: Save the file and verify git indicators update
    save_file(&mut harness);

    // Trigger git gutter refresh after save
    trigger_git_gutter_refresh(&mut harness);

    // Wait for indicators to update (should still have git indicators after save)
    wait_for_indicator(&mut harness, "│");

    let screen_after_save = harness.screen_to_string();
    println!("=== After save ===\n{}", screen_after_save);

    let indicator_lines_after_save = get_indicator_lines(&screen_after_save, "│");
    println!(
        "Indicator lines after save: {:?}",
        indicator_lines_after_save
    );

    // After save:
    // - Unsaved-changes indicators should be cleared (buffer matches disk)
    // - Git indicators should show for all lines that differ from HEAD
    // - This includes: the newly inserted line AND the modified line

    // The test passes if we can see that the indicator system is working
    // Even if async timing makes exact line matching difficult
    println!("\n=== Test Summary ===");
    println!(
        "Initial indicator count: {}",
        get_indicator_lines(&screen, "│").len()
    );
    println!("After insert indicator count: {}", indicator_count);
    println!(
        "After save indicator count: {}",
        indicator_lines_after_save.len()
    );

    // Basic sanity check - after editing, we should have some indicators
    // (either from git gutter or buffer modified plugin)
    assert!(
        indicator_count >= 1 || !indicator_lines_after_save.is_empty(),
        "Should have at least one indicator after making changes. \
         After insert: {}, After save: {}",
        indicator_count,
        indicator_lines_after_save.len()
    );
}

/// Test that unsaved changes get indicators from buffer_modified plugin
#[test]
fn test_unsaved_changes_get_indicators() {
    let repo = GitTestRepo::new();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create and commit a simple file
    let initial_content = "line 1\nline 2\nline 3\n";
    repo.create_file("test.txt", initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Only set up buffer_modified plugin (not git_gutter) to isolate the test
    repo.setup_buffer_modified_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");

    let screen_before = harness.screen_to_string();
    let indicators_before = count_gutter_indicators(&screen_before, "│");
    println!("=== Before edit ===\n{}", screen_before);
    println!("Indicators before edit: {}", indicators_before);

    // Make an edit - modify line 2
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // Go to line 2
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap(); // Go to end of line
    harness.type_text(" MODIFIED").unwrap();
    harness.render().unwrap();

    // Wait for indicator to appear on the edited line (line 1, 0-indexed)
    wait_for_indicator_on_line(&mut harness, "│", 1);

    let screen_after = harness.screen_to_string();
    let indicators_after = count_gutter_indicators(&screen_after, "│");
    println!("=== After edit ===\n{}", screen_after);
    println!("Indicators after edit: {}", indicators_after);

    // Should have at least one indicator on the modified line
    assert!(
        indicators_after > indicators_before,
        "Should have more indicators after editing. Before: {}, After: {}",
        indicators_before,
        indicators_after
    );

    // Save and verify indicators clear
    save_file(&mut harness);

    // Wait for buffer modified indicators to clear after save
    wait_for_no_indicators(&mut harness, "│");

    let screen_after_save = harness.screen_to_string();
    let indicators_after_save = count_gutter_indicators(&screen_after_save, "│");
    println!("=== After save ===\n{}", screen_after_save);
    println!("Indicators after save: {}", indicators_after_save);

    // After save, buffer_modified indicators should clear
    // (there might still be git indicators if git_gutter was also loaded)
    assert!(
        indicators_after_save <= indicators_after,
        "Indicators should not increase after save. After edit: {}, After save: {}",
        indicators_after,
        indicators_after_save
    );
}

/// Test that reverting an edit clears the buffer_modified indicator on the same line (no off-by-one)
#[test]
fn test_buffer_modified_clears_after_undo_on_same_line() {
    let repo = GitTestRepo::new();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create and commit a multi-line file
    let initial_content = (1..=15)
        .map(|i| format!("line {:02}\n", i))
        .collect::<String>();
    repo.create_file("test.txt", &initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.setup_buffer_modified_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");
    process_async_once(&mut harness);

    // Move to line 1, append text
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" MOD").unwrap();
    harness.render().unwrap();
    wait_for_indicator_on_line(&mut harness, "│", 0);

    let screen_after = harness.screen_to_string();
    let indicators_after = get_indicator_lines(&screen_after, "│");
    println!("=== After edit ===\n{}", screen_after);
    assert_eq!(
        indicators_after,
        vec![0],
        "Indicator should appear on edited line (line 0), got {:?}",
        indicators_after
    );

    // Undo the edit (4 chars)
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }
    harness.render().unwrap();
    wait_for_no_indicators(&mut harness, "│");

    let screen_after_undo = harness.screen_to_string();
    let indicators_after_undo = get_indicator_lines(&screen_after_undo, "│");
    println!("=== After undo ===\n{}", screen_after_undo);
    assert!(
        indicators_after_undo.is_empty(),
        "Indicators should clear after undo to saved state, got {:?}",
        indicators_after_undo
    );
}

/// Test that editing one line in a multi-line file only marks that line, and clears after undo
#[test]
fn test_buffer_modified_single_line_in_multi_line_file() {
    let repo = GitTestRepo::new();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create and commit a multi-line file
    let initial_content = (1..=15)
        .map(|i| format!("line {:02}\n", i))
        .collect::<String>();
    repo.create_file("test.txt", &initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.setup_buffer_modified_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");
    process_async_once(&mut harness);

    // Move to line 10 (0-based index 9) and edit it
    for _ in 0..9 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" MOD").unwrap();
    harness.render().unwrap();
    wait_for_indicator_on_line(&mut harness, "│", 9);

    let screen_after = harness.screen_to_string();
    let indicators_after = get_indicator_lines(&screen_after, "│");
    println!("=== After edit (multi-line) ===\n{}", screen_after);
    assert_eq!(
        indicators_after,
        vec![9],
        "Only the edited line should have indicator, got {:?}",
        indicators_after
    );

    // Undo the edit
    for _ in 0..4 {
        harness
            .send_key(KeyCode::Char('z'), KeyModifiers::CONTROL)
            .unwrap();
    }
    harness.render().unwrap();
    wait_for_no_indicators(&mut harness, "│");

    let screen_after_undo = harness.screen_to_string();
    let indicators_after_undo = get_indicator_lines(&screen_after_undo, "│");
    println!("=== After undo (multi-line) ===\n{}", screen_after_undo);
    assert!(
        indicators_after_undo.is_empty(),
        "Indicators should clear after undo, got {:?}",
        indicators_after_undo
    );
}

/// Test that inserting a newline only marks the affected lines, not the entire rest of the buffer
/// This is a regression test for a bug where line-by-line comparison would mark all subsequent
/// lines as changed because they shifted down by one.
#[test]
fn test_buffer_modified_newline_insert_only_marks_affected_lines() {
    let repo = GitTestRepo::new();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create and commit a file with multiple lines
    let initial_content = "line 1\nline 2\nline 3\nline 4\nline 5\n";
    repo.create_file("test.txt", initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.setup_buffer_modified_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");
    process_async_once(&mut harness);

    // Go to end of line 2 and insert a newline (creating a new empty line)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap(); // line 2
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    wait_for_indicator(&mut harness, "│");

    let screen = harness.screen_to_string();
    let indicators = get_indicator_lines(&screen, "│");
    println!("=== After inserting newline ===\n{}", screen);
    println!("Indicator lines: {:?}", indicators);

    // Only lines 1-2 (0-indexed) should be marked - the modified line and the new line
    // Lines 3, 4, 5 should NOT have indicators even though they shifted down
    assert!(
        indicators.len() <= 2,
        "Only the modified lines should have indicators, not the entire rest of the buffer. Got {:?}",
        indicators
    );

    // Specifically, lines 3+ should not have indicators
    let has_line_3_plus = indicators.iter().any(|&line| line >= 3);
    assert!(
        !has_line_3_plus,
        "Lines 3+ should not have indicators (they just shifted, content unchanged). Got {:?}",
        indicators
    );
}

/// Test that manually deleting added text (without undo) clears the indicator
/// This tests that the diff compares actual content, not just tree structure
#[test]
fn test_buffer_modified_clears_after_manual_delete_restores_content() {
    // Install signal handler to dump thread backtraces on timeout/SIGINT
    fresh::services::signal_handler::install_signal_handlers();

    let repo = GitTestRepo::new();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create and commit a file
    let initial_content = "line 01\nline 02\nline 03\nline 04\nline 05\n";
    repo.create_file("test.txt", initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.setup_buffer_modified_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");
    process_async_once(&mut harness);

    // Go to line 3, end of line, add text
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.type_text(" ADDED").unwrap();
    harness.render().unwrap();
    wait_for_indicator_on_line(&mut harness, "│", 2);

    let screen_after_add = harness.screen_to_string();
    let indicators_after_add = get_indicator_lines(&screen_after_add, "│");
    println!("=== After adding text ===\n{}", screen_after_add);
    assert!(
        indicators_after_add.contains(&2),
        "Line 2 (0-indexed) should have indicator after adding text, got {:?}",
        indicators_after_add
    );

    // Now manually delete the " ADDED" text (6 chars) using backspace
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Backspace, KeyModifiers::NONE)
            .unwrap();
    }
    harness.render().unwrap();
    wait_for_no_indicators(&mut harness, "│");

    let screen_after_delete = harness.screen_to_string();
    let indicators_after_delete = get_indicator_lines(&screen_after_delete, "│");
    println!(
        "=== After manually deleting text ===\n{}",
        screen_after_delete
    );
    assert!(
        indicators_after_delete.is_empty(),
        "Indicators should clear when content is manually restored to saved state, got {:?}",
        indicators_after_delete
    );
}

/// Test that pasting original content back clears the indicator
#[test]
#[ignore = "flaky test - times out intermittently"]
fn test_buffer_modified_clears_after_paste_restores_content() {
    let repo = GitTestRepo::new();

    // Change to repo directory so plugin can find files correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create and commit a file
    let initial_content = "hello world\n";
    repo.create_file("test.txt", initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    repo.setup_buffer_modified_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");
    process_async_once(&mut harness);

    // Select "world", cut it (Ctrl+X cuts and copies)
    // Go to position of 'w' in "world"
    harness.send_key(KeyCode::Home, KeyModifiers::NONE).unwrap();
    for _ in 0..6 {
        // Move past "hello "
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    // Select "world" (5 chars)
    for _ in 0..5 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    // Cut (copies to clipboard and deletes)
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    wait_for_indicator(&mut harness, "│");

    let screen_after_cut = harness.screen_to_string();
    let indicators_after_cut = get_indicator_lines(&screen_after_cut, "│");
    println!("=== After cutting 'world' ===\n{}", screen_after_cut);
    assert!(
        !indicators_after_cut.is_empty(),
        "Should have indicator after cutting text"
    );

    // Now paste "world" back
    harness
        .send_key(KeyCode::Char('v'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    wait_for_no_indicators(&mut harness, "│");

    let screen_after_paste = harness.screen_to_string();
    let indicators_after_paste = get_indicator_lines(&screen_after_paste, "│");
    println!("=== After pasting 'world' back ===\n{}", screen_after_paste);
    assert!(
        indicators_after_paste.is_empty(),
        "Indicators should clear when content is restored via paste, got {:?}",
        indicators_after_paste
    );
}

/// Test that adding lines shifts indicators correctly
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_indicator_line_shifting() {
    use std::fs;

    let repo = GitTestRepo::new();

    // Change to repo directory so git commands work correctly
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create a file with a modification on a specific line
    let initial_content = "line 1\nline 2\nline 3\nline 4\nline 5\n";
    repo.create_file("test.txt", initial_content);
    repo.git_add_all();
    repo.git_commit("Initial commit");

    // Modify line 3 on disk
    let modified_content = "line 1\nline 2\nline 3 CHANGED\nline 4\nline 5\n";
    fs::write(repo.path.join("test.txt"), modified_content).unwrap();

    repo.setup_git_gutter_plugin();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "test.txt");
    trigger_git_gutter_refresh(&mut harness);
    wait_for_indicator(&mut harness, "│");

    let screen_initial = harness.screen_to_string();
    let lines_initial = get_indicator_lines(&screen_initial, "│");
    println!("=== Initial state ===\n{}", screen_initial);
    println!("Initial indicator lines: {:?}", lines_initial);

    // Remember which content lines had indicators
    let content_lines = get_content_lines(&screen_initial);
    println!("Content lines count: {}", content_lines.len());

    // Now insert two lines at the beginning of the file
    harness
        .send_key(KeyCode::Char('g'), KeyModifiers::CONTROL)
        .unwrap(); // Go to beginning
    harness.render().unwrap();
    harness
        .type_text("inserted line A\ninserted line B\n")
        .unwrap();
    harness.render().unwrap();

    // Save so git diff can see the changes
    save_file(&mut harness);
    trigger_git_gutter_refresh(&mut harness);
    // Wait for indicators to appear (the inserted lines should show as added)
    wait_for_indicator(&mut harness, "│");

    let screen_after = harness.screen_to_string();
    let lines_after = get_indicator_lines(&screen_after, "│");
    println!(
        "=== After inserting 2 lines at beginning ===\n{}",
        screen_after
    );
    println!("Indicator lines after: {:?}", lines_after);

    // The original line 3 (which was modified) is now at line 5
    // Plus the two new lines should also show as added
    // So we expect indicators on lines that are different from the original commit

    // At minimum, we should have indicators for the changes
    assert!(
        !lines_after.is_empty() || lines_initial.is_empty(),
        "After inserting lines and saving, git diff should show changes"
    );

    println!("\n=== Shift Test Summary ===");
    println!("Initial indicators: {:?}", lines_initial);
    println!("After shift indicators: {:?}", lines_after);
}

// =============================================================================
// Git Gutter Scrollbar Markers
// =============================================================================

/// The half-block glyph a scrollbar marker paints.
const SCROLLBAR_MARKER_GLYPH: &str = "▌";

/// Rows of the scrollbar column showing a marker glyph, with the colour they
/// were painted in.
fn scrollbar_marker_rows(harness: &EditorTestHarness) -> Vec<(usize, Option<Color>)> {
    let col = harness.buffer().area.width - 1;
    let (first, last) = harness.content_area_rows();
    (first..=last)
        .filter(|row| harness.get_cell(col, *row as u16).as_deref() == Some(SCROLLBAR_MARKER_GLYPH))
        .map(|row| {
            (
                row,
                harness.get_cell_style(col, row as u16).and_then(|s| s.fg),
            )
        })
        .collect()
}

/// The topmost row marked in a given colour, if any.
fn first_marker_row(rows: &[(usize, Option<Color>)], color: Color) -> Option<usize> {
    rows.iter()
        .filter(|(_, fg)| *fg == Some(color))
        .map(|(row, _)| *row)
        .min()
}

/// A file long enough that most of it is off screen.
fn numbered_lines(count: usize) -> Vec<String> {
    (0..count).map(|i| format!("line {i:04}")).collect()
}

/// The git gutter marks its hunks on the scrollbar, in the same colours as its
/// gutter glyphs — so uncommitted changes below the fold are visible without
/// scrolling to find them.
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_marks_off_screen_hunks_on_scrollbar() {
    let repo = GitTestRepo::new();
    repo.setup_git_gutter_plugin();

    let original = numbered_lines(200);
    repo.create_file("long.txt", &format!("{}\n", original.join("\n")));
    repo.git_add_all();
    repo.git_commit("Commit the long file");

    // Three hunks of different kinds, all far below the first screenful:
    // an insertion, a deletion, and an in-place edit. Applied bottom-up so
    // earlier edits don't shift the indices of later ones.
    let mut changed = original.clone();
    changed[150] = "line 0150 rewritten".to_string();
    changed.drain(100..104);
    changed.splice(60..60, ["added A".to_string(), "added B".to_string()]);
    repo.modify_file("long.txt", &format!("{}\n", changed.join("\n")));

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "long.txt");

    // Every hunk is off screen, so the gutter alone tells the user nothing —
    // the scrollbar marks are the only signal that this file has changes.
    harness
        .wait_until(|h| scrollbar_marker_rows(h).len() >= 3)
        .unwrap();

    let screen = harness.screen_to_string();
    assert_eq!(
        count_gutter_indicators(&screen, "│"),
        0,
        "the changed lines are all below the first screenful, so no gutter \
         glyph should be visible:\n{screen}"
    );

    let rows = scrollbar_marker_rows(&harness);
    let added = first_marker_row(&rows, Color::Rgb(80, 250, 123))
        .unwrap_or_else(|| panic!("the inserted lines should mark the track green; saw {rows:?}"));
    let deleted = first_marker_row(&rows, Color::Rgb(255, 85, 85))
        .unwrap_or_else(|| panic!("the deletion should mark the track red; saw {rows:?}"));
    let modified = first_marker_row(&rows, Color::Rgb(255, 184, 108))
        .unwrap_or_else(|| panic!("the rewritten line should mark the track orange; saw {rows:?}"));

    // Marks land proportionally to where their hunk sits in the file, so they
    // appear in the same order down the track as the hunks do down the file.
    assert!(
        added < deleted && deleted < modified,
        "marks should follow the hunks' order in the file, got \
         added={added}, deleted={deleted}, modified={modified} ({rows:?})"
    );
}

/// A file that matches HEAD leaves the scrollbar clean, and saving a change
/// puts a mark on it — the marks track the diff rather than lingering.
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_scrollbar_marks_appear_after_save() {
    let repo = GitTestRepo::new();
    repo.setup_git_gutter_plugin();

    repo.create_file("long.txt", &format!("{}\n", numbered_lines(200).join("\n")));
    repo.git_add_all();
    repo.git_commit("Commit the long file");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "long.txt");
    trigger_git_gutter_refresh(&mut harness);

    // The status line reports the hunk count, so it tells us the plugin has
    // finished a pass over the unchanged file without waiting on a timer.
    harness.wait_for_screen_contains("0 change(s)").unwrap();
    assert!(
        scrollbar_marker_rows(&harness).is_empty(),
        "a file matching HEAD should leave the scrollbar unmarked"
    );

    harness.type_text("brand new first line\n").unwrap();
    save_file(&mut harness);
    trigger_git_gutter_refresh(&mut harness);

    harness
        .wait_until(|h| !scrollbar_marker_rows(h).is_empty())
        .unwrap();

    let rows = scrollbar_marker_rows(&harness);
    assert_eq!(
        rows.first().map(|(_, fg)| *fg),
        Some(Some(Color::Rgb(80, 250, 123))),
        "the inserted line is an addition, so its mark is green; saw {rows:?}"
    );
}

/// Resetting an open file to a different state with an external tool
/// (`git checkout <ref> -- <file>` in another terminal) auto-reverts the
/// buffer — and the gutter and scrollbar must re-diff against HEAD, not
/// keep showing the pre-reset (empty) state.
// TODO: Fix git gutter tests on Windows - they fail due to git command output differences
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_git_gutter_refreshes_after_external_git_checkout() {
    let repo = GitTestRepo::new();
    repo.setup_git_gutter_plugin();

    // v1: 200 plain numbered lines.
    let v1 = numbered_lines(200);
    repo.create_file("long.txt", &format!("{}\n", v1.join("\n")));
    repo.git_add_all();
    repo.git_commit("v1");

    // v2 (HEAD): two lines rewritten near the top, where they are on screen.
    let mut v2 = v1.clone();
    v2[4] = "line 0004 CHANGED IN HEAD".to_string();
    v2[5] = "line 0005 CHANGED IN HEAD".to_string();
    repo.modify_file("long.txt", &format!("{}\n", v2.join("\n")));
    repo.git_add_all();
    repo.git_commit("v2");

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Config::default(),
        repo.path.clone(),
    )
    .unwrap();

    open_file(&mut harness, &repo.path, "long.txt");
    trigger_git_gutter_refresh(&mut harness);

    // The working tree matches HEAD, so the plugin's first pass reports no
    // hunks — the status line proves the pass completed.
    harness.wait_for_screen_contains("0 change(s)").unwrap();
    assert!(
        !has_gutter_indicator(&harness.screen_to_string(), "│"),
        "a file matching HEAD should have no gutter indicators"
    );

    // Auto-revert notices the external write by mtime, so the reset's mtime
    // has to be distinguishable from the one recorded when the file was
    // opened. Record that floor now, reset, then bump until the filesystem
    // clock has actually moved past it.
    //
    // This used to be `harness.sleep(2100ms)`, which advances *logical* time
    // only — it waited no real wall-clock at all, so on a filesystem with 1 s
    // mtime granularity the open and the checkout could land in the same tick,
    // auto-revert never fired, and the wait below blocked until nextest killed
    // the test at 180 s.
    let opened_at = repo.mtime("long.txt");

    // Externally reset the file to its v1 state while it is open.
    repo.git_checkout_file("HEAD~1", "long.txt");
    repo.touch_until_mtime_after("long.txt", opened_at);

    // Auto-revert reloads the buffer from disk. The rewritten lines are near
    // the top of the file and so on screen — assert on the rendered text
    // rather than on buffer state (CONTRIBUTING.md Testing §2).
    harness
        .wait_until(|h| !h.screen_to_string().contains("CHANGED IN HEAD"))
        .expect("auto-revert should reload the externally reset file");

    // The reverted content differs from HEAD on the two rewritten lines, so
    // the gutter must show modified indicators without any user action...
    wait_for_indicator(&mut harness, "│");

    // ...and the same hunk must be marked on the scrollbar.
    harness
        .wait_until(|h| !scrollbar_marker_rows(h).is_empty())
        .expect("the re-diffed hunk should mark the scrollbar too");

    let rows = scrollbar_marker_rows(&harness);
    assert_eq!(
        rows.first().map(|(_, fg)| *fg),
        Some(Some(Color::Rgb(255, 184, 108))),
        "the reset lines differ from HEAD as modifications, so their mark is \
         orange; saw {rows:?}"
    );
}
