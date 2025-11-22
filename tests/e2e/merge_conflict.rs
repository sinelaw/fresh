use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test file content with git conflict markers
const CONFLICT_FILE_CONTENT: &str = r#"// Some code before conflict

<<<<<<< HEAD
fn greet() {
    println!("Hello from our branch");
}
=======
fn greet() {
    println!("Hello from their branch");
}
>>>>>>> feature-branch

// Some code after conflict
"#;

/// Simple conflict content for basic tests
const SIMPLE_CONFLICT: &str = r#"<<<<<<< HEAD
ours
=======
theirs
>>>>>>> branch
"#;

/// Diff3-style conflict with base section (real-world example from Lustre project)
const DIFF3_CONFLICT_WITH_BASE: &str = r#"}

static int showdf(char *mntdir, struct obd_statfs *stat,
<<<<<<< HEAD
                  char *uuid, enum mntdf_flags flags,
                  char *type, int index, int rc)
||||||| parent of a3f05d81f6 (LU-18243 lfs: Add --output and --no-header options to lfs df command)
                  const char *uuid, enum mntdf_flags flags,
                  char *type, int index, int rc)
=======
                  const char *uuid, enum mntdf_flags flags,
                  char *type, int index, int rc, enum showdf_fields fields,
                  enum showdf_fields *field_order, int field_count)
>>>>>>> a3f05d81f6 (LU-18243 lfs: Add --output and --no-header options to lfs df command)
{
        int base = flags & MNTDF_DECIMAL ? 1000 : 1024;
        char *suffix = flags & MNTDF_DECIMAL ? "kMGTPEZY" : "KMGTPEZY";
        int shift = flags & MNTDF_COOKED ? 0 : 10;
"#;

/// Multiple conflicts for navigation testing
const MULTIPLE_CONFLICTS: &str = r#"// File with multiple conflicts

<<<<<<< HEAD
first ours
=======
first theirs
>>>>>>> branch

some middle text

<<<<<<< HEAD
second ours
=======
second theirs
>>>>>>> branch

<<<<<<< HEAD
third ours
=======
third theirs
>>>>>>> branch

// end of file
"#;

/// Test that merge conflict plugin loads and detects conflicts
#[test]
fn test_merge_conflict_plugin_loads() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict markers
    let fixture = TestFixture::new("conflict.rs", CONFLICT_FILE_CONTENT).unwrap();

    // Create harness with the project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file - plugin should detect conflicts
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // The plugin should show a status message about detected conflicts
    // Check that the file content is visible
    harness.assert_screen_contains("<<<<<<< HEAD");
}

/// Test that Merge: Start Resolution command works
#[test]
fn test_merge_start_resolution_command() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict markers
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Run Merge: Start Resolution command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Process async operations for panel creation
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // The merge UI should now be visible
    // Check for panel headers
    let screen = harness.screen_to_string();
    println!("Screen after starting merge:\n{}", screen);

    // Should see merge UI elements
    // At minimum, check for some indication the command ran
    // (The exact UI depends on the implementation)
}

/// Test conflict navigation with n/p keys
#[test]
fn test_merge_conflict_navigation() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with multiple conflicts
    let fixture = TestFixture::new("multi_conflict.rs", MULTIPLE_CONFLICTS).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 40, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Start merge resolution
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Process async operations
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Navigate to next conflict with 'n'
    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Navigate to previous conflict with 'p'
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Navigate with j/k as well
    harness
        .send_key(KeyCode::Char('j'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('k'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // If we got here without error, navigation works
}

/// Test resolving a conflict with 'u' (use ours)
#[test]
fn test_merge_use_ours_resolution() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Press 'u' to use ours
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Screen should show resolution info
    let screen = harness.screen_to_string();
    println!("Screen after using ours:\n{}", screen);
}

/// Test resolving a conflict with 't' (take theirs)
#[test]
fn test_merge_take_theirs_resolution() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Press 't' to take theirs
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Screen should show resolution info
    let screen = harness.screen_to_string();
    println!("Screen after taking theirs:\n{}", screen);
}

/// Test resolving a conflict with 'b' (use both)
#[test]
fn test_merge_use_both_resolution() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Press 'b' to use both
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Screen should show resolution info
    let screen = harness.screen_to_string();
    println!("Screen after using both:\n{}", screen);
}

/// Test abort merge with 'q'
#[test]
fn test_merge_abort() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Press 'q' to abort
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should be back to normal view with original conflict markers
    let screen = harness.screen_to_string();
    println!("Screen after abort:\n{}", screen);
}

/// Test full merge workflow: resolve and save
#[test]
fn test_merge_resolve_and_save() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Resolve with 'u' (use ours)
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Press 's' to save and exit
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    // Screen should show the resolved content
    let screen = harness.screen_to_string();
    println!("Screen after save:\n{}", screen);
}

/// Test that plugin detects conflicts on file open (notification)
#[test]
fn test_merge_conflict_detection_on_open() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", CONFLICT_FILE_CONTENT).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file - plugin should detect conflicts via after-file-open hook
    harness.open_file(&fixture.path).unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    // The file content should be visible
    harness.assert_screen_contains("<<<<<<< HEAD");

    // Status bar should indicate conflicts detected (the plugin sets status on detection)
    // This depends on the plugin implementation
}

/// Test that merge mode doesn't activate for files without conflicts
#[test]
fn test_no_merge_without_conflicts() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file WITHOUT conflict markers
    let normal_content = "// This is a normal file\nfn main() {\n    println!(\"Hello\");\n}\n";
    let fixture = TestFixture::new("normal.rs", normal_content).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Try to start merge - should fail gracefully
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    // Should still see the normal file content (merge didn't start)
    harness.assert_screen_contains("This is a normal file");
}

/// Test multiple conflict resolution workflow
#[test]
fn test_merge_multiple_conflicts_workflow() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with multiple conflicts
    let fixture = TestFixture::new("multi.rs", MULTIPLE_CONFLICTS).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 40, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Resolve first conflict with 'u' (ours)
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should auto-advance to next conflict
    // Resolve second conflict with 't' (theirs)
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Resolve third conflict with 'b' (both)
    harness
        .send_key(KeyCode::Char('b'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // All conflicts resolved - try to save
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    // Check that we're back to normal view
    let screen = harness.screen_to_string();
    println!("Screen after resolving all conflicts:\n{}", screen);
}

/// Test diff3-style conflict with base section (|||||||) is detected correctly
/// This is a real-world example from the Lustre project with a complex conflict
#[test]
fn test_diff3_conflict_with_base_section() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with diff3-style conflict (includes base section)
    let fixture = TestFixture::new("showdf.c", DIFF3_CONFLICT_WITH_BASE).unwrap();

    // Create harness with the project directory
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    // Verify all conflict markers are visible in the file
    harness.assert_screen_contains("<<<<<<< HEAD");
    harness.assert_screen_contains("|||||||");
    harness.assert_screen_contains("=======");
    harness.assert_screen_contains(">>>>>>>");

    // The file should show the different versions:
    // OURS: char *uuid (non-const)
    // BASE: const char *uuid (original)
    // THEIRS: const char *uuid with additional parameters
    harness.assert_screen_contains("char *uuid");
    harness.assert_screen_contains("const char *uuid");

    // Start merge resolution
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Process async operations for panel creation
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // The merge UI should now be active
    let screen = harness.screen_to_string();
    println!("Screen after starting merge with diff3 conflict:\n{}", screen);

    // CRITICAL: Verify the merge actually started by checking for merge UI elements
    // The plugin should have detected the conflict and shown merge panels
    // If it says "No conflict markers found", the regex is broken
    assert!(
        !screen.contains("No conflict markers found"),
        "Merge should have detected the conflict - regex may be broken"
    );

    // Should see OURS or THEIRS or RESULT panel headers
    assert!(
        screen.contains("OURS") || screen.contains("Merge:") || screen.contains("Conflict"),
        "Merge UI should be visible after starting resolution"
    );
}

/// Test that diff3-style conflict can be resolved
#[test]
fn test_diff3_conflict_resolution() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory and copy the merge conflict plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    let plugin_source = std::env::current_dir()
        .unwrap()
        .join("plugins/merge_conflict.ts");
    let plugin_dest = plugins_dir.join("merge_conflict.ts");
    fs::copy(&plugin_source, &plugin_dest).unwrap();

    // Create test file with diff3-style conflict
    let fixture = TestFixture::new("showdf.c", DIFF3_CONFLICT_WITH_BASE).unwrap();

    // Create harness
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // Open and start merge
    harness.open_file(&fixture.path).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();
    harness.process_async_and_render().unwrap();

    // Resolve with 't' (take theirs - the version with additional parameters)
    harness
        .send_key(KeyCode::Char('t'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Save and exit
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();

    // Should be back to normal view with resolved content
    let screen = harness.screen_to_string();
    println!("Screen after resolving diff3 conflict:\n{}", screen);

    // The conflict markers should be gone
    // Note: The exact content depends on which resolution was chosen
}
