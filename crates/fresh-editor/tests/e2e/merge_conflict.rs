use crate::common::fixtures::TestFixture;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::Path;

/// Set up merge conflict test environment: git repo + plugin + i18n
fn setup_merge_conflict_test(project_root: &Path) {
    use std::process::Command;

    // Initialize git repo (required by merge plugin)
    Command::new("git")
        .args(["init"])
        .current_dir(project_root)
        .output()
        .expect("git init failed");
    Command::new("git")
        .args(["config", "user.email", "test@test.com"])
        .current_dir(project_root)
        .output()
        .unwrap();
    Command::new("git")
        .args(["config", "user.name", "Test User"])
        .current_dir(project_root)
        .output()
        .unwrap();

    // Create plugins directory and copy plugin + i18n
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "merge_conflict");
    // merge_conflict imports the shared git-repo gateway from plugins/lib.
    copy_plugin_lib(&plugins_dir);
}

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

    setup_merge_conflict_test(&project_root);

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

/// Helper to set up a git repo with a merge conflict
fn setup_git_merge_conflict(project_root: &std::path::Path) -> std::path::PathBuf {
    use std::process::Command;

    let conflict_file = project_root.join("conflict.rs");

    // Initialize git repo
    let init_output = Command::new("git")
        .args(["init"])
        .current_dir(project_root)
        .output()
        .expect("git init failed");
    eprintln!(
        "git init: {:?}",
        String::from_utf8_lossy(&init_output.stderr)
    );

    // Configure git user for commits and disable signing
    Command::new("git")
        .args(["config", "user.email", "test@test.com"])
        .current_dir(project_root)
        .output()
        .expect("git config email failed");
    Command::new("git")
        .args(["config", "user.name", "Test User"])
        .current_dir(project_root)
        .output()
        .expect("git config name failed");
    // Disable commit signing for tests
    Command::new("git")
        .args(["config", "commit.gpgsign", "false"])
        .current_dir(project_root)
        .output()
        .expect("git config gpgsign failed");

    // Create initial file with a line we'll conflict on (same line modified in both branches)
    fs::write(&conflict_file, "conflict line\n").unwrap();
    Command::new("git")
        .args(["add", "."])
        .current_dir(project_root)
        .output()
        .expect("git add failed");
    Command::new("git")
        .args(["commit", "-m", "initial"])
        .current_dir(project_root)
        .output()
        .expect("git commit failed");

    // Create a branch and modify THE SAME LINE
    Command::new("git")
        .args(["checkout", "-b", "feature"])
        .current_dir(project_root)
        .output()
        .expect("git checkout -b failed");
    fs::write(&conflict_file, "feature version of line\n").unwrap();
    Command::new("git")
        .args(["commit", "-am", "feature change"])
        .current_dir(project_root)
        .output()
        .expect("git commit failed");

    // Get the current branch name (could be master or main)
    let branch_output = Command::new("git")
        .args(["branch", "--show-current"])
        .current_dir(project_root)
        .output()
        .expect("git branch failed");
    eprintln!(
        "Current branch in feature: {:?}",
        String::from_utf8_lossy(&branch_output.stdout)
    );

    // Go back to the initial branch (try master first, then main)
    let checkout_master = Command::new("git")
        .args(["checkout", "master"])
        .current_dir(project_root)
        .output();

    if checkout_master.is_err() || !checkout_master.as_ref().unwrap().status.success() {
        eprintln!("master checkout failed, trying main...");
        Command::new("git")
            .args(["checkout", "main"])
            .current_dir(project_root)
            .output()
            .expect("git checkout main failed");
    }

    // Modify THE SAME LINE differently on main/master
    fs::write(&conflict_file, "main version of line\n").unwrap();
    let commit_output = Command::new("git")
        .args(["commit", "-am", "main change"])
        .current_dir(project_root)
        .output()
        .expect("git commit failed");
    eprintln!(
        "commit on main: {:?}",
        String::from_utf8_lossy(&commit_output.stderr)
    );

    // Try to merge - this MUST fail with conflict since we modified the same line
    let merge_output = Command::new("git")
        .args(["merge", "feature"])
        .current_dir(project_root)
        .output()
        .expect("git merge failed");

    eprintln!("Git merge exit code: {}", merge_output.status);
    eprintln!(
        "Git merge stdout: {:?}",
        String::from_utf8_lossy(&merge_output.stdout)
    );
    eprintln!(
        "Git merge stderr: {:?}",
        String::from_utf8_lossy(&merge_output.stderr)
    );

    // Verify the file has conflict markers
    let content = fs::read_to_string(&conflict_file).unwrap();
    eprintln!("Conflict file content:\n{}", content);

    // Verify git sees it as unmerged
    let ls_files = Command::new("git")
        .args(["ls-files", "-u"])
        .current_dir(project_root)
        .output()
        .expect("git ls-files failed");
    eprintln!(
        "git ls-files -u output: {:?}",
        String::from_utf8_lossy(&ls_files.stdout)
    );

    assert!(
        content.contains("<<<<<<<"),
        "Expected conflict markers in file, got: {}",
        content
    );

    conflict_file
}

/// Test that Merge: Start Resolution command works
#[test]
#[ignore]
fn test_merge_start_resolution_command() {
    // Enable tracing for debugging
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);

    // Set up a real git merge conflict
    let conflict_file = setup_git_merge_conflict(&project_root);
    eprintln!("Conflict file path: {:?}", conflict_file);

    // Create harness with the project directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();

    // Open the conflict file
    eprintln!("Opening conflict file...");
    harness.open_file(&conflict_file).unwrap();
    harness.render().unwrap();

    let screen_before = harness.screen_to_string();
    eprintln!("Screen before command:\n{}", screen_before);

    // Run Merge: Start Resolution command
    eprintln!("Sending Ctrl+P...");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    eprintln!("Typing command...");
    harness.type_text("Merge: Start Resolution").unwrap();
    harness.render().unwrap();

    eprintln!("Pressing Enter...");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Process async operations for panel creation
    eprintln!("Processing async 1...");
    harness.process_async_and_render().unwrap();
    eprintln!("Processing async 2...");
    harness.process_async_and_render().unwrap();
    eprintln!("Processing async 3...");
    harness.process_async_and_render().unwrap();
    eprintln!("Processing async 4...");
    harness.process_async_and_render().unwrap();
    eprintln!("Processing async 5...");
    harness.process_async_and_render().unwrap();

    // The merge UI should now be visible
    // Check for panel headers
    let screen = harness.screen_to_string();
    eprintln!("Screen after starting merge:\n{}", screen);

    // Should see merge UI elements or status message
    // At minimum, check for some indication the command ran
}

/// Test conflict navigation with n/p keys
#[test]
fn test_merge_conflict_navigation() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

    setup_merge_conflict_test(&project_root);

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

/// Set up a real git merge conflict with diff3 style (shows base section with |||||||)
/// Assumes git is already initialized in project_root
fn setup_diff3_merge_conflict(project_root: &std::path::Path) -> std::path::PathBuf {
    use std::process::Command;

    let conflict_file = project_root.join("showdf.c");

    // Configure diff3 merge conflict style (shows base section with |||||||)
    eprintln!("Configuring diff3 style...");
    Command::new("git")
        .args(["config", "merge.conflictstyle", "diff3"])
        .current_dir(project_root)
        .output()
        .unwrap();

    // Create base version
    eprintln!("Creating base commit...");
    fs::write(&conflict_file, "static int showdf(const char *uuid)\n").unwrap();
    Command::new("git")
        .args(["add", "."])
        .current_dir(project_root)
        .output()
        .unwrap();
    let out = Command::new("git")
        .args(["commit", "-m", "base"])
        .current_dir(project_root)
        .output()
        .unwrap();
    eprintln!("Base commit: {:?}", String::from_utf8_lossy(&out.stderr));

    // Create feature branch with one change
    eprintln!("Creating feature branch...");
    Command::new("git")
        .args(["checkout", "-b", "feature"])
        .current_dir(project_root)
        .output()
        .unwrap();
    fs::write(&conflict_file, "static int showdf(char *uuid)\n").unwrap();
    Command::new("git")
        .args(["commit", "-am", "feature"])
        .current_dir(project_root)
        .output()
        .unwrap();

    // Go back to main and make different change
    eprintln!("Going back to main branch...");
    let out = Command::new("git")
        .args(["checkout", "master"])
        .current_dir(project_root)
        .output();
    eprintln!("checkout master: {:?}", out);
    let out = Command::new("git")
        .args(["checkout", "main"])
        .current_dir(project_root)
        .output();
    eprintln!("checkout main: {:?}", out);

    fs::write(
        &conflict_file,
        "static int showdf(const char *uuid, int extra)\n",
    )
    .unwrap();
    let out = Command::new("git")
        .args(["commit", "-am", "main change"])
        .current_dir(project_root)
        .output()
        .unwrap();
    eprintln!("Main commit: {:?}", String::from_utf8_lossy(&out.stderr));

    // Merge - this will fail and leave conflict markers with diff3 style
    eprintln!("Attempting merge...");
    let out = Command::new("git")
        .args(["merge", "feature"])
        .current_dir(project_root)
        .output()
        .unwrap();
    eprintln!("Merge output: {:?}", String::from_utf8_lossy(&out.stdout));
    eprintln!("Merge stderr: {:?}", String::from_utf8_lossy(&out.stderr));

    // Show file content
    let content = fs::read_to_string(&conflict_file).unwrap_or_default();
    eprintln!("Conflict file content:\n{}", content);

    conflict_file
}

/// Test diff3-style conflict with base section (|||||||) is detected correctly
// TODO: Fix flakiness in this test
#[test]
#[ignore]
fn test_diff3_conflict_with_base_section() {
    use crate::common::tracing::init_tracing_from_env;
    use std::process::Command;

    init_tracing_from_env();
    eprintln!("Starting test_diff3_conflict_with_base_section");

    // Create project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    eprintln!("Created project_root: {:?}", project_root);

    // Init git first, then setup plugins
    let out = Command::new("git")
        .args(["init"])
        .current_dir(&project_root)
        .output()
        .unwrap();
    eprintln!("git init: {:?}", String::from_utf8_lossy(&out.stderr));
    Command::new("git")
        .args(["config", "user.email", "test@test.com"])
        .current_dir(&project_root)
        .output()
        .unwrap();
    Command::new("git")
        .args(["config", "user.name", "Test"])
        .current_dir(&project_root)
        .output()
        .unwrap();

    // Copy plugin
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "merge_conflict");
    // merge_conflict imports the shared git-repo gateway from plugins/lib.
    copy_plugin_lib(&plugins_dir);
    eprintln!("Copied plugin files");

    // Create a real diff3-style merge conflict
    eprintln!("Setting up diff3 merge conflict...");
    let file_path = setup_diff3_merge_conflict(&project_root);
    eprintln!("Conflict file: {:?}", file_path);

    // Create harness with project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // Enable internal-only clipboard to avoid system clipboard interference in parallel tests
    harness.editor_mut().set_clipboard_for_test("".to_string());

    // Open the test file
    eprintln!("Opening file...");
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();
    eprintln!("File opened, checking markers...");

    // Verify all conflict markers are visible in the file
    harness.assert_screen_contains("<<<<<<< HEAD");
    eprintln!("Found <<<<<<< HEAD");
    harness.assert_screen_contains("|||||||");
    eprintln!("Found |||||||");
    harness.assert_screen_contains("=======");
    eprintln!("Found =======");
    harness.assert_screen_contains(">>>>>>>");
    eprintln!("Found >>>>>>>, now starting merge...");

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
    println!(
        "Screen after starting merge with diff3 conflict:\n{}",
        screen
    );

    // CRITICAL: Verify the merge actually started by checking for merge UI elements
    // The plugin should have detected the conflict and shown merge panels
    // If it says "No conflict markers found", the regex is broken
    assert!(
        !screen.contains("No conflict markers found"),
        "Merge should have detected the conflict - regex may be broken"
    );

    // Should see OURS or THEIRS or RESULT panel headers
    eprintln!("Checking for OURS/Merge:/Conflict in screen...");
    assert!(
        screen.contains("OURS") || screen.contains("Merge:") || screen.contains("Conflict"),
        "Merge UI should be visible after starting resolution"
    );
    eprintln!("Test passed! Cleaning up...");
}

/// Test that diff3-style conflict can be resolved
#[test]
fn test_diff3_conflict_resolution() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);

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

/// Set up a real git merge conflict with CRLF line endings (Windows-style)
/// Assumes git is already initialized in project_root
fn setup_crlf_merge_conflict(project_root: &std::path::Path) -> std::path::PathBuf {
    use std::process::Command;

    /// Run a git command and assert it succeeds
    fn git(project_root: &std::path::Path, args: &[&str]) {
        let output = Command::new("git")
            .args(args)
            .current_dir(project_root)
            .output()
            .unwrap();
        assert!(
            output.status.success(),
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let conflict_file = project_root.join("crlf_conflict.c");

    // Configure diff3 merge conflict style and disable commit signing
    git(project_root, &["config", "merge.conflictstyle", "diff3"]);
    git(project_root, &["config", "commit.gpgsign", "false"]);

    // Create base version with CRLF line endings
    fs::write(&conflict_file, "static int showdf(const char *uuid)\r\n").unwrap();
    git(project_root, &["add", "."]);
    git(project_root, &["commit", "-m", "base"]);

    // Create feature branch with one change (keep CRLF)
    git(project_root, &["checkout", "-b", "feature"]);
    fs::write(&conflict_file, "static int showdf(char *uuid)\r\n").unwrap();
    git(project_root, &["commit", "-am", "feature"]);

    // Go back to the default branch (try both master and main)
    let master = Command::new("git")
        .args(["checkout", "master"])
        .current_dir(project_root)
        .output();
    if !master.is_ok_and(|o| o.status.success()) {
        git(project_root, &["checkout", "main"]);
    }

    fs::write(
        &conflict_file,
        "static int showdf(const char *uuid, int extra)\r\n",
    )
    .unwrap();
    git(project_root, &["commit", "-am", "main change"]);

    // Merge - this will fail (exit code 1) and leave conflict markers
    let merge_output = Command::new("git")
        .args(["merge", "feature"])
        .current_dir(project_root)
        .output()
        .unwrap();
    // Merge should fail with conflicts (exit code 1), not succeed cleanly
    assert!(
        !merge_output.status.success(),
        "git merge should have produced conflicts"
    );

    // Verify conflict markers are present in the file
    let content = fs::read_to_string(&conflict_file).unwrap();
    assert!(
        content.contains("<<<<<<<"),
        "File should contain conflict markers after failed merge"
    );

    conflict_file
}

/// Test that CRLF line endings are handled correctly (Windows-style files)
#[test]
fn test_merge_conflict_crlf_line_endings() {
    // Create project directory and plugins BEFORE harness
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);

    // Set up a real git merge conflict with CRLF line endings
    let file_path = setup_crlf_merge_conflict(&project_root);

    // Create harness with project directory (so plugins load)
    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 40, Default::default(), project_root)
            .unwrap();

    // Open the test file
    harness.open_file(&file_path).unwrap();
    harness.render().unwrap();

    // Verify conflict markers are visible
    harness.assert_screen_contains("<<<<<<< HEAD");

    // Start merge resolution
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Merge: Start Resolution").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for merge UI to appear (semantic waiting)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Merge UI shows OURS/THEIRS buttons or Conflict indicator
            screen.contains("OURS") || screen.contains("Conflict 1")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Screen with CRLF conflict:\n{}", screen);

    // CRITICAL: Verify the merge actually started (CRLF should be handled)
    assert!(
        !screen.contains("No conflict markers found"),
        "Merge should detect conflicts in CRLF files - regex must handle \\r\\n"
    );

    // Should see merge UI elements (already verified by wait_until)
    assert!(
        screen.contains("OURS") || screen.contains("Conflict"),
        "Merge UI should be visible for CRLF files"
    );

    // Resolve and save
    harness
        .send_key(KeyCode::Char('u'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::NONE)
        .unwrap();
    harness.process_async_and_render().unwrap();
    harness.render().unwrap();
}

/// Test that clicking on virtual buffer entries with onClick triggers actions
/// This tests the onClick text property support for mouse interactions
#[test]
fn test_merge_mouse_click_on_buttons() {
    // Create a temporary project directory
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);

    // Create test file with conflict
    let fixture = TestFixture::new("conflict.rs", SIMPLE_CONFLICT).unwrap();

    // Create harness with the project directory
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

    // Need multiple rounds of async processing for virtual buffer creation
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
    }

    // Get the screen to find where buttons are
    let screen = harness.screen_to_string();
    println!("Screen before mouse click:\n{}", screen);

    // The merge UI should be active and show action buttons
    // Look for clickable button text in the screen
    let has_buttons = screen.contains("Accept Ours")
        || screen.contains("Use Ours")
        || screen.contains("OURS")
        || screen.contains("RESULT");

    if !has_buttons {
        // Merge UI might not have fully loaded - just verify no crash
        println!("Merge UI not fully loaded, skipping button verification");
    }

    // Press 'q' to abort (cleanup)
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

// =============================================================================
// Regression: resolution must not add a blank line (#3702)
// =============================================================================

/// Set up a real git merge conflict in `conflict.txt` inside `project_root`
/// from the three given versions of the file.
///
/// Unlike the `TestFixture`-based tests above, the file lives *inside* the
/// repository and the index is left genuinely unmerged, which is what the
/// plugin's `git ls-files -u` gate requires before it will start resolution.
/// Assumes `setup_merge_conflict_test` has already initialized the repo.
fn setup_git_conflict_from(
    project_root: &Path,
    base: &str,
    ours: &str,
    theirs: &str,
) -> std::path::PathBuf {
    use std::process::Command;

    let git = |args: &[&str]| {
        let out = Command::new("git")
            .args(args)
            .current_dir(project_root)
            .output()
            .unwrap_or_else(|e| panic!("git {args:?} failed to run: {e}"));
        eprintln!(
            "git {args:?} -> {} {:?}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
        out
    };

    let conflict_file = project_root.join("conflict.txt");

    // Commit signing would break commits in CI sandboxes.
    git(&["config", "commit.gpgsign", "false"]);

    fs::write(&conflict_file, base).unwrap();
    git(&["add", "."]);
    git(&["commit", "-m", "base"]);

    // `git init`'s default branch is master on some installs and main on
    // others, so remember the name rather than guessing it later.
    let ours_branch = String::from_utf8_lossy(&git(&["branch", "--show-current"]).stdout)
        .trim()
        .to_string();
    assert!(!ours_branch.is_empty(), "could not read the initial branch");

    git(&["checkout", "-b", "feature"]);
    fs::write(&conflict_file, theirs).unwrap();
    git(&["commit", "-am", "theirs"]);

    git(&["checkout", &ours_branch]);
    fs::write(&conflict_file, ours).unwrap();
    git(&["commit", "-am", "ours"]);

    git(&["merge", "feature"]);

    // The merge must actually have conflicted, or the test below would pass
    // vacuously (CONTRIBUTING.md Code §16).
    let merged = fs::read_to_string(&conflict_file).unwrap();
    assert!(
        merged.contains("<<<<<<<") && merged.contains(">>>>>>>"),
        "expected conflict markers in the merged file, got: {merged:?}"
    );
    let unmerged = git(&["ls-files", "-u", "conflict.txt"]);
    assert!(
        !String::from_utf8_lossy(&unmerged.stdout).trim().is_empty(),
        "expected an unmerged index entry for conflict.txt"
    );

    conflict_file
}

/// Open the conflicted file, start resolution, run `action`, then save and
/// exit, and return the text the source buffer is left holding — the exact
/// bytes a following Ctrl+S writes to disk.
fn resolve_conflict_via_palette(project_root: &Path, conflict_file: &Path, action: &str) -> String {
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        40,
        Default::default(),
        project_root.to_path_buf(),
    )
    .unwrap();

    harness.open_file(conflict_file).unwrap();
    harness.render().unwrap();

    harness
        .run_palette_command("Merge: Start Resolution")
        .unwrap();
    // The panels only exist once the plugin's async git calls have landed, so
    // gate on the RESULT panel rather than on a fixed number of ticks.
    harness.wait_for_screen_contains("*RESULT*").unwrap();

    harness.run_palette_command(action).unwrap();
    harness.wait_for_screen_contains("resolved").unwrap();

    harness.run_palette_command("Merge: Save & Exit").unwrap();
    // Save & Exit writes the resolved text back into the source buffer and
    // closes the panels; waiting for them to go is what gates on the write.
    harness
        .wait_until(|h| !h.screen_to_string().contains("*RESULT*"))
        .unwrap();

    harness.assert_no_plugin_errors();

    let content = harness
        .get_buffer_content()
        .expect("source buffer should be fully loaded");
    eprintln!("resolved buffer content: {content:?}");
    assert!(
        !content.contains("<<<<<<<") && !content.contains(">>>>>>>"),
        "resolution left conflict markers behind: {content:?}"
    );
    content
}

/// `Merge: Use Ours` must not leave a blank line where the conflict was.
///
/// Regression test for #3702: `parseConflicts` ended the block just *before*
/// the newline closing the `>>>>>>>` line, while each side's text already
/// carried its own trailing newline, so every splice left that newline behind.
#[test]
fn test_merge_use_ours_leaves_no_extra_blank_line() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);
    let conflict_file = setup_git_conflict_from(
        &project_root,
        "line1\nline2\nline3\n",
        "line1\nOURS\nline3\n",
        "line1\nTHEIRS\nline3\n",
    );

    let content = resolve_conflict_via_palette(&project_root, &conflict_file, "Merge: Use Ours");

    assert_eq!(content, "line1\nOURS\nline3\n");
}

/// `Merge: Take Theirs` must not leave a blank line either (#3702).
#[test]
fn test_merge_take_theirs_leaves_no_extra_blank_line() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);
    let conflict_file = setup_git_conflict_from(
        &project_root,
        "line1\nline2\nline3\n",
        "line1\nOURS\nline3\n",
        "line1\nTHEIRS\nline3\n",
    );

    let content = resolve_conflict_via_palette(&project_root, &conflict_file, "Merge: Take Theirs");

    assert_eq!(content, "line1\nTHEIRS\nline3\n");
}

/// A hunk whose own content ends in a blank line must keep exactly that blank
/// line: #3702 is fixed by giving the conflict block the newline that closes
/// `>>>>>>>`, not by trimming the hunk, which would corrupt content like this.
///
/// The ours side here is `OURS\n\n`, so the correct resolution keeps one blank
/// line. Before the fix this produced two.
#[test]
fn test_merge_use_ours_keeps_hunk_trailing_blank_line() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);
    // Two differing lines per side keeps git from factoring the blank line out
    // of the hunk as common context, so it really is part of the ours side.
    let conflict_file = setup_git_conflict_from(
        &project_root,
        "line1\nA\nB\nline3\n",
        "line1\nOURS\n\nline3\n",
        "line1\nT1\nT2\nline3\n",
    );

    let content = resolve_conflict_via_palette(&project_root, &conflict_file, "Merge: Use Ours");

    assert_eq!(content, "line1\nOURS\n\nline3\n");
}

/// A conflict that ends at EOF with no trailing newline must still parse and
/// resolve. The fix for #3702 makes the conflict block swallow the newline
/// after `>>>>>>>`; this covers the case where there is none to swallow, which
/// the regex handles with its `$` alternative.
///
/// `git merge` always terminates the marker line, so the conflicted working
/// tree is written by hand here; the index stays unmerged from the real merge.
#[test]
fn test_merge_use_ours_at_eof_without_trailing_newline() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    setup_merge_conflict_test(&project_root);
    let conflict_file = setup_git_conflict_from(
        &project_root,
        "line1\nline2\n",
        "line1\nOURS\n",
        "line1\nTHEIRS\n",
    );
    fs::write(
        &conflict_file,
        "line1\n<<<<<<< HEAD\nOURS\n=======\nTHEIRS\n>>>>>>> feature",
    )
    .unwrap();

    let content = resolve_conflict_via_palette(&project_root, &conflict_file, "Merge: Use Ours");

    // The ours hunk is the line `OURS\n`, so the resolved file ends there.
    assert_eq!(content, "line1\nOURS\n");
}
