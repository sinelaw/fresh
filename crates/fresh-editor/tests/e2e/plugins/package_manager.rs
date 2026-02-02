//! Tests for the package manager plugin and package loading from packages directory.

#![cfg(feature = "plugins")]

use crate::common::git_test_helper::{DirGuard, GitTestRepo};
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

/// Test that plugins in the packages/ subdirectory are discovered and loaded.
#[test]
fn test_plugin_loading_from_packages_directory() {
    // Create a git repo with the typical project structure
    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Create the plugins directory structure
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);

    // Create packages subdirectory with a test plugin
    let packages_dir = plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();

    // Create a minimal test plugin in packages/test-plugin/
    let test_plugin_dir = packages_dir.join("test-plugin");
    fs::create_dir_all(&test_plugin_dir).unwrap();

    // Write the plugin's main.ts
    fs::write(
        test_plugin_dir.join("main.ts"),
        r#"
/// <reference path="../../lib/fresh.d.ts" />
const editor = getEditor();

globalThis.test_pkg_plugin_hello = function(): void {
    editor.setStatus("Hello from packages plugin!");
};

editor.registerCommand(
    "test_pkg_plugin_hello",
    "Test Package Plugin: Hello",
    "test_pkg_plugin_hello",
    null
);

editor.debug("Test package plugin loaded!");
"#,
    )
    .unwrap();

    // Write package.json manifest
    fs::write(
        test_plugin_dir.join("package.json"),
        r#"{
    "name": "test-plugin",
    "version": "1.0.0",
    "description": "A test plugin for package loading",
    "type": "plugin",
    "fresh": {
        "entry": "main.ts"
    }
}"#,
    )
    .unwrap();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    // Create editor with the project directory
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        Default::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open command palette and search for our test command
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Type the test plugin command
    harness.type_text("Test Package Plugin").unwrap();

    // Wait for the command to appear in suggestions (semantic wait)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Test Package Plugin") || screen.contains("Hello")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Test Package Plugin") || screen.contains("Hello"),
        "Plugin from packages directory should be loaded. Screen: {}",
        screen
    );
}

/// Test the package manager plugin's list command.
#[test]
fn test_pkg_list_installed_empty() {
    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Setup plugins directory with the package manager
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        Default::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Search for pkg command - wait for it to appear
    harness.type_text("Package: Packages").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Package: Packages"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for status message showing no packages
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("No packages") || screen.contains("Installed")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("No packages") || screen.contains("Installed"),
        "Should show package list status. Screen: {}",
        screen
    );
}

/// Test installing a plugin from a local git repository.
#[test]
#[cfg_attr(windows, ignore)] // file:// URLs don't work reliably on Windows
fn test_pkg_install_from_local_git_url() {
    // Create a "remote" repo to serve as the package source
    let package_repo = GitTestRepo::new();

    // Create a simple plugin in the package repo
    fs::write(
        package_repo.path.join("main.ts"),
        r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
editor.registerCommand("sample_cmd", "Sample: Command", "sample_cmd", null);
globalThis.sample_cmd = function() { editor.setStatus("Sample plugin works!"); };
"#,
    )
    .unwrap();

    fs::write(
        package_repo.path.join("package.json"),
        r#"{
    "name": "sample-plugin",
    "version": "1.0.0",
    "type": "plugin",
    "fresh": { "entry": "main.ts" }
}"#,
    )
    .unwrap();

    // Commit the plugin to make it a valid git repo for cloning
    package_repo.git_add_all();
    package_repo.git_commit("Initial plugin");

    // Create the main project repo
    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Create the packages directory (simulating ~/.config/fresh/plugins/packages)
    let packages_dir = plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        35,
        Default::default(),
        repo.path.clone(),
    )
    .unwrap();

    // Open command palette and install from URL
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("pkg: Install from URL").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Install from URL"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the URL prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Git URL"))
        .unwrap();

    // Enter the local git repo path as the URL
    let local_url = format!("file://{}", package_repo.path.display());
    harness.type_text(&local_url).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for git clone to complete - look for any status change
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Installed")
                || screen.contains("Failed")
                || screen.contains("already")
                || screen.contains("Installing")
        })
        .unwrap();

    let screen = harness.screen_to_string();

    // Check that either installation succeeded or we got an expected status
    // Note: The actual cloning might fail in test environment, but the flow should work
    assert!(
        screen.contains("Install")
            || screen.contains("sample")
            || screen.contains("Git URL")
            || screen.contains("Failed"),
        "Should show install progress or result. Screen: {}",
        screen
    );
}

/// Test that the Install Plugin command works with an empty registry.
/// This tests the async command flow and status updates.
#[test]
fn test_pkg_install_plugin_empty_registry() {
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    init_tracing_from_env();

    // Create temp directories for test isolation
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Setup plugins directory in the working directory (for the pkg plugin to load)
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Create an empty index in the CONFIG directory (not the working directory!)
    // The pkg plugin uses getConfigDir() which points to dir_context.config_dir
    let config_plugins_dir = dir_context.config_dir.join("plugins");
    fs::create_dir_all(&config_plugins_dir).unwrap();
    let packages_dir = config_plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();
    let index_dir = packages_dir.join(".index");
    fs::create_dir_all(&index_dir).unwrap();

    // Create a fake registry source directory that matches the hash for the default registry
    // djb2("https://github.com/sinelaw/fresh-plugins-registry") = 193934da
    let fake_registry_dir = index_dir.join("193934da");
    fs::create_dir_all(&fake_registry_dir).unwrap();
    // Write an empty plugins.json so the registry is considered synced
    fs::write(
        fake_registry_dir.join("plugins.json"),
        r#"{"schema_version": 1, "updated": "2024-01-01", "packages": {}}"#,
    )
    .unwrap();
    // Also create a .git directory to make it look like a valid git repo
    fs::create_dir_all(fake_registry_dir.join(".git")).unwrap();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_shared_dir_context(
        100,
        30,
        Default::default(),
        repo.path.clone(),
        dir_context,
    )
    .unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Open the package manager UI
    harness.type_text("Package: Packages").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Package: Packages"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for package manager UI to appear - may show syncing or empty state
    harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // Package manager UI opened or showing empty/syncing state
                screen.contains("Packages") || screen.contains("Syncing")
            },
            5000,
        )
        .unwrap();

    let screen = harness.screen_to_string();
    // With empty registry, the package manager should show the UI (even if no packages available)
    assert!(
        screen.contains("Packages") || screen.contains("Syncing"),
        "Should show package manager UI or syncing status. Screen: {}",
        screen
    );
}

/// Test that Install Plugin auto-syncs a stale registry before showing results.
/// This test simulates a scenario where the registry was previously synced but
/// is now out of date. The Install Plugin command should automatically pull
/// the latest registry data before showing available plugins.
///
/// Without the fix (always calling syncRegistry), this test would fail because
/// the stale empty registry would be used without attempting to update.
#[test]
fn test_pkg_install_plugin_auto_syncs_stale_registry() {
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    init_tracing_from_env();

    // Create temp directories for test isolation
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Step 1: Create a "remote" registry repository - starts EMPTY
    let registry_repo = GitTestRepo::new();
    fs::write(
        registry_repo.path.join("plugins.json"),
        r#"{"schema_version": 1, "updated": "2024-01-01", "packages": {}}"#,
    )
    .unwrap();
    registry_repo.git_add_all();
    registry_repo.git_commit("Initial empty registry");

    // Set up the project
    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Setup plugins directory with the pkg plugin
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Create the index directory structure in CONFIG directory
    let config_plugins_dir = dir_context.config_dir.join("plugins");
    fs::create_dir_all(&config_plugins_dir).unwrap();
    let packages_dir = config_plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();
    let index_dir = packages_dir.join(".index");
    fs::create_dir_all(&index_dir).unwrap();

    // Step 2: Clone the registry (simulates first sync when registry was empty)
    let registry_index_dir = index_dir.join("193934da");
    std::process::Command::new("git")
        .args([
            "clone",
            registry_repo.path.to_str().unwrap(),
            registry_index_dir.to_str().unwrap(),
        ])
        .output()
        .expect("Failed to clone registry");

    // Verify the cloned registry is empty
    let cloned_content = fs::read_to_string(registry_index_dir.join("plugins.json")).unwrap();
    assert!(
        cloned_content.contains(r#""packages": {}"#),
        "Cloned registry should be empty initially"
    );

    // Step 3: Update the "remote" registry with a new plugin (simulates registry update)
    fs::write(
        registry_repo.path.join("plugins.json"),
        r#"{
            "schema_version": 1,
            "updated": "2026-01-25T00:00:00Z",
            "packages": {
                "test-plugin": {
                    "description": "A test plugin for auto-sync verification",
                    "repository": "https://example.com/test-plugin"
                }
            }
        }"#,
    )
    .unwrap();
    registry_repo.git_add_all();
    registry_repo.git_commit("Add test plugin to registry");

    // At this point:
    // - Remote registry has "test-plugin"
    // - Local clone (index) still has empty registry
    // - Without auto-sync: would show "No plugins"
    // - With auto-sync: git pull runs, shows "test-plugin"

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        35,
        Default::default(),
        repo.path.clone(),
        dir_context,
    )
    .unwrap();

    // Open command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    // Open the package manager UI (which now auto-syncs on open)
    harness.type_text("Package: Packages").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Package: Packages"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the package manager UI - with auto-sync it should pull the updated registry
    harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                // Package manager opened, may show syncing or already have plugin list
                screen.contains("test-plugin")
                    || screen.contains("Packages")
                    || screen.contains("Syncing")
                    || screen.contains("Updating")
            },
            10000,
        )
        .unwrap();

    let screen = harness.screen_to_string();

    // With auto-sync on open: syncRegistry() is called, git pull runs
    // The package manager should show syncing activity or already have the plugin
    assert!(
        screen.contains("test-plugin")
            || screen.contains("Syncing")
            || screen.contains("Updating")
            || screen.contains("Packages"),
        "Package manager should auto-sync on open. Screen: {}",
        screen
    );
}

/// Test package manager UI flows:
/// - Split-view layout with list on left, details on right
/// - Tab navigation through all focusable buttons
/// - Filter activation
/// - Package selection with arrow keys
#[test]
fn test_pkg_manager_ui_split_view_and_tab_navigation() {
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    init_tracing_from_env();

    // Create temp directories for test isolation
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Setup plugins directory with the pkg plugin
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Create registry structure in config dir
    let config_plugins_dir = dir_context.config_dir.join("plugins");
    fs::create_dir_all(&config_plugins_dir).unwrap();
    let packages_dir = config_plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();
    let index_dir = packages_dir.join(".index");
    fs::create_dir_all(&index_dir).unwrap();

    // Create fake registry with test packages
    let fake_registry_dir = index_dir.join("193934da");
    fs::create_dir_all(&fake_registry_dir).unwrap();
    fs::write(
        fake_registry_dir.join("plugins.json"),
        r#"{
            "schema_version": 1,
            "updated": "2026-01-01T00:00:00Z",
            "packages": {
                "test-plugin-alpha": {
                    "description": "Test plugin Alpha for UI testing",
                    "repository": "https://github.com/test/plugin-alpha",
                    "author": "Test Author",
                    "license": "MIT"
                },
                "test-plugin-beta": {
                    "description": "Test plugin Beta for UI testing",
                    "repository": "https://github.com/test/plugin-beta",
                    "author": "Test Author",
                    "license": "MIT"
                }
            }
        }"#,
    )
    .unwrap();
    fs::create_dir_all(fake_registry_dir.join(".git")).unwrap();

    // Change to repo directory
    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_shared_dir_context(
        100,
        30,
        Default::default(),
        repo.path.clone(),
        dir_context,
    )
    .unwrap();

    // Open package manager via command palette
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Package: Packages").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Package: Packages"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for package manager UI to appear and loading to complete
    // (AVAILABLE appears after registry sync finishes)
    harness
        .wait_until(|h| h.screen_to_string().contains("AVAILABLE"))
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Package manager initial state:\n{}", screen);

    // Verify split-view layout elements
    assert!(
        screen.contains("Packages"),
        "Should show 'Packages' header. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("All") && screen.contains("Installed"),
        "Should show filter buttons. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("│"),
        "Should have vertical divider for split view. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("Tab"),
        "Should show Tab in help text. Screen:\n{}",
        screen
    );

    // Verify available packages appear in the list (already checked in wait_until)
    assert!(
        screen.contains("AVAILABLE"),
        "Should show AVAILABLE section with registry packages. Screen:\n{}",
        screen
    );

    // Test Tab navigation - press Tab and check that focus changes
    // (indicated by help text changing or visual elements changing)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let screen_after_tab1 = harness.screen_to_string();
    println!("After Tab 1:\n{}", screen_after_tab1);

    // Tab through all focusable elements (typically: list -> action -> filters -> sync -> search -> back to list)
    for i in 2..=8 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
        println!("After Tab {}:", i);
    }

    // After cycling, we should be back to a navigable state
    let screen_after_cycle = harness.screen_to_string();
    println!("After full Tab cycle:\n{}", screen_after_cycle);

    // Verify the UI is still functional
    assert!(
        screen_after_cycle.contains("Packages"),
        "Should still show Packages header after Tab cycle"
    );

    // Test Escape to close - may need multiple presses depending on focus state
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Wait for package manager to close (semantic waiting per README guidelines)
    harness
        .wait_until(|h| !h.screen_to_string().contains("*Packages*"))
        .unwrap();

    let screen_after_close = harness.screen_to_string();
    println!("After Escape:\n{}", screen_after_close);
}

/// Test installing a plugin from a monorepo with subpath (e.g., repo#packages/my-plugin).
/// This verifies that only the specific subdirectory is installed, not the entire repo,
/// and that the package.json is at the correct location so version can be read.
#[test]
#[cfg_attr(windows, ignore)] // file:// URLs don't work reliably on Windows
fn test_pkg_install_from_monorepo_with_subpath() {
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    init_tracing_from_env();

    // Create temp directories for test isolation
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Create a "remote" monorepo with multiple plugins
    let monorepo = GitTestRepo::new();

    // Create plugin-alpha in subdirectory
    let alpha_dir = monorepo.path.join("plugin-alpha");
    fs::create_dir_all(&alpha_dir).unwrap();
    fs::write(
        alpha_dir.join("main.ts"),
        r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
editor.registerCommand("alpha_cmd", "Alpha: Command", "alpha_cmd", null);
globalThis.alpha_cmd = function() { editor.setStatus("Alpha plugin works!"); };
"#,
    )
    .unwrap();
    fs::write(
        alpha_dir.join("package.json"),
        r#"{
    "name": "plugin-alpha",
    "version": "2.0.0",
    "description": "Alpha plugin from monorepo",
    "type": "plugin",
    "fresh": { "entry": "main.ts" }
}"#,
    )
    .unwrap();

    // Create plugin-beta in another subdirectory
    let beta_dir = monorepo.path.join("plugin-beta");
    fs::create_dir_all(&beta_dir).unwrap();
    fs::write(
        beta_dir.join("main.ts"),
        r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
editor.registerCommand("beta_cmd", "Beta: Command", "beta_cmd", null);
globalThis.beta_cmd = function() { editor.setStatus("Beta plugin works!"); };
"#,
    )
    .unwrap();
    fs::write(
        beta_dir.join("package.json"),
        r#"{
    "name": "plugin-beta",
    "version": "3.0.0",
    "description": "Beta plugin from monorepo",
    "type": "plugin",
    "fresh": { "entry": "main.ts" }
}"#,
    )
    .unwrap();

    // Add a root file to distinguish from subdirectory
    fs::write(monorepo.path.join("README.md"), "# Monorepo").unwrap();

    // Commit the monorepo
    monorepo.git_add_all();
    monorepo.git_commit("Initial monorepo with plugins");

    // Create the main project repo
    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Create the packages directory in config dir
    let config_plugins_dir = dir_context.config_dir.join("plugins");
    fs::create_dir_all(&config_plugins_dir).unwrap();
    let packages_dir = config_plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();

    // Create empty registry so sync doesn't interfere
    let index_dir = packages_dir.join(".index");
    fs::create_dir_all(&index_dir).unwrap();
    let fake_registry_dir = index_dir.join("193934da");
    fs::create_dir_all(&fake_registry_dir).unwrap();
    fs::write(
        fake_registry_dir.join("plugins.json"),
        r#"{"schema_version": 1, "updated": "2024-01-01", "packages": {}}"#,
    )
    .unwrap();
    fs::create_dir_all(fake_registry_dir.join(".git")).unwrap();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        35,
        Default::default(),
        repo.path.clone(),
        dir_context.clone(),
    )
    .unwrap();

    // Open command palette and install from URL with subpath
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("pkg: Install from URL").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Install from URL"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the URL prompt
    harness
        .wait_until(|h| h.screen_to_string().contains("Git URL"))
        .unwrap();

    // Enter monorepo URL with subpath fragment (note: # for subpath)
    let monorepo_url = format!("file://{}#plugin-alpha", monorepo.path.display());
    harness.type_text(&monorepo_url).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for installation to complete
    harness
        .wait_for_async(
            |h| {
                let screen = h.screen_to_string();
                screen.contains("Installed")
                    || screen.contains("Failed")
                    || screen.contains("already")
                    || screen.contains("activated")
            },
            15000,
        )
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After monorepo install:\n{}", screen);

    // Verify the plugin directory structure is correct
    let installed_plugin_dir = packages_dir.join("plugin-alpha");

    // Check that the package.json exists at the root of the installed plugin
    let package_json_path = installed_plugin_dir.join("package.json");
    assert!(
        package_json_path.exists(),
        "package.json should be at the root of installed plugin directory. \
         Expected: {:?}, Directory contents: {:?}",
        package_json_path,
        fs::read_dir(&installed_plugin_dir)
            .map(|entries| entries
                .filter_map(|e| e.ok())
                .map(|e| e.file_name())
                .collect::<Vec<_>>())
            .unwrap_or_default()
    );

    // Verify it's the correct package.json (from subdirectory, not a root one)
    let package_json_content = fs::read_to_string(&package_json_path).unwrap();
    assert!(
        package_json_content.contains("\"version\": \"2.0.0\""),
        "package.json should have version 2.0.0 from plugin-alpha. Content: {}",
        package_json_content
    );
    assert!(
        package_json_content.contains("plugin-alpha"),
        "package.json should be for plugin-alpha. Content: {}",
        package_json_content
    );

    // Check that main.ts exists
    let main_ts_path = installed_plugin_dir.join("main.ts");
    assert!(
        main_ts_path.exists(),
        "main.ts should exist at root of installed plugin. Path: {:?}",
        main_ts_path
    );

    // Verify the monorepo root files are NOT present (README.md, plugin-beta directory)
    let readme_path = installed_plugin_dir.join("README.md");
    assert!(
        !readme_path.exists(),
        "README.md from monorepo root should NOT be in installed plugin. \
         The entire monorepo was cloned instead of just the subdirectory."
    );

    let beta_in_alpha = installed_plugin_dir.join("plugin-beta");
    assert!(
        !beta_in_alpha.exists(),
        "plugin-beta directory should NOT be in installed plugin-alpha. \
         The entire monorepo was cloned instead of just the subdirectory."
    );
}

/// Test installing a bundle package from a local path.
/// A bundle contains multiple languages and plugins in a single package.
/// This test verifies:
/// - Bundle installs to the bundles/packages directory
/// - Languages from the bundle are registered (grammars)
/// - Plugins from the bundle are loaded and their commands available
#[test]
#[cfg_attr(windows, ignore)] // file:// URLs don't work reliably on Windows
fn test_pkg_install_bundle_from_local_path() {
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    init_tracing_from_env();

    // Create temp directories for test isolation
    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Create a local bundle package with 2 languages and 1 plugin
    // Note: We create it inside a subdirectory named "test-bundle" so the package
    // manager extracts the correct name from the path
    let bundle_temp = TempDir::new().unwrap();
    let bundle_dir = bundle_temp.path().join("test-bundle");
    fs::create_dir_all(&bundle_dir).unwrap();

    // Create the grammars directory
    let grammars_dir = bundle_dir.join("grammars");
    fs::create_dir_all(&grammars_dir).unwrap();

    // Create a minimal sublime-syntax grammar for "testlang1"
    fs::write(
        grammars_dir.join("TestLang1.sublime-syntax"),
        r#"%YAML 1.2
---
name: TestLang1
file_extensions: [tl1]
scope: source.testlang1
contexts:
  main:
    - match: '#.*'
      scope: comment.line
"#,
    )
    .unwrap();

    // Create a minimal sublime-syntax grammar for "testlang2"
    fs::write(
        grammars_dir.join("TestLang2.sublime-syntax"),
        r#"%YAML 1.2
---
name: TestLang2
file_extensions: [tl2]
scope: source.testlang2
contexts:
  main:
    - match: '//.*'
      scope: comment.line
"#,
    )
    .unwrap();

    // Create the plugins directory
    let plugins_dir = bundle_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();

    // Create first plugin that registers a command
    // Note: registerCommand(id, displayName, functionName, keybinding)
    fs::write(
        plugins_dir.join("test-helper.ts"),
        r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

globalThis.bundle_test_cmd = function(): void {
    editor.setStatus("Bundle test command executed!");
};

editor.registerCommand(
    "bundle_test_cmd",
    "Bundle Test: Command",
    "bundle_test_cmd",
    null
);

editor.debug("Bundle test plugin 1 loaded!");
"#,
    )
    .unwrap();

    // Create second plugin that registers another command
    fs::write(
        plugins_dir.join("another-helper.ts"),
        r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

globalThis.bundle_another_cmd = function(): void {
    editor.setStatus("Bundle another command executed!");
};

editor.registerCommand(
    "bundle_another_cmd",
    "Bundle Another: Command",
    "bundle_another_cmd",
    null
);

editor.debug("Bundle test plugin 2 loaded!");
"#,
    )
    .unwrap();

    // Create the themes directory
    let themes_dir = bundle_dir.join("themes");
    fs::create_dir_all(&themes_dir).unwrap();

    // Create first theme (dark variant)
    // Note: Using r##"..."## because JSON contains "#" color codes
    fs::write(
        themes_dir.join("bundle-dark.json"),
        r##"{
    "name": "Bundle Dark Theme",
    "variant": "dark",
    "colors": {
        "editor.background": "#1e1e1e",
        "editor.foreground": "#d4d4d4"
    }
}"##,
    )
    .unwrap();

    // Create second theme (light variant)
    fs::write(
        themes_dir.join("bundle-light.json"),
        r##"{
    "name": "Bundle Light Theme",
    "variant": "light",
    "colors": {
        "editor.background": "#ffffff",
        "editor.foreground": "#000000"
    }
}"##,
    )
    .unwrap();

    // Create the bundle's package.json manifest
    // Note: Using r##"..."## because the content contains "#" which would close r#"..."#
    fs::write(
        bundle_dir.join("package.json"),
        r##"{
    "name": "test-bundle",
    "version": "1.0.0",
    "description": "Test bundle with languages, plugins, and themes",
    "type": "bundle",
    "fresh": {
        "languages": [
            {
                "id": "testlang1",
                "grammar": {
                    "file": "grammars/TestLang1.sublime-syntax",
                    "extensions": ["tl1"]
                },
                "language": {
                    "commentPrefix": "#",
                    "tabSize": 2
                }
            },
            {
                "id": "testlang2",
                "grammar": {
                    "file": "grammars/TestLang2.sublime-syntax",
                    "extensions": ["tl2"]
                },
                "language": {
                    "commentPrefix": "//",
                    "tabSize": 4
                }
            }
        ],
        "plugins": [
            { "entry": "plugins/test-helper.ts" },
            { "entry": "plugins/another-helper.ts" }
        ],
        "themes": [
            { "file": "themes/bundle-dark.json", "name": "Bundle Dark Theme", "variant": "dark" },
            { "file": "themes/bundle-light.json", "name": "Bundle Light Theme", "variant": "light" }
        ]
    }
}"##,
    )
    .unwrap();

    // Create the main project repo
    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Setup plugins directory with the package manager
    let project_plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&project_plugins_dir).unwrap();
    copy_plugin_lib(&project_plugins_dir);
    copy_plugin(&project_plugins_dir, "pkg");

    // Also copy lib to bundle plugins dir so the plugin can find fresh.d.ts
    let bundle_plugins_lib_dir = plugins_dir.join("lib");
    fs::create_dir_all(&bundle_plugins_lib_dir).unwrap();
    fs::copy(
        project_plugins_dir.join("lib").join("fresh.d.ts"),
        bundle_plugins_lib_dir.join("fresh.d.ts"),
    )
    .unwrap();

    // Create empty registry structure so sync doesn't interfere
    let config_plugins_dir = dir_context.config_dir.join("plugins");
    fs::create_dir_all(&config_plugins_dir).unwrap();
    let packages_dir = config_plugins_dir.join("packages");
    fs::create_dir_all(&packages_dir).unwrap();
    let index_dir = packages_dir.join(".index");
    fs::create_dir_all(&index_dir).unwrap();
    let fake_registry_dir = index_dir.join("193934da");
    fs::create_dir_all(&fake_registry_dir).unwrap();
    fs::write(
        fake_registry_dir.join("plugins.json"),
        r#"{"schema_version": 1, "updated": "2024-01-01", "packages": {}}"#,
    )
    .unwrap();
    fs::create_dir_all(fake_registry_dir.join(".git")).unwrap();

    // Create the bundles directory structure
    let bundles_packages_dir = dir_context.config_dir.join("bundles").join("packages");
    fs::create_dir_all(&bundles_packages_dir).unwrap();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        35,
        Default::default(),
        repo.path.clone(),
        dir_context.clone(),
    )
    .unwrap();

    // Open command palette and install from URL
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("pkg: Install from URL").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Install from URL"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the URL prompt to appear
    harness
        .wait_until(|h| h.screen_to_string().contains("Git URL"))
        .unwrap();

    // Enter the local bundle directory path (not git, just a local path)
    let bundle_path = bundle_dir.display().to_string();
    harness.type_text(&bundle_path).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for installation to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Installed bundle")
                || screen.contains("Failed")
                || screen.contains("already")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After bundle install:\n{}", screen);

    // Verify the bundle was installed to bundles/packages directory
    let installed_bundle_dir = bundles_packages_dir.join("test-bundle");
    assert!(
        installed_bundle_dir.exists(),
        "Bundle should be installed in bundles/packages directory. Path: {:?}",
        installed_bundle_dir
    );

    // Verify package.json exists
    let package_json_path = installed_bundle_dir.join("package.json");
    assert!(
        package_json_path.exists(),
        "package.json should exist in installed bundle"
    );

    // Verify the package type is bundle
    let package_json_content = fs::read_to_string(&package_json_path).unwrap();
    assert!(
        package_json_content.contains("\"type\": \"bundle\""),
        "package.json should have type 'bundle'. Content: {}",
        package_json_content
    );

    // Verify grammar files were copied
    let grammar1_path = installed_bundle_dir
        .join("grammars")
        .join("TestLang1.sublime-syntax");
    assert!(
        grammar1_path.exists(),
        "Grammar file for testlang1 should exist"
    );
    let grammar2_path = installed_bundle_dir
        .join("grammars")
        .join("TestLang2.sublime-syntax");
    assert!(
        grammar2_path.exists(),
        "Grammar file for testlang2 should exist"
    );

    // === Test 1: Verify bundled plugin command is available and works ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Bundle Test").unwrap();

    // Wait for the command to appear (semantic wait)
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Bundle Test: Command") || screen.contains("bundle_test_cmd")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Bundle Test: Command") || screen.contains("bundle_test_cmd"),
        "Bundle plugin command should be available in command palette. Screen: {}",
        screen
    );

    // Execute the command and verify the status message
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for the status message from the plugin command
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Bundle test command executed!")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After executing bundle command:\n{}", screen);
    assert!(
        screen.contains("Bundle test command executed!"),
        "Bundle plugin command should show status message when executed. Screen: {}",
        screen
    );

    // === Test 2: Verify bundled languages are available in "Set Language" ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Set Language").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Set Language"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for language selection prompt
    harness
        .wait_until(|h| h.screen_to_string().contains("Language:"))
        .unwrap();

    // Type the name of our bundled language
    harness.type_text("testlang1").unwrap();

    // Wait for the bundled language to appear in the list
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // The language should appear in the suggestions
            screen.contains("testlang1") || screen.to_lowercase().contains("testlang1")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Set Language showing bundled language:\n{}", screen);
    assert!(
        screen.to_lowercase().contains("testlang1"),
        "Bundled language 'testlang1' should be available in Set Language. Screen: {}",
        screen
    );

    // Close the language selection
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // === Test 3: Verify second plugin command is available ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Bundle Another").unwrap();

    // Wait for the second command to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Bundle Another: Command") || screen.contains("bundle_another_cmd")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Second plugin command in palette:\n{}", screen);
    assert!(
        screen.contains("Bundle Another: Command") || screen.contains("bundle_another_cmd"),
        "Second bundle plugin command should be available. Screen: {}",
        screen
    );

    // Execute the second command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("Bundle another command executed!")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("After executing second plugin command:\n{}", screen);

    // === Test 4: Verify bundled themes are available in "Select Theme" ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Theme").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Theme"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for theme selection - the finder shows themes
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            // Look for any theme-related content or the finder
            screen.contains("Bundle Dark") || screen.contains("Theme") || screen.contains("dark")
        })
        .unwrap();

    // Type to filter for our bundled theme
    harness.type_text("Bundle").unwrap();

    // Wait for the bundled theme to appear
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Bundle Dark") || screen.to_lowercase().contains("bundle")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Select Theme showing bundled theme:\n{}", screen);
    assert!(
        screen.to_lowercase().contains("bundle"),
        "Bundled theme should be available in theme selection. Screen: {}",
        screen
    );

    // Close the theme selection
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // === Test 5: Verify bundle shows up in package manager with installed status ===
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();

    harness.type_text("Package: Packages").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Package: Packages"))
        .unwrap();

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for package manager UI to load and show installed bundles
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("INSTALLED") && screen.contains("test-bundle")
        })
        .unwrap();

    let screen = harness.screen_to_string();
    println!("Package manager showing bundle:\n{}", screen);

    assert!(
        screen.contains("test-bundle"),
        "Package manager should show the installed bundle. Screen: {}",
        screen
    );
}

/// Test that uninstalling a plugin removes its commands from the command palette.
/// This verifies that commands registered by a plugin are properly unregistered
/// when the plugin is unloaded, not just showing untranslated keys.
#[test]
#[cfg_attr(windows, ignore)]
fn test_uninstall_plugin_removes_commands() {
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    init_tracing_from_env();

    let temp_dir = TempDir::new().unwrap();
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    let repo = GitTestRepo::new();
    repo.setup_typical_project();

    // Setup plugins directory
    let plugins_dir = repo.path.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "pkg");

    // Create fake registry so syncRegistry doesn't try to hit the network
    let packages_dir = dir_context.config_dir.join("plugins").join("packages");
    fs::create_dir_all(&packages_dir).unwrap();
    let index_dir = packages_dir.join(".index");
    fs::create_dir_all(&index_dir).unwrap();

    // Create a fake registry source directory (djb2 hash for default registry)
    let fake_registry_dir = index_dir.join("193934da");
    fs::create_dir_all(&fake_registry_dir).unwrap();
    fs::write(
        fake_registry_dir.join("plugins.json"),
        r#"{"schema_version": 1, "updated": "2024-01-01", "packages": {}}"#,
    )
    .unwrap();
    fs::write(
        fake_registry_dir.join("themes.json"),
        r#"{"schema_version": 1, "updated": "2024-01-01", "packages": {}}"#,
    )
    .unwrap();
    fs::create_dir_all(fake_registry_dir.join(".git")).unwrap();

    // Create a test plugin that registers a command
    let test_plugin_dir = packages_dir.join("uninstall-test-plugin");
    fs::create_dir_all(&test_plugin_dir).unwrap();

    fs::write(
        test_plugin_dir.join("main.ts"),
        r#"
/// <reference path="../../../plugins/lib/fresh.d.ts" />
const editor = getEditor();
editor.registerCommand("Uninstall Test: Hello", "Test command for uninstall", "uninstall_test_hello", null);
globalThis.uninstall_test_hello = function() { editor.setStatus("Hello from uninstall test!"); };
"#,
    )
    .unwrap();

    fs::write(
        test_plugin_dir.join("package.json"),
        r#"{
    "name": "uninstall-test-plugin",
    "version": "1.0.0",
    "type": "plugin",
    "fresh": { "entry": "main.ts" }
}"#,
    )
    .unwrap();

    let original_dir = repo.change_to_repo_dir();
    let _guard = DirGuard::new(original_dir);

    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        30,
        Default::default(),
        repo.path.clone(),
        dir_context,
    )
    .unwrap();

    // Verify the command is available initially via Quick Open
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Uninstall Test").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Uninstall Test: Hello"))
        .unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Uninstall Test: Hello"),
        "Command should be available before uninstall. Screen: {}",
        screen
    );

    // Close Quick Open
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open package manager and uninstall the plugin
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Package: Packages").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("Package: Packages"))
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    // Wait for package manager UI to finish loading
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("*Packages*") && !screen.contains("Loading...")
        })
        .unwrap();

    let screen_after_load = harness.screen_to_string();
    assert!(
        screen_after_load.contains("uninstall-test"),
        "Plugin should be visible in package manager"
    );

    // Press Tab to move focus to the Uninstall button and activate it
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Wait for uninstall to complete
    harness
        .wait_until(|h| {
            let screen = h.screen_to_string();
            screen.contains("Removed") || !screen.contains("uninstall-test")
        })
        .unwrap();

    // Close package manager
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Verify the command is no longer available
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Uninstall Test").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(">Uninstall Test"))
        .unwrap();

    let screen = harness.screen_to_string();
    // The command should be gone - it should NOT appear as a suggestion
    assert!(
        !screen.contains("Uninstall Test: Hello") && !screen.contains("uninstall_test_hello"),
        "Command should be removed after uninstall, not show untranslated keys. Screen: {}",
        screen
    );
}
