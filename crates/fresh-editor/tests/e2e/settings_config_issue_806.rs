/// E2E tests for issue #806: Settings UI overwriting manual config.json changes
///
/// When the user manually edits config.json while Fresh is running, then uses
/// the Settings UI to change a simple setting and saves, the manual edits
/// should be preserved.
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;

fn send_text(harness: &mut EditorTestHarness, text: &str) {
    for c in text.chars() {
        harness
            .send_key(KeyCode::Char(c), KeyModifiers::NONE)
            .unwrap();
    }
}

/// Issue #806: External config.json edits lost when saving from Settings UI
///
/// Scenario:
/// 1. User starts Fresh with initial config
/// 2. User manually edits config.json (while Fresh is running) to add custom LSP settings
/// 3. User opens Settings UI and changes a simple setting (tab_size)
/// 4. User saves from Settings UI
/// 5. Expected: Manual LSP edits should be preserved
/// 6. Actual (BUG): Manual LSP edits are lost
#[test]
fn test_issue_806_external_config_edits_lost_on_settings_save() {
    // Create harness with temp project
    let mut harness = EditorTestHarness::with_temp_project(100, 40).unwrap();
    harness.render().unwrap();

    // Get the user config path from the temp directory
    // The harness creates a DirectoryContext::for_testing(temp_dir.path())
    // which puts config at temp_dir/config/config.json
    let temp_dir = harness
        .project_dir()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let config_dir = temp_dir.join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");

    // Step 1: Start with empty/minimal config
    fs::write(&user_config_path, r#"{"theme": "default"}"#).unwrap();

    // Editor is already running, so it has loaded its initial config
    // (which may be empty or have the theme we just wrote, depending on timing)

    // Step 2: User manually edits config.json WHILE Fresh is running
    // This simulates opening config.json in another editor and adding custom LSP
    fs::write(
        &user_config_path,
        r#"{
            "theme": "default",
            "lsp": {
                "rust-analyzer": {
                    "enabled": true,
                    "command": "rust-analyzer",
                    "args": ["--log-file", "/tmp/rust-analyzer-test.log"],
                    "languages": ["rust"]
                }
            }
        }"#,
    )
    .unwrap();

    // Step 3: User opens Settings UI
    harness.open_settings().unwrap();

    // Verify settings is open
    assert!(
        harness.editor().is_settings_open(),
        "Settings should be open after Ctrl+,"
    );

    // Step 4: User searches for and changes tab_size setting
    // Search for "tab"
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "tab_size");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // Confirm search
    harness.render().unwrap();

    // Navigate to tab_size value and change it
    // Press Right to increment the value (tab_size is typically a number spinner)
    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Step 5: Tab to footer and press Save
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Press Enter on Save button
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify settings is closed
    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after saving"
    );

    // Step 6: Verify the saved config file still has the custom LSP settings
    let saved_content = fs::read_to_string(&user_config_path).unwrap();
    let saved_json: serde_json::Value = serde_json::from_str(&saved_content).unwrap();

    eprintln!(
        "Issue #806 E2E test - Saved config after Settings UI save:\n{}",
        serde_json::to_string_pretty(&saved_json).unwrap()
    );

    // CRITICAL ASSERTION: The manually-added LSP section must still be present
    assert!(
        saved_json.get("lsp").is_some(),
        "BUG #806: 'lsp' section was lost! Manual config.json edits should be preserved. \
         Saved content: {}",
        saved_content
    );

    assert!(
        saved_json
            .get("lsp")
            .and_then(|l| l.get("rust-analyzer"))
            .is_some(),
        "BUG #806: 'lsp.rust-analyzer' was lost! Manual config edits should be preserved. \
         Saved content: {}",
        saved_content
    );

    // Verify custom args are preserved
    let saved_args = saved_json
        .get("lsp")
        .and_then(|l| l.get("rust-analyzer"))
        .and_then(|r| r.get("args"));
    assert!(
        saved_args.is_some(),
        "BUG #806: 'lsp.rust-analyzer.args' should be preserved. Saved content: {}",
        saved_content
    );
}

/// Issue #806 Variant: Custom language config lost on settings save
///
/// Similar to the main test but with custom language settings instead of LSP
#[test]
fn test_issue_806_custom_language_config_lost_on_settings_save() {
    let mut harness = EditorTestHarness::with_temp_project(100, 40).unwrap();
    harness.render().unwrap();

    let temp_dir = harness
        .project_dir()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let config_dir = temp_dir.join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");

    // Start with empty config, then "externally" add custom language
    fs::write(&user_config_path, r#"{}"#).unwrap();

    // Simulate external edit - add custom language
    fs::write(
        &user_config_path,
        r#"{
            "languages": {
                "mylangage": {
                    "extensions": [".myext"],
                    "comment_prefix": "//"
                }
            }
        }"#,
    )
    .unwrap();

    // Open Settings UI
    harness.open_settings().unwrap();

    assert!(
        harness.editor().is_settings_open(),
        "Settings should be open"
    );

    // Just save without changing anything to trigger the bug
    // Use Ctrl+S shortcut to save directly (more reliable than navigating to button)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Wait for settings to close (may need a few renders)
    for _ in 0..5 {
        if !harness.editor().is_settings_open() {
            break;
        }
        harness.render().unwrap();
    }

    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after saving"
    );

    // Verify custom language is preserved
    let saved_content = fs::read_to_string(&user_config_path).unwrap();
    let saved_json: serde_json::Value = serde_json::from_str(&saved_content).unwrap();

    eprintln!(
        "Issue #806 variant - Saved config:\n{}",
        serde_json::to_string_pretty(&saved_json).unwrap()
    );

    assert!(
        saved_json
            .get("languages")
            .and_then(|l| l.get("mylangage"))
            .is_some(),
        "BUG #806: Custom language 'mylangage' was lost! Manual config edits should be preserved. \
         Saved content: {}",
        saved_content
    );
}

/// Issue #806 Variant: External config edit then Settings UI save with actual change
///
/// This is the exact scenario reported:
/// 1. Fresh starts with initial config (lsp.rust.auto_start = true) LOADED INTO MEMORY
/// 2. User externally edits config.json to change auto_start to false
/// 3. User opens Settings UI in the same Fresh session
/// 4. User modifies a DIFFERENT setting (auto_indent)
/// 5. User saves from Settings UI
/// 6. BUG: The auto_start change is reverted because the in-memory config
///    still has auto_start=true, and that gets written to the delta
///
/// This test only uses keyboard events and verifies file output (no internal state).
#[test]
fn test_issue_806_external_edit_then_settings_change_and_save() {
    use crate::common::harness::HarnessOptions;
    use fresh::config_io::DirectoryContext;
    use tempfile::TempDir;

    // Step 1: Create isolated temp directory and config file BEFORE creating the editor
    let temp_dir = TempDir::new().unwrap();
    let config_dir = temp_dir.path().join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");

    // Write initial config with auto_start = true
    // This will be loaded into the editor's in-memory config
    let initial_config = r#"{
    "lsp": {
        "rust": {
            "auto_start": true
        }
    }
}"#;
    fs::write(&user_config_path, initial_config).unwrap();

    // Create DirectoryContext pointing to our temp dir (isolated .config)
    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // Create project_root inside temp_dir for the working directory
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir_all(&project_root).unwrap();

    // Create empty plugins dir to prevent embedded plugin loading
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();

    // Step 2: Create the editor - this loads the config file into memory
    let mut harness = EditorTestHarness::create(
        100,
        40,
        HarnessOptions::new()
            .with_working_dir(project_root)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.render().unwrap();

    // At this point, the editor has loaded auto_start=true into memory

    // Step 3: User externally edits config.json (while Fresh is running)
    // Changes auto_start from true to false
    let edited_config = r#"{
    "lsp": {
        "rust": {
            "auto_start": false
        }
    }
}"#;
    fs::write(&user_config_path, edited_config).unwrap();

    // Verify the external edit was written correctly
    let after_external_edit = fs::read_to_string(&user_config_path).unwrap();
    eprintln!("Config after external edit:\n{}", after_external_edit);
    assert!(
        after_external_edit.contains("\"auto_start\": false"),
        "External edit should have set auto_start to false"
    );

    // Step 4: Open Settings UI (Ctrl+,)
    harness.open_settings().unwrap();

    // Step 5: Change a DIFFERENT setting (search for auto_indent and toggle it)
    // Press / to open search in settings
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "auto_indent");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Toggle the auto_indent checkbox (space to toggle)
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Step 6: Save from Settings UI (Ctrl+S)
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();

    // Give the save operation time to complete
    for _ in 0..10 {
        harness.render().unwrap();
    }

    // Step 7: Verify the external auto_start=false edit is preserved in the file
    let final_content = fs::read_to_string(&user_config_path).unwrap();
    let final_json: serde_json::Value = serde_json::from_str(&final_content).unwrap();

    eprintln!(
        "Issue #806 - Final config after Settings save:\n{}",
        serde_json::to_string_pretty(&final_json).unwrap()
    );

    // CRITICAL: The external auto_start = false change must be preserved
    let auto_start = final_json
        .get("lsp")
        .and_then(|l| l.get("rust"))
        .and_then(|r| r.get("auto_start"))
        .and_then(|a| a.as_bool());

    assert_eq!(
        auto_start,
        Some(false),
        "BUG #806: lsp.rust.auto_start was reverted from false back to true! \
         The in-memory config had auto_start=true, and that overwrote the external edit. \
         External config edits should be preserved when saving from Settings UI. \
         Final content: {}",
        final_content
    );

    // Keep temp_dir alive until test completes
    drop(temp_dir);
}

/// Issue #806 - Tests that config is properly reloaded when Settings UI opens
///
/// This tests a potential fix approach: reloading config from disk when Settings opens
#[test]
fn test_settings_should_reflect_external_config_changes() {
    let mut harness = EditorTestHarness::with_temp_project(100, 40).unwrap();
    harness.render().unwrap();

    let temp_dir = harness
        .project_dir()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();
    let config_dir = temp_dir.join("config");
    fs::create_dir_all(&config_dir).unwrap();
    let user_config_path = config_dir.join("config.json");

    // Start with default theme
    fs::write(&user_config_path, r#"{"theme": "default"}"#).unwrap();

    // Externally change theme to "dracula"
    fs::write(&user_config_path, r#"{"theme": "dracula"}"#).unwrap();

    // Open Settings UI
    harness.open_settings().unwrap();

    assert!(
        harness.editor().is_settings_open(),
        "Settings should be open"
    );

    // Search for theme
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    send_text(&mut harness, "theme");
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // The Settings UI should ideally show "dracula" as the current value
    // (This tests whether config is reloaded when Settings opens)
    // Note: This might not be implemented yet, but documents expected behavior

    // For now, just close without saving
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(
        !harness.editor().is_settings_open(),
        "Settings should be closed after Esc"
    );
}
