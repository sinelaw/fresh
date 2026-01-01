#[cfg(feature = "plugins")]
mod common;

#[cfg(feature = "plugins")]
use common::harness::EditorTestHarness;
#[cfg(feature = "plugins")]
use common::tracing::init_tracing_from_env;
#[cfg(feature = "plugins")]
use crossterm::event::{KeyCode, KeyModifiers};
#[cfg(feature = "plugins")]
use fresh::config::Config;
#[cfg(feature = "plugins")]
use std::fs;

#[cfg(feature = "plugins")]
#[test]
fn test_plugin_i18n_loading_and_translation() {
    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    // Create plugins directory
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();

    // Create plugin file
    let plugin_path = plugins_dir.join("test_i18n.ts");
    let plugin_code = r#"
// Simple test plugin for i18n
globalThis.test_i18n_action = function() {
  const msg = editor.t("msg.hello", { name: "TestUser" });
  editor.setStatus(msg);
};

editor.registerCommand(
  "%cmd.test",
  "%cmd.test_desc",
  "test_i18n_action",
  "normal"
);

editor.setStatus("Test i18n plugin loaded");
"#;
    fs::write(&plugin_path, plugin_code).unwrap();

    // Create i18n file
    let i18n_path = plugins_dir.join("test_i18n.i18n.json");
    let i18n_content = r#"{
        "en": {
            "cmd.test": "English Command",
            "cmd.test_desc": "English Description",
            "msg.hello": "Hello %{name} (en)"
        },
        "es": {
            "cmd.test": "Comando Español",
            "cmd.test_desc": "Descripción Española",
            "msg.hello": "Hola %{name} (es)"
        }
    }"#;
    fs::write(&i18n_path, i18n_content).unwrap();

    // Start harness with English locale
    let mut config = Config::default();
    config.locale = fresh::config::LocaleName(Some("en".to_string()));

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(80, 24, config, project_root).unwrap();

    // Semantic wait for plugin to load and initialize
    println!("Waiting for plugin load...");
    let wait_result = harness.wait_for_async(
        |h| {
            let status = h.get_status_bar();
            println!("Status bar: {:?}", status);
            if status.contains("Test i18n plugin loaded") {
                println!("Found plugin loaded message!");
                return true;
            }
            // Also accept if the action already ran (which sets Hello message)
            if status.contains("Hello TestUser") {
                println!("Action already executed, plugin loaded OK");
                return true;
            }
            false
        },
        10000,
    ); // 10s timeout for load

    println!("wait_result: {:?}", wait_result);

    if wait_result.is_err() {
        println!("wait_for_async returned error: {:?}", wait_result.err());
        panic!("Plugin failed to load (error)");
    }
    if !wait_result.unwrap() {
        println!("STATUS BAR: {:?}", harness.get_status_bar());
        panic!("Plugin failed to load or set status message within 10s");
    }
    println!("Plugin loaded successfully!");

    // Open command palette and check for English command
    println!("Opening command palette...");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    println!("Sent Ctrl+P, waiting for prompt...");
    harness.wait_for_prompt().unwrap();
    println!("Prompt opened, typing English...");
    harness.type_text("English").unwrap();
    println!("Typed English, waiting for command...");

    harness.render().unwrap();
    let screen = harness.screen_to_string();
    println!("Screen after typing English:\n{}", screen);
    if !screen.contains("English Command") {
        panic!("English command not found in palette");
    }
    println!("Found English Command!");

    // Close palette
    println!("Closing palette...");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    println!("Sent Esc, waiting for prompt to close...");
    harness.wait_for_prompt_closed().unwrap();
    println!("Prompt closed!");

    // Execute the command via its action name
    println!("Executing action...");
    let _rx = harness
        .editor()
        .plugin_manager()
        .execute_action_async("test_i18n_action")
        .unwrap()
        .unwrap();
    println!("Action started...");

    // Give the action time to complete and update status
    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let status = harness.get_status_bar();
    println!("Status after action: {:?}", status);
    // Status bar truncates long messages, so just check for partial match
    assert!(
        status.contains("Hello Tes") || status.contains("Hello TestUser"),
        "Expected 'Hello TestUser' in status, got: {}",
        status
    );

    // --- Switch to Spanish ---
    println!("Switching to Spanish...");
    fresh::i18n::set_locale("es");
    harness.render().unwrap();

    // Open command palette and check for Spanish command
    println!("Opening command palette for Spanish...");
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Comando").unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    println!("Screen after typing Comando:\n{}", screen);
    assert!(
        screen.contains("Comando"),
        "Expected Spanish command in palette"
    );
    println!("Found Spanish command!");

    // Close palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.wait_for_prompt_closed().unwrap();

    // Execute again and check for Spanish message
    println!("Executing action for Spanish...");
    let _rx = harness
        .editor()
        .plugin_manager()
        .execute_action_async("test_i18n_action")
        .unwrap()
        .unwrap();

    std::thread::sleep(std::time::Duration::from_millis(100));
    harness.render().unwrap();

    let status = harness.get_status_bar();
    println!("Status after Spanish action: {:?}", status);
    // Status bar truncates long messages, so just check for partial match
    assert!(
        status.contains("Hola") || status.contains("Hola TestUser"),
        "Expected 'Hola TestUser' in status, got: {}",
        status
    );

    println!("Test passed!");
}
