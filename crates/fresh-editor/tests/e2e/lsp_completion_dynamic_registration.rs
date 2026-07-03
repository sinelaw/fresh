//! Regression test for sinelaw/fresh#2340.
//!
//! Eclipse jdtls (and every other Eclipse LSP4J-based server) advertises almost
//! nothing in its static `initialize` result and instead registers providers
//! like `textDocument/completion` *dynamically* via a `client/registerCapability`
//! request — a request whose JSON-RPC id LSP4J always sends as a **string**.
//!
//! Fresh modelled JSON-RPC ids as `i64`, so that string-id request failed to
//! deserialize as a `Request` and — because `JsonRpcMessage` is
//! `#[serde(untagged)]` — silently fell through to `Notification`, which ignores
//! the stray `id`. The dynamic-capability handler therefore never ran, completion
//! stayed gated off, and the request was never even acknowledged. The observable
//! symptom: Java completions never appear even though diagnostics (a plain
//! server notification) work fine.
//!
//! The fake server here mirrors that exact shape: no static `completionProvider`,
//! then a string-id `client/registerCapability` for completion on `initialized`.
//! Before the fix `completion_capable_lsp_server_count` stays 0 forever and the
//! `wait_until` below times out; after the fix the capability turns on and the
//! completion popup renders the fake item.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses a Bash script")]
fn test_completion_works_when_registered_dynamically_with_string_id() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;

    let _server = FakeLspServer::spawn_with_dynamic_completion(temp_dir.path())?;
    let script_path = FakeLspServer::dynamic_completion_script_path(temp_dir.path());

    // "rust" is just a convenient language with a built-in entry we override
    // with our fake server; the bug is about JSON-RPC transport, not Java.
    let test_file = temp_dir.path().join("script.rs");
    std::fs::write(&test_file, "fn main() {\n    \n}\n")?;

    let mut config = fresh::config::Config::default();
    // Trigger completion explicitly so the test doesn't depend on debounced
    // quick-suggestion timing.
    config.editor.quick_suggestions = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![]),
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("fake-jdtls".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // The server advertises no completion statically; it only becomes
    // completion-capable once it has registered the provider dynamically with
    // its string-id `client/registerCapability`. Waiting on this both proves
    // the registration was parsed (the regression) and removes any race
    // between registration and the one-shot completion trigger below.
    harness.wait_until(|h| {
        h.editor()
            .active_window()
            .completion_capable_lsp_server_count("rust")
            >= 1
    })?;

    // Move into the function body and ask for completion.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // The completion popup must render the item served by the fake LSP.
    harness.wait_until(|h| h.editor().active_window().completion_items_count() > 0)?;
    assert!(
        harness.screen_to_string().contains("dynamicCompletion"),
        "expected the dynamically-registered completion item on screen, screen was:\n{}",
        harness.screen_to_string()
    );

    Ok(())
}
