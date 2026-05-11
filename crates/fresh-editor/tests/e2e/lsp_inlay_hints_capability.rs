//! Regression test: when an LSP server does not advertise
//! `inlayHintProvider` in its `initialize` response, the editor must
//! never send `textDocument/inlayHint` to that server.
//!
//! Reproduces the original report against vscode-json-language-server
//! ("Unhandled method textDocument/inlayHint (code -32601)"): the
//! server replies with MethodNotFound because the editor was sending
//! inlay-hint requests without checking advertised capabilities.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses Bash")]
fn test_inlay_hint_skipped_when_server_does_not_advertise_capability() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_with_logging(temp_dir.path())?;

    // Per-test log file so parallel runs don't race on the default path.
    let log_file = temp_dir.path().join("inlay_hint_capability_log.txt");
    let test_file = temp_dir.path().join("test.json");
    std::fs::write(&test_file, "{\n  \"name\": \"value\"\n}\n")?;

    // `spawn_with_logging` advertises completion/definition/hover but NOT
    // `inlayHintProvider` — same shape as vscode-json-language-server for
    // the offending request.
    let mut config = fresh::config::Config::default();
    config.editor.enable_inlay_hints = true;
    config.lsp.insert(
        "json".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("fake-json-ls".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        80,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Phase 1: wait until the server has reported its capabilities.
    // `initialized_lsp_server_count` counts handles whose capabilities
    // arrived via `LspInitialized` — the same handler that dispatches
    // the inlay-hint / pull-diagnostic follow-ups.
    harness.wait_until(|h| {
        h.editor()
            .active_window()
            .initialized_lsp_server_count("json")
            > 0
    })?;

    // Phase 2: trigger a hover and wait for the server to receive it.
    // LSP commands for this server share one outgoing channel, so a hover
    // request landing in the server's log proves every command queued
    // before it (including any inlay-hint request from the init handler
    // or from `file_operations::notify_lsp_open`) has already flushed.
    harness.send_key(KeyCode::Char('k'), KeyModifiers::ALT)?;
    harness.render()?;
    harness.wait_until(|_| {
        let content = std::fs::read_to_string(&log_file).unwrap_or_default();
        content.lines().any(|line| line == "textDocument/hover")
    })?;

    // With the capability fix, the editor filters inlay-hint requests out
    // before sending because the fake server did not advertise
    // `inlayHintProvider`. Without it, `textDocument/inlayHint` would
    // appear in the log and the real server would respond with -32601.
    let content = std::fs::read_to_string(&log_file).unwrap_or_default();
    assert!(
        !content.lines().any(|line| line == "textDocument/inlayHint"),
        "server received textDocument/inlayHint despite not advertising \
         inlayHintProvider; full method log:\n{}",
        content
    );

    Ok(())
}
