//! Regression test for sinelaw/fresh#1679.
//!
//! R languageserver 0.3.17 advertises `semanticTokensProvider` and
//! `colorProvider` in its initialize response but never replies to the
//! corresponding requests. Before the transport fix, Fresh's first
//! `textDocument/semanticTokens/range` request would land at the front of a
//! per-server queue and every subsequent command (didChange, completion,
//! signatureHelp, …) would wait behind the response that never arrived —
//! the completion popup just stayed at `⋯` forever.
//!
//! The fake LSP variant used here mirrors that exact shape: replies to
//! `textDocument/completion` and `textDocument/signatureHelp` normally,
//! silently drops `textDocument/semanticTokens/range`,
//! `textDocument/semanticTokens/full` and `textDocument/documentColor`. A
//! correctly-implemented transport must let completion requests through
//! within a few seconds even while the semantic-tokens reply is missing.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses Bash")]
fn test_completion_works_when_server_silently_drops_semantic_tokens() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;

    let _server = FakeLspServer::spawn_drops_semantic_tokens(temp_dir.path())?;
    let script_path = FakeLspServer::drops_semantic_tokens_script_path(temp_dir.path());

    // Use a file extension associated with a language for which Fresh has
    // no built-in LSP — "rust" works for the test because we override its
    // configured server with our fake one.
    let test_file = temp_dir.path().join("script.rs");
    std::fs::write(&test_file, "fn main() {\n    \n    \n    \n    \n    \n}\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = false;
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("fake-r".to_string()),
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

    // Wait until the LSP has initialized so completion requests will reach
    // the fake server. Without the transport fix, the editor still reaches
    // this point — initialize itself is answered — but every later request
    // wedges behind the unanswered semantic-tokens request.
    harness.wait_until(|h| h.editor().initialized_lsp_server_count("rust") >= 1)?;

    // Move into the function body and type a few characters. Each keypress
    // generates a `textDocument/didChange` notification. With the bug, those
    // notifications are stuck behind the unanswered semantic-tokens request
    // on the same server's command queue and never reach the server.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    harness.type_text("med")?;
    harness.render()?;

    // Trigger completion explicitly.
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;
    harness.render()?;

    // The completion popup must show the fake item ("median") within a
    // bounded time well below the per-request timeout. Without the fix this
    // never resolves and `wait_until` times out.
    harness.wait_until(|h| h.editor().completion_items_count() > 0)?;

    let n = harness.editor().completion_items_count();
    assert!(
        n >= 1,
        "expected completion popup to render at least one item from the fake LSP, got {n}",
    );

    Ok(())
}
