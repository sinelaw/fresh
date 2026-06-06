//! Regression test for sinelaw/fresh#2212.
//!
//! When the user invokes "Code Actions" at a cursor position covered by an
//! LSP diagnostic, the `textDocument/codeAction` request must carry the
//! matching diagnostic in `context.diagnostics`. Many servers (clangd,
//! eslint, ...) gate quickfix actions on this — sending an empty context
//! produces zero actions and the user sees a misleading "No code actions
//! available".
//!
//! This test wires up a fake LSP server that:
//!   * publishes a diagnostic at line 0, cols 4..5 on `didOpen`, and
//!   * returns a quickfix only when `context.diagnostics` is non-empty.
//!
//! Without the fix the popup never materializes (the server returns `[]`
//! because Fresh sent `"diagnostics": []`). With the fix the popup shows
//! "Remove unused variable".

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};

#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_code_action_request_includes_overlapping_diagnostic() -> anyhow::Result<()> {
    init_tracing_from_env();
    fresh::services::signal_handler::install_signal_handlers();

    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_with_diagnostic_gated_code_actions(temp_dir.path())?;

    let test_file = temp_dir.path().join("test.rs");
    // "let x = 5;" — the fake LSP flags the `x` at col 4..5.
    std::fs::write(&test_file, "let x = 5;\nfn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::diagnostic_gated_code_actions_script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the LSP handshake to complete.
    harness.wait_until(|h| h.editor().active_window().is_lsp_server_ready("rust"))?;

    // Wait for the diagnostic round-trip — the fake server publishes
    // diagnostics on `didOpen`, but it arrives asynchronously.
    harness.wait_until(|h| {
        h.editor()
            .get_stored_diagnostics()
            .values()
            .any(|v| !v.is_empty())
    })?;

    // Cursor starts at (0, 0). Move it to (0, 4) — on the diagnostic.
    for _ in 0..4 {
        harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
    }
    harness.render()?;

    // Trigger code actions via the command palette (Alt+. is unreliable on
    // macOS CI terminals — see sibling tests).
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.type_text("Code Actions")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // With the fix, the diagnostic-gated quickfix appears.
    harness.wait_for_screen_contains("Remove unused variable")?;

    Ok(())
}
