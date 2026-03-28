/// Regression test: when two LSP servers are configured for the same language,
/// the second server's capabilities overwrite the first's in the per-language
/// capability map. This causes semantic token requests to be routed to the wrong
/// server (handle_for_feature_mut returns the first server in the vec, regardless
/// of which server actually advertised the capability).
///
/// Reproduces: "LSP response error: Method Not Found: textDocument/semanticTokens/range (code -32601)"
use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses Bash")]
fn test_multi_lsp_semantic_tokens_capability_mismatch() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;

    // Server A: does NOT support semantic tokens at all
    let _server_a = FakeLspServer::spawn_no_semantic_tokens(temp_dir.path())?;
    // Server B: supports semantic tokens (full + range)
    let _server_b = FakeLspServer::spawn(temp_dir.path())?;

    let test_file = temp_dir.path().join("test_multi.rs");
    std::fs::write(&test_file, "fn main() { let value = 1; }\n")?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_semantic_tokens_full = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            // Server A first: no semantic tokens — but handle_for_feature_mut picks it
            fresh::services::lsp::LspServerConfig {
                command: FakeLspServer::no_semantic_tokens_script_path(temp_dir.path())
                    .to_string_lossy()
                    .to_string(),
                args: vec![],
                enabled: true,
                auto_start: true,
                name: Some("no-semtok".to_string()),
                process_limits: fresh::services::process_limits::ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                root_markers: Default::default(),
                only_features: None,
                except_features: None,
            },
            // Server B second: has full semantic tokens support
            fresh::services::lsp::LspServerConfig {
                command: FakeLspServer::script_path(temp_dir.path())
                    .to_string_lossy()
                    .to_string(),
                args: vec![],
                enabled: true,
                auto_start: true,
                name: Some("with-semtok".to_string()),
                process_limits: fresh::services::process_limits::ProcessLimits::default(),
                initialization_options: None,
                env: Default::default(),
                language_id_overrides: Default::default(),
                root_markers: Default::default(),
                only_features: None,
                except_features: None,
            },
        ]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for LSP to be running
    harness.wait_until(|h| {
        h.editor()
            .running_lsp_servers()
            .contains(&"rust".to_string())
    })?;

    // Semantic tokens should eventually appear. With the bug, the request is either
    // routed to the wrong server (Method Not Found) or never made (capabilities
    // overwritten to false), so this wait_until never completes and nextest times out.
    let ns = fresh::services::lsp::semantic_tokens::lsp_semantic_tokens_namespace();
    harness.wait_until(|h| {
        let state = h.editor().active_state();
        state
            .overlays
            .all()
            .iter()
            .any(|o| o.namespace.as_ref() == Some(&ns))
    })?;

    Ok(())
}
