//! Regression: non-standard server->client LSP notifications
//! (`textDocument/clangd.fileStatus`, `$/memoryUsage`, `rust-analyzer/*`, ...)
//! must reach plugins subscribed with `editor.on("lsp/custom_notification", ...)`.
//!
//! The notification dispatcher in `services/lsp/async_handler.rs` used to end
//! in `_ => tracing::debug!("Unhandled notification: ...")`, dropping every
//! method it did not know, and `Editor::handle_custom_notification` was dead
//! code that only fed the ControlEvent broadcaster, never plugin handlers.

#![cfg(feature = "plugins")]

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crate::common::tracing::init_tracing_from_env;
use fresh::config::Config;
use fresh::services::lsp::LspServerConfig;
use fresh::services::process_limits::ProcessLimits;
use fresh::types::LspLanguageConfig;
use std::fs;

/// Minimal plugin: surface every custom LSP notification it receives in the
/// status bar, so the test can observe delivery on screen.
const PLUGIN_SOURCE: &str = r#"
/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

function onCustomNotification(payload: {
    language: string;
    method: string;
    params: Record<string, unknown> | null;
}): void {
    if (!payload) {
        return;
    }
    const status = payload.params && (payload.params as any).status;
    editor.setStatus(`CUSTOM_NOTIF ${payload.method} ${String(status)}`);
}
registerHandler("onCustomNotification", onCustomNotification);

editor.on("lsp/custom_notification", "onCustomNotification");
"#;

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses Bash")]
fn test_custom_lsp_notification_reaches_plugin_hook() -> anyhow::Result<()> {
    init_tracing_from_env();

    let temp_dir = tempfile::TempDir::new()?;
    // The default fake server sends `textDocument/clangd.fileStatus`
    // (`{"uri": ..., "status": "ready"}`) right after `textDocument/didOpen`.
    let _fake_server = FakeLspServer::spawn(temp_dir.path())?;

    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root)?;
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir)?;
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("custom_notif_probe.ts"), PLUGIN_SOURCE)?;

    let source_file = project_root.join("main.rs");
    fs::write(&source_file, "fn main() {}\n")?;

    let mut config = Config::default();
    config.lsp.insert(
        "rust".to_string(),
        LspLanguageConfig::Multi(vec![LspServerConfig {
            command: FakeLspServer::script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: Some(vec![]),
            enabled: true,
            auto_start: true,
            process_limits: ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())?;

    harness.open_file(&source_file)?;
    harness.render()?;

    // Semantic wait (no in-test timeout; nextest times out externally).
    // Read the plugin status slot rather than the screen: the status bar
    // truncates long messages. `ready` only appears if `params` reached the
    // plugin as a JSON object (a stringified payload would yield `undefined`).
    harness.wait_until(|h| {
        h.editor().get_status_message().map(|m| m.as_str())
            == Some("CUSTOM_NOTIF textDocument/clangd.fileStatus ready")
    })?;

    Ok(())
}
