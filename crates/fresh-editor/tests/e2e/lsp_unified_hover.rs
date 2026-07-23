//! E2E tests for unified LSP hover across multiple servers.
//!
//! Hover fans out to every capable server and merges the non-null results
//! into a single popup. Previously hover queried only the first capable
//! server, so a server returning `null` (e.g. vtsls on a Tailwind class)
//! suppressed a second server's hover entirely — sinelaw/fresh#2635.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{EditorTestHarness, HarnessOptions};

/// Build an LSP server config pointing at `command` with an optional name.
fn server_config(command: std::path::PathBuf, name: &str) -> fresh::services::lsp::LspServerConfig {
    fresh::services::lsp::LspServerConfig {
        command: command.to_string_lossy().to_string(),
        args: Some(vec![]),
        enabled: true,
        auto_start: true,
        process_limits: fresh::services::process_limits::ProcessLimits::default(),
        initialization_options: None,
        env: Default::default(),
        language_id_overrides: Default::default(),
        root_markers: Default::default(),
        name: Some(name.to_string()),
        only_features: None,
        except_features: None,
    }
}

/// Create a harness wired to two hover-capable fake servers for `rust`, open a
/// test file, and wait until both servers have finished initialization.
fn harness_with_two_servers(
    temp_dir: &std::path::Path,
    server_a: std::path::PathBuf,
    server_b: std::path::PathBuf,
) -> anyhow::Result<EditorTestHarness> {
    let test_file = temp_dir.join("test.rs");
    std::fs::write(&test_file, "fn example_function() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            server_config(server_a, "server-a"),
            server_config(server_b, "server-b"),
        ]),
    );

    let mut harness = EditorTestHarness::create(
        120,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Both servers must finish their handshake before we fire the hover; a
    // request sent while a server is still `Initializing` is dropped, so with
    // two servers we'd otherwise race and lose the merge.
    harness.wait_until(|h| {
        h.editor()
            .active_window()
            .initialized_lsp_server_count("rust")
            >= 2
    })?;

    Ok(harness)
}

/// #2635 repro: the first server returns `null`, the second returns content.
/// The hover popup must show the second server's content rather than being
/// suppressed by the first server's null.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_null_first_server_does_not_suppress_second() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _server_a = FakeLspServer::spawn_hover_null(temp_dir.path())?;
    let _server_b = FakeLspServer::spawn_hover_alpha(temp_dir.path())?;

    let mut harness = harness_with_two_servers(
        temp_dir.path(),
        FakeLspServer::hover_null_script_path(temp_dir.path()),
        FakeLspServer::hover_alpha_script_path(temp_dir.path()),
    )?;

    harness.editor_mut().request_hover()?;

    // The second server's hover content must appear even though the first
    // server returned null.
    harness.wait_for_screen_contains("HoverAlpha content")?;
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "hover popup should be visible with the second server's content"
    );

    Ok(())
}

/// Both servers return non-null hovers: the popup must contain BOTH bodies,
/// merged into a single card.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_merges_bodies_from_both_servers() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _server_a = FakeLspServer::spawn_hover_alpha(temp_dir.path())?;
    let _server_b = FakeLspServer::spawn_hover_beta(temp_dir.path())?;

    let mut harness = harness_with_two_servers(
        temp_dir.path(),
        FakeLspServer::hover_alpha_script_path(temp_dir.path()),
        FakeLspServer::hover_beta_script_path(temp_dir.path()),
    )?;

    harness.editor_mut().request_hover()?;

    // Wait for both bodies to be merged into the popup.
    harness.wait_for_screen_contains("HoverAlpha content")?;
    harness.wait_for_screen_contains("HoverBeta content")?;

    assert!(
        harness.editor().active_state().popups.is_visible(),
        "hover popup should be visible"
    );
    harness.assert_screen_contains("HoverAlpha content");
    harness.assert_screen_contains("HoverBeta content");

    Ok(())
}

/// All servers return `null`: exactly one "no hover" status message, and no
/// hover popup.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_all_null_reports_no_hover_and_shows_no_popup() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    // Writing the null-hover script once is enough: both config entries point
    // at it, so the editor launches two independent null-hover processes.
    let _server = FakeLspServer::spawn_hover_null(temp_dir.path())?;
    let null_script = FakeLspServer::hover_null_script_path(temp_dir.path());

    let mut harness =
        harness_with_two_servers(temp_dir.path(), null_script.clone(), null_script.clone())?;

    harness.editor_mut().request_hover()?;

    // The "no hover" status fires only once every server has answered.
    let no_hover = "No hover information available";
    harness.wait_until(|h| {
        h.editor()
            .get_status_message()
            .map(|m| m.contains(no_hover))
            .unwrap_or(false)
    })?;

    assert!(
        !harness.editor().active_state().popups.is_visible(),
        "no hover popup should be shown when all servers return null"
    );

    Ok(())
}
