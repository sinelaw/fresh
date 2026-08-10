//! Regression test for sinelaw/fresh#2744: inlay hints must refresh after a
//! buffer edit.
//!
//! Root cause: the edit path (`send_lsp_changes_for_buffer`) writes the
//! `scheduled_inlay_hints_request` debounce slot, but nothing ever consumed
//! it (diagnostics work because `check_diagnostic_pull_timer` consumes their
//! slot). `check_inlay_hints_timer`, called from the idle tick, is the
//! missing consumer.
//!
//! These tests drive a fake LSP that advertises `inlayHintProvider` and
//! assert on the debounce slot end-to-end:
//!   * with hints enabled, an edit arms the slot and the idle tick (run by
//!     `wait_until`) consumes it — proving a fresh request is dispatched;
//!   * with hints disabled, edits never arm the slot.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;

fn inlay_lsp_config(temp: &std::path::Path, enable_inlay_hints: bool) -> fresh::config::Config {
    let mut config = fresh::config::Config::default();
    config.editor.enable_inlay_hints = enable_inlay_hints;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::inlay_hints_script_path(temp)
                .to_string_lossy()
                .to_string(),
            args: None,
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("fake-rust-inlay".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );
    config
}

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses Bash")]
fn test_inlay_hints_refresh_after_edit_when_enabled() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_with_inlay_hints(temp_dir.path())?;

    let test_file = temp_dir.path().join("main.rs");
    std::fs::write(&test_file, "let x = 1;\nfoo();\n")?;

    let config = inlay_lsp_config(temp_dir.path(), true);
    let mut harness = EditorTestHarness::create(
        80,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the initial inlay-hint round trip to land: virtual texts only
    // appear once the server has been spawned, the buffer opened (didOpen),
    // and hints applied. This guarantees a subsequent edit takes the
    // didChange path (not the first-time didOpen path).
    harness.wait_until(|h| !h.editor().active_state().virtual_texts.is_empty())?;

    // A real edit must arm the debounce slot (the producer gates on
    // enable_inlay_hints and only sets it once a didChange was sent).
    harness.type_text("z")?;
    assert!(
        harness
            .editor()
            .active_window()
            .scheduled_inlay_hints_request
            .is_some(),
        "editing an enabled buffer must schedule an inlay-hints refresh"
    );

    // The idle tick (run each iteration by wait_until) must consume the slot
    // once the debounce deadline passes — this is the missing consumer under
    // test. Without it the slot would stay Some forever and hints never refresh.
    harness.wait_until(|h| {
        h.editor()
            .active_window()
            .scheduled_inlay_hints_request
            .is_none()
    })?;

    Ok(())
}

#[test]
#[cfg_attr(target_os = "windows", ignore = "FakeLspServer uses Bash")]
fn test_inlay_hints_slot_never_armed_when_disabled() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_with_inlay_hints(temp_dir.path())?;

    let test_file = temp_dir.path().join("main.rs");
    std::fs::write(&test_file, "let x = 1;\nfoo();\n")?;

    let config = inlay_lsp_config(temp_dir.path(), false);
    let mut harness = EditorTestHarness::create(
        80,
        24,
        crate::common::harness::HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the server to spin up so edits actually produce didChange
    // notifications (any_sent == true) — otherwise the "disabled" assertion
    // would pass vacuously for the wrong reason.
    harness.wait_until(|h| {
        h.editor()
            .active_window()
            .running_lsp_servers()
            .contains(&"rust".to_string())
    })?;
    harness.wait_for_async_quiescence(4)?;

    // Several real edits, each flushing a didChange. With hints disabled the
    // producer must never arm the debounce slot.
    for ch in ["a", "b", "c"] {
        harness.type_text(ch)?;
        assert!(
            harness
                .editor()
                .active_window()
                .scheduled_inlay_hints_request
                .is_none(),
            "editing with inlay hints disabled must not schedule a refresh"
        );
        harness.wait_for_async_quiescence(2)?;
    }

    Ok(())
}
