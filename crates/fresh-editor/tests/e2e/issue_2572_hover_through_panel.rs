//! Reproduction for issue #2572: "mouse will hover through 'search and
//! replace in project'".
//!
//! With the Search/Replace panel open in the bottom Utility Dock, moving the
//! mouse over the *panel* rows used to trigger an LSP **hover** request. The
//! request was aimed at the editor's *active* buffer (the code file that was
//! open before the panel), using a byte position computed from the *panel's*
//! geometry — so a hover card for the code appeared "through" the panel.
//!
//! The fix: hover only fires for the buffer the mouse is actually over. Over
//! the Search/Replace panel (a virtual UI buffer with no language server),
//! nothing should pop up.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::time::Duration;

fn setup_project() -> (tempfile::TempDir, std::path::PathBuf) {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "search_replace");

    // A short Rust file with a symbol to hover.
    fs::write(project_root.join("main.rs"), "fn foo() {}\n").unwrap();

    let status = std::process::Command::new("git")
        .args(["init", "-q"])
        .current_dir(&project_root)
        .status()
        .unwrap();
    assert!(status.success());
    let status = std::process::Command::new("git")
        .args(["add", "main.rs"])
        .current_dir(&project_root)
        .status()
        .unwrap();
    assert!(status.success());

    (temp_dir, project_root)
}

fn open_search_replace_panel(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;
    harness.type_text("Search and Replace")?;
    harness.wait_until(|h| h.screen_to_string().contains("Search and Replace"))?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    // The "Search:" control line only renders once the dock panel is up.
    harness.wait_until(|h| h.screen_to_string().contains("Search:"))?;
    Ok(())
}

fn row_of(harness: &EditorTestHarness, needle: &str) -> u16 {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|l| l.contains(needle))
        .unwrap_or_else(|| panic!("expected screen to contain '{needle}'\nScreen:\n{screen}"))
        as u16
}

#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_hover_does_not_leak_through_search_replace_panel() -> anyhow::Result<()> {
    let (_temp_dir, project_root) = setup_project();

    let _fake_server = FakeLspServer::spawn(&project_root)?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::script_path(&project_root)
                .to_string_lossy()
                .to_string(),
            args: Some(vec![]),
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

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())?;
    harness.open_file(&project_root.join("main.rs"))?;
    harness.render()?;

    // Open the Search/Replace panel in the bottom dock (it takes focus), then
    // click back into the code split so the *code* buffer is active again while
    // the panel stays open below — the real-world state after opening the panel
    // and returning to the file (e.g. clicking a search result).
    open_search_replace_panel(&mut harness)?;
    harness.mouse_click(10, 2)?;
    harness.render()?;

    // Precondition, established from rendered output only: hovering the code
    // symbol "foo" pops the fake server's hover card. This proves in one step
    // that (a) the LSP round-trip works and (b) the code buffer is active — a
    // hover only fires for the buffer under the pointer, so the card appearing
    // here means the pointer's split *is* the live code buffer. Column 10 lands
    // on "foo" once the line-number gutter is accounted for (matching the
    // working hover fixture in `e2e/lsp.rs`).
    harness.mouse_move(10, 2)?;
    harness.render()?;
    harness.editor_mut().force_check_mouse_hover();
    harness.wait_until(|h| h.screen_to_string().contains("Test hover content"))?;

    // Move the pointer off the text to dismiss the card before the real check.
    harness.mouse_move(0, 0)?;
    harness.render()?;
    harness.editor_mut().force_check_mouse_hover();
    harness.wait_until(|h| !h.screen_to_string().contains("Test hover content"))?;

    // Now move the mouse over a panel row (the "Search:" control line). On the
    // unfixed editor this fired a hover request for the still-active code buffer
    // — using a byte offset taken from the panel's geometry — and the card
    // leaked in over the panel. With the fix, the pointer is over the panel's
    // virtual buffer (no language server) so nothing is requested.
    let panel_row = row_of(&harness, "Search:");
    harness.mouse_move(6, panel_row)?;
    harness.render()?;

    // Negative check: there is no event to wait for, so pump the hover
    // machinery a bounded number of times — enough that the erroneous request
    // would have fired and rendered — then assert the card never appeared.
    for _ in 0..40 {
        harness.editor_mut().force_check_mouse_hover();
        harness.process_async_and_render()?;
        harness.sleep(Duration::from_millis(20));
    }

    assert!(
        !harness.screen_to_string().contains("Test hover content"),
        "LSP hover leaked through the Search/Replace panel.\nScreen:\n{}",
        harness.screen_to_string()
    );

    Ok(())
}
