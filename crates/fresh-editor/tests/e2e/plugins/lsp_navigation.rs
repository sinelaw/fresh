//! E2E tests for lsp_navigation plugin

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use ratatui::style::Color;
use std::fs;

const FAKE_LSP_SCRIPT: &str = r#"#!/bin/bash
read_message() {
    local content_length=0
    while IFS=: read -r key value; do
        key=$(echo "$key" | tr -d '\r\n')
        value=$(echo "$value" | tr -d '\r\n ')
        if [ "$key" = "Content-Length" ]; then
            content_length=$value
        fi
        if [ -z "$key" ]; then
            break
        fi
    done
    if [ $content_length -gt 0 ]; then
        dd bs=1 count=$content_length 2>/dev/null
    fi
}
send_message() {
    local message="$1"
    local length=${#message}
    echo -en "Content-Length: $length\r\n\r\n$message"
}
while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"documentSymbolProvider":true,"textDocumentSync":1}}}'
            ;;
        "initialized") ;;
        "textDocument/didOpen"|"textDocument/didChange"|"textDocument/didSave") ;;
        "textDocument/documentSymbol")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"name":"MyClass","kind":5,"location":{"uri":"file://test.ts","range":{"start":{"line":0,"character":0},"end":{"line":8,"character":1}}}},{"name":"constructor","kind":9,"location":{"uri":"file://test.ts","range":{"start":{"line":1,"character":2},"end":{"line":3,"character":3}}}},{"name":"myMethod","kind":6,"location":{"uri":"file://test.ts","range":{"start":{"line":5,"character":2},"end":{"line":7,"character":3}}}}]}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"#;

const TEST_FILE_CONTENT: &str = r#"class MyClass {
  constructor() {
    return true;
  }

  myMethod(a: number): number {
    return a;
  }
}
"#;

fn setup_lsp_test() -> anyhow::Result<(EditorTestHarness, tempfile::TempDir)> {
    let temp_dir = tempfile::TempDir::new()?;
    let project_root = temp_dir.path().to_path_buf();

    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir)?;
    copy_plugin(&plugins_dir, "lsp_navigation");
    copy_plugin_lib(&plugins_dir);

    let script_path = project_root.join("fake_lsp.sh");
    fs::write(&script_path, FAKE_LSP_SCRIPT)?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&script_path)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&script_path, perms)?;
    }

    let test_file = project_root.join("test.ts");
    fs::write(&test_file, TEST_FILE_CONTENT)?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "typescript".to_string(),
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
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(100, 30, config, project_root)?;

    harness.open_file(&test_file)?;
    harness.process_async_and_render()?;
    harness.wait_until(|h| h.screen_to_string().contains("LSP (on)"))?;

    Ok((harness, temp_dir))
}

fn open_symbol_navigation(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.process_async_and_render()?;
    harness.type_text("Go to LSP Symbol")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.wait_for_prompt()?;
    harness.render()?;

    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("[class] MyClass")
            || screen.contains("[construct] constructor")
            || screen.contains("[method] myMethod")
    })?;

    Ok(())
}

/// Test LSP navigation functionality with a fake LSP server
///
/// This test verifies that the lsp_navigation plugin works correctly:
/// 1. LSP server responds to textDocument/documentSymbol
/// 2. The lsp_navigation plugin receives the results
/// 3. The symbols are displayed in the command palette with correct labels
#[test]
#[cfg_attr(windows, ignore)] // Uses bash script for fake LSP server
fn test_lsp_navigation_symbols() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;
    open_symbol_navigation(&mut harness)?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("[class] MyClass"),
        "Screen should contain '[class] MyClass'. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("[construct] constructor"),
        "Screen should contain '[construct] constructor'. Screen:\n{}",
        screen
    );
    assert!(
        screen.contains("[method] myMethod"),
        "Screen should contain '[method] myMethod'. Screen:\n{}",
        screen
    );

    // Verify navigation through the symbols list changes the visual selection
    // highlight in the finder. The default selection (index 0 = MyClass) is
    // highlighted immediately — Down then moves to each subsequent symbol.

    // Check the default (initial) selection is MyClass
    harness.render()?;

    let selection =
        selected_suggestion_text(&harness).expect("Should find a selected suggestion row");
    assert!(
        selection.contains("[class] MyClass"),
        "Default selection should show MyClass, got: {selection}"
    );

    // Move to constructor (index 1)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.wait_until(|h| {
        selected_suggestion_text(h).is_some_and(|t| t.contains("[construct] constructor"))
    })?;

    // Move to myMethod (index 2)
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.wait_until(|h| {
        selected_suggestion_text(h).is_some_and(|t| t.contains("[method] myMethod"))
    })?;

    Ok(())
}

/// Test that the symbol matching the cursor line is preselected
#[test]
#[cfg_attr(windows, ignore)]
fn test_lsp_navigation_preselection() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;

    // Move cursor to line 5 (myMethod line)
    for _ in 0..5 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }
    harness.render()?;

    open_symbol_navigation(&mut harness)?;

    // Verify myMethod is preselected (not the default first item)
    let selection =
        selected_suggestion_text(&harness).expect("Should find a selected suggestion row");
    assert!(
        selection.contains("[method] myMethod"),
        "myMethod should be preselected when cursor is on line 5, got: {selection}"
    );

    Ok(())
}

/// Test that cursor on a line with no symbol defaults to first item
#[test]
#[cfg_attr(windows, ignore)]
fn test_lsp_navigation_no_match_fallback() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;

    // Move cursor to line 4 (blank line between constructor and myMethod)
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    }
    harness.render()?;

    open_symbol_navigation(&mut harness)?;

    // Verify the default (first item) is selected
    let selection =
        selected_suggestion_text(&harness).expect("Should find a selected suggestion row");
    assert!(
        selection.contains("[class] MyClass"),
        "Default (first) item should be selected on non-symbol line, got: {selection}"
    );

    Ok(())
}

/// Confirming a result jumps the cursor to the *exact* position of the
/// symbol name (including column), not the start of the declaration line.
#[test]
#[cfg_attr(windows, ignore)]
fn test_lsp_navigation_jumps_to_precise_column() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;
    open_symbol_navigation(&mut harness)?;

    // Default selection is MyClass (index 0); step down to myMethod (index 2).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.wait_until(|h| {
        selected_suggestion_text(h).is_some_and(|t| t.contains("[method] myMethod"))
    })?;

    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.process_async_and_render()?;

    // `myMethod` is indented two spaces, so its name starts at column 3. The
    // status bar must show that column — the cursor lands on the name, not at
    // column 1 (the start of the declaration line). Observing the rendered
    // column proves the precise-column jump.
    harness.wait_until(|h| h.screen_to_string().contains("Col 3"))?;
    assert!(
        harness.screen_to_string().contains("Col 3"),
        "Enter should jump to the symbol name's column (Col 3). Screen:\n{}",
        harness.screen_to_string()
    );

    Ok(())
}

/// Preview is non-destructive: browsing the list (and cancelling) never
/// moves the cursor — only confirming with Enter does.
#[test]
#[cfg_attr(windows, ignore)]
fn test_lsp_navigation_cancel_restores_cursor() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;

    // Move to a known starting position: line 3 (`    return true;`).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.wait_until(|h| h.screen_to_string().contains("Ln 3, Col 1"))?;

    open_symbol_navigation(&mut harness)?;

    // Browse the list (preview moves the cursor so each symbol scrolls into
    // view) — the status bar tracks the previewed symbol, not line 3.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.process_async_and_render()?;

    // Cancel — the cursor (and status bar) must return to the start position.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE)?;
    harness.wait_until(|h| h.screen_to_string().contains("Ln 3, Col 1"))?;
    assert!(
        harness.screen_to_string().contains("Ln 3, Col 1"),
        "Esc should restore the original cursor position (Ln 3, Col 1). Screen:\n{}",
        harness.screen_to_string()
    );

    Ok(())
}

/// Each result row shows a snippet of the symbol's source line. The method
/// signature (with its argument list) appears only in the snippet, never in
/// the `[kind] name` label — so finding it proves the snippet is rendered.
#[test]
#[cfg_attr(windows, ignore)]
fn test_lsp_navigation_shows_line_snippet() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;
    open_symbol_navigation(&mut harness)?;
    harness.render()?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("myMethod(a:"),
        "Results list should show the source-line snippet next to the symbol. Screen:\n{}",
        screen
    );

    Ok(())
}

/// Moving through the list paints an overlay marker on the current symbol's
/// name in the buffer.
#[test]
#[cfg_attr(windows, ignore)]
fn test_lsp_navigation_highlights_symbol_overlay() -> anyhow::Result<()> {
    let (mut harness, _temp_dir) = setup_lsp_test()?;
    open_symbol_navigation(&mut harness)?;

    // Move to myMethod so its name is previewed (and overlaid) in the buffer.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.wait_until(|h| {
        selected_suggestion_text(h).is_some_and(|t| t.contains("[method] myMethod"))
    })?;
    harness.process_async_and_render()?;

    // The high-contrast theme (Config default) paints search matches — which
    // the overlay reuses — with bg [255, 255, 0].
    let match_bg = Color::Rgb(255, 255, 0);
    let row = buffer_overlay_row_text(&harness, match_bg)
        .expect("an overlay marker should be painted on the current symbol in the buffer");
    assert!(
        row.contains("myMethod"),
        "overlay should mark the myMethod name in the buffer, got row: {row}"
    );

    Ok(())
}

/// Scan the buffer region for a cell painted with `match_bg` and return that
/// row's text. We skip the menu/tab rows (y < 2 — the active tab is also
/// yellow) and the rightmost column (x >= 90 — a render edge artifact), and
/// stay in the top half of the screen, above the bottom-anchored finder
/// popup whose snippet highlight reuses the same colour.
fn buffer_overlay_row_text(harness: &EditorTestHarness, match_bg: Color) -> Option<String> {
    let screen = harness.screen_to_string();
    let buffer_rows = (screen.lines().count() / 2) as u16;

    for y in 2..buffer_rows {
        for x in 0..90u16 {
            if harness
                .get_cell_style(x, y)
                .is_some_and(|s| s.bg == Some(match_bg))
            {
                return Some(harness.screen_row_text(y));
            }
        }
    }
    None
}

/// Scan the screen for a cell styled with `suggestion_selected_bg`
/// (the visual highlight colour for the currently-selected suggestion)
/// and return the cleaned text of the row.
fn selected_suggestion_text(harness: &EditorTestHarness) -> Option<String> {
    let screen = harness.screen_to_string();
    let height = screen.lines().count();
    // The high-contrast theme (Config default) uses suggestion_selected_bg = [0, 100, 200]
    let selected_bg = Color::Rgb(0, 100, 200);

    for y in 0..height {
        let y = y as u16;
        // Check past the border column (x=0) for the selection background
        if harness
            .get_cell_style(2, y)
            .is_some_and(|s| s.bg == Some(selected_bg))
        {
            return Some(harness.screen_row_text(y));
        }
    }
    None
}
