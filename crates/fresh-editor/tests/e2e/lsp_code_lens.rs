//! End-to-end coverage for rendering and executing LSP code lenses.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

fn create_code_lens_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

LOG_FILE="$1"
> "$LOG_FILE"

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
    if [ "$content_length" -gt 0 ]; then
        dd bs=1 count="$content_length" 2>/dev/null
    fi
}

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "METHOD:$method" >> "$LOG_FILE"
    echo "BODY:$msg" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"codeLensProvider":{"resolveProvider":false}}}}'
            ;;
        "textDocument/codeLens")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"command":{"title":"Run Test","command":"test.run","arguments":["unit"]}}]}'
            ;;
        "workspace/executeCommand")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_code_lens_lsp.sh");
    std::fs::write(&script_path, script).expect("failed to write fake LSP script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut permissions = std::fs::metadata(&script_path).unwrap().permissions();
        permissions.set_mode(0o755);
        std::fs::set_permissions(&script_path, permissions).unwrap();
    }

    script_path
}

/// Open `source` as `test.rs` against the fake code-lens server and wait until
/// the server is ready. Returns the harness and the path the server logs the
/// traffic it saw to.
fn open_with_code_lens_server(
    temp_dir: &std::path::Path,
    source: &str,
) -> anyhow::Result<(EditorTestHarness, std::path::PathBuf)> {
    let log_file = temp_dir.join("code_lens.log");
    let script_path = create_code_lens_lsp_script(temp_dir);
    let test_file = temp_dir.join("test.rs");
    std::fs::write(&test_file, source)?;

    let mut config = fresh::config::Config::default();
    config.editor.enable_code_lens = true;
    config.editor.enable_inlay_hints = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![log_file.to_string_lossy().to_string()]),
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
        80,
        24,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.to_path_buf()),
    )?;
    harness.open_file(&test_file)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().active_window().is_lsp_server_ready("rust"))?;

    Ok((harness, log_file))
}

/// The screen cell a needle starts at, as (row, column) in terminal cells.
fn screen_cell_of(screen: &str, needle: &str) -> Option<(u16, u16)> {
    screen.lines().enumerate().find_map(|(row, line)| {
        line.find(needle).map(|byte| {
            let col = unicode_width::UnicodeWidthStr::width(&line[..byte]);
            (row as u16, col as u16)
        })
    })
}

#[test]
#[cfg_attr(windows, ignore = "uses a Bash fake LSP server")]
fn test_code_lens_renders_and_executes_command() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let (mut harness, log_file) = open_with_code_lens_server(temp_dir.path(), "fn main() {}\n")?;

    harness.wait_for_screen_contains("Run Test")?;
    assert!(
        harness.screen_to_string().contains("Run Test"),
        "code lens should render as a virtual line"
    );

    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.type_text("Execute Code Lens")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_for_screen_contains("Code Lenses")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:workspace/executeCommand")
            && log.contains("\"command\":\"test.run\"")
            && log.contains("\"arguments\":[\"unit\"]")
    })?;

    Ok(())
}

/// A lens is run by clicking the title where it is drawn, so drive that:
/// locate "Run Test" on the rendered screen and press that cell. This is the
/// only coverage of the mouse path — `code_lens_command_at_screen_position`
/// resolves the press against the rows the pane last drew, which the command
/// palette above never reaches.
#[test]
#[cfg_attr(windows, ignore = "uses a Bash fake LSP server")]
fn test_clicking_rendered_code_lens_executes_command() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let (mut harness, log_file) = open_with_code_lens_server(temp_dir.path(), "fn main() {}\n")?;

    harness.wait_for_screen_contains("Run Test")?;
    let screen = harness.screen_to_string();
    let (row, col) = screen_cell_of(&screen, "Run Test")
        .unwrap_or_else(|| panic!("code lens must be drawn on screen:\n{screen}"));

    // The lens is a virtual line, so the source it belongs to is still there.
    assert!(
        screen.contains("fn main() {}"),
        "the lens must not displace the source line:\n{screen}"
    );

    harness.mouse_click(col, row)?;

    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:workspace/executeCommand")
            && log.contains("\"command\":\"test.run\"")
            && log.contains("\"arguments\":[\"unit\"]")
    })?;

    Ok(())
}

/// "Toggle Code Lens" from the command palette hides the lenses and brings
/// them back — the only way to turn the feature off without editing config.
///
/// Asserts on the rendered screen both ways round: hiding that leaves the
/// lens drawn, or a re-enable that never re-requests, are both invisible to
/// a test that only checks one direction.
#[test]
#[cfg_attr(windows, ignore = "uses a Bash fake LSP server")]
fn test_toggle_code_lens_hides_and_restores_the_lenses() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let (mut harness, _log_file) = open_with_code_lens_server(temp_dir.path(), "fn main() {}\n")?;

    harness.wait_for_screen_contains("Run Test")?;
    // The source stays put throughout; only the lens line comes and goes.
    assert!(harness.screen_to_string().contains("fn main() {}"));

    let toggle = |h: &mut EditorTestHarness| -> anyhow::Result<()> {
        h.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
        h.render()?;
        h.type_text("Toggle Code Lens")?;
        h.wait_for_screen_contains("Toggle Code Lens")?;
        h.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
        h.render()?;
        Ok(())
    };

    toggle(&mut harness)?;
    harness.wait_until(|h| !h.screen_to_string().contains("Run Test"))?;
    assert!(
        harness.screen_to_string().contains("fn main() {}"),
        "turning lenses off must not disturb the source:\n{}",
        harness.screen_to_string()
    );

    toggle(&mut harness)?;
    harness.wait_for_screen_contains("Run Test")?;

    Ok(())
}
