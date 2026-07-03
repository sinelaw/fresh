//! Regression test for issue #2146.
//!
//! Markdown compose (preview) mode hides LSP diagnostic highlighting and the
//! gutter indicators. e.g. enabling harper-ls to find spelling mistakes: the
//! diagnostics show up in source mode but vanish the moment compose mode is
//! toggled on, even though the diagnostic data is still present (the status
//! bar still reports `E:1`).
//!
//! The test uses a fake LSP server (configured for the `markdown` language)
//! that publishes a single error diagnostic on `didOpen`, then drives the real
//! `markdown_compose` plugin to toggle compose mode and checks that the gutter
//! diagnostic indicator (`●`) survives the transition.

use crate::common::harness::EditorTestHarness;

/// Fake LSP server (à la harper-ls) that publishes one error diagnostic on
/// `didOpen`. The diagnostic covers the misspelled word on line 2.
fn create_markdown_diag_server_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_md_diag_log.txt}"
> "$LOG_FILE"
DID_OPEN_URI=""

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
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    echo "RECV: method=$method id=$msg_id" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}}}}}'
            ;;
        "initialized")
            ;;
        "textDocument/didOpen")
            DID_OPEN_URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "ACTION: didOpen uri=$DID_OPEN_URI" >> "$LOG_FILE"
            # Spelling error on "sentance" (line 2, chars 5-13).
            send_message '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"'"$DID_OPEN_URI"'","diagnostics":[{"range":{"start":{"line":2,"character":5},"end":{"line":2,"character":13}},"severity":1,"source":"harper","message":"Did you mean \"sentence\"?"}],"version":1}}'
            echo "SENT: publishDiagnostics" >> "$LOG_FILE"
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            break
            ;;
        *)
            if [ -n "$method" ] && [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":null}'
            fi
            ;;
    esac
done
echo "SERVER: exiting" >> "$LOG_FILE"
"##;

    let script_path = dir.join("fake_md_diag_server.sh");
    std::fs::write(&script_path, script).expect("Failed to write markdown diag server script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("Failed to get script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("Failed to set script permissions");
    }

    script_path
}

/// Issue #2146: LSP diagnostics should remain visible after toggling compose
/// mode. The diagnostic data is still live (the status bar shows `E:1`) but the
/// gutter indicator (`●`) disappears because compose mode hides line numbers,
/// which in turn disables the entire left margin (gutter).
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_compose_mode_keeps_diagnostic_gutter() -> anyhow::Result<()> {
    use crate::common::harness::{copy_plugin, copy_plugin_lib};
    use crossterm::event::{KeyCode, KeyModifiers};

    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    // -- Set up a project with the real markdown_compose plugin ──────────
    let temp_dir = tempfile::tempdir()?;
    let project_root = temp_dir.path().join("project");
    std::fs::create_dir(&project_root)?;

    let plugins_dir = project_root.join("plugins");
    std::fs::create_dir(&plugins_dir)?;
    copy_plugin(&plugins_dir, "markdown_compose");
    copy_plugin_lib(&plugins_dir);

    let script_path = create_markdown_diag_server_script(temp_dir.path());
    let log_file = temp_dir.path().join("md_diag_log.txt");

    // Line 2 holds the misspelled word "sentance" the fake server flags.
    let md_path = project_root.join("notes.md");
    std::fs::write(&md_path, "# Notes\n\nThis sentance is wrong.\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "markdown".to_string(),
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

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root)?;

    // The error background color the diagnostic overlay paints on the word.
    let error_bg = harness.editor().theme().diagnostic_error_bg;

    // Scan the content area for `word` and report whether every cell that
    // spells it carries `error_bg` as its background — i.e. the diagnostic
    // highlight is actually painted on the word, not just stored.
    let word_is_highlighted = |h: &EditorTestHarness, word: &str| -> bool {
        let (first_row, last_row) = h.content_area_rows();
        for y in first_row..=last_row {
            let row = h.get_row_text(y as u16);
            if let Some(byte_idx) = row.find(word) {
                // `find` returns a byte offset; the gutter glyphs (●, │) are
                // multi-byte, so convert to a screen column count.
                let start_col = row[..byte_idx].chars().count();
                return (start_col..start_col + word.chars().count()).all(|x| {
                    h.get_cell_style(x as u16, y as u16)
                        .and_then(|s| s.bg)
                        .map(|bg| bg == error_bg)
                        .unwrap_or(false)
                });
            }
        }
        false
    };

    // Open the markdown file (triggers initialize + didOpen → publishDiagnostics).
    harness.open_file(&md_path)?;
    harness.render()?;

    // Wait for the diagnostic to round-trip into the editor.
    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("SENT: publishDiagnostics")
    })?;
    harness.wait_until(|h| h.screen_to_string().contains("E:1"))?;

    // -- Source mode: the gutter diagnostic indicator must be visible ────
    let source_screen = harness.screen_to_string();
    assert!(
        source_screen.contains('●'),
        "Expected the diagnostic gutter indicator (●) in source mode.\nScreen:\n{}",
        source_screen
    );
    // ...and the misspelled word itself is highlighted with the error bg.
    assert!(
        word_is_highlighted(&harness, "sentance"),
        "Expected the misspelled word to be highlighted in source mode.\nScreen:\n{}",
        source_screen
    );

    // -- Toggle compose mode via the command palette ─────────────────────
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;
    harness.type_text("Toggle Compose")?;
    harness.wait_for_screen_contains("Toggle Compose")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.wait_for_prompt_closed()?;

    // Set a compose width narrower than the terminal so there's a reading
    // margin. The diagnostic / indicator gutter is drawn in that reclaimed
    // desk margin (so it doesn't shrink the text width).
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.wait_for_prompt()?;
    harness.type_text("Set Compose Width")?;
    harness.wait_for_screen_contains("Set Compose Width")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.wait_for_prompt()?;
    harness.type_text("80")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.wait_for_prompt_closed()?;

    // Wait for compose mode to settle. Compose hides the line-number gutter,
    // so the `│` separator disappears from the content line — a reliable
    // signal that the mode flipped (the heading text itself stays on screen).
    harness.wait_until_stable(|h| {
        let s = h.screen_to_string();
        s.contains("sentance") && !s.lines().any(|l| l.contains("sentance") && l.contains('│'))
    })?;

    let compose_screen = harness.screen_to_string();

    // The diagnostic data is still present — the status bar still reports it.
    assert!(
        compose_screen.contains("E:1"),
        "Diagnostic data should still be live in compose mode (status bar E:1).\nScreen:\n{}",
        compose_screen
    );

    // Issue #2146: the gutter indicator must still be shown in compose mode.
    assert!(
        compose_screen.contains('●'),
        "Issue #2146: the diagnostic gutter indicator (●) disappeared in \
         compose mode.\nScreen:\n{}",
        compose_screen
    );

    // Issue #2146: the word-level highlight must also survive compose mode.
    assert!(
        word_is_highlighted(&harness, "sentance"),
        "Issue #2146: the misspelled word lost its diagnostic highlight in \
         compose mode.\nScreen:\n{}",
        compose_screen
    );

    Ok(())
}
