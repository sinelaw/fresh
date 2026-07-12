//! Tests for issue #2605: "Format Buffer" ignores the active selection and
//! reformats the whole file — the documented LSP range formatting was
//! unreachable when a (default) external formatter existed.
//!
//! Root cause (pre-fix): `format_buffer` always ran the configured external
//! formatter first, and that formatter can only rewrite the whole file. The
//! LSP fallback (`request_formatting`) was only reached when no formatter was
//! set, and even then it always sent `textDocument/formatting` (whole
//! document), never `textDocument/rangeFormatting`. A selection was therefore
//! silently ignored and the entire buffer was reformatted.
//!
//! Fix: when a selection is active and the language server advertises range
//! formatting, `format_buffer` routes through LSP `textDocument/rangeFormatting`
//! for just that range — bypassing the whole-file external formatter — matching
//! VS Code's "Format Selection". Without a selection, the existing whole-file
//! behavior is preserved.
//!
//! These tests use a bash fake LSP, so they are skipped on Windows.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};

/// A fake LSP server that advertises both whole-document and range formatting.
///
/// - `textDocument/rangeFormatting` returns an edit that reformats *only* the
///   first line (`local a=1` -> `local a = 1`) and logs the request.
/// - `textDocument/formatting` (whole document) logs the request and returns an
///   edit that also touches line 2 — so if it is ever called by mistake the
///   out-of-selection line changes and the test fails.
fn create_range_formatting_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash

LOG_FILE="${1:-/tmp/fake_lsp_range_log.txt}"
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
    if [ -z "$msg" ]; then break; fi

    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    echo "METHOD:$method" >> "$LOG_FILE"
    echo "---" >> "$LOG_FILE"

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"documentFormattingProvider":true,"documentRangeFormattingProvider":true,"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false}}}}'
            ;;
        "textDocument/rangeFormatting")
            echo "RANGE_BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            # Reformat only line 0: "local a=1" -> "local a = 1".
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":9}},"newText":"local a = 1"}]}'
            ;;
        "textDocument/formatting")
            echo "FORMAT_BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            # Whole-document: also touches line 1 so a wrong call is visible.
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":[{"range":{"start":{"line":0,"character":0},"end":{"line":1,"character":9}},"newText":"local a = 1\nlocal b = 2"}]}'
            ;;
        "textDocument/diagnostic")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"items":[]}}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
    esac
done
"##;

    let script_path = dir.join("fake_lsp_range_formatting.sh");
    std::fs::write(&script_path, script).expect("Failed to write fake LSP script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }

    script_path
}

/// An external formatter that appends a sentinel line. It stands in for a
/// default formatter like rustfmt: it can only rewrite the whole file. If
/// `format_buffer` runs it, the sentinel appears in the buffer and the test
/// fails — proving whether the selection routed to the external formatter or
/// to LSP range formatting.
fn create_sentinel_formatter(dir: &std::path::Path) -> std::path::PathBuf {
    let script = "#!/bin/sh\ncat\nprintf '%s\\n' '-- EXTERNAL_FORMATTER_RAN'\n";
    let path = dir.join("sentinel_fmt.sh");
    std::fs::write(&path, script).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&path, perms).unwrap();
    }
    path
}

fn setup(
    temp_dir: &tempfile::TempDir,
    log_file: &std::path::Path,
    with_external_formatter: bool,
) -> anyhow::Result<(EditorTestHarness, std::path::PathBuf)> {
    let script_path = create_range_formatting_lsp_script(temp_dir.path());
    let test_file = temp_dir.path().join("test.lua");
    std::fs::write(&test_file, "local a=1\nlocal b=2\nlocal c=3\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "lua".to_string(),
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

    if with_external_formatter {
        let fmt = create_sentinel_formatter(temp_dir.path());
        let entry = config
            .languages
            .entry("lua".to_string())
            .or_insert_with(fresh::config::LanguageConfig::default);
        entry.formatter = Some(fresh::config::FormatterConfig {
            command: fmt.to_string_lossy().to_string(),
            args: vec![],
            stdin: true,
            timeout_ms: 10_000,
        });
    }

    let mut harness = EditorTestHarness::create(
        120,
        30,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(temp_dir.path().to_path_buf()),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for the LSP to be reported ready (capabilities received).
    harness.wait_for_screen_contains("LSP (on)")?;

    Ok((harness, test_file))
}

fn run_format_buffer(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.render()?;
    harness.type_text("Format Buffer")?;
    harness.wait_for_screen_contains("Format Buffer")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;
    Ok(())
}

/// With an active selection and an external formatter configured, Format Buffer
/// must range-format only the selection via LSP — not run the whole-file
/// external formatter.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_selection_routes_to_lsp_range_formatting() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("range_log.txt");
    let (mut harness, _f) = setup(&temp_dir, &log_file, /*with_external_formatter=*/ true)?;

    // Select the whole first line: from (0,0) down to (1,0).
    harness.send_key(KeyCode::Down, KeyModifiers::SHIFT)?;
    harness.render()?;
    assert!(
        harness.has_selection(),
        "precondition: a selection should be active"
    );

    run_format_buffer(&mut harness)?;

    // The server must receive a range-formatting request.
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("METHOD:textDocument/rangeFormatting")
    })?;

    // And the range edit must be applied: line 0 reformatted, lines 1-2 intact.
    harness.wait_until(|h| {
        h.get_buffer_content().as_deref() == Some("local a = 1\nlocal b=2\nlocal c=3\n")
    })?;

    let log = std::fs::read_to_string(&log_file).unwrap_or_default();
    assert!(
        !log.contains("METHOD:textDocument/formatting"),
        "whole-document formatting must not be requested when a selection is active; log:\n{log}"
    );

    let content = harness.get_buffer_content().unwrap_or_default();
    assert!(
        !content.contains("EXTERNAL_FORMATTER_RAN"),
        "the whole-file external formatter must not run for a selection; buffer:\n{content}"
    );

    Ok(())
}

/// Without a selection, Format Buffer keeps the existing whole-file behavior:
/// the configured external formatter runs and rewrites the whole buffer.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_no_selection_uses_whole_file_external_formatter() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let log_file = temp_dir.path().join("range_log_nosel.txt");
    let (mut harness, _f) = setup(&temp_dir, &log_file, /*with_external_formatter=*/ true)?;

    assert!(
        !harness.has_selection(),
        "precondition: no selection should be active"
    );

    run_format_buffer(&mut harness)?;

    // The external formatter appends its sentinel to the whole buffer.
    harness.wait_until(|h| {
        h.get_buffer_content()
            .unwrap_or_default()
            .contains("EXTERNAL_FORMATTER_RAN")
    })?;

    let log = std::fs::read_to_string(&log_file).unwrap_or_default();
    assert!(
        !log.contains("METHOD:textDocument/rangeFormatting"),
        "range formatting must not be requested without a selection; log:\n{log}"
    );

    Ok(())
}
