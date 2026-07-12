//! Reproduction for sinelaw/fresh#2615 — "incremental type checking not
//! working".
//!
//! User setup: two Python LSP servers configured for the same language,
//! `ruff` (linter) and `ty` (type checker), both of which serve diagnostics
//! via the pull model (`textDocument/diagnostic`). Symptom: `ty`'s type
//! errors never refresh after an edit — they only update on restart.
//!
//! Root cause: every pull-diagnostic request site resolved a *single*
//! handle via `handle_for_feature_mut(Diagnostics)` — the first-listed
//! diagnostic-capable server. But `Diagnostics` is a *merged* feature:
//! results are supposed to be combined from all eligible servers. So only
//! the first server (`ruff`) was ever re-pulled after an edit; the second
//! (`ty`) went stale until a restart re-opened the document.
//!
//! This test spawns two fake pull-diagnostic servers for the same language,
//! each logging the URIs it is asked to pull. After an edit, BOTH servers
//! must be re-pulled.

use crate::common::harness::EditorTestHarness;

/// Fake pull-diagnostic server: advertises `diagnosticProvider`, logs every
/// `textDocument/diagnostic` URI to the file given as `$1`, and always
/// responds with a full report carrying one diagnostic.
fn create_pull_diag_server_script(dir: &std::path::Path, file_name: &str) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_pull_log.txt}"
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
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"capabilities":{"positionEncoding":"utf-16","textDocumentSync":{"openClose":true,"change":2,"save":{}},"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false}}}}'
            ;;
        "initialized") ;;
        "textDocument/didOpen")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "DIDOPEN: $URI" >> "$LOG_FILE"
            ;;
        "textDocument/diagnostic")
            URI=$(echo "$msg" | grep -o '"uri":"[^"]*"' | head -1 | cut -d'"' -f4)
            echo "PULL: $URI" >> "$LOG_FILE"
            send_message '{"jsonrpc":"2.0","id":'"$msg_id"',"result":{"kind":"full","resultId":"fake","items":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"severity":1,"message":"diag from fake server"}]}}'
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

    let script_path = dir.join(file_name);
    std::fs::write(&script_path, script).expect("Failed to write fake pull-diag server");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).unwrap();
    }

    script_path
}

fn pull_count(log_path: &std::path::Path, uri_fragment: &str) -> usize {
    std::fs::read_to_string(log_path)
        .unwrap_or_default()
        .lines()
        .filter(|l| l.starts_with("PULL:") && l.contains(uri_fragment))
        .count()
}

/// Drive the editor event loop (processing async LSP messages and advancing
/// the debounce clock) until `cond` holds or `max_iters` elapse. Unlike
/// `harness.wait_until`, this is bounded: a regression makes the test *fail*
/// rather than hang forever on an unsatisfiable condition.
fn pump_until<F>(harness: &mut EditorTestHarness, max_iters: usize, cond: F) -> anyhow::Result<bool>
where
    F: Fn() -> bool,
{
    for _ in 0..max_iters {
        harness.tick_and_render()?;
        if cond() {
            return Ok(true);
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    Ok(cond())
}

/// With two pull-diagnostic servers configured for one language, an edit
/// must re-pull diagnostics from BOTH servers. Before the fix, only the
/// first-listed server was ever pulled, so the second server's diagnostics
/// (e.g. `ty`'s type errors) never refreshed until restart.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_all_pull_diagnostic_servers_repulled_after_edit() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = tempfile::tempdir()?;
    let script_a = create_pull_diag_server_script(temp_dir.path(), "fake_server_a.sh");
    let script_b = create_pull_diag_server_script(temp_dir.path(), "fake_server_b.sh");
    let log_a = temp_dir.path().join("server_a_log.txt");
    let log_b = temp_dir.path().join("server_b_log.txt");

    let rust_file = temp_dir.path().join("api.rs");
    std::fs::write(&rust_file, "fn main() {}\n")?;

    let make_server = |script: &std::path::Path, log: &std::path::Path, name: &str| {
        fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: Some(vec![log.to_string_lossy().to_string()]),
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
    };

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![
            make_server(&script_a, &log_a, "linter"),
            make_server(&script_b, &log_b, "typechecker"),
        ]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&rust_file)?;
    harness.render()?;

    // Both servers should receive the initial open pull. This alone already
    // failed before the fix for the second (non-first-listed) server.
    let both_opened = pump_until(&mut harness, 200, || {
        pull_count(&log_a, "api.rs") >= 1 && pull_count(&log_b, "api.rs") >= 1
    })?;
    assert!(
        both_opened,
        "BUG #2615: both servers should receive the initial open pull \
         (linter={}, typechecker={})",
        pull_count(&log_a, "api.rs"),
        pull_count(&log_b, "api.rs"),
    );

    // Edit the buffer: schedules a per-edit diagnostic re-pull (debounced).
    use crossterm::event::{KeyCode, KeyModifiers};
    harness.send_key(KeyCode::Char('x'), KeyModifiers::NONE)?;

    // After the edit, BOTH servers must be pulled again — not just the
    // first-listed one. This is the core of #2615.
    let both_repulled = pump_until(&mut harness, 300, || {
        pull_count(&log_a, "api.rs") >= 2 && pull_count(&log_b, "api.rs") >= 2
    })?;

    let a = pull_count(&log_a, "api.rs");
    let b = pull_count(&log_b, "api.rs");
    eprintln!("[TEST] linter pulls={a}, typechecker pulls={b}");

    assert!(
        a >= 2,
        "Sanity: first-listed server should be re-pulled after an edit (pulls={a})"
    );
    assert!(
        both_repulled && b >= 2,
        "BUG #2615: the second diagnostic server was not re-pulled after an edit \
         (pulls={b}). Diagnostics is a merged feature — every pull-capable server \
         must be re-pulled, otherwise its diagnostics (e.g. `ty` type errors) go \
         stale until restart."
    );

    Ok(())
}
