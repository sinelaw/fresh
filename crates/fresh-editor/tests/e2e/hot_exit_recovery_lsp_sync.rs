//! Regression test: hot-exit recovery must keep LSP in sync.
//!
//! Flow that caused the bug:
//! 1. fresh restores a workspace that includes a file-backed buffer
//!    with unsaved modifications saved via hot-exit.
//! 2. `open_file_no_focus` loads the on-disk file and immediately calls
//!    `notify_lsp_file_opened`, which sends `didOpen` to the LSP server
//!    with the on-disk content.
//! 3. `apply_workspace` then applies hot-exit recovery, mutating the
//!    buffer directly via `buffer.delete` + `buffer.insert`. No LSP
//!    notification is sent.
//! 4. The LSP server's view of the document is now the pre-recovery
//!    (on-disk) content, while fresh's buffer and display have the
//!    post-recovery content. Every position-bearing LSP response is
//!    offset by the size of the edits before the position — hovers,
//!    semantic tokens, diagnostics, etc. all land on the wrong column.
//!
//! The fix sends a full-document `didChange` after recovery replay, so
//! the server stays in sync regardless of when its `didOpen` was sent.
//!
//! The test uses a fake LSP that logs every `didOpen`/`didChange` body
//! to a file, then asserts that the final server-side view contains the
//! post-recovery marker. Without the fix, only the pre-recovery body is
//! ever sent and `wait_until` times out on the post-recovery content.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use fresh::config_io::DirectoryContext;
use tempfile::TempDir;

/// Fake LSP server script: logs every didOpen/didChange message body to
/// a file. Returns empty diagnostics so fresh doesn't get stuck on
/// semantic highlights.
fn create_logging_lsp_script(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r##"#!/bin/bash
LOG_FILE="${1:-/tmp/fake_lsp_log.txt}"
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

    case "$method" in
        "textDocument/didOpen")
            echo "METHOD:textDocument/didOpen" >> "$LOG_FILE"
            echo "BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
        "textDocument/didChange")
            echo "METHOD:textDocument/didChange" >> "$LOG_FILE"
            echo "BODY:$msg" >> "$LOG_FILE"
            echo "---" >> "$LOG_FILE"
            ;;
    esac

    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"textDocumentSync":2,"diagnosticProvider":{"interFileDependencies":false,"workspaceDiagnostics":false}}}}'
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

    let script_path = dir.join("fake_lsp_logging.sh");
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

fn rust_lsp_config(script: &std::path::Path, log_file: &std::path::Path) -> Config {
    let mut config = Config::default();
    config.editor.hot_exit = true;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: vec![log_file.to_string_lossy().to_string()],
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
    config
}

/// Returns the concatenation of every logged message body. The assertion
/// looks for a specific marker anywhere in a didOpen or didChange body.
fn log_body_text(log_file: &std::path::Path) -> String {
    std::fs::read_to_string(log_file).unwrap_or_default()
}

#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_hot_exit_recovery_syncs_lsp() -> anyhow::Result<()> {
    let _ = tracing_subscriber::fmt()
        .with_env_filter("fresh=debug")
        .try_init();

    let temp_dir = TempDir::new()?;
    let project_dir = temp_dir.path().join("project");
    std::fs::create_dir(&project_dir)?;
    let script_path = create_logging_lsp_script(temp_dir.path());
    let log_file = temp_dir.path().join("lsp_log.txt");
    let file_path = project_dir.join("test.rs");

    // ORIGINAL_MARKER on disk; the modified buffer (saved via hot-exit)
    // replaces it with MODIFIED_MARKER.
    std::fs::write(&file_path, "fn main() {\n    let ORIGINAL_MARKER = 1;\n}\n")?;

    let dir_context = DirectoryContext::for_testing(temp_dir.path());

    // --- Session 1: open, edit, clean shutdown. No LSP configured so the
    //     recovery file is the only state that crosses between sessions.
    {
        let mut config = Config::default();
        config.editor.hot_exit = true;

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )?;

        harness.editor_mut().set_session_mode(true);
        harness.open_file(&file_path)?;
        harness.render()?;

        // Navigate to "ORIGINAL_MARKER", select the word, and replace it
        // with "MODIFIED_MARKER". We drive via key events so the buffer
        // goes through the normal edit path and recovery picks it up.
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
        harness.send_key(KeyCode::Home, KeyModifiers::NONE)?;
        // Move right past the four-space indent and "let " (8 chars total)
        // to land on the first char of ORIGINAL_MARKER.
        for _ in 0..8 {
            harness.send_key(KeyCode::Right, KeyModifiers::NONE)?;
        }
        // Select the word (ORIGINAL_MARKER has 15 chars)
        for _ in 0..15 {
            harness.send_key(KeyCode::Right, KeyModifiers::SHIFT)?;
        }
        harness.type_text("MODIFIED_MARKER")?;
        harness.render()?;

        // Sanity: the modification landed in the buffer.
        assert!(
            harness.screen_to_string().contains("MODIFIED_MARKER"),
            "Precondition: edit should land in session 1. Screen:\n{}",
            harness.screen_to_string()
        );

        harness.shutdown(true)?;

        // On-disk file must still hold the original content — hot-exit
        // saves to the recovery dir, not to the working file.
        let disk = std::fs::read_to_string(&file_path)?;
        assert!(
            disk.contains("ORIGINAL_MARKER"),
            "Precondition: on-disk file should still hold ORIGINAL_MARKER. Got:\n{}",
            disk
        );
    }

    // --- Session 2: restore workspace with fake LSP configured. Recovery
    //     will replay MODIFIED_MARKER into the buffer. The LSP must end
    //     up with MODIFIED_MARKER as the document content.
    {
        let config = rust_lsp_config(&script_path, &log_file);

        let mut harness = EditorTestHarness::create(
            80,
            24,
            HarnessOptions::new()
                .with_config(config)
                .with_working_dir(project_dir.clone())
                .with_shared_dir_context(dir_context.clone())
                .without_empty_plugins_dir(),
        )?;

        let restored = harness.startup(true, &[])?;
        assert!(restored, "Workspace should be restored");
        harness.render()?;

        // Sanity: the display reflects the recovered buffer.
        harness.wait_for_screen_contains("MODIFIED_MARKER")?;

        // Wait until the fake LSP has logged at least one didOpen.
        harness.wait_until(|_| log_body_text(&log_file).contains("METHOD:textDocument/didOpen"))?;

        // Core assertion: the server-side view of the document must
        // reflect the recovered content. That means either:
        //   (a) the didOpen body already contains MODIFIED_MARKER
        //       (if LSP was spawned after recovery), or
        //   (b) a follow-up didChange contains MODIFIED_MARKER
        //       (if LSP was opened with on-disk content and then
        //       re-synced).
        //
        // Before the fix, (a) doesn't happen (didOpen fires inside
        // `open_file_no_focus` with on-disk content) and (b) doesn't
        // happen either (recovery mutates the buffer without notifying
        // LSP), so `wait_until` hangs until nextest's external timeout.
        harness.wait_until(|_| log_body_text(&log_file).contains("MODIFIED_MARKER"))?;

        // Defensive: the ORIGINAL text must not be the most recent
        // server-side state. Walk the log and check that the last
        // message body that touches document content holds the modified
        // marker rather than the original.
        let log = log_body_text(&log_file);
        let last_marker = last_occurrence_of_either(&log, "MODIFIED_MARKER", "ORIGINAL_MARKER");
        assert_eq!(
            last_marker,
            Some("MODIFIED_MARKER"),
            "The server's last content update should reflect the recovered \
             buffer, not the on-disk file. Full LSP log:\n{log}"
        );
    }

    Ok(())
}

/// Return whichever needle appears latest in `haystack`, or `None` if
/// neither appears. Used to check the final server-side state.
fn last_occurrence_of_either<'a>(haystack: &str, a: &'a str, b: &'a str) -> Option<&'a str> {
    let ia = haystack.rfind(a);
    let ib = haystack.rfind(b);
    match (ia, ib) {
        (Some(pa), Some(pb)) => Some(if pa >= pb { a } else { b }),
        (Some(_), None) => Some(a),
        (None, Some(_)) => Some(b),
        (None, None) => None,
    }
}
