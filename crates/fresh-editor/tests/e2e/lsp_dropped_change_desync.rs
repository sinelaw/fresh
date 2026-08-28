//! E2E regression test for #3038 — "LSP events seem to get stuck".
//!
//! The LSP command queue is bounded. A server that stops draining its stdin
//! (a stalled `gopls`, a long reindex) backs that queue up until the editor
//! can no longer enqueue a `didChange`. Those notifications carry *ranges*, so
//! losing one desynchronises the server's copy of the document from the buffer
//! for the rest of the server's life: every diagnostic it publishes afterwards
//! describes text the user never wrote, and no amount of further editing
//! repairs it.
//!
//! The reporter hit this with `gopls` while typing normally; the shape below
//! is the same failure made deterministic — the fake server stops reading
//! stdin on command, the test types past the queue's capacity, restores the
//! buffer to its original contents, and lets the server catch up.
//!
//! The fake server keeps its own copy of the document by applying the changes
//! it receives, exactly as a real server does, and reports an error whenever
//! that copy differs from the file's known-good contents. That makes the
//! divergence observable as the editor's error indicator.

use crate::common::harness::EditorTestHarness;

/// Buffer contents the file starts and ends at. The fake server reports an
/// error whenever its copy of the document differs from this.
const CLEAN_TEXT: &str = "ok";

/// How many single-character edits to make while the server is stalled.
///
/// This has to overflow the OS pipe buffer (64 KiB on Linux) *and* the
/// editor's bounded command queue before any change is actually dropped, so
/// it is deliberately well past both. The test asserts below that drops really
/// happened, so an environment with a larger pipe fails loudly rather than
/// passing vacuously.
const EDITS_WHILE_STALLED: usize = 600;

/// A fake LSP server that tracks the document the way a real one does.
///
/// Arguments: `<log> <stall-file> <clean-text>`. While the stall file exists
/// the server does not read stdin at all, which is what backs the editor's
/// queue up. It publishes one error whenever its copy of the document differs
/// from `<clean-text>`, and none when they match.
fn write_document_tracking_server(dir: &std::path::Path) -> std::path::PathBuf {
    let script = r#"#!/usr/bin/env python3
import sys, os, json, time

LOG, STALL, CLEAN = sys.argv[1], sys.argv[2], sys.argv[3]
# Unbuffered, so a stall really leaves the bytes in the pipe.
fin = os.fdopen(sys.stdin.fileno(), "rb", 0)
fout = os.fdopen(sys.stdout.fileno(), "wb", 0)

docs = {}
changes_received = 0


def log(msg):
    with open(LOG, "a") as f:
        f.write(msg + "\n")


def send(payload):
    body = json.dumps(payload).encode()
    fout.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)


def read_message():
    stalled = False
    while os.path.exists(STALL):
        if not stalled:
            log("STALLED")
            stalled = True
        time.sleep(0.01)
    if stalled:
        log("RESUMED")

    length = 0
    while True:
        line = fin.readline()
        if not line:
            return None
        line = line.strip()
        if not line:
            break
        key, _, value = line.decode().partition(":")
        if key.strip().lower() == "content-length":
            length = int(value.strip())
    if length <= 0:
        return None
    body = b""
    while len(body) < length:
        chunk = fin.read(length - len(body))
        if not chunk:
            return None
        body += chunk
    return json.loads(body.decode())


def apply_change(text, change):
    # A change with no range replaces the whole document.
    if change.get("range") is None:
        return change["text"]
    lines = text.split("\n")

    def offset(pos):
        line = max(0, min(pos["line"], len(lines) - 1))
        base = sum(len(l) + 1 for l in lines[:line])
        # Clamp: once the server has fallen behind, the editor's positions
        # can point past the end of the copy it is holding.
        return base + min(pos["character"], len(lines[line]))

    start = max(0, min(offset(change["range"]["start"]), len(text)))
    end = max(start, min(offset(change["range"]["end"]), len(text)))
    return text[:start] + change["text"] + text[end:]


def publish(uri, version):
    text = docs.get(uri, "")
    if text == CLEAN:
        diagnostics = []
    else:
        diagnostics = [{
            "range": {"start": {"line": 0, "character": 0},
                      "end": {"line": 0, "character": 1}},
            "severity": 1,
            "message": "server copy differs from buffer",
        }]
    params = {"uri": uri, "diagnostics": diagnostics}
    if version is not None:
        params["version"] = version
    send({"jsonrpc": "2.0", "method": "textDocument/publishDiagnostics",
          "params": params})
    log("PUBLISH errors=%d len=%d" % (len(diagnostics), len(text)))


while True:
    msg = read_message()
    if msg is None:
        break
    method = msg.get("method")
    params = msg.get("params") or {}

    if method == "initialize":
        send({"jsonrpc": "2.0", "id": msg.get("id"),
              "result": {"capabilities": {"textDocumentSync": 2}}})
    elif method == "shutdown":
        send({"jsonrpc": "2.0", "id": msg.get("id"), "result": None})
    elif method == "exit":
        break
    elif method == "textDocument/didOpen":
        doc = params["textDocument"]
        docs[doc["uri"]] = doc["text"]
        log("DIDOPEN")
        publish(doc["uri"], doc.get("version"))
    elif method == "textDocument/didChange":
        doc = params["textDocument"]
        uri = doc["uri"]
        text = docs.get(uri, "")
        for change in params.get("contentChanges", []):
            text = apply_change(text, change)
        docs[uri] = text
        changes_received += 1
        log("CHANGES_RECEIVED %d" % changes_received)
        publish(uri, doc.get("version"))
    elif msg.get("id") is not None:
        send({"jsonrpc": "2.0", "id": msg["id"], "result": None})
"#;

    let script_path = dir.join("document_tracking_lsp.py");
    std::fs::write(&script_path, script).expect("write fake server");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&script_path)
            .expect("script metadata")
            .permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&script_path, perms).expect("chmod script");
    }

    script_path
}

fn count_from_log(log: &str, prefix: &str) -> usize {
    log.lines()
        .filter_map(|line| line.strip_prefix(prefix))
        .filter_map(|rest| rest.trim().parse::<usize>().ok())
        .max()
        .unwrap_or(0)
}

/// Typing through a stalled server and then restoring the buffer must leave
/// the editor reporting no errors, because the server's copy of the document
/// gets repaired rather than left diverged.
///
/// Without the fix the editor drops the `didChange`s it cannot enqueue, the
/// server stays stuck on a document the user never wrote, and the error
/// indicator never clears.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // fake server is a POSIX script
fn dropped_did_change_recovers_once_server_drains() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let script_path = write_document_tracking_server(temp_dir.path());
    let log_file = temp_dir.path().join("lsp.log");
    let stall_file = temp_dir.path().join("stall");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, CLEAN_TEXT)?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: Some(vec![
                log_file.to_string_lossy().to_string(),
                stall_file.to_string_lossy().to_string(),
                CLEAN_TEXT.to_string(),
            ]),
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("DIDOPEN")
    })?;

    harness.send_key(
        crossterm::event::KeyCode::End,
        crossterm::event::KeyModifiers::NONE,
    )?;

    // Stop the server reading stdin, then edit far past what the editor can
    // queue for it. The server only re-checks the gate between messages, so
    // one edit is needed to carry it out of the read it is parked in.
    std::fs::write(&stall_file, "")?;
    harness.type_text("(")?;
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("STALLED")
    })?;

    harness.type_text(&"(".repeat(EDITS_WHILE_STALLED - 1))?;
    // Put the buffer back exactly as it started. These deletions are ordinary
    // range edits, so they cannot repair a server that missed the insertions.
    harness.send_key_repeat(
        crossterm::event::KeyCode::Backspace,
        crossterm::event::KeyModifiers::NONE,
        EDITS_WHILE_STALLED,
    )?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains(CLEAN_TEXT),
        "buffer should be back to its original contents.\nScreen:\n{screen}"
    );

    // Let the server catch up on everything the editor managed to send.
    std::fs::remove_file(&stall_file)?;
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("RESUMED")
    })?;

    // The server works through its backlog and reports the divergence it is
    // holding. Waiting for this first keeps the assertion below from passing
    // just because diagnostics had not arrived yet.
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("PUBLISH errors=1")
    })?;

    // The editor must repair the server's copy, so the error clears.
    harness.wait_until(|h| !h.screen_to_string().contains("E:1"))?;

    let log = std::fs::read_to_string(&log_file)?;
    let received = count_from_log(&log, "CHANGES_RECEIVED");
    let sent = EDITS_WHILE_STALLED * 2;
    assert!(
        received < sent,
        "the stall must actually overflow the command queue, otherwise this \
         test proves nothing: server received {received} of {sent} changes.\n\
         Log:\n{log}"
    );

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("E:1"),
        "no error should be reported for a buffer matching the server's copy.\n\
         Screen:\n{screen}\nLog:\n{log}"
    );

    // The screen clearing is the symptom; the invariant is that the server
    // ends up holding the buffer's text. Assert that directly, so this cannot
    // pass on a mechanism that merely hides the server's diagnostics.
    let last_publish = log
        .lines()
        .rfind(|line| line.starts_with("PUBLISH"))
        .unwrap_or("<none>");
    assert_eq!(
        last_publish,
        format!("PUBLISH errors=0 len={}", CLEAN_TEXT.len()),
        "the server's copy of the document must converge on the buffer.\n\
         Log:\n{log}"
    );

    Ok(())
}
