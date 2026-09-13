//! E2E regression test for #3258 — stale LSP diagnostics after a vi-mode `dd`.
//!
//! `dd` edits through the plugin API, which did not notify the language
//! server, so its copy of the document kept the deleted line.
//!
//! The fake server tracks the document the way a real one does, so "the
//! server's copy matches the buffer" is asserted directly rather than inferred
//! from the diagnostics on screen.
//!
//! The last test covers the other half of the same report — the diagnostic
//! appearing in a second file — which came from `didSave` carrying the focused
//! buffer's text under another buffer's URI.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use fresh::input::keybindings::Action::PluginAction;
use std::fs;
use std::path::Path;

/// Line 3 is the one `dd` removes.
const INITIAL_TEXT: &str =
    "fn main() {\n    let keep = 1;\n    let unused = 2;\n    println!(\"{keep}\");\n}\n";

/// What the buffer holds once line 3 is deleted.
const AFTER_DD_TEXT: &str = "fn main() {\n    let keep = 1;\n    println!(\"{keep}\");\n}\n";

/// A fake LSP server that keeps its own copy of the document and logs it as
/// `DOC <json-encoded text>` after each notification it applies.
///
/// Arguments: `<log>`.
fn write_document_tracking_server(dir: &Path) -> std::path::PathBuf {
    let script = r#"#!/usr/bin/env python3
import sys, os, json

LOG = sys.argv[1]
fin = os.fdopen(sys.stdin.fileno(), "rb", 0)
fout = os.fdopen(sys.stdout.fileno(), "wb", 0)

docs = {}


def log(msg):
    with open(LOG, "a") as f:
        f.write(msg + "\n")


def send(payload):
    body = json.dumps(payload).encode()
    fout.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)


def read_message():
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
        return base + min(pos["character"], len(lines[line]))

    start = max(0, min(offset(change["range"]["start"]), len(text)))
    end = max(start, min(offset(change["range"]["end"]), len(text)))
    return text[:start] + change["text"] + text[end:]


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
        log("DOC " + json.dumps(docs[doc["uri"]]))
    elif method == "textDocument/didChange":
        doc = params["textDocument"]
        uri = doc["uri"]
        text = docs.get(uri, "")
        for change in params.get("contentChanges", []):
            text = apply_change(text, change)
        docs[uri] = text
        log("DOC " + json.dumps(text))
    elif method == "textDocument/didSave":
        doc = params["textDocument"]
        log("DIDSAVE %s %s" % (doc["uri"], json.dumps(params.get("text"))))
    elif msg.get("id") is not None:
        send({"jsonrpc": "2.0", "id": msg["id"], "result": None})
"#;

    let script_path = dir.join("document_tracking_lsp.py");
    fs::write(&script_path, script).expect("write fake server");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&script_path)
            .expect("script metadata")
            .permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&script_path, perms).expect("chmod script");
    }

    script_path
}

/// The document the server currently holds, read fresh: it appends as it works.
fn server_document(log_file: &Path) -> Option<String> {
    let log = fs::read_to_string(log_file).ok()?;
    let last = log.lines().rev().find_map(|l| l.strip_prefix("DOC "))?;
    serde_json::from_str::<String>(last).ok()
}

/// The text the server was handed by the `didSave` for the file named
/// `basename`, or `None` if it has had no such save.
fn saved_text(log_file: &Path, basename: &str) -> Option<String> {
    let log = fs::read_to_string(log_file).ok()?;
    let line = log
        .lines()
        .rev()
        .filter_map(|l| l.strip_prefix("DIDSAVE "))
        .find(|rest| {
            rest.split(' ')
                .next()
                .is_some_and(|uri| uri.ends_with(basename))
        })?;
    let (_uri, text) = line.split_once(' ')?;
    serde_json::from_str::<Option<String>>(text).ok().flatten()
}

/// The fake server as Rust's language server, with vi_mode loaded on request.
fn lsp_harness(
    with_vi_mode: bool,
) -> anyhow::Result<(
    EditorTestHarness,
    tempfile::TempDir,
    std::path::PathBuf,
    std::path::PathBuf,
)> {
    let temp_dir = tempfile::TempDir::new()?;
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root)?;

    let script_path = write_document_tracking_server(temp_dir.path());
    let log_file = temp_dir.path().join("lsp.log");

    let mut config = Config::default();
    if with_vi_mode {
        let plugins_dir = project_root.join("plugins");
        fs::create_dir(&plugins_dir)?;
        copy_plugin(&plugins_dir, "vi_mode");
        copy_plugin_lib(&plugins_dir);
        config.plugins.insert(
            "vi_mode".to_string(),
            PluginConfig {
                enabled: true,
                path: None,
                settings: serde_json::json!({}),
            },
        );
    }
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

    let test_file = project_root.join("main.rs");
    fs::write(&test_file, INITIAL_TEXT)?;

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, project_root.clone())?;
    harness.editor_mut().set_clipboard_for_test(String::new());

    Ok((harness, temp_dir, test_file, log_file))
}

fn vi_mode_lsp_harness() -> anyhow::Result<(
    EditorTestHarness,
    tempfile::TempDir,
    std::path::PathBuf,
    std::path::PathBuf,
)> {
    lsp_harness(true)
}

fn enable_vi_mode(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    harness.wait_until(|h| {
        h.editor()
            .command_registry()
            .read()
            .unwrap()
            .get_all()
            .iter()
            .any(|c| c.action == PluginAction("vi_mode_toggle".to_string()))
    })?;

    harness.send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)?;
    harness.render()?;
    harness.type_text("Toggle Vi")?;
    harness.wait_for_screen_contains("Toggle Vi mode")?;
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))?;
    Ok(())
}

/// Deleting a line with vi-mode `dd` must leave the server holding the same
/// text as the buffer. Before the fix it was never told about the deletion.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // fake server is a POSIX script
fn vi_dd_keeps_the_server_in_sync_with_the_buffer() -> anyhow::Result<()> {
    let (mut harness, _temp_dir, test_file, log_file) = vi_mode_lsp_harness()?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // The server has the file as it was opened.
    harness.wait_until(|_| server_document(&log_file).as_deref() == Some(INITIAL_TEXT))?;

    enable_vi_mode(&mut harness)?;

    // Put the caret on `let unused = 2;` (line 3) and delete the line with dd.
    harness.send_key(KeyCode::Char('j'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Char('j'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))?;

    harness.send_key(KeyCode::Char('d'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))?;
    harness.send_key(KeyCode::Char('d'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))?;

    // Pins the failure below to what the server was told, not to what `dd` did.
    harness.wait_until(|h| h.get_buffer_content().as_deref() == Some(AFTER_DD_TEXT))?;

    // Waiting, not asserting: a server that never converges leaves the test
    // pending until nextest kills it (CONTRIBUTING §3).
    harness.wait_until(|_| server_document(&log_file).as_deref() == Some(AFTER_DD_TEXT))?;

    // Restated so the invariant is readable here, and so a copy that converged
    // and then diverged still fails.
    assert_eq!(
        server_document(&log_file).as_deref(),
        Some(AFTER_DD_TEXT),
        "the server's copy of the document must follow a vi-mode `dd`.\n\
         Log:\n{}",
        fs::read_to_string(&log_file).unwrap_or_default()
    );

    Ok(())
}

/// The same deletion followed by a save: `didSave` carries the text but does
/// not replace the server's copy, so the save cannot repair a divergence.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // fake server is a POSIX script
fn saving_after_vi_dd_does_not_leave_the_server_diverged() -> anyhow::Result<()> {
    let (mut harness, _temp_dir, test_file, log_file) = vi_mode_lsp_harness()?;

    harness.open_file(&test_file)?;
    harness.render()?;
    harness.wait_until(|_| server_document(&log_file).as_deref() == Some(INITIAL_TEXT))?;

    enable_vi_mode(&mut harness)?;

    harness.send_key(KeyCode::Char('j'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Char('j'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Char('d'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().editor_mode() == Some("vi-operator-pending".to_string()))?;
    harness.send_key(KeyCode::Char('d'), KeyModifiers::NONE)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().editor_mode() == Some("vi-normal".to_string()))?;

    harness.send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)?;
    harness.render()?;
    harness.wait_until(|_| fs::read_to_string(&test_file).unwrap_or_default() == AFTER_DD_TEXT)?;
    harness.wait_until(|_| {
        fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("DIDSAVE")
    })?;
    harness.wait_until(|_| server_document(&log_file).as_deref() == Some(AFTER_DD_TEXT))?;

    assert_eq!(
        server_document(&log_file).as_deref(),
        Some(AFTER_DD_TEXT),
        "after saving, the server's copy of the document must match the buffer.\n\
         Log:\n{}",
        fs::read_to_string(&log_file).unwrap_or_default()
    );

    Ok(())
}

/// Save All must hand each server the text of the buffer it names.
///
/// It took the text from whichever buffer was focused, so saving two modified
/// files sent the focused one's contents under the other one's URI — the
/// report's second symptom, a diagnostic appearing in a file that never had
/// the offending code.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // fake server is a POSIX script
fn save_all_sends_each_buffer_its_own_text() -> anyhow::Result<()> {
    let (mut harness, _temp_dir, main_file, log_file) = lsp_harness(false)?;
    let project_root = main_file.parent().expect("file has a parent").to_path_buf();

    let alpha = project_root.join("alpha.rs");
    let beta = project_root.join("beta.rs");
    fs::write(&alpha, "fn alpha() {}\n")?;
    fs::write(&beta, "fn beta() {}\n")?;

    // alpha is edited first, so beta is the focused buffer at save time.
    harness.open_file(&alpha)?;
    harness.render()?;
    harness.type_text("// edit\n")?;
    harness.open_file(&beta)?;
    harness.render()?;
    harness.type_text("// edit\n")?;

    let (_saved, failed) = harness.editor_mut().save_all()?;
    assert_eq!(failed, 0, "no buffer should fail to save");

    let alpha_on_disk = fs::read_to_string(&alpha)?;
    let beta_on_disk = fs::read_to_string(&beta)?;
    assert!(
        alpha_on_disk.contains("// edit") && beta_on_disk.contains("// edit"),
        "both edits should have reached disk: {alpha_on_disk:?} / {beta_on_disk:?}"
    );
    assert_ne!(alpha_on_disk, beta_on_disk, "the two files must differ");

    harness.wait_until(|_| saved_text(&log_file, "alpha.rs").is_some())?;
    harness.wait_until(|_| saved_text(&log_file, "beta.rs").is_some())?;

    assert_eq!(
        saved_text(&log_file, "alpha.rs").as_deref(),
        Some(alpha_on_disk.as_str()),
        "the didSave for alpha.rs must carry alpha.rs's text.\nLog:\n{}",
        fs::read_to_string(&log_file).unwrap_or_default()
    );
    assert_eq!(
        saved_text(&log_file, "beta.rs").as_deref(),
        Some(beta_on_disk.as_str()),
        "the didSave for beta.rs must carry beta.rs's text.\nLog:\n{}",
        fs::read_to_string(&log_file).unwrap_or_default()
    );

    Ok(())
}
