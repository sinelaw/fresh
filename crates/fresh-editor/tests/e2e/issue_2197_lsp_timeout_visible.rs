//! Tests for issue #2197: LSP requests time out silently.
//!
//! A server can complete `initialize` — so the editor reports it ready —
//! and then answer nothing at all. That is what pyright 1.1.408 does when
//! the client advertises `workspace.workspaceFolders` (microsoft/pyright#11239,
//! fixed in 1.1.409). Fresh cancels each request after 30s
//! (`$/cancelRequest`) and moves on, which is the right thing to do, but it
//! did so entirely silently: hover showed nothing, F12 said "No definition
//! found", and the status bar kept reading `LSP (python) ready` the whole
//! time.
//!
//! The server-side hang is not Fresh's bug; the invisibility was. These
//! tests pin the surfacing: every timeout is reported on the status bar,
//! and a server whose requests keep expiring stops being rendered as "on".
//!
//! The last test pins the compatibility push that happens to unblock that
//! pyright: a `workspace/didChangeConfiguration` right after `initialized`,
//! which VS Code, Neovim and Helix all send and servers have come to rely on.

use crate::common::harness::EditorTestHarness;
use fresh::services::async_bridge::{AsyncMessage, LspServerStatus};
use std::time::Duration;

/// Push a message onto the editor's async bridge and let the editor drain
/// it — the same path the LSP task's messages arrive on.
fn deliver(harness: &mut EditorTestHarness, message: AsyncMessage) {
    harness
        .editor()
        .async_bridge()
        .expect("harness editor has an async bridge")
        .sender()
        .send(message)
        .expect("bridge receiver is alive");
    harness.editor_mut().process_async_messages();
}

fn timeout_message(method: &str, consecutive: u32) -> AsyncMessage {
    AsyncMessage::LspRequestTimeout {
        language: "python".to_string(),
        server_name: "pyright-langserver".to_string(),
        method: method.to_string(),
        timeout: Duration::from_secs(30),
        consecutive,
    }
}

#[test]
fn test_issue_2197_request_timeout_is_reported_on_the_status_bar() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness.render()?;

    deliver(&mut harness, timeout_message("textDocument/hover", 1));
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("timed out") && status.contains("textDocument/hover"),
        "a 30s request timeout must reach the user, naming the request that \
         expired; got {status:?}",
    );
    assert!(
        status.contains("30s"),
        "the message should say how long the request waited; got {status:?}",
    );
    // …and it must actually be on screen, not just in the editor's state.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("textDocument/hover"),
        "the timeout must be rendered on the status bar\nScreen:\n{screen}",
    );

    // A second timeout in a row says the server has stopped answering.
    deliver(&mut harness, timeout_message("textDocument/definition", 2));
    harness.render()?;
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("not responding"),
        "repeated timeouts should say the server is not responding; got {status:?}",
    );

    Ok(())
}

#[test]
fn test_issue_2197_unresponsive_server_is_not_rendered_as_ready() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;

    // The server came up fine: this is exactly the state the bug report
    // describes — "ready" while nothing works.
    deliver(
        &mut harness,
        AsyncMessage::LspStatusUpdate {
            language: "python".to_string(),
            server_name: "pyright-langserver".to_string(),
            status: LspServerStatus::Running,
            message: None,
        },
    );
    harness.render()?;
    assert!(
        harness.screen_to_string().contains("LSP (on)"),
        "pre-condition: a running server renders as on\nScreen:\n{}",
        harness.screen_to_string(),
    );

    // Requests start expiring. The second one in a row is what the LSP
    // task reports as `Unresponsive`.
    deliver(&mut harness, timeout_message("textDocument/diagnostic", 1));
    deliver(&mut harness, timeout_message("textDocument/hover", 2));
    deliver(
        &mut harness,
        AsyncMessage::LspStatusUpdate {
            language: "python".to_string(),
            server_name: "pyright-langserver".to_string(),
            status: LspServerStatus::Unresponsive,
            message: None,
        },
    );
    harness.render()?;

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("LSP (on)"),
        "BUG #2197: the indicator still claims the server is on while every \
         request times out\nScreen:\n{screen}",
    );
    assert!(
        screen.contains("LSP (stuck)"),
        "the indicator should say the server is not answering\nScreen:\n{screen}",
    );

    // Recovery: the LSP task reports Running again on the first answered
    // request, and the indicator must go back to normal.
    deliver(
        &mut harness,
        AsyncMessage::LspStatusUpdate {
            language: "python".to_string(),
            server_name: "pyright-langserver".to_string(),
            status: LspServerStatus::Running,
            message: None,
        },
    );
    harness.render()?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("LSP (on)") && !screen.contains("LSP (stuck)"),
        "an answered request should clear the stuck state\nScreen:\n{screen}",
    );

    Ok(())
}

/// The symptom users actually hit: `F12` on a server that never answers
/// reported "No definition found" — indistinguishable from a symbol that
/// genuinely has no definition. An expired request has to say so.
#[test]
fn test_issue_2197_empty_result_after_a_timeout_says_the_request_expired() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness.render()?;

    // A go-to-definition request is outstanding…
    harness
        .editor_mut()
        .active_window_mut()
        .pending_goto_definition_request = Some(7);
    // …it expires…
    deliver(&mut harness, timeout_message("textDocument/definition", 1));
    // …and the editor is handed the empty result the expiry produces.
    deliver(
        &mut harness,
        AsyncMessage::LspGotoDefinition {
            request_id: 7,
            locations: Vec::new(),
        },
    );
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        !status.contains("No definition found"),
        "BUG #2197: an unanswered request is reported as if the symbol had no \
         definition; got {status:?}",
    );
    assert!(
        status.contains("timed out") && status.contains("textDocument/definition"),
        "the message should say the request expired; got {status:?}",
    );

    Ok(())
}

/// The same message must not hijack a genuinely empty result later on: the
/// timeout only explains results that arrive right after it.
#[test]
fn test_issue_2197_stale_timeout_does_not_explain_later_empty_results() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness.render()?;

    deliver(&mut harness, timeout_message("textDocument/definition", 1));

    // Age the recorded timeout past the window in which it can explain an
    // empty result.
    {
        let window = harness.editor_mut().active_window_mut();
        let record = window
            .lsp_request_timeouts
            .get_mut(&("python".to_string(), "textDocument/definition".to_string()))
            .expect("timeout was recorded");
        record.at = std::time::Instant::now() - Duration::from_secs(120);
    }

    harness
        .editor_mut()
        .active_window_mut()
        .pending_goto_definition_request = Some(9);
    deliver(
        &mut harness,
        AsyncMessage::LspGotoDefinition {
            request_id: 9,
            locations: Vec::new(),
        },
    );
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("No definition found"),
        "a genuinely empty answer should still read as one; got {status:?}",
    );

    Ok(())
}

/// The editor issues inlay-hint, diagnostic, semantic-token and folding
/// requests on its own, several per edit. Announcing each expiry would
/// overwrite whatever the user was reading every 30s for as long as a stuck
/// server stayed stuck, so background traffic stays off the status bar —
/// the indicator carries that news instead.
#[test]
fn test_issue_2197_background_request_timeouts_do_not_take_the_status_bar() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness
        .editor_mut()
        .set_status_message("Saved main.py".to_string());
    harness.render()?;

    for method in [
        "textDocument/inlayHint",
        "textDocument/diagnostic",
        "textDocument/semanticTokens/range",
        "textDocument/foldingRange",
    ] {
        deliver(&mut harness, timeout_message(method, 1));
    }
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert_eq!(
        status, "Saved main.py",
        "background request timeouts must not overwrite what the user was reading",
    );

    // A request the user made does reach the status bar.
    deliver(&mut harness, timeout_message("textDocument/hover", 1));
    harness.render()?;
    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("textDocument/hover"),
        "a user-invoked request that expires is still reported; got {status:?}",
    );

    Ok(())
}

/// An expiry explains the request it belongs to and nothing else: a
/// background inlay-hint timeout must not turn a definition the server
/// answered correctly (and emptily) into "no answer".
#[test]
fn test_issue_2197_a_timeout_only_explains_its_own_method() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness.render()?;

    deliver(&mut harness, timeout_message("textDocument/inlayHint", 1));

    harness
        .editor_mut()
        .active_window_mut()
        .pending_goto_definition_request = Some(11);
    deliver(
        &mut harness,
        AsyncMessage::LspGotoDefinition {
            request_id: 11,
            locations: Vec::new(),
        },
    );
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("No definition found"),
        "a definition the server answered promptly is not explained by an \
         unrelated background timeout; got {status:?}",
    );

    Ok(())
}

/// One expiry explains one empty result. The record is consumed, so a
/// second empty answer — this time a real one — reads as a real one.
#[test]
fn test_issue_2197_an_expiry_explains_only_the_next_empty_result() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness.render()?;

    deliver(&mut harness, timeout_message("textDocument/definition", 1));

    for (request_id, expected) in [(21u64, "timed out"), (22u64, "No definition found")] {
        harness
            .editor_mut()
            .active_window_mut()
            .pending_goto_definition_request = Some(request_id);
        deliver(
            &mut harness,
            AsyncMessage::LspGotoDefinition {
                request_id,
                locations: Vec::new(),
            },
        );
        harness.render()?;
        let status = harness
            .editor()
            .get_status_message()
            .cloned()
            .unwrap_or_default();
        assert!(
            status.contains(expected),
            "expected {expected:?} for request {request_id}; got {status:?}",
        );
    }

    Ok(())
}

/// A server that starts answering again must not keep explaining empty
/// results with expiries from when it was quiet.
#[test]
fn test_issue_2197_recovery_forgets_earlier_expiries() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness.render()?;

    deliver(&mut harness, timeout_message("textDocument/definition", 2));
    deliver(
        &mut harness,
        AsyncMessage::LspStatusUpdate {
            language: "python".to_string(),
            server_name: "pyright-langserver".to_string(),
            status: LspServerStatus::Running,
            message: None,
        },
    );

    harness
        .editor_mut()
        .active_window_mut()
        .pending_goto_definition_request = Some(31);
    deliver(
        &mut harness,
        AsyncMessage::LspGotoDefinition {
            request_id: 31,
            locations: Vec::new(),
        },
    );
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        status.contains("No definition found"),
        "after the server answered again, an empty result is just empty; got {status:?}",
    );

    Ok(())
}

/// A server that goes quiet and then dies must still be treated as a crash:
/// `Unresponsive` is the state most likely to precede a death, and losing the
/// crash path would leave the dead server's diagnostics on screen with no
/// auto-restart.
#[test]
fn test_issue_2197_a_stuck_server_that_dies_is_still_a_crash() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;

    for status in [LspServerStatus::Running, LspServerStatus::Unresponsive] {
        deliver(
            &mut harness,
            AsyncMessage::LspStatusUpdate {
                language: "python".to_string(),
                server_name: "pyright-langserver".to_string(),
                status,
                message: None,
            },
        );
    }
    harness.editor_mut().set_status_message(String::new());

    // The wedged server now dies.
    deliver(
        &mut harness,
        AsyncMessage::LspStatusUpdate {
            language: "python".to_string(),
            server_name: "pyright-langserver".to_string(),
            status: LspServerStatus::Error,
            message: None,
        },
    );
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert!(
        !status.is_empty(),
        "a server dying after going quiet must be handled as a crash (restart \
         and a message), not silently; status was empty",
    );

    Ok(())
}

/// Completion and signature help are issued by the editor on ordinary typing,
/// so their expiries must not take the status bar either — against a wedged
/// server, typing a line would otherwise queue one message per keystroke.
#[test]
fn test_issue_2197_auto_triggered_completion_timeouts_stay_off_the_status_bar() -> anyhow::Result<()>
{
    let temp_dir = tempfile::tempdir()?;
    let file = temp_dir.path().join("main.py");
    std::fs::write(&file, "def add(a, b):\n    return a + b\n")?;

    let mut harness = EditorTestHarness::new(120, 30)?;
    harness.open_file(&file)?;
    harness
        .editor_mut()
        .set_status_message("Saved main.py".to_string());
    harness.render()?;

    for method in ["textDocument/completion", "textDocument/signatureHelp"] {
        deliver(&mut harness, timeout_message(method, 1));
    }
    harness.render()?;

    let status = harness
        .editor()
        .get_status_message()
        .cloned()
        .unwrap_or_default();
    assert_eq!(
        status, "Saved main.py",
        "editor-issued completion traffic must not claim the status bar",
    );

    Ok(())
}

/// The handshake pushes the settings once after `initialized`, before any
/// document is opened, the way VS Code (`synchronize.configurationSection`),
/// Neovim (`settings`) and Helix (`config`) do. pyright 1.1.408 never
/// finished setting up its workspace without it; other servers merely expect
/// it. The logging fake server records every method it receives, in order.
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Uses Bash-based fake LSP server
fn test_issue_2197_settings_are_pushed_right_after_initialized() -> anyhow::Result<()> {
    use crate::common::fake_lsp::FakeLspServer;

    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_with_logging(temp_dir.path())?;
    let log_file = temp_dir.path().join("handshake_log.txt");
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::logging_script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
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

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;
    harness.open_file(&test_file)?;
    harness.render()?;

    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("textDocument/didOpen")
    })?;

    let methods: Vec<String> = std::fs::read_to_string(&log_file)?
        .lines()
        .map(str::to_string)
        .collect();
    let position = |method: &str| {
        methods
            .iter()
            .position(|m| m == method)
            .unwrap_or_else(|| panic!("{method} was never sent; handshake was {methods:?}"))
    };

    assert!(
        position("initialize") < position("initialized"),
        "handshake order: {methods:?}"
    );
    assert!(
        position("initialized") < position("workspace/didChangeConfiguration"),
        "the settings push follows `initialized`: {methods:?}"
    );
    assert!(
        position("workspace/didChangeConfiguration") < position("textDocument/didOpen"),
        "the settings push precedes the first document: {methods:?}"
    );
    assert_eq!(
        methods
            .iter()
            .filter(|m| m.as_str() == "workspace/didChangeConfiguration")
            .count(),
        1,
        "pushed exactly once: {methods:?}"
    );

    Ok(())
}
