//! E2E coverage for plugin-claimed LSP *client commands*.
//!
//! A server may return a `Command` it does not execute itself — anything
//! absent from its `executeCommandProvider` list is the client's to
//! interpret — and LSP says nothing about what such a command means. The
//! core therefore interprets none of them: a plugin claims the name with
//! `registerLspClientCommands` and handles it on the `lsp_execute_command`
//! hook, and only an *unclaimed* name goes back to the server as
//! `workspace/executeCommand`.
//!
//! These tests pin both halves of that fork, because getting it wrong is
//! silent in opposite directions: a claimed command sent to the server is
//! rejected by a server that never offered it, and an unclaimed command
//! swallowed by the core never runs at all.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::time::Duration;

/// A fake LSP server whose single code lens carries `command_json` (an LSP
/// `Command` object), logging every method it saw to `$1`.
fn create_lsp_script_with_command(dir: &std::path::Path, command_json: &str) -> std::path::PathBuf {
    let script = format!(
        r##"#!/bin/bash
LOG_FILE="$1"
> "$LOG_FILE"

read_message() {{
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
}}

send_message() {{
    local message="$1"
    local length=${{#message}}
    printf "Content-Length: $length\r\n\r\n%s" "$message"
}}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then break; fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | cut -d':' -f2)
    echo "METHOD:$method" >> "$LOG_FILE"
    echo "BODY:$msg" >> "$LOG_FILE"
    case "$method" in
        "initialize")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":{{"capabilities":{{"textDocumentSync":2,"codeLensProvider":{{"resolveProvider":false}}}}}}}}'
            ;;
        "textDocument/codeLens")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":[{{"range":{{"start":{{"line":0,"character":0}},"end":{{"line":0,"character":0}}}},"command":{command}}}]}}'
            ;;
        "workspace/executeCommand")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":null}}'
            ;;
        "shutdown")
            send_message '{{"jsonrpc":"2.0","id":'$msg_id',"result":null}}'
            break
            ;;
    esac
done
"##,
        command = command_json
    );

    let script_path = dir.join("fake_client_command_lsp.sh");
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

/// A fake server whose lens carries `command_name` with a token payload.
fn create_lsp_script(dir: &std::path::Path, command_name: &str) -> std::path::PathBuf {
    create_lsp_script_with_command(
        dir,
        &format!(
            r#"{{"title":"Run Lens","command":"{command_name}","arguments":[{{"tag":"payload"}}]}}"#
        ),
    )
}

/// A fake server whose lens is a rust-analyzer runnable, as the real server
/// sends: `rust-analyzer.runSingle` carrying one `Runnable` argument.
fn create_runnable_lsp_script(
    dir: &std::path::Path,
    runnable: &serde_json::Value,
) -> std::path::PathBuf {
    create_lsp_script_with_command(
        dir,
        &format!(
            r#"{{"title":"Run Lens","command":"rust-analyzer.runSingle","arguments":[{runnable}]}}"#
        ),
    )
}

fn lsp_config(script_path: &std::path::Path, log_file: &std::path::Path) -> fresh::config::Config {
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
    config
}

/// Load a plugin file through the "Load Plugin from Buffer" palette action.
/// Mirrors `terminal_hooks.rs`.
fn load_plugin(harness: &mut EditorTestHarness, source: &str, file_name: &str) {
    let project_dir = harness.project_dir().unwrap();
    let plugin_file = project_dir.join(file_name);
    std::fs::write(&plugin_file, source).unwrap();
    harness.open_file(&plugin_file).unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.type_text("Load Plugin from Buffer").unwrap();
    for _ in 0..3 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    for _ in 0..10 {
        harness.process_async_and_render().unwrap();
        harness.sleep(Duration::from_millis(50));
    }
    harness.assert_no_plugin_errors();
}

/// The lens's command name, claimed by the plugin loaded below.
const CLAIMED_COMMAND: &str = "demo-server.runThing";
/// A command no plugin claims, so it must reach the server.
const UNCLAIMED_COMMAND: &str = "demo-server.serverSide";

fn claiming_plugin(receipt_path: &std::path::Path) -> String {
    format!(
        r#"
const editor = getEditor();
editor.registerLspClientCommands(["{command}"]);
editor.on("lsp_execute_command", (data) => {{
  if (data.command !== "{command}") return;
  editor.writeFile(
    "{receipt}",
    JSON.stringify({{
      command: data.command,
      args: data.arguments,
      title: data.title,
      language: data.language,
    }})
  );
}});
"#,
        command = CLAIMED_COMMAND,
        receipt = receipt_path.to_string_lossy(),
    )
}

/// Open `test.rs` against the fake server and wait for the lens to render.
fn open_with_lens(
    harness: &mut EditorTestHarness,
    dir: &std::path::Path,
) -> anyhow::Result<std::path::PathBuf> {
    let test_file = dir.join("test.rs");
    std::fs::write(&test_file, "fn main() {}\n")?;
    harness.open_file(&test_file)?;
    harness.render()?;
    harness.wait_until(|h| h.editor().active_window().is_lsp_server_ready("rust"))?;
    harness.wait_for_screen_contains("Run Lens")?;
    Ok(test_file)
}

/// Click the lens where it is drawn on screen.
fn click_lens(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    let screen = harness.screen_to_string();
    let (row, col) = screen
        .lines()
        .enumerate()
        .find_map(|(row, line)| {
            line.find("Run Lens").map(|byte| {
                let col = unicode_width::UnicodeWidthStr::width(&line[..byte]);
                (row as u16, col as u16)
            })
        })
        .unwrap_or_else(|| panic!("lens must be drawn on screen:\n{screen}"));
    harness.mouse_click(col, row)?;
    Ok(())
}

#[test]
#[cfg_attr(windows, ignore = "uses a Bash fake LSP server")]
fn test_claimed_client_command_goes_to_the_plugin_not_the_server() -> anyhow::Result<()> {
    let script_dir = tempfile::tempdir()?;
    let log_file = script_dir.path().join("lsp.log");
    let script_path = create_lsp_script(script_dir.path(), CLAIMED_COMMAND);
    let receipt = script_dir.path().join("receipt.json");

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_project_root()
            .with_config(lsp_config(&script_path, &log_file)),
    )?;

    // Claim before the lens is ever run, as a plugin does at load time.
    load_plugin(&mut harness, &claiming_plugin(&receipt), "claimer.ts");

    let project_dir = harness.project_dir().unwrap();
    open_with_lens(&mut harness, &project_dir)?;
    click_lens(&mut harness)?;

    harness.wait_until(|_| receipt.exists())?;
    let recorded: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&receipt)?).expect("receipt is JSON");

    assert_eq!(recorded["command"], CLAIMED_COMMAND);
    assert_eq!(
        recorded["title"], "Run Lens",
        "the lens title rides along so the plugin can name it in messages"
    );
    assert_eq!(recorded["language"], "rust");
    // Arguments are handed over as the server's own JSON, untouched.
    let args: serde_json::Value =
        serde_json::from_str(recorded["args"].as_str().expect("args is a JSON string"))
            .expect("args parses");
    assert_eq!(args, serde_json::json!([{"tag": "payload"}]));

    // The whole point of claiming: the server never sees it. It never
    // offered to run this command, so sending it would only be rejected.
    let log = std::fs::read_to_string(&log_file).unwrap_or_default();
    assert!(
        !log.contains("METHOD:workspace/executeCommand"),
        "a claimed command must not be sent to the server, but the log shows:\n{log}"
    );

    Ok(())
}

#[test]
#[cfg_attr(windows, ignore = "uses a Bash fake LSP server")]
fn test_unclaimed_client_command_still_goes_to_the_server() -> anyhow::Result<()> {
    let script_dir = tempfile::tempdir()?;
    let log_file = script_dir.path().join("lsp.log");
    let script_path = create_lsp_script(script_dir.path(), UNCLAIMED_COMMAND);
    let receipt = script_dir.path().join("receipt.json");

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_project_root()
            .with_config(lsp_config(&script_path, &log_file)),
    )?;

    // A plugin is loaded and claims a *different* command, so the registry
    // is non-empty: this pins the fork on the name, not on "any claims at all".
    load_plugin(&mut harness, &claiming_plugin(&receipt), "claimer.ts");

    let project_dir = harness.project_dir().unwrap();
    open_with_lens(&mut harness, &project_dir)?;
    click_lens(&mut harness)?;

    harness.wait_until(|_| {
        let log = std::fs::read_to_string(&log_file).unwrap_or_default();
        log.contains("METHOD:workspace/executeCommand")
            && log.contains(&format!("\"command\":\"{UNCLAIMED_COMMAND}\""))
    })?;

    assert!(
        !receipt.exists(),
        "the plugin must not receive a command it did not claim"
    );

    Ok(())
}

/// The real `rust-lsp` plugin translating a rust-analyzer `Runnable` into an
/// argv, end to end.
///
/// This replaces unit tests that used to sit in `app::lsp_requests` when the
/// translation lived in the core. The interesting cases are all here:
/// `overrideCargo` is a command *line* (so it may be several words), `--`
/// separates only when there are executable args, and the cwd is the
/// workspace root when rust-analyzer names one. `overrideCargo` points at a
/// recorder script so the assertion is the exact argv the terminal spawned
/// with, not a proxy for it.
#[test]
#[cfg_attr(windows, ignore = "uses Bash for the fake server and the recorder")]
fn test_rust_analyzer_runnable_is_translated_to_an_argv() -> anyhow::Result<()> {
    use portable_pty::{native_pty_system, PtySize};

    if native_pty_system()
        .openpty(PtySize {
            rows: 1,
            cols: 1,
            pixel_width: 0,
            pixel_height: 0,
        })
        .is_err()
    {
        eprintln!("Skipping: PTY unavailable in this environment");
        return Ok(());
    }

    let dir = tempfile::tempdir()?;
    let log_file = dir.path().join("lsp.log");
    let argv_file = dir.path().join("argv.txt");

    // Stands in for `cargo`: records the argv it was called with, verbatim.
    let recorder = dir.path().join("recorder.sh");
    std::fs::write(
        &recorder,
        format!(
            "#!/bin/bash\nprintf '%s\\n' \"$@\" > {}\n",
            argv_file.to_string_lossy()
        ),
    )?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&recorder)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&recorder, perms)?;
    }

    // A server returning a rust-analyzer runnable lens. `overrideCargo` is
    // two words on purpose: the recorder plus a flag, the way a real
    // `cargo +nightly` override is.
    let workspace_root = dir.path().join("workspace");
    std::fs::create_dir_all(&workspace_root)?;
    let runnable = serde_json::json!({
        "label": "test mymod::works",
        "kind": "cargo",
        "args": {
            "environment": {},
            "cwd": workspace_root.to_string_lossy(),
            "workspaceRoot": workspace_root.to_string_lossy(),
            "overrideCargo": format!("{} --recorder-flag", recorder.to_string_lossy()),
            "cargoArgs": ["test", "--lib"],
            "executableArgs": ["mymod::works", "--exact"]
        }
    });
    let script_path = create_runnable_lsp_script(dir.path(), &runnable);

    let mut harness = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_project_root()
            // The real embedded `rust-lsp.ts` is what claims the command and
            // does the translation, so it has to actually load.
            .with_forced_embedded_plugins()
            .with_config(lsp_config(&script_path, &log_file)),
    )?;

    let project_dir = harness.project_dir().unwrap();
    open_with_lens(&mut harness, &project_dir)?;
    click_lens(&mut harness)?;

    harness.wait_until(|_| argv_file.exists() && !std::fs::read(&argv_file).unwrap().is_empty())?;
    let argv: Vec<String> = std::fs::read_to_string(&argv_file)?
        .lines()
        .map(str::to_string)
        .collect();

    assert_eq!(
        argv,
        vec![
            // the override's trailing words survive as separate argv entries
            "--recorder-flag",
            "test",
            "--lib",
            // `--` appears exactly once, before the executable args
            "--",
            "mymod::works",
            "--exact",
        ],
        "runnable must flatten to this argv"
    );

    Ok(())
}
