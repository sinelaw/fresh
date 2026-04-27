//! Reproducer for the dev-container LSP "Go to Definition" path-translation
//! bug.
//!
//! Background. Container authorities mount the host workspace into the
//! container at `remoteWorkspaceFolder` — typically not the same string
//! as the host workspace path. The editor's [`FilesystemSpec::Local`]
//! doc-comment claims paths "translate 1:1 between host and container",
//! and that's true for the *bytes* of the prefix once you cross the
//! mount, but it isn't true for the *path string itself*: a host
//! workspace at `/tmp/xxx/proj` shows up inside the container at
//! `/workspaces/proj`. The LSP server runs in the container, so URIs in
//! its outgoing `textDocument/*` traffic and incoming `Location`
//! responses use the container-side path. The editor lives on the host,
//! so its buffer file paths are host-side. There's currently no
//! translation between the two — that's the bug, and this test pins it.
//!
//! What this test does. It drives a real attach via the in-tree fake
//! devcontainer CLI, configures the Python LSP to point at
//! [`scripts/fake-lsp/bin/fake-pylsp`] (a bash stub that records every
//! URI it receives and answers `definition` with a configurable
//! `Location`), opens a host file, and triggers Go-to-Definition. The
//! fake LSP is set up to claim the definition lives at
//! `file:///workspaces/proj/util.py` — a *container* path that doesn't
//! exist on the host. The test then asserts both translation
//! directions:
//!
//!   * **host → container** (`didOpen` URI must use the container
//!     path the editor told docker to mount at, not the host path);
//!   * **container → host** (after the `Location` comes back, the
//!     active buffer's host path must resolve to `util.py` *on the
//!     host*, and the cursor must land at the line/character the LSP
//!     returned).
//!
//! The test is expected to fail today because the editor sends host
//! URIs in `didOpen` and opens the literal container URI from the
//! definition response. The fix lives in the URI-construction sites
//! around `app/types.rs:file_path_to_lsp_uri` and the inverse
//! `app/mod.rs:uri_to_path` — those need to know about the active
//! authority's host↔container path mapping.

#![cfg(all(unix, feature = "plugins"))]

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::{Path, PathBuf};

/// Path to the in-tree `fake-pylsp` bin dir. Tests prepend this to
/// PATH so `command -v fake-pylsp` resolves both on the host (for the
/// pre-attach phase) and inside the fake "container" (the captured
/// `userEnvProbe` PATH echoes this dir back through `docker exec
/// -e PATH=…`).
fn fake_lsp_bin_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../scripts/fake-lsp/bin")
        .canonicalize()
        .expect("scripts/fake-lsp/bin must exist relative to CARGO_MANIFEST_DIR")
}

/// Set up a workspace that triggers the dev-container popup and is
/// otherwise minimal. The python LSP config + fake-lsp wiring happens
/// at the harness layer; this just plants the source files and a
/// `.devcontainer/devcontainer.json`.
fn set_up_workspace() -> (tempfile::TempDir, PathBuf) {
    fresh::i18n::set_locale("en");

    let temp = tempfile::tempdir().unwrap();
    let workspace = temp.path().canonicalize().unwrap();

    let dc = workspace.join(".devcontainer");
    fs::create_dir_all(&dc).unwrap();
    fs::write(
        dc.join("devcontainer.json"),
        r#"{
            "name": "fake-lsp-go-to-def",
            "image": "mcr.microsoft.com/devcontainers/base:ubuntu",
            "remoteUser": "vscode",
            "userEnvProbe": "loginInteractiveShell"
        }"#,
    )
    .unwrap();

    // Distinct contents on each side so an accidental "open the same
    // file twice" doesn't smuggle a false positive past the cursor
    // assertion. `util.py` is intentionally long enough to host a
    // real line 5.
    fs::write(
        workspace.join("main.py"),
        "from util import helper\n\n\ndef main():\n    helper()\n",
    )
    .unwrap();
    fs::write(
        workspace.join("util.py"),
        "# util.py — host copy\n\
         # line 1\n\
         # line 2\n\
         # line 3\n\
         # line 4\n\
         def helper():\n\
         \treturn 'host-side definition target'\n",
    )
    .unwrap();

    let plugins_dir = workspace.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    copy_plugin(&plugins_dir, "devcontainer");

    (temp, workspace)
}

/// Wait until the devcontainer plugin's "Reopen in Container?" popup
/// is rendered. Mirrors `devcontainer_attach_e2e::wait_for_attach_popup`
/// but inlined here so the test file stays self-contained.
fn wait_for_attach_popup(harness: &mut EditorTestHarness) {
    bounded_wait(harness, "devcontainer plugin command registration", |h| {
        let reg = h.editor().command_registry().read().unwrap();
        reg.get_all().iter().any(|c| c.name == "%cmd.run_lifecycle")
    });
    harness.editor().fire_plugins_loaded_hook();
    bounded_wait(harness, "Reopen in Container popup", |h| {
        let screen = h.screen_to_string();
        screen.contains("Dev Container Detected") && screen.contains("Reopen in Container")
    });
}

fn bounded_wait<F>(harness: &mut EditorTestHarness, what: &str, mut cond: F)
where
    F: FnMut(&EditorTestHarness) -> bool,
{
    for _ in 0..200 {
        harness.tick_and_render().unwrap();
        if cond(harness) {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    panic!(
        "bounded_wait timed out: {what} not satisfied in 200 ticks (~10s).\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Promote the plugin-staged authority to active. Same idiom as the
/// existing devcontainer attach e2e — the harness has no main loop, so
/// the test does the `take_pending_authority → set_boot_authority`
/// swap inline.
fn wait_for_container_authority(harness: &mut EditorTestHarness) -> String {
    for _ in 0..200 {
        harness.tick_and_render().unwrap();
        if let Some(auth) = harness.editor_mut().take_pending_authority() {
            harness.editor_mut().set_boot_authority(auth);
            return harness.editor().authority().display_label.clone();
        }
        if harness
            .editor()
            .authority()
            .display_label
            .starts_with("Container:")
        {
            return harness.editor().authority().display_label.clone();
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        harness.advance_time(std::time::Duration::from_millis(50));
    }
    panic!(
        "container authority never landed.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

fn read_uri_log(state: &Path) -> String {
    fs::read_to_string(state.join("fake_lsp_uris")).unwrap_or_default()
}

/// Reproducer.
///
/// Today this test fails because:
///   * `didOpen` carries `file:///<host>/main.py` rather than
///     `file:///workspaces/proj/main.py`; and
///   * the `Location` URI `file:///workspaces/proj/util.py` is opened
///     verbatim as a host path, so either the open fails or the active
///     buffer ends up at the wrong host file (and the cursor land
///     position drifts).
///
/// Both directions need to be fixed for Go-to-Definition to work in a
/// devcontainer. Don't gut one side without exercising the other.
#[test]
fn goto_definition_translates_uris_between_host_and_container() {
    let (_workspace_temp, workspace) = set_up_workspace();
    let main_py = workspace.join("main.py");
    let host_util_py = workspace.join("util.py");

    // Configure python's LSP to use the `fake-pylsp` shim. We
    // deliberately use the bare command name (no path) so the lookup
    // goes through PATH inside the fake "container" — the same path
    // the production code uses via `docker exec sh -c 'command -v
    // pylsp'`.
    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "python".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "fake-pylsp".to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: vec![".devcontainer".to_string(), ".git".to_string()],
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_config(config)
            .with_fake_devcontainer()
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Pin the in-container workspace path so it diverges from the
    // host path. Set AFTER `with_fake_devcontainer()` acquires its
    // process-global mutex — setting before would race with parallel
    // tests' cleanup of the same env var.
    std::env::set_var("FAKE_DC_REMOTE_WORKSPACE", "/workspaces/proj");

    // Pre-state: the fake-lsp uri log must not exist yet (or be
    // empty). The fake-pylsp creates it on first message, so we
    // assert the symptom only after triggering Go-to-Definition.
    let state = harness
        .fake_devcontainer_state()
        .expect("with_fake_devcontainer was set")
        .to_path_buf();

    // Prepend the fake-lsp bin dir to the *host* PATH — the
    // pre-attach probe (`captureContainerLoginEnv` etc.) and the post-
    // attach `docker exec` both need to find `fake-pylsp`. With
    // `FAKE_DC_REMOTE_WORKSPACE=/workspaces/proj` and the host's PATH
    // injected through the fake docker shim, this is the simplest way
    // to make the binary reachable from "inside" the container.
    let fake_lsp_bin = fake_lsp_bin_dir();
    let host_path = std::env::var("PATH").unwrap_or_default();
    let already_on_path = host_path
        .split(':')
        .any(|p| Path::new(p) == fake_lsp_bin.as_path());
    if !already_on_path {
        std::env::set_var("PATH", format!("{}:{}", fake_lsp_bin.display(), host_path));
    }

    // Have `userEnvProbe` echo a PATH that includes the fake-lsp dir
    // so the in-container `command -v fake-pylsp` probe (and the
    // subsequent `docker exec -e PATH=… fake-pylsp` spawn) can
    // resolve the binary. The fake docker reuses the parent process's
    // env when no `-e` is passed, but the captured probe overrides
    // PATH on every spawn — so it *must* contain our dir.
    fs::write(
        state.join("probe_response"),
        format!(
            "PATH=/home/vscode/.local/bin:/usr/local/bin:/usr/bin:{}\n\
             HOME=/home/vscode\n\
             LANG=C.UTF-8\n",
            fake_lsp_bin.display()
        ),
    )
    .expect("write probe_response");

    // Pin the definition target the fake LSP returns. The URI uses
    // the *container* workspace path so the editor (on the host) has
    // to translate it back. Line 5 col 0 lands on `def helper():` in
    // util.py — far enough from line 0 that an off-by-N translation
    // bug shows up clearly.
    fs::write(
        state.join("fake_lsp_definition_uri"),
        "file:///workspaces/proj/util.py\n",
    )
    .expect("write fake_lsp_definition_uri");
    fs::write(state.join("fake_lsp_definition_line"), "5\n")
        .expect("write fake_lsp_definition_line");
    fs::write(state.join("fake_lsp_definition_character"), "0\n")
        .expect("write fake_lsp_definition_character");

    harness.tick_and_render().unwrap();

    // Drive the attach popup → "Reopen in Container" → wait for
    // authority. After this point, every spawn we drive (including
    // the LSP) routes through the container authority's
    // `DockerLongRunningSpawner`.
    wait_for_attach_popup(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let label = wait_for_container_authority(&mut harness);
    assert!(
        label.starts_with("Container:"),
        "expected container authority, got {label:?}"
    );

    // Open main.py — the LSP autostarts because `auto_start: true`
    // and the file extension matches the python language config.
    harness.open_file(&main_py).unwrap();

    // Wait for the LSP to handshake. The fake-pylsp logs every URI
    // it sees; an `initialize` line is the earliest signal that the
    // server is alive and the editor is talking to it.
    bounded_wait(&mut harness, "fake-pylsp initialize", |_| {
        let log = read_uri_log(&state);
        log.lines().any(|l| l.starts_with("initialize "))
    });

    // Wait for the editor to send `didOpen` for main.py before we
    // ask for a definition — without this the request races the
    // open notification.
    bounded_wait(&mut harness, "fake-pylsp didOpen", |_| {
        let log = read_uri_log(&state);
        log.lines()
            .any(|l| l.starts_with("didOpen ") && l.contains("main.py"))
    });

    // Move the cursor onto the `helper()` call inside `def main():`.
    // main.py contents:
    //   line 0: `from util import helper`
    //   line 1: ``
    //   line 2: ``
    //   line 3: `def main():`
    //   line 4: `    helper()`
    for _ in 0..4 {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    for _ in 0..6 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.process_async_and_render().unwrap();

    // Trigger Go-to-Definition. F12 is the conventional binding;
    // mirrors `lsp_goto_definition_readonly.rs`.
    harness
        .send_key(KeyCode::F(12), KeyModifiers::NONE)
        .unwrap();
    bounded_wait(&mut harness, "fake-pylsp definition request", |_| {
        let log = read_uri_log(&state);
        log.lines().any(|l| l.starts_with("definition "))
    });
    // Give the editor time to receive the response and act on it.
    for _ in 0..40 {
        harness.process_async_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(25));
        harness.advance_time(std::time::Duration::from_millis(25));
    }

    // ── Direction 1: host → container ────────────────────────────
    // The first `didOpen` URI the LSP saw must be the *container*
    // path (the workspace mounted at `/workspaces/proj`). The host
    // path leaks in today.
    let log = read_uri_log(&state);
    let did_open_lines: Vec<&str> = log.lines().filter(|l| l.starts_with("didOpen ")).collect();
    assert!(
        !did_open_lines.is_empty(),
        "expected at least one didOpen URI in the fake-lsp log; full log:\n{log}"
    );
    let first_did_open = did_open_lines[0];
    assert!(
        first_did_open.contains("file:///workspaces/proj/main.py"),
        "didOpen URI must use the in-container workspace path \
         `/workspaces/proj/main.py`, but got: {first_did_open:?}.\n\
         Full uri log:\n{log}\n\
         (This is the host→container URI translation gap: the editor \
          tells the LSP about a file at the host path, which the \
          in-container server can't see.)"
    );
    assert!(
        !first_did_open.contains(workspace.to_str().unwrap()),
        "didOpen URI must NOT carry the host workspace path; got: \
         {first_did_open:?}. Full uri log:\n{log}"
    );

    // ── Direction 2: container → host ────────────────────────────
    // The fake LSP returned `file:///workspaces/proj/util.py` line 5
    // col 0. After the editor processes the response, the active
    // buffer should be the host's `util.py`, and the cursor should
    // sit at line 5 col 0 — not line 0 (which is what an
    // open-empty-buffer fallback would give).
    let active_path: Option<PathBuf> = harness
        .editor()
        .active_state()
        .buffer
        .file_path()
        .map(|p| p.to_path_buf());
    assert_eq!(
        active_path.as_deref(),
        Some(host_util_py.as_path()),
        "after Go-to-Definition the active buffer's host path must be \
         the host's util.py, not the literal container path returned \
         by the LSP. Got: {active_path:?}. \
         (Container→host URI translation gap.)"
    );

    let cursor_pos = harness.cursor_position();
    let (line, character) = harness
        .editor()
        .active_state()
        .buffer
        .position_to_lsp_position(cursor_pos);
    assert_eq!(
        (line, character),
        (5, 0),
        "after Go-to-Definition the cursor must be at line 5, col 0 \
         (matching the fake LSP's response). A line:0 col:0 result \
         usually means the editor opened a fresh empty buffer for \
         the container URI instead of resolving it to the host file."
    );

    // Cleanup any per-test env that we set above so neighbouring
    // tests in the same process don't inherit it. The fake-devcontainer
    // mutex serializes us, so no race window.
    std::env::remove_var("FAKE_DC_REMOTE_WORKSPACE");
}

/// Common setup for the container-fetched-buffer tests below: planted
/// workspace, fake-pylsp on PATH, container authority attached, and
/// `main.py` opened so an LSP handshake has happened. Returns the
/// per-test state dir so callers can write `fake_lsp_definition_*`
/// pinning the response, and stash files under `container_fs/`.
fn arrange_attached_session_with_open_main_py() -> (
    tempfile::TempDir,
    std::path::PathBuf,
    EditorTestHarness,
    std::path::PathBuf,
) {
    let (workspace_temp, workspace) = set_up_workspace();
    let main_py = workspace.join("main.py");

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "python".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: "fake-pylsp".to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: vec![".devcontainer".to_string(), ".git".to_string()],
            name: None,
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness = EditorTestHarness::create(
        160,
        40,
        HarnessOptions::new()
            .with_working_dir(workspace.clone())
            .with_config(config)
            .with_fake_devcontainer()
            .without_empty_plugins_dir(),
    )
    .unwrap();

    // Set FAKE_DC_REMOTE_WORKSPACE *after* the fake-devcontainer
    // mutex is held (it's acquired inside `with_fake_devcontainer`).
    // Setting before would race with parallel tests' cleanup of the
    // same env var — they could clobber the value between our
    // `set_var` and the lock acquisition.
    std::env::set_var("FAKE_DC_REMOTE_WORKSPACE", "/workspaces/proj");

    let state = harness
        .fake_devcontainer_state()
        .expect("with_fake_devcontainer was set")
        .to_path_buf();

    let fake_lsp_bin = fake_lsp_bin_dir();
    let host_path = std::env::var("PATH").unwrap_or_default();
    if !host_path
        .split(':')
        .any(|p| Path::new(p) == fake_lsp_bin.as_path())
    {
        std::env::set_var("PATH", format!("{}:{}", fake_lsp_bin.display(), host_path));
    }
    fs::write(
        state.join("probe_response"),
        format!(
            "PATH=/home/vscode/.local/bin:/usr/local/bin:/usr/bin:{}\n\
             HOME=/home/vscode\n\
             LANG=C.UTF-8\n",
            fake_lsp_bin.display()
        ),
    )
    .expect("write probe_response");

    harness.tick_and_render().unwrap();

    wait_for_attach_popup(&mut harness);
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let label = wait_for_container_authority(&mut harness);
    assert!(
        label.starts_with("Container:"),
        "expected container authority, got {label:?}"
    );

    harness.open_file(&main_py).unwrap();
    bounded_wait(&mut harness, "fake-pylsp initialize", |_| {
        read_uri_log(&state)
            .lines()
            .any(|l| l.starts_with("initialize "))
    });
    bounded_wait(&mut harness, "fake-pylsp didOpen", |_| {
        read_uri_log(&state)
            .lines()
            .any(|l| l.starts_with("didOpen ") && l.contains("main.py"))
    });

    (workspace_temp, workspace, harness, state)
}

/// Pin a definition target the fake LSP will return for the next
/// `textDocument/definition` request.
fn pin_fake_lsp_definition(state: &Path, uri: &str, line: u32, character: u32) {
    fs::write(state.join("fake_lsp_definition_uri"), format!("{uri}\n"))
        .expect("write fake_lsp_definition_uri");
    fs::write(state.join("fake_lsp_definition_line"), format!("{line}\n"))
        .expect("write fake_lsp_definition_line");
    fs::write(
        state.join("fake_lsp_definition_character"),
        format!("{character}\n"),
    )
    .expect("write fake_lsp_definition_character");
}

/// Stash a file under `<state>/container_fs/<abs_container_path>` so
/// the fake docker shim's `cat` special-case can serve it. Mirrors
/// what `docker exec <id> cat <path>` would return for a real
/// container.
fn stash_container_file(state: &Path, container_path: &str, content: &str) {
    let stash = state.join("container_fs").join(
        container_path
            .strip_prefix('/')
            .expect("container_path must be absolute"),
    );
    fs::create_dir_all(stash.parent().expect("non-root container path")).unwrap();
    fs::write(&stash, content).unwrap_or_else(|e| panic!("stash {stash:?}: {e}"));
}

/// Run cursor down N times, right M times, then trigger Goto-Def.
fn trigger_goto_definition(harness: &mut EditorTestHarness, down: usize, right: usize) {
    for _ in 0..down {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    for _ in 0..right {
        harness
            .send_key(KeyCode::Right, KeyModifiers::NONE)
            .unwrap();
    }
    harness.process_async_and_render().unwrap();
    harness
        .send_key(KeyCode::F(12), KeyModifiers::NONE)
        .unwrap();
}

/// Settle the editor: pump async messages a few times to give the
/// goto-def response + any container-fetch round-trip time to land.
fn settle(harness: &mut EditorTestHarness) {
    for _ in 0..40 {
        harness.process_async_and_render().unwrap();
        std::thread::sleep(std::time::Duration::from_millis(25));
        harness.advance_time(std::time::Duration::from_millis(25));
    }
}

/// Reproducer: Goto-Definition into a *container-only* file (the
/// canonical case is jumping into `flask/app.py` from
/// `~/.local/lib/python3.12/site-packages/`, which only exists inside
/// the container's Python venv). The translation table doesn't help
/// here because the path isn't under the workspace mount.
///
/// Expected behaviour: the editor fetches the file's contents through
/// the active authority (`docker exec <id> cat <path>`) and opens the
/// result as a *read-only* buffer whose path is the in-container path.
/// Cursor lands at the LSP-reported line/column.
///
/// Currently fails because the editor calls `open_file` with the raw
/// container path, which the host filesystem can't see — the buffer
/// either errors out or ends up empty at the wrong location.
#[test]
fn goto_definition_into_container_only_file_opens_read_only_buffer() {
    let (_workspace_temp, _workspace, mut harness, state) =
        arrange_attached_session_with_open_main_py();

    // The container-only file the LSP will point at. Mirrors the
    // user-reported scenario verbatim (Flask installed into the
    // vscode user's local site-packages).
    let container_path = "/home/vscode/.local/lib/python3.12/site-packages/flask/app.py";
    let container_content = "# flask/app.py — fetched from container\n\
                             # line 1\n\
                             # line 2\n\
                             def some_app_helper():\n\
                             \treturn 'this content lives only in the container'\n";
    stash_container_file(&state, container_path, container_content);

    // Pin the LSP response. Line 3 is `def some_app_helper():` in
    // the stashed content — far from line 0 so we can tell an empty
    // fallback buffer apart from a real fetch.
    pin_fake_lsp_definition(&state, &format!("file://{container_path}"), 3, 0);

    // Trigger Goto-Def from main.py line 4 (the `helper()` call).
    trigger_goto_definition(&mut harness, 4, 6);
    bounded_wait(&mut harness, "fake-pylsp definition request", |_| {
        read_uri_log(&state)
            .lines()
            .any(|l| l.starts_with("definition "))
    });
    settle(&mut harness);

    // ── Container-fetched buffer assertions ──────────────────────
    // The active buffer's path is the container path verbatim — no
    // host translation possible because the file isn't under the
    // workspace mount. The buffer's contents are what we stashed,
    // proving the fetch went through the docker exec route. The
    // buffer is read-only because container-only files have no
    // host-side writeback path. The cursor lands where the LSP
    // pointed.
    let active_path: Option<PathBuf> = harness
        .editor()
        .active_state()
        .buffer
        .file_path()
        .map(|p| p.to_path_buf());
    assert_eq!(
        active_path.as_deref(),
        Some(Path::new(container_path)),
        "active buffer path must be the container path; got {active_path:?}"
    );

    let content = harness
        .editor()
        .active_state()
        .buffer
        .to_string()
        .expect("buffer content readable");
    assert_eq!(
        content, container_content,
        "active buffer content must match what `docker exec cat` returned"
    );

    assert!(
        harness.editor().is_active_buffer_read_only(),
        "container-fetched buffers must be read-only (no host-side writeback)"
    );

    let cursor_pos = harness.cursor_position();
    let (line, character) = harness
        .editor()
        .active_state()
        .buffer
        .position_to_lsp_position(cursor_pos);
    assert_eq!(
        (line, character),
        (3, 0),
        "cursor must land at the LSP-reported line/character. (0,0) \
         usually means the editor created an empty buffer instead of \
         fetching the container file."
    );

    std::env::remove_var("FAKE_DC_REMOTE_WORKSPACE");
}

/// Reproducer for the failure mode: Goto-Def returns a URI that
/// resolves to neither a host-visible file nor a container-readable
/// one (e.g. a stale path the LSP cached, or a typo in a server's
/// own location math). The editor must surface a user-visible error
/// instead of silently opening a phantom buffer.
///
/// Currently fails because the editor calls `open_file` on the
/// untranslatable path and either errors silently or opens an empty
/// buffer.
#[test]
fn goto_definition_to_unreachable_uri_surfaces_error_message() {
    let (_workspace_temp, _workspace, mut harness, state) =
        arrange_attached_session_with_open_main_py();

    // Path doesn't exist on host; we deliberately *don't* stash it
    // under `container_fs/`, so the fake docker `cat` falls through
    // to real `cat` and exits 1.
    let unreachable = "/this/path/exists/nowhere/ghost.py";
    pin_fake_lsp_definition(&state, &format!("file://{unreachable}"), 7, 0);

    trigger_goto_definition(&mut harness, 4, 6);
    bounded_wait(&mut harness, "fake-pylsp definition request", |_| {
        read_uri_log(&state)
            .lines()
            .any(|l| l.starts_with("definition "))
    });
    settle(&mut harness);

    // The active buffer must NOT be a phantom at the unreachable
    // path. The most likely "bad" outcome is the editor opening an
    // empty buffer at `/this/path/exists/nowhere/ghost.py` (visible
    // as the active buffer's path matching the unreachable string),
    // which we explicitly forbid.
    let active_path: Option<PathBuf> = harness
        .editor()
        .active_state()
        .buffer
        .file_path()
        .map(|p| p.to_path_buf());
    assert_ne!(
        active_path.as_deref(),
        Some(Path::new(unreachable)),
        "Goto-Def into an unreachable URI must NOT open a phantom \
         buffer at that path. Got: {active_path:?}"
    );

    // The status line should mention the failure. Observation via
    // the rendered screen (per CONTRIBUTING §2). The status bar
    // truncates with `...` once it runs out of room, so we look for
    // a stable prefix that the renderer will keep — "could not open"
    // is unambiguous and short enough to fit alongside the
    // filename / cursor / mode segments at our 160-col harness.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("could not open"),
        "status line should surface the failure so the user knows the \
         goto-def didn't navigate. Screen:\n{screen}"
    );

    std::env::remove_var("FAKE_DC_REMOTE_WORKSPACE");
}
