//! Regression test for issue #1612: when a configured LSP server
//! crashes instantly, user-activity-triggered spawns (didChange, etc.)
//! bypass the `handle_server_crash` throttle and respawn the server on
//! every edit, flooding the log with repeated spawn-related warnings.
//!
//! This test configures a bash script that exits immediately (mimicking
//! a broken marksman binary from the bug report), opens a file so fresh
//! auto-starts the server, then pokes the editor repeatedly to drive
//! new `try_spawn`/`force_spawn` calls. The fix is expected to cap the
//! total number of spawns at `MAX_RESTARTS_IN_WINDOW` (= 5); before the
//! fix, this number is unbounded.
//!
//! The test is intentionally strict about the cap: we assert the spawn
//! count does not exceed the window maximum after driving many more
//! activity events than that. Without the fix, every edit triggers a
//! fresh spawn and the assertion trips.

use crate::common::harness::EditorTestHarness;
use std::time::Duration;

/// Write a crash-on-start fake LSP script. Each invocation appends a
/// line to `count_file` so the test can count how many times fresh
/// spawned the server. The script exits before responding to
/// `initialize`, producing the "broken pipe"/"closed stdout" pattern
/// reported in the issue.
fn write_crash_script(dir: &std::path::Path, count_file: &std::path::Path) -> std::path::PathBuf {
    let script = format!(
        r#"#!/bin/bash
echo "spawn $$" >> "{count}"
# Exit immediately — do not handshake. fresh will see EOF on stdout
# and/or broken pipe on stdin, mark the server crashed, and remove it
# from the handles list.
exit 1
"#,
        count = count_file.to_string_lossy()
    );

    let path = dir.join("crash_lsp.sh");
    std::fs::write(&path, script).expect("write crash script");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&path).unwrap().permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&path, perms).unwrap();
    }

    path
}

fn spawn_count(count_file: &std::path::Path) -> usize {
    std::fs::read_to_string(count_file)
        .map(|s| s.lines().filter(|l| !l.is_empty()).count())
        .unwrap_or(0)
}

/// When the configured LSP crashes instantly on every spawn, user
/// edits should not be able to drive unbounded respawns. The restart
/// tracker in `LspManager` caps auto-restarts at `MAX_RESTARTS_IN_WINDOW`
/// (= 5) inside a 180s window; this cap must apply to ALL spawn entry
/// points, not just the crash-handler path.
#[test]
#[cfg_attr(target_os = "windows", ignore)]
fn test_crash_loop_is_bounded() -> anyhow::Result<()> {
    crate::common::tracing::init_tracing_from_env();

    let temp = tempfile::tempdir()?;
    let count_file = temp.path().join("spawn_count.txt");
    let script = write_crash_script(temp.path(), &count_file);

    let file = temp.path().join("test.rs");
    std::fs::write(&file, "fn main() {}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script.to_string_lossy().to_string(),
            args: vec![],
            enabled: true,
            auto_start: true,
            process_limits: fresh::services::process_limits::ProcessLimits::default(),
            initialization_options: None,
            env: Default::default(),
            language_id_overrides: Default::default(),
            root_markers: Default::default(),
            name: Some("crash-lsp".to_string()),
            only_features: None,
            except_features: None,
        }]),
    );

    let mut harness =
        EditorTestHarness::with_config_and_working_dir(120, 30, config, temp.path().to_path_buf())?;

    harness.open_file(&file)?;
    harness.render()?;

    // Wait for the first spawn so the test isn't racing open_file.
    harness.wait_until(|_| spawn_count(&count_file) >= 1)?;

    // Drive many activity events. Each `type_text` call dispatches edit
    // events, which route through `send_lsp_changes_for_buffer` →
    // `try_spawn`, which (pre-fix) calls `force_spawn` unconditionally.
    // We allow real time between rounds so the spawned script has a
    // chance to exit and the crash handler to remove the handle,
    // enabling the next edit to trigger another spawn.
    //
    // We do MANY more rounds than the 5-spawn cap so that the test
    // proves the cap covers this path — not just that we got lucky with
    // timing.
    const ROUNDS: usize = 30;
    const CAP: usize = 5; // mirrors MAX_RESTARTS_IN_WINDOW in manager.rs

    for _ in 0..ROUNDS {
        harness.type_text("x")?;
        // Give the child a moment to exit and fresh to observe EOF and
        // clean up handles before the next edit goes through.
        harness.sleep(Duration::from_millis(100));
        // Also tick the editor so async messages (server-crash
        // notifications) are drained.
        harness.render()?;
    }

    // Allow any in-flight spawns to complete.
    harness.sleep(Duration::from_millis(500));
    harness.render()?;

    let n = spawn_count(&count_file);

    // Before the fix: n grows roughly linearly with ROUNDS.
    // After the fix: n is bounded by the restart cap.
    assert!(
        n <= CAP,
        "expected at most {CAP} spawns (restart cap), got {n}. \
         User activity is bypassing the LSP restart throttle — \
         regression of issue #1612."
    );

    Ok(())
}
