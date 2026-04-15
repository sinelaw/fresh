//! Tests for issue #1573: "Format Buffer" with rustfmt hangs and doesn't format.
//!
//! Root cause (pre-fix): `Editor::run_formatter` wrote the entire buffer to
//! the formatter's stdin in a single `write_all`, then entered a polling
//! loop on `child.try_wait()` waiting for the process to exit. The stdout
//! pipe, however, was never drained during the wait, so any formatter
//! whose output exceeds the kernel's pipe buffer (64KB on Linux) fills
//! stdout, blocks writing, and never exits. We poll forever until the
//! timeout kills the process — the user's "hang and never format" symptom.
//!
//! Fix: stream stdin in a background thread and call `wait_with_output`,
//! which internally drains stdout/stderr in dedicated threads, so none of
//! the pipes can fill up.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, FormatterConfig, LanguageConfig};
use std::fs;
use std::os::unix::fs::PermissionsExt;

fn config_with_rust_formatter(script_path: &std::path::Path) -> Config {
    let mut config = Config::default();
    let entry = config
        .languages
        .entry("rust".to_string())
        .or_insert_with(LanguageConfig::default);
    entry.formatter = Some(FormatterConfig {
        command: script_path.display().to_string(),
        args: vec![],
        stdin: true,
        timeout_ms: 10_000,
    });
    config
}

/// Create a shell script formatter that reads all of stdin and emits a
/// fixed-size stdout. We write `output_line` `repeats` times so the test
/// can verify the buffer was replaced with the formatter's output without
/// depending on real rustfmt. When `repeats` is set large we exercise the
/// stdout-pipe deadlock that motivated the fix.
fn write_formatter_script(path: &std::path::Path, output_line: &str, repeats: usize) {
    let script = format!(
        "#!/bin/sh\ncat > /dev/null\nfor i in $(seq 1 {repeats}); do\n  printf '%s\\n' '{output_line}'\ndone\n",
    );
    fs::write(path, script).unwrap();
    let mut perm = fs::metadata(path).unwrap().permissions();
    perm.set_mode(0o755);
    fs::set_permissions(path, perm).unwrap();
}

fn run_format_buffer_via_palette(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("Format Buffer").unwrap();
    harness.wait_for_screen_contains("Format Buffer").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_prompt_closed().unwrap();
}

#[test]
fn test_issue_1573_format_buffer_applies_small_output() {
    let tmp = tempfile::TempDir::new().unwrap();
    let script = tmp.path().join("fmt.sh");
    write_formatter_script(&script, "formatted-line", 10);

    let rs_path = tmp.path().join("sample.rs");
    fs::write(&rs_path, "fn main(){let x=1;println!(\"{}\",x);}\n").unwrap();

    let config = config_with_rust_formatter(&script);
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&rs_path).unwrap();
    harness.render().unwrap();

    run_format_buffer_via_palette(&mut harness);

    let expected = "formatted-line\n".repeat(10);
    harness
        .wait_until(|h| h.get_buffer_content().as_deref() == Some(expected.as_str()))
        .unwrap();
}

#[test]
fn test_issue_1573_format_buffer_does_not_deadlock_on_large_output() {
    // A formatter that writes well past the 64KB pipe buffer on Linux.
    // Before the fix, the editor writes the whole buffer to stdin and
    // then polls `try_wait`; the formatter fills its stdout pipe and
    // blocks, so the editor hangs until the per-formatter 10-second
    // timeout kicks in and the format ultimately fails.
    let tmp = tempfile::TempDir::new().unwrap();
    let script = tmp.path().join("fmt.sh");
    // 200 KB of output: 4000 lines × 50 chars each.
    let line = "X".repeat(49);
    write_formatter_script(&script, &line, 4_000);

    let rs_path = tmp.path().join("sample.rs");
    fs::write(&rs_path, "fn main(){let x=1;println!(\"{}\",x);}\n").unwrap();

    let config = config_with_rust_formatter(&script);
    let mut harness = EditorTestHarness::with_config(80, 24, config).unwrap();
    harness.open_file(&rs_path).unwrap();
    harness.render().unwrap();

    let start = std::time::Instant::now();
    run_format_buffer_via_palette(&mut harness);

    // Must complete before the 10-second timeout would have fired. A
    // properly-plumbed formatter returns well under a second even for
    // hundreds of KB of output.
    let elapsed = start.elapsed();
    assert!(
        elapsed < std::time::Duration::from_secs(5),
        "Format Buffer deadlocked (took {elapsed:?}, well over the expected sub-second limit)",
    );

    // Buffer content must have been replaced with the formatter's output.
    let got = harness.get_buffer_content().unwrap_or_default();
    assert!(
        got.lines().count() >= 4_000,
        "Format Buffer did not apply the formatter's output \
         (expected >= 4000 lines, got {} chars / {} lines)",
        got.len(),
        got.lines().count(),
    );
    assert!(
        got.lines().next().unwrap_or("").starts_with(&line),
        "Format Buffer did not replace the buffer with the formatter's output; \
         first line was {:?}",
        got.lines().next().unwrap_or("")
    );
}
