//! A plugin timer firing while a macro is being recorded must not land in
//! the macro.
//!
//! `editor.setInterval` handlers run through the same action dispatch a
//! keypress uses, which used to append each tick to the in-flight recording:
//! "Macro: Save to init.ts" then wrote steps like
//! `{ action: "liveSessionsTick" }` between the keys the user actually typed.
//! A tick is the plugin's doing, not the user's, so it has no place in a
//! replayable record of their input.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config_io::DirectoryContext;
use std::fs;
use std::time::Duration;

/// The number in a `<prefix>=<n>` marker on the status line, if one is shown.
fn marker_count(harness: &EditorTestHarness, prefix: &str) -> Option<u32> {
    let line = harness.get_status_bar();
    let rest = line.split(prefix).nth(1)?;
    let digits: String = rest.chars().take_while(|c| c.is_ascii_digit()).collect();
    digits.parse().ok()
}

#[test]
fn plugin_timer_ticks_are_not_recorded_into_a_macro() {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let mut harness = EditorTestHarness::with_shared_dir_context(
        120,
        24,
        Default::default(),
        working_dir,
        dir_context,
    )
    .expect("harness");

    fs::write(
        config_dir.join("init.ts"),
        r#"
        const editor = getEditor();
        let ticks = 0;
        registerHandler("macro_probe_tick", function () {
            ticks += 1;
            editor.setStatus("TICKS=" + ticks);
        });
        editor.setInterval(10, "macro_probe_tick");

        registerHandler("macro_probe_report", function () {
            // Counts, not the step list: the status bar elides a long line.
            const steps = editor.getMacro("0") || [];
            const count = function (name) {
                return steps.filter(function (s) { return s.action === name; }).length;
            };
            editor.setStatus("TICKS=" + ticks +
                " TICK_STEPS=" + count("macro_probe_tick") +
                " CHAR_STEPS=" + count("insert_char"));
        });
        editor.registerCommand("Macro Probe Report", "report register 0",
                               "macro_probe_report", null);
        "#,
    )
    .unwrap();
    harness.editor_mut().load_init_script(true);

    harness
        .wait_until(|h| marker_count(h, "TICKS=").unwrap_or(0) >= 1)
        .expect("the timer should start ticking");
    let ticks_before = marker_count(&harness, "TICKS=").unwrap();

    harness.run_palette_command("Record Macro").unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text("0").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();

    harness.type_text("ab").unwrap();
    // Let the timer come due several times while recording.
    for _ in 0..5 {
        harness.advance_time(Duration::from_millis(50));
        harness.tick_and_render().unwrap();
    }

    harness.run_palette_command("Stop Recording Macro").unwrap();
    harness.run_palette_command("Macro Probe Report").unwrap();
    harness
        .wait_until(|h| marker_count(h, "CHAR_STEPS=").is_some())
        .expect("the probe should report the recorded steps");

    let status = harness.get_status_bar();
    let ticks_after = marker_count(&harness, "TICKS=").unwrap();
    assert!(
        ticks_after > ticks_before,
        "the timer must have fired during the recording for this test to mean anything; status = {status:?}"
    );
    assert_eq!(
        marker_count(&harness, "CHAR_STEPS="),
        Some(2),
        "the typed keys should be recorded; status = {status:?}"
    );
    assert_eq!(
        marker_count(&harness, "TICK_STEPS="),
        Some(0),
        "a timer tick must not be recorded into the macro; status = {status:?}"
    );
}
