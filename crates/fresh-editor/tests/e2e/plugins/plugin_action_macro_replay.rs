//! A macro step that runs a plugin handler replays as that handler, once, in
//! order.
//!
//! A key bound to a plugin handler is recorded as the handler's name. The
//! actions the handler issues through `editor.executeActions` are its doing,
//! not separate input: replay re-runs the handler, which issues them again,
//! so recording them as well made replay do the work twice. And the handler
//! runs on the plugin thread, so replay has to wait for it to settle before
//! the next step, or a key typed after it in the recording lands first.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, Keybinding, KeybindingMapName};
use fresh::config_io::DirectoryContext;
use fresh::services::time_source::TimeSource;
use std::fs;
use std::time::Duration;

fn binding(chord: &str, action: &str, char_arg: Option<&str>) -> Keybinding {
    let mut args = std::collections::HashMap::new();
    if let Some(c) = char_arg {
        args.insert("char".to_string(), serde_json::Value::from(c));
    }
    Keybinding {
        key: String::new(),
        modifiers: Vec::new(),
        keys: Vec::new(),
        chord: chord.to_string(),
        action: action.to_string(),
        args,
        when: None,
    }
}

/// A harness with `content` open and an init.ts that registers `handler` (a
/// JS function expression) as the `probe` command. Alt+Y runs it, Alt+R
/// toggles recording into register 0 and Alt+P plays register 0. The handler
/// is wrapped to count its runs, reported as `PROBE_DONE=<n>` once it settles.
fn harness(content: &str, handler: &str) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let dir_context = DirectoryContext::for_testing(temp.path());
    let config_dir = dir_context.config_dir.clone();
    fs::create_dir_all(&config_dir).unwrap();
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let file = working_dir.join("notes.txt");
    fs::write(&file, content).unwrap();

    fs::write(
        config_dir.join("init.ts"),
        format!(
            r#"
            const editor = getEditor();
            const probe = {handler};
            let runs = 0;
            registerHandler("probe", async function () {{
                await probe();
                runs += 1;
                editor.setStatus("PROBE_DONE=" + runs);
            }});
            editor.registerCommand("Probe", "test handler", "probe", null);
            editor.setStatus("PROBE_READY");
            "#
        ),
    )
    .unwrap();

    let mut config = Config {
        active_keybinding_map: KeybindingMapName("default".to_string()),
        ..Default::default()
    };
    // Alt+Y is one of the two Alt+letter keys the default keymap leaves free.
    config.keybindings.push(binding("M-y", "probe", None));
    config
        .keybindings
        .push(binding("M-r", "toggle_macro_recording", Some("0")));
    config
        .keybindings
        .push(binding("M-p", "play_macro", Some("0")));

    let mut harness =
        EditorTestHarness::with_shared_dir_context(120, 24, config, working_dir, dir_context)
            .expect("harness");
    harness.open_file(&file).unwrap();
    harness.editor_mut().load_init_script(true);
    wait_for_status(&mut harness, "PROBE_READY");
    (harness, temp)
}

fn alt(harness: &mut EditorTestHarness, c: char) {
    harness
        .send_key(KeyCode::Char(c), KeyModifiers::ALT)
        .unwrap();
}

fn wait_for_status(harness: &mut EditorTestHarness, text: &str) {
    harness
        .wait_until(|h| h.get_status_bar().contains(text))
        .unwrap_or_else(|e| panic!("status never showed {text:?}: {e}"));
}

/// Play register 0, which holds one run of the handler, then drain what is
/// left. Returns the logical time until the replay first reported done.
///
/// The status line holds one message, so "done" is whichever of the replay's
/// own "Played macro" and the handler's second `PROBE_DONE` lands last: the
/// replay's when it waited for the handler, the handler's when it did not.
fn replay(harness: &mut EditorTestHarness) -> Duration {
    let started = harness.time_source().now();
    alt(harness, 'p');
    harness
        .wait_until(|h| {
            let status = h.get_status_bar();
            status.contains("Played macro '0'") || status.contains("PROBE_DONE=2")
        })
        .expect("the replay should finish");
    let elapsed = harness.time_source().now() - started;
    harness.wait_for_async_quiescence(10).unwrap();
    elapsed
}

const MOVE_DOWN: &str = r#"function () {
    editor.executeActions([{ action: "move_down" }]);
}"#;

/// The vi-mode shape: the handler waits on the host before it acts, so the
/// plugin thread reports the call as started well before the action lands.
const MOVE_DOWN_AFTER_AWAIT: &str = r#"async function () {
    await editor.getBufferText(editor.getActiveBufferId(), 0, 1);
    editor.executeActions([{ action: "move_down" }]);
}"#;

#[test]
fn a_plugin_handler_step_replays_once() {
    let (mut harness, _tmp) = harness("1\n2\n3\n4\n5\n6\n", MOVE_DOWN);

    alt(&mut harness, 'r');
    alt(&mut harness, 'y');
    wait_for_status(&mut harness, "PROBE_DONE=1");
    alt(&mut harness, 'r');

    replay(&mut harness);
    let status = harness.get_status_bar();
    assert!(
        status.contains("Ln 3,"),
        "replaying one Alt+Y should move down one line, to line 3; status = {status:?}"
    );
}

/// Record Alt+Y then `x`, and replay it from after that `x` on line 2.
fn record_move_then_type_and_replay(handler: &str) -> String {
    let (mut harness, _tmp) = harness("aaa\nbbb\nccc\nddd\n", handler);

    alt(&mut harness, 'r');
    alt(&mut harness, 'y');
    wait_for_status(&mut harness, "PROBE_DONE=1");
    harness.type_text("x").unwrap();
    alt(&mut harness, 'r');
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "aaa\nxbbb\nccc\nddd\n"
    );

    replay(&mut harness);
    harness.get_buffer_content().unwrap()
}

#[test]
fn a_key_after_a_plugin_handler_step_replays_after_it() {
    assert_eq!(
        record_move_then_type_and_replay(MOVE_DOWN),
        "aaa\nxbbb\ncxcc\nddd\n",
        "replay should run the handler's move before typing the `x`"
    );
}

#[test]
fn replay_waits_for_an_async_handler_to_finish() {
    assert_eq!(
        record_move_then_type_and_replay(MOVE_DOWN_AFTER_AWAIT),
        "aaa\nxbbb\ncxcc\nddd\n",
        "replay should wait out the handler's await before typing the `x`"
    );
}

/// A handler that opens a prompt settles only once the prompt is answered,
/// and the answer is the next steps of the recording. Replay has to go on to
/// them rather than wait for the handler, which would stall it until the
/// per-step time limit.
#[test]
fn replay_answers_a_prompt_the_handler_opened() {
    let (mut harness, _tmp) = harness(
        "body\n",
        r#"async function () {
            const answer = await editor.prompt("Probe:", "");
            if (answer !== null) {
                editor.insertText(editor.getActiveBufferId(), 0, "[" + answer + "]");
            }
        }"#,
    );

    alt(&mut harness, 'r');
    alt(&mut harness, 'y');
    harness.wait_for_prompt().unwrap();
    harness.type_text("hi").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    wait_for_status(&mut harness, "PROBE_DONE=1");
    alt(&mut harness, 'r');
    assert_eq!(harness.get_buffer_content().unwrap(), "[hi]body\n");

    let elapsed = replay(&mut harness);
    assert_eq!(harness.get_buffer_content().unwrap(), "[hi][hi]body\n");
    assert!(
        elapsed < Duration::from_secs(1),
        "replay should go on to the prompt's answer, not wait out the time limit; took {elapsed:?}"
    );
}
