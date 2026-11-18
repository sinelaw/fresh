//! #3796: keys wait for a plugin mode's previous key handler only in modes
//! that ask for it (`defineMode(..., holdKeys)`), so a slow handler in any
//! other mode doesn't stall typing.

use crate::common::harness::{copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh_core::api::PluginCommand;
use std::fs;

const PLUGIN: &str = r###"
const editor = getEditor();

async function tst_hold_slow() {
    await editor.delay(5000);
}
registerHandler("tst_hold_slow", tst_hold_slow);

editor.defineMode("tst-nohold", [["a", "tst_hold_slow"]], true);
editor.defineMode("tst-hold", [["a", "tst_hold_slow"]], true, false, false, true);

editor.setStatus("khold ready");
"###;

fn set_mode(harness: &mut EditorTestHarness, mode: &str) {
    harness
        .editor_mut()
        .handle_plugin_command(PluginCommand::SetEditorMode {
            mode: Some(mode.to_string()),
        })
        .unwrap();
}

#[test]
fn test_slow_handler_holds_keys_only_in_opted_in_mode() {
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    fs::create_dir(&project_root).unwrap();
    let plugins_dir = project_root.join("plugins");
    fs::create_dir(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join("tst_key_hold.ts"), PLUGIN).unwrap();
    let test_file = project_root.join("test.txt");
    fs::write(&test_file, "hello\n").unwrap();

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        30,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    harness.open_file(&test_file).unwrap();
    harness.wait_for_screen_contains("khold ready").unwrap();

    // A mode without `holdKeys`: the slow handler runs, and the key after
    // it is not queued behind it.
    set_mode(&mut harness, "tst-nohold");
    harness
        .send_key_without_drain(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    assert!(
        !harness.editor().holds_keys_for_plugin(),
        "a mode that didn't opt in must not hold keys"
    );

    // The same handler in a mode that opted in holds the next key.
    set_mode(&mut harness, "tst-hold");
    harness
        .send_key_without_drain(KeyCode::Char('a'), KeyModifiers::NONE)
        .unwrap();
    assert!(
        harness.editor().holds_keys_for_plugin(),
        "a holdKeys mode holds keys behind its unsettled handler"
    );
}
