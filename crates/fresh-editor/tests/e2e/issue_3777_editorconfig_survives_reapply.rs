//! `.editorconfig` must keep applying after the buffer's settings are
//! re-derived from config (#3777).
//!
//! A file opened from the command line is opened before the background
//! grammar build finishes; when it does, every open buffer's language is
//! re-detected and its config re-applied, which used to reset the
//! `.editorconfig` indentation back to the user's `tab_size`.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

fn setup() -> anyhow::Result<(tempfile::TempDir, EditorTestHarness, std::path::PathBuf)> {
    let temp = tempfile::TempDir::new()?;
    let root = temp.path().canonicalize()?;
    std::fs::write(
        root.join(".editorconfig"),
        "root = true\n\n[*]\nindent_style = tab\ntab_width = 8\n",
    )?;
    let file = root.join("m.txt");
    std::fs::write(&file, "")?;

    let mut config = fresh::config::Config::default();
    config.editor.tab_size = 2;
    let harness = EditorTestHarness::with_config_and_working_dir(80, 24, config, root)?;
    Ok((temp, harness, file))
}

fn assert_editorconfig_applied(harness: &mut EditorTestHarness) -> anyhow::Result<()> {
    let settings = &harness.editor().active_state().buffer_settings;
    assert!(settings.use_tabs, "indent_style = tab from .editorconfig");
    assert_eq!(settings.tab_size, 8, "tab_width from .editorconfig");

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;
    assert_eq!(harness.get_buffer_content().unwrap(), "\t");
    Ok(())
}

#[test]
fn editorconfig_survives_grammar_build() -> anyhow::Result<()> {
    let (_temp, mut harness, file) = setup()?;
    harness.open_file(&file)?;

    // The startup grammar build completing re-detects every open buffer.
    let sent = harness
        .editor()
        .async_bridge()
        .expect("async bridge")
        .sender()
        .send(
            fresh::services::async_bridge::AsyncMessage::GrammarRegistryBuilt {
                registry: fresh::primitives::grammar::GrammarRegistry::empty(),
                callback_ids: Vec::new(),
            },
        );
    assert!(sent.is_ok(), "grammar-built message delivered");
    harness.process_async_and_render()?;

    assert_editorconfig_applied(&mut harness)
}

#[test]
fn editorconfig_survives_reset_buffer_settings() -> anyhow::Result<()> {
    let (_temp, mut harness, file) = setup()?;
    harness.open_file(&file)?;

    // "Reset Buffer Settings" gives the buffer what a fresh open would,
    // which includes the .editorconfig.
    harness.editor_mut().reset_buffer_settings();

    assert_editorconfig_applied(&mut harness)
}
