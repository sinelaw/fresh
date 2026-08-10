//! E2E reproduction for sinelaw/fresh#2952: accepting a completion applied
//! *another* candidate's auto-import.
//!
//! Once the client advertises `resolveSupport.additionalTextEdits` (#2603),
//! servers offer unimported symbols — and then duplicate labels become the
//! norm rather than the exception. rust-analyzer offers one `HashMap` row
//! per crate exporting one:
//!
//! ```text
//! S HashMap        (use oxc_allocator::HashMap)
//! S HashMapStrategy (use proptest::collection::HashMapStrategy)
//! S HashMap        (use std::collections::HashMap)
//! ```
//!
//! The accept path passed only the selected row's *label* down to
//! `apply_completion_additional_edits`, which recovered the item with
//! `completion_items.iter().find(|i| i.label == label)` — the first row
//! sharing the label. Selecting the `std::collections::HashMap` row
//! inserted `use oxc_allocator::HashMap` instead, both for candidates that
//! ship their edits eagerly and (rust-analyzer's real behaviour) for
//! candidates whose edits arrive through `completionItem/resolve`.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Start an editor against the duplicate-label fake server in `mode`
/// (`"eager"` or `"resolve"`), with the completion popup open on two
/// candidates both labelled `HashMap`.
fn open_duplicate_label_popup(
    temp_dir: &tempfile::TempDir,
    mode: &str,
) -> anyhow::Result<EditorTestHarness> {
    let log_file = temp_dir.path().join(format!("dup_label_{mode}_log.txt"));
    let test_file = temp_dir.path().join("main.rs");
    std::fs::write(&test_file, "fn main() {\n    let m = HashMa\n}\n")?;

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: FakeLspServer::duplicate_labels_script_path(temp_dir.path())
                .to_string_lossy()
                .to_string(),
            args: Some(vec![
                log_file.to_string_lossy().to_string(),
                mode.to_string(),
            ]),
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

    // Wait for the server to be initialized and the document opened.
    harness.wait_until(|_| {
        std::fs::read_to_string(&log_file)
            .unwrap_or_default()
            .contains("textDocument/didOpen")
    })?;

    // Put the cursor at the end of "    let m = HashMa" and ask for
    // completions.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;

    // Both same-labelled candidates must be on screen, told apart only by
    // the import path in their labelDetails.
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("HashMap (use wrong_crate::HashMap)")
            && screen.contains("HashMap (use std::collections::HashMap)")
    })?;

    Ok(harness)
}

/// Accepting the *second* of two same-labelled candidates must apply that
/// candidate's `additionalTextEdits`, not the first one's.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_accepting_second_same_label_candidate_applies_its_own_import() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_duplicate_labels(temp_dir.path())?;

    let mut harness = open_duplicate_label_popup(&temp_dir, "eager")?;

    // Move down to the std::collections candidate and accept it.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;

    // Either import landing ends the wait; the assertions below decide
    // which one was correct, so applying the wrong one fails the test
    // instead of hanging on a condition that will never hold.
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("use std::collections::HashMap;")
            || screen.contains("use wrong_crate::HashMap;")
    })?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("use std::collections::HashMap;")
            && !screen.contains("use wrong_crate::HashMap;"),
        "accepting the second candidate must apply its own auto-import, not \
         the first candidate's; screen was:\n{screen}"
    );
    assert!(
        screen.contains("let m = HashMap"),
        "the identifier itself must still be inserted; screen was:\n{screen}"
    );

    Ok(())
}

/// Same, for candidates whose imports are deferred to
/// `completionItem/resolve` — rust-analyzer's actual behaviour, and the
/// path that decides which `use` line the user ends up with in practice.
/// The wrong item being *sent* is enough to lose here: the server answers
/// the item it was given.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_resolve_deferred_import_follows_the_selected_candidate() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_duplicate_labels(temp_dir.path())?;

    let mut harness = open_duplicate_label_popup(&temp_dir, "resolve")?;

    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;

    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        // Either import landing ends the wait; the assertion below decides
        // which one was correct, so a wrong import fails instead of hanging.
        screen.contains("use std::collections::HashMap;")
            || screen.contains("use wrong_crate::HashMap;")
    })?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("use std::collections::HashMap;")
            && !screen.contains("use wrong_crate::HashMap;"),
        "the resolve request must carry the candidate the user selected, so \
         the resolved import is std::collections::HashMap; screen was:\n{screen}"
    );

    Ok(())
}
