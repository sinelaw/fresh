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
//!
//! The same "which candidate is this, really?" question has three answers
//! to get right, one test each here: accepting a row, keeping the
//! highlight on it while typing narrows the list, and — when a language is
//! served by several servers at once — sending its `completionItem/resolve`
//! to the server that offered it rather than to whichever server happens
//! to advertise resolve first.

use crate::common::fake_lsp::FakeLspServer;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Screen row showing the popup entry whose import path is `import`.
fn row_showing(harness: &EditorTestHarness, import: &str) -> u16 {
    let needle = format!("(use {import})");
    let screen = harness.screen_to_string();
    screen
        .lines()
        .position(|line| line.contains(&needle))
        .unwrap_or_else(|| panic!("no popup row for '{needle}'; screen was:\n{screen}")) as u16
}

/// Whether the popup row at `row` is the highlighted one, judged the way
/// the user judges it: by the selection background painted across it.
fn row_is_highlighted(harness: &EditorTestHarness, row: u16) -> bool {
    let selection_bg = harness.editor().theme().popup_selection_bg;
    let text = harness.screen_row_text(row);
    let column = text
        .find("HashMap")
        .unwrap_or_else(|| panic!("row {row} does not show a candidate: {text:?}"))
        as u16;
    harness
        .get_cell_style(column, row)
        .and_then(|style| style.bg)
        == Some(selection_bg)
}

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

/// Typing one more character re-filters the list — and must leave the
/// highlight on the candidate the user had picked, not snap it back to the
/// first row sharing that candidate's label.
///
/// Both `HashMap` rows survive typing the `p` of `HashMap`, so the
/// selection has somewhere to stay; restoring it by label put it on the
/// wrong (first) row, and accepting then applied that row's import.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_typing_another_character_keeps_the_selected_candidate_highlighted() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let _fake_server = FakeLspServer::spawn_duplicate_labels(temp_dir.path())?;

    let mut harness = open_duplicate_label_popup(&temp_dir, "eager")?;

    // Pick the std::collections candidate.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.render()?;
    assert!(
        row_is_highlighted(&harness, row_showing(&harness, "std::collections::HashMap")),
        "precondition: the second candidate is the selected one; screen was:\n{}",
        harness.screen_to_string()
    );

    // Type the last character of the word. Both candidates still match, so
    // the popup keeps both rows.
    harness.send_key(KeyCode::Char('p'), KeyModifiers::NONE)?;
    harness.wait_until(|h| h.screen_to_string().contains("let m = HashMap"))?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("HashMap (use wrong_crate::HashMap)")
            && screen.contains("HashMap (use std::collections::HashMap)"),
        "both candidates must survive the narrower prefix; screen was:\n{screen}"
    );
    assert!(
        row_is_highlighted(&harness, row_showing(&harness, "std::collections::HashMap")),
        "the highlight must stay on the candidate the user selected; screen was:\n{screen}"
    );
    assert!(
        !row_is_highlighted(&harness, row_showing(&harness, "wrong_crate::HashMap")),
        "only one row may be highlighted; screen was:\n{screen}"
    );

    // And accepting from there still applies the selected candidate's import.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("use std::collections::HashMap;")
            || screen.contains("use wrong_crate::HashMap;")
    })?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("use std::collections::HashMap;")
            && !screen.contains("use wrong_crate::HashMap;"),
        "accepting after the re-filter must apply the highlighted candidate's \
         auto-import; screen was:\n{screen}"
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

/// Open the completion popup against **two** servers for one language,
/// each offering a `HashMap` candidate whose import only that server can
/// produce.
fn open_two_server_popup(temp_dir: &tempfile::TempDir) -> anyhow::Result<EditorTestHarness> {
    let script = FakeLspServer::write_per_server_import_script(temp_dir.path())?;
    let test_file = temp_dir.path().join("main.rs");
    std::fs::write(&test_file, "fn main() {\n    let m = HashMa\n}\n")?;

    let server = |name: &str| fresh::services::lsp::LspServerConfig {
        command: script.to_string_lossy().to_string(),
        args: Some(vec![
            temp_dir
                .path()
                .join(format!("{name}_log.txt"))
                .to_string_lossy()
                .to_string(),
            name.to_string(),
        ]),
        enabled: true,
        auto_start: true,
        process_limits: fresh::services::process_limits::ProcessLimits::default(),
        initialization_options: None,
        env: Default::default(),
        language_id_overrides: Default::default(),
        root_markers: Default::default(),
        name: Some(name.to_string()),
        only_features: None,
        except_features: None,
    };

    let mut config = fresh::config::Config::default();
    config.lsp.insert(
        "rust".to_string(),
        // `alpha` is first, so it is also the first server advertising
        // completion-resolve support — the server the resolve request used
        // to go to no matter which candidate was accepted.
        fresh::types::LspLanguageConfig::Multi(vec![server("alpha"), server("beta")]),
    );

    let mut harness = EditorTestHarness::with_config_and_working_dir(
        120,
        30,
        config,
        temp_dir.path().to_path_buf(),
    )?;

    harness.open_file(&test_file)?;
    harness.render()?;

    // Wait for *both* servers to be initialized and the document opened.
    // Ctrl+Space is a one-shot: a server that has not reported its
    // capabilities yet is skipped when the request fans out, and nothing
    // ever re-asks — so asking too early leaves the popup showing one
    // server's candidate, or none at all, and the wait below hangs
    // forever. Waiting per server is what makes the merged list
    // deterministic with more than one server behind the language.
    for name in ["alpha", "beta"] {
        let log_file = temp_dir.path().join(format!("{name}_log.txt"));
        harness.wait_until(|_| {
            std::fs::read_to_string(&log_file)
                .unwrap_or_default()
                .contains("textDocument/didOpen")
        })?;
    }

    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;

    // Both servers' candidates on screen means both have answered, so the
    // merged list is complete — no timer involved.
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("HashMap (use alpha::HashMap)")
            && screen.contains("HashMap (use beta::HashMap)")
    })?;

    Ok(harness)
}

/// With several servers behind one language, `completionItem/resolve` must
/// go to the server that offered the accepted candidate.
///
/// `CompletionItem::data` is an opaque, server-private handle, so a sibling
/// server cannot answer for it — and resolve is where auto-imports come
/// from, so asking the wrong one lands the wrong `use` line. The client
/// used to send the resolve to the first server advertising resolve
/// support for the language, which is only ever right by luck.
#[test]
#[cfg_attr(
    target_os = "windows",
    ignore = "FakeLspServer uses a Bash script which is not available on Windows"
)]
fn test_resolve_goes_to_the_server_that_offered_the_candidate() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let mut harness = open_two_server_popup(&temp_dir)?;

    // Select `beta`'s candidate — the two servers answer independently, so
    // which row it landed on is whichever the screen shows.
    let beta_row = row_showing(&harness, "beta::HashMap");
    if !row_is_highlighted(&harness, beta_row) {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
        harness.render()?;
    }
    let beta_row = row_showing(&harness, "beta::HashMap");
    assert!(
        row_is_highlighted(&harness, beta_row),
        "precondition: beta's candidate is selected; screen was:\n{}",
        harness.screen_to_string()
    );

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE)?;

    // Whichever server was asked answers with its own import, so either
    // one landing ends the wait and the assertion below decides which was
    // correct — a wrong resolve fails instead of hanging.
    harness.wait_until(|h| {
        let screen = h.screen_to_string();
        screen.contains("use beta::HashMap;") || screen.contains("use alpha::HashMap;")
    })?;

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("use beta::HashMap;") && !screen.contains("use alpha::HashMap;"),
        "the resolve must be sent to beta — the server that offered the \
         accepted candidate — so beta's import is the one that lands; \
         screen was:\n{screen}"
    );

    Ok(())
}
