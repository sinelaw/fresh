//! Odin (<https://odin-lang.org>) as a first-class language: `//` comments,
//! brace-structured auto-indent, and the language's own tab convention.
//!
//! Syntax highlighting for `.odin` is covered by
//! `syntax_highlighting_coverage::test_highlight_odin`, which opens the same
//! fixture and asserts the renderer produced more than one colour; this file
//! covers everything else a `.odin` buffer is supposed to know about itself.
//!
//! Assertions are on the screen where the thing being tested is a rendered
//! fact (the detected language in the status bar, the tab indicator), and on
//! the buffer text where it is an edit the user made (the comment marker, the
//! indent a newline was given) — which is what the user would inspect after
//! typing.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use tempfile::TempDir;

/// A harness with auto-indent on, holding `content` in a `.odin` file, cursor
/// at the end.
fn odin_buffer(content: &str) -> (EditorTestHarness, TempDir) {
    let temp = TempDir::new().unwrap();
    let path = temp.path().join("main.odin");
    // Written to disk rather than typed, so auto-pairing cannot insert a
    // closing brace the test did not ask for.
    std::fs::write(&path, content).unwrap();

    let mut config = Config::default();
    config.editor.auto_indent = true;
    let mut harness = EditorTestHarness::create(
        90,
        24,
        HarnessOptions::new()
            .with_config(config)
            // As in `pascal_indent`: the indent tier is only reached when the
            // buffer has a syntect grammar. Odin's block opener is `{`, so
            // without this the generic bracket fallback indents anyway and
            // the indent test passes for the wrong reason.
            .with_full_grammar_registry()
            .without_empty_plugins_dir(),
    )
    .unwrap();
    harness.open_file(&path).unwrap();
    harness
        .send_key(KeyCode::End, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    (harness, temp)
}

/// Opening a `.odin` file is enough for the editor to name the language — the
/// status bar says so, which is where a user looks.
#[test]
fn an_odin_file_is_detected_as_odin() {
    let (harness, _temp) = odin_buffer("package main\n");
    assert!(
        harness.screen_to_string().contains("Odin"),
        "the status bar should name the language.\nScreen:\n{}",
        harness.screen_to_string()
    );
}

/// Toggle Comment uses `//`, Odin's line comment — and toggles back off,
/// which is the half that catches a prefix that is merely inserted.
#[test]
fn toggle_comment_uses_a_double_slash() {
    let (mut harness, _temp) = odin_buffer("package main\n");
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    let commented = harness.get_buffer_content().unwrap();
    assert!(
        commented.starts_with("// package main") || commented.starts_with("//package main"),
        "expected a `//` line comment, got {commented:?}"
    );

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "package main\n",
        "toggling again should take the comment back off"
    );
}

/// Every Odin block opens with `{`, and pressing Enter after one indents.
///
/// Odin's control flow takes no parentheses (`if count > 3 {`) and has no
/// braceless form, so the brace is the whole signal — which is why the four
/// heads below are checked as a set rather than only a procedure body.
#[test]
fn a_newline_after_a_block_opener_is_indented() {
    for head in [
        "main :: proc() {",
        "Point :: struct {",
        "Flags :: bit_field u8 {",
        "if count > 3 {",
    ] {
        let (mut harness, _temp) = odin_buffer(head);
        harness
            .send_key(KeyCode::Enter, KeyModifiers::NONE)
            .unwrap();
        harness.render().unwrap();

        let content = harness.get_buffer_content().unwrap();
        let body = content
            .strip_prefix(head)
            .and_then(|rest| rest.strip_prefix('\n'))
            .unwrap_or_else(|| panic!("unexpected buffer after Enter: {content:?}"));
        assert!(
            !body.is_empty() && body.chars().all(|c| c == ' ' || c == '\t'),
            "Enter after {head:?} should leave an indent, got {content:?}"
        );
    }
}

/// Odin's own convention is tabs, displayed 8 wide, and the language's
/// defaults say so. Typed indentation therefore inserts a tab, not spaces.
#[test]
fn indentation_defaults_to_tabs_eight_wide() {
    let config = Config::default();
    let odin = config
        .languages
        .get("odin")
        .expect("Odin should have a built-in language entry");
    assert_eq!(odin.use_tabs, Some(true), "Odin indents with tabs");
    assert_eq!(odin.tab_size, Some(8), "shown 8 columns wide");
    assert_eq!(odin.comment_prefix.as_deref(), Some("//"));
    assert_eq!(odin.extensions, vec!["odin".to_string()]);

    // And the buffer honours it: Tab at the start of a line inserts a tab.
    let (mut harness, _temp) = odin_buffer("package main\n");
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        harness.get_buffer_content().unwrap().ends_with('\t'),
        "Tab should insert a tab character, got {:?}",
        harness.get_buffer_content().unwrap()
    );
}

/// `ols` is offered but never started on its own: it has to be built from
/// source, so assuming it is on PATH would make every Odin file report a
/// missing server.
#[test]
fn the_ols_language_server_is_configured_but_not_auto_started() {
    let config = Config::default();
    let servers = config
        .lsp
        .get("odin")
        .expect("Odin should have an LSP entry")
        .as_slice();
    let ols = servers.first().expect("one server configured");
    assert_eq!(ols.command, "ols");
    assert!(ols.enabled, "available when the user asks for it");
    assert!(
        !ols.auto_start,
        "must not start on its own — `ols` is a build-from-source server"
    );
}
