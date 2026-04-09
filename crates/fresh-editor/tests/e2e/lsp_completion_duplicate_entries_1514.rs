//! E2E reproduction for the LSP completion duplicate-entries bug tracked as
//! sinelaw/fresh#1514 (originally surfaced in the discussion thread
//! sinelaw/fresh#1444 about clangd users seeing duplicated / stale items in
//! the completion popup).
//!
//! Root cause summary: closing the completion popup via the normal
//! "pass-through" path (e.g. pressing Enter or typing a non-word character)
//! runs `Editor::hide_popup()` but does **not** clear `self.completion_items`.
//! The next completion request's response is then merged ("extend") with the
//! leftover items from the previous request, so the popup ends up containing
//! every item the editor has ever received. See:
//!
//!   * `crates/fresh-editor/src/app/lsp_requests.rs::handle_completion_response`
//!     ("Store/extend original items for type-to-filter …")
//!   * `crates/fresh-editor/src/app/render.rs::hide_popup` — the close path
//!     that doesn't touch `completion_items`.
//!   * `crates/fresh-editor/src/app/popup_actions.rs::handle_popup_cancel`
//!     — the *other* close path, which *does* clear `completion_items` and
//!     is only reached when Esc cancels the popup.
//!
//! The test drives two sequential Ctrl+Space completion cycles against a
//! minimal inline Bash fake LSP and asserts that the second popup doesn't
//! still contain items rendered by the first popup.

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};

/// Count non-overlapping occurrences of `needle` in `haystack`.
fn count_occurrences(haystack: &str, needle: &str) -> usize {
    if needle.is_empty() {
        return 0;
    }
    let mut count = 0;
    let mut idx = 0;
    while let Some(pos) = haystack[idx..].find(needle) {
        count += 1;
        idx += pos + needle.len();
    }
    count
}

/// Write a minimal Bash LSP server that implements just enough of the
/// protocol for this test:
///
/// * `initialize` — advertises a completion provider.
/// * `textDocument/completion` — returns a proper `CompletionList` with
///   `isIncomplete` (lsp_types requires it; without it the response fails
///   to deserialise and the editor silently sees an empty completion list).
/// * A generic fallback that replies `null` to any other request the
///   editor happens to send (e.g. inlayHint, foldingRange, semanticTokens)
///   so the LSP pipeline doesn't stall waiting for a response.
///
/// Returns the path to the generated script.
fn write_fake_lsp_script(dir: &std::path::Path) -> anyhow::Result<std::path::PathBuf> {
    let script = r#"#!/bin/bash
read_message() {
    local content_length=0
    while IFS= read -r line; do
        line="${line%$'\r'}"
        if [ -z "$line" ]; then
            break
        fi
        case "$line" in
            Content-Length:*)
                content_length="${line#Content-Length:}"
                content_length="${content_length// /}"
                ;;
        esac
    done
    if [ "$content_length" -gt 0 ] 2>/dev/null; then
        dd bs=1 count="$content_length" 2>/dev/null
    fi
}

send_message() {
    local message="$1"
    local length=${#message}
    printf "Content-Length: %d\r\n\r\n%s" "$length" "$message"
}

while true; do
    msg=$(read_message)
    if [ -z "$msg" ]; then
        break
    fi
    method=$(echo "$msg" | grep -o '"method":"[^"]*"' | cut -d'"' -f4)
    msg_id=$(echo "$msg" | grep -o '"id":[0-9]*' | head -1 | cut -d':' -f2)
    case "$method" in
        "initialize")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"capabilities":{"completionProvider":{"triggerCharacters":[".",":",">"]},"textDocumentSync":1}}}'
            ;;
        "textDocument/completion")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":{"isIncomplete":false,"items":[{"label":"test_function","kind":3,"insertText":"test_function"},{"label":"test_variable","kind":6,"insertText":"test_variable"},{"label":"test_struct","kind":22,"insertText":"test_struct"}]}}'
            ;;
        "shutdown")
            send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            break
            ;;
        *)
            # Reply with null to any other request so the editor's request
            # pipeline keeps draining; notifications (no id) are dropped.
            if [ -n "$msg_id" ]; then
                send_message '{"jsonrpc":"2.0","id":'$msg_id',"result":null}'
            fi
            ;;
    esac
done
"#;
    let path = dir.join("dup_fake_lsp.sh");
    std::fs::write(&path, script)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = std::fs::metadata(&path)?.permissions();
        perms.set_mode(0o755);
        std::fs::set_permissions(&path, perms)?;
    }
    Ok(path)
}

/// A fresh completion request must not leave items from the previous
/// completion popup on screen. Regression for sinelaw/fresh#1514
/// (duplicate / stale entries in the completion popup, originally reported
/// in discussion #1444).
#[test]
#[cfg_attr(target_os = "windows", ignore)] // Bash-based fake LSP server
fn test_completion_popup_has_no_duplicates_after_second_request_1514() -> anyhow::Result<()> {
    let temp_dir = tempfile::tempdir()?;
    let script_path = write_fake_lsp_script(temp_dir.path())?;

    // Several empty indented slots inside main() so the popup (which
    // renders below the cursor) has enough room to lay out all its items
    // in both rounds without hitting the bottom of the viewport.
    let test_file = temp_dir.path().join("dup.rs");
    std::fs::write(&test_file, "fn main() {\n    \n    \n    \n    \n    \n}\n")?;

    // Disable quick_suggestions so stray character input doesn't fire
    // extra (debounced) completion requests that the test isn't expecting.
    let mut config = fresh::config::Config::default();
    config.editor.quick_suggestions = false;
    config.lsp.insert(
        "rust".to_string(),
        fresh::types::LspLanguageConfig::Multi(vec![fresh::services::lsp::LspServerConfig {
            command: script_path.to_string_lossy().to_string(),
            args: vec![],
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

    // Wait until the LSP server has initialized so completion requests
    // will actually reach the fake server and come back.
    harness.wait_until(|h| h.editor().initialized_lsp_server_count("rust") >= 1)?;

    // ---- Round 1: first indented empty line ----------------------------
    harness.send_key(KeyCode::Down, KeyModifiers::NONE)?;
    harness.send_key(KeyCode::End, KeyModifiers::NONE)?;
    harness.render()?;

    // Type a prefix that matches all three fake items, then trigger
    // completion with Ctrl+Space.
    harness.type_text("test_")?;
    harness.render()?;
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;

    // Wait until the popup renders with the first response on screen.
    harness.wait_until(|h| h.screen_to_string().contains("test_function"))?;

    // Sanity check: each label appears exactly once in the round-1 popup.
    {
        let screen = harness.screen_to_string();
        for label in ["test_function", "test_variable", "test_struct"] {
            let n = count_occurrences(&screen, label);
            assert_eq!(
                n, 1,
                "Round 1: expected '{label}' exactly once in the rendered \
                 screen, found {n}. Screen:\n{screen}",
            );
        }
    }

    // ---- Round 2: dismiss popup via Enter, new completion request -----
    // Pressing Enter while the completion popup is open takes the
    // "pass-through" path: the popup's input handler defers ClosePopup and
    // returns Ignored, so the newline is inserted and hide_popup() runs.
    // That path does NOT clear self.completion_items — which is what sets
    // up the bug.
    harness.send_key(KeyCode::Enter, KeyModifiers::NONE)?;
    harness.render()?;

    // Wait until the old popup is gone from the screen before triggering
    // the second round, so we're sure the popup we observe next is a
    // freshly rendered one.
    harness.wait_until(|h| !h.screen_to_string().contains("test_function"))?;

    // Type the prefix again on the fresh line and fire another completion
    // request. With the bug, `handle_completion_response` extends the
    // leftover items from round 1 with this new response and the popup
    // renders each label twice.
    harness.type_text("test_")?;
    harness.render()?;
    harness.send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)?;

    // Wait for the round-2 popup to render.
    harness.wait_until(|h| h.screen_to_string().contains("test_function"))?;

    // Assert: the popup for the second completion request must show each
    // fake-LSP label exactly once — no duplicates from round 1.
    let screen = harness.screen_to_string();
    for label in ["test_function", "test_variable", "test_struct"] {
        let n = count_occurrences(&screen, label);
        assert_eq!(
            n, 1,
            "Round 2: completion popup should show '{label}' exactly once \
             after the second completion request, but found {n} copies. \
             This is the duplicate-entries bug tracked by sinelaw/fresh#1514 \
             (originally discussed in sinelaw/fresh#1444). Screen:\n{screen}",
        );
    }

    Ok(())
}
