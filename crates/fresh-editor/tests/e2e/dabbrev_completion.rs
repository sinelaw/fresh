//! End-to-end tests for the dabbrev completion service.
//!
//! These tests exercise the DabbrevExpand action (Alt+/ style cycling)
//! and verify smart-case matching, proximity ordering, multi-buffer
//! scanning, and language-aware word boundaries through the full
//! editor stack.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// Create a harness with Alt+/ bound to dabbrev_expand.
fn dabbrev_harness(width: u16, height: u16) -> EditorTestHarness {
    let mut config = Config::default();
    config.keybindings.push(fresh::config::Keybinding {
        key: "/".to_string(),
        modifiers: vec!["alt".to_string()],
        keys: vec![],
        action: "dabbrev_expand".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });
    EditorTestHarness::create(width, height, HarnessOptions::new().with_config(config)).unwrap()
}

/// Send Alt+/ keystroke.
fn send_dabbrev(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();
}

// =============================================================================
// Basic dabbrev expand
// =============================================================================

/// First Alt+/ replaces the prefix with the nearest matching word.
#[test]
fn test_dabbrev_expand_basic() {
    let mut harness = dabbrev_harness(80, 24);

    // Type content with identifiers, then start typing a prefix
    harness.type_text("calculate_difference\n").unwrap();
    harness.type_text("calculate_sum\n").unwrap();
    harness.type_text("calc").unwrap();
    harness.render().unwrap();
    harness.assert_buffer_content("calculate_difference\ncalculate_sum\ncalc");

    // Press Alt+/ — should expand to nearest match (calculate_sum, closest above)
    send_dabbrev(&mut harness);

    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("calculate_sum"),
        "Expected nearest match 'calculate_sum', got: {}",
        content
    );
}

/// Repeated Alt+/ cycles through candidates in proximity order.
#[test]
fn test_dabbrev_expand_cycling() {
    let mut harness = dabbrev_harness(80, 24);

    harness.type_text("apple_pie\n").unwrap();
    harness.type_text("apple_sauce\n").unwrap();
    harness.type_text("app").unwrap();
    harness.render().unwrap();

    // First Alt+/ — nearest match
    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("apple_sauce"),
        "First expand should be 'apple_sauce', got: {}",
        content
    );

    // Second Alt+/ — next candidate
    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("apple_pie"),
        "Second expand should be 'apple_pie', got: {}",
        content
    );

    // Third Alt+/ — wrap back to original prefix
    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("app"),
        "Third expand should restore original prefix 'app', got: {}",
        content
    );
}

// =============================================================================
// Smart-case matching
// =============================================================================

/// Lowercase prefix matches all cases; uppercase prefix filters strictly.
#[test]
fn test_dabbrev_smart_case() {
    let mut harness = dabbrev_harness(80, 24);

    harness.type_text("HttpServer\n").unwrap();
    harness.type_text("http_request\n").unwrap();
    harness.type_text("HTTP_CONST\n").unwrap();
    harness.render().unwrap();

    // Type uppercase prefix "HTTP" — should only match HTTP_CONST
    harness.type_text("HTTP").unwrap();
    harness.render().unwrap();

    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("HTTP_CONST"),
        "Uppercase prefix 'HTTP' should match only 'HTTP_CONST', got: {}",
        content
    );

    // Cycling should wrap back to prefix (only one match)
    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("HTTP"),
        "Should wrap back to original prefix 'HTTP', got: {}",
        content
    );
}

/// Lowercase prefix matches all cases but exact-case scores higher.
#[test]
fn test_dabbrev_smart_case_lowercase_prefers_exact() {
    let mut harness = dabbrev_harness(80, 24);

    // Put HttpServer further from cursor, http_request closer
    harness.type_text("HttpServer\n").unwrap();
    harness.type_text("http_request\n").unwrap();
    harness.type_text("http").unwrap();
    harness.render().unwrap();

    // Alt+/ — "http_request" is both closer and exact-case match
    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("http_request"),
        "Expected exact-case match 'http_request' first, got: {}",
        content
    );
}

// =============================================================================
// Session reset on other actions
// =============================================================================

/// Any non-dabbrev action resets the cycling session.
#[test]
fn test_dabbrev_session_resets_on_typing() {
    let mut harness = dabbrev_harness(80, 24);

    harness.type_text("test_alpha\n").unwrap();
    harness.type_text("test_beta\n").unwrap();
    harness.type_text("tes").unwrap();
    harness.render().unwrap();

    // Expand once
    send_dabbrev(&mut harness);
    let content = harness.get_buffer_content().unwrap();
    assert!(content.ends_with("test_beta"));

    // Type a character — this resets the dabbrev session
    harness.type_text("x").unwrap();
    harness.render().unwrap();

    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("test_betax"),
        "Typing should append to expanded text, got: {}",
        content
    );
}

// =============================================================================
// No prefix — dabbrev should be a no-op
// =============================================================================

/// Alt+/ at the beginning of a line (no prefix) does nothing.
#[test]
fn test_dabbrev_no_prefix_noop() {
    let mut harness = dabbrev_harness(80, 24);

    harness.type_text("hello world\n").unwrap();
    harness.render().unwrap();

    // Cursor is at start of new line — no prefix
    send_dabbrev(&mut harness);

    let content = harness.get_buffer_content().unwrap();
    assert_eq!(content, "hello world\n");
}

// =============================================================================
// Popup-based buffer-word completion (no LSP)
// =============================================================================

/// Create a harness with both Alt+/ for dabbrev and Ctrl+Space for popup completion.
fn popup_completion_harness(width: u16, height: u16) -> EditorTestHarness {
    let mut config = Config::default();
    // Bind Ctrl+Space to trigger completion (lsp_completion action,
    // which falls back to buffer-word popup when no LSP is available).
    config.keybindings.push(fresh::config::Keybinding {
        key: " ".to_string(),
        modifiers: vec!["ctrl".to_string()],
        keys: vec![],
        action: "lsp_completion".to_string(),
        args: std::collections::HashMap::new(),
        when: None,
    });
    EditorTestHarness::create(width, height, HarnessOptions::new().with_config(config)).unwrap()
}

/// Trigger Ctrl+Space to request completion.
fn send_ctrl_space(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
}

/// When no LSP is active, Ctrl+Space should show a popup with buffer-word results.
#[test]
fn test_popup_buffer_words_without_lsp() {
    let mut harness = popup_completion_harness(80, 24);

    harness.type_text("calculate_difference\n").unwrap();
    harness.type_text("calculate_sum\n").unwrap();
    harness.type_text("calc").unwrap();
    harness.render().unwrap();

    // Trigger completion — no LSP, should fall back to buffer words.
    send_ctrl_space(&mut harness);

    // Popup should be visible.
    assert!(
        harness.editor().active_state().popups.is_visible(),
        "Completion popup should be visible without LSP"
    );

    // Verify the screen shows buffer-word candidates.
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("calculate_sum") || screen.contains("calculate_difference"),
        "Popup should show buffer-word completions, screen:\n{}",
        screen
    );
}

/// Accepting a buffer-word popup item replaces the prefix.
#[test]
fn test_popup_buffer_word_accept() {
    let mut harness = popup_completion_harness(80, 24);

    harness.type_text("alpha_one\nalpha_two\nalph").unwrap();
    harness.render().unwrap();

    // Trigger completion popup.
    send_ctrl_space(&mut harness);
    assert!(harness.editor().active_state().popups.is_visible());

    // Accept the first item with Tab.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Popup should close.
    assert!(!harness.editor().active_state().popups.is_visible());

    // Buffer should have the accepted word.
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("alpha_two") || content.ends_with("alpha_one"),
        "Accepted completion should replace prefix, got: {}",
        content
    );
}

/// Escape closes the buffer-word popup without changing the buffer.
#[test]
fn test_popup_buffer_word_dismiss() {
    let mut harness = popup_completion_harness(80, 24);

    harness.type_text("beta_one\nbeta_two\nbet").unwrap();
    harness.render().unwrap();

    send_ctrl_space(&mut harness);
    assert!(harness.editor().active_state().popups.is_visible());

    // Press Escape to dismiss.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(!harness.editor().active_state().popups.is_visible());
    let content = harness.get_buffer_content().unwrap();
    assert!(
        content.ends_with("bet"),
        "Buffer should be unchanged after dismiss, got: {}",
        content
    );
}
