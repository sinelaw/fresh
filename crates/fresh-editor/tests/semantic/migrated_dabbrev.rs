//! Faithful migrations of `tests/e2e/dabbrev_completion.rs`
//! (DabbrevExpand action — Alt+/-style word completion that
//! cycles through buffer words). Excludes the popup-based
//! tests (`test_popup_buffer_words_*`) which need ModalScenario
//! popup-stack inspection.
//!
//! No mocks: `Action::DabbrevExpand` is dispatched directly,
//! same path the keymap binding hits.

use crate::common::scenario::buffer_scenario::{
    assert_buffer_scenario, check_buffer_scenario, BufferScenario, CursorExpect,
};
use fresh::test_api::Action;

fn type_chars(s: &str) -> impl Iterator<Item = Action> + '_ {
    s.chars().map(Action::InsertChar)
}

#[test]
fn migrated_dabbrev_expand_picks_nearest_match_above_cursor() {
    // Original: `test_dabbrev_expand_basic`. With buffer
    // "calculate_difference\ncalculate_sum\n" + prefix "calc",
    // DabbrevExpand should expand "calc" to the nearest matching
    // word above the cursor — "calculate_sum".
    let mut actions: Vec<Action> =
        type_chars("calculate_difference\ncalculate_sum\ncalc").collect();
    actions.push(Action::DabbrevExpand);

    let expected = "calculate_difference\ncalculate_sum\ncalculate_sum";
    assert_buffer_scenario(BufferScenario {
        description: "DabbrevExpand expands 'calc' to nearest 'calculate_sum'".into(),
        initial_text: String::new(),
        actions,
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_dabbrev_cycles_through_candidates_in_proximity_order() {
    // Original: `test_dabbrev_expand_cycling`. Three
    // DabbrevExpands cycle: nearest match, next match, then
    // wrap back to the original prefix.
    let mut actions: Vec<Action> = type_chars("apple_pie\napple_sauce\napp").collect();
    actions.extend(std::iter::repeat_n(Action::DabbrevExpand, 3));

    // After cycling 3 times: nearest ("apple_sauce") → next
    // ("apple_pie") → wrap back to "app".
    let expected = "apple_pie\napple_sauce\napp";
    assert_buffer_scenario(BufferScenario {
        description: "Three DabbrevExpands cycle: nearest, next, wrap-to-prefix".into(),
        initial_text: String::new(),
        actions,
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_dabbrev_uppercase_prefix_filters_strictly() {
    // Original: `test_dabbrev_smart_case`. Uppercase prefix
    // "HTTP" matches only the all-caps "HTTP_CONST", not the
    // mixed-case "HttpServer" or lowercase "http_request".
    let mut actions: Vec<Action> =
        type_chars("HttpServer\nhttp_request\nHTTP_CONST\nHTTP").collect();
    actions.push(Action::DabbrevExpand);

    let expected = "HttpServer\nhttp_request\nHTTP_CONST\nHTTP_CONST";
    assert_buffer_scenario(BufferScenario {
        description: "DabbrevExpand with uppercase 'HTTP' matches only HTTP_CONST".into(),
        initial_text: String::new(),
        actions,
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_dabbrev_lowercase_prefers_exact_case() {
    // Original: `test_dabbrev_smart_case_lowercase_prefers_exact`.
    // Lowercase prefix "http" matches both "HttpServer" and
    // "http_request"; the exact-case match wins.
    let mut actions: Vec<Action> = type_chars("HttpServer\nhttp_request\nhttp").collect();
    actions.push(Action::DabbrevExpand);

    let expected = "HttpServer\nhttp_request\nhttp_request";
    assert_buffer_scenario(BufferScenario {
        description: "DabbrevExpand with lowercase 'http' prefers exact-case 'http_request'".into(),
        initial_text: String::new(),
        actions,
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_dabbrev_session_resets_on_typing() {
    // Original: `test_dabbrev_session_resets_on_typing`. After
    // an expand, typing 'x' must NOT keep cycling; it appends
    // to the expanded text.
    let mut actions: Vec<Action> = type_chars("test_alpha\ntest_beta\ntes").collect();
    actions.push(Action::DabbrevExpand); // → "test_beta"
    actions.push(Action::InsertChar('x'));

    let expected = "test_alpha\ntest_beta\ntest_betax";
    assert_buffer_scenario(BufferScenario {
        description: "Typing 'x' after DabbrevExpand appends, doesn't cycle".into(),
        initial_text: String::new(),
        actions,
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

#[test]
fn migrated_dabbrev_at_start_of_empty_prefix_is_noop() {
    // Original: `test_dabbrev_no_prefix_noop`. Cursor at start
    // of an empty line (no word-prefix to expand) ⇒ no-op.
    let mut actions: Vec<Action> = type_chars("hello world\n").collect();
    actions.push(Action::DabbrevExpand);

    let expected = "hello world\n";
    assert_buffer_scenario(BufferScenario {
        description: "DabbrevExpand at start of empty line is a no-op".into(),
        initial_text: String::new(),
        actions,
        expected_text: expected.into(),
        expected_primary: CursorExpect::at(expected.len()),
        ..Default::default()
    });
}

/// Anti-test: drops the final `DabbrevExpand` from the basic
/// scenario. Without it, the buffer ends with just the typed
/// "calc" prefix, not "calculate_sum".
#[test]
fn anti_dabbrev_dropping_action_yields_check_err() {
    let actions: Vec<Action> = type_chars("calculate_difference\ncalculate_sum\ncalc").collect();
    let scenario = BufferScenario {
        description: "anti: DabbrevExpand dropped — prefix 'calc' must remain unexpanded".into(),
        initial_text: String::new(),
        actions,
        expected_text: "calculate_difference\ncalculate_sum\ncalculate_sum".into(),
        expected_primary: CursorExpect::at(
            "calculate_difference\ncalculate_sum\ncalculate_sum".len(),
        ),
        ..Default::default()
    };
    assert!(
        check_buffer_scenario(scenario).is_err(),
        "anti-test: without DabbrevExpand, the buffer ends with the literal \
         'calc' prefix, not the expanded 'calculate_sum'"
    );
}
