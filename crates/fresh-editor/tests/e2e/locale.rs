// E2E tests for the locale/i18n system
//
// These tests mutate global locale state (fresh_i18n::set_locale) and MUST run
// sequentially. The pin lives in `common::global_state` so non-locale tests
// that pin a non-English locale (`tests/e2e/lsp_completion_french_locale.rs`,
// devcontainer fixture tests that call `fresh::i18n::set_locale`) can share
// it — without that, this module's tests race against them and observe a
// stale non-English locale, producing flaky failures like
// `Expected screen to contain 'cancelada'` against a French screen.
//
// The pin also excludes *harness construction*, which is the other writer of
// the global locale: every `EditorTestHarness` calls
// `i18n::init_with_config` with its own config's locale, so before the pin
// covered it any unrelated test building an editor reset the locale to `en`
// mid-test — the same `'cancelada'` failure, from the opposite direction.

use crate::common::global_state::pin_config_globals;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, LocaleName};

#[test]
fn test_default_locale_shows_english_search_options() {
    let _lock = pin_config_globals();
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Open search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Default locale should show English search option labels
    harness.assert_screen_contains("Case Sensitive");
    harness.assert_screen_contains("Whole Word");
    harness.assert_screen_contains("Regex");
}

#[test]
fn test_locale_from_config_spanish_search_options() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("es".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Open search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Spanish locale should show Spanish search option labels
    harness.assert_screen_contains("Distinguir");
    harness.assert_screen_contains("Palabra completa");
}

#[test]
fn test_locale_from_config_german_search_options() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("de".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Open search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // German locale should show German search option labels
    harness.assert_screen_contains("Ganzes Wort");
}

#[test]
fn test_locale_from_config_french_search_options() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("fr".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Open search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // French locale should show French search option labels
    harness.assert_screen_contains("Mot entier");
}

#[test]
fn test_locale_from_config_japanese_buffer_name() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("ja".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Japanese locale should show Japanese buffer name for unnamed buffer
    // Note: Wide characters may have spaces inserted by terminal backend
    harness.assert_screen_contains("無");
}

#[test]
fn test_locale_from_config_chinese_buffer_name() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("zh-CN".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Chinese locale should show Chinese buffer name for unnamed buffer
    // Note: Wide characters may have spaces inserted by terminal backend
    harness.assert_screen_contains("未");
}

#[test]
fn test_locale_switch_via_command_palette() {
    let _lock = pin_config_globals();
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Open search to verify initial English locale
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Case Sensitive");

    // Close search with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open the locale picker. Via `run_palette_command` so Enter fires on the
    // filtered row rather than on whatever happened to be selected when the
    // last keystroke landed — Quick Open re-filters on input change only.
    harness.run_palette_command("Select Locale").unwrap();

    // Should show locale selection prompt
    harness.wait_for_screen_contains("Select locale:").unwrap();

    // Navigate to Spanish. The picker opens with an *empty* query and the
    // current locale's row selected (`start_select_locale_prompt` ends in
    // `set_input_plain("")`), and Enter confirms the selected row's value
    // rather than the typed text — so what has to happen here is that
    // typing re-filters the list and lands the selection on the `es` row.
    // The backspaces are no-ops on the empty query, kept because they also
    // re-run the filter from a known state. Filtering is synchronous with
    // the keystroke (`filter_suggestions` runs off the prompt's
    // input-changed path, not off a tick), so `type_text` returning is
    // already the gate.
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("es").unwrap();
    harness.render().unwrap();

    // Confirm selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Verify locale changed status message (shown in the new locale)
    // After switching to Spanish, the message is shown in Spanish: "Idioma cambiado a"
    //
    // Waited for, not sampled: confirming the picker re-inits i18n and
    // repaints, and a wrong row would leave this message absent — so the wait
    // is a real gate on the switch having happened, and its periodic screen
    // dumps name the problem if it didn't.
    harness.wait_for_screen_contains("Idioma cambiado").unwrap();

    // Open search again to verify Spanish is now active
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Search UI should now show Spanish labels
    harness.wait_for_screen_contains("Distinguir").unwrap();
    harness.assert_screen_not_contains("Case Sensitive");
}

#[test]
fn test_invalid_locale_falls_back_to_english() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("nonexistent-locale".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Open search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Should fall back to English search option labels
    harness.assert_screen_contains("Case Sensitive");
}

#[test]
fn test_locale_switch_updates_search_cancelled_message() {
    let _lock = pin_config_globals();
    let config = Config {
        locale: LocaleName(Some("es".to_string())),
        ..Default::default()
    };

    let mut harness = EditorTestHarness::with_config(100, 24, config).unwrap();
    harness.render().unwrap();

    // Open search with Ctrl+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Cancel search with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();

    // Should show Spanish cancelled message.
    //
    // Waited for, not sampled off a single `render()`: Escape is dispatched
    // as a mode binding, so the status line it sets lands through the async
    // drain rather than inside `handle_key`, and the status bar competes for
    // its row with whatever the plugins have posted by then. A wait resolves
    // on the first frame that carries the message instead of on the first
    // frame after the key — and if the message never arrives, the periodic
    // screen dumps name the problem.
    harness.wait_for_screen_contains("cancelada").unwrap();
}

#[test]
fn test_locale_switch_updates_menu_labels() {
    let _lock = pin_config_globals();
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Open File menu with Alt+F to verify initial English labels
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Verify English menu items are shown
    harness.assert_screen_contains("New File");
    harness.assert_screen_contains("Save");

    // Close the menu with Escape
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Open command palette with Ctrl+P
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // Type to filter for locale command
    harness.type_text("Select Locale").unwrap();
    harness.render().unwrap();

    // Execute the command
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Should show locale selection prompt
    harness.assert_screen_contains("Select locale:");

    // Type to filter for Spanish
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text("es").unwrap();
    harness.render().unwrap();

    // Confirm selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    // Open File menu again with Alt+F
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::ALT)
        .unwrap();
    harness.render().unwrap();

    // Verify Spanish menu items are now shown
    harness.assert_screen_contains("Nuevo archivo");
    harness.assert_screen_contains("Guardar");
    harness.assert_screen_not_contains("New File");
}

#[test]
fn test_multiple_locales_can_be_loaded() {
    let _lock = pin_config_globals();
    // Test a few key locales with search UI elements
    // For CJK locales, we test for a single character since wide chars may have spaces
    let locales_and_expected = vec![
        ("en", "Case Sensitive"),
        ("es", "Distinguir"),
        ("de", "Ganzes Wort"),
        ("fr", "Mot entier"),
        ("ja", "単"), // Single char to avoid wide-char spacing issues
        ("zh-CN", "全"),
        ("ko", "전"),
        ("ru", "Слово"),
        ("pt-BR", "Palavra"),
        ("bg", "Цяла дума"),
    ];

    // One editor, ten locales. The search option labels are translated by
    // `t!` inside the status bar's render (`view/ui/status_bar.rs`), not
    // captured when the search bar opens, so re-initialising i18n and
    // rendering again is the same code path a per-locale harness would
    // exercise — `init_with_config` is verbatim what the harness calls with
    // `config.locale`, so the locale-code matching ("zh-CN", "pt-BR") is
    // still covered.
    //
    // Building ten full editors instead cost ten plugin-runtime startups in
    // a single test body, which is what made this the one locale test at
    // real risk of nextest's slow-timeout on a loaded runner: ten
    // independent chances to be caught behind a slow start, in a test whose
    // own subject is a table of strings.
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Open search once; it stays open across the locale switches below.
    harness
        .send_key(KeyCode::Char('f'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    for (locale, expected_text) in locales_and_expected {
        fresh::i18n::init_with_config(Some(locale));
        harness.render().unwrap();

        harness.assert_screen_contains(expected_text);
    }
}

/// Helper function to switch locale via command palette
fn switch_locale(harness: &mut EditorTestHarness, locale: &str, search_command: &str) {
    // Open the picker through `run_palette_command`, which waits for the
    // command to be listed as a *result* before confirming. Typing the name
    // and pressing Enter blind fires on whichever row was selected when the
    // last keystroke was processed — Quick Open re-filters on input change,
    // not per frame — so on a loaded runner this could run some other command
    // and leave the locale untouched.
    harness.run_palette_command(search_command).unwrap();

    // Clear the default "en" and type the new locale code (matching existing test pattern)
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Backspace, KeyModifiers::NONE)
        .unwrap();
    harness.type_text(locale).unwrap();
    harness.render().unwrap();

    // Confirm selection
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

#[test]
fn test_locale_switch_affects_file_browser_columns() {
    let _lock = pin_config_globals();
    // Test that file browser column headers are properly localized when switching locales
    // on a live editor instance
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Verify English file browser columns first
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Name");
    harness.assert_screen_contains("Size");
    harness.assert_screen_contains("Modified");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to Spanish (start from English -> "Select Locale")
    switch_locale(&mut harness, "es", "Select Locale");

    // Verify Spanish file browser columns
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Nombre");
    harness.assert_screen_contains("Tamaño");
    harness.assert_screen_contains("Modificado");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to German (start from Spanish -> "Seleccionar idioma")
    switch_locale(&mut harness, "de", "Seleccionar idioma");

    // Verify German file browser columns
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Größe");
    harness.assert_screen_contains("Geändert");
}

#[test]
fn test_locale_switch_affects_clipboard_messages() {
    let _lock = pin_config_globals();
    // Test that clipboard status messages are properly localized when switching locales
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Type some text
    harness.type_text("hello").unwrap();
    harness.render().unwrap();

    // Select all and copy - verify English message
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copied");

    // Switch to Spanish (from English)
    switch_locale(&mut harness, "es", "Select Locale");

    // Select all and copy - verify Spanish message
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copiado");

    // Switch to French (from Spanish)
    switch_locale(&mut harness, "fr", "Seleccionar idioma");

    // Select all and copy - verify French message
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Copié");
}

#[test]
fn test_locale_switch_affects_file_browser_show_hidden() {
    let _lock = pin_config_globals();
    // Test that "Show Hidden" checkbox label is properly localized when switching locales
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // Verify English "Show Hidden" label
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Show Hidden");
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Switch to Spanish (from English)
    switch_locale(&mut harness, "es", "Select Locale");

    // Verify Spanish "Show Hidden" label
    harness
        .send_key(KeyCode::Char('o'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Mostrar ocultos");
}

#[test]
fn test_locale_switch_affects_command_palette_commands() {
    let _lock = pin_config_globals();
    let mut harness = EditorTestHarness::new(100, 24).unwrap();
    harness.render().unwrap();

    // 1. Open command palette and search for "Open File"
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    // In English
    harness.type_text("Open File").unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Open File");

    // Close command palette
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // 2. Switch to Spanish
    switch_locale(&mut harness, "es", "Select Locale");

    // 3. Open command palette and search for "Abrir archivo" (Spanish for Open File)
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    harness.type_text("Abrir archivo").unwrap();
    harness.render().unwrap();

    // Should confirm that we see the translated command
    harness.assert_screen_contains("Abrir archivo");
}
