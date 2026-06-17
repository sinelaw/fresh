//! E2E reproducers for issue #2345 — "html linewrap and broken settings".
//!
//! Two distinct, user-visible defects are triggered by editing a language's
//! settings through **View → Settings → General → Languages → HTML**:
//!
//!   1. After enabling *Auto Surround* for HTML and saving, line-wrap can be
//!      turned off but can never be turned back on for HTML buffers. The root
//!      cause is that the language entry dialog persists *every* field — even
//!      the ones the user never touched — coercing inherited/`null` booleans
//!      (`line_wrap`, `auto_close`, …) to explicit `false`. That writes a
//!      spurious per-language `line_wrap: false` override which always wins
//!      over the global `Toggle Line Wrap` command.
//!
//!   2. In that same dialog, *Auto Surround* (and the other nullable booleans)
//!      render as an empty checkbox `[ ]` even though they inherit `true` from
//!      the global default — so the setting "appears disabled by default" while
//!      it is actually enabled.
//!
//! Both tests drive only keyboard events and assert on rendered output, per
//! CONTRIBUTING.md ("E2E Tests Observe, Not Inspect").

use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

/// A short HTML document whose second line is far wider than the viewport and
/// ends in a distinctive token. The token is only visible on screen when the
/// line is soft-wrapped; if wrapping is off the line is truncated and the token
/// scrolls out of view. That makes "is line-wrap actually on?" observable from
/// rendered output alone.
const LONG_HTML: &str = "<!DOCTYPE html>\n<p>This is a deliberately very long line of HTML body text that keeps going well past the right edge of the terminal viewport so that it must soft wrap onto additional rows before it can reach the final ENDWRAPTOKEN at the very end here.</p>\n";

/// Build a config with a single `html` language entry so the Settings
/// "Languages" map is deterministic to navigate. Global `line_wrap` and
/// `auto_surround` keep their defaults (both `true`).
fn html_only_config() -> Config {
    let mut config = Config::default();
    config.languages.retain(|name, _| name == "html");
    assert!(
        config.editor.line_wrap,
        "precondition: global line_wrap defaults to true"
    );
    assert!(
        config.editor.auto_surround,
        "precondition: global auto_surround defaults to true"
    );
    config
}

/// Run a command-palette command by name (fuzzy match, pick the top result).
fn run_command(harness: &mut EditorTestHarness, command: &str) {
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_prompt().unwrap();
    harness.type_text(command).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
}

/// Return the full text of the first rendered row that contains `needle`.
fn row_with(harness: &EditorTestHarness, needle: &str) -> String {
    let screen = harness.screen_to_string();
    screen
        .lines()
        .find(|line| line.contains(needle))
        .unwrap_or("")
        .to_string()
}

/// From a freshly opened Settings panel, focus the right-hand list and walk
/// down to the "Languages" map, then open the (already-focused) `html` entry's
/// Edit Value dialog. Leaves the dialog open with focus on its first field.
fn open_html_language_dialog(harness: &mut EditorTestHarness) {
    harness.open_settings().unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    // Walk down until the (single) language map entry is the focused row — its
    // "[Enter to edit]" affordance is the reliable signal across terminal
    // heights (the "Languages:" label can be visible before the entry is
    // selected).
    for _ in 0..60 {
        if harness.screen_to_string().contains("[Enter to edit]") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    assert!(
        harness.screen_to_string().contains("[Enter to edit]"),
        "language map row should be focused with an edit affordance"
    );
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        harness.screen_to_string().contains("Auto Surround"),
        "language entry dialog should show the Auto Surround field"
    );
}

/// Issue #2345 (1): editing a language's settings must not silently disable
/// line wrap for that language. After enabling Auto Surround for HTML and
/// saving, toggling line wrap off and back on must re-wrap the buffer.
#[test]
fn issue_2345_html_line_wrap_survives_language_settings_edit() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    let _fixture = harness
        .load_buffer_from_text_named("page.html", LONG_HTML)
        .unwrap();
    harness.render().unwrap();

    // Sanity: line wrap is on by default, so the tail token is visible.
    harness.assert_screen_contains("ENDWRAPTOKEN");

    // Reproduce the reported flow: enable Auto Surround for HTML and save.
    open_html_language_dialog(&mut harness);
    // Fields are alphabetical: Auto Close, Auto Indent, Auto Surround.
    // From the first field, two Downs land on Auto Surround.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // toggle Auto Surround on
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Auto Surround").contains("[v]"),
        "Auto Surround should read as checked after toggling it on; row was: {:?}",
        row_with(&harness, "Auto Surround")
    );
    // Save the entry dialog, then save the settings (closes the panel).
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_screen_contains("ENDWRAPTOKEN").unwrap();

    // Now toggle line wrap off, then back on.
    run_command(&mut harness, "Toggle Line Wrap"); // off
    harness.assert_screen_not_contains("ENDWRAPTOKEN");
    run_command(&mut harness, "Toggle Line Wrap"); // on again

    // The buffer must wrap again — the tail token must be back on screen.
    // Pre-fix this fails: a spurious per-language `line_wrap: false` override
    // was persisted, so the global toggle can no longer re-enable wrapping.
    harness.assert_screen_contains("ENDWRAPTOKEN");
}

/// Issue #2345 (2): in the language entry dialog, a nullable boolean whose
/// value is inherited (unset) must not be rendered as a plainly disabled
/// checkbox `[ ]` — that misrepresents an inherited setting as off. It renders
/// as a neutral `[-]` chip instead.
#[test]
fn issue_2345_inherited_auto_surround_shows_neutral_chip() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    harness.render().unwrap();

    open_html_language_dialog(&mut harness);

    // Per-language auto_surround is unset (inherits the global default), so the
    // dialog must show the neutral inherited chip `[-]`, not a disabled `[ ]`.
    let row = row_with(&harness, "Auto Surround");
    assert!(
        row.contains("Auto Surround"),
        "Auto Surround row should be rendered; screen was:\n{}",
        harness.screen_to_string()
    );
    assert!(
        row.contains("[-]") && !row.contains("[ ]"),
        "inherited Auto Surround should render as neutral `[-]`, not disabled `[ ]`; row was: {:?}",
        row
    );
}

/// Issue #2345: each optional (nullable) field in a language entry dialog has a
/// per-field inherit affordance — a dim `(Inherited)` badge while inherited, and
/// a clickable `[Inherit]` button while overriding. Clicking it reverts just
/// that one field to inherited, without touching its siblings.
#[test]
fn issue_2345_per_field_inherit_button_reverts_single_field() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    harness.render().unwrap();

    open_html_language_dialog(&mut harness);

    // Every nullable field starts inherited → shows the (Inherited) badge.
    assert!(
        row_with(&harness, "Auto Surround").contains("(Inherited)"),
        "inherited field should show the (Inherited) badge; row: {:?}",
        row_with(&harness, "Auto Surround")
    );

    // Override Auto Surround (Auto Close, Auto Indent, Auto Surround; toggle on).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let row = row_with(&harness, "Auto Surround");
    assert!(
        row.contains("[v]") && row.contains("[Inherit]"),
        "an overriding field shows its value plus a clickable [Inherit] button; row: {:?}",
        row
    );
    // A sibling the user never touched stays inherited.
    assert!(
        row_with(&harness, "Line Wrap").contains("(Inherited)"),
        "untouched sibling must stay inherited; row: {:?}",
        row_with(&harness, "Line Wrap")
    );

    // Click the [Inherit] button — only Auto Surround is overriding, so it's the
    // only [Inherit] on screen.
    let (bx, by) = harness
        .find_text_on_screen("[Inherit]")
        .expect("the [Inherit] button should be visible while overriding");
    harness.mouse_click(bx + 1, by).unwrap();
    harness.render().unwrap();

    let row = row_with(&harness, "Auto Surround");
    assert!(
        row.contains("[-]") && row.contains("(Inherited)"),
        "after clicking [Inherit], the field reverts to inherited (neutral chip + badge); row: {:?}",
        row
    );
}

/// Issue #2345: the per-field `[Inherit]` button must be reachable by keyboard,
/// not just the mouse. Once a field is overriding, Tab moves focus from its
/// control onto its `[Inherit]` button, and Enter there reverts the field to
/// inherited.
#[test]
fn issue_2345_inherit_button_reachable_by_tab() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    harness.render().unwrap();

    open_html_language_dialog(&mut harness);

    // Override Auto Surround (Auto Close, Auto Indent, Auto Surround; toggle on).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Auto Surround").contains("[v]"),
        "precondition: Auto Surround overriding; row: {:?}",
        row_with(&harness, "Auto Surround")
    );

    // Tab moves focus onto this field's [Inherit] button; Enter activates it.
    // (If Tab had skipped to the next field, Enter would not inherit Auto
    // Surround and this assertion would fail.)
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let row = row_with(&harness, "Auto Surround");
    assert!(
        row.contains("[-]") && row.contains("(Inherited)"),
        "Tab-to-[Inherit] then Enter should revert the field to inherited; row: {:?}",
        row
    );
}

/// Issue #2345: Shift+Tab reaches the per-field `[Inherit]` button too. Tabbing
/// forward past an overriding field's button to the next field, then
/// Shift+Tab once, must land back on that button (not skip it), so Enter there
/// inherits the right field.
#[test]
fn issue_2345_inherit_button_reachable_by_back_tab() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    harness.render().unwrap();

    open_html_language_dialog(&mut harness);

    // Override Auto Surround (Auto Close, Auto Indent, Auto Surround; toggle on).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Auto Surround").contains("[v]"),
        "precondition: Auto Surround overriding; row: {:?}",
        row_with(&harness, "Auto Surround")
    );

    // Forward: control -> [Inherit] button -> next field.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Shift+Tab once lands back on Auto Surround's [Inherit] button; Enter
    // inherits it. If Shift+Tab had skipped the button, Enter would not inherit
    // Auto Surround and the assertion below would fail.
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let row = row_with(&harness, "Auto Surround");
    assert!(
        row.contains("[-]") && row.contains("(Inherited)"),
        "Shift+Tab to [Inherit] then Enter should revert the field to inherited; row: {:?}",
        row
    );
}

/// Issue #2345: a field with a built-in default (no inheritance chain) gets a
/// `[Reset]` button — distinct from `[Inherit]` — that appears only once the
/// value differs from that default, and restores it. `Auto Indent` is a plain
/// (non-nullable) boolean whose built-in default is `true`.
#[test]
fn issue_2345_reset_button_restores_builtin_default() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    harness.render().unwrap();

    open_html_language_dialog(&mut harness);

    // At its built-in default, the field offers no Reset (nothing to undo) and
    // — being non-nullable — never offers Inherit.
    let row = row_with(&harness, "Auto Indent");
    assert!(
        row.contains("[v]") && !row.contains("[Reset]") && !row.contains("[Inherit]"),
        "Auto Indent at default should show no action buttons; row: {:?}",
        row
    );

    // Change it (Auto Close -> Auto Indent), and [Reset] appears.
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // toggle Auto Indent off
    harness.render().unwrap();
    let row = row_with(&harness, "Auto Indent");
    assert!(
        row.contains("[ ]") && row.contains("[Reset]") && !row.contains("[Inherit]"),
        "changed non-nullable field should offer [Reset] only; row: {:?}",
        row
    );

    // Tab onto [Reset] and activate it; the field returns to its default.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    let row = row_with(&harness, "Auto Indent");
    assert!(
        row.contains("[v]") && !row.contains("[Reset]"),
        "[Reset] should restore the built-in default and then disappear; row: {:?}",
        row
    );
}

/// Issue #2345: the `[Reset]` button is reachable both ways too. Tab forward
/// past it to the next field, then Shift+Tab back onto it, and Enter resets the
/// right field — exercising the action-button focus stops in both directions.
#[test]
fn issue_2345_reset_button_reachable_by_back_tab() {
    let mut harness = EditorTestHarness::with_config(120, 30, html_only_config()).unwrap();
    harness.render().unwrap();

    open_html_language_dialog(&mut harness);

    // Change Auto Indent so it offers [Reset] (Auto Close -> Auto Indent).
    harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap(); // toggle off
    harness.render().unwrap();
    assert!(
        row_with(&harness, "Auto Indent").contains("[Reset]"),
        "precondition: Auto Indent should offer [Reset]; row: {:?}",
        row_with(&harness, "Auto Indent")
    );

    // Forward: control -> [Reset] -> next field (Auto Surround).
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    // Shift+Tab lands back on Auto Indent's [Reset]; Enter resets it.
    harness
        .send_key(KeyCode::BackTab, KeyModifiers::SHIFT)
        .unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    let row = row_with(&harness, "Auto Indent");
    assert!(
        row.contains("[v]") && !row.contains("[Reset]"),
        "Shift+Tab to [Reset] then Enter should restore the default; row: {:?}",
        row
    );
}

/// Issue #2345: an object/JSON field with a non-null built-in default — a
/// language's `formatter` — can be reset to that default from the UI. Its only
/// other action, Inherit → null, would *clear* the formatter rather than
/// restore it, so without a [Reset] there'd be no way back to the bundled
/// value. (Reset on these fields is mouse-only; the JSON editor isn't a Tab
/// stop.)
#[test]
fn issue_2345_reset_restores_builtin_formatter() {
    // A language that ships a formatter (c -> clang-format), overridden here to
    // a custom command so it differs from the built-in default.
    let mut config = Config::default();
    config.languages.retain(|name, _| name == "c");
    let mut fmt = config
        .languages
        .get("c")
        .unwrap()
        .formatter
        .clone()
        .expect("c ships a built-in formatter");
    assert_eq!(
        fmt.command, "clang-format",
        "precondition: c default formatter"
    );
    fmt.command = "my-custom-fmt".to_string();
    config.languages.get_mut("c").unwrap().formatter = Some(fmt);

    let mut harness = EditorTestHarness::with_config(120, 50, config).unwrap();
    harness.render().unwrap();
    // Only `c` is in the languages map, so this opens c's dialog.
    open_html_language_dialog(&mut harness);

    // The Formatter shows the override and offers a [Reset] distinct from
    // [Clear]. It reads [Clear], not [Inherit], because a formatter has no
    // global fallback — setting it to null just unsets it.
    assert!(
        harness.screen_to_string().contains("my-custom-fmt"),
        "dialog should show the overridden formatter; screen:\n{}",
        harness.screen_to_string()
    );
    let row = row_with(&harness, "Formatter");
    assert!(
        row.contains("[Reset]") && row.contains("[Clear]") && !row.contains("[Inherit]"),
        "an overriding formatter should offer [Reset] and [Clear] (not [Inherit]); row: {:?}",
        row
    );

    // Click [Reset]: the bundled clang-format is restored and [Reset] goes away
    // (the value now matches the default).
    let (bx, by) = harness
        .find_text_on_screen("[Reset]")
        .expect("[Reset] should be visible on the Formatter row");
    harness.mouse_click(bx + 1, by).unwrap();
    harness.render().unwrap();

    assert!(
        harness.screen_to_string().contains("clang-format"),
        "after [Reset] the bundled clang-format should be restored; screen:\n{}",
        harness.screen_to_string()
    );
    assert!(
        !row_with(&harness, "Formatter").contains("[Reset]"),
        "[Reset] should disappear once the value matches the default; row: {:?}",
        row_with(&harness, "Formatter")
    );
}
