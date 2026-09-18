//! Confirmations are a modal dialog, not a line of grey text on the last row.
//!
//! The reported symptom was the quit prompt: with unsaved buffers, `Ctrl+Q`
//! asked on the terminal's bottom line — the same row a status message uses,
//! forty rows below where the user was looking — and spelled its answers as
//! parenthesised letters run together in a sentence. People read the result
//! as a hang.
//!
//! Every test here drives keys or the mouse and asserts on what is on screen,
//! which is the whole point: the old prompt *was* on screen, technically.

use crate::common::global_state::pin_config_globals;
use crate::common::harness::EditorTestHarness;
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;

const WIDTH: u16 = 100;
const HEIGHT: u16 = 30;

/// The i18n locale is a process global, and one test below pins Czech to
/// exercise a locale whose accelerator letters collide. `pin_config_globals`
/// keeps every *other* harness construction out while it is held, but a test
/// that already built its harness would render that Czech screen mid-body —
/// so every test in this file takes the pin, and they serialize against each
/// other rather than against the whole suite.
fn pin() -> impl Drop {
    pin_config_globals()
}

/// A harness with one dirty, file-backed buffer.
fn dirty_buffer(config: Config) -> (EditorTestHarness, std::path::PathBuf) {
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(WIDTH, HEIGHT, config).expect("harness");
    let dir = harness.project_dir().expect("project dir");
    let file = dir.join("notes.txt");
    std::fs::write(&file, "original\n").unwrap();
    harness.open_file(&file).unwrap();
    harness.type_text("EDITED").unwrap();
    harness.render().unwrap();
    (harness, file)
}

/// Which row the given text is on, if any.
fn row_of(harness: &EditorTestHarness, needle: &str) -> Option<u16> {
    (0..HEIGHT).find(|r| harness.screen_row_text(*r).contains(needle))
}

fn quit(harness: &mut EditorTestHarness) {
    harness
        .send_key(KeyCode::Char('q'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
}

/// The core of the report: the question is a card in the middle of the screen
/// with its outcomes as buttons, not a sentence on the last row.
#[test]
fn quitting_with_unsaved_work_asks_in_a_centred_dialog() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    quit(&mut harness);

    assert!(!harness.should_quit(), "the question must block the exit");

    harness.assert_screen_contains("Unsaved Changes");
    harness.assert_screen_contains("1 buffer has unsaved changes.");
    harness.assert_screen_contains("Save and Quit");
    harness.assert_screen_contains("Discard and Quit");
    harness.assert_screen_contains("Cancel");

    // Where it is, is the fix. The title sits well clear of both the last row
    // and the first: a modal the user has to hunt for is the bug.
    let title = row_of(&harness, "Unsaved Changes").expect("the dialog's title is on screen");
    assert!(
        title > 2 && title < HEIGHT - 4,
        "the dialog must be in the body of the screen, not pinned to an edge; \
         title row was {title} of {HEIGHT}"
    );

    // And the bottom row is not where the question went.
    let last = harness.screen_row_text(HEIGHT - 1);
    assert!(
        !last.contains("unsaved changes"),
        "the confirmation must not be on the last row any more; found {last:?}"
    );
}

/// The answers are spelled out. The old prompt's `(s)ave, (d)iscard, (C)ancel`
/// is exactly what people could not read.
#[test]
fn the_dialog_does_not_state_its_answers_as_parenthesised_letters() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    quit(&mut harness);

    let screen = harness.screen_to_string();
    for legacy in ["(s)ave", "(d)iscard", "(C)ancel", "(q)uit"] {
        assert!(
            !screen.contains(legacy),
            "{legacy:?} is the old bottom-row spelling and must be gone"
        );
    }
}

/// Hot exit adds an outcome rather than a whole second phrasing of the
/// question — which is what let eight message strings become four.
#[test]
fn hot_exit_offers_a_recoverable_quit_as_a_fourth_button() {
    let _pin = pin();
    let mut config = Config::default();
    config.editor.hot_exit = true;
    let (mut harness, _file) = dirty_buffer(config);
    quit(&mut harness);

    harness.assert_screen_contains("Save and Quit");
    harness.assert_screen_contains("Discard and Quit");
    harness.assert_screen_contains("Quit (recoverable)");
    harness.assert_screen_contains("Cancel");
}

/// Without hot exit there is no recoverable option to offer.
#[test]
fn without_hot_exit_there_is_no_recoverable_quit() {
    let _pin = pin();
    let mut config = Config::default();
    config.editor.hot_exit = false;
    let (mut harness, _file) = dirty_buffer(config);
    quit(&mut harness);

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("recoverable"),
        "nothing recovers the work without hot exit, so nothing may promise it"
    );
}

/// Arrow keys move between the buttons, and the armed one is marked on screen
/// by brackets — legible even where colour is not.
#[test]
fn arrows_move_the_armed_button() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    quit(&mut harness);

    let buttons = row_of(&harness, "Save and Quit").expect("the button row is on screen");
    let armed = |h: &EditorTestHarness| h.screen_row_text(buttons);

    assert!(
        armed(&harness).contains("[ Save and Quit ]"),
        "the dialog opens with the safe outcome armed; row was {:?}",
        armed(&harness)
    );

    harness
        .send_key(KeyCode::Right, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(
        armed(&harness).contains("[ Discard and Quit ]"),
        "Right must arm the next button; row was {:?}",
        armed(&harness)
    );

    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        armed(&harness).contains("[ Save and Quit ]"),
        "Left must arm the previous one; row was {:?}",
        armed(&harness)
    );

    // Wrapping: one Left from the first lands on the last.
    harness.send_key(KeyCode::Left, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    assert!(
        armed(&harness).contains("[ Cancel ]"),
        "the selection wraps; row was {:?}",
        armed(&harness)
    );
}

/// Esc is the retreat, and the editor is still running afterwards.
#[test]
fn escape_cancels_and_leaves_the_editor_running() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    quit(&mut harness);
    harness.assert_screen_contains("Unsaved Changes");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(!harness.should_quit(), "Esc must not exit");
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Unsaved Changes"),
        "Esc must dismiss the dialog; screen still shows it"
    );
}

/// Enter takes the armed outcome. Right-Right-Enter from the top is
/// "Cancel" without hot exit — three keystrokes, all visible on screen.
#[test]
fn enter_takes_the_armed_outcome() {
    let _pin = pin();
    let mut config = Config::default();
    config.editor.hot_exit = false;
    let (mut harness, _file) = dirty_buffer(config);
    quit(&mut harness);

    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    let buttons = row_of(&harness, "Cancel").expect("the button row is on screen");
    assert!(harness.screen_row_text(buttons).contains("[ Cancel ]"));

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.should_quit(), "Enter on Cancel must not exit");
    assert!(!harness.screen_to_string().contains("Unsaved Changes"));
}

/// The single letters still work — the accelerator is the same key the
/// bottom-row prompt asked for, so nobody's muscle memory breaks.
#[test]
fn the_old_letter_answers_still_work() {
    let _pin = pin();
    let mut config = Config::default();
    config.editor.hot_exit = false;
    let (mut harness, file) = dirty_buffer(config);
    quit(&mut harness);

    harness
        .send_key(KeyCode::Char('d'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert!(
        harness.should_quit(),
        "`d` must still mean discard and quit"
    );
    // Discarded, so the file on disk is untouched.
    assert_eq!(std::fs::read_to_string(&file).unwrap(), "original\n");
}

/// Clicking a button is the answer. One press, not a select-then-confirm
/// two-step: these buttons already say what they do.
#[test]
fn clicking_a_button_answers_the_question() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    quit(&mut harness);

    let row = row_of(&harness, "Cancel").expect("the button row is on screen");
    let text = harness.screen_row_text(row);
    let col = text.find("Cancel").expect("Cancel is on that row") as u16;

    harness.mouse_click(col, row).unwrap();
    harness.render().unwrap();

    assert!(!harness.should_quit(), "clicking Cancel must not exit");
    assert!(
        !harness.screen_to_string().contains("Unsaved Changes"),
        "clicking Cancel must dismiss the dialog"
    );
}

/// A modal owns the keyboard: a key aimed at the buffer behind it must not
/// reach the buffer.
#[test]
fn the_buffer_behind_the_dialog_does_not_take_keys() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    let before = harness.get_buffer_content();
    quit(&mut harness);

    // `x` is nobody's accelerator here; it must be swallowed, not typed.
    harness
        .send_key(KeyCode::Char('x'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();

    assert_eq!(
        harness.get_buffer_content(),
        before,
        "a keystroke aimed past the modal must not land in the buffer"
    );
    harness.assert_screen_contains("Unsaved Changes");
}

/// Deleting from the file explorer opens on **Cancel**: the dialog can appear
/// right under the pointer, and an armed destructive button one Enter away is
/// the accident the modal exists to prevent.
#[test]
fn a_destructive_dialog_opens_on_its_retreat() {
    let _pin = pin();
    let mut harness = EditorTestHarness::with_temp_project(WIDTH, HEIGHT).unwrap();
    let dir = harness.project_dir().unwrap();
    let victim = dir.join("victim.txt");
    std::fs::write(&victim, "keep me\n").unwrap();
    harness.render().unwrap();

    harness.editor_mut().start_confirm_prompt(
        "body".to_string(),
        fresh::view::prompt::PromptType::ConfirmDeleteFile {
            path: victim.clone(),
            is_dir: false,
        },
        fresh::app::confirm_dialog::delete("Delete file 'victim.txt'?".to_string()),
    );
    harness.render().unwrap();

    let row = row_of(&harness, "Delete").expect("the dialog is on screen");
    let buttons = (0..HEIGHT)
        .find(|r| harness.screen_row_text(*r).contains("[ Cancel ]"))
        .unwrap_or(row);
    let text = harness.screen_row_text(buttons);
    assert!(
        text.contains("[ Cancel ]") && !text.contains("[ Delete ]"),
        "a delete dialog must open with Cancel armed; row was {text:?}"
    );
    assert!(victim.exists());
}

/// Regression: **Cancel must never be read as Discard**, in any locale.
///
/// The confirm handlers lower-case the answer they compare, and Czech ships
/// `prompt.key.discard = "z"` beside `prompt.key.cancel = "Z"` (Russian the
/// same with `о`/`О`). A Cancel button that sent its own letter would arrive
/// at `handle_confirm_quit_modified` as `"z"` and throw the user's work away
/// — the exact accident the dialog exists to prevent, made reachable by one
/// click. The button sends the empty answer instead, which every handler
/// reads as "do nothing"; the letter stays as the accelerator only.
#[test]
fn cancel_never_discards_even_where_the_locale_letters_collide() {
    let _pin = pin();

    let mut config = Config::default();
    config.editor.hot_exit = false;
    // Through the config, not `set_locale`: building the harness re-runs
    // `i18n::init_with_config`, which would put the locale back to the
    // config's before the first keystroke.
    config.locale = Some("cs").into();
    let (mut harness, file) = dirty_buffer(config);
    quit(&mut harness);
    // The dialog really is in Czech — otherwise the letters do not collide
    // and this test would pass without testing anything.
    harness.assert_screen_contains("Neuložené změny");

    // Esc resolves to the dialog's retreat, the same one the button sends.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    assert!(!harness.should_quit(), "cancelling must not exit");
    assert!(
        harness.editor().active_state().buffer.is_modified(),
        "cancelling must not discard the unsaved changes"
    );
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "original\n",
        "and must not have written anything either"
    );
}

/// A press outside the card is swallowed, not taken as an answer. The scrim
/// dims and blocks; only a button or Esc resolves the question.
#[test]
fn clicking_outside_the_card_does_not_answer() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());
    quit(&mut harness);

    // The top-left corner is the menu bar, well clear of the card.
    harness.mouse_click(2, 0).unwrap();
    harness.render().unwrap();

    assert!(!harness.should_quit());
    harness.assert_screen_contains("Unsaved Changes");
}

/// `Ctrl+G` on a buffer with no line index asks whether to scan. **Neither
/// button is a retreat** — one scans, the other opens the byte-offset prompt —
/// so Esc has to dismiss the question rather than resolve to the last button,
/// which is what it did when this was a row prompt.
#[test]
fn escape_dismisses_a_dialog_whose_every_button_does_something() {
    let _pin = pin();
    let (mut harness, _file) = dirty_buffer(Config::default());

    harness.editor_mut().start_goto_line_scan_confirm();
    harness.render().unwrap();
    harness.assert_screen_contains("Go to Line");
    harness.assert_screen_contains("Go to Byte Offset");

    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Go to Byte Offset"),
        "Esc must dismiss the question, not pick the last button"
    );
    assert!(
        !screen.contains("byte offset"),
        "and must not have opened the byte-offset prompt either. Screen:\n{screen}"
    );
}

/// The letters each prompt advertised stay live inside a modal that swallows
/// every key. `prompt.quit_confirm` said `(y)es, (N)o`, and its handler also
/// takes the `Action::Quit` letter.
#[test]
fn the_quit_confirmation_still_answers_to_yes() {
    let _pin = pin();
    let mut config = Config::default();
    config.editor.confirm_quit = true;
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(WIDTH, HEIGHT, config).expect("harness");
    harness.render().unwrap();

    quit(&mut harness);
    harness.assert_screen_contains("Quit Fresh");
    assert!(!harness.should_quit(), "the question blocks the exit");

    harness
        .send_key(KeyCode::Char('y'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(harness.should_quit(), "`y` was the advertised key");
}

/// Its `(N)o` half, and the default: a prompt that exists to catch a stray
/// `Ctrl+Q` must not have Quit armed one stray Enter later.
#[test]
fn the_quit_confirmation_opens_on_no_and_answers_to_it() {
    let _pin = pin();
    let mut config = Config::default();
    config.editor.confirm_quit = true;
    let mut harness =
        EditorTestHarness::with_temp_project_and_config(WIDTH, HEIGHT, config).expect("harness");
    harness.render().unwrap();

    quit(&mut harness);
    let row = (0..HEIGHT)
        .find(|r| harness.screen_row_text(*r).contains("Cancel"))
        .expect("the button row is on screen");
    assert!(
        harness.screen_row_text(row).contains("[ Cancel ]"),
        "a stray Ctrl+Q must not leave Quit armed; row was {:?}",
        harness.screen_row_text(row)
    );

    harness
        .send_key(KeyCode::Char('n'), KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    assert!(!harness.should_quit(), "`n` was the advertised key");
}
