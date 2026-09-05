//! E2E tests for the bundled `welcome_screen` plugin.
//!
//! This file exists because green CI observed *none* of the fourteen
//! rendering and interaction defects found by driving the real binary
//! by hand. Every one of them was a key going to the wrong place or a
//! row landing in the wrong column — nothing a compile, a lint or a
//! unit test in this repo looks at. The plugin e2e harness does look at
//! exactly that, and this plugin had no tests in it.
//!
//! So these assert behaviours that were *observed broken*, not the
//! happy path: what the finder does with the characters the page also
//! binds, where focus goes when the reader leaves it, and the one
//! lifecycle rule the plugin has — open at startup as a tab, and touch
//! nothing else.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use fresh::model::event::BufferId;
use std::fs;

const TAB_BAR_ROW: u16 = 1;

/// A harness rooted in a scratch directory holding the real plugin, under
/// the caller's config.
fn harness_with_welcome_config(config: Config) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "welcome_screen");
    copy_plugin_lib(&plugins_dir);

    let harness = EditorTestHarness::with_config_and_working_dir(120, 40, config, working_dir)
        .expect("harness");
    (harness, temp)
}

fn harness_with_welcome() -> (EditorTestHarness, tempfile::TempDir) {
    harness_with_welcome_config(Config::default())
}

/// Fire startup and return the one buffer it added.
fn startup_welcome_tab(harness: &mut EditorTestHarness) -> BufferId {
    let before = harness.editor().all_buffer_ids_for_tests();
    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();
    harness
        .editor()
        .all_buffer_ids_for_tests()
        .into_iter()
        .find(|id| !before.contains(id))
        .expect("startup opened a Welcome buffer")
}

/// Bring the page to the front the way a reader does: startup adds the
/// tab behind the host's `[No Name]` seed, and the palette turns to it.
fn open_welcome(harness: &mut EditorTestHarness) {
    startup_welcome_tab(harness);
    harness.run_palette_command("Welcome").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("JUST EDIT TEXT"))
        .expect("welcome screen renders its three doors");
}

/// The page renders, at the width the design is drawn for.
#[test]
fn welcome_screen_renders_its_three_doors() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
    let screen = harness.screen_to_string();
    for door in ["JUST EDIT TEXT", "CLASSIC IDE", "ORCHESTRATE"] {
        assert!(
            screen.contains(door),
            "door {door:?} missing from:\n{screen}"
        );
    }
}

/// **The precedence bug.** `0`-`3`, `/` and Space are bound by this
/// page's mode *and* are ordinary characters. Mode bindings used to
/// resolve first, so none of them reached the finder: typing `1` jumped
/// to Level 1, `/` vanished so `src/main` typed as `srcmain`, and Space
/// opened whichever result was marked. Between them that is every
/// digit, the one separator every path contains, and the space.
///
/// The host now gives a focused text widget the key first. This types a
/// string containing all three kinds and asserts the field got them.
#[test]
fn the_finder_receives_the_characters_the_page_also_binds() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    // `/` focuses the finder — the one use of the key that is not typing.
    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("find ["))
        .expect("the finder field takes focus");

    for ch in "src/main1 x".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .wait_until(|h| h.screen_to_string().contains("src/main1 x"))
        .expect(
            "every character reaches the field: the slash (a path separator, \
             not a re-focus), the digit (not a level jump) and the space \
             (not an activation)",
        );
}

/// **The focus bug.** Leaving the finder clears focus — and the host
/// used to re-seed it onto the first tabbable widget on the next
/// repaint, which on this page is the "Show this screen on startup"
/// switch. It is off screen by then, so the next Space silently turned
/// the page off: a persisted setting changed with nothing to say why.
///
/// `autoFocusFirst: false` makes "nothing focused" a real state. After
/// Escape, Space must reach no widget at all.
#[test]
fn leaving_the_finder_does_not_park_focus_on_the_startup_switch() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    harness
        .send_key(KeyCode::Char('/'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("find ["))
        .expect("the finder field takes focus");

    // Escape leaves the field (it does not close the page from here).
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();

    // The switch's own confirmation is the tell: it says so when it
    // changes, precisely because a setting must not change silently.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Welcome: hidden"),
        "Space after leaving the finder toggled the startup switch — focus \
         was re-seeded onto it. Screen:\n{screen}"
    );
}

/// **The reopen loop.** The page used to read `buffer_closed` on an
/// emptied workspace as an invitation, on the reasoning that "launched
/// with nothing" and "left with nothing" are the same state. They are
/// not: closing your last file is something you asked for, and getting a
/// full-page document back instead of an empty pane reads as the editor
/// undoing the close — with no way left to say "close everything". It is
/// a startup surface now, and this is the difference.
#[test]
fn closing_the_last_buffer_does_not_reopen_the_welcome_screen() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    // Escape closes the page: the reader's own dismissal.
    harness.send_key(KeyCode::Esc, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    assert!(
        !harness.screen_to_string().contains("JUST EDIT TEXT"),
        "precondition: Escape closes the page"
    );

    // Now empty the workspace the way `Ctrl+W` on the last tab does.
    for id in harness.editor().all_buffer_ids_for_tests() {
        let _ = harness.editor_mut().close_buffer(id);
    }
    harness.wait_for_async_quiescence(4).unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("JUST EDIT TEXT"),
        "closing the last buffer summoned the welcome screen:\n{screen}"
    );
}

/// **A bare start adds a tab and changes nothing else.** The host seeds
/// one untitled `[No Name]` buffer at launch; the page appears beside it
/// and leaves it the pane and the focus. The page used to close that seed,
/// then to close it only under one setting — and each rule needed the
/// plugin to know what the seed *meant*. It no longer asks.
#[test]
fn a_bare_startup_adds_a_welcome_tab_behind_the_empty_buffer() {
    let (mut harness, _tmp) = harness_with_welcome();
    let before = harness.editor().all_buffer_ids_for_tests();
    let active_before = harness.editor().active_buffer_id();
    assert_eq!(before.len(), 1, "precondition: the host seeds one buffer");

    startup_welcome_tab(&mut harness);

    let after = harness.editor().all_buffer_ids_for_tests();
    for id in &before {
        assert!(
            after.contains(id),
            "the welcome screen closed buffer {id:?}: {before:?} became {after:?}"
        );
    }
    assert_eq!(
        harness.editor().active_buffer_id(),
        active_before,
        "the page took the focus from the reader's empty buffer"
    );
    let tab_bar = harness.screen_row_text(TAB_BAR_ROW);
    assert!(
        tab_bar.contains("[No Name]") && tab_bar.contains("Welcome"),
        "expected `[No Name]` and a Welcome tab side by side, got: {tab_bar}"
    );
    assert!(
        !harness.screen_to_string().contains("JUST EDIT TEXT"),
        "the page is in the foreground of a workspace it should have \
         joined quietly"
    );
}

/// **The empty-buffer setting is not this plugin's business.** With
/// `auto_create_empty_buffer_on_last_buffer_close` off, the page used to
/// read the seed as a placeholder and close it. Whatever the host does
/// with its own seed is the host's call; the page does exactly what it
/// does under the default.
#[test]
fn the_empty_buffer_setting_changes_nothing_about_startup() {
    let mut config = Config::default();
    config.editor.auto_create_empty_buffer_on_last_buffer_close = false;
    let (mut harness, _tmp) = harness_with_welcome_config(config);
    let before = harness.editor().all_buffer_ids_for_tests();
    let active_before = harness.editor().active_buffer_id();

    let welcome = startup_welcome_tab(&mut harness);

    let after = harness.editor().all_buffer_ids_for_tests();
    for id in &before {
        assert!(
            after.contains(id),
            "the welcome screen closed buffer {id:?} because of a setting \
             that is not its own: {before:?} became {after:?}"
        );
    }
    assert!(after.contains(&welcome));
    assert_eq!(
        harness.editor().active_buffer_id(),
        active_before,
        "startup changed which buffer is active"
    );
}

/// **`showOnStartup` is the one switch, and off means off.** Nothing
/// opens at launch; `Welcome` in the palette still works.
#[test]
fn show_on_startup_off_opens_nothing_until_asked() {
    let mut config = Config::default();
    config.plugins.insert(
        "welcome_screen".to_string(),
        PluginConfig {
            enabled: true,
            path: None,
            settings: serde_json::json!({ "showOnStartup": false }),
        },
    );
    let (mut harness, _tmp) = harness_with_welcome_config(config);
    let before = harness.editor().all_buffer_ids_for_tests();

    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();
    assert_eq!(
        harness.editor().all_buffer_ids_for_tests(),
        before,
        "the page opened at startup with showOnStartup off"
    );

    harness.run_palette_command("Welcome").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("JUST EDIT TEXT"))
        .expect("the palette command still opens the page");
}

/// **Startup means startup — not "startup that found an empty
/// workspace".** The page asked whether anything else was open and gave
/// up if anything was, so restoring a session, or plain `fresh
/// note.txt`, meant never seeing it again: the only way back was to
/// close every buffer and relaunch. Opening is unconditional now.
#[test]
fn startup_with_a_file_already_open_still_gets_a_welcome_tab() {
    let (mut harness, tmp) = harness_with_welcome();
    let path = tmp.path().join("work").join("note.txt");
    fs::write(&path, "alpha\nbeta\n").unwrap();
    harness.open_file(&path).unwrap();

    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();

    let screen = harness.screen_to_string();
    assert!(
        screen.contains("Welcome"),
        "no Welcome tab: the page skipped startup because the workspace \
         was not empty. Screen:\n{screen}"
    );
}

/// The other half of the same rule: a workspace that already has
/// something in it keeps looking at it. The page is a tab you can turn
/// to, not a thing that lands on top of the file you asked for — and
/// not one that lands on top of it and then gets out of the way again,
/// which is what opening-then-switching-back looked like: the tab bar
/// appeared without the page, the page took the pane, and the file came
/// back. Creation itself now leaves the active buffer alone
/// (`background` on `createVirtualBuffer`), so there is no moment in
/// between to see.
#[test]
fn a_welcome_tab_beside_a_file_does_not_take_the_foreground() {
    let (mut harness, tmp) = harness_with_welcome();
    let path = tmp.path().join("work").join("note.txt");
    fs::write(&path, "alpha\nbeta\n").unwrap();
    harness.open_file(&path).unwrap();
    let active_before = harness.editor().active_buffer_id();

    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();

    assert_eq!(
        harness.editor().active_buffer_id(),
        active_before,
        "startup changed which buffer is active"
    );
    let screen = harness.screen_to_string();
    assert!(
        screen.contains("alpha"),
        "the file the reader asked for is no longer on screen:\n{screen}"
    );
    assert!(
        !screen.contains("JUST EDIT TEXT"),
        "the welcome page took the foreground from a buffer that was \
         already open:\n{screen}"
    );
}

/// **A page composed off screen composed against the wrong pane.** The
/// layout is measured from `getViewport()`, which reports the *active*
/// split — so a page opened behind a file took that file's pane geometry
/// and, once the reader switched to it, painted at a measure the pane
/// could not hold: the wordmark wrapped mid-glyph and every centred row
/// sat far right. `viewport_changed` could not save it either, because
/// the stale key it recorded while hidden equals the key it computes
/// when shown. Bringing the page to the front repaints it once.
#[test]
fn a_welcome_tab_opened_behind_a_file_paints_correctly_when_shown() {
    let (mut harness, tmp) = harness_with_welcome();
    let path = tmp.path().join("work").join("note.txt");
    fs::write(&path, "alpha\nbeta\n").unwrap();
    harness.open_file(&path).unwrap();

    let welcome = startup_welcome_tab(&mut harness);

    // Switch to the Welcome tab, the way `Ctrl+PageDown` does.
    harness.editor_mut().switch_buffer(welcome);
    harness.wait_for_async_quiescence(4).unwrap();

    // The three doors sit side by side on one row at this width. If the
    // page composed against a wider pane they wrap and the row breaks up.
    let screen = harness.screen_to_string();
    let doors = screen
        .lines()
        .find(|l| l.contains("JUST EDIT TEXT"))
        .unwrap_or_default()
        .to_string();
    assert!(
        doors.contains("CLASSIC IDE") && doors.contains("ORCHESTRATE"),
        "the page painted at a measure its pane cannot hold — the three \
         doors are no longer one row. Screen:\n{screen}"
    );
}

/// **Opening a file never closes the tab.** The page used to "step aside"
/// for a file when nobody had touched it, which took an engagement score
/// and a was-it-ever-shown flag to get even half right, and still deleted
/// a tab the reader had not asked to close. Here the reader has never
/// turned to it.
#[test]
fn opening_a_file_leaves_a_welcome_tab_the_reader_has_not_seen() {
    let (mut harness, tmp) = harness_with_welcome();
    let first = tmp.path().join("work").join("note.txt");
    fs::write(&first, "alpha\n").unwrap();
    harness.open_file(&first).unwrap();

    let welcome = startup_welcome_tab(&mut harness);

    let second = tmp.path().join("work").join("other.txt");
    fs::write(&second, "beta\n").unwrap();
    harness.open_file(&second).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    assert!(
        harness
            .editor()
            .all_buffer_ids_for_tests()
            .contains(&welcome),
        "opening a file closed a Welcome tab the reader had never seen"
    );
}

/// The same rule for a page the reader *has* turned to and not touched —
/// exactly the case the old step-aside rule fired on. It stays; the
/// reader closes tabs, not the plugin.
#[test]
fn opening_a_file_leaves_a_welcome_page_the_reader_was_looking_at() {
    let (mut harness, tmp) = harness_with_welcome();
    let first = tmp.path().join("work").join("note.txt");
    fs::write(&first, "alpha\n").unwrap();
    harness.open_file(&first).unwrap();

    let welcome = startup_welcome_tab(&mut harness);
    // Turn to the tab the way `Ctrl+PageDown` does — not the palette,
    // which the old rule counted as "engagement" and spared.
    harness.editor_mut().switch_buffer(welcome);
    harness
        .wait_until(|h| h.screen_to_string().contains("JUST EDIT TEXT"))
        .expect("switching to the tab shows the page");

    let second = tmp.path().join("work").join("other.txt");
    fs::write(&second, "beta\n").unwrap();
    harness.open_file(&second).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    assert!(
        harness
            .editor()
            .all_buffer_ids_for_tests()
            .contains(&welcome),
        "the page the reader was looking at was closed by their next file \
         open, as if the plugin still stepped aside"
    );
}

/// **Tab moves the caret, not just a highlight.** The page is a document
/// with a real caret in it, so focus and the caret are two answers to
/// one question — what am I looking at. They used to be able to
/// disagree: Tab advanced focus down the page while the caret stayed on
/// the top row, and the reader was left with two "you are here" markers
/// pointing at different things.
///
/// The first Tab stop is the startup switch on the page's own first
/// row, where the caret already is, so it is the *second* Tab — onto the
/// first door card — that has a move to make.
#[test]
fn tab_brings_the_caret_to_the_control_it_focused() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    for _ in 0..2 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    }
    harness.wait_for_async_quiescence(4).unwrap();

    let (_, door_row) = harness
        .find_text_on_screen("Open a file & go")
        .expect("the first door card's own row is on screen");
    let caret = harness
        .render_observing_cursor()
        .unwrap()
        .expect("the page draws a caret");
    assert_eq!(
        caret.1,
        door_row,
        "Tab focused the first door and left the caret behind on row {} \
         instead of the card's own row {}. Screen:\n{}",
        caret.1,
        door_row,
        harness.screen_to_string()
    );
}

/// **The other direction: moving the caret disarms whatever Tab left
/// focused.** A click on a paragraph is the reader saying "I am reading
/// here"; it used to leave the last Tab's target focused — possibly off
/// screen — so the next Space or Enter fired a control they could not
/// see. Here that control is the one that changes a persisted setting,
/// which is exactly the case where a silent activation is worst.
#[test]
fn moving_the_caret_onto_prose_disarms_the_focused_control() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    // Tab once: the startup switch takes focus.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    // Click the tagline — prose, carrying no control of any kind.
    let (col, row) = harness
        .find_text_on_screen("It grows when your work does")
        .expect("the tagline is on screen");
    harness.mouse_click(col + 2, row).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    // The switch says so when it changes, precisely because a persisted
    // setting must not change silently.
    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Welcome: hidden"),
        "Space after clicking on prose toggled the startup switch — the \
         caret moved and focus stayed behind. Screen:\n{screen}"
    );
    assert!(
        screen.contains("[✓] Show this screen on startup"),
        "the startup switch is no longer on: Space reached it from a \
         caret parked on a paragraph. Screen:\n{screen}"
    );
}

/// **And the caret arriving at an editable field puts you in it.**
/// Walking down the page with an arrow key onto the finder's input means
/// the next character typed goes into the finder — no Tab count, no `/`,
/// nothing to know. The caret is on the field, so the field has focus.
#[test]
fn the_caret_arriving_at_the_finder_field_types_into_it() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    // `1` puts the Level 1 banner at the top of the pane, with the caret
    // on it — the finder card is a few rows below, in the same viewport.
    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("find ["))
        .expect("the finder card is on screen");

    let (_, field_row) = harness
        .find_text_on_screen("find [")
        .expect("the finder field is on screen");
    let caret_row = harness
        .render_observing_cursor()
        .unwrap()
        .expect("the page draws a caret")
        .1;
    assert!(
        caret_row < field_row,
        "expected the caret above the finder field, got caret row \
         {caret_row} and field row {field_row}"
    );
    // Counted from what is on screen rather than from the card's
    // structure: how many rows a framed card spends above its first
    // control is the layout's business, not this test's.
    for _ in 0..(field_row - caret_row) {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.wait_for_async_quiescence(4).unwrap();

    for ch in "zq".chars() {
        harness
            .send_key(KeyCode::Char(ch), KeyModifiers::NONE)
            .unwrap();
    }

    harness
        .wait_until(|h| h.screen_to_string().contains("zq"))
        .expect(
            "the characters went nowhere: arriving at the field with the \
             caret did not give it focus",
        );
}

/// **A focus region is a control, not a row.** The three door cards sit
/// side by side on the same buffer rows, so a caret row cannot say which
/// of them the reader is on — only a column can. Resolving focus by row
/// also made Tab between two controls of one row impossible: the move to
/// the second seated the caret on the row they share, and the row handed
/// focus straight back to the first.
#[test]
fn tab_between_two_cards_on_one_row_carries_the_caret_across_it() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    // Two Tabs: past the startup switch, onto the first door.
    for _ in 0..2 {
        harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    }
    harness.wait_for_async_quiescence(4).unwrap();
    let first = harness
        .render_observing_cursor()
        .unwrap()
        .expect("the page draws a caret");

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    let second = harness
        .render_observing_cursor()
        .unwrap()
        .expect("the page draws a caret");

    assert_eq!(
        second.1, first.1,
        "the two doors share a row, so the caret should not have changed rows"
    );
    assert!(
        second.0 > first.0,
        "the caret stayed at column {} moving from the first door to the \
         second: focus is still resolved a whole row at a time. Screen:\n{}",
        first.0,
        harness.screen_to_string()
    );

    // And focus really is on the second door — its activation jumps to
    // Level 2. If the row had handed focus back to the first door this
    // would land on Level 1 instead.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("LEVEL 2 · IT'S A PROJECT NOW")
        })
        .expect("Enter on the second door opens Level 2");
}

/// **Tab from nothing focused starts beside the caret, not at the top of
/// the page.** Focus is cleared every time the caret lands on prose,
/// which on this page is most rows — so a Tab ring that always restarted
/// at its first entry would yank a reader who had scrolled to Level 3
/// back to the startup switch.
#[test]
fn tab_after_reading_down_the_page_does_not_jump_back_to_the_top() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    // `3` puts the Level 3 banner at the top of the pane, with the caret
    // on it — a banner is not a Tab stop, so nothing is focused.
    harness
        .send_key(KeyCode::Char('3'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| {
            h.screen_to_string()
                .contains("LEVEL 3 · RUN THE WHOLE SHOP")
        })
        .expect("the page scrolls to Level 3");

    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains("Show this screen on startup"),
        "Tab from Level 3 went back to the first control on the page. \
         Screen:\n{screen}"
    );
    assert!(
        screen.contains("The Orchestrator dock"),
        "Tab left Level 3 entirely. Screen:\n{screen}"
    );
}

/// **A banner and its opening sentence are two things.** Set tight, the
/// rule and the line under it read as one two-line heading — the
/// sentence looked like a subtitle of the rule rather than the first
/// line of the level it introduces.
#[test]
fn a_level_banner_has_air_before_its_description() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);

    harness
        .send_key(KeyCode::Char('1'), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("LEVEL 1 · JUST EDIT"))
        .expect("the Level 1 banner is on screen");

    let (_, rule_row) = harness
        .find_text_on_screen("LEVEL 1 · JUST EDIT")
        .expect("the Level 1 banner is on screen");
    let gap = harness.screen_row_text(rule_row + 1);
    let description = harness.screen_row_text(rule_row + 2);
    assert!(
        gap.trim().is_empty(),
        "no air between the Level 1 rule and its description — row \
         {} reads {gap:?}",
        rule_row + 1
    );
    assert!(
        description.contains("Open a file. Type. Save."),
        "the Level 1 description is not two rows under its rule — row \
         {} reads {description:?}. Screen:\n{}",
        rule_row + 2,
        harness.screen_to_string()
    );
}
