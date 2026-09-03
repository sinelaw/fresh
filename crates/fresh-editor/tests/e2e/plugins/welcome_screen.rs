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
//! binds, and where focus goes when the reader leaves it.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::Config;
use std::fs;

const TAB_BAR_ROW: u16 = 1;

/// A harness rooted in a scratch directory holding the real plugin.
///
/// `seed_wanted` is `editor.auto_create_empty_buffer_on_last_buffer_close`,
/// the setting the page reads to decide what the host's untitled seed
/// means. Most tests here want the page in the foreground of a bare start,
/// which is the *off* case: the seed is a placeholder the page replaces.
fn harness_with_welcome_seed(seed_wanted: bool) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "welcome_screen");
    copy_plugin_lib(&plugins_dir);

    let mut config = Config::default();
    config.editor.auto_create_empty_buffer_on_last_buffer_close = seed_wanted;
    let harness = EditorTestHarness::with_config_and_working_dir(120, 40, config, working_dir)
        .expect("harness");
    (harness, temp)
}

/// The common case for these tests: the reader wants no empty buffer, so a
/// bare start lands on the page.
fn harness_with_welcome() -> (EditorTestHarness, tempfile::TempDir) {
    harness_with_welcome_seed(false)
}

/// Bring the page up the way a bare `fresh` does.
fn open_welcome(harness: &mut EditorTestHarness) {
    harness.editor_mut().fire_ready_hook();
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

/// The startup path is the one that survives: a launch with nothing to
/// restore still brings the page up. Guards the fix above against being
/// "fixed" into a page that never opens by itself at all.
#[test]
fn an_empty_startup_still_opens_the_welcome_screen() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
}

/// **Startup means startup — not "startup that found an empty
/// workspace".** The page asked whether anything else was open and gave
/// up if anything was, so restoring a session, or plain `fresh
/// note.txt`, meant never seeing it again: the only way back was to
/// close every buffer and relaunch. Opening is now unconditional. What a
/// non-empty workspace changes is only whether the page comes to the
/// front, which is the next test.
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

/// **When the reader wants an empty buffer, the page touches nothing.**
/// With `auto_create_empty_buffer_on_last_buffer_close` on, the host's
/// `[No Name]` seed is the reader's scratch buffer: it keeps the pane and
/// the focus, and the page is a tab beside it. Closing it — as the page
/// once did unconditionally — would be a plugin deleting the very buffer
/// the reader's setting asked for.
#[test]
fn a_workspace_that_wants_an_empty_buffer_keeps_it_focused() {
    let (mut harness, _tmp) = harness_with_welcome_seed(true);
    // The harness draws no tab bar for a lone buffer, so the seed is
    // observed by id, not by its `[No Name]` label.
    let before = harness.editor().all_buffer_ids_for_tests();
    let active_before = harness.editor().active_buffer_id();
    assert_eq!(before.len(), 1, "precondition: the host seeds one buffer");

    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();

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
        "the page is in the foreground of a workspace whose reader asked \
         for an empty buffer"
    );
}

/// **When the reader wants no empty buffer, the page replaces the seed.**
/// With the setting off, Fresh still seeds one untitled buffer at launch;
/// leaving it beside the page is a `[No Name]` tab the reader explicitly
/// asked never to see. The page closes it and takes the pane — the one
/// buffer this plugin will ever close, and only here.
#[test]
fn a_workspace_that_wants_no_empty_buffer_gets_only_the_welcome_page() {
    let (mut harness, _tmp) = harness_with_welcome_seed(false);
    let seed = harness.editor().all_buffer_ids_for_tests();
    assert_eq!(
        seed.len(),
        1,
        "precondition: the host seeds one buffer regardless of the setting"
    );

    open_welcome(&mut harness);

    let after = harness.editor().all_buffer_ids_for_tests();
    assert!(
        !after.contains(&seed[0]),
        "the seed survived a start the reader asked to be empty: {seed:?} -> {after:?}"
    );
    assert_eq!(
        after.len(),
        1,
        "expected the page to be the only buffer: {after:?}"
    );
    assert!(
        !harness.screen_to_string().contains("[No Name]"),
        "a `[No Name]` label is still on screen"
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

    let before = harness.editor().all_buffer_ids_for_tests();
    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();

    // Switch to the Welcome tab, the way `Ctrl+PageDown` does. It is the
    // one buffer startup added.
    let welcome = harness
        .editor()
        .all_buffer_ids_for_tests()
        .into_iter()
        .find(|id| !before.contains(id))
        .expect("startup opened a Welcome buffer");
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

/// **The startup tab did not survive being ignored.** `engaged` is only
/// set by a keystroke or click *on the page*, and the step-aside rule
/// closes an unengaged page when a file opens — a rule written for a page
/// occupying the pane. The background tab never occupies anything, so the
/// reader's first `Ctrl+P` deleted a tab they had not yet seen, and the
/// only way back was a palette command that produced another one just as
/// short-lived.
#[test]
fn a_welcome_tab_the_reader_has_not_seen_survives_their_first_file_open() {
    let (mut harness, tmp) = harness_with_welcome();
    let first = tmp.path().join("work").join("note.txt");
    fs::write(&first, "alpha\n").unwrap();
    harness.open_file(&first).unwrap();

    let before = harness.editor().all_buffer_ids_for_tests();
    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();
    let welcome = harness
        .editor()
        .all_buffer_ids_for_tests()
        .into_iter()
        .find(|id| !before.contains(id))
        .expect("startup opened a Welcome buffer");

    // The reader opens something. The page is not in their way — it has
    // never been on screen — so it has nothing to step aside from.
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

/// **Summoning the page counts as engagement.** `openWelcome`'s
/// already-open path brings the buffer forward and used to leave
/// `engaged` false — and since startup now always creates the buffer,
/// that is the path `Welcome` takes for the rest of the session. The
/// reader asked for the page by name, read it, opened a file, and the
/// step-aside rule destroyed it as if nobody had touched it.
#[test]
fn asking_for_the_page_by_name_keeps_it_through_the_next_file_open() {
    let (mut harness, tmp) = harness_with_welcome();
    let first = tmp.path().join("work").join("note.txt");
    fs::write(&first, "alpha\n").unwrap();
    harness.open_file(&first).unwrap();

    let before = harness.editor().all_buffer_ids_for_tests();
    harness.editor_mut().fire_ready_hook();
    harness.wait_for_async_quiescence(4).unwrap();
    let welcome = harness
        .editor()
        .all_buffer_ids_for_tests()
        .into_iter()
        .find(|id| !before.contains(id))
        .expect("startup opened a Welcome buffer");

    harness.run_palette_command("Welcome").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains("JUST EDIT TEXT"))
        .expect("the palette command brings the page forward");

    let second = tmp.path().join("work").join("other.txt");
    fs::write(&second, "beta\n").unwrap();
    harness.open_file(&second).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();

    assert!(
        harness
            .editor()
            .all_buffer_ids_for_tests()
            .contains(&welcome),
        "the page the reader summoned by name was closed by their next \
         file open, as if it had been ambient"
    );
}
