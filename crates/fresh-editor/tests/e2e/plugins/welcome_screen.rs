//! E2E tests for the bundled `welcome_screen` plugin.
//!
//! The page is short: the latest release's "What's new" (compiled in from
//! `CHANGELOG.md`), a Nerd Font icons checkbox, and the essentials. These
//! drive the real plugin with keys and the mouse and assert on the screen
//! and on the saved config, plus the one lifecycle rule the plugin has:
//! open at startup as a tab behind whatever is there, and touch nothing
//! else.

use crate::common::harness::{copy_plugin, copy_plugin_lib, EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, PluginConfig};
use fresh::config_io::DirectoryContext;
use fresh::model::event::BufferId;
use std::fs;

const TAB_BAR_ROW: u16 = 1;

/// A heading the page always has, whatever the changelog holds.
const PAGE_MARKER: &str = "Essentials";

/// The Nerd Font glyph the file icons use for a `.rs` file (dev-rust).
const RUST_NERD_GLYPH: char = '\u{e7a8}';

/// A harness rooted in a scratch directory holding the real plugin, under
/// the caller's config, with its config directory inside the same scratch
/// directory so a test can read what the page saved.
fn harness_with_welcome_config(config: Config) -> (EditorTestHarness, tempfile::TempDir) {
    let temp = tempfile::TempDir::new().expect("tempdir");
    let working_dir = temp.path().join("work");
    fs::create_dir_all(&working_dir).unwrap();
    let plugins_dir = working_dir.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin(&plugins_dir, "welcome_screen");
    copy_plugin_lib(&plugins_dir);

    let dir_context = DirectoryContext::for_testing(&temp.path().join("state"));
    let harness = EditorTestHarness::create(
        120,
        40,
        HarnessOptions::new()
            .with_config(config)
            .with_working_dir(working_dir)
            .with_shared_dir_context(dir_context)
            .without_empty_plugins_dir(),
    )
    .expect("harness");
    (harness, temp)
}

fn harness_with_welcome() -> (EditorTestHarness, tempfile::TempDir) {
    harness_with_welcome_config(Config::default())
}

/// Bring the page to the front the way a reader does: startup adds the
/// tab behind the host's `[No Name]` seed, and the palette turns to it.
fn open_welcome(harness: &mut EditorTestHarness) {
    startup_welcome_tab(harness);
    harness.run_palette_command("Welcome").unwrap();
    harness
        .wait_until(|h| h.screen_to_string().contains(PAGE_MARKER))
        .expect("the welcome page renders");
    settle(harness);
}

/// The latest release in the repository's changelog, read the plain way:
/// the first `## x.y.z` heading and the bold lead of its first bullet.
fn changelog_latest() -> Option<(String, String)> {
    let text =
        fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/../../CHANGELOG.md")).ok()?;
    let mut lines = text.lines();
    let version = lines
        .by_ref()
        .find_map(|l| l.strip_prefix("## "))?
        .trim()
        .to_string();
    let first = lines.find_map(|l| l.strip_prefix("* **"))?;
    let lead = first.split("**").next()?.replace('`', "");
    Some((version, lead))
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
        !harness.screen_to_string().contains(PAGE_MARKER),
        "precondition: Escape closes the page"
    );

    // Now empty the workspace the way `Ctrl+W` on the last tab does.
    for id in harness.editor().all_buffer_ids_for_tests() {
        let _ = harness.editor_mut().close_buffer(id);
    }
    harness.wait_for_async_quiescence(4).unwrap();

    let screen = harness.screen_to_string();
    assert!(
        !screen.contains(PAGE_MARKER),
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
        !harness.screen_to_string().contains(PAGE_MARKER),
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
        .wait_until(|h| h.screen_to_string().contains(PAGE_MARKER))
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
        !screen.contains(PAGE_MARKER),
        "the welcome page took the foreground from a buffer that was \
         already open:\n{screen}"
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
        .wait_until(|h| h.screen_to_string().contains(PAGE_MARKER))
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

/// **The page is text you can quote.** Its mode used to inherit no normal
/// bindings, so `Shift+Right` — every shift-movement, in fact — reached
/// nothing at all, and `Ctrl+C` with it: a document the reader could read
/// and could not copy a line of. The mode inherits now, and the selection
/// the editor makes is described over the page, because the pane draws
/// this tree rather than the buffer under it.
#[test]
fn a_keyboard_selection_is_visible_and_copies() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
    let (col, row) = harness
        .find_text_on_screen("A terminal text editor")
        .expect("the tagline is on screen");
    harness.mouse_click(col, row).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    for _ in 0..8 {
        harness
            .send_key(KeyCode::Right, KeyModifiers::SHIFT)
            .unwrap();
    }
    harness.wait_for_async_quiescence(4).unwrap();

    let ground = harness.buffer()[(col.saturating_sub(2), row)].bg;
    let selected = harness.buffer()[(col + 3, row)].bg;
    assert_ne!(
        selected,
        ground,
        "the selected run is painted in the page's own ground: a selection \
         nobody can see. Screen:\n{}",
        harness.screen_to_string()
    );

    harness.editor_mut().set_clipboard_for_test(String::new());
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    assert_eq!(
        harness.editor_mut().clipboard_content_for_test(),
        "A termin",
        "Ctrl+C did not take the eight characters Shift+Right selected"
    );
}

/// **And swept with the pointer.** A described pane has no
/// screen-to-byte projection of its own — its content is the panel's
/// subtree, not the buffer's leaf — so the buffer's drag path could not
/// answer where the pointer is in the text. The page's own window can,
/// and the selection it makes is the buffer's, not a second model.
#[test]
fn dragging_across_the_page_selects_what_it_crossed() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
    let (col, row) = harness
        .find_text_on_screen("A terminal text editor")
        .expect("the tagline is on screen");

    harness.editor_mut().set_clipboard_for_test(String::new());
    harness.mouse_drag(col, row, col + 8, row).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    let ground = harness.buffer()[(col.saturating_sub(2), row)].bg;
    let selected = harness.buffer()[(col + 3, row)].bg;
    assert_ne!(
        selected,
        ground,
        "the dragged run is not lit. Screen:\n{}",
        harness.screen_to_string()
    );
    harness
        .send_key(KeyCode::Char('c'), KeyModifiers::CONTROL)
        .unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    assert_eq!(
        harness.editor_mut().clipboard_content_for_test(),
        "A termin",
        "the drag selected something other than what it crossed"
    );
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

/// Wait for the page to stop moving: two consecutive frames drawn the same.
///
/// The page's column is centred by a layout hint that lands a frame after
/// the first paint, so a test that reads a position off one frame and
/// clicks it on the next can land on the neighbouring character.
///
/// Bounded: a page that never settles should fail the test rather than
/// hang it.
fn settle(harness: &mut EditorTestHarness) {
    let mut previous = String::new();
    for _ in 0..40 {
        let seen = harness.screen_to_string();
        if seen == previous {
            return;
        }
        previous = seen;
        harness.wait_for_async_quiescence(1).unwrap();
    }
    panic!("the welcome page never stopped repainting. Screen:\n{previous}");
}

/// **What's new is the changelog's latest release.** The bullets are
/// compiled in from `CHANGELOG.md` at build time, so the page names the
/// version at the top of the changelog and lists its first change.
#[test]
fn whats_new_lists_the_changelogs_latest_release() {
    let Some((version, first_lead)) = changelog_latest() else {
        return; // A packaged build without the changelog has no list.
    };
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
    let screen = harness.screen_to_string();
    assert!(
        screen.contains(&format!("What's new in {version}")),
        "no \"What's new in {version}\" heading. Screen:\n{screen}"
    );
    assert!(
        screen.contains(&first_lead),
        "the changelog's first bullet ({first_lead:?}) is not listed. Screen:\n{screen}"
    );
    // It comes before the checkbox and the essentials.
    let row_of = |needle: &str| harness.find_text_on_screen(needle).map(|(_, r)| r);
    assert!(row_of("What's new") < row_of("Nerd Font icons"));
    assert!(row_of("Nerd Font icons") < row_of(PAGE_MARKER));
}

/// **The Nerd Font checkbox owns the real setting.** Ticking it writes
/// `editor.nerd_font_icons` to the user's config file, as Settings would,
/// and turns the icons on at once: the `.rs` tab gets its Rust glyph on
/// the next frame. Unticking turns them off again.
#[test]
fn the_nerd_font_checkbox_saves_the_setting_and_shows_the_icons() {
    let (mut harness, tmp) = harness_with_welcome();
    let rs = tmp.path().join("work").join("main.rs");
    fs::write(&rs, "fn main() {}\n").unwrap();
    harness.open_file(&rs).unwrap();
    open_welcome(&mut harness);

    let tab_glyph =
        |h: &EditorTestHarness| h.screen_row_text(TAB_BAR_ROW).contains(RUST_NERD_GLYPH);
    assert!(
        !harness.editor().config().editor.nerd_font_icons,
        "default is off"
    );
    assert!(
        !tab_glyph(&harness),
        "no Nerd Font glyph in the tabs while off"
    );
    assert!(
        harness.screen_to_string().contains("[ ] Nerd Font icons"),
        "the checkbox starts unticked. Screen:\n{}",
        harness.screen_to_string()
    );

    // The checkbox is the page's first control: Tab reaches it, Space ticks it.
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| h.editor().config().editor.nerd_font_icons)
        .expect("the setting is applied");
    harness
        .wait_until(|h| tab_glyph(h))
        .expect("the .rs tab shows its Nerd Font glyph once the box is ticked");
    assert!(
        !harness.screen_to_string().contains("[ ] Nerd Font icons"),
        "the checkbox still reads unticked. Screen:\n{}",
        harness.screen_to_string()
    );

    // The user config: the one config.json under the test's state dir.
    let saved =
        walk_for_config(&tmp.path().join("state")).expect("the page wrote the user's config file");
    let json: serde_json::Value = serde_json::from_str(&saved).unwrap();
    assert_eq!(
        json.pointer("/editor/nerd_font_icons"),
        Some(&serde_json::json!(true)),
        "the user config does not carry the setting:\n{saved}"
    );

    // And off again.
    harness
        .send_key(KeyCode::Char(' '), KeyModifiers::NONE)
        .unwrap();
    harness
        .wait_until(|h| !h.editor().config().editor.nerd_font_icons && !tab_glyph(h))
        .expect("unticking turns the icons off");
}

fn walk_for_config(dir: &std::path::Path) -> std::io::Result<String> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if path.is_dir() {
            if let Ok(s) = walk_for_config(&path) {
                return Ok(s);
            }
        } else if path.file_name().is_some_and(|n| n == "config.json") {
            return fs::read_to_string(path);
        }
    }
    Err(std::io::Error::other("no config.json"))
}

/// **The essentials do what they name.** Clicking `File explorer` toggles
/// the explorer; an action name nothing owns would fail only in the log.
#[test]
fn the_file_explorer_essential_opens_the_explorer() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
    let (col, row) = harness
        .find_text_on_screen("File explorer")
        .expect("the essentials list the explorer");
    let before = harness.screen_to_string().contains("File Explorer");
    harness.mouse_click(col + 2, row).unwrap();
    harness.wait_for_async_quiescence(4).unwrap();
    let after = harness.screen_to_string().contains("File Explorer");
    assert_ne!(
        before,
        after,
        "clicking `File explorer` did not toggle the explorer. Screen:\n{}",
        harness.screen_to_string()
    );
}

/// **The essentials carry the reader's keys**, resolved from the keymap
/// (the default one here) rather than left for the reader to look up.
#[test]
fn the_essentials_show_their_keybindings() {
    let (mut harness, _tmp) = harness_with_welcome();
    open_welcome(&mut harness);
    let palette_key = "Ctrl+P";
    let (_, row) = harness
        .find_text_on_screen("Command palette")
        .expect("the palette is the first essential");
    assert!(
        harness.screen_row_text(row).contains(palette_key),
        "the palette row lacks its key {palette_key:?}: {}",
        harness.screen_row_text(row)
    );
}
