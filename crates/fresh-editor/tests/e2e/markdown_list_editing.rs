//! End-to-end tests for Markdown list editing and language edit rules.
//!
//! Markdown's list editing is its built-in `edit_rules` setting
//! (`languages.markdown.edit_rules`), which the editor applies inside
//! Enter / Tab / Shift+Tab:
//! - Enter continues list items (bullets, ordered, checkboxes)
//! - Enter on an empty list item removes the marker
//! - Tab on a blank list item indents and cycles the bullet; elsewhere Tab
//!   is the normal Tab
//! - The rules apply to Markdown buffers only, and keep up with keys sent
//!   back to back (#3813)
//! - They follow the language at the cursor: inside a fenced code block the
//!   block's language's rules and indentation apply, not Markdown's
//!
//! Any language can have rules in config.json (`[]` turns them off), and
//! plugins can register more (`registerEditRules`), tried after the config's.
//!
//! Most tests are cases of one shape: open a file holding some text, run a
//! list of [`Step`]s (keys, typing, extra cursors, checks of the buffer), as
//! [`md_cases!`] and [`fence_cases!`] spell them. Each case is its own
//! `#[test]`. Tests with setup of their own are written out below them.

use crate::common::fixtures::TestFixture;
use crate::common::harness::EditorTestHarness;
use crate::common::tracing::init_tracing_from_env;
use crossterm::event::{KeyCode, KeyModifiers};
use std::fs;
use std::path::PathBuf;
use Content::*;
use Step::*;

// ---------------------------------------------------------------------------
// Harnesses, steps and the case tables
// ---------------------------------------------------------------------------

/// Create a harness in a fresh project directory, with the built-in
/// config: Markdown's list editing needs no plugin.
fn markdown_harness(width: u16, height: u16) -> EditorTestHarness {
    markdown_harness_with_config(width, height, Default::default())
}

/// [`markdown_harness`] with a given config.
fn markdown_harness_with_config(
    width: u16,
    height: u16,
    config: fresh::config::Config,
) -> EditorTestHarness {
    init_tracing_from_env();
    EditorTestHarness::with_temp_project_and_config(width, height, config).unwrap()
}

/// Helper: open a markdown file. The rules need no activation: they apply
/// from the first key.
fn open_md(harness: &mut EditorTestHarness, path: &std::path::Path) {
    harness.open_file(path).unwrap();
    harness.render().unwrap();
}

/// Write `name` holding `content` in the harness's project, open it and
/// draw it.
fn open_project_file(harness: &mut EditorTestHarness, name: &str, content: &str) -> PathBuf {
    let path = harness.project_dir().unwrap().join(name);
    fs::write(&path, content).unwrap();
    harness.open_file(&path).unwrap();
    harness.render().unwrap();
    path
}

/// Open `name` holding `content`, press End then Enter, and return the text.
fn enter_at_end_of_first_line(
    harness: &mut EditorTestHarness,
    name: &str,
    content: &str,
) -> String {
    let fixture = TestFixture::new(name, content).unwrap();
    open_md(harness, &fixture.path);
    run_steps(harness, &[END, ENTER]);
    harness.get_buffer_content().unwrap()
}

/// Put the cursor at the end of line `line` (0-based).
fn end_of_line(harness: &mut EditorTestHarness, line: usize) {
    harness
        .send_key(KeyCode::Home, KeyModifiers::CONTROL)
        .unwrap();
    for _ in 0..line {
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
    }
    harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
}

/// What a [`Step`] checks the buffer's text for.
#[derive(Clone, Copy, Debug)]
enum Content {
    Is(&'static str),
    StartsWith(&'static str),
    Contains(&'static str),
    /// At least this many lines.
    LinesAtLeast(usize),
    /// This character, exactly this many times.
    Count(char, usize),
}

impl Content {
    fn matches(self, text: &str) -> bool {
        match self {
            Is(want) => text == want,
            StartsWith(want) => text.starts_with(want),
            Contains(want) => text.contains(want),
            LinesAtLeast(n) => text.lines().count() >= n,
            Count(c, n) => text.matches(c).count() == n,
        }
    }
}

/// One step of a case.
#[derive(Clone, Copy, Debug)]
enum Step {
    /// A key, as `send_key` delivers it (it renders afterwards).
    Key(KeyCode, KeyModifiers),
    /// Text, as `type_text` delivers it.
    Type(&'static str),
    /// Add a cursor below the last one, with no frame drawn after it.
    AddCursorBelow,
    /// Put the cursor at the end of this line (0-based): [`end_of_line`].
    EndOfLine(usize),
    /// Process async messages and render, three times over.
    Drain,
    /// Wait until the buffer matches.
    Wait(Content),
    /// The buffer matches now, with no waiting.
    Expect(Content),
    /// The active buffer's language is this.
    Language(&'static str),
}

const END: Step = Key(KeyCode::End, KeyModifiers::NONE);
const HOME: Step = Key(KeyCode::Home, KeyModifiers::NONE);
const DOWN: Step = Key(KeyCode::Down, KeyModifiers::NONE);
const RIGHT: Step = Key(KeyCode::Right, KeyModifiers::NONE);
const ENTER: Step = Key(KeyCode::Enter, KeyModifiers::NONE);
const TAB: Step = Key(KeyCode::Tab, KeyModifiers::NONE);
const BACK_TAB: Step = Key(KeyCode::BackTab, KeyModifiers::SHIFT);
const CTRL_HOME: Step = Key(KeyCode::Home, KeyModifiers::CONTROL);
const SHIFT_DOWN: Step = Key(KeyCode::Down, KeyModifiers::SHIFT);
const SHIFT_END: Step = Key(KeyCode::End, KeyModifiers::SHIFT);
/// Add a cursor below (Ctrl+Alt+Down).
const CTRL_ALT_DOWN: Step = Key(
    KeyCode::Down,
    KeyModifiers::CONTROL.union(KeyModifiers::ALT),
);
const UNDO: Step = Key(KeyCode::Char('z'), KeyModifiers::CONTROL);

/// Run `steps` against the active buffer.
fn run_steps(harness: &mut EditorTestHarness, steps: &[Step]) {
    for &step in steps {
        match step {
            Key(code, modifiers) => harness.send_key(code, modifiers).unwrap(),
            Type(text) => harness.type_text(text).unwrap(),
            AddCursorBelow => harness.editor_mut().add_cursor_below(),
            EndOfLine(line) => end_of_line(harness, line),
            Drain => {
                for _ in 0..3 {
                    harness.process_async_and_render().unwrap();
                }
            }
            Wait(want) => harness
                .wait_until(|h| h.get_buffer_content().is_some_and(|c| want.matches(&c)))
                .unwrap(),
            Expect(Is(want)) => harness.assert_buffer_content(want),
            Expect(want) => {
                let content = harness.get_buffer_content().unwrap();
                assert!(
                    want.matches(&content),
                    "the buffer does not match {want:?}: {content:?}"
                );
            }
            Language(language) => {
                assert_eq!(harness.editor().active_state().language, language)
            }
        }
    }
}

/// A Markdown harness with `config`, `name` holding `content` open in it,
/// and `steps` run; then no plugin errors.
fn run_md(config: fresh::config::Config, name: &str, content: &str, steps: &[Step]) {
    let mut harness = markdown_harness_with_config(80, 24, config);
    let fixture = TestFixture::new(name, content).unwrap();
    open_md(&mut harness, &fixture.path);
    run_steps(&mut harness, steps);
    harness.assert_no_plugin_errors();
}

/// `name: "file", "content" => [steps];` is a `#[test] fn name` that runs
/// [`run_md`] with the default config.
macro_rules! md_cases {
    ($(
        $(#[$attr:meta])*
        $name:ident: $file:literal, $content:literal => [$($step:expr),* $(,)?];
    )*) => {$(
        $(#[$attr])*
        #[test]
        fn $name() {
            run_md(Default::default(), $file, $content, &[$($step),*]);
        }
    )*};
}

/// `name: harness, "file", "content" => [steps];` is a `#[test] fn name`
/// that opens the file with [`open_in_project`] (waiting for its grammar)
/// in `harness` and runs the steps.
macro_rules! fence_cases {
    ($(
        $(#[$attr:meta])*
        $name:ident: $harness:expr, $file:literal, $content:literal => [$($step:expr),* $(,)?];
    )*) => {$(
        $(#[$attr])*
        #[test]
        fn $name() {
            let mut harness = $harness;
            open_in_project(&mut harness, $file, $content);
            run_steps(&mut harness, &[$($step),*]);
        }
    )*};
}

// ---------------------------------------------------------------------------
// Where the rules apply
// ---------------------------------------------------------------------------

md_cases! {
    /// In a .md file Enter continues a list item, with no activation step.
    test_rules_apply_in_md_file: "readme.md", "- item\n"
        => [END, ENTER, Expect(Is("- item\n- \n"))];

    /// In a non-markdown file Enter is the plain newline: a Rust file must
    /// not continue Markdown list markers.
    test_rules_do_not_apply_in_non_md_file: "main.rs", "- item\n"
        => [END, ENTER, Expect(Is("- item\n\n"))];
}

// ---------------------------------------------------------------------------
// Enter key: list continuation
// ---------------------------------------------------------------------------

md_cases! {
    /// Enter at the end of an unordered list item inserts a newline followed
    /// by the same indent and bullet.
    test_enter_continues_unordered_list: "list.md", "- top\n  - nested\n"
        => [DOWN, END, ENTER, Wait(Contains("  - nested\n  - "))];

    /// Enter at the end of an ordered list item inserts the next number.
    test_enter_continues_ordered_list: "ordered.md", "1. first\n2. second\n"
        => [DOWN, END, ENTER, Wait(Contains("2. second\n3. "))];

    /// Enter at the end of a checkbox item inserts an unchecked checkbox.
    test_enter_continues_checkbox: "checkbox.md", "- [x] done task\n"
        => [END, ENTER, Wait(Contains("- [x] done task\n- [ ] "))];

    /// Enter on an empty list item (just the bullet) removes the marker.
    test_enter_clears_empty_bullet: "empty.md", "- item\n- \n"
        => [DOWN, END, ENTER, Wait(Contains("- item\n\n"))];
}

// ---------------------------------------------------------------------------
// Enter key: non-list (whitespace copying)
// ---------------------------------------------------------------------------

md_cases! {
    /// Enter on a line with no indentation inserts a plain newline: the
    /// second line is empty, with no leading spaces.
    test_enter_no_indent_on_unindented_line: "plain.md", "Hello world\n"
        => [END, ENTER, Wait(Contains("Hello world\n\n")), Expect(StartsWith("Hello world\n\n"))];

    /// Enter on a deeply-indented line matches the *line's* leading
    /// whitespace.
    test_enter_deep_indent: "deep.md", "        deep indent text\n"
        => [END, ENTER, Wait(Contains("deep indent text\n        "))];
}

// ---------------------------------------------------------------------------
// Tab key: insert spaces (non-list context)
// ---------------------------------------------------------------------------

md_cases! {
    /// Tab inserts 4 spaces at the cursor position.
    test_tab_inserts_spaces: "tab.md", "text\n"
        => [HOME, TAB, Wait(StartsWith("    text"))];

    /// Tab with several lines selected indents every selected line, as it
    /// does outside Markdown, instead of replacing the selection with spaces
    /// (#3666).
    test_tab_indents_a_multi_line_selection: "urls.md",
        "https://a.example/one\nhttps://b.example/two\nhttps://c.example/three\n"
        => [
            CTRL_HOME, SHIFT_DOWN, SHIFT_DOWN, SHIFT_END, TAB,
            Wait(Is("    https://a.example/one\n    https://b.example/two\n    https://c.example/three\n")),
        ];

    /// Tab with several cursors indents at every cursor, not just the primary.
    test_tab_with_multiple_cursors_indents_each_line: "multi.md", "one\ntwo\nthree\n"
        => [
            CTRL_HOME,
            // Add the cursors and press Tab with no frame in between, as a
            // fast key sequence does: the handler must still see all three.
            AddCursorBelow, AddCursorBelow, TAB,
            Wait(Is("    one\n    two\n    three\n")),
        ];

    /// Multiple Tab presses accumulate spaces.
    test_multiple_tabs: "tabs.md", "x\n"
        => [HOME, TAB, TAB, Wait(StartsWith("        x"))];
}

/// Tab follows `editor.tab_size` instead of a fixed four columns, both for a
/// plain line and for the indent + bullet cycle on a blank list item, and
/// Shift+Tab takes the same width back off (#3713).
#[test]
fn test_tab_follows_editor_tab_size() {
    let mut config = fresh::config::Config::default();
    config.editor.tab_size = 2;
    run_md(
        config,
        "tab2.md",
        "text\n- \n",
        &[
            HOME,
            TAB,
            Wait(StartsWith("  text\n")),
            // Blank list item: indent by one level and cycle the bullet.
            DOWN,
            END,
            TAB,
            Wait(Is("  text\n  + \n")),
            // Shift+Tab removes that level again and cycles the bullet back.
            BACK_TAB,
            Wait(Is("  text\n- \n")),
        ],
    );
}

/// `languages.markdown.tab_size` and `.use_tabs` override the global setting
/// for the list rules' indentation too (#3713).
#[test]
fn test_tab_follows_markdown_language_settings() {
    let mut config = fresh::config::Config::default();
    config.editor.tab_size = 2;
    config.languages.get_mut("markdown").unwrap().tab_size = Some(3);
    run_md(
        config.clone(),
        "tab3.md",
        "text\n",
        &[HOME, TAB, Wait(Is("   text\n"))],
    );

    config.languages.get_mut("markdown").unwrap().use_tabs = Some(true);
    run_md(
        config,
        "tabs.md",
        "text\n- \n",
        &[
            HOME,
            TAB,
            Wait(StartsWith("\ttext\n")),
            DOWN,
            END,
            TAB,
            Wait(Is("\ttext\n\t+ \n")),
            BACK_TAB,
            Wait(Is("\ttext\n- \n")),
        ],
    );
}

/// A Markdown file named on open but not yet on disk gets the
/// `languages.markdown` settings from the start, not only once it has been
/// saved (#3713).
#[test]
fn test_new_markdown_file_uses_markdown_language_settings() {
    let mut config = fresh::config::Config::default();
    config.languages.get_mut("markdown").unwrap().tab_size = Some(3);
    let mut harness = markdown_harness_with_config(80, 24, config);

    let path = harness.project_dir().unwrap().join("not_yet_saved.md");
    open_md(&mut harness, &path);
    run_steps(
        &mut harness,
        &[Type("text"), HOME, TAB, Wait(Is("   text"))],
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Tab / Shift+Tab on a blank list item: indent and cycle the bullet
// ---------------------------------------------------------------------------

md_cases! {
    /// Tab on a blank unordered list item indents it and cycles the bullet
    /// (`*` -> `-`, indented by 4).
    test_tab_cycles_bullet_on_blank_item: "cycle.md", "* \n"
        => [END, TAB, Wait(StartsWith("    - "))];

    /// Shift+Tab on a blank list item de-indents it and cycles the bullet in
    /// reverse, the exact opposite of Tab: `    - ` (as from a Tab on `* `)
    /// goes back to `* `.
    ///
    /// Forward (Tab):  * → - → + → *  (each step +4 spaces indent)
    /// Reverse (Shift+Tab): * → + → - → *  (each step −4 spaces indent)
    test_shift_tab_reverse_cycles_bullet_on_blank_item: "reverse_cycle.md", "    - \n"
        => [END, BACK_TAB, Wait(StartsWith("* \n"))];

    /// Full round-trip: Tab cycles * → - → + → *, each adding 4 spaces of
    /// indent, then Shift+Tab reverses the entire sequence back to the
    /// starting state. Async work is drained between the keys.
    test_tab_shift_tab_full_round_trip: "roundtrip.md", "* \n"
        => [
            END,
            TAB, Wait(StartsWith("    - \n")), Drain,
            TAB, Wait(StartsWith("        + \n")), Drain,
            TAB, Wait(StartsWith("            * \n")), Drain,
            BACK_TAB, Wait(StartsWith("        + \n")), Drain,
            BACK_TAB, Wait(StartsWith("    - \n")), Drain,
            BACK_TAB, Wait(StartsWith("* \n")),
        ];
}

// ---------------------------------------------------------------------------
// Normal typing, and Enter then typing
// ---------------------------------------------------------------------------

md_cases! {
    /// Characters are inserted normally (the buffer is not read-only).
    test_normal_typing_works: "type.md", "\n"
        => [HOME, Type("Hello markdown"), Expect(Contains("Hello markdown"))];

    /// A realistic flow: Enter on a list item continues the bullet, then
    /// the item's text is typed after it.
    test_enter_then_type_workflow: "workflow.md", "- item\n"
        => [
            END, ENTER, Wait(Contains("- item\n- ")),
            Type("next"), Expect(Contains("- item\n- next")),
        ];
}

// ---------------------------------------------------------------------------
// Switching buffers, other extensions
// ---------------------------------------------------------------------------

/// The rules follow the buffer the key lands in: after switching from a .md
/// buffer to a plain-text one, Enter there is the plain newline, and back in
/// the .md buffer it continues lists again.
#[test]
fn test_rules_follow_the_active_buffer() {
    let mut harness = markdown_harness(80, 24);

    let md_fixture = TestFixture::new("doc.md", "- a\n").unwrap();
    open_md(&mut harness, &md_fixture.path);

    open_project_file(&mut harness, "notes.txt", "- plain\n");
    run_steps(&mut harness, &[END, ENTER, Expect(Is("- plain\n\n"))]);

    open_md(&mut harness, &md_fixture.path);
    run_steps(
        &mut harness,
        &[CTRL_HOME, END, ENTER, Expect(Is("- a\n- \n"))],
    );
    harness.assert_no_plugin_errors();
}

/// `.mdx` files are plain text by default (no built-in language claims the
/// extension), so they get no list editing. Configured as a language with
/// the id `mdx` — or added to `markdown`'s extensions — they do.
#[test]
fn test_rules_apply_for_mdx_when_configured() {
    let mut harness = markdown_harness(80, 24);
    let content = enter_at_end_of_first_line(&mut harness, "plain.mdx", "- item\n");
    assert_eq!(harness.editor().active_state().language, "text");
    assert_eq!(content, "- item\n\n");

    let mut config = fresh::config::Config::default();
    let mut mdx = config.languages["markdown"].clone();
    mdx.extensions = vec!["mdx".to_string()];
    config.languages.insert("mdx".to_string(), mdx);
    let mut harness = markdown_harness_with_config(80, 24, config);
    let content = enter_at_end_of_first_line(&mut harness, "component.mdx", "- item\n");
    assert_eq!(harness.editor().active_state().language, "mdx");
    assert_eq!(content, "- item\n- \n");
    harness.assert_no_plugin_errors();
}

md_cases! {
    /// The rules also apply to .markdown files.
    test_rules_apply_for_markdown_extension: "readme.markdown", "1. one\n"
        => [END, ENTER, Expect(Is("1. one\n2. \n"))];
}

// ---------------------------------------------------------------------------
// Multiple cursors (#1140), undo, line endings
// ---------------------------------------------------------------------------

md_cases! {
    /// Enter with multiple cursors inserts newlines at ALL cursor positions,
    /// not just the primary (#1140: "Pressing enter with multiple cursors only
    /// adds a newline to the last cursor"): 4 lines become at least 5, and
    /// typing after it lands at both cursors.
    test_multi_cursor_enter_inserts_at_all_cursors: "multi.md", "aaa\nbbb\nccc\n"
        => [
            // After the first character of line 1, and the same below.
            CTRL_HOME, RIGHT, CTRL_ALT_DOWN,
            ENTER, Wait(LinesAtLeast(5)),
            Type("X"), Wait(Count('X', 2)),
        ];

    /// The same with the cursors at the end of list items.
    test_multi_cursor_enter_on_list_items: "list_multi.md", "- item one\n- item two\n- item three\n"
        => [
            CTRL_HOME, END, CTRL_ALT_DOWN,
            ENTER, Wait(LinesAtLeast(5)),
            Type("X"), Wait(Count('X', 2)),
        ];

    /// Enter continues the marker at every cursor, each with its own kind.
    test_multi_cursor_enter_continues_each_marker: "mc.md", "- one\n* two\n1. th\n"
        => [
            END, AddCursorBelow, AddCursorBelow, ENTER, Type("X"),
            Expect(Is("- one\n- X\n* two\n* X\n1. th\n2. X\n")),
        ];

    /// With several cursors, one on a blank bullet and one on an indented
    /// plain line, Shift+Tab applies the rule at the first and the normal
    /// dedent at the second, and both cursors land where their line's text
    /// moved to.
    test_multi_cursor_shift_tab_mixes_rule_and_normal_dedent: "mix.md", "    - \n    text\n"
        => [
            // Cursor at the end of line 1 and at column 6 of line 2.
            END, AddCursorBelow, BACK_TAB, Type("X"),
            Expect(Is("* X\nteXxt\n")),
        ];

    /// A rule's edit is one undo step, like the key it stands in for.
    test_rule_edits_undo_in_one_step: "undo.md", "- one\n"
        => [
            END, ENTER, Expect(Is("- one\n- \n")),
            // Tab rewrites both sides of the cursor (a delete and an insert).
            TAB, Expect(Is("- one\n    + \n")),
            UNDO, Expect(Is("- one\n- \n")),
            // Enter on the empty item clears it; undo brings the marker back.
            ENTER, Expect(Is("- one\n\n")),
            UNDO, Expect(Is("- one\n- \n")),
            UNDO, Expect(Is("- one\n")),
        ];

    /// A CRLF buffer continues its list with CRLF line breaks: the rule's
    /// line ends before the `\r`, and its template's line break is written as
    /// the buffer's line ending.
    test_enter_continues_list_in_crlf_buffer: "crlf.md", "- one\r\n2. two\r\n"
        => [
            END, ENTER, Type("X"), Expect(Is("- one\r\n- X\r\n2. two\r\n")),
            DOWN, END, ENTER, Type("Y"), Expect(Is("- one\r\n- X\r\n2. two\r\n3. Y\r\n")),
        ];
}

// ---------------------------------------------------------------------------
// Keys sent back to back (#3813)
// ---------------------------------------------------------------------------

/// Hand keys to the editor back to back, as a fast typist or a paste-like
/// burst does: no render, no wait, no draining of plugin work in between.
fn burst(harness: &mut EditorTestHarness, keys: &[(KeyCode, KeyModifiers)]) {
    for &(code, modifiers) in keys {
        harness.editor_mut().handle_key(code, modifiers).unwrap();
    }
}

fn chars(text: &str) -> Vec<(KeyCode, KeyModifiers)> {
    text.chars()
        .map(|c| (KeyCode::Char(c), KeyModifiers::NONE))
        .collect()
}

/// #3813: `- one`, Enter, `two`, Enter, `three` in one burst comes out in
/// the order typed. The list continuation used to come from an async plugin
/// handler, so the characters after Enter landed before it did.
#[test]
fn test_burst_typing_around_enter_keeps_key_order() {
    let mut harness = markdown_harness(80, 24);
    let fixture = TestFixture::new("burst.md", "").unwrap();
    open_md(&mut harness, &fixture.path);

    let enter = (KeyCode::Enter, KeyModifiers::NONE);
    let mut keys = chars("- one");
    keys.push(enter);
    keys.extend(chars("two"));
    keys.push(enter);
    keys.extend(chars("three"));
    burst(&mut harness, &keys);
    // The edits are already there, before any async work is looked at.
    harness.assert_buffer_content("- one\n- two\n- three");

    // Tab / Shift+Tab on a blank item and Enter on an empty one, in a burst.
    let tab = (KeyCode::Tab, KeyModifiers::NONE);
    let back_tab = (KeyCode::BackTab, KeyModifiers::SHIFT);
    let mut keys = vec![enter, tab];
    keys.extend(chars("deep"));
    keys.extend([enter, back_tab]);
    keys.extend(chars("out"));
    keys.extend([enter, enter]);
    keys.extend(chars("para"));
    burst(&mut harness, &keys);
    harness.render().unwrap();
    assert_eq!(
        harness.get_buffer_content().unwrap(),
        "- one\n- two\n- three\n    + deep\n- out\npara"
    );
    harness.assert_no_plugin_errors();
}

// ---------------------------------------------------------------------------
// Edge cases of applying a rule's edit
// ---------------------------------------------------------------------------

/// A line that isn't UTF-8 gets the key's normal behaviour: the rules match
/// text, and a lossy copy of the line (U+FFFD for each bad byte) has a
/// different length than the bytes the edit would be made at. It used to
/// compute the edit's offsets from that copy (and underflow in debug builds).
#[test]
fn test_enter_on_a_line_that_is_not_utf8_is_the_plain_newline() {
    let mut harness = markdown_harness(100, 24);
    let path = harness.project_dir().unwrap().join("bytes.md");
    fs::write(&path, b"- a\xffb\n").unwrap();
    open_md(&mut harness, &path);
    // Opened as UTF-8 regardless, so the buffer holds the 0xFF byte.
    harness
        .send_key(KeyCode::Char('p'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("Reload with Encoding").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.assert_screen_contains("Reload with encoding:");
    harness
        .send_key(KeyCode::Char('a'), KeyModifiers::CONTROL)
        .unwrap();
    harness.type_text("UTF-8").unwrap();
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.assert_screen_contains("a<FF>b");

    run_steps(&mut harness, &[END, ENTER, Type("X")]);
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    assert_eq!(fs::read(&path).unwrap(), b"- a\xffb\nX\n");

    // A UTF-8 line below it still continues its list.
    run_steps(&mut harness, &[HOME, Type("- "), END, ENTER, Type("Y")]);
    assert!(
        harness
            .get_buffer_content()
            .unwrap()
            .ends_with("- X\n- Y\n"),
        "{:?}",
        harness.get_buffer_content().unwrap()
    );
}

/// With one cursor on a line a rule handles and another between brackets
/// (Enter's bracket expansion moves that cursor back up a line), both
/// cursors land where they belong. The expansion's move was shifted for the
/// rule's edit twice, once by the rules and once by the multi-cursor apply,
/// which put the second cursor past the end of the buffer.
#[test]
fn test_multi_cursor_enter_mixes_rule_and_bracket_expansion() {
    let mut harness = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "python": { "edit_rules": [
            { "on": "newline", "before": r"^(\s*# ?)\S", "insert": "\n$1" }
        ] } } }),
    );
    assert!(harness.config().editor.auto_indent);
    assert!(harness.config().editor.auto_close);

    open_project_file(&mut harness, "mix.py", "# a\nff()");
    // Cursors at the end of `# a`, and between `(` and `)` below it.
    run_steps(&mut harness, &[CTRL_HOME, END, AddCursorBelow]);
    harness.render().unwrap();
    assert_eq!(harness.editor().active_cursors().count(), 2);

    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    let len = harness.get_buffer_content().unwrap().len();
    for (_, c) in harness.editor().active_cursors().iter() {
        assert!(c.position <= len, "a cursor past the end: {}", c.position);
    }
    harness.type_text("X").unwrap();
    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.split('\n').collect();
    assert_eq!(lines.len(), 5, "{content:?}");
    assert_eq!(lines[0], "# a");
    assert_eq!(lines[1], "# X", "{content:?}");
    assert_eq!(lines[2], "ff(", "{content:?}");
    assert_eq!(lines[3].trim_start(), "X", "{content:?}");
    assert_eq!(lines[4], ")", "{content:?}");
}

/// A rule that rewrites only the text after the cursor leaves the cursor
/// where it was (its insert would otherwise carry the cursor past it).
#[test]
fn test_rule_changing_only_the_text_after_the_cursor_keeps_the_cursor() {
    let mut harness = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "python": { "edit_rules": [
            { "on": "indent", "before": r"\($", "after": r"^\)$", "replace_after": "  )" }
        ] } } }),
    );
    open_project_file(&mut harness, "after.py", "f()\n");
    run_steps(
        &mut harness,
        &[
            RIGHT,
            RIGHT,
            TAB,
            Expect(Is("f(  )\n")),
            Type("X"),
            Expect(Is("f(X  )\n")),
        ],
    );
}

// ---------------------------------------------------------------------------
// Edit rules are a language setting: config.json turns them off, adds them
// for other languages, and a reload brings the current ones
// ---------------------------------------------------------------------------

/// Write `config` as the user's config.json and reload, as editing the file
/// and `editor.reloadConfig()` do.
fn write_user_config_and_reload(harness: &mut EditorTestHarness, config: serde_json::Value) {
    let path = harness.editor().dir_context().config_path();
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(&path, config.to_string()).unwrap();
    harness.editor_mut().reload_config();
}

/// A config.json with one rule for `language`: Enter on a line comment
/// starting with `marker` continues the comment.
fn comment_rule(language: &str, marker: &str) -> serde_json::Value {
    serde_json::json!({ "languages": { language: { "edit_rules": [
        { "on": "newline", "before": format!(r"^([ \t]*{marker} ?)\S"), "insert": "\n$1" }
    ] } } })
}

/// `"edit_rules": []` for Markdown in config.json turns list editing off:
/// Enter is the plain newline, Tab the plain Tab.
#[test]
fn test_user_config_turns_markdown_list_editing_off() {
    let mut harness = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "markdown": { "edit_rules": [] } } }),
    );
    assert!(harness.config().languages["markdown"].edit_rules.is_empty());

    let content = enter_at_end_of_first_line(&mut harness, "off.md", "- item\n");
    assert_eq!(content, "- item\n\n", "no list continuation");
    let fixture = TestFixture::new("off_tab.md", "* \n").unwrap();
    open_md(&mut harness, &fixture.path);
    // No bullet cycling.
    run_steps(&mut harness, &[END, TAB, Expect(StartsWith("* "))]);
}

/// Rules for another language, written in config.json, apply in its
/// buffers. One that doesn't compile is dropped on its own (with a warning
/// naming `languages.python.edit_rules` and its index); the others apply.
#[test]
fn test_user_config_edit_rules_for_another_language() {
    let mut harness = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "python": { "edit_rules": [
            { "on": "newline", "before": "([", "insert": "" },
            { "on": "newline", "before": r"^([ \t]*# ?)\S", "insert": "\n$1" }
        ] } } }),
    );
    let rules = &harness.config().languages["python"].edit_rules;
    assert_eq!(
        rules.len(),
        1,
        "the invalid rule is dropped, the other kept"
    );

    open_project_file(&mut harness, "t.py", "    # note\nx = 1\n");
    run_steps(
        &mut harness,
        &[
            END,
            ENTER,
            Type("more"),
            Expect(Is("    # note\n    # more\nx = 1\n")),
        ],
    );

    // Markdown keeps its built-in rules: the user's file set Python's only.
    let content = enter_at_end_of_first_line(&mut harness, "still.md", "- item\n");
    assert_eq!(content, "- item\n- \n");
}

/// A buffer already open follows a reload: it gets the rules config.json
/// has now, not the ones it opened with.
#[test]
fn test_config_reload_brings_changed_edit_rules() {
    let mut harness = markdown_harness(80, 24);
    let fixture = TestFixture::new("reload.md", "- item\n> quote\n").unwrap();
    open_md(&mut harness, &fixture.path);
    run_steps(
        &mut harness,
        &[
            END,
            ENTER,
            Expect(Is("- item\n- \n> quote\n")),
            UNDO,
            Expect(Is("- item\n> quote\n")),
        ],
    );

    // Now Markdown's rules continue block quotes and nothing else.
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "markdown": { "edit_rules": [
            { "on": "newline", "before": "^(>+ )", "insert": "\n$1" }
        ] } } }),
    );
    run_steps(
        &mut harness,
        &[
            CTRL_HOME,
            END,
            ENTER,
            // The list rule is gone.
            Expect(Is("- item\n\n> quote\n")),
            DOWN,
            END,
            ENTER,
            Expect(Is("- item\n\n> quote\n> \n")),
        ],
    );
}

/// Saving an unrelated Markdown setting in the Settings UI (Auto Close)
/// writes the whole Markdown entry, rules included, to config.json. The
/// rules must come back exactly: the empty-item rule's `"replace_before":
/// ""` was stripped on write, so it no longer loaded and Enter on an empty
/// item stopped ending the list.
#[test]
fn test_settings_save_of_another_markdown_field_keeps_the_rules() {
    let mut config = fresh::config::Config::default();
    config.languages.retain(|name, _| name == "markdown");
    let mut harness = markdown_harness_with_config(120, 34, config);
    harness.render().unwrap();

    harness.open_settings().unwrap();
    harness
        .select_settings_category("Syntax & Languages")
        .unwrap();
    harness.send_key(KeyCode::Tab, KeyModifiers::NONE).unwrap();
    harness.render().unwrap();
    for _ in 0..60 {
        if harness.screen_to_string().contains("[Enter to edit]") {
            break;
        }
        harness.send_key(KeyCode::Down, KeyModifiers::NONE).unwrap();
        harness.render().unwrap();
    }
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness.render().unwrap();
    harness.assert_screen_contains("Auto Close");
    // The rules list shows each rule's key and pattern.
    harness.assert_screen_contains("newline: ^");
    // The first field is Auto Close: toggle it, save the entry, then the
    // settings.
    harness
        .send_key(KeyCode::Enter, KeyModifiers::NONE)
        .unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();
    harness
        .send_key(KeyCode::Char('s'), KeyModifiers::CONTROL)
        .unwrap();
    harness.render().unwrap();

    let path = harness.editor().dir_context().config_path();
    let written: serde_json::Value =
        serde_json::from_str(&fs::read_to_string(&path).unwrap()).unwrap();
    assert!(
        written.pointer("/languages/markdown/auto_close").is_some(),
        "the save reached the file: {written:#}"
    );

    // Read back from disk, as at the next start.
    harness.editor_mut().reload_config();
    assert_eq!(
        harness.config().languages["markdown"].edit_rules,
        fresh::config::Config::default().languages["markdown"].edit_rules,
        "the rules read back exactly: {written:#}"
    );
    let content = enter_at_end_of_first_line(&mut harness, "after_save.md", "- \n");
    assert_eq!(content, "\n", "Enter on an empty item still ends the list");
    let content = enter_at_end_of_first_line(&mut harness, "after_save2.md", "1. one\n");
    assert_eq!(content, "1. one\n2. \n");
}

/// The documented way to get list editing in `.mdx` files: add `mdx` to
/// Markdown's extensions (in config.json, here as the config the editor
/// starts with). The `.mdx` buffer is then Markdown, with its rules.
#[test]
fn test_mdx_added_to_markdown_extensions_gets_list_editing() {
    let mut config = fresh::config::Config::default();
    config
        .languages
        .get_mut("markdown")
        .unwrap()
        .extensions
        .push("mdx".to_string());
    let mut harness = markdown_harness_with_config(80, 24, config);
    let content = enter_at_end_of_first_line(&mut harness, "page.mdx", "1. one\n");
    assert_eq!(harness.editor().active_state().language, "markdown");
    assert_eq!(content, "1. one\n2. \n");

    // The same from the user's config.json, after a reload.
    let mut harness = markdown_harness(80, 24);
    write_user_config_and_reload(
        &mut harness,
        serde_json::json!({ "languages": { "markdown": {
            "extensions": ["md", "markdown", "mdx"]
        } } }),
    );
    let content = enter_at_end_of_first_line(&mut harness, "other.mdx", "- item\n");
    assert_eq!(harness.editor().active_state().language, "markdown");
    assert_eq!(content, "- item\n- \n");
}

// ---------------------------------------------------------------------------
// `insert_line_below` (vi's `o`) vs Enter
// ---------------------------------------------------------------------------

/// `insert_line_below` opens a line below the cursor's line from anywhere on
/// it, continuing the list; on an empty item it opens a new item instead of
/// clearing the one it opens from (Enter's rule there).
#[test]
fn test_insert_line_below_continues_an_empty_item() {
    use fresh::input::keybindings::Action;
    let mut harness = markdown_harness(80, 24);

    let fixture = TestFixture::new("below.md", "- \n1. one\n").unwrap();
    open_md(&mut harness, &fixture.path);
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InsertLineBelow);
    run_steps(&mut harness, &[Type("a"), Expect(Is("- \n- a\n1. one\n"))]);

    // From the start of a numbered item: the next number, below it.
    run_steps(&mut harness, &[DOWN, HOME]);
    harness
        .editor_mut()
        .dispatch_action_for_tests(Action::InsertLineBelow);
    run_steps(
        &mut harness,
        &[Type("b"), Expect(Is("- \n- a\n1. one\n2. b\n"))],
    );
}

// ---------------------------------------------------------------------------
// Plugins: `executeAction`, and rules for another language
// ---------------------------------------------------------------------------

/// A project directory whose `plugins` directory holds the plugin library
/// and `file` with `source`. Returns the temporary directory (keep it alive)
/// and the project root.
#[cfg(feature = "plugins")]
fn project_with_plugin(file: &str, source: &str) -> (tempfile::TempDir, PathBuf) {
    use crate::common::harness::copy_plugin_lib;

    init_tracing_from_env();
    let temp_dir = tempfile::TempDir::new().unwrap();
    let project_root = temp_dir.path().join("project_root");
    let plugins_dir = project_root.join("plugins");
    fs::create_dir_all(&plugins_dir).unwrap();
    copy_plugin_lib(&plugins_dir);
    fs::write(plugins_dir.join(file), source).unwrap();
    (temp_dir, project_root)
}

/// A plugin's `executeAction("insert_newline")` is the user's Enter: on an
/// empty list item it clears the item. Only `insert_line_below` passes over
/// the rule that rewrites the line.
#[cfg(feature = "plugins")]
#[test]
fn test_plugin_insert_newline_on_an_empty_item_is_enter() {
    use fresh::input::keybindings::Action;

    let (_temp_dir, project_root) = project_with_plugin(
        "newline_actions.ts",
        r#"/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
registerHandler("t_enter", () => { editor.executeAction("insert_newline"); });
registerHandler("t_below", () => { editor.executeAction("insert_line_below"); });
// A handler runs as a plugin action only once a command names it: that is
// what ties the action to this plugin.
editor.registerCommand("Test Enter", "", "t_enter", null);
editor.registerCommand("Test Line Below", "", "t_below", null);
"#,
    );
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    // The commands reaching the registry means the plugin is loaded.
    harness
        .wait_until(|h| {
            let registry = h.editor().command_registry().read().unwrap();
            let commands = registry.get_all();
            ["t_enter", "t_below"].iter().all(|name| {
                commands
                    .iter()
                    .any(|c| c.action == Action::PluginAction(name.to_string()))
            })
        })
        .unwrap();

    for (file, action, after) in [
        ("items.md", "t_enter", "\n"),
        ("items2.md", "t_below", "- \n- \n"),
    ] {
        let md = project_root.join(file);
        fs::write(&md, "- \n").unwrap();
        open_md(&mut harness, &md);
        harness.send_key(KeyCode::End, KeyModifiers::NONE).unwrap();
        harness
            .editor_mut()
            .dispatch_action_for_tests(Action::PluginAction(action.to_string()));
        harness
            .wait_until(|h| h.get_buffer_content().is_some_and(|c| c == after))
            .unwrap();
    }
    harness.assert_no_plugin_errors();
}

/// A test plugin's rules for Python: continue `#` comments on Enter. A
/// malformed set is refused with an error naming the rule; unloading the
/// plugin drops its rules.
#[cfg(feature = "plugins")]
#[test]
fn test_edit_rules_from_another_plugin_for_another_language() {
    use fresh_core::api::EditRuleTrigger;

    let (_temp_dir, project_root) = project_with_plugin(
        "py_comments.ts",
        r#"/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
try {
  editor.registerEditRules("bad", {
    languages: ["python"],
    rules: [{ on: "newline", before: "(", insert: "" }],
  });
} catch (e) {
  // The message names the rule and the field.
  const named = String(e).includes('rules[0].before:');
  editor.setStatus(named ? "REFUSED-NAMED" : "REFUSED-OTHER " + e);
}
editor.registerEditRules("comments", {
  languages: ["python"],
  rules: [{ on: "newline", before: "^([ \\t]*# ?)\\S", insert: "\n$1" }],
});
"#,
    );
    let mut harness = EditorTestHarness::with_config_and_working_dir(
        100,
        24,
        Default::default(),
        project_root.clone(),
    )
    .unwrap();
    let python_rules = |h: &EditorTestHarness| {
        h.editor()
            .edit_rules()
            .read()
            .unwrap()
            .has_rules("python", EditRuleTrigger::Newline)
    };
    harness.wait_until(python_rules).unwrap();
    harness
        .wait_until(|h| h.get_status_bar().contains("REFUSED"))
        .unwrap();
    assert!(
        harness.get_status_bar().contains("REFUSED-NAMED"),
        "the error names the rule: {}",
        harness.get_status_bar()
    );

    let py = project_root.join("t.py");
    fs::write(&py, "    # note\n").unwrap();
    harness.open_file(&py).unwrap();
    run_steps(
        &mut harness,
        &[
            END,
            ENTER,
            Type("more"),
            Expect(Is("    # note\n    # more\n")),
        ],
    );

    harness
        .editor()
        .plugin_manager()
        .unload_plugin("py_comments")
        .unwrap();
    harness.wait_until(|h| !python_rules(h)).unwrap();
    assert!(
        !harness
            .editor()
            .edit_rules()
            .read()
            .unwrap()
            .has_rules("python", EditRuleTrigger::Indent),
        "nothing of the plugin's is left registered"
    );
    run_steps(&mut harness, &[ENTER, Type("x")]);
    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.split('\n').collect();
    assert_eq!(
        lines[..2],
        ["    # note", "    # more"],
        "the lines before are untouched: {content:?}"
    );
    assert_eq!(
        lines[2].trim_start(),
        "x",
        "an unloaded plugin's rules no longer apply (no `# ` continued): {content:?}"
    );
    assert_eq!(lines.len(), 4, "{content:?}");
}

// ---------------------------------------------------------------------------
// Code blocks: the rules of the language at the cursor
// ---------------------------------------------------------------------------
//
// These need the highlighter's embedded regions, so they run with the full
// grammar registry (the default test registry is empty, and a buffer with
// no highlighter knows no regions).

/// A harness with the full grammar registry, in a fresh project directory.
fn grammar_harness() -> EditorTestHarness {
    grammar_harness_with(None)
}

/// [`grammar_harness`] with a given config. Without one the harness turns
/// `editor.auto_indent` off for simpler tests, so a test about indentation
/// passes a config that keeps it on.
fn grammar_harness_with(config: Option<fresh::config::Config>) -> EditorTestHarness {
    use crate::common::harness::HarnessOptions;
    init_tracing_from_env();
    let mut options = HarnessOptions::new()
        .with_project_root()
        .with_empty_plugins_dir()
        .with_full_grammar_registry();
    if let Some(config) = config {
        options = options.with_config(config);
    }
    EditorTestHarness::create(100, 24, options).unwrap()
}

/// [`grammar_harness_with`] with auto-indent on (the user default) and the
/// harness's other test simplifications.
fn auto_indent_harness() -> EditorTestHarness {
    let mut config = fresh::config::Config::default();
    config.editor.auto_indent = true;
    config.editor.auto_close = false;
    config.editor.auto_surround = false;
    config.editor.animations = false;
    grammar_harness_with(Some(config))
}

/// Open `name` holding `content` in the harness's project, and draw it once
/// its grammar is in place. With the full grammar registry a file can open
/// before the grammar it needs is ready (it is re-detected when the build
/// lands); keys sent before that find no embedded-region data, and the
/// cursor rightly falls back to the buffer's language.
fn open_in_project(harness: &mut EditorTestHarness, name: &str, content: &str) {
    let path = harness.project_dir().unwrap().join(name);
    fs::write(&path, content).unwrap();
    harness.open_file(&path).unwrap();
    harness
        .wait_until(|h| {
            h.editor()
                .active_state()
                .highlighter
                .syntax_name()
                .is_some()
        })
        .unwrap();
    harness.render().unwrap();
}

fence_cases! {
    /// Inside a ```python block a `- x` line is Python, not a list item:
    /// Enter is the plain newline, with no `- ` continued.
    test_no_list_continuation_inside_a_python_fence: grammar_harness(),
        "fence.md", "```python\n- x\n```\n"
        => [
            Language("markdown"),
            EndOfLine(1), ENTER, Type("q"), Expect(Is("```python\n- x\nq\n```\n")),
        ];

    /// Enter after `def f():` in a ```python block indents like Python,
    /// though Markdown's own `auto_indent` is off.
    test_enter_indents_like_python_inside_a_python_fence: auto_indent_harness(),
        "def.md", "```python\ndef f():\n```\n"
        => [EndOfLine(1), ENTER, Type("pass"), Expect(Is("```python\ndef f():\n    pass\n```\n"))];

    /// A `}` typed on an indented line of a ```js block dedents by
    /// JavaScript's rules, as in a `.js` file: Markdown's own `auto_indent`
    /// is off, and a delimiter left indented would keep the next lines (the
    /// closing fence among them) indented too.
    test_closing_brace_dedents_inside_a_js_fence: auto_indent_harness(),
        "brace.md", "```js\nfunction g() {\n    q\n```\n"
        => [EndOfLine(2), ENTER, Type("}"), Expect(Is("```js\nfunction g() {\n    q\n}\n```\n"))];

    /// `else:` typed on an indented line of a ```python block dedents to its
    /// `if`, as in a `.py` file.
    test_else_dedents_inside_a_python_fence: auto_indent_harness(),
        "else.md", "```python\nif x:\n    a\n```\n"
        => [EndOfLine(2), ENTER, Type("else:"), Expect(Is("```python\nif x:\n    a\nelse:\n```\n"))];

    /// Outside a fence, Markdown's `auto_indent: false` holds: a `}` typed on
    /// an indented line stays where it was typed.
    test_closing_brace_outside_a_fence_keeps_its_indent: auto_indent_harness(),
        "prose.md", "text {\n  \n"
        => [EndOfLine(1), Type("}"), Expect(Is("text {\n  }\n"))];

    /// The block's delimiter lines belong to Markdown, and right after the
    /// closing one list editing is back.
    test_list_continuation_right_after_a_closing_fence: grammar_harness(),
        "after.md", "```python\nx = 1\n```\n- after\n"
        => [EndOfLine(3), ENTER, Type("next"), Expect(Is("```python\nx = 1\n```\n- after\n- next\n"))];
}

/// A Python edit rule from the user's config applies on a line of a
/// ```python block, and not on the same text outside it.
#[test]
fn test_python_config_rule_applies_inside_a_python_fence_only() {
    let mut harness = grammar_harness();
    write_user_config_and_reload(&mut harness, comment_rule("python", "#"));
    open_in_project(
        &mut harness,
        "rule.md",
        "```python\n# note\n```\n# heading\n",
    );
    run_steps(
        &mut harness,
        &[
            EndOfLine(1),
            ENTER,
            Type("more"),
            Expect(Is("```python\n# note\n# more\n```\n# heading\n")),
            EndOfLine(4),
            ENTER,
            Type("h2"),
            // A Markdown heading is not a Python comment.
            Expect(Is("```python\n# note\n# more\n```\n# heading\nh2\n")),
        ],
    );
}

/// A block that names no language — or one Fresh doesn't know — is code
/// in no language: no list editing either.
#[test]
fn test_no_list_rules_in_an_unlabelled_or_unknown_fence() {
    let mut harness = grammar_harness();
    open_in_project(&mut harness, "bare.md", "```\n- y\n```\n");
    run_steps(
        &mut harness,
        &[
            EndOfLine(1),
            ENTER,
            Type("u"),
            Expect(Is("```\n- y\nu\n```\n")),
        ],
    );

    open_in_project(&mut harness, "unknown.md", "```nosuchlang\n1. one\n```\n");
    run_steps(
        &mut harness,
        &[
            EndOfLine(1),
            ENTER,
            Type("v"),
            Expect(Is("```nosuchlang\n1. one\nv\n```\n")),
        ],
    );
}

/// Tab on a blank bullet inside a block is the plain Tab: no indent-and-
/// cycle of the bullet.
#[test]
fn test_tab_inside_a_fence_does_not_cycle_bullets() {
    let mut harness = grammar_harness();
    open_in_project(&mut harness, "tab.md", "```python\n- \n```\n");
    run_steps(&mut harness, &[EndOfLine(1), TAB]);
    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.split('\n').collect();
    assert!(
        lines[1].starts_with("- ") && lines[1].len() > 2,
        "the line keeps its `- ` and gets the Tab's whitespace: {content:?}"
    );
}

/// Each cursor gets the rules of its own line's language: one in a list
/// continues it, one on a fence's opening line gets Markdown's plain
/// newline, one inside the ```python block gets Python's.
#[test]
fn test_cursors_in_and_out_of_a_fence_each_get_their_own_rules() {
    let mut harness = grammar_harness();
    open_in_project(&mut harness, "mc.md", "- a\n```python\n- b\n```\n");
    run_steps(
        &mut harness,
        &[CTRL_HOME, SHIFT_DOWN, SHIFT_DOWN, SHIFT_END],
    );
    harness.editor_mut().add_cursors_to_line_ends();
    assert_eq!(harness.editor().active_cursors().count(), 3);
    harness.render().unwrap();
    run_steps(
        &mut harness,
        &[
            ENTER,
            Type("X"),
            Expect(Is("- a\n- X\n```python\nX\n- b\nX\n```\n")),
        ],
    );
}

/// The same holds for any host whose highlighter knows embedded regions,
/// not just Markdown: a Vue `<script>` block is JavaScript, so
/// JavaScript's rules and indentation apply there.
#[test]
fn test_vue_script_block_gets_javascript_rules() {
    let mut harness = grammar_harness();
    write_user_config_and_reload(&mut harness, comment_rule("javascript", "//"));
    open_in_project(
        &mut harness,
        "App.vue",
        "<template>\n  <div/>\n</template>\n<script>\n// note\nfunction f() {\n</script>\n",
    );
    run_steps(
        &mut harness,
        &[
            EndOfLine(4),
            ENTER,
            Type("more"),
            EndOfLine(6),
            ENTER,
            Type("x"),
            Expect(Is(
                "<template>\n  <div/>\n</template>\n<script>\n// note\n// more\nfunction f() {\n    x\n</script>\n",
            )),
        ],
    );

    // Outside the script block the JavaScript rule doesn't apply.
    run_steps(&mut harness, &[EndOfLine(1), HOME, Type("// c "), ENTER]);
    let content = harness.get_buffer_content().unwrap();
    let lines: Vec<&str> = content.split('\n').collect();
    assert!(lines[1].ends_with("// c "), "{content:?}");
    assert_eq!(
        lines[2].trim_start(),
        "<div/>",
        "the template is not JavaScript: no `// ` continued: {content:?}"
    );
}

/// A file in a language with no embedded regions is unaffected: a Python
/// file's rules and indentation are its own, wherever the cursor is.
#[test]
fn test_non_embedding_language_is_unaffected() {
    let mut harness = grammar_harness();
    write_user_config_and_reload(&mut harness, comment_rule("python", "#"));
    open_in_project(&mut harness, "t.py", "# note\ndef f():\n- x\n");
    assert!(!harness
        .editor()
        .active_state()
        .highlighter
        .hosts_embedded_regions());
    run_steps(
        &mut harness,
        &[
            EndOfLine(0),
            ENTER,
            Type("more"),
            EndOfLine(2),
            ENTER,
            Type("pass"),
            EndOfLine(4),
            ENTER,
            Type("y"),
            Expect(Is("# note\n# more\ndef f():\n    pass\n- x\ny\n")),
        ],
    );
}

/// Rules a plugin registers for a language apply inside a block of that
/// language in a Markdown file, and not on the file's own lines.
#[cfg(feature = "plugins")]
#[test]
fn test_plugin_rules_follow_the_language_of_a_fence() {
    use crate::common::harness::HarnessOptions;
    use fresh_core::api::EditRuleTrigger;

    let (_temp_dir, project_root) = project_with_plugin(
        "py_semis.ts",
        r#"/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();
editor.registerEditRules("semis", {
  languages: ["python"],
  rules: [{ on: "newline", before: ";$", insert: "\n;; " }],
});
"#,
    );
    let mut harness = EditorTestHarness::create(
        100,
        24,
        HarnessOptions::new()
            .with_working_dir(project_root.clone())
            .without_empty_plugins_dir()
            .with_full_grammar_registry(),
    )
    .unwrap();
    harness
        .wait_until(|h| {
            h.editor()
                .edit_rules()
                .read()
                .unwrap()
                .has_rules("python", EditRuleTrigger::Newline)
        })
        .unwrap();

    let md = project_root.join("p.md");
    fs::write(&md, "a;\n```python\nb;\n```\n").unwrap();
    open_md(&mut harness, &md);
    run_steps(
        &mut harness,
        &[
            EndOfLine(2),
            ENTER,
            Type("c"),
            EndOfLine(0),
            ENTER,
            Type("d"),
            Expect(Is("a;\nd\n```python\nb;\n;; c\n```\n")),
        ],
    );
}
