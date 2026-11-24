//! How cursors are drawn on the cells they sit on (issues #620, #1900, #614).
//!
//! One rule covers every cursor in the focused pane:
//!
//! * The terminal's hardware cursor draws the primary cursor only when it is
//!   the pane's *only* cursor; the editor then paints nothing on that cell.
//! * Otherwise the editor paints every cursor itself — secondaries always, and
//!   the primary too when there are several cursors (or no hardware cursor at
//!   all, as in the GUI) — and the hardware cursor is hidden, so all cursors
//!   look alike (#620).
//! * An editor-painted cursor is the inverse of the cell as it would be drawn
//!   without it, selection included: it never drops the selection (#614) and
//!   it stays distinct inside a reverse-video selection (#1900).
//!
//! Everything here is observed through the rendered cells and whether the
//! frame showed a hardware cursor.

use crate::common::harness::{EditorTestHarness, HarnessOptions};
use crossterm::event::{KeyCode, KeyModifiers};
use fresh::config::{Config, CursorStyle};
use fresh::view::color_support::{painted_rgb, ColorCapability};
use ratatui::style::{Modifier, Style};

fn harness_with(config: Config) -> EditorTestHarness {
    EditorTestHarness::with_config(80, 24, config).unwrap()
}

fn themed(theme: &str) -> Config {
    let mut config = Config::default();
    config.theme = theme.into();
    config
}

fn key(h: &mut EditorTestHarness, code: KeyCode, modifiers: KeyModifiers) {
    h.send_key(code, modifiers).unwrap();
}

/// Screen cells of the `n` characters starting at the first occurrence of
/// `text` on screen.
fn cells_of(h: &EditorTestHarness, text: &str) -> Vec<(u16, u16)> {
    let (x, y) = h
        .find_text_on_screen(text)
        .unwrap_or_else(|| panic!("{text:?} not on screen:\n{}", h.screen_to_string()));
    (0..text.chars().count() as u16)
        .map(|i| (x + i, y))
        .collect()
}

fn style_at(h: &EditorTestHarness, (x, y): (u16, u16)) -> Style {
    h.get_cell_style(x, y).expect("cell inside the screen")
}

fn reversed(style: Style) -> bool {
    style.add_modifier.contains(Modifier::REVERSED)
}

/// Two identical lines, a cursor at the end of each, and the last word
/// selected backward on both (Ctrl+Shift+Left): each cursor sits on the first
/// character of its selection.
fn two_cursors_on_backward_selections(h: &mut EditorTestHarness) {
    h.load_buffer_from_text("one wxyz\none wxyz\n").unwrap();
    key(h, KeyCode::End, KeyModifiers::NONE);
    key(h, KeyCode::Down, KeyModifiers::CONTROL | KeyModifiers::ALT);
    key(
        h,
        KeyCode::Left,
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
    );
    h.render().unwrap();
}

/// Rows of the two `wxyz` words drawn by `two_cursors_on_backward_selections`.
fn selected_words(h: &EditorTestHarness) -> Vec<Vec<(u16, u16)>> {
    let first = cells_of(h, "wxyz");
    let next_row = first[0].1 + 1;
    let second = first.iter().map(|&(x, _)| (x, next_row)).collect();
    vec![first, second]
}

/// #620: with several cursors every cursor is the same block, the primary
/// included, and the terminal draws no cursor of its own on top.
#[test]
fn every_cursor_looks_the_same_when_there_are_several() {
    let mut h = harness_with(Config::default());
    h.load_buffer_from_text("ab ab ab\n").unwrap();
    // Ctrl+D three times: a cursor on each `ab`. Left collapses each
    // selection to its start, so every cursor sits on an `a`.
    for _ in 0..3 {
        key(&mut h, KeyCode::Char('d'), KeyModifiers::CONTROL);
    }
    key(&mut h, KeyCode::Left, KeyModifiers::NONE);

    let hardware = h.render_observing_cursor().unwrap();
    assert_eq!(
        hardware, None,
        "with several cursors the editor paints them all; a hardware cursor \
         on top would draw the primary unlike the others"
    );

    let (x, y) = h.find_text_on_screen("ab ab ab").unwrap();
    let cursor_cells: Vec<Style> = [0, 3, 6]
        .iter()
        .map(|dx| style_at(&h, (x + dx, y)))
        .collect();
    for (i, style) in cursor_cells.iter().enumerate() {
        assert!(
            reversed(*style),
            "cursor {i} is not drawn as a block: {style:?}"
        );
    }
    assert!(
        cursor_cells.windows(2).all(|w| w[0] == w[1]),
        "the cursors are drawn differently: {cursor_cells:?}"
    );
    // The `b`s between them are plain text.
    assert!(!reversed(style_at(&h, (x + 1, y))));

    // Back to one cursor: the terminal draws it again, and the editor paints
    // nothing on its cell — a block of our own would cancel against a
    // terminal that draws its cursor by inverting the cell.
    key(&mut h, KeyCode::Esc, KeyModifiers::NONE);
    let hardware = h.render_observing_cursor().unwrap();
    let (cx, cy) = hardware.expect("a single cursor is the terminal's");
    assert!(
        !reversed(style_at(&h, (cx, cy))),
        "the terminal draws a single cursor; the editor must not invert its cell"
    );
    let row_reversed = (0..80).filter(|&c| reversed(style_at(&h, (c, y)))).count();
    assert_eq!(row_reversed, 0, "no editor-drawn cursor is left behind");
}

/// #1900 / #614: a cursor never takes its cell out of the selection. A
/// secondary cursor sitting on the first character of a backward selection
/// is drawn as the inverse of that *selected* cell, exactly like the primary.
#[test]
fn a_cursor_inside_a_selection_keeps_the_selection() {
    let mut h = harness_with(Config::default());
    two_cursors_on_backward_selections(&mut h);
    let selection_bg = h.editor().theme().selection_bg;

    for word in selected_words(&h) {
        let cursor = style_at(&h, word[0]);
        assert!(reversed(cursor), "cursor cell is not a block: {cursor:?}");
        assert_eq!(
            cursor.bg,
            Some(selection_bg),
            "the cursor cell dropped the selection it sits in"
        );
        for &cell in &word[1..] {
            let s = style_at(&h, cell);
            assert_eq!(s.bg, Some(selection_bg), "selected cell {cell:?}: {s:?}");
            assert!(!reversed(s), "only the cursor cell is inverted: {s:?}");
        }
    }
}

/// #1900, "the reverse rendering effect rules itself out": on a theme whose
/// selection is reverse video, a cursor drawn by adding REVERSED on top was a
/// no-op — the cursor vanished into its selection. The cursor is the inverse
/// of the selected cell, so it shows as the one un-reversed cell in the band.
#[test]
fn a_cursor_inside_a_reverse_video_selection_is_distinct() {
    let mut h = harness_with(themed("terminal"));
    two_cursors_on_backward_selections(&mut h);

    for word in selected_words(&h) {
        let cursor = style_at(&h, word[0]);
        assert!(
            !reversed(cursor),
            "cursor cell at {:?} is indistinguishable from its reverse-video selection: {cursor:?}",
            word[0]
        );
        for &cell in &word[1..] {
            assert!(
                reversed(style_at(&h, cell)),
                "selected cell {cell:?} lost its reverse video"
            );
        }
    }
}

/// The GUI has no hardware cursor, so the editor draws even a lone primary.
/// Inside a reverse-video selection it must still stand out.
#[test]
fn software_cursor_inside_a_reverse_video_selection_is_distinct() {
    let mut h = harness_with(themed("terminal"));
    h.set_software_cursor_only(true);
    h.load_buffer_from_text("one wxyz\n").unwrap();
    key(&mut h, KeyCode::End, KeyModifiers::NONE);
    key(
        &mut h,
        KeyCode::Left,
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
    );
    h.render().unwrap();

    let word = cells_of(&h, "wxyz");
    let cursor = style_at(&h, word[0]);
    assert!(
        !reversed(cursor),
        "GUI cursor vanished into its reverse-video selection: {cursor:?}"
    );
    assert_eq!(
        h.get_cell(word[0].0, word[0].1).as_deref(),
        Some("w"),
        "the character under the cursor is still drawn"
    );
    for &cell in &word[1..] {
        assert!(reversed(style_at(&h, cell)));
    }
}

/// #614 must not come back: with one cursor and a bar shape, the terminal
/// draws the bar and the cell under it keeps its selection highlight — no
/// block of the editor's own, and no hole in the selection.
#[test]
fn a_lone_bar_cursor_leaves_the_selected_cell_selected() {
    let mut config = Config::default();
    config.editor.cursor_style = CursorStyle::SteadyBar;
    let mut h = harness_with(config);
    h.load_buffer_from_text("one wxyz\n").unwrap();
    key(&mut h, KeyCode::End, KeyModifiers::NONE);
    key(
        &mut h,
        KeyCode::Left,
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
    );

    let hardware = h.render_observing_cursor().unwrap();
    let word = cells_of(&h, "wxyz");
    assert_eq!(
        hardware,
        Some(word[0]),
        "the terminal draws the lone cursor"
    );
    let selection_bg = h.editor().theme().selection_bg;
    let cursor = style_at(&h, word[0]);
    assert_eq!(
        cursor.bg,
        Some(selection_bg),
        "#614: first selected char lost its highlight"
    );
    assert!(
        !reversed(cursor),
        "no editor block under a terminal-drawn bar"
    );
}

/// #1900 on a 256-colour terminal: the default theme's selection quantized to
/// `#00005F`, a navy 1.04:1 from the cursor line's `#121212` — the selection a
/// Shift+Ctrl+Left made was simply not visible. Nearest-entry quantization
/// keeps it a visible band.
#[test]
fn selection_is_visible_on_a_256_color_terminal() {
    let mut h = EditorTestHarness::create(
        80,
        24,
        HarnessOptions::new()
            .with_config(Config::default())
            .with_color_capability(ColorCapability::Color256),
    )
    .unwrap();
    h.load_buffer_from_text("one wxyz\n").unwrap();
    key(&mut h, KeyCode::End, KeyModifiers::NONE);
    key(
        &mut h,
        KeyCode::Left,
        KeyModifiers::CONTROL | KeyModifiers::SHIFT,
    );
    h.render().unwrap();

    let word = cells_of(&h, "wxyz");
    let selected = style_at(&h, word[1]).bg.expect("selected cell has a bg");
    let before = style_at(&h, (word[0].0 - 1, word[0].1))
        .bg
        .expect("cell before the selection has a bg");
    assert!(
        matches!(selected, ratatui::style::Color::Indexed(_)),
        "the frame is painted for a 256-colour terminal: {selected:?}"
    );
    let luma = |c| {
        let (r, g, b) = painted_rgb(c, ColorCapability::Color256).unwrap();
        0.2126 * r as f64 + 0.7152 * g as f64 + 0.0722 * b as f64
    };
    let delta = (luma(selected) - luma(before)).abs();
    assert!(
        delta >= 20.0,
        "selection {selected:?} is only {delta:.1} apart from the line it is on ({before:?})"
    );
}
