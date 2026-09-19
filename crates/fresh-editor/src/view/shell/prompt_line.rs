//! The prompt row: the bottom line a prompt is typed into, described.
//!
//! **The last chrome region to leave the painter.** The row was
//! `HostRegion::PromptLine`, a `Host` the fold handed to
//! `StatusBarRenderer::render_prompt`, which drew the label and the input and
//! wrote the caret's cell into an out-parameter the fold carried for it — the
//! one channel besides the display list's own cursor by which a caret reached
//! the terminal. Now the row is runs: the label, and the input as one styled
//! run carrying the caret as a byte (`cursor_byte`), so the tree places the
//! cursor where the glyphs put it and the fold has no caret to carry.
//!
//! The input scrolls to keep the caret in view, as the painter's did
//! (`input_hscroll`): the window starts at a grapheme boundary and the run's
//! caret byte is stated inside the window. Which needs the row's width, so
//! the row is a `layout_reader`, like every other described strip that fits
//! itself to what it is given.

use std::rc::Rc;

use fresh_ui::desc::Wrap;
use fresh_ui::render::prim::wrap_text;
use fresh_ui::{
    gesture, layout_reader, row, text_runs, Event, GestureKind, Key, LayoutInfo, MouseButton, Node,
    Run, Sizing,
};
use unicode_segmentation::UnicodeSegmentation;

use super::msg::{UiFact, UiMsg};
use crate::app::shell_host::shell_theme::pair;
use crate::primitives::display_width::str_width;
use crate::view::ui::status_bar::{input_hscroll, path_display_sep, truncate_path, TruncatedPath};

/// The most rows a confirmation prompt may wrap onto. A message that still
/// doesn't fit is cut at the end of the last row with an ellipsis, so a long
/// translation on a small terminal can't take over the screen (issue #3214).
pub const MAX_WRAPPED_ROWS: usize = 3;

/// Marks the spot where a confirmation message was cut short.
const ELLIPSIS: &str = "…";

/// What the row shows: the prompt's message, its query and, for the
/// file-open prompts, the directory the query completes in.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct PromptRow {
    pub message: String,
    pub input: String,
    /// The caret's byte in `input`.
    pub cursor: usize,
    /// The selected bytes of `input`, start before end.
    pub selection: Option<(usize, usize)>,
    /// The directory a file-open prompt completes in, shown colourised
    /// between the message and the input and truncated in the middle when
    /// the whole row would not fit.
    pub dir: Option<std::path::PathBuf>,
    /// The prompt is a confirmation whose message lists its answers, so the
    /// row wraps the message when it's too wide instead of cutting it off.
    /// See [`PromptType::is_confirmation`](crate::view::prompt::PromptType::is_confirmation).
    pub wraps: bool,
}

/// The key of the input run, for the readers that ask where the query is.
pub fn input_key() -> Key {
    Key::Str("prompt_line:input".into())
}

fn base() -> String {
    pair("ui.prompt_fg", "ui.prompt_bg")
}

fn selection() -> String {
    pair("ui.prompt_selection_fg", "ui.prompt_selection_bg")
}

fn dir_ink() -> String {
    pair("ui.help_separator_fg", "ui.prompt_bg")
}

fn ellipsis_ink() -> String {
    pair("ui.menu_highlight_fg", "ui.prompt_bg")
}

/// The row, fitted to the width layout gives it.
///
/// A normal prompt is always one row tall, and its query scrolls sideways
/// when it gets long. A confirmation prompt is as tall as its wrapped message,
/// up to [`MAX_WRAPPED_ROWS`] (see [`wrapped`]). The frame sizes its prompt
/// region the same way, so the body above gives up exactly the rows the
/// prompt takes.
pub fn prompt_line(p: &PromptRow) -> Node<UiMsg> {
    let height = match p.wraps {
        true => Sizing::Auto,
        false => Sizing::Cells(1),
    };
    let p = Rc::new(p.clone());
    layout_reader(move |info: LayoutInfo| build(&p, info.constraints.max_w))
        .h(height)
        .theme(base())
}

/// The row at `width` cells: the label, then the input's window. A
/// confirmation prompt is wrapped instead.
fn build(p: &PromptRow, width: u16) -> Node<UiMsg> {
    if p.wraps {
        return wrapped(p, width);
    }

    let label = label_runs(p, width);
    let label_cells: usize = label.iter().map(|r| str_width(&r.text)).sum();
    let label_cols = label_cells.min(width as usize) as u16;
    let input_cols = width - label_cols;

    let input = input_window(p, input_cols, true).flex(1);
    row().children([text_runs(label).w(Sizing::Cells(label_cols)), input])
}

/// A confirmation prompt at `width` cells.
///
/// Its message spells out the keys it accepts, like "(s)ave, (d)iscard,
/// (C)ancel?", so cutting off the end hides the very keys the user needs
/// (issue #3214). The message and whatever the user has typed so far wrap
/// together as one paragraph with the caret after the typed text, so typing
/// an answer doesn't change the layout. A message that fits stays on one row.
///
/// When the paragraph needs more than [`MAX_WRAPPED_ROWS`] rows, the rest is
/// dropped and the last row ends in an ellipsis. The typed text still goes
/// after the ellipsis, so the user can see what they pressed.
fn wrapped(p: &PromptRow, width: u16) -> Node<UiMsg> {
    let cursor = p.cursor.min(p.input.len());
    let whole = format!("{}{}", p.message, p.input);
    let rows = wrap_text(&whole, width, Wrap::Word);
    if rows.len() <= MAX_WRAPPED_ROWS {
        return text_runs(message_and_input(p, &p.message))
            .wrap()
            .cursor_byte(p.message.len() + cursor)
            .w(Sizing::Cells(width));
    }

    // Keep the first rows as they wrapped, then fit as much of the last row
    // as leaves room for the ellipsis and the typed text.
    let (kept, rest) = rows.split_at(MAX_WRAPPED_ROWS - 1);
    let room = (width as usize).saturating_sub(str_width(ELLIPSIS) + str_width(&p.input));
    let mut shown = kept.join("\n");
    shown.push('\n');
    shown.push_str(fit_width(rest[0].trim_end(), room));
    shown.push_str(ELLIPSIS);
    let caret = shown.len() + cursor;
    text_runs(message_and_input(p, &shown))
        .cursor_byte(caret)
        .w(Sizing::Cells(width))
        .h(Sizing::Cells(MAX_WRAPPED_ROWS as u16))
}

/// `text` in the row's own ink, followed by the typed input and its
/// selection.
fn message_and_input(p: &PromptRow, text: &str) -> Vec<Run> {
    let mut runs = vec![Run::plain(text)];
    if !p.input.is_empty() {
        runs.extend(input_runs(&p.input, p.selection));
    }
    runs
}

/// The longest start of `s` that fits in `cells` cells, cut between
/// graphemes so a wide character is never split.
fn fit_width(s: &str, cells: usize) -> &str {
    let mut used = 0;
    for (at, g) in s.grapheme_indices(true) {
        used += str_width(g);
        if used > cells {
            return &s[..at];
        }
    }
    s
}

/// The query's window at `cols` cells, with the caret stated inside it when
/// `show_cursor` — the bottom row and the overlay card's input band show the
/// same query the same way. A press on it places the caret by byte.
pub(crate) fn input_window(p: &PromptRow, cols: u16, show_cursor: bool) -> Node<UiMsg> {
    let cursor = p.cursor.min(p.input.len());
    let cursor_cells = str_width(&p.input[..cursor]);
    let scroll = input_hscroll(cursor_cells, cols as usize);
    let skip = window_start(&p.input, scroll);
    let window = &p.input[skip..];
    let runs = input_runs(
        window,
        p.selection
            .map(|(a, b)| (a.saturating_sub(skip), b.saturating_sub(skip))),
    );
    let mut runs = text_runs(runs);
    if show_cursor {
        runs = runs.cursor_byte(cursor.saturating_sub(skip));
    }
    gesture(runs).key(input_key()).on(
        GestureKind::Press,
        Rc::new(move |e: &Event| {
            if e.button != MouseButton::Left {
                return None;
            }
            let byte = e.text_byte?;
            e.stop();
            Some(UiMsg::Ui(UiFact::PromptInputPress { byte: skip + byte }))
        }),
    )
}

/// The byte where the input's window starts, so that `scroll` cells are
/// skipped — at a grapheme boundary, which the painter's cell-wise scroll
/// did not respect.
fn window_start(input: &str, scroll: usize) -> usize {
    let mut cells = 0;
    for (at, g) in input.grapheme_indices(true) {
        if cells >= scroll {
            return at;
        }
        cells += str_width(g);
    }
    input.len()
}

/// The input's window as runs: the selected bytes in the selection's ink,
/// the rest in the row's.
fn input_runs(window: &str, selection: Option<(usize, usize)>) -> Vec<Run> {
    let mut runs = Vec::new();
    match selection {
        Some((a, b)) if a < b && a < window.len() => {
            let b = b.min(window.len());
            if a > 0 {
                runs.push(Run::plain(&window[..a]));
            }
            runs.push(Run::themed(&window[a..b], selection_theme()));
            if b < window.len() {
                runs.push(Run::plain(&window[b..]));
            }
        }
        _ => runs.push(Run::plain(window)),
    }
    runs
}

fn selection_theme() -> String {
    selection()
}

/// The label: the message, and for a file-open prompt the directory after
/// it — truncated in the middle once message, directory and input would
/// take more than nine tenths of the row, the way the painter drew it.
fn label_runs(p: &PromptRow, width: u16) -> Vec<Run> {
    let mut runs = vec![Run::plain(&p.message)];
    let Some(dir) = &p.dir else {
        return runs;
    };
    let prefix_len = str_width(&p.message);
    let dir_path = dir.to_string_lossy();
    let dir_path_len = dir_path.len() + 1; // the trailing slash
    let input_len = p.input.len();
    let threshold = (width as usize * 90) / 100;
    let truncated = if prefix_len + dir_path_len + input_len > threshold {
        let available = threshold
            .saturating_sub(prefix_len)
            .saturating_sub(input_len);
        truncate_path(dir, available)
    } else {
        TruncatedPath {
            prefix: String::new(),
            truncated: false,
            suffix: dir_path.to_string(),
            sep: path_display_sep(&dir_path),
        }
    };
    let with_slash = |s: &str| match s.ends_with('/') {
        true => s.to_string(),
        false => format!("{s}/"),
    };
    if truncated.truncated {
        runs.push(Run::themed(&truncated.prefix, dir_ink()));
        runs.push(Run::themed(
            format!("{}[...]", truncated.sep),
            ellipsis_ink(),
        ));
        runs.push(Run::themed(with_slash(&truncated.suffix), dir_ink()));
    } else {
        runs.push(Run::themed(with_slash(&truncated.suffix), dir_ink()));
    }
    runs
}

#[cfg(test)]
mod tests {
    use super::*;
    use fresh_ui::{Draw, Point, Size, Ui};

    /// The English quit prompt with hot exit on, placeholders filled in. It is
    /// 99 columns, so it wraps onto two rows at 80.
    const QUIT_EN: &str = "1 buffer has unsaved changes. (s)ave and quit, \
                           (d)iscard and quit, (q)uit (recoverable), (C)ancel? ";

    /// The same prompt in German. At 30 columns it would need five rows,
    /// which is more than the prompt is allowed to take.
    const QUIT_DE: &str = "1 Buffer hat ungespeicherte Änderungen. (s)peichern und beenden, \
                           (v)erwerfen und beenden, (q)eenden (wiederherstellbar), (A)bbrechen? ";

    fn frame(p: &PromptRow, width: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(prompt_line(p), Size::new(width, 1));
        ui
    }

    /// The row laid out with room to grow downward, so a test can see how
    /// many rows it actually takes.
    fn laid_out(p: &PromptRow, width: u16, height: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(prompt_line(p), Size::new(width, height));
        ui
    }

    /// A confirmation prompt with `input` typed and the caret after it.
    fn confirmation(message: &str, input: &str) -> PromptRow {
        PromptRow {
            message: message.into(),
            input: input.into(),
            cursor: input.len(),
            selection: None,
            dir: None,
            wraps: true,
        }
    }

    fn row_text(ui: &Ui<UiMsg>) -> String {
        let mut cells: Vec<(i32, String)> = ui
            .spec()
            .items
            .iter()
            .filter_map(|i| match &i.draw {
                Draw::Lines(l) => Some((i.rect.x, l.join(""))),
                _ => None,
            })
            .collect();
        cells.sort_by_key(|(x, _)| *x);
        cells.into_iter().map(|(_, s)| s).collect()
    }

    fn caret(ui: &Ui<UiMsg>) -> Option<(i32, i32)> {
        ui.spec().cursor.map(|c| (c.pos.x, c.pos.y))
    }

    /// The drawn text as visual rows, keyed by absolute `y` (an item's rect
    /// plus the line's index within it), each row's cells joined left to
    /// right. Unlike `row_text`, this keeps the rows apart, so a wrapped
    /// prompt reads as several lines. A `Draw::Lines` carries one string per
    /// visual row it occupies.
    fn rows_text(ui: &Ui<UiMsg>) -> Vec<String> {
        let mut by_row: std::collections::BTreeMap<i32, Vec<(i32, String)>> =
            std::collections::BTreeMap::new();
        for i in &ui.spec().items {
            if let Draw::Lines(lines) = &i.draw {
                for (n, line) in lines.iter().enumerate() {
                    by_row
                        .entry(i.rect.y + n as i32)
                        .or_default()
                        .push((i.rect.x, line.to_string()));
                }
            }
        }
        by_row
            .into_values()
            .map(|mut cells| {
                cells.sort_by_key(|(x, _)| *x);
                cells.into_iter().map(|(_, s)| s).collect()
            })
            .collect()
    }

    /// The painter's own test, on the description: a query longer than the
    /// row scrolls so its tail and the caret are in view, the caret riding
    /// the last column; moving the caret left moves the window with it; a
    /// short query sits after the label with the caret after it.
    #[test]
    fn a_long_query_scrolls_to_keep_the_caret_on_the_row() {
        let input: String = ('a'..='z').cycle().take(100).collect();
        let mut p = PromptRow {
            message: "Search: ".into(),
            input: input.clone(),
            cursor: 100,
            selection: None,
            dir: None,
            wraps: false,
        };
        let ui = frame(&p, 80);
        let text = row_text(&ui);
        assert!(text.starts_with("Search: "), "the label stays: {text:?}");
        // 8 label cells leave 72 for the input; scroll = 100 - 71 = 29.
        let tail: String = input.chars().skip(29).collect();
        assert!(text.ends_with(&tail), "the tail is in view: {text:?}");
        assert_eq!(caret(&ui), Some((79, 0)), "the caret rides the last column");

        p.cursor = 85;
        let ui = frame(&p, 80);
        let window: String = input.chars().skip(85 - 71).collect();
        assert!(
            row_text(&ui).ends_with(&window),
            "the window follows the caret"
        );
        assert_eq!(caret(&ui), Some((79, 0)));

        p.input = "abc".into();
        p.cursor = 3;
        let ui = frame(&p, 80);
        assert_eq!(row_text(&ui), "Search: abc");
        assert_eq!(caret(&ui), Some((11, 0)));
    }

    /// A press on the query reports the byte under the pointer, in the whole
    /// query's bytes even when the window is scrolled.
    #[test]
    fn a_press_on_the_query_names_the_byte_under_the_pointer() {
        let input: String = ('a'..='z').cycle().take(100).collect();
        let p = PromptRow {
            message: "Search: ".into(),
            input,
            cursor: 100,
            selection: None,
            dir: None,
            wraps: false,
        };
        let mut ui = frame(&p, 80);
        // Column 10 is the third cell of the input window, which starts at
        // byte 29.
        let d = ui.dispatch(fresh_ui::Input::press(
            Point::new(10, 0),
            fresh_ui::MouseButton::Left,
            fresh_ui::Mods::NONE,
        ));
        let got: Vec<UiFact> = d
            .msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect();
        assert_eq!(got, vec![UiFact::PromptInputPress { byte: 31 }]);
    }

    /// The file-open prompt shows the directory before the query, and cuts
    /// the middle of it out once the row would overflow.
    #[test]
    fn a_file_open_prompt_shows_and_truncates_the_directory() {
        let p = PromptRow {
            message: "Open: ".into(),
            input: "main.rs".into(),
            cursor: 7,
            selection: None,
            dir: Some("/home/me/src".into()),
            wraps: false,
        };
        let ui = frame(&p, 80);
        assert_eq!(row_text(&ui), "Open: /home/me/src/main.rs");
        assert_eq!(caret(&ui), Some((26, 0)));

        let deep = PromptRow {
            dir: Some("/home/me/projects/deep/nested/src".into()),
            ..p.clone()
        };
        let ui = frame(&deep, 40);
        let text = row_text(&ui);
        assert!(text.contains("[...]"), "truncated in the middle: {text:?}");
        assert!(text.ends_with("main.rs"), "the query is whole: {text:?}");
    }

    /// A confirmation prompt that's wider than the row wraps instead of
    /// losing its end, so "(C)ancel" is still on screen at 80 columns
    /// (issue #3214).
    #[test]
    fn a_long_confirmation_message_wraps_across_rows() {
        assert!(str_width(QUIT_EN) > 80, "the fixture overflows 80 columns");
        let ui = laid_out(&confirmation(QUIT_EN, ""), 80, 6);
        let rows = rows_text(&ui);

        assert_eq!(rows.len(), 2, "the message wrapped onto two rows: {rows:?}");
        assert!(
            rows.iter().any(|r| r.contains("(C)ancel")),
            "the cancel option is visible: {rows:?}"
        );
        assert!(
            rows.iter().all(|r| str_width(r) <= 80),
            "no visual row overflows the width: {rows:?}"
        );
        let (x, y) = caret(&ui).expect("the caret is still drawn");
        assert_eq!(
            (x as usize, y),
            (str_width(&rows[1]), 1),
            "the caret waits after the message: {rows:?}"
        );
    }

    /// Typing an answer doesn't undo the wrap. The confirmation prompts take
    /// the answer as typed input (press "d", then Enter), so checking whether
    /// the input is empty would put the prompt back on one clipped row the
    /// moment the user pressed a key.
    #[test]
    fn typing_an_answer_keeps_the_message_wrapped() {
        let ui = laid_out(&confirmation(QUIT_EN, "d"), 80, 6);
        let rows = rows_text(&ui);

        assert_eq!(rows.len(), 2, "still two rows: {rows:?}");
        assert!(
            rows.iter().any(|r| r.contains("(C)ancel")),
            "the cancel option is still visible: {rows:?}"
        );
        assert!(
            rows[1].ends_with("? d"),
            "the typed answer follows the message: {rows:?}"
        );
        let (x, y) = caret(&ui).expect("the caret is still drawn");
        assert_eq!(
            (x as usize, y),
            (str_width(&rows[1]), 1),
            "the caret sits after the answer: {rows:?}"
        );
    }

    /// A message that would need more than three rows stops at three, and
    /// the last row ends in an ellipsis so it's clear something was cut.
    #[test]
    fn a_message_too_long_for_three_rows_ends_in_an_ellipsis() {
        let ui = laid_out(&confirmation(QUIT_DE, ""), 30, 10);
        let rows = rows_text(&ui);

        assert_eq!(
            rows.len(),
            MAX_WRAPPED_ROWS,
            "capped at three rows: {rows:?}"
        );
        assert!(
            rows[2].ends_with(ELLIPSIS),
            "the last row shows the cut: {rows:?}"
        );
        assert!(
            rows.iter().all(|r| str_width(r) <= 30),
            "no visual row overflows the width: {rows:?}"
        );
        assert_eq!(caret(&ui).map(|(_, y)| y), Some(2), "caret on the last row");
    }

    /// When the message is cut, whatever the user typed still shows right
    /// after the ellipsis, with the caret after it.
    #[test]
    fn a_cut_message_still_shows_the_typed_answer() {
        let ui = laid_out(&confirmation(QUIT_DE, "v"), 30, 10);
        let rows = rows_text(&ui);

        assert_eq!(
            rows.len(),
            MAX_WRAPPED_ROWS,
            "capped at three rows: {rows:?}"
        );
        assert!(
            rows[2].ends_with("…v"),
            "the answer follows the ellipsis: {rows:?}"
        );
        assert!(str_width(&rows[2]) <= 30, "the last row fits: {rows:?}");
        let (x, y) = caret(&ui).expect("the caret is still drawn");
        assert_eq!((x as usize, y), (str_width(&rows[2]), 2));
    }

    /// A confirmation prompt that fits stays on one row: the wrap only kicks
    /// in when the message actually overflows.
    #[test]
    fn a_short_confirmation_message_stays_on_one_row() {
        let ui = laid_out(&confirmation("Discard changes? (y/n) ", ""), 80, 6);
        assert_eq!(rows_text(&ui), vec!["Discard changes? (y/n) ".to_string()]);
    }

    /// An input prompt stays on its one row even when its label is wider
    /// than the terminal and nothing has been typed yet, and it keeps its
    /// caret. This is the "Replace with (regex): " case from the review.
    #[test]
    fn an_input_prompt_with_a_long_label_stays_on_one_row() {
        let p = PromptRow {
            message: "Replace with (regex): ".into(),
            input: String::new(),
            cursor: 0,
            selection: None,
            dir: None,
            wraps: false,
        };
        let ui = laid_out(&p, 20, 6);
        assert_eq!(rows_text(&ui).len(), 1, "one row: {:?}", rows_text(&ui));
        assert_eq!(
            caret(&ui).map(|(_, y)| y),
            Some(0),
            "the caret is drawn on that row"
        );
    }

    /// The selected bytes are their own run, in the selection's ink.
    #[test]
    fn the_selection_is_its_own_run() {
        let p = PromptRow {
            message: "> ".into(),
            input: "hello".into(),
            cursor: 5,
            selection: Some((1, 3)),
            dir: None,
            wraps: false,
        };
        let ui = frame(&p, 40);
        let themed: Vec<(String, String)> = ui
            .spec()
            .items
            .iter()
            .filter_map(|i| match &i.draw {
                Draw::Lines(l) => Some((l.join(""), i.theme.as_str().to_string())),
                _ => None,
            })
            .collect();
        assert!(
            themed.iter().any(|(t, th)| t == "el" && th == &selection()),
            "{themed:?}"
        );
    }
}
