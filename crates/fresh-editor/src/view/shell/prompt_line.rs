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

use fresh_ui::{
    gesture, layout_reader, row, text_runs, Event, GestureKind, Key, LayoutInfo, MouseButton, Node,
    Run, Sizing,
};
use unicode_segmentation::UnicodeSegmentation;

use super::msg::{UiFact, UiMsg};
use crate::app::shell_host::shell_theme::pair;
use crate::primitives::display_width::str_width;
use crate::view::ui::status_bar::{input_hscroll, path_display_sep, truncate_path, TruncatedPath};

/// What the row shows: the prompt's message, its query, and — for the
/// file-open prompts — the directory the query completes in.
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
pub fn prompt_line(p: &PromptRow) -> Node<UiMsg> {
    let p = Rc::new(p.clone());
    layout_reader(move |info: LayoutInfo| build(&p, info.constraints.max_w))
        .h(Sizing::Cells(1))
        .theme(base())
}

/// The row at `width` cells: the label, then the input's window.
fn build(p: &PromptRow, width: u16) -> Node<UiMsg> {
    let label = label_runs(p, width);
    let label_cells: usize = label.iter().map(|r| str_width(&r.text)).sum();
    let label_cols = label_cells.min(width as usize) as u16;
    let input_cols = width - label_cols;

    let input = input_window(p, input_cols, true).flex(1);
    row().children([text_runs(label).w(Sizing::Cells(label_cols)), input])
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

    fn frame(p: &PromptRow, width: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(prompt_line(p), Size::new(width, 1));
        ui
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

    /// The selected bytes are their own run, in the selection's ink.
    #[test]
    fn the_selection_is_its_own_run() {
        let p = PromptRow {
            message: "> ".into(),
            input: "hello".into(),
            cursor: 5,
            selection: Some((1, 3)),
            dir: None,
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
