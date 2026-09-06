//! The file-open browser dialog, as a description.
//!
//! **The last painted interior.** Every other overlay the frame carries is a
//! tree by now; this one was a painter (`view/ui/file_browser.rs`) that laid
//! out its toggles, its shortcuts, its column headers and its entry rows,
//! *recorded the cell span of each* into a `FileBrowserLayout`, and handed the
//! layout to three hit tests (`handle_file_open_click`, `_double_click`,
//! `compute_file_browser_hover`) and one wheel handler that re-derived from
//! those spans what the painter had just known. The tree above it was a
//! pointer-only surface that reported screen coordinates and the recorded
//! spans answered them — the same seam the overlay prompt's toolbar sat on,
//! and the seam that made `Modality::Pointer` a painted surface's claim.
//!
//! What the painter measured, layout measures: a localized label with a live
//! keybinding string in it is a `text` node and its width is the tree's
//! answer. What the painter recorded, the tree keeps: every interactive
//! element is a keyed node that answers its own press, and the web reads the
//! rectangles back by key (`rects`) instead of from a side table. And what
//! the model kept for the painter — a scroll offset five places agreed on by
//! hand, and the viewport height the input handlers had to be told — is the
//! `List`'s: the window belongs to the viewport, which reveals the selection
//! when it moves and reports where it is on the bar it already draws.
//!
//! The dialog's placement was already the tree's (above the prompt row, as
//! wide as it); this lands the content, and with it the hit tests, the
//! coordinate facts, the hover targets for the rows and the bar, and the
//! `Window::file_browser_layout` side table are deleted.

use std::path::{Path, PathBuf};
use std::rc::Rc;

use fresh_i18n::t;
use fresh_ui::widgets::{Activate, List, RowState};
use fresh_ui::{
    col, gesture, layout_reader, row, stack, text, text_runs, Anchor, Elide, Event, Fit,
    GestureKind, Key, LayoutInfo, MouseButton, Node, Place, PointerMode, Run, Sizing,
};

use crate::app::file_open::{BrowserPart, SortMode, Toggle};
use crate::app::shell_host::shell_theme::{attrs, pair};
use crate::app::types::HoverTarget;
use crate::view::ui::status_bar::truncate_path;

use super::frame::{region_key, HostRegion};
use super::msg::{UiFact, UiMsg};

/// One of the two checkboxes on the dialog's first row.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ToggleItem {
    pub id: Toggle,
    /// The localized label.
    pub label: String,
    /// The character of the label to underline as its mnemonic, as a byte
    /// offset into `label`. The painter underlined the `E` of `Encoding` by
    /// splitting a hardcoded string; the label is localized now, so the
    /// builder finds the character and this says where it landed.
    pub mnemonic: Option<usize>,
    /// The keybinding that toggles it, when one is bound.
    pub shortcut: Option<String>,
    pub active: bool,
}

/// One of the navigation shortcuts on the second row: `..`, `/`, `~`, …
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Shortcut {
    pub label: String,
    pub description: String,
}

/// One entry of the directory, with its columns already formatted.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Entry {
    pub name: String,
    pub is_dir: bool,
    pub is_symlink: bool,
    /// `None` for a directory, or when the size is unknown; both print `--`.
    pub size: Option<String>,
    pub modified: Option<String>,
    /// Whether the entry matches the filter typed into the prompt. A
    /// non-matching entry is dimmed rather than hidden, and ignores hover.
    pub matches: bool,
}

/// What the list band shows.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Listing {
    Loading,
    Error(String),
    Entries(Vec<Entry>),
}

/// The dialog, as the shell states it.
#[derive(Clone, Debug, PartialEq)]
pub struct Browser {
    /// How tall the dialog is. App state rather than a measurement: the rule
    /// is "the space above the prompt, less the menu bar's row, capped at 20",
    /// and the row it must not cover is a fact about the frame's contents
    /// rather than about the dialog's own.
    pub height: u16,
    /// The directory being browsed. Its title, and the list's identity: a
    /// different directory is a different list, which starts at the top.
    pub dir: PathBuf,
    pub toggles: Vec<ToggleItem>,
    pub shortcuts: Vec<Shortcut>,
    /// The highlighted shortcut, when the keyboard is on the navigation row.
    pub selected_shortcut: Option<usize>,
    pub sort: SortMode,
    pub ascending: bool,
    pub listing: Listing,
    /// The selected entry, when the keyboard is on the list. Controlled: the
    /// editor holds it, and the list reveals it when it moves.
    pub selected: Option<usize>,
    /// Which of the dialog's own controls the pointer is over. The rows are
    /// not here: their hover is the `List`'s own state.
    pub hover: Option<BrowserPart>,
}

/// The dialog's key.
pub fn key() -> Key {
    Key::Str("file_browser".into())
}

/// The list's key, for the readers below.
pub fn list_key() -> Key {
    Key::Str("file_browser_list".into())
}

pub fn row_key(index: usize) -> Key {
    Key::Pair("file_browser_row".into(), index as u64)
}

pub fn toggle_key(t: Toggle) -> Key {
    Key::Pair("file_browser_toggle".into(), t as u64)
}

pub fn shortcut_key(index: usize) -> Key {
    Key::Pair("file_browser_shortcut".into(), index as u64)
}

pub fn column_key(m: SortMode) -> Key {
    Key::Pair("file_browser_column".into(), m as u64)
}

/// The three sortable columns, in the order the header shows them.
pub const COLUMNS: [SortMode; 3] = [SortMode::Name, SortMode::Size, SortMode::Modified];

/// The dialog as a layer above the prompt row, as wide as that row.
///
/// `stretch_to_anchor` is what "anchor to the prompt line's x (right of a left
/// dock, if any) so the picker never overlaps the dock column" was doing by
/// hand: the prompt row already starts right of the dock, and taking its whole
/// extent is one statement rather than an `x` and a `width` that have to agree
/// with it.
pub fn layer(b: &Browser) -> Node<UiMsg> {
    fresh_ui::layer()
        .anchor(Anchor::Node(region_key(HostRegion::PromptLine)))
        .place(Place::Above)
        .stretch_to_anchor()
        .fit(Fit::CLAMP)
        // **The pointer is the dialog's while it is up.** Nothing behind it
        // is interactive to a press, and a pointer over it must not be read
        // as a pointer over the buffer it covers (`modal_overlay_active` is
        // `Ui::modal_up`, and the LSP hover tracker asks it). The keyboard
        // is untouched: the browser's keys are the prompt's, through the
        // prompt's own `Modality::Focus` layer. See `Modality::Pointer`.
        .modality(fresh_ui::Modality::Pointer)
        .child(dialog(b).h(Sizing::Cells(b.height)).key(key()))
}

/// The dialog: a bordered box with the directory captioned on its top edge.
///
/// Stacked, as `modal::title_strip` explains: a caption given a row of its own
/// is a row the content no longer has.
fn dialog(b: &Browser) -> Node<UiMsg> {
    absorb(
        stack()
            .theme(pair("ui.popup_border_fg", "ui.popup_bg"))
            .children([
                col().border().child(
                    col()
                        .children([navigation(b), header(b), listing(b)])
                        .flex(1),
                ),
                title_strip(&b.dir),
            ]),
    )
}

/// A press that reaches the dialog's own chrome stops there.
///
/// The pointer-only surface this replaces claimed everything, and said why: a
/// right press and a triple-click had fallen through the painter's box to the
/// split underneath, and "a modal dialog that lets a triple-click select a line
/// of the buffer behind it is not a behaviour worth carrying over". The
/// controls stop their own presses, so this fires only where none did — the
/// border, the header's slack, the empty rows under a short listing.
///
/// The wheel needs nothing here. The list's viewport takes a notch that can
/// move its window, and the layer contains the rest: a wheel over the header,
/// or a sideways one over the rows, is absorbed at the dialog's edge rather
/// than handed to the buffer hidden beneath — which is the library's rule
/// for every floating surface, not a handler of this one's.
fn absorb(n: Node<UiMsg>) -> Node<UiMsg> {
    gesture(n).on(
        GestureKind::Press,
        Rc::new(|e: &Event| {
            e.stop();
            None
        }),
    )
}

/// The directory, on the top border, with the middle cut out when it does not
/// fit. `Block::title` with the highlighted `[...]` the painter built by hand
/// — measured against the width the tree gives the strip, which is what
/// `layout_reader` is for.
fn title_strip(dir: &Path) -> Node<UiMsg> {
    let ring = pair("ui.popup_border_fg", "ui.popup_bg");
    let mark = pair("ui.menu_highlight_fg", "ui.popup_bg");
    let dir = dir.to_path_buf();
    let clear = |n: Node<UiMsg>| n.pointer_mode(PointerMode::Transparent);
    clear(row().h(Sizing::Cells(1)).children([
        // `Block::title` starts one cell in from the corner.
        clear(row().w(Sizing::Cells(1))),
        clear(layout_reader(move |info: LayoutInfo| {
            // The painter's `width - 4`: the two corners and a cell of air on
            // either side of the text. The strip is already one cell in.
            let max = (info.constraints.max_w as usize).saturating_sub(3);
            let p = truncate_path(&dir, max);
            let runs = if p.truncated {
                vec![
                    Run::themed(" ", &ring),
                    Run::themed(&p.prefix, &ring),
                    Run::themed(format!("{}[...]", p.sep), &mark),
                    Run::themed(&p.suffix, &ring),
                    Run::themed(" ", &ring),
                ]
            } else {
                vec![Run::themed(format!(" {} ", p.to_string_plain()), &ring)]
            };
            text_runs(runs).theme(ring.clone())
        })),
        clear(row().flex(1)),
    ]))
}

fn press(fact: UiFact) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |e: &Event| {
        if e.button != MouseButton::Left {
            return None;
        }
        e.stop();
        Some(UiMsg::Ui(fact.clone()))
    })
}

fn hover(part: Option<BrowserPart>) -> fresh_ui::Handler<UiMsg> {
    Rc::new(move |_: &Event| Some(UiMsg::Ui(UiFact::Hover(part.map(HoverTarget::FileBrowser)))))
}

/// A control that reports its press and its hover, and is keyed so the web
/// can read where it landed.
fn control(n: Node<UiMsg>, k: Key, fact: UiFact, part: BrowserPart) -> Node<UiMsg> {
    gesture(n)
        .key(k)
        .on(GestureKind::Press, press(fact))
        .on_enter(hover(Some(part)))
        .on_leave(hover(None))
}

fn separator() -> Node<UiMsg> {
    text(" │ ").theme(pair("ui.help_separator_fg", "ui.popup_bg"))
}

/// The two rows above the header: the checkboxes, then the shortcuts.
///
/// One band for the press that lands on neither: the painter moved the
/// keyboard's section to the navigation row for "clicked in nav area but not
/// on a shortcut", and the controls stop their own presses, so what reaches
/// this handler is exactly that press.
fn navigation(b: &Browser) -> Node<UiMsg> {
    gesture(col().children([toggles(b), shortcuts(b)]))
        .on(GestureKind::Press, press(UiFact::BrowserNavigation))
}

fn toggles(b: &Browser) -> Node<UiMsg> {
    let mut cells: Vec<Node<UiMsg>> = vec![row().w(Sizing::Cells(1))];
    for (i, t) in b.toggles.iter().enumerate() {
        if i > 0 {
            cells.push(separator());
        }
        cells.push(toggle(t, b.hover == Some(BrowserPart::Toggle(t.id))));
    }
    row().h(Sizing::Cells(1)).children(cells)
}

/// `☑ Label (shortcut)`, with the mnemonic underlined inside the label.
fn toggle(t: &ToggleItem, hovered: bool) -> Node<UiMsg> {
    let ink = if hovered {
        pair("ui.menu_hover_fg", "ui.menu_hover_bg")
    } else if t.active {
        pair("ui.menu_highlight_fg", "ui.popup_bg")
    } else {
        pair("ui.help_key_fg", "ui.popup_bg")
    };
    let shortcut_ink = if hovered {
        ink.clone()
    } else {
        pair("ui.help_separator_fg", "ui.popup_bg")
    };
    let icon = if t.active { "☑ " } else { "☐ " };
    let mut runs = vec![Run::themed(icon, &ink)];
    match t.mnemonic.and_then(|at| {
        t.label
            .get(at..)
            .and_then(|s| s.chars().next())
            .map(|c| (at, c))
    }) {
        Some((at, c)) => {
            runs.push(Run::themed(&t.label[..at], &ink));
            runs.push(Run::themed(c.to_string(), attrs_of(&ink, &["underline"])));
            runs.push(Run::themed(&t.label[at + c.len_utf8()..], &ink));
        }
        None => runs.push(Run::themed(&t.label, &ink)),
    }
    if let Some(s) = &t.shortcut {
        runs.push(Run::themed(format!(" ({s})"), &shortcut_ink));
    }
    control(
        text_runs(runs).theme(ink),
        toggle_key(t.id),
        UiFact::BrowserToggle(t.id),
        BrowserPart::Toggle(t.id),
    )
}

/// A pair with attributes added — `attrs` takes names, and the pair here is
/// already one.
fn attrs_of(ink: &str, names: &[&str]) -> String {
    use crate::app::shell_host::shell_theme::{Attrs, Ink};
    match Ink::parse(ink) {
        Some(i) => i.plus(Attrs::all_named(names.iter().copied())).to_string(),
        None => ink.to_string(),
    }
}

fn shortcuts(b: &Browser) -> Node<UiMsg> {
    let mut cells: Vec<Node<UiMsg>> = vec![text(format!(" {}", t!("file_browser.navigation")))
        .theme(pair("ui.help_separator_fg", "ui.popup_bg"))];
    for (i, s) in b.shortcuts.iter().enumerate() {
        if i > 0 {
            cells.push(separator());
        }
        let ink = if b.selected_shortcut == Some(i) {
            attrs("ui.popup_text_fg", "ui.suggestion_selected_bg", &["bold"])
        } else if b.hover == Some(BrowserPart::Shortcut(i)) {
            pair("ui.menu_hover_fg", "ui.menu_hover_bg")
        } else {
            pair("ui.help_key_fg", "ui.popup_bg")
        };
        cells.push(control(
            text(format!(" {} ", s.label)).theme(ink),
            shortcut_key(i),
            UiFact::BrowserShortcut(i),
            BrowserPart::Shortcut(i),
        ));
    }
    row().h(Sizing::Cells(1)).children(cells)
}

/// The painter's column widths: the size column, and the modified column
/// with the two cells of air before it.
const SIZE_W: u16 = 10;
const MODIFIED_W: u16 = 14;
const GAP: &str = "  ";

/// The sortable header. The name column takes what the fixed two leave,
/// which is what `width - size - date - 4` computed.
fn header(b: &Browser) -> Node<UiMsg> {
    let base = attrs("ui.help_key_fg", "ui.menu_dropdown_bg", &["bold"]);
    let arrow = if b.ascending { "▲" } else { "▼" };
    let mark = |m: SortMode| if b.sort == m { arrow } else { " " };
    let ink = |m: SortMode| -> String {
        if b.sort == m {
            attrs("ui.menu_highlight_fg", "ui.menu_dropdown_bg", &["bold"])
        } else if b.hover == Some(BrowserPart::Column(m)) {
            attrs("ui.menu_hover_fg", "ui.menu_hover_bg", &["bold"])
        } else {
            base.clone()
        }
    };
    let column = |m: SortMode, label: String| {
        control(
            text(label).theme(ink(m)),
            column_key(m),
            UiFact::BrowserSort(m),
            BrowserPart::Column(m),
        )
    };
    let name = format!(" {}{}", t!("file_browser.name"), mark(SortMode::Name));
    let size = format!(
        "{:>w$}",
        format!("{}{}", t!("file_browser.size"), mark(SortMode::Size)),
        w = SIZE_W as usize
    );
    let modified = format!(
        "{GAP}{:>w$}",
        format!(
            "{}{}",
            t!("file_browser.modified"),
            mark(SortMode::Modified)
        ),
        w = MODIFIED_W as usize
    );
    row().h(Sizing::Cells(1)).theme(base.clone()).children([
        column(SortMode::Name, name).flex(1),
        column(SortMode::Size, size).w(Sizing::Cells(SIZE_W)),
        // Absorbs the rest of the row, as the painter's span did, so every
        // press on the header lands on some column.
        column(SortMode::Modified, modified).w(Sizing::Cells(MODIFIED_W + GAP.len() as u16)),
    ])
}

/// The list band: the rows, or the one line that says why there are none.
fn listing(b: &Browser) -> Node<UiMsg> {
    let dim = pair("ui.help_separator_fg", "ui.popup_bg");
    // Keyed as the list is: the band is the same band, and the web's
    // `listRect` — where its wheel is aimed — is this rectangle either way.
    let notice = |s: String, ink: String| {
        col()
            .flex(1)
            .key(list_key())
            .child(text(s).theme(ink).elide(Elide::Tail).h(Sizing::Cells(1)))
    };
    match &b.listing {
        Listing::Loading => notice(t!("file_browser.loading").to_string(), dim),
        Listing::Error(e) => notice(
            t!("file_browser.error", error = e).to_string(),
            pair("diagnostic.error_fg", "ui.popup_bg"),
        ),
        Listing::Entries(rows) if rows.is_empty() => {
            notice(format!(" {}", t!("file_browser.empty")), dim)
        }
        Listing::Entries(rows) => entries(b, rows),
    }
}

/// A row's ground, by the painter's ladder. A non-matching row ignores hover:
/// `base_style` picked the dim arm before the hover one.
fn row_bg(matches: bool, st: RowState) -> &'static str {
    match st {
        RowState::Selected | RowState::SelectedBlur => "ui.suggestion_selected_bg",
        RowState::Hover if matches => "ui.menu_hover_bg",
        _ => "ui.popup_bg",
    }
}

fn row_theme(matches: bool, st: RowState) -> String {
    let bg = row_bg(matches, st);
    match st {
        RowState::Selected | RowState::SelectedBlur => pair("ui.popup_text_fg", bg),
        RowState::Hover if matches => pair("ui.menu_hover_fg", bg),
        _ if !matches => attrs("ui.help_separator_fg", bg, &["dim"]),
        _ => pair("ui.popup_text_fg", bg),
    }
}

/// A directory's name in the accent colour, over the row's own ground — and
/// still dim when the row is.
fn dir_theme(matches: bool, st: RowState) -> String {
    let bg = row_bg(matches, st);
    if matches {
        pair("ui.help_key_fg", bg)
    } else {
        attrs("ui.help_key_fg", bg, &["dim"])
    }
}

fn entry_row(e: &Entry, st: RowState) -> Node<UiMsg> {
    let t = row_theme(e.matches, st);
    let selected = matches!(st, RowState::Selected | RowState::SelectedBlur);
    let name = if e.is_dir {
        format!("{}/", e.name)
    } else if e.is_symlink {
        format!("{}@", e.name)
    } else {
        e.name.clone()
    };
    let name_ink = if e.is_dir && !selected {
        dir_theme(e.matches, st)
    } else {
        t.clone()
    };
    row().h(Sizing::Cells(1)).children([
        text(name).theme(name_ink).elide(Elide::Tail).flex(1),
        text(format!(
            "{:>w$}",
            e.size.as_deref().unwrap_or("--"),
            w = SIZE_W as usize
        ))
        .theme(t.clone())
        .w(Sizing::Cells(SIZE_W)),
        text(format!(
            "{GAP}{:>w$}",
            e.modified.as_deref().unwrap_or("--"),
            w = MODIFIED_W as usize
        ))
        .theme(t)
        .w(Sizing::Cells(MODIFIED_W + GAP.len() as u16)),
    ])
}

/// The scope a directory's list lives in: a new directory is a new list.
///
/// The rows are keyed by index, so without this the list that shows `/etc`
/// after `/usr` would be the *same* element with different rows in it — and
/// keep the window `/usr` had been wheeled to. `set_entries` reset
/// `scroll_offset` to zero for the same reason; the scope says it once, as
/// identity rather than as a write.
fn dir_scope(dir: &Path) -> String {
    format!("file_browser:{}", dir.display())
}

fn entries(b: &Browser, rows: &[Entry]) -> Node<UiMsg> {
    let rows = Rc::new(rows.to_vec());
    let for_row = rows.clone();
    let for_theme = rows.clone();
    let list = List::windowed_stateful(rows.len(), row_key, move |i, st| match for_row.get(i) {
        Some(e) => entry_row(e, st),
        None => row().h(Sizing::Cells(1)),
    })
    .row_theme(move |i, st| row_theme(for_theme.get(i).is_none_or(|e| e.matches), st))
    // Controlled, and empty is a real state: a fresh directory has no
    // selection until the user types or moves.
    .selection(b.selected)
    // The bar's column is always reserved, as the painter's
    // `scrollbar_width` was, so a list that grows past its box does not
    // reflow its columns by a cell.
    .scrollbar_gutter()
    .scrollbar_theme(pair("ui.scrollbar_thumb_fg", "ui.scrollbar_track_fg"))
    // A click selects (and puts the name in the prompt); the second opens.
    .activate_on(Activate::DoubleClick)
    // The keyboard is the prompt's: Up and Down reach `FileOpenState`
    // through `FileBrowserInputHandler`, and the selection comes back here
    // every frame.
    .focusable(false)
    .on_select(|i| UiMsg::Ui(UiFact::BrowserSelect(i)))
    .on_activate(|i| UiMsg::Ui(UiFact::BrowserActivate(i)));
    fresh_ui::scope(
        dir_scope(&b.dir),
        col()
            .flex(1)
            .key(list_key())
            .child(fresh_ui::ComponentExt::node(list)),
    )
    .flex(1)
}

/// Where the dialog's parts landed, read back off the laid-out tree.
///
/// What `FileBrowserLayout` was — minus the hit tests, which the nodes answer
/// themselves. The one reader left is the web projection, which draws its own
/// card from these and sends its clicks to the cells the tree placed the
/// controls at.
#[derive(Debug, Clone, Default)]
pub struct Rects {
    pub dialog: ratatui::layout::Rect,
    /// The rows' band; also the bar's height.
    pub list: ratatui::layout::Rect,
    /// The bar's track, when the list overflows.
    pub scrollbar: Option<ratatui::layout::Rect>,
    pub toggles: Vec<(Toggle, ratatui::layout::Rect)>,
    pub shortcuts: Vec<ratatui::layout::Rect>,
    pub columns: Vec<(SortMode, ratatui::layout::Rect)>,
}

pub fn rects(
    ui: &fresh_ui::Ui<UiMsg>,
    size: ratatui::layout::Rect,
    shortcuts: usize,
) -> Option<Rects> {
    let at = |k: Key| super::rect_of(ui, &k, size);
    let dialog = at(key())?;
    Some(Rects {
        dialog,
        list: at(list_key()).unwrap_or_default(),
        scrollbar: scrollbar_item(ui.spec()).map(|i| super::screen_rect(i.rect, size)),
        toggles: [Toggle::ShowHidden, Toggle::DetectEncoding]
            .into_iter()
            .filter_map(|t| Some((t, at(toggle_key(t))?)))
            .collect(),
        shortcuts: (0..shortcuts).filter_map(|i| at(shortcut_key(i))).collect(),
        columns: COLUMNS
            .into_iter()
            .filter_map(|m| Some((m, at(column_key(m))?)))
            .collect(),
    })
}

fn scrollbar_item(spec: &fresh_ui::LayoutSpec) -> Option<&fresh_ui::Item> {
    let key = list_key();
    let range = spec.index.iter().find(|(k, _)| *k == key)?.1.clone();
    spec.items[range]
        .iter()
        .find(|i| matches!(i.draw, fresh_ui::Draw::Scrollbar { .. }))
}

/// Which slice of the list is on screen, as `(first, visible)`, and where
/// the bar's thumb sits within its track as `(start, end)`.
///
/// **The framework owns the window; this reads it.** `FileOpenState::
/// scroll_offset` was written by the wheel, the scrollbar click, every
/// selection move and `set_entries`, and read by the painter and the web —
/// agreeing by hand about one number. A list that fits its box emits no bar
/// and answers a window from zero with a thumb that fills the track.
pub fn window(ui: &fresh_ui::Ui<UiMsg>, size: ratatui::layout::Rect) -> Option<Window> {
    let list = super::rect_of(ui, &list_key(), size)?;
    let visible = list.height as usize;
    let bar = scrollbar_item(ui.spec()).and_then(|i| match i.draw {
        fresh_ui::Draw::Scrollbar {
            offset,
            content,
            window,
            ..
        } => Some((offset, content, window, i.rect.h)),
        _ => None,
    });
    Some(match bar {
        Some((offset, content, window, track)) => {
            let (top, len) = fresh_ui::Draw::scrollbar_thumb(offset, content, window as u32, track);
            Window {
                first: offset as usize,
                visible,
                thumb: (top as usize, (top + len) as usize),
            }
        }
        None => Window {
            first: 0,
            visible,
            thumb: (0, visible),
        },
    })
}

/// See [`window`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Window {
    pub first: usize,
    pub visible: usize,
    /// In rows from the top of the track: `(start, end)`.
    pub thumb: (usize, usize),
}

/// Every name this module can hand to `shell_theme`. The guard test walks it.
#[cfg(test)]
fn every_theme_name() -> Vec<String> {
    let mut out = vec![
        pair("ui.popup_border_fg", "ui.popup_bg"),
        pair("ui.menu_highlight_fg", "ui.popup_bg"),
        pair("ui.help_separator_fg", "ui.popup_bg"),
        pair("ui.menu_hover_fg", "ui.menu_hover_bg"),
        pair("ui.help_key_fg", "ui.popup_bg"),
        attrs("ui.popup_text_fg", "ui.suggestion_selected_bg", &["bold"]),
        attrs("ui.help_key_fg", "ui.menu_dropdown_bg", &["bold"]),
        attrs("ui.menu_highlight_fg", "ui.menu_dropdown_bg", &["bold"]),
        attrs("ui.menu_hover_fg", "ui.menu_hover_bg", &["bold"]),
        pair("diagnostic.error_fg", "ui.popup_bg"),
        pair("ui.scrollbar_thumb_fg", "ui.scrollbar_track_fg"),
    ];
    for st in [
        RowState::Normal,
        RowState::Hover,
        RowState::Selected,
        RowState::SelectedBlur,
    ] {
        for matches in [true, false] {
            out.push(row_theme(matches, st));
            out.push(dir_theme(matches, st));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, region_key, Frame};
    use fresh_ui::{Axis, Input, Mods, Point, Size, Ui};

    fn entry(i: usize) -> Entry {
        Entry {
            name: format!("file_{i:02}.txt"),
            is_dir: i == 0,
            is_symlink: false,
            size: (i > 0).then(|| format!("{i} B")),
            modified: Some("just now".into()),
            matches: true,
        }
    }

    fn browser(n: usize, height: u16) -> Browser {
        Browser {
            height,
            dir: PathBuf::from("/home/u/projects/fresh"),
            toggles: vec![
                ToggleItem {
                    id: Toggle::ShowHidden,
                    label: "Show Hidden".into(),
                    mnemonic: None,
                    shortcut: Some("Alt+.".into()),
                    active: false,
                },
                ToggleItem {
                    id: Toggle::DetectEncoding,
                    label: "Detect Encoding".into(),
                    mnemonic: Some(7),
                    shortcut: Some("Alt+E".into()),
                    active: true,
                },
            ],
            shortcuts: vec![
                Shortcut {
                    label: "..".into(),
                    description: "Parent directory".into(),
                },
                Shortcut {
                    label: "/".into(),
                    description: "Root directory".into(),
                },
                Shortcut {
                    label: "~".into(),
                    description: "Home directory".into(),
                },
            ],
            selected_shortcut: None,
            sort: SortMode::Name,
            ascending: true,
            listing: Listing::Entries((0..n).map(entry).collect()),
            selected: None,
            hover: None,
        }
    }

    fn frame(b: Browser, dock: Option<u16>, w: u16, h: u16, ui: &mut Ui<UiMsg>) {
        ui.frame(
            frame_tree(Frame {
                prompt_line: true,
                dock,
                browser: Some(b),
                ..Frame::default()
            }),
            Size::new(w, h),
        );
    }

    fn laid_out(b: Browser, dock: Option<u16>, w: u16, h: u16) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        frame(b, dock, w, h, &mut ui);
        ui
    }

    fn facts(d: fresh_ui::Dispatch<UiMsg>) -> Vec<UiFact> {
        d.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .filter(|f| *f != UiFact::ClearTabMenus)
            .collect()
    }

    fn rect(ui: &Ui<UiMsg>, k: Key) -> fresh_ui::Rect {
        ui.rect_of(
            ui.find_by_key(&k)
                .unwrap_or_else(|| panic!("{k:?} in the tree")),
        )
    }

    fn click(ui: &mut Ui<UiMsg>, at: Point, n: u8) -> Vec<UiFact> {
        let mut out = facts(ui.dispatch(Input::press_n(at, MouseButton::Left, Mods::NONE, n)));
        out.extend(facts(ui.dispatch(Input::release(
            at,
            MouseButton::Left,
            Mods::NONE,
        ))));
        out
    }

    fn size(w: u16, h: u16) -> ratatui::layout::Rect {
        ratatui::layout::Rect::new(0, 0, w, h)
    }

    /// **The dialog is the prompt row's width, sitting on top of it.**
    #[test]
    fn it_sits_above_the_prompt_row_and_is_as_wide_as_it() {
        let ui = laid_out(browser(5, 20), None, 120, 40);
        let prompt = ui.rect_of(
            ui.find_by_key(&region_key(HostRegion::PromptLine))
                .expect("the prompt row"),
        );
        let b = rect(&ui, key());
        assert_eq!((b.x, b.w), (prompt.x, prompt.w), "as wide as the row");
        assert_eq!(b.y + b.h as i32, prompt.y, "directly above it");
        assert_eq!(b.h, 20);
    }

    /// And with a dock it starts right of the column, because the prompt row
    /// does.
    #[test]
    fn a_dock_moves_it_right_because_the_prompt_row_moves_right() {
        let ui = laid_out(browser(5, 20), Some(24), 120, 40);
        let b = rect(&ui, key());
        assert_eq!(b.x, 24, "right of the dock column");
        assert_eq!(b.w, 120 - 24);
    }

    /// **The interior is the painter's, row for row.** Two navigation rows,
    /// the header, then the entries, inside the ring — the geometry
    /// `FileBrowserLayout` recorded, read off the tree instead.
    #[test]
    fn the_rows_are_where_the_painter_put_them() {
        let ui = laid_out(browser(5, 20), None, 80, 30);
        let b = rect(&ui, key());
        let toggle = rect(&ui, toggle_key(Toggle::ShowHidden));
        let shortcut = rect(&ui, shortcut_key(0));
        let column = rect(&ui, column_key(SortMode::Name));
        let list = rect(&ui, list_key());
        assert_eq!(toggle.y, b.y + 1, "checkboxes on the first inner row");
        assert_eq!(shortcut.y, b.y + 2, "shortcuts under them");
        assert_eq!(column.y, b.y + 3, "then the header");
        assert_eq!(list.y, b.y + 4, "then the rows");
        assert_eq!(
            list.y + list.h as i32,
            b.y + b.h as i32 - 1,
            "down to the ring"
        );
        assert_eq!(rect(&ui, row_key(4)).y, list.y + 4);
        // Every control has a width the web can aim a click at.
        for k in [
            toggle_key(Toggle::DetectEncoding),
            shortcut_key(2),
            column_key(SortMode::Size),
            column_key(SortMode::Modified),
        ] {
            assert!(rect(&ui, k.clone()).w > 0, "{k:?} has cells");
        }
    }

    /// A click on a row selects it and a double click opens it — by index,
    /// which the row knows, rather than by a coordinate the painter recorded.
    #[test]
    fn a_click_on_a_row_selects_it_and_a_double_click_opens_it() {
        let mut ui = laid_out(browser(5, 20), None, 80, 30);
        let r = rect(&ui, row_key(2));
        let at = Point::new(r.x + 3, r.y);
        assert!(click(&mut ui, at, 1).contains(&UiFact::BrowserSelect(2)));
        assert!(click(&mut ui, at, 2).contains(&UiFact::BrowserActivate(2)));
    }

    /// The controls answer their own presses; the tree no longer reports a
    /// coordinate for a side table to resolve.
    #[test]
    fn the_toggles_shortcuts_and_headers_answer_their_own_presses() {
        let mut ui = laid_out(browser(5, 20), None, 80, 30);
        let mid = |r: fresh_ui::Rect| Point::new(r.x + (r.w as i32) / 2, r.y);
        let cases: Vec<(Key, UiFact)> = vec![
            (
                toggle_key(Toggle::ShowHidden),
                UiFact::BrowserToggle(Toggle::ShowHidden),
            ),
            (
                toggle_key(Toggle::DetectEncoding),
                UiFact::BrowserToggle(Toggle::DetectEncoding),
            ),
            (shortcut_key(1), UiFact::BrowserShortcut(1)),
            (
                column_key(SortMode::Size),
                UiFact::BrowserSort(SortMode::Size),
            ),
            (
                column_key(SortMode::Modified),
                UiFact::BrowserSort(SortMode::Modified),
            ),
        ];
        for (k, want) in cases {
            let at = mid(rect(&ui, k.clone()));
            let got = facts(ui.dispatch(Input::press(at, MouseButton::Left, Mods::NONE)));
            assert_eq!(got, vec![want], "{k:?}");
        }
    }

    /// A press on the navigation band off any shortcut moves the keyboard
    /// there, as the painter's "clicked in nav area but not on a shortcut"
    /// arm did.
    #[test]
    fn a_press_beside_the_shortcuts_moves_the_keyboard_to_the_navigation_row() {
        let mut ui = laid_out(browser(5, 20), None, 80, 30);
        let s = rect(&ui, shortcut_key(0));
        // The " Navigation: " label, left of the first shortcut.
        let at = Point::new(s.x - 4, s.y);
        let got = facts(ui.dispatch(Input::press(at, MouseButton::Left, Mods::NONE)));
        assert_eq!(got, vec![UiFact::BrowserNavigation]);
    }

    /// **A right press and a triple stop here.** The painter's box was not
    /// opaque, so both fell through to the split beneath.
    #[test]
    fn a_right_press_stops_at_the_dialog() {
        let mut ui = laid_out(browser(5, 20), None, 120, 40);
        let b = rect(&ui, key());
        let got = ui.dispatch(Input::press(
            Point::new(b.x + 12, b.y + 5),
            MouseButton::Right,
            Mods::NONE,
        ));
        assert!(got.claimed, "the dialog takes it");
        assert!(facts(got).is_empty(), "and does nothing with it");
    }

    /// **The window is the viewport's, and it is read back.** The wheel over
    /// the rows moves it; a sideways wheel is absorbed rather than panning
    /// the buffer hidden beneath.
    #[test]
    fn the_wheel_scrolls_the_list_and_the_sideways_one_is_absorbed() {
        let mut ui = laid_out(browser(40, 12), None, 80, 30);
        let list = rect(&ui, list_key());
        let at = Point::new(list.x + 4, list.y + 2);
        assert_eq!(window(&ui, size(80, 30)).map(|w| w.first), Some(0));
        let down = ui.dispatch(Input::Wheel {
            pos: at,
            delta: 3,
            axis: Axis::Vertical,
            mods: Mods::NONE,
        });
        assert!(down.claimed);
        frame(browser(40, 12), None, 80, 30, &mut ui);
        let w = window(&ui, size(80, 30)).expect("a window");
        assert_eq!(w.first, 3, "the wheel moved the window: {w:?}");
        assert_eq!(w.visible, list.h as usize);
        assert!(
            w.thumb.1 - w.thumb.0 < w.visible && w.thumb.1 <= w.visible,
            "the thumb is shorter than the track it rides: {w:?}"
        );
        let side = ui.dispatch(Input::Wheel {
            pos: at,
            delta: 3,
            axis: Axis::Horizontal,
            mods: Mods::NONE,
        });
        assert!(side.claimed, "absorbed");
        assert!(facts(side).is_empty(), "and it means nothing here");
    }

    /// Regression for #245, on the list's own terms: a viewport of three rows
    /// keeps the selection on screen wherever the keyboard moves it. The
    /// model's `clamp_scroll_to_selection` did this with a height the
    /// renderer had to report back; the `List` reveals a controlled
    /// selection when it changes.
    #[test]
    fn the_selection_is_revealed_when_it_moves() {
        // Ring 2 + navigation 2 + header 1 + three rows.
        let mut ui = laid_out(browser(10, 8), None, 80, 30);
        assert_eq!(rect(&ui, list_key()).h, 3);
        let mut b = browser(10, 8);
        for sel in [4usize, 9, 0] {
            b.selected = Some(sel);
            frame(b.clone(), None, 80, 30, &mut ui);
            let w = window(&ui, size(80, 30)).expect("a window");
            assert!(
                (w.first..w.first + w.visible).contains(&sel),
                "selection {sel} outside the window {w:?}"
            );
        }
    }

    /// A new directory is a new list, and it starts at the top — what
    /// `set_entries` reset `scroll_offset` for.
    #[test]
    fn a_new_directory_starts_at_the_top() {
        let mut ui = laid_out(browser(40, 12), None, 80, 30);
        let list = rect(&ui, list_key());
        ui.dispatch(Input::Wheel {
            pos: Point::new(list.x + 4, list.y + 2),
            delta: 5,
            axis: Axis::Vertical,
            mods: Mods::NONE,
        });
        frame(browser(40, 12), None, 80, 30, &mut ui);
        assert_eq!(window(&ui, size(80, 30)).map(|w| w.first), Some(5));
        let mut elsewhere = browser(40, 12);
        elsewhere.dir = PathBuf::from("/etc");
        frame(elsewhere, None, 80, 30, &mut ui);
        assert_eq!(window(&ui, size(80, 30)).map(|w| w.first), Some(0));
    }

    /// The web's readers: every rectangle the projection draws from is on the
    /// tree, and the bar is there exactly when the list overflows.
    #[test]
    fn the_rects_are_read_back_and_the_bar_appears_only_on_overflow() {
        let ui = laid_out(browser(5, 20), None, 80, 30);
        let r = rects(&ui, size(80, 30), 3).expect("rects");
        assert_eq!(r.toggles.len(), 2);
        assert_eq!(r.shortcuts.len(), 3);
        assert_eq!(r.columns.len(), 3);
        assert!(r.scrollbar.is_none(), "five rows fit");
        assert_eq!(r.list.height, 20 - 2 - 3);
        let ui = laid_out(browser(40, 12), None, 80, 30);
        let r = rects(&ui, size(80, 30), 3).expect("rects");
        let bar = r.scrollbar.expect("forty rows overflow");
        assert_eq!(bar.height, r.list.height);
        assert_eq!(
            bar.x,
            r.list.x + r.list.width - 1,
            "on the list's last column"
        );
    }

    /// The loading, error and empty states are one line each, in the band
    /// the rows would occupy.
    #[test]
    fn the_empty_states_take_the_list_band() {
        let mut b = browser(0, 12);
        for listing in [
            Listing::Loading,
            Listing::Error("denied".into()),
            Listing::Entries(vec![]),
        ] {
            b.listing = listing;
            let ui = laid_out(b.clone(), None, 80, 30);
            assert_eq!(
                rect(&ui, list_key()).h,
                12 - 2 - 3,
                "the band is still there"
            );
            let w = window(&ui, size(80, 30)).expect("a window over nothing");
            assert_eq!((w.first, w.visible), (0, 7));
        }
    }

    /// `shell_theme`'s contract is that a name is real theme keys, and a name
    /// it does not know falls back to the base style *silently*.
    #[test]
    fn every_theme_name_is_a_real_key() {
        use crate::view::theme::Theme;
        let theme = Theme::from_json(r#"{"name":"test"}"#).expect("defaults");
        for name in every_theme_name() {
            let (fg, bg) = crate::app::shell_host::shell_theme::names(&name);
            for half in [fg, bg] {
                let half = half.unwrap_or_else(|| panic!("{name:?} has an unnamed half"));
                assert!(
                    theme.resolve_theme_key(&half).is_some(),
                    "{half:?} (in {name:?}) is not a theme key"
                );
            }
        }
    }
}
