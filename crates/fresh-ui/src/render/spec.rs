//! The display list.
//!
//! Paint does not touch cells. It produces a flat, ordered, absolute, keyed
//! list, and backends are folds over it: a TUI writes cells, a web backend
//! patches DOM nodes by key, tests assert on the list itself rather than
//! scraping a rendered screen.

use std::ops::Range;
use std::rc::Rc;

use crate::desc::{HostId, Scrim};
use crate::element::ElementId;
use crate::key::Key;

use super::geom::{Point, Rect, Size};

/// One frame's worth of drawing, in paint order.
#[derive(Clone, Debug, Default)]
pub struct LayoutSpec {
    pub frame: Size,
    /// Paint order *is* list order.
    pub items: Vec<Item>,
    /// Where the out-of-flow half begins.
    ///
    /// Layers paint after the tree they were declared in, so every item a
    /// `Layer` produced — including a scrim, which belongs to no keyed
    /// subtree — sits in one contiguous tail starting here. `items.len()` when
    /// no layer painted.
    ///
    /// A backend that draws content of its own *between* in-flow and
    /// out-of-flow content needs this: a host mid-migration paints its own
    /// surfaces between the two, and one covering the other is exactly what
    /// the split decides. Deriving it from the outside is not possible — a
    /// layer need not be keyed (`widgets::Dropdown` is not) and a scrim never
    /// is — which is why the library reports it rather than leaving each
    /// backend to guess.
    pub layers_from: usize,
    /// Key to the range of items its subtree produced. Used for hit-testing
    /// shortcuts, for patching a retained backend, and by tests.
    pub index: Vec<(Key, Range<usize>)>,
    pub cursor: Option<CursorSpec>,
}

impl LayoutSpec {
    pub fn clear(&mut self) {
        self.items.clear();
        self.index.clear();
        self.layers_from = 0;
        self.cursor = None;
    }

    /// The in-flow half: everything the tree itself painted.
    pub fn in_flow(&self) -> &[Item] {
        &self.items[..self.layers_from.min(self.items.len())]
    }

    /// The out-of-flow half: everything the layers painted, scrims included.
    pub fn layers(&self) -> &[Item] {
        &self.items[self.layers_from.min(self.items.len())..]
    }

    /// The items a keyed subtree produced.
    pub fn items_for(&self, key: &Key) -> &[Item] {
        match self.index.iter().find(|(k, _)| k == key) {
            Some((_, r)) => &self.items[r.clone()],
            None => &[],
        }
    }

    /// Every item whose visible rectangle is non-empty.
    pub fn visible(&self) -> impl Iterator<Item = &Item> {
        self.items.iter().filter(|i| !i.visible_rect().is_empty())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    pub key: Option<Key>,
    /// The element that produced it, so a backend can correlate without a key.
    pub id: ElementId,
    /// Absolute position.
    pub rect: Rect,
    /// Ancestor clips intersected.
    pub clip: Rect,
    /// Where this item's appearance comes from.
    pub theme: ThemeKey,
    pub draw: Draw,
}

impl Item {
    pub fn visible_rect(&self) -> Rect {
        self.rect.intersect(self.clip)
    }
}

/// Per-item provenance: the nearest enclosing `theme(..)` tag. A backend maps
/// it to colours; the library never interprets it.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct ThemeKey(pub Option<Rc<str>>);

impl ThemeKey {
    pub fn as_str(&self) -> &str {
        self.0.as_deref().unwrap_or("")
    }
}
/// Which corner glyphs a [`Draw::Border`] is drawn with.
///
/// **A border's corners are a description, not a backend default.** The fold
/// drew ratatui's `BorderType::Plain` for every box, which is right for the
/// editor's chrome and wrong for a plugin panel: a `WidgetSpec` card and a
/// labelled section have always worn `╭╮╰╯`, and describing one turned it
/// square. A backend that has only one corner set can round to whichever it
/// draws; a backend that has both is told which was meant.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default, Hash)]
pub enum BorderStyle {
    /// `┌┐└┘` — ratatui's `BorderType::Plain`, and the editor's chrome.
    #[default]
    Plain,
    /// `╭╮╰╯` — the plugin widget vocabulary's card and labelled section.
    Rounded,
    /// `┏┓┗┛` with heavy edges — a colour-independent "this one is selected"
    /// marker. `mark_list_card_selected` swaps a card's light glyphs for these
    /// rather than banding the row, "even when colours are too subtle"; a
    /// description that could not name it would lose the marker on any theme
    /// whose selection colour is faint.
    Heavy,
}

impl BorderStyle {
    /// The six glyphs, in the order a box is drawn: horizontal, vertical, and
    /// the four corners clockwise from the top left.
    pub fn glyphs(self) -> (char, char, char, char, char, char) {
        match self {
            BorderStyle::Plain => ('─', '│', '┌', '┐', '┘', '└'),
            BorderStyle::Rounded => ('─', '│', '╭', '╮', '╯', '╰'),
            BorderStyle::Heavy => ('━', '┃', '┏', '┓', '┛', '┗'),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Draw {
    /// A background region.
    Fill,
    /// A background laid over what was painted before it, keeping the text
    /// there: the region inherits its content and takes the theme's ground.
    Wash,
    /// A box outline drawn inside `rect`, in the given corner style.
    Border(BorderStyle),
    /// Covers everything painted before it.
    Scrim(Scrim),
    /// Text, one entry per visual row.
    ///
    /// A row's columns are what [`glyph::glyphs`](crate::render::glyph::glyphs)
    /// says they are: a backend paints each cluster it yields at the column it
    /// names and blanks the continuation cells of a wide one. Stepping one cell
    /// per `char` puts everything after a wide glyph two cells left of where
    /// layout measured it.
    Lines(Vec<Rc<str>>),
    Scrollbar {
        /// How far the window has travelled, in whatever the viewport counts.
        offset: u32,
        /// Total extent of the content, in the same unit. Wider than a
        /// coordinate, because a million-row list has no cell extent that fits
        /// one.
        content: u32,
        /// Extent of the window, in that same unit — *not* in cells. For a
        /// cell-scrolling viewport the two coincide and the window is the
        /// track's height; for an item-scrolling one it is a count of items,
        /// and a bar drawn from the track instead would say a list of
        /// five-row cards is five times as visible as it is.
        window: u16,
        /// Which way the track runs. A vertical bar's track is its
        /// rectangle's height and its thumb travels down it; a horizontal
        /// bar's is the width. The item's rectangle is one cell thick across
        /// the other axis.
        axis: crate::event::Axis,
        /// Marks on the track: an overview of where things are in the
        /// content — search hits, diagnostics, unsaved changes — each at a
        /// track cell, in its own theme. Part of the bar's own item rather
        /// than a second overlay measured against it, so a backend paints the
        /// mark and the thumb it may share a cell with together. `full`
        /// paints the whole cell in the mark's theme (a highlighted track
        /// cell); otherwise a backend paints a glyph in the mark's foreground
        /// over the bar's own cell, so mark and scroll position stay readable
        /// in one cell.
        marks: Rc<[Mark]>,
    },
    /// A region whose text the backend may let the user select. The library
    /// holds no selection model; this only says where selecting is meaningful.
    Selectable,
    /// Content the host owns and draws itself.
    Host(HostId),
}

/// One mark on a [`Draw::Scrollbar`]'s track.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mark {
    /// The track cell, from the bar's start.
    pub at: u16,
    /// Where the mark's colour comes from: a backend paints the glyph in this
    /// theme's foreground (and, for a `full` mark, the cell in its
    /// background).
    pub theme: ThemeKey,
    /// Paint the whole cell rather than a glyph over the bar's cell.
    pub full: bool,
}

impl Draw {
    /// A bar with no marks, running down its rectangle.
    pub fn scrollbar(offset: u32, content: u32, window: u16) -> Draw {
        Draw::Scrollbar {
            offset,
            content,
            window,
            axis: crate::event::Axis::Vertical,
            marks: Rc::from(Vec::new()),
        }
    }

    /// Thumb geometry for a [`Draw::Scrollbar`], in track cells: `(top, len)`.
    ///
    /// Shared so every backend renders the bar identically and correctly at the
    /// extremes — the naive `offset * track / content` truncates and leaves the
    /// thumb a row short of the end at maximum scroll. This maps the offset
    /// across `[0, max]` onto the thumb's travel `[0, track - len]`, so a
    /// fully-scrolled thumb sits flush against the bottom.
    ///
    /// **The window is an argument because it is not the track.** `offset`,
    /// `content` and `window` are all in the viewport's own unit — cells for a
    /// cell-scrolling window, items for an item-scrolling one — and `track` is
    /// the bar's height on screen. They coincide for the first kind and part
    /// company for the second: a list of 16 five-row cards showing four of
    /// them in a 22-cell track is a window of *four*, and reading the track as
    /// the window made `len` come out `⌈22·22/16⌉` — clamped to the whole
    /// track, a solid stripe that says nothing and cannot be dragged.
    ///
    /// **The length rounds up.** Flooring under-states the window: 28 rows of
    /// 434 is 6.5% of the track, which floors to a single cell claiming 3.6%
    /// — and a one-cell thumb is the hardest thing on the bar to grab, so the
    /// row the user aims at lands on the track and page-jumps instead. The
    /// thumb should never claim *less* of the track than the window actually
    /// shows, so the division ceils and the result is clamped to the track.
    pub fn scrollbar_thumb(offset: u32, content: u32, window: u32, track: u16) -> (u16, u16) {
        let t = track as u32;
        if content == 0 || t == 0 {
            return (0, track);
        }
        let len = (t * window.max(1)).div_ceil(content).clamp(1, t);
        let max_off = content.saturating_sub(window);
        let top = (offset.min(max_off) * (t - len))
            .checked_div(max_off)
            .unwrap_or(0);
        (top as u16, len as u16)
    }
}

/// What a render object emits during paint.
///
/// The key, the element and the theme come from the framework, so an
/// implementation says only *what* to draw and *where*.
pub struct DrawList {
    pub(crate) items: Vec<Item>,
    pub(crate) key: Option<Key>,
    pub(crate) id: ElementId,
    pub(crate) theme: ThemeKey,
    pub(crate) cursor: Option<CursorSpec>,
}

impl DrawList {
    pub(crate) fn new(id: ElementId) -> Self {
        DrawList {
            items: Vec::new(),
            key: None,
            id,
            theme: ThemeKey::default(),
            cursor: None,
        }
    }

    /// Put the text cursor here. The last one emitted in a frame wins, which is
    /// what makes the innermost editable surface own it.
    pub fn set_cursor(&mut self, at: super::geom::Point) {
        self.cursor = Some(CursorSpec {
            pos: at,
            visible: true,
        });
    }

    /// Draw over this node's own rectangle.
    pub fn push(&mut self, draw: Draw, g: super::object::Geom) {
        self.push_at(draw, g.rect, g.clip);
    }

    /// Draw over some other rectangle inside this node — a scrollbar down one
    /// edge, for instance.
    pub fn push_at(&mut self, draw: Draw, rect: Rect, clip: Rect) {
        let theme = self.theme.clone();
        self.push_themed(draw, rect, clip, theme);
    }

    /// Draw with a theme more specific than the one the framework assigned.
    ///
    /// Provenance is the framework's by default — an item takes the nearest
    /// enclosing `theme(..)` tag. This is for the case where one node's output
    /// is not uniform: a text run whose pieces are styled differently emits an
    /// item per piece, each naming its own. The item still carries exactly one
    /// theme, so nothing downstream changes.
    pub fn push_themed(&mut self, draw: Draw, rect: Rect, clip: Rect, theme: ThemeKey) {
        self.items.push(Item {
            key: self.key.clone(),
            id: self.id,
            rect,
            clip,
            theme,
            draw,
        });
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CursorSpec {
    pub pos: Point,
    pub visible: bool,
}
