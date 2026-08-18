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
    /// Key to the range of items its subtree produced. Used for hit-testing
    /// shortcuts, for patching a retained backend, and by tests.
    pub index: Vec<(Key, Range<usize>)>,
    pub cursor: Option<CursorSpec>,
}

impl LayoutSpec {
    pub fn clear(&mut self) {
        self.items.clear();
        self.index.clear();
        self.cursor = None;
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Draw {
    /// A background region.
    Fill,
    /// A box outline drawn inside `rect`.
    Border,
    /// Covers everything painted before it.
    Scrim(Scrim),
    /// Text, one entry per visual row.
    Lines(Vec<Rc<str>>),
    Scrollbar {
        /// How far the window has travelled, in whatever the viewport counts.
        offset: u32,
        /// Total extent of the content, in the same unit. Wider than a
        /// coordinate, because a million-row list has no cell extent that fits
        /// one.
        content: u32,
        /// Extent of the window, in cells.
        window: u16,
    },
    /// A region whose text the backend may let the user select. The library
    /// holds no selection model; this only says where selecting is meaningful.
    Selectable,
    /// Content the host owns and draws itself.
    Host(HostId),
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
        self.items.push(Item {
            key: self.key.clone(),
            id: self.id,
            rect,
            clip,
            theme: self.theme.clone(),
            draw,
        });
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CursorSpec {
    pub pos: Point,
    pub visible: bool,
}
