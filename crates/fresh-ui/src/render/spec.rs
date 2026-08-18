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
        /// Cells scrolled past.
        offset: u16,
        /// Total extent of the content.
        content: u16,
        /// Extent of the window onto it.
        window: u16,
    },
    /// Content the host owns and draws itself.
    Host(HostId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CursorSpec {
    pub pos: Point,
    pub visible: bool,
}
