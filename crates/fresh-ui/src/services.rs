//! `cx.services`: what is valid from construction.
//!
//! The counterpart is `cx.geometry`, which is valid only after the first
//! layout. Splitting them puts the validity window in the type rather than in
//! documentation: a component holding a `Services` has no path to a rectangle,
//! so reading geometry during `build` — which would make build depend on
//! layout, which depends on build — is a compile error rather than an
//! assertion.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::element::ElementId;
use crate::render::geom::{Point, Rect, Size};
use crate::render::object::Band;

/// Work handed to whatever runs it. The library does not name a runtime.
pub type Job = Box<dyn FnOnce() + Send>;

/// The scheduler, the spawner, and the registries. Cloneable, cheap, and free
/// of geometry.
#[derive(Clone)]
pub struct Services {
    pub(crate) spawn: Rc<dyn Fn(Job)>,
    pub(crate) store: Option<Rc<dyn crate::behavior::Store>>,
}

impl Default for Services {
    fn default() -> Self {
        Services {
            spawn: Rc::new(|job| {
                std::thread::spawn(job);
            }),
            store: None,
        }
    }
}

impl Services {
    /// Run a job somewhere other than the UI thread. Replaced by the host when
    /// it has an executor of its own.
    pub fn spawn(&self, job: Job) {
        (self.spawn)(job)
    }
}

/// `cx.geometry`: what is valid only once layout has run.
///
/// Reachable from an event handler, a ticker and a task callback — never from
/// `build`, because `BuildCx` has no way to produce one.
#[derive(Clone, Copy)]
pub struct Geometry<'a, M: 'static> {
    pub(crate) ui: &'a crate::schedule::Ui<M>,
    pub(crate) id: ElementId,
}

impl<M: 'static> Geometry<'_, M> {
    /// This element's absolute rectangle.
    pub fn rect(&self) -> Rect {
        self.ui.rect_of(self.id)
    }

    pub fn size(&self) -> Size {
        self.ui.rect_of(self.id).size()
    }

    /// The clip this element inherited from its ancestors.
    pub fn clip(&self) -> Rect {
        self.ui.clip(self.id)
    }

    /// A viewport's offset, and the size of the content behind it.
    ///
    /// Both are in the unit that viewport's offset counts — see
    /// [`Geometry::window`], which is the only thing that says which unit that
    /// is.
    pub fn scroll(&self) -> (Point, Size) {
        self.ui.scroll(self.id)
    }

    /// A viewport's window, **in the unit its offset counts**. `None` for an
    /// element that is not scrolling anything.
    ///
    /// The unit is not the rectangle's. A cell-scrolling window's offset is a
    /// row and its window is its own height, so `window.h == rect().h`; an
    /// item-scrolling one's offset is an index and `window.h` is *how many
    /// items* fit, which is that height divided by the band — four, not
    /// twelve, for three-row cards in a twelve-row box. [`Geometry::band`]
    /// tells the two apart: `Some(_)` means items, `None` means cells. Reading
    /// the height for both put a list of three-row cards eleven items down
    /// inside a "fifteen-row" window, and drew a scrollbar thumb that filled
    /// its own track.
    pub fn window(&self) -> Option<Rect> {
        self.ui.window(self.id)
    }

    /// The band this viewport's items sit in, when its offset counts items.
    /// `None` where the offset counts cells — there an item is not a thing.
    ///
    /// This is the unit tag for [`Geometry::window`] and [`Geometry::scroll`],
    /// and for [`Band::Cells`] it is also the number that converts between
    /// them: one item is that many cells tall.
    pub fn band(&self) -> Option<Band> {
        self.ui.band(self.id)
    }

    /// Another element's rectangle, addressed by key.
    pub fn rect_of_key(&self, k: &crate::key::Key) -> Option<Rect> {
        self.ui.find_by_key(k).map(|e| self.ui.rect_of(e))
    }
}

// ---------------------------------------------------------------------------
// Geometry a callback can hold
// ---------------------------------------------------------------------------

/// One element's geometry, as of the last layout.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct GeomSnapshot {
    pub rect: Rect,
    pub clip: Rect,
    pub scroll: Point,
    pub content: Size,
    /// The window a scrolling element shows onto its content, `None` for one
    /// that shows none. **In the unit the offset counts**, which `band` is
    /// what states — see [`GeomHandle::window`].
    pub window: Option<Rect>,
    /// The band, published beside the window by the same walk and travelling
    /// with it for the same reason: `Some(_)` says `window` and `scroll` count
    /// items and how many cells one of them is, `None` says they count cells.
    pub band: Option<Band>,
}

/// The geometry of the elements somebody is watching. Refreshed once per frame,
/// so the cost is the number of live handles rather than the size of the tree.
#[derive(Default)]
pub(crate) struct GeomStore {
    pub(crate) entries: HashMap<ElementId, GeomSnapshot>,
}

/// A handle to one element's geometry, taken during construction and read
/// afterwards.
///
/// This is what makes geometry reachable from an event handler, a ticker and a
/// task callback: those run while the tree is borrowed mutably, so they hold a
/// handle rather than a reference into it. The validity window is unchanged —
/// reading during `build` would make build depend on layout, which depends on
/// build, and is rejected.
#[derive(Clone)]
pub struct GeomHandle {
    store: Rc<RefCell<GeomStore>>,
    sched: crate::schedule::SchedRef,
    id: ElementId,
}

impl GeomHandle {
    pub(crate) fn new(
        store: Rc<RefCell<GeomStore>>,
        sched: crate::schedule::SchedRef,
        id: ElementId,
    ) -> Self {
        store.borrow_mut().entries.entry(id).or_default();
        GeomHandle { store, sched, id }
    }

    /// The element this handle addresses.
    pub fn target(&self) -> ElementId {
        self.id
    }

    #[track_caller]
    fn read(&self) -> GeomSnapshot {
        debug_assert!(
            self.sched.borrow().building.is_none(),
            "geometry is not readable during build"
        );
        self.store
            .borrow()
            .entries
            .get(&self.id)
            .copied()
            .unwrap_or_default()
    }

    #[track_caller]
    pub fn rect(&self) -> Rect {
        self.read().rect
    }

    #[track_caller]
    pub fn size(&self) -> Size {
        self.read().rect.size()
    }

    /// The clip this element inherited from its ancestors.
    #[track_caller]
    pub fn clip(&self) -> Rect {
        self.read().clip
    }

    /// A viewport's offset, and the size of the content behind it.
    ///
    /// Both are in the unit that viewport's offset counts — see
    /// [`GeomHandle::window`], which is the only thing that says which unit
    /// that is.
    #[track_caller]
    pub fn scroll(&self) -> (Point, Size) {
        let g = self.read();
        (g.scroll, g.content)
    }

    /// A viewport's window, **in the unit its offset counts**. `None` for an
    /// element that is not scrolling anything.
    ///
    /// The unit is not the rectangle's. A cell-scrolling window's offset is a
    /// row and its window is its own height, so `window.h == rect().h`; an
    /// item-scrolling one's offset is an index and `window.h` is *how many
    /// items* fit, which is that height divided by the band — four, not
    /// twelve, for three-row cards in a twelve-row box. [`GeomHandle::band`]
    /// tells the two apart: `Some(_)` means items, `None` means cells. Reading
    /// the height for both put a list of three-row cards eleven items down
    /// inside a "fifteen-row" window, and drew a scrollbar thumb that filled
    /// its own track.
    #[track_caller]
    pub fn window(&self) -> Option<Rect> {
        self.read().window
    }

    /// The band this viewport's items sit in, when its offset counts items.
    /// `None` where the offset counts cells — there an item is not a thing.
    ///
    /// This is the unit tag for [`GeomHandle::window`] and
    /// [`GeomHandle::scroll`], and for [`Band::Cells`] it is also the number
    /// that converts between them: one item is that many cells tall.
    #[track_caller]
    pub fn band(&self) -> Option<Band> {
        self.read().band
    }
}
