//! `cx.services`: what is valid from construction.
//!
//! The counterpart is `cx.geometry`, which is valid only after the first
//! layout. Splitting them puts the validity window in the type rather than in
//! documentation: a component holding a `Services` has no path to a rectangle,
//! so reading geometry during `build` — which would make build depend on
//! layout, which depends on build — is a compile error rather than an
//! assertion.

use std::rc::Rc;

use crate::element::ElementId;
use crate::render::geom::{Point, Rect, Size};

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
                std::thread::spawn(move || job());
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

    /// A viewport's window, and the size of the content behind it.
    pub fn scroll(&self) -> (Point, Size) {
        self.ui.scroll(self.id)
    }

    /// Another element's rectangle, addressed by key.
    pub fn rect_of_key(&self, k: &crate::key::Key) -> Option<Rect> {
        self.ui.find_by_key(k).map(|e| self.ui.rect_of(e))
    }
}
