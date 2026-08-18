//! A complete application built on the library, and the harness that drives it.
//!
//! Between `model` and `view` it uses everything the library has: all seven
//! primitives, ambients, behaviors including `Tasks`, both list forms, layers
//! at all three modalities, pointer capture, focus scopes, traversal and
//! intents. It depends on nothing outside this crate, so it doubles as the
//! reference for how an application is put together — the same shape the
//! editor's chrome will take.
//!
//! ```text
//!   loop {
//!       let spec = ui.frame(view(&app), size);   // reconcile, lay out, paint
//!       backend.draw(spec);                      // fold over the display list
//!       for msg in ui.dispatch(input) {          // handlers run here
//!           update(&mut app, msg);               // ordinary code
//!       }
//!   }
//! ```

pub mod model;
pub mod view;

pub use model::{update, App, Filter, Msg, Task, Theme, COMMANDS};
pub use view::{shortcuts, view};

use crate::event::Input;
use crate::render::geom::Size;
use crate::test::screen::{render_with, Screen};
use crate::Ui;

/// A stand-in for whatever the application talks to. The tutorial's
/// `Services { net, store }`: constructed once, owned by the program, and
/// reachable from the update function.
#[derive(Default)]
pub struct Services {
    pub sync_count: usize,
}

/// The application, the library and the loop, wired together.
pub struct Demo {
    pub app: App,
    pub ui: Ui<Msg>,
    pub services: Services,
    pub size: Size,
}

impl Demo {
    pub fn new(size: Size) -> Self {
        Demo::with_app(App::default(), size)
    }

    pub fn with_app(app: App, size: Size) -> Self {
        let mut ui = Ui::new();
        ui.set_shortcuts(shortcuts());
        let mut demo = Demo {
            app,
            ui,
            services: Services::default(),
            size,
        };
        demo.render();
        demo
    }

    /// One frame: hand over a freshly built description and lay it out.
    pub fn render(&mut self) {
        self.ui.frame(view(&self.app), self.size);
    }

    /// One turn of the loop: route an input, apply what came back, redraw.
    pub fn input(&mut self, input: Input) -> Vec<Msg> {
        let mut msgs = self.ui.dispatch(input);
        msgs.extend(self.ui.take_messages());
        for m in msgs.clone() {
            update(&mut self.app, m);
        }
        self.render();
        msgs
    }

    /// Deliver a result from the outside world, the way a network client would.
    pub fn deliver_sync(&mut self, n: usize) -> bool {
        self.services.sync_count = n;
        let handle = self.app.sync_slot.borrow();
        let ok = handle.as_ref().map(|h| h.deliver(n)).unwrap_or(false);
        drop(handle);
        self.render();
        ok
    }

    pub fn resize(&mut self, size: Size) {
        self.size = size;
        self.render();
    }

    /// What the frame looks like. Fill regions are drawn from their theme
    /// provenance, which is what per-item `ThemeKey` is for.
    pub fn screen(&self) -> Screen {
        render_with(self.ui.spec(), |theme| match theme {
            "grip" => Some('│'),
            "menubar" => Some('▁'),
            // Selection and focus are appearances, not structure: the
            // description says which is which and the backend decides how they
            // read.
            "list.row.selected" => Some('░'),
            "button.focused" | "field.focused" | "toggle.focused" => Some('▪'),
            _ => None,
        })
    }
}
