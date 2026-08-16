//! Chrome surfaces as REGISTERED components.
//!
//! This is slice 0 of `docs/internal/chrome-event-model-plan.md`: the
//! central `chrome_boxes()` enumeration is dissolved into one
//! [`ChromeComponent`] per surface, each contributing its own boxes to
//! the per-event tree from the live state and paint caches it owns.
//! The registry below ([`components`]) is the ONE list — the chrome
//! analogue of `widgets::kinds::behavior()`. Later slices grow the
//! trait (per-gesture handlers, `hit_stack` dispatch, parent links,
//! keyboard) without moving this code again.
//!
//! The tree is rebuilt per event from live state — deliberately never
//! persisted (per-event freshness is what keeps stale-geometry races
//! impossible; see the plan's "what NOT to do").

mod base;
mod context_menu;
mod dock;
mod file_browser;
mod file_explorer;
mod floating_modal;
mod menu;
mod popups;
mod prompt;
mod search_options;
mod splits;
mod status_bar;

use super::Editor;
use crate::widgets::LayoutBox;

/// Per-event sink the components push their boxes into. Wraps the
/// frame dimensions so full-frame guards don't each re-read them.
pub(crate) struct ChromeTreeBuilder {
    frame_width: u32,
    frame_height: u32,
    boxes: Vec<LayoutBox>,
}

impl ChromeTreeBuilder {
    fn new(frame_width: u32, frame_height: u32) -> Self {
        ChromeTreeBuilder {
            frame_width,
            frame_height,
            boxes: Vec::new(),
        }
    }

    /// A full-frame surface — a guard/capture whose semantics ARE
    /// full-screen (close guards, absorb/dismiss, modal scrims,
    /// position-blind wheel capture), never a geometry proxy.
    pub(crate) fn full(&mut self, kind: &'static str, z: u8) {
        let mut b = LayoutBox::plain(kind, 0, 0, self.frame_width, self.frame_height);
        b.z = z;
        self.boxes.push(b);
    }

    /// A surface at its painted rectangle.
    pub(crate) fn rect(&mut self, kind: &'static str, z: u8, r: ratatui::layout::Rect) {
        let mut b = LayoutBox::plain(
            kind,
            r.y as u32,
            r.x as u32,
            r.width as u32,
            r.height as u32,
        );
        b.z = z;
        self.boxes.push(b);
    }
}

/// One registered chrome surface. Slice 0 registers GEOMETRY only —
/// `collect` contributes this component's boxes for THIS event, read
/// from the live state/caches the surface owns. The per-gesture
/// handlers join the trait in later slices, dissolving the central
/// dispatch matches the same way `WidgetImpl` dissolved the widget
/// ones.
pub(crate) trait ChromeComponent: Sync {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder);
}

/// The ONE chrome registry — every routable surface, once.
///
/// Ordering: components are grouped (each pushes its boxes
/// contiguously), which preserves the legacy `chrome_boxes()` PER-KIND
/// push order exactly; cross-kind order within a z band differs from
/// the legacy interleaving, which is behavior-neutral because every
/// walk either orders by a per-gesture kind array (wheel/click) or
/// sorts by z with at most one accepting surface per band
/// (hover/right-click/double-click). `assert_parity` checks the
/// per-kind invariant on every debug-build event.
pub(crate) fn components() -> &'static [&'static dyn ChromeComponent] {
    &[
        &context_menu::ContextMenu,
        &prompt::Prompt,
        &popups::Popups,
        &file_browser::FileBrowser,
        &floating_modal::FloatingModal,
        &dock::Dock,
        &splits::Splits,
        &menu::Menu,
        &file_explorer::FileExplorer,
        &status_bar::StatusBar,
        &search_options::SearchOptions,
        &base::Base,
    ]
}

/// Build the chrome surface tree for one event: every component
/// contributes its live boxes. Replaces the monolithic enumeration.
pub(crate) fn chrome_tree(ed: &Editor) -> Vec<LayoutBox> {
    let frame = ed.active_chrome().last_frame;
    let mut t = ChromeTreeBuilder::new(frame.width as u32, frame.height as u32);
    for c in components() {
        c.collect(ed, &mut t);
    }
    t.boxes
}

/// Slice-0 tripwire: the component registry must reproduce the legacy
/// enumeration's surface set — same kinds, and per kind the same boxes
/// in the same order (rows/cols/extents/z). Runs on every event in
/// debug builds (which is what the e2e suites run), then the legacy
/// body is deleted with the parity check.
#[cfg(debug_assertions)]
pub(crate) fn assert_parity(new: &[LayoutBox], legacy: &[LayoutBox]) {
    use std::collections::HashMap;
    fn by_kind(list: &[LayoutBox]) -> HashMap<&'static str, Vec<(u32, u32, u32, u32, u8)>> {
        let mut m: HashMap<&'static str, Vec<(u32, u32, u32, u32, u8)>> = HashMap::new();
        for b in list {
            m.entry(b.kind)
                .or_default()
                .push((b.row, b.col, b.width, b.height, b.z));
        }
        m
    }
    debug_assert_eq!(
        by_kind(new),
        by_kind(legacy),
        "chrome component registry must reproduce the legacy chrome_boxes() surfaces"
    );
}
