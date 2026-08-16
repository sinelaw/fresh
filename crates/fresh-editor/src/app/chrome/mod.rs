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

use super::types::HoverTarget;
use super::Editor;
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

/// One box in the per-event chrome tree: the geometry (the SAME
/// `LayoutBox` type the panel model uses, so hit math and flags are
/// shared) plus which registered component owns it — dispatch calls
/// the owner's handlers instead of matching kind strings.
pub(crate) struct ChromeBox {
    pub lb: LayoutBox,
    /// Index into [`components`].
    pub owner: usize,
}

/// What a component did with a pointer gesture on one of its boxes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Disposition {
    /// Handled — the walk stops.
    Consumed,
    /// Side effects applied, but the event must keep routing — the
    /// act-then-continue guards (transient dismiss, explorer menu
    /// clear). Distinct from `Pass` so the contract is visible even
    /// though today's flat walk treats both as "continue".
    PassAfter,
    /// Not this surface's event — the walk continues to the next box.
    Pass,
}

/// Which press gesture a pointer event carries. Triple-click stays a
/// buffer-selection concern outside chrome.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerPress {
    Left,
    Right,
    Double,
}

/// One pointer gesture offered to a component's box, press kind
/// included — per-gesture behavior lives in the handler (the way
/// `WidgetImpl::on_pointer` takes `event_type`), not in one trait
/// method per press. `modifiers` matter only to the editor surface
/// (Ctrl+click follows links); guards and chrome controls ignore
/// them.
pub(crate) struct ChromePointer {
    pub press: PointerPress,
    pub col: u16,
    pub row: u16,
    pub modifiers: crossterm::event::KeyModifiers,
}

/// Per-event sink the components push their boxes into. Wraps the
/// frame dimensions so full-frame guards don't each re-read them, and
/// stamps each box with the collecting component's registry index.
pub(crate) struct ChromeTreeBuilder {
    frame_width: u32,
    frame_height: u32,
    current_owner: usize,
    boxes: Vec<ChromeBox>,
}

impl ChromeTreeBuilder {
    fn new(frame_width: u32, frame_height: u32) -> Self {
        ChromeTreeBuilder {
            frame_width,
            frame_height,
            current_owner: 0,
            boxes: Vec::new(),
        }
    }

    pub(crate) fn push(&mut self, lb: LayoutBox) {
        self.boxes.push(ChromeBox {
            lb,
            owner: self.current_owner,
        });
    }

    /// A full-frame surface — a guard/capture whose semantics ARE
    /// full-screen (close guards, absorb/dismiss, modal scrims,
    /// position-blind wheel capture), never a geometry proxy.
    pub(crate) fn full(&mut self, kind: &'static str, z: u8) {
        let mut b = LayoutBox::plain(kind, 0, 0, self.frame_width, self.frame_height);
        b.z = z;
        self.push(b);
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
        self.push(b);
    }
}

/// One registered chrome surface: `collect` contributes its boxes for
/// THIS event from the live state/caches the surface owns, and the
/// per-gesture handlers own its behavior (slice 1: hover, right-click,
/// double-click — left-click and wheel still ride the central kind
/// arrays until slice 2). Handlers receive the box they were offered
/// and decline (`None`/`Pass`) where its geometry is coarser than
/// their real target, exactly as the central match arms did.
pub(crate) trait ChromeComponent: Sync {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder);

    /// Name the hover target under the pointer, or decline so the walk
    /// falls through to the next surface down.
    fn hover(&self, _ed: &Editor, _bx: &LayoutBox, _col: u16, _row: u16) -> Option<HoverTarget> {
        None
    }

    /// A pointer press (left / right / double, per `ev.press`) on one
    /// of this component's boxes.
    fn on_pointer(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _ev: &ChromePointer,
    ) -> AnyhowResult<Disposition> {
        Ok(Disposition::Pass)
    }

    /// A wheel delta over one of this component's boxes. `Consumed`
    /// stops the walk; a scroll surface already at its bound (or a
    /// box whose real target the pointer missed) returns `Pass` so
    /// the wheel keeps falling — scroll chaining.
    fn on_wheel(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
        _delta: i32,
    ) -> AnyhowResult<Disposition> {
        Ok(Disposition::Pass)
    }
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
/// contributes its live boxes, each stamped with its owner. Replaces
/// the monolithic enumeration.
pub(crate) fn chrome_tree(ed: &Editor) -> Vec<ChromeBox> {
    let frame = ed.active_chrome().last_frame;
    let mut t = ChromeTreeBuilder::new(frame.width as u32, frame.height as u32);
    for (i, c) in components().iter().enumerate() {
        t.current_owner = i;
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
