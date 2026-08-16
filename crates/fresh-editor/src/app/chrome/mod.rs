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
mod modals;
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

impl std::borrow::Borrow<LayoutBox> for ChromeBox {
    fn borrow(&self) -> &LayoutBox {
        &self.lb
    }
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

    /// Whole-channel mouse capture for modal surfaces. A component
    /// whose modal is up claims the ENTIRE mouse event stream — every
    /// event kind (press, drag, release, wheel, move) — before any
    /// gesture walk runs, exactly the contract the deleted
    /// `dispatch_modal_mouse` ladder enforced: nothing may leak to a
    /// terminal, buffer, or surface beneath a modal. `None` = not
    /// capturing. The first capturing component in registry order
    /// wins, so the registry lists the modal band first, in the same
    /// relative order `overlay_layers()` ranks those layers (slice 6
    /// derives `overlay_layers` from the components, collapsing the
    /// duplicated activity predicates).
    ///
    /// Whole-event capture (rather than per-gesture boxes) is the
    /// honest intermediate: the modal handlers own presses, drags,
    /// releases, and hover as one unit; decomposing them onto the
    /// gesture walks needs the pointer-grab slot and the scan's
    /// opacity gate, recorded as the residue of this slice.
    fn capture_mouse(
        &self,
        _ed: &mut Editor,
        _ev: crossterm::event::MouseEvent,
        _is_double_click: bool,
    ) -> Option<AnyhowResult<bool>> {
        None
    }

    /// Keyboard grab for a component whose open surface owns the
    /// keyboard with a custom dispatcher (the native context menus:
    /// navigation keys drive the menu, everything else is swallowed
    /// so it can't leak into the buffer beneath). `Some` = the key is
    /// consumed with the handler's result; `None` = not grabbing,
    /// normal dispatch continues. Offered by `handle_key` ahead of
    /// `KeyContext` resolution, first grabbing component in registry
    /// order wins — the chrome keyboard analogue of `capture_mouse`.
    /// This is the plan's minimal keyboard slice: the broader
    /// focused-chrome-ring model stays gated on the prompt-as-widgets
    /// and Settings migrations.
    fn on_key(
        &self,
        _ed: &mut Editor,
        _code: crossterm::event::KeyCode,
        _modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        None
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
/// Ordering: every gesture scans the tree with
/// [`crate::widgets::layout_box::hit_stack`] — effective-z bands,
/// children above parents, and document order within a band. z bands
/// ride a x10 scale (180 context menu … 10 editor, 0 base) so
/// per-gesture guard/capture surfaces occupy their own slots
/// (transient dismiss at 175 above the z170 prompt/popup targets, the
/// overlay prompt's wheel modal at 160 above its position-blind
/// suggestion capture at 155, its click scrim down at 15 just above
/// the editor band). Within a band, registry order IS precedence:
/// components push specific targets before guards.
pub(crate) fn components() -> &'static [&'static dyn ChromeComponent] {
    &[
        // The modal band: whole-channel mouse capture, first-active
        // wins, ranked as `overlay_layers()` ranks their layers.
        // (FloatingModal captures too — it sits lower, after the
        // surfaces that render above it.)
        &modals::Settings,
        &modals::KeybindingEditor,
        &modals::CalibrationWizard,
        &modals::WorkspaceTrust,
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
