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
mod theme_info;

use super::types::HoverTarget;
use super::Editor;

/// Overlay-layer ranks, DESCENDING = higher keyboard/modal precedence.
/// Deliberately independent of both registry order and box z: the menu
/// LAYER outranks the prompt's and the popups' (an open menu owns the
/// keyboard over them) while its boxes sit in a lower pointer band,
/// and the native context menus rank BELOW the popup layer (the
/// unfocused-popup guard's `take_while` must not see them) while their
/// boxes sit at the very top. Event-debug (1000) is hardcoded in
/// `overlay_layers` — a debugging instrument, not a component.
pub(crate) mod layer_rank {
    pub(crate) const SETTINGS: u16 = 900;
    pub(crate) const KEYBINDING_EDITOR: u16 = 890;
    pub(crate) const CALIBRATION_WIZARD: u16 = 880;
    pub(crate) const WORKSPACE_TRUST: u16 = 870;
    pub(crate) const MENU: u16 = 860;
    pub(crate) const PROMPT: u16 = 850;
    pub(crate) const POPUP: u16 = 840;
    pub(crate) const CONTEXT_MENU: u16 = 830;
    pub(crate) const FLOATING_MODAL: u16 = 820;
    pub(crate) const DOCK: u16 = 810;
    pub(crate) const EDITOR_BASE: u16 = 0;
}
use crate::widgets::LayoutBox;
use anyhow::Result as AnyhowResult;

/// Shared cell-in-rect test for component geometry.
pub(crate) fn in_rect(col: u16, row: u16, rect: ratatui::layout::Rect) -> bool {
    col >= rect.x && col < rect.x + rect.width && row >= rect.y && row < rect.y + rect.height
}

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
    /// clear). NOT interchangeable with `Pass`: on a `pointer_opaque`
    /// box, `Pass` STOPS the walk (the opacity gate absorbs the
    /// event) while `PassAfter` keeps walking — an observer's
    /// continue must not be blocked by its own box's opacity. Today
    /// every PassAfter producer is a non-opaque guard, so the two
    /// only diverge if an opaque surface adopts observer semantics.
    PassAfter,
    /// Not this surface's event — the walk continues to the next box.
    Pass,
}

/// Which press gesture a pointer event carries. Triple-click ROUTES
/// through the tree like every other press (overlay swallow, popup
/// block/dismiss, then the split line-select arm); only the selection
/// semantics themselves are a buffer concern.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerPress {
    Left,
    Right,
    Double,
    Triple,
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

    /// Frame height in rows — for components whose box spans the
    /// full column height (the dock column and its resize border).
    pub(crate) fn frame_height(&self) -> u32 {
        self.frame_height
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

/// The active pointer GRAB, if any: press-established routing that
/// owns the pointer until release. Grabs are NOT bubble dispatch — a
/// drag must keep routing to its owner even when the pointer crosses
/// an alternate-screen terminal or any other surface (the btop-resize
/// bug). The FULL press-to-release roster lives here: the terminal
/// forward sink suppresses forwarding for every grab, and
/// `handle_mouse_drag` dispatches on the grab instead of a
/// hand-ordered flag ladder. Derived from live drag state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerGrab {
    /// Dock width resize from its right border.
    DockResize,
    /// Drag-to-select in a widget markdown/text document.
    WidgetText,
    /// A floating/dock panel's list scrollbar drag.
    WidgetScrollbar,
    /// A split's vertical scrollbar (thumb-relative or track-jump).
    VScrollbar,
    /// A split's horizontal scrollbar.
    HScrollbar,
    /// Text selection inside an info popup.
    PopupSelect,
    /// The prompt's suggestion-list scrollbar (overlay and dropdown).
    PromptScrollbar,
    /// A buffer popup's scrollbar.
    PopupScrollbar,
    /// Split-separator resize.
    SplitSeparator,
    /// File-explorer width resize from its border.
    ExplorerWidth,
    /// A press on a live terminal grid whose first motion converts to
    /// scrollback text selection (selection intent).
    TerminalSelectPending,
    /// Buffer text selection (the press placed the caret; drags
    /// extend the selection).
    TextSelection,
    /// A tab being dragged toward a drop zone.
    TabDrag,
}

/// The grab in effect for the current event, if any. The terminal
/// forward sink consults this instead of a hand-listed field check,
/// and the Drag arm dispatches on it. Checked in the old drag
/// ladder's order so precedence is unchanged when (rarely) two flags
/// coexist.
pub(crate) fn pointer_grab(ed: &Editor) -> Option<PointerGrab> {
    if ed.dock_resizing {
        return Some(PointerGrab::DockResize);
    }
    if ed.widget_text_drag.is_some() {
        return Some(PointerGrab::WidgetText);
    }
    if ed
        .dock
        .as_ref()
        .is_some_and(|p| p.scrollbar_drag_key.is_some())
        || ed
            .floating_widget_panel
            .as_ref()
            .is_some_and(|p| p.scrollbar_drag_key.is_some())
    {
        return Some(PointerGrab::WidgetScrollbar);
    }
    let ms = &ed.active_window().mouse_state;
    if ms.dragging_scrollbar.is_some() {
        return Some(PointerGrab::VScrollbar);
    }
    if ms.dragging_horizontal_scrollbar.is_some() {
        return Some(PointerGrab::HScrollbar);
    }
    if ms.selecting_in_popup.is_some() {
        return Some(PointerGrab::PopupSelect);
    }
    if ms.dragging_prompt_scrollbar {
        return Some(PointerGrab::PromptScrollbar);
    }
    if ms.dragging_popup_scrollbar.is_some() {
        return Some(PointerGrab::PopupScrollbar);
    }
    if ms.dragging_separator.is_some() {
        return Some(PointerGrab::SplitSeparator);
    }
    if ms.dragging_file_explorer || ms.drag_start_explorer_width.is_some() {
        return Some(PointerGrab::ExplorerWidth);
    }
    if ms.terminal_drag_pending.is_some() {
        return Some(PointerGrab::TerminalSelectPending);
    }
    if ms.dragging_text_selection {
        return Some(PointerGrab::TextSelection);
    }
    if ms.dragging_tab.is_some() {
        return Some(PointerGrab::TabDrag);
    }
    None
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
    /// falls through to the next surface down. Takes `&mut Editor` like
    /// the pointer handlers (not `&Editor` like `collect`): live-state
    /// geometry derivation can lazily load buffer chunks (the status
    /// bar's cursor-column segment), which needs the mutable borrow.
    fn hover(
        &self,
        _ed: &mut Editor,
        _bx: &LayoutBox,
        _col: u16,
        _row: u16,
    ) -> Option<HoverTarget> {
        None
    }

    /// React to a hover-target transition (enter / leave / move),
    /// offered to EVERY component after the hover walk names the new
    /// target — the reaction half of hover, living with the surface
    /// it drives (the menu's auto-switch/submenu machine, the context
    /// menu's highlight, the explorer's status tooltip). Components
    /// key off the target variants they own; reactions are
    /// independent — one surface reacting never suppresses another's
    /// leave-reaction (the old central ladder's early returns did).
    /// Return true to request a re-render beyond the target diff
    /// itself.
    fn on_hover_change(
        &self,
        _ed: &mut Editor,
        _old: Option<&HoverTarget>,
        _new: Option<&HoverTarget>,
        _col: u16,
        _row: u16,
    ) -> bool {
        false
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
    /// capturing. Offered in RANK order over the owner-stamped
    /// `overlay_stack()` (every capturing component declares a layer
    /// from the same activity predicate its capture gates on), so the
    /// `layer_rank` consts are the ONE precedence source for capture
    /// and keyboard alike — registry order no longer encodes it.
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

    /// This component's overlay-layer contributions, from live state:
    /// `(rank, Layer)` pairs pushed into `out` (see [`layer_rank`]).
    /// `Editor::overlay_layers()` concatenates every component's
    /// contributions and sorts by rank descending — the layer stack is
    /// DERIVED from the registry, so a surface's presence, keyboard
    /// ownership, `KeyContext`, and PTY blocking are declared by its
    /// component instead of a central conditional ladder.
    fn layers(&self, _ed: &Editor, _out: &mut Vec<(u16, crate::app::overlay::Layer)>) {}

    /// PRE-BAND keyboard grab. `Some` = the key is consumed with the
    /// handler's result; `None` = not grabbing, dispatch continues.
    /// Offered by `handle_key` BEFORE the `on_layer_key` walk, first
    /// grabbing component in registry order wins — which means grabs
    /// as a CLASS outrank every `layer_rank`, regardless of any layer
    /// the component declares. Membership is therefore restricted, by
    /// ruling, to the two shapes a rank cannot express:
    ///
    ///   - a whole-pipeline OBSERVER (ThemeInfo: dismiss-and-continue
    ///     side effects then `None` — the keyboard PassAfter), which
    ///     must see the key even when a higher surface consumes it;
    ///   - a custom-dispatcher modal transparent to `KeyContext`
    ///     resolution (ContextMenu: its layer exposes
    ///     `key_context: None` and its rank is deliberately NOT its
    ///     keyboard precedence — ruling at its site, #2587).
    ///
    /// Any surface whose precedence IS expressible as a rank belongs
    /// on `on_layer_key` instead: the dock and the floating modal
    /// started here and were moved when their grabs proved to invert
    /// the declared ranks (a focused dock eating Esc ahead of an open
    /// prompt while `get_key_context` said `Prompt`).
    fn on_key(
        &self,
        _ed: &mut Editor,
        _code: crossterm::event::KeyCode,
        _modifiers: crossterm::event::KeyModifiers,
    ) -> Option<AnyhowResult<()>> {
        None
    }

    /// Layer-targeted keyboard dispatch — THE key walk. After the
    /// pre-band (event-debug, terminal input, getNextKey capture,
    /// `on_key` grabs), `Editor::dispatch_layer_keyboard` walks the
    /// owner-stamped `overlay_stack()` top-down, offering the key to
    /// each layer's declaring component through this method — the
    /// keyboard analogue of `dispatch_pointer` walking `hit_stack`
    /// over owner-stamped boxes. `None` = this layer declines and the
    /// walk continues to the next layer down; `Some` = this layer
    /// dealt with the key, with the handler's result — including
    /// `Some(Ok(Ignored))`, which still stops the walk (the
    /// query-replace confirm prompt consumes every key that way).
    /// The error channel carries `handle_action` failures up through
    /// `handle_key`, matching the old staged pipeline's `?`s.
    /// Interiors stay bespoke, per the modal-mouse ruling: the
    /// component is the dispatch slot, only ROUTING is derived.
    fn on_layer_key(
        &self,
        _ed: &mut Editor,
        _layer: &crate::app::overlay::Layer,
        _event: &crossterm::event::KeyEvent,
    ) -> Option<AnyhowResult<crate::input::handler::InputResult>> {
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

    /// A HORIZONTAL wheel delta (Shift+wheel, or a native
    /// ScrollLeft/ScrollRight) over one of this component's boxes —
    /// same walk and chaining contract as [`Self::on_wheel`].
    /// Surfaces with no horizontal axis simply decline.
    fn on_hwheel(
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
        // The theme inspector: its trigger and popup ride the very
        // top of the routable bands (a debug instrument that must see
        // Ctrl+Right-Click under any surface), and its key dismissal
        // must run before the context menu's keyboard grab.
        &theme_info::ThemeInfo,
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
