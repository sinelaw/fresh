//! Per-kind widget behaviour behind a single trait.
//!
//! This is phase 1 of `docs/internal/widget-framework-v2-review.md` §4.3:
//! `WidgetSpec` stays a closed, serializable wire type (it crosses the
//! plugin sandbox boundary and feeds the row and web renderers), while
//! the *behaviour* for each kind lives in one `WidgetImpl` per kind,
//! looked up through the single [`behavior`] dispatch below. The goal is
//! that exactly one `match` on the spec's kind survives in the codebase —
//! this one — and every other per-kind decision happens inside an impl
//! that only ever sees its own variant.
//!
//! The migration that built this module was incremental (kind by kind,
//! behaviour-preserving, guarded by the render unit tests); it is now
//! complete — [`behavior`] is total and `render::render_collected` is a
//! pure delegation to it.
//!
//! The trait currently has a single entry point, [`WidgetImpl::collect`],
//! mirroring today's one-pass renderer. The later phases of the plan grow
//! it (`measure`/`arrange` when the constraint layout lands, `on_event`
//! when input dispatch moves off the per-kind probes in
//! `app/widget_runtime.rs`) without moving the code again.

mod button;
mod component;
mod containers;
mod divider;
pub(crate) mod dropdown;
pub(crate) mod dual_list;
mod hint_bar;
pub(crate) mod list;
pub(crate) mod number;
mod popup;
mod raw;
mod spacer;
pub(crate) mod text;
mod toggle;
pub(crate) mod tree;
mod window_embed;

use std::collections::HashMap;

use fresh_core::api::WidgetSpec;

use super::registry::WidgetInstanceState;
use super::render::{CollectedOutput, RenderContext};

/// Static box-tree metadata for one widget node: what its
/// [`crate::widgets::LayoutBox`] should carry, derived from the spec
/// alone. `render_collected` combines this with the collected row count
/// to push the node's box after `collect` returns, so containers only
/// ever handle child-box *merging*.
#[derive(Debug, Clone, Default)]
pub(crate) struct BoxMeta {
    pub kind: &'static str,
    pub key: Option<String>,
    /// Mirrors `collect_tabbable`'s rules exactly (keyed, non-disabled,
    /// `focusable` where the variant has the flag) — the derived focus
    /// ring must reproduce the collected one order-for-order.
    pub focusable: bool,
    pub scrollable: bool,
    pub pointer_opaque: bool,
    pub focus_trap: bool,
    /// This widget is a panel's PRIMARY scroll surface for
    /// picker-style forwarding: a typed-filter panel routes Up/Down/
    /// Enter and the positionless wheel at the first widget declaring
    /// this (List, Tree, and markdown document views — NOT plain
    /// multi-row textareas, which scroll with their caret). Distinct
    /// from `scrollable` (wheel-over-the-widget), which plain
    /// textareas do have.
    pub picker_scroll_target: bool,
}

impl BoxMeta {
    pub(crate) fn plain(kind: &'static str) -> Self {
        BoxMeta {
            kind,
            ..Default::default()
        }
    }
}

/// Deferred plugin notifications collected while a widget handles a
/// key: `(event_type, payload)` pairs fired against the handling
/// widget's key, after its state mutations land and the panel
/// repaints.
#[derive(Debug, Default)]
pub(crate) struct KeyFx {
    pub events: Vec<(String, serde_json::Value)>,
    /// Move the panel's focus by this many ring positions after the
    /// handler's state lands (Enter on a form-like widget committing
    /// the field and advancing). Focus order is panel policy — the
    /// kind only *requests* the move; the dispatcher walks the ring.
    pub focus_advance: Option<i32>,
    /// Ask the host to flash the panel's overlay scrollbar (keyboard
    /// nav in the dock moves the selection without the pointer, so
    /// the hover-revealed bar needs an explicit blink to show where
    /// the selection sits). Which panels *have* such a bar is host
    /// policy — a no-op elsewhere.
    pub flash_scrollbar: bool,
    /// Text to place on the editor's clipboard (a copy/cut chord on
    /// a text widget's selection). The clipboard is host state, so
    /// the kind hands the text out rather than reaching for it.
    pub clipboard_copy: Option<String>,
}

/// Effects a pointer handler requests beyond mutating panel state:
/// the shared [`KeyFx`] channel (deferred events, focus advance,
/// scrollbar flash, clipboard) plus the one pointer-only host action.
#[derive(Debug, Default)]
pub(crate) struct PointerFx {
    pub key: KeyFx,
    /// Text: place the caret at the clicked byte. The mapping from
    /// click cell to value byte (and the markdown-document row
    /// variant) is click-path knowledge the panel doesn't have, so
    /// the kind requests it and the dispatcher runs the host helper.
    pub place_caret: bool,
}

/// How panel-level Up/Down treats a kind that is the panel's
/// scrollable picker target — see [`WidgetImpl::picker_nav`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PickerNav {
    /// Arrows don't reach this kind.
    Skip,
    /// Move this widget's selection; the typing widget keeps focus.
    Peek,
    /// Move panel focus INTO this widget.
    TakeFocus,
}

/// What a widget kind did with a resolved pointer hit.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PointerDisposition {
    /// Fire the hit's default event (its recorded payload, tagged
    /// `via: "click"`) against the owning widget's key. This is the
    /// no-override behaviour — a plain Button click fires `activate`
    /// this way.
    Default,
    /// The handler did everything (state mutation + any `fx` events);
    /// the recorded default event must NOT fire.
    Consumed,
}

/// What the focused widget did with a key.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum KeyDisposition {
    /// Not this widget's key — the panel-level dispatch proceeds.
    Pass,
    /// Fully handled; the dispatcher repaints and stops.
    Consumed,
    /// The widget updated itself (e.g. closed its popup) but the key
    /// must still act on the surface beneath it — Enter submitting
    /// the form, Tab advancing focus. The dispatcher repaints and
    /// proceeds.
    PassAfter,
}

/// Behaviour for one widget kind. Implementations are unit structs;
/// each `collect` destructures its own `WidgetSpec` variant (a
/// mismatched variant is a dispatch bug and renders nothing rather
/// than panicking).
pub(crate) trait WidgetImpl: Sync {
    /// Render this node (and, for containers, its subtree) into rows,
    /// hit areas, and next-tick instance state. Semantics are identical
    /// to the corresponding arm of the legacy `render_collected` match.
    fn collect(
        &self,
        spec: &WidgetSpec,
        prev: &HashMap<String, WidgetInstanceState>,
        next_state: &mut HashMap<String, WidgetInstanceState>,
        ctx: RenderContext<'_>,
        panel_width: u32,
    ) -> CollectedOutput;

    /// This node's layout-box metadata: the tag, key, and dispatch
    /// flags its [`crate::widgets::LayoutBox`] carries. Each impl
    /// answers for its own variant — there is deliberately no central
    /// kind→tag table.
    fn box_meta(&self, spec: &WidgetSpec) -> BoxMeta;

    /// A key event dispatched to the focused widget before the
    /// panel-level fallbacks (Tab cycling, Enter submit, arrows).
    /// This is what dissolved `handle_widget_key`'s popup
    /// short-circuits: a kind whose own open popup needs the key
    /// claims it here (`Dropdown` claims Up/Down while `open`,
    /// `Text` claims them while its completion list is showing) and
    /// returns [`KeyDisposition::Pass`] otherwise, letting the key
    /// bubble. No central function knows these popups exist.
    ///
    /// Mutate `panel` for state changes; queue plugin notifications
    /// on `fx` — the dispatcher rerenders and fires them after the
    /// handler returns.
    fn on_key(
        &self,
        _spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        _key: &str,
        _fx: &mut KeyFx,
    ) -> KeyDisposition {
        KeyDisposition::Pass
    }

    /// A pointer hit resolved to this widget (the hit's OWNER — for
    /// a List row that is the List, not the row). This is what
    /// dissolved `deliver_widget_hit`'s string-kind ladder: the state
    /// a click mutates (tree expansion, list/tree selection, dropdown
    /// open flag, dual-list cursors) is mutated here by the kind that
    /// owns it, and the dispatcher only applies `fx` and — on
    /// [`PointerDisposition::Default`] — fires the hit's recorded
    /// event. Focus already moved to the owner before this runs (when
    /// tabbable), matching click-to-focus everywhere.
    fn on_pointer(
        &self,
        _spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        _event_type: &str,
        _payload: &serde_json::Value,
        _fx: &mut PointerFx,
    ) -> PointerDisposition {
        PointerDisposition::Default
    }

    /// Panel focus moved ONTO (`gained`) or OFF (`!gained`) this
    /// kind's widget — the kind's chance to keep its instance state
    /// coherent with focus. Tree clears a blurred tree's selection
    /// and seeds a focused one's first visible row, so focus and the
    /// selected-row highlight are always ONE element. Default: no-op.
    fn on_focus_change(
        &self,
        _panel: &mut crate::widgets::WidgetPanelState,
        _key: &str,
        _gained: bool,
    ) {
    }

    /// The semantic event the plugin-facing `WidgetAction::Activate`
    /// (a mode binding's Enter) fires against this kind when focused.
    /// `None` = activation is a no-op for this kind — deliberately
    /// NOT wired for List (plugins drive list activation through
    /// `select`/`activate_event` on the smart-key path instead), so
    /// only Button and Toggle answer.
    fn activate_event(&self, _spec: &WidgetSpec) -> Option<(&'static str, serde_json::Value)> {
        None
    }

    /// CAPABILITY: panel-level Up/Down lands on a focused widget of
    /// this kind that has no vertical axis of its own — advance focus
    /// instead (arrows walk the controls like Tab, the dock's
    /// button-only context menus). Declared by Button/Toggle; the
    /// panel key router asks this instead of matching kinds.
    fn arrows_advance_focus(&self) -> bool {
        false
    }

    /// CAPABILITY: how panel-level Up/Down treats this kind when it is
    /// the panel's scrollable picker target. `Peek` moves this
    /// widget's selection while the typing widget keeps focus (List:
    /// filter-and-arrow); `TakeFocus` moves panel focus INTO this
    /// widget so it becomes the single focused element (Tree: a real
    /// tabbable target — peeking would leave two focus rings and
    /// Enter acting on the wrong element); `Skip` = arrows don't
    /// reach it. Declared by the kind; the router asks this instead
    /// of matching kinds.
    fn picker_nav(&self) -> PickerNav {
        PickerNav::Skip
    }

    /// CAPABILITY: Enter in a single-line filter input fires this
    /// scrollable picker target's activation (type-then-Enter without
    /// tabbing to the list). The event itself comes from
    /// [`Self::picker_activate_event`]. Declared by List/Tree.
    fn activates_on_picker_enter(&self) -> bool {
        false
    }

    /// The activation event the picker-Enter path fires for this
    /// kind's CURRENT selection (distinct from [`Self::activate_event`],
    /// which answers the focused-widget Activate action and
    /// deliberately excludes List/Tree). Consulted only when
    /// [`Self::activates_on_picker_enter`] is true.
    fn picker_activate_event(
        &self,
        _spec: &WidgetSpec,
        _key: &str,
        _panel: &crate::widgets::WidgetPanelState,
    ) -> Option<(String, serde_json::Value)> {
        None
    }

    /// A wheel delta bubbling through this widget's box. Return true
    /// when the widget consumed it (actually moved its viewport) —
    /// the dispatcher then rerenders the panel and stops bubbling. A
    /// widget already at its bound returns false so the event keeps
    /// bubbling (scroll chaining), ultimately falling through to the
    /// enclosing buffer scroll. The default is "not scrollable".
    fn on_wheel(
        &self,
        _spec: &WidgetSpec,
        _widget_key: &str,
        _panel: &mut crate::widgets::WidgetPanelState,
        _delta: i32,
    ) -> bool {
        false
    }
}

/// The one kind-dispatch — the single surviving `match` on a
/// `WidgetSpec`'s kind. Total: every kind has an impl.
pub(crate) fn behavior(spec: &WidgetSpec) -> &'static dyn WidgetImpl {
    match spec {
        WidgetSpec::HintBar { .. } => &hint_bar::HintBar,
        WidgetSpec::Spacer { .. } => &spacer::Spacer,
        WidgetSpec::Divider { .. } => &divider::Divider,
        WidgetSpec::Raw { .. } => &raw::Raw,
        WidgetSpec::Toggle { .. } => &toggle::Toggle,
        WidgetSpec::Button { .. } => &button::Button,
        WidgetSpec::WindowEmbed { .. } => &window_embed::WindowEmbed,
        WidgetSpec::Number { .. } => &number::Number,
        WidgetSpec::Dropdown { .. } => &dropdown::Dropdown,
        WidgetSpec::DualList { .. } => &dual_list::DualList,
        WidgetSpec::List { .. } => &list::List,
        WidgetSpec::Tree { .. } => &tree::Tree,
        WidgetSpec::Text { .. } => &text::Text,
        WidgetSpec::Row { .. } => &containers::Row,
        WidgetSpec::Col { .. } => &containers::Col,
        WidgetSpec::LabeledSection { .. } => &containers::LabeledSection,
        WidgetSpec::Overlay { .. } => &containers::Overlay,
        WidgetSpec::Component { .. } => &component::Component,
        WidgetSpec::Popup { .. } => &popup::Popup,
    }
}

pub use tree::collect_visible_tree_indices;
