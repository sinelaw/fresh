//! The floating plugin panel's *frame*: where it goes, its ring, its title and
//! its `[×]`.
//!
//! The panel's interior is the widget runtime's — nineteen `WidgetSpec`
//! variants painted by `render_floating_widget_panel` — and stays that way
//! until C.1. What was never the runtime's is the box around it, and the
//! painter owned that too: it derived the rectangle from a width percentage
//! and a content row count, drew a `Block`, wrote `[×]` into the top border,
//! and then **filed that button's rectangle in `close_button_rect` so a mouse
//! handler could compare against it**. That last step is the migration's
//! signature defect — geometry computed by a painter, recorded, and hit-tested
//! later — and it is what this module removes.
//!
//! **What stays with the painter, deliberately.** The dimming pass. A scrim is
//! the tree's answer and this layer could carry one, but the dock's own panel
//! is painted *after* the tree's overlay band, so a scrim declared here would
//! be overpainted by the dock and the frame would read half-dimmed. The dock's
//! content is C.5b; the scrim goes when it does, and until then the painter's
//! two `apply_dimming` calls stay where they can still see the dock. Recorded
//! here rather than left to be rediscovered.

use std::rc::Rc;

use fresh_ui::{
    col, gesture, layer, row, text, Align, Anchor, Event, Fit, GestureKind, Key, MouseButton, Node,
    Place, PointerMode, Sizing,
};

use crate::app::shell_host::shell_theme::{attrs, pair};

use super::msg::{UiFact, UiMsg};

/// Where the panel sits, with the content measurements the placement needs.
///
/// The percentage and the counts are **content**, not geometry: a plugin
/// mounts at `{widthPct, heightPct}` because it does not know how tall its
/// content will be, and only the editor can count the rows the spec produced.
/// What the tree does with them is the arithmetic that used to be the
/// painter's.
///
/// `heightPct` is absent on purpose. The painter passed it to
/// `centered_overlay_rect` and then threw the resulting height away — the box
/// is as tall as its content, clamped to the frame, and has been since the
/// fit-to-content fix. Carrying it here would carry a value nothing reads.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Spot {
    /// Centred in its bounds: as wide as the request, as tall as the content.
    Centered {
        width_pct: u8,
        /// Rows the spec produced, borders excluded.
        content_rows: u16,
    },
    /// An unobtrusive context-menu popup at an absolute screen cell. It hugs
    /// its items rather than taking a percentage, and it is clamped so the
    /// whole box stays on screen.
    Anchored {
        x: u16,
        y: u16,
        /// The widest entry, borders excluded.
        content_cols: u16,
        content_rows: u16,
    },
}

/// The panel's frame, with everything resolved from live state.
#[derive(Clone, Debug)]
pub struct Panel {
    pub spot: Spot,
    /// Rendered into the top border when centred. An anchored popup wears no
    /// title, which is the painter's rule kept.
    pub title: Option<String>,
    /// Whether the `[×]` is offered — centred panels only. An anchored popup
    /// is dismissed by clicking away from it.
    pub closable: bool,
    /// A focused panel lights its ring with the accent, so exactly one chrome
    /// region wears it at a time.
    pub focused: bool,
    /// Whether the panel lays into the whole frame or into the chrome column
    /// beside the dock. The orchestrator's global modals opt into the former
    /// so they are not cramped into the region right of their own dock.
    pub fullscreen: bool,
    /// **The interior, when it is described rather than painted.**
    ///
    /// `Some` when the panel's spec uses only variants
    /// `view::shell::widgets` describes; `None` sends the whole panel down
    /// the runtime's path, which is what still paints a `WindowEmbed`. A
    /// panel is one or the other and never half of each — see
    /// `widgets::covered`.
    pub interior: Option<Interior>,
}

/// **A plugin panel's keymap, on the tree.** The mode a panel's plugin
/// defined (`defineMode`) and the resolver its bindings live in; the
/// panel's interior captures a key the mode explicitly binds ahead of the
/// widget that holds focus, and the key arrives as the bound action
/// (`UiMsg::Action`).
///
/// This is where the router's "an explicit mode binding for the key wins
/// over the panel's smart-key defaults" used to be decided, one key at a
/// time, after the tree had already declined the key: now it is a property
/// of the panel's node, resolved on the capture leg, and a panel in a slot
/// whose keys are not a mode's — a sidebar section — simply declares none.
#[derive(Clone)]
pub struct Keymap {
    pub mode: String,
    pub resolver: std::sync::Arc<std::sync::RwLock<crate::input::keybindings::KeybindingResolver>>,
    /// The panel's focused widget is a field you type into. **A focused
    /// text field takes a printable key ahead of the mode's bindings** —
    /// the rule the router applied when the mode was consulted host-side,
    /// kept here now that the keymap is consulted on the tree: a mode that
    /// binds Space, `/` or a digit binds them for the controls, and a
    /// field with the keyboard still types them.
    pub text_focused: bool,
}

impl std::fmt::Debug for Keymap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Keymap")
            .field("mode", &self.mode)
            .finish_non_exhaustive()
    }
}

impl Keymap {
    /// The action `mode` explicitly binds `k` to, if any — and none for a
    /// printable key while a text field has the keyboard (see
    /// [`Keymap::text_focused`]).
    fn action(&self, k: fresh_ui::KeyPress) -> Option<crate::input::keybindings::Action> {
        let printable = matches!(k.code, fresh_ui::KeyCode::Char(_))
            && (k.mods == fresh_ui::Mods::NONE || k.mods == fresh_ui::Mods::SHIFT);
        if self.text_focused && printable {
            return None;
        }
        let ev = super::input::crossterm_key_event(k)?;
        let ctx = crate::input::keybindings::KeyContext::Mode(self.mode.clone());
        self.resolver.read().ok()?.explicit_binding(&ev, &ctx)
    }
}

/// A described interior: the spec, and the host state it reads.
#[derive(Clone, Debug)]
pub struct Interior {
    pub spec: std::rc::Rc<fresh_core::api::WidgetSpec>,
    pub states: std::rc::Rc<std::collections::HashMap<String, crate::widgets::WidgetInstanceState>>,
    pub focus_key: String,
    /// See [`super::widgets::Ctx::keyboard`].
    pub keyboard: bool,
    /// The panel is a *page* (`WidgetPanelOptions::page`): its content is
    /// described inside one host-owned viewport that scrolls as a whole,
    /// and this is the host's handle on that window — `scrollToWidget` and
    /// the page's arrow keys are commands on it, applied by the layout that
    /// measured the page. `None` for every panel whose lists window
    /// themselves.
    pub page: Option<std::rc::Rc<fresh_ui::behavior::Anchor>>,
    pub hovered_key: Option<String>,
    pub hovered_item_key: String,
    /// The open dropdown pop-over's hovered option, as a decimal index, or
    /// empty. See [`super::widgets::Ctx::hovered_popup_row`].
    pub hovered_popup_row: String,
    pub marker_gutter: bool,
    /// The row budget auto-sized lists and trees are windowed to.
    ///
    /// `None` for a pane-mounted panel, and that is not "unknown": the dock
    /// and the floating panel know their inner height as *state* — it is the
    /// box they were placed in — while a pane's is whatever rectangle layout
    /// hands the subtree, so `Slot::Pane` reads it off its own constraints
    /// instead. Recording it here would be a paint-time rectangle travelling
    /// back into the description that produced it.
    pub avail_height: Option<u32>,
    /// See [`super::widgets::Ctx::scrollbar_reveal`].
    pub scrollbar_reveal: Option<bool>,
    /// The plugin mode whose bindings this panel's keys resolve against
    /// first, if any. See [`Keymap`] and [`interior`].
    pub keymap: Option<Keymap>,
    /// The host resources a `markdown: true` `Text` widget renders through.
    ///
    /// See [`super::widgets::Ctx::markdown`]. Owned handles rather than the
    /// borrow the collector takes, because the description outlives the
    /// `Editor` borrow that produced it; [`MarkdownInk::ctx`] takes the
    /// borrow back where the node is built.
    pub markdown: Option<MarkdownInk>,
}

/// The live theme and grammar set, held by handle so a description can carry
/// them.
///
/// **A description is built away from the `Editor`, and markdown needs both.**
/// `crate::widgets::MarkdownCtx` borrows them, which is right for the
/// collector — it runs inside one call — and impossible for an `Interior`,
/// which is built in `Editor::panel_interior` and read in a layout closure
/// several frames' worth of borrows later. Cloning the handles is what makes
/// the crossing possible; neither is deep (`Theme` is already cloned once per
/// frame for the palette, and the registry is shared).
#[derive(Clone, Debug)]
pub struct MarkdownInk {
    pub theme: std::sync::Arc<crate::view::theme::Theme>,
    pub grammars: std::sync::Arc<crate::primitives::grammar::GrammarRegistry>,
}

impl MarkdownInk {
    /// The borrow the collector takes.
    pub fn ctx(&self) -> crate::widgets::MarkdownCtx<'_> {
        crate::widgets::MarkdownCtx {
            theme: &self.theme,
            grammars: Some(&self.grammars),
        }
    }
}

/// The box itself. Its rectangle is what the painter used to call
/// `overlay_rect`.
pub fn key() -> Key {
    Key::Str("panel_frame".into())
}

/// The content area, for the interior painter to read its rectangle from.
/// What `last_inner_rect` recorded, derived instead.
pub fn body_key() -> Key {
    Key::Str("panel_body".into())
}

pub fn close_key() -> Key {
    Key::Str("panel_close".into())
}

impl Panel {
    /// The box's height, borders included. The request is a hint in this
    /// direction and always has been: shorter content shrinks the box, taller
    /// content grows it up to the frame.
    ///
    /// **A described interior states its own height; a painted one cannot.**
    /// The counts in `Spot` are the row and column tallies of the widget
    /// runtime's text mirror, and for a described panel they are a *second*
    /// measurement of a subtree layout is about to measure anyway — the last
    /// thing on this branch that made the mirror a rendering input rather than
    /// a text mirror. `Auto` is "whatever the content needs, within the
    /// incoming constraint", which is exactly the fit-to-content rule the
    /// arithmetic was spelling out.
    ///
    /// A painted interior is a `Host` leaf. A host has no intrinsic size by
    /// definition — the tree hands it a rectangle and knows nothing about what
    /// goes in it — so `Auto` there would collapse the box to its border. The
    /// counts stay for exactly that case, and go with it.
    fn height(&self) -> Sizing {
        if self.interior.is_some() {
            return Sizing::Auto;
        }
        let content = match self.spot {
            Spot::Centered { content_rows, .. } | Spot::Anchored { content_rows, .. } => {
                content_rows
            }
        };
        Sizing::Cells(content.saturating_add(2).max(3))
    }

    /// **An anchored popup's width stays the mirror's count, even described,
    /// and this is not an omission.**
    ///
    /// The box hugs its content horizontally, so `Auto` is what it wants to
    /// say — but the interior is built by a `layout_reader`, which needs a
    /// *number* for the width before it can produce a row, and under `Auto`
    /// the number it would be handed is the whole screen. A divider would come
    /// out a hundred columns wide and set the very width it was asked about.
    /// The height has no such loop: nothing in the interior is built from a
    /// row budget it would then determine.
    ///
    /// So this one measurement outlives the rest, and what removes it is the
    /// interior stating its own natural width — the same step that lets the
    /// `layout_reader` go.
    fn anchored_width(&self, content_cols: u16) -> Sizing {
        Sizing::Cells(content_cols.saturating_add(2).max(6))
    }
}

/// The panel's frame as a layer.
///
/// **It does not claim the pointer**, and that is not an oversight: the
/// panel's whole channel is already claimed by `modal::layer`, which routes it
/// to the interior's own hit-testing. This layer is declared after that one,
/// so it is offered the pointer first, and every node of it but the `[×]` is
/// transparent — so the button takes its own press and everything else falls
/// through to the routing that was already there. One button migrates without
/// the interior having to.
///
/// `within` is how "beside the dock" is said. The painter expressed it by
/// being handed `chrome_area` instead of the frame; a layer that names the
/// region it may be placed inside says the same thing, in the place that does
/// the placing.
/// **A focused panel's keyboard — confinement without a swallow.**
///
/// `chrome::Dock::on_layer_key` and `chrome::FloatingModal::on_layer_key`
/// were offered every key by the ranked overlay walk while their layer said
/// `owns_keyboard`, and both end in `dispatch_floating_widget_key`, which
/// **declines**: a shortcut the panel does not bind blurs the dock and falls
/// through to the editor's own resolution. That is the same shape the prompt
/// has, and the same [`fresh_ui::Modality::Focus`] says it — see
/// `super::prompt::keys_layer` for the two halves and why the claim is
/// completed host-side.
///
/// What the ranks said, the frame's declaration order says: these are
/// declared under the overlay prompt's card and the popups
/// (`POPUP > FLOATING_MODAL > DOCK` — the R1 rank-inversion fix, kept), the
/// dock's under the floating panel's, and both under the menus and the modal
/// band. Nothing consults an integer.
///
/// Paints nothing and takes no pointer: the panel's frame, its `[×]` and its
/// described interior are all elsewhere, and every one of them answers its
/// own press.
///
/// **Where the focusables are, when the interior is described.** The layer has
/// to be declared here — its rank among the other keyboard layers is this
/// position in the frame — while the panel's controls are declared in the dock
/// column or the floating box, elsewhere entirely. Confinement is containment,
/// so without `scope` traversal was confined to *this layer*, whose only child
/// was the sink: every widget in the panel was focusable and none was
/// reachable, and `apply_autofocus` pulled a click-focused one back out on the
/// next frame.
///
/// `scope` names the interior instead. The sink then has no reason to exist —
/// it is outside the scope, so it can neither be focused nor see a key — and
/// the fallback that claims what the widgets decline moves inside, where the
/// focus chain runs through it. See [`interior`].
pub fn keys_layer(slot: super::widgets::Slot, scope: Option<Key>) -> Node<UiMsg> {
    use fresh_ui::Modality;
    let l = layer()
        .anchor(Anchor::Screen(Align::Start))
        .place(Place::Fill)
        .pointer_mode(PointerMode::Ignore)
        .modality(Modality::Focus);
    // A pane's layer is the base's, not an overlay's: the gates that ask
    // "is something layered over the content" (`Editor::focus_in_a_layer`)
    // tell it apart by this key. See [`is_base_layer`].
    let l = match slot {
        super::widgets::Slot::Pane(leaf) => l.key(pane_keys_key(leaf)),
        _ => l,
    };
    match scope {
        Some(k) => l.scope_at(k),
        // No described interior — an empty dock, or a panel the adapter could
        // not describe. There is nothing in the tree to confine to, so the
        // sink stays: it holds focus so the layer keeps answering the
        // containment questions, and claims every key for the runtime.
        None => l.child(
            fresh_ui::focusable(row())
                .key(sink_key(slot))
                .pointer_mode(PointerMode::Ignore)
                .autofocus()
                .on_key(move |e: &fresh_ui::Event| {
                    e.stop();
                    Some(UiMsg::Ui(UiFact::PanelKey(slot)))
                }),
        ),
    }
}

/// The key of a pane-mounted panel's keyboard layer.
///
/// The only keyboard layer that carries one, because it is the only one
/// that is not an overlay: it confines the ring to the panel in the pane
/// while the pane is active, exactly as the dock's does for the dock, but
/// nothing is layered *over* the content by it — so a paste still belongs
/// to the panel and a terminal in another pane is not gated by it.
pub fn pane_keys_key(leaf: crate::model::event::LeafId) -> Key {
    Key::Pair("keys_layer:pane".into(), leaf.0 .0 as u64)
}

/// Whether `k` names a keyboard layer that is the base's rather than an
/// overlay's. See [`pane_keys_key`].
pub fn is_base_layer(k: &Key) -> bool {
    matches!(k, Key::Pair(name, _) if &**name == "keys_layer:pane")
}

/// The key [`keys_layer`] names as its focus scope, per slot.
///
/// Every described interior carries one, whether or not a keyboard layer
/// names it as a scope: the ring a panel's focus advances along is read off
/// the interior by this key (`Ui::next_in`), and a pane-mounted panel has a
/// ring to advance along even though its keyboard is the buffer's.
/// The key of a panel's keyboard sink — the focusable that holds focus
/// when the panel's layer names no scope. With the interior's key
/// ([`interior_key`]) it is how `frame::key_context_of` reads that a
/// panel in this slot has the keyboard.
pub fn sink_key(slot: super::widgets::Slot) -> Key {
    match slot {
        super::widgets::Slot::Dock => Key::Str("keys:dock".into()),
        super::widgets::Slot::Floating => Key::Str("keys:floating_panel".into()),
        super::widgets::Slot::Sidebar(i) => Key::Pair("keys:sidebar".into(), i as u64),
        super::widgets::Slot::Pane(leaf) => Key::Pair("keys:pane".into(), leaf.0 .0 as u64),
        super::widgets::Slot::Settings | super::widgets::Slot::SettingsEntry => {
            Key::Str("keys:settings".into())
        }
        // The toolbar never raises a layer of its own: it sits in the
        // prompt's, whose sink is the card's input row.
        super::widgets::Slot::PromptToolbar => Key::Str("keys:prompt".into()),
    }
}

pub fn interior_key(slot: super::widgets::Slot) -> Key {
    let n = match slot {
        super::widgets::Slot::Dock => 0,
        super::widgets::Slot::Floating => 1,
        super::widgets::Slot::Settings => 2,
        super::widgets::Slot::SettingsEntry => 3,
        // One per pane: several panes may each mount a panel, and a ring is
        // read off one panel's interior at a time. The pane's content slot
        // *is* the panel's interior when a panel is mounted there
        // (`splits::panel_content`), and the slot carries the pane's own
        // content key, so that key names the interior.
        super::widgets::Slot::Pane(leaf) => return super::splits::content_key(leaf),
        super::widgets::Slot::PromptToolbar => 4,
        // Past the fixed slots, one per section.
        super::widgets::Slot::Sidebar(i) => 16 + i as u64,
    };
    Key::Pair("panel_interior".into(), n)
}

/// The panel's described interior: the scope its keyboard layer names, and the
/// fallback for every key its widgets decline.
///
/// **One node, two jobs, and they belong together.** The scope has to be an
/// ancestor of the controls — that is what confinement means — and the
/// fallback has to be on the focus chain for the bubble leg to reach it, which
/// is the same requirement. Splitting them would be two nodes with one
/// invariant between them.
///
/// **The panel's keymap rides on it** (`keymap`): a key the panel's plugin
/// mode explicitly binds is taken on the capture leg — before the widget
/// that holds focus, and before the traversal that would otherwise resolve
/// Tab — and arrives as the bound action. Every other key still reaches the
/// runtime through `PanelKey`, because a described widget attaches no key
/// handler of its own — the kinds' key handling is host-side, so nothing in
/// the tree competes for `Enter` or the arrows. Tab is different: declining
/// it is how the tree's ring moves focus, and it is declined here for every
/// panel, because a mode that binds Tab has already taken it above.
///
/// **Declining a key it might need back is safe here because of the layer's
/// modality, not because of anything this node does.** A declined Tab the ring
/// cannot serve — a panel holding one widget, or none — leaves `move_focus`
/// with nowhere to go, and what happens to the key then belongs to
/// [`keys_layer`]: `Modality::Focus` confines traversal without swallowing, so
/// `dispatch` reports the key unclaimed and the router answers it exactly as it
/// did before any of this. A layer that *swallows* — `Modality::Keyboard`, which
/// is what the settings dialog declares — would drop that Tab instead, with no
/// move and no host. `fresh-ui`'s
/// `a_key_the_ring_cannot_serve_is_handed_back_only_by_a_focus_layer` pins the
/// asymmetry.
pub fn interior(
    slot: super::widgets::Slot,
    keymap: Option<Keymap>,
    rests_empty: bool,
    body: Node<UiMsg>,
) -> Node<UiMsg> {
    let capture: Option<Capture> = keymap.map(|km| {
        Rc::new(move |e: &fresh_ui::Event| {
            let action = km.action(e.key?)?;
            e.stop();
            Some(UiMsg::Action(action))
        }) as Capture
    });
    interior_capturing(slot, capture, rests_empty, body)
}

/// A key handler on an interior's capture leg: what the *surface* says a key
/// means before the focused widget sees it. See [`interior_capturing`].
pub type Capture = Rc<dyn Fn(&fresh_ui::Event) -> Option<UiMsg>>;

/// [`interior`], with the surface's own capture-leg rule instead of a
/// plugin mode's keymap.
///
/// A panel's keymap is one such rule — a key the mode binds is the mode's
/// action ahead of any widget — and the overlay prompt's toolbar has another:
/// a key that navigates or types is the *query input's*, and pressing it on a
/// focused toggle hands the keyboard back to the input with the key
/// (`overlay_prompt::toolbar`). Both are properties of the interior node,
/// stated on the tree rather than in a host stage after the tree declined.
pub fn interior_capturing(
    slot: super::widgets::Slot,
    capture: Option<Capture>,
    rests_empty: bool,
    body: Node<UiMsg>,
) -> Node<UiMsg> {
    let (w, h) = (body.w, body.h);
    let n = fresh_ui::focusable(body)
        .w(w)
        .h(h)
        .key(interior_key(slot))
        .skip_traversal();
    let n = match capture {
        Some(c) => n.on_key_capture(move |e: &fresh_ui::Event| c(e)),
        None => n,
    };
    // **"Nothing focused" is a state the description says.** A panel whose
    // focus key is empty — one that declared `autoFocusFirst: false` and has
    // not been given a focus — marks its own scope, so the tree rests focus
    // on the interior rather than landing on the first control and telling
    // the registry, through the `WidgetFocus` echo, about a focus the panel
    // never chose. Tab from here starts from outside the ring.
    let n = match rests_empty {
        true => n.autofocus(),
        false => n,
    };
    n.on_key(move |e: &fresh_ui::Event| {
        let tab = e
            .key
            .is_some_and(|k| matches!(k.code, fresh_ui::KeyCode::Tab | fresh_ui::KeyCode::BackTab));
        if tab {
            // Declined, and deliberately not stopped: `propagate_key`
            // returns false, the key resolves to an intent, and
            // `default_for_intent` moves focus inside this scope. That is
            // the tree's ring doing what the box arena's ring did.
            return None;
        }
        e.stop();
        Some(UiMsg::Ui(UiFact::PanelKey(slot)))
    })
}

/// The panel's layer, and its claim on the pointer.
///
/// **The pointer is the panel's while it is up, and the keyboard is its own
/// layer's.** `Modality::Pointer` says the first: nothing behind the box is
/// interactive to a press, a move or a wheel, which is what the full-frame
/// modal slot under this layer used to say by claiming every event and
/// handing it to a handler that swallowed it. The keys go to the widget
/// runtime through the panel's own `Modality::Focus` layer
/// (`keys_layer`), which is why this is not `Exclusive` — an exclusive
/// layer owns the keyboard, containment made it the focus scope, and with
/// nothing focusable inside it focus was dropped.
///
/// An anchored popup (a plugin's right-click menu) is dismissed by a press
/// outside it, the way a menu is — `Dismiss::OUTSIDE_POINTER`, and the press
/// is spent on the dismissal; a centred modal swallows the outside press
/// instead, because it has explicit Cancel and Esc. That was the one arm of
/// `handle_floating_modal_mouse` that did anything, against a rectangle the
/// painter recorded (`last_inner_rect`); the layer knows its own box.
pub fn layer_for(p: &Panel) -> Node<UiMsg> {
    let l = layer().modality(fresh_ui::Modality::Pointer);
    match &p.spot {
        Spot::Centered { width_pct, .. } => {
            let l = match p.fullscreen {
                true => l,
                false => l.within(super::frame::chrome_key()),
            };
            l.anchor(Anchor::Screen(Align::Center))
                .place(Place::Over)
                .child(
                    frame_box(p)
                        .w(Sizing::Pct(*width_pct))
                        .min_w(20)
                        .h(p.height())
                        // The floor the arithmetic carried: a box is its two
                        // border rows and at least one row of content, even
                        // when the spec produced none.
                        .min_h(3)
                        .key(key()),
                )
        }
        // Full-frame whatever the dock is doing: the anchor is an absolute
        // screen cell that may sit over the dock column.
        Spot::Anchored {
            x, y, content_cols, ..
        } => l
            .anchor(Anchor::Point(*x, *y))
            .place(Place::Over)
            .fit(Fit::CLAMP)
            .dismiss(fresh_ui::Dismiss::OUTSIDE_POINTER)
            .on_dismiss(|_| UiMsg::Ui(UiFact::PanelClosed))
            .child(
                frame_box(p)
                    .w(p.anchored_width(*content_cols))
                    .min_w(6)
                    .h(p.height())
                    .min_h(3)
                    .key(key()),
            ),
    }
}

/// The ring, its ground, and the strip that sits on its top edge.
///
/// **The box claims its own presses**, and it has to. Layers are hit-tested
/// top down and the *first one with any path at the point wins* — a
/// transparent node still produces a path — so a press on the box's slack
/// that fell through would reach the buffer behind. A described interior's
/// widgets answer their own presses before this is reached; a painted one's
/// never answered any (the modal slot's handler it was routed to had no
/// arm for a press inside the box), so the press stops here either way.
///
/// **Never the wheel.** Scrolling is framework-owned: the library runs its
/// scroll chain for a notch *nothing claimed*, so a catch-all that stops the
/// wheel stops every viewport inside this box from scrolling. The chain
/// stops at the first out-of-flow node, and this box is inside a layer, so a
/// notch that scrolls nothing here is still absorbed rather than reaching
/// the buffer behind. `keybinding::swallow` had the same bug and the same
/// fix.
fn frame_box(p: &Panel) -> Node<UiMsg> {
    let ring = ring_theme(p);
    let framed = col().theme(ring).border().child(body(p));
    let claim = move |e: &Event| {
        e.stop();
        None
    };
    let mut g = gesture(fresh_ui::stack().children([framed, border_strip(p)]));
    for kind in [GestureKind::Press, GestureKind::Release, GestureKind::Move] {
        g = g.on(kind, Rc::new(claim));
    }
    g
}

fn ring_theme(p: &Panel) -> String {
    // The same accent the file explorer's focused border wears.
    match p.focused {
        true => pair("editor.cursor", "ui.suggestion_bg"),
        false => pair("ui.popup_border_fg", "ui.suggestion_bg"),
    }
}

/// The top border's overlay: the title where `Block::title` put it, and `[×]`
/// where the painter's `overlay_rect.width - 4` put it.
///
/// Transparent all the way down, container included, except the button — the
/// hit walk stops at the first child that blocks, so one opaque cell here
/// hides the interior behind the whole strip.
///
/// **And it is one row tall, which is the only row it has anything on.** It
/// used to be a column with a flexible filler under it, so the strip covered
/// the whole box; a transparent node still *produces a path*, and that path
/// runs up through the frame's catch-all gesture, which swallows what the
/// interior does not answer. Paths are tried topmost-first and the first one
/// to claim ends the dispatch — so the filler's path was offered before the
/// interior's, the catch-all took it, and **every press on a described
/// widget died one node short of the widget it landed on**. A strip that
/// ends where its content ends has no path below its row to offer.
fn border_strip(p: &Panel) -> Node<UiMsg> {
    let centred = matches!(p.spot, Spot::Centered { .. });
    let clear = |n: Node<UiMsg>| n.pointer_mode(PointerMode::Transparent);
    let mut cells: Vec<Node<UiMsg>> = vec![
        // `Block::title` starts one cell in from the corner.
        clear(row().w(Sizing::Cells(1))),
        clear(match (centred, p.title.as_deref()) {
            (true, Some(t)) => text(format!(" {t} ")).theme(attrs(
                "ui.popup_border_fg",
                "ui.suggestion_bg",
                &["bold"],
            )),
            _ => row().w(Sizing::Cells(0)),
        }),
        clear(row().flex(1)),
    ];
    if centred && p.closable {
        cells.push(close_button(p));
        // The painter left the last column clear.
        cells.push(clear(row().w(Sizing::Cells(1))));
    }
    row()
        .h(Sizing::Cells(1))
        .pointer_mode(PointerMode::Transparent)
        .children(cells)
}

/// The `[×]`, answering its own press.
///
/// It dismisses exactly as Esc and Cancel do — the same
/// `dismiss_floating_panel_with_cancel` path, which fires the panel's `cancel`
/// widget event. The old arm checked this rectangle *before* the general panel
/// hit-test so the click could not also focus a widget underneath; the node
/// stops the event, which is the same statement without the ordering.
fn close_button(p: &Panel) -> Node<UiMsg> {
    gesture(text("[×]").theme(ring_theme(p)))
        .key(close_key())
        .on(
            GestureKind::Press,
            Rc::new(|ev: &Event| {
                if ev.button != MouseButton::Left {
                    return None;
                }
                ev.stop();
                Some(UiMsg::Ui(UiFact::PanelClosed))
            }),
        )
}

/// The content area.
///
/// **Transparent when the interior is a painter**, so the box behind it
/// takes the press — the painter never answered one. **Opaque when it is
/// described**, because then the widgets answer their own presses and a
/// press that reaches the area but no widget is the panel's to swallow, not
/// the buffer's.
fn body(p: &Panel) -> Node<UiMsg> {
    let Some(i) = p.interior.clone() else {
        // **A host fills the box; it does not size it.** The box's height came
        // from the mirror's row count precisely because a `Host` leaf has no
        // intrinsic size, so the body takes the remainder it was given.
        return row()
            .flex(1)
            .key(body_key())
            .pointer_mode(PointerMode::Transparent);
    };
    // **A described body is as tall as its rows, and the box follows it.**
    // `flex(1)` here would fill a remainder that no longer exists — the box's
    // own height is `Auto` now, and a flexible child measures as nothing under
    // an indefinite constraint, so the frame would collapse to its border.
    // Width still fills: the cross axis of the enclosing column, stretched.
    let area = row().w(Sizing::Flex(1)).key(body_key());
    let keymap = i.keymap.clone();
    // **The width the widgets are laid out at is layout's answer, not the
    // caller's.** A centred panel is a percentage of its bounds, so nobody
    // knows the content width until the box has been measured — and the
    // collectors need it as a number before they can render a row. That is
    // what `layout_reader` is for: build the subtree from the constraints the
    // node was given. The alternative is the caller computing the percentage
    // itself, which is the second layout this migration exists to remove.
    let rests_empty = i.keyboard && i.focus_key.is_empty();
    let inner = fresh_ui::layout_reader(move |info: fresh_ui::LayoutInfo| {
        super::widgets::node(
            &i.spec,
            info.constraints.max_w.max(1),
            &super::widgets::Ctx {
                slot: super::widgets::Slot::Floating,
                states: &i.states,
                focus_key: i.focus_key.clone(),
                keyboard: i.keyboard,

                hovered_key: i.hovered_key.clone(),
                marker_gutter: i.marker_gutter,
                hovered_item_key: i.hovered_item_key.clone(),
                hovered_popup_row: i.hovered_popup_row.clone(),
                avail_height: i.avail_height,
                scrollbar_reveal: i.scrollbar_reveal,
                surface: super::widgets::panel_surface(),
                markdown: i.markdown.as_ref().map(|m| m.ctx()),
            },
        )
    });
    // The scope its keyboard layer names, and the fallback for what its
    // widgets decline — see `interior`. Every described interior is one,
    // whether or not anything in it is a Tab stop: an interior with no
    // stops holds focus itself (the tree rests focus on a scope's own
    // element, and `rests_empty` marks it), so its keymap and its fallback
    // answer for it exactly as they do for one full of controls.
    let inner = interior(super::widgets::Slot::Floating, keymap, rests_empty, inner);
    area.child(inner)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::view::shell::frame::{frame_tree, Frame};
    use fresh_ui::{Input, Mods, MouseButton, Point, Size, Ui};
    use ratatui::layout::Rect;

    const FRAME: Rect = Rect {
        x: 0,
        y: 0,
        width: 100,
        height: 30,
    };

    /// The arithmetic this module replaced, kept verbatim as the oracle.
    ///
    /// Copied out of `render_floating_widget_panel` rather than paraphrased:
    /// a parity test whose oracle is a re-derivation of what the code now does
    /// proves the code agrees with itself.
    mod painter {
        use ratatui::layout::Rect;

        pub fn centered_overlay_rect(area: Rect, width_pct: u8, height_pct: u8) -> Rect {
            let w_pct = width_pct.clamp(1, 100) as u32;
            let h_pct = height_pct.clamp(1, 100) as u32;
            let w = ((area.width as u32 * w_pct) / 100) as u16;
            let h = ((area.height as u32 * h_pct) / 100) as u16;
            let w = w.max(20).min(area.width);
            let h = h.max(8).min(area.height);
            Rect {
                x: area.x + (area.width.saturating_sub(w)) / 2,
                y: area.y + (area.height.saturating_sub(h)) / 2,
                width: w,
                height: h,
            }
        }

        pub fn centered(area: Rect, width_pct: u8, rows: u16) -> Rect {
            let requested = centered_overlay_rect(area, width_pct, 50);
            let needed_h = rows.saturating_add(2);
            let effective_h = needed_h.clamp(3, area.height.max(3));
            Rect {
                x: requested.x,
                y: area.y + (area.height.saturating_sub(effective_h)) / 2,
                width: requested.width,
                height: effective_h,
            }
        }

        pub fn anchored(area: Rect, x: u16, y: u16, cols: u16, rows: u16) -> Rect {
            let w = cols.saturating_add(2).clamp(6, area.width);
            let h = rows.saturating_add(2).clamp(3, area.height);
            let max_x = area.x + area.width.saturating_sub(w);
            let max_y = area.y + area.height.saturating_sub(h);
            Rect {
                x: x.clamp(area.x, max_x),
                y: y.clamp(area.y, max_y),
                width: w,
                height: h,
            }
        }
    }

    fn panel(spot: Spot) -> Panel {
        Panel {
            spot,
            title: Some("A Dialog".into()),
            closable: true,
            focused: true,
            fullscreen: true,
            interior: None,
        }
    }

    fn laid_out(p: Option<Panel>) -> Ui<UiMsg> {
        let mut ui: Ui<UiMsg> = Ui::new();
        ui.frame(
            frame_tree(Frame {
                panel: p,
                ..Frame::default()
            }),
            Size::new(FRAME.width, FRAME.height),
        );
        ui
    }

    fn rect(ui: &Ui<UiMsg>, k: &Key) -> Option<Rect> {
        super::super::rect_of(ui, k, FRAME)
    }

    /// **The box lands where the painter put it.** Every shape the painter had
    /// a branch for: content shorter than the request, content taller than the
    /// frame, and the narrow-width floor.
    #[test]
    fn a_centred_panel_is_placed_where_the_arithmetic_put_it() {
        for (pct, rows) in [(50u8, 6u16), (90, 3), (30, 40), (10, 5), (100, 1)] {
            let ui = laid_out(Some(panel(Spot::Centered {
                width_pct: pct,
                content_rows: rows,
            })));
            assert_eq!(
                rect(&ui, &key()),
                Some(painter::centered(FRAME, pct, rows)),
                "centred at {pct}% with {rows} rows"
            );
        }
    }

    /// And an anchored popup, including both clamps — a cell near the right
    /// edge and one near the bottom, where the box would otherwise hang off.
    #[test]
    fn an_anchored_panel_is_clamped_the_way_the_painter_clamped_it() {
        for (x, y, cols, rows) in [
            (10u16, 5u16, 12u16, 4u16),
            (95, 5, 12, 4),
            (10, 28, 12, 4),
            (99, 29, 30, 10),
            (0, 0, 2, 1),
        ] {
            let ui = laid_out(Some(panel(Spot::Anchored {
                x,
                y,
                content_cols: cols,
                content_rows: rows,
            })));
            assert_eq!(
                rect(&ui, &key()),
                Some(painter::anchored(FRAME, x, y, cols, rows)),
                "anchored at ({x},{y}) sized {cols}x{rows}"
            );
        }
    }

    /// The content area is the box less its ring — what `Block::inner` gave the
    /// painter, derived from the same layout that placed the box rather than
    /// asked of a second widget.
    #[test]
    fn the_content_area_is_the_box_less_its_border() {
        let ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let box_rect = rect(&ui, &key()).expect("a box");
        let body = rect(&ui, &body_key()).expect("a content area");
        assert_eq!(
            body,
            Rect {
                x: box_rect.x + 1,
                y: box_rect.y + 1,
                width: box_rect.width - 2,
                height: box_rect.height - 2,
            }
        );
    }

    /// **`[×]` where `overlay_rect.width - 4` put it**, which is the rectangle
    /// the painter filed and a mouse arm compared against.
    #[test]
    fn the_close_button_sits_where_the_painter_recorded_it() {
        let ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let box_rect = rect(&ui, &key()).expect("a box");
        assert_eq!(
            rect(&ui, &close_key()),
            Some(Rect {
                x: box_rect.x + box_rect.width - 4,
                y: box_rect.y,
                width: 3,
                height: 1,
            })
        );
    }

    fn facts(d: fresh_ui::Dispatch<UiMsg>) -> Vec<UiFact> {
        d.msgs
            .into_iter()
            .filter_map(|m| match m {
                UiMsg::Ui(f) => Some(f),
                _ => None,
            })
            .collect()
    }

    /// **The panel's keymap takes a key its mode binds before the widget
    /// that holds focus sees it, and the key arrives as the action.** A
    /// panel with no keymap hands the same key to its fallback, which names
    /// the panel for the router.
    #[test]
    fn a_key_the_panels_mode_binds_arrives_as_its_action() {
        use crate::input::keybindings::{Action, KeybindingResolver};
        use fresh_core::api::WidgetSpec;
        let mut config = crate::config::Config::default();
        config.keybindings.push(crate::config::Keybinding {
            key: "enter".to_string(),
            modifiers: Vec::new(),
            keys: Vec::new(),
            action: "save".to_string(),
            args: std::collections::HashMap::new(),
            when: Some("mode:form".to_string()),
        });
        let resolver =
            std::sync::Arc::new(std::sync::RwLock::new(KeybindingResolver::new(&config)));
        let interior = |keymap: Option<Keymap>| Interior {
            spec: std::rc::Rc::new(WidgetSpec::Col {
                children: vec![WidgetSpec::Button {
                    label: "ok".into(),
                    focused: false,
                    intent: Default::default(),
                    key: Some("ok".into()),
                    disabled: false,
                    focusable: true,
                    bare: false,
                    full_width: false,
                    hover_style: None,
                    style: None,
                }],
                key: None,
            }),
            states: Default::default(),
            focus_key: "ok".into(),
            keyboard: true,

            page: None,
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap,
            markdown: None,
        };
        let framed = |keymap: Option<Keymap>| {
            let mut p = panel(Spot::Centered {
                width_pct: 60,
                content_rows: 4,
            });
            p.interior = Some(interior(keymap));
            let mut ui: Ui<UiMsg> = Ui::new();
            ui.frame(
                frame_tree(Frame {
                    panel_keys: true,
                    panel: Some(p),
                    ..Frame::default()
                }),
                Size::new(FRAME.width, FRAME.height),
            );
            let _ = ui.take_messages();
            ui
        };
        let enter = Input::Key(fresh_ui::KeyPress::with(
            fresh_ui::KeyCode::Enter,
            Mods::NONE,
        ));

        let mut ui = framed(Some(Keymap {
            mode: "form".into(),
            resolver: resolver.clone(),
            text_focused: false,
        }));
        let got = ui.dispatch(enter);
        assert!(got.claimed, "the keymap claims what it binds");
        assert!(
            got.msgs
                .iter()
                .any(|m| matches!(m, UiMsg::Action(Action::Save))),
            "and the key arrives as the bound action: {:?}",
            got.msgs
        );

        let mut ui = framed(None);
        let got = ui.dispatch(enter);
        assert_eq!(
            facts(got),
            vec![UiFact::PanelKey(super::super::widgets::Slot::Floating)],
            "without a keymap the fallback names the panel"
        );
    }

    /// **A focused text field takes a printable key ahead of the mode.**
    /// The mode binds Space; while a field has the keyboard, Space is
    /// typed into it (the fallback names the panel, and the router feeds
    /// the field), and only a key that is not a character — Enter — is
    /// still the mode's.
    #[test]
    fn a_focused_field_types_a_printable_key_the_mode_also_binds() {
        use crate::input::keybindings::{Action, KeybindingResolver};
        let mut config = crate::config::Config::default();
        for (key, action) in [("space", "save"), ("enter", "save")] {
            config.keybindings.push(crate::config::Keybinding {
                key: key.to_string(),
                modifiers: Vec::new(),
                keys: Vec::new(),
                action: action.to_string(),
                args: std::collections::HashMap::new(),
                when: Some("mode:form".to_string()),
            });
        }
        let resolver =
            std::sync::Arc::new(std::sync::RwLock::new(KeybindingResolver::new(&config)));
        let km = Keymap {
            mode: "form".into(),
            resolver,
            text_focused: true,
        };
        let space = fresh_ui::KeyPress::with(fresh_ui::KeyCode::Char(' '), Mods::NONE);
        assert_eq!(km.action(space), None, "Space is the field's");
        let shifted = fresh_ui::KeyPress::with(fresh_ui::KeyCode::Char('A'), Mods::SHIFT);
        assert_eq!(km.action(shifted), None, "a shifted letter is still typed");
        let enter = fresh_ui::KeyPress::with(fresh_ui::KeyCode::Enter, Mods::NONE);
        assert_eq!(km.action(enter), Some(Action::Save), "Enter is the mode's");
        let km = Keymap {
            text_focused: false,
            ..km
        };
        assert_eq!(
            km.action(space),
            Some(Action::Save),
            "with no field focused the mode binds Space"
        );
    }

    /// **The button answers its own press, and it wins.** The panel's whole
    /// channel is claimed by the modal layer underneath; this is the one cell
    /// that is not the interior's, and the ordering that used to be a comment
    /// ("checked BEFORE the general panel hit-test") is now the tree's.
    #[test]
    fn a_press_on_the_close_button_is_the_buttons_and_not_the_interiors() {
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let cb = rect(&ui, &close_key()).expect("a button");
        let got = facts(ui.dispatch(Input::press(
            Point::new(cb.x as i32 + 1, cb.y as i32),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert_eq!(got, vec![UiFact::PanelClosed]);
    }

    /// And a press one cell to the left of it is the interior's, as every
    /// other cell of the panel is. This is the assertion that would fail if
    /// the strip were opaque — the failure the popup wave already paid for
    /// once, where one solid title cell hid the whole frame behind it.
    #[test]
    fn a_press_beside_the_close_button_falls_through_to_the_interior() {
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let cb = rect(&ui, &close_key()).expect("a button");
        let got = facts(ui.dispatch(Input::press(
            Point::new(cb.x as i32 - 2, cb.y as i32),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert!(
            got.is_empty(),
            "the frame is decoration; only the button is not: {got:?}"
        );
    }

    /// A press in the middle of the content area, likewise — the interior
    /// hit-tests itself and must still be reached.
    #[test]
    fn a_press_in_the_content_area_reaches_the_interior() {
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let body = rect(&ui, &body_key()).expect("a content area");
        let got = ui.dispatch(Input::press(
            Point::new(
                body.x as i32 + body.width as i32 / 2,
                body.y as i32 + body.height as i32 / 2,
            ),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(got.claimed, "the box takes it");
        assert!(facts(got).is_empty(), "and nothing behind it is asked");
    }

    /// **An anchored popup is dismissed by a press outside it**, and a
    /// centred modal is not — the one arm of `handle_floating_modal_mouse`
    /// that did anything, which compared the press against a rectangle the
    /// painter had recorded. The layer knows its own box.
    #[test]
    fn a_press_outside_an_anchored_popup_dismisses_it_and_a_modal_swallows_it() {
        let mut ui = laid_out(Some(panel(Spot::Anchored {
            x: 10,
            y: 5,
            content_cols: 12,
            content_rows: 4,
        })));
        let got = ui.dispatch(Input::press(
            Point::new(60, 20),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert_eq!(facts(got), vec![UiFact::PanelClosed]);
        let mut ui = laid_out(Some(panel(Spot::Centered {
            width_pct: 60,
            content_rows: 8,
        })));
        let got = ui.dispatch(Input::press(
            Point::new(1, 1),
            MouseButton::Left,
            Mods::NONE,
        ));
        assert!(got.claimed, "the modal owns the pointer");
        assert!(facts(got).is_empty(), "and swallows a press beside it");
    }

    /// An anchored popup wears neither title nor button — the painter's rule,
    /// and the reason is that it is dismissed by clicking away from it.
    #[test]
    fn an_anchored_popup_has_no_close_button() {
        let ui = laid_out(Some(panel(Spot::Anchored {
            x: 10,
            y: 5,
            content_cols: 12,
            content_rows: 4,
        })));
        assert!(rect(&ui, &close_key()).is_none());
    }

    /// **A described box is as tall as its rows, not as tall as the mirror
    /// said.** The `Spot`'s count is deliberately wrong here — nine rows for a
    /// two-row spec — and the box comes out four cells: two rows of content
    /// between two border rows. That is 2.3's exit condition for this slot:
    /// the runtime's text mirror no longer feeds the panel's geometry, so
    /// deleting it would move nothing.
    ///
    /// The count still governs a *painted* panel, and the case above this one
    /// pins that. A `Host` leaf has no intrinsic size; the mirror is the only
    /// thing that can answer for it.
    #[test]
    fn a_described_box_is_measured_not_counted() {
        use fresh_core::api::WidgetSpec;
        let spec = WidgetSpec::Raw {
            entries: vec![
                fresh_core::text_property::TextPropertyEntry::text("alpha"),
                fresh_core::text_property::TextPropertyEntry::text("beta"),
            ],
            key: None,
        };
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 9,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(spec),
            states: Default::default(),
            focus_key: String::new(),
            keyboard: true,

            page: None,
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap: None,
            markdown: None,
        });
        let described = rect(&laid_out(Some(p.clone())), &key()).expect("a described box");
        assert_eq!(described.height, 4, "two rows inside two borders");

        // The same spot with no interior is the painter's, and still counts.
        let mut painted = p;
        painted.interior = None;
        assert_eq!(
            rect(&laid_out(Some(painted)), &key())
                .expect("a painted box")
                .height,
            11,
            "nine rows inside two borders, because a host cannot say"
        );
    }

    /// **The interior is in the tree.** A described panel's widget rows are
    /// laid out inside the content area — not painted over it afterwards by a
    /// second pass — so they have rectangles the pointer can be tested
    /// against, which is the whole point of describing them.
    #[test]
    fn a_described_interior_lands_inside_the_content_area() {
        use fresh_core::api::WidgetSpec;
        let spec = WidgetSpec::Col {
            children: vec![WidgetSpec::Raw {
                entries: vec![
                    fresh_core::text_property::TextPropertyEntry::text("alpha"),
                    fresh_core::text_property::TextPropertyEntry::text("beta"),
                ],
                key: None,
            }],
            key: None,
        };
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 4,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(spec),
            states: Default::default(),
            focus_key: String::new(),
            keyboard: true,

            page: None,
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap: None,
            markdown: None,
        });
        let ui = laid_out(Some(p));
        let body = rect(&ui, &body_key()).expect("a content area");
        for text in ["alpha", "beta"] {
            // The panel is a layer, so its content is in the layer band —
            // which is the whole point: it is placed, not painted afterwards.
            let at = ui
                .spec()
                .in_flow()
                .iter()
                .chain(ui.spec().layers().iter())
                .find_map(|i| match &i.draw {
                    fresh_ui::Draw::Lines(l) if l.iter().any(|s| &**s == text) => Some(i.rect),
                    _ => None,
                })
                .unwrap_or_else(|| panic!("no row for {text}"));
            assert!(
                at.x >= body.x as i32
                    && at.y >= body.y as i32
                    && at.y < (body.y + body.height) as i32,
                "{text} at {at:?} is outside the content area {body:?}"
            );
        }
    }

    /// **A press on a described widget delivers that widget's hit.** The area
    /// keeping the press (below) and a widget answering it are two different
    /// facts, and only the first was asserted: the panel's frame wraps its
    /// whole box in one gesture that swallows what the interior does not
    /// answer, and if that gesture is reached before the widget's the press
    /// dies one node short of the thing it landed on.
    #[test]
    fn a_press_on_a_described_widget_delivers_its_hit() {
        use fresh_core::api::WidgetSpec;
        let spec = WidgetSpec::Button {
            label: "Go".into(),
            focused: false,
            intent: Default::default(),
            key: Some("go".into()),
            disabled: false,
            focusable: true,
            bare: false,
            full_width: false,
            hover_style: None,
            style: None,
        };
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 4,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(spec),
            states: Default::default(),
            focus_key: String::new(),
            keyboard: true,

            page: None,
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap: None,
            markdown: None,
        });
        let mut ui = laid_out(Some(p));
        let at = ui
            .spec()
            .in_flow()
            .iter()
            .chain(ui.spec().layers().iter())
            .find_map(|i| match &i.draw {
                fresh_ui::Draw::Lines(l) if l.iter().any(|s| s.contains("Go")) => Some(i.rect),
                _ => None,
            })
            .expect("the button paints");
        let got = facts(ui.dispatch(Input::press(
            Point::new(at.x + 1, at.y),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert!(
            got.iter().any(|f| matches!(
                f,
                UiFact::WidgetHit { event: hit, .. } if hit.widget_key == "go"
            )),
            "a press on the button at {at:?} should be its hit, got {got:?}"
        );
    }

    /// A described panel's content area is **opaque**: the widgets answer
    /// their own presses, and one that reaches the area but no widget is the
    /// panel's to swallow.
    #[test]
    fn a_described_content_area_does_not_let_presses_through() {
        use fresh_core::api::WidgetSpec;
        let mut p = panel(Spot::Centered {
            width_pct: 60,
            content_rows: 4,
        });
        p.interior = Some(Interior {
            spec: std::rc::Rc::new(WidgetSpec::Raw {
                entries: vec![fresh_core::text_property::TextPropertyEntry::text("x")],
                key: None,
            }),
            states: Default::default(),
            focus_key: String::new(),
            keyboard: true,

            page: None,
            hovered_key: None,
            hovered_item_key: String::new(),
            hovered_popup_row: String::new(),
            marker_gutter: false,
            avail_height: None,
            scrollbar_reveal: None,
            keymap: None,
            markdown: None,
        });
        let mut ui = laid_out(Some(p));
        let body = rect(&ui, &body_key()).expect("a content area");
        let got = facts(ui.dispatch(Input::press(
            Point::new(body.x as i32 + 1, body.y as i32 + 1),
            MouseButton::Left,
            Mods::NONE,
        )));
        assert!(
            got.is_empty(),
            "the described area keeps the press, got {got:?}"
        );
    }

    /// No panel, no box.
    #[test]
    fn no_panel_means_no_frame_in_the_tree() {
        let ui = laid_out(None);
        assert!(rect(&ui, &key()).is_none());
        assert!(rect(&ui, &body_key()).is_none());
    }
}
