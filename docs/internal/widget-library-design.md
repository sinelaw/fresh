# One Widget Library — Design Spec

> _AI-generated design doc. **Entirely PLANNED** — nothing here ships today._
>
> Target: one tree/layout/event/render library used by **both** the
> plugin-facing widgets **and** all editor chrome (menus, prompt, popups,
> context menus, dock, splits, modals, status bar).
>
> Third doc in an arc. [`widget-framework-v2-review.md`](widget-framework-v2-review.md)
> diagnosed the panel side and built the per-kind `WidgetImpl` dispatch, the
> `LayoutBox` arena and hit-path propagation;
> [`chrome-event-model-plan.md`](chrome-event-model-plan.md) registered the chrome
> surfaces and put them behind one pointer walk and one keyboard walk. This doc
> is where those two point: the same model for both trees, with the
> hand-specified data (ranks, z bands, guard boxes, grab enum, pre-band stages)
> replaced by declared node properties. It assumes both arcs as its starting
> point rather than restating them.

---

## 1. Why

Today there are two trees. The panel tree has containment, per-node handlers,
a derived focus ring, and delegation. The chrome tree is a flat list of
rootless rectangles dispatched by a central walk, where precedence is
hand-assigned integers and "inside/outside" is emulated with full-frame guard
boxes. Chrome pays for the missing structure with hand-specified exceptions:
a rank table, a pre-band of stages that bypass dispatch, a global drag enum,
and five separate encodings of modality.

This spec collapses both into one model. The design rule throughout: **a
component declares its own nodes and listens on its own nodes, and nothing
else.** Any behavior that needs global knowledge is expressed as a *declared
property* of a node (modality, dismissal, capture), never as a central list.

### Non-goals

- **Buffer/terminal text rendering.** The token IR → `ViewLine` pipeline, syntax
  highlighting, folding, wrapping, and PTY grids stay as they are, reached
  through `Kind::Host`.
- **Retained mutable widget objects.** The tree is rebuilt per frame from
  model state (immediate mode preserved; see §5).
- **Damage tracking.** crossterm's back-buffer diff stays the incremental layer.
- **Animation.** The existing frame-buffer animation runner is unchanged.

---

## 2. Model

One arena per frame. Children precede parents (root last). `NodeId` is an
arena index, valid for one frame. `Key` is stable across frames and is what
instance state, focus, and tests address.

```rust
pub struct Node<M> {
    pub key: Option<Key>,          // stable identity; required to hold state or focus
    pub kind: Kind<M>,
    pub layout: Layout,
    pub focus: FocusSpec,
    pub pointer: PointerMode,      // Opaque (default) | Transparent | Ignore
    pub keymap: Option<KeyScope>,  // contributes to keybinding resolution (§7.6)
    pub listeners: Listeners<M>,
    pub observers: Observers<M>,   // root-installed; see §7.4
    pub children: Vec<Node<M>>,
}

pub enum Kind<M> {
    // containers
    Row, Col, Stack,
    // controls
    Text(TextSpec), List(ListSpec), Tree(TreeSpec), Button(ButtonSpec),
    Toggle(ToggleSpec), Number(NumberSpec), Dropdown(DropdownSpec),
    DualList(DualListSpec), Divider, Spacer, Raw(Vec<Line>),
    // out-of-flow subtree (§4)
    Layer(LayerSpec),
    // foreign content: buffer split, terminal grid, native window
    Host(HostId),
}
```

`M` is the app's message type. Chrome instantiates `Node<Action>`; plugin
panels instantiate `Node<WidgetEvent>`. The engine is generic over `M` and
never inspects it.

Every `Kind` variant is implemented by one `Widget` impl — the single
kind-dispatch, as today:

```rust
pub trait Widget<M> {
    fn caps(&self) -> Caps;                                   // focusable, scroll, picker_target, …
    fn measure(&self, n: &Node<M>, c: Constraints) -> Size;   // §3
    fn arrange(&self, n: &Node<M>, r: Rect, out: &mut LayoutOut);
    fn emit(&self, n: &Node<M>, g: &Geom, s: &State, out: &mut DrawList);  // §5 — no cells, no ratatui
    fn on_event(&self, n: &Node<M>, e: &Event, cx: &mut Cx<M>) -> Flow;    // §7
}
```

`Caps` replaces every "is this kind a …" match outside the impl (scroll
target, arrow-peek participant, activates-on-picker-Enter, settable value).
A new kind adds one impl and one dispatch arm; nothing else.

---

## 3. Layout

Two passes, standard constraint layout.

```rust
pub struct Layout {
    pub w: Sizing, pub h: Sizing,     // Auto | Cells(u16) | Flex(u16) | Pct(u8)
    pub min: Extent, pub max: Extent,
    pub pad: Edges, pub gap: u16,
    pub align: Align,                 // cross axis
    pub justify: Justify,             // main axis
    pub overflow: Overflow,           // Visible | Clip | Scroll
}
```

- `measure(constraints) -> Size` bottom-up: `Auto` asks the child, `Cells`/`Pct`
  are fixed, `Flex` reports its minimum.
- `arrange(rect)` top-down: leftover main-axis space is divided by `Flex`
  weight; `min`/`max` clamp; `Overflow::Scroll` establishes a scroll container
  and writes a `Scroll { total, visible, offset }` onto the node's layout entry.

Output is `LayoutOut`: for every node, `rect`, `clip`, `z`, optional `scroll`,
and its parent link. **`LayoutOut` is the sole source of geometry** — for
painting, hit-testing, the Scene projection, and tests. Nothing reads geometry
recorded during paint (§5.4).

---

## 4. Floating: `Layer`

A `Layer` is a normal child in the tree — so it inherits containment, focus
order, propagation, and its owner's identity — but is laid out and painted
out of flow.

```rust
pub struct LayerSpec {
    pub anchor: Anchor,          // Parent | Node(Key) | Point(u16,u16) | Screen(Align)
    pub place: Place,            // Below | Above | RightOf | LeftOf | Over | Fill
    pub fit: Fit,                // FLIP | CLAMP | SHIFT
    pub modality: Modality,      // None | Inert | Exclusive
    pub scrim: Option<Scrim>,    // dim / block behind
    pub dismiss: Dismiss,        // bitflags: OUTSIDE_POINTER | ESCAPE | ANY_KEY | ANY_INPUT
    pub wheel_capture: bool,     // position-blind wheel intent (§7.4)
}
```

- **Anchoring** is relative to a node or a cell; `Fit` handles flip/clamp near
  frame edges. This subsumes both the dropdown pop-over channel and the
  screen-space popup channel.
- **Modality** is one property with total reach: `Inert` marks everything
  outside the layer's subtree non-interactive; `Exclusive` additionally
  suppresses host-leaf raw input (PTY forwarding). Pointer, keyboard, focus
  ring, hover, and cursor visibility all derive from `inert` — no separate
  `blocks_terminal_input`, `modal_overlay_active`, or cursor-suppression list.
- **Dismissal is declarative.** `OUTSIDE_POINTER` means "a pointer press whose
  target is not inside this subtree closes me" — a containment test the engine
  does with the tree it already has. This is what replaces every full-frame
  close-guard box.

**Stacking**: layers paint in tree order within their parent stacking context;
a `Layer` creates one, so a layer's descendants can never interleave with
content outside it. There is no global z scale to co-ordinate.

---

## 5. Rendering

The current renderer is immediate-mode: every frame re-derives the screen and
crossterm diffs cells. That stays. Two things change: geometry becomes an
*output of layout* rather than a side effect of painting, and the engine never
touches cells. Instead it walks the **visible** part of the tree and emits a
**layout spec** — a flat, ordered, backend-independent display list that sits
between the tree and any actual drawing.

### 5.1 Frame phases

```
1. build     state → Node tree             pure; no geometry, no I/O
2. layout    measure/arrange → LayoutOut   rects, clips, z, scroll, parents
3. emit      cull + walk visible → LayoutSpec      ← the intermediate
4. present   LayoutSpec → cells | DOM | JSON | assertions
```

Only phase 1 reads app state. Phases 2–3 are mechanical and pure. Phase 4 is
per-backend and knows nothing about nodes, events, or state.

### 5.2 `LayoutSpec` — the intermediate

```rust
pub struct LayoutSpec {
    pub frame: Size,
    pub items: Vec<Item>,               // paint order; z already resolved
    pub index: Vec<(Key, Range<usize>)>,// key → its items (hit-test, tests, web patching)
    pub cursor: Option<CursorSpec>,
}

pub struct Item {
    pub key: Option<Key>,
    pub rect: Rect,        // absolute frame coords
    pub clip: Rect,        // ancestor clips already intersected
    pub theme: ThemeKey,   // per-item provenance (feeds the theme inspector)
    pub draw: Draw,
}

pub enum Draw {
    Fill(ThemeKey),
    Border(BorderStyle),
    Scrim(Scrim),
    Lines(Vec<Line>),        // resolved text + inline styles, already windowed
    Scrollbar(ScrollbarSpec),
    Host(HostId),            // foreign content: the backend resolves it (§5.5)
}
```

Properties that matter:

- **Flat and ordered** — paint order *is* list order, so stacking is resolved
  once during emit and no consumer re-derives it.
- **Absolute and pre-clipped** — no consumer needs the tree or a transform stack.
- **No borrows of app state** — it is a value, so it can be snapshotted,
  serialized, diffed, or built off-thread.
- **Keyed** — `index` makes every item addressable, which is what lets tests
  assert on structure instead of scraping cells, and lets the web backend patch
  by key.

### 5.3 Emit is the visible-tree walk

Emit is where "only render what's on screen" happens. It walks the tree in
stacking order and skips work four ways:

| skip | rule |
|---|---|
| off-screen / clipped out | `rect ∩ clip` empty → prune the whole subtree |
| occluded | an opaque `Layer` covering the frame stops emission of in-flow content beneath it |
| scrolled out | a `Overflow::Scroll` container emits only `[offset, offset+visible)` of its children |
| conditionally absent | `if_(false)` nodes never enter the tree in phase 1 |

Cost is therefore **O(visible items)**, not O(tree): a `List` over 100k rows
emits at most a screenful of `Lines`, and a collapsed subtree emits nothing.
The scroll-window rule is the same one `List`/`Tree` already apply internally
today; here it is a property of the walk, so every container inherits it.

Widgets contribute through a narrow sink:

```rust
impl DrawList<'_> {
    fn fill(&mut self, r: Rect, t: ThemeKey);
    fn lines(&mut self, r: Rect, lines: impl IntoIterator<Item = Line>);
    fn border(&mut self, r: Rect, s: BorderStyle);
    fn scrim(&mut self, r: Rect, s: Scrim);
    fn host(&mut self, r: Rect, id: HostId);
    fn scrollbar(&mut self, r: Rect, s: ScrollbarSpec);
    fn cursor(&mut self, at: (u16, u16), shape: CursorShape);
}
```

`Geom` (passed to `emit`) carries the node's resolved rect, clip, and scroll
window, so a widget emits only its visible rows without computing visibility
itself.

### 5.4 Backends

A backend is a fold over `items`:

- **TUI** — writes into a ratatui `Buffer` (a `Buffer`, not a `Frame`, so
  offscreen previews and tests work), then the existing colour-capability
  conversion and crossterm's cell diff run unchanged.
- **Web** — maps items to DOM nodes / a JSON patch stream, keyed by `Key`.
- **Tests** — assert on `items`/`index` directly: "the menu dropdown exists,
  at this rect, above the editor" with no screen scraping.

TUI/web parity stops being two implementations of the same chrome and becomes
one IR with two presenters.

### 5.5 Host leaves

```rust
pub trait HostLeaf {
    fn measure(&self, c: Constraints) -> Size;
    fn emit(&self, r: Rect, out: &mut DrawList);    // or leave Draw::Host for the backend
    fn hit(&self, local: (u16, u16)) -> HostHit;    // opaque, or a semantic position
    fn raw_input(&self) -> RawInput;                // None | Keys | KeysAndPointer  (PTY)
}
```

Buffer splits and terminal grids keep their existing pipelines. A leaf may
either emit `Lines` (so buffer content lands in the IR and the web backend
gets it for free) or emit `Draw::Host` and let the TUI backend call the
token-IR renderer directly — the second is the cheap path for large buffers
and the first is what web parity wants, so this is a per-leaf choice, not a
global one.

`raw_input` is how a live PTY asks for unprocessed events; the engine honors
it only when the leaf is focused and not `inert`, replacing the
terminal-forward gate and its suppression list.

### 5.6 One geometry source

Today's chrome layout cache is written *during* paint and read by hit-testing,
which is why paint-time surfaces need a parallel click path. Here hit-testing
reads `LayoutOut` (phase 2) and everything visual reads `LayoutSpec` (phase 3).
An anchored, flipped layer is resolved in phase 2 like everything else, so
there is no such thing as a rect only known after drawing.

### 5.7 Damage (optional, later)

Because `LayoutSpec` is a keyed value, consecutive frames can be diffed to get
a per-item change set. The TUI does not need it (crossterm already diffs
cells), but the web backend wants it for patching, and it is the natural place
to add coarse damage if profiling ever asks for it. Not required for v1.

---

## 6. State and effects

**Spec/instance split** (as today, generalized): the tree is a pure projection
of app state, rebuilt freely; per-node runtime state (scroll offset, selection,
text caret, expansion, open flags) persists across frames in a `State` map
keyed by `Key`. Rebuilding the tree never loses it; dropping a `Key` drops it.

Handlers do **not** take `&mut App`. They emit:

```rust
pub struct Cx<'a, M> {
    pub phase: Phase, pub target: NodeId, pub current: NodeId,
    pub state: &'a mut State,      // this node's own instance state only
    pub out: &'a mut Vec<M>,       // messages to the app
    pub fx: &'a mut Effects,       // engine actions
}

pub enum Effect {
    CapturePointer(NodeId), ReleasePointer,
    Focus(Key), FocusNext, FocusPrev,
    DismissLayer(NodeId), ScrollIntoView(NodeId),
    Clipboard(ClipboardOp), Rerender,
}
```

This is the boundary that makes encapsulation real: a component can change its
own state, ask the engine for an engine-level action, or tell the app something
happened. It cannot reach a sibling.

---

## 7. Events

### 7.1 Types

```rust
pub enum Event {
    Pointer(PointerEvent),   // Down{button,count} | Up | Move | Enter | Leave
    Wheel(WheelEvent),       // dx, dy
    Key(KeyEvent),
    Text(String),            // committed text / paste / IME
    Focus(FocusEvent),       // Gained | Lost
}
pub enum Phase { Capture, Target, Bubble }
pub enum Flow  { Continue, Stop }
```

`Flow::Stop` is `stopPropagation`. There is no `PassAfter`: acting without
stopping *is* `Continue`, so act-then-continue guards need no special value.
Suppressing a kind's built-in behavior while still bubbling is
`cx.fx.prevent_default()`.

### 7.2 Dispatch

```
target = hit_test(point)            // pointer/wheel;  = focused node for key/text
path   = ancestors(target)          // root … target
for n in path            { widget(n).on_event(Capture) ; listeners(n, Capture) }   // stop → done
for n in [target]        { widget(n).on_event(Target)  ; listeners(n, Target)  }
for n in path.rev()      { widget(n).on_event(Bubble)  ; listeners(n, Bubble)  }
```

Built-in behavior runs before listeners at each node, and either can stop.
Wheel bubbling with `Overflow::Scroll` containers gives scroll chaining for
free: a container consumes only if it actually moved, otherwise the wheel
continues outward and is dropped at the root.

### 7.3 Hit testing

```
hit_test(p):
  for layer in stacking order, topmost first:
     if inert(layer) skip
     n = deepest node in layer whose rect contains p and pointer != Ignore
     if n is Some: return n if pointer == Opaque else continue below
  return root
```

`PointerMode::Opaque` is the default — a popup absorbs what it doesn't handle
without declaring anything. `Transparent` passes through after its own
handlers; `Ignore` is not hittable at all.

### 7.4 Observers — the only global registration

Some behavior is genuinely whole-frame by intent, not by geometry: dismiss a
transient popup on any key, cancel a rename on any mouse event, scroll a
bottom-anchored suggestion list wherever the pointer is, an input-capture
debug overlay. These register as **observers**, declared by the owning node,
installed at the root, run in tree order before dispatch:

```rust
Node::observe(EventKind::Key, Msg::DismissHover)      // sees every key, doesn't consume
Node::observe_capture(EventKind::Wheel, Msg::Scroll)  // consumes before dispatch
```

Observers are active only while their declaring node is mounted, so they are
still encapsulated — but they are the *one* mechanism with global reach, and
`Layer.dismiss` / `Layer.wheel_capture` are sugar over them. Everything that
is a pre-band stage today becomes one of these.

### 7.5 Pointer capture

A press handler calls `cx.fx.capture_pointer(self)`; subsequent `Move`/`Up`
route to that node regardless of position, until release or unmount. That is
the whole drag model — per-node, declared by the component that took the
press. No global grab enum, no derivation order, no central drag or release
match. Host leaves can capture too (terminal selection).

### 7.6 Keyboard and keymaps

The keyboard target is the focused node; propagation is its ancestor chain.
Keybinding resolution walks the same path: the first node with a `KeyScope`
that binds the chord wins, falling back to the root scope. `KeyContext` as a
single global enum disappears — context *is* focus position — and so does any
rank table, because "who gets this key first" is answered by the tree.

Modal keyboard ownership is `Modality::Inert`: nodes outside the layer aren't
in the focused node's path, so they cannot see the key.

---

## 8. Focus

```rust
pub struct FocusSpec { pub focusable: bool, pub scope: bool, pub auto: bool }
```

- **Order** is document order over focusable, non-inert nodes — derived, never
  collected separately.
- **Scope** (`scope: true`) traps Tab cycling inside a subtree; nesting scopes
  to the nearest enclosing one. A `Layer` with `Modality::Inert` implies a
  scope.
- **`auto`** takes focus when its subtree mounts (a menu's list, a prompt's
  input).
- Focus moves emit `Focus(Gained/Lost)` through the normal dispatch, so a kind
  can keep its own state coherent (seeding a tree's selection on focus).
- One `focus: Option<Key>` for the whole app. Dock focus, prompt toolbar focus,
  and popup focus become positions in one ring.

---

## 9. Plugin projection

Plugins do not get `Node<M>`. `WidgetSpec` stays a **stable serializable subset**
that deserializes into the same node tree:

| exposed to plugins | internal only |
|---|---|
| containers, controls, `Layer` with `anchor: Parent \| Point` | `Kind::Host`, `Modality::Exclusive`, `KeyScope`, observers, raw `Sizing::Pct` |
| `on: { activate, select, change, … }` → `WidgetEvent` | arbitrary `M`, `Effect` |

Rationale: chrome's vocabulary must grow freely, and `WidgetSpec` is versioned
public API with a TypeScript declaration. One engine, one node model, two
vocabularies — the internal one is a superset. Plugin listeners map to
messages, so a plugin's handler is the same encapsulated registration a chrome
component uses.

```ts
panel("sessions", Col([
  TextInput({ key: "filter", on: { changed: "filter" } }),
  List({ key: "list", items, on: { activate: "open", select: "preview" } }),
]));
```

---

## 10. Examples

### 10.1 Chrome menu bar + dropdown

```rust
fn menu_bar(m: &MenuModel) -> Node<Action> {
    Row::new().h(1).keymap(KeyScope::MenuBar).children(
        m.menus.iter().enumerate().map(|(i, menu)| {
            Button::label(&menu.title)
                .key(key!("menu", i))
                .on(Click, Action::ToggleMenu(i))
                .on(PointerEnter, Action::HoverMenu(i))   // auto-switch while open
                .child_if(m.open == Some(i), || menu_dropdown(menu, i))
        }),
    )
}

fn menu_dropdown(menu: &Menu, i: usize) -> Node<Action> {
    Layer::new()
        .anchor(Anchor::Parent).place(Place::Below).fit(FLIP | CLAMP)
        .modality(Modality::Inert)
        .dismiss(OUTSIDE_POINTER | ESCAPE)
        .child(
            List::items(menu.items.iter().map(item_row))
                .key(key!("menu", i, "items"))
                .autofocus()
                .on(Activate, |e| Action::RunMenuItem(i, e.index))
                .child_if_open(menu.submenu(), |s| submenu_layer(s)),
        )
}
```

What disappears: the close-guard box (`dismiss`), the dropdown's z number
(stacking context), the menu's rank (tree position + `Modality`), the hover
auto-switch machine's central wiring (an ordinary listener), and the
mnemonic/navigation dispatcher (`List` built-ins + `KeyScope::MenuBar`).
Submenus nest as layers anchored to their parent row — arbitrarily deep, no
new precedence.

### 10.2 Split buffers, tabs, scrollbars, separators

```rust
fn workspace(w: &Window) -> Node<Action> {
    Col::new().children([
        menu_bar(&w.menus).if_(w.menu_visible),
        Row::new().flex(1).children([
            explorer(w).w(w.explorer_width).if_(w.explorer_visible),
            splits(&w.tree, w).flex(1),
            dock(w).w(w.dock_width).if_(w.dock_visible),
        ]),
        status_bar(w).h(1),
        prompt(w).if_(w.prompt.is_some()),
    ])
}

fn splits(t: &SplitTree, w: &Window) -> Node<Action> {
    match t {
        SplitTree::Leaf(id) => Col::new().children([
            tab_strip(*id, w).h(1).overflow(Overflow::Scroll),
            Row::new().flex(1).children([
                Host::new(HostId::Buffer(*id)).flex(1).focusable()
                    .overflow(Overflow::Scroll)
                    .on(PointerDown, Action::PlaceCaret)   // handler captures the pointer
                    .keymap(KeyScope::Buffer),
                vscrollbar(*id, w).w(1),
            ]),
            hscrollbar(*id, w).h(1).if_(w.needs_hscroll(*id)),
        ]),
        SplitTree::Node { dir, a, b, ratio } => container(*dir).children([
            splits(a, w).flex(*ratio),
            Divider::grabbable()
                .key(key!("sep", t.id()))
                .on(PointerDown, Action::BeginResize(t.id())),
            splits(b, w).flex(100 - *ratio),
        ]),
    }
}
```

`Action::BeginResize`'s handler calls `capture_pointer`, so every subsequent
move routes to the separator even across a full-screen terminal — the
btop-resize case, from the primitive rather than from a global grab table.
Wheel over the buffer scrolls it; if it's at its bound the wheel bubbles to
the split container and then dies at the root, which is today's wheel floor,
unstated. Tab-strip horizontal scroll is `Overflow::Scroll` on a `Row`.

### 10.3 Context popup menu

```rust
fn tab_with_menu(t: &Tab, cm: Option<&ContextMenu>) -> Node<Action> {
    Button::label(&t.title)
        .key(key!("tab", t.id))
        .on(Click, Action::FocusTab(t.id))
        .on(SecondaryClick, Action::OpenTabMenu(t.id))
        .child_if_some(cm.filter(|c| c.owner == t.id), |c| {
            Layer::new()
                .anchor(Anchor::Point(c.col, c.row)).place(Place::Below).fit(FLIP | CLAMP)
                .modality(Modality::Inert)
                .dismiss(OUTSIDE_POINTER | ESCAPE)
                .child(List::items(c.items.iter().map(menu_row)).autofocus()
                    .on(Activate, |e| Action::TabMenuItem(t.id, e.index)))
        })
}
```

The menu is a *child of the tab it acts on*, so "which tab does this menu
target" is structural rather than carried in menu state, and outside-dismiss,
keyboard navigation, and modality come from the two declared properties.
Right-click inside an already-open menu is absorbed by `Opaque` with no arm.
All four of today's native context menus are this function with different
item lists.

### 10.4 Command palette / prompt with suggestions and preview

```rust
fn prompt(p: &Prompt) -> Node<Action> {
    Layer::new()
        .anchor(Anchor::Screen(if p.overlay { Align::Center } else { Align::Bottom }))
        .modality(if p.overlay { Modality::Inert } else { Modality::None })
        .scrim(p.overlay.then(Scrim::dim))
        .dismiss(ESCAPE)
        .wheel_capture(!p.overlay)          // bottom dropdown: wheel scrolls the list anywhere
        .keymap(if p.has_options { KeyScope::SearchPrompt } else { KeyScope::Prompt })
        .child(Col::new().focus_scope().children([
            Text::input(&p.input).key("prompt:input").autofocus()
                .on(Changed, Action::PromptInput)
                .on(Submit, Action::PromptConfirm),
            toolbar(p).if_(p.has_options),                       // Toggles; Tab reaches them
            Row::new().flex(1).children([
                List::items(p.suggestions.iter().map(row))
                    .key("prompt:suggestions").flex(1)
                    .overflow(Overflow::Scroll)
                    .on(Select, |e| Action::PromptSelect(e.index))
                    .on(Activate, |e| Action::PromptConfirmAt(e.index)),
                preview(p).flex(1).if_(p.overlay),
            ]),
        ]))
}
```

`focus_scope` is the overlay toolbar ring — Tab cycles input → toggles → list
and stops there. The suggestion list's scrollbar is part of `Overflow::Scroll`,
so press and drag are one thing, handled by pointer capture inside the scroll
container. `wheel_capture` states the position-blind intent explicitly instead
of a full-frame box at a hand-picked z. The overlay's click-absorption is the
scrim.

### 10.5 Transient popup over a split

```rust
fn hover_popup(h: &HoverPopup) -> Node<Action> {
    Layer::new()
        .anchor(Anchor::Point(h.col, h.row)).place(Place::Below).fit(FLIP | CLAMP)
        .dismiss(ANY_KEY | OUTSIDE_POINTER)          // transient: an observer under the hood
        .child(Text::markdown(&h.body)
            .overflow(Overflow::Scroll)
            .selectable()                            // drag-select via pointer capture
            .max_h(20))
}
```

Non-modal (no `Modality`), so the buffer underneath keeps the keyboard, but
`ANY_KEY` dismissal still fires because observers run before dispatch — which
is exactly the "must see the key even when a higher modal consumes it"
requirement, expressed as a property of the popup instead of a stage in a
central pipeline.

### 10.6 Modal dialog

```rust
fn trust_dialog(d: &TrustPrompt) -> Node<Action> {
    Layer::new()
        .anchor(Anchor::Screen(Align::Center)).modality(Modality::Exclusive)
        .scrim(Some(Scrim::dim)).dismiss(if d.cancellable { ESCAPE } else { Dismiss::NONE })
        .child(Col::new().focus_scope().pad(1).border().children([
            Text::markdown(&d.body).overflow(Overflow::Scroll).flex(1),
            radio_group(&d.options).key("trust:choice").autofocus(),
            Row::new().gap(2).children([
                Button::label("OK").on(Click, Action::ConfirmTrust),
                Button::label(d.secondary_label()).on(Click, Action::CancelTrust),
            ]),
        ]))
}
```

`Modality::Exclusive` is the whole of today's mouse capture, keyboard capture,
PTY block, hover suppression, and cursor suppression. The two-step
select-then-confirm is just a radio group plus a button — no bespoke interior
and no whole-channel capture handler.

---

## 11. Mapping from today

| today | here |
|---|---|
| full-frame guard boxes (close/dismiss/blur/clear) | `Layer.dismiss` |
| `layer_rank` table + `owns_keyboard` | tree position + `Modality` + focus |
| per-box hand-assigned `z` | stacking contexts, tree order |
| `capture_mouse` | `Modality::Exclusive` |
| `blocks_terminal_input`, `modal_overlay_active`, cursor suppression | derived from `inert` / `HostLeaf::raw_input` |
| keyboard pre-band stages | `Node::observe` |
| `PointerGrab` enum + grab-keyed drag/release routing | `Effect::CapturePointer` |
| `KeyContext` enum | `KeyScope` on nodes + focus path |
| `Disposition::{Consumed, PassAfter, Pass}` | `Flow::{Stop, Continue}` + `prevent_default` |
| `pointer_opaque` opt-in | `PointerMode::Opaque` default |
| `HitArea` / `row_target` / `owner()` | `target` vs `currentTarget` in `Cx` |
| `PanelPopup`, `screen_space` boxes, paint-recorded rects | `Layer` + `LayoutOut` |
| chrome layout cache (written during paint) | `LayoutOut` (written during layout) |
| painters writing cells into a `Frame` | `emit` → `LayoutSpec` → backend |
| `Scene`'s hand-written `*_view` methods | `LayoutSpec` (+ the tree for semantics) |
| per-widget scroll windowing repeated per kind | the emit walk's scroll rule |
| `collect_tabbable` + derived ring | one derived ring |
| residual per-kind matches (Text single-vs-multi-line, hit synthesis) | `Caps` — extending the base's `picker_nav` / `arrows_advance_focus` / `activates_on_picker_enter` pattern |

---

## 12. Prerequisites and risks

1. **Compositor first.** Layers over host content need paint-time compositing
   with clipping and z — the same gap that makes buffer-mounted panels drop
   their overlay/popup channels today. Nothing else in this spec is reachable
   without it.
2. **Constraint layout must land** before chrome can be expressed (§3); chrome
   is the surface that needs it most and has the most existing behavior to
   preserve.
3. **Geometry inversion is the risky refactor**, not the tree: moving from
   paint-recorded to layout-recorded rects touches every hit-test and the
   Scene projection, and it is where visual/behavioral regressions will come
   from. Keep the debug parity oracle (event-time geometry == paint walk)
   during the transition.
4. **Plugin wire freeze.** Decide the exposed subset (§9) before chrome starts
   using the internal vocabulary, or chrome's needs will leak into versioned
   plugin API.
5. **Cost per frame.** The tree is rebuilt per frame; the current per-panel
   "re-render on any state change" is too coarse for full chrome (a hover
   crossing would rebuild everything). Build must be cheap and allocation-lean
   — arena-allocated nodes, reused `items` buffers, `State` lookups by `Key`
   rather than subtree rebuilds. Emit is O(visible), which is what makes the
   per-frame rebuild affordable; measure it before adding damage tracking.
6. **Keep the IR dumb.** `LayoutSpec` items must carry no layout semantics —
   no flex, no auto-sizing, no relative positioning. The moment a backend has
   to compute geometry, there are two layout systems. Items are absolute,
   pre-clipped, and ordered, or the intermediate has failed at its job.
7. **Adoption order.** Cheapest surfaces first — context menus, menu
   dropdowns, info popups, file browser — then the prompt; splits and
   terminals stay `Host` leaves permanently; keybinding editor and calibration
   wizard last. Each ported surface should delete its guard boxes, its rank
   entry, and its pre-band stage, or it hasn't really moved.
