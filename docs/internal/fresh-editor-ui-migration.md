# Migrating the Fresh editor UI onto `fresh-ui`

> _AI-generated. **Current-state survey is IMPLEMENTED; the design and plan are
> PLANNED.** This is the editor-side companion to the two library documents:
> [`widget-library-design.md`](widget-library-design.md) (the architecture
> authority for `fresh-ui`) and
> [`widget-library-implementation-plan.md`](widget-library-implementation-plan.md)
> (which builds the library in Part 1 and sketches the migration as Part 2,
> M0–M9). Those docs describe the target and the waves in the abstract. This one
> grounds them in the editor as it exists today — what each surface actually is,
> where its state and geometry live, how input reaches it — and turns the
> abstract waves into concrete, file-level moves. Where this doc and the two
> library docs disagree about the target, they win; where they disagree about
> the current editor, the source wins._

---

## 0. Situation

`fresh-ui` is **built and standalone** (PR #3024 plus the R1–R11 and Part 1c–1e
follow-ups). It is a retained, reconciling UI tree — immutable descriptions →
persistent elements matched by `(type, key)` → render objects holding geometry
and focus — that emits a backend-independent `LayoutSpec` display list. Its only
runtime dependency is `unicode-width`. It has the real `RenderObject` layer, a
retained focus tree, ambients, `Tasks`, layers with modality/dismissal, pointer
capture, and the full widget set (`Button`, `Toggle`, `TextField`, `List`
eager+windowed, `Tree`, `Dropdown`, `RadioGroup`, `Number`, `DualList`). The
demo under `crates/fresh-ui/tests/support/demo/` and `examples/interactive.rs`
exercise every capability against a terminal backend — with one gap that matters
here: no backend has ever drawn real `Draw::Host` content (both folds stub it as
a `▒` fill), and that is exactly the capability this migration leans on (§4.4).

**It is not yet wired into the editor.** No file under `crates/fresh-editor`,
`crates/fresh-core`, or `crates/fresh-plugin-runtime` references `fresh_ui`.
Part 2 has not started. So this is a genuine greenfield adoption, not a
course-correction — which is the good case: the library was finished and frozen
before any surface depended on it.

The editor's UI, meanwhile, is **already halfway to this model on its own
terms** and stopped at exactly the wall `fresh-ui` was built to get past. The
`ChromeComponent` registry, the `Scene` projection, the shared
`LayoutBox`/`hit_stack` primitive, and the derived `overlay_stack()` are a
proto-retained-widget tree. What they lack — real containment, one precedence
order, per-node pointer capture, one focus ring — is precisely what `fresh-ui`
supplies. The migration is less a rewrite than **finishing a refactor the
codebase already committed to** (the forward arc named in
[`chrome-event-model-plan.md`](chrome-event-model-plan.md) and
[`widget-framework-v2-review.md`](widget-framework-v2-review.md)).

---

## 1. The one hard constraint: the keep/migrate boundary

The optimized, file-backed text buffers and the text-rendering pipeline **do not
migrate**. They keep their existing logic and are reached from `fresh-ui` through
a `Host` leaf. The boundary is not fuzzy — it is a single function signature.

### The line is `SplitRenderer::render_content`

`view/ui/split_rendering/mod.rs` exposes (all associated functions, no `&self`):

```rust
pub fn render_content(buf: &mut ratatui::buffer::Buffer, area: Rect, …)
    -> /* per-leaf layout caches: split areas, tab layouts, view_line_mappings,
         scrollbar/separator areas, close/maximize buttons */
pub fn compute_content_layout(…)  // layout-only: the same caches, WITHOUT painting
pub fn render_phantom_leaf(…)     // off-tree previews (palette preview, web slices)
```

`render_content` already paints into an **arbitrary `Buffer` at an arbitrary
`Rect`** ([`rendering-and-layout.md`](rendering-and-layout.md) calls it
composable into any buffer — offscreen previews, tests, and the web bridge).
`compute_content_layout` matters just as much for this plan: it yields the
geometry (`view_line_mappings`, and from them the caret's screen position)
*before* anything paints — the tool for same-frame caret-anchored layers
(§4.4). Everything these touch is *keep*; everything that carves the rect they
are handed and reads the caches they return is *migrate*.

One honest caveat on "untouched": `render_content`'s unit is the **whole split
tree** — separators, tab bars, scrollbars, scroll-sync, composite views in one
call. The target frame (§4.2, M9) wants per-leaf `Host` nodes with `fresh-ui`
tabs and dividers, which requires either keeping the whole grid as one `Host`
leaf or decomposing the orchestration layer per-leaf. That is an open scoping
decision (§6.2), not a settled detail.

| KEEP — behind a `Host` leaf, logic untouched | MIGRATE — onto `fresh-ui` |
|---|---|
| `view/ui/split_rendering/**` (token IR → `ViewLine`; `render_line/**` and its `CellPass` per-char state machine; gutter, folding, conceal, virtual-text, soft-break, scrollbar glyphs, composite view) | `app/chrome/**` (the whole `ChromeComponent` registry) |
| `view/line_wrap_cache.rs`, `view/wrap_index.rs`, `view/wrap_machine.rs` (tier-1/tier-2 wrap caches) | `view/scene.rs` (the `Scene` projections become the tree's data model) |
| `view/viewport.rs`, `view/folding.rs`, `view/conceal.rs`, `view/soft_break.rs`, `view/virtual_text.rs`, `view/margin.rs`, `view/composite_view.rs` | `view/ui/{menu,tabs,status_bar,suggestions,scrollbar,scroll_panel,file_explorer,file_browser,focus,layout}.rs` |
| `EditorState`, `Buffer`, the piece tree, markers, undo (see [`text-model.md`](text-model.md), [`buffers-splits-undo.md`](buffers-splits-undo.md)) | `view/popup.rs` + `view/popup/**`; `view/settings/**`; `view/controls/**`; `view/keybinding_editor.rs`; `view/workspace_trust_dialog.rs` |
| `Viewport` scroll math, `view_line_mappings` (mouse→byte hit-test slices), the per-cell theme-provenance map | The frame layout, dock/sidebar carve, and the modal z-band in `app/render.rs` |

**What crosses the seam.** The text view is not a black box; three of its outputs
are read by chrome and must keep flowing:

1. `view_line_mappings` — per-visual-row `ViewLineMapping` slices used for O(1)
   screen→byte hit-testing and click-to-cursor. Published by the backend fold,
   keyed by `HostId` (§4.4) — or derived pre-paint via `compute_content_layout`.
2. The per-cell theme-provenance map (`CellThemeInfo`) — read by the Scene/web
   projection and the theme inspector.
3. The **caret**. The hardware cursor is committed at end-of-frame and arbitrated
   against late overlays (`cursor_suppressed_by_late_overlay`). `fresh-ui` has
   `LayoutSpec.cursor` (a `TextField` sets it) — but that field is fixed during
   the library's paint pass, while `render_content` computes the buffer caret at
   *fold* time, after the spec is finalized. The frame therefore needs one "who
   owns the caret" arbitration that merges the host caret outside the library's
   cursor mechanism, or derives it early via `compute_content_layout` (§4.4).

**Ordering the seam must preserve.** Today geometry is "painted, then projected":
`render_content` writes the layout caches *during* paint, and chrome/Scene read
them *after*. `fresh-ui` inverts this for its own tree — layout is a distinct pass
before paint, geometry comes from the layout pass, and **paint never touches
cells** (`render/spec.rs` opens with exactly that sentence): a render object
emits display-list items, and per-cell content has one route across the seam,
`Draw::Host(HostId)`. So the two orderings meet **in the backend fold**, not in
the leaf: the leaf's `layout` reserves the rect and its `paint` emits
`Draw::Host(id)`; the editor-driven fold, walking `LayoutSpec.items` in paint
order after `ui.frame()` returns, maps that item back to `render_content` and
captures the caches editor-side, keyed by `HostId`. Event handlers read those
caches on the next event; same-frame consumers (caret-anchored layers) use
`compute_content_layout` at build time instead. §4.4 spells out the mechanism
and its consequences.

---

## 2. The current UI, as it actually is

This section is the survey the migration needs. It samples the surfaces the
request named — editor/window, menus, file explorer, the widget system, settings,
splits, prompts, the plugin widget API — plus the mouse/keyboard event dispatch
that ties them together.

### 2.1 The frame and the `Editor` object

`Editor` (`app/mod.rs`) is the central object but **not** a buffer owner. Buffers
and splits live on `Window` (`app/window/**`); `Editor` holds a `windows` map and
an `active_window`, and derives the active buffer/splits/explorer/prompt/popups
through accessors (`active_window()`, `active_state()`, `split_manager()`,
`active_chrome()`, `active_layout()`). It also holds the cross-cutting services:
config, theme (`Arc<RwLock<Theme>>`), registries, keybindings, clipboard, the
plugin manager and async bridge, the dock and floating-panel state, and the
widget registry.

`Editor::render(&mut self, frame: &mut Frame)` (`app/render.rs`) is **immediate
mode**: the whole screen is re-derived every frame; the runtime loop decides
*when* to call it. The ordered flow is, in essence:

1. drain pre-layout plugin commands (the one place inside draw that takes the
   plugin lock);
2. `compute_dock_split` → carve the left dock column;
3. animation snapshot; reset the cell-theme map; scroll-sync; request semantic
   ranges for visible splits;
4. a ratatui `Layout` splits the chrome area into
   `[menu_bar, main_content, status_bar, search_options, prompt_line]`;
5. carve the file-explorer sidebar out of `main_content`; render it;
6. **`SplitRenderer::render_content(frame.buffer_mut(), editor_content_area, …)`**
   inside a single `WindowBuffers::with_all_mut` split-borrow — this is the text
   pipeline, and it returns the per-leaf layout caches
   (`split_areas`, `tab_layouts`, `view_line_mappings`, scrollbar/separator
   areas) onto `active_layout_mut()`;
7. paint the chrome on top, in a fixed order: status bar, search options, prompt
   line, prompt/buffer/global popups, menu bar (last), context menus, tab-drag,
   software cursor, deferred hardware-cursor commit, frame-buffer animations;
8. `render_panels_and_modals` — dock, full-screen modals, floating panel, trust
   modal (the topmost z-band);
9. `convert_buffer_colors` (256/16 fallback) over the finished buffer;
   `bump_ui_gen()`.

The whole of steps 2, 4, 5, 7, 8 is chrome composition — the part that migrates.
Step 6 is the `Host` leaf.

### 2.2 Chrome: a registry that is a proto-widget-tree

There **is** a central abstraction, and it is already component-shaped.

- `trait ChromeComponent: Sync` (`app/chrome/mod.rs`) — one ZST implementor per
  surface. Its methods are the parallel dispatch interface: `collect` (contribute
  geometry boxes), `hover`/`on_hover_change`, `on_pointer`, `on_wheel`/`on_hwheel`,
  `capture_mouse`, `on_key` (pre-band grab), `on_layer_key` (the keyboard walk),
  and `layers` (precedence contribution).
- `components() -> &'static [&dyn ChromeComponent]` — **the** registry, 17
  entries: Settings, KeybindingEditor, CalibrationWizard, WorkspaceTrust,
  ThemeInfo, ContextMenu, Prompt, Popups, FileBrowser, FloatingModal, Dock,
  Splits, Menu, FileExplorer, StatusBar, SearchOptions, Base.

Notably, **the text view is itself a component** (`Splits`, contributing a
`chrome:editor` box) and the keyboard/pointer floor is a component (`Base`,
z-0). The editor content is the lowest-precedence participant in the same tree,
not a privileged root — which is exactly the `fresh-ui` stance (no privileged
internal surface) and makes the mapping natural.

Per-surface, the pattern is uniform: `collect` reads a geometry rect (from a
paint cache or a live derivation) and pushes a kind-tagged `LayoutBox` stamped
with the component's registry index; the handlers delegate to `impl Editor`
methods that hold the real behavior and mutate `Editor`/`Window` fields. The
components are stateless; **all state is on `Editor` or `Window`.**

### 2.3 Two decoupled precedence systems

This is the single most important structural fact for the migration, and the
thing `fresh-ui` collapses. Pointer stacking and keyboard/modal precedence are
**two separate orderings that deliberately disagree**:

- **Pointer z** — each `LayoutBox` carries a `z` on an ×10 band scheme (context
  menu ~180 … tabs 60 … scrollbars 50 … status 40 … editor 10 … base 0). The
  pointer walk is `hit_stack` (effective-z desc, then depth desc, then document
  order), with registry order as the intra-band tiebreak.
- **Keyboard / modal rank** — a *separate* hand-tuned constant table,
  `chrome::layer_rank`: `SETTINGS=900 … MENU=860 PROMPT=850 POPUP=840
  CONTEXT_MENU=830 FLOATING_MODAL=820 DOCK=810 EDITOR_BASE=0`. Each component
  contributes `(rank, Layer)` via `layers()`; `Editor::overlay_stack()`
  concatenates all contributions (plus a hardcoded EventDebug head at 1000) and
  stable-sorts rank-descending into **the** single ordered `OwnedLayer` list.

`overlay_stack()` is consumed by the keyboard walk, the mouse-capture band, the
PTY-input gate (`presents_blocking_overlay`), `modal_overlay_active`,
`popup_blocked_by_higher_modal`, and `get_key_context`. The two orderings
intentionally diverge (e.g. a menu's *keyboard* layer outranks the prompt, but
its *boxes* sit in a lower pointer band; context menus rank below popups for
keyboard but their boxes sit at the top pointer band), and the relationships are
pinned by tests. Precedence is therefore **data spread across 17 `layers()`
impls and two constant tables**, not a property of a tree.

### 2.4 Geometry: two sources, one deliberate seam

Chrome geometry comes from two places, and a migration underway (slice 7 of the
chrome-event-model plan) is moving surfaces from the first to the second:

- **Paint-recorded caches** — `ChromeLayout` (editor-global) and
  `WindowLayoutCache` (per-window splits/tabs/explorer). `render_content` and the
  popup painters write rects here *during* paint; `collect` reads them at event
  time.
- **Live-derived** — `status_bar_layout_now()`, `menu_layout_now()` recompute
  geometry from state at event time, and the paint pass debug-asserts paint ==
  derivation. Their retired cache fields are gone. This class is itself a
  waypoint, not a destination: a live derivation is still a *second* spelling of
  the paint walk's arithmetic, kept honest by an assertion that release builds
  compile out. A migrated surface has neither — the tree lays it out once and
  everyone reads the result. `search_options_layout_now()` was the first to go
  that way (§6.2); `Editor::search_option_spans_now()` reads its spans back off
  the laid-out tree instead.

A subset **stays paint-recorded by explicit ruling**: `popup_areas`,
`global_popup_areas`, the prompt suggestions/toolbar/preview rects, the file
browser layout, the workspace-trust dialog rect, and the floating-panel paint
fields. The reason is real and constrains the migration: these anchor to
**paint-produced text layout** — the cursor's screen position, the wrap maps. Any
`fresh-ui` tree that positions a completion popup at the caret must therefore be
able to read the caret's post-layout screen position out of the `Host` leaf. This
is the §1 "what crosses the seam" requirement seen from the other side.

### 2.5 The `Scene` projection

`view/scene.rs` is **not** a retained scene graph — it is a set of
`Serialize`-deriving **semantic projections** computed once per frame from
`Editor` state plus the last-frame geometry caches: `MenuView`, `TabBarView`,
`StatusView`, `PaletteView`, `ScenePopup`, `FileExplorerView`,
`FileBrowserView`, `TrustDialogView`, `WidgetSurfaceView`, `ContextMenuView`,
`AuxModalView`, `KeybindingEditorView`. It is the single source of truth for
*what the chrome is* (which items/tabs/rows exist, enabled/checked, and their
rects), and it is consumed by both the TUI painter and the web frontend. It
deliberately **excludes buffer text** — the web bridge slices *rendered cells*
out of the framebuffer for the buffer and preview panes (the `PaletteView`
preview rect is exactly such a slice).

This is a gift to the migration: `Scene` is already "the description of the
chrome, minus geometry, minus text." It becomes the props the `fresh-ui`
components read. It is also the parity oracle — `scene_parity.rs` is the check
that the web projection has not diverged, and it must keep passing through every
wave.

### 2.6 Event dispatch — keyboard

Entry `app/input.rs`, `handle_key_press` → `handle_key`. Ordered pre-band stages,
then the derived walk:

1. `bump_ui_gen()` (invalidate per-event memos);
2. event-debug intercept;
3. `dispatch_terminal_input` (terminal mode, `terminalBypass`, scrollback) —
   short-circuits if `presents_blocking_overlay()`;
4. `try_resolve_next_key_callback` (plugin `getNextKey()`);
5. **pre-band grabs**: `for c in components() { c.on_key(...) }` — by ruling only
   two members participate (ThemeInfo as observer, ContextMenu as a
   custom-dispatch modal);
6. transient-popup dismissal observer;
7. **the walk**: `dispatch_layer_keyboard` walks `overlay_stack()` top-down,
   offering the key to each layer's owner via `on_layer_key`; first `Some` stops.
   `Base` always answers, so the walk always terminates.

The pipeline **tail** — mode bindings → composite router → chord/keybinding
resolution — lives in `Base`'s handler, reaching the keybinding resolver, which
resolves key→`Action` against the `KeyContext` from `get_key_context()`.
`KeyContext` (an enum in `fresh-core/action.rs`) is the current stand-in for a
focus ring: it is *derived* from the topmost keyboard-owning layer, not stored.

Note a deliberate asymmetry: the keyboard side does **not** get the
one-tree-per-event treatment the pointer side does, because key handlers
"mutate then decline" (a popup rung processes `ClosePopup` and falls through),
which forces per-handler `get_key_context()` re-derivation.

### 2.7 Event dispatch — pointer

Entry `app/mouse_input.rs`, `handle_mouse` → `handle_mouse_impl`:

1. **modal capture band** — walk `overlay_stack()` in rank order; the first
   component whose modal is up claims the *whole* mouse channel via
   `capture_mouse`. (This replaced a deleted `dispatch_modal_mouse` ladder.)
2. pre-walk observers (LSP-rename cancel, GPM cursor);
3. **terminal forwarding gate** — suppressed when a `pointer_grab` is active, a
   context menu is open, or an opaque chrome box covers the point;
4. **one chrome tree per event** — `chrome_tree(self)` collects every component's
   boxes, validated-memoized on `(ui_gen, overlay_stack)` with a debug oracle
   that rebuilds and compares;
5. dispatch: `dispatch_pointer` for presses, `dispatch_wheel` for scroll. Both
   walk the `hit_stack`; `dispatch_pointer` dedups on `(owner, kind)` and honors
   `Disposition::{Consumed, PassAfter, Pass}` (a declining *opaque* box absorbs
   the event); `dispatch_wheel` has **no** opacity gate and **no** dedup, so a
   declining surface lets the wheel keep falling for scroll-chaining.

Fine click geometry is pure and `Editor`-free (`app/click_geometry.rs`):
`screen_to_buffer_position*` maps (col,row) + content rect + gutter +
`ViewLineMapping` → buffer byte + virtual-space overshoot.

### 2.8 Drags, hover, focus — the three ad-hoc clusters

These are the three places the flat tree cannot express what it needs, and each
is a hand-rolled substitute for a `fresh-ui` primitive:

- **Drags → `PointerGrab`.** `pointer_grab(ed)` is a hand-ordered match over ~13
  drag flags scattered across `Editor` and `mouse_state` (`dock_resizing`,
  `widget_text_drag`, `dragging_scrollbar`, `dragging_horizontal_scrollbar`,
  `selecting_in_popup`, `dragging_prompt_scrollbar`, `dragging_popup_scrollbar`,
  `dragging_separator`, `dragging_file_explorer`, `terminal_drag_pending`,
  `dragging_text_selection`, `dragging_tab`, …). The grab owns the pointer from
  press to release regardless of what is under it. This is exactly what per-node
  `cx.capture_pointer()` replaces — one slot, owned by the node that started the
  drag.
- **Modal capture → `capture_mouse`.** Full-screen modals (Settings, keybinding
  editor, calibration wizard, workspace trust) and the floating modal contribute
  **no boxes**; they swallow the entire mouse channel before the walk, and each
  has a bespoke `handle_*_mouse`. This is what `Modality::Exclusive` + a
  `FocusScope` replaces.
- **Hover** is two disjoint systems: a chrome hover-target walk
  (`update_hover_target`, memoized, offering enter/leave transitions to every
  component) and content trackers kept **outside** the walk on purpose — the LSP
  hover state machine and terminal-link hover, because their debounced
  request/keep-alive state cannot be expressed as an enter/leave diff. In
  `fresh-ui`, hover is framework state on the render object; a component
  *mirrors* it via `on_enter`/`on_leave` (the G1 finding in the implementation
  plan). The content trackers stay as they are — they live behind the `Host`
  leaf.

**Focus** today is not one thing: keyboard focus is the derived `KeyContext`;
"which split/buffer is active" is `Window` state guarded by `set_active_buffer`
and `focus_split` (with a pane-buffer invariant that once caused a panic);
per-popup `focused` flags; per-panel `widget_registry.focus_key`; and the
overlay-toolbar focus ring derived from layout boxes. `fresh-ui` unifies all of
these into one focus tree with scopes and a traversal policy.

### 2.9 The command → action → event pipeline (unchanged by this work)

Three deliberately separate vocabularies, and **all three survive the
migration**:

| Layer | Type | Role |
|---|---|---|
| Command | `Command` (`fresh-core/command.rs`) | user-facing, localized, context-filtered palette entry |
| Action | `Action` enum, ~230 variants (`fresh-core/action.rs`) | the rebinding & serialization currency; executed by `handle_action` |
| Event | `Event::{Insert,Delete,MoveCursor,BulkEdit,…}` | the buffer-mutation, undo, and plugin-hook unit |

The separation exists for rebindability (Actions round-trip to strings; Events
are position-specific and would not replay), undo/hooks (Events are the
transaction record), and layout-independence (`MoveUp` resolves to a concrete
`Event::MoveCursor` late). `fresh-ui` handlers return **messages** — but the
message type cannot simply *be* `Action`: `Action` deliberately has no
positional variants (no click-at-byte, no drag-to-offset, no
tab-select-(leaf, index)) precisely because it is the serializable rebinding
currency. The tree's message type is therefore a wrapper —
`enum UiMsg { Action(Action), /* UI-geometry messages */ }` — with the
UI-geometry half consumed by `update` and never entering the keybinding
namespace. (The library's own demo makes the same choice: its message type is a
bespoke `Msg`, not a reused action enum.) The Shortcuts → Intents → Actions
chain sits *in front of* the pipeline as before: a key on the focus chain
resolves to an `Intent`, an `Intent` to an `Action` at the focused node, and
from there the existing `handle_action` pipeline is unchanged. This is the
single most important "keep" on the input side. One more keep: fresh-ui's
static `Intent`/`Shortcut` tables cannot absorb the user-rebindable, chorded,
multi-keymap resolver — the realistic shape is that the resolver survives as
the root fallback key handler (today's `Base` tail), with per-node shortcuts
reserved for widget-local keys.

### 2.10 The plugin widget system

Plugins describe UI as a data tree and the host owns everything else. The wire
type is `WidgetSpec` (`fresh-core/api.rs`) — a serde-tagged, `#[ts(export)]`
**closed enum**, 19 kinds: containers (`Row`/`Col`/`LabeledSection`/`Overlay`/
`Component`/`Popup`), structural (`Spacer`/`Divider`/`HintBar`/`Raw`), controls
(`Toggle`/`Button`/`Number`/`Dropdown`/`DualList`), data views
(`List`/`Tree`/`Text`), and `WindowEmbed` — a rect reserved inside a plugin
panel for the native per-window render path, i.e. host content *inside*
plugin-described UI (it matters for M6; see §4.6). Plugins build it with `plugins/lib/widgets.ts` and call
`panel.set(spec)`, which issues `PluginCommand::MountWidgetPanel` /
`UpdateWidgetPanel`.

Host-side (`widgets/**`, `app/widget_runtime.rs`): one `match` on kind
(`kinds/mod.rs::behavior`) dispatches to a `WidgetImpl` per kind
(`collect`/`box_meta`/`on_key`/`on_pointer`/…). The central rule is
**spec/instance separation**: spec values are initial-only; after first render
host-owned `WidgetInstanceState` (list scroll, selection, tree expansion, text
edit state) is authoritative, keyed by the widget's stable string `key` in a
`HashMap`. Identity/diffing is manual string-key matching carried forward each
render; a `WidgetMutation` fast path (`SetValue`, `SetItems`, `SetExpandedKeys`,
`AppendTreeNodes`, …) mutates in place to dodge re-transmitting a large tree
(the `js_to_json` walk of a 5000-node tree blocks the JS thread ~1s). Events flow
back through the `widget_event` hook, delivered only to the owning plugin, with a
deliberate **one-frame lag** (PluginCommands drain on the next frame).

This is a hand-rolled, string-keyed, side-table version of exactly what
`fresh-ui` is: a retained tree with `(type, key)` identity, element-owned state,
and a reconciler. The `WidgetInstanceState` map is the "side-table problem" the
design doc's Appendix A calls out by name.

### 2.11 Settings and the controls library

Settings is **mid-unification** and instructive. Schema drives it: schemars runs
offline over the config struct, the committed `config-schema.json` is parsed into
a `SettingCategory` tree, and `build_pages` turns each entry into a
`SettingControl` (10 variants: Toggle, Number, Dropdown, Text, TextList, DualList,
Map, ObjectArray, Json, Complex). `x-` schema extensions carry UI hints
(`x-enum-from: "$themes"` pulls live theme options, etc.).

The important part is what has and has not been unified:

- **Rendering is already on the widget framework.** There is no `Control` trait
  and no per-control paint code — `view/settings/widget_map.rs` projects the full
  control state into a `WidgetSpec` every frame and renders through
  `widgets::render_spec`. The old `view/controls/*/render.rs` paint modules are
  **deleted**.
- **State, input, layout, and theming remain bespoke.** Each control keeps a
  `*State`/`*Colors`/`*Layout`/`*Event` module with hand-written
  `handle_key`/`handle_mouse`, and `view/settings/input.rs` routes keys through
  per-control editing handlers. So the same widgets have **two state stores**
  (the `SettingControl` states *and* a bridge `HashMap<key, WidgetInstanceState>`)
  and `widget_map.rs` re-seeds one from the other every frame.
- **The keybinding editor is a third, entirely separate hand-rolled modal** —
  table + search + edit dialog — that uses none of `view/controls`,
  `SettingControl`, or `WidgetSpec`.

The duplication a `fresh-ui` migration removes here is concentrated in the
control *state/input/layout/theming* layer, not rendering — and it folds three
systems (controls, plugin widgets, keybinding editor) into one.

---

## 3. Why migrate — the shared root cause

Every recurring UI bug class in the survey traces to one root, stated in
[`widget-framework-v2-review.md`](widget-framework-v2-review.md): **the tree is
flat.** No component sets `LayoutBox.parent`; `focusable`/`focus_trap`/`scroll`
are reserved and unset at chrome level. Containment is faked, precedence is
tabulated, drags are a flag ladder, modals bypass the walk. Concretely:

1. **~10 full-frame "guard" boxes** simulate "outside my rect" containment —
   `menu_close_guard`, `context_menu_close_guard`, `dock_blur`, `transient_guard`,
   `popup_guard`, `clear_explorer_menu`, `tab_menu_clear_guard`, and the prompt's
   **five** per-gesture full-frame boxes. Each is a real parent/clip/modal node's
   job, done by hand.
2. **Two precedence orderings** (pointer z-bands + `layer_rank`) that a real tree
   expresses once as tree order + stacking contexts + `Modality`.
3. **The `PointerGrab` flag ladder** — ~13 drag flags — that one per-node pointer
   capture replaces.
4. **`capture_mouse` modals** that bypass the walk, replaced by a focus-trap node
   + scrim.
5. **String-keyed side tables** (`WidgetInstanceState`, the Settings bridge map)
   that element identity replaces.
6. **Two/three parallel control vocabularies** (chrome, plugin widgets, settings
   controls) that one widget set replaces.

The design doc's Appendix A ("What this replaces in Fresh today") is the full
mapping; the survey above is where each row of it actually lives.

---

## 4. The target design — the whole UI as one `fresh-ui` tree

The end state is one `fresh-ui` description tree, rebuilt each frame from `Editor`
state, with the buffer/terminal panes as `Host` leaves. `Editor::render` stops
being an immediate-mode painter and becomes: build the tree, hand it to
`ui.frame(build(editor), size)`, fold the returned `LayoutSpec` into the ratatui
`Buffer`. Input becomes: translate the terminal event to `fresh_ui::Input`,
`ui.dispatch` it, apply the returned messages — `Action`s plus UI-geometry
messages (§2.9) — through the existing pipeline.

### 4.1 The root tree

Sketch (message type `UiMsg`, the `Action` wrapper from §2.9), mirroring the
demo's shape and the design doc's §15 examples:

```rust
fn build(ed: &Editor) -> Node<UiMsg> {
    provide(&THEME, ed.theme_snapshot(),                     // §4.5
      col().children([
        menu_bar(ed).if_(ed.menu_bar_visible),               // M3
        row().flex(1).children([
            dock_column(ed).w(Cells(ed.dock_width)).if_(ed.dock.visible),  // M9 carve, M6 content
            file_explorer(ed).w(Cells(ed.explorer_width)).if_(ed.explorer_open), // M9
            split_grid(&ed.split_tree()).flex(1),            // M9 — Host leaves
        ]),
        search_options(ed).if_(ed.search_active),            // M1
        status_bar(ed).h(Cells(1)),                          // M1
        prompt_line(ed).if_(ed.prompt.is_some()),            // M5 (bottom mode)
      ])
      // overlays are children of the node they belong to, not a global z-stack:
      .child_if(ed.any_context_menu(), || context_menu(ed))  // M2
      .child_if(ed.palette_overlay(),  || palette(ed))       // M5
      .child_if(ed.any_modal(),        || modal(ed)))        // M7
}
```

The two precedence tables (§2.3) **do not survive**. Precedence is tree order plus
stacking contexts plus `Modality`; a submenu nests as a further `Layer` anchored
to its row; a modal declares `Modality::Exclusive` and everything else falls out.

### 4.2 Per-surface mapping

Each surface maps onto a small, already-built combination of primitives and
widgets. The `Scene` projection (§2.5) is the props each reads.

| Surface | `fresh-ui` expression | Notes |
|---|---|---|
| **Menu bar + dropdowns** (`MenuView`) | `row()` of `Dropdown`s; submenus are nested `Layer`s anchored to their row, `Modality::Inert`, `dismiss(OUTSIDE_POINTER∣ESCAPE)`. Mnemonics are `Intent`s on the root focusable (as in the demo). | Replaces the close-guard box, the dropdown z-number, the rank entry, and the hover auto-switch machine. Hover auto-switch is `on_enter` firing a `Toggle` message while a menu is open. |
| **Context menus** (`ContextMenuView`) | `Layer{ anchor: Point(click), place: Below, fit: FLIP∣CLAMP, modality: Inert, dismiss: OUTSIDE_POINTER∣ESCAPE }` wrapping a `List::keyed(...).autofocus()`. The menu is a child of the node it acts on, so its target is tree position, not stored state. | The demo's `context_menu` is this verbatim. Replaces the four `Window` context-menu structs' *highlight* (element state), the close-guard box, and the pre-band keyboard grab. |
| **Prompt / command palette** (`PaletteView`) | `Layer` (Center overlay or Bottom line) → `FocusScope(col([ TextField, toolbar?, row([ List::keyed(results).selected_id(...), preview? ]) ]))`. Re-key on `prompt_type` to reset editing state. | Query is *controlled* (committed to `prompt_histories`); caret/selection/scroll are element state. Selection stores the result **id**, not an index. Replaces the overlay toolbar ring, the click scrim, the position-blind wheel box, the `SearchPrompt`/`Prompt` context switch, and the manual-scroll latch. |
| **Info/hover/signature popups** (`ScenePopup`) | `Layer{ anchor: Point(caret_screen_pos), place: Below, fit, dismiss: ANY_KEY∣OUTSIDE_POINTER }` over `Viewport(styled markdown body).selectable().max_h(n)`. Non-modal. (There is no `TextRun::markdown` in the library — the body is composed from today's `PopupContent::Markdown(Vec<StyledLine>)`; see the inline-styling decision, §6.2.) | **Anchor needs the caret's pre-paint screen position via `compute_content_layout`** (§1, §4.4). The LSP hover *state machine* stays behind the leaf; only the rendered popup migrates. |
| **Splits / tabs / scrollbars** (`TabBarView`) | `split_grid` recursion: `Leaf → col([ tab_strip, row([ Focusable(Host::buffer(id)).flex(1), vscrollbar ]) ])`; `Split → flex_dir([ a, Gesture(Divider).on_press(capture_pointer), b ])`. | The divider captures the pointer on press — the whole drag mechanism, replacing the separator arm of `PointerGrab`. The active split's border is `focus_within`. Buffers/terminals are `Host` leaves (§4.4). This shape requires decomposing `render_content` per leaf — an open scoping decision (§1 caveat, §6.2). |
| **File explorer** (`FileExplorerView`) | `Tree` (or `List::windowed` over `get_display_nodes()`), selection controlled by `Window` state, `expanded_dirs` controlled (it is serialized). Context menu as above. | The `FileTree` model (lazy `TreeNode`, sort/filter, incremental search, decorations) is app state the `Tree` renders; only rendering/hit-testing/scroll move onto the widget. |
| **Dock + floating plugin panels** (`WidgetSurfaceView`) | The `WidgetSpec` → `Node<Action>` translation (§4.6) mounted in a dock column or a `Layer`. | `WidgetInstanceState` dissolves into element state. This is the plugin-API-visible wave. |
| **Settings** | The schema-driven form built directly from `SettingControl` as `col()` of `Toggle`/`Number`/`Dropdown`/`TextField`/`DualList`/`List`/`Tree`, inside a `Modality::Exclusive` `Layer` + `FocusScope`. | Deletes `widget_map.rs` (no per-frame projection), the dual state store, the bespoke `input.rs` handlers, and `view/controls/*`. The keybinding editor becomes another form in the same modal. |
| **Status bar / ~~search-options row~~** (`StatusView`) | `row().h(1)` of `TextRun`/`Button` segments; already live-derived, so the least coupled. | The M1 warm-up wave. The search-options row is **landed** — a `row()` of `gesture(text_runs(..))` toggles with fixed-width gaps between them, `Press` producing the real `Action::ToggleSearch*` and nothing positional but `UiFact::Hover`. |
| **Full-screen modals** (`TrustDialogView`, `AuxModalView`, `KeybindingEditorView`) | `Layer{ anchor: Screen(Center), modality: Exclusive, scrim: Dim, dismiss }` + `FocusScope`. | `Modality::Exclusive` subsumes whole-channel capture, `blocks_terminal_input`, and the hover/cursor suppression lists as one property. |

**Inline styling is an unsolved decision that cuts across this table.**
`TextProps` is a single unstyled `Rc<str>`, and a display-list `Item` carries
one `ThemeKey` — while today's chrome is full of intra-line styling: palette
fuzzy-match highlights, menu mnemonics, markdown popups, settings search
highlights, git-status coloring in explorer rows. Either the library grows
styled spans (a one-time library change, decided before M3), or the editor
composes one `TextRun` node per styled span and accepts the node count — under
a cell-identical acceptance bar. Decide before M3 (mnemonics) and M5 (match
highlights); see §6.2.

### 4.3 State homes

Every wave begins by classifying the surface's fields into four homes. This is
the discipline the implementation plan §6 sets out; the survey lets us fill the
column concretely.

| Home | Owner | Editor examples |
|---|---|---|
| **App state** (prop, passed down) | `Editor`/`Window` | which menu is *present*, which plugin/spec a panel mounts, `dock_width`, `menu_bar_visible`, the `SettingControl` values being edited |
| **Element state** (disposed with the widget) | the element | menu highlight, context-menu highlight, prompt scroll/caret, popup scroll, theme-info popup, dropdown open flag |
| **Framework state** (render objects) | `fresh-ui` | one focus position (replaces `key_context`, `dock.focused`, popup `focused`, `Prompt.toolbar_focus`), all `PointerGrab` drag flags (→ pointer capture), hover, multi-click detection |
| **Session state** (serialized ⇒ app state) | serde structs read by daemon/workspace/orchestrator | per-split scroll, `tab_scroll_offset`, `expanded_dirs`, `prompt.input` history |

**The invariant that guards this:** if a wave changes `workspace.rs`
serialization, something was misclassified. Persisted view state must be app
state because elements are disposed on unmount and do not survive a restart. The
library's `Persisted<T>` is for **new incidental state only** — it is *not* the
home for Fresh's existing typed, versioned serde view-state, which the daemon and
orchestrator read independently of any UI component. The restore suites
(`workspace_persistence_gates.rs`, `daemon_workspace_restore_parity.rs`, the
`orchestrator_*_restore` tests) are the guard.

Consequence worth stating plainly: **`Editor` gets smaller.** Most of its UI
fields are view state, so a wave mostly *deletes* fields — the god-object shrinks
as surfaces move to element/framework state.

### 4.4 The `Host` seam — where the text pipeline plugs in

The library's rule is that **paint never touches cells**: `RenderObject::paint`
has the signature `fn paint(&self, g: Geom, out: &mut DrawList)` and emits
display-list items; per-cell content has exactly one route across the seam,
`Draw::Host(HostId)` — "content the host owns and draws itself." The mechanism
is therefore split in two, and who-calls-whom matters:

- **The leaf** (`BufferHost`, on the exported `HostLeaf`/`RenderObject` path,
  closed in R2/D12): `layout` reserves the rect; `paint` emits `Draw::Host(id)`
  and nothing else; `hit` returns `Hit::Opaque` — hit results are *not*
  messages ("layout, paint and hit-testing never see a message");
  `takes_raw_input` reports PTY ownership so `Modality::Exclusive` above it
  derives suppression via `raw_input_leaves()` instead of the current
  `blocks_terminal_input` flag.
- **The fold** (editor-side, after `ui.frame()` returns): walking
  `LayoutSpec.items` in paint order, a `Draw::Host(id)` item calls back into
  the editor — *this* is where the `WindowBuffers::with_all_mut` disjoint
  borrow is taken and `render_content` runs into the shared cell buffer at the
  item's rect. The returned caches (`view_line_mappings`, cell-theme map,
  pending hardware cursor) are stored editor-side keyed by `HostId`. The
  library never sees them and never owns the buffers.

Consequences the plan must own:

- **The `Ui` cannot live on the `Editor`.** The fold reads the display list off
  the `Ui` while calling back into `&mut Editor`; if the `Ui` were a field of
  `Editor`, those borrows would conflict and the callback could not take the
  `with_all_mut` split. `Ui` and `Editor` are siblings, as in the library's
  tutorial — so `Editor::render(&mut self, frame)` becomes a free function or a
  method taking the `Ui` as a parameter. Asserted at compile time by
  `_the_ui_must_not_live_on_the_editor` in `app/shell_host.rs`.

- **Click→byte is an element-level concern.** A `Gesture` wrapping the host
  node maps the event position through the fold-published mappings and
  `click_geometry` in its handler, emitting a `UiMsg`; the render object itself
  cannot.
- **The caret arrives after the spec.** `LayoutSpec.cursor` is fixed during the
  library's paint pass, but `render_content` computes the hardware cursor at
  fold time. Either the frame merges the host caret outside the library's
  cursor mechanism (fold output wins for buffer carets), or geometry is derived
  early via `compute_content_layout` — which caret-anchored popups need anyway
  for same-frame anchoring.
- **The fold-callback API** — a per-`HostId` callback over `&mut Editor`, run
  mid-fold in paint order — is the concrete API the whole seam hangs on. Both
  of the library's own backends stub `Draw::Host` as a `▒` fill, so it had to
  be built here; it now exists in prototype as `view::shell::fold::HostPainter`
  with `impl HostPainter for Editor` calling `render_content` (§5.0).
- **A relayout footgun**: `HostSpec::Leaf` compares by factory-`Rc` pointer
  equality, so a `build()` that allocates a fresh leaf closure each frame
  forces relayout of every host leaf every frame. Host factories must be
  hoisted (`Shared`/cached fields), and the M0 prototype should assert it.

### 4.5 Theme integration

`fresh-ui` says only *where* appearance comes from: every `Item` carries a
`ThemeKey` string, and the backend maps it to colors (the demo's `style()` fn).
The editor already has a rich theme system (`view/theme`, syntect category
mapping, live preview) and a per-cell theme-provenance map. Integration is: the
TUI backend maps `ThemeKey → resolved Theme colors` (the same lookup
`*Colors::from_theme` does today), the theme is an **ambient** (`provide(&THEME,
…)`) so a theme change dirties only its dependents rather than forcing a root
rebuild — a benefit realized only where subtrees are hoisted into `Shared` or
components, since under a naive whole-root rebuild-per-frame the root rebuilds
regardless (§4.7) — and buffer text keeps its own per-cell theming inside the
`Host` leaf untouched. One granularity note: today's theme inspector reads a
per-*cell* provenance map; migrated chrome carries one `ThemeKey` per
display-list *item*, so chrome provenance is synthesized at fold time at item
granularity (buffer cells keep the per-cell map via the leaf) — confirm the
inspector survives the coarsening (§6.2). `convert_buffer_colors` (256/16 fallback) stays a post-process over
the folded cell buffer.

### 4.6 The plugin boundary

A plugin already sends a whole description tree; that *is* layer 1 crossing a
wire (design §13). The migration keeps the wire type `WidgetSpec` where it is
(`fresh-core`, unchanged for compatibility) and adds a **host-side translation**
`WidgetSpec → Node<Action>` in `fresh-editor` (the M6 wave). The reconciler moves
host-side, so `WidgetInstanceState` (list scroll, tree expansion, selection)
becomes element state — a plugin re-sending its spec no longer loses scroll. Two
externally visible changes, both needing a release cycle:

- **Keyed builders require a key function.** This breaks `widgets.ts` `List`/
  `Tree` calls without keys. Ship the new builders one release ahead, deprecate
  the old ones with a load-time warning.
- **State survival changes.** A plugin that compensated for state loss on re-send
  now sees state persist. Changelog item.

The plugin vocabulary stays a **stable subset** of the internal one (no `Host`,
no `Modality::Exclusive`, no focus policies, no arbitrary `M`), versioned with a
`.d.ts`, so the internal vocabulary can evolve without breaking plugins. One
kind needs care under that rule: `WidgetSpec::WindowEmbed` *is* host content
inside plugin UI. The subset still exposes no `Host` — the host-side
translation maps `WindowEmbed` to a nested `Host` leaf itself, so the
translation, not the plugin, names the leaf.

### 4.7 Rebuild cost and frame scheduling

The sketch above has `Editor::render` rebuild the whole chrome description from
`Editor` every frame. That is the *opposite* of the library's cost model, which
bounds rebuilds by pushing state down, hoisting invariant subtrees into
`Shared`, and letting ambients dirty only their dependents; a whole-root
rebuild per frame makes the identity short-circuit inert (fresh closures every
build) and re-reconciles every element on every PTY tick. At chrome scale
(hundreds of nodes) that may well be fine — but it is an assumption, not a
measurement, and it inverts the model the library documents. M0 therefore
includes a benchmark — full chrome build + reconcile at a sustained input rate
(≥60 events/s) — and the practice rules that fall out of it (which subtrees
must be `Shared`, which state moves into components so dirty marks stay deep,
host-factory hoisting per §4.4) are written down before M1.

---

## 5. The migration plan

This refines Part 2 of
[`widget-library-implementation-plan.md`](widget-library-implementation-plan.md)
(the deletion ledger and verification strategy hold as written) with the
concrete current-state findings. The one-implementation-at-a-time rule is
unchanged, and so are the wave *contents*.

**The acceptance test needs a third criterion.** Cell-identical output and
pointer parity are necessary and they are not sufficient: the dropdown chain
passes both while its description carries a rect, a pre-chosen width, a
pre-decided item count and strings already fitted to a width nothing measured.
A surface that passes those two has moved its *pixels* into the tree; it has
not necessarily moved its *content model*. So the bar is:

1. cell-identical output,
2. pointer and keyboard parity, and
3. **the tree measures this surface** — no rect, width or fitted string reaches
   the description pre-computed.

Where a wave meets 1 and 2 but not 3, say so rather than calling it migrated.
Today: the **search-options row meets all three** — the first surface that does,
and the reason it was scheduled ahead of the file explorer despite being the
smaller piece; the status bar has not started; the **menu-bar dropdowns meet 1
and 2 but not 3** (§6.2 item 5); the context menus meet all three except that
their opening point is handed in; the frame's regions meet 3 by construction,
being `Host` leaves whose only description *is* a rectangle the tree assigns. What does change is the **direction**: §5.0 argues — and a PoC
demonstrates — that the frame should migrate **first**, not last, which
reverses M0's mount point and dissolves M9 into the stages that follow. The
wave table in §5.2 is kept because its per-surface deletions are still the
work; read it as *what* each step deletes, with §5.0's stages as *when*.

### 5.0 Direction — outside-in, and why it inverts the original order

There are two ways to sequence this, and the choice matters more than the
wave contents.

- **Inside-out** — what the implementation plan's Part 2 assumes, and what the
  wave table below inherits: mount `fresh-ui` subtrees into rectangles carved
  by today's frame, migrating leaf surfaces first and the frame **last** (M9).
- **Outside-in** — make the frame a `fresh-ui` tree **first**, with every
  region below it a `Host` leaf that the fold paints using today's painters,
  then replace regions with native descriptions one at a time. The buffer is
  the last `Host` leaf standing.

**Outside-in is the better order**, for three reasons that all follow from the
code rather than from taste:

1. **It is the library's native shape.** `Ui::frame(root, size)` takes a
   whole-frame `Size`; there is no sub-rect mount. Inside-out has to invent one
   (M0 item 4), build every wave on it, and then delete it at M9. Outside-in
   uses the API as designed and invents nothing.
2. **Overlays get real semantics on day one.** A `Layer`'s stacking, modality,
   dismissal and focus scoping are properties of *being a child of the tree*.
   Mounted into a rectangle, a migrated context menu still needs the old
   precedence machinery for everything outside that rectangle — so M2's
   mechanisms are half-simulated, then redone once the frame inverts. Under the
   shell, the first migrated overlay is a real `Layer` immediately. That is
   where the value of this migration actually is (§3).
3. **The hybrid precedence rule already exists.** Today `Base` is the floor —
   pointer z0, `layer_rank::EDITOR_BASE` 0, "everything not otherwise
   claimed". The shell's legacy `Host` leaf *is* that floor. Migrated surfaces
   sit above it as ordinary tree children, and the legacy region shrinks
   monotonically. No new precedence concept is introduced during the
   transition, and the two tables (§2.3) die at the end rather than being
   emulated throughout.

The costs are real but bounded: the fold callback and caret merge are needed on
day one (already M0's hard part — earlier, not extra); dispatch runs in three
stages while the transition lasts (legacy capture band → `fresh-ui` dispatch →
legacy floor), collapsing to one when the last region migrates; and legacy
overlays painted after the fold are invisible to `fresh-ui` hit-testing, so the
legacy capture band — which today already arbitrates exactly this — stays in
front until overlays migrate.

**What does not change:** the wave *contents* (M1–M8) and the deletion ledger.
Only the frame's position moves, from last to first.

#### The PoC, and what it found

The whole shell rests on one testable claim: **`fresh-ui` can reproduce the
editor's frame rectangles exactly.** If it cannot, every region shifts and the
cell-identical bar is lost at step one. `crates/fresh-editor/tests/ui_shell_frame_parity.rs`
tests it: it builds the frame skeleton as a `fresh-ui` description, folds it to
`Draw::Host` items, and compares the rectangles against the ratatui `Layout`
calls `render.rs` actually makes — across 192 visibility combinations
(menu / status / search-options / prompt-line / dock / explorer on either side)
crossed with a grid of terminal sizes.

Two findings:

1. **Parity holds wherever the visible fixed rows fit** — exactly, including
   the dock and sidebar carves, at every width band from 1 column to 200. The
   frame layout is five `Length(n)` rows and one `Min(0)`, which
   `Sizing::Cells` and `flex(1)` reproduce without rounding. The shell can
   therefore take over the frame with no visible change.
2. **One divergence, in the deep-squeeze band.** When the visible fixed rows
   *cannot* all fit (frame height below the number of one-row regions), both
   engines give the content row nothing and drop a row — but a *different* one:
   ratatui's solver starves an interior row and keeps the last; `fresh-ui`
   fills in order and starves the last. `render.rs` flags this band on purpose
   ("running the actual split … keeps small-terminal squeeze behavior identical
   by construction").

#### The fold, prototyped

The frame parity above proves the *layout*. The other half — the fold, where
the backend meets `Draw::Host` — is prototyped in
`crates/fresh-editor/src/view/shell/fold.rs` (display list → cells, with a
`HostPainter` callback) and `crates/fresh-editor/src/app/shell_host.rs`
(`impl HostPainter for Editor`, calling `render_content`). Four things it
establishes:

- **Paint order is preserved across the seam.** Host regions are painted
  *inline* as their item is reached, so a chrome item later in the list lands
  on top — the popup-over-a-buffer case. A fold that collected host items and
  painted them in a second pass would invert this; the test
  `chrome_painted_after_a_host_lands_on_top_of_it` pins it.
- **The borrow works, on one condition.** `paint_body` assembles all ~28 of
  `render_content`'s parameters — the `WindowBuffers::with_all_mut` disjoint
  split, the theme read-guard, the config bundle — from `&mut Editor` *inside*
  the callback, while the display list being folded is borrowed from the `Ui`.
  That type-checks only because **the `Ui` does not live on the `Editor`**:
  `ui.spec()` borrows the `Ui`, the callback borrows the editor, and the two
  must be separate objects. This is the arrangement the library's own tutorial
  uses (`app` and `ui` side by side in `main`), and it is now a compile-time
  assertion (`_the_ui_must_not_live_on_the_editor`) rather than a claim.
- **The caret rule falls out instead of being listed.** `LayoutSpec.cursor` —
  set by a focused native `TextField` — wins over a caret a host region wrote
  through the `pending_hardware_cursor` out-parameter. That reproduces today's
  "an overlay's field takes the caret from the buffer" without the
  `cursor_suppressed_by_late_overlay` suppression list: if a native field has
  focus, it set the cursor, so it wins by construction.
- **`impl HostPainter for Editor` is the thing that shrinks.** Every region
  still listed in its `match` is one the old painters own; each stage moves one
  out into a native description, until only `HostRegion::Body` — the buffer and
  terminal grid — is left. That one never migrates.

#### Input, the other direction

`view/shell/input.rs` carries events the other way: the crossterm key and mouse
events `Editor::handle_key_press` and `Editor::handle_mouse` already receive,
translated into `fresh_ui::Input`.

The rule that makes the hybrid dispatch of S1 work is that translation is
**lossy in one direction only**. Everything the library understands passes
through faithfully; everything it does not returns `None` and stays on the
existing path. The shell takes what it understands and the legacy floor keeps
the rest, which is exactly the three-stage arrangement §5.0 describes.

Writing this adapter is also what found the library's one real gap, and it is
worth recording how that was decided, because the same judgement recurs. Three
things did not cross the seam. Each was tested against two questions — *is the
concept backend-neutral* (meaningful for a terminal, the web DOM and a test
alike), and *does the editor actually need it*:

| Gap | Verdict |
|---|---|
| **Horizontal wheel** | **Fixed in the library.** The scroll model was already two-dimensional (`ViewportProps::scroll` is a pair, `ScrollInfo::max` is a `Point`) but `Input::Wheel` had no axis and `scroll_chain` only moved `y` — an axis implied by the geometry everywhere except where it could be driven. The editor genuinely uses it (`ScrollLeft`/`ScrollRight`, a whole `on_hwheel` arm). Closed by the wheel-axis change this migration is stacked on. |
| **Keys with no counterpart** (`Insert`, media keys) | **Left alone.** Tested empirically rather than assumed: every key token bound by the shipped keymap was diffed against `KeyCode`, and the gap is empty. `KeyCode` is deliberately an abstract vocabulary, not a mirror of crossterm; widening it preemptively invites terminal specifics to leak. Grow it when a real binding needs it. |
| **Key kinds** (press / repeat / release) | **Left alone, and declining is correct.** Treating a repeat as a press is the right behaviour for every widget in the set — a held arrow should move a list. No widget needs release, and a `kind` field would force every consumer to reason about a case none of them handle. |

The general rule, worth applying to the two library changes §5.4 already
predicts: fix the library when the gap is an **internal asymmetry** — something
the model already half-expresses — and leave it alone when the gap is only
"the backend has a concept the library chose not to have".

Two translations encode decisions rather than mechanics:

- **Drag is reported as a move.** The library routes it by pointer capture, so
  the node that took the press keeps receiving motion without the backend
  distinguishing the two. That is the whole drag mechanism, and it is what
  replaces the `PointerGrab` flag ladder (§2.8).
- **The physical chord is reported, not the layout reading.**
  `fresh_input_parser::KeyPress` carries both, and the editor's keymap decides
  which wins. That decision belongs to the keybinding resolver, which survives
  at the root fallback (§2.9) — not to a widget.

What the prototype does *not* yet do: thread the real per-frame state (hover
targets, LSP-waiting, cursor hiding) instead of defaults, publish `BodyOutput`
(`view_line_mappings`, tab layouts) to the geometry bridge, and run the
three-stage dispatch for real. None touches the borrow, which was the open
question; all are mechanical work for S1.

Finding 2 is **not a layout bug to fix in `fresh-ui`.** It is a signal that
*which rows are visible* belongs in `build()` as a function of the available
height — app state deciding structure — rather than being left to
solver-specific starvation order. Deciding that explicitly is better behavior
than either engine's accident, and it removes the dependence entirely. It is
recorded as a decision (§6.2) and pinned by a test so it cannot drift
unnoticed.

The PoC also surfaced a smaller structural point worth carrying into M0: the
dock's bail-out rules (`EDITOR_MIN`/`DOCK_MIN`) are **app logic keyed on the
frame width**, and `build()` cannot read geometry. A real shell resolves that
width from state before building (a resize is an event like any other), or uses
`LayoutReader`. The same will be true of every "how wide is the frame" decision
that currently reads `size` at the top of `render`.

#### Landing S1 without a flag day

The frame does not have to switch over in one commit. This codebase already has
the right technique for hoisting geometry: when the chrome-layout work moved
surfaces from paint-recorded caches to live derivations, the paint pass
**debug-asserted that paint == derivation** before the cache was deleted
(§2.4). The same applies here.

**S1a** — `Editor::render` builds the shell's `Frame` from the same visibility
flags it already computes, runs `region_rects`, and `debug_assert`s every
region against the rectangle the ratatui `Layout` just produced. In debug
builds this runs on every frame, so the ~315-file e2e suite becomes the check
that `fresh-ui` can take the frame over — thousands of real frames at real
sizes, with real dock, explorer, prompt and suggestion states. Release builds
are untouched, and nothing about painting changes.

The assertion deliberately skips the squeeze band (frame shorter than its fixed
rows), where the two engines starve different rows by design. That is the
decision recorded in §6.2, not a defect, and it is pinned separately.

**S1b** — **landed.** S1a came back clean across the whole e2e suite, so the
frame's geometry now comes from the shell: one description, laid out once,
giving every region its rectangle. The five-row `Layout` and the sidebar carve
are gone from `render`.

Three things were needed to make the swap safe, and they are the reusable part
of the lesson:

- **Hidden regions still have positions.** A zero-height prompt row is where
  the suggestions popup anchors, so the description carries *every* region,
  hidden ones at zero size, mirroring the `Length(0)` constraints the ratatui
  layout used. The parity test compares all seven, empty ones included.
- **Decisions and geometry separate cleanly.** `split_file_explorer_area` both
  decided whether a sidebar shows and computed where it goes; it is now
  `file_explorer_layout_request`, which answers only "is there one, how wide,
  which side". The shell turns that into rectangles. Splitting the two is what
  let the geometry move without the policy moving with it.
- **Presence is app state, not geometry.** A hidden sidebar has a zero-width
  rectangle like anything else, so callers keep distinguishing the two by
  `Option` rather than by measuring.

The S1a assertion is removed with the code it checked — with only one
derivation left in `render`, it would have compared the shell against itself.
`tests/ui_shell_frame_parity.rs` keeps both honest instead, and covers far more
combinations than a running editor reaches.

#### S1 complete: the seam is live and inert

Three pieces closed it, and they share a shape worth naming — **each is
load-bearing and changes nothing**:

- **The retained tree persists.** `Editor::shell_ui` holds the `Ui` across
  frames, so element state, focus and the dirty set survive. It is held in an
  `Option` and **moved out for the duration of a frame** rather than borrowed
  from `self`: the display list is borrowed from the `Ui` while the fold calls
  back into `&mut Editor`, and as a plain field those borrows conflict. That is
  the same disjointness the sibling arrangement gives; either satisfies the
  constraint §4.4 records.
- **Native items paint through the fold.** `fold_native` walks the display list
  and skips `Host` items, so a region can move into the tree on its own while
  the ones around it keep their existing painters and their existing
  rectangles. When the last region is native this collapses into the general
  `fold`. A test pins the property the working state depends on: a frame of
  host regions paints *nothing*.
- **Input reaches the shell first.** `shell_dispatch` offers each translated
  event to the tree ahead of the legacy walk — stage two of the three-stage
  arrangement, with the modal-capture band still ahead of it and the existing
  walk still the floor. No node carries a handler yet, so it declines
  everything and every event lands where it always did.

The message type landed with it: `UiMsg::{Action, Ui}` (§2.9). Everything a
user could bind stays an `Action` and goes through `handle_action` unchanged;
positional facts get a `Ui` variant that is applied and never serialized.

**What this buys:** a surface stops being a `Host` leaf and immediately draws
through the fold, takes its own input, and keeps its own state — with no
further plumbing. That is what makes S2 and S3 a sequence of independent swaps
rather than one flag day.

#### The wave order was wrong, and the code said so

The plan ordered M1 (status bar) before M2 (context menus) by increasing risk.
Reading both renderers reverses that:

- `render_status` is ~3,200 lines carrying a right-side **drop heuristic**
  (shed low-priority elements until they fit alongside a ~40%-capped-at-40 left
  budget, never dropping the first), a narrow-terminal case under 15 columns,
  per-element truncation, separator theming, per-element styling that varies
  with LSP state, and cell-provenance runs. Reproducing it cell-identically is
  a wave in itself.
- `render_context_menu` is ~50 lines: clear the area, pad each label, draw a
  bordered block.

So context menus went first — which is also what the plan wanted for a
different reason: M2 is its designated go/no-go, the first surface to need a
layer at all.

**Context menu paint, migrated.** A context menu is now an ordinary `Layer` in
the frame's tree rather than a separately-ranked surface with its own painter.
Three things kept it cell-identical:

- **The position is the old one.** The layer anchors at the point
  `ContextMenu::clamped_position` already computed, rather than letting `fit`
  place it — so the menu lands on the same cells, and the hit-testing that has
  *not* migrated keeps agreeing with what is drawn.
- **The padding is the old one**, reproduced character for character
  (`" {:<width$}"`) rather than re-derived, because the row is what the cells
  actually contain.
- **The border glyphs changed to plain** (`┌┐└┘`). The fold drew rounded
  corners, which nothing had noticed because no surface had used a border yet;
  ratatui's default — and every bordered surface in the editor — is plain.

**Paint moved late.** The fold used to run straight after layout, which was
fine while it painted nothing. An overlay sits *above* the content around it,
so layout still runs early (the regions need rectangles) and paint now runs
where the context-menu paint used to be. Paint order is what puts a menu on
top.

`render_context_menus` and `render_context_menu` are deleted.

**Pointer input and dismissal, migrated.** This is the first place the
migration's central claim pays out: behaviour that was *written down* becomes
behaviour that is *declared*.

| Was | Is |
|---|---|
| a full-frame `chrome:context_menu_close_guard` box, pushed at z180, with a pointer arm that dismissed and consumed | `Modality::Exclusive` on the layer — everything outside is non-interactive because the layer says so, and no host leaf beneath it takes raw input |
| a `chrome:context_menu` box plus `handle_click_context_menus`, hit-testing the pointer against the menu's rect to decide activate / dismiss / inert-border | the rows' own `on_click`, and `Dismiss::OUTSIDE_POINTER` for everything else |
| `hover` + `on_hover_change`, walking hover targets to produce `HoverTarget::ContextMenuItem` and feed the highlight back | the rows' own `on_enter` |
| a "right-click inside an open menu" arm, so the menu is not re-opened or re-targeted | `on_secondary_click` that stops propagation |

`app/chrome/context_menu.rs` loses 138 lines: its `collect` is now empty, and
`on_pointer`, `hover`, `on_hover_change` and `handle_click_context_menus` are
gone.

**One thing worth recording**, because it decides parity: dismissal is
evaluated on the **press**, not the release (`hit.rs`'s `Input::Press` arm),
which is exactly when the close-guard box dismissed too. A first version of the
test watched only the release and saw nothing.

**Keyboard, migrated.** Arrows, Enter and the modal swallow were a pre-band
keyboard grab — a whole pipeline stage that ran before every layer rank. They
are now an `on_key` on a focused child of the layer, and the pre-band grab
stage is down to one component (the theme inspector's observer).

Escape is the interesting one: it is *not* handled. The layer declares
`Dismiss::ESCAPE`, and a key that dismisses a layer is answered by that layer.
The handler must therefore decline it without stopping — stopping claims the
key inside `propagate_key`, which returns before `dispatch_key` reaches
dismissal at all, so the menu would swallow Escape and stay open. That is the
shape of every modal that migrates: *act on what you own, stop what you swallow,
and let the layer's declared dismissals through untouched.*

`ContextMenu::on_key` and `handle_context_menu_key` are deleted.

**Geometry, unified.** The layer anchored at the point
`ContextMenu::clamped_position` computed — a bridge, while hit-testing was
still legacy, that outlived its reason and left the editor with two places that
decided where a menu goes. The layer now takes the **raw** click point with
`Fit::CLAMP`, which is the same arithmetic (`x.min(frame - box).max(0)`)
declared instead of written, and `clamped_position` is deleted.

Its second caller was the web `Scene`, and unpicking that is the part worth
recording, because every later wave hits it:

- **Suppression is a fold decision, not a tree one.** The web bridge renders
  with `suppress_chrome_cells` so the cells carry buffer interiors only, and
  the menu's tree derivation was gated on that flag — which meant the retained
  spec had no menu on exactly the path that needed its rectangle. The gate
  moved to the `fold_native` call: the description is built either way, and
  only the cell-writing half is skipped. That is what "backends are folds over
  the display list" is worth in practice — two frontends, one layout.
- **Consumers read the spec.** `context_menu::menu_rect(spec)` returns where
  the menu actually landed, found through `LayoutSpec::index` by the layer's
  key, and `Editor::shell_menu_rect` is its caller-side partner (the same shape
  as `shell_region_now`, for a surface that is not a host region). The `Scene`
  asks it instead of re-deriving the clamp.

What remains of the old implementation is `layer_rank::CONTEXT_MENU`, and only
because the PTY gate reads `blocks_terminal_input` off the overlay stack while
the library derives the same fact from `raw_input()` — which is only meaningful
once host leaves *declare* that they take raw input. Every region is a
`PlainHost` today, so deriving it now would report the terminal blocked on
every frame. It retires with the terminal grid's own host leaf (S5).

#### Menu-bar dropdowns, paint

The second overlay, and the first with real structure: a dropdown is a *chain*
— the menu's own box plus one per open submenu level, each placed against the
one before it. Each level is now a `Layer`, and the chain is the order they are
declared in. That is the whole of "a submenu paints over the level it opened
from"; the old renderer got the same result by painting in a loop, and a
z-ordered scheme would have had to state it as a rule.

The interesting work was not the tree. It was that the old dropdown decided
three things in one pass and spelled two of them twice:

- **`MenuRowStyle`** replaces two style ladders that had already drifted.
  `build_dropdown_item_line` chose colours for the cells and
  `record_dropdown_item_run` chose provenance keys for the theme inspector —
  the same decision, written out separately, and the recorder's copy did not
  know about hover, so it reported a hovered row as an ordinary one. There is
  now one ladder with three renderings: `style()` for ratatui, `theme_keys()`
  for the inspector, `shell_theme()` for the display list. The inspector's
  hover bug goes with it.
- **`dropdown_item_text`** is what a row *says*, separated from how it looks —
  the padding, the checkbox glyph, the keybinding hint, the submenu arrow,
  reproduced character for character because the row is what the cells contain.
- **The walk publishes the description.** `MenuLayout` gains
  `shell_dropdowns`: the same pass that decides each level's rectangle now also
  says what its rows read, and the shell paints from that. Not a second
  derivation of the menu — the only one.

`render_dropdown_level` no longer writes a cell, so its `frame`, `theme` and
`draw` parameters are gone, as is the ratatui `Paragraph`/`Block` that drew the
box.

**Paint only, deliberately.** The levels carry no modality, no dismissal and no
handlers, and each is anchored at the rectangle `fit_dropdown_area` already
chose. Pointer input still runs through `chrome::Menu`'s boxes and the
full-frame `chrome:menu_close_guard`. That is the same three-step shape the
context menus used — cells, then input, then let the layer's own `fit` decide
placement — and it is what keeps the not-yet-migrated hit-testing agreeing with
what is drawn.

#### The fold, split into two bands

The single fold was the migration's real ordering constraint, and it was
stricter than the plan admitted. There is one display list and *many* legacy
painters, and the legacy painters are not in the list — so one fold pass can
only sit on one side of them. It sat late, which made every native surface
paint above everything unmigrated. That is right for overlays and wrong for
everything else: the file explorer paints *first* among the legacy painters, so
making it native under one fold would have put the sidebar on top of the body,
the popups and the modals, silently.

`fold_native` now takes a `Band`. `render` calls it twice — `Background` before
any legacy painter, `Overlay` after all of them — and the legacy painters run
in between. Each band lands where its surface belongs, and the
"migrate top-down through the old paint order" rule retires with it.

**The cut is the library's**, and briefly was not. The first version derived it
here, by matching item keys against a hand-kept list of the frame's layer
families — which is precisely the "no hand-specified exceptions" the library's
second goal rules out, and it was already wrong twice over:

- **A scrim carries no key**, and is pushed *before* its layer's own items. An
  index-derived boundary puts it on the background side, so a modal's dimming
  would paint under the content it exists to dim. That is the rest of S3.
- **A layer need not be keyed at all.** `widgets::Dropdown`'s is not, so its
  whole pop-over produces no index entry and reads as in-flow content — it
  would vanish under the legacy painters. That is the first library widget S4
  reaches for.

`LayoutSpec::layers_from` says it outright (base PR #3052), with `in_flow()`
and `layers()` as the two halves, and `OVERLAY_FAMILIES` and its guard test are
deleted. Same diagnosis as `Dispatch::claimed`: the library already computed it
and threw it away.

**When a `fresh-ui` change is warranted**, stated properly, because this keeps
coming up and a vague version of it has already been applied wrongly in both
directions:

- **Underivability.** Can a correct consumer compute this from the library's
  *existing outputs*? If yes, it is not a library change, however convenient.
  `layers_from` fails that test — a scrim carries no key and is pushed before
  its layer's items, and `widgets::Dropdown`'s layer carries no key at all, so
  every backend-side derivation is silently wrong. A placement offset passes it:
  anchor to a different node.
- **Internal inconsistency.** The library contradicting its own contract is a
  library bug wherever it surfaces. An `Item` declares a rect and both in-repo
  backends painted outside it — the fix belongs there, not in every caller
  pre-fitting every string.
- **A caller in the same PR, plus a test that fails without it.** No primitive
  lands on speculation. `layers_from` had `fold_band` and the demo's F2 band;
  `Draw::Lines` clipping had a failing golden. `Anchor::Node` and four `Place`
  variants had neither and have never been constructed by anything, anywhere.

*Not* "would another consumer want this?" — that question is unfalsifiable, is
reliably answered yes by whoever wants the feature, and at value granularity
rejects everything (no second consumer wants `Sizing::Cells(12)` "with the same
meaning" either). It is what admitted the six unused variants.

That PR also closed a second gap it turned up. A `Draw::Lines` run can be
longer than the rect carrying it, because layout hands a constrained node the
width it was *allowed* rather than the width its content wants — and every
backend in the repo, the fold included, drew the string while honouring only
the *inherited* clip. So an over-long row painted straight through its own
border, which is what the ratatui `Paragraph`'s silent truncation had been
hiding. An item declares how much room it has; the backends clip to it now.

**What this unblocks, all at once:** the status bar and the search-options row
(S2, which had been stuck precisely because popups paint over the bar's row),
the menu bar's own row, the dock column, and the file explorer. None of them
depends on the overlay waves any more — their order is now a scheduling choice.

#### The menu bar row, native — the first background region

The first surface migrated *under* the legacy painters rather than over them,
and the two-pass fold's first real consumer.

- **`BarLabelStyle`** is the bar's `MenuRowStyle`: one ladder (normal / active /
  hovered) with three renderings — `style()` for ratatui, `theme_keys()` for the
  theme inspector, `shell_theme()` for the display list.
- **The mnemonic is a run, not a sibling.** One underlined character inside a
  label is text styled *within itself*, which is exactly what `text_runs` was
  added for; laying the three pieces out side by side would let them wrap and
  truncate independently. The underline is carried as part of the run's theme
  name (`menu.bar.item.active.mnemonic`), because an item carries one
  `ThemeKey` and the library never interprets it.
- **`MenuRenderer::render` is gone.** It painted; `compute_layout` did the same
  walk with painting switched off, and a `debug_assert` held them in step.
  Nothing paints a cell there any more, so there is one walk and nothing to
  keep in step. `render_menu_bar` becomes `record_menu_theme_runs` — the walk's
  only remaining side effect is the inspector's provenance.

**A region is named, not announced.** `regions_of` used to scan the display
list for `Draw::Host` items, which works only while every region *is* a host: a
native region emits no such item, and neither does a region that paints nothing
at all (a hidden row, a bar with no labels). Both still have rectangles that
callers ask for by name. Every region node now carries `region_key(r)` — host
leaf or native alike — and `regions_of` is a layout query (`Ui::find_by_key` +
`rect_of`) rather than a paint scan. That is goal 5 applied to the migration's
own bookkeeping: layout computes rectangles, everything else reads them.

The parity sweep caught this, which is the argument for having built it:
migrating the bar dropped region 2 from 8586 of its cases, silently, until the
oracle said so.

#### Menu pointer input, and the close guard

`app/chrome/menu.rs` goes from 204 lines to 71 — `collect`, `hover`,
`on_hover_change`, `on_pointer`, `handle_click_menu_bar` and
`handle_click_menu_dropdown_surface` all deleted, along with
`compute_menu_dropdown_hover` and the coordinate-driven
`handle_menu_dropdown_click`. What is left is the layer entry and the keyboard
grab.

| Was | Is |
|---|---|
| `chrome:menu_bar` box at z120, hit-tested against `MenuLayout::menu_at` | the bar labels' own `on_click`, which already know which menu they open |
| `chrome:menu_dropdown` box per level, plus `hit_test` → `(depth, index)` | each row's own `on_click`, which already knows its level and position |
| a full-frame `chrome:menu_close_guard` box at z110 | `Dismiss::OUTSIDE_POINTER` on the outermost level |
| `hover` + `on_hover_change` walking hover targets | each row's `on_enter`, feeding the *same* `menu_hover_reaction` |

Three things are worth recording, because each is a rule the next surface will
meet.

**Modality is `None`, not `Exclusive`.** Clicking another bar label while a
menu is open must close the first and open the second from one press — every
platform does. An exclusive layer makes the bar underneath inert and costs the
user a click. With `None` the dismissal fires first and the label's own click
follows, so the pair reads "close this, open that".

**Which makes the toggle a matter of *when*, not of what to remember.**
Clicking the *open* menu's own label closes it. Dismissal runs first, so by the
time any message is applied the menu is already shut and asking "is this menu
open?" answers no and reopens it — a toggle that never toggles.

The first fix had the label close over its own open-ness at build time, and it
was wrong in the running editor while passing every test: the main loop
repaints between press and release, so the release ran against a tree rebuilt
with nothing open. `mouse_click` in the harness sends both halves back to back
and could not express the gap; `mouse_click_with_repaint` now can, and
`test_mouse_click_toggles_menu_across_a_repaint` is the test that fails without
the fix.

The fix is to put the toggle where the state is. The bar acts on the **press**,
which is also what the pre-migration code did (`MouseEventKind::Down`), so the
dismissal and the toggle land in one dispatch — and `shell_dispatch` snapshots
`menu_state.active_menu` before applying a single message of it. The general
shape is the opposite of what the first attempt suggested: *a handler that needs
to know what was true before the event should not carry the answer; it should
be dispatched where the answer is still there to read.*

It also produces the right gesture split for free. The bar acts on press and
the rows act on release, which is precisely press-on-bar, drag-to-item,
release-to-activate — how a menu bar is used.

**A migrated surface claims the pointer over its own cells.** The hover target
lives in one field (`mouse_state.hover_target`) that the legacy walk rewrites
on every move. With the bar's chrome box deleted, that walk finds nothing there
and clears what the tree just set. So the bar row and the dropdown boxes take
`Move` — after which the legacy walk does not run for that event at all. Every
background region that migrates will need the same, until the walk itself goes.

`activate_menu_item(depth, index, menu)` is split out of the coordinate form: a
row that answers its own click already knows which row it is, and should not
have to hand back a cell for a hit-test to turn into the index it started from.

One deliberate behaviour change: a **right**-click outside an open menu used to
be swallowed by the guard box, which consumed any button. Dismissal fires for
any button but claims only the primary one, so a right-click now closes the
menu *and* goes on to open the context menu — the same ruling the context-menu
wave made, for the same reason.

#### Four corrections from the second review

An unbiased agent reviewed the branch against the library's stated goals
(`docs/internal/fresh-ui-migration-review-2.md`). Its verdict was that the
frame swap and the context-menu wave are on-goal and the menu-bar wave was
not. Four things came out of it, beyond the band cut above.

**The caret is wired, not asserted about.** `fold_native`'s return value was
taken, `debug_assert`ed `None`, and dropped with `let _ =` — a seam that would
be discovered missing by the first native field having no cursor. It now flows
into the end-of-frame commit, ahead of both the buffer's caret and the
sidebar's, and it needs none of the obscured/suppressed guards those two carry:
a native field is *in* the tree, so if it has focus it is on top by
construction. `cursor_suppressed_by_late_overlay` retires with the last
unmigrated overlay rather than growing another entry.

**`build` no longer reads layout.** `shell_frame` reached `menu_layout_now` →
`shell_region_now` → the retained tree, so building the description consulted
the rectangles the *previous* frame produced — the loop the library's own
`Ui::rect` refuses at runtime, and one frame stale into the bargain. The bar's
rectangle was never a layout result anyway: it is the chrome column's top row,
and `compute_dock_split` already decided that column from state alone.
`menu_layout_in(bar_rect)` takes it as an argument now.

**The tests could not see a style.** Every shell test rendered through a
palette answering `Style::default()`, so a highlighted row, a bold label and an
underlined mnemonic all came out identical — which is how four cell-level bugs
reached CI in one wave. `fold::test_palette` gives each theme name a distinct
colour and reproduces the two modifiers the real palette applies, and the
migrated surfaces now assert cell styles: the mnemonic run differs from the
characters beside it, the highlighted context-menu row differs from its
neighbours, and a dropdown painted over bold cells comes out unbold — the
display-list-is-not-a-diff rule, pinned at the cell.

**And the exact message list, not `contains`.** The click-bubbling bug produced
`[MenuBarPress, CloseMenu]` — open and shut in one gesture — and passed a
`contains` assertion happily. The input tests assert the whole list now.

#### Revised stages

| Stage | What moves | Why here |
|---|---|---|
| **S1** | Frame skeleton: every region a `Host` leaf, painted by today's painters. Input fully delegated. | **Landed.** The frame's geometry is the shell's, the retained tree persists across frames, native items paint through the fold, and input is offered to the shell ahead of the legacy walk. Every region is still a `Host` leaf, so nothing has changed on screen — which is the point. Remaining: the body still paints through `Editor::render`'s own `render_content` call, not through the seam — `fold_native` installs a `Skip` painter, so `HostPainter::paint_host` is never reached in the live path and `paint_body`'s `BodyState::default()` drops nothing today. Threading real state means *moving the body onto the seam*, which is S5's per-leaf `render_content` decision (§6.2 item 7) rather than a loose end here. |
| **S2** | The live-derived regions — status bar, search-options row — become native descriptions. | **Done.** Both surfaces describe what is on them and layout decides every column. The **search-options row** was the first surface to meet the third acceptance criterion. The **status bar** was the one that paid for it twice: `render_status` placed every element *and* emitted a `StatusBarLayout`, then `compute_status_layout` re-ran the whole walk at event time over state that may have moved. `clickable_rects`, `plugin_token_areas`, `segments` and `provenance_runs` all read the laid-out tree now; plugin tokens became first-class (they were a second loop the click rail reached only after missing every built-in). What stayed app-side is *which* right-hand elements appear when the bar is too narrow — a content decision from measured text, not geometry. |
| **S3** | Overlays become real `Layer`s: context menus → dropdowns/menu bar → popups → prompt/palette → modals. | The value stage. Each one deletes guard boxes, a rank entry, and a slice of the capture band. **Context menus: done** (below) — paint, pointer, dismissal, keyboard and geometry, with only the `blocks_terminal_input` rank entry left behind. **Menu bar: paint and pointer migrated** — the bar row is a native background region, the dropdown chain is a stack of layers, and the close guard is a dismissal property.

**The two remainders are both blocked, and neither is a loose end.**

*The keyboard grab* is blocked on a precedence decision, found while trying to finish it. `MenuInputHandler` is capture-all (Esc / Enter / arrows+hjkl / Home / End, everything else swallowed) — that part maps onto the context menu's `on_key` shape directly. But `chrome::menu::menu_action_binding` consults the `menu` keymap section *first*, and it lives in the layer walk, which runs **after** the shell is offered the key (`app::input`). A capture-all `on_key` on the dropdown chain therefore swallows every user-bound menu key before the keymap is ever consulted — reintroducing exactly the bug that consult was added to fix (Emacs `C-n` / `C-g` in an open menu doing nothing). The context menu never hit this because it consults no keymap. **Decide first:** does keymap resolution move ahead of shell dispatch, or does a migrated surface get to ask the keymap from inside its description? Every later keyboard-owning surface — the palette, the prompt, the modals — hits the same question, so it is worth answering once.

*The rank entries* cannot go until the rest of S3 does. `blocks_terminal_input` is contributed by six components — popups, dock, floating modal, base, menu, context menu — and `presents_blocking_overlay` is the single source of truth for "is anything modal up?". Removing the two migrated entries would mean an open context menu or menu stops blocking PTY routing. They retire with the last unmigrated overlay, not before. |
| **S4** | Dock column, file explorer, plugin panels. | **File explorer: done** — the panel is a native region, rows and slots are measured by the tree, `trailing_slot_screen_bounds` and the old renderer's paint half are deleted, and the grip paints its own hover column via `layout_reader`. **Dock column and plugin panels remain**, and they are the M6 wave rather than a remainder: `WidgetSpec` → `Node` translation, element state replacing `WidgetInstanceState`, and a plugin API change. No longer waits on S3: the two-pass fold means a background region can migrate while the overlays above it have not. What it *does* wait on is §6.2's "colour that is not a theme name" — the explorer's slots and the panels' widgets both carry plugin-supplied colours, which a `ThemeKey` cannot name. The plugin API change (keyed `List`/`Tree` items) is deprecated ahead of it, so its release cycle runs in parallel rather than in series. |
| **S5** | Splits, tabs, scrollbars decompose; the buffer becomes the only `Host` leaf. | Requires the per-leaf `render_content` decision (§6.2); last because it is the only stage that touches the KEEP side. |

### 5.1 M0 — the seam (a genuine prototype, not just plumbing)

M0 is re-scoped from "pure plumbing" to a **prototype gate**: the Host seam
(§4.4) has never been exercised by any backend, and every wave stands on it.
Seven pieces:

1. **TUI backend** in `fresh-editor`: `LayoutSpec` → ratatui `Buffer`, mapping
   `Item::theme` (`ThemeKey`) through the resolved `Theme` and synthesizing
   chrome theme provenance per item. (The `examples/interactive.rs` fold is the
   reference shape for chrome — note it stubs `Draw::Host`.)
2. **The fold-callback API** — the concrete shape of "backend meets
   `Draw::Host`": a per-`HostId` callback over `&mut Editor`, run mid-fold in
   paint order, taking the `with_all_mut` borrow and running `render_content`
   at the item's rect (§4.4). Nothing like it exists yet; this *is* the
   prototype.
3. **`HostLeaf` impls** — `BufferHost` and a terminal-grid host emitting
   `Draw::Host(id)`, with hoisted factories (the `HostSpec::Leaf` pointer-
   equality footgun, §4.4).
4. **The frame skeleton** — the whole frame as a `fresh-ui` tree with one
   `Host` leaf per region, replacing the ratatui `Layout` carves. Proven
   rect-for-rect against today's layout by
   `crates/fresh-editor/tests/ui_shell_frame_parity.rs` (§5.0). Under
   outside-in this replaces the inside-out mount point, which is not built at
   all.
5. **Input adapter** — terminal event → `fresh_ui::Input`, and messages back
   out as `UiMsg` (Actions + UI-geometry) into the existing pipeline.
6. **Caret arbitration** — one "who owns the caret this frame" decision,
   replacing `cursor_suppressed_by_late_overlay`: the fold-time host caret
   merged against `LayoutSpec.cursor`, with `compute_content_layout` for
   same-frame anchors.
7. **Geometry bridge** — fold-published caches keyed by `HostId`
   (`view_line_mappings`, cell-theme map) readable by handlers, plus the
   pre-paint path for caret-anchored layers.

**Exit:** the frame renders through the shell with every region still painted
by today's painters and cell output unchanged, a one-line status segment
renders and takes a click as a native description, **and** a prototype
`BufferHost` proves the seam end-to-end — real
buffer cells under `Draw::Host`, one caret-anchored popup anchored via
`compute_content_layout`, one click-to-cursor through a `Gesture` handler —
plus the §4.7 rebuild benchmark. No wave is scheduled until this exit holds.

### 5.2 Waves (increasing risk)

| Wave | Surface | New mechanism exercised | Deletes (survey-grounded) |
|---|---|---|---|
| **M1** | Status bar, ~~search-options row~~ (**row done**) | static layout, click targets | the live-derived `status_bar_layout_now` path and its `StatusView` painter. The search-options half is landed: `chrome/search_options.rs`, `SearchOptionsLayout` (+`compute`/`checkbox_at`), `SearchOptionsHover`, `StatusBarRenderer::render_search_options` and both of the row's `debug_assert_eq!` oracles are deleted |
| **M2** ⟵ **go/no-go** | Context menus (tab / new-tab / explorer / close-split) | `Layer`, `Modality::Exclusive`, `dismiss`, list nav | `chrome/context_menu.rs`, its close-guard box, its `on_key` pre-band grab, its rank entry, the four `Window` context-menu highlight fields |
| **M3** | Menu bar, dropdowns, submenus | nested layers, hover auto-switch, mnemonics | `chrome/menu.rs`, the `view/ui/menu.rs` dispatch half, the menu close-guard box, the hover auto-switch machine |
| **M4** | Info/hover/signature popups, theme inspector | transient dismissal via observers, scroll, text selection | `chrome/popups.rs`, `chrome/theme_info.rs`, `view/popup_mouse.rs` remnants, the transient-dismiss pre-band stage (the LSP hover *state machine* stays behind the leaf) |
| **M5** | File browser, prompt / command palette | `FocusScope`, text input, results list, preview | `chrome/prompt.rs`, `chrome/file_browser.rs`, `view/prompt_input.rs`, the overlay toolbar ring, the click scrim, the position-blind wheel box, the manual-scroll latch |
| **M6** | Plugin panels: dock + floating | `WidgetSpec` → `Node` translation, element state replacing `WidgetInstanceState`, **plugin API change** | `widgets/kinds/*` dispatch, `widget_runtime.rs`, `WidgetInstanceState`, `WidgetMutation` fast path |
| **M7** | Modals: workspace trust, keybinding editor, calibration wizard | `Modality::Exclusive` | `chrome/modals.rs`, `capture_mouse`, `blocks_terminal_input`, the cursor/hover suppression lists, the bespoke `handle_*_mouse` |
| **M8** | Settings (+ keybinding editor form) | the largest interior; rendering already on `WidgetSpec` | `view/settings/*` control layer, `view/controls/*`, `widget_map.rs`, the dual state store, the bespoke settings `input.rs` |
| **M9** | Frame: splits, tabs, scrollbars, dock column, explorer pane | the frame itself; all else nests inside | `chrome/splits.rs`, `chrome/base.rs`, `chrome/mod.rs` (registry, `layer_rank`, `chrome_tree`), `mouse_input.rs` dispatch engines, `PointerGrab`, the chrome half of `render.rs`, `KeyContext` |

**M2 is the decision point.** It is the first wave using layers, modality,
dismissal and focus together. If the seam and the model hold there, the later
waves apply the same mechanisms; if not, the library is corrected before wave
three rather than after eight surfaces depend on it.

**M9 dissolves under outside-in.** The wave table above is inherited from the
inside-out plan, where the frame migrated last. Under §5.0 the frame is S1
instead, so M9's contents split: the frame layout, dock and explorer carves
move to the front (S1), and what remains — the split grid, tabs, scrollbars,
and the removal of `app/chrome/`, `PointerGrab`, `KeyContext` and the two
precedence tables — becomes S5, the last stage, because it is the only one that
touches the KEEP side (§6.2). The deletions are the same; only their order
changes. M2 remains the go/no-go: it is still the first wave to use layers,
modality, dismissal and focus together, and under the shell it does so for
real rather than through a mount point.

**Surfaces the wave table doesn't name**, and where they land:

- **Tab-drag ghost / drop indicator** (`app/tab_drag.rs`) — the *grab* is
  pointer capture; the floating preview is a `Layer` following the pointer.
  M9.
- **Frame-buffer animations** (`view/animation.rs`) — cell-level snapshots of
  the previous frame with no display-list expression; they stay post-processes
  over the folded buffer, beside `convert_buffer_colors` (or are retired — a
  reviewed decision, §6.2).
- **Warning domains** (`app/warning_domains.rs`) — plugin-visible status
  indicators + popup content; rides M1 (segments) and M4 (popup); named in the
  M6 changelog because plugins observe it.
- **Orchestrator dock shell pages and session preview**
  (`render_dormant_shell_page`, `render_session_preview_into_rect`) — the
  preview is a second `Host` leaf (the phantom-leaf path); M6/M9.
- **`WidgetSpec::WindowEmbed`** — host content inside plugin UI; translated
  host-side to a nested `Host` leaf (§4.6). M6.
- **Live-grep preview and quick-open hint overlays** — M5 (the preview is a
  phantom-leaf `Host`).
- **`fresh-gui` native macOS menus** (`crates/fresh-gui/src/native_menu.rs`) —
  built from the `Menu` model, which stays app state; M3 must keep that model
  intact and name the second menu frontend in its PR.
- **Scrollbar overview-ruler markers** (plugin API) — `Draw::Scrollbar` carries
  only `{offset, content, window}`; see §6.2 before M9.

### 5.3 Verification

The e2e suite (~315 files) is the primary mechanism, used as-is:

1. **Cell output stays byte-identical** per wave (the existing snapshot/visual
   harness). A diff is a defect or a reviewed intended change. Reproducing exact
   spacing — including cases that look wrong — is a real part of each wave; make
   any deliberate visual change *separately*, after the wave, so a regression is
   distinguishable from an intended change.
2. **`scene_parity.rs` passes** through every wave — the web projection has not
   diverged. As each surface migrates, its `Scene` projection becomes the
   component's props rather than a separate output, but the projected data must
   match until the web frontend consumes `LayoutSpec` directly.
3. **The standing parity oracles** (event-time geometry vs paint walk; focus
   ring) stay enabled until the surface they cover migrates, then are removed
   with it.
4. **Per-wave routing tests** — the existing precedence tests (clicks not
   reaching the buffer through a popup, modality, focus order) pass unchanged
   against the new implementation.
5. **New `LayoutSpec`-level assertions** by key are *added* alongside the cell
   assertions, not in place of them.

### 5.4 Risks and stop points

1. **L1/L2 semantics are fixed** (they are — Part 1 is done and its deviation
   register is closed). The library is frozen; a wave that needs a library change
   is a signal to stop and fix the library, not to fork behavior into the editor.
   Two such changes are already foreseeable (styled text spans, scrollbar
   markers — §6.2): schedule them as library work *before* the wave that needs
   them rather than discovering them mid-wave.
2. **Cell-identical output is a hard constraint**, and the biggest single cost
   per wave.
3. **M6 changes plugin-visible behavior** (state survival) and breaks the API
   (required keys). It needs a release cycle of its own.
4. **M8 (Settings) is optional as a stopping point.** It is the largest interior
   and the least coupled to dispatch; stopping after M7 with Settings still on
   the current (already-half-unified) path is a supported end state.
5. **Two implementations of one surface must not persist across waves.** A wave
   that cannot delete its predecessor indicates a defect in the seam — fix the
   seam rather than accumulate a second UI stack.

   **A wave may split by channel, and must say so.** That rule is about the
   same behaviour existing twice, not about a surface having to migrate in one
   step. Paint, pointer, keyboard and modality can move separately — the
   context menu moved paint, then pointer, and still has its keyboard grab and
   its `layer_rank` entry — and that is legitimate *provided each channel has
   exactly one implementation at every moment*. The menu's pointer input lives
   in the tree and nowhere else; its keyboard grab lives in the chrome
   component and nowhere else.

   The cost is a surface spread across three files mid-wave, which is only
   tolerable while it is *visibly* mid-wave. So a split wave names, in the
   surface's own module doc, which channels have moved and which have not —
   `view/shell/context_menu.rs` does — and the wave is not finished until the
   last channel lands and the old file is deleted.
6. **The caret and the caret-anchored popups are the subtle seam.** Get the M0
   caret arbitration and geometry-cache bridge right, or M4/M5 popups mis-anchor.

---

## 6. Decisions and open questions

An independent review of this doc against the source (the review verified the
§2 survey nearly claim-for-claim, corrected the §4.4 seam mechanism to the
fold-callback form now described there, and investigated the original open
questions) settles the first five below and leaves a concrete pre-wave decision
list.

### 6.1 Answered

1. **Web frontend endgame — a parallel track, not a wave.** `Scene` +
   `scene_parity` stay the web wire format through M9: until the frame inverts,
   the UI is hybrid, and a `LayoutSpec`-consuming web frontend would need both
   protocols at once. After M9 the web can consume `LayoutSpec` (it is designed
   for DOM patching by key) — but buffer panes remain cell slices either way,
   because they are `Draw::Host`; the display list never fully replaces the
   cell channel.
2. **`WidgetMutation` — still needed, reinterpreted.** `List::windowed` solves
   *render* cost, not *wire* cost: the 5000-node `js_to_json` walk happens at
   the plugin↔host boundary regardless of how rows are rendered. Keep
   `AppendTreeNodes`/`SetItems` & co. as **edits to the host-side data source**
   that the windowed builders read by index, rather than edits to a retained
   widget tree. The plugin API survives unchanged.
3. **Mutate-then-decline keyboard rungs — expressible per node.** A bubbling
   handler that returns `None` continues the walk, so "process and fall
   through" ports directly. What does *not* port is **cross-layer guard
   coupling** — a rung consulting a higher layer's state mid-walk (the
   unfocused-popup Esc interception guarded by
   `popup_blocked_by_higher_modal`; the popup rung entangled with the
   deferred-action queue). Each such rung is converted in M2/M4 into a
   capture-phase observer on an ancestor or an `Action` that re-dispatches —
   with a routing test per rung, because cell-identical output cannot see
   these behaviors.
4. **Content hover trackers — feasible, but not via a leaf-raised message.**
   Render objects cannot emit messages; they don't need to. The LSP-hover and
   terminal-link state machines already live in app state (`app/hover.rs` +
   the async bridge); `build(ed)` includes the popup `Layer` when that state
   says so. The hard part is the anchor — the §4.4 geometry bridge /
   `compute_content_layout` — plus one verification: pointer *motion* must
   keep reaching the leaf for the debounce machines while `fresh-ui` hover
   exists above it.
5. **Multi-window — one `Ui`, window subtree keyed by `WindowId`.** Fresh
   windows are not concurrent OS windows; exactly one is rendered per frame.
   Cross-window content (orchestrator session preview, `WindowEmbed`) appears
   as additional `Host` leaves. Switching windows disposes the outgoing
   subtree's element state — acceptable because anything that must survive a
   switch is `Window` app state by the §4.3 rule, matching today's behavior.

### 6.2 Open decisions — settle each before the wave it blocks

1. ~~**How does the editor learn the tree claimed an event?**~~ — **decided,
   and it was the review's finding rather than the plan's.** The seam's
   contract was briefly carried in the message channel: `shell_dispatch` read
   "claimed" off "did any message come back", and a `UiFact::Consumed` message
   existed to mean "there is no message". That produced a real bug (a
   right-click outside a menu closed it and then failed to open the next one)
   and silently swallowed hover from the legacy trackers.

   `fresh-ui` reports it now — `Dispatch { msgs, claimed }` — which the
   library's own gap rule required, since `propagate_all` already computed it
   and threw it away. **The general lesson: when the editor finds itself
   inferring something about routing, that is a missing library capability, not
   a place for an editor convention.**
2. ~~**Migrate in paint order**~~ — **superseded.** The rule was: one fold,
   running after every legacy painter, so a region may become native only once
   everything that paints over it already has. It was the migration's real
   ordering constraint and it was stricter than it looked — the file explorer
   paints *first* among the legacy painters, so under one fold it could not
   migrate until nearly everything else had.

   The fold is two passes now, `Background` before every legacy painter and
   `Overlay` after all of them, so each band lands where its surface belongs
   and the ordering rule retires. Nothing replaces it: which band an item
   belongs to is `LayoutSpec::layers_from`'s answer, not a convention anyone
   has to maintain.
3. ~~**The fold-callback API**~~ — **prototyped** (§5.0). `HostPainter` +
   `impl HostPainter for Editor`; paint order, clipping, the caret rule and the
   borrow are covered by tests. What remains is threading real per-frame state
   and publishing `BodyOutput` to the geometry bridge — mechanical S1 work. The
   `Ui`-beside-`Editor` constraint it revealed is recorded in §4.4.
4. ~~**Colour that is not a theme name**~~ — **decided and shipped: a name is
   always real theme keys.**

   The entry here proposed minting `dyn:N` names during build. That was wrong
   twice over — it makes `build` mutate a table, which goal 3 forbids in as many
   words, and `dyn:17` destroys the provenance a `ThemeKey` exists to carry. The
   review's content-derived alternative was better but still invented a name
   space.

   What shipped needs neither, because the editor already had the answer:
   `Theme::resolve_theme_key("section.field")` is a generic, table-generated
   name resolver, and `Theme::resolve_modifier_key` reads the attribute declared
   on the same table row. The shell was hand-writing a twenty-arm match over
   names of its own beside it.

   **A shell `ThemeKey` is `fg_key/bg_key`**, optionally `+bold` / `+underline`.
   A cell needs two colours and an `Item` carries one name, so the name is a
   pair; both halves resolve through the editor's own table. Nothing is invented,
   every colour on screen traces to an entry a user can edit, and the six
   spellings for two orthogonal attributes
   (`menu.bar.item{,.mnemonic}{,.active,.hover}`) collapse into one pair plus
   composable attributes — which is the point, because the blow-up arrives in
   earnest with the explorer's git status × selection × cut × focus.

   It also converges two things that were saying the same thing in different
   words: the theme inspector has always recorded provenance as
   `ThemeRun { fg_key, bg_key }`, exactly this pair. `MenuRowStyle::shell_theme`
   is now derived from `theme_keys()`, so the display list and the inspector
   cannot disagree — and unifying them fixed a real drift, where a hovered bar
   label reported its *resting* keys to the inspector while painting hover
   colours.

   **The one genuine limit**, for whoever migrates the explorer: plugins may
   send `OverlayColorSpec::Rgb`, a shipped wire variant, and a raw colour is not
   a theme entry. Two things make it smaller than it looks. Every in-repo slot
   provider already sends `ThemeKey` (`syntax.string`, `syntax.type`,
   `ui.file_status_modified_fg`, and LSP diagnostics), so the built-ins need
   nothing. And the plugins that genuinely need raw colour are the ones
   displaying colour *as data* — `theme_editor.ts`'s swatch grid — which should
   not be theme entries. When the explorer migrates, the choice is to give that
   family a self-describing name (`rgb:7f3fbf`, resolved by the same parser) or
   to leave those rows behind a `Host` leaf. Neither needs a side table, and
   neither is needed before then.

   `resolve_overlay_color` collapsing a spec to a `Color` inside the slot
   *cache* — long before any description exists — is the thing to move when that
   day comes.

5. **The dropdown chain's placement** (the tail of M3). *A dependency, not a
   decision — and the previous entry here got both its facts wrong.*

   It claimed `Anchor::Node` "can name the parent but not the offset", so a
   `Place` offset was needed as a base PR. False: **anchor to the row above**.
   `Place::RightOf` then yields the wanted rectangle for every row, today, with
   no library change. It also claimed `fit_dropdown_area` "runs once, so nothing
   can drift" — it ran twice per frame in release and three times in debug, from
   three different bar rectangles, held together by a `debug_assert_eq!` that
   release compiles out. (Now one walk; see `Editor::menu_layout_frame`.)

   The real blocker is upstream of placement: **the chain has no content model
   in the tree to place.** `DropdownLevel` carries `x`, `y`, `width` on a type
   whose only job is to produce a description — the design doc's own stop sign
   ("a description type with a rect field indicates layers 1 and 3 have been
   merged") — plus pre-fitted strings, a pre-decided item count, and a
   pre-chosen width. Nothing is measured, so `Place::RightOf` has nothing to
   place against and `Fit` is not expressible at all.

   And the two rules genuinely differ, so turning `fit` on is a pixel change,
   not a refactor. Measured on one frame (w=22, parent box x=4 w=12, sub w=10,
   no room right): `Fit::FLIP` gives `sub.x = 12`, the editor's rule gives
   `x = 1` — **eleven cells apart**. Vertically `fit_dropdown_area` never flips;
   it *drops items* to fit, where `Fit` would move the box and keep them all.

   **The order, therefore:** rows become real nodes → the box measures itself →
   `Fit` becomes expressible → the flip/truncation divergence above becomes a
   reviewed pixel change → *only then* is the border offset a question, and the
   leading answer is `Anchor::Node(row_above)`, which needs nothing.

   If a primitive is still wanted after that, it is **not** a scalar offset. It
   is the relationship — *place the layer so that this descendant lands on the
   anchor* — which survives a border-thickness change and has a real second
   consumer in this repo's future: `crates/fresh-gui/src/native_menu.rs`, where
   a macOS-style popup button puts the *selected* item over the trigger.

   **Before relying on any of it, note the API is largely unexercised**:
   `Anchor::Node` has zero callers and zero tests in the whole repository, and
   four of six `Place` variants (`Above`, `RightOf`, `LeftOf`, `Fill`) are never
   constructed anywhere — library, tests, demo, or editor. Whoever uses one owes
   it a test first.

6. ~~**Inline styled text**~~ — **not a gap; it was already there.** Listed as
   blocking M3/M5, but `text_runs` takes styled pieces as one logical string
   that measures, wraps and truncates as a unit. Mnemonics, match highlights
   and the explorer's git colouring all use it. Checked against the library
   rather than the note, while surveying for the second base PR.
7. **Per-leaf decomposition of `render_content`** (blocks M9). Its unit today
   is the whole split tree; the target grid wants per-leaf `Host` nodes with
   `fresh-ui` tabs and dividers. Either the whole grid stays one `Host` leaf
   (and M9's headline deletions shrink), or the orchestration layer is split
   per leaf — a refactor on the KEEP side (`render_phantom_leaf` shows a
   per-leaf path exists, minus the cross-tree logic). A scoping decision, not
   a detail.
8. **Scrollbar markers** (blocks M9). `Draw::Scrollbar` carries only
   `{offset, content, window}`; the plugin overview-ruler marker API has no
   expression. Extend the library's scrollbar, keep scrollbars behind the
   `Host` leaf, or drop the API.
9. ~~**The message-type split**~~ — **decided and shipped** as
   `UiMsg::{Action, Ui(UiFact)}` (`view/shell/msg.rs`). Anything bindable stays
   an `Action`; positional facts are `UiFact` and are never serialized.
10. **Frame scheduling and rebuild cost** — still open, and no longer gating.
   It was written as an M0 exit criterion; S1, the context-menu wave and the
   frame swap all shipped without it, so calling it a gate was wrong. The
   measurement is still worth taking (a full chrome rebuild per frame, plus a
   `Vec<String>` of labels per open menu), but it is a performance question to
   answer with a profile, not a precondition.
11. **Row visibility under squeeze** — still open; S1 shipped without deciding
   it, and the divergence is *recorded* by
   `squeeze_band_starves_a_different_row_than_ratatui`, not resolved by it. When the visible fixed rows
   cannot fit, `fresh-ui` and ratatui starve different rows (§5.0). Decide
   which rows `build()` drops as a function of available height, making the
   choice explicit app state instead of inheriting either engine's starvation
   order. Pinned today by
   `squeeze_band_starves_a_different_row_than_ratatui`.
12. **Frame-buffer animations.** Cell-snapshot effects have no display-list
   expression: keep them as post-processes over the folded buffer (beside
   `convert_buffer_colors`), or retire them deliberately.
13. **Theme-inspector granularity** (§4.5). Chrome provenance coarsens from
   per-cell to per-item; confirm the inspector survives, with buffer cells
   keeping the per-cell map via the leaf.
   *Sharpened by the search-options row.* Every migrated surface now carries
   its provenance in the display list already — a shell theme name **is** the
   `fg_key/bg_key` pair the inspector records (§ `shell_theme`) — so the
   inspector could be fed from the fold, once and for all surfaces, instead of
   from a per-surface `theme_runs` walk. What stops it today is a type: the
   fold's names are `String`s built at build time and `ThemeRun`'s keys are
   `&'static str`. Until that is settled, a migrated surface whose legacy walk
   is deleted records nothing, and the inspector says "No theme key recorded
   here" — which is what the search-options row said before it migrated too
   (issue #2362 pinned exactly that), so nothing regressed; it simply did not
   improve. The menu bar is the exception only because its legacy walk still
   runs to produce runs.
14. ~~**A bordered box does not clip its children**~~ — **fixed in the second
   base PR** (#3095). `.border()` insets what is inside it but nothing bounded
   it, and only a `Viewport` bounded anything at all — so a row the panel could
   not fit painted over the panel's own right border. `BoxProps.clip` now
   exists, `border()` implies it, and the bound is the *content* rect rather
   than the outer edge. Pinned in `shell::file_explorer`'s tests as the escape
   (`clip(false)`) and the frame surviving (the default). The fourth item in
   the family that produced the first base PR — `pointer_mode` on any node,
   `min_w`/`min_h`, `Event::clicks` — and the first that was a correctness bug
   rather than a missing expression.

   Three things listed here as gaps were not gaps, and are closed rather than
   deferred: inline styled spans are `text_runs` (was item 6); modal focus
   containment is `Modality::Exclusive` + `FocusScope`; and content shaped by
   its own extent — a rule, a divider, the explorer's own full-height grip
   glyph — is what `layout_reader` is for. Still genuinely absent: markers on
   `Draw::Scrollbar` (item 8).

15. **Wheel-semantics parity** (M4/M5). Today's wheel walk has no dedup and no
   opacity gate *by ruling*; `fresh-ui` chains wheels by "a `Viewport` claims
   only if its offset changed." Close but not identical — e.g. a
   scrolled-to-bound popup over the buffer. Land an explicit parity test
   before relying on either behavior.
