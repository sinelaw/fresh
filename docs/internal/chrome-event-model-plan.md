# Chrome registration + tree dispatch: the final arc

Companion to `widget-framework-v2-review.md` phase 4. That plan's goal —
components REGISTER, events DISPATCH by hit-testing a real tree,
PROPAGATE capture/bubble-style, per-component HANDLERS own behavior —
is DONE panel-locally (`widgets/`: `LayoutBox` arena with
parents/z/flags, `hit_path`, `WidgetImpl` with
`collect`/`box_meta`/`on_key`/`on_wheel`/`on_pointer`, effect channels,
one `kinds::behavior()` dispatch). App-level chrome is halfway: ONE
`chrome_boxes()` tree serves all five pointer gestures, but it is a
central enumeration (no registration), flat (no parents, no bubble),
ordered per-gesture for wheel/click (`WHEEL_ORDER`/`CLICK_ORDER`),
guarded by pre-walk ladders (`dispatch_modal_mouse`, dock captures,
`LayerKind`), with handlers in central match arms. This document is the
audited design for closing that gap. Line references are at branch
commit 1090058; expect drift.

## Design rulings (made once, up front)

**No full capture phase.** Every "capture" consumer in the current code
is geometric (full-frame modal boxes) or a pointer GRAB (press-to-release
drag routing: dock resize, separators, explorer width, tab drag,
scrollbar thumbs). A deepest-first hit-stack scan with per-box
`pointer_opaque` + an explicit grab slot reproduces all of it; the scan
receives the root→target path, so a root-down capture pre-loop is a
five-line addition inside one function if a real consumer ever appears.
Do not build it speculatively — the panel model (which this mirrors)
bubbles and never captures.

**One walk primitive: `hit_stack`, in `widgets/layout_box.rs`.**
`hit_stack(boxes, row, col) -> Vec<usize>` = all boxes containing the
point, ordered by (effective_z desc, depth desc, doc-order desc), where
effective_z = max z along the ancestor path (stacking-context rule).
Ancestors immediately follow descendants, so one linear
consume-or-continue scan IS bubble-up; lower-z stacks follow, so it is
also today's flat fall-through. `pointer_opaque` on a declining box
stops the scan (nothing beneath sees the event). `hit_path` (panels) is
`hit_stack` truncated to the first stack and stays untouched.

**Dispositions: `{Consumed, Pass, PassAfter}`.** `PassAfter` is
required — two live guards act-then-continue (`chrome:transient_guard`
dismisses transients and keeps routing; `chrome:clear_explorer_menu`
likewise). Never model those as consume.

**Same `LayoutBox` type as panels, with a sidecar.** Chrome boxes need
instance payloads (LeafId, ContainerId, popup index) that
`LayoutBox.key: Option<String>` would force into strings:

```rust
struct ChromeBox { lb: LayoutBox, owner: usize /* components() index */, tag: ChromeTag }
```

`hit_stack` operates on the `LayoutBox` projection, shared with panels.

**Panel-local trees mount as delegated subtrees, not grafts.** The
chrome tree holds ONE box per mounted panel (dock, floating, split
widget panels); its component's handlers translate screen→local and
delegate into the panel's own `p.boxes` tree — the pattern the prompt
toolbar already proves (`prompt_toolbar_boxes` + origin offset). Never
graft panel boxes into the chrome arena.

**Rebuild per event, never persist.** The tree is ~30 boxes built from
live state per gesture; per-event freshness is a deliberate, previously
litigated property (staleness bugs were the alternative). Keep it.

## Registration

One `ChromeComponent` per module under `app/chrome/`, one total
registry mirroring `kinds::behavior()`:

```rust
trait ChromeComponent: Sync {
    fn collect(&self, ed: &Editor, t: &mut ChromeTreeBuilder);   // per event, live state
    fn on_pointer(&self, ed: &mut Editor, bx: &ChromeBox, ev: &ChromePointer) -> Disposition;
    fn on_wheel(&self, ed: &mut Editor, bx: &ChromeBox, col: u16, row: u16, delta: i32) -> Disposition;
    fn hover(&self, ed: &Editor, bx: &ChromeBox, col: u16, row: u16) -> Option<HoverTarget>;
    fn on_key(&self, ed: &mut Editor, key: &KeyEvent) -> KeyDisposition { Pass }   // slice 6
    fn layer(&self, ed: &Editor) -> Option<Layer> { None }                          // slice 6
}
fn components() -> &'static [&'static dyn ChromeComponent];       // the ONE list
```

`ChromePointer` carries the press kind (left/right/double/triple) the
way `WidgetImpl::on_pointer` carries `event_type` — per-gesture
behavior lives in the handler, not in four trait methods.

Roster (component → boxes contributed): `context_menu` (menu rect z18 +
opaque close-guard backdrop child), `prompt` (suggestions outer → items
+ scrollbar children; preview; toolbar mount; wheel-capture box;
click scrim), `popups` (per-popup opaque rects → items + scrollbar;
PassAfter dismiss observer), `file_browser`, `floating_modal` (panel +
close button + opaque backdrop), `dock` (column → content, resize
border, scrollbar zones), `menu` (bar → title cells; open: dropdown →
submenu chain as children; close-guard backdrop), `splits` (per-split →
tab bar → tabs, buttons, v/h scrollbars, editor content; separators),
`file_explorer` (→ title/close, rows, resize border; clear-menu
PassAfter guard), `status_bar` (→ segments), `search_options` (→
checkboxes), `base` (z0 full-frame fallback), and — slice 5 — a top
z-band (z20+) of `settings` / `keybinding_editor` /
`calibration_wizard` / `workspace_trust` full-frame opaque+trap
components whose handlers are the existing bespoke dispatchers
(`dispatch_modal_mouse` deletes; the modal INTERIORS stay bespoke —
the component is the dispatch slot, so Settings' later replacement
never touches dispatch again).

Registration relocates the `*Layout` cache READS into component
modules; retiring the caches themselves is per-component: give each a
pure `layout(state, frame) -> rects` that both paint and `collect`
call, then delete the cache field. Cheap for menu / splits / tabs /
search options / status bar (frame + state derived); keep
content-dependent paint products (`view_line_mappings`,
`cell_theme_map`) out of scope; suggestions/popup rects are middle —
one small PR each.

## Ordering arrays → tree properties

Everything in `WHEEL_ORDER`/`CLICK_ORDER` maps to structure; they
differ from plain z in exactly three places:

- menu / context-menu close guards → full-frame `pointer_opaque`
  backdrop CHILDREN of the open menu node (deeper items win; backdrop
  dismisses + consumes).
- popup absorb/guard/transient-dismiss → popups are opaque rects
  (inside-clicks nothing claims die there = absorb); the dismiss half
  is one full-frame PassAfter observer box at the popup band.
- the wheel's high overlay-prompt slot vs click's low scrim slot → the
  prompt component contributes TWO thin full-frame boxes at different
  z: a wheel-capture box (declines pointer) in the overlay band and a
  click scrim (declines wheel) just above the editor band. Per-gesture
  SEMANTICS stay in handlers; per-gesture ORDERING stops existing.

Validation: keep old sort and new scan side by side for one slice with
an order-equivalence assertion over a probe grid × state matrix (the
focus-ring debug-assert precedent).

## Pre-walk ladders

`LayerKind` variants: Settings/KeybindingEditor/CalibrationWizard/
WorkspaceTrust/FloatingModal → modal-band components. Menu/Prompt/Popup
→ already surfaces, become components. The four context-menu variants →
ONE `context_menu` component (they already share `ContextMenuCore`).
Dock → component with grab. EventDebug stays a pre-walk debugging
intercept, deliberately. Editor → `base`. `overlay_layers()` becomes
DERIVED from component `layer()` contributions in z order, keeping
`get_key_context`, the PTY gate, and `popup_blocked_by_higher_modal`
intact; the enum then shrinks to semantic residue as cleanup. Also
staying pre-walk: the terminal mouse-forward sink and terminal-link
Ctrl+click; the theme-info popup special case becomes a small
component in slice 2.

## Keyboard (minimal slice only, this arc)

1. `context_menu.on_key` replaces the `handle_context_menu_key` ladder
   rung: "if the top of the tree's stack owns the keyboard, offer it
   the key" — one rung dissolved, zero behavior change.
2. `dispatch_modal_keyboard`'s find_map re-expressed over derived
   `overlay_layers()`.
3. STOP. Prompt/Popup key blocks have documented fall-through semantics
   and the chrome-wide focus ring (`focused: Option<FocusId>` unifying
   dock focus, `Prompt.toolbar_focus`, popup focus) is gated on
   prompt-as-widgets + Settings. The slot exists: chrome boxes carry
   `focusable`/`focus_trap`, so `focus_ring_scoped` works on the chrome
   tree the day a chrome focus id exists.

## Migration order (each slice green + shippable)

0. `app/chrome/` scaffold: trait, builder, registry; split
   `chrome_boxes()` VERBATIM into per-component `collect()`s (still
   flat, same kinds/z); `chrome_tree()` = loop. Dispatchers unchanged.
   Pure code motion.
1. Hover/right-click/double-click arms move into components
   (`hover()`, `on_pointer`); their three z-sorted walks call
   components instead of matching kind strings.
2. Click + wheel arms move into `on_pointer`/`on_wheel`
   (`click_surface_dispatch`, `wheel_chrome_scroll` dissolve); ORDER
   arrays still drive ordering. Theme-info popup becomes a component.
3. Parent links + `hit_stack`; the three z-sorted gestures switch to
   the shared scan; guards become backdrop/observer boxes for them.
   Side-by-side order-equivalence test.
4. Wheel + click join the scan; dual-box prompt encoding; popups →
   opaque rects; DELETE `WHEEL_ORDER`/`CLICK_ORDER`. Highest-risk
   slice — keep single-purpose. (Wheel e2e suites are string-asserted;
   budget a few fixes.)
5. Pre-walk mouse ladders dissolve: modal-band components, dock
   component, formal grab slot; `dispatch_modal_mouse` and the
   click/rclick dock captures delete.
6. Keyboard minimal slice (above); context-menu `LayerKind` variants
   collapse to one.
7. Geometry-from-layout per component (menu → splits/tabs → search
   options → status bar → suggestions/popups), deleting each `*Layout`
   cache as its pure `layout()` lands. Many small PRs; the ONLY slice
   allowed to change pixels — golden/e2e churn lands here, one cache
   per PR.

Handler destinations (today's arm → module) are enumerated in the
review transcript; the shape: `chrome/menu.rs` also absorbs the
menu-hover state machine from `update_hover_target`; `chrome/splits.rs`
takes separators, buttons, editor-click delegation and the dbl/triple
click scans; `mouse_input.rs` shrinks to event decode, multi-click
detection, grab routing, the walk driver, and drag/up plumbing
(~1,000 lines of dispatchers replaced by a ~150-line driver plus moved
code).

## What NOT to do

- No persisted tree (freshness property).
- No consume-modeling of act-then-continue guards (PassAfter).
- No re-hit-testing during drags (grab slot; the btop-resize bug).
- No grafting panel box trees into the chrome arena (delegate).
- No speculative capture phase.
- No Settings-interior migration, no `KeyContext` semantic changes.
- Keep handlers on plain-data trees (`Vec<ChromeBox>` snapshot,
  component indices) so `&mut Editor` stays available — the
  snapshot-then-dispatch pattern `dispatch_modal_mouse` uses.
- `screen_space` boxes keep resolving against paint-recorded rects
  before panel-space dispatch; the chrome scan must not hit-test them.
