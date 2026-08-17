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

## Keyboard (full registration arc — K slices)

The original ruling here was "minimal slice only" (grabs + the modal
find_map re-expressed over derived layers, then STOP). That ruling is
SUPERSEDED by explicit direction: the main key pipeline must be
registration-based, not a staged hand-ordered ladder. The design
mirrors the pointer side exactly:

- **Pointer**: components `collect()` boxes → `ChromeTreeBuilder`
  stamps each box's owner → `dispatch_pointer` walks `hit_stack`
  calling the owner's `on_pointer`.
- **Keyboard**: components `layers()` declare ranked layers →
  `overlay_stack()` stamps each layer's owner → ONE walk iterates the
  stack top-down calling the owner's `on_layer_key`. Consumed stops
  the walk; declined falls through to the next layer down; the base
  layer terminates it. Interiors stay bespoke (the modal-mouse
  precedent: the component is the dispatch slot); only ROUTING is
  derived.

`on_layer_key(ed, layer, event) -> Option<InputResult>` — `None` =
this layer declines, keep walking; `Some(Ignored)` also keeps walking
(the prompt/popup fall-through contract, preserved as a walk
semantic); any other `Some` stops.

Slices, each green + shippable:

- **K1**: `overlay_stack()` (owner-stamped layers, `overlay_layers()`
  derives from it); `on_layer_key` on the trait; the walk replaces
  `dispatch_modal_keyboard`'s four-kind find_map ladder — Settings /
  KeybindingEditor / CalibrationWizard arms into `modals.rs`, Menu arm
  into `menu.rs`. EventDebug's hardcoded head has owner `None`
  (pre-band debugging instrument, unchanged).
- **K2**: the prompt block (file browser → query-replace confirm →
  overlay toolbar focus ring → prompt) into `Prompt`'s
  `on_layer_key`. Prompt-first because the block order (prompt before
  popups) matches the rank order (850 > 840) — migrating top-down
  keeps the walk sequence identical to the block sequence at every
  intermediate step.
- **K3**: the popup block (completion resolver → workspace-trust keys
  → global popups → buffer popups) into `Popups` / `WorkspaceTrust`
  handlers. Note: moving the WT rung to its 870-ranked layer makes WT
  keys beat an open prompt (the block ran the prompt first) — that
  ALIGNS dispatch with `get_key_context`, which already resolves WT
  above Prompt; the old divergence between context and dispatch is
  the bug class this arc exists to kill. The transient-dismissal
  stage stays pre-walk BY RULING: it must observe every key even when
  a higher modal consumes it (typing under Settings still dismisses a
  hover popup), which no first-consumer walk can express — it is a
  pre-band observer like event-debug. The unfocused-popup resolver
  migrates into `Popups` (a non-owning layer still gets visited;
  higher owners consuming replaces its hand-rolled
  `popup_blocked_by_higher_modal` guard).
- **K4**: the tail — mode bindings, composite router, chord/keybinding
  resolution — becomes `Base`'s `on_layer_key`; `handle_key` reduces
  to the pre-band (event-debug, terminal input, getNextKey capture,
  `on_key` grabs) plus THE walk.

Still gated on prompt-as-widgets + Settings: the chrome-wide focus
ring (`focused: Option<FocusId>` unifying dock focus,
`Prompt.toolbar_focus`, popup focus). The slot exists: chrome boxes
carry `focusable`/`focus_trap`, so `focus_ring_scoped` works on the
chrome tree the day a chrome focus id exists.

## R wave — duplicate-mechanism / partial-migration fixes

A three-lens adversarial audit (half-wired capability surface,
duplicate encodings, partial migrations) of the completed arcs found
the offense class of the mid-arc review surviving in new places. All
fixed:

- **R1 — grab-band rank inversion.** Dock and FloatingModal keyboard
  grabs moved from the pre-band `on_key` loop onto `on_layer_key`,
  gated on `layer.owns_keyboard`. The grab class outranks every
  `layer_rank` by pipeline position, so their grabs beat an open
  prompt/menu/popup while `get_key_context` said otherwise (dock
  focused + prompt open: Esc blurred the dock instead of cancelling
  the prompt). Pre-band membership is now restricted BY RULING (trait
  doc) to whole-pipeline observers (ThemeInfo) and custom-dispatcher
  modals (ContextMenu, whose rank is deliberately not its keyboard
  precedence).
- **R2 — modal surfaces claim the horizontal wheel.** Prompt (overlay
  modal + suggestions), Popups, and FileBrowser gained `on_hwheel`
  absorb arms mirroring their vertical modal claims — Shift+wheel no
  longer pans the buffer beneath them. Fold-toggle double/triple-click
  moved from `handle_mouse`'s pre-walk into `Splits::on_pointer`, so
  popup opacity and the overlay swallow block it by construction.
  Panel-clipped `Popup { screen_space: false }` now actually promotes
  (`promotes_as_overlay`, one predicate for the absorb loop and both
  row-budget filters) — its rows float, hits stamp overlay, boxes get
  the z bump that arms `pointer_opaque`; regression test added.
- **R3 — capture order derived from ranks.** The `capture_mouse` loop
  walks the owner-stamped `overlay_stack()` instead of registry
  order — the registry-order duplicate of the precedence (two
  hand-synced encodings, comment-only sync) is deleted; rank is the
  one source for keyboard and capture alike. FloatingModal's
  unreachable box + `on_wheel` and the handler-less `chrome:dock` box
  deleted (dead geometry).
- **R4 — release grab-keyed; forward-sink derived.** Mouse-up
  dispatches on `pointer_grab` with per-grab finalizers (dock width
  persist, tab drop, separator relayout) + a blanket sweep, matching
  the Drag arm. Terminal mouse-forwarding is additionally suppressed
  by a DERIVED check — any `pointer_opaque` chrome box over the cell
  (popups over an alt-screen terminal no longer leak clicks into the
  PTY); the context-menu suppression stays a named check by ruling
  (its boxes are deliberately non-opaque).
- **R5 — one encoding per fact.** `bottom_row_flags` replaces four
  hand-copied spellings of the bottom-row visibility conditions;
  `context_menu_core_mut` derives WHICH menu from the immutable
  walk's kind (one precedence encoding, exhaustive-match re-borrow);
  the status-bar clickable roster documented as compile-checked at
  both ends (exhaustive matches, no wildcards);
  `CollectedOutput::shift_channels` moves all six geometry channels
  in one call (labeled section), and `assemble_wrapped_row` now
  threads embeds instead of silently dropping them.
- **R6 — rulings written.** `update_lsp_hover_state` (content-tracker
  beside the walk; redundant context-menu check deleted — its layer
  already drives `modal_overlay_active`), terminal-link hover (same
  seam), `HitArea.context_click` dock-only consumer scope,
  `PassAfter`-vs-opacity contract, and the stale
  "Settings/Menu/Prompt" trio enumerations.

- **R7 — dead old-model code deleted; paint gates derived.** A
  zero-caller audit of the whole event surface found and deleted the
  survivors: `popup_mouse.rs`'s click/hover/drag dispatch half
  (`PopupClickResult`, `hit_test_click`, `hover_target`,
  `content_position`, `handle_popup_selection_drag` — already
  diverged from the chrome Popups handlers that replaced them; only
  the acknowledged `is_over_*` rect predicates survive),
  `ScrollbarState::click_to_offset` (documented-buggy, zero
  production callers), the `InputHandler` hierarchy half
  (`focused_child*`, the no-op child-first dispatch branch,
  `InputResult::or`, the four `is_key*` helpers — no handler ever had
  a child; the module doc now describes the real model: flat
  per-surface interiors invoked from components, routing in the layer
  walk), and the write-only `drag_start_popup_scroll` field. The
  LSP-rename cancel got its pre-band whole-channel-observer ruling.
  `cursor_suppressed_by_late_overlay` is DERIVED from the overlay
  stack (its seven-item hand list had drifted from `hide_cursor`'s,
  which now consumes it — fixing both lists' omissions: the caret no
  longer blinks through the calibration wizard, trust prompt, context
  menus, or the centered modal), and the inline WorkspaceTrust
  resolver matches collapsed onto `workspace_trust_on_top()`. Stale
  docs fixed: `capture_mouse` (rank-order offering, post-R3),
  `StatusBarClickable` (retired cache reference), the
  `overlay_hit_test` ghost.

- **R8 — inline-review wave.** Drag-arm bodies moved to their owning
  components (grab-keyed routing only in the walk file); dedup keyed
  on `(owner, kind)`; ONE `chrome_tree` per mouse event; `handle_key`
  degrades instead of panicking with the base-layer contract under
  test; capture/layer gates paired on one activity predicate per
  modal; first unit tests under `app/chrome/` (rank relations,
  distinctness, base-layer tail).
- **R9 — the must-land list (review issue-comment).** (1) RULING: the
  chrome "tree" is currently FLAT — no component sets `parent`, the
  full-frame guard boxes are the deliberate flat-world containment
  encoding, and `parent`/`focusable`/`focus_trap`/`scroll` are
  reserved-but-unset at chrome level (documented at
  `ChromeTreeBuilder`; parent links + chrome focus are #3024). (2)
  The per-kind key/value policies became CAPABILITIES:
  `arrows_advance_focus` (Button/Toggle), `picker_nav` (List peeks,
  Tree takes focus), `activates_on_picker_enter` +
  `picker_activate_event` (List/Tree), and the plugin value-set
  mutation arms delegate to kind fns (`number::set_value_state`,
  `dropdown::set_index_state`, `dual_list::set_included_state`) —
  the router and dispatch arms hold no kind matches. (3) Overstated
  claims trimmed in the PR body. (4) Precedence GOLDEN tests
  (`e2e/dispatch_precedence.rs`): wheel and horizontal wheel over a
  popup never move the buffer; a click on popup chrome never moves
  the buffer cursor; workspace-trust keys beat an open prompt. (5)
  THE closed `screen_space` roster lives on `ChromeLayout`'s doc —
  additions require a ruling here. (6) RULING: the keyboard walk
  does NOT get one-stack-per-event — handlers may mutate then
  decline, so the handler-level `get_key_context` rebuilds are
  load-bearing freshness, not waste (documented at
  `dispatch_layer_keyboard`; invalidation-aware derivation is
  #3024). (7) The `PassAfter`-vs-opacity walk contract is code
  (`pointer_walk_step`, unit-tested), not prose.

- **R10 — validated memo for `chrome_tree` (perf slice).** The
  per-event derivation cost is amortized WITHOUT a hand-maintained
  invalidation roster. RULING (learned the hard way — a first cut
  keyed both derivations on a counter bumped at "every mutation
  funnel", and CI's debug oracle failed 82 tests across four families
  of unbumped paths: plugin dispatch opening modals, dock
  focus/blur, terminal-mode `key_context` flips, context-menu
  dismissal): the set of Editor APIs that can flip a layer predicate
  is unbounded, so a counter alone is exactly the enumerated-roster
  antipattern this model rejects. Staleness must be CHECKED, not
  trusted. The design: `overlay_stack` is deliberately NEVER memoized
  — it is ~17 cheap activity predicates and is itself the ground
  truth. `chrome_tree` (the expensive derivation) caches
  `(ui_gen, stack_snapshot, tree)` and reuses only when the coarse
  `ui_gen` epoch matches AND a fresh stack build equals the snapshot
  — presence/claim changes from ANY path invalidate by derivation.
  `ui_gen` covers the geometry epoch (paint caches move only under
  `render`/`relayout`) and is bumped coarsely at the event funnels:
  `handle_key` entry, `handle_mouse` exit when the event reported
  `needs_render` (a quiet motion stream reuses one tree across MANY
  events — the case the slice exists for), `handle_action`,
  `process_deferred_actions`, `relayout`, `show_popup`/`hide_popup`,
  `editor_tick` when it did work, `render` at its end. Each actual
  rebuild advances `ui_tree_seq`; the hover-cell memo in
  `update_hover_target` keys on `(tree_seq, col, row)` so it inherits
  the validation, collapsing terminal motion bursts to one hover walk
  per cell. Debug builds still oracle-check every memo hit against a
  full rebuild (`debug_assert_eq!`), keeping the whole scheme a
  checked invariant. The keyboard walk's handler-level rebuilds stay
  UNTOUCHED per the R9 ruling (mutate-then-decline).

Remaining recorded residue after R: `is_mouse_over_any_popup`'s
parallel rect query (acknowledged in-tree, blocking-safe), and the
federated widget/chrome trees seam + hand-ordered registry data (the
two open structural items — the forward-design PR #3024).

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

## Execution status

Slices 0-4 (dispatch), 5a (modal capture through the registry —
`dispatch_modal_mouse` deleted), and 6a/6b (keyboard grabs;
`overlay_layers()` derived from per-component `layers()` with
explicit `layer_rank`s; the four context-menu `LayerKind`s collapsed
to one) are DONE.

Keyboard registration arc: **K1 DONE** (`overlay_stack()` owner
stamping, `on_layer_key`, the walk replacing
`dispatch_modal_keyboard`'s four-kind ladder), **K2 DONE** (prompt
block → `Prompt::on_layer_key` / `dispatch_prompt_key`), **K3 DONE**
(popup block → `Popups::on_layer_key` / `dispatch_popup_keys`; WT
rung → `WorkspaceTrust::on_layer_key`; the unfocused-popup
interception moved from `handle_key` into the popup rungs with its
`popup_blocked_by_higher_modal` guard kept for byte-identical
precedence; `dispatch_modal_input` deleted — `handle_key` calls the
walk directly; `on_layer_key` gained the anyhow error channel),
**K4 DONE** (the pipeline tail — mode bindings, composite routing,
chord/keybinding resolution — is `Base::on_layer_key` /
`dispatch_base_key` in `chrome/base.rs`; the base layer always
answers, so the walk always terminates; `handle_key` is the pre-band
— event-debug, terminal input, getNextKey capture, `on_key` grabs,
the pre-walk transient-dismissal observer — plus ONE walk call).
The main key pipeline is now registration-based end to end: a new
surface registers a component, declares a ranked layer, and its
keyboard routing exists. Remaining, in order:

- **DONE — dock captures + grab slot + opacity gate.** The dock's
  click/right-click routing is the Dock component's boxes and arms
  (`chrome:dock_border` starts the width-resize grab,
  `chrome:dock_column` clicks/context, `chrome:dock_blur` PassAfter
  observer at 195); the three precedence caveats resolved — the walk
  already runs after the LSP-rename-cancel hook and terminal-forward
  sink, and a centered popup over the dock column now wins by z
  (deliberate fix). `chrome::PointerGrab`/`pointer_grab()` names the
  press-to-release drags; the terminal-forward suppression consults
  it. The scan's opacity gate covers the pointer gestures (wheel
  exempt for scroll chaining); `chrome:popup_absorb` is deleted —
  absorb is the popups' own `pointer_opaque`.
- **Modal per-gesture decomposition — DESIGN-INTERMEDIATE, not
  debt.** `capture_mouse` stays whole-channel: the modal interiors
  are bespoke by ruling (the component is the dispatch slot), and
  their drag/release handling would need the full drag-routing
  decomposition first. Revisit only if a modal ever needs to share
  its surface with chrome beneath.
- **Geometry-from-layout (7).** Per component, one cache per PR, the
  only pixel-touching slice. Findings from the first scoping pass:
  `render_search_options`' geometry is checked-state-independent
  ("[x]"/"[ ]" same width) — it depends on area, confirm_each
  presence, locale labels, and keybinding hint strings, so a pure
  `SearchOptionsLayout::compute(area, confirm_shown, keybindings)`
  is extractable with paint consuming it; BUT every cache retirement
  also needs its AREA derivation hoisted (the bar's rect comes from
  the frame-layout decisions in `render`), which is the real
  per-cache cost. The status bar's `clickable` segments are
  content-dependent (rendered label widths: encoding, LSP state) —
  middle difficulty, not cheap. The menu's geometry is interleaved
  through `MenuRenderer::render` + `render_dropdown_chain` +
  `render_dropdown_level` (~800 lines) — the largest single hoist.
  `ContextMenuCore::rect` is already pure (the model). Order the
  series: search_options → tabs/status bar → menu →
  suggestions/popups.
- **7a: search_options — DONE.** `SearchOptionsLayout::compute(area,
  use_regex, confirm_shown, keybindings)` is the pure span math;
  `Editor::search_options_layout_now()` derives the AREA from live
  state (`compute_dock_split` + the same bottom-up prompt-row math
  `render` uses) and feeds it to `compute`. Two debug asserts pin
  the derivation: the paint walk in `render_search_options` must
  equal `compute()` on the same inputs, and the row `render` paints
  at must equal the row `search_options_layout_now()` derives.
  `ChromeLayout.search_options_layout` deleted; consumers (chrome
  component collect+hover, `handle_click_search_options`, scene.rs
  web projection) all call `search_options_layout_now()`.
- **7b: status bar — DONE.** The bar's geometry is content-dependent
  (rendered label widths: encoding, LSP state, cursor position,
  messages), so the hoist splits differently from 7a:
  `Editor::with_status_bar_ctx` gathers every input from live state
  (the ~100-line construction the paint pass used, now shared) and
  `StatusBarRenderer::compute_status_layout` runs the painter's own
  element/width/placement walk frame-free (`render_status` takes
  `Option<&mut Frame>`); `status_bar_area_now()` (`&self`) re-runs
  the actual vertical `Layout` split so small-terminal squeeze
  behavior matches by construction. Two debug asserts pin paint ==
  derivation (area, and clickable+plugin-token geometry).
  `StatusBarChrome.clickable` / `.plugin_token_areas` deleted;
  consumers (chrome component hover, `handle_click_status_bar`, the
  four popup-anchor sites in popup_dialogs) call
  `status_bar_layout_now()` / `status_bar_clickable_area_now()`.
  `StatusBarChrome` keeps `area` + `segments` as the web's semantic
  PAINT capture (`status_view` mirrors the painted frame — paint
  output, not event geometry; same ruling as the overlays channel).
  Trait ruling: `ChromeComponent::hover` now takes `&mut Editor`
  like the pointer handlers — live-state geometry can lazily load
  buffer chunks (the cursor-column segment) — while `collect` stays
  `&Editor` (boxes need only `*_area_now()` rects, keeping tree
  collection read-only).
- **7c: menu — DONE.** Smaller than scoped: the renderer already
  computed layout paint-free for the web (`draw: bool`), so the
  hoist was mechanical — `MenuRenderer::compute_layout(screen,
  area, …)` wraps the walk with `frame: Option<&mut Frame>` (the
  only non-draw frame uses were `frame.area()` dimension reads, now
  the `screen` param). `Editor::menu_layout_now()` is `&self`
  (every input the render gathered is a `&self` read + lock), so
  ALL consumers rewired without signature changes: chrome Menu
  collect+hover, `compute_menu_dropdown_hover`,
  `handle_menu_dropdown_click`, `handle_click_menu_bar`, and the
  web `menu_view`. `ChromeLayout.menu_layout` deleted; parity
  oracle in `render_menu_bar` compares the whole `MenuLayout`
  (bar_area included). Freshness note: `menu_layout_now` reads
  `expanded_menus_cache` refreshed by the paint pass — the same
  content source and staleness class the retired cache had.
  Consolidation: `chrome_rows_now()` now derives all five chrome
  rows via the actual `Layout` split with condition-computed
  constraints; `status_bar_area_now` / `menu_bar_area_now` gate on
  their row's visibility CONDITIONS (not chunk height — a squeezed
  zero-height row must still round-trip for parity) and pick their
  chunk. Remaining 7 series: tabs/status-bar segments are done;
  suggestions/popups rects are the tail (their rects are
  paint-positioned popup geometry — screen_space class, may
  legitimately stay paint-recorded; decide when reached).
- **7d: suggestions/popups rects — RULED paint-recorded, slice 7
  CLOSED.** The remaining `ChromeLayout` rects (`popup_areas`,
  `global_popup_areas`, `suggestions_area` / `suggestions_outer_area`
  / `suggestions_scrollbar_rect`, `prompt_results_area` /
  `prompt_preview_area`) stay paint captures, deliberately. The
  distinction from the retired caches: search-options / status-bar /
  menu geometry was derivable from live state WITHOUT paint (frame
  split + content walks), so recording it created a second,
  stale-able source. The popup channel's rects are anchored to
  paint-produced text layout: buffer popups position at
  `viewport.cursor_screen_position(&mut buffer, …)` offset by the
  split's `content_rect` from `WindowLayoutCache.split_areas` — the
  visual-row wrap maps and split rects that exist only as the paint
  pass's output. An event-time derivation would still resolve
  against those recordings, adding a second code path without
  removing the paint dependency — exactly the `screen_space` class
  the plan already rules on ("screen_space boxes keep resolving
  against paint-recorded rects"). Global popups alone
  (`calculate_area(size, None)` is pure) COULD derive live, but
  splitting one channel across two geometry sources buys nothing:
  every popup push/pop/scroll requests a repaint, and the pointer
  handlers (`handle_click_global_popups`,
  `is_mouse_over_any_popup`, the dismiss guards) re-check the LIVE
  stacks before acting, so a stale rect can miss for at most one
  pre-repaint event — it can never act on a vanished popup. The
  suggestions rects are the same shape: geometry produced inside
  `SuggestionsRenderer` interleaved with truncation/scroll state,
  consumed by handlers that re-check the live prompt. Slice 7 (and
  the chrome plan's execution arc) is complete: every retirable
  geometry cache is retired, and what remains recorded is recorded
  by ruling, not by accident.
  The re-audit sweep named three more members of this ruled class
  that the list above omitted — same reasoning applies to each:
  `workspace_trust_dialog` (paint-recorded dialog layout; the modal
  band's handlers re-read the live dialog state before acting, so a
  stale rect can misplace at most one pre-repaint click),
  `prompt_toolbar_boxes`/`prompt_toolbar_origin` (a paint-recorded
  `LayoutBox` tree — literally the `screen_space` clause), and
  `Window.file_browser_layout` (paint-recorded dialog layout; the
  FileBrowser component's no-layout-yet full-frame absorb fallback
  is the staleness class handled explicitly). Also in this class by
  construction: the floating panels' paint fields (`last_inner_rect`,
  `scrollbar_tracks`, `popup_rect`/`popup_hits`, entries/overlays).

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
