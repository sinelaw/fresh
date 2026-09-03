# Second review: the `fresh-ui` editor migration, against the library's own goals

> Independent adversarial review of branch `claude/fresh-editor-fresh-ui-migration-glu9af`
> (PR #3028) at commit `bb4de04`, with a note on uncommitted work in the tree.
> The yardstick is [`crates/fresh-ui/README.md`](../../crates/fresh-ui/README.md)
> and [`widget-library-design.md`](widget-library-design.md), not
> [`fresh-editor-ui-migration.md`](fresh-editor-ui-migration.md), whose claims are
> treated here as assertions to verify.

**Verdict up front.** The frame swap (S1/S1b) and the context-menu wave are genuinely
good work and genuinely on-goal. The last three commits are not. `d318955` (two-band
fold) solves a real, forced problem with a mechanism that is already known-broken for
the next two stages; `a43dd92` and `7a478a0` moved the menu bar onto the tree while
leaving a second geometry authority, a second walk, a second provenance path and a
`debug_assert` holding two derivations in step — all of which the commit messages claim
were removed. Four distinct correctness bugs introduced by `7a478a0` are being fixed in
the working tree *now* — a click that opened and immediately shut every menu, rows that
painted through their own border, styles inherited from whatever was underneath, and a
`Move` claim that killed hover, drags and plugin hooks — after the commits landed and
after all 52 shell tests went green. That is the clearest available evidence that the
"cell-identical acceptance bar" is not enforced by anything except the e2e suite.

The single most important item: **`OVERLAY_FAMILIES` should be replaced by a
`layers_from` index on `LayoutSpec` before S3's modals or S4's first library `Dropdown`,
both of which it is already wrong for** (§Q2).

## 0. State of the tree while this was written

`git status` at review time shows uncommitted work in progress, which grew while this
review was being written:

```
 M crates/fresh-editor/src/view/shell/fold.rs      (+ Style::reset() before patching)
 M crates/fresh-editor/src/view/shell/menu.rs      (+ e.stop() on label/row clicks, exact-list
                                                     tests, claim_move() deleted)
 M crates/fresh-editor/src/view/ui/menu.rs         (+ fit(): pad-or-truncate every dropdown row)
 M crates/fresh-editor/src/app/mouse_input.rs      (+ shell_owns_hover gate in update_hover_target)
 M crates/fresh-editor/src/app/shell_host.rs       (+ set shell_owns_hover on MenuHover)
 M crates/fresh-editor/src/app/mod.rs              (+ the flag)
 M crates/fresh-editor/src/app/editor_init.rs      (+ its init)
```

All of these are fixes for bugs `7a478a0` introduced. They are called out below where
relevant so that nothing already fixed is reported as open. Everything else in this
review is against `bb4de04`.

`cargo test -p fresh-editor --lib view::shell` passes (52 tests). The e2e suite was out
of scope by instruction; one finding below (§Q4.2) predicts an e2e-invisible live-app bug and
names the test that cannot see it; the drag finding in §Q5 was independently fixed in the
working tree mid-review.

---

## 1. The goals, as the library states them

From [`crates/fresh-ui/README.md`](../../crates/fresh-ui/README.md) lines 22–41,
verbatim:

> 1. **One library for everything on screen.** Menus, prompt, popups, context
>    menus, dock, splits, modals, status bar and plugin panels are built from the
>    same primitives with the same event and layout rules.
> 2. **Generic registration and propagation, with no hand-specified exceptions.**
>    Precedence, modality, dismissal and focus order are *derived* from structure
>    and declared properties, never read from a central ordered list.
> 3. **Rebuilding a description costs one allocation per node.** No registration,
>    no resource acquisition, no side effects — so there is no incentive to avoid
>    rebuilds or to mutate retained state directly.
> 4. **Identity is explicit.** Which on-screen object is which across frames is
>    decided by tree position and an author-supplied key, both inspectable —
>    never by hashing an identifier stack into a side table.
> 5. **One source of geometry.** Layout computes rectangles; hit-testing,
>    painting and tests read them. Geometry is not derived during paint.
> 6. **Composition is the only extension mechanism.** The primitives available to
>    library code are the primitives available to user code.
> 7. **Backend independence.** Paint produces a display list; TUI cells, the web
>    DOM and test assertions are consumers of that list.

Two supporting rules from the design spec are load-bearing below:

- §3, *Invariant*: "descriptions carry no geometry. A description type with a rect
  field indicates layers 1 and 3 have been merged."
- `schedule.rs:677-680`, on `Ui::rect`: geometry is "Never [readable] during `build`:
  build is a function of the description, state and ambients, and reading geometry from
  it would make build depend on layout, which depends on build."

---

## 2. The earlier review: what was closed, what was not

There is no earlier review *file*. The prior independent review's findings live in
commits `641a99f`, `fc077b0`, `2860b15` and in §6.2 of the migration doc. Audited
against the current source:

| Prior finding | Status now |
|---|---|
| Claim inferred from "did any message come back" | **Closed and held.** `Dispatch::claimed`, `shell_host.rs:325`. |
| Event-time geometry built a throwaway `Ui` per query | **Closed, then partly undone.** `shell_region_now` reads the retained tree (`render.rs:2372`), but `a43dd92` replaced the single display-list scan with **seven** `find_by_key` calls (`frame.rs:253-256`), and `find_by_key` is a full DFS of the element tree (`render/layout.rs:761-777`). `shell_region_now` then discards six of the seven. Cost per pointer event went up, not down. |
| Dead `pub` code surviving because nothing warns | **Regressed.** `MenuLayout::{menu_at, item_at, submenu_item_at, hit_test}` and `MenuHit` lost their last non-test callers in `7a478a0` and are still exported (`view/ui/mod.rs:61`). Same failure mode, same file family, one commit later. |
| Shell dispatched before the modal capture band | **Closed and held** (`mouse_input.rs:107-121`). |
| The retained tree must not be silently replaced | **Closed and held** (`render.rs:199-201`, `expect` not `unwrap_or_default`). |
| The caret was being dropped | **Half-closed.** It is taken and `debug_assert`ed (`render.rs:1032-1037`) — and then discarded with `let _ = shell_caret;`. See §Q2.3. |
| "A wave may split by channel, **and must say so** in the surface's own module doc" | **Violated by the very next wave.** `view/shell/menu.rs:9-15` still reads "**Paint only, for now.** The levels carry no modality, no dismissal and no handlers: pointer input still runs through `chrome::Menu`'s boxes and the full-frame `chrome:menu_close_guard`." Every clause of that has been false since `7a478a0`. The discipline installed to stop split waves rotting lasted one commit. |
| Parity test's doc was false | **Closed** (`tests/ui_shell_frame_parity.rs:18-22` now says "a pin, not an oracle"). |

---

## 3. Goal-by-goal alignment

### Goal 1 — one library for everything on screen: **honoured, with a scope caveat**

Context menus and menu-bar dropdowns are ordinary `Layer`s in the frame tree
(`frame.rs:201-209`), not separately-ranked surfaces. `render_context_menus`,
`render_context_menu`, `MenuRenderer::render` and `render_dropdown_level`'s painting are
all gone. This is real.

The caveat: the tree is still fed *descriptions computed by the old renderer*
(`MenuLayout::shell_bar`, `MenuLayout::shell_dropdowns`, `view/ui/menu.rs:40,47`). The
surfaces are expressed in the library's primitives but authored by the legacy walk. That
is a defensible transition shape; it is not what the commit messages say (§Q6).

### Goal 2 — no hand-specified exceptions: **violated, and the violation is new**

`frame::OVERLAY_FAMILIES` (`frame.rs:146`) is a hand-maintained central list of names
that authors must edit to add a surface, and getting it wrong makes the surface silently
invisible. The README's exact words are "never read from a central ordered list that
authors must edit to add a surface". `OVERLAY_FAMILIES` is unordered rather than ordered,
which is the only part of that sentence it escapes.

Worse, it is a list of *editor-declared* names being used to answer a *library*
question ("which items came from a layer?"). The library knows the answer exactly
(`schedule::pending_layers`) and does not expose it. Detail in §Q2.

Elsewhere goal 2 is honoured well: `Dismiss::OUTSIDE_POINTER` really did replace
`chrome:menu_close_guard` and `chrome:context_menu_close_guard`; `Modality` really did
replace `blocks_terminal_input` reasoning for the menus (modulo the PTY gate, which is
honestly documented at `chrome/context_menu.rs:23-36`).

### Goal 3 — rebuild is cheap and side-effect-free: **bending**

`frame_tree` itself is pure. But the *inputs* to it are not cheap and the pipeline that
produces them is not side-effect free:

- `shell_frame` (`render.rs:2322`) runs `menu_layout_now()`, a complete menu walk
  (bar labels, mnemonic resolution, the whole open dropdown chain, `dropdown_item_text`
  per row) allocating a `String` per run and per row, **every frame**.
- `record_menu_theme_runs` (`render.rs:2620`) runs the *same walk again*, every frame,
  for `CellThemeRecorder` provenance.
- In debug builds a third walk runs inside `debug_assert_eq!(self.menu_layout_now(), …)`
  (`render.rs:2635`).

So the frame costs 2 full menu walks in release and 3 in debug. The migration doc
demoted the rebuild benchmark from an M0 exit gate to "a performance question to answer
with a profile" (§6.2 item 6) — that was defensible when nothing had migrated; it is
less defensible now that the per-frame cost is measurably tripled for one surface and
the same pattern is queued for the status bar (~3,200 lines of drop heuristics) next.

Also: building the menu description mutates nothing, but it *reads* `expanded_menus_cache`,
which `record_menu_theme_runs` refreshes later in the same frame (`render.rs:2601-2606`) —
so the description is built from a cache one frame stale, by design, and the code says so.

### Goal 4 — identity is explicit: **honoured**

`region_key(r)` is `Key::Pair("region", r.id())` (`frame.rs:229`); layer keys are
`Key::Str("context_menu")` and `Key::Pair("menu_dropdown", depth)`. All author-supplied,
all inspectable, no hashed identifier stacks, no side tables. The plugin-side
`item_keys` deprecation (`widgets/keying.rs`) is the same goal pushed across the plugin
wire a release ahead of when it is required — good, well-scoped, well-tested work.

The one wrinkle is that the same key namespace now carries three unrelated meanings —
reconciliation identity, geometry lookup name, and *paint-band membership* — and only
the first is the library's. See §Q3.

### Goal 5 — one source of geometry: **honoured for the frame and the context menu; violated for the menu bar and the dropdowns**

Honoured, and impressively so:
- the frame's five rows and sidebar carve are the tree's (`render.rs:188-207`);
- `clamped_position` is deleted and `Fit::CLAMP` does the same arithmetic. I checked
  this claim against the deleted code (`git show baf36c9 -- app/types/context_menu.rs`):
  old `if x + w > sw { sw.saturating_sub(w) } else { x }` vs new `x.min(fw-sw).max(0)`
  (`render/layout.rs:816-822`) — identical, including the zero floor. The claim is true;
  `menu_rect` (`context_menu.rs:164`) then reads the single answer back for the web
  `Scene`. This is the model working exactly as advertised.

Violated:
- **The dropdown box's rectangle is computed twice.** `fit_dropdown_area`
  (`view/ui/menu.rs:1074-1098`) decides `x/y/width/height`; the layer is then anchored
  at that point with default `Fit` (no clamp, no flip — `desc.rs:443-455`) and the tree
  lays out its own box from `w(Cells(level.width))` and the row count. Nothing asserts
  the two agree. The web `Scene` reads `dropdown_box`/`item_areas` from the legacy walk
  (`scene.rs:218-240`) while the TUI paints from the tree — the exact two-frontends,
  two-geometries situation the context menu wave eliminated, left in place one commit
  later.
- **`build()` reads last frame's layout.** `shell_frame` → `menu_layout_now`
  (`render.rs:2438`) → `menu_bar_area_now` → `shell_region_now` → `regions_of(self.shell_ui, …)`,
  and `shell_ui` has not been re-framed yet at that point (`ui.frame` is at
  `render.rs:203`, `shell_frame` is called at `render.rs:188`). So the menu's content and
  the dropdown chain's absolute positions are computed against the *previous* frame's bar
  rectangle. On the first frame after startup the tree is empty and the bar rect is
  `0×0`; after a dock toggle, `bar_area.x` is stale by the dock width for one frame,
  which moves an open dropdown by that much. This is precisely the loop the library
  refuses to allow inside `build` (`schedule.rs:677-680`) — evaded only because the read
  happens just outside `ui.frame`, where the `building` debug-assert cannot see it.

### Goal 6 — composition is the only extension mechanism: **honoured**

Nothing in the migration reaches past the public primitive set. `HostPainter` and
`Palette` are editor-side traits over the display list, not library extension points.
The one library change taken (`Dispatch { msgs, claimed }`, `7b0e72b`) was correctly
diagnosed as an internal asymmetry — `propagate_all` already computed it and threw it
away — and taken as a library change rather than an editor convention. That is the
migration's best single decision and the rule it derived from it ("when the editor finds
itself inferring something about routing, that is a missing library capability") is
right. It is then *not applied* to `overlay_start` (§Q2), which is the same shape of gap.

### Goal 7 — backend independence: **honoured in shape, leaking in practice**

The suppression fix in `baf36c9` is the goal working: the description is built whether
or not cells are wanted, and only the fold is skipped (`render.rs:1013`, `2350-2352`).
Two frontends, one layout. Good.

Leaking:
- `ShellPalette::style` (`shell_host.rs:214-247`) is a hand-written 18-arm match that
  must grow one arm per migrated surface. That is the backend's job by design, so it is
  on-goal — but it is on a trajectory to become a second central table, and the
  `dyn:N` proposal (§Q7) would make it a table *plus* a per-frame side map.
- The fold ignores `Item::rect` when drawing `Draw::Lines` (`fold.rs:187-196`): it walks
  the string from `rect.x` and stops only at the inherited `clip`, which is not narrowed
  by ordinary boxes. An over-long run paints straight through its own box's right border.
  The working tree fixes this **editor-side**, by padding-or-truncating every dropdown row
  to the exact cell width in the description (`view/ui/menu.rs`, new `fit()`), which means
  every future migrated surface must hand-fit every string to its rect. The library's own
  two backends have the same bug (`examples/interactive.rs:257-264`,
  `tests/support/screen.rs:104-111`), so by the migration's own rule this is an internal
  asymmetry — the item declares a rect and the backends ignore it — and belongs in the
  library, or at minimum in `fold`, not in each description.

---

## 4. The numbered questions

### Q1 — goal-by-goal alignment

Answered in §3. Summary: goals 1, 4, 6 honoured; 3, 5, 7 bending with named violations;
goal 2 violated by `OVERLAY_FAMILIES`.

### Q2 — the two-pass fold (`d318955`)

**The two bands are legitimate and forced. The way the cut is found is a hack, and it is
already broken for the next stage.**

The forcing argument is correct and I could not find a way around it: there is one
display list and many legacy painters, the legacy painters are not in the list, so one
pass can only sit on one side of them. The file explorer painting first among the legacy
painters really does mean a single late fold cannot ever let it migrate. Splitting the
fold is the right move, and `render.rs:215-241` / `996-1038` place the two passes
correctly. `Band` will collapse back to one pass when the last region goes native. As a
transitional mechanism the *shape* is fine.

The **derivation** is where it goes wrong.

#### Q2.1 Deriving the cut by matching key families is unsound

`overlay_start` (`fold.rs:125-132`) takes the minimum `start` over `spec.index` entries
whose key name is in `OVERLAY_FAMILIES`. This is exact only under an invariant nobody
enforces: *every layer that can appear in this tree carries a key whose family name is in
that list*. The library guarantees "layers paint last" (`render/paint.rs:26-29`) — it
guarantees nothing about how the editor recognises them, and the doc comment on
`overlay_start` conflates the two ("exact as long as layers paint last, which the library
states"). Layers painting last makes the *tail* contiguous; it does not make
`OVERLAY_FAMILIES` complete.

Four concrete break cases, in increasing order of how soon they arrive:

1. **A layer with no key at all, or a key the editor did not write.** `spec.index` only
   gets an entry for keyed subtrees that painted at least one item
   (`paint.rs:113-118`); an unkeyed layer contributes nothing to it. Its items then fall
   before the cut, get painted in the `Background` band, and are overpainted by every
   legacy painter. Silently.

   **This is not hypothetical: `fresh_ui::widgets::Dropdown` opens exactly such a layer**
   (`crates/fresh-ui/src/widgets/menu.rs:131-145` — `layer().anchor(Anchor::Parent).place(Below)…`,
   no `.key()`). The next stage, S4, is "dock column, file explorer, plugin panels", and
   plugin panels and settings forms are the natural first users of the library's own
   `Dropdown`. The first one that mounts will have an invisible pop-over and no test will
   say so.

2. **A layer with a scrim — i.e. every modal, which is the rest of S3.** `paint_layer`
   pushes the `Draw::Scrim` item with `key: None` *before* calling `paint_render` on the
   layer (`paint.rs:41-56`), and the layer's index range therefore starts *after* the
   scrim. The cut lands after the scrim item, so the scrim goes in the **Background**
   band — painted before the legacy painters and immediately obliterated by them — while
   the modal's content goes in the Overlay band. A dimmed modal will render with no dim.
   And `Scrim::Opaque` additionally does `spec.items.clear(); spec.index.clear()`
   (`paint.rs:45-46`), erasing the native menu-bar row from the list entirely while the
   legacy painters continue to paint the whole editor underneath.

3. **An overlay-family name used on an in-flow node.** The cut moves earlier and
   arbitrary in-flow items get painted in the overlay band, on top of legacy chrome.
   Nothing prevents it; `is_overlay_key` matches on the family string alone
   (`frame.rs:149-156`).

4. **Nested layers** are fine, as it happens: a layer inside a layer registers in
   `pending_layers` itself and is skipped by the outer `paint_render`'s out-of-flow check
   (`paint.rs:105-108`), so it paints in the tail like any other. If it is keyed with a
   known family it lands correctly. If not, case 1.

#### Q2.2 The test that is supposed to hold the lists together does not

`overlays_are_recognised` (`fold.rs:685-701`) builds a `Frame` containing exactly one
context menu and two dropdowns and asserts the recognised-key count is `3`. Its doc says
"Every layer `frame_tree` can declare must be recognised as an overlay". It cannot check
that: it enumerates nothing, it inspects no `frame_tree` source, and a fourth layer type
added to `frame_tree` but not to this fixture leaves the test green. `Frame` has no
"every layer" enumeration to iterate, and the library exposes no "how many layers did
this frame produce" count to compare against — which is exactly the missing capability.

The neighbouring test `the_background_band_is_the_regions_and_the_overlay_band_is_the_layers`
computes both the cut and the expected partition from the *same* predicate, so it can
only ever fail on non-contiguity — a real property, but not the one at risk.

#### Q2.3 The caret across the two bands is not correct, and is not wired at all

Three separate problems:

- `fold_band` gives `LayoutSpec::cursor` to whichever band is `Overlay`, unconditionally
  (`fold.rs:239-241`), while its comment says "belongs to whichever band placed it". It
  does not check. A native `TextField` in the **background** band (the prompt line and
  the search-options row, both queued for S2) sets `spec.cursor`, and any caller that
  folds only the background band drops it. Both shell test modules do exactly that
  (`menu.rs` tests call `fold_native(…, Band::Background)` alone).
- The general `fold` is dead on the render path — `grep` finds one call, in a test
  (`fold.rs:523`). The caret rule the migration doc celebrates ("falls out instead of
  being listed") is a property of a function the editor never calls.
- `render` takes the overlay band's caret and throws it away: `let _ = shell_caret;`
  after a `debug_assert!(shell_caret.is_none(), …)` (`render.rs:1032-1037`). In release
  builds the first migrated field will simply have no caret. The debug-assert is the only
  thing standing between here and that, and the corresponding background-band assert
  (`render.rs:236`) is vacuous — `fold_native` uses a `Skip` painter, so `host_caret` is
  always `None`, and `Band::Background` returns `None` for the native cursor by
  construction. It can never fire.

#### Q2.4 Should `layers_from` be done now?

**Yes, now, not later.** It is a two-field change (`LayoutSpec.layers_from: usize`, set
in `flush_paint` before the layer loop — plus, to be correct for scrims, recording the
index *before* `paint_layer` pushes the scrim item). It removes `OVERLAY_FAMILIES`,
`is_overlay_key`, the untestable `overlays_are_recognised`, and all four break cases at
once. It satisfies the migration's own stated rule for when to change the library:
`pending_layers` already computes the boundary and `flush_paint` throws it away — the
identical diagnosis that produced `Dispatch::claimed`.

Deferring it means S3's modals ship a broken scrim and S4's first library `Dropdown`
ships an invisible pop-over, and by then two more surfaces will have been written
against the family-name convention.

### Q3 — `regions_of` as a layout query keyed by `region_key` (`a43dd92`)

**Naming a region with a key is sound. The implementation is not, and the key namespace
is being overloaded.**

Sound: the motivation is correct — a native region and an empty region both emit no
`Draw::Host` item but both still have a rectangle, and scanning the display list loses
exactly those, silently (the parity sweep caught it: region 2 vanished from 8586 cases).
Reading the rect from layout rather than from paint is goal 5 applied to the migration's
own bookkeeping, and `Ui::find_by_key` + `rect_of` is the library's own sanctioned
mechanism — `Services::rect_of_key` (`services.rs:78-80`) is literally that pair. It
duplicates nothing the tree already exposes to the editor, because the editor holds no
`ElementId` handles: `frame_tree` returns a `Node` and the elements are the library's.

Not sound:

- `find_by_key` is an unindexed DFS over the whole element tree
  (`render/layout.rs:761-777`). `regions_of` runs seven of them (`frame.rs:253-256`) and
  `shell_region_now` then discards six (`render.rs:2381-2384`). `status_bar_area_now`,
  `menu_bar_area_now` and `menu_layout_now` each call it, several times per frame and per
  pointer event. The previous implementation was one linear scan of a short list. This is
  the "geometry is cheap to read" assumption being spent before the tree is large.
- The `Key` namespace now carries three meanings: reconciliation identity (the library's),
  geometry lookup name (the editor's, fine), and **paint-band membership** (the editor's,
  not fine — §Q2). The third is the side channel. `region_key` is not the problem;
  `is_overlay_key` is.
- `HostRegion` is now a misnomer. Its doc says "A region of the frame the host still
  paints itself" and "The discriminants are the `HostId` values carried in `Draw::Host`"
  (`frame.rs:15-18`), but `MenuBar` no longer appears in any `Draw::Host`, and its arm in
  `HostPainter::paint_host` is a no-op alongside five others (`shell_host.rs:173-178`).
  The enum is now "names of frame regions", and the `HostId` coupling is incidental.

### Q4 — the `UiFact` vocabulary (`view/shell/msg.rs`)

#### Q4.1 `HoverTarget` in the message type is a boundary violation

`UiFact::MenuHover(Option<crate::app::types::HoverTarget>)` (`msg.rs:54`) puts a 25-variant
editor-wide enum — carrying `PathBuf`, `TabTarget`, `LeafId`, `ContainerId`, `SortMode`,
`StatusBarClickable` (`app/types/hover.rs:6-68`) — into the shell tree's message type.
Every node in the tree is now generic over a type that transitively names the file
browser's sort modes and the status bar's clickable segments.

The pragmatic defence in the doc-comment is real ("migrating *where the pointer is* does
not require rewriting *what the menu does about it*") and I would accept it for a
transitional wave. But `HoverTarget` is the *central enumeration of every surface in the
editor* — the thing goal 2 exists to dissolve — and it has just been made part of the
new model's vocabulary rather than being left on the legacy side of the seam. The
minimal fix costs almost nothing: `UiFact::MenuHover(MenuHoverTarget)` with three
variants (`Bar(usize)`, `Row(usize)`, `Submenu(depth, index)`) and the widening to
`HoverTarget` done in `apply_ui_fact`, which already rewrites the target anyway
(`shell_host.rs:381-391` fills in the active menu index). That also deletes the odd
`MenuDropdownItem(0, index)` placeholder-zero convention in `shell/menu.rs:182`.

#### Q4.2 `was_active` is not merely a smell — it is wrong in the live app

The reasoning in `msg.rs:57-61` is correct as far as it goes: dismissal fires on the
**press** (`hit.rs:113`), the click is emitted on the **release**, so asking "is this
menu open?" inside the click handler answers no. Snapshotting at build time is a
reasonable answer *if the tree is not rebuilt between the press and the release*.

It is. `handle_mouse` returns `needs_render = true` for the press, and the main loop
draws before reading the next event (`main.rs:6129-6144`) — subject only to the 60 Hz
frame gate, which a human press-to-release interval (50–150 ms) clears comfortably.
`Editor::render` rebuilds the whole description (`render.rs:203`), so by the time the
release arrives the label's handler is the *new* closure with `active: false`, and
`MenuBarClick { was_active: false }` reopens the menu the press just closed. Element
identity survives the rebuild, so the click still lands — on the stale-free, wrong-valued
handler.

This is invisible to every test because `EditorTestHarness::mouse_click` sends `Down`
and `Up` with no render in between (`tests/common/harness.rs:1307-1327`), and the shell's
own `click()` helper does the same (`shell/menu.rs` input tests). So
`test_mouse_click_toggles_menu` (`tests/e2e/menu_bar.rs:202`) passes while the live
behaviour is "click an open menu's label → it flickers shut and reopens", and it is
frame-timing dependent, i.e. intermittent.

The general shape the doc draws from this — "a handler that needs to know what was true
*before* the event must close over it" — is therefore the wrong lesson. The right lesson
is that a build-time snapshot is only valid for the duration of one build, and
press→release spans builds. The robust forms are: make the label's press (not click)
carry the toggle, so snapshot and consumption are in the same event; or move the
toggle decision into `apply_ui_fact` keyed on a fact the *editor* owns (e.g. "this press
dismissed menu N", recorded when `CloseMenu` is applied) rather than on a snapshot the
tree carries across a rebuild.

#### Q4.3 Dropping `Eq` does not matter, but the reason given is false

`msg.rs:33-34` says "`HoverTarget` carries paths and is only partially comparable".
`PathBuf` is `Eq`; so are `TabTarget`, `LeafId`, `ContainerId`, `SplitDirection`,
`SortMode` and `StatusBarClickable` (all checked). `HoverTarget` simply derives
`PartialEq` and not `Eq` (`app/types/hover.rs:5`). Adding `Eq` there restores it. The
loss is harmless in itself; it matters only as a symptom — an app-wide type was pulled
into the message vocabulary and dragged its trait surface with it, and the comment
rationalised the consequence instead of noticing it.

#### Q4.4 The message *list* is a mutation script with no arbitration

This is the deeper issue `MenuBarClick` exposes. `shell_dispatch` applies every message
in emission order (`shell_host.rs:326-337`), and bubble order is target→root
(`hit.rs:416-421`), so an ancestor handler's mutation runs *after* a descendant's. At
`bb4de04` the bar label emitted `MenuBarClick` and the row behind it then emitted
`CloseMenu`: **clicking a menu-bar label opened the menu and immediately closed it.** The
shell's own tests used `assert!(got.contains(…))` and passed. The working tree fixes it
with `e.stop()` on labels and rows and rewrites the tests to compare the exact list —
both correct, and the exact-list assertion is the right standard going forward.

The structural point survives the fix: correctness now depends on every descendant
handler remembering to `stop()` whenever any ancestor on its path also mutates the same
state, and nothing checks that. Three of the four handlers in `shell/menu.rs` now call
`e.stop()` for that reason. That is a hand-maintained exception per handler — goal 2's
failure mode in miniature.

### Q5 — claiming `Move` on migrated surfaces

**It was a hack, and an expensive one. It was deleted from the working tree while this
review was being written** — `claim_move` is gone from `view/shell/menu.rs` and the seam
is now a one-event flag, `Editor::shell_owns_hover` (`app/mod.rs:1149-1153`), set in
`apply_ui_fact`'s `MenuHover` arm (`app/shell_host.rs:378`) and consumed at the top of
`update_hover_target` (`app/mouse_input.rs:527-539`). That is approximately option 1
below and it is the right direction. The analysis is kept because it names the cost that
was being paid at `bb4de04`, and because the replacement has a leak of its own (§Q5.3).

#### Q5.1 What the claim cost, at `bb4de04`

The stated rule ("a migrated surface owns the pointer over its own cells",
`shell/menu.rs:28-33` at HEAD) is a legitimate *principle*. `e.stop()` on `Move` is not
an implementation of it: it does not tell the legacy walk "this cell is mine", it tells
the entire editor "this event is finished", and `shell_dispatch` returning `true` makes
`handle_mouse_impl` `return Ok(true)` at `mouse_input.rs:119` — before everything else in
the function. For every pointer motion over the menu bar row or an open dropdown, that
skipped:

| Skipped | Where | Consequence |
|---|---|---|
| plugin `mouse_move` hook | `mouse_input.rs:310-331` | plugins stop receiving motion over the bar |
| `update_hover_target` → every component's `on_hover_change` | `mouse_input.rs:335`, `507-538` | the *transition* is never delivered: a hovered tab close button or file-explorer status tooltip stays lit/shown when the pointer moves onto the bar; the dock's overlay scrollbar reveal (`chrome/dock.rs:108-139`) never hides |
| `update_terminal_link_hover` | `mouse_input.rs:344` | Ctrl+hover underline state not cleared when the pointer moves onto a dropdown covering a terminal |
| `update_lsp_hover_state` | `mouse_input.rs:348` | LSP hover debounce not updated/cancelled |
| `update_widget_hover` | `mouse_input.rs:355` | dock icon-button hover |
| `mouse_cursor_position` / GPM redraw | `mouse_input.rs:145-150` | with GPM active the software mouse cursor **freezes** over the bar |
| the LSP-rename pre-walk cancel observer | `mouse_input.rs:133-140` | a rename prompt is no longer cancelled by interacting with the bar |
| `mouse_state.last_position` | `mouse_input.rs:390` | stale |

#### Q5.2 And it broke drags, including one with an issue number

`view/shell/input.rs:113` maps `MouseEventKind::Drag(_)` to `Input::Move` — deliberately,
because the library routes drags by pointer capture. But the legacy surfaces have no
fresh-ui pointer capture: the press landed on a `Host` leaf with no handlers. So a legacy
drag whose pointer crossed the menu bar row was claimed by `claim_move()` and
`handle_mouse_drag` never ran for those events.

Row 0 is the menu bar whenever `editor.show_menu_bar` is true, which is the default
(`config.rs:1997`). Dragging a text selection *past the top edge* means dragging to row 0 —
issue #3006's own gesture and its own test:
`tests/e2e/issue_3006_drag_beyond_text_area.rs:198` does `drag_to(&mut harness, 10, 0)` in
a loop and asserts the viewport scrolls and the selection extends on **every** step. Tab
drags and dock-resize drags crossing row 0 were affected the same way, and the
`chrome_drag_active` guard that exists precisely to stop this class of problem
(`mouse_input.rs:160-171`) is computed *after* `shell_dispatch`. Worth confirming that the
working tree's change makes `cargo test -p fresh-editor --test all_tests issue_3006` green
again before committing.

#### Q5.3 The replacement leaks, because a hover message is not a hover position

`shell_owns_hover` is set only when a `MenuHover` fact is emitted, and `MenuHover` is
emitted only from `on_enter`. `GestureKind::Enter` fires exactly once per element per
entry (`crates/fresh-ui/src/hit.rs:517-527`) — so the *second* motion event within the
same label, or within the same dropdown row, emits nothing, leaves the flag `false`, and
lets the legacy walk run. `compute_hover_target` finds no chrome box on the bar row (the
box is deleted), returns `None`, and clears the `MenuBarItem`/`MenuDropdownItem` the tree
just set. Since no further `Enter` will fire while the pointer stays on that element,
nothing puts it back.

The visible symptom is the hover highlight — `BarLabelStyle::Hovered`,
`MenuRowStyle::Hovered`, both keyed on `mouse_state.hover_target` — appearing on entry to
a label or row and vanishing on the next motion inside it. Labels are 6+ cells wide and
dropdown rows are the width of the box, so this is easy to hit. The `hover_cell_memo`
does not save it: the memo key includes `(col, row)`, so a move to a different cell of the
same element misses.

The fix is to gate on *where the pointer is*, not on *whether a message was emitted*: set
the flag whenever the tree's hit path for this `Move` lands inside a migrated region — a
geometry question the retained tree can answer directly — or, equivalently, whenever
`Ui::hit_test(pos)` reaches any element under a `region_key`/overlay-family subtree. That
also generalises to every future migrated background region without another per-surface
message.

#### Q5.4 The longer-term shape

The honest framing is that two systems write one field. The flag is the right *kind* of
answer for a transition, and its doc-comment correctly commits it to retiring with the
walk. Two further guards are cheap and worth taking now: report `claimed` only for gesture
kinds where claiming is meaningful — never for `Move`, which is a broadcast rather than a
gesture — and decline to offer any pointer event to the tree while a legacy
`pointer_grab` is active, using the `chrome_drag_active` value already computed twenty
lines below the dispatch. The first makes §Q5.1 structurally impossible to reintroduce;
the second makes §Q5.2 impossible for every surface, not just the menu bar.


### Q6 — duplication ledger

**Genuinely removed:**

- `chrome:context_menu_close_guard`, `chrome:context_menu`, `handle_click_context_menus`,
  `ContextMenu::on_key`, `handle_context_menu_key`, `ContextMenuHit`, `ContextMenu::rect`,
  `ContextMenu::hit`, `clamped_position` — gone, replaced by declared layer properties.
  `app/chrome/context_menu.rs` is 121 lines and its `collect` is empty.
- `chrome:menu_bar`, `chrome:menu_dropdown`, `chrome:menu_close_guard`,
  `handle_click_menu_bar`, `handle_click_menu_dropdown_surface`,
  `compute_menu_dropdown_hover`, the coordinate-driven `handle_menu_dropdown_click` —
  gone. `app/chrome/menu.rs` is 71 lines. Verified.
- `render_context_menus`, `render_context_menu`, `MenuRenderer::render`, the ratatui
  `Paragraph`/`Block` in `render_dropdown_level` — gone.
- Two style ladders collapsed into one (`MenuRowStyle`, `BarLabelStyle`), with the
  inspector's hover bug going with it. Real, and the three-renderings shape
  (`style()` / `theme_keys()` / `shell_theme()`) is a good pattern.
- The second frame-layout implementation (`4826d75`) and `assert_parity` (`641a99f`).

**Moved, not removed:**

- **`MenuLayout` now carries a description.** `shell_bar` and `shell_dropdowns`
  (`view/ui/menu.rs:40,47`) are `Node`-shaped data on a struct whose whole purpose is
  rectangles. The design spec's §3 invariant is the mirror of this ("a description type
  with a rect field indicates layers 1 and 3 have been merged"); this is the same merge
  from the other side — a geometry type with description fields. `DropdownLevel` itself
  carries `x`, `y`, `width` *and* pre-rendered row strings (`shell/menu.rs:137-144`), so
  the description that goes into the tree literally has a rect in it.
- **The menu walk still runs twice per frame** (three times in debug), and the two
  results are still held together by a `debug_assert`. The commit message for `a43dd92`
  says: *"Nothing paints a cell there any more, so there is one walk and nothing to keep
  in step."* `render.rs:2620` runs `compute_layout`, `render.rs:2635-2639`
  `debug_assert_eq!`s it against `self.menu_layout_now()` — which runs `compute_layout`
  again. Two walks; a `debug_assert` keeping them in step. The claim is false as written.
  What actually changed is *which* two derivations are being compared.
- **`CellThemeRecorder` survives alongside `ThemeKey`.** Per-cell provenance for the
  inspector (`view/ui/menu.rs:1112-1125`, `render.rs:2599-2642`) and per-item `ThemeKey`
  in the display list are two provenance systems fed from one ladder. Keeping the
  inspector working is legitimate; the cost is that the *entire second walk exists only
  to feed it* — `record_menu_theme_runs`'s only remaining outputs are theme runs and a
  cache refresh. Worse, the two are derived from different geometry: the recorder's runs
  come from `current_x` accumulation in the legacy walk with no clipping to `area.width`,
  while the painted cells come from the tree's row layout of `text_runs`. On a narrow
  terminal these disagree and the inspector reports provenance for cells the bar never
  painted.
- **Dropdown geometry**, §3/goal 5 above: `fit_dropdown_area` and the tree both compute
  the box.
- **Dead-again `pub` API**: `MenuLayout::{menu_at, item_at, submenu_item_at, hit_test}`,
  `MenuHit`.

**Net:** the pointer and dismissal duplication is genuinely gone — that is the migration's
real win and it is a large one. The *geometry and provenance* duplication for the menu
was not removed; it was re-shaped, and one of its two halves is now inside the struct
that feeds the new model.

### Q7 — "colour that is not a theme name" (`bb4de04`, §6.2 item 2)

**The diagnosis is right. The chosen answer is the worst of the three and there is a
fourth that is strictly better.**

Right: an `Item` carries one `ThemeKey`, `ExplorerSlotPayload::fg` /
`name_color_hint` are `ratatui::Color` (`view/file_tree/slots.rs:22,48,55`), and they come
from plugins. Naming this before starting S4 rather than discovering it mid-wave is
exactly the right instinct, and refusing to build the mechanism before its consumer is
the right instinct too.

One correction to the framing: the plugin wire type is already half-named —
`OverlayColorSpec::{Rgb(u8,u8,u8), ThemeKey(String)}` (`fresh-core/src/api.rs:919-925`),
resolved at `view/file_tree/slots.rs:338-343`. Only the `Rgb` arm has no slot to name,
and its values come from cached decoration state, not from arbitrary per-frame
computation. The set is smaller and more stable than "not known until the frame is built"
suggests.

Why per-frame minted `dyn:N` is the wrong answer:

- **It makes `build` have a side effect.** "Intern each dynamic style *as the description
  is built*" means the build mutates a per-frame table. README goal 3 is explicit: "No
  registration, no resource acquisition, **no side effects**." This would be the first
  side effect in the editor's build, introduced for cosmetics.
- **It destroys what `ThemeKey` is for.** The library defines it as "Per-item provenance:
  the nearest enclosing `theme(..)` tag" (`render/spec.rs:69-70`). `dyn:3` is provenance
  for nothing — it is an index into a table that exists for one frame. The theme
  inspector, the surface that consumes provenance, would report `dyn:3`.
- **It is a side table keyed by a minted identifier**, resolvable only by pairing it with
  an out-of-band map produced in the same pass. Goal 4's objection to side tables is
  stated about *identity*, but the objection is the same: a name that means nothing on
  its own.
- **It makes display-list assertions unstable.** The README puts "test assertions" among
  the display list's three consumers; `dyn:N` numbering depends on build traversal order,
  so any test asserting on `Item::theme` for a plugin-coloured row breaks when an
  unrelated row is added.
- **It breaks the web backend's premise.** `LayoutSpec` is designed for "DOM patching by
  `Key`" — an item whose theme name is `dyn:3` this frame and `dyn:5` next frame changes
  appearance with no structural change and no stable name to key a style rule on.

**The better fourth option: a content-derived name.** `ThemeKey` is an opaque string the
library never interprets — so name the colour after itself: `"rgb:7f3fbf"`, or
`"fg:rgb:7f3fbf/bg:menu.dropdown"` for the compound case. It is deterministic, stable
across frames, comparable, inspectable, needs no side table, needs no library change,
keeps `build` pure, keeps display-list tests stable, and lets the web backend emit a real
style. The backend's cost is one `strip_prefix("rgb:")` arm in `ShellPalette::style`.
This is strictly better than option 1 on every axis the doc scores option 1 on, and it
gives away nothing option 1 keeps.

(Option 2, a colour variant on `ThemeKey`, is more honest still and the objection to it —
"every backend then has to understand a colour model" — is weaker than stated, since a
backend can fall back on the variant it does not want. But `rgb:` naming gets the same
result with no library change, which is the tiebreaker while the migration is mid-flight.)

Note also that the *span* problem and the *colour* problem are the same problem: an item
carries one `ThemeKey`, so a row with a plugin-coloured badge and a theme-coloured name
needs one run per span regardless. `Run::themed` already exists and is already used for
the mnemonic (`shell/menu.rs:86-90`). So the mechanism for §6.2 item 2 is: one run per
span, named `rgb:…` where the colour is dynamic. That is nearly free, and it partially
settles §6.2 item 3 (inline styled text) at the same time.

### Q8 — what is accumulating that should be paid down now

Answered in §5 (do now) and §6 (fine to defer).

### Q9 — migration-doc claims that are not true of the code

Verified against source. Claims that hold are listed briefly; claims that fail are cited.

**Hold:**

- `clamped_position` and `Fit::CLAMP` are the same arithmetic (checked against the
  deleted code — §3, goal 5).
- Suppression is a fold decision, not a tree one (`render.rs:1013`, `2350-2352`).
- "The pre-band grab stage is down to one component" — only `theme_info.rs:111`
  implements `on_key` (`chrome/mod.rs:439` is the default).
- "`app/chrome/menu.rs` goes from 204 lines to 71" — the file is 71 lines.
- The `Ui` is held in an `Option` and moved out per frame; `expect`, not
  `unwrap_or_default` (`render.rs:196-201`).
- Escape is deliberately *not* stopped so the layer's `ESCAPE` dismissal runs
  (`context_menu.rs:143-147`). Correct and correctly explained.
- The parity test now describes itself accurately as a pin, not an oracle.

**Do not hold:**

1. **"there is one walk and nothing to keep in step"** (`a43dd92` message; §5.0
   "The menu bar row, native"). Two walks per frame plus a `debug_assert` comparing them
   (`render.rs:2620`, `2635-2639`). See §Q6.
2. **"The levels carry no modality, no dismissal and no handlers"** — `view/shell/menu.rs:9-15`,
   the module doc of the file that gives them modality (`:205`), dismissal (`:207`) and
   four handlers. Stale since `7a478a0`, which is the commit that installed the "a split
   wave names which channels have moved in the surface's own module doc" rule's subject.
3. **`Modality::Inert` for the context menu.** The migration doc §4.2, the module doc
   (`view/shell/context_menu.rs:8`) and the in-function comment (`:63`) all say `Inert`;
   the code says `Modality::Exclusive` (`:110`). So does `app/chrome/context_menu.rs:13`.
   The code's choice is deliberate and explained two lines below it — but four places now
   say the opposite of what runs.
4. **"`HoverTarget` carries paths and is only partially comparable"** (`msg.rs:33-34`).
   Every component of `HoverTarget` is `Eq`; the enum just doesn't derive it. §Q4.3.
5. **"`frame::OVERLAY_FAMILIES` is checked by a test"** (§6.2 item 0). The test checks a
   hand-built fixture against a hard-coded count of 3. §Q2.2.
6. **"the derivation … is exact as long as layers paint last, which the library states"**
   (`fold.rs:122-124`). Layers painting last is necessary, not sufficient; the derivation
   also requires `OVERLAY_FAMILIES` to be complete, which nothing states or checks. §Q2.1.
7. **"A test pins the property the working state depends on: a frame of host regions
   paints *nothing*"** (§5.0, S1). `a_frame_of_host_regions_paints_nothing`
   (`fold.rs:700-717`) still passes, but the menu bar row is native now and *does* emit a
   `Draw::Fill` for its `"menu.bar"` ground; the test only survives because its `plain`
   palette returns `Style::default()` and the fill writes spaces. The test no longer
   pins what its name says.
8. **The caret rule** — "`LayoutSpec.cursor` … wins over a caret a host region wrote" is
   true of `fold`, which the render path does not call, and false of `render`, which
   discards the value. §Q2.3.

**Also worth recording, though the doc does not claim otherwise:** no shell test asserts
a *style*. Every test module defines `fn plain(_: &ThemeKey) -> Style { Style::default() }`.
That is why `7a478a0` could ship a display list that inherited `BOLD` from whatever legacy
cells were underneath (fixed in the working tree with `Style::reset().patch(…)`) with all
52 shell tests green. For a migration whose stated acceptance bar is "cell-identical",
the tests assert glyphs and never colours.

---

## 5. Do now — before this entrenches

Ordered. Each is scoped to be a single commit.

1. **Add `layers_from` to `LayoutSpec` and delete `OVERLAY_FAMILIES`.**
   Set it in `flush_paint` (`crates/fresh-ui/src/render/paint.rs:22-29`) *before* the
   layer loop begins, so a scrim item pushed by `paint_layer` falls on the overlay side.
   Replace `overlay_start` with a field read; delete `is_overlay_key`,
   `OVERLAY_FAMILIES` and `overlays_are_recognised`. This closes the unkeyed-layer case
   (`fresh_ui::widgets::Dropdown`, `crates/fresh-ui/src/widgets/menu.rs:131`), the scrim
   case, and the wrong-family case together, and it is the same "the library already
   computes it and throws it away" diagnosis that produced `Dispatch::claimed`.
   *Blocks S3's modals and S4's first library `Dropdown`. Do it first.*

2. **Finish the `shell_owns_hover` change now in the working tree.** Deleting
   `claim_move` was right. Gate the flag on *pointer position inside a migrated region*
   rather than on a `MenuHover` message having been emitted, or the hover highlight
   flickers off on the second motion event inside any label or dropdown row (§Q5.3).
   Then add the two structural guards from §Q5.4: never report `Move` as claimed, and do
   not offer a pointer event to the tree while a legacy `pointer_grab` is active
   (`chrome_drag_active`, `mouse_input.rs:160`). Add a hover-within-one-label test and
   confirm `cargo test -p fresh-editor --test all_tests issue_3006` is green.

3. **Fix the `was_active` toggle.** Move the toggle decision off a build-time snapshot
   consumed one event later (§Q4.2), and add a test that renders between press and
   release — the harness's `mouse_click` cannot express this today
   (`tests/common/harness.rs:1307`), so it needs a `mouse_click_with_repaint` helper.
   Every future surface that reacts to a dismissal-then-click pair inherits this bug
   otherwise, and the doc currently teaches the broken pattern as "the general shape".

4. **Wire the caret, or stop pretending it is wired.** Either merge `shell_caret` into
   the end-of-frame cursor commit (`render.rs:1037`) or delete the return value from the
   `fold_native` call and say plainly that native carets are not supported until S2.
   Also fix `fold_band` to report `spec.cursor` from the band that placed it rather than
   from `Band::Overlay` unconditionally (`fold.rs:239-241`), and delete the vacuous
   background-band assert (`render.rs:236`).

5. **Clip `Draw::Lines` to `Item::rect` in the fold** (`fold.rs:187-196`), instead of
   pre-fitting every string in every description. The library's own backends have the
   same gap (`examples/interactive.rs:257`, `tests/support/screen.rs:104`), so fix it
   there too — an item declares a rect and the backends ignore it, which is the internal
   asymmetry the migration's own rule says to close in the library. Keep the working
   tree's `fit()` if the padding is semantically part of the row; drop it if it is only
   there to avoid overflow.

6. **Assert styles in shell tests.** Give the test palette distinct, checkable styles and
   assert them for at least one row of each migrated surface. Without this the
   cell-identical bar is enforced only by the e2e suite, and the last three commits show
   that catching it there is catching it late.

7. **Give the menu-bar dropdown one geometry.** Either let the layer place itself
   (`Fit::FLIP.or(Fit::CLAMP)`, as the library's own `Dropdown` does) and have the web
   `Scene` read the rectangles back via `items_for(Key::Pair("menu_dropdown", depth))` —
   the shape `menu_rect` already established for the context menu — or add a
   `debug_assert` that the tree's box equals `fit_dropdown_area`'s. The first is the
   migration's own stated direction; the second is the minimum.

8. **Break the build→layout loop for the menu.** `shell_frame` must not read last frame's
   rectangles (`render.rs:2322` → `2438` → `2372`). Pass the bar rect the frame is about
   to be laid out with, or compute the label runs without needing a rect at all (they only
   use `area.x`, which is the dock width and is already known from `compute_dock_split`).

9. **Delete the dead `pub` API again** — `MenuLayout::{menu_at, item_at,
   submenu_item_at, hit_test}`, `MenuHit`, and the `view/ui/mod.rs:61` re-export. This is
   the second time; consider whether `MenuLayout`'s remaining fields are all still live.

10. **Fix the four stale claims** in §Q9 items 2, 3, 4, 7 — they are one-line edits and
    three of them are module docs that state the opposite of the code beneath them.

## 6. Fine to defer

- **The rebuild benchmark** (§6.2 item 6). Still worth taking, still not a gate. Revisit
  when the status bar migrates, since that surface's walk is the expensive one.
- **`HoverTarget` out of `UiFact`** (§Q4.1). It should not ship past S3, but it is a
  contained ugliness with no correctness consequence and the widening code already lives
  in `apply_ui_fact`.
- **`UiFact`'s `Eq`.** Add `#[derive(Eq)]` to `HoverTarget` whenever convenient; nothing
  depends on it.
- **`find_by_key`'s cost.** Seven DFS walks per query is fine at this tree size. It stops
  being fine when the split grid and plugin panels are in the tree; the fix then is a key
  index in the library, not editor-side caching.
- **`HostRegion`'s name and doc** now that `MenuBar` is not a host. Rename with the next
  region that goes native.
- **`layer_rank::CONTEXT_MENU` / `MENU`.** Honestly documented as blocked on host leaves
  declaring `takes_raw_input` (`chrome/context_menu.rs:29-36`). Correct to leave.
- **The squeeze band** (§6.2 item 7). Recorded and pinned; no user is in that band.
- **Per-cell vs per-item theme provenance** (§6.2 item 9). Real, but it only bites when
  `CellThemeRecorder` is finally deleted, which is not this stage.
- **`region_rects` building a throwaway `Ui`** (`frame.rs:271-280`). Tests only.

---

## 7. What is good, briefly

The context-menu wave is the migration working as designed: a guard box, a rank entry, a
hover walk, a pre-band keyboard grab and a duplicated clamp all deleted and replaced by
declared properties, with the placement decided once and read back by both frontends.
`Dispatch::claimed` was the right call and the rule derived from it is the right rule.
The frame parity sweep found a silent regression (region 2 missing from 8586 cases) that
nothing else would have caught, and the outside-in argument in §5.0 is correct on all
three of its stated grounds. `widgets/keying.rs` is exemplary: scoped, attributed,
tested, and shipped a release ahead of when it is needed.

And the working tree shows the corrective reflex working: `claim_move` was deleted, the
click bubbling was stopped, the style reset was added — all within hours of the commits
that introduced them.

The problem is not the direction. It is that the last three commits each chose the
version that needed no library change — a derived cut instead of `layers_from`, a
build-time snapshot instead of an event-scoped fact, `e.stop()` on `Move` instead of a
hover-ownership rule, a minted `dyn:N` instead of a content-derived name — and each of
those choices is one the migration's own stated rule ("fix the library when the gap is an
internal asymmetry") would have decided the other way.
