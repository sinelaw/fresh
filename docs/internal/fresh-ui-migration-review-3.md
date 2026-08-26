# Third review: the `fresh-editor` → `fresh-ui` migration

Branch `claude/fresh-editor-fresh-ui-migration-glu9af` @ `084f3ec` (PR #3028), 30
commits above `2b37170`. Written as an independent adjudication, not a status
report. Where I disagree with the author's own conclusion I say so.

Two things in this review were established by running code, not by reading it: a
standalone probe crate built against `crates/fresh-ui` reproducing the editor's
exact layer shape. Both results are load-bearing and both contradict claims
currently written into the source and the migration doc.

---

## 1. The yardstick — the seven goals, as `crates/fresh-ui/README.md:24-41` states them

1. **One library for everything on screen.** Menus, prompt, popups, context
   menus, dock, splits, modals, status bar and plugin panels are built from the
   same primitives with the same event and layout rules.
2. **Generic registration and propagation, with no hand-specified exceptions.**
   Precedence, modality, dismissal and focus order are *derived* from structure
   and declared properties, never read from a central ordered list.
3. **Rebuilding a description costs one allocation per node.** No registration,
   no resource acquisition, no side effects — so there is no incentive to avoid
   rebuilds or to mutate retained state directly.
4. **Identity is explicit.** Which on-screen object is which across frames is
   decided by tree position and an author-supplied key, both inspectable —
   never by hashing an identifier stack into a side table.
5. **One source of geometry.** Layout computes rectangles; hit-testing, painting
   and tests read them. Geometry is not derived during paint.
6. **Composition is the only extension mechanism.** The primitives available to
   library code are the primitives available to user code.
7. **Backend independence.** Paint produces a display list; TUI cells, the web
   DOM and test assertions are consumers of that list.

Plus one invariant from the design spec that the goals list does not restate,
and which turns out to matter more than any of them here:

> **Invariant:** descriptions carry no geometry. A description type with a rect
> field indicates layers 1 and 3 have been merged.
> — `docs/internal/widget-library-design.md:77-79`

---

## 2. THE DILEMMA — adjudicated

**Neither A nor B. Both are arguing about the wrong object, and A's central
factual premise is false.**

### 2.1 The premise both positions rest on is wrong

§6.2 item 3 and `view/shell/menu.rs:23-26` both assert:

> `Anchor::Node(key)` with `Place::RightOf` can name the parent, but there is no
> way to say "and one row up".

There is. You name a different node. Anchor the submenu to the row *above* the
one it opened from — and, for the first row, to the enclosing box — and
`Place::RightOf` produces the wanted rectangle exactly, for every row, with no
library change whatsoever.

I built it. Output of the probe, a faithful reconstruction of the chain against
the real `crates/fresh-ui`:

```
open_at=0: parent row y=2 right=15 | sub box y=1 x=16 | first sub item y=2 | ALIGNED=true
open_at=1: parent row y=3 right=15 | sub box y=2 x=15 | first sub item y=3 | ALIGNED=true
open_at=2: parent row y=4 right=15 | sub box y=3 x=15 | first sub item y=4 | ALIGNED=true
open_at=3: parent row y=5 right=15 | sub box y=4 x=15 | first sub item y=5 | ALIGNED=true
```

`ALIGNED` is `sub.y + 1 == parent_row.y` — the submenu's first item on the parent
row, which is what `test_submenu_first_item_aligns_with_parent_item`
(`tests/e2e/menu_bar.rs:613`) requires. The x also lands on `box.right() - 1`,
matching `current_x = dropdown_rect.x + dropdown_rect.width - 1`
(`view/ui/menu.rs:883-886`), for every row but the zeroth, where it is one cell
out and one more key fixes it.

So Position A's justification ("moving it needs something the library does not
have") is simply untrue. And Position B's own alternative — "expressing the
offset by *which node* you anchor to" — is not a hypothetical future shape. It
is available today, and B under-sold it by filing it as speculation.

### 2.2 Position B's stated reasoning is also wrong, though its conclusion is closer

B says an offset integer is "application policy leaking into the library — the
integer means 'my border is one cell', which only the caller knows."

That argument does not survive contact with its own premises:

- The editor is *already* handing the library two absolute integers computed by
  a full placement walk including width derivation, clamping and edge-flipping
  (`Anchor::Point(level.x, level.y)`, `view/shell/menu.rs:192`). If `+dy` is
  policy leaking in, `Anchor::Point` is the entire policy sitting outside. You
  cannot call one a leak and the other "a legitimate seam" on the grounds of
  where meaning lives; they differ only in how much of the derivation stays out.
- The *value* of an offset is caller-specific; the *operation* is not. "Shift
  the placed rect by (dx, dy) after placement, before fit" is a general
  geometric operation with one meaning. By B's test `Sizing::Cells(12)` would
  also be a leak, because 12 means "my longest label plus padding".
- The empirical form of B's test — "would a second, unrelated consumer want
  this, with the same meaning?" — **fails**. Every serious anchored-popup system
  has exactly this primitive: `GdkPopupLayout::set_offset(dx, dy)` in GTK4,
  `offset()` in Floating UI / Popper (its single most-used middleware),
  `PopupWindow.showAsDropDown(anchor, xoff, yoff)` on Android, `inset`/`margin`
  on the anchored element in CSS anchor positioning. B asserts no unrelated
  consumer would want it; four of them shipped it.

The defensible version of B is not the one written. It is: *a half-migrated
placement is worse than a clean seam — `Anchor::Point` puts all the derivation
on one side of a sharp boundary, whereas an offset would leave a residue on the
caller's side while claiming the job was done.* That is a real principle. It is
not the argument B makes.

### 2.3 The third framing: placement is the last thing to migrate, not the next

Both positions frame this as "how do we get the chain to place itself." Both
skip the prior question: **the chain has no content model in the tree to place.**

What `frame_tree` receives is not a menu. It is a picture of one:

```rust
pub struct DropdownLevel { pub x: u16, pub y: u16, pub width: u16, pub rows: Vec<DropdownRow> }
```
— `view/shell/menu.rs:131-137`

`x`, `y`, `width` on a type whose only job is to produce a description. That is
the design doc's own stop sign: *"A description type with a rect field indicates
layers 1 and 3 have been merged"* (`widget-library-design.md:77-79`). And it is
not only the rect. `MenuRenderer::compute_layout` also decides:

- the box's width (`calculate_dropdown_width`, `view/ui/menu.rs:909`),
- **which items exist at all** (`items_to_show = items.len().min(max_items)`,
  `view/ui/menu.rs:978`),
- every row's string, pre-padded and pre-truncated to the content width
  (`dropdown_item_text` → `fit(...)`, `view/ui/menu.rs:1100`),
- the row's style name (`MenuRowStyle::of`, `view/ui/menu.rs:1012`),
- and the per-row hit rectangles (`view/ui/menu.rs:1002`).

The tree is then *told* its width (`.w(Sizing::Cells(level.width))`,
`view/shell/menu.rs:201`) and handed strings already fitted to it. There is
nothing left for layout to compute. Under those conditions "let the layer place
itself" is not even well-formed: `Place::RightOf` places against a *measured*
box, and nothing here is measured.

The consequence is concrete, and it is a second thing my probe established:
**the editor's flip rule and the library's are not the same rule.** Same frame,
same box, no room to the right:

```
frame w=22, parent box x=4 w=12 (right=16), sub w=10
  library FLIP gives sub.x = 12
  editor's rule would give x = 1   (dropdown_rect.x.saturating_sub(w).saturating_add(1))
```

Eleven cells apart. The vertical rules differ too: `fit_dropdown_area`
(`view/ui/menu.rs:1042-1062`) never flips vertically — it *drops items* to fit
(`height.min(terminal_height - y)`, then `items_to_show`) — where `Fit::FLIP` /
`Fit::CLAMP` would move the box and keep every item. Under the migration's own
cell-identical acceptance bar, adding an offset primitive would buy you nothing:
you would still not be able to turn `fit` on.

So the ordering is forced, and it is the opposite of what §6.2 implies:

> Rows become real nodes → the box measures itself → `Fit` becomes usable → the
> flip/clamp divergence becomes a reviewable pixel change → *only then* is the
> border offset a question, and by then the answer is a one-line choice between
> `Anchor::Node(row_above)` and whatever primitive is obviously missing.

Doing the offset base PR now (A) buys nothing and adds a knob to an API surface
where — I checked the whole repository — `Anchor::Node` has **zero** callers and
**zero** tests, and four of six `Place` variants (`Above`, `RightOf`, `LeftOf`,
`Fill`) are likewise never constructed anywhere, in the library, its tests, its
demo, or the editor. Adding a seventh knob to six untested ones is not "small".

Declaring it settled forever (B) is also wrong, because it records the wrong
reason. The reason is not "`Anchor::Point` is a legitimate seam." It is "this
surface has not migrated its content model, and placement cannot migrate before
it."

### 2.4 Two claims in the current source that should be corrected

**(a) "runs once and feeds all three consumers — so nothing here can drift"**
(`view/shell/menu.rs:15-21`, and §6.2 item 3). It does not run once. Per frame:

| # | Call site | Bar rect it derives from |
|---|---|---|
| 1 | `shell_frame` → `menu_layout_in` (`app/render.rs:2368`) | `compute_dock_split`, from state |
| 2 | `record_menu_theme_runs` → `compute_layout` (`app/render.rs:2685`) | `region(HostRegion::MenuBar)`, this frame's layout |
| 3 | `menu_layout_now` (`app/render.rs:2484`), for the web `Scene` and for `debug_assert_eq!` at `app/render.rs:2702` | `regions_of(shell_ui, …)`, last committed layout |

That is two full walks in release and three in debug, from three different
rectangle sources, reconciled only by a `debug_assert_eq!` that is compiled out
of release. It is one *implementation* with three *invocations* — which is a
materially weaker claim than the one written, and it is the claim on which
B's "the payoff is near zero because nothing can drift" rests. Note also that
moving to `Anchor::Node` against the real bar labels would collapse the bar-rect
authority to one (the tree's), which is a payoff B does not count.

**(b) The chain is not "nested layers."** The M3 wave table
(`fresh-editor-ui-migration.md`, §5.2) names *nested layers* as the mechanism M3
exists to exercise, and design §15.1 says "Submenus nest as further layers
anchored to their row, to arbitrary depth". What shipped is flat siblings on the
root: `frame.children(super::menu::dropdown_chain(&f.dropdowns))`
(`view/shell/frame.rs:182`). M3 did not exercise its headline mechanism, and
nobody said so. My probe confirms nested layers + `Anchor::Node` do work
correctly — `resolve_layers` (`crates/fresh-ui/src/render/layout.rs:729-753`)
walks a worklist that grows during iteration, so a layer declared inside another
layer's subtree is appended by `arrange` and resolved afterwards, against the
parent's *final* rect. The capability is sound; it is simply unused.

### 2.5 And the flat shape is a live bug

This is the finding that should reorder the branch's priorities.

`OUTSIDE_POINTER` dismissal is an ancestor test over the hit path —
`if path.contains(&lid) { continue; }` (`crates/fresh-ui/src/hit.rs:675`) — and
`hit_test` returns only the **topmost** path (`hit.rs:247-249`). Because the
levels are siblings rather than nested, a press inside a *submenu* is not inside
the depth-0 layer's subtree. Only depth 0 declares dismissal
(`view/shell/menu.rs:208-222`). So:

```
--- flat chain (the editor's shape) ---
press on DEPTH-0 row1 (6,3):    []
release on DEPTH-0 row1 (6,3):  [RowClick(0, 1)]
press on DEPTH-1 row0 (18,4):   [Dismiss0]     <== the outermost layer dismisses
release on DEPTH-1 row0 (18,4): [RowClick(1, 0)]
```

In the editor, `Dismiss0` is `UiFact::CloseMenu` → `close_menu_with_auto_hide()`,
applied immediately inside `shell_dispatch` (`app/shell_host.rs:330-341`). By the
release, `menu_state.active_menu` is `None`, and `UiFact::MenuItemClick` bails at
its first line (`app/shell_host.rs:413-415`). The main loop also repaints between
press and release — the fact `c8ed601` discovered and pinned with
`mouse_click_with_repaint` — so the description no longer contains the submenu
either.

**Clicking a menu-bar submenu item with the mouse activates nothing and closes
the menu.** It is not caught because every submenu test in
`tests/e2e/menu_bar.rs` is keyboard-driven; the one mouse test
(`test_submenu_first_item_aligns_with_parent_item:613`) only hovers.

Nesting the chain fixes this by construction: level *d+1* inside level *d*'s
subtree means a press on a deep row has level 0 in its ancestor path, so the
`path.contains` test passes and no dismissal fires. It is the same change that
would make `Anchor::Node` usable and retire `fit_dropdown_area`'s position half.
That is a much better reason to nest than the offset ever was.

### 2.6 Verdict and disposition

- **Do not add a placement offset to `Place`.** Not because offsets are policy —
  they are a normal primitive that four other systems ship — but because the
  premise that it is needed is false, the payoff is zero until the content model
  moves, and the API it would extend is four-sixths untested.
- **Do not record `Anchor::Point` as a settled, permanent seam either.** It is
  correct *for the context menu*, where the point is a raw click and the caller
  genuinely owns it (`view/shell/context_menu.rs:104`). It is a placeholder for
  the dropdown chain, where the point is the output of a placement walk.
- **Do nest the chain, now, as a bug fix** — not as a geometry migration. Nesting
  alone fixes §2.5 without touching `fit_dropdown_area`: keep `Anchor::Point` on
  each level, just declare level *d+1* under level *d*. That is a small, testable
  change with a user-visible defect behind it.
- **Write down, in §6.2 item 3, the actual blocking order**: rows become nodes →
  the box measures itself → `Fit` becomes expressible → the flip and truncation
  divergences (§2.3, with the numbers) become a reviewed pixel change → the
  border offset is decided then, and `Anchor::Node(row_above)` is the leading
  candidate because it needs nothing. Record that `Anchor::Node` and four `Place`
  variants have no consumer and no test, so anyone relying on them owes a test
  first. That is what stops re-litigation: not a ruling, a dependency.
- If, after all that, a primitive is still wanted, the shape to propose is
  **not** a scalar offset. It is the relationship B gestured at — "place the
  layer so that *this descendant* lands on the anchor" — because it is
  declarative, it survives a border thickness change, and it has a real second
  consumer: a macOS-style popup button, which puts the *selected* item over the
  trigger. That consumer is already in this repo's future:
  `crates/fresh-gui/src/native_menu.rs` is named in §5.2 as a second menu
  frontend M3 must keep working.

---

## 3. The base-PR rule

The proposed rule: a `fresh-ui` base PR is warranted when *"the library already
computes it and discards it"* or *"the library's own model is internally
inconsistent"*, tested by *"would a second, unrelated consumer want this, with
the same meaning?"*.

**The two limbs are sound. The test is not, and it is the part doing the work.**

**Limb 1 — "already computes it and discards it" is excellent**, and it is
strictly better than the usual "is this general?" because it is *decidable by
reading the library*. It classifies both merged changes correctly and for the
right reason:

- `Dispatch::claimed` — `propagate_all` computed it and threw it away.
- `LayoutSpec::layers_from` — `flush_paint` knows the boundary exactly
  (`crates/fresh-ui/src/render/paint.rs:28`) and used to discard it.

It has a second virtue the rule's statement doesn't claim: in both cases the
fact was **underivable from outside**, not merely inconvenient. A scrim carries
no key and is pushed *before* its layer's items; `widgets::Dropdown`'s layer
carries no key at all. Any backend-side derivation is silently wrong. That
underivability is the real justification, and it is sharper than "already
computes it" — a library computes plenty of things a caller could recompute. I
would restate limb 1 as: **the library holds a fact no consumer can reconstruct
from the library's own outputs.**

**Limb 2 — "the library's own model is internally inconsistent" is also right**,
and the `Draw::Lines` clip is the clean example: an `Item` declares a `rect`, and
both in-repo backends ignored it (`examples/interactive.rs`,
`tests/support/screen.rs`). An item that declares a rect and is painted outside
it is a contradiction in the display-list contract, not a missing feature.
Correctly classified. The editor-side alternative — pre-`fit()`ing every string
in every description — would have made every future migrated surface hand-fit
every string, which is the tell.

**The test fails.** "Would a second, unrelated consumer want this, with the same
meaning?" has three failure modes:

1. **It wrongly excludes correct changes, and did so here.** Applied honestly to
   a placement offset it *passes* (GTK, Floating UI, Android, CSS all ship one) —
   yet the author used it to reject one. So in practice the test is being applied
   to the *value* rather than the *operation*, and at that granularity nothing
   passes: no second consumer wants `Sizing::Cells(12)` "with the same meaning"
   either. A test that rejects `Cells(12)` is not a test.
2. **It wrongly admits speculation.** "Would a consumer want this?" is
   unfalsifiable and reliably answered yes by the person who wants the feature.
   This branch has the evidence: `Anchor::Node`, `Place::{Above, RightOf, LeftOf,
   Fill}` all passed some version of this test into the library and have never
   been constructed by anything, anywhere, including the library's own tests.
   Six speculative variants is what the test admits.
3. **It has no failure branch for "the library is right and the caller is
   wrong."** The most common outcome of a mid-migration gap is that the caller is
   trying to reproduce a legacy behaviour the library deliberately doesn't have —
   which is exactly the flip/truncation divergence in §2.3. The rule offers no
   verdict there, so it defaults to "add it to the library."

**Replace the test with two.** Both are decidable, neither is a vibe:

> **(i) Underivability.** Can a correct consumer compute this from the library's
> existing outputs? If yes, it is not a base PR — no matter how convenient.
> (`layers_from`: no. `claimed`: no. An offset: yes — name a different node.)
>
> **(ii) A caller in the tree, in the same PR.** No primitive lands without a
> consumer *and* a test that fails without it. `layers_from` had `fold_band` and
> the F2 band; `Draw::Lines` clipping had a failing golden. `Anchor::Node` had
> neither, and it shows.

Keep both limbs; they are the good half. Retire the counterfactual.

---

## 4. Goal by goal

**Goal 1 — one library for everything on screen: honouring.** Context menus and
the dropdown chain are ordinary `Layer`s in the frame tree
(`view/shell/frame.rs:178-186`), the bar row is a native in-flow region
(`frame.rs:163-168`), and no surface is privileged. The caveat from review 2
stands and has grown sharper, not weaker: the surfaces are *expressed* in the
library's primitives but *authored* by the legacy walk (§2.3).

**Goal 2 — no hand-specified exceptions: bending, and improving.** The new
violation review 2 found (`OVERLAY_FAMILIES`) is gone — I verified there is no
occurrence of `OVERLAY_FAMILIES`, `is_overlay_key` or `overlay_start` left in the
tree — and `LayoutSpec::layers_from` replaced it with a fact the library reports.
That is the single best decision on this branch. What remains is the pre-existing
`layer_rank` table (`app/chrome/mod.rs:48-60`), a central ordered list of eleven
constants; the two entries for migrated surfaces (`MENU` at `chrome/menu.rs:34`,
`CONTEXT_MENU` at `chrome/context_menu.rs:47`) are honestly documented as blocked
on host leaves declaring `takes_raw_input`. Not new debt. Correct to leave.

Against that: `view/shell/menu.rs:208-222` is a hand-written exception in all but
name — a comment explaining why `Modality::None` plus press-ordering plus a
build-time open-ness snapshot combine to make a toggle work. §2.5 shows the
arrangement is also incomplete. It reads like the guard boxes the migration
exists to delete, relocated into a comment.

**Goal 3 — rebuild is cheap and side-effect-free: bending, unchanged since
review 2.** `frame_tree` is pure. The pipeline feeding it costs two full menu
walks per frame in release and three in debug (§2.4a), each allocating a `String`
per label run and per dropdown row. Review 2 raised this; nothing addressed it;
it is not on the do-now list. It is still not a gate — but the status bar
(~3,200 lines of drop heuristics) is queued behind the same pattern, and
"measure it later" has now survived two reviews.

**Goal 4 — identity is explicit: honoured.** `region_key`
(`view/shell/frame.rs:205`), `Key::Pair("menu_dropdown", depth)`
(`view/shell/menu.rs:188`), `Key::Str("context_menu")`. All author-supplied, all
inspectable. `widgets/keying.rs` remains exemplary — a deprecation shipped a
release ahead of the requirement, attributed per plugin and per widget. The one
wrinkle review 2 named (the key namespace carrying paint-band membership) is
gone with `OVERLAY_FAMILIES`.

**Goal 5 — one source of geometry: honoured for the frame and the context menu;
violated for the dropdowns, and more deeply than the doc admits.** The frame's
regions are the tree's (`regions_of`, `frame.rs:226`), and the context menu is
the model working: `clamped_position` deleted, `Fit::CLAMP` doing the arithmetic,
`menu_rect` reading the one answer back for the web `Scene`
(`view/shell/context_menu.rs:104-108`). Review 2's "build reads last frame's
layout" finding is properly closed (`menu_layout_in` takes the rect as an
argument, `app/render.rs:2498`).

The dropdowns are not merely "a placement the editor performs that the layer
could declare". The description carries `x`, `y`, `width` and pre-fitted strings
(§2.3), so *no* geometry for this surface is computed by layout — and the walk
that computes it runs three times per frame from three rect sources (§2.4a). The
module doc's framing understates this by a wide margin.

**Goal 6 — composition is the only extension mechanism: honoured.** Nothing
reaches past the public primitive set; `HostPainter` and `Palette`
(`view/shell/fold.rs:36-56`) are editor-side traits over the display list.

**Goal 7 — backend independence: honoured in shape, and one leak closed.** The
`Draw::Lines` clip moved into the library's backends *and* into `fold_band`
(`view/shell/fold.rs:184`), which is where review 2 said it belonged. The
suppression split (description built whether or not cells are wanted) still
holds.

The remaining leak is `ShellPalette::style` (`app/shell_host.rs:213-247`), and it
has got worse in a way worth naming: the mnemonic is encoded by *multiplying
names*. `menu.bar.item`, `menu.bar.item.mnemonic`, `menu.bar.item.active`,
`menu.bar.item.active.mnemonic`, `menu.bar.item.hover`,
`menu.bar.item.hover.mnemonic` — six names for two orthogonal attributes, i.e.
the product. Add "disabled" and it is twelve. This is the same pressure as §6.2
item 2 (dynamic colour) and item 4 (inline spans), showing up early, and it
argues that the fix for all three is structured names rather than more names.

---

## 5. The numbered questions

### Q1 — goal by goal

§4 above.

### Q2 — is `fresh-ui` still generic? Is `layers_from` an editor-shaped hole?

**Generic, and `layers_from` is a real capability, not a hole.** Three reasons,
in ascending order of strength:

1. The feature it serves — "a host paints content the tree does not own, and
   needs it between in-flow content and layers" — is not editor-specific. Any
   embedding host has it: a game with an engine-drawn HUD under its UI layers, a
   canvas app with its own painter, a native shell hosting a document view.
2. **It is underivable from outside**, which is the test that actually matters.
   A scrim carries no key and is pushed *before* its layer's own items
   (`crates/fresh-ui/src/render/paint.rs:39-64`); `widgets::Dropdown`'s layer
   carries no key at all (`crates/fresh-ui/src/widgets/menu.rs:131-132`). Any
   key-index derivation puts both on the wrong side, silently. Only the library
   can answer.
3. Its shape is minimal and total: one `usize`, plus `in_flow()`/`layers()`. It
   collapses to `0` under an opaque scrim (`paint.rs:54`), which is the correct
   behaviour — the host's own band then paints under everything and is hidden by
   the scrim, matching what an opaque modal means.

Would a from-scratch web or GUI backend want it? A web backend, yes and more so:
in-flow content patches into the document flow while layers become a portal /
top-layer subtree, and it needs to know where one ends. A GUI backend hosting a
native subview (a video surface, a map widget) wants exactly the same cut.

The one criticism: the API is exercised only by a demo band bolted onto
`examples/interactive.rs` for the purpose. That is a *test consumer*, not a real
one — legitimate here because the real consumer is `fold_band`
(`view/shell/fold.rs:151-155`) and it shipped in the same wave, which satisfies
the "caller in the tree" test I propose in §3. Contrast `Anchor::Node`, which
has no consumer of any kind.

So: the library has not been bent toward one consumer. If anything the
counter-pressure is visible — the migration has repeatedly chosen the
no-library-change option (§2, and review 2's closing paragraph), which is a
different failure mode from bending.

### Q3 — trajectory

**Converging on the mechanisms; accumulating on the surfaces. The gap is
widening, and the branch's own record is the evidence for both halves.**

Converging:
- Every guard box and pre-band grab this branch touched was replaced by a
  declared property, not by another special case. `chrome/menu.rs:16-29` is a
  `collect` that does nothing, with a comment listing the three boxes it deleted.
- The corrective reflex is real and fast. `c8ed601` found a bug that passed every
  test, built the harness capability that could express it
  (`mouse_click_with_repaint`), fixed the cause rather than the symptom, and
  deleted the mechanism (`was_active`) that made it possible.
- Review 2's do-now list of ten is eight closed, one partially, one open by
  argument. That is a good response rate for a list that hostile.

Accumulating:
- The migration is producing *descriptions of pictures*, not descriptions of
  content (§2.3). Three surfaces now have this shape: the bar (`BarItem.runs`
  pre-cut), the dropdowns (`DropdownLevel` with a rect), the context menu
  (`row_label` pre-padded). Every one of them will need a second migration later,
  and the second migration is the one that deletes code.
- The cost of the parallel derivation is not falling. Two-to-three menu walks per
  frame plus a `debug_assert_eq!` oracle is the shape of a surface being
  *duplicated*, not moved, and it must persist until the legacy walk is deleted.
- The two facts in §2.4 — "runs once" and "nested layers" — were both stated in
  the source and both wrong. That is the third review in a row to find load-
  bearing doc claims that the code contradicts (review 2's §Q9 found four). The
  rate is not improving.
- §2.5 is a mouse-visible defect in a wave the doc describes as having migrated
  pointer input.

Net: the direction is right and the mechanisms hold. But "wave N is done" is
being declared at *cell parity plus pointer parity*, and cell parity is
achievable while the whole content model stays outside the tree. Unless the exit
criterion changes, S3 will complete with every overlay in the tree and every
overlay's content still authored by `view/ui/*`, and S4–S9 will inherit it.

### Q4 — the open §6.2 items

**Colour that is not a theme name (item 2).** *Diagnosis right; the author's
answer is wrong and the earlier review's is right.* Review 2's four objections to
per-frame minted `dyn:N` all hold, and the first is decisive on its own: interning
during build makes `build` mutate a per-frame table, and goal 3 says "no side
effects" in as many words. `"rgb:7f3fbf"` is deterministic, stable across frames,
inspectable, needs no side table, keeps `build` pure, keeps display-list
assertions stable, and gives the web backend a name it can key a style rule on.
`ThemeKey` is documented as opaque to the library (`render/spec.rs:96-97`), so a
structured name is within contract. The backend cost is one `strip_prefix("rgb:")`
arm in a match that already has eighteen.

I would go one step further than review 2 and treat this as the same problem as
the mnemonic explosion in §4/goal 7: the theme namespace is already growing as
the *product* of orthogonal attributes. Settle a small structured grammar now —
`base.name[.modifier]*`, with `rgb:RRGGBB` as a leaf that resolves to itself —
before three more surfaces each invent their own spelling. That is a backend-side
convention, not a library change, and it costs nothing to write down.

**Placement offsets (item 3).** Diagnosis wrong (§2.1: the premise is false),
proposed resolution wrong (§2.6). Rewrite as a dependency, not a decision.

**`layer_rank` entries (deferred, review 2 §6).** *Diagnosis right, resolution
right.* `chrome/context_menu.rs:24-36` states plainly why the entry survives: the
PTY gate reads `blocks_terminal_input` off the overlay stack while the library
derives the same fact from `raw_input()`, which is only meaningful once host
leaves declare that they take raw input — and today every region is a
`PlainHost`. Deriving it now would report the terminal blocked on every frame.
That is a correct dependency, correctly documented at its site, retiring with
S5. Leave it. The one thing to add: `layer_rank::MENU` is *not* in the same
position — the menu's dismissal, modality and pointer routing have all migrated,
so its entry now exists only for the keyboard grab. Say which of the two reasons
each surviving entry has, or the honest note on `CONTEXT_MENU` will be read as
covering both.

### Q5 — what to stop, start, change

§6.

---

## 6. Stop / start / change

**Stop**

1. **Stop declaring a wave migrated on cell-and-pointer parity.** The dropdowns
   pass both while the description carries a rect, a width, and pre-fitted
   strings. Add a third criterion — *the tree measures this surface* — and say
   openly which waves have not met it.
2. **Stop settling library questions in prose while the code says otherwise.**
   Review 2 item 7 asked for one of two concrete things: let the layer place
   itself, or add a `debug_assert` that the tree's box equals
   `fit_dropdown_area`. Neither was done. What landed was a longer explanation of
   why it is fine. Two of that explanation's load-bearing claims are false
   (§2.4). If the answer to a review item is "no", the artifact should be a test
   that pins the current behaviour, not a paragraph.
3. **Stop adding `Place`/`Anchor` variants without a caller.** Six exist with no
   consumer and no test anywhere in the repository.

**Start**

4. **Start with §2.5.** Nest the dropdown chain as a bug fix, with a mouse test
   that clicks a submenu item. It is small, it is user-visible, and it happens to
   unblock everything the offset debate was about.
5. **Start writing an executable probe when a library gap is claimed.** Both
   decisive facts in this review came from ~80 lines against the real crate,
   in under a minute of build time. The claim "the library cannot express this"
   is cheap to falsify and was not falsified before it was written into two
   files.
6. **Start asserting the parity oracles in release, or accept they are decorative
   for shipped builds.** `app/render.rs:2702` is the only thing holding three
   derivations of the menu together, and it is `debug_assert_eq!`.

**Change**

7. **Change the base-PR test** from "would an unrelated consumer want this" to
   the two decidable ones in §3: *underivable from the library's outputs*, and
   *a caller plus a failing test in the same PR*. Keep both existing limbs.
8. **Change §6.2 item 3** from a decision into a dependency, with the flip and
   truncation numbers from §2.3 recorded so the next person does not rediscover
   them. Note that `Anchor::Node(row_above)` already expresses the offset, and
   that `Anchor::Node` has no test.
9. **Change the §6.2 answer for colour** to content-derived names, and settle the
   theme-name grammar at the same time (§Q4).
10. **Change §6.2's numbering.** It currently has two items numbered `0`, two
    numbered `3` and two numbered `4`, so "§6.2 item 3" is ambiguous between
    placement offsets and per-leaf `render_content` — in a document whose whole
    purpose is to be cited.

---

## 7. What is good

`layers_from` is the rule working: a fact the library had, could not be derived
from outside, taken as a base PR with a caller and a demo in the same change.
The `Draw::Lines` clip is the second limb working the same way. The context-menu
wave remains the reference for what a migrated surface looks like. `c8ed601` is
what a review response should look like — bug found in the running editor, harness
extended to express it, cause fixed, enabling mechanism deleted. And
`widgets/keying.rs` is still the best-scoped work in the effort.

The direction is not in question. What is in question is the exit bar, and
whether a wave can be called done while the surface it migrated is still a
picture the old renderer drew.
