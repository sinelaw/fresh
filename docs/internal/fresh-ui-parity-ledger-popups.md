# Popups: what the surface says, and what `fresh-ui` says back

The same exercise the prompt got (`fresh-ui-parity-ledger-prompt.md`), for
`view/popup.rs` (1990 lines) and `app/chrome/popups.rs` (541). Written before
any code, because that ordering is what caught the prompt's invented theme keys
and its two uncovered painter tests.

The method: enumerate what the painter and the chrome component actually
*enforce*, name the `fresh-ui` concept for each, and be explicit about the ones
with no concept — separating "the library needs this" from "we were encoding a
legacy artefact".

## What a popup is

`Popup` is already close to a description. It carries content
(`Text | Markdown | List | Custom`), a `position` strategy, a width, a
`max_height`, whether it is bordered, and a handful of behaviour flags
(`transient`, `focused`, `resolver`). No rectangle. `calculate_area` turns the
strategy into one, once per frame, and `render_with_hover` paints it.

That is the good news and the reason this surface is worth doing next: the model
survives almost intact. What moves is placement, painting, and the click rail.

## Rules a fresh-ui concept already states

| The popup's rule | The concept |
|---|---|
| 1. `AtCursor` — the popup's corner at the caret | `Anchor::Point` + `Place::Over` |
| 2. `BelowCursor` — below it, **or above when there is no room** | `Place::Below` + `Fit::FLIP` |
| 3. `AboveCursor` — bottom border one row above the caret | `Place::Above` |
| 4. Every strategy clamps to the area's absolute edges | `Fit::CLAMP` |
| 5. `Centered` | `Anchor::Screen(Align::Center)` |
| 6. `CenteredOverlay { width_pct, height_pct }` | `Anchor::Screen(Align::Center)` + `Sizing::Pct` |
| 7. `BottomRight` | `Anchor::Screen(Align::End)` |
| 8. `bordered`, with a title in the top border | `border()` |
| 9. `Clear` behind the popup, then a background style | a themed box fills its own ground |
| 10. Height is `content_height().min(max_height).min(available)` | `Sizing::Auto` under `max_h` |
| 11. A scrollbar when the content overflows, and not otherwise | `viewport().scrollbar()` |
| 12. `PopupContent::List` — selection, hover tint, click to select | `widgets::List` + `row_theme` |
| 13. Text and markdown wrap to the inner width | `text().wrap()` |
| 14. A pointer event inside a popup that no handler wants dies there | the absorb handler the prompt's popup already uses |
| 15. `transient` popups dismiss on the next input | `Dismiss::any_input` |
| 16. A popup that is not `focused` does not take the keyboard | `focusable()`, and focus is the tree's |
| 17. Several popups stack, the last one on top | layers paint in declaration order |

Seventeen of twenty. The two placement families that fall out of `Fit` are worth
dwelling on, because they are the shape of this whole migration: `BelowCursor`'s
"not enough space below, put above" is thirteen lines of arithmetic that
`Fit::FLIP` is the name of, and every strategy ends with the same
`right.saturating_sub(width)` / `.max(terminal_area.x)` pair, which is
`Fit::CLAMP` written out six times.

## Rules no fresh-ui concept states

One of the three turned out not to be a gap at all, and one is a boundary
rather than a gap. Both are kept here with the reasoning, because the record of
what was *not* needed is the part that stops the next surface reaching for API
it does not need.

### A. `AboveStatusBarAt { x, status_row }` — *withdrawn; the point was never needed*

The LSP-status popup hugs the status bar *and* sits at the column of the segment
that opened it. Both halves are already stated in the source, and they are
stated about different things:

> `status_row` is the actual row of the status bar in the current frame —
> passing it in lets the popup hug the status bar regardless of whether the
> prompt line is visible (which shifts the status bar by a row when it
> auto-hides).

That is `Anchor::Node(region_key(HostRegion::StatusBar))` + `Place::Above`,
exactly — and it is *better* than the current code, which has to be handed
`status_row` because it cannot ask. But the `x` is a point, and an `Anchor` is
one thing: a node, a point, or the screen. There is no "this node's row, that
column".

**Checked, and it is not a library gap.** The status bar is already migrated:
its elements are keyed `item_key(side, index)` and `status_bar::segments` reads
their rectangles off the laid-out tree. Following the caller confirms it —
`popup_dialogs` asks `status_bar_clickable_area_now(Lsp)`, which walks
`status_bar_clickable_rects_now()`, which is that same tree read, and then
throws the rectangle away down to `(status_row, col_start)` so it can be passed
as two numbers.

So the anchor is `Anchor::Node(item_key(..))` + `Place::Above`, both axes from
one node, and the popup hangs off *the segment that opened it* — which is what
the feature means and what the prose already says it wants. The `x` and
`status_row` parameters exist only because a popup could not name a node.

The alternative shape — a cross-axis offset on the layer (`.offset_x(n)`) —
would have been a number again, inviting exactly the drift anchoring removes.
Worth recording that it was considered and rejected: the first instinct on
seeing "one axis from a node, one from a point" is to add the missing axis to
the library, and the right answer was that the point was never needed.

The one thing to carry over is the reservation the current code makes:

> Reserve the rightmost column for the editor scrollbar. Without the
> reservation, a popup that overflows the right edge gets clamped flush to the
> area's right edge and its right border paints over the scrollbar of the split
> underneath.

`Fit::CLAMP` clamps to the frame, not to "the frame minus the split's
scrollbar". That is a real difference and it is the one part of this variant
that still needs somewhere to live — most likely as the anchor node being the
*chrome column* rather than the frame, once the split grid is a region with its
own rectangle (S5). Until then it is a one-column inset the description states.

### B. Text selection inside a popup

`PopupTextSelection` is a `((line, col), (line, col))` pair with `contains`, and
the chrome component drives it through `PointerGrab::PopupSelect`. `Draw::Selectable` exists in the library and is documented as:

> A region whose text the backend may let the user select. The library holds no
> selection model; this only says where selecting is meaningful.

So the boundary is already drawn, and it is drawn deliberately: the *region* is
the tree's, the *selection* is the host's. This is not a gap — it is a place
where the migration stops. Worth stating in the ledger so nobody later reads the
surviving `PopupTextSelection` as unfinished work.

### C. `Custom(Vec<String>)` content

A fourth content variant that is `Text` with a different name. No concept needed;
it is a merge, and it should happen before the description is written rather
than being carried across.

### D. `AtCursor` never clamped its `y` — *divergence, kept*

`calculate_area` clamps `x` for every strategy and never clamps `y` for
`AtCursor`. The rectangle is allowed to run off the bottom, and
`clamp_rect_to_bounds` then **truncates** it at paint — `height:
rect.height.min(max_height)` with the origin left where it was. A five-line
popup opened on the last row shows one line.

`Fit::CLAMP` pulls the whole box back inside instead. That is what clamping
means, it is what every other strategy already does horizontally, and a popup
cut to one line is not a popup anyone wanted. Kept as a behaviour change with a
test that names it rather than a silently adjusted expectation.

### E. A layer cannot align within its anchor — *closed, `Layer::align_to_anchor`*

`BottomRight` puts the popup at `height - popup_height - 2`, and the 2 is "leave
room for the status bar" — the same rule `AboveStatusBarAt` states properly.
Stating it properly is `Anchor::Node(status bar)` + `Place::Above`, and that
lands the popup at the bar's **left** edge; this variant wants its right.

A layer can already match its anchor's extent on the free axis
(`stretch_to_anchor`, added for the prompt). What it cannot do is *align* within
it. Those are the same concept at different settings — `stretch_to_anchor` is
the `Align::Stretch` case — so the shape is probably `Layer::align(Align)` with
`stretch_to_anchor` becoming its `Stretch` spelling rather than a second
builder.

`align_to_anchor(Align)` landed with `stretch_to_anchor` becoming its
`Align::Stretch` spelling rather than a second mechanism beside it — the
builder stays because "as wide as the thing it hangs off" reads better at a
call site, and its existing test passes unchanged through the new path, which
is the point of doing it that way round.

`BottomRight` is now `Anchor::Node(status bar)` + `Place::Above` +
`align_to_anchor(End)`. That is also a fix: the `- 2` was a guess at where the
bar is, and it is wrong by a row whenever the prompt line's visibility moves
it — the same failure mode `AboveStatusBarAt` was given a `status_row`
parameter to avoid.

### F. `Anchor::Point` is a point; a caret is a cell — *closed, `Anchor::Cell`*

`Place::Below` on `Anchor::Point(x, y)` lands on row `y`, because a point
resolves to a zero-size rect and "below" a zero-height thing is itself. The
painter means `cursor_y + 1`: below the caret's **cell**. `Fit::FLIP` is off by
the same one in the other direction (`anchor.y - sh` versus the painter's
`cursor_y - sh`).

This is not a bug in either — they mean different things, and the library is
right that a point is zero-size. `Anchor::Point` is what a *click position* is,
and the context menu uses it correctly. What is missing is the other one: a
cell. `Anchor::Cell(x, y)` resolving to a 1×1 rect makes `Place::Below` mean
`y + 1` and the flip mean `y - sh`, both exactly the painter's arithmetic, with
no arm anywhere reading "+1".

`Anchor::Cell` resolves to 1×1 and both callers now use it. `BelowCursor` is
exact against the painter across the sweep. `AboveCursor` is finding G below.

### G. `AboveCursor` covers the caret — *divergence, kept*

The painter disagrees with its own comment:

```rust
PopupPosition::AboveCursor => {
    // Position so bottom of popup is one row above cursor
    (cursor_y + 1).saturating_sub(height)
}
```

`cursor_y + 1 - height` puts the popup's *last* row on `cursor_y` — on the
caret, not above it. `Anchor::Cell` + `Place::Above` gives `cursor_y - height`,
which is what the comment says, and what the sibling `BelowCursor` already does
in the other direction (`cursor_y + 1`, clearing the caret's cell).

Kept, with a test that states both. A popup covering the character it is
anchored to is the bug `Anchor::Cell` exists to make unsayable, and the
asymmetry between the two siblings is the kind of thing that survives only
because the two arms were written as separate arithmetic.

### H. A popup is bounded by the window, not by the chrome column — *divergence, taken deliberately*

`calculate_area` is handed the *chrome area* — the column right of a left dock —
and every one of its six strategies clamps into it. A `fresh-ui` layer is placed
against the frame. With no dock the two are the same rectangle; with one, a
sweep over every reachable caret column and a range of widths says they part
company in exactly two places, and only two.

**Centred popups, whenever a dock is open.** The painter centres in the column,
so a modal slides sideways when a dock opens and sits off-centre in the window
for as long as it is there. The surfaces that use `Centered` and
`CenteredOverlay` are modals — workspace trust, Live Grep's 80% overlay — and a
modal centres on the window in every other piece of software. Taking the frame
is the conventional answer, and it also stops the popup moving because of
something unrelated to it.

**A popup wider than the chrome column.** The painter pins `x` to the column's
left edge, and `clamp_rect_to_bounds` then cuts the right end off. The tree
pushes it flush to the window's right edge: whole, lying over the dock.
Truncation loses content; overlap loses none. A floating surface over a panel is
what floating means.

*Not* a divergence, though it looks like one: a caret to the **left** of the
dock's edge, which the painter guards with `.max(area.x)`. The caret is in the
buffer and the buffer is inside the chrome column, so the input is unreachable —
the guard defends against a state that cannot occur.

This is the one finding where the ledger's usual instinct is inverted. Elsewhere
a difference is a thing to justify or undo; here the painter's rule is the
accident — it follows from *which rectangle happened to be passed in*, not from
a decision anyone made about popups — and matching it would have meant adding a
"confine this layer to that region" concept to `fresh-ui` with no other caller.
The probe is what settled it: the concept looked necessary until the sweep
showed the disagreement was two rules wide, both of them improvements.

### I. Dismissal claims the press, and a transient popup's must not — *closed*

The two guards (`chrome:transient_guard`, `chrome:popup_guard`) were the last
part of this surface answered outside the tree, and `Dismiss` was the concept
that should replace them. It did not fit, for one reason.

`dismiss_for_pointer` runs for any button and *claims* for the primary one, and
the comment at that site says why: "a left click outside a menu is spent closing
it — that is the whole gesture." True for a menu. Not true for a hover popup:
the guards dismissed and returned `PassAfter`, so clicking into the buffer while
a tooltip is up both dismisses it and places the cursor. Under `Dismiss` that
first click would have been eaten — a tooltip charging the user a click to get
rid of it.

So `Dismiss` says which of the two it is. `pass_through` is the rule the wheel
already follows two hundred lines above it — act, and claim only when the act
was the whole of it — applied to the pointer, and it is what a transient popup's
layer declares. The claim is what the *host* is told; the tree's own handlers
run either way, which is the distinction the test pins.

Both guards are gone, and `is_mouse_over_any_popup` with them.

**One thing fixed at the same time.** `dismiss_for_pointer` decided "inside the
layer" from `hit_test`, the topmost path only — the same first-path assumption
that was wrong in `scroll_chain` and `scrollbar_hit`. A popup whose own title
strip is transparent puts the press on a path that reaches behind it too, and
only one of the two says the press was inside; this now asks every path. It was
recorded as unreachable when the guards still stood, and became reachable the
moment the layer started declaring its own dismissal.

## What this retires

Written before the wave; what follows each item is what actually happened.

* `calculate_area`'s six strategies, which become five anchors and a `Fit`.
  **Done, and the function with them.** `asked_size` is what is left — the size
  half, which all six computed identically — and the parity sweep that proved
  the port has been rewritten as a table of rectangles, because a dead function
  kept alive as a test oracle is the enabling mechanism the review says to
  delete once it has been used.
* `PointerGrab::PopupScrollbar` and its drag, on the same "one dead root" chain
  the prompt's scrollbar grab turned out to be. **Done** — and
  `PointerGrab::PopupSelect` went with it, which this list did not expect
  either: pointer capture is what a selection drag needs, and the library has
  it. Along with
  `handle_click_popup_scrollbar`, `handle_popup_scrollbar_drag`,
  `dragging_popup_scrollbar` and `scroll_popup`: the bar is the viewport's and
  `hit.rs` owns the press on its gutter.
* Three chrome boxes: `chrome:popups`, `chrome:popup_scrollbar`,
  `chrome:transient_guard`, plus `chrome:popup_guard`. **All four are gone**,
  and `Popups::collect` with them — the component contributes no geometry at
  all now. What each became: the scrollbar track is the viewport's gutter, the
  opaque rect is a press-absorbing gesture inside the popup's own frame, and
  the two guards are `Dismiss::OUTSIDE_POINTER.passing_through()` (finding I).

  Finding B moved too, which this list did not expect. A link click and the
  press that starts a text selection wanted a position *inside* the content,
  and the reading was that the library had no drag gesture to extend a
  selection with. It has pointer capture, which is the same thing said properly:
  the press captures, and `Event::local` on a gesture *inside* the viewport is
  already in the content's coordinates — no inner rectangle to subtract, no
  stored `scroll_offset` to add back. What a press *means* is still the host's,
  which is what finding B was actually about.
* `ChromeLayout::popup_areas` and `global_popup_areas` — the last two entries in
  the paint-recorded roster that this migration can reach. (`workspace_trust_dialog`
  and `Window::file_browser_layout` are the two modals, which come after.)
  **Still here, and honestly so.** Nothing routes input through them any more.
  What is left reading them is geometry that is genuinely wanted elsewhere: the
  web scene projects popups from them, and the hover keep-alive asks whether the
  pointer is over one. They are recorded *from the tree* rather than computed,
  so they are a cache of the tree's answer instead of a second derivation of it
  — which is the state the explorer's rects passed through before they went.

## How each rule is tested

Rules 1–17 get shell-level unit tests in `view/shell/popup.rs`, in the style the
prompt's and the status bar's already use: build the description, lay it out,
dispatch an `Input`, assert the `UiFact` or read the rectangle back. The
placement family (1–7) gets a table test over cursor positions near each edge,
which is where `Fit` earns its keep and where the hand-written clamps are most
likely to have disagreed with each other.

The painter's own tests get the same treatment the suggestion painter's did:
each one's *rule* is located in the new tests before the painter is deleted, and
any rule without a home gets a test written for it first. That step is not
optional — it is what caught the two scrollbar-presence rules that would
otherwise have been dropped silently.
