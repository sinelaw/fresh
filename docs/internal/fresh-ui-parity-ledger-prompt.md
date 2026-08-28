# Parity ledger — the prompt

The prompt is the next surface to migrate: suggestion list (bottom-anchored
dropdown and floating-overlay forms), its scrollbar, the overlay preview pane,
the toolbar, and the row the input itself occupies.

**Why this document exists.** Nine invariants have been silently dropped so far
in this migration — the status bar's width budget, Escape closing the menu, the
`hjkl` arms, the press-vs-release gestures on three surfaces, the wheel step, the
explorer's union box, the caret's background. Every one had the same shape: the
old code enforced a rule that nothing wrote down, the description did not carry
it, and no test noticed. Every one was found by CI, days later, and root-caused
backwards.

So this ledger is written **before** the painter is touched, not after. Each rule
below is a thing `view/ui/suggestions.rs`, `view/ui/status_bar.rs::render_prompt`
and `app/chrome/prompt.rs` enforce today. Each gets a test that fails if the
description drops it.

The second column is the point. This is not a line-by-line port: the question for
each rule is **which fresh-ui concept states it**. Where a concept exists, the old
mechanism is deleted rather than translated. Where none does, that is a finding
about the library — recorded at the bottom — and *not* something to reimplement
in consumer code by reflex.

## Rules that a fresh-ui concept already states

| # | Rule the old code enforces | Concept that states it | What gets deleted |
|---|---|---|---|
| 1 | The list shows at most `MAX_VISIBLE_SUGGESTIONS` rows, windowed around the selection | `list().windowed(..)` | the manual `start_idx` / `visible_count` window |
| 2 | Hovering a row reports its index; clicking selects | `list().on_select(..)` | `hover()`, `handle_click_suggestions` |
| 2b | Double-clicking always confirms | **none — see finding B** | (not yet carried) |
| 3 | The selected row is highlighted | `list().selected(i)` | the style ladder in the painter |
| 4 | A scrollbar appears when the list overflows; pressing the gutter jumps, dragging follows | `list().scrollbar()` — `hit.rs` owns press-to-jump and drag | `handle_click_prompt_scrollbar`, `prompt_scrollbar_offset_for_row`, `chrome:prompt_scrollbar` |
| 5 | The list is placed under the input row, or centered as a floating overlay | `layer().anchor(..).place(..).fit(..)` | `suggestions_outer_area` / `prompt_results_area` placement arithmetic |
| 6 | While the overlay prompt is up, clicks that reach the body never move the buffer cursor | the body host declines the pointer — see finding C | `chrome:overlay_prompt_scrim` (z15) |
| 7 | …and neither do double- or triple-clicks | same rule, same place | `chrome:overlay_prompt_modal` (z160) |
| 8 | …and neither does any right-click, plain or Ctrl+ | same rule, same place | `chrome:overlay_rclick_guard` (z195) |
| 9 | A toolbar control takes the click, moves keyboard focus to itself, and Tab continues from there | keyed `focusable()` nodes + a gesture each | `prompt_toolbar_boxes`, `prompt_toolbar_origin`, the `hit_path` walk |
| 10 | Escape closes the prompt | `Dismiss::ESCAPE` on the layer | the key arm |
| 11 | The input row scrolls horizontally to keep the caret visible | `input_hscroll` stays — it is a text-model rule, not layout | (nothing; kept deliberately) |

Rules 1–10 are all **deletions**. That is the test of whether the library earns
its place on this surface: ten hand-rolled mechanisms, ten existing concepts.

## Rules no fresh-ui concept states

These are the findings. Each is a place where porting line-by-line would move
layout logic into consumer code and quietly invert the "principled" claim.

One of them (D) was withdrawn on a second reading — it is recorded rather
than deleted, because "this looks like a missing concept and is not" is the more
useful half of the exercise.

### A. Space priority — *third occurrence*

The suggestion row is four columns — name, keybinding, description, source — and
the rule is a **yield order**, not a placement:

- keybinding and source columns exist only when some visible row supplies them,
  and free their width otherwise;
- the name column is sized to the longest visible name, so *names are never
  truncated while room remains*;
- the description absorbs the squeeze first, the source column last;
- for path-like names (no keybinding, no source) the name takes up to 60% of the
  row instead.

`flex` cannot say this. `prim.rs` resolves children in order against the space
that is left — that is placement, not precedence. This is the **same gap** that
produced `left_budget` for the status bar, where the rule was "reserve the right
side before spending the left".

Two occurrences was a coincidence; three is a missing concept. The options:

1. Write a second bespoke budget function beside `left_budget`. Cheapest now,
   and the point at which "the editor keeps re-implementing layout because the
   library cannot state it" stops being a risk and becomes the design.
2. Give fresh-ui a yield order on flex children — a priority that says who gives
   up space first, independent of declaration order. Both call sites collapse to
   a declaration, and `left_budget` is deleted rather than joined.

**Recommendation: (2), before the prompt's description is written.** This is the
first time the migration would pay to extend the library rather than route
around it, and it is exactly the check that keeps the thesis honest.

### B. `List` cannot tell a single click from a double — *closed, `List::activate_on`*

The widget wires rows to `GestureKind::Click` and lets `on_activate` win over
`on_select` on that same single click:

> A click both moves the selection and activates the row … activation wins when
> both are present.

The prompt needs the two separated. A single click selects, and confirms only
when `prompt_type.click_confirms()` says a click commits; a double click always
confirms, which is the mouse-only commit path for the prompts that merely
preview on a single click (issue #1660). Setting both handlers would confirm
every click; setting only `on_activate` would lose the preview prompts.

Wiring `on_select` alone gets single-click exactly right, because
`select_suggestion` already carries the `click_confirms` decision. The
double-click rule has **no expression** and is not yet carried.

`Event::clicks` already exists and the framework already counts runs — the
editor's own multi-click detector hands it in on every press. So the gap is in
`List`'s handler signature (`Fn(usize) -> M`), not in the model underneath.

**Closed by `List::activate_on(Activate::Click | Activate::DoubleClick)`.** Not
a click-count on the handler, in the end: which click commits is not a property
of one activation, it is what kind of list this is. A palette row commits on the
first click because selecting it *is* choosing it; a file list selects on the
first and opens on the second because the user may want to look at what they
picked. Stated once, on the list, it also says what the *first* click of a
double-click list does — which a per-call count would have left to each handler
to work out.

`Activate::Click` is the default, so every existing caller and every test on
them is unchanged. The prompt now sets both handlers: `on_select` carries the
`click_confirms` decision it already carried, and `on_activate` is the
unconditional commit. `handle_click_suggestions` and
`handle_click_suggestions_confirm` — the two coordinate hit-tests that recovered
an index the row already knew — have nothing left to do once the rail moves.

### C. Position-blind wheel capture

`chrome:prompt_suggestions` is a **full-frame** box at z155 whose only job is:
while a prompt with suggestions is open, the wheel scrolls that list *wherever
the pointer sits*. fresh-ui routes the wheel by position — a viewport scrolls
when the pointer is over it.

This is deliberate behaviour (the bottom-anchored dropdown is small and the
pointer is usually elsewhere), so it cannot simply be dropped. No concept states
it. Smallest honest options: a `wheel_capture` flag on `Layer`, or keep one
editor-side arm and record it as residue with a test.

### D. Per-gesture modality — *withdrawn; it was the wrong reading*

Recorded first as a gap, then found not to be one. Kept here because the
correction is the point: the shape of the old encoding suggested a library
concept that is not needed, and reaching for it would have added API for a
legacy artefact.

The overlay prompt looks modal for some gestures and transparent for others.
Its click scrim rides low (z15) so chrome peeking out from under the overlay —
tabs, scrollbars, the status bar — still takes clicks, while the wheel and
double-click bands sit high (z155/z160) and swallow everything, and a
right-click guard sits higher still (z195). Three boxes across three bands for
one surface. `Modality` is all-or-nothing, so the obvious conclusion was that it
needs per-gesture granularity.

It does not. Every one of those three guards exists to stop a gesture reaching
**the editor body** — moving the cursor, word-selecting, line-selecting, opening
a context menu. None of them is about the overlay at all. The rule is:

> the body does not act on the pointer while an overlay prompt owns the keyboard

which is a statement about a host leaf, not about a layer, and host leaves
already decide what they do with input. Expressed there it is one rule instead
of three boxes, and it needs nothing from the library.

The z-band encoding is what made it look otherwise: "the body ignores the
pointer" had to be written as "cover everything below z15", and once it is a
covering box it acquires a z, and once it has a z the other two gestures need
their own. `Modality` stays all-or-nothing.

### E. `List` stamps its own theme vocabulary — *closed, `List::row_theme`*

`List` set each row's theme to `list.row`, `list.row.selected`,
`list.row.selected.blur` or `list.row.hover`, **overwriting** whatever the row
builder had named. That is a good default and a bad only option: this editor's
theme has no entry for any of those four names, and `shell_theme::resolve`
falls back to the plain editor ground for a name it cannot resolve *silently*.
The first `List` in the editor would have drawn every row — selection included
— in the buffer's own colours, with nothing failing anywhere.

Not derivable. A host cannot compute the name itself, because two of the four
states are the widget's private business: `hovered` and `focused` live in
`ListState`, mirrored from Enter/Leave and focus transitions. Nor can it paint
underneath: the stamped fill is emitted before the row's content, so a host
covering it is paying for a fill it does not want and losing hover with it.

`List::row_theme(|index, RowState| -> String)` is the split the rest of the
library already draws: **the widget owns the state machine, the host owns the
palette.** `RowState::theme()` is still the default when no host names one, so
the existing vocabulary and every test on it stand. Caller and test land with
it: `view::shell::prompt::theme` is the whole of `row_base_style`,
`keybinding_style` and `source_style`, and it made the painter's hover arm
reachable — which the wrapper approach would have thrown away.

The guard for the other half of this is host-side:
`every_theme_name_is_a_real_key` walks every name `prompt.rs` can emit and
asserts both halves resolve. An earlier draft of that module used five keys
that exist nowhere; nothing failed, every row simply painted grey.

### F. No ellipsis, and no truncation direction — *closed, `Node::elide`*

`priority` decides a column's width during layout, which is the point — but it
means the host can no longer truncate its own text, because it does not know
the width until layout is over. The library clips instead, so
`truncate_tail_ellipsis`'s `…` is lost, and so is the file finder's rule that a
*path* truncates at the head so its filename survives while a *command name*
truncates at the tail.

The two are one concept: a run says how it gives up cells, and which end
survives is part of what it says. `text(..).elide(Elide::Tail | Elide::Head)`,
resolved at **paint** rather than at measure — that is the crux. Measurement
reports the natural width; it is layout that decides the run gets less, so the
number to cut to does not exist until the rectangle does.

Both directions have a live caller in the same change: the prompt's name column
takes `Elide::Head` when the list is a file finder, so a path keeps its
filename, and `Elide::Tail` otherwise, because "Toggle Compose/Preview (All
Files)" contains a slash and is still a command name. Description and source
take the tail form. The status bar's segments want it too, and the explorer's
truncated filenames after that.

Cells rather than characters, which is not pedantry: `ColumnLayout` carries a
regression test for a panic from truncating a description at a multi-byte
boundary, and counting `char`s would also put the mark one cell early for every
CJK glyph.

One cosmetic difference, deliberate: the painter wrote `...` for a description
and `…` for a name. The library writes `…` for both, so the mark costs one cell
everywhere and the arithmetic is `width - 1` rather than a second measurement.

## How each rule is tested

Rules 1–5 and 9–11 get shell-level unit tests in `view/shell/prompt.rs`, in the
style the status bar and menu already use: build the description, lay it out,
dispatch an `Input`, assert the `UiFact`. Rules 6–8 get one test each that a
press/double/right-press outside the card produces no buffer-cursor message.
Finding A gets a table test over row widths mirroring `left_budget`'s.

None of these needs the editor to be running, and all of them fail today if the
description is written without the rule.
