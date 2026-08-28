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
| 2 | Hovering a row reports its index; clicking selects; double-clicking confirms | `list().on_select(..).on_activate(..)` | `hover()`, `handle_click_suggestions`, `handle_click_suggestions_confirm` |
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

One of the three (C) was withdrawn on a second reading — it is recorded rather
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

### B. Position-blind wheel capture

`chrome:prompt_suggestions` is a **full-frame** box at z155 whose only job is:
while a prompt with suggestions is open, the wheel scrolls that list *wherever
the pointer sits*. fresh-ui routes the wheel by position — a viewport scrolls
when the pointer is over it.

This is deliberate behaviour (the bottom-anchored dropdown is small and the
pointer is usually elsewhere), so it cannot simply be dropped. No concept states
it. Smallest honest options: a `wheel_capture` flag on `Layer`, or keep one
editor-side arm and record it as residue with a test.

### C. Per-gesture modality — *withdrawn; it was the wrong reading*

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

## How each rule is tested

Rules 1–5 and 9–11 get shell-level unit tests in `view/shell/prompt.rs`, in the
style the status bar and menu already use: build the description, lay it out,
dispatch an `Input`, assert the `UiFact`. Rules 6–8 get one test each that a
press/double/right-press outside the card produces no buffer-cursor message.
Finding A gets a table test over row widths mirroring `left_budget`'s.

None of these needs the editor to be running, and all of them fail today if the
description is written without the rule.
