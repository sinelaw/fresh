# The agent chat surface — known gaps

> _**PLANNED — none of this is implemented.** A punch list of accepted defects
> and agreed changes to the agent chat, recorded so the reasoning survives
> until someone picks them up. Everything the chat does today is described in
> [agent-control-plane.md](agent-control-plane.md) §7 (Home) and §8 (the
> mailbox); this doc only covers what is wrong with it._

The chat exists twice over one transcript model: as a column in **Home**, and
as a collapsible section at the bottom of the **dock**. Both are built by the
orchestrator plugin from the same `WidgetSpec` vocabulary, and both address an
agent through a completion popup on a single-line text field. That sharing is
what makes the items below worth writing down rather than fixing ad hoc — most
of them are host-side widget behaviour rather than plugin layout, so a
plugin-local patch would fix one surface and leave the other alone. That failure mode is not hypothetical:
Enter-to-accept in the popup already took different paths on the two surfaces
— Home forwards the key to the panel via a mode binding, the dock routes it
through the input router — and a fix verified on one of them left the other
broken until it was caught by hand.

## 1. The completion popup should lose its vertical borders

**Today.** The popup renders as a full box: a top border row, a bottom border
row, and a `│ … │` pair of side columns on every candidate row.

**Wanted.** Drop the left and right edges. Keep the horizontal rules — they are
what separates the candidates from the transcript behind them, and the popup is
an overlay drawn over live content, so it does need *some* boundary.

**Why.** The right border is not visible in practice. The popup is sized to its
content and floats inside a panel that ends shortly after it, so the right edge
either lands under the panel's own border or is clipped away — it costs a
column and paints nothing the user sees. Once the right edge is gone the left
one is no longer a frame, just a stray rule down one side, so the pair goes
together.

**Where.** Host-side, in the widget renderer's completion-popup path — the
border rows and the per-row side columns are emitted there, not by the plugin.
Because the popup is shared with every other text field in the product
(settings, the dock's search box), this wants a variant rather than a change to
the only shape available.

## 2. The composer should lose its label and its brackets

**Today.** The message field renders as `to [which agent?    ]` while picking
and `[@proj/codex-probe    ]` once addressed.

**Wanted.** No `to` prefix, no `[` `]` around the field.

**Why.** Both are chrome that restates what the surface already says. The
section header already reads `▸ chat · → @name`; the field is the only input on
the row, so bracketing it distinguishes it from nothing. The brackets also make
the address look like a token in a form rather than the start of a sentence you
are writing.

**Where.** This splits in two, and the split is the reason it is not a
five-minute change:

- The `to` label is a plugin-side string — the chat spec passes it only while
  picking, and passing empty is the whole fix.
- The `[` `]` are **host-rendered**, unconditionally, by the text-input
  renderer, which composes every field as *optional label, `[`, inner, `]`*.
  Every text field in the product gets them. Removing them for the composer
  needs a bare/borderless variant on the widget, not a plugin change.

## 3. Transcript layout: author on its own row, message flush left, wrapped to the chat's width

**Today.** Messages render with the author inline and the body indented to a
gutter under it, so a sent line looks like:

```
         you ▸ @proj/codex-probe run
             ▸ the whole suite
             ▸ please
```

The body is pushed right by the author gutter and the `▸` continuation marker,
and it wraps against that reduced width.

**Wanted.**

- The author (`you`, or the agent's name) on its **own row**, left-aligned,
  before the message.
- The message body left-aligned **flush to the chat area's edge** — no gutter,
  no continuation marker.
- Word-wrapped to the **full width of the chat area**.

**Why.** The gutter costs about a dozen columns on every line of every message,
in the narrowest column of the layout — the dock chat is a fraction of an
already narrow panel. Prose wrapped to what is left breaks every few words, and
the fragment above ("run / the whole suite / please") is what a one-line message
looks like under it. Hoisting the author to its own row spends one row per
message and buys those columns back on all of them, which is the right trade for
anything longer than a sentence. It is also the conventional shape for a
transcript, so it reads as a conversation rather than a table.

**Note.** The width this wraps against is measured wrong today — see §7.

## 4. A narrow dock pushes the composer off the bottom edge — layout bug

**Today.** Narrow the dock until the toolbar's search field wraps below the
`New Task…` button, and the chat's input field disappears past the dock's
bottom edge.

**Wanted.** The toolbar growing a row must take that row from the **workspace
list**, which is the flexible region. Nothing below the list may move. This is
already the stated invariant for the chat itself — the chat's rows come off the
top of the tree's budget precisely "so adding it shortens the list rather than
pushing anything off the dock's bottom" — the toolbar simply does not honour
it.

**Why it happens.** Two independent causes, both of which can produce the
symptom on their own:

1. **The wrap is predicted against the wrong width.** The dock spec computes
   whether the toolbar wraps by measuring the button and search field against
   the dock's *default* width — a fraction of the terminal, clamped — and adds
   the resulting 1-or-2 rows to the chrome budget that the list is sized
   against. But the **host wraps against the actual rendered width**. The
   estimate carries a slack constant and a floored field width, so the two
   agree only when the real width equals the default. When they disagree the
   host draws two toolbar rows while the budget was computed for one; the list
   is sized one row too tall, and since the list sits above everything else,
   the error lands at the bottom edge and evicts the composer. The code
   comment acknowledges the estimate "only needs to be close for the
   default-dock case" — this is the case where that is not true.

2. **The list has a hard floor.** The list height is clamped to a minimum of
   six rows. On a short dock, chrome plus that floor can exceed the available
   height, and the overflow again leaves at the bottom. This one needs no
   width mismatch at all — it is reachable by making the dock short rather
   than narrow.

**Where.** Fixing (1) by refining the estimate is treating the symptom: the
plugin cannot see the width the host actually rendered at, so any arithmetic it
does is a guess that drifts. The durable fixes are to have the host report the
wrap (or the real content width) back to the plugin, or to pin the composer to
the bottom edge structurally so it cannot be pushed off by a mis-sized sibling —
which also disposes of (2).

## 5. The wheel scrolls the workspace list wherever the pointer is — bug

**Today.** Turning the mouse wheel anywhere over the dock scrolls the workspace
tree, including when the pointer is over the chat transcript.

**Wanted.** The wheel moves the element under the pointer, and nothing else.
Over the transcript it scrolls the transcript.

**Why it happens.** The host's panel wheel routing is already position-aware
and already correct: it looks up the pointer's row/column in the panel's
emitted **scroll regions** and scrolls the widget it lands in. Regions are
emitted for every keyed `List`/`Tree` whether or not it overflows, precisely so
that a wheel over a short list is not rerouted to a scrollable sibling
elsewhere on the panel.

The transcript emits no such region, because it is not a list. It is a `raw()`
block of pre-rendered rows. So the hit test finds nothing under the pointer,
and the lookup falls through to a last-resort `find_scrollable_widget_key`,
which returns the **first** `Tree`/`List` in declaration order — the workspace
tree. The wheel is not being misrouted so much as defaulting, because as far as
the host is concerned the pointer is over nothing scrollable at all.

This means the bug is not in the routing and cannot be fixed there. Suppressing
the fallback would only make the wheel dead over the chat instead of wrong.

## 6. The transcript needs a scrollbar when it overflows

**Today.** There is none, and there is nothing for one to attach to: the
transcript is not a viewport onto a history, it is a slice. Each render
recomputes the wrapped lines, pads them to the section's height, and keeps the
**tail** — the newest message is the one you came to read, so the block is
pinned to the bottom. Anything above the cut is not scrolled out of view, it
was never emitted. There is no scroll offset, no total height, and so no
scrollbar geometry to draw.

**Wanted.** A scrollbar whenever the history exceeds the visible rows.

**Why it is the same work as §5.** Both need the transcript to stop being a
computed slice and become a real scrollable region: a scroll offset in the
plugin's chat state, a full line count to clamp against, and a keyed widget the
host emits a scroll region for. Once that exists, §5's hit test finds the
transcript on its own and the wheel routes correctly with no change to the
routing code, and the scrollbar has geometry to render.

Two behaviours to preserve when it lands, both of which the slice gets for free
today and a viewport will not:

- **Stick to the bottom.** While the user has not scrolled up, new messages
  must keep the view pinned to the newest — the same "follow the tail unless
  the user took over" rule the multi-line Text wheel path already implements
  with its `user_scrolled` flag.
- **Don't fight the composer.** Scrolling the transcript must not move focus
  out of the input or disturb the caret; the composer is the panel's last row
  and the thing the user is typing into.

Worth deciding at the same time: whether this becomes a general scrollable
`raw()` region in the host, or whether the transcript is rebuilt on the
existing `List` widget, which already has regions, scrolling, and a scrollbar.
The second is less new host surface but forces the transcript's wrapped prose
into list rows.

## 7. Cross-cutting: the plugin measures against the default width, not the real one

§3 and §4 are the same root cause seen from two angles. The chat's text width
and the toolbar's wrap prediction are both derived from the dock's *default*
width — a function of terminal size and clamps — and neither has any way to
learn the width the dock was actually drawn at. Everything downstream of that
number (word wrap, truncation, the chrome budget, and so the position of the
dock's bottom edge) is correct only while the two coincide.

Whoever takes §3 or §4 should decide this first, because fixing either one
against the guessed width bakes the guess in deeper.

## Not in this pass

One further gap, recorded here because it was established while tracing the
send path and belongs with the rest, but which is **not** part of the numbered
items above and should not be bundled into them:

**Delivery is reported, receipt is not.** Pressing Enter writes one file into
the target agent's `inbox/` and reports `delivered`. Nothing is injected into
the agent's terminal — by design (see agent-control-plane.md §8.1) — so the
agent acts only when it next polls its inbox, which its briefing tells it to do
at the start of each turn. If the agent is mid-turn the message waits for the
turn boundary. If the agent is parked at an interactive prompt — precisely the
state the list flags as `waiting`, and the state a user is most likely to be
replying to — it is not running a turn loop at all, so it may never poll, and
the message can sit undelivered-in-fact while the UI has said it was delivered.
The unmoved file in `inbox/` is the evidence, but only after the fact; nothing
surfaces the discrepancy at send time.
