# Orchestrator: the layout the plugin still does by hand

> _Open-work register. Written after the dock-and-dialogs redesign
> (sinelaw/fresh#3251) landed on the retained-mode tree. Nothing here is
> required to merge it; two items are visible defects and the rest are the
> difference between conforming to the refactor and compiling after it._
>
> _The order matters more than the list. One item — making the form's section
> builders functions of their input — is a precondition for most of the
> others, and doing it first may retire some of them outright. Every fix here
> is host-side or plugin-side ordinary code; only one still argues for a new
> field on the wire, and it argues for it last._

## What this is and is not about

`retained-mode-ui.md` states a done-criterion under *Working rules*: **a
description with a rect, a width or a pre-fitted string is still a picture the
old renderer drew.** It is worth being careful about how far that reaches. It
is written about the editor's *own* descriptions, with
`Editor::status_bar_description` as its live example — a surface where the tree
could simply take the work back.

A plugin is a different contract, and the wire format is *designed* to carry
measurements: `Text { field_width, max_visible_chars, rows }`, `label_width`,
`List { visible_rows }`, `LabeledSection { width_pct }`,
`TextPropertyEntry { pad_to_chars, truncate_to_chars }`, `widthPct` and
`heightPct` at mount. A plugin that says how wide its field is has not broken a
rule.

What makes the sites below worth listing is narrower and testable:

1. **The host already knows the answer at description-build time.**
   `view/shell/panel.rs:758` builds the whole widget subtree inside
   `fresh_ui::layout_reader(|info| … info.constraints.max_w …)`, and the
   comment above it names this exact failure: *"The alternative is the caller
   computing the percentage itself, which is the second layout this migration
   exists to remove."* Every width guessed in `orchestrator.ts` is a second
   layout, computed a frame behind the first.
2. **A measured value goes stale and nothing re-measures it.** See the first
   item — this is not a purity argument, it is a bug you can reproduce by
   dragging a terminal edge.
3. **The same file already has the idiom.** `flexLine`, in the same plugin:
   *"The host right-aligns the right group to the row's actual width … **no
   plugin-side width estimate needed**."* Two footers in this branch, two
   strategies.
4. **fresh-ui has a sanctioned mechanism for this and the editor's own shell
   uses it everywhere.** `layout_reader` — *"build a subtree from the
   constraints this node is given"* (`desc.rs:1188`) — is how the tab strip
   caps names (*"full names when they all fit, twenty-five columns each when
   they do not"*, `tabs.rs:25`), how the sidebar draws walls as long as the
   body turns out to be, and how the keybinding overlay resolves its width:
   *"content resolved from a **known** extent rather than geometry recorded
   from a paint"* (`keybinding.rs:18`). The intended end state goes further —
   *"`BuildCx` should expose no geometry at all; only a `LayoutReader`'s
   closure should receive a `Geometry`"*. Every decision below belongs inside
   one, and `panel.rs:758` already puts the whole widget subtree there, so
   `widgets.rs::node()`'s `width` parameter **is** layout's answer.

**The constraint that shapes every fix below.** That same doc line: the builder
*"runs during layout, **may run more than once per frame** under intrinsic
sizing, and **may not call `set_state`**."* So work moved host-side must be a
pure function of (spec, width) — which is exactly why the plugin's
mutate-and-restore measuring could never be transplanted as it stands, and why
purity is the first thing to fix rather than the last.

**The counts, at HEAD.** `editor.stringWidth` reads **3**: two inside
`clipToWidth` and one in the machines table's `pad`, which is one feature site,
not three. `getScreenSize()` reads **5** against **4** on the branch point — a
single new call, the form's fixed-versus-compact fit test. Both figures were
several times larger when this register was first written, and the work below
is what closed the gap; the numbers are restated here because a register whose
arithmetic is stale is worse than no register.

**A warning to anyone citing this document.** Earlier revisions carried
`orchestrator.ts:NNNN` line references throughout, and every one of them went
stale within a day of being written — the file is fifteen thousand lines and
this branch moved most of them. Items below name *functions*, which survive an
edit. Where a line number appears it is for a file that is not moving.

---

## What is open

### ~~The sections are nullary closures over a module global~~ — done, all but one

`connectionFields()`, `modeTailFields()`, `cmdField()` and
`projectPathFields()` all took no arguments and read the module-level `form`.
That was the root cause of everything in this item: a builder that is not a
function of its input cannot be asked "how tall would you be in *that* shape"
without first making the global *be* that shape.

So `measureVariant` saved six fields of the live form, mutated them, called the
builder, and restored in a `finally`, six times per fixed render.

**Closed.** `connectionFields(f)`, `modeTailFields(f)` and `cmdField(f)` take
the form they describe; `measureVariant` is deleted and `rowsOf(f, build)`
replaces it, so a candidate shape is a spread and there is nothing to put back.
**`projectPathFields()` is the one that still reads the global** — it is
reserved at a constant two rows and never measured in a variant shape, so it
was not forced, but it is the last of the four and should follow the others.

`blankRows(n)` and `padRows(rows, n)` remain, and the reasoning about them is
below.

Two things follow, and they are separable:

**The hazard.** The six-field save list is hand-maintained against a form of
thirty-odd fields, so a section that comes to depend on a seventh measures
correctly and corrupts silently. This needs no library change and no design:
**give the builders a parameter.**

```ts
function connectionFields(f: NewSessionForm): WidgetSpec[] { … }

const connRows = Math.max(
  connectionFields({ ...form, backend: "ssh", machineId: null, sshPick: form.sshHosts.length }).length,
  connectionFields({ ...form, backend: "kubernetes", machineId: null }).length,
);
```

No mutation, no `finally`, no save list, no ordering hazard, and
`measureVariant` is deleted rather than made safe. It is also the precondition
for anything else here: a builder that reads a global is not a pure function of
(spec, width), so it could never move inside a `layout_reader`, whose builder
*"may run more than once per frame and may not call `set_state`"*.

**The remaining objection, and the answer to it.** The count still reaches the
description as `blankRows(n)`, where the tree cannot tell a reserved row from a
real one — and it is only correct while every spec element renders as exactly
one row, which is true today and quietly depended upon. The proposal was a
container that names its alternatives and reserves the tallest, measured where
the width is known.

**It is not worth building, and the parameter change is why.** The
alternatives are functions of *form state* — an SSH host typed by hand, a
cluster with no named target, the worktree group open — so a host that
measured them would have to receive the form's state machine over the wire and
be told which shapes to try. What it would do with them is a `Math.max` over
three row counts. A plugin saying how tall its section is belongs to the same
contract as `Text { rows }`, `List { visible_rows }` and `field_width`: the
wire format is designed to carry it. What was wrong was never that the plugin
knew a row count; it was *how it obtained one* — by mutating the live form, and
by building a whole form to count it. Both of those are gone. **Closed: no
container.**

**Cost.** The parameter change is mechanical and touches four builders and
their call sites.
**Blocks.** Everything: no host-side move is possible while the builders read a
global. **Do this first.**

### Measured values go stale on resize

The resize handler refreshed `openDialog` and `openPanel` and nothing else. The
New Workspace form, the machine dialogs and the Explain popup are separate
`FloatingWidgetPanel`s, and none of them re-rendered, so a measurement taken at
mount outlived the frame it was taken in: the footer kept a flush-right it no
longer fitted, the fixed-versus-compact choice stayed frozen at the old height,
and the popup's prose stayed wrapped to a terminal that was gone.

**Closed twice over.** The resize handler now calls `renderForm()`,
`renderMachineDialog()`, `renderMachinesDialog()` and `renderExplainPopup()` —
the stopgap that keeps the arithmetic. Then the arithmetic itself went: the
footer no longer measures (`justify_end`), and the popup no longer wraps
(`Label { wrap }`). What is left for the resize handler to refresh is the
form's fit test, which is the one plugin-side measurement that survives, and
the dock's own width.

Every other item in this register is a purity argument that this one made
concrete: a measurement the plugin takes is a fact about a frame that has
passed.

### ~~A machines row pads its columns and never truncates~~ — done

`machinesRowEntry` built a five-column table by padding each cell to a fixed
width with a local `pad()` that widened and never cut, so a machine name of 14
columns shifted every column after it on that row and only that row. A visible
defect rather than a design complaint.

**Closed.** `pad` now fits the cell in both directions — *"a column is a width,
not a minimum"* — cutting with an ellipsis where it used to overflow.

**What is still owed here**, and it is smaller than it looks: the fitting is
done with `clipToWidth` against a column count the plugin names, where
`TextPropertyEntry` already carries `pad_to_chars` **and** `truncate_to_chars`
and `styledRow` exposes both. Saying the column widths and letting the host fit
them would retire the plugin's last `stringWidth` calls. A real table — a
`List` whose rows are columns the host measures — is the larger version.
**Cost.** Small. **Blocks.** Nothing.

### ~~The footer estimates a width the host has~~ — done

`formFooterRows` made the action buttons flush right when they fit and wrapped
from the left when they did not, and the plugin chose:

```ts
const need = labels.reduce((n, l) => n + editor.stringWidth(l) + 4, 0) + labels.length * 6 + 6;
const avail = Math.floor(editor.getScreenSize().width * (FORM_WIDTH_PCT / 100)) - 4;
need <= avail ? row(flexSpacer(), ...kids) : wrappingRow(...kids)
```

The estimate is also wrong. `FORM_WIDTH_PCT = 75` of 80 columns is 60, and
`frame_box` takes two, so the inner width is 58 where the plugin computes 56 —
the footer wraps a couple of columns before it needs to. `labels.length * 6` is
not chrome at all; it is a fudge for the accelerators and two `spacer(2)` gaps.

**What this needs, and what it does not.** It is tempting to reach for
`Node::priority` ("higher yields last", `fresh-ui/src/desc.rs:1510`). It is the
wrong primitive: `priority` reorders *sizing* only (`render/prim.rs:781`), so
the loser of a contest is allotted less width and **clipped**. The footer's
whole point is that no button is clipped, which is why it wraps.

But it does not need a new layout primitive either. The decision is
"right-align if it fits, else wrap", and `widgets.rs::node()` is already handed
the settled width — `panel.rs:758` builds the subtree inside a `layout_reader`.
**So the branch belongs in the description builder, host-side**, which is
precisely what `tabs.rs:25` does for the same shape of question: *"full names
when they all fit, twenty-five columns each when they do not."*

The plugin's part shrinks to one declarative property — a `Row` that says its
children are a group to be right-aligned when they fit — and the host, holding
the width, decides. Intent over the wire, geometry nowhere near it. That is
also the version that cannot go stale, because it is re-decided every time
layout runs.

**Closed, and it did need one `fresh-ui` change after all.** `Row` gained
`justify_end` on the wire and `endRow()` in the plugin library; `fresh-ui`
gained `Justify`, because settling a wrapping row's lines against its end is a
main-axis rule the box had no way to state. `formFooterRows` no longer
measures anything, and the branch is re-decided every time layout runs.

It did **not** unblock the radio item below, which cannot consume the same
mechanism.

**One thing this left unpinned**, recorded so it is not rediscovered as a
regression: the `fresh-ui` tests construct the justified row with an explicit
width, which makes `main_extent` exact. `widgets.rs` sets no width on the
footer row, so in a real panel it is right-aligned within its own content box
and is carried to the panel's edge only by the `Align::Stretch` intrinsic pass
re-measuring it at the enclosing column's settled width — which is the width of
the widest sibling row. That holds today because the form's widest row is a
`fullWidth` field. A host-side test that pins the footer's column against
mixed-width siblings is owed.

### ~~The Explain popup pre-fits its prose~~ — done

`buildExplainSpec` computed `inner = max(30, floor(width * 0.7) - 6)`, then
ran a hand-rolled `wrap()` with a hanging indent, `clipToWidth` to
cut a terminal line to the border, and `splitToWidth` to break a token wider
than the box — a quoted pattern, which is what the popup exists to show.

**The open question, answered: `Label { wrap }`, not the markdown `Text`.**
`WidgetSpec::Text { markdown: true, rows > 1 }` is, since `d1e8f95`,
`viewport(text_runs(…).wrap(Hanging))` with a caret, a selection, a
press-and-drag gesture and host-side `Up`/`Down`. Putting a read-only notice
inside one adds a Tab stop and a caret to a popup whose only focus stop is
`[ Close ]`, makes the plugin supply the `rows` count this register exists to
remove, and forces the per-row inks — the dim rules line, the dim-italic
recent lines, the state-coloured head — to round-trip through generated
markdown source to come back as colours. That is worse coupling than the `Raw`
it would replace.

Nothing new was needed in `fresh-ui`: `Wrap::Hanging` (`desc.rs:280`) *is* the
hand-rolled `wrap(text, "• ", "  ")`, and its own doc is the argument —
*"Only the thing that wraps knows where it broke… a caller that wanted this had
to wrap the text itself, which means deciding the width, which is the layout's
answer and not the caller's."* `Wrap::Word`'s over-long-token break is
`splitToWidth`. So `Label` gained one boolean, the description arm gained a
wrapping sibling of `entry_row`, and the plugin lost `inner`, the `0.7`, the
`wrap()` closure, `splitToWidth` outright (the popup was its only caller) and
its `getScreenSize()` call.

**Two things this settled that were guesses before.** A wrapping child wraps
inside the box rather than measuring at its natural width and widening it —
`panel::body()` hugs its content, so that was the risk that could have sunk the
approach; a `rows_at` test pins it. And `render_label` writes the marker gutter
and `label_width` as real leading spaces, which `Wrap::Hanging` reads as the
line's indent, so a wrapped field hint stays inside its field column for free.

**One deliberate cosmetic change.** `Wrap::Hanging` indents from the line's
*own leading whitespace*, so it cannot out-dent a bullet: a continuation now
aligns with the `•` rather than with the text after it. Matching the old look
exactly would mean changing the canonical wrap rule for every consumer — the
markdown viewer and the trust dialog included — for one popup's bullets.

**The recent output lines are deliberately not wrapped.** They are the agent's
screen lines, not prose: one can be hundreds of columns and `recentLines` goes
to 50, so reflowing would push the close button off the panel, and a progress
bar or a table reflowed is a different picture. They are cut at the border and
say so, with `elide: "tail"` — the head is the part a rule matched. That is the
second field, and it is the tree marking its own cut rather than the plugin
appending an `…` at a width it had to be told: the same move `widgets.rs:3547`
already made for a button's label, whose comment states it — *"only measurement
knows whether the label fit"*. `clipToWidth` keeps one caller, the machines
table's column padding, and `splitToWidth` is gone.

### ~~The form is built to be counted, and sometimes thrown away~~ — done

`buildFormSpec` read:

```ts
const fixed = buildFormSpecFixed();
const rows = (fixed as { children?: unknown[] }).children?.length ?? 0;
const h = editor.getScreenSize().height;
if (h <= 0 || rows + 3 <= h - FIXED_FORM_SCREEN_MARGIN) return fixed;
return buildFormSpecCompact();
```

— the form's children counted through a cast that reaches past the type, and
the count compared against the *screen* by way of two transcribed constants.

Both layouts now hand back their rows, so the choice is made on a number the
type already carries and the cast is gone. The comparison is against the panel:
it mounts at `FORM_HEIGHT_PCT` of the screen with a border row above and below,
which is its inner height exactly, and the same constant now drives the mount
and the fit test so the two cannot drift. `FIXED_FORM_SCREEN_MARGIN` and the
`+ 3` approximated that one quantity and agreed with it only near a 40-row
terminal — at the form's real height the two rules flip at the same size, so
this corrects the derivation rather than a visible defect.

The fixed-versus-compact choice itself **survives**: a modal that overflows a
24-row terminal is worse than a content-sized one, and something has to decide.
What is still open is only *where* the panel's height comes from — the plugin
computes it from its own mount options, which is exact but is still
`getScreenSize()`; the host holds the same number as `avail_height`
(`view/shell/widgets.rs:198`, set for floating slots at `app/render.rs:6465`).
Reading it rather than deriving it waits on the same item as the dock's chrome
arithmetic below.

A test now pins what the fixed layout is *for*: flipping `Launch in` swaps the
entire workspace field set for the "runs here" note and neither the agent row
nor the centred dialog moves.

### ~~`Label` cannot carry two styled runs~~ — done

The Explain popup's first row is a state glyph in the state's colour followed
by a workspace name in bold. Two styled runs on one line was the one thing no
kind expressed, so it stayed a `raw(`.

`Label` now takes `segments: StyledSegment[]` — what `styledRow` takes
(`widgets.ts:125`), not what `Raw` carries, which is a list of whole rows.
Non-empty segments replace `text`, the same contract `TextPropertyEntry`
already states for its own; `render_label` leads them with the gutter and
indent as an unstyled run and hands the row on for `normalize_widths` to
resolve, so offsets are shifted once, by the code that already does it.

`Raw` in the dock and dialogs goes 9 sites to 8 (3111, 3113, 3223, 3267, 3272,
4979, 5104, 5595), several of them the same shape. It also retired the plugin's
one place that passed a `TextPropertyEntry` where a `WidgetSpec` belongs — the
unknown-session pill's `child:` — which was a latent render bug the typecheck
had been reporting all along.

### `getHomeDir` has a caller and no test that fails without it

The working rule is *a caller in the same PR **and a test that fails without
it***. The plugin asks the editor for the home it resolved rather than reading
`$HOME`, and `DirectoryContext::for_testing` already points home at a temp
directory (`config_io.rs:1086`), honoured by the bridge (`bridge.rs:333`) — but
the three e2e tests that touch this assert only the *empty* case, which passes
either way on a CI runner with no `~/.ssh/config`.

**Fix.** Plant a config in the harness's temp home and assert the alias appears
in the Machine control. That test fails against an `$HOME` read.
**Cost.** One e2e test — the smallest item here.
**Blocks.** Nothing, but it is the only item that is a stated rule violation
rather than a design-goal gap.

### The dock's `⋯` disambiguates two presses by wall clock

**`orchestrator.ts`, `lastMenuDismissed`.** A press on `⋯` with its menu up
arrives twice — once as the layer's dismissal, once as the button's own
activation — and the second must not reopen what the first shut. The plugin
tells them apart with a timestamp and a 300 ms window, which cannot distinguish
*"this press closed the menu"* from *"a press a moment ago closed it and this is
a fresh open"*. So for 300 ms after dismissing the menu by clicking anything
else, `⋯` does nothing: the dismissal already cleared the menu, and the
guard then suppresses the open. Rapid clicking is inside that window.

**`fresh-ui` already answers this and an `Overlay` cannot reach the answer.**
`dismiss_for_pointer` exempts a press that lands on the layer's anchor, so
opening and closing from one button is one gesture — which is why the dropdown
pop-over needs no timer. `WidgetSpec::Overlay` anchors to a zero-height marker
at the row it would have occupied, so that it floats without pushing the rows
below it down, and no press can be inside a row of no height.

**And naming the trigger is not enough, which is the part worth recording.**
An attempt at this added `Overlay { anchor_key }` and a `LayerProps::trigger`
for the dismissal rule to resolve ahead of the anchor. It is the right shape and
it does not work, because `hit_node` gives a button's node **no key at all** —
`find_by_key` finds nothing and the exemption still never fires, so both the
dismissal and the activation run and the menu reopens itself. The dropdown
escapes this only because its *row* is keyed explicitly
(`dropdown_anchor_key`), not its button. The attempt was reverted after
`pressing_the_dock_menu_glyph_again_closes_the_menu` hung on it.

**Fix.** A widget's node carrying a key derived from its widget key, the way
`dropdown_anchor_key` already does for one kind — then `anchor_key` works as
designed. That is a change to every keyed control's identity in the tree, so it
wants its own change and its own run of the e2e suite.
**Cost.** Larger than it looks, which is how it got here. **Blocks.** Nothing;
the behaviour is a 300 ms dead window on a re-click, not a defect a user would
report.

### A radio decides its own spacing

**`render.rs:1678`, `render_radio`.** A row too wide for its panel drops the
gaps between options and then the label column, by building the row, measuring
it, and building it again — up to three times (`render.rs:1735`), though the
common path is one build and a `str_width` check.

The overflow is real but rarer than first claimed: the row is gutter 2 +
label column 15 + `": "` + `(•) SSH` 7 + gap 3 + `( ) Kubernetes` 14 = 43
columns, and the Add Machine dialog mounts at `widthPct: 60` (8425), which on
an 80-column terminal is 48 less 2 for the border — 46, with three to spare.
The shrink path engages at about 60 terminal columns, not 80.

This cannot be handed to a layout primitive as cheaply as it looks: the gaps
are `RADIO_GAP` characters inside one `TextPropertyEntry`, not children, so
making them yield means rebuilding the row as N nodes and re-deriving the
`option_ranges` the hit-splitting uses (`render.rs:1707`). That is a rewrite of
the kind's hit surface, not a deletion of a loop.
**Cost.** Larger than it appears. **Blocks.** Nothing; the behaviour is
correct.

### The dismissal rule walks every hit path on every press

**`crates/fresh-ui/src/hit.rs`,** in `dismiss_for_pointer`: the anchored-trigger
exemption tests rectangle containment over every node of every hit path, on
every press with a dismissible layer up, resolving each node's rect as it goes.

**A correction.** An earlier revision of this section claimed the branch *"adds
call sites"* to `find_by_key`, which `retained-mode-ui.md` names as "the live
asymptotic hole". That is false, and a reader should not carry it away:
`find_by_key` appears exactly once in `hit.rs` on the branch point and exactly
once at HEAD — the `Anchor::Node` resolution, which is unchanged context in the
diff. What the branch actually adds is the `rect_of` lookups and the
`paths × nodes` containment scan, which are a real cost and the reason this
entry stays. The scan is bounded by hit-path depth, which is small, and by the
number of live dismissible layers, which is one or two.
**Cost.** Follows the geometry index. **Blocks.** Nothing.

### Chrome arithmetic transcribed into the plugin

`maxListRowsForScreen` subtracts a literal `14` from 90% of the terminal
height, with a comment enumerating the chrome it accounts for.
`modalSessionColWidth` recomputes 90% × 34% − 4. Four of the five
`getScreenSize()` calls are these and their neighbours, and predate the
redesign; the fifth is the form's fit test, which now derives the panel's inner
height from the percentage the form itself mounts at rather than from a
transcribed margin — exact, but still a number the host already holds as
`avail_height` (`view/shell/widgets.rs`, set for floating slots in
`app/render.rs`).

**Fix.** The host telling a panel the rect it was given, so a plugin sizing
content to its own panel reads a fact. Related to the keyed geometry index.
**Cost.** Waits on that item. **Blocks.** Nothing.

---

## Not on this list

- **The dialogs' constant size.** Reserving the tallest shape is the right
  behaviour and the point of the redesign; what is open is how the tallest
  shape is found, and whether the plugin is the one to find it.
- **`Raw` as a kind.** The documented escape hatch and the migration path.
- **The formatters taking a panel width.** `render_dropdown`, `render_label`,
  `single_line` and `completion_popup` all do, by design — a formatter fits one
  row inside a width layout settled. Only the *re-render to decide* is off.
- **Widths in the spec.** `field_width`, `visible_rows`, `width_pct` and the
  rest are the contract, not debt.

---

## Sequencing

*This section was written as a plan. It is kept as a record of what the plan
turned out to be worth — the order held, and the last step did answer two of
its own questions.*

**Purity first, and it was not optional.** Three of the four section builders
took a form parameter and `measureVariant` was deleted. It removed a
silent-corruption hazard, needed no design, and was the precondition for every
host-side move: a builder that reads a module global cannot run inside a
`layout_reader`. `projectPathFields` is the one that did not need it and so
did not get it.

**Then the two defects**, both done: the form, machine and Explain panels
re-render on resize, and the machines table cuts as well as pads.

**Then move the decisions host-side**, done: the footer's right-align-or-wrap
and the Explain popup's wrapping are both decided where layout holds the width.
Each retired a `getScreenSize()` call and a hand-rolled measurement, and
neither can go stale. It cost one wire property and one `fresh-ui` enum
(`justify_end` / `Justify`) and one wire property and no library change
(`Label { wrap }`), which is close to the "no protocol change beyond intent"
this step was planned under, but not free.

**Then re-ask the remaining questions, which may have answered themselves.**
Two of the three have. The reserved-height container is **closed**: with the
builders pure, reserving is a `Math.max` over three row counts, and a host
doing it instead would need the form's state machine on the wire to get them.
`buildFormSpec`'s fixed-versus-compact choice **survives** as a choice — a
modal that overflows a short terminal is worse than a content-sized one — but
not as it was written: the cast and the transcribed chrome are gone, and what
remains open is only where the panel's height is read from. And `Label` wanted
wrapping rather than the markdown `Text`, for the reasons above.

**Left where they are:** the radio's spacing (correct, and untangling it means
rebuilding the kind's hit surface), the dismissal path's containment scan
(counted against master's geometry-index item), and the dock's chrome
arithmetic (waits on the same). `getHomeDir`'s missing test is independent of
all of it and can land any time.

**Still owed, found by review after the work above landed:**
`projectPathFields` is the last nullary builder; the machines table's column
fitting should say `truncate_to_chars` rather than call `clipToWidth`; and the
dock menu's wall-clock guard waits on a keyed button node, per the item above —
an attempt at it is in this branch's history, reverted, with what it ran into.

**Closed since:** the second `[user@]host[:port]` parser in `buildSshSpec`,
which the register listed as a duplicate of the one `parseSshTarget` had
already been fixed in. It is gone; the pending row's `ssh:<destination>` label
is where a test can see which parser ran.

**New, from the remote worktree group:** two reservations that were measured
from live state rather than from the form. `tailRowsMax` read `target` off the
form it was handed, where the current-workspace shape has no tail at all, so
the two shapes reserved different heights and flipping `Launch in` re-centred
the dialog — fixed by forcing `target: "new"` in both probes, but the shape is
worth watching: every `rowsOf` probe spread inherits whatever the live form
says about a field the builder branches on, and only the fields a probe
overrides are pinned. The focus cycle had the same class of problem in reverse:
it enumerated the local worktree group's keys itself instead of deriving them,
so the remote group arrived unreachable by Tab. Both now read the renderer's
own branch structure. A test that walks the cycle against the rendered stops,
rather than against a list, would close the class.
