# The shell's stylesheet: a class, and what each backend makes of it

**Status: stages 1–3 shipped; 4 is blocked on 6.** A class slot sits beside the
theme key in `fresh-ui`, `app::shell_style` is the terminal's rule table, and a
button reaches both backends as a naked label wearing `button` — the terminal
drawing `[` and `]` into the columns the rule reserved, the web drawing a pill
on the same item. What is *not* done is the card (§10 stage 4), which needs a
border that can drop a side (§6), and the deletion of `render_button`, which
turns out to belong to the retained-mode migration rather than to this design
(§10 stage 5).

The stage list is kept as a record of what each stage taught, not only of what
it planned — several of the sketches below were wrong in ways only writing them
revealed, and those corrections are the useful part.

**Audience:** whoever builds it. It assumes `retained-mode-ui.md` — especially
§3.9, the web consuming the display list — has been read once.

---

## 1. The problem

A plugin's `Button` reaches the screen as a **string**. The widget runtime's
`render_button` formats the label into `[ label ]` — frame, focus marker and
all — and the shell's adapter hands that finished text to a node. By the time
it is a display-list item it is one `Draw::Lines` run carrying
`"[ New Task… ▾ ]"`, an `Ink` naming the two theme keys it is painted with, and
nothing else.

So a second backend cannot draw a button. The web's dock can now *colour* that
run — an item ships the theme keys it was painted from, and a web theme re-maps
the handful it dresses — but it cannot make it a control, because **the item
says how it is painted and never what it is**. Two symptoms, one cause:

- A button styled as a pill would have its brackets *inside* the pill. The
  label is not separable from its frame, because the frame is characters in the
  same string.
- In the Winamp skin an unselected card's outline shares `ui.suggestion_fg`
  with the entry text beside it, so dimming the box dims the session names with
  it. The display list says `ui.suggestion_fg`; it never says "card".

The second is the more telling: it is not about buttons. Any decoration whose
*role* is not recoverable from its colour has the same problem.

## 2. The idea

**A class names what a thing is. Each backend owns a table from class to
appearance.** The terminal's table is Rust; the web's is CSS. The description
carries the naked label and no decoration at all.

A node carries a **list** of classes, space-separated, as CSS does — `button
primary focused` rather than one name per combination. The terminal's table
says a button is drawn `[ label ]`; the web's says it is a rounded ground with
padding. Neither backend is told what the other does, and the description says
neither — it says *button primary*, and that its content is the word
`New Task…`.

**The vocabulary is open.** A class the terminal's table has never heard of
decorates nothing there and reaches the web's stylesheet intact, which is what
lets a plugin — or a theme author — name something the shell has no opinion
about and style it in CSS alone. §12 records why that is safe here even though
draw kinds are closed.

## 3. What is already there

Nearly all of it, which is the argument for this shape over any other.

- **A stylesheet.** `ShellPalette`'s `Palette::style` calls `shell_theme`'s
  `resolve`: a name goes in, an appearance comes out, the backend owns the
  mapping, it is evaluated per item at paint time, and the library never
  interprets the name. That is a stylesheet in every sense — the `ThemeKey` is
  the selector, `resolve` is the rule table, `Style` is the declaration block.
- **A region that paints itself.** In `fresh-ui`'s paint pass, a node that
  names its own appearance emits a `Draw::Fill` over its own rectangle before
  its content — *"a region that names its own appearance is a region that
  paints: the backend decides what the name looks like"*. A classed button
  therefore already produces exactly the item a frame rule would decorate.
- **A border the fold already draws.** `Draw::Border` and its corner styles
  exist, and `fold`'s border arm renders them.
- **A library that already speaks in classes.** `fresh_ui::widgets::Button`
  holds a naked `label`, expresses its frame as padding, and names its state as
  a class — `button`, `button.focused`, `button.hover`, `button.disabled`. It
  spells those into the *theme* slot because that is the only slot it has; with
  a class slot beside it (§9) the dotted names become an ordinary class list,
  and a dotted name remains a perfectly good single class. The *shell* is what
  diverged, by writing resolved ink pairs into that slot instead of naming
  anything.
- **A web that already receives the name.** `Editor::tree_view` ships each
  item's theme keys, and the web's fold applies them as custom properties a
  theme can re-map. A class would arrive by the same road.

What is missing is that `resolve` answers with a `Style` — foreground,
background, attributes. A `Style` cannot say *bracketed*, and it cannot say
*one cell of padding*.

## 4. The box model

The unification that makes this one mechanism instead of two: **an affix is a
border, and a border is the box model.** `[ New Task… ]` is a left border of
`[`, one cell of padding, the label, one cell of padding, a right border of
`]`. Cards, dividers, focus gutters and underline rows are the same shape with
different sides.

```rust
struct Rule {
    border:  Option<Border>,
    padding: Edges<u16>,
    ink:     Option<Ink>,
}

struct Edges<T> { l: T, r: T, t: T, b: T }

enum Border {
    /// A box on four sides, drawn with a named corner set — today's `Draw::Border`.
    Box(BorderStyle),
    /// Arbitrary glyphs on the sides that have them: `[` and `]`, `▸`, `▔`.
    Sides { l: Option<Glyph>, r: Option<Glyph>, t: Option<Glyph>, b: Option<Glyph> },
}
```

Three properties, because three are what the first classes need:

| class | rule |
|---|---|
| `button` | `border: Sides { l: "[", r: "]" }`, `padding: { l: 1, r: 1 }` |
| `focused` | merged over it; adds the focus marker as a left side |
| `card` | `border: Box(Rounded)`, `padding: { l: 1, r: 1 }` |

A node's classes are applied in order and later ones win, property by property
— the whole of the cascade this design needs. A class with no rule contributes
nothing and is not an error.

**There is no `margin`, no `fill`, no shadow.** A property that is named but
unimplemented is worse than one that is absent — it invites a caller the code
cannot serve. `margin` is the sharpest case, because it changes layout: a stub
of it is a bug waiting for its first user. When a divider wants a fill
character, the stage that builds dividers adds `fill`, and not before.

**Geometry is derived, never declared.** A rule's reserved cells are the sides
its border occupies plus its padding. There is no separate `pad: 2` to keep in
agreement with a `frame: brackets` — there is one datum with two readings, so
they cannot drift.

## 5. Two phases, one datum

Padding is layout; colour is paint. `resolve` runs during the fold, after
layout has fixed every rectangle — so a class that reserves columns must be
read *before* that. It does not follow that layout must consult the stylesheet:
the shell rebuilds its description every frame, so the **builder** can resolve
the rule and apply the geometry, and `fresh-ui` stays free of any interpretation
of names.

| consumer | reads | when |
|---|---|---|
| the shell's widget adapter | the reserved cells | description build, before layout |
| the fold's `Draw::Fill` arm | `border` glyphs | paint |
| `shell_theme`'s `resolve` | `ink` | paint — this is today's path |
| the web | the class name | its own stylesheet, in CSS |

**The invariant that keeps it honest:** one function computes a rule's reserved
cells, and both the builder and the fold call it. Two implementations of that
arithmetic is how brackets end up written over a label.

### 5a. The box is a `Fill`; the contents are runs — both wear the class

The display list is flat, so a class has to be *inherited*: a run drawn inside
a classed node carries that node's class, which is the only way an item knows
which control it belongs to once the tree is gone. That leaves one question a
backend must not get wrong — if a button's box and its label both say
`button`, what stops a rule painting the control twice?

The **draw kind** does, and the rule is:

- A node that names a class paints a `Draw::Fill` over its own rectangle,
  exactly as one that names a theme does. That item **is** the control's box:
  the frame glyphs go there in the terminal, the pill background and the
  rounded outline go there on the web.
- Every other item wearing the same class is *inside* the box — the label run,
  a marker, a nested rule. Those are styled as contents (ink, weight), never as
  another box.

So a backend matches on kind and class together, never class alone:
`.k-fill[data-class~="button"]` is the control, `.k-lines[data-class~="button"]`
is what it says. The terminal gets the same split for free, because only the
`Draw::Fill` arm of the fold draws a frame.

The corollary is that naming a class **replaces** the inherited list rather
than extending it, mirroring the theme key. There is no descendant selector in
a flat list, so "a button inside a card" is written by whoever describes it, as
one list: `classes("card button")`.

## 6. Structure belongs to the description; appearance belongs to the class

The line to hold, when deciding where a thing goes:

- *"There is a box here, and it groups these children"* — structural. It
  changes layout and it is the same fact in every backend. The description.
- *"A box is drawn `╭─╮`"* — appearance. The web draws the same box as a
  rounded outline. The class.

By that test `Draw::Border(BorderStyle::Rounded)` is on the wrong side today:
the description names corner glyphs, which is a terminal's answer to a question
the web answers differently. The end state is a description that says *bordered,
class `card`* with each backend choosing the glyphs. That is a larger migration
than the first stages below and is named here as the direction, not scheduled.

## 7. The cascade, and the objection it answers

The shell moved *away* from class names deliberately, and the reason is written
down where the ink grammar is defined: a hand-written match over names like
`menu.bar.item.active.mnemonic` — six spellings for two orthogonal attributes —
was the combinatorial blow-up that arrives in earnest with the file explorer
(git status × selection × cut × focus). *"A grammar does not blow up; a list of
names does."*

That objection is real and this design must answer it rather than walk past it.
The answer is that **the blow-up was the absence of a cascade, not the presence
of names.** Every combination needed its own spelled-out name because nothing
merged. `button` + `.focused` + `.danger` as *merging* rules is N + M entries,
not N × M — which is the same insight the grammar had, applied to selectors
instead of values.

And the two axes are not alike. Colour genuinely is combinatorial, so it keeps
its grammar: a class resolves *to* an `Ink`, and the compositional part lives
in the table rather than in front of it. Structure is not: a button is a button
whether it is focused, hovered, danger or disabled. A small closed set of roles
does not multiply.

## 8. What this is not

- **Scrollbars.** `Draw::Scrollbar` has its own thumb arithmetic and marks.
  Leave it alone.
- **The text pipeline.** `Draw::Host` is where the buffer, the terminal grid
  and window embeds live, permanently.
- **Shadows and margins.** They paint outside the element's rectangle, which
  the fold clips. Out of scope, and deliberately not in the `Rule` type.
- **A user-facing stylesheet.** The table is Rust-side data to begin with. It
  is shaped so that becoming theme-file data later is a change of source, not a
  rewrite — but that is not this design.

## 9. What the library still owes

- ~~**A slot for the class.**~~ *Landed.* `Node::classes(..)` sets it,
  `layout.rs` inherits it exactly as it inherits the theme, and every `Item`
  carries a `Classes` beside its `ThemeKey` — an opaque string the library
  stores and never interprets. Naming one also makes a node paint its own
  ground, per §5a. Inert until a backend asks for it — which stages 2 and 3
  then did, in the terminal's fold and in CSS.
- **Per-side padding.** `Pad` is symmetric in `x` and `y`, so a left-only focus
  marker cannot be expressed as padding. `Edges` is needed before the box model
  is honest. Small and contained.

  *Not yet needed, and stage 2 says why:* the focus marker turned out not to be
  the button's at all. Every focusable kind in a marker-gutter panel reserves
  the same two columns — that is what keeps a row from reflowing as focus moves
  between controls of different kinds — so the gutter belongs to the row and
  sits outside the box. What is left inside is `[` and `]`, one cell each, and
  `Rule::reserved_x` asserts that symmetry rather than assuming it: the day a
  rule wants sides of different widths, that assertion is what says `Edges` is
  now due.
- **Backend-chosen border glyphs**, when §6's direction is taken. Not needed
  for the first stages: `Border::Box` can pass today's `BorderStyle` through and
  nothing changes.

## 10. Stages

Each stage is independently verifiable, and the terminal's appearance is the
invariant throughout.

1. ~~**`resolve` learns the classes, ink only.**~~ *Landed*, as
   `app::shell_style`. `button`, `button.primary`, `button.danger`,
   `button.focused`, `button.hover`, `button.disabled` resolve to exactly the
   inks `render_button`'s style ladder produces today. No geometry, no glyphs.

   Two things the sketch above left unsaid, settled by writing it:

   - **A rule's ink is *partial*.** `Ink` has two halves and both are required,
     but `button.hover` states a background and nothing else — which is what
     lets a Danger button stay red while answering the pointer, and is exactly
     what the ladder's `..base` spreads were doing by hand. So a rule carries a
     `Decl` — `Option<Paint>` per half — and `Decl::over(Ink)` is what puts it
     on a surface.
   - **Attributes replace; they do not accumulate.** `button.disabled` has to
     say *not bold* over a Primary base, and a union can only ever add. So
     `attrs: Option<Attrs>` — unstated leaves the earlier value, stated
     replaces it whole. Uniform with the two colour halves, and it is why the
     kind class need not be dropped from a disabled button's list: the cascade
     settles it, not the caller.

   The proof is an exhaustive equivalence: every (kind × bare × focused ×
   hovered × disabled) state resolves through the table to the ink the ladder
   produces — forty-eight of them once `bare` joined at stage 2. Nothing in
   production read the table when it landed, which was the point of landing it
   alone: the equivalence was checkable before anything could regress on it.
2. ~~**The box model, and the naked label.**~~ *Landed.* `Rule` gains
   `border: Sides` and `pad_x`; `shell::widgets::button_node` builds the button
   from the naked label with the frame reserved; `fold`'s `Draw::Fill` arm
   draws the glyphs. The tests asserting on `[ Label ]` in rendered cells
   passed untouched, which is the whole claim.

   Three things this taught, none of them visible from the sketch:

   - **The mirror is a second backend.** The widget tests fold the display list
     with their own miniature of `fold`, and it knew about `Draw::Border` but
     not about a class's sides — so the first run drew a button with no
     brackets at all. Both now call one `Rule::side_glyphs`, which is the same
     discipline `reserved_x` already had, applied to placement.
   - **Kind and class, never class alone** — §5a, immediately: the mirror's
     first fix matched the class alone and drew `[` through the `G` of `Go`,
     because the label run wears the class too.
   - **A box has to state its width.** A `col` inside a `col` stretches, and a
     stretched button painted its focus band across the whole panel. The width
     is `reserved_x * 2 + label`, from the same arithmetic as the padding.
3. ~~**The web reads the class.**~~ *Landed.* `TreeItemView::classes` carries
   the list, `72-tree.js` puts it on the element as `data-class`, and
   `45-tree.css` draws a pill on `.k-fill[data-class~="button"]` — the same
   item the terminal draws `[` and `]` on. This is the payoff: the web is no
   longer re-colouring a bracketed string, because there is no bracketed string
   to re-colour.
4. **`card`, and the second class.** *Attempted, and it is blocked on §6 —
   this is the finding, not a deferral.*

   A tree card is not a bordered node with rows inside it. `render_tree_card`
   bakes the box into **text**: `╭──╮` is a row, every content row is
   `│` + content + `│`, and the content is padded to the card's inner width by
   `content_row`, which also implements `align: right` and `align: between`
   over a plugin-supplied `splitByte`. Migrating means the shell reserves the
   ring and the fold draws it, which means that padding logic has one home and
   two readers — the same split `Frame` gives the button, and about the same
   size.

   That part is tractable. What blocks it is the dock's two selection looks:

   - `mark_list_card_selected` swaps the frame for a heavy one — expressible,
     as a `BorderStyle` on a `card.selected` rule;
   - `open_card_edge` **drops the right side**, so the active card opens onto
     the editor beside it. That is the whole marker, and it is made of glyphs.

   `Draw::Border` draws four sides or none. A box with one side missing needs
   the model §6 describes — a four-sided border with a corner style, where
   `Box` and `Sides` are the same type rather than two variants — and until
   that exists a `card` class can only describe the cards that are *not*
   selected, which is the one state nobody is looking at.

   A partial version was considered and rejected: putting the class on the
   block's `col` gives the web a box the width of the **panel**, while an
   indented card is two columns narrower. That is a wrong answer shipped, not
   a smaller right one.
5. **The widget runtime's own button path.** *Half landed, and the other half
   is not the stage's to do.*

   The duplication this stage exists to end is that two renderers each knew a
   button is `[ label ]`. That is gone: `widgets::frame::Frame::BUTTON` holds
   the glyphs and the padding, `render_button` composes its text from it, and
   `shell_style`'s `button` rule reserves its columns from it. Changing `[` to
   `(` now moves the text, the padding and the reserved columns together, and a
   test asserts the two readings agree on the rendered width.

   What does **not** happen is deleting `render_button`. The premise was that
   the shell's adapter is its last caller; it is not. `render_floating_spec`
   and `render_panel_spec` still run the whole text projection on every mount
   and repaint, and `plugin_dispatch` stores the `entries` it returns
   alongside the instance and focus state. That path is a *text* pipeline: a
   naked label and a class say nothing to it, because its entire output is a
   string. `render_button` goes when that pipeline goes, which is the
   retained-mode migration and not this design.

   `Frame` lives in `fresh-editor-core` for the same reason — `shell_style`
   depends on `Ink`, which depends on `Theme`, so the shell's crate cannot be
   seen from below. A frame is cells: no theme, no ink, no display list, so it
   sits at the bottom where both readers can reach it.
6. **The `Draw::Border` question** (§6). Now on the critical path rather than
   optional: stage 4 waits on it.

## 11. Definition of done

**Asserted** — each a test that fails if the property is lost:

- A classed button's cells are identical to the string-formatted one's, for
  every state in the ladder (resting, focused, hovered, disabled, primary,
  danger, bare).
- The reserved-cell arithmetic has one implementation, called by both the
  builder and the fold.
- An unknown class decorates nothing, and is not an error — while an
  unresolvable *ink* stays the loud failure it is today. Two slots, two
  disciplines: the strict one keeps its audit, the tolerant one carries names
  the shell has no opinion about.
- A class reaches the DOM as an attribute value and is never interpolated into
  markup, because an open vocabulary means the string may come from a plugin.
- The runtime's text and the shell's box agree on how wide a button is,
  because both read one `Frame`. (This replaces "`render_button` is gone",
  which stage 5 found to be a claim about the retained-mode migration rather
  than about this design — see the stage.)
- The web renders a classed button as a control, with no frame glyphs in its
  text, and an unclassed run byte-identically to today.
- Theme provenance survives: a class still reports the `fg_key`/`bg_key` pair
  the inspector records, so the class is not a hole in the audit.

## 12. Decisions taken

- **The theme key stays; the class sits beside it.** Two slots, each shaped
  like what it names: the ink grammar keeps the compositional win it was built
  for, and the class keeps a vocabulary that is a list rather than a product.
  It costs the library one field (§9), which is the price of not overloading a
  slot that already has a job.
- **The widget runtime's button path migrates** (stage 5), so the duplication
  between its string and the fold's glyphs has an end date rather than becoming
  permanent. *Amended by what stage 5 found:* the duplication is what ends, via
  a shared `Frame`; `render_button` itself outlives this design, because the
  text projection it serves is still live and a naked label says nothing to a
  pipeline whose whole output is a string.
- **The class vocabulary is open.** Draw kinds are closed for a reason that
  does not apply here: an unknown *kind* cannot be laid out at all, so it
  "breaks the second backend", while an unknown *class* matches no rule and the
  item paints with its ink exactly as it did. That is how CSS degrades, and it
  is what lets a class be invented for the web alone — a plugin naming
  something the terminal has no opinion about. An earlier draft of this
  document argued the opposite by carrying the draw-kind rule across to
  classes; the two are not the same rule.

## 13. Still open

- **May a plugin set classes, and from where?** The mechanism allows it the
  moment the vocabulary is open, but the widget spec would need a field and the
  web a policy for names it does not recognise. Nothing here depends on
  answering it.
- **Does the table ever become theme-file data?** It is Rust-side to begin
  with, and shaped so that the answer can change without a rewrite.
- **What replaces `Border` when §6 is taken?** Stage 4 needs a box that can
  drop a side, and stage 6 needs the corner glyphs to be a backend's choice
  rather than a description's. Those are the same change: one four-sided
  border, each side present or absent, with a corner style the terminal reads
  and the web ignores — at which point `Box` and `Sides` stop being two
  variants of an enum and become one shape. The button's `Sides` is already
  half of it.
