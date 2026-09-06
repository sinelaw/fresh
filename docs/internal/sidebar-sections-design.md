# The sidebar as a column of sections

> _Design note. Status: **SHIPPED** — the section model, dividers, accordion,
> persistence, `Slot::Sidebar`, `mountSidebarSection` and the Markdown
> contents section are all in the tree; the host asks it leaves open are
> recorded in `retained-mode-ui.md` §9. Written before any of it existed, it answers
> sinelaw/fresh#3045 ("Make the file explorer sidebar vertically splittable"):
> whether the request duplicates something Fresh already has, what the feature
> looks like, and how a plugin uses it — worked through on a first consumer, a
> table-of-contents section for Markdown. Revised after the retained-mode
> merge (#3135 and the ~120 commits before it) put the explorer, the grips and
> the plugin panels on the `fresh-ui` tree; the first revision's sequencing
> argument is now history, recorded in §7._

---

## 1. What is being asked for

The request: make the file explorer sidebar vertically splittable — stacked
sections with a draggable divider, so the lower section can host a plugin
panel, another tree, or anything else. The stated reason is that the sidebar is
single-occupancy today, so anything wanting to live alongside the explorer has
to replace it entirely.

Two claims are bundled there, and they need separating because only one of them
is true.

**True:** the sidebar column is single-occupancy. It is one described panel
(`view::shell::file_explorer::Explorer`) sized as a fixed-width child of the
frame's body row, it holds exactly one thing — the file tree — and it has no
notion of a second occupant.

**Not true:** that a panel wanting to live "alongside the explorer" must replace
it. Fresh has two other places a panel can live, both of which coexist with the
explorer (§2). What it does not have is a way to put a panel *inside the
sidebar column*, stacked under the tree.

The distinction matters for triage. This is not "plugin panels have nowhere to
go". It is "the sidebar is the one region of the frame that cannot be
subdivided, and it is the region users most expect to subdivide" — and, after
the merge, "the sidebar is the one region whose *content type* is fixed, when
every other region hosts a described plugin panel."

---

## 2. Overlap audit

### 2.1 Fresh already has four ways to place a panel

| Mechanism | What it is on the tree | Resize | Who uses it |
|---|---|---|---|
| **File explorer sidebar** | A described panel (`shell::file_explorer`) carved as a `Sizing::Cells` child of the body row, left or right | `Grip::ExplorerWidth` on its inner border; width persists per workspace as percent or absolute columns | The file tree, and only the file tree |
| **Utility Dock** | A *tagged singleton leaf inside the split tree* — at most one leaf carries the role | `Grip::Separator`; ratio persists with the split tree | Diagnostics, search/replace results, terminals, quickfix, code tour; plugins target it by split role |
| **Editor-global left dock** | A described column (`shell::dock::column`) holding a plugin `panel::Interior`, pinned left of *all* chrome | `Grip::DockWidth`; width survives toggling | The Orchestrator's workspace switcher; any plugin panel that re-anchors itself there |
| **Floating widget panel** | A `Layer` holding a `panel::Interior`: centred modal, or a popup anchored at a screen cell | Percentage, or content | Plugin panels by default; context menus |

So the sidebar is not the only home for auxiliary UI — it is the only home that
is *adjacent to the file tree*, and the only one whose content is not a
`panel::Interior`.

**Conclusion: #3045 is not a duplicate.** No existing mechanism subdivides the
sidebar. But it is not a greenfield feature either: the content a section
would host, the grip that would resize it, and the slot vocabulary that would
route its input all exist. That is the constraint that shapes the design
(§4.1).

### 2.2 One grip, three appliers — the divider is a fourth variant

When this note was first written the editor had three independent
divider-drag implementations, each with its own hit-test rect, drag-start
snapshot, mouse-state flag and clamping rule, all routed by a hand-ranked
ladder (`chrome::pointer_grab`). The merge collapsed the *routing*:
`view::shell::grip::draggable` wraps a node so that its press calls
`capture_pointer`, and every later move and the release arrive back at it as
`UiFact::GripDrag { which, x, y }` / `GripRelease`, wherever the pointer has
gone. `Grip` names which — `DockWidth`, `Separator`, `ExplorerWidth` — and one
`match` in the applier does the arithmetic each surface always did.

What stays app-side is the *state* (`dragging_file_explorer`,
`dragging_separator`, `dock_resizing`, and the start-width snapshots), which
is right: whether a drag is in progress is a fact about the editor.

So a section divider is **`Grip::SectionDivider(index)`** — one more variant,
one more arm, no new mechanism. The first revision's argument for waiting
("don't write a fourth ladder") has been overtaken by the ladder being gone.

### 2.3 Correlated issues

| Issue | State | Relationship |
|---|---|---|
| **#950** — sidebar with file outline for markdown TOC, typst, etc. | Open | A *consumer*. §5 is the design for it. |
| **#1791** — side panel for markdown table-of-contents and code outline, navigable and auto-syncing | Open | The same consumer, specified in more detail; §5 covers its markdown half and names what the code-outline half still needs. |
| **#1468** — move sidebar to the right side | Closed | Established the left/right side setting. Sections must mirror correctly (§6). |
| **#1213** — absolute (fixed) width for the file explorer | Closed | Established that sidebar extent is percent *or* absolute, user-chosen, and that a drag preserves the variant. Section heights follow the same rule. |
| **#2282** — terminal tabs in the Utility Dock, vertically | Open | Sibling ask, different region. Same underlying want: regions of the frame should compose. |

---

## 3. What driving it actually shows

Traced from live sessions at 84x26 against the repo's own tree — the frames
below are redrawn from those captures, not from the render code. Three
observations shaped the design.

### 3.1 Today

```text
  File   Edit   View   Selection   Go   LSP   Help                                  
┌ File Explorer ─────────×─┐ main.rs ×   +                                        □×
│▼ demo                    │▾ 1 │ fn main() -> Result<()> {                         
│  ▼ crates                │  2 │     let editor = Editor::new()?;                  
│    > fresh-core          │  3 │     editor.run()                                  
│    > fresh-ui            │  4 │ }                                                 
│  ▼ docs                  │  5 │                                                   
│      design.md        ●  │▾ 6 │ impl Editor {                                     
│    Cargo.toml            │▾ 7 │     fn run(&mut self) -> Result<()> {             
│    lib.rs                │  8 │         loop { self.tick()?; }                    
│    main.rs            ●  │  9 │     }                                             
│    README.md             │ 10 │ }                                                 
│                          │ 11 │                                                   
└──────────────────────────┘~                                                       
  Restricted  Local  Ln 1, Col 1       LF  ASCII  Rust   LSP (off)   Palette: Ctrl+P
```

The sidebar is one bordered block filling the column. Its right border column
is the width grip. Two details a section header must not collide with: the
selection marker `▌` painted in the first content column, and the plugin
decoration slot (`●`) that layout pushes to the right edge of each row with a
flex spacer.

### 3.2 The sidebar is the only region that doesn't compose

Open a terminal in the Utility Dock and the frame stacks — *except* under the
sidebar:

```text
  File   Edit   View   Selection   Go   LSP   Explorer   Help                       
┌ File Explorer ─────────×─┐ main.rs ×   +                                        □×
│▼ demo                    │▾ 1 │ fn main() -> Result<()> {                         
│  ▼ crates                │  2 │     let editor = Editor::new()?;                  
│    > fresh-core          │  3 │     editor.run()                                  
│    > fresh-ui            │  4 │ }                                                 
│  ▼ docs                  │  5 │                                                   
│      design.md        ●  │▾ 6 │ impl Editor {                                     
│    Cargo.toml            │────────────────────────────────────────────────────────
│    lib.rs                │ bash — /demo ×   +                                   □×
│    main.rs            ●  │root@vm:/demo#                                          
│    README.md             │                                                        
│                          │                                                        
└──────────────────────────┘                                                        
  Restricted  Local  Ln 1, Col 1       LF  ASCII  Rust   LSP (off)   Palette: Ctrl+P
```

The dock's separator is a plain rule that begins where the sidebar's border
ends. The editor area splits; the sidebar doesn't. That picture is the request,
stated as a diagram.

It also settles a design question. The split grid's divider carries **no
title** — identity lives in the tab strip below it, with its own controls
(`□×`) at the right. Mirroring that in the sidebar would cost two rows per
boundary (rule + tab strip) out of a column typically 24-30 columns wide, where
a tab strip cannot fit meaningful labels anyway. So the sidebar takes the other
option: **the divider row is the section header**, in the shape the explorer's
own title bar already uses.

### 3.3 Under vertical pressure, today's sidebar just empties

Shrinking the terminal to four rows leaves the explorer as a top and bottom
border with no content between them, still occupying its full width. There is
no minimum, and no collapse. Whatever this feature does under pressure is a new
decision, not a rule to inherit (§3.7).

One related papercut, observed rather than inferred: when the panel is not
focused its title carries the focus keybinding, and at a narrow width the
suffix and the close button collide — `┌ File Explorer (Ctrl+E)  main.rs` was
captured with the `×─┐` overwritten entirely. A section header adds a chevron
to that same row, so it inherits the pressure; `title_strip` should truncate
the title, not the controls, and that fix serves both.

### 3.4 Two sections

```text
  File   Edit   View   Selection   Go   LSP   Explorer   Help                       
┌ ▼ File Explorer ───────×─┐ main.rs ×   +                                        □×
│▼ demo                    │▾ 1 │ fn main() -> Result<()> {                         
│  ▼ crates                │  2 │     let editor = Editor::new()?;                  
│    > fresh-core          │  3 │     editor.run()                                  
│    > fresh-ui            │  4 │ }                                                 
│  ▼ docs                  │  5 │                                                   
│      design.md        ●  │▾ 6 │ impl Editor {                                     
├ ▼ Outline ─────────────×─┤▾ 7 │     fn run(&mut self) -> Result<()> {             
│▼ fn main                 │  8 │         loop { self.tick()?; }                    
│    let editor            │  9 │     }                                             
│  ▶ fn run                │ 10 │ }                                                 
│▶ impl Editor             │ 11 │                                                   
└──────────────────────────┘~                                                       
  Restricted  Local  Ln 1, Col 1       LF  ASCII  Rust   LSP (off)   Palette: Ctrl+P
```

**The key layout decision: adjacent sections share one border row.** Section
one's bottom border *is* section two's top border, and that shared row carries
section two's title, in the explorer's existing title shape — lead, fill,
close. Two separately bordered blocks would spend two rows of chrome per
boundary; this spends one. The shared row is the drag handle, the collapse
toggle and the section header at once.

### 3.5 After dragging the divider up

```text
┌ ▼ File Explorer ───────×─┐
│▼ demo                    │
│  ▼ crates                │
│    > fresh-core          │
├ ▼ Outline ─────────────×─┤
│▼ fn main                 │
│    let editor            │
│  ▶ fn run                │
│▶ impl Editor             │
│                          │
│                          │
│                          │
└──────────────────────────┘
```

The section above the divider takes an explicit height; the last section always
flexes to absorb the remainder. Both neighbours clamp at one content row, so a
section can't be dragged out of existence — collapsing is the reversible way to
reclaim its space.

### 3.6 Three sections, one collapsed

```text
┌ ▼ File Explorer ───────×─┐
│▼ demo                    │
│  ▼ crates                │
│    > fresh-core          │
│    > fresh-ui            │
│  ▼ docs                  │
├ ▶ Outline ─────────────×─┤
├ ▼ Git Changes ─────────×─┤
│M  main.rs                │
│A  docs/design.md         │
│?  scratch.txt            │
│                          │
└──────────────────────────┘
```

A collapsed section keeps its header row and gives up its body; the chevron
toggles it. Modelling sections as a list from the start means N > 2 needs no
re-modelling.

**Accordion semantics.** The column behaves as an accordion in the sense the
word has in every sidebar users know: collapsing a section gives its rows to
the sections still open, opening one takes them back, and the header row is
the whole control.

- **Toggle.** A press anywhere on a header row except its `×` toggles that
  section; so does Enter or Space when the header has focus. The chevron is
  the *indicator* (`▼` open, `▶` collapsed), not the only target — at
  24 columns a one-cell target is a bad one.
- **Where the rows go.** A collapsed section is exactly one row tall. The rows
  it gave up go to the open sections below it first (they are the ones that
  move), and the last open section is always the remainder. When only one
  section is open it takes the whole column (§5.1's second frame is that
  state); when every section is collapsed the column is a stack of header
  rows over empty ground.
- **Explicit heights survive a round trip.** A section with a user-dragged
  height keeps that height across collapse and re-open; only the remainder
  section flexes. So collapsing the tree to look at the contents and opening
  it again puts the divider back where the user left it.
- **Exclusive mode.** `sidebar.accordion = "free" | "exclusive"`, default
  `free`. In `exclusive`, opening a section collapses every other one — the
  one-panel-at-a-time sidebar — and the drag handles are inert because there
  is never more than one open body to divide. `free` is the default because
  the first consumer's whole point is tree *and* contents at once.
- **Squeeze reuses it** (§3.7): pressure collapses from the bottom up using
  the same state, and remembers which sections *it* collapsed so it can
  restore only those when the column grows back.

### 3.7 Squeezed

```text
┌ ▼ File Explorer ───────×─┐
│▼ demo                    │
│  ▼ crates                │
│    > fresh-core          │
├ ▶ Outline ─────────────×─┤
├ ▶ Git Changes ─────────×─┤
└──────────────────────────┘
```

When the column is shorter than the sum of the sections' minimums, the sidebar
collapses **from the bottom up** until what remains fits, and restores on the
way back out. This is decided in the `Frame` builder, not in layout: `build()`
cannot read geometry — the same reason `Frame::resolve_dock` resolves the
dock's bail-out from the last known width before the description exists — and
the migration's frame work recorded that `fresh-ui` and ratatui starve
different rows when a band is over-subscribed, so a caller who cares picks its
own order. The sidebar cares: the top section is the tree.

---

## 4. The model

### 4.1 A section hosts content that already exists

The rule that keeps this from becoming a fifth placement mechanism:

> A sidebar section is either the file tree, or a plugin panel's
> `panel::Interior` — the *same* value the dock column and the floating panel
> already mount.

After the merge that is not a metaphor but a type. `Editor::panel_interior(slot)`
resolves a mounted panel's spec, instance states, focus and hover into an
`Interior`, and `shell::dock::column` hands it to `widgets::node` with a
`widgets::Slot::Dock` so its hits route back to the right panel. A sidebar
section is the same call with a new slot:

```text
app::PanelSlot         += Sidebar(index)
view::shell::widgets::Slot += Sidebar(index)      // mirrors it, as Dock/Floating/Pane do

Frame.explorer: Option<Explorer>   becomes   Frame.sidebar: Option<Sidebar>

Sidebar
  cols:      u16                   // resolved against the chrome width, as today
  on_left:   bool
  sections:  Vec<Section>

Section
  kind:      Explorer(file_explorer::Explorer) | Panel(panel::Interior)
  title:     String                // the explorer's is built by explorer_title
  rows:      u16                   // resolved: Rows(n) | Pct(n) → rows, last = remainder
  collapsed: bool
  focused:   bool                  // exactly one chrome region wears the accent
```

`Vec` from the start even though the first cut ships two sections.

**The alternative considered and rejected:** mirror the split grid exactly —
a plain separator rule plus a per-section tab strip, so one section could hold
several panels as tabs (which is also what #2282 wants for the dock). It loses
on width, not on principle: §3.2 shows the dock's tab strip carrying a
truncated path and two controls across 56 columns, and the same strip in a
24-column sidebar has room for neither. Tabs within a section stay open as a
later addition.

### 4.2 The description

`frame_tree`'s `sidebar` closure today is one line:
`named(HostRegion::Explorer, file_explorer::explorer(e)).w(Cells(e.cols))`. It
becomes a column:

```text
col().w(Cells(s.cols)).children(
  for (i, sec) in sections:
    if i > 0:  header_row(i, sec)            // ├ ▼ Title ────×─┤  — h(1)
    body(i, sec).h(if last { Flex(1) } else { Cells(sec.rows) })
)

header_row(i, sec) = stack([
  border_strip(title, chevron, close),                        // paints
  grip::draggable(Grip::SectionDivider(i), transparent_row,  // captures the press
                  press = SectionResizeBegin { index: i, y }),
  chevron_gesture(SectionToggle { index: i }),                 // on the glyph's cells
  close_gesture(SectionClose { index: i }),                    // on the ×
])

body(i, Explorer(e)) = memo(e.clone(), build_explorer_rows)     // the panel minus its top border
body(i, Panel(p))    = layout_reader(|c| widgets::node(&p.spec, c.max_w - 2,
                          &Ctx { slot: Slot::Sidebar(i), ..p.ctx() }))
```

Three things this borrows rather than invents:

- **The explorer's chrome splits at the border.** `file_explorer::explorer` is
  `stack([panel(e), overlay(e)])`, where `overlay` is the title strip and the
  width grip. A section's *first* row is a header row and its body is
  `panel(e)` without a top border, so the explorer's description gains a
  `first`/`last` pair of flags and loses nothing else. The width grip stays on
  the column, not on a section: it spans every section's right edge, as today.
- **The interior width rule is the dock's.** `dock::column` lays the interior
  at `max_w - DIVIDER_COLS` and passes that same number as the wrap width,
  because the two being one number is what put the title bar's `×` back
  against the divider. A sidebar section has a left and a right border rather
  than a divider, so its inner width is `cols - 2`, laid and wrapped alike.
- **Memoisation is per section.** `explorer()` is already
  `memo(e.clone(), build_explorer)`, which is the migration's 0.1 rule
  applied to the largest surface; each section's body memoises on its own
  value so an edit that changes a TOC row does not rebuild the file tree.

### 4.3 Sizing and the drag

Section extent mirrors the width model #1213 established: sized in **rows or
percent, whichever the user chose**, resolved to rows before the description is
built, and a drag preserves the variant. The last section is always the
remainder, so the column is exactly filled and there is no rounding drift.

The drag is `GripDrag { which: SectionDivider(i), y }`: the applier snapshots
the section's rows at `SectionResizeBegin` and recomputes them from the
absolute pointer row on each move, clamping both neighbours at one content row.
Recomputing from the absolute row rather than accumulating deltas is what the
explorer-width applier already does (`handle_file_explorer_border_drag` works
from `drag_start_position`), and it is why a long drag cannot drift away from
the cursor. `GripRelease` clears the snapshot and persists.

### 4.4 Focus and keys

The merge left the keyboard half-migrated (`KeyContext` is A.5 in the migration
doc, still open), and the design has to work on both sides of that line.

*Today's shape, which already has the answer.* The explorer takes keys through
`KeyContext::FileExplorer`. The dock takes keys through
`panel::keys_layer(Slot::Dock)` — a `Modality::Focus` layer declared in the
frame under everything that outranks a focused dock — so its widgets are
offered the key by containment and hand back what they decline. Those two
coexist now, in one frame, with focus moving between them by action
(`ToggleDockFocus`). A plugin section is the dock's case with a different slot:
`panel::keys_layer(Slot::Sidebar(i))`, raised only for the focused section. The
explorer section keeps its context. One new action, *focus next sidebar
section*, cycles the explorer and the panel sections in order; the existing
`FocusFileExplorer` keeps meaning what it means.

*After A.5.* `KeyContext` dissolves into focus scopes and the sidebar column is
one scope with the sections as children in tree order. The design does not
change; the special case for the explorer section goes.

### 4.5 Persistence

Section layout is *session state*, so it is app state: the migration's rule is
that anything the workspace file serializes must stay on the editor, because
elements are disposed on unmount and do not survive a restart. `FileExplorerState`
gains `sections: Vec<SectionState>` — kind (the explorer, or a panel's
composite `(plugin, id)` identity), extent, collapsed — defaulted so an existing
workspace with no such key restores as exactly one Explorer section filling the
column, byte-identical to today. A section whose plugin is not loaded restores
as a header row with a "panel unavailable" body rather than vanishing.

### 4.6 Defaults

**The default configuration is one section.** Out of the box the sidebar looks
and behaves exactly as it does now — §3.1, not §3.4. The chevron and the shared
border row appear only once a second section exists. A feature that changes
the default sidebar for every user who never asked for a second panel has
mis-scoped itself.

### 4.7 Plugin API

Additive, and shaped like the dock's. The orchestrator mounts its column with
`mountFloatingWidget(spec, { asDock: true })` and then sizes it with
`floatingPanelControl(id, "dock_width", cols)`. A sidebar section is the same
pair with a different placement:

```text
mountFloatingWidget(id, spec, { asSidebar: true, title: "Contents" })
floatingPanelControl(id, "sidebar_rows", 8)        // requested rows; the user's drag overrides
floatingPanelControl(id, "sidebar" | "dock" | "center")   // re-anchor, as today
```

Host-side that is `PanelPlacement::SidebarSection { rows }` beside `LeftDock`
and `Centered`, `PanelSlot::Sidebar(i)` beside `Dock` and `Floating`, and
nothing else: the spec, the reconcile, `widgetCommand`, the mutations and the
`widget_event` hits are unchanged, because every one of them is keyed by the
panel's `(plugin, id)` and not by where it is drawn.

### 4.8 The web

The section model is app state and the web consumes the display list, so the
explorer section reaches the web the way the explorer does today. Plugin
panels on the web were deliberately deleted with the retained-mode work and
return by consuming the display list (`retained-mode-ui.md` §3.9); a
plugin section is TUI-only until that lands, which is the same gap the dock has.

---

## 5. The first consumer: a table-of-contents section for Markdown

The request behind #950 and #1791, designed as a plugin so the sidebar API is
proved on a real occupant before a second one exists. It works in **both**
Markdown modes — source, and compose/preview — for a reason worth stating
first: compose is a *view mode* on the same buffer (`ViewMode::PageView`,
set through `setViewMode(id, "compose")`), which conceals markers and
re-lays lines but does not move a byte. Every position a TOC needs is a
source byte offset in both modes.

### 5.1 What it looks like

Source mode, the cursor on line 12 inside *What users asked for*:

```text
  File   Edit   View   Selection   Go   LSP   Explorer   Help                       
┌ ▼ File Explorer ───────×─┐ design.md ×   +                                      □×
│▼ demo                    │  6 │ ## Motivation                                     
│  ▼ crates                │  7 │                                                   
│    > fresh-core          │  8 │ The sidebar is the one region of the frame        
│    > fresh-ui            │    │ that cannot be subdivided.                        
├ ▼ Contents ────────────×─┤  9 │                                                   
│▼ Sidebar sections        │ 10 │ ### What users asked for                          
│ ▼ Motivation             │ 11 │                                                   
│▌   What users asked for  │ 12 │ Stacked sections with a draggable divider.        
│    What exists today     │ 13 │                                                   
│ ▶ The model              │ 14 │ ### What exists today                             
│ ▶ Sequencing             │ 15 │                                                   
└──────────────────────────┘ 16 │ Four placements, three grips.                     
  Restricted  Local  Ln 12, Col 1      LF  ASCII  Markdown   LSP (off)              
```

Compose mode, with the file tree collapsed to give the contents the column:

```text
  File   Edit   View   Selection   Go   LSP   Explorer   Help                       
┌ ▶ File Explorer ───────×─┐ design.md ×   +                                      □×
├ ▼ Contents ────────────×─┤                                                        
│▼ Sidebar sections        │    Motivation                                          
│ ▼ Motivation             │                                                        
│▌   What users asked for  │    The sidebar is the one region of the frame that     
│    What exists today     │    cannot be subdivided.                               
│ ▶ The model              │                                                        
│ ▶ Sequencing             │    What users asked for                                
│                          │                                                        
│                          │    Stacked sections with a draggable divider.          
│                          │                                                        
│                          │    What exists today                                   
└──────────────────────────┘                                                        
  Restricted  Local  Ln 12, Col 1      LF  ASCII  Markdown   LSP (off)              
```

The selected row is the heading whose span contains the cursor, and it wears
the explorer's `▌` so the two trees read as one family. Rows are indented one
column per level (`indent_cols: 1` — the spec's own option for narrow panels),
with the disclosure glyph the `Tree` widget already draws. The `#` markers are
not shown in either mode: a contents list is not the source.

### 5.2 Why a plugin, and which one

The markdown knowledge already lives in `markdown_compose.ts`: `scanHeadings`
walks the document once, skipping fenced code, and returns each heading's byte
offset and level — it feeds the scrollbar's heading markers today, bounded by
`large_file_threshold_bytes` so a huge file degrades rather than stalls. The
TOC is a second reader of the same scan, and the plugin already knows when
compose turns on and off. A built-in `Outline` section kind was considered and
rejected: it would put Markdown parsing in the host, and the code-outline half
of #1791 will arrive from LSP `documentSymbol` — a different source feeding the
*same* section kind, which is the point of the section being a plugin panel.

### 5.3 The spec

A `Tree` — flat, depth-first, plugin-owned expansion, which is exactly the
shape `WidgetSpec::Tree` is (the migration doc notes it maps onto
`widgets::List`, not `widgets::Tree`, *because* the plugin owns `expanded_keys`):

```text
{ kind: "tree",
  nodes:     headings.map(h => { text: h.title, depth: h.level - 1, hasChildren: h.hasChildren }),
  item_keys: headings.map(h => String(h.byte)),      // stable across edits below the heading
  selected_index: -1,                                // driven by sync, never by the spec
  expanded_keys:  <all>,                             // initial only; SetExpandedKeys thereafter
  indent_cols: 1 }
```

Keys are byte offsets rather than titles because two headings can share a
title and none can share an offset.

### 5.4 Keeping it current

`lines_changed` reports the viewport, not the document — the compose plugin
learned this the hard way and answered it with `prescanHeadingMarkers`. The
TOC does the same: a full `getBufferText` scan on `after_file_open` and on
compose toggle, then a **debounced rescan on `after_insert` / `after_delete`**
(edits arrive per keystroke; the rescan waits for a pause). Over the size cap
the panel shows the headings it last saw and says so in its title, the way the
scrollbar markers already degrade.

### 5.5 Sync

Two hooks, two modes, one rule for which wins:

- **`cursor_moved`** carries `new_position` (a byte) and `line`. The plugin
  finds the last heading at or before it — a binary search over the sorted
  offsets — and pushes `WidgetMutation::SetSelectedIndex`. That is the fast
  path: no re-emit, no relayout of anything but the band.
- **`viewport_changed`** carries `top_byte`. When the buffer's pane does not
  have focus — the user is in the sidebar, or reading in another split — the
  cursor is not what they are looking at, and the selected row follows the
  viewport top instead.
- A `toc.follow` setting (`cursor` | `scroll`) lets a reader pin the second
  mode. Default `cursor`.

Selection *also* has to stay visible in the panel: `Tree` windows its rows,
and the host auto-scrolls a `List` to its selection on keyboard moves but not
on a mutation. Revealing the selected row after `SetSelectedIndex` is the one
small host change this consumer asks for (§5.8).

### 5.6 Navigation

- `widget_event { select }` (a click, or Enter on the focused tree):
  `scrollToLineCenter(split, buffer, line)` then `openFile(path, line, 0)` —
  the existing "jump to a location" pair — so the cursor lands on the heading
  and the pane centres it. Focus stays in the sidebar on a click, moves to the
  pane on Enter: the same split the explorer makes between preview and open.
- `widget_event { expand }`: the plugin toggles the key in its own
  `expanded_keys` and pushes `SetExpandedKeys`. **Optionally** it also folds
  the section in the buffer: `setFoldingRanges` publishes toggleable ranges in
  the shape LSP folding does, so a TOC fold and a buffer fold can be one
  gesture. Off by default (`toc.fold_buffer: false`) — collapsing an outline to
  see its shape should not hide text the user is reading.
- Keyboard inside the section is the `Tree`'s: Up/Down move the selection
  (which, in `cursor` mode, does *not* move the buffer cursor until Enter —
  browsing the outline is not editing), Left/Right fold, Enter jumps.

### 5.7 Both modes, one plugin

| | Source | Compose |
|---|---|---|
| Heading positions | byte offsets from the scan | the same bytes — conceals hide `#`, they do not shift text |
| Row text | title without markers | the same |
| Sync source | cursor / viewport, per §5.5 | the same hooks fire; compose's soft-wrap changes screen rows, not bytes |
| Jump | `scrollToLineCenter` + `openFile(line)` | the same — line numbers are source lines even when the gutter is hidden |
| When the mode flips | — | the plugin already handles the toggle; the TOC re-scans once (the document did not change, but a stale panel across a mode switch is the kind of thing that erodes trust) |

### 5.8 What it needs from the host

Nothing beyond §4 — that is the test the consumer exists to run. Two things it
would *like*, both small and both general:

1. **Reveal on `SetSelectedIndex`.** A selection the plugin sets should scroll
   into the tree's window the way a keyboard move does.
2. **Sticky ancestors in `Tree`.** The explorer pins a scrolled node's
   ancestors at the top (`viewport_display_indices`, capped like VS Code's
   sticky scroll). A long document's contents wants the same — the current H1
   and H2 pinned while the H3s scroll — and it is the explorer's own logic
   moved one level down into the widget.

And one thing it cannot have yet, named so it is not rediscovered: the
code-outline half of #1791. `LspFeature::DocumentSymbols` is plumbed host-side
and exposed to no plugin. When it is, an outline section is this section with
a different scan.

---

## 6. What this touches

| Area | Change |
|---|---|
| `shell::frame` | `Frame.explorer` → `Frame.sidebar`; the `sidebar` closure becomes a column of section bodies and header rows |
| `shell::file_explorer` | `Explorer` gains first/last flags so its border and title strip can be a section's; the width grip moves to the column |
| `shell::grip` / `msg` | `Grip::SectionDivider(usize)`; `UiFact::{SectionResizeBegin, SectionToggle, SectionClose, SidebarFocus}` |
| `shell::widgets` | `Slot::Sidebar(usize)`; `panel::keys_layer` raised per focused section |
| `app::PanelSlot` / `PanelPlacement` | `Sidebar(usize)` / `SidebarSection { rows }`; `panel_interior` unchanged |
| Applier (`shell_host`) | Arms for the four facts and the grip; bottom-up collapse in the `Frame` builder next to `resolve_dock` |
| Focus | One action, *focus next sidebar section*; the explorer keeps `KeyContext::FileExplorer` until A.5 |
| Persistence | `FileExplorerState.sections`, defaulted |
| Config | Optional default section list; `toc.follow`, `toc.fold_buffer` on the plugin side |
| Plugin API | `asSidebar` at mount; `sidebar` / `sidebar_rows` ops |
| Plugin | The TOC in `markdown_compose.ts`, or a `markdown_toc.ts` sharing its scanner |
| Tests | Frame-rect coverage for N sections and the squeeze order; restore coverage for the empty-`sections` default; a drag test that the divider tracks the pointer; a TOC test that the selected row follows the cursor in both modes |

**Right-side sidebars** need one detail right: the *width* grip is the
column's inner edge — right border on a left sidebar, left border on a right
one — while the section dividers are interior rows and identical on both sides.

---

## 7. Sequencing

The first revision of this note argued for waiting until the migration moved
the explorer onto the tree, because everything the feature needed was either
about to be provided (pointer capture, flex sizing, described panels) or about
to be deleted (the chrome registry's boxes, the pointer-grab ladder). That has
happened. **There is no prerequisite left**; what remains open in the
migration changes internals, not the design:

| Migration item | Effect on this design |
|---|---|
| 0.1 memoised subtrees | Per-section `memo`, as the explorer already does |
| 0.3 components own state | A section's `collapsed` is persisted, so it stays app state by the migration's own rule; nothing here moves |
| A.5 `KeyContext` → scopes | Dissolves the explorer section's special case (§4.4); the sidebar becomes one scope |
| C.2 `WidgetInstanceState` → element state | Invisible to the TOC: its expansion is plugin-owned by the spec's contract |
| Web plugin panels return | Plugin sections appear on the web with the dock |

Four changes, each shippable alone:

1. **The section model, with one section.** `Frame.sidebar`, the column, the
   explorer as section 0. Byte-identical output; the parity and e2e suites are
   the proof.
2. **Dividers, collapse, persistence.** The header row, `Grip::SectionDivider`,
   bottom-up squeeze, `FileExplorerState.sections`.
3. **`Slot::Sidebar` and the plugin ops.** A plugin panel as a section; the
   dock's keys layer per section; *focus next section*.
4. **The Markdown contents section.** The consumer, and the two host asks in
   §5.8 if they are wanted.

Change 1 alone is worth landing early: it is the refactor the other three sit
on, and it is the one that has to be proved cell-identical.

---

## 8. Open questions

1. **Squeeze order** (§3.7) — bottom-up collapse is proposed. Proportional
   shrink keeps every section visible but makes all of them useless at once.
2. **Does the explorer stay pinned first?** Proposed yes for the first cut;
   reordering is a separate feature with its own drag affordances.
3. **Per-window or per-workspace?** The explorer's width and visibility are
   per-window today, persisted per workspace. Sections should follow; a
   window switch that changes the section list is a visible jump worth
   confirming against the restore suites.
4. **Where the TOC lives** — inside `markdown_compose.ts`, which owns the
   scanner and the mode toggle, or a sibling plugin importing it. Inside is
   simpler; a sibling is the shape the LSP outline will take.
5. **Should a TOC fold drive a buffer fold by default?** Proposed no (§5.6).
