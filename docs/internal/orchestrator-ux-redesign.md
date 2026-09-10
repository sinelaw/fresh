# Orchestrator dock & dialogs — UX redesign

> _Design note. Status: **PARTLY IMPLEMENTED** — §2 (dock) and §3 (dialogs)
> ship; §4 (SSH host picker) and §5 (machines) do not. See
> "Implementation status" at the end. The "Today" blocks are transcripts
> captured by driving Fresh 0.5.1 by hand in tmux; they are the evidence
> for every decision that follows, and describe the surfaces as they were
> before §2 and §3 landed._

Purpose: record a visual redesign of the Orchestrator's two surfaces — the
**dock panel** and the **New Workspace / Run Agent dialogs** — plus two new
surfaces the redesign implies: an **SSH host picker** driven by
`~/.ssh/config`, and a **machine registry** (`Add Machine`, `Machines`) for
launching work on pre-configured hosts.

This note is about layout, alignment and control placement only. The
semantic work it assumes — a five-state `agentState` with `blocked`, state
rollup, and an attention queue — is a separate concern and is not designed
here; where a wireframe shows a state glyph it stands in for that work
rather than specifying it.

Every ASCII block below is generated and width-checked by
`scripts/gen-orchestrator-wireframes.py`, so the box drawing is exact
rather than hand-counted. Edit the generator, not this file.

---

## 1. Conventions

| Mark | Meaning |
|------|---------|
| `▸` in the left margin | keyboard focus (the widget library's marker gutter) |
| `[ Label ]` | a **button** — `Frame::BUTTON`, unchanged from today |
| `[ ]` / `[v]` | a **toggle**, with the glyphs `render_toggle` already uses |
| `( )` / `(•)` | a **radio** — a widget kind that does not exist yet (§3.2) |
| `[ value… ]` | a text input; in the themed UI it also carries a field background |
| `[ value… ▾]` | a dropdown |
| `█` / `░` | scrollbar thumb / track |
| `●` `◐` `✓` `·` `?` | blocked / working / done / idle / unknown |
| `↳` | a hint belonging to the field above it |

Two notes on fidelity, because both were mistakes made while drawing this:

**Colour does more work than a monochrome capture shows.** `capture-pane -p`
strips it, and an early draft of this note argued from those transcripts
that brackets were overloaded. They are not: `[ … ]` has one meaning —
*interactive widget* — carried by the shared `Frame::BUTTON` constant, and
the kinds are separated by content plus weight, hue and background fill.
Buttons are bold and accent-coloured, destructive ones red, inputs carry a
lighter field background. The wireframes below keep every bracket the
widget library already draws.

**The scrollbar track is a background colour, not a glyph.** The widget
renderer emits `█` for the thumb and nothing for the track. `░` is drawn
here only so the affordance is visible in a plain-text figure.

---

## 2. The dock

### 2.1 Today

```
┌─ Orchestrator ───────────────────────┐
│ [ New Task… ▾ ] [Search Tasks      ] │
│ [ ▸ Filters ] [ view: compact ]      │
├──────────────────────────────────────┤
│  · payments-api                      │
│  · fix-webhook-retry · claude        │
│  · add-idempotency · codex           │
│  · schema-cleanup                    │
│  · upgrade-vite · claude             │
│  · dark-mode · claude                │
└──────────────────────────────────────┘
```

Four rows of chrome — title, two button rows, rule — above a list that
typically holds three to eight rows. Opening **Filters** adds four more.
The four header controls are drawn at equal weight but have very different
frequencies: New Task is used a few times a day, Search is dead weight
until there are twenty workspaces, Filters is weekly, and `view: compact`
is set once and never touched.

Two faults in the row itself: `· fresh-1 · /root/fakebin/cla…` leads with a
state dot, then a **separator** dot, then a truncated command — the least
useful field, occupying the width where agent kind and age belong. The same
`·` glyph is doing semantic duty and punctuation duty in one row.

And the list gives no sign that it scrolls.

### 2.2 Chosen layout

Actions on one row at the top, then the attention line, then the list. The
four header controls collapse to two.

```
┌──────────────────────────────────────┐
│ + New                  / search    ⋯ │
├──────────────────────────────────────┤
│ ● 2 need you · ✓ 1 done            ▾ │
├──────────────────────────────────────┤
│ ▾ payments-api            ●2 ✓1     █│
│   ● fix-webhook-retry  claude    4m █│
│   ✓ add-idempotency    codex    10m █│
│   · schema-cleanup     —            █│
│ ▾ web                     ●1 ◐1     █│
│   ● upgrade-vite       claude   22m ░│
│   ◐ dark-mode          claude    1m ░│
│ ▸ infra                   ?1        ░│
│                                      │
└──────────────────────────────────────┘
```

`+ New` loses its `▾`. Today that caret opens a two-item menu, so creating
a workspace costs *click → menu → choose* — two clicks on the common path
to serve the rare one. Moving folder creation into `⋯` lets `+ New` go
straight to the dialog; New Folder stays at two clicks, which is correct
for how often it is used.

Width: `[ New Task… ▾ ]` is 15 columns and `+ New` is 5; `[ ▸ Filters ]` is
13 and `⋯` is 1. Both header rows today total 70 columns of controls
against 36 for the single row above.

The scrollbar rides the workspace list only — the rows above and below it
do not scroll, and not drawing a track on them is what says so.

### 2.3 The attention line disappears when it is not needed

```
┌──────────────────────────────────────┐
│ + New                  / search    ⋯ │
├──────────────────────────────────────┤
│ ▾ payments-api                      █│
│   · fix-webhook-retry  claude   12m █│
│   · add-idempotency    codex    41m █│
│   · schema-cleanup     —            █│
│ ▾ web                               █│
│   · upgrade-vite       claude    8m █│
│   · dark-mode          claude    3m █│
│ ▸ infra                             █│
│                                     ░│
│                                     ░│
└──────────────────────────────────────┘
```

With nothing blocked there is no chrome at all above the list: the row that
would carry the summary is simply not drawn. The thumb is longer here
because fewer rows are hidden.

### 2.4 The `⋯` menu

Three of the four header controls are *settings*, not actions, and settings
belong in a menu. Folder creation joins them, and the dock's title row and
its `×` are absorbed too — closing the dock is rare and `Alt+O` undoes it.

The menu is short and fixed, so it has no scrollbar. That contrast is
deliberate: a track means the region scrolls.

```
┌──────────────────────────────────────────┐
│ New folder…                              │
│ Manage workspaces…                       │
├──────────────────────────────────────────┤
│ view      compact · comfortable · detail │
│ show      [ ] empty   [ ] all worktrees  │
│ scope     this project ▾                 │
├──────────────────────────────────────────┤
│ Hide dock                        Alt+O   │
└──────────────────────────────────────────┘
```

### 2.5 Search on demand

`/` turns the action row into a filter with a match count and `Esc` to
leave — the behaviour the command palette already has. A permanent search
box above a three-row list is pure cost; at twenty workspaces it is
essential, so it should appear exactly when it earns its row.

```
┌────────────────────────────────────────────┐
│ ▾ payments-api                            █│
│   ● fix-webhook-retry   claude       4m   █│
│ ▾ web                                     █│
│   ● upgrade-vite        claude      22m   ░│
│   · (3 more matches)                      ░│
├────────────────────────────────────────────┤
│ / webh▏                        5 of 9   Esc│
└────────────────────────────────────────────┘
```

### 2.6 Narrow

At 26 columns the same structure holds; only the columns inside each row
are dropped. The scrollbar is the one piece of chrome that does not shrink.

```
┌──────────────────────────┐
│ ● 2 need you           ▾ │
├──────────────────────────┤
│ ▾ payments-api  ●2 ✓1   █│
│   ● fix-webhook…    4m  █│
│   ✓ add-idempot…   10m  █│
│   · schema-clea…        █│
│ ▾ web           ●1 ◐1   █│
│   ● upgrade-vite   22m  ░│
│   ◐ dark-mode       1m  ░│
│ ▸ infra         ?1      ░│
├──────────────────────────┤
│ + New       /         ⋯  │
└──────────────────────────┘
```

### 2.7 Alternates considered

A **footer bar** — the same two controls at the bottom of the panel rather
than the top. It keeps the top-left corner, where eyes land, on content,
and is the arrangement several terminal panels settle on. Rejected because
actions at the top are the more conventional expectation in this editor; it
remains the better choice if the dock is ever judged to feel action-heavy.

A **scope-selector header** (`payments-api ▾` on its own row, `⋯` at the
right) surfaces the project filter now buried in the Filters drawer.
Rejected because it spends a permanent row on a control many users never
change; it belongs in `⋯` until multi-project work is common.

---

## 3. The dialogs

### 3.1 Today

```
┌ ORCHESTRATOR :: New Workspace ────────────────────────────────────────[×]┐
│  Launch in: [New workspace     ▼]                                        │
│                                                                          │
│Run in:   [ Local ]   [ SSH ]   [ Kubernetes ]   [ Devcontainer ]  ←/→ …  │
│                                                                          │
│╭─ Project Path ─────────────────────────────────────────────────────────╮│
││ ▸ [default (leave blank to use): /home/user/fresh                    ] ││
│╰────────────────────────────────────────────────────────────────────────╯│
│╭─ Workspace Name ───────────────────────────────────────────────────────╮│
││   [fresh-2                                                           ] ││
│╰────────────────────────────────────────────────────────────────────────╯│
│  Agent: [custom…  ▼]                                                     │
│  [ ▶ Advanced… ]                                                         │
│                                                                          │
│  [ Create Workspace ]    [ Create in Background ]    [ Cancel ]          │
│                                                                          │
│Tab next / accept  S-Tab prev  ←→ change option  ↑↓ suggest … ^Enter creat│
└──────────────────────────────────────────────────────────────────────────┘
```

Six faults, all positional rather than functional:

1. **Three different left margins.** `  Launch in:` at indent 2, `Run in:`
   at indent 0, `╭─ Project Path ─╮` at the box edge, `  Agent:` back at 2.
   Nothing forms a vertical edge.
2. **Two label systems.** Some labels are inline (`Agent: [custom… ▼]`),
   others are box titles (`╭─ Workspace Name ─╮`) — same kind of field,
   same dialog.
3. **A nested box per text input**, so every single-line field costs three
   rows. SSH mode stacks six: eighteen rows for six inputs.
4. **~120 columns wide** for content needing about fifty.
5. **The hint bar is clipped mid-word** — eight hints in one row, the last
   cut to `^Enter creat`. A bug, not density.
6. **Focus is marked in two places** — at the dialog margin for inline rows
   (`▸ Agent:`), inside the box for boxed fields (`│ ▸ [`).

A seventh, in content rather than layout: instructions live inside value
slots. `[default (leave blank to use): /home/user/fresh]` and
`[build-01 · deploy@build-01:22 · or paste ssh://host/path]` both read as
if that string is the value.

### 3.2 One real widget collision

Not brackets — see §1 — but this: reading the escape sequences, the
**selected backend `[ Local ]` renders bold + cyan, which is exactly the
treatment the primary action `[ Create Workspace ]` carries**. Destructive
`[ Cancel ]` is bold + red. So "the option you are on" and "the button that
submits" are drawn identically, and the row of four backends reads as four
buttons of which one is armed.

They are not buttons. `Run in` is a mutually exclusive selection — the
`←/→ switch type` hint says so — and it should use a widget kind that says
that. **This asks for a `Radio` widget in the library**, not a local
workaround in this dialog: the point of `Frame::BUTTON` being one shared
constant is that widget kinds stay consistent across the whole TUI, and a
one-off radio drawn only here would break exactly the property that makes
the current buttons trustworthy. Adding the kind frees bold + accent to
mean "this submits" everywhere.

### 3.3 The grid

One rule does most of the work: a **right-aligned label gutter** and a
single field column, so every `[` in the dialog — inputs, dropdowns and
toggles alike — lands on the same column. The boxes around fields go away;
the alignment does the grouping they were attempting.

Disclosure is driven by values rather than a separate toggle, so
**`Advanced…` disappears**: the agent command appears when the agent is
`custom…`, and the worktree fields appear when the worktree toggle is on.

Buttons stay buttons. The footer keeps `[ Label ]` and gains each button's
accelerator beside it, which is what lets the eight-hint bar go.

### 3.4 New Workspace — local

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│           Run in   (•) Local      ( ) SSH      ( ) Kubernetes              │
│                                                                            │
│▸         Project   [ ~/code/payments-api                         ]         │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ claude               ▾]                               │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
│                    [v] Create a git worktree                               │
│      from branch   [ main                 ▾]                               │
│       new branch   [ fix-webhook-retry                           ]         │
│                                                                            │
│                    [ ] Auto mode — fewer approval prompts                  │
│                    [v] Teach the agent Fresh's CLI                         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

Sixteen rows against today's twenty-eight with Advanced open, showing the
same fields. Against today's *collapsed* eighteen it is two rows taller,
because the worktree fields that today need Advanced are visible here.

**Devcontainer is gone from `Run in`.** It is a property of a project, not
a host — every other option in that row names *where a machine is* — so it
does not belong in the same mutually exclusive set. It reappears in the
machine picker (§5.1) as its own group, which is the one place a
per-project backend sits honestly beside per-host ones.

### 3.5 The agent command, revealed by the dropdown

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│           Run in   (•) Local      ( ) SSH      ( ) Kubernetes              │
│                                                                            │
│          Project   [ ~/code/payments-api                         ]         │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ custom…              ▾]                               │
│▸         Command   [ /root/fakebin/claude-blocking               ]         │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
│                    [ ] Create a git worktree                               │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

### 3.6 Run Agent

The same grid with fewer rows. The two dialogs share one form, one
alignment and one footer; today they share a form but not a visual system.

```
┌─ Run Agent ─────────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ Current workspace    ▾]                               │
│▸           Agent   [ claude               ▾]                               │
│     Start prompt   [ Review the diff on this branch              ]         │
│                                                                            │
│                    [ ] Auto mode — fewer approval prompts                  │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Run ] ⏎                                     [ Cancel ] Esc              │
└────────────────────────────────────────────────────────────────────────────┘
```

### 3.7 Hints belong under the field, never inside it

```
┌─ Placeholders vs hints ────────────────────────────────────────────────────┐
│                                                                            │
│▸            Host   [ build-01                                    ]         │
│                      ↳ deploy@build-01:22 · ssh://host/path                │
│                                                                            │
│      Remote path   [                                             ]         │
│                      ↳ blank = remote home                                 │
│                                                                            │
└────────────────────────────────────────────────────────────────────────────┘
```

---

## 4. SSH

### 4.1 A host picker driven by `~/.ssh/config`

The host field opens the hosts already configured on the machine. Choosing
one is a click; typing filters. The list scrolls — nine hosts here, five
visible — and `Other host…` sits below the scrolling region, so it stays
reachable however far the list is scrolled.

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│           Run in   ( ) Local      (•) SSH      ( ) Kubernetes              │
│                                                                            │
│▸            Host   [                                             ]         │
│                    ┌────────────────────────────────────────────────┐      │
│                    │ ▸ build-01     deploy@build-01.ci.internal    █│      │
│                    │   gpu-box      noam@10.4.2.19                 █│      │
│                    │   staging      deploy@staging.example.com     █│      │
│                    │   prod         deploy@prod.example.com        ░│      │
│                    │   testbox      root@127.0.0.1:2222            ░│      │
│                    ├────────────────────────────────────────────────┤      │
│                    │   Other host…                                  │      │
│                    └────────────────────────────────────────────────┘      │
│      Remote path   [ /srv/payments-api                           ]         │
│                                                                            │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ claude               ▾]                               │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

### 4.2 After picking

The list collapses and the resolved target is shown inline, dim, right of
the field. User, port and identity come from the config entry, so those
fields never appear — **the config is the source of truth and the dialog
only points at it**. Only `Other host…` reveals them.

The blank line between `Remote path` and `Name` is the whole grouping
device: connection above, workspace below.

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│           Run in   ( ) Local      (•) SSH      ( ) Kubernetes              │
│                                                                            │
│             Host   [ build-01             ▾]  deploy@build-01.ci.internal  │
│▸     Remote path   [ /srv/payments-api                           ]         │
│                                                                            │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ claude               ▾]                               │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

A variant with dim section captions instead of the bare blank line was
drawn and rejected — it spends two rows saying what the gap already says.

### 4.3 No `~/.ssh/config`

An empty dropdown is worse than none. With no parsed hosts the field is a
plain input and the manual fields show directly.

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│           Run in   ( ) Local      (•) SSH      ( ) Kubernetes              │
│                                                                            │
│▸            Host   [                                             ]         │
│                    ↳ no hosts in ~/.ssh/config — type user@host[:port]     │
│    Identity file   [ ~/.ssh/id_ed25519                           ]         │
│      Remote path   [ /srv/payments-api                           ]         │
│                                                                            │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ claude               ▾]                               │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

### 4.4 Parsing caveats

The picker is only as good as the parse, and `~/.ssh/config` has more shape
than it looks:

- **`Host` lines carry several aliases.** `Host staging prod` is one block
  governing both names; settings must attach to every alias on the line,
  not the last. A naive parser gets this wrong — the one written while
  drafting this note did, on the first try.
- **Wildcard patterns are not hosts.** `Host bastion-*` and `Host *` carry
  defaults and must be dropped rather than offered.
- **`Include` must be followed.** `Include ~/.ssh/config.d/*` is common in
  managed setups, and skipping it makes the list look empty for exactly the
  users who would benefit most.
- **`%h`-style tokens do not resolve.** `HostName %h.example.com` cannot be
  displayed as a literal target.

Use a real parser rather than a regex.

Reading `~/.ssh/config` is the whole of Fresh's involvement with it.
**Fresh never writes to it** — see §5.3.

---

## 5. Machines

### 5.1 One machine control instead of a backend row

Once saved machines exist, `Run in:` and the host field collapse into a
single dropdown. Local becomes one entry among others, and Devcontainer —
which §3.4 removed from the backend row — gets its own group below the
machines, since it is per-project rather than per-host.

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│▸         Machine   [                      ▾]                               │
│                    ┌────────────────────────────────────────────────┐      │
│                    │ ▸ Local            this computer              █│      │
│                    │   build-01    ssh  deploy@build-01.ci.internal█│      │
│                    │   gpu-box     ssh  noam@10.4.2.19             █│      │
│                    │   ml-cluster  k8s  research / -l app=trainer  ░│      │
│                    │   staging     ssh  deploy@staging.example.com ░│      │
│                    ├────────────────────────────────────────────────┤      │
│                    │   Devcontainer     in this project             │      │
│                    │   Add machine…                                 │      │
│                    └────────────────────────────────────────────────┘      │
│                                                                            │
│          Project   [ ~/code/payments-api                         ]         │
│                                                                            │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ claude               ▾]                               │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

### 5.2 Remembering a host

A host typed by hand can be saved without leaving the dialog.

```
┌─ New Workspace ─────────────────────────────────────────────────────────[×]┐
│                                                                            │
│        Launch in   [ New workspace        ▾]                               │
│          Machine   [ Other host…          ▾]                               │
│▸          Target   [ noam@10.4.2.19                              ]         │
│    Identity file   [ ~/.ssh/id_ml                                ]         │
│      Remote path   [ /srv/payments-api                           ]         │
│                                                                            │
│                    [v] Remember this machine                               │
│               as   [ gpu-box               ]                               │
│                                                                            │
│             Name   [ fix-webhook-retry                           ]         │
│            Agent   [ claude               ▾]                               │
│     Start prompt   [ Harden token validation…                    ]         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc             │
└────────────────────────────────────────────────────────────────────────────┘
```

### 5.3 Add Machine

A dialog that registers a machine and nothing else. The connection is
**tested before the record is saved**, and the result appears in the dialog
beside the fields that caused it.

```
┌─ Add Machine ───────────────────────────────────────────────────────────[×]┐
│                                                                            │
│             Kind   (•) SSH      ( ) Kubernetes                             │
│                                                                            │
│▸            Name   [ gpu-box                                     ]         │
│           Target   [ noam@10.4.2.19                              ]         │
│    Identity file   [ ~/.ssh/id_ml                                ]         │
│      SSH options   [                                             ]         │
│     Default path   [ /srv                                        ]         │
│                                                                            │
│                    ✓ Connected · Ubuntu 24.04 · git 2.43 · 8 cores         │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Save ] ⏎     [ Test connection ] T          [ Cancel ] Esc              │
└────────────────────────────────────────────────────────────────────────────┘
```

**The registry is Fresh's own state, and Fresh does not touch
`~/.ssh/config`.** Machines live in the platform data dir beside
`workspaces/` and `orchestrator/state/` (see `orchestrator-sessions.md`
§3.1), never in the working tree and never in the user's ssh
configuration. `~/.ssh/config` is read to populate §4.1 and is otherwise
left alone: it is a user-owned file that other tools parse, and a workspace
tool silently editing it is a surprise nobody asked for. A host saved here
is a Fresh machine; a host in `~/.ssh/config` is an ssh host; the picker
offers both and neither becomes the other.

The failure case is the point of the whole dialog, and it is worth being
concrete about the bar. A comparable tool's add-machine command, driven by
hand for this note, refused three different ways with nothing but "lost
connection to server; machine was not saved" — and wrote no matching line
to its own log, so there was no next step to take. That is the failure mode
to design against: an error that names neither the cause nor the field that
produced it. Testing before the write, and reporting beside the input at
fault, is the cheap way to clear it.

```
┌─ Add Machine ───────────────────────────────────────────────────────────[×]┐
│                                                                            │
│             Kind   (•) SSH      ( ) Kubernetes                             │
│                                                                            │
│             Name   [ gpu-box                                     ]         │
│▸          Target   [ noam@10.4.2.19                              ]         │
│    Identity file   [ ~/.ssh/id_ml                                ]         │
│      SSH options   [                                             ]         │
│     Default path   [ /srv                                        ]         │
│                                                                            │
│                    ✗ Permission denied (publickey)                         │
│                        the identity file above was rejected by the host    │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Save anyway ] ⏎    [ Test again ] T         [ Cancel ] Esc              │
└────────────────────────────────────────────────────────────────────────────┘
```

Kubernetes reuses the same grid; only the fields differ.

```
┌─ Add Machine ───────────────────────────────────────────────────────────[×]┐
│                                                                            │
│             Kind   ( ) SSH      (•) Kubernetes                             │
│                                                                            │
│▸            Name   [ ml-cluster                                  ]         │
│          Context   [ gke_prod_us-central1 ▾]                               │
│        Namespace   [ research             ▾]                               │
│              Pod   [ -l app=trainer                              ]         │
│     Default path   [ /workspace                                  ]         │
│                                                                            │
│                    ✓ Connected · 3 pods match · kubectl 1.31               │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ Save ] ⏎     [ Test connection ] T          [ Cancel ] Esc              │
└────────────────────────────────────────────────────────────────────────────┘
```

### 5.4 Machines

The manager doubles as the quick-launch surface: `⏎` on a row opens
New Workspace with that machine already chosen. The list scrolls; `+ Add
machine…` sits outside the scrolling region so it is always reachable.

```
┌─ Machines ──────────────────────────────────────────────────────────────[×]┐
│                                                                            │
│  ▸ Local            this computer                            3 workspaces █│
│    build-01    ssh  deploy@build-01.ci.internal    ✓ ok      2            █│
│    gpu-box     ssh  noam@10.4.2.19                 ✓ ok      1            █│
│    ml-cluster  k8s  research / -l app=trainer      ✗ 2m ago  —            ░│
│    staging     ssh  deploy@staging.example.com     ✓ ok      —            ░│
│                                                                            │
│  + Add machine…                                                            │
│                                                                            │
├────────────────────────────────────────────────────────────────────────────┤
│  [ New workspace here ] ⏎   [ Edit ] E   [ Test ] T    [ Close ] Esc       │
└────────────────────────────────────────────────────────────────────────────┘
```

Three routes reach a configured machine: `+ New` with the Machine dropdown
defaulting to the last one used, `⋯ ▸ Machines…` then `⏎` on a row, or
typing the machine's name into the Machine field, which filters.

---

## 6. Naming

The dock currently says **Task** (`New Task… ▾`, `Search Tasks`) while the
command palette, the dialogs and `glossary.md` all say **workspace** —
which the glossary settled deliberately, and where "session" was retired
precisely because it meant too many things.

The dock's strings are the drift, not the glossary. The redesign resolves
it without a rename: `+ New` and `/` are both noun-free, so the two
offending strings disappear rather than needing changing. Every dialog
title here says **workspace** accordingly.

---

## 7. Open questions

- **Does either list stay readable at twenty-plus workspaces?** Everything
  above was judged against a dock holding three to eight rows. The filter
  drawer, folders and search all earn their space at scale, and the density
  argument for moving them into `⋯` weakens there.
- **Is `⋯` discoverable enough?** It is a "what is in here?" affordance
  where `Filters` was at least a noun. The trade looks right at 38 columns
  but deserves watching a real user hit it.
- **The `Radio` widget (§3.2) is real work**, not a dialog-local change: it
  needs a kind in the widget library, a `Frame`, focus and hover
  behaviour, and capture-test coverage, so that it is consistent wherever
  a mutually exclusive choice appears.
- **Does a saved machine's identity outlive its `~/.ssh/config` twin?**
  A host can exist in both stores with different settings. The picker shows
  both; which wins when the names collide is not designed here.

---

## 8. Implementation status

What landed, by section, and what it was built on. The rule throughout
was to add the missing pieces to the widget library and reach for them
from `orchestrator.ts`, never to hand-roll a look in the plugin.

Widget library (`crates/fresh-editor-core/src/widgets`):

- **`Radio` kind** (§3.2): `label: (•) A   ( ) B`, host-owned selection,
  ←/→ and Home/End, one click target per option, `change {index, value}`,
  `WidgetMutation::SetRadio`; the shell adapter paints what the runtime
  paints (parity-tested).
- **`Label` kind**: one row of static text — a field's hint, a status
  line, a read-only summary — styled and indentable into a form's field
  column. It replaces every `Raw` the dialogs used for prose.
- **Right-aligned label gutter** (§3.3): `labelAlign: "right"` at mount,
  honoured by `Text`, `Toggle`, `Number`, `Dropdown` and `Radio` through
  one `label_width`; a chip-first `Toggle` and a `Label` indent into the
  same column, and a `Text` field's completion list lines its candidates
  up under the value wherever the value sits.
- Lists and trees already painted a scrollbar on overflow (§1); nothing
  to add.

Dock (§2): the header is `[ + New ]` … `/ search` `⋯`; `+ New` opens the
dialog directly; the `⋯` menu holds New Folder, Manage workspaces, the
density rows, the two show switches, the project scope and Hide dock (the
title row and its `×` are gone, §2.4); search is on demand (§2.5) with a
match count. The attention line (§2.3) is not built — it needs the
five-state `agentState` this note deliberately does not design.

Dialogs (§3): one grid for New Workspace and Run Agent (§3.4–3.6);
`Run in` is a radio; no boxes; hints under fields (§3.7); disclosure is
value-driven (the command field follows `custom…`, the branch fields
follow the worktree toggle) so the `Advanced…` fold is gone; footer
buttons keep `[ Label ]` and carry their accelerator; Devcontainer left the
`Run in` set. Placeholders still carry some instructions (`Project Path`,
`SSH options`); moving those under the field is a follow-up.

Not built: the SSH host picker (§4) and the machine registry, `Add
Machine` and `Machines` (§5).
