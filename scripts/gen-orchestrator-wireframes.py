#!/usr/bin/env python3
"""Generate docs/internal/orchestrator-ux-redesign.md.

The wireframes in that note are ASCII, and hand-counting box drawing does not
survive editing: every frame here is built through padding helpers and checked,
so a row that overflows its box fails the build instead of shipping crooked.

Usage:  python3 scripts/gen-orchestrator-wireframes.py [OUT]
        (default OUT: docs/internal/orchestrator-ux-redesign.md)
"""
import sys, re
from textwrap import dedent

LW, W, FIELDCOL, PW, DOCK = 15, 76, 20, 48, 38
THUMB, TRACK = "█", "░"


def box(rows, w=W, title=None, close=True):
    out = []
    if title:
        t, tail = f"─ {title} ", ("[×]" if close else "")
        out.append("┌" + t + "─" * (w - len(t) - len(tail)) + tail + "┐")
    else:
        out.append("┌" + "─" * w + "┐")
    for r in rows:
        if r == "-":
            out.append("├" + "─" * w + "┤")
        else:
            if len(r) > w:
                raise ValueError(f"overflow {len(r) - w} in {w}-wide box: {r!r}")
            out.append("│" + r.ljust(w) + "│")
    out.append("└" + "─" * w + "┘")
    return "\n".join(out)


def bar(rows, w, first, size):
    """Give a scrollable region its 1-column scrollbar."""
    return [r.ljust(w - 1) + (THUMB if first <= i < first + size else TRACK)
            for i, r in enumerate(rows)]


def fld(label, value, w=44, focus=False, kind="text", trail=""):
    mark = "▸" if focus else " "
    body = ("[ " + value.ljust(w - 3) + "▾]") if kind == "drop" else \
           ("[ " + value.ljust(w) + "]") if kind == "text" else value
    row = f"{mark} {label.rjust(LW)}   {body}"
    return (row + "  " + trail).rstrip() if trail else row


def plain(t, indent=FIELDCOL):
    return " " * indent + t


def pop(scrolling, footer=(), first=0, size=2):
    out = [" " * FIELDCOL + "┌" + "─" * PW + "┐"]
    for r in bar(list(scrolling), PW, first, size):
        if len(r) > PW:
            raise ValueError(f"popup overflow {len(r) - PW}: {r!r}")
        out.append(" " * FIELDCOL + "│" + r.ljust(PW) + "│")
    if footer:
        out.append(" " * FIELDCOL + "├" + "─" * PW + "┤")
        for r in footer:
            out.append(" " * FIELDCOL + "│" + r.ljust(PW) + "│")
    out.append(" " * FIELDCOL + "└" + "─" * PW + "┘")
    return out


def mrow(name, kind, target, sel=False):
    return f" {'▸' if sel else ' '} {name.ljust(11)} {kind.ljust(4)} {target}"


def check(text):
    for blk in re.findall(r"^┌.*?^└[^\n]*", text, re.S | re.M):
        widths = {len(l) for l in blk.split("\n") if l[:1] in "┌│├└"}
        if len(widths) != 1:
            raise AssertionError(f"ragged box, widths={sorted(widths)}\n{blk}")
    return text


P = []
def md(s):  P.append(dedent(s).strip("\n"))
def art(s): P.append("```\n" + check(s) + "\n```")

# ══════════════════════════════════════════════════════════════════════════
md("""
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
""")

art(box([
    " [ New Task… ▾ ] [Search Tasks      ]",
    " [ ▸ Filters ] [ view: compact ]",
    "-",
    "  · payments-api",
    "  · fix-webhook-retry · claude",
    "  · add-idempotency · codex",
    "  · schema-cleanup",
    "  · upgrade-vite · claude",
    "  · dark-mode · claude",
], w=DOCK, title="Orchestrator", close=False))

md("""
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
""")

TREE = [
    " ▾ payments-api            ●2 ✓1",
    "   ● fix-webhook-retry  claude    4m",
    "   ✓ add-idempotency    codex    10m",
    "   · schema-cleanup     —",
    " ▾ web                     ●1 ◐1",
    "   ● upgrade-vite       claude   22m",
    "   ◐ dark-mode          claude    1m",
    " ▸ infra                   ?1",
]
ATTN = " ● 2 need you · ✓ 1 done            ▾"

art(box([" + New                  / search    ⋯ ", "-", ATTN, "-"]
        + bar(TREE, DOCK, 0, 5) + [""], w=DOCK, close=False))

md("""
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
""")

QUIET = [
    " ▾ payments-api",
    "   · fix-webhook-retry  claude   12m",
    "   · add-idempotency    codex    41m",
    "   · schema-cleanup     —",
    " ▾ web",
    "   · upgrade-vite       claude    8m",
    "   · dark-mode          claude    3m",
    " ▸ infra",
    "",
    "",
]
art(box([" + New                  / search    ⋯ ", "-"] + bar(QUIET, DOCK, 0, 8),
        w=DOCK, close=False))

md("""
    With nothing blocked there is no chrome at all above the list: the row that
    would carry the summary is simply not drawn. The thumb is longer here
    because fewer rows are hidden.

    ### 2.4 The `⋯` menu

    Three of the four header controls are *settings*, not actions, and settings
    belong in a menu. Folder creation joins them, and the dock's title row and
    its `×` are absorbed too — closing the dock is rare and `Alt+O` undoes it.

    The menu is short and fixed, so it has no scrollbar. That contrast is
    deliberate: a track means the region scrolls.
""")

art(box([
    " New folder…",
    " Manage workspaces…",
    "-",
    " view      compact · comfortable · detail",
    " show      [ ] empty   [ ] all worktrees",
    " scope     this project ▾",
    "-",
    " Hide dock                        Alt+O",
], w=42, close=False))

md("""
    ### 2.5 Search on demand

    `/` turns the action row into a filter with a match count and `Esc` to
    leave — the behaviour the command palette already has. A permanent search
    box above a three-row list is pure cost; at twenty workspaces it is
    essential, so it should appear exactly when it earns its row.
""")

art(box(bar([
    " ▾ payments-api",
    "   ● fix-webhook-retry   claude       4m",
    " ▾ web",
    "   ● upgrade-vite        claude      22m",
    "   · (3 more matches)",
], 44, 0, 3) + ["-", " / webh▏                        5 of 9   Esc"],
    w=44, close=False))

md("""
    ### 2.6 Narrow

    At 26 columns the same structure holds; only the columns inside each row
    are dropped. The scrollbar is the one piece of chrome that does not shrink.
""")

art(box([" ● 2 need you           ▾", "-"] + bar([
    " ▾ payments-api  ●2 ✓1",
    "   ● fix-webhook…    4m",
    "   ✓ add-idempot…   10m",
    "   · schema-clea…",
    " ▾ web           ●1 ◐1",
    "   ● upgrade-vite   22m",
    "   ◐ dark-mode       1m",
    " ▸ infra         ?1",
], 26, 0, 5) + ["-", " + New       /         ⋯"], w=26, close=False))

md("""
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
""")

art("""┌ ORCHESTRATOR :: New Workspace ────────────────────────────────────────[×]┐
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
└──────────────────────────────────────────────────────────────────────────┘""")

md("""
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
""")

FOOT_T = ["-", "  [ Create ] ⏎    [ Create in background ] ⇧⏎    [ Cancel ] Esc"]
RUNIN = "(•) Local      ( ) SSH      ( ) Kubernetes"

art(box([
    "",
    fld("Launch in", "New workspace", w=24, kind="drop"),
    fld("Run in", RUNIN, kind="raw"),
    "",
    fld("Project", "~/code/payments-api", focus=True),
    fld("Name", "fix-webhook-retry"),
    fld("Agent", "claude", w=24, kind="drop"),
    fld("Start prompt", "Harden token validation…"),
    "",
    plain("[v] Create a git worktree"),
    fld("from branch", "main", w=24, kind="drop"),
    fld("new branch", "fix-webhook-retry"),
    "",
    plain("[ ] Auto mode — fewer approval prompts"),
    plain("[v] Teach the agent Fresh's CLI"),
    "",
] + FOOT_T, title="New Workspace"))

md("""
    Sixteen rows against today's twenty-eight with Advanced open, showing the
    same fields. Against today's *collapsed* eighteen it is two rows taller,
    because the worktree fields that today need Advanced are visible here.

    **Devcontainer is gone from `Run in`.** It is a property of a project, not
    a host — every other option in that row names *where a machine is* — so it
    does not belong in the same mutually exclusive set. It reappears in the
    machine picker (§5.1) as its own group, which is the one place a
    per-project backend sits honestly beside per-host ones.

    ### 3.5 The agent command, revealed by the dropdown
""")

art(box([
    "",
    fld("Launch in", "New workspace", w=24, kind="drop"),
    fld("Run in", RUNIN, kind="raw"),
    "",
    fld("Project", "~/code/payments-api"),
    fld("Name", "fix-webhook-retry"),
    fld("Agent", "custom…", w=24, kind="drop"),
    fld("Command", "/root/fakebin/claude-blocking", focus=True),
    fld("Start prompt", "Harden token validation…"),
    "",
    plain("[ ] Create a git worktree"),
    "",
] + FOOT_T, title="New Workspace"))

md("""
    ### 3.6 Run Agent

    The same grid with fewer rows. The two dialogs share one form, one
    alignment and one footer; today they share a form but not a visual system.
""")

art(box([
    "",
    fld("Launch in", "Current workspace", w=24, kind="drop"),
    fld("Agent", "claude", w=24, kind="drop", focus=True),
    fld("Start prompt", "Review the diff on this branch"),
    "",
    plain("[ ] Auto mode — fewer approval prompts"),
    "",
    "-",
    "  [ Run ] ⏎                                     [ Cancel ] Esc",
], title="Run Agent"))

md("""
    ### 3.7 Hints belong under the field, never inside it
""")

art(box([
    "",
    fld("Host", "build-01", focus=True),
    plain("↳ deploy@build-01:22 · ssh://host/path", indent=FIELDCOL + 2),
    "",
    fld("Remote path", ""),
    plain("↳ blank = remote home", indent=FIELDCOL + 2),
    "",
], title="Placeholders vs hints", close=False))

md("""
    ---

    ## 4. SSH

    ### 4.1 A host picker driven by `~/.ssh/config`

    The host field opens the hosts already configured on the machine. Choosing
    one is a click; typing filters. The list scrolls — nine hosts here, five
    visible — and `Other host…` sits below the scrolling region, so it stays
    reachable however far the list is scrolled.
""")

TASKROWS = [fld("Name", "fix-webhook-retry"),
            fld("Agent", "claude", w=24, kind="drop"),
            fld("Start prompt", "Harden token validation…"), ""]
SSHTOP = ["",
          fld("Launch in", "New workspace", w=24, kind="drop"),
          fld("Run in", "( ) Local      (•) SSH      ( ) Kubernetes", kind="raw"),
          ""]

art(box(SSHTOP + [fld("Host", "", focus=True)] + pop([
    " ▸ build-01     deploy@build-01.ci.internal",
    "   gpu-box      noam@10.4.2.19",
    "   staging      deploy@staging.example.com",
    "   prod         deploy@prod.example.com",
    "   testbox      root@127.0.0.1:2222",
], footer=["   Other host…"], first=0, size=3)
    + [fld("Remote path", "/srv/payments-api"), ""] + TASKROWS + FOOT_T,
    title="New Workspace"))

md("""
    ### 4.2 After picking

    The list collapses and the resolved target is shown inline, dim, right of
    the field. User, port and identity come from the config entry, so those
    fields never appear — **the config is the source of truth and the dialog
    only points at it**. Only `Other host…` reveals them.

    The blank line between `Remote path` and `Name` is the whole grouping
    device: connection above, workspace below.
""")

art(box(SSHTOP + [
    fld("Host", "build-01", w=24, kind="drop",
        trail="deploy@build-01.ci.internal"),
    fld("Remote path", "/srv/payments-api", focus=True),
    "",
] + TASKROWS + FOOT_T, title="New Workspace"))

md("""
    A variant with dim section captions instead of the bare blank line was
    drawn and rejected — it spends two rows saying what the gap already says.

    ### 4.3 No `~/.ssh/config`

    An empty dropdown is worse than none. With no parsed hosts the field is a
    plain input and the manual fields show directly.
""")

art(box(SSHTOP + [
    fld("Host", "", focus=True),
    plain("↳ no hosts in ~/.ssh/config — type user@host[:port]"),
    fld("Identity file", "~/.ssh/id_ed25519"),
    fld("Remote path", "/srv/payments-api"),
    "",
] + TASKROWS + FOOT_T, title="New Workspace"))

md("""
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
""")

art(box(["",
    fld("Launch in", "New workspace", w=24, kind="drop"),
    fld("Machine", "", w=24, kind="drop", focus=True)]
    + pop([
        mrow("Local", "", "this computer", sel=True),
        mrow("build-01", "ssh", "deploy@build-01.ci.internal"),
        mrow("gpu-box", "ssh", "noam@10.4.2.19"),
        mrow("ml-cluster", "k8s", "research / -l app=trainer"),
        mrow("staging", "ssh", "deploy@staging.example.com"),
    ], footer=["   Devcontainer     in this project", "   Add machine…"],
        first=0, size=3)
    + ["",
    fld("Project", "~/code/payments-api"),
    "",
] + TASKROWS + FOOT_T, title="New Workspace"))

md("""
    ### 5.2 Remembering a host

    A host typed by hand can be saved without leaving the dialog.
""")

art(box(["",
    fld("Launch in", "New workspace", w=24, kind="drop"),
    fld("Machine", "Other host…", w=24, kind="drop"),
    fld("Target", "noam@10.4.2.19", focus=True),
    fld("Identity file", "~/.ssh/id_ml"),
    fld("Remote path", "/srv/payments-api"),
    "",
    plain("[v] Remember this machine"),
    fld("as", "gpu-box", w=22),
    "",
] + TASKROWS + FOOT_T, title="New Workspace"))

md("""
    ### 5.3 Add Machine

    A dialog that registers a machine and nothing else. The connection is
    **tested before the record is saved**, and the result appears in the dialog
    beside the fields that caused it.
""")

art(box([
    "",
    fld("Kind", "(•) SSH      ( ) Kubernetes", kind="raw"),
    "",
    fld("Name", "gpu-box", focus=True),
    fld("Target", "noam@10.4.2.19"),
    fld("Identity file", "~/.ssh/id_ml"),
    fld("SSH options", ""),
    fld("Default path", "/srv"),
    "",
    plain("✓ Connected · Ubuntu 24.04 · git 2.43 · 8 cores"),
    "",
    "-",
    "  [ Save ] ⏎     [ Test connection ] T          [ Cancel ] Esc",
], title="Add Machine"))

md("""
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
""")

art(box([
    "",
    fld("Kind", "(•) SSH      ( ) Kubernetes", kind="raw"),
    "",
    fld("Name", "gpu-box"),
    fld("Target", "noam@10.4.2.19", focus=True),
    fld("Identity file", "~/.ssh/id_ml"),
    fld("SSH options", ""),
    fld("Default path", "/srv"),
    "",
    plain("✗ Permission denied (publickey)"),
    plain("  the identity file above was rejected by the host",
          indent=FIELDCOL + 2),
    "",
    "-",
    "  [ Save anyway ] ⏎    [ Test again ] T         [ Cancel ] Esc",
], title="Add Machine"))

md("""
    Kubernetes reuses the same grid; only the fields differ.
""")

art(box([
    "",
    fld("Kind", "( ) SSH      (•) Kubernetes", kind="raw"),
    "",
    fld("Name", "ml-cluster", focus=True),
    fld("Context", "gke_prod_us-central1", w=24, kind="drop"),
    fld("Namespace", "research", w=24, kind="drop"),
    fld("Pod", "-l app=trainer"),
    fld("Default path", "/workspace"),
    "",
    plain("✓ Connected · 3 pods match · kubectl 1.31"),
    "",
    "-",
    "  [ Save ] ⏎     [ Test connection ] T          [ Cancel ] Esc",
], title="Add Machine"))

md("""
    ### 5.4 Machines

    The manager doubles as the quick-launch surface: `⏎` on a row opens
    New Workspace with that machine already chosen. The list scrolls; `+ Add
    machine…` sits outside the scrolling region so it is always reachable.
""")

def machrow(name, kind, target, status, tasks, sel=False):
    return (f"  {'▸' if sel else ' '} {name.ljust(12)}{kind.ljust(5)}"
            f"{target.ljust(31)}{status.ljust(10)}{tasks}")

art(box([""] + bar([
    machrow("Local", "", "this computer", "", "3 workspaces", sel=True),
    machrow("build-01", "ssh", "deploy@build-01.ci.internal", "✓ ok", "2"),
    machrow("gpu-box", "ssh", "noam@10.4.2.19", "✓ ok", "1"),
    machrow("ml-cluster", "k8s", "research / -l app=trainer", "✗ 2m ago", "—"),
    machrow("staging", "ssh", "deploy@staging.example.com", "✓ ok", "—"),
], W, 0, 3) + [
    "",
    plain("+ Add machine…", indent=2),
    "",
    "-",
    "  [ New workspace here ] ⏎   [ Edit ] E   [ Test ] T    [ Close ] Esc",
], title="Machines"))

md("""
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
""")

# ══════════════════════════════════════════════════════════════════════════
md("""
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

""")

out = "\n\n".join(P) + "\n"
dest = sys.argv[1] if len(sys.argv) > 1 else "docs/internal/orchestrator-ux-redesign.md"
with open(dest, "w", encoding="utf-8") as fh:
    fh.write(out)
print(f"wrote {dest} — {len(out.splitlines())} lines, "
      f"{out.count('```') // 2} figures")
