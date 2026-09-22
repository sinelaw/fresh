# Launch dialog — prompt-first redesign

Status: proposal. Supersedes the "one fixed size" rule of
`orchestrator-ux-redesign.md` §3.8 for the New Workspace / Run Agent form;
keeps every field and behaviour that section lists.

## 1. What it looks like today

Captured in tmux (130×40, `fresh` in a fresh git repo, `claude` picked):

```
┌ ORCHESTRATOR :: New Workspace ────────────────────────────────────────[×]┐
│        Launch in: [New workspace     ▼]                                   │
│          Machine: [Local        ▼]                                        │
│                   ↳ this computer                                         │
│                                                                           │
│                                                                           │  ← 5 blank rows reserved
│                                                                           │    for SSH/k8s connection
│                                                                           │    fields that aren't shown
│                                                                           │
│▸    Project Path: [/home/me/repos/fresh                                 ] │  ← focus lands here
│                   ↳ blank uses this default                               │
│   Workspace Name: [demo-1                                               ] │
│            Agent: [claude   ▼]                                            │
│     Start prompt: [Initial task for the agent (optional)                ] │  ← the thing you came for
│                                                                           │
│                                                                           │
│                   [ ] Auto mode (fewer approval prompts)                  │
│                   [v] Teach agent the Fresh CLI                           │
│                                                                           │
│                   [v] Create a git worktree                               │
│  Checkout branch: [HEAD                                                 ] │
│                   ↳ no origin configured                                  │
│  New branch name: [                                                     ] │
│                   ↳ blank: new branch demo-1, cut from HEAD               │
│                   ↳ made at ~/.local/share/fresh/orchestrator/…/demo-1    │
│                                                                           │
│                                                                           │
│                                                                           │
│        [ Create Workspace ]  ^⏎    [ Create in Background ]  [ Cancel ] Esc│
│                   Tab next / accept  ←→ change option  ↑↓ suggest / history│
└───────────────────────────────────────────────────────────────────────────┘
```

Run Agent is the same form on `Launch in: Current workspace`: ~30 rows,
of which 3 carry information (`runs in this workspace: demo`, `Agent`,
`[ Run ]`); the rest is padding reserved for fields that mode never shows.

### Problems

1. **The prompt is buried.** It is the 7th control, labelled "Start prompt",
   marked "(optional)", single-line, and only appears after an agent is
   picked. Focus opens on Project Path, so the fastest path to "run claude on
   X" is Tab ×3 → → → Tab → type → ^⏎.
2. **Padding bands.** §3.8 reserves every section at its tallest shape so
   rows never move. The cost is 8–25 blank rows in every common shape, which
   reads as a broken layout rather than a stable one.
3. **Every knob is always open.** Machine, path, name, branch, new branch,
   worktree path — six rows of git/host plumbing that the default answer is
   right for ~always, shown at equal weight to the prompt.
4. **Mode switch as a dropdown.** `Launch in` is a closed dropdown whose
   other value hides/reveals half the dialog; the user can't see both
   choices at once, and the title changes under them.
5. **Notes everywhere.** Seven `↳` lines, each explaining a default for a
   field the user usually never touches, doubling the dialog's height.
6. **Footer weight.** Three buttons + accelerator glyphs + a hint bar: 2 rows
   and ~6 things to read to find "go".

## 2. Principles

- **The dialog is built around the prompt.** Everything
  else has a correct default and is shown as a sentence, not a form.
- **Stable top, growing bottom.** Prompt, agent and the Launch button never
  move. Anything that changes height sits below them and only grows when the
  user asked it to (opened a fold / picked a remote). This replaces
  "pad everything to max" with "put variable things last".
- **Summary + fold, not hide.** Every current field is one keypress away,
  and the collapsed summary always states what Launch will do (branch, base,
  machine, path) — the preview the `↳` notes gave, in one line.
- **Hints on focus only.** A field's `↳` note renders while that field is
  focused (or when it holds an error), not all at once.

## 3. The new shape

Visual rules, applied to every shape below:

- **Breathing room.** 3 cells of inner side padding, a blank row under the
  title and above/below the footer, one blank row between groups. The
  dialog gets ~4 rows taller than a dense version, and is still ~10 rows
  shorter than today's because the reserved blank bands are gone.
- **Three labelled groups, top to bottom: Prompt, Agent, Where.** Each
  group label sits on its own column at the left margin; within Where, the
  field labels are right-aligned into one column as today.
- **The prompt box is the only boxed element.** It's the visual anchor: full
  width, rounded frame, 4 rows. Every other input keeps the `[ … ]` style.
- **Where is fenced by two thin rules** so it reads as one collapsible unit,
  whether it is one line or ten.
- **One primary button, bottom-right**, with the secondary action as plain
  text to its left. One quiet key-hint line under it.

### 3.1 New workspace — collapsed (default)

```
┌─ New Workspace ────────────────────────────────────────────────────── × ─┐
│                                                                           │
│   ( New workspace )    Here · demo                                        │
│                                                                           │
│   Prompt                                                                  │
│   ╭─────────────────────────────────────────────────────────────────────╮ │
│ ▸ │ fix the flaky resize test in split_view.rs█                         │ │
│   │                                                                     │ │
│   │                                                                     │ │
│   │                                                                     │ │
│   ╰─────────────────────────────────────────────────────────────────────╯ │
│                                                                           │
│   Agent    [ claude ▾ ]       [ ] Auto mode      [✓] Teach Fresh CLI      │
│                                                                           │
│   ─────────────────────────────────────────────────────────────────────   │
│   ▸ Where   Local · ~/repos/fresh                                         │
│             new worktree fresh-47, branched from origin/master            │
│   ─────────────────────────────────────────────────────────────────────   │
│                                                                           │
│                                   Launch in background    [  Launch  ]    │
│                                                                           │
│        Ctrl+⏎ launch   Alt+⏎ background   Alt+W where   Esc cancel        │
└───────────────────────────────────────────────────────────────────────────┘
```

- **Focus opens in the prompt** (▸). Enter = newline, Ctrl+Enter = Launch.
  Typing a prompt and pressing Ctrl+Enter is the whole flow.
- **Mode switch** is a two-segment toggle on the first row: the selected
  segment is bracketed `( … )`; the other is plain text. ←/→ or click flips it.
  "Here" carries the current workspace's name. The title follows the mode
  (`New Workspace` / `Run Agent`).
- **Agent row**: dropdown and the agent's switches on one line. A switch the
  agent doesn't support is not drawn. `custom…` inserts a `Command` row
  directly under it.
- **Where summary** is two lines: *where it runs* (machine · path) and *what
  it does to git* (the existing branch-plan preview). Non-git path:
  `in place, no git`. Existing linked worktree: `attach to existing
  worktree <name>`.

### 3.2 New workspace — expanded (local)

Enter / click on `▸ Where`, or Alt+W, from anywhere. The fold state is
remembered for next time.

```
┌─ New Workspace ────────────────────────────────────────────────────── × ─┐
│                                                                           │
│   ( New workspace )    Here · demo                                        │
│                                                                           │
│   Prompt                                                                  │
│   ╭─────────────────────────────────────────────────────────────────────╮ │
│   │ fix the flaky resize test in split_view.rs                          │ │
│   │                                                                     │ │
│   │                                                                     │ │
│   │                                                                     │ │
│   ╰─────────────────────────────────────────────────────────────────────╯ │
│                                                                           │
│   Agent    [ claude ▾ ]       [ ] Auto mode      [✓] Teach Fresh CLI      │
│                                                                           │
│   ─────────────────────────────────────────────────────────────────────   │
│   ▾ Where                                                                 │
│                                                                           │
│          Machine   [ Local ▾ ]                                            │
│          Project   [ ~/repos/fresh                                    ]   │
│        Workspace   [ fresh-47                                         ]   │
│                                                                           │
│              Git   [✓] Create a worktree                                  │
│      Branch from ▸ [ origin/master                                    ]   │
│                    ↳ an existing branch or ref; blank = origin/master     │
│       New branch   [ fresh-47                                         ]   │
│                                                                           │
│                    worktree at ~/.local/share/fresh/orchestrator/…/fresh-47
│   ─────────────────────────────────────────────────────────────────────   │
│                                                                           │
│                                   Launch in background    [  Launch  ]    │
│                                                                           │
│        Ctrl+⏎ launch   Alt+⏎ background   Alt+W where   Esc cancel        │
└───────────────────────────────────────────────────────────────────────────┘
```

- The top half is identical to the collapsed form — nothing above the rule
  moves.
- Where has two sub-groups separated by a blank row: **location** (Machine,
  Project, Workspace) and **git** (worktree toggle, Branch from, New
  branch). Renames: "Checkout branch" → "Branch from", "Workspace Name" →
  "Workspace", "Project Path" → "Project".
- `New branch` shows the planned name as its placeholder instead of a
  separate "blank: new branch …" note.
- The one `↳` note shows only under the focused field (here: Branch from).
- The worktree location is a single dim line at the end of the group, not a
  note on a field.
- Worktree off: `Branch from` stays (in-place checkout), `New branch` and
  the location line disappear.

### 3.3 New workspace — expanded, remote machine

Picking any non-local machine opens the fold by itself (there's something
to fill in). The connection fields sit right under Machine.

```
│   ▾ Where                                                                 │
│                                                                           │
│          Machine   [ Other host… ▾ ]                                      │
│             Host   [ user@box:22                                      ]   │
│         Identity   [ ~/.ssh/id_ed25519                    (optional)  ]   │
│      SSH options   [ -J jump                              (optional)  ]   │
│                    [ ] Remember this machine as [ box              ]      │
│                                                                           │
│          Project   [ ~/src/app                                        ]   │
│        Workspace   [ app-3                                            ]   │
│                                                                           │
│              Git   [✓] Create a worktree           ✓ git repo on box      │
│      Branch from   [ main                                             ]   │
│       New branch   [ app-3                                            ]   │
│                                                                           │
│                    worktree at ~/.fresh/worktrees/app/app-3 (on box)      │
│   ─────────────────────────────────────────────────────────────────────   │
```

- The remote-git check result goes on the worktree row, to the right:
  `checking box…`, `✓ git repo on box`, `not a git repo`,
  `can't reach box`, `new host key — you'll confirm on launch`.
- "Remember this machine" moves up into the connection group, since it's
  about the connection.
- Kubernetes uses the same slot: `Target`, `Context`, `Namespace`, `Pod`,
  then Remember. A saved machine or ssh-config host shows no connection
  rows, just the Machine row with a hint (`user@host:port`).
- Collapsed summary for a remote: `▸ Where   box · ~/src/app` /
  `new worktree app-3, branched from main`.

### 3.4 Here (Run Agent)

```
┌─ Run Agent ────────────────────────────────────────────────────────── × ─┐
│                                                                           │
│     New workspace    ( Here · demo )                                      │
│                                                                           │
│   Prompt                                                                  │
│   ╭─────────────────────────────────────────────────────────────────────╮ │
│ ▸ │ █                                                                   │ │
│   │                                                                     │ │
│   │                                                                     │ │
│   │                                                                     │ │
│   ╰─────────────────────────────────────────────────────────────────────╯ │
│                                                                           │
│   Agent    [ claude ▾ ]       [ ] Auto mode      [✓] Teach Fresh CLI      │
│                                                                           │
│                                                            [  Run  ]      │
│                                                                           │
│                    Ctrl+⏎ run   Esc cancel                                │
└───────────────────────────────────────────────────────────────────────────┘
```

No Where section: the location is the "Here · demo" segment. Switching to
New workspace adds the Where section between Agent and the footer; the
prompt text and agent choice carry over.

### 3.5 Agent variations

```
│   Agent    [ terminal ▾ ]                                                 │
```
`terminal`: the prompt box stays in place but reads `Prompt — sent to the
shell as its first command` (or is disabled; see open questions). No
switches.

```
│   Agent    [ custom… ▾ ]                                                  │
│  Command   [ codex resume --last                                      ]   │
```
`custom…`: a Command row appears under Agent; the prompt box is used if
the command's agent takes one.

### 3.6 Short terminals

Under ~34 rows the blank spacer rows are dropped first (padding → 0, rules
stay), and the prompt box shrinks to 2 rows. The Where section, when
expanded, scrolls inside the dialog rather than pushing the footer off.

### 3.7 Submitting

The form turns read-only: the prompt stays visible (dimmed) with the Where
summary under it and `Creating workspace… Esc to cancel` in the footer, in
place of today's separate `Run in: / Host: / Project:` view.

## 4. Feature parity checklist

| Today | Redesign |
|---|---|
| Launch in: Current / New | two-segment toggle, first row |
| Machine dropdown (Local, saved, ssh-config hosts, Other host…, Kubernetes…, Devcontainer) | fold, first row; picking remote auto-opens fold |
| SSH target / identity / options | fold, under Machine when `Other host…` |
| k8s target / context / namespace / pod | fold, under Machine when `Kubernetes…` |
| Project Path (+ completions, history, linked-worktree hint) | fold `Project` — same widget, note on focus |
| Workspace Name (auto placeholder) | fold `Workspace`; also in collapsed summary |
| Agent dropdown, custom… → Command | agent row; Command under it when custom |
| Start prompt | `Prompt` box, top, multi-line, initial focus |
| Auto mode / Teach Fresh CLI (per-agent) | same switches, inline on agent row |
| Create git worktree / Checkout branch / New branch / branch plan / worktree path | fold; plan + path condensed into summary line |
| Remote git probe states (probing / unreachable / untrusted / non-git) | fold, inline status next to the worktree toggle |
| Remember this machine as … | fold, last row, typed hosts only |
| Create / Create in Background / Cancel | Launch (^⏎) / Launch in background (Alt+⏎) / × + Esc |
| Error row | above the buttons, red, replaces nothing |
| Compact fallback for short terminals | the new shape *is* compact; fold opens scrollable |
| Prefill from discovery (`openFormPrefilled`), `Machines ▸ New workspace here` | unchanged; seeded machine ≠ Local ⇒ fold opens |

## 5. Implementation notes

All widgets exist in `plugins/lib/widgets.ts` (`radio`, `textArea`,
`dropdown`, `toggle`, `row`, `button`). Changes are confined to
`orchestrator.ts`:

- `buildFormSpecFixed` / `buildFormSpecCompact` / `padRows` /
  `connectionRowsMax` / `tailRowsMax` → one `buildFormSpec` that emits
  header, prompt, agent row, where-line-or-fold, footer. Deletes the
  row-reservation machinery.
- `targetRow` → a two-segment toggle (`radio` laid out in a `row`). `mountFormPanel` initial focus → `start_prompt`.
- `startPromptFields` → `textArea({rows: 4})` under a `Prompt` label, rounded frame. `FORM_MODE_BINDINGS` already
  binds `C-Enter` to submit; the `Enter` shim (`orchestrator_form_key_enter`)
  must forward to the host's text dispatch (newline) when focus is the
  prompt box instead of advancing focus. Add `M-Enter` → create-bg and `M-w` → toggle Where (currently unbound in this mode).
- Padding: a blank row between groups and 3-cell inner side padding; `divider` rules fence Where. Drop spacers first on short screens (§3.6).
- New `form.whereOpen` (persisted), `whereSummary(f)` built from
  `branchPlanNote`, `plannedWorkspaceName`, machine label and path.
- `fieldNote` calls gated on `focusKey === key || error`.
- Focus cycle (`rebuildFormFocusCycle`) follows the new order: radio,
  prompt, agent (+command, switches), where toggle/fields, buttons.
- Tests that pin rows (`tests/e2e/plugins/orchestrator_new_dialog.rs`,
  `orchestrator_*`) need their screen assertions updated; the submit paths
  and probes are untouched, so behavioural tests should hold.

## 6. Open questions

1. Prompt for `terminal`: run it as the first command, or disable the box?
2. Keep the where-fold open state per-user globally, or per project?
3. Should `Here` be the default when the palette's `Run Agent…` is used and
   `New workspace` for `+ New` / Alt+N (today's split), or should both open
   on the last-used choice?
