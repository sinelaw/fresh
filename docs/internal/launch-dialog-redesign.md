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

- **The dialog asks one question: "what should the agent do?"** Everything
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

### 3.1 Default (New Workspace, local git repo) — 11 rows

```
┌ New Workspace ────────────────────────────────────────────────────────[×]┐
│  ● New workspace   ○ Here (demo)                                          │
│                                                                           │
│▸ What should the agent do?                                                │
│  ┌───────────────────────────────────────────────────────────────────────┐│
│  │ fix the flaky resize test in split_view.rs█                           ││
│  │                                                                       ││
│  └───────────────────────────────────────────────────────────────────────┘│
│  Agent [claude ▼]   [ ] auto   [v] teach Fresh CLI                        │
│                                                                           │
│  ▸ Local · ~/repos/fresh · new branch fresh-47 from origin/master   Alt+O │
│                                                                           │
│                              [ Launch ]  ^⏎    Launch in background  Alt+⏎│
└───────────────────────────────────────────────────────────────────────────┘
```

- Focus opens **in the prompt** (a 3-row `textArea`; Enter = newline,
  ^⏎ = launch). Type, ^⏎. Two actions from palette to running agent.
- `● New workspace ○ Here (demo)` is a `radio` replacing `Launch in`: both
  choices visible, ←/→ flips, the workspace name is right there. The title
  follows it (`New Workspace` / `Run Agent`) as today.
- The agent row packs dropdown + its switches onto one line; switches still
  appear only for agents that support them. `custom…` reveals the Command
  field directly under the row (unchanged rule, §3.5).
- The prompt box shows for **every** agent. For `terminal` it becomes
  `Command to run` (the shell gets it as its first line) — or, if we'd rather
  not add behaviour, it's disabled with placeholder `terminal takes no
  prompt`. Either way it doesn't vanish, so the layout doesn't jump.
- The **where-line** is a fold: the summary is `branchPlanNote` +
  machine + path compressed to one line. Enter / click / Alt+O opens it.
- One primary button. `Cancel` goes (× and Esc remain, hint is in the
  frame); `Create in Background` becomes a quiet link-styled button with its
  own accelerator, still Tab-reachable.

### 3.2 Where-fold open (local)

```
│  ▾ Where                                                           Alt+O │
│        Machine  [Local ▼]                                                 │
│        Project  [~/repos/fresh                                        ]   │
│      Workspace  [fresh-47                                             ]   │
│                 [v] new git worktree                                      │
│      Branch off [origin/master                                        ]   │
│     New branch  [                                                     ]   │
│                 ↳ blank: new branch fresh-47, cut from origin/master      │  (focused-field note)
```

Same fields, same order, same probes and completions as today — just
grouped under the fold and without the reserved padding. The fold's open
state is remembered (`orchestrator.where_open`), so a user who always tweaks
branches sees it open every time.

### 3.3 Remote machine picked

Picking a non-local machine auto-opens the fold (there is something to fill
in) and shows only that machine's connection fields — SSH target/identity/
options, or k8s target/context/namespace/pod — followed by `Remember this
machine`. Rows appear below the agent row, so nothing above moves.

```
│  ▾ Where                                                                  │
│        Machine  [Other host… ▼]                                           │
│           Host  [user@box:22                                          ]   │
│       Identity  [~/.ssh/id_ed25519 (optional)                         ]   │
│    SSH options  [-J jump (optional)                                   ]   │
│        Project  [~ (remote home)                                      ]   │
│                 [v] new git worktree   ✓ git repo on box                  │
│                 [ ] remember this machine as [box        ]                │
```

The collapsed summary for a saved machine reads
`▸ gpu-box · ~/src/app · new branch app-3 from main`.

### 3.4 Run Agent (Here) — 8 rows

```
┌ Run Agent ────────────────────────────────────────────────────────────[×]┐
│  ○ New workspace   ● Here (demo)                                          │
│                                                                           │
│▸ What should the agent do?                                                │
│  ┌───────────────────────────────────────────────────────────────────────┐│
│  │ █                                                                     ││
│  └───────────────────────────────────────────────────────────────────────┘│
│  Agent [claude ▼]   [ ] auto   [v] teach Fresh CLI                        │
│                                                        [ Run ]  ^⏎        │
└───────────────────────────────────────────────────────────────────────────┘
```

No where-line (there is no "where" to choose); the path is in the radio
label. Flipping the radio to `New workspace` inserts the where-line above
the buttons — the prompt and its text are untouched.

### 3.5 Submitting

The read-only "Connecting…" view keeps its content but uses the same
summary line instead of `Run in: / Host: / Project:` rows, with the
prompt shown dimmed above it so the user sees what they sent.

## 4. Feature parity checklist

| Today | Redesign |
|---|---|
| Launch in: Current / New | radio row, top |
| Machine dropdown (Local, saved, ssh-config hosts, Other host…, Kubernetes…, Devcontainer) | fold, first row; picking remote auto-opens fold |
| SSH target / identity / options | fold, under Machine when `Other host…` |
| k8s target / context / namespace / pod | fold, under Machine when `Kubernetes…` |
| Project Path (+ completions, history, linked-worktree hint) | fold `Project` — same widget, note on focus |
| Workspace Name (auto placeholder) | fold `Workspace`; also in collapsed summary |
| Agent dropdown, custom… → Command | agent row; Command under it when custom |
| Start prompt | prompt box, top, multi-line, initial focus |
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
- `targetRow` → `radio`. `mountFormPanel` initial focus → `start_prompt`.
- `startPromptFields` → `textArea({rows: 3})`. `FORM_MODE_BINDINGS` already
  binds `C-Enter` to submit; the `Enter` shim (`orchestrator_form_key_enter`)
  must forward to the host's text dispatch (newline) when focus is the
  prompt box instead of advancing focus. Add `M-Enter` → create-bg.
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
