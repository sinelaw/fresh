# Launch dialog & repositories — redesign

Status: proposal. Replaces the New Workspace / Run Agent form described in
`orchestrator-ux-redesign.md` §3.4–§3.8 (including its "one fixed size"
rule), and adds a Repositories registry next to Machines. Every existing
field and behaviour is kept; §5 maps each one.

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

- **The prompt comes first.** Focus opens in a multi-line prompt box. Type,
  press Ctrl+Enter.
- **Two independent choices say where:** *Project* (a known repository or a
  folder) and *Machine*. Each is one dropdown.
- **The main clone is visible, never guessed.** For a repository, the clone
  that worktrees are cut from is shown on every screen and is set in one
  place.
- **Nothing is cloned without asking.** Every clone, whether from Launch or
  from the Repositories dialog, goes through a confirmation that names the
  URL, machine and path.
- **No automatic scanning.** Fresh never searches a machine for clones. You
  type a path or browse to one, and Fresh checks that path only.
- **Stable top, growing bottom.** Prompt, agent and the footer don't move.
  Anything that changes height sits below them and only grows when you ask
  (the `▹ Details` row, a remote machine, a warning).

### 2.1 Visual rules

- 4 cells of inner padding on each side; a blank row under the title and
  around the footer.
- **Section headers carry their own rule** (`PROMPT ────`, `AGENT ────`,
  `WHERE ────`, `GIT ────`, `MAIN CLONE ────`), with a blank row below each
  and between sections.
- One label column (right-aligned, 16 cells) for every labelled row in every
  dialog, so values always start at the same column.
- Status goes directly under or beside the value it describes: `✓` ok, `⚠`
  needs a decision (warning color, `diagnostic.warning_fg`), `✗` blocked
  (error color). A `⚠` or `✗` note is the only colored text.
- The prompt box is the only framed input. The footer is a full-width rule,
  then the primary button at the right with the secondary action as plain
  text beside it, then one key-hint line.
- Short terminals: drop the blank rows under the section headers first, then
  shrink the prompt box to 2 rows. Expanded sections scroll inside the
  dialog; the footer stays visible.

## 3. Model

| Choice | Answers | Examples |
|---|---|---|
| **Project** | what you work on: a known repository, or any folder | fresh, infra, `~/notes` |
| **Machine** | where it runs | Local, gpu-box, ml-cluster |
| **Main clone** | the clone of that repository, on that machine, that worktrees are cut from | Local → `~/repos/fresh`, gpu-box → `~/src/fresh` |
| **Git mode** | what this launch does to git | new worktree, or work in the main clone |

- A repository has **at most one main clone per machine**.
- **Worktrees always go in Fresh's data directory**, as today:
  `<data dir>/orchestrator/<slug>/<name>` locally, and
  `~/.fresh/worktrees/<repo>/<name>` on a remote machine. There is no
  setting for this.
- **Folder** is the escape hatch: any directory, git or not. This is today's
  behaviour, unchanged.

The repository registry lives next to machines, in the editor's state store
(namespace `repositories`, one entry per repository, for the same reasons
machines are kept one per entry), never inside a working tree:

```
fresh
  remote          git@github.com:sinelaw/fresh.git     (optional; identifies the repo)
  clone new to    ~/src/<name>                         (suggested path when cloning)
  main clones
    local         ~/repos/fresh
    gpu-box       ~/src/fresh
```

The remote identifies a repository, so the same repo on two machines is one
entry with two main clones. A repository with no remote is allowed; it can't
be cloned, only pointed at.

## 4. Wireframes

### 4.1 New Workspace — default

```
┌─ New Workspace ─────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    ( New workspace )      Here · demo                                         │
│                                                                               │
│    PROMPT ────────────────────────────────────────────────────────────────    │
│                                                                               │
│    ╭─────────────────────────────────────────────────────────────────────╮    │
│    │ fix the flaky resize test in split_view.rs█                         │    │
│    │                                                                     │    │
│    │                                                                     │    │
│    ╰─────────────────────────────────────────────────────────────────────╯    │
│                                                                               │
│    AGENT ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│           Agent   [ claude ▾     ]     [ ] Auto mode     [✓] Teach Fresh CLI  │
│                                                                               │
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fresh ▾          ]     Machine  [ Local ▾        ]        │
│                   main clone ~/repos/fresh                                    │
│                   new worktree fresh-47, branched from origin/master          │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                   Launch in background     [   Launch   ]     │
│                                                                               │
│          Ctrl+⏎ launch    Alt+⏎ background    Esc close                      │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- Focus opens in the prompt. Enter adds a newline; Ctrl+Enter launches.
- **Mode switch** on the first row: `( New workspace )` / `Here · demo`.
  ←/→ or click flips it. The title follows (`New Workspace` / `Run Agent`).
  The dialog opens on whichever mode was used last, whatever opened it (§7).
- **Agent row**: the dropdown and that agent's switches on one line. A switch
  the agent doesn't support isn't drawn.
- **WHERE**: the Project and Machine dropdowns, then two lines saying which
  main clone will be used and what will happen to git.

### 4.2 Project dropdown

```
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fr█              ]     Machine  [ Local ▾        ]        │
│                   ╭──────────────────────────────────────────────╮            │
│                   │  ▸ fresh         github.com/sinelaw/fresh    │            │
│                   │    fresh-site    github.com/sinelaw/fresh-si…│            │
│                   │    dotfiles      local only                  │            │
│                   │  ──────────────────────────────────────────  │            │
│                   │    Folder…       any directory, git or not   │            │
│                   │    Manage repositories…                      │            │
│                   ╰──────────────────────────────────────────────╯            │
```

Repositories first, then `Folder…` and `Manage repositories…`. Typing
filters.

### 4.3 Machine dropdown

```
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fresh ▾          ]     Machine  [ █              ]        │
│                                        ╭──────────────────────────────────╮   │
│                                        │    Local      ✓ ~/repos/fresh    │   │
│                                        │  ▸ gpu-box    ✓ ~/src/fresh      │   │
│                                        │    build-01   no main clone      │   │
│                                        │    ml-cluster ✗ path missing     │   │
│                                        │  ──────────────────────────────  │   │
│                                        │    Other host…                   │   │
│                                        │    Kubernetes…                   │   │
│                                        │    Devcontainer                  │   │
│                                        │    Manage machines…              │   │
│                                        ╰──────────────────────────────────╯   │
```

Each machine shows **its main clone state for the selected repository**,
so you can see which machines are ready before you pick. `Other host…`,
`Kubernetes…` and `Devcontainer` work as today (§4.9).

### 4.4 Details (the `▹ Details` row)

```
┌─ New Workspace ─────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    ( New workspace )      Here · demo                                         │
│                                                                               │
│    PROMPT ────────────────────────────────────────────────────────────────    │
│                                                                               │
│    ╭─────────────────────────────────────────────────────────────────────╮    │
│    │ fix the flaky resize test in split_view.rs                          │    │
│    │                                                                     │    │
│    │                                                                     │    │
│    ╰─────────────────────────────────────────────────────────────────────╯    │
│                                                                               │
│    AGENT ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│           Agent   [ claude ▾     ]     [ ] Auto mode     [✓] Teach Fresh CLI  │
│                                                                               │
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fresh ▾          ]     Machine  [ gpu-box ▾      ]        │
│      Main clone   ~/src/fresh               ✓ master · clean                  │
│                   Change…   Open main clone                                   │
│                                                                               │
│    GIT ───────────────────────────────────────────────────────────────────    │
│                                                                               │
│            Mode   (•) New worktree     ( ) Work in the main clone             │
│     Branch from   [ origin/master                        ]                    │
│      New branch   [ fresh-47                             ]                    │
│       Workspace   [ fresh-47                             ]                    │
│                                                                               │
│                   worktree at ~/.fresh/worktrees/fresh/fresh-47 on gpu-box    │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                   Launch in background     [   Launch   ]     │
│                                                                               │
│          Ctrl+⏎ launch    Alt+⏎ background    Esc close                      │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- The same two dropdowns stay in place. Expanding adds the main clone row
  and the GIT section.
- `Change…` opens the Repositories dialog on this repository and machine
  (§4.12). `Open main clone` opens the main clone itself as a workspace.
- Git mode is a radio. With `Work in the main clone`, `Branch from` becomes
  `Check out` and `New branch` goes away.
- The open/closed state is remembered.

### 4.5 No main clone on this machine

```
┌─ New Workspace ─────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    ( New workspace )      Here · demo                                         │
│                                                                               │
│    PROMPT ────────────────────────────────────────────────────────────────    │
│                                                                               │
│    ╭─────────────────────────────────────────────────────────────────────╮    │
│    │ fix the flaky resize test in split_view.rs                          │    │
│    │                                                                     │    │
│    │                                                                     │    │
│    ╰─────────────────────────────────────────────────────────────────────╯    │
│                                                                               │
│    AGENT ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│           Agent   [ claude ▾     ]     [ ] Auto mode     [✓] Teach Fresh CLI  │
│                                                                               │
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fresh ▾          ]     Machine  [ build-01 ▾     ]        │
│                   ⚠ No main clone of fresh on build-01.                       │
│                     [ Use an existing clone… ]  or Launch to clone (asks)     │
│                   new worktree fresh-47, branched from origin/master          │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                   Launch in background     [   Launch   ]     │
│                                                                               │
│          Ctrl+⏎ launch    Alt+⏎ background    Esc close                      │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- The note is in the warning color. It offers two ways forward:
  - **`Use an existing clone…`** opens the Repositories dialog preselected on
    this repository and machine (§4.12).
  - **Press Launch** and you're asked to confirm a clone first (§4.6).
- A repository with no remote can't be cloned. The note is then
  error-colored (`✗ fresh has no remote; pick a clone on build-01`) and
  Launch is disabled until a main clone is set.

### 4.6 Launch asks before cloning

```
┌─ New Workspace ─────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    ( New workspace )      Here · demo                                         │
│                                                                               │
│    PROMPT ────────────────────────────────────────────────────────────────    │
│                                                                               │
│    ╭─────────────────────────────────────────────────────────────────────╮    │
│    │ fix the flaky resize test in split_view.rs                          │    │
│    │                                                                     │    │
│    │                                                                     │    │
│    ╰─────────────────────────────────────────────────────────────────────╯    │
│                                                                               │
│    AGENT ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│           Agent   [ claude ▾     ]     [ ] Auto mode     [✓] Teach Fresh CLI  │
│                                                                               │
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fresh ▾          ]     Machine  [ build-01 ▾     ]        │
│                   ⚠ No main clone of fresh on build-01.                       │
│                     [ Use an existing clone… ]  or Launch to clone (asks)     │
│                   new worktree fresh-47, branched from origin/master          │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│    ⚠ Clone before launching?                                                  │
│                                                                               │
│      git@github.com:sinelaw/fresh.git                                         │
│      → build-01 : ~/src/fresh      (becomes fresh's main clone there)         │
│                                                                               │
│                                        Cancel     [   Clone and launch   ]    │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- The confirmation replaces the footer. It names the URL, the machine and
  the path (the repository's `Clone new to`).
- `Clone and launch` clones, saves the path as the main clone for that
  machine, then cuts the worktree and launches. Progress shows in the
  launching view. `Cancel` (or Esc) returns to the form with nothing
  changed.
- `Launch in background` asks the same question.

### 4.7 Folder

```
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ Folder… ▾        ]     Machine  [ Local ▾        ]        │
│          Folder   [ ~/notes                              ]  Browse…           │
│                   plain folder · opens as is                                  │
│                                                                               │
│  ─ ─ ─ or, a git folder: ─ ─ ─                                                │
│                                                                               │
│          Folder   [ ~/work/fresh-copy                    ]  Browse…           │
│                   a clone of fresh · its main clone on Local is ~/repos/fresh │
│                   [ Make this the main clone ]   [ Switch to fresh ]          │
│                                                                               │
```

- Picking `Folder…` adds a Folder field (with `Browse…` on the selected
  machine) under the dropdowns. A non-git folder opens as is, with no GIT
  section.
- A git folder whose origin matches a known repository says so and offers
  `Make this the main clone` or `Switch to fresh`. A git folder with an
  unknown remote offers `Save as repository`. A git folder gets the GIT
  section (under `▹ Details`) as usual.

### 4.8 Remote machine typed by hand

```
│    WHERE ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│         Project   [ fresh ▾          ]     Machine  [ Other host… ▾  ]        │
│            Host   [ user@box:22                          ]                    │
│        Identity   [ ~/.ssh/id_ed25519                    ]  optional          │
│     SSH options   [ -J jump                              ]  optional          │
│                   [ ] Remember this machine as [ box          ]               │
│                                                                               │
│                   ⚠ No main clone of fresh on box.                            │
│                     [ Use an existing clone… ]  or Launch to clone (asks)     │
│                                                                               │
```

The connection fields for `Other host…` (and Target / Context / Namespace /
Pod for `Kubernetes…`) go directly under the dropdowns, followed by
`Remember this machine`. A saved machine or `~/.ssh/config` host shows no
connection rows.

### 4.9 Agent variants

```
│    AGENT ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│           Agent   [ custom… ▾    ]                                            │
│         Command   [ codex resume --last                  ]                    │
│                                                                               │
│  ─ ─ ─ or ─ ─ ─                                                               │
│                                                                               │
│           Agent   [ terminal ▾   ]     no prompt · opens a shell              │
│                                                                               │
```

`custom…` adds a Command row. `terminal` has no prompt: the prompt box stays
in place but is disabled, so nothing moves.

### 4.10 Run Agent (Here)

```
┌─ Run Agent ─────────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│      New workspace      ( Here · demo )                                       │
│                                                                               │
│    PROMPT ────────────────────────────────────────────────────────────────    │
│                                                                               │
│    ╭─────────────────────────────────────────────────────────────────────╮    │
│    │ █                                                                   │    │
│    │                                                                     │    │
│    │                                                                     │    │
│    ╰─────────────────────────────────────────────────────────────────────╯    │
│                                                                               │
│    AGENT ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│           Agent   [ claude ▾     ]     [ ] Auto mode     [✓] Teach Fresh CLI  │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                                               [   Run   ]     │
│                                                                               │
│                         Ctrl+⏎ run    Esc close                               │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

The same dialog with `Here` selected. There's no WHERE section. Switching
to `New workspace` adds it above the footer; the prompt and agent carry
over.

### 4.11 Repositories

Opened from the palette (`Orchestrator: Repositories`), the dock's `⋯` menu,
or `Manage repositories…` in the Project dropdown.

```
┌─ Repositories ──────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    [ / filter                             ]               + Add repository    │
│                                                                               │
│    ▸ fresh          github.com/sinelaw/fresh                                  │
│      fresh-site     github.com/sinelaw/fresh-site                             │
│      dotfiles       local only                                                │
│                                                                               │
│    FRESH ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│                   [ New workspace here ]   [ Remove… ]                        │
│                                                                               │
│          Remote   git@github.com:sinelaw/fresh.git                            │
│    Clone new to   [ ~/src/<name>                   ]                          │
│                                                                               │
│    MAIN CLONE ────────────────────────────────────────────────────────────    │
│                                                                               │
│         Machine   [ Local ▾             ]     + Add machine…                  │
│            Path   [ ~/repos/fresh                        ]  Browse…           │
│                   ✓ git repo · origin matches · master · clean                │
│                                                                               │
│                   Changes save as you make them.                              │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                                              [   Done   ]     │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- The top list is repositories. Below it are the selected repository's
  remote and `Clone new to` path.
- **MAIN CLONE** has one **Machine dropdown** and `+ Add machine…`, and the
  main clone **Path** for that machine with `Browse…`.
- The selected repository's own actions sit under its name:
  `New workspace here` (also `⏎` on a list row) opens New Workspace on it
  with the selected machine; `Remove…` asks in place, then removes it from
  the list (clones on disk untouched).
- **Nothing waits on a Save.** `Clone new to` is written as it is typed; the
  main clone Path is written once its check passes (a path still being typed,
  missing, or another repository's is left unsaved and the saved one stands).
  Clearing Path forgets that machine's main clone. The footer is one `Done`.

The Machine dropdown shows each machine's state for this repository:

```
│         Machine   [ █                   ]     + Add machine…                  │
│                   ╭──────────────────────────────────────╮                    │
│                   │    Local        ✓ ~/repos/fresh      │                    │
│                   │    gpu-box      ✓ ~/src/fresh        │                    │
│                   │  ▸ build-01     no main clone        │                    │
│                   │    ml-cluster   ✗ path missing       │                    │
│                   ╰──────────────────────────────────────╯                    │
```

**`+ Add machine…`** opens the existing Add Machine dialog. On Save it
returns here with the new machine selected and an empty Path.

### 4.12 Repositories, opened from New Workspace

`Use an existing clone…` and `Change…` open the same dialog with **the
repository selected, the Machine dropdown set to the launch dialog's
machine, and focus in Path**. The footer returns to New Workspace.

```
┌─ Repositories ──────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    [ / filter                             ]               + Add repository    │
│                                                                               │
│    ▸ fresh          github.com/sinelaw/fresh                                  │
│      fresh-site     github.com/sinelaw/fresh-site                             │
│      dotfiles       local only                                                │
│                                                                               │
│    FRESH ─────────────────────────────────────────────────────────────────    │
│                                                                               │
│          Remote   git@github.com:sinelaw/fresh.git                            │
│    Clone new to   [ ~/src/<name>                   ]                          │
│                                                                               │
│    MAIN CLONE ────────────────────────────────────────────────────────────    │
│                                                                               │
│         Machine   [ build-01 ▾          ]     + Add machine…                  │
│            Path   [ █                                    ]  Browse…           │
│                   ⚠ No main clone on build-01.                                │
│                     Type or browse to an existing clone, or [ Clone… ]        │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                                [ Back to New Workspace ]      │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- A Path that checks out is saved as it settles, as in §4.11;
  `Back to New Workspace` returns, and New Workspace then shows
  `main clone <path>`. The prompt and every other field are kept.
- `[ Clone… ]` fills Path with the `Clone new to` path and opens the clone
  confirmation (§4.15).

### 4.13 Browse…

```
│            Path   [ /opt/ci/                             ]  Browse…           │
│                   ╭─ build-01 : /opt/ci ─────────────────────────────╮        │
│                   │     ..                                           │        │
│                   │   ▸ fresh/                              git      │        │
│                   │     logs/                                        │        │
│                   │     tools/                                       │        │
│                   │                                                  │        │
│                   │   ⏎ open   Space select   Bksp up   Esc close    │        │
│                   ╰──────────────────────────────────────────────────╯        │
```

A directory browser **on the selected machine** (the local filesystem, or
the machine's connection for ssh / k8s). It starts at the current path or
the machine's home. It reuses the editor's folder-select browser (the one
behind Switch Project), rooted on that machine's filesystem. Folders that
contain `.git` are tagged `git` in the listing; there is no search.

### 4.14 Checking the path you gave

```
│            Path   [ /opt/ci/fresh                        ]  Browse…           │
│                   ✓ git repo · origin matches · master · clean                │
│                                                                               │
│            Path   [ /opt/ci/other                        ]  Browse…           │
│                   ✗ git repo, but origin is github.com/acme/other             │
│                                                                               │
│            Path   [ /opt/ci                              ]  Browse…           │
│                   ✗ not a git repo                                            │
│                                                                               │
│            Path   [ /opt/ci/fresh-new                    ]  Browse…           │
│                   ⚠ doesn't exist on build-01.   [ Clone here… ]              │
```

- An existing path must be a git repo whose origin matches the repository's
  remote. Otherwise it's `✗` and `Use as main clone` is disabled.
- A path that doesn't exist is `⚠`, **never cloned automatically**. It
  offers `Clone here…`, and `Use as main clone` stays disabled until the
  clone finishes.

### 4.15 Clone confirmation, progress, done

```
│            Path   [ /opt/ci/fresh-new                    ]  Browse…           │
│                   ⚠ Clone git@github.com:sinelaw/fresh.git                    │
│                     into build-01 : /opt/ci/fresh-new ?                       │
│                                                 Cancel   [  Clone  ]          │
│                                                                               │
│            Path   [ /opt/ci/fresh-new                    ]  Browse…           │
│                   cloning…  Receiving objects  43%            Cancel          │
│                                                                               │
│            Path   [ /opt/ci/fresh-new                    ]  Browse…           │
│                   ✓ cloned · master · clean                                   │
```

The confirmation names the URL, machine and path, and the clone only starts
on `Clone`. Progress is inline and can be cancelled (a partial directory is
removed). When it's done, the path is a valid main clone and
`Use as main clone` is enabled.

### 4.16 Add Repository

```
┌─ Add Repository ────────────────────────────────────────────────────────── × ─┐
│                                                                               │
│    SOURCE ────────────────────────────────────────────────────────────────    │
│                                                                               │
│     URL or path   [ git@github.com:sinelaw/fresh.git█          ]              │
│                   ✓ reachable · default branch master                         │
│                   [ Browse for a local clone… ]                               │
│                                                                               │
│            Name   [ fresh                                      ]              │
│    Clone new to   [ ~/src/<name>                               ]              │
│                                                                               │
│    MAIN CLONE ────────────────────────────────────────────────────────────    │
│                                                                               │
│         Machine   [ Local ▾             ]     + Add machine…                  │
│            Path   [ ~/repos/fresh                        ]  Browse…           │
│                   ✓ git repo · origin matches · master · clean                │
│                   Other machines can be set later in Repositories.            │
│                                                                               │
│    ───────────────────────────────────────────────────────────────────────    │
│                                                                               │
│                                                  Cancel     [   Save   ]      │
│                                                                               │
└───────────────────────────────────────────────────────────────────────────────┘
```

- **Paste a URL:** checked with `git ls-remote`; the name comes from the URL.
- **A local clone:** type its path, or `Browse for a local clone…` (this
  machine's folders; `⏎` on a git folder picks it). A folder inside a clone
  resolves to the clone's top level. Its `origin` is shown and, by default,
  becomes the remote — `[v] Use its origin as the remote (…)` unticked keeps
  the repository local only (as does a clone with no origin). The name comes
  from the origin, else the folder, and the clone fills in the Local main
  clone.
- MAIN CLONE works as in §4.11–4.15: a Machine dropdown with
  `+ Add machine…`, a Path with `Browse…`, a check of that path only, and
  `Clone here…` with confirmation for a path that doesn't exist.

## 5. Parity with today

| Today | Redesign |
|---|---|
| Launch in: Current / New | mode switch, first row; opens on the last-used mode |
| Start prompt (single line, optional, 7th control) | PROMPT box, multi-line, initial focus |
| Agent dropdown, custom… → Command | AGENT row; Command row when custom |
| Auto mode / Teach Fresh CLI (per agent) | same switches, on the agent row |
| Machine dropdown (Local, saved, ssh-config hosts, Other host…, Kubernetes…, Devcontainer) | Machine dropdown in WHERE, with each machine's main-clone state |
| SSH target / identity / options; k8s target / context / namespace / pod | under the dropdowns when `Other host…` / `Kubernetes…` (§4.8) |
| Remember this machine as … | same, under the connection fields |
| Project Path on a git repo + worktree | Project = repository + Machine, main clone shown; or Project = Folder |
| Project Path on a non-git dir | Project = Folder, plain |
| Workspace Name | Workspace, under `▹ Details`; also shown in the summary line |
| Create a git worktree toggle | Git mode radio: New worktree / Work in the main clone |
| Checkout branch / New branch name | Branch from / New branch (placeholder shows the planned name) |
| Branch-plan and worktree-path notes | the summary line under WHERE; the worktree path line in GIT |
| Worktrees under the data dir | unchanged, always |
| Existing linked worktree → attach | Folder on its path; the note names its main clone; Git defaults to working in the folder |
| Remote git probe states (probing / unreachable / untrusted / non-git) | status line under the main clone / Folder |
| Create / Create in Background / Cancel | Launch / Launch in background / Esc and × |
| Discovered-session prefill | Folder prefilled (unchanged) |
| `Machines ▸ New workspace here` | Machine preselected; the last project used on it |
| Compact fallback for short terminals | §2.1: drop spacing, then shrink the prompt; sections scroll |

## 6. Implementation notes

- **Launch form** (`orchestrator.ts`): replace `buildFormSpecFixed` /
  `buildFormSpecCompact` / `padRows` / `connectionRowsMax` / `tailRowsMax`
  with one builder that emits mode switch, PROMPT, AGENT, WHERE, optional
  GIT, footer. This deletes the row-reservation machinery.
- Mode switch: `radio` in a `row`. Prompt: `textArea({rows: 3})`, initial
  focus. The `Enter` shim (`orchestrator_form_key_enter`) forwards a newline
  when the prompt has focus. Add `M-Enter` → launch in background, and
  a focusable `▹ Details` / `▿ Hide details` row toggles the fold. (No key
  chord: the editor's keymaps already spend every Alt+letter.)
- `form.projectPath` becomes `form.project`: `{kind: "repo", id}` or
  `{kind: "folder", path}`. The existing probes run against the main clone
  (repo) or the folder path.
- **Repository registry**: a `repositories` state-store namespace beside
  `machines`, with
  `loadRepositories` / `upsertRepository` mirroring the machine helpers.
  Main-clone validation reuses `pathIsInsideGitWorkTree`, `probeRemoteGit`
  and an origin-URL compare.
- **Clone**: `git clone <remote> <path>`, locally or over the machine's
  authority, only after a confirmation. It is cancellable, and a partial
  directory is removed.
- **Repositories dialog**: a new floating panel like the Machines dialog.
  `+ Add machine…` calls the existing Add Machine flow with a return
  callback. `Browse…` needs the host's folder-select browser exposed to
  plugins, rooted on a given authority.
- Tests that pin rows (`tests/e2e/plugins/orchestrator_new_dialog.rs` and
  the other `orchestrator_*` tests) need their screen assertions updated.
  Submit paths and probes are unchanged.

### 6.1 As built — where it differs from the wireframes

- **Machine dropdown labels.** A dropdown cell is 20 columns wide, so the
  per-machine clone state is a glyph (`✓ Local`, `· build-01`) in the
  Repositories dialog; the New Workspace Machine dropdown shows plain names
  and the WHERE lines say what the chosen machine has.
- **The details fold** is a focusable `▹ Details` / `▿ Hide details` row
  under WHERE (hollow triangles, since the filled `▸` is the focus marker);
  the Workspace field sits in WHERE while it is open, the GIT section below.
- **The dialog stays centred** and grows when details open; the prompt,
  agent and footer keep their order but not their absolute rows.
- **Focus.** The `▸` marker now sits next to the focused control's label
  (it used to sit at the panel's left edge), the focus band starts at the
  label rather than column 0, and link-styled buttons carry the marker too.
- **Keys.** Enter in the prompt is a newline; Ctrl+Enter launches from
  anywhere, Alt+Enter launches in the background; Esc closes an open list,
  then a pending clone question, then the dialog.

## 7. Decisions

1. **Mode opens on whatever was used last.** `+ New`, Alt+N, `New Workspace`
   and `Run Agent…` all open the same dialog on the last-used mode
   (`orchestrator.last_launch_mode`, a global setting). The two palette
   entries still exist and differ only in their name.
2. **No per-repository defaults.** A repository is a remote, a
   `Clone new to` path and its main clones, nothing more. Agent, git mode
   and branch names always come from the launch dialog.
3. **Browse works on every machine kind, including Kubernetes**, even though
   listing a pod (`kubectl exec … ls`) is slow. The browser shows
   `loading…` in place of the listing while it waits; typing in Path stays
   available the whole time.
