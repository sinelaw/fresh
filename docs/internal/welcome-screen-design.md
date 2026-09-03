# Welcome Screen — a ladder, not a launcher

> _Design note. Status: **PARTLY IMPLEMENTED** — `plugins/welcome_screen.ts`
> ships the ladder, the three doors, the jump keys, foldable cards, the live
> finder, the live theme picker, the real git and Orchestrator cards, and the
> startup toggle. §13 records what the build taught, including the three
> places the wireframes above are still aspirational. AI-generated from the
> source; where it disagrees with the code, the code is authoritative._

Fresh replaces the `[No Name]` scratch buffer and the blank placeholder hint
with an **interactive welcome buffer**: a scrollable document, rendered in the
editor's own idiom, that onboards three very different audiences without
overwhelming the simplest one.

The screen's whole argument is one sentence, and it is the product's argument
too: **it starts as simple as nano, and scales to mission control — only when
you ask it to.** The first viewport mentions no LSP, no git, no worktrees and
no agents. You reach those by scrolling, which is exactly the gesture that
says "show me more."

---

## 1. What this replaces

Three empty states exist today, and none is a starting point:

1. **`[No Name]` scratch buffer** (`editor.auto_create_empty_buffer_on_last_buffer_close = true`,
   the default) — a text cursor in a buffer that will never be saved.
2. **The blank pane** (same setting, `false`) — a `synthetic_placeholder`
   buffer, hidden from tabs, painting one inert centred line:
   `Ctrl+P  command palette · Ctrl+O  open file · Ctrl+E  file explorer`.
3. **The Dashboard plugin** — opt-in, off by default, and an *information*
   surface (git, disk, weather, PRs) rather than an onboarding one.

All three answer a question nobody asked. None of them tells a first-time user
that the thing they just installed also runs four coding agents across four
worktrees — and none of them reassures a nervous one that `Ctrl+S` still
saves.

---

## 2. The shape: progressive disclosure as a ladder

Three audiences, one screen, ordered by sophistication:

| Rung | Audience | What they need to see |
|---|---|---|
| **First viewport** | everyone | logo, one line, three doors, four verbs, one promise |
| **Level 1 · Just edit** | "I want to edit a file" | recent files, big-file handling, `$EDITOR` setup |
| **Level 2 · It's a project now** | "I expect a real IDE" | LSP, hunk-level git review, themes, power tools |
| **Level 3 · Run the whole shop** | "I orchestrate agents" | the Orchestrator dock, worktrees, remotes, daemon |

Three affordances keep the ladder navigable rather than merely long:

- **Three numbered path cards** on the first screen. Click one, or press
  `1` / `2` / `3`, and the buffer scrolls to that level. Nobody has to
  discover the depth by scrolling blindly.
- **A depth meter in the status bar** — `editor → IDE → orchestrator` — that
  lights the segment you are in as you scroll, and highlights the matching
  path card. It is a "you are here" for a document whose whole structure is
  depth.
- **Fold arrows in the gutter.** Every card folds to a single line. A user who
  does not care about git folds that card and it stays folded.

### Show, don't list

Every major feature is a **small live demo**, not a bullet:

| Card | What is actually live |
|---|---|
| Pick up where you left off | a real `TextInput`; typing really fuzzy-finds, `Enter` really opens |
| Language smarts | a real embedded editor view: real grammar, real hover popup, real diagnostic with its real code action |
| Review your diff | stage / unstage really run; the file counts on the left really move |
| Make it yours | the theme buttons restyle the entire editor, live, for real |
| The Orchestrator dock | the dock's own widget list; arrowing it swaps the transcript beside it |

Two things fall out of this that a bullet list can never do. Every
interaction teaches a real keybinding in passing (the finder card is where you
learn `Ctrl+P`, not a line that says "press `Ctrl+P`"). And the screen cannot
lie about the product, because it *is* the product: if the hover popup is
ugly, the welcome screen is ugly.

---

## 3. Why a buffer

The welcome screen is a **virtual buffer with a tab**, called `Welcome`. Not a
modal, not a dock panel, not a hidden placeholder. That is the most
Fresh-native answer available, and it earns a surprising amount for free:

- **It scrolls,** because buffers scroll. The status bar's scroll readout
  (`top` / `58%` / `bot`) is the reader's progress bar through the ladder.
- **It ends in `~` tildes,** because that is how every file in Fresh ends. The
  page finishes the way the editor finishes.
- **It has a gutter,** which is where the fold arrows live — the same
  affordance as folding code.
- **It is stripped from workspace serialization,** like every virtual buffer,
  so it can never come back as a stale tab in a restored session.
- **It closes like a tab,** because it is one. No bespoke dismiss gesture.

And it makes the mock honest: what the wireframes below draw is a Fresh
buffer, drawn by Fresh's own renderer, in the user's own theme.

---

## 4. The primitives this rides on

This concept looks expensive. It is not: every capability it needs already
ships. This section is the feasibility spine — the reason the build order in
§10 starts at "assemble" rather than "invent".

| Need | Existing primitive |
|---|---|
| A tabbed, read-only, plugin-owned page | `createVirtualBuffer({ name, mode, readOnly, showLineNumbers:false })` — the Dashboard pattern |
| Interactive controls **inside** that page | `mountWidgetPanel(panelId, bufferId, spec)` — "mount a declarative widget panel inside a virtual buffer". It renders the spec *into the buffer's content* via `set_virtual_buffer_content` and maps widget focus onto a real buffer cursor. `search_replace.ts` already ships a buffer-mounted panel with live text inputs. |
| Buttons, lists, trees, text inputs, toggles, dropdowns | the widget library (`button`, `list`, `tree`, `textInput`, `toggle`, `divider`, `hintBar`, …) — the same one the Orchestrator dock and the tour panel are built from |
| A **real editor view** embedded in a card | `windowEmbed({ windowId, rows })` — reserves a rectangle the host paints the live window UI into: split tree, terminals, syntax highlighting, decorations |
| Syntax-highlighted fenced code in a card | `Text` widgets with `markdown: true` render through the shared markdown engine **with the grammar registry** attached |
| Vertical scrolling, `~` filler, scroll position | the buffer's own; widget panels pin *horizontal* scroll and leave vertical alone |
| The depth meter | a status-bar `CustomToken` element plus the per-buffer `status_bar_values` map |
| Keyboard model for a cursorless page | `defineMode(name, bindings, readOnly, allowTextInput, inheritNormalBindings)` — the Dashboard's `j`/`k`/`Tab`/`Enter` idiom |
| Mouse | the `mouse_click` hook with per-row registered column ranges — the Dashboard's hit-testing pattern |
| Not a plugin dependency | the widget runtime is **deliberately not gated** behind the `plugins` cargo feature, so core can mount panels in a plugin-less build |

Two genuinely new pieces are needed, both small and both useful beyond this
screen:

1. **A `{scroll}` status-bar element** (`top` / `NN%` / `bot`). The status bar
   has no scroll readout today. Every long buffer wants one.
2. **Scroll-position → depth-meter plumbing**: the welcome buffer needs to
   know which level banner is on screen. This is a viewport-row comparison
   against known banner rows, recomputed on scroll.

---

## 5. The screen

**1 — First viewport.** The zero-anxiety zone: logo, one line, three doors, four verbs, one promise. Nothing here mentions LSP, git, worktrees or agents.

```text
 ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │  >_  File   Edit   View   Selection   Go   LSP   Help                                                                │
 │  Welcome ×  │  Ctrl+P to open a file…                                                                                │   <- a real, closable tab — and a ghost tab that teaches Ctrl+P
 │               ███████╗██████╗ ███████╗███████╗██╗  ██╗                                                               │
 │               ██╔════╝██╔══██╗██╔════╝██╔════╝██║  ██║                                                               │
 │               █████╗  ██████╔╝█████╗  ███████╗███████║                                                               │
 │               ██╔══╝  ██╔══██╗██╔══╝  ╚════██║██╔══██║                                                               │
 │               ██║     ██║  ██║███████╗███████║██║  ██║                                                               │
 │               ╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝                                                               │
 │                                                                                                                      │
 │               A terminal text editor and IDE.  It grows when your work does.                                         │   <- one line of positioning. no feature list, no bullets.
 │               v0.4.10  ·  single static binary  ·  open source                                                       │
 │                                                                                                                      │
 │               ── WHAT BRINGS YOU HERE? ──                                                                            │   <- three doors, sized like doors
 │                                                                                                                      │
 │               ┌────────────────────────────┐ ┌────────────────────────────┐ ┌────────────────────────────┐           │   <- 1 / 2 / 3 jump straight down; the status bar tracks which door you took
 │               │ [1] JUST EDIT TEXT         │ │ [2] CLASSIC IDE            │ │ [3] ORCHESTRATE            │           │
 │               │ Open a file & go           │ │ Code with LSP & git        │ │ Run agents in parallel     │           │
 │               │ Notes, configs, huge logs. │ │ Completions, goto & hover, │ │ One workspace per worktree │           │
 │               │ Standard keys, full mouse  │ │ hunk-level diff review,    │ │ — claude, codex, aider and │           │
 │               │ — nothing to learn first.  │ │ splits, themes, plugins.   │ │ remotes. Tour the diffs.   │           │
 │               │                            │ │                            │ │                            │           │
 │               │ jump ↓  ·  or press 1      │ │ jump ↓  ·  or press 2      │ │ jump ↓  ·  or press 3      │           │
 │               └────────────────────────────┘ └────────────────────────────┘ └────────────────────────────┘           │
 │                                                                                                                      │
 │               ▸ Open file                                      Ctrl+O                                                │
 │               ▸ Find a recent file                             Ctrl+P                                                │   <- the plain verbs, for anyone who already knows what they want
 │               ▸ New buffer                                     Ctrl+N                                                │
 │                                                                                                                      │
 │               ┌──────────────────────────────────────────────────────────────────────────────────────┐               │
 │               │ Nothing to learn first.  It works like you'd expect:  Ctrl+S saves,  Ctrl+Z undoes,  │               │   <- reassurance BEFORE capability — this is the anxiety valve
 │               │ Ctrl+F finds,  Ctrl+C/V copy-paste — and the mouse just works.  Click, drag, select. │               │
 │               └──────────────────────────────────────────────────────────────────────────────────────┘               │
 │                                       ▼ scroll — the rest is here when you need it ▼                                 │
 │  Welcome   [editor]  →  IDE  →  orchestrator   Palette: Ctrl+P               LF · UTF-8   Tokyo Night   14:32   top  │
 └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**2 — Level 1 · Just edit.** Everyday editing. The finder is a live widget, not a picture of one; the second card is folded to show that any card can be dismissed.

```text
 ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │  >_  File   Edit   View   Selection   Go   LSP   Help                                                                │
 │  Welcome ×  │  Ctrl+P to open a file…                                                                                │
 │               ──── LEVEL 1 · JUST EDIT ──────────────────────────────────────────────────────────────────            │   <- the level banner. scrolling IS the disclosure.
 │               Open a file. Type. Save. Fresh stays out of the way.                                                   │
 │                                                                                                                      │
 │           ▾   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │   <- gutter fold arrows — the same affordance as folding code
 │               │ Pick up where you left off                                 this box is live — type in it │           │
 │               ├──────────────────────────────────────────────────────────────────────────────────────────┤           │
 │               │  ┌ fuzzy find ────────────────────────────────────────────────────────────┐              │           │
 │               │  │ cfg█                                                                   │              │           │   <- a real TextInput widget. the fuzzy finder actually runs.
 │               │  └────────────────────────────────────────────────────────────────────────┘              │           │
 │               │                                                                                          │           │
 │               │  ▸ ./config.toml                                                   1 h ago               │           │   <- Enter opens it. the demo IS the feature.
 │               │    src/store.rs                                                  14 min ago              │           │
 │               │    deploy@prod:/etc/nginx/nginx.conf                               yesterday             │           │
 │               │                                                                                          │           │
 │               │  Fresh remembers your cursor position in every file.  Hot Exit restores                  │           │
 │               │  unsaved buffers after a crash — even unnamed scratch ones.                              │           │
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │                                                                                                                      │
 │           ▸   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │   <- a folded card: one line until you want it
 │               │ Built for the ugly files too                                    folded — click ▸ to open │           │
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │                                                                                                                      │
 │           ▾   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │
 │               │ Make it your $EDITOR                                        quality-of-life from day one │           │
 │               ├──────────────────────────────────────────────────────────────────────────────────────────┤           │
 │               │  # Use Fresh for commit messages and rebases                                             │           │   <- fenced code, highlighted by the real grammar engine
 │               │  git config --global core.editor "fresh --wait"                                          │           │
 │               │                                                                                          │           │
 │               │  # Keep a project session alive across terminal disconnects                              │           │
 │               │  fresh -a myproject                                                                      │           │
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │  Welcome   [editor]  →  IDE  →  orchestrator   Palette: Ctrl+P               LF · UTF-8   Tokyo Night   14:32   31%  │
 └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**3 — Level 2 · It's a project now.** IDE features, each one demonstrated rather than listed. The code pane is a real embedded editor view; the git pane really stages.

```text
 ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │  >_  File   Edit   View   Selection   Go   LSP   Help                                                                │
 │  Welcome ×  │  Ctrl+P to open a file…                                                                                │
 │               ──── LEVEL 2 · IT'S A PROJECT NOW ─────────────────────────────────────────────────────────            │
 │               Language servers, git review, themes — here the whole time, waiting.                                   │
 │                                                                                                                      │
 │           ▾   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │
 │               │ Language smarts, zero setup                                hover the dotted & wavy words │           │
 │               ├──────────────────────────────────────────────────────────────────────────────────────────┤           │
 │               │   29 │ pub struct UserStore {                                                            │           │   <- a real editor view, embedded. real grammar, real gutter.
 │               │        ┄┄┄┄┄┄┄┄┄                                                                         │           │
 │               │        ┌────────────────────────────────────────────────┐                                │           │   <- the editor's own hover popup, not a drawing of one
 │               │   30 │ │ struct UserStore                               │                                │           │
 │               │   31 │ │   users: HashMap<u64, User>                    │                                │           │
 │               │   42 │ │ Owns all users by id.                          │ -> impl Iterator {             │           │
 │               │   43 │ │ F12 goto definition · Shift+F12 references     │ .is_actve)                     │           │
 │               │        └────────────────────────────────────────────────┘ ~~~~~~~~                       │           │
 │               │   44 │     }                                                                             │           │
 │               │  ⚠ unknown field `is_actve` — a field with a similar name exists: `is_active`            │           │   <- a real diagnostic, carrying its real code action
 │               │    Code action:  Ctrl+.  →  rename to is_active                                          │           │
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │                                                                                                                      │
 │           ▾   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │
 │               │ Review your diff before it reviews you                 the stage buttons work — try them │           │
 │               ├──────────────────────────────────────────────────────────────────────────────────────────┤           │
 │               │  STAGED (1)               │ @@ src/store.rs · 42–44   staged ✓ [unstage] [discard]       │           │   <- these buttons run. the counts on the left really move.
 │               │   M src/store.rs          │      pub fn active_users(&self) …                            │           │
 │               │                           │ -        self.users.values()                                 │           │
 │               │  UNSTAGED (1)             │ +        self.users.values().filter(|u| u.is_active)         │           │
 │               │   M src/main.rs           │                                                              │           │   <- hunk-level review, exactly as you'd use it on a live repo
 │               │                           │ @@ src/store.rs · 61–62   unstaged [stage] [discard]         │           │
 │               │  UNTRACKED (1)            │      impl UserStore {                                        │           │
 │               │   ? notes/todo.md         │ +    pub fn len(&self) -> usize { self.users.len() }         │           │
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │  Welcome   editor  →  [IDE]  →  orchestrator   Palette: Ctrl+P               LF · UTF-8   Tokyo Night   14:32   58%  │
 └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**4 — Level 3 · Run the whole shop.** Agent orchestration. Clicking or arrowing the dock swaps the transcript beside it — because it is the dock, embedded.

```text
 ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │  >_  File   Edit   View   Selection   Go   LSP   Help                                                                │
 │  Welcome ×  │  Ctrl+P to open a file…                                                                                │
 │               ──── LEVEL 3 · RUN THE WHOLE SHOP ─────────────────────────────────────────────────────────            │
 │               One workspace per git worktree. An agent in each. Hop with an arrow key.                               │
 │                                                                                                                      │
 │           ▾   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │
 │               │ The Orchestrator dock                                 click a workspace, or ↑ ↓ the list │           │   <- the last rung. this card is why the ladder exists.
 │               ├──────────────────────────────────────────────────────────────────────────────────────────┤           │
 │               │  WORKSPACES              │ branch fix/login-bug · +124 −18 · PR #241 · CI running        │           │   <- the dock's own widget list — the same one Alt+O gives you
 │               │  ▸ fix/login-bug      ●  │ ┌────────────────────────────────────────────────────┐        │           │   <- arrowing the list swaps the transcript. it IS the live dock.
 │               │    feat/i18n-ko       ◐  │ │ worktree ~/w/fix-login-bug                         │        │           │
 │               │    chore/deps         ✓  │ │ claude ▸ Reproduced the race in session refresh.   │        │           │
 │               │    deploy@prod        ⇅  │ │ claude ▸ Patched token rotation; 3 files changed.  │        │           │
 │               │                          │ │ claude ▸ Running the auth test suite…              │        │           │   <- a real terminal, embedded. that cursor really blinks.
 │               │  + add workspace         │ │ tests: 41 passed, 2 running █                      │        │           │
 │               │    (cuts a worktree      │ └────────────────────────────────────────────────────┘        │           │
 │               │     and a branch)        │                                                               │           │
 │               │                          │ ● working   ◐ waiting on you   ✓ done   ⇅ remote              │           │   <- the glyph legend, once, where the glyphs are
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │                                                                                                                      │
 │           ▾   ┌──────────────────────────────────────────────────────────────────────────────────────────┐           │
 │               │ Your other machines are workspaces too                           SSH + detachable daemon │           │
 │               ├──────────────────────────────────────────────────────────────────────────────────────────┤           │
 │               │                                                                                          │           │
 │               │  # Edit nginx config on prod — saves transfer only the patch                             │           │
 │               │  fresh deploy@prod:/etc/nginx/nginx.conf                                                 │           │   <- your other machines, on the same ladder
 │               │                                                                                          │           │
 │               │  # Open a file in an already-running daemon                                              │           │
 │               │  fresh --cmd daemon open-file myproject src/main.rs:42                                   │           │
 │               │                                                                                          │           │
 │               └──────────────────────────────────────────────────────────────────────────────────────────┘           │
 │                                                                                                                      │
 │                                                                                                                      │
 │  Welcome   editor  →  IDE  →  [orchestrator]   Palette: Ctrl+P               LF · UTF-8   Tokyo Night   14:32   84%  │
 └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**5 — The end of the buffer.** Links and the closing line. Below it, the buffer's own `~` filler: the page ends the way every file in Fresh ends.

```text
 ┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
 │  >_  File   Edit   View   Selection   Go   LSP   Help                                                                │
 │  Welcome ×  │  Ctrl+P to open a file…                                                                                │
 │                                                                                                                      │
 │               ──────────────────────────────────────────────────────────────────────────────────────────             │
 │                                                                                                                      │
 │               That's the whole ladder.  Most days you'll live on rung one — the rest keeps up                        │   <- the last line grants permission to stay on rung one
 │               when you climb.                                                                                        │
 │                                                                                                                      │
 │               Docs      Keybindings      Plugin registry      GitHub      Discord                                    │
 │                                                                                                                      │
 │               [x] Show this screen on startup                                                                        │   <- the screen knows when to get out of the way
 │                                                                                                                      │
 │           ~                                                                                                          │   <- the buffer's own end-of-file tildes. it really is a buffer.
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │           ~                                                                                                          │
 │  Welcome   editor  →  IDE  →  [orchestrator]   Palette: Ctrl+P               LF · UTF-8   Tokyo Night   14:32   bot  │
 └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
```

**6 — A narrow pane (62×34).** One column, cards stacked, the depth meter abbreviated. Below ~46 columns the screen falls back to the plain hint line.

```text
 ┌────────────────────────────────────────────────────────────┐
 │  >_ File Edit View …                                       │
 │  Welcome ×                                                 │
 │     ███████╗██████╗ ███████╗███████╗██╗  ██╗               │
 │     ██╔════╝██╔══██╗██╔════╝██╔════╝██║  ██║               │
 │     █████╗  ██████╔╝█████╗  ███████╗███████║               │
 │     ██╔══╝  ██╔══██╗██╔══╝  ╚════██║██╔══██║               │
 │     ██║     ██║  ██║███████╗███████║██║  ██║               │
 │     ╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝               │
 │                                                            │
 │     It grows when your work does.                          │
 │                                                            │
 │     ┌──────────────────────────────────────────────────┐   │   <- cards stack. the ascii art is the last thing dropped.
 │     │ [1] JUST EDIT TEXT                               │   │
 │     │ Open a file & go                                 │   │
 │     │ Notes, configs, huge logs.                       │   │
 │     │ jump ↓  ·  press 1                               │   │
 │     └──────────────────────────────────────────────────┘   │
 │                                                            │
 │     ┌──────────────────────────────────────────────────┐   │
 │     │ [2] CLASSIC IDE                                  │   │
 │     │ Code with LSP & git                              │   │
 │     │ Completions, git, themes.                        │   │
 │     │ jump ↓  ·  press 2                               │   │
 │     └──────────────────────────────────────────────────┘   │
 │                                                            │
 │     ┌──────────────────────────────────────────────────┐   │
 │     │ [3] ORCHESTRATE                                  │   │
 │     │ Agents in parallel                               │   │
 │     │ Worktrees, remotes, dock.                        │   │
 │     │ jump ↓  ·  press 3                               │   │
 │     └──────────────────────────────────────────────────┘   │
 │                                                            │
 │  [editor] → IDE → orch                         14:32  top  │
 └────────────────────────────────────────────────────────────┘
```
---

## 6. Copy

Three rules, in priority order.

**Reassure first, impress later.** The first viewport's job is to lower the
pulse of someone who typed `fresh notes.txt` and got an IDE. Hence the
reassurance card — `Ctrl+S` saves, `Ctrl+Z` undoes, the mouse works — sitting
*above the fold*, and hence the deliberate absence of the words LSP, git,
worktree and agent from that first screen.

**Editor voice: plain, confident, no exclamation marks.** "Open a file. Type.
Save. Fresh stays out of the way." Not "Welcome to Fresh! 🎉". The product is
a terminal editor; the copy sounds like one.

**The last line grants permission to stop climbing.** "That's the whole
ladder. Most days you'll live on rung one — the rest keeps up when you climb."
A screen that shows off three levels of power must end by saying that using
one of them is the normal outcome, or it reads as a demand.

A note on the level names. `JUST EDIT` / `IT'S A PROJECT NOW` / `RUN THE WHOLE
SHOP` are deliberately in the user's voice about their own situation, not in
the product's voice about its feature tiers. "Level 2: IDE Features" would be
a manual; "It's a project now" is a thing that happens to you.

---

## 7. Interaction

| Input | Effect |
|---|---|
| `1` / `2` / `3` | scroll to that level; the path card and depth meter update |
| `0` | back to the top |
| scroll / `PgUp` / `PgDn` / mouse wheel | ordinary buffer scrolling; the depth meter follows |
| `Tab` / `Shift+Tab` | move focus between interactive widgets |
| `Enter` / `Space` | activate the focused widget |
| click on a fold arrow, or `za` on its row | fold / unfold that card |
| typing, while the finder is focused | it is a real text input; it really searches |
| `Ctrl+P`, `F1`, `Alt+O`, the menu bar | all work — this is a normal pane, not a modal |
| `Ctrl+W` / the tab's `×` | close it, like any tab |

**Focus.** A `welcome` mode (via `defineMode`) with `inheritNormalBindings:
false`: on a cursorless page, every key is either bound here or intentionally
inert. When a `TextInput` is focused the widget runtime flips `show_cursors`
on and puts a real cursor in it — the same path the search/replace panel uses.

**Reveal on scroll.** Cards below the fold fade in as they enter the viewport,
so the first screen stays calm and the ladder feels like descent rather than a
wall of content. Fresh already has a frame-buffer animation layer
(`editor.animations`) and already gates on it; when animations are off, or the
terminal is slow, cards simply appear. This is decoration and it is the first
thing to go.

**Mouse.** Everything clickable is a widget with a registered hit region;
hovering a row underlines it. Rows that are prose are not clickable, so the
underline stays an honest affordance.

---

## 8. Lifecycle and configuration

> _This section was written for the Rust-core design. The shipped
> implementation needs no host change, so it adds no core setting: the
> welcome screen is gated by its own plugin config, and the two older empty
> states stay governed by the core boolean that already governed them. What
> ships is described below; the original single-key proposal is kept after
> it as the road not taken._

```jsonc
{
  "plugins": {
    "welcome_screen": {
      // false removes the plugin entirely, including the palette command
      "enabled": true,
      "settings": {
        // false: never open on its own; `Welcome` in the palette still works
        "showOnStartup": true
      }
    }
  },
  "editor": {
    // what you fall back to once the welcome screen stands down
    //   true  — the historical [No Name] scratch buffer
    //   false — the blank pane with the one-line hint
    "auto_create_empty_buffer_on_last_buffer_close": true
  }
}
```

`showOnStartup` governs the one automatic path: launch. Not "launch with
nothing to restore" — the page opens either way, and what an already-busy
workspace changes is only whether it comes to the front. Emptying the
workspace later is not a path at all; see "When it opens" below, so the two
older empty states stay exactly as they were.
The startup toggle on the page writes a plugin *global state* override that
wins over the config field, so flipping it in the UI persists without
rewriting `config.json`; the config field is what the Settings UI edits and
the fallback when no override has been set.

All three empty states therefore remain reachable, and turning the welcome
screen off restores exactly the previous behaviour rather than a new one.

**The road not taken.** The core design proposed folding all three into one
tri-state `editor.empty_workspace_screen` (`"welcome"` / `"empty_buffer"` /
`"blank"`), migrating from the boolean by treating an explicitly-set
`Option<bool>` as the old value. That is still the tidier surface, and worth
doing if the screen ever moves into the core. It is deliberately not done
here: adding a core setting to gate a plugin would be the only host change in
an otherwise plugin-only feature, and the boolean it would deprecate is
already load-bearing across the test suite.

**When it opens.**

- At startup, always — as the foreground buffer when the session restore
  produced nothing, and as a tab beside the work when it produced anything.
- On demand: `Welcome` in the palette, or **Help ▸ Welcome**.

**It never takes the view and gives it back.** A background open is a
`background: true` on `createVirtualBuffer`, not an open followed by a switch
back. Creation used to make the new buffer active unconditionally, so the
only way to leave a restored session alone was to switch away afterwards —
and that is two visible switches: the tab bar came up without the page, the
page took the pane, the file returned. Worse, the panel composed its layout
during that flicker and kept it. The host now gates the one
`set_active_buffer` call on the flag, so the page is added to the tab bar
and the current buffer is left alone.

**Startup means startup, not "startup that found an empty workspace".** The
page used to ask whether anything else was open and give up if anything was,
so restoring a session — or plain `fresh note.txt` — meant never seeing it
again: the only way back was to close every buffer and relaunch, which is
not a thing anyone does on purpose. Opening is unconditional now.
`hasOtherBuffers` survives, demoted: it decides the foreground, so
`fresh note.txt` still lands on `note.txt`.

**The seed follows the reader's own setting.** Fresh seeds one untitled
`[No Name]` buffer at launch. Whether the reader wants it is a question the
editor already asks — `editor.auto_create_empty_buffer_on_last_buffer_close`
— and this page reads the answer rather than inventing its own:

- **Setting on** (the default): the seed is the reader's scratch buffer. It
  keeps the pane and the focus; the page opens as a tab beside it. A bare
  `fresh` lands on `[No Name]` with `Welcome ×` in the tab bar.
- **Setting off**: the reader has said they want no empty buffer, so the seed
  is the placeholder this page replaces. The page closes it and takes the
  pane. A bare `fresh` shows one tab, `Welcome`, and no `[No Name]` anywhere.

For a while the page reaped the seed unconditionally, and then — after that
closed scratch buffers too — not at all. Both were wrong for the same reason:
the seed's meaning is not the page's to decide. The test for "seed" is
structural (not a file, no name, never modified) and is applied only at
startup and only on the foreground path; the `Welcome` palette command
closes nothing, because the palette is not startup.

**No lit cursor row.** The page passes `highlightCurrentLine: false` to
`createVirtualBuffer`. The caret's row means nothing on a page laid out by
widgets, and a highlighted band across the centred wordmark read as a
selection. The option is new and general: any panel whose rows are not lines
of a document can use it.

**Closing buffers never opens it.** It is a *startup* surface, not an
empty-workspace surface. It used to take the second path too, on the
reasoning that "launched with nothing" and "left with nothing" are the same
state. They are not: startup is the one moment the reader has not just told
us what they want, and every other emptying of the workspace is a close they
asked for. Reopening the page there read as the editor undoing that close,
and left no way to say "close everything" — you closed the last file and got
a full-page document back. The `buffer_closed` handler now does nothing but
drop this plugin's handle on its own buffer when that buffer is the one that
went away.

**When it gets out of the way.** This is the ruling that decides whether the
screen is a good citizen or a nuisance:

> A welcome buffer that was **on screen, auto-opened and never interacted
> with** — never scrolled, never focused, no widget touched — closes itself
> when a real file opens. One the user **engaged with** stays open until they
> close it. One that has never been on screen is not in the way of anything,
> and stays.

An ambient screen you ignored was ambient; a document you started reading is
yours. The third case is new with the background tab, and the rule had to
learn it: "step aside" was written for a page *occupying the pane*, and a tab
the reader has never seen has nothing to step aside from — closing it is not
politeness but deleting a tab they never asked to close. It used to do
exactly that, on the very first file they opened.

So `fresh src/main.rs` *does* leave a Welcome tab behind now, deliberately.
CLI file arguments are drained before `fire_ready_hook` (`main.rs`,
`process_pending_file_opens`), so the `after_file_open` for `main.rs` arrives
while there is no page yet and is ignored; `ready` then opens the page as a
background tab, and nothing later removes it. Turning to that tab and then
opening a file does close it, which is the original rule doing its job.

Asking for the page by name — `Welcome` in the palette — counts as
engagement whether or not the buffer already exists. Otherwise the startup
tab stays ambient for the whole session and evaporates on the next file open,
however many times the reader summons it.

**Closing it never reopens it.** Closing the welcome buffer leaves whichever
empty state the core settings choose. There is no loop, and no way to get
trapped.

**The startup toggle.** `[✓] Show this screen on startup` sits on its own
line directly under the chips, at the head of the first viewport. A control
for "I don't want this screen" belongs where someone who doesn't want the
screen will actually look — the first thing they see — not at the bottom of a
page they were never going to scroll. It was flushed right on the chips line
for one revision; on a 150-column terminal that put it sixty columns from
anything it related to, which is the same mistake the verb keys made (§13.13).
It is a bare button rather than a `toggle` widget: the two draw the same box,
but a button can say what it does under the pointer. The panel auto-focuses
its first widget, so the initial focus is moved to the first door explicitly:
a stray `Enter` on open must not switch the screen off.

**Neighbouring surfaces.**

- **Dashboard.** If it is enabled with auto-open it creates its own virtual
  buffer and simply wins; the two never draw at once. Documented, not
  special-cased.
- **`file_explorer.auto_open_on_last_buffer_close`.** The explorer still opens
  if configured, but focus stays in the welcome buffer.
- **Workspace-trust dialog.** A blocking modal in a higher z-band; it renders
  over the welcome buffer and owns the keyboard, exactly as it does over
  everything else.
- **`restore_previous_session`.** Untouched. A restored session has buffers,
  so no welcome screen.

---

## 9. Quality floor

- **Responsive.** Two breakpoints. Below ~76 columns the path cards stack into
  one column, the depth meter abbreviates, and card bodies reflow (frame 6).
  Below ~46 columns, or ~14 rows, the screen falls back to today's single
  centred hint line — a welcome screen that wraps is worse than none.
- **Reduced motion.** Reveal-on-scroll and the scroll-hint bob are gated on
  the animations setting; with it off, everything is simply present.
- **Keyboard-complete.** Every interactive element is reachable by `Tab` and
  activatable by `Enter`. Nothing is mouse-only.
- **Colour is never the only signal.** `●` / `◐` / `✓` / `⇅` in the dock carry
  a text legend on the same card; diagnostics carry their text, not just a
  squiggle. All colours come from theme keys, so the screen follows a theme
  switch — including the one made from its own theme card.
- **i18n.** Every string is a `t!()` key under `welcome.*`. Layout is computed
  from measured widths, never from assumed English lengths — the wireframes
  are the English rendering, not the layout contract.
- **Never blocks a frame.** The recent-file list and the workspace list are
  read off the editor thread (`spawn_off_loop_effect`) and cached; the first
  paint uses what boot discovery already loaded, and late arrivals repaint.

---

## 10. Cost, and what is honestly hard

The concept is buildable on shipping primitives, but three things deserve
naming rather than hand-waving:

1. **The embedded live views are the expensive part.** `windowEmbed` paints a
   real window into a reserved rectangle. For the LSP card that means standing
   up a real buffer with a real language server to demo against. The
   mitigation is to ship the card with a **static, syntax-highlighted sample**
   first (markdown `Text` with grammars — cheap, no LSP) and upgrade to a live
   embed only if the demo proves worth the machinery. The wireframe draws the
   destination; phase 3 draws the affordable version.
2. **Folds are widget-level, not buffer-level.** Fresh's real folding
   (`view/folding.rs`) works on buffer syntax, which a widget panel does not
   have. The gutter arrows are drawn by the panel and collapse the card by
   re-rendering its spec. Visually and behaviourally identical; mechanically
   not the same code, and `za` needs an explicit binding in the `welcome` mode
   rather than falling through.
3. **A long widget spec re-renders as a whole.** `updateWidgetPanel` replaces
   the spec; `widgetMutate` is the targeted fast path. A ladder this long
   should use `widgetMutate` for fold toggles and finder keystrokes, or every
   keypress in the finder re-transmits the entire page.

---

## 11. Open questions

1. **Does the welcome buffer get a keybinding of its own?** Everything else on
   the first screen teaches one. `Alt+H`? Or is the palette entry enough?
2. **Should Level 3 appear at all on a machine with no git repo and no
   worktrees?** Showing "one workspace per worktree" to someone editing
   `~/notes.txt` is honest about the product but useless to them. Options:
   always show it (the ladder is the pitch), or fold Level 3 by default
   outside a repo. Leaning: always show, always expanded — the whole point is
   that the ceiling is visible from the floor.
3. **First run versus every run.** Should the screen remember it has been seen
   and open *collapsed to the first viewport* thereafter, with levels folded?
   That trades the "I forgot Fresh could do that" rediscovery for a shorter
   page.
4. **Does the theme card write the theme, or preview it?** Writing config from
   a welcome screen is a real mutation. Proposal: it applies live like the
   theme picker does, and persists only on an explicit "keep this one".

---

## 12. Build order

| Phase | Contents |
|---|---|
| 1 | The buffer: `Welcome` virtual buffer, `welcome` mode, `empty_workspace_screen` setting with migration, open/close/get-out-of-the-way lifecycle. Static content, no widgets. |
| 2 | The ladder: first viewport, three path cards, `1`/`2`/`3` jumps, level banners, `{scroll}` status element, depth meter. |
| 3 | Cards as widget panels: fold arrows, the live finder, static syntax-highlighted code and diff samples, the startup toggle. |
| 4 | Live demos: theme picker, git staging, the embedded Orchestrator dock. |
| 5 | Polish: reveal-on-scroll, narrow breakpoints, the sub-minimum fallback, locale keys. |
| 6 | Flip the default to `"welcome"`; user docs under `docs/features/`. |

Phases 1–2 are shippable on their own and already beat all three of today's
empty states. The default does not move until phase 6.

---

## 13. What the build taught

`plugins/welcome_screen.ts` implements this design. It is a TypeScript plugin
— **no host change was needed**, which was the bet §4 made and it held. The
page is a virtual buffer with a `WidgetPanel` mounted into it; every control
is a widget from `plugins/lib/widgets.ts`; the demos read real data through
`spawnProcess`, `getAllThemes` / `applyTheme`, and
`getPluginApi("orchestrator").listWorkspaces()`.

Eleven things the wireframes did not know:

1. **A panel repaint replaces the whole buffer, so it parks the viewport at
   line 0.** Fine for a panel that fits its pane; wrong for a document you
   scroll. Every repaint now captures `getViewport().topLine` and restores it
   afterwards. Folding only removes rows *below* a card's header, so the
   restore is exact rather than approximate.

2. **`viewport_changed` fires on height changes too — including the one the
   command palette causes by taking a row.** Repainting there cancelled the
   prompt the user had just opened (`Search cancelled.` in the status bar,
   every time). The listener now dedupes on **width only**: width is what the
   layout depends on, and every list pins its own `visibleRows`, so height
   changes have nothing to recompute.

3. **`scrollBufferToLine` is a *reveal*, not a scroll-to-top** — it
   deliberately leaves `viewport_height / 3` of context above its target.
   Right for "show me this match", wrong for a level jump and for the repaint
   restore above. A local `scrollTopTo` compensates rather than asking for a
   second host verb.

4. **`move_page_up` / `move_page_down` page the *cursor*.** On a cursorless
   page whose cursor is wherever the widget runtime last parked it, that jumps
   somewhere the reader never was. Page keys compute the new top line from the
   viewport instead.

5. **A mode with `allowTextInput` owns the keyboard**: the host blocks unbound
   Ctrl-/Alt-modified keys so a focused text field can never be hijacked by
   Open or Save. That is the right default, and it means the accelerators this
   page promises have to be named — `FORWARDED` lists them and each one
   forwards to the real action, so a rebound key keeps working. Notably they
   do *not* mark the page engaged: reaching past it for the palette is the
   ambient case.

6. **Tab moves widget focus, but the host only scrolls the pane for a focused
   *text* widget.** A focused button further down a long document was
   invisible. The `focus` event now reveals the focused widget's card, using
   card-header rows read back from the painted buffer — the same read-back
   `resolveLevelLines` already did for the jump keys. A keyed read-only widget
   also joins the Tab cycle, so the markdown sample is deliberately keyless.

7. **`getAllThemes()` answers with the registry object, not a list.** Its keys
   are the theme names.

8. **Closing the page reopened it.** `closeBuffer` fires `buffer_closed`,
   which — with no other buffer left — was exactly the condition the ambient
   open path watched for. Escape, the tab's `×` and `Ctrl+W` were all
   unclosable, and a `dismissed` flag was added to hold the ambient path off
   for as long as the workspace stayed empty. The flag was a patch on the
   ambient path, and the ambient path is now gone (§8, "closing buffers never
   opens it"), so both are: nothing reopens the page but startup and the
   `Welcome` command.

9. **A `List` inside a `labeledSection` cannot reach the section's right
   border.** Its items are emitted at their natural width, so every finder
   result ended in a `…` clip marker exactly where the frame should be — and
   padding cannot fix it: one column short leaves the border undrawn, one
   column over draws the marker in its cell. `raw` rows, which the host pads
   to the enclosing section, do reach it. The results are rows now and the
   plugin owns the selection, which `finderIndex` already was.

10. **Enter on a single-line `Text` widget is advance-focus**, so a finder
    that merely forwarded the key moved on instead of opening the pick.

11. **`executeAction` with a name no action and no plugin handler owns fails
    only in the log.** The Git log button was wired to `git_log`, which is the
    palette *label*; the handler is `show_git_log`. The click did nothing on
    screen and reported "executed successfully". Cross-plugin dispatch does
    work — `start_review_branch` (audit_mode) and `orchestrator_new` resolve
    fine — but the name has to be the handler's, and a button whose provider
    may not be loaded should be gated: the Orchestrator's two are drawn only
    when `getPluginApi("orchestrator")` answers.

12. **Hand-wrapped prose only fits the width it was wrapped at.** The three
    door bodies were literal arrays of short lines, set to a third of a wide
    terminal. Below 96 columns the doors stack full-width, and those lines
    stayed a 22-column column inside a 57-column box — the one place on the
    page that looked broken at the narrow breakpoint. They are single
    sentences now, wrapped at render time to whichever width is in force.

13. **Nothing right-aligned reads as connected across a wide measure.** The
    verb keys sat at the right edge of the column and the startup toggle at
    the right edge of the chips line; at 150 columns neither looked like it
    belonged to the thing it named. The keys are a column of their own four
    spaces past the longest label now, and the toggle has its own line. The
    surviving right-aligned things all have a rule or a box connecting them
    to their label — a card heading's hint, a workspace row's branch.

14. **A panel repaint keeps the pane's scroll position.** The plugin had a
    save-and-restore around `panel.set()`, on the belief that a repaint parks
    the viewport at line 0. It does not — and the restore, which travels
    through the host's *reveal* path, lands a line off, so every keystroke in
    the finder walked the page up the screen. Deleting it fixed the drift.

15. **A relative scroll needs its own ceiling.** `getViewport().topLine` does
    not refresh between the plugin's own scrolls, so the plugin's model of the
    top line is the authority — and at the bottom of the document, where the
    pane stops moving, that model kept climbing. Holding `Down` past the end
    bought as many dead `Up` presses. The model is clamped to the painted line
    count now.

16. **Buffer-mounted widget panels had no hover at all** — the one finding
    that could not be fixed from the plugin side. `update_widget_hover` walks
    the `Dock` and `Floating` panel slots; a panel mounted into a *buffer* has
    no `FloatingWidgetPanel` to hold a hovered key, so `hoverStyle` on any of
    its widgets was dead spec. The fix is small and belongs to the runtime
    rather than to this page: the hovered keys live on the panel's registry
    state, and a second tracker resolves the pointer through the same
    `screen_to_buffer_position` → `hit_test_row_aware` pair the mounted click
    path already uses, so hover and click can never disagree. Settings and
    Search & Replace get it too.

17. **A one-column margin is a wobble, not a margin.** The measure is capped
    at 88 columns and centred in the pane, page-view style. Between 88 and 92
    columns the arithmetic yields a one-column indent, which reads as a
    misalignment rather than as composition; below two columns of gap the page
    stays flush left.

18. **Two scroll paths is one too many.** The `0` / `Home` jump called
    `scrollBufferToLine` directly while everything else went through the
    tracked helper, so the plugin's model of the top line was left
    wherever the reader had been before — and the next `Down` computed
    from there, jumping to the end of the document. Everything scrolls
    through the one helper now.

19. **A focus event can arrive before the viewport catches up**, so
    `revealLine` reading `getViewport().topLine` judged an on-screen row
    off-screen and yanked the page to it. It reads the tracked line.

20. **Repainting a panel that holds a text widget can pull the pane to
    that widget.** Folding a card *by click* never moved the page;
    folding the same card *by keyboard* dropped the reader two cards
    away, at the finder field. With the finder card folded — no text
    widget in the panel — the keyboard fold held its place exactly,
    which named the cause. A fold now re-asserts the line it happened
    on.

21. **`labeledSection` emits no hit of its own**, so a `key` on one is
    inert: clicking a door's frame could not be routed. Its headline is
    a full-width bare button instead, which is also what gives the card
    a hover.

22. **`widthPct` applies only to a Block child of a Row.** The stacked
    doors filled their column for free while they were children of a
    `Col`; wrapping each in a `row` to carry the page margin switched
    `widthPct` back on and shrank them to a third of the pane. The
    percentage has to name the layout it is in.

23. **The host already had the page margin.** The measure was centred by
    padding every raw row with spaces, which the fenced code sample could
    not join — a markdown code block turns leading spaces into NBSP and
    paints its background across them, so the sample carried a slab of
    grey the width of the whole margin. `setLayoutHints({ composeWidth })`
    is the mechanism markdown compose mode uses: the host centres the
    render area and paints the flanking margins paper-on-desk
    (`ui.compose_margin_bg` outside, a one-column paper edge inside).
    Handing it the measure replaced every scrap of the padding
    machinery, and the sample came right on its own.

    Two consequences worth knowing. The panel still lays out at the
    *pane* width, not the compose width, so anything that fills the panel
    (a `labeledSection` left to fill a `Col`) is wider than the area the
    host clips to — on a pane a little wider than the measure, the
    stacked doors' top border wrapped. And a code block still paints only
    as far as its text, so the sample is padded to a rectangle: trailing
    spaces inside the fence carry the background the same way leading
    ones do.

24. **The hover key lights every widget that shares it.** Making each
    interior row of a door a full-width bare button keyed to that door
    means a click anywhere inside the card jumps, and hovering any row
    lights the whole card at once. `hoverStyle` takes `bold`, so the
    highlight can carry emphasis rather than only colour.

25. **The empty-workspace screen has to mean empty.** The ambient-open
    condition asked whether any *file* buffer was open, so closing the
    last text buffer with a terminal or an agent still running popped the
    page up over a workspace that was plainly in use. It was fixed to count
    every buffer except this page and the host's own untitled seed — which
    is still the test the startup path uses, now the only path that asks.

26. **`hoverStyle` had no sibling.** A bare button is just its label, so
    the only way to mark a word as clickable was to spend a colour on
    it — and `intent` offers three fixed looks, none of them an
    underline. `Button` now carries a `style` for the resting state, the
    same shape as `hover_style` and one state earlier. Focus, hover and
    disabled still win over it, in that order of immediacy. The page
    uses it for the conventional mark: clickable words are underlined,
    and under the pointer they lift — brightest ink, bold, underline
    kept — rather than being highlighted. A background band reads as a
    *selection*, a state the thing is in; a glow reads as the pointer
    being on it, which is what is actually true.

    It is not applied to everything that can be clicked. A framed
    `[ button ]` already says what it is, a fold arrow is a glyph, and a
    full-width finder row would underline its padding as well as its
    path. Inside a door only the verb row is marked — a card is a card,
    not a link.

27. **A code block wraps rather than truncates**, so a sample too wide
    for its box silently loses its own tail to continuation rows. The
    sample has a short variant, chosen against the width the box
    actually draws at rather than a derived one. It sits in a rounded
    `labeledSection`, inset from the prose: a listing, not a paragraph.
    Its background is the host's `ui.inline_code_bg`, hardwired in the
    markdown renderer shared with every hover popup and the markdown
    preview — not this page's to switch off.

28. **A mark that only appears on hover teaches nothing** — you have to
    already be pointing at the thing to learn that you could point at
    it. Clickable words carry their underline always; the pointer adds
    the lift, not the mark. Which forces a rule about labels: an
    underline runs the width of the button's cells, so the label has to
    be exactly the text. Markers (`▸ `, `● `) sit outside the button
    now, and nothing underlined is `fullWidth` — a full-width result row
    underlined its padding and read as a rule drawn across the card.

    The framed buttons are deliberately not underlined: their brackets
    already say what they are, so they take the lift alone. The rule is
    "if a thing shows an underline it shows it always", not "everything
    clickable is underlined".

29. **A section can be hovered by proxy.** `labeledSection` emits no hit
    of its own, so it is never what the pointer is on. Giving it the key
    of the control inside it makes `ctx.is_hovered` true whenever that
    control is, and its border and legend light with the card instead of
    watching the card light. `LabeledSection` carries a `hover_style`
    for this — the extension path `Button`'s own docs describe.

30. **Sharing one key across a group makes focus paint the group.** Every
    row of a door carries the card's key so the whole card is one target;
    the focus clamp lands on the one tabbable row, and every other row
    rendered itself focused too — a standing band across the card at
    rest. `Button` now gates focus rendering on `focusable`: a widget
    dropped from the Tab cycle can never be what focus is on, so it must
    not look like it.

31. **"I closed it" answers one question, not the session.** The dismissal
    flag was set for good, so closing the page once meant it never returned
    however many times the workspace emptied afterwards — which reads
    exactly like a screen that appears at random. Scoping it to a single
    emptying fixed that symptom and kept the cause: a page that comes back
    when you close things. Dropping the reopen-on-empty behaviour outright
    (§8) retired the flag with it — there is no longer a question for "I
    closed it" to answer.

32. **A background tab was laid out to its neighbour's width.** A page
    opened behind a file painted at a measure its pane could not hold: the
    wordmark wrapped mid-glyph, the doors broke across two rows, every
    centred row sat far right. `widget_panel_width` sizes a panel from
    `compose_width` — correctly, and its own comment says why, measured on
    this page — but it read `vs.compose_width`, and `SplitViewState` derefs
    to whichever buffer is *active* in that split. So the panel was sized
    from the neighbouring tab's answer (`None`, hence the whole split)
    whenever it was not the one on screen, and nothing repaints a
    background tab afterwards, so it stayed wrong until a resize. It now
    reads `vs.buffer_state(buffer_id).compose_width` — this buffer's own.
    The pane width beside it is a property of the split and still comes
    through the deref.

    This was worth chasing to the host rather than papering over. The
    plugin-side workarounds all failed, each for the same reason: a
    re-render, a panel remount and a repaint armed on `buffer_activated`
    read the same wrong width, and `viewport_changed` never fires for this
    buffer at all. Opening in the foreground and handing the pane back a
    tick later did work — on Linux and macOS, while Windows CI, where a
    later repaint landed after the hand-back, showed the bug unchanged.
    A fix that depends on which repaint wins is not a fix.

33. **A background tab hears nothing about geometry.** `viewport_changed`
    is fired per *split*, against a `previous_viewports` tuple read through
    `SplitViewState`'s deref — i.e. the active buffer's. A page sitting
    behind a file is told nothing: not a resize, and not the switch that
    finally shows it, because the split's tuple after the switch is the
    same pane it already was. So a page created at 140 columns and brought
    forward after the terminal shrank to 70 kept a `composeWidth` hint
    describing a terminal that no longer existed.

    The page now catches up when it comes to the front: `buffer_activated`
    schedules a repaint one tick later — `getViewport()` at the moment of
    activation still reports the viewport this buffer had when it was last
    on screen, and only the following frame corrects it — which clears
    `paneWidth`, recomputes `layoutKey` and repaints if the shape moved.
    The `await editor.flush()` between the hint and the repaint is load
    bearing: `widget_panel_width` reads `compose_width` while it processes
    the update, so a repaint issued in the same breath as the hint is laid
    out against the previous one.

34. **The tab-switch slide froze the pane at its first frame, and the
    screen kept that frame after the slide.** Even with 33 fixed, the
    corrected page did not appear until the reader's next keystroke, and
    for a while the note here blamed an idle editor drawing no frame, and
    then a stale row index. Probing one frame end to end settled it: the
    buffer held the new text, the tokeniser read it, the content pass
    painted it (toggle at column 36) — and `animations.apply_all`, a few
    lines later in `Editor::render`, painted the old cells (57) back over
    the pane.

    `Ctrl+PageDown` starts a 260ms `SlideIn` over the pane. Two things it
    did were wrong, both general, neither about this page:

    - It took its "after" snapshot **once, on its first apply** — the first
      frame after the switch, when the pane still held the stale layout —
      and shifted that for the slide's whole duration. The plugin's catch-up
      (33) lands ~100ms in and was painted every frame, and covered every
      frame. It now retakes the snapshot from the freshly painted frame on
      every apply: the content pass runs before the runner, so `buf` is the
      pane as it is *now*, which is the only thing a slide may show shifted.
      A snapshot is 69×37 cells; per frame for 260ms that is nothing.
    - When the last effect finished, the frame it finished on was its own
      composite, the runner retired it, `is_active()` went false, and no one
      asked for another frame. The runner now owes one settle frame after an
      effect retires (`take_settle_frame`), and the frame loop treats it as
      "animations active" for that one iteration. This is what "a repaint
      with no input behind it" actually was: not a missing frame, but a
      frame painted from a snapshot, followed by no frame at all.

    A wrong turn worth recording: `set_virtual_buffer_content` skipping
    `wrap_indices.damage_all()` looked like this bug and is not — the row
    index is rebuilt on the version bump regardless. The e2e test written
    for it passed with the change reverted, which is how the theory was
    caught; it is not in this PR.

### Still aspirational

- **The LSP card** shows a real syntax-highlighted Rust sample (a markdown
  `Text` widget carrying the grammar registry — the highlighting is genuinely
  the editor's own), but no live hover popup or diagnostic. That needs a real
  buffer with a real language server behind a `windowEmbed`, which is §10's
  first cost item.
- **The git card** reports the real branch and the real changed-file list, but
  the stage / unstage buttons of the mock are not there; it links to the
  branch-diff review instead.
- **The Orchestrator card** lists the real workspaces with their agent state
  and focuses one on click, but does not embed a live terminal transcript.

### Verified by hand

Driven in tmux at 150×42, 64×42 and 52×30, against both a two-file scratch
repo and this repository (where the workspace-trust modal correctly renders
over the page and owns the keyboard, and the finder fuzzy-matches the whole
tracked tree — `wlcscr` finds `welcome_screen.ts`): the ladder
and jump keys, `/` to the finder, live fuzzy-find over `git ls-files` and
`Enter` to open a hit, folding by click and by `Enter`, live theme switching
by click (status bar confirms), the startup toggle flipping and **persisting
across a restart** (the screen then stays away, and the `Welcome` command
brings it back), `Ctrl+P` opening the palette from the page, the `[No Name]`
seed being retired on open, `fresh notes.txt` never leaving a Welcome tab
behind, and both responsive breakpoints.
