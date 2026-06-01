# Orchestrator Dock — Usability Test Findings (NN/g style)

**Feature:** Orchestrator Dock (persistent left session column; Alt+O /
`Ctrl+P → "Orchestrator: Toggle Dock"`).
**Build:** `cargo build --bin fresh` (debug, **no `--release`**) →
`target/debug/fresh` (v0.3.10).
**Method:** Moderated task-based usability test, think-aloud, single
evaluator standing in for the "Devon" persona (developer juggling three
parallel sessions). Protocol: [`ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md`](ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md).
**Harness:** `tmux 160×42` + `send-keys` + `capture-pane` (plain **and**
`-e` colour captures to read focus state from the divider/highlight).
**Evidence:** numbered screen captures in `/tmp/uxrun/NN_label.txt`
(referenced inline as `cap NN`).
**Date:** 2026-06-01.

> Severity = NN/g 0–4 (see guide §4). Each finding names the violated
> Nielsen heuristic and a concrete fix. The appendix lists what already
> works, as a sanity check on scope.

---

## Fix status (2026-06-01, branch `claude/orchestrator-dock-ux-test-dsXNW`)

| ID | Status | Notes |
|----|--------|-------|
| **F7** | ✅ **Fixed** | Unborn-HEAD detected up front with a friendly message instead of raw `git worktree add` errors. e2e regression test. |
| **F5** | ✅ **Fixed** | Dock filter resets when focus leaves the dock, so re-entry shows the full list. e2e regression test. |
| **F4** | ✅ **Reclassified** | Not a separate bug — a manifestation of **F1**. The host's dock Esc handler always blurs when focus isn't on the filter; the "Esc → +New / list unreachable" reading happened only while a *terminal* was active (F1 key-leakage). Guard test added with a non-terminal active window. |
| **F2** | 🔎 **Root-caused; fix deferred (focus-model decision)** | `show_file_explorer()` → `toggle_file_explorer()` → `take_focus_for_file_explorer()` sets `key_context = FileExplorer`, so *showing* the explorer also *focuses* it. The launch session is opened this way at startup (`main.rs` `show_file_explorer()`), so it starts explorer-focused; a dock dive then preserves that per-window focus (intentional, documented on `Window::key_context`), landing the first keystrokes in the tree. There is **no `focusEditor` plugin API**, so a contained plugin fix isn't available. A correct fix either (a) shows the explorer without focusing it at startup/dive, or (b) is the gaps-doc **P1** refactor (dock as `KeyContext` chrome, where dive is a normal context transition). Both change documented focus semantics → needs a deliberate decision, not a drive-by patch. |
| **F1** | ⏳ **Open (large)** | The principled fix is the gaps-doc **P1** (dock as an editor-global `KeyContext` chrome region) so a terminal buffer's mode can't shadow dock keys and the focus indicator can't disagree with the keyboard target. Sizeable refactor; F4 also resolves once this lands. |
| **F3** | ◑ **Partly covered** | The **buffer** active-window case already reflows on dock-close (existing test `dock_close_reflows_buffer_to_full_width`). The **terminal** active-window case (my repro) still leaves a stale gutter until a resize — host terminal-reflow on dock hide. |
| **F6** | ⏸ **Deferred (low ROI)** | The project tag already conveys the project for non-active rows; the tag's presence/absence is a load-bearing switch signal (plugin comment), and the alternative (name = basename) changes branch naming. P1 severity doesn't justify the blast radius yet. |
| **F8** | ⏸ **Deferred (debatable)** | The Tab re-pop is the intentional directory-descent affordance (complete to `foo/` → show its contents). The annoyance is the popup overlaying the next fields (a host placement concern), not the re-trigger itself. |

---

## Scorecard

| Task | Goal | Result |
|------|------|:------:|
| T1 | Open the dock | ✔ unaided (Alt+O) |
| T2 | Read the list | ✔ (one labeling nit → F6) |
| T3 | Switch sessions (↑↓ live-switch) | ✔ reliable |
| T4 | Dive & edit | ◑ **edits land in the file tree first** → F2 |
| T5 | Filter by name | ✔ (persistence nit → F5) |
| T6 | New session | ✔ form coexists beside dock (error-msg nit → F7, F8) |
| T7 | Multi-select & actions | ◑ works, but focus gets stuck → F4 |
| T8 | Toggle scope/worktrees/empty | ✔ |
| T9 | Coexist w/ palette/Settings/explorer | ✔ no overlap, no swallowed keys |
| T10 | Hide & round-trip | ◑ **stale gutter on hide** → F3 |
| T11 | **Core loop** (work→list→switch→work ×N) | ✔ mechanics solid; editor sessions hit F2 on first touch |
| RQ2 | Focus indicator | ✔ divider cyan↔muted on Alt+O |

Two task-blocking issues (F2, F1) and a state where the dock becomes
keyboard-unusable (F1) are the headline results. Everything *visual*
about the dock — layout, coexistence, the focus indicator, live-switch —
is solid; the problems cluster in **focus/input routing**.

---

## The core loop (T11) — focus session → list → switch → focus, repeated

This is the dock's bread-and-butter: work in a session, **Alt+O** to the
list, **↑/↓** to another session, **Alt+O** to drop into it, work, and
back again. I ran it for several cycles across a mix — `alphaproj`
(editor), `betaproj` + `gammaproj` (terminal/agent sessions).

**The switching machinery is solid.** Every Alt+O toggled focus
session↔list; every ↑/↓ live-switched and the active window re-rooted
(alpha↔beta↔gamma↔alpha); each session's content **persisted** as I
cycled away and back (alpha's buffer kept its edits the whole time —
caps 96–102); the focus indicator flipped each time. For
**terminal/agent sessions the loop is seamless**: Alt+O into the session
and you're typing in its terminal immediately (`echo BETAMARK` landed in
the betaproj shell — cap 98).

**The one rough edge is F2, and the loop is where it bites hardest.** The
*first* time you Alt+O into an **editor** session, focus lands on its
file explorer, so your first keystrokes filter the tree instead of
editing: typing `MARKA` after dropping into `alphaproj` put `/MARKA` in
the explorer header, the buffer untouched (cap 96). It is a **one-time
papercut per session**, not every cycle — once you `Ctrl+E`/click into
the buffer, that session *remembers* buffer focus, and later Alt+O
round-trips (even after switching all the way out to beta and back) land
you straight in the buffer: `PROBE` then `RETURN` both inserted into
alpha's document on re-entry (caps 101–102). So the loop is fluent for
agent sessions and fluent for editor sessions *after the first touch* —
the cost is the surprising first-keystrokes-filter-the-tree moment each
new editor session inflicts before you've trained it. (Full fix = F2.)

**Minor wrinkle:** right after a switch I twice saw the list **re-order**
(the active session jumping toward the top) and, momentarily, the
highlighted row not matching the active window (caps "Down→alphaproj");
it settled to a stable order and a matched selection within the same
interaction, so it reads as a transient re-sort rather than a persistent
desync — but on a fast loop it can make the next ↑/↓ land on an
unexpected row. Worth watching; folded into F4's focus-consistency note.

---

## P3 — Major (high priority)

### F1. The dock can claim keyboard focus while the terminal eats the keys
**Heuristics:** #1 Visibility of system status (the indicator lies);
#3 User control & freedom (no obvious escape).
**RQ:** RQ2, RQ6. **Severity: 3.**

When a **terminal** session is the active window, the dock can enter a
state where its focus indicator shows **focused** (the right-edge divider
is the accent cyan, `38;2;0;255;255`) but every keystroke is actually
routed to the terminal's PTY. Pressing the dock's own keys (Tab, ↑↓,
Space) then leaks into the shell: after a handful of Tabs the terminal
displayed bash's completion prompt **`Display all 1552 possibilities?
(y or n)`** — proof the keys went to the PTY, not the dock (cap 54). In
that state, Tab/↑↓/Space/`/`/Esc and even **toggling focus with Alt+O
twice did not recover** the dock — the user is locked out of their own
session list (caps 50–58).

The indicator and the real keyboard target disagree, and the documented
recovery ("from a terminal session exit terminal-input first with
`Ctrl+]`") is undiscoverable from the UI — nothing tells the user their
keys are going to the terminal, because the dock looks focused.

*Note:* a plain live-switch onto a terminal session keeps the dock in
control (cap 69 — Space toggled the checkbox, no leak); the wedge is
reached after combinations involving diving into the terminal and/or
Tabbing to the action buttons, which makes it intermittent and therefore
**harder to diagnose in the field, not rarer**.

*Root cause (matches `dock-ux-test-plan.md`'s note):* dock keys are
dispatched at the floating-panel layer, but a terminal buffer's mode can
shadow them, and the "dock focused" flag (drives the divider) is tracked
separately from whether terminal-input is active.

*Fix:* make the focus indicator tell the truth — when the dock is
"focused" but the active window's terminal still holds raw input, either
(a) actually steal input from the PTY while the dock owns focus, or
(b) show the dock as *blurred* and surface a hint ("`Ctrl+]` to leave the
terminal"). The principled answer is the gaps-doc **P1 (dock as
`KeyContext` chrome)**: focus precedence then resolves deterministically
and the terminal can't shadow the dock.

### F2. Diving into a session lands focus in the file tree, not the buffer
**Heuristic:** #7 Flexibility & efficiency; mismatch with the user's
mental model ("press Enter, then type → I'm editing").
**RQ:** RQ3. **Severity: 3.**

From the dock, selecting a session and pressing **Enter** ("dive")
activates the window with its **file-explorer pane focused**. The first
keystrokes therefore *filter the file tree* instead of editing the
buffer: typing `ZEBRA` after a dive put `/ZEBRA` in the explorer's filter
header, not into the document (caps 20–21). The user must `Ctrl+E` or
click into the buffer first — a hidden, mandatory extra step on the
single most common action.

Editing itself is fine once a buffer is focused (cap 23: opening
`readme.txt` then typing `XYZZY` inserted correctly, dock stayed
visible), so the defect is purely the **focus landing site** on dive.

**Scope (from the T11 loop):** this is a **first-touch papercut per
session**, not an every-cycle one. The very first focus of an editor
session lands on the explorer; once you `Ctrl+E`/click into the buffer,
that session *remembers* buffer focus and subsequent Alt+O round-trips —
even after switching out to other sessions and back — land in the buffer
(`PROBE`/`RETURN` inserted on re-entry, caps 101–102). It still hits the
user on every *new* editor session, silently, exactly when they expect to
type — so it stays P3 (surprise + a stray-keystroke / mild
data-confusion risk), just bounded.

*Fix:* on the **first** dive / `set_active_window` for a session, seed
focus on the editor pane (the buffer), not the explorer. Confirmed as an
open gap in `orchestrator-dock-gaps.md`; the gaps-doc P1 refactor
resolves it as a normal context transition.

---

## P2 — Minor→Major

### F3. Hiding the dock leaves a stale empty gutter
**Heuristics:** #1 Visibility of system status; #8 Aesthetic & minimal
design. **RQ:** RQ7. **Severity: 2.**

Running **"Orchestrator: Toggle Dock"** to hide the dock removed the dock
but the editor chrome did **not** reclaim the freed ~40-column strip —
the menu bar and content stayed shifted right, leaving a blank gutter on
the left (cap 79). It persisted across a re-capture (cap 80) and only
corrected after a terminal **resize forced a relayout** (cap 81). So a
user who hides the dock to "get the full width back" is left with a dead
left margin until they happen to resize.

*Root cause:* matches the documented `last_frame_width/height store full
size, not chrome_area` bug — the chrome relayout uses the stale full
width. *Fix:* recompute `chrome_area` width on dock hide (don't wait for
a resize event).

### F4. Focus restoration after the bulk-action buttons is inconsistent
**Heuristics:** #4 Consistency & standards; #3 User control & freedom.
**RQ:** RQ5. **Severity: 2.**

After Tabbing from the list to the bulk actions (`[ Stop ] [ Arch ]
[ Del ]`) and pressing **Esc to back out**, focus did not reliably return
to the session list:
- In one run Esc moved focus to the **`[ + New ]`** button at the *top*
  of the dock while the list still showed a highlighted row (a stale
  cursor), so ↑↓/Space silently operated on the wrong control and the
  list looked focused but wasn't (caps 50–52).
- In another run Esc **left focus on the buttons** (cap 76).

Either way the model "Esc returns me to the list I came from" is broken,
and the misleading leftover row-highlight (F1's cousin) compounds it.

*Fix:* Esc from any dock sub-control returns focus to the sessions list
and clears the stale highlight from non-focused controls; make the
*focused* control, not a leftover cursor, the only thing that reads as
selected.

---

## P1 — Minor

### F5. The filter persists silently and has no clear affordance
**Heuristics:** #1 Visibility; #6 Recognition over recall.
**RQ:** RQ4. **Severity: 1–2.**

`Esc` **on the list** (after a filter) leaves the dock *without clearing
the filter*. Refocusing later (Alt+O) shows a list silently narrowed to
the old query — only the filter box (still reading `gamma`) hints why
sessions are "missing" (caps 26–31). There's no one-key "clear filter"
on the list; the user must re-enter the filter and backspace it out
(cap 32). A returning user can reasonably think sessions disappeared.

*Fix:* clear the filter when the dock blurs (or on `Esc`-to-leave), or
show a persistent "filtered: N hidden — Esc to clear" affordance on the
list.

### F6. New sessions are auto-named `session-N`, losing the project
**Heuristics:** #2 Match between system & real world; #6 Recognition.
**RQ:** RQ5. **Severity: 1.**

A session created for `/tmp/uxtest/betaproj` is listed as **`session-1`**
/ `session-2`, not `betaproj` (caps 13, 15). The project only appears as
a secondary tag *when it differs from the active project*, and the active
session shows none — so "which project is `session-2`?" requires reading
the tag or diving. (Mitigated: the **filter matches the project path**,
so typing `gamma` still finds `session-3 gammaproj` — cap 25.)

*Fix:* default the session name to the project basename (e.g.
`betaproj`, `betaproj-2`), or always show the project on the row.

### F7. Worktree-creation errors are raw git text
**Heuristic:** #9 Help users recognize, diagnose, recover from errors.
**RQ:** RQ5. **Severity: 1.**

Creating a session whose repo has an unborn `HEAD` surfaced the raw git
failure verbatim in the form and status bar: **`Error: fatal: invalid
reference: session-1`** / `fatal: invalid reference: HEAD` (caps 10,
10b; log shows the two failed `git worktree add` attempts). It's
git-jargon, doesn't say *what to do*, and the fallback attempt produced a
*second*, equally cryptic message.

*(This was triggered by a test-setup mistake — my throwaway repos had no
commits — but the message quality is the finding, not the trigger.)*

*Fix:* translate common worktree failures ("This repository has no
commits yet — make an initial commit before creating a worktree
session.") and don't echo the raw second-attempt error.

### F8. Tab in the New-Session path field re-pops the completion over fields
**Heuristic:** #5 Error prevention. **RQ:** RQ5. **Severity: 1.**

In the New Session form, **Tab** on the Project Path field both *accepts*
the path completion and *re-triggers* a completion popup that overlays
the next fields (worktree checkbox / Session Name), so a Tab-to-advance
user momentarily can't see where they are (cap 06). `Esc` clears just the
popup (good — cap 07), but the double-duty Tab is surprising.

*Fix:* once a completion is accepted, Tab should advance to the next
field without immediately reopening the popup.

---

## Observation (not scored as a defect)

- **`fresh dir1 dir2 dir3` at launch does not create three sessions** —
  only the launching cwd becomes a session; the extra directory args
  don't populate the dock (caps 00–02). For a user whose mental model is
  "open my three projects → see three sessions", this is a dead entry
  point; the working path is `+ New` per project. May be by design
  (multi-dir is meant for forwarding to a *running* instance), but worth
  a docs note or a launch-time "open each dir as a session" behaviour.

---

## Appendix — what already works (sanity check on scope)

- **Open & layout (T1):** Alt+O opens *and* focuses the dock as a
  full-height left column with the editor chrome to its right
  (cap 01). `+ New`, scope, worktrees/empty toggles, filter, and the
  pinned `↑↓ switch / Enter edit / Esc editor` hint all render.
- **Focus indicator (RQ2):** the right-edge divider is accent-cyan when
  the dock is focused and muted when blurred, toggling with Alt+O — a
  clear, glanceable signal (cap dv/dv2). *(Its only failure mode is F1,
  where it can be wrong.)*
- **Live-switch (T3):** ↑↓ re-roots the active window with the selection;
  selection and active window stayed in lock-step across alpha↔beta↔gamma
  (caps probe sequence). **List order is stable** across switches.
- **Filter (T5):** `/` focuses it, typing filters live (matching name
  *and* project path), `Enter` returns to the list **without diving**
  (cap 26).
- **Toggles (T8):** Alt+T (worktrees), Alt+I (empty/1-file), Alt+P
  (scope) each flip state; scope `this` correctly narrows to the active
  project (caps 33–35).
- **New Session coexistence (T6):** the form opens as a centered modal
  **beside the still-visible dock** (cap 05), and `Esc` is contextual
  (closes the completion popup before the form — cap 07).
- **Multi-select (T7):** Space toggles `[x]` on list rows; the bulk bar
  `[ Stop ] [ Arch ] [ Del ]` appears and the buttons are Tab-reachable
  (caps 36–37). *(Marred only by F4's focus issue.)*
- **Coexistence (T9):** `Ctrl+P` falls through (dock blurs, stays
  visible) and the palette renders **right of the dock column** with no
  overlap (caps 61–62); **Settings** renders in the chrome area beside
  the dock, fully visible, `Esc` closes cleanly (caps 63–64); `Ctrl+E`
  focuses the explorer with the dock unchanged (cap 65).
- **No key leak when focused on a non-terminal (T13):** typing while the
  dock is focused over a buffer does **not** leak into the document or
  the filter (cap 24).
- **Editing (T4 tail):** once a buffer is focused, typing inserts and the
  dock stays visible/blurred (cap 23).
- **Re-show (T10 tail):** "Toggle Dock" re-opens the dock at the left
  edge with the session list intact (cap 82).
- **Core loop (T11):** the focus-toggle loop (Alt+O ↔ list, ↑/↓ switch)
  is reliable across many cycles and a mix of editor + terminal sessions;
  active window re-roots every time and per-session state persists
  (caps 96–102). Seamless for agent/terminal sessions; for editor
  sessions it's seamless after the one-time F2 first-touch.
