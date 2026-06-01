# Orchestrator Dock — Usability Test Guide (NN/g style)

**Feature:** Orchestrator Dock — the persistent, non-modal left session
column. Opened/focused with **Alt+O** (or `Ctrl+P → "Orchestrator:
Toggle Dock"`).
**Build:** `cargo build --bin fresh` (debug, **no `--release`**) →
`./target/debug/fresh`.
**Method:** Moderated, task-based usability test with think-aloud,
following the Nielsen Norman Group protocol (goal-based scenarios, not
instructions; observe, don't lead; rate findings on the NN/g 0–4
severity scale).
**Harness:** `tmux` + `send-keys` + `capture-pane` (a single evaluator
drives the keyboard and reads the rendered screen, standing in for a
representative user).

> This is the **protocol**. The companion run + findings live in
> [`ORCHESTRATOR_DOCK_NNG_FINDINGS.md`](ORCHESTRATOR_DOCK_NNG_FINDINGS.md).
> The lower-level engineering checklist is
> [`dock-ux-test-plan.md`](dock-ux-test-plan.md); this guide is the
> *user-centred* layer above it — it asks "can a developer accomplish
> their goal?", not "does control X fire?".

---

## 1. Study goals & research questions

The dock's job-to-be-done: *let a developer running several parallel
agent/editor sessions see them all, switch between them instantly, and
act on them — without losing their place in the file they're editing.*

We want to answer:

- **RQ1 — Discoverability.** Can a new user find and open the dock, and
  do they understand what it lists once it appears?
- **RQ2 — Switching.** Is moving between sessions fast, predictable, and
  reversible? Does the user always know which session is active and
  whether the dock or the editor has the keyboard?
- **RQ3 — Diving & editing.** Once a user picks a session, can they edit
  in it immediately, with the cursor where they expect?
- **RQ4 — Finding & filtering.** With many sessions, can the user narrow
  to the one they want (filter, scope, worktrees, trivial toggle)?
- **RQ5 — Lifecycle.** Can the user create a new session, multi-select,
  and reach session actions, with correct affordances/disabled states?
- **RQ6 — Coexistence.** Do the command palette, menus, Settings, Live
  Grep, and the file explorer keep working *with the dock up* — no
  overlap, no swallowed keys, no stuck focus?
- **RQ7 — Exit / round-trip.** Can the user dismiss the dock and return
  to ordinary editing with zero residue (no leaked key-capture, correct
  cursor, full width reclaimed)?

---

## 2. Participant persona

> **"Devon" — staff engineer, parallel-agent workflow.**
> Runs three coding agents at once, each in its own git worktree:
> `alphaproj` (refactor in progress), `betaproj` (waiting on tests),
> `gammaproj` (just spun up). Lives in the terminal; fluent with VS
> Code / Sublime keybindings; has *never* used Fresh's orchestrator
> before. Mental model: "a sidebar of my sessions, like editor tabs but
> for whole projects." Impatient with modal dialogs that trap focus.

Devon represents the target user: competent, keyboard-first, but with
**no prior knowledge of the dock's specific keys**. Where Devon has to
guess, that's the discoverability signal we care about.

---

## 3. Test environment & setup

```bash
# Build (debug — NOT --release, per the task)
cargo build --bin fresh

# Three throwaway git projects = three sessions
mkdir -p /tmp/uxtest/{alphaproj,betaproj,gammaproj}
for p in alphaproj betaproj gammaproj; do
  git -C /tmp/uxtest/$p init -q
  printf 'hello from %s\n' "$p" > /tmp/uxtest/$p/readme.txt
  git -C /tmp/uxtest/$p add -A && git -C /tmp/uxtest/$p commit -qm init
done

# tmux harness — generous size so nothing is clipped
tmux new-session -d -s ux -x 160 -y 42
tmux send-keys -t ux \
  './target/debug/fresh --log-file /tmp/ux.log /tmp/uxtest/alphaproj /tmp/uxtest/betaproj /tmp/uxtest/gammaproj' Enter
```

Capture after every interaction with
`tmux capture-pane -t ux -p` and save numbered snapshots to
`/tmp/uxrun/NN_label.txt` for the findings doc.

**Reading the dock:** the focused region's **right-edge divider** wears
the accent (cursor) colour; blurred, it's muted. Session rows show a
status glyph (`*` working / `✓` idle), a `[ ]`/`[x]` checkbox, the name
(bold = active), and a project/`on-disk` tag.

---

## 4. Severity rating scale (NN/g)

| Rating | Meaning |
|:------:|---------|
| **0** | Not a usability problem. |
| **1** | Cosmetic — fix only if spare time. |
| **2** | Minor — low priority. |
| **3** | Major — high priority; users are blocked or badly slowed. |
| **4** | Catastrophe — must fix before ship (data loss, dead end, crash). |

Each finding also gets the heuristic it violates (Nielsen's 10) and a
fix suggestion.

---

## 5. Moderator script

1. **Intro (not recorded as data).** "This is a test of the software,
   not of you. Think aloud — tell me what you see, what you expect, and
   what you're trying to do. There are no wrong answers; if you're
   stuck, that's useful for us."
2. **Warm-up.** "Tell me, just by looking — what is this editor showing
   you right now?" (Baseline mental model before the dock exists.)
3. **Run the tasks below in order.** Hand over the *goal*, never the
   keystroke. Stay silent while Devon explores; only unblock after a
   genuine dead end, and record it as a failure.
4. **Post-task probe** after each: "How easy or hard was that (1–7)?
   What, if anything, surprised you?"
5. **Debrief.** SUS-style closers in §7.

---

## 6. Task scenarios

> Each task is a **goal**, with success criteria the moderator checks
> against the captured screen, plus the data to record. Do **not** read
> the "expected path" aloud — it's for the moderator only.

### T1 — Open the session sidebar  *(RQ1)*
**Scenario:** "You've launched the editor with three projects on the go.
You want to see all your sessions in one place. Bring up that view."
- *Expected path:* Alt+O (or `Ctrl+P → Orchestrator: Toggle Dock`).
- **Success:** a left column titled `ORCHESTRATOR` appears with the
  three sessions; the editor chrome (menu/`File`) sits to its right.
- **Record:** did Devon find it unaided? first guess? time-to-open.

### T2 — Make sense of the list  *(RQ1)*
**Scenario:** "Without doing anything yet — what is this list telling
you? Which session is active? Is any of them busy?"
- **Success:** Devon can read off the session names, identify the active
  one (bold), and interpret the status glyph (`*`/`✓`).
- **Record:** misread glyphs/labels; anything they expected but missing.

### T3 — Switch to another session  *(RQ2)*
**Scenario:** "Switch over to your `betaproj` work."
- *Expected path:* ↓ to highlight `beta` → editor live-re-roots.
- **Success:** the editor pane to the right retargets to betaproj; list
  order stays stable; Devon can tell focus is in the dock.
- **Record:** did the live-switch surprise them (good or bad)? Could
  they tell the dock vs editor had the keyboard?

### T4 — Dive in and edit  *(RQ3)*
**Scenario:** "Open `readme.txt` in that session and add a line of
text."
- *Expected path:* Enter to dive (focus → editor), edit the buffer.
- **Success:** keystrokes land in the buffer (not the dock/explorer);
  cursor visible and correct; dock stays visible (blurred).
- **Record:** **known-suspect** — does diving into a *switched* session
  land focus in the file tree instead of the buffer? (gaps doc flags
  this). Capture exactly where the first keystroke goes.

### T5 — Find a session by name  *(RQ4)*
**Scenario:** "Imagine you had 20 sessions. Jump to the one called
`gamma` by searching."
- *Expected path:* `/` focuses filter → type `gamma` → list narrows →
  Enter returns to list → ↓/Enter dive.
- **Success:** live filtering; Enter-in-filter returns to list (does NOT
  dive); navigation works on filtered results.
- **Record:** did Devon expect Enter to jump straight in? Esc behaviour.

### T6 — Spin up a new session  *(RQ5)*
**Scenario:** "Start a brand-new session for a fourth project."
- *Expected path:* `Alt+N` / `+ New Session` button → form → fill →
  submit; **dock stays visible** beside the form.
- **Success:** form opens without destroying the dock; fields legible;
  Esc cancels cleanly.
- **Record:** did the dock vanish (regression)? Were optional fields
  obvious as optional?

### T7 — Select several & reach actions  *(RQ5)*
**Scenario:** "Pick two sessions you're done with — you want to act on
both at once."
- *Expected path:* Space toggles `[x]` on rows; bulk actions
  (Stop/Archive/Delete) become reachable; disabled states correct.
- **Success:** checkboxes toggle on the right rows; an action surface
  appears; destructive actions ask for confirm.
- **Record:** could Devon tell what's selected? Were actions
  discoverable? disabled-state clarity (e.g. last window).

### T8 — Change what the list shows  *(RQ4)*
**Scenario:** "You think there are some sessions hidden. Show *all*
worktrees and any empty sessions; then limit the list to just the
current project."
- *Expected path:* `Alt+T` worktrees, `Alt+I` empty/1-file, `Alt+P`
  scope.
- **Success:** each toggle visibly changes the list / a checkbox state;
  scope label flips (this ↔ all).
- **Record:** did Devon find these toggles at all? mnemonic guessability.

### T9 — Use the rest of the editor with the dock up  *(RQ6)*
**Scenario:** "With the sidebar open, open the command palette, then
Settings, then the file explorer. Does everything still behave?"
- *Expected path:* `Ctrl+P` (blurs dock, palette opens, dock stays);
  Settings renders *beside* the dock; `Ctrl+E` explorer focuses, dock
  stays.
- **Success:** no overlap of the dock column; no swallowed keys; Esc
  returns cleanly each time.
- **Record:** any z-order overlap; any stuck focus; any key the dock ate.

### T10 — Put it away and edit normally  *(RQ7, round-trip)*
**Scenario:** "You're done orchestrating. Hide the sidebar and just edit
a file like a normal editor — then bring the sidebar back."
- *Expected path:* Alt+O (or Toggle Dock) to hide → edit/save/palette
  with **full width**, no dock residue → Alt+O to re-show (reopens
  focused, list intact).
- **Success:** chrome reclaims the full width; no leaked dock
  key-capture; cursor correct; re-show is clean. Repeat ×2 — no drift.
- **Record:** any residual dim, stuck mode, wrong cursor, or stale list.

---

## 7. Debrief questions

- "In one sentence, what is this sidebar for?"
- "What was the most confusing moment?"
- "Was there ever a point you didn't know whether typing would go to the
  sidebar or the file?"
- "Did anything feel like it might have lost your work or your place?"
- SUS-lite (1–5, strongly disagree → strongly agree):
  1. I'd find this easy to use regularly.
  2. The sidebar and the editor felt well integrated.
  3. I always knew which session was active.
  4. I always knew where my keystrokes would go.
  5. I could recover easily when I went somewhere I didn't mean to.

---

## 8. Scoring & reporting

For each task record **Completion** (✔ unaided / ◑ completed after a
hint / ✘ failed), **time/turns**, **errors**, and a **satisfaction**
1–7. Roll observations into severity-ranked findings (§4) in the
findings doc, each tagged with the violated heuristic and a concrete
fix. Lead with P3/P4 (blockers), then P2/P1, then what already works
(appendix) as a sanity check on scope.
