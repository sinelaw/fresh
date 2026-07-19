# Default Pane Source (Split / New-Pane / Empty-State unification)

> _Forward-looking UX design, not a description of what ships today._ This
> document proposes unifying the three disconnected moments where Fresh decides
> "what goes in a pane" — splitting, adding a pane/tab, and the last-buffer
> void — behind a single, per-workspace **default pane source**.
>
> Convention note: this is a PLANNED doc. It is deliberately UX-first;
> implementation is sketched only where it grounds a UX claim. Where a claim
> about current behavior disagrees with the source, the source wins.

---

## 1. Motivation — one decision, made in three unrelated places

Today, "what content a new pane holds" is answered independently — and
inconsistently — in three places:

| Moment | Current behavior | Where |
|--------|------------------|-------|
| **Split a pane** | Always **mirrors** the focused pane's active buffer (with a fresh cursor). No prompt, no choice. | `app/split_actions.rs` (`split_pane_impl`) |
| **Open a terminal** | Always spawns the user's **shell**. No terminal-vs-agent choice in-window. | `app/terminal.rs` (`open_terminal_in_window`, `open_terminal_split`) |
| **Pick a coding agent** | The **only** place with an agent picker (terminal / claude / aider / custom) — but it creates a whole new **window/session**, not a pane. | `plugins/orchestrator.ts` (`agentPresets`) New Session form |
| **Close the last buffer** | Forks two ways via one config bool: a visible `[No Name]` buffer, or a hidden **placeholder** pane with a centered hint. | `app/buffer_close.rs` (`resolve_close_replacement`) |

The important observation from the code: **"terminal vs coding agent" is not a
pane/buffer type in the core.** An agent is just a terminal (`virtual_buffer`
of mode `"terminal"`) whose PTY runs `claude` instead of a bare shell. So the
distinction the user wants to unify is already only skin-deep — this is a
UX/conceptual consolidation, not a data-model rewrite.

**Goal:** make *"what fills a pane"* a single first-class concept — the
**pane source** — selectable where it matters, with a configurable
**default** per workspace, so the fast paths stay fast and terminals / agents /
buffers become peers rather than three special cases.

---

## 2. Concept: the pane source

A **pane source** is a named recipe for the content of a newly-created pane.
The full set:

| Source | New pane contains | Cost |
|--------|-------------------|------|
| **Mirror** | the focused pane's active buffer (today's split behavior) | free |
| **Empty buffer** | a fresh unsaved scratch buffer (`[No Name]`) | free |
| **New file…** | opens the file finder into the new pane | free |
| **Terminal** | shell PTY | spawns a process |
| **Claude** | agent PTY running `claude` | spawns a process |
| **aider / custom…** | agent PTY running a custom command | spawns a process |

The last three are **process-backed**; the first three are free. This split
matters for the empty-state rules in §5.

### Naming

Surface this to users as **"Default pane source"** — *not* "default buffer"
(sounds like *a* buffer, not a rule) and *not* "default split source" (too
narrow now that it also governs the empty state). Reserve "buffer" for actual
content. The internal working title may differ.

---

## 3. The three moments, unified

All three collapse onto one resolver — conceptually `pane_of(source)` — so the
same source list drives every entry point and a pane is indistinguishable
regardless of how it was born.

| Moment | With this feature |
|--------|-------------------|
| **Split** | Fast path splits into the **default source**; a `Split with…` variant opens the source menu at the split site. |
| **`+` button** | The existing "New Terminal / New File" grows into the full source menu (adds Claude / aider / custom). |
| **Close last buffer** | Fills the void with the **default source** instead of the hard-coded `[No Name]`-or-placeholder fork. The current config bool becomes two ordinary entries (`Empty buffer`, `Placeholder`) in the same menu. |

This is what makes it feel like *one* feature: split, `+`, and the void all
resolve through the same "give me a pane of source X" logic.

---

## 4. Interaction model — keep the fast path, add a choice path

Splitting is high-frequency; "always ask" would punish muscle memory. Two tiers:

**1. Fast path (unchanged speed).** `Split Right` / `Split Down` immediately
create a pane of the **default source** — no prompt. The only change from today
is that the default is *configurable* rather than hard-coded to `Mirror`.

**2. Choice path (one extra keystroke).** A `Split with…` variant (chord,
held modifier, or palette command) opens the source menu at the split location,
with the current default pre-highlighted:

```
┌───────────────┬──────────────────────┐
│               │ ▸ Mirror this pane    │
│   editor.rs   │   Empty buffer        │
│               │   Terminal            │
│               │ ★ Claude              │  ← ★ = current default
│               │   aider               │
│               │   Custom command…     │
│               │   Pick file…          │
└───────────────┴──────────────────────┘
```

Arrows / number keys / click to choose; `Enter` accepts the highlighted
default. The choice path costs one keystroke, not a context switch.

---

## 5. The empty state (last-buffer void)

Confirmed against the source: **the last split cannot be closed** (Fresh refuses
it with a "cannot close" status and keeps the pane), so the real void is
**closing the last buffer/tab**, handled in one place (`resolve_close_replacement`).
Because this fires *automatically* — the user closed something, they did not
request a new pane — process-backed sources need extra care.

### Rules

1. **`Mirror` has no referent when there's nothing to mirror.** If the default
   resolves to `Mirror` (or `Last used` with no history), the void **falls back
   to `Empty buffer`** — the safe, side-effect-free choice.

2. **Never auto-spawn a process as a *fallback*.** A process only launches in
   the void if a process-backed source is *explicitly* the configured default.

3. **Free defaults render instantly** (`Empty buffer`, `New file`,
   `Placeholder`).

4. **Process-backed defaults use a lazy placeholder** rather than launching
   immediately. Fresh already has a placeholder render path (a flagged empty
   buffer + a centered dimmed hint line). Reuse it with a source-specific hint,
   e.g.:

   ```
                    ⏎ Start Claude   ·   Ctrl+O open file   ·   Esc empty buffer
   ```

   This honors the workspace's intent, spends nothing (no process, no tokens)
   until the first keystroke, and — crucially — keeps a single-key path to
   just close/quit. Launching `claude` outright is the simpler alternative but
   risks surprising someone who closed everything meaning to exit.

### Why lazy is the default recommendation

In many editors, "close the last thing" is how people *quit*. A workspace
default that silently respawns a running process makes the app feel
un-closable. The lazy placeholder resolves this for free: the home screen is
not a process you must kill, yet it still expresses the workspace's identity
(a Claude workspace lands on a "Start Claude" home; an ops workspace on a
terminal home; a writing workspace on an empty buffer).

---

## 6. What "default" means — the setting

- **Per-workspace, not just global.** A window *is* an agent session in Fresh,
  so the default pane source is a natural per-workspace property. A Claude
  session might default to `Mirror` (you're reading code) while an ops
  workspace defaults to `Terminal`. A global fallback covers unconfigured
  workspaces.

- **Subsumes an existing toggle.** `auto_create_empty_buffer_on_last_buffer_close`
  is effectively a two-value version of "what fills the void" (`Empty buffer`
  vs `Placeholder`). The new setting generalizes that boolean into the full
  source enum — richer capability, not net-new surface area. Migration: `true`
  → default `Empty buffer`; `false` → default `Placeholder`.

- **`Last used` as an adaptive option.** Offer `Last used` as a selectable
  default: the default silently becomes whatever source you last picked in the
  choice menu. Zero config, adapts within a session; less predictable, so it is
  *one of* the options, not the only behavior.

---

## 7. UX details to settle before build

- **Focus after creation.** Process-backed sources (Terminal / Claude) should
  focus the new pane — you want to type into it immediately. `Mirror` may keep
  focus on the source pane. Decide deliberately, per source.

- **Visual identity.** Give agent panes a distinct tab affordance (e.g.
  `◆ claude`) so a Claude pane is instantly distinguishable from a shell or a
  file. This is what makes "several different possibilities" legible at a glance.

- **Closing a running agent pane.** Reuse the terminal-close guard: confirm
  ("Claude is running — close anyway?") before tearing down a pane with a live
  process.

- **Consistency across entry points.** A pane of source X must look and behave
  identically whether it came from a split, the `+` button, or the void — same
  tab identity, same focus rule. The user should never be able to tell "how"
  the pane was born.

- **Direction-aware defaults (optional, later).** Some editors treat a vertical
  split (side-by-side) as "compare" → `Mirror`, and a horizontal split (below)
  as "run/observe" → `Terminal`/agent. Powerful but adds a rule to learn. Ship a
  single default first; add per-direction only if users ask.

---

## 8. Alternatives considered

- **A — Configurable default + optional choice menu (RECOMMENDED).** §4–§6.
  Keeps today's split speed, choice is one keystroke away, default is
  per-workspace, empty state included. Lowest disruption, highest payoff.

- **B — Always show the menu on split.** Cleanest mental model and best
  discoverability, but taxes high-frequency splitters every time. Good as an
  *opt-in* ("always prompt on split"), not as the only mode.

- **C — Leave split alone; add a `New Pane → source → direction` command.**
  Least risk to muscle memory, but does *not* deliver the core ask — split still
  mirrors, so "the default drives the split" is lost. Fallback only.

- **D — Source-aware `+` button, no split changes.** Extend the tab `+` menu to
  the full source list. Cheapest; ships the unification for *tabs*, not splits.
  Best as a **companion to A** so the same menu appears at both the `+` and the
  split.

**Chosen:** **A, with D as a companion** — one shared source menu, surfaced at
split-time and at the `+`, backed by a per-workspace default, with the empty
state resolving through the same rule.

---

## 9. Open questions

1. Scope of the default: per-workspace only, or also per-window-role (e.g. the
   Utility Dock)?
2. `Last used` — session-scoped or persisted across restarts?
3. Should `Custom command…` remembered entries appear as their own named
   sources in the menu (like the Orchestrator's custom agents)?
4. Direction-aware defaults — worth the added rule, or a settings-page trap?
5. For the lazy placeholder, is a single hint line enough, or should the empty
   state grow into a proper "workspace home" surface (recent files, session
   actions)?

---

## 10. Grounding references (current behavior)

- Split mirrors active buffer: `crates/fresh-editor/src/app/split_actions.rs`
  (`split_pane_impl`).
- Terminal split seeds a fresh terminal (the pattern for non-mirrored splits):
  `crates/fresh-editor/src/app/terminal.rs` (`open_terminal_split`).
- Agent picker (terminal / claude / aider / custom) lives only in the New
  Session form: `crates/fresh-editor/plugins/orchestrator.ts` (`agentPresets`,
  `AGENT_REGISTRY`).
- Last-buffer void resolver and the `[No Name]`-vs-placeholder fork:
  `crates/fresh-editor/src/app/buffer_close.rs` (`resolve_close_replacement`),
  config `auto_create_empty_buffer_on_last_buffer_close`
  (`crates/fresh-editor/src/config.rs`).
- Placeholder render path to reuse for lazy sources:
  `crates/fresh-editor/src/view/ui/split_rendering/orchestration/mod.rs`
  (`render_placeholder_hint`); `synthetic_placeholder` flag in
  `crates/fresh-editor/src/app/types/buffer_meta.rs`.
- The last split cannot be closed: `close_active_split` /
  `SplitManager::close_split` (status `split.cannot_close`).
