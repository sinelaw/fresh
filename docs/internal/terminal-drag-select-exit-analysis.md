# Terminal drag-select: why the split stays stuck in scrollback, and how it should exit

Status: **implemented** — §5's recommendation landed (see §6 for what shipped
and the deltas discovered during implementation). §§1–4 document the state
machine and defect as they stood before the change.
Scope: the live↔scrollback state machine around mouse drag-to-select on the
live terminal grid (added in PR #2701), the `jump_to_end_on_output`
selection-suppression, and the recommended automatic exit condition.
All code paths are in the shared core (`crates/fresh-editor/src/app/`), so
everything below applies identically to the TUI and the web UI; the web
bridge only forwards mouse/key events.

## 1. The current state machine

Per (split, terminal buffer) there are exactly two states. The source of
truth is `TerminalBuffer::scrollback_splits` (`app/window/mod.rs:104`) — a
split is **live** iff absent from the set, **scrollback** iff present. The
only writer is `Window::set_split_terminal_scrollback`
(`app/window/mod.rs:2379`). The `KeyContext::Terminal`/`Normal` edge is a
projection derived from that set for the focused split by
`sync_terminal_mode_flags` (`app/active_focus.rs:189`), and
`focused_terminal_live()` (`app/window/mod.rs:2410`) just reads the
projection.

### Live → scrollback (all call `set_split_terminal_scrollback(.., true)`)

| trigger | site |
|---|---|
| `Ctrl+Space` / `Ctrl+]` / `` Ctrl+` `` in terminal mode | `app/terminal.rs:1342` → `enter_terminal_scrollback` |
| `Escape` (`Action::TerminalEscape`) | `app/input.rs:2348` |
| `Shift+PageUp` | `app/input_dispatch.rs:552` |
| wheel scroll-up over the live grid | `app/mouse_input.rs:518` |
| **left-drag on the live grid** (new) | `app/terminal_mouse.rs:328` `begin_terminal_grid_selection` |
| terminal process exit (all splits) | `app/async_dispatch.rs:1043` |

### Scrollback → live (all call `enter_terminal_mode`, `app/terminal.rs:1369`)

| trigger | site |
|---|---|
| `Ctrl+Space` / `Ctrl+]` / `` Ctrl+` `` | `app/input_dispatch.rs:84` via `should_enter_terminal_mode` (`app/terminal_input.rs:96`) |
| any plain char / Enter / Tab / Backspace (key is then forwarded to the PTY) | same path |
| `Action::FocusTerminal` | `app/input.rs:2336` |
| **new PTY output**, iff `terminal.jump_to_end_on_output` (default `true`) **and no selection is active** | `app/async_dispatch.rs:767` `handle_terminal_output` |

`enter_terminal_mode` clears the split's scrollback edge, restores
`KeyContext::Terminal`, truncates the backing file back to the history end
(removing the appended visible-screen tail) and scrolls the grid to the
bottom. Note what it does **not** do: it never touches the split view
state's cursor — the selection anchor survives (this matters, §3).

### The drag-select flow through those states

1. **mouse-down** on a live grid: `handle_mouse_click` focuses the split
   and, when `terminal.mouse_drag_selects` is on, records
   `MouseState::terminal_drag_pending = (split, buffer, col, row)`
   (`app/click_handlers.rs:287-299`). State is still live — a bare click
   never leaves live mode.
2. **first drag event**: `handle_mouse_drag` sees the pending origin
   (`app/mouse_input.rs:3021-3030`) and calls
   `begin_terminal_grid_selection` (`app/terminal_mouse.rs:328`), which
   drops the split into scrollback (`set_split_terminal_scrollback(true)` +
   `sync_terminal_mode_flags` → `sync_terminal_to_buffer` pins the viewport
   so the view is pixel-identical to the grid), anchors a normal text
   selection at the press origin, and hands off to the standard
   `dragging_text_selection` machinery.
3. **mouse-up**: `clear_active_window_drag_state`
   (`app/mouse_input.rs:300`, `:4405`) clears every drag bookkeeping field
   (including `terminal_drag_pending` and `dragging_text_selection`) but
   deliberately **keeps the selection** (anchor stays set).
4. **`Ctrl+C`**: key context is `Normal` in scrollback, and Ctrl-modified
   keys don't match `should_enter_terminal_mode`, so the key resolves to
   `Action::Copy` (`app/input.rs:1168`) → `copy_selection`
   (`app/clipboard.rs:129`). `copy_selection` copies the text and — like
   every editor copy — **does not collapse the selection**.
5. Nothing else happens. There is no transition armed to fire.

## 2. Where "stuck in scrollback" comes from

The only *automatic* scrollback→live transition is the output-driven one in
`handle_terminal_output`, and it is guarded (`app/async_dispatch.rs:753-769`)
by `selection_active`:

```
dragging_text_selection || terminal_drag_pending.is_some()
    || primary cursor anchor exists and != position   (focused split)
```

That guard is correct and deliberate *before* the copy: without it, a chatty
program's next output line would call `enter_terminal_mode`, truncate the
backing buffer, and destroy the selection before the user could copy it —
the exact case drag-to-select exists for.

The trap is that the guard's third clause **outlives the copy**. After
`Ctrl+C` the selection persists, so getting back to live requires *both*
of:

- the selection must be collapsed (a bare click in the pane, or Ctrl+Space /
  typing which bypass the guard entirely), **and**
- new output must arrive afterwards.

In the most common workflow — run a command, drag-select part of its
output while the shell sits at an idle prompt, copy — *neither* happens on
its own. The shell prints nothing further, so even a user who clicks to
collapse the selection stays in scrollback indefinitely. Verified end-to-end
against the web bridge (Playwright driving `webui_server`, statusbar scene
region as the mode oracle):

```
[after drag]                       … Terminal mode disabled - read only (Ctrl+Space to resume) …
[after Ctrl+C copy]                … Copied …                     ← hint gone, still scrollback
[output arrives, selection active] … Copied …                     ← suppressed, still scrollback
[after click collapses selection]  … still scrollback (no output yet)
[next output after collapse]       … Terminal mode enabled        ← only NOW resumes
```

Two aggravating details:

- **Discoverability regression**: the "read only (Ctrl+Space to resume)"
  text is a transient status *message*; the `Copied` message from the copy
  immediately overwrites it. At exactly the moment the user is stranded, the
  UI stops telling them how to get out.
- `Ctrl+Space` "toggle" asymmetry: the same chord that resumes also drops a
  live terminal back into scrollback, so a user mashing it lands wherever
  the parity of presses leaves them.

## 3. Confirmed latent bug: the phantom selection

`sync_terminal_to_buffer` (`app/terminal.rs:1678`) reloads the buffer and
pins the *primary cursor position* to the anchor byte
(`app/terminal.rs:1763`), but never clears the cursor's selection
`anchor`. `enter_terminal_mode` doesn't either. So the anchor set by a drag
survives a `Ctrl+Space` resume, and the *next* entry into scrollback — for
any reason — resurrects it against freshly re-synced buffer contents:

```
drag-select → Ctrl+C → Ctrl+Space (resume, anchor NOT cleared)
→ Ctrl+Space (or wheel-up) back into scrollback
→ a phantom selection the user never made is rendered
   (observed: an entire prompt line highlighted)
→ selection_active is true → output-driven auto-resume is suppressed
→ this split now NEVER auto-resumes again (until a click/typing)
```

Reproduced end-to-end: after the resume/re-enter cycle, output arrived and
the split stayed `read only` with a visible bogus highlight. This is worse
than the reported complaint — it detaches the suppression from any real
selection — and any fix for the exit condition should also clear the anchor
on the scrollback↔live transitions.

## 4. Evaluating the candidate exit conditions

The design constraint everything must respect: the selection must survive —
against arbitrary PTY output — from mouse-up until the user has copied it,
and the protections the current design gets right must not regress
(bare click keeps the terminal live; `Ctrl+Space` keeps working; a user
deliberately reading scrollback must not be yanked to the bottom).

**(a) Resume on mouse-up ending the drag.** Non-starter. The selection's
byte ranges refer to the synced scrollback buffer; `enter_terminal_mode`
truncates that buffer's backing file and the live grid has no selection
model to migrate into, so resuming at mouse-up destroys the selection
before it can be copied. Copy-on-mouse-up (X11 primary-selection style)
would sidestep that but silently clobbers the clipboard on every drag —
not this editor's clipboard model, and it would still leave "drag to
scroll-and-read" impossible.

**(b) Resume when the copy completes.** Matches the gesture: drag → copy →
done is one arc, and copy is its natural end. Works in the idle-shell case
(no output needed). Two problems if applied *unconditionally* to every
terminal-scrollback copy: a user who entered scrollback explicitly
(`Ctrl+Space`, wheel) and copied something from deep history gets yanked to
the bottom, losing their reading position — a regression of the manual
scrollback model; and copy-without-selection (line copy) resuming would be
bizarre.

**(c) Resume when the selection is cleared (click collapses it).** Fixes
the "clicked away but still stuck" leg (today that click only removes the
suppression; it still needs output to fire). Doesn't touch the primary
complaint on its own — after copy the selection is still there, so the user
must additionally click. Same caveat as (b): unconditional, it breaks
click-to-place-cursor for deliberate scrollback readers.

**(d) Re-enable `jump_to_end_on_output` once the copy is done** (collapse
the selection on copy, keep waiting for output). Least surprising visually
(no immediate viewport jump), preserves all current protections — but it
does **not** fix the reported case: an idle shell emits nothing, so the
user who copied at a quiet prompt stays stuck exactly as today. It's a
component of a fix, not a fix.

**(e) Never auto-resume; improve discoverability.** This is the status quo
users are complaining about, and the status-message overwrite (§2) means
the one breadcrumb disappears on copy. Rejected as the primary answer,
though the discoverability half (a persistent scrollback statusbar segment
rather than a transient message) is worth doing regardless.

### The missing distinction

Every option is wrong *unconditionally* and right *conditionally*, and the
condition is the same one each time: **who initiated scrollback**. A drag
on the live grid enters scrollback as an implementation detail — the user
never asked to leave the live terminal, so the detour should end as
automatically as it began. `Ctrl+Space` / `Shift+PageUp` / wheel-up is an
explicit request for a stable reading view — the current "resume manually,
or on output once no selection pins the view" model is right for it and
must not change.

## 5. Recommendation

Track drag-initiated ("implicit") scrollback separately, and end it when
the selection gesture ends:

1. **Copy resumes.** `Ctrl+C` (or Edit▸Copy — same `Action::Copy`) with an
   active selection while the focused split is in *implicit* scrollback:
   copy, collapse the selection, `enter_terminal_mode`. Immediate, output
   not required — fixes the idle-shell case, which is the complaint.
2. **Click-away resumes.** A bare mouse-down in an implicit-scrollback pane
   collapses the selection and resumes live. The split then behaves exactly
   like the live grid it appears to be: click = focus, click-then-drag =
   new selection (the mouse-down re-records `terminal_drag_pending` via the
   existing live-grid path). Selection abandoned = back to a normal
   terminal, matching plain-terminal muscle memory.
3. **Engaging with scrollback converts it to explicit.** Wheel/keyboard
   scrolling (or `Shift+PageUp`, or paging keys) while in implicit
   scrollback clears the implicit marker: the user is now *reading*, and
   from then on the existing explicit-scrollback rules apply — copy does
   NOT yank them, output resumes only once no selection is active. This is
   the line that protects the "select, then scroll up to check something,
   then copy" flow from surprise jumps.
4. **Clear the anchor on transition** (both `enter_terminal_mode` and the
   cursor-pinning in `sync_terminal_to_buffer`): fixes the phantom-selection
   bug (§3) for every path, independent of the rest.
5. **Leave `handle_terminal_output` untouched.** The suppression is still
   exactly right for the window between mouse-up and copy (in both modes),
   and for explicit-scrollback selections after it.

What this preserves: bare click on the live grid still only focuses
(nothing changes before a drag starts); a selection still survives chatty
output until copied; `Ctrl+Space` still toggles both ways;
`terminal.mouse_drag_selects = false` still disables the whole feature
(no pending origin is ever recorded); explicit scrollback UX is unchanged.

Known accepted edge: after a copy-resume, a *second* `Ctrl+C` goes to the
PTY as SIGINT (the selection is gone, the split is live). This mirrors
VS Code's "Ctrl+C copies iff a terminal selection exists" behavior and is
the standard resolution of that ambiguity, but it's worth a line in the
docs. No new config knob initially; if field feedback wants one,
`terminal.resume_after_copy: bool` (default `true`) slots cleanly next to
`mouse_drag_selects`.

### Functions that change

| change | site |
|---|---|
| add per-split "implicit (drag-initiated)" marker alongside `scrollback_splits`, e.g. `drag_scrollback: HashSet<LeafId>`; cleared whenever the split leaves scrollback or the marker is downgraded | `TerminalBuffer`, `app/window/mod.rs:86` (+ accessors near `:2369`, pruned in `forget_split_terminal_modes`) |
| set the marker when the drag drops the split into scrollback | `begin_terminal_grid_selection`, `app/terminal_mouse.rs:328` |
| explicit entries never set (and clear any stale) marker | `enter_terminal_scrollback` `app/terminal.rs:1442`, `DeferredAction::EnterScrollbackMode` `app/input_dispatch.rs:552`, wheel-up entry `app/mouse_input.rs:518` |
| after `copy_selection` with a selection, if focused split is implicit-scrollback: collapse selection + `enter_terminal_mode` | `Action::Copy` arm, `app/input.rs:1168` (keeps `copy_selection` itself terminal-agnostic) |
| bare mouse-down on an implicit-scrollback terminal pane: resume live first, then fall into the existing live-grid click branch (focus + record pending origin) | `handle_mouse_click` terminal branch, `app/click_handlers.rs:287` |
| scrolling an implicit-scrollback split downgrades the marker to explicit | scroll path for scrollback panes, `app/mouse_input.rs` around `:518` / `handle_mouse_scroll` |
| clear primary-cursor `anchor` when resuming live / when pinning the cursor on sync | `enter_terminal_mode` `app/terminal.rs:1369`, `sync_terminal_to_buffer` `app/terminal.rs:1760-1768` |

### Tests that need to move

- `web-ui/test/drive.mjs` terminal-selection block (lines ~708-755)
  currently asserts the *old* contract: it presses `Ctrl+Space` after the
  copy and expects that to resume. Under the recommendation the copy itself
  resumes; the block should assert auto-resume on copy, then verify
  `Ctrl+Space` still round-trips, and keep the bare-click-stays-live check.
- New core e2e tests (`tests/e2e/terminal.rs`): copy-resumes-implicit,
  click-away-resumes-implicit, scroll-converts-implicit-to-explicit (copy
  then does NOT resume), explicit-scrollback copy does NOT resume, and a
  regression test for the phantom selection (drag → copy → resume →
  re-enter scrollback → new output must resume; no selection rendered).

## 6. What shipped (implementation notes)

The recommendation in §5 was implemented as specified, with three deltas
discovered during implementation:

- **Double/triple-click select on the live grid.** With click-away-resume in
  place, a double-click's first press resumes an implicit-scrollback pane, so
  its second press lands on the live grid — which used to be inert. Rather
  than regress word-select, the live grid now supports it: double-click
  selects the word (word-wise drag extension included) and triple-click the
  line, through the same implicit-scrollback detour as a drag
  (`begin_terminal_grid_word_selection` / `begin_terminal_grid_line_selection`,
  gated on `terminal.mouse_drag_selects`). Copying those selections resumes
  the grid like any drag selection.
- **Render-independent drag resolution on terminal scrollback.** Drag events
  processed after the live→scrollback flip but before the next render used to
  resolve against view-line mappings cached from a *previous* buffer view of
  the split, throwing the selection head far from the pointer (reproducible
  in the web bridge, whose event bursts outrun renders, and in the test
  harness). Terminal scrollback is unwrapped and gutter-free, so
  `handle_text_selection_drag` now resolves those positions directly
  (`terminal_grid_byte_at`: viewport top line + row, columns 1:1 plus
  horizontal scroll) with no cache dependency.
- **Scrollbar interaction also downgrades.** Grabbing the scrollback view's
  scrollbar is scrollback *reading*, so it clears the implicit marker exactly
  like wheel scrolling.

The phantom-selection fix is the `enter_terminal_mode` clear only. An earlier
draft also cleared selections in `sync_terminal_to_buffer`, but that path
runs on every focus change of a scrollback split and would have destroyed
legitimate selections on refocus; every resume goes through
`enter_terminal_mode`, which is sufficient.

The `Copy` status message reads "Copied - terminal resumed" when the copy
resumes the grid, keeping the mode change visible.

Verified by: 6 new core e2e tests (`cargo test -p fresh-editor --test
e2e_tests terminal_drag_select` + explicit/phantom/double-click tests), the
updated `web-ui/test/drive.mjs` terminal block (copy auto-resumes, Ctrl+Space
round-trips, bare click stays live — 140/140 checks green), and a manual
Playwright sweep of all five exit rules against the web bridge.

**Known residual (pre-existing, out of scope):** under many repeated
enter/exit cycles in the *web bridge*, the scrollback backing file
accumulates stale copies of the visible screen (an un-truncated re-append per
cycle somewhere in the web-only flow), which can momentarily corrupt a drag's
selection extent mid-gesture and leave stale highlight cells in the pane.
The same interleaving is clean in the core harness
(single-cycle and 4-cycle drag/copy/click/wheel sequences show no
accumulation: backing file stays minimal, selections exact). This reproduces
identically on the pre-change code — the change strictly improves it (the
output-suppression no longer wedges) — and should be chased as a separate
web-bridge/backing-file issue.

## 7. Repro notes (web bridge)

Build and run `cargo build --features web -p fresh-editor --example
webui_server`, start it on a port, then drive with Playwright (Chromium at
`/opt/pw-browsers/chromium`). Mode oracle: the statusbar scene region
(`window.fresh.scene.regions.statusbar`) — "read only (Ctrl+Space to
resume)" vs "Terminal mode enabled". To make output arrive without typing
(typing would itself resume), schedule it before dragging:
`echo MARKER; (sleep 6; echo LATER-1; sleep 6; echo LATER-2) &`, then
drag over the MARKER row, `Ctrl+C`, and observe: LATER-1 does not resume
(selection alive), a bare click doesn't either (no output), LATER-2 after
the click does. The phantom-selection repro is: drag → `Ctrl+Space` →
`Ctrl+Space` → observe the bogus highlight and that LATER-3 never resumes.
