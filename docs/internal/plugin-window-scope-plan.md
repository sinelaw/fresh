# Window scope for plugin state and chrome

> _Design note. Status: phases 0–5 **IMPLEMENTED** (sinelaw/fresh#3327);
> phase 6 is the open decision it always was. Answers sinelaw/fresh#3326
> ("Sidebar sections: unreachable by keyboard, editor-global across windows,
> and stale/dead panels"): what was found, why the plugin API makes the
> mistake easy, and the phased plan that fixed the reported defects and
> closed the class they belong to. The defects were reproduced
> interactively; the analysis is from the source. Where this note and the
> code disagree, the code wins. As built: the announcer is
> `app::focus_announcer`, the scopes are `app::sidebar::SectionScope` with
> out-of-scope sections *parked* rather than hidden (so the column, the focus
> cycle and hit-testing never see them), restored placeholders expire after a
> few frames' grace with their layout kept as a hint, and the foreign-id
> check is `Editor::plugin_buffer_in_active_window`._

---

## 1. What was found

The trigger was a small question — can the keyboard reach the Markdown
"Contents" section? — and the answer surfaced a chain of defects that share one
root. Lettered as in the issue:

| | Defect | Root |
|---|---|---|
| A | `focus_next_sidebar_section` is the only keyboard entry to a plugin section and is bound in no shipped keymap; undocumented, untested | reach |
| B | `markdown_toc` registers no command that focuses its own section | reach |
| C | a section can be "ON" while the sidebar column is hidden; nothing reveals | reach |
| D | sidebar sections are one editor-global list; a window shows another window's outline | **scope** |
| D′ | activating a row of that foreign outline opens a *second, independent* buffer of the file in the current window — two unsynchronised copies of one path, last save wins | **scope** |
| E | `set_active_window` fires `active_window_changed` but not `buffer_activated`; 12 of the 14 bundled plugins that track the active buffer go stale on a window switch | **lifecycle** |
| F | a section restored from a workspace whose plugin never re-mounts it is a "Panel unavailable" placeholder forever, re-saved on every quit | lifecycle |
| G | the outline follows the viewport only when the section itself has focus; with the *explorer* holding the keyboard the plugin cannot tell and the outline stops following (§5.5 of the sidebar design) | signal |
| H | the explorer's reveal is silent when it cannot reveal (unnamed buffer, out-of-root file); the status message forks and is clobbered | feedback |

The three bold roots — scope, lifecycle, and the signal a plugin gets about
focus — are one problem seen from three sides: **the API hands plugins ids
and events with no window attached, resolves everything against the active
window without saying so, and leaves lifecycle bookkeeping to each plugin.**
A, B, C and H are ordinary gaps and are handled in the same plan because they
touch the same code.

## 2. The model as it stands

Five facts about the plugin seat explain every defect above.

**Two id spaces, one invisible.** Buffers and splits are per-window: opening
the same file in two windows makes two independent buffers with two ids. The
API is built on those ids — roughly a hundred methods take a `bufferId`, two
dozen a `splitId`, a handful a `windowId`. Nothing says which window an id
belongs to: `BufferInfo` has no window field, and the buffer hooks
(`buffer_activated`, `buffer_closed`, `after_file_open`, `cursor_moved`,
`viewport_changed`) carry `buffer_id` alone. Only the PTY hooks
(`terminal_output`, `terminal_exit`) carry a `window_id`. So a plugin's natural
cache is a naked buffer id, which is exactly what `markdown_toc` holds.

**Everything resolves against the active window, silently.** The state
snapshot plugins read is populated from the active window only, so
`getBufferInfo` on another window's id returns `null`. Buffer-addressed
commands (`getBufferText`, `setViewMode`, …) look the id up in the active
window's map and do nothing when it is absent. A stale id from another window
is indistinguishable from a closed buffer, and neither case logs.

**The lifecycle vocabulary has a hole at the window boundary.**
`buffer_activated` is fired from ten separate call sites — every path that
changes the active buffer remembers to call `run_hook` by hand — and the window
switch is the path that does not. Closing a window fires `window_closed` only,
never `buffer_closed` for the buffers it drops. The two hooks plugins key their
state on are the two that do not cross windows.

**Mounted chrome has no owner beyond the plugin.** A panel is keyed
`(plugin, id)`. The host cannot know when a panel's subject is gone; the plugin
has to, and the previous fact means it cannot. All three plugin-panel slots —
the centred modal, the dock, the sidebar sections — are editor-global. The dock
is global *by design* (it lists every workspace); the sidebar inherited the
decision without the justification, and its own design note had already listed
"per-window or per-workspace?" as an open question that should follow the
explorer. Persistence, meanwhile, is per-window: each workspace file carries
its own `file_explorer.sections`. The saved data model is ahead of the runtime.

**The right tools exist but are opt-in and scattered.** Per-window,
per-plugin, persisted state (`setWindowState`) is used by two plugins;
`activeWindow()`/`listWindows()` by three; `openFileInBackground` and
`createTerminal` already accept a target window; `run_hook_for_plugin` already
delivers a hook to one plugin. The one plugin that handles a window switch
correctly does so by re-running "apply for the active window" on *both* hooks —
bookkeeping every plugin must rediscover.

## 3. What "robust" has to mean

The plan is judged against these, in order:

1. **Identity carries scope.** Anything a plugin can hold across turns — a
   buffer, a split, a panel — names the window it belongs to, in the value
   itself, not in documentation.
2. **The narrow scope is the default.** A plugin that says nothing gets the
   window-scoped behaviour. Editor-global has to be asked for by name, so no
   surface can inherit the dock's default by accident again.
3. **The host owns lifecycle.** When a panel's subject goes away, the host
   hides or removes the panel and tells the plugin. A plugin may still do its
   own bookkeeping; it must not have to.
4. **One announcer.** Focus changes are announced by comparing state, not by
   remembering to call a hook on each path.
5. **Wrongness is loud.** A foreign id, a section pointing at a buffer that is
   not on screen, a placeholder outliving plugin load: each is detectable by
   the host and is detected in debug builds and tests.

## 4. The plan

Phases are ordered by leverage: each earlier phase reduces the blast radius of
the later ones. Phases 1–3 fix every lettered defect; 4–5 are what keeps them
fixed; 6 is a decision, not a task.

### Phase 0 — pin the defects

Regression tests first, on the existing harness (`create_window_at` +
`set_active_window` is the multi-window pattern the orchestrator tests use):

- Open a `.md` in window A; switch to window B whose active buffer is not
  Markdown; assert no Contents section is *visible*; switch back; assert it is.
- Same, with a second `.md` in window B; assert each window shows its own
  outline, and that activating a row never opens a file in the other window
  (D′).
- Restore a workspace naming a section no loaded plugin mounts; assert no
  "Panel unavailable" placeholder survives plugin load.
- Hook parity: for every path that changes `(active_window, active_split,
  active_buffer)`, assert the hooks a plugin would need arrived — this is the
  test that fails today on `set_active_window` and on `close_window`.
- The focus cycle: explorer → each section → editor, from a hidden sidebar
  and from a collapsed section.

### Phase 1 — one focus announcer (fixes E for all plugins at once)

Replace the ten hand-placed `buffer_activated` calls with one
`announce_focus` step that diffs the active `(window, split, buffer)` triple
against the last announced one and fires `active_window_changed`,
`buffer_deactivated` and `buffer_activated` on change. Run it where the plugin
state snapshot is already refreshed before hook dispatch — that refresh's own
comment says it "captures window create/close and focus changes without
hooking each site"; this is the same trick one level up. The existing call
sites can stay as no-ops during migration (the announcer is idempotent) and be
deleted once the parity test is green.

Also: closing a window fires `buffer_closed` for each buffer it drops, before
`window_closed`.

Impact: no plugin changes; `markdown_toc` and `welcome_screen` start
unmounting on a window switch by their existing handlers; `git_statusbar`,
`diagnostics_panel` and the rest refresh. Cost: one triple comparison per
snapshot refresh.

### Phase 2 — put the window in the identity plugins hold (A, F-loud)

- `BufferInfo.window_id`, and `window_id` on every buffer- or split-bearing
  hook payload: `buffer_activated`, `buffer_deactivated`, `buffer_closed`,
  `after_file_open`, `before/after_file_save`, `cursor_moved`,
  `viewport_changed`, `after_insert`, `after_delete`, `widget_event`. The
  Rust side is an added field on each `HookArgs` variant; the TypeScript types
  are regenerated. Existing plugins ignore the field and keep working.
- A composed hook, `active_buffer_changed { window_id, buffer_id, previous,
  reason }` with `reason` one of `buffer | window | split | open | restore`.
  Twelve of the fourteen listeners want "the thing the user is looking at
  changed"; today they approximate it with `buffer_activated` alone. The old
  hooks stay; the bundled plugins migrate to the composed one as they are
  touched.
- Debug builds and tests: a buffer-addressed command whose id is not in the
  active window logs at `warn`, and panics under the test harness. This alone
  would have caught every one of the twelve.

### Phase 3 — scoped chrome (D, D′, F, C)

Give every mounted panel a **scope**, declared at mount:

```ts
mountSidebarSection(id, spec, title, rows, {
  scope?: { buffer: number } | { window: number } | "editor",  // default: { window: activeWindow() }
  reveal?: boolean,                                            // show the column and the section
})
```

The same option on the centred modal and, for completeness, on the dock
(whose only sensible value is `"editor"`, which the orchestrator states
explicitly).

Host behaviour by scope:

- **buffer** — visible only while that buffer is the active buffer of the
  active window; hidden otherwise (layout keeps its rows); removed on that
  buffer's `buffer_closed`, with the panel's `cancel` event delivered to the
  owner. Never persisted: the plugin recreates it from the buffer.
- **window** — visible only while that window is active; removed on
  `window_closed`; persisted in that window's workspace file only.
- **editor** — today's behaviour; persisted in every workspace file, as now.

Storage: keep the one list on the editor (an `"editor"` section has to live
somewhere all windows can see) and add `scope` to `SidebarSection`; the frame
description filters by scope against the active window and buffer, and the
focus cycle skips hidden sections. The alternative — a list per `Window`,
matching persistence — was considered and rejected only because an
editor-scoped section would then need replicating; if no editor-scoped
section ever materialises, moving the list is a later simplification.

Placeholders: a restored section becomes a placeholder marked *pending*, kept
until plugins have loaded plus one activation of its owning plugin, then
dropped. Its rows and collapsed state survive as a layout hint keyed
`(plugin, id)`, so a later mount lands where the user left it. A section the
plugin marks non-persistent is never written.

`reveal`: mounting with `reveal: true` (and `floatingPanelControl(id,
"reveal")`) shows the sidebar column and un-collapses the section — the same
pair `focus_sidebar_section` already performs. "ON" then means visible.

`markdown_toc` and `welcome_screen` mount with `{ buffer }` and delete their
own unmount-on-activate code. `markdown_toc` additionally registers
`Markdown: Focus Contents`.

### Phase 4 — chrome focus as a signal (G) and honest feedback (H)

- A `chrome_focus_changed { region: "editor" | "explorer" | "dock" | "section",
  panel_id? }` hook, fired by the announcer from the same diff (the key
  context and the focused panel are both already state). The §5.5 rule —
  the viewport drives the outline whenever the pane does not have the
  keyboard — becomes answerable without each plugin guessing.
- `sync_file_explorer_to_active_file` reports why it did not move: a status
  line for "no file behind this buffer" and "file is outside the project".
  One status wording for opening and focusing the explorer, and the async
  "ready" message no longer overwrites it (it is appended or dropped when a
  more specific message is newer).

### Phase 5 — keyboard reach (A, B)

- Bind `focus_next_sidebar_section` in every shipped keymap, in the
  `normal`, `fileExplorer`, `dock` and `terminal` contexts, the way
  `toggle_dock_focus` is duplicated per context. Add a `focus_prev`
  counterpart. Document the cycle where the explorer's keys are documented.
- `floatingPanelControl(id, "focus")` gets a command-registration helper so
  a plugin's "Focus <section>" command is one line.

### Phase 6 — a decision, not a task: cross-window addressing

Buffer ids are already globally unique (one allocator). Resolving
buffer-addressed commands across all windows would turn a stale id from a
silent no-op into either the right effect or a loud failure, and would make
`openFileInBackground(path, windowId)` and `createTerminal(window_id)` the rule
rather than the exceptions. It also lets a plugin act on a window the user is
not looking at, which is a capability question for the sandbox and for the
orchestrator's "the dock lives above windows" model. Phase 2's `warn` gives the
data to decide: if the log shows bundled plugins routinely holding foreign ids
after Phase 1, resolve; if it is quiet, the invisible id space was the whole
problem and the narrower fix is enough.

## 5. Invariants the host checks

Stated so they can be asserted in debug builds and in the e2e suite after
every render:

1. Every visible buffer-scoped section names the active buffer of the active
   window.
2. Every visible window-scoped section names the active window.
3. No placeholder section exists once plugins have loaded and its owner has
   been activated once.
4. The announced focus triple equals the actual one.
5. A buffer-addressed plugin command names a buffer in the active window
   (until Phase 6 says otherwise).

## 6. What ships independently

Phases 0, 1, 2 and 5 are independent of each other and of the rest; each is a
small PR. Phase 3 depends on 1 (removal on `buffer_closed` needs the hook to
fire on window close) and is the one that changes a plugin-facing signature —
additively, with a default. Phase 4 depends on 1 for its signal. The order
above is the recommended order, not a dependency chain.

## 7. Open questions

- Should `"editor"` scope exist for sidebar sections at all, or is the dock
  the only legitimate editor-global chrome? If the latter, the section list
  moves onto `Window` and §4 Phase 3's storage paragraph inverts.
- Does a buffer-scoped section survive the buffer moving between splits in
  the same window? (Proposed: yes — scope is the buffer, not the split.)
- Which of the twelve stale-on-switch plugins have user-visible symptoms
  beyond the two panels? `git_statusbar` on an orchestrator dive into a
  worktree on another branch is the likely one; it was not reproduced.
