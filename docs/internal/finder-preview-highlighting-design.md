# Search-result previews: open the file, don't compose a snippet

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: PLANNED.** Design for sinelaw/fresh#3196 — the follow-up to
sinelaw/fresh#3104, which put syntax colours on the source embedded in the
Review Diff and Git Log panels.

The issue asks for syntax highlighting in the preview buffers the
grep-style finders show. The answer this doc argues for is not to colour
the snippet but to **stop composing one**: preview the real file as an
ephemeral *preview tab*, exactly as the File Explorer does on a single
click — through the explorer's own code, not a second implementation of
it. Highlighting then arrives for free, along with everything else a real
buffer has. Only the surface that genuinely cannot be a file — the results
/ Quickfix *list*, whose rows come from many files — still needs the
declared-region mechanism of
[`embedded-language-highlighting.md`](embedded-language-highlighting.md).

## 1. Three surfaces, one of them already right

| Surface | What it is | Colours today |
|---|---|---|
| Live Grep's floating overlay, right-hand pane | a **real file buffer**, opened for preview and painted by the per-leaf pipeline a normal split uses | **yes** |
| `*Preview*`, the split the Finder opens (Git Grep, Find References, any finder with `preview: true`) | a **composed virtual buffer**: header, rule, then `>  123 │ <line>` rows of ±5 context | no |
| the Finder's panel / the Quickfix list Live Grep exports | a **composed virtual buffer**: title, file headers, one `  path:line:col  <matched line>` row per result | no |

The first row is the tell. Live Grep already answered this question once,
in the host, by previewing the file itself — and its pane has colours,
gutter, wrap and scrolling that the `*Preview*` beside it does not. The
snippet is the odd one out, not the overlay.

## 2. What the snippet costs

`Finder.updatePreview` reads the whole file through the plugin API, slices
±5 lines around the match, prefixes each with a marker and a padded line
number, and writes the result into a virtual buffer in a split it owns.
What that buffer cannot have, because it is not a file:

- syntax colours (the issue), and everything else keyed to a language;
- the real gutter, folds, soft wrap, indent guides, rulers;
- search inside the preview, occurrence highlighting, bracket matching;
- LSP decoration — diagnostics squiggles on the lines you are looking at;
- scrolling past the ±5 window, or stepping into the file to keep reading;
- correct behaviour on anything the slice mishandles: CRLF, tabs, very
  long lines, files whose encoding is not UTF-8.

It also duplicates work the host already does well: reading a file, and
showing part of one.

## 3. The proposal: the File Explorer's preview tabs, shared

The editor already has an ephemeral-open concept with worked-out
invariants — `Editor::open_file_preview`, what the explorer calls when you
single-click a file:

- **At most one preview exists editor-wide**, anchored to a split. Opening
  the next one in that split closes the previous.
- **Already-open files are never demoted.** If the result's file is a tab
  the user already had, the preview switches to it and changes no preview
  state, so nothing of theirs is closed later.
- **Walking away is commitment.** Focusing another split, splitting,
  dragging the tab, or *touching* the buffer promotes it to a permanent
  tab and fires the `after_file_open` hook that preview-opening deferred.
- **Browsing does not pollute history.** Position history is suppressed
  for the open, so a run of previews doesn't flood back/forward.
- **The tab says so** — a preview tab renders with the preview indicator,
  so the user can see the difference between "looking" and "opened".

That is the behaviour the ask describes, already built and already tested.
The finders should **call it**, not re-implement it.

### 3.1 What is shared, and what stays the caller's

Read `open_file_preview` as two things stuck together: a **policy header**
that is the explorer's, and a **preview discipline** that is nobody's in
particular. The refactor separates them and shares the second.

Caller policy — moves out:

- **The `file_explorer.preview_tabs` gate.** It is the explorer's setting
  and it stays the explorer's; the finder preview is not gated by it and
  does not read it. (This is also a small cleanup: the key is checked in
  *two* places today — inside `open_file_preview` and again in the
  explorer's keyboard-nav path — and after the split there is one check,
  in the explorer wrapper.) A finder that ever wants an off switch gets
  its own key; it does not borrow the explorer's.
- **Which split the preview lands in.** The explorer passes
  `preferred_split_for_file()` — the unlabeled editor pane, so a sidebar
  never previews into itself. A finder passes the split it is browsing
  from.
- **What to do about an interactive open** (§4, first bullet).

Preview discipline — becomes the shared core, unchanged in behaviour:
dismissing a popup anchored to the buffer being left, suppressing position
history for the open, opening with `OpenKind::Preview` so the
`after_file_open` hook is deferred, telling "opened a new file" from
"switched to one already open", closing or promoting the outgoing preview
depending on whether it shared the target split, and anchoring
`window.preview` as the single source of truth. Roughly:

```
preview_file_into_split(path, target_split, at: Option<(line, col)>) -> Result<BufferId>
```

with `open_file_preview(path)` reduced to the explorer's gate plus a call
to it, and the plugin command a validated split id plus a call to it. No
second copy of the promote/replace rules, and a fix to any of them lands
for both surfaces at once.

Two small generalizations the split argument forces, both of which the
current code only avoids by making the target split *active* first:

- **A preview open that names its split and changes no focus.**
  `open_file_preview` reaches its split through
  `preferred_split_for_file()` inside `open_file`; `openFileInSplit`
  reaches its split by calling `set_active_split` and then opening. The
  shared core needs the target as a parameter instead — insert the buffer
  into that split's view state and make it that split's active buffer,
  without touching which split is active. The committing
  `openFileInSplit` can ride the same targeted open afterwards (it keeps
  the focus change; it just stops being the *mechanism* for targeting).
- **A jump that names its buffer.** `jump_to_line_column` moves
  `active_state_mut()`'s cursor via `jump_active_cursor_to`, which is only
  correct when the previewed buffer is the active one. It needs the target
  passed in, with today's call sites passing "the active one".

### 3.2 The focus gap, and why panel mode needs it

`open_file_preview` leaves keyboard focus where it was — that is how the
explorer previews into the editor pane while the sidebar keeps the keys. A
prompt-mode finder is in the same position: the prompt is chrome, so the
preview lands in the editing split under it with no focus change at all.

Panel-mode finders are not. `navigateOnCursorMove` reaches its source
split by focusing it and focusing back, and **a focus change to another
split promotes the preview** (`promote_preview_if_not_in_split` — walking
away is commitment). A plugin doing that dance would commit every preview
it opened, which is exactly the accumulation being removed. Hence the
no-focus targeted open above; with it, the plugin API is
`openFileInSplit(splitId, path, line, column, { preview: true })` (or a
sibling `previewFileInSplit`) and the panel keeps its cursor.

Panel mode is worth fixing in the same pass for its own sake: its
navigation opens with a *committing* open today, so arrowing down a
diagnostics list already leaves one permanent tab per file.

### 3.3 Cancel has to restore

The explorer never restores: browsing *is* the interaction there, and
walking away commits. A search is different — Escape should put the user
back in the buffer they were reading, not in the last result they arrowed
past.

Prior art on both sides of the FFI: the Quick Open `:N` goto-line prompt
snapshots the split's buffer and cursor and restores them when the prompt
is cancelled, and the Live Grep overlay does the same for its phantom
preview, closing only the buffers *it* loaded and never one the user
already had open. Cancel is therefore: close the current preview if we
opened it and it is unmodified, then restore the split's previous active
buffer and cursor. Confirm is the opposite and needs nothing new — Enter
already opens the file, which promotes the preview the user is looking at
rather than opening a second buffer.

## 4. What it buys, and what it costs

Buys: every item in §2, plus one less bespoke buffer shape in the tree,
plus the finders finally matching the Live Grep overlay users already see.

Costs, honestly:

- **An open that wants to ask the user something.** Loading a *large* file
  whose encoding needs a full pass (non-resynchronizable, or non-UTF-8)
  does not silently guess: the buffer loader bails with
  `LargeFileEncodingConfirmation`, and callers turn that into a modal
  asking whether to load it. That is right for a deliberate open and wrong
  for a browse — a modal must not appear over a live search because the
  selection moved onto a `.csv` in Latin-1. The explorer's keyboard-nav
  preview already takes this position (it logs and skips; the user can
  still press Enter for the full flow), while its single-click path raises
  the modal — an inconsistency between two entry points to the same
  gesture that the refactor should settle rather than copy. The finder
  behaves like arrow-nav: a browse never prompts, a commit does. One
  shared classifier for "this open wants the user" instead of a third
  place that downcasts the error.
- **LSP churn is the real cost.** Each preview open is a `didOpen` and
  each replacement a `didClose`; `open_file_preview` already carries a
  `TODO(perf)` saying so for the explorer. A finder previews on every
  selection move *and* on the first result of every search refresh — i.e.
  per keystroke — so this must be debounced (preview only after the
  selection has been still briefly), and the debounce belongs in the
  Finder where the keystrokes are. Without it, rapid typing walks a heavy
  server through dozens of open/close pairs. The snippet had no LSP cost
  at all; this is the one place the trade is genuinely worse.
- **Whole files, not slices.** In practice a wash: `updatePreview` already
  reads the entire file across the FFI to show eleven lines.
- **Buffers the user modifies.** Type in a preview and it is promoted, by
  the existing mutation rule; cancel must then leave it alone rather than
  close it.
- **Remote authority.** Content arrives asynchronously, so a preview can
  paint empty for a frame. The overlay already lives with this.
- **File watching.** Each open registers a watch; confirm the close path
  drops it, or a long search session accumulates watchers.
- **What is lost from the snippet**: the `path:line:col` header line, the
  `>` marker on the match row, and the deliberate ±5 framing. The gutter,
  the cursor and the status bar carry the first two; the third becomes
  "the file, positioned at the match", which is what a peek should be.

## 5. The results list still needs declared regions

The Quickfix / panel list is not a file and cannot become one: its rows
are `  path:line:col  <trimmed matched line>`, drawn from many files, each
row an isolated line at its own offset. Colouring it stays a
`setSyntaxRegions` job, and the design is unchanged from the region
mechanism's other consumers:

- **one region per row** — `prefix` is the byte length of everything that
  row prints before its content, `language` is that row's file, and
  `streams` is a fresh id per row so each row parses standalone (an id the
  host has not seen gets a new parser; the host keeps only the few most
  recent and evicts the rest, which is what "start clean" costs);
- title, file headers, the blank line and the help footer get no region
  and keep the styling they have;
- `prefix` is **measured**, never assumed — the lesson of "review-diff:
  measure a row's gutter, don't assume it", where a padded-but-untruncated
  line number and a three-byte `│` counted as one character fed a Markdown
  heading its own marker and it stopped being a heading. `lib/git_history.ts`
  exports the `byteLength` helper this needs, and
  `commitDetailSyntaxRegions` is the shape to copy: a pure function from
  the entries the buffer is filled with to the regions, so offsets and
  content cannot disagree;
- regions are cleared when the content is set, so re-declare after every
  write.

Two limits to state rather than fix: the list trims each match's leading
indentation and truncates past 100 characters with an ellipsis, and the
parser sees exactly that.

## 6. Where this points: one preview primitive

§3.1 shares the preview *discipline* between the explorer and the finders.
The remaining duplication is one level up: Live Grep's overlay carries its
own phantom-buffer bookkeeping (`loaded_buffers`, hidden-from-tabs,
restore-on-close) because it renders its preview into a card rather than a
split, and the goto-line prompt carries its own snapshot/restore.

They agree on the hard parts — at most one live preview, never close a
buffer the user already had, never close a modified one, restore what was
there — and each states them separately. The consolidation this points at
is a host-owned **preview session**: begin one against a split, preview
files into it, end it with either *commit* (the user chose a result) or
*restore* (they cancelled). The explorer's preview tab is that session
with no restore; the overlay's is that session rendered into a card.

Not a prerequisite. §3.1 plus a plugin-side cancel is enough to ship the
behaviour; the session is what to build when the fourth consumer appears,
or when the overlay and the finder start disagreeing about an edge case.

## 7. Properties worth a test

E2E, in the Rust plugin suites (`tests/e2e/plugins/`; `lsp_find_references`
is the natural home):

1. **It is a real buffer.** A preview of a `.rs` result paints a keyword in
   the keyword colour — the issue's actual ask, satisfied without a region.
2. **Browsing does not accumulate tabs.** Arrowing through results across
   several files leaves exactly one preview tab, and it is the last one.
3. **Confirm promotes rather than reopens.** Enter on a result leaves that
   file as a permanent tab, with no second buffer for the same path and
   the deferred `after_file_open` fired exactly once.
4. **Cancel restores.** Escape returns the split to the buffer and cursor
   the search started from, and closes only what the search opened.
5. **A file the user already had open is untouched.** Previewing it and
   then cancelling leaves its tab open and un-demoted.
6. **A modified preview survives.** Type in a preview, cancel the search:
   the buffer stays, promoted.
7. **The panel case.** Cursor movement in the Diagnostics/Quickfix panel
   previews without stealing focus from the panel and without promoting on
   each move — the regression the focus bounce would cause.
8. **The explorer's setting is the explorer's.** With
   `file_explorer.preview_tabs` off, the explorer stops previewing and the
   finder's previews are unaffected.
9. **A browse never prompts.** A result in a large non-UTF-8 file leaves
   the search running with no preview and no modal; opening it with Enter
   still gets the confirmation.
10. **The list's own colours** (§5): rows colour by their own file; an
    unknown language is unchanged; the search-match highlight still wins
    over the syntax colour, mirroring the word-level-diff test.

## 8. Open questions

- **Does the preview belong in the source split, or in a split of its
  own?** The explorer model says source split, and it needs no split
  management. Find References may want the list and the code side by side —
  but in prompt mode the list is chrome floating over the editor, so the
  source split already reads as a peek pane. Start with the source split.
- **Debounce interval**, and whether it belongs in the Finder or beside the
  preview open in the host.
- **Whether `preview: true` rides `openFileInSplit`** or gets its own
  command. An options bag keeps one call site; a sibling command keeps the
  committing open's signature honest about what it does.
- **Whether the explorer's single click should stop raising the
  confirmation modal**, matching its own arrow-key path (§4). Settling it
  is a behaviour change to the explorer, so it wants its own call.
