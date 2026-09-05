# Search-result previews: open the file, don't compose a snippet

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: PLANNED.** Design for sinelaw/fresh#3196 — the follow-up to
sinelaw/fresh#3104, which put syntax colours on the source embedded in the
Review Diff and Git Log panels.

The issue asks for syntax highlighting in the preview buffers the
grep-style finders show. The answer this doc argues for is not to colour
the snippet but to **stop composing one**: preview the real file as an
ephemeral *preview tab*, exactly as the File Explorer does on a single
click. Highlighting then arrives for free, along with everything else a
real buffer has. Only the surface that genuinely cannot be a file — the
results / Quickfix *list*, whose rows come from many files — still needs
the declared-region mechanism of
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

## 3. The proposal: preview tabs, the File Explorer's semantics

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

That is precisely the behaviour the ask describes, already built, already
tested, already the thing users know from the explorer. The finders should
call it instead of composing a buffer.

**Prompt-mode finders** (Git Grep, Find References): drop `updatePreview`,
the `*Preview*` buffer and the preview split entirely. On every selection
change, preview the selected result's file into the split the search was
started from, cursor on the match. Enter commits — that is the open the
finder already performs, and it promotes the preview it is already looking
at rather than opening a second buffer. Escape restores (§3.2).

**Panel-mode finders** (Diagnostics, the Quickfix list): `navigateOnCursorMove`
already opens the item's file in the source split as the cursor moves —
with a *committing* open, so today arrowing down a diagnostics list leaves
one permanent tab per file. Routing it through the preview open fixes that
accumulation as a side effect; it is the same bug the issue is about, one
surface over.

### 3.1 The one host gap: previewing without bouncing focus

`open_file_preview` targets `preferred_split_for_file()` and leaves
keyboard focus where it was — that is how the explorer previews into the
editor pane while the sidebar keeps the keys. A prompt-mode finder is in
the same position: the prompt is chrome, so the preview can land in the
editing split under it with no focus change at all.

Panel mode is not. It reaches its source split by focusing it and focusing
back, and a *focus change to another split promotes the preview*
(`promote_preview_if_not_in_split` — walking away is commitment). A plugin
doing that dance would commit every preview it opened, which is exactly
the accumulation we are removing.

So the host needs one addition: **a preview open that names its split and
changes no focus.** Either an options bag on the existing command —
`openFileInSplit(splitId, path, line, column, { preview: true })` — or a
sibling `previewFileInSplit`. It routes to the same preview bookkeeping
(single preview, replace, promote, suppressed history, deferred hook) and
must not call `set_active_split`. Everything else on the path already
exists.

### 3.2 Cancel has to restore

The explorer never restores: browsing *is* the interaction, and walking
away commits. A search is different — Escape should put the user back in
the buffer they were reading, not in the last result they arrowed past.

Prior art on both sides of the FFI: the Quick Open `:N` goto-line prompt
snapshots the split's buffer and cursor and restores them when the prompt
is cancelled, and the Live Grep overlay does the same for its phantom
preview, closing only the buffers *it* loaded and never one the user
already had open. The finder wants that behaviour, so cancel is: close the
current preview if we opened it and it is unmodified, then restore the
split's previous active buffer and cursor.

Doing this in the plugin is possible (it knows the source split and can
record the buffer and cursor), but it re-derives a rule the host already
implements twice. See §6.

## 4. What it buys, and what it costs

Buys: every item in §2, plus one less bespoke buffer shape in the tree,
plus the finders finally matching the Live Grep overlay users already see.

Costs, honestly:

- **LSP churn is the real one.** Each preview open is a `didOpen` and each
  replacement a `didClose`; `open_file_preview` already carries a
  `TODO(perf)` saying so for the explorer. A finder previews on every
  selection move *and* on the first result of every search refresh — i.e.
  per keystroke — so this must be debounced (preview only after the
  selection has been still briefly), and the debounce belongs in the
  Finder where the keystrokes are. Without it, rapid typing walks a heavy
  server through dozens of open/close pairs. The snippet had no LSP cost
  at all; this is the one place the trade is genuinely worse.
- **Whole files, not slices.** In practice a wash: `updatePreview` already
  reads the entire file across the FFI to show eleven lines. But a preview
  must never *prompt* — the explorer's path can surface a large-file
  encoding confirmation, and a modal over a live search is unacceptable.
  A preview that would need confirmation is a preview that is skipped.
- **Buffers the user modifies.** Type in a preview and it is promoted, by
  the existing mutation rule; cancel must then leave it alone rather than
  close it.
- **Remote authority.** Content arrives asynchronously, so a preview can
  paint empty for a frame. The overlay already lives with this.
- **File watching.** Each open registers a watch; confirm the close path
  drops it, or a long search session accumulates watchers.
- **The setting's name.** The preview-tab mechanism is gated by
  `file_explorer.preview_tabs`, and with it off the explorer falls back to
  a *committing* open. A finder must not: with previews off it should
  simply not preview, rather than open a permanent tab per result. The
  honest fix is to grow the key into an editor-level one and alias the old
  path.
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

There are three preview implementations in the tree today — the explorer's
preview tab (host), Live Grep's overlay phantom buffer with its own
`loaded_buffers` bookkeeping and restore (host), and the Finder's composed
snippet (plugin). They agree on the hard parts (at most one live preview,
never close a buffer the user already had, never close a modified one,
restore what was there) and each states them separately.

The consolidation this design points at is a host-owned **preview session**:
begin one against a split, preview files into it, end it with either
*commit* (the user chose a result) or *restore* (they cancelled). The
explorer's preview tab is that session with no restore; the overlay's
phantom buffer is that session rendered into a card instead of a split;
the finder becomes the third consumer instead of the third implementation.

Not a prerequisite. §3.1 plus a plugin-side cancel is enough to ship the
behaviour; the session is what to build if a fourth consumer appears, or
when the overlay and the finder start disagreeing about an edge case.

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
8. **The list's own colours** (§5): rows colour by their own file; an
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
