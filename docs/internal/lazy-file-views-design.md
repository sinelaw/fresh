# Design: lazy file views — showing a file through a plugin, in its own tab

Status: proposed (not implemented). Open With (#3838) is the first user of
the parts that exist today; this document is where viewers go from there.

A file whose bytes are not meant to be read as text — a PDF, an image, an
EPUB — or a text file better read another way — a CSV as a table — should be
viewable inside the editor through a plugin, in the file's own tab, with the
same "only what is on screen costs anything" behaviour the editor already has
for huge text files.

---

## 1. The problem

**What is being shown.**

* The file is often not text: an image, a PDF, an archive, an EPUB (a zip of
  XHTML), a binary. Its bytes mean nothing as lines of characters.
* What a tool makes of it is not text either. It is something visual — a
  picture, a laid-out page, a table — that a terminal can only approximate as
  coloured character cells (ANSI colours, block and box-drawing glyphs). The
  view is a grid of styled cells, not a document someone edits.
* Both can be huge: a 300 MB PDF of 2,000 pages, a 5 GB CSV, a hex view of a
  large binary. Neither the whole rendering nor the whole file should have to
  be produced, held in memory or written to disk before anything shows.

**What we want.**

* The view is a window onto the rendering: scroll and jump through it like any
  long buffer, while only what is on screen (and a margin) has to exist.
* The editor says what is being looked at, in the view's own terms, as the
  user scrolls or resizes: rows N–M at width W.
* The plugin maps that to the file — which pages, chapters, records or byte
  ranges make those rows — reads only those bytes, on the machine the file is
  on, and renders only that part.
* The editor shows what comes back, and the plugin can fill rows in later,
  refine them, or replace them.
* It is one mechanism any viewer plugin builds on, not one per format.

**Constraints.**

* The file keeps one tab. A viewer is how that tab shows the file, never a
  second tab or a stand-in buffer the rest of the editor mistakes for the file
  (§3 shows what goes wrong otherwise).
* Nothing ever writes a rendering back to the file.
* Files of any window, remote ones included, are read where they live.
* The editor never waits on a plugin: scrolling stays responsive while the
  viewer catches up.

## 2. Use cases

| Format | What a user does |
|---|---|
| **PDF** | Open a 2,000-page manual and start reading at once; scroll; jump to page 1,500 and back; read a page as text (`pdftotext -f N -l N`) or as a picture (render the page, then `chafa`); search the whole document; copy a paragraph; open one in an SSH window. |
| **Image** | Open a 50 MB photo fitted to the pane; resize and have it redraw; zoom and pan, decoding only the visible part of a huge or tiled image; see its size and format. |
| **CSV** | Open a 5 GB CSV as aligned columns at once; keep the header pinned; jump to row 3,000,000; edit a cell, add or delete a row, undo, save — the file itself changes; search; handle quoted fields with commas or line breaks. |
| **EPUB** | Read a book reflowed to the pane's width; use the table of contents; resize and keep the place; search; copy a passage; see inline images. |

## 3. What the model already says, and why a stand-in buffer is wrong

The editor's model (on master):

* A **tab** is a buffer (or a buffer group) in a pane's tab list
  (`SplitViewState::open_buffers`); a buffer belongs to one window.
* **"File X is open"** means a buffer whose `file_path()` is X. The open path's
  already-open check (`Window::open_file_no_focus_inner`) is the choke point
  every open goes through, and about fifteen other places — replace in files,
  LSP workspace edits, recovery, buffer-group `path` panels, the explorer
  preview, Live Grep's preview, rename and delete, `findBufferByPath` — look a
  file's buffer up the same way.
* All of them rely on one identity: **the buffer for file X holds X's text.**

A first attempt (an earlier revision of #3838) made a plugin's viewer a read-only
virtual buffer that *counted* as the file's tab. It kept one tab per file for
the opens it covered, and broke the identity for everything else: a project
replace applied its edits to the viewer and left the file unchanged; an LSP
rename edited the viewer; recovered unsaved work could land in a buffer that
cannot be saved; the explorer's preview or Live Grep's closed the viewer;
renaming or deleting the file left the viewer claiming a path that was gone;
and opening with an encoding still made a second tab.

The lesson is the first constraint above: **the file keeps its one real
buffer, and a viewer is a way of showing that buffer's tab.** Master already
has that: `createBufferGroup(name, mode, layout, { inTabOf: bufferId })` shows
a group in a file's own tab — "the tab stays the file's tab in every way (its
name, its unsaved mark, closing it, its place in the tab bar); only what it
shows changes" — and Markdown's dual view uses it.

## 4. Designs considered

| | Design | In short |
|---|---|---|
| A | **On-demand rows** | A read-only buffer of N rows (known or estimated) whose content the editor asks the plugin for, range by range; the editor caches, shows placeholders, and re-asks on invalidation. |
| B | **Placeholder buffer, patched** | The plugin makes N placeholder rows and replaces the visible range as it is told what is visible. |
| C | **Plugin canvas** | The plugin draws one screenful of cells into a panel that owns its scrolling, and reports its total height. |
| D | **Terminal running a viewer tool** | The tab shows an embedded terminal running a terminal PDF/image/CSV viewer. |
| E | **The file's own buffer, drawn differently** | The file's lazily loaded buffer stays; the plugin restyles the visible part (as Markdown compose does). |
| F | **Rendering streamed to a temp file** | The tool writes the whole rendering to a file, opened in large-file mode and grown as it is written. |

How each serves the use cases (✓ good, ~ workable with real limits,
✗ unsuitable):

| | PDF | Image | CSV view | CSV edit | EPUB |
|---|---|---|---|---|---|
| A on-demand rows | ✓ | ~ | ✓ | ✗ | ✓ |
| B placeholder buffer | ~ | ~ | ✗ memory | ✗ | ~ |
| C plugin canvas | ~ (✓ as pictures) | ✓ | ~ | ✗ | ~ |
| D terminal tool | ~ | ~ | ~ | ~ | ~ |
| E file's own buffer | ✗ | ✗ | ✓ | ✓ | ✗ |
| F temp-file rendering | ~ text only | ~ | ~ | ✗ | ✓ |

The notes behind the grid:

* **PDF.** Rows map to pages, so A renders only visible pages; E cannot work,
  as a PDF's bytes are not laid out in page order. F converts everything first
  (seconds to minutes); C suits page pictures but leaves selection and search
  to the plugin.
* **Image.** The output is about one screen and depends on the width, so A's
  row cache is invalidated by every resize; C — a screen-sized canvas with
  zoom and pan events, rendering only the visible tile — is the fit.
* **CSV.** Only E keeps editing real: the file's own buffer is already lazy, and
  edits, undo and save are the file's. The plugin only aligns what is visible.
  Open points: column widths come from sampling, and a quoted line break
  splits a record across lines.
* **EPUB.** Rows map to chapters, so A unzips and lays out only the visible
  chapter, with a row count estimated until each is laid out. Books are small,
  so F is good enough as a start.

No single design serves all four. The split is between **a rendering that is
not the file's text** (PDF, EPUB, images: A, or C for pictures) and **the
file's own text drawn differently** (CSV: E). A and E turn out to be one
contract with two kinds of position.

## 5. Proposed design: one range contract, two kinds of position

**The contract.**

* **The editor drives.** It knows what is on screen (the visible range and the
  width of each pane showing the view), tells the plugin when that changes,
  and never waits: unanswered rows show as placeholders.
* **The plugin answers lazily.** It maps the range to the file, reads only those
  bytes — through the file's window (`WindowPath`, `openMachine({ kind:
  "window" })`), so a remote file is read on its host — renders them, and
  sends back styled cells for exactly that range.
* **The editor caches and invalidates.** It keeps what it was given, asks only
  for what it lacks, and drops what a width change or the plugin's
  "these rows changed" makes stale.
* **It lives in the file's own tab.** The file keeps its one real buffer and its
  one tab; a view is shown in that tab (the `inTabOf` mechanism) and closing
  the view shows the file again. Nothing writes a rendering to the file, and
  every place that looks a file up by path keeps finding its real buffer.

**Kind A: rows of a separate rendering** (PDF, EPUB, images in text form).

* Positions are rows of the rendering. The plugin says how many there are, or
  an estimate it revises as it learns (an EPUB's chapters as they are laid
  out).
* The editor asks for rows N–M at width W; only the plugin knows which pages,
  chapters or tiles those are.
* Read-only. The editor's cursor, selection and copy work on loaded rows;
  "go to page" and search across the whole document go through the plugin,
  which knows the mapping and can answer with a row to jump to.

**Kind E: the file's own text, drawn differently** (CSV, logs, hex of a binary).

* Positions are byte ranges of the file's real buffer, which is loaded lazily
  as any huge file is.
* The editor says which bytes are visible; the plugin says how they look
  (aligned columns, colour, hidden separators, virtual text) without changing
  them. Most of this exists today: `lines_changed` reports the lines the
  editor is about to draw, and conceals, overlays, soft breaks and virtual
  lines restyle them — Markdown compose is built on it.
* Everything else is the file's own: editing, undo, save, search, LSP.

**API sketch** (illustrative; names and shapes to be settled):

```ts
// Kind A: a lazily rendered view in a file's tab.
const view = await editor.createRenderedView({
  inTabOf: bufferId,              // the file's own tab
  rows: { estimate: 120_000 },    // or an exact count
  handler: "pdfRows",             // called with { viewId, from, to, width }
});
registerHandler("pdfRows", async ({ viewId, from, to, width }) => {
  const cells = await renderPagesFor(from, to, width);   // reads only those pages
  editor.setRenderedRows(viewId, from, cells);           // styled cells, row by row
});
editor.setRenderedViewRows(view.id, { exact: 118_440 }); // the count, once known
editor.invalidateRenderedRows(view.id, from, to);        // re-ask for a range

// Navigation (§6) adds options to the same call: the plugin maps between
// rows and the file, and names its "go to" unit. Each answer may come later.
//   goTo: { unit: "%goto.page", handler: "pdfRowForPage" },  // Go to Line becomes Go to Page
//   rowForByte: "pdfRowForByte",   // file byte (or fraction) -> row
//   bytesForRow: "pdfBytesForRow", // row -> file bytes, for history and switching views
// Row cells may carry a gutter label: { row, cells, label: "p. 1500" }.

// Kind E: restyle the file's own buffer as it is drawn (exists today).
editor.on("lines_changed", ({ buffer_id, lines }) => { /* align the visible rows */ });
```

**Responsibilities.**

| The editor | The plugin |
|---|---|
| Which rows or bytes are visible, per pane, and at what width | Mapping a range to the file's pages, records or bytes |
| Caching answered ranges; placeholders for the rest | Reading only those bytes, where the file lives |
| Asking again on scroll, resize or invalidation; never waiting | Rendering styled cells for exactly that range |
| Cursor, selection, copy on loaded rows; the scrollbar from rows, an estimate or file bytes (§6) | The row count or estimate; mapping rows to file bytes and back; its "go to" unit and gutter labels; search across the whole document |
| Keeping the file's one buffer and tab; never writing a rendering | Saying when rendered rows are stale |

## 6. Navigation: the scrollbar, Go to Line, Go to byte offset

Navigation follows what the editor already does for a huge text file: a file
over the large-file threshold opens without a line index, so its gutter shows
byte offsets, its scrollbar is proportional to bytes (`MarkerBasis::Bytes` in
`view/scrollbar_marker.rs`), and Go to Line offers to scan the file for a line
index first ("Scan Line Index", run where the file lives). Positions get more
exact as more is known, and nothing waits for the whole file. Lazy views keep
that shape.

**Kind E — the file's own text.** Nothing changes: it is the file's buffer.
The scrollbar uses the bases it uses today (rows, lines, or bytes on an
unscanned large file); Go to Line, Go to byte offset, the gutter and position
history are the file's own. A plugin that adds rows (virtual lines, wrapping
of wide cells) is drawn under the visual-row basis as Markdown compose is.
"Go to row 3,000,000" of a CSV is Go to Line, except where quoted line breaks
make records and lines differ; there the plugin can offer its own "go to
record", answered with a byte offset.

**Kind A — a separate rendering.** Its positions are rendered rows, which are
not the file's lines or bytes, so each navigation needs a basis the plugin
can honour:

| | Exact row count known | Only an estimate | Nothing known yet |
|---|---|---|---|
| **Scrollbar** | Proportional to rows, like a scanned file | Proportional to the estimate; when the plugin revises it, the row on screen stays put and the thumb moves | Proportional to the file's bytes, like an unscanned large file: the plugin maps a byte fraction to rows |
| **Dragging the thumb / Page Down far** | Asks for the target rows | Asks for rows at the estimated position; the plugin answers with the true rows there | Asks the plugin for the rows at that fraction of the file |

* **The plugin maps between rows and the file.** Two questions the editor can
  ask, each answered later (the editor shows placeholders meanwhile, as for
  rows): *which rows does file byte B (or fraction F) start at*, and *which
  file bytes make row R*. A hex view answers both by arithmetic; a PDF answers
  with the page that holds an object; an EPUB with the chapter's entry. A
  plugin that cannot map a direction says so, and the editor offers only what
  it can do.
* **The gutter shows the plugin's labels,** as a large file shows byte offsets
  instead of line numbers: page numbers, chapter names, hex offsets. The
  plugin returns a label per row with the row's cells, or none.
* **Go to Line** in a view means its own unit, which the plugin names: "Go to
  Page" for a PDF, "Go to Chapter" for an EPUB, "Go to Offset" for hex. The
  plugin resolves the input to a row; the editor jumps there and asks for the
  rows. Without a named unit, Go to Line goes to a rendered row, which is
  only meaningful with an exact row count.
* **Go to byte offset** maps through the plugin from file byte to row. It is
  the natural jump for a hex view and for "show me where this byte is"; for a
  PDF it lands on the page that holds the byte, if the plugin can tell.
* **Position history and bookmarks** keep a row and, when the plugin gave
  one, the file bytes the row came from. A revised estimate moves rows; the
  file anchor does not, so a jump back asks the plugin for the row of that
  anchor rather than trusting the stale row number.
* **Switching between text and a view** keeps the place where it can: the
  file byte under the cursor in text becomes a row through the plugin, and
  back.

## 7. Open questions

* **Extent.** How often a plugin may revise an estimate before a moving thumb
  is a distraction, and what "go to end" does on an estimate (ask the plugin
  for its last rows, as §6 does for any far jump).
* **Width.** Images and reflowed text depend on the pane's width, so a resize
  invalidates everything; two panes of different widths need two caches.
* **Placeholders.** What an unanswered row shows, and whether a short delay
  before showing it avoids flicker.
* **Positions.** What cursor, selection, search and copy mean on rows that are
  not loaded, and what a plugin that cannot map rows to file bytes loses
  (§6: history falls back to row numbers).
* **Pictures.** Whether images want a canvas (design C) next to rows, and pixel
  graphics (Kitty/Sixel) later.
* **Restore.** Groups are rebuilt by plugins after a restart, so a file shown
  in a view comes back as its text unless the plugin re-applies the view.
* **Defaults.** "Always open .pdf with …" is routing a file type to a view when
  it opens; it builds on this and is separate work.

## 8. Phasing

1. **#3838**, simplified to the parts that are sound: plugin items in the explorer
   and tab menus, `openInSystemApp`, and Open With as a plugin. A viewer is
   shown in the file's own tab through `createBufferGroup({ inTabOf })` and
   filled with `setVirtualBufferContent`. One tab per file holds by
   construction; a viewer's content is not lazy yet.
2. **Kind A** — `createRenderedView` and its range contract; Open With viewers
   move onto it, and become lazy.
3. **Kind E** — whatever byte-range reporting `lines_changed` lacks for binary
   files (whose "lines" are arbitrary), and a CSV viewer/editor on it.
4. **Defaults** — routing a file type to a view when it opens ("Always open
   with").
