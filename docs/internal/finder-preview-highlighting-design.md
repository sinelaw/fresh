# Syntax highlighting in the finder preview buffers

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: PLANNED.** Design for sinelaw/fresh#3196 — the follow-up ask to
sinelaw/fresh#3104, which put syntax colours on the source embedded in the
Review Diff and Git Log panels. Companion to
[`embedded-language-highlighting.md`](embedded-language-highlighting.md),
whose fourth tool — regions a plugin declares — is the whole mechanism this
design uses. Nothing here needs a host change.

## 1. What is plain today, and what is not

Three surfaces show a search result's source, and only one of them is
coloured:

| Surface | What it is | Colours today |
|---|---|---|
| Live Grep's floating overlay, right-hand pane | a **real file buffer**, opened for preview and painted by the same per-leaf pipeline a normal split uses | **yes** — the ordinary highlighting path, nothing to do |
| `*Preview*`, the split the Finder opens (Git Grep, Find References, any finder with `preview: true`) | a **virtual buffer** the plugin composes: a header, a rule, then `>  123 │ <line>` rows of ±5 context around the match | no |
| the Finder's panel / the Quickfix list Live Grep exports | a **virtual buffer** the plugin composes: a title, optional file headers, then one `  path:line:col  <matched line>` row per result | no |

The asymmetry is the whole complaint: the overlay pane looks like the
editor, and the two composed buffers beside it look like a log file.

The reason is not an oversight. A composed buffer is not a document any
grammar can parse — a gutter, headers, a rule, rows drawn from many files —
so the highlighting engine is deliberately a *plain text* host for it. That
is exactly the situation `setSyntaxRegions` was added for.

## 2. The mechanism

Tool 4 of `embedded-language-highlighting.md`: the plugin that composed the
buffer tells the host where its rows carry code, and the host colours them.
A region is `{start, end, language, prefix, streams}` over the buffer's own
bytes:

- **`language`** — a path or a language token. Nothing is opened or read; it
  only picks the grammar, resolved through the same catalog that would pick
  one for a file of that name. A language the catalog does not claim leaves
  those rows exactly as they are.
- **`prefix`** — the bytes at the start of *every* row of the region that are
  not code. The host strips them before feeding the row to the child parser,
  so the parser's offsets land on the source and a grammar rule anchored to
  the start of a line still matches.
- **`streams`** — the parsers the rows feed, first one colouring the row. A
  stream keeps its parse state across the rows of a region *and across other
  regions*, keyed by its id; an id the snapshot has not seen starts a fresh
  parser. This is what makes an interleaved diff correct, and — see §3 — it
  is also how a preview says "start clean here".

Two host-side rules shape the design:

- **The buffer must be virtual.** Both target buffers are (they come from
  `createVirtualBufferInSplit`). A file's own grammar can never be taken
  away by a plugin.
- **Setting the content clears the regions.** Every write must be followed
  by a fresh declaration.

## 3. The preview pane

The preview is the easy and valuable case: a *contiguous* slice of a
*single* file. It maps onto one region with one stream.

**Where the code goes.** `crates/fresh-editor/plugins/lib/finder.ts`, beside
the entry building in `updatePreview` — one helper, and every finder with
`preview: true` gets colours at once. Model it on
`commitDetailSyntaxRegions` in `lib/git_history.ts`: a pure function from
the `TextPropertyEntry[]` the buffer is filled with to the regions, walking
the same array in the same order, so the byte offsets and the content it
declares cannot disagree. The preview rows already carry the property the
walk needs to classify them (`header`, `separator`, `match`, `context`);
give the code rows the file they came from as well, so the helper stays a
function of its entries.

**One region per gutter run.** The gutter is `${marker}${lineNum} │ `, and
its byte width is *not* a constant:

- the line number is padded to four columns but never truncated, so past
  line 9,999 the gutter grows — the lesson of "review-diff: measure a row's
  gutter, don't assume it", where an assumed prefix fed a Markdown heading
  row its own diff marker and the heading stopped being a heading;
- `│` is three bytes in UTF-8 while it is one JavaScript character, and
  `prefix` is bytes.

So measure the gutter each row actually emitted, with the `byteLength`
helper `lib/git_history.ts` already exports, and open a new region when the
width changes. In a preview window that is at most a screen tall, the width
changes at most once, so this is one region in practice and two at a
boundary.

**A fresh stream per refresh.** Number the preview's stream from a counter
that increases on every refresh. An id the host has not seen gets a new
parser, which is what a preview wants twice over: the window starts in the
middle of a file, so no state from above it exists to inherit, and the
*previous* preview's state (of a different file, in a different language)
must not leak into this one. This is the same trick the commit-detail
regions use when they number a hunk's two streams `2h` and `2h+1` — a fresh
pair per hunk means a fresh parse per hunk. The host keeps only the few most
recent parsers and evicts the rest, so an ever-increasing id costs nothing.

**Declare after every write.** `updatePreview` has two paths — the first
call awaits `createVirtualBufferInSplit` and only then knows the buffer id;
later calls write with `setVirtualBufferContent`, which clears the regions.
`setSyntaxRegions` goes immediately after both.

**Cost.** A preview is ~12 rows. Building the regions is one linear pass
over an array that was just built, and needs no I/O — the file content is
the string `updatePreview` already read. The parse is the child parse the
feature exists to perform, over a dozen lines, on a refresh the user
triggered by moving the selection.

Note the path to use for `language` is the entry's own file path, not the
authority-resolved path the read goes through: the grammar is chosen by
name, and a remote-authority prefix is noise the catalog does not need.

## 4. The results / Quickfix list

Same helper shape, weaker payoff, and worth doing only after §3 lands.

Each row is `  path:line:col  <trimmed matched line>`, so unlike the preview
nothing is shared between rows: the code starts at a different column on
every row, consecutive rows may come from different files, and each row is
an isolated line rather than a run. The declaration therefore is **one
region per row**: `prefix` = the byte length of everything that row prints
before its content, `language` = that row's file, `streams` = a fresh id per
row, so every row parses standalone. Title, file headers, the blank line and
the help footer get no region and keep the styling they have.

Two honest limits to state rather than fix: the list trims each match's
leading indentation and truncates it past 100 characters with an ellipsis,
and the parser sees exactly that — a token-level colouring of a line that is
not quite the line in the file.

## 5. Deliberately out of scope

- **The Live Grep overlay pane.** Already correct; it is a real buffer.
- **Terminal-scrollback matches.** Universal Search can match inside a
  terminal's backing file under the data dir. Those have no language and
  their preview is a real buffer anyway; they stay plain.
- **A new host API.** `prefix`, `language` and `streams` are per-region
  already, and a fresh stream id is a supported way to say "start clean".
  If a later consumer wants to restart a parser without consuming an id,
  that is a host change to argue for then, not a prerequisite now.
- **A setting.** The diff panels colour their source unconditionally; a
  preview that matches the editor is the expected behaviour, not a mode.

## 6. Properties worth a test

E2E, in the Rust plugin suites (`tests/e2e/plugins/` — `lsp_find_references`
is the natural home for the preview cases), asserting on rendered cell
colour with the `wait_for_fg` shape the diff-highlighting tests use:

1. **It colours.** A preview of a `.rs` result paints a keyword in the
   keyword colour.
2. **The gutter is measured, not assumed.** A preview of a Markdown file at
   a line past 9,999 still paints the heading as a heading — the case that
   catches an off-by-N prefix, because the grammar anchors the rule to the
   start of the line.
3. **An unknown language is unchanged.** A preview of a file the catalog
   does not claim renders exactly as it does today.
4. **The match still wins.** The search-match highlight on the matched token
   sits above the syntax colour, mirroring the existing "word-level diff
   still wins over the syntax colour" test.
5. **No leak between previews.** Moving the selection from a `.rs` result to
   a `.md` one repaints with Markdown colours and nothing carried over —
   the regression a reused stream id would produce.
6. **The window's edge is a known behaviour.** A preview whose first row
   falls inside a block comment colours from the window's top, not from the
   file's. Assert it so a later change to the context window has to say so.

## 7. Limits, stated up front

- The window starts mid-file, so a construct opened above it — a block
  comment, a raw string — is invisible to the parser and those rows colour
  as if the construct began at the window. The diff panels make the same
  trade-off; a handful of context lines makes it rare and never wrong for
  more than the top of one pane.
- The grammar is chosen by path. A file whose language the catalog does not
  claim keeps today's plain rows, and the host says so once per language in
  the log.
