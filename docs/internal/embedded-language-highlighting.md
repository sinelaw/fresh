# Embedded-Language Highlighting (Mixed-Language Files)

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: IMPLEMENTED** for TextMate-engine hosts: Markdown fenced code
blocks (the motivating case — issue #2689) and Vue `<script>`/`<style>`
blocks (the proof of generality: two region kinds in one host, `lang`
attributes, and default languages, added with spec-table entries plus
grammar scopes and no engine control-flow changes). Companion to
[`syntax-highlighting.md`](syntax-highlighting.md), which describes the
checkpoint/incremental engine this mechanism extends.

## The problem

Some files legitimately contain more than one language: Markdown fences,
HTML `<script>`/`<style>`, Vue/Svelte components, templating languages. The
highlighting engine is deliberately incremental and viewport-bounded
(checkpoints + windowed parsing, no full-buffer rescans on edit), so any
mixed-language support has to preserve those properties. A point fix that
scans the whole buffer for fences on every edit, caches per buffer version,
and turns itself off above a size threshold breaks both design rules
("avoid full-buffer scans", no size cliff) — that's what this mechanism
replaces.

## Three existing tools, and when to use which

There are now three ways to get a second language highlighted, and they are
**not** interchangeable. Rule of thumb for future mixed-language cases:

1. **The grammar itself embeds the other language** (via the shared
   `SyntaxSet`): HTML embedding CSS/JS, PHP embedding HTML, and most
   templating languages work this way already, because TextMate grammars
   can push contexts from *another* grammar in the same set. The engine's
   sequential `ParseState` tracks those transitions natively — checkpoints,
   forward extension and convergence all just work (see the embedded-CSS
   e2e tests). **Reach for this first** whenever the embedded language is
   *statically known to the host grammar*. It's pure grammar data: add or
   extend a `.sublime-syntax` in the build-time dump; zero engine changes.
   This is also the answer for genuinely *interleaved* templating (Jinja,
   ERB, Twig-style): a template grammar that switches contexts mid-line
   between template markers and host text is exactly what TextMate
   grammars are good at, and the engine already supports it.

2. **The engine-level embedded-region mechanism** (this doc): for hosts
   where the embedded language is *named by the document*, so no grammar
   can enumerate it statically. Markdown fences are the canonical case:
   the info string ("```rust", "```py", "~~~{.python}") can name any of
   the ~140 syntaxes in the set, including user/plugin-registered ones.
   Vue single-file components are the second: `<script lang="...">` /
   `<style lang="...">` name the language, with js/css as per-region
   defaults when no `lang` is given. Block-delimited, line-granular
   regions only.

3. **`highlight_string`**: one-shot highlighting of a detached string
   (hover popups, the markdown *preview* renderer). Never use it for
   buffer content — it has no incrementality and no cache.

4. **Regions declared by the plugin** (`setSyntaxRegions`): for buffers a
   plugin *composes* — the Review Diff stream, a log — which are not
   documents any grammar can parse (a gutter, headers, comment boxes, rows
   from many files and two sides of each). The plugin declares byte-range
   regions, each with a language (a path or token), a per-row prefix to
   skip, and the parser *streams* its rows feed; the engine becomes a
   "region host" (plain text of its own) and runs the child parsers over
   the declared rows through the same checkpointed, viewport-windowed
   path as tools 1 and 2. Regions are span markers, so edits move them.
   Streams are what make an interleaved diff correct: the old and new
   side of a hunk each keep their own parser across the other side's rows
   and across anything else in between, so a docstring is one docstring.

## How the engine-level mechanism works

The TextMate engine's resumable parse state is a **composite snapshot**:
the host parser's `(ParseState, ScopeStack)` plus, while inside a
recognized region, an embedded child parser's `(syntax, ParseState,
ScopeStack)`. That snapshot — not just the host state — is what
checkpoints, the cache tail state, and the convergence comparison carry.
Because every incremental path already flows through those snapshots, the
mechanism inherits the engine's whole lifecycle for free: resume-from-
checkpoint into the middle of a region, forward extension while scrolling,
partial update with convergence after edits, and the streaming-tail rules.

Region detection is driven by the **host grammar's own scopes**, not by a
second lexer. A host declares one spec per region *kind* (Vue has two:
script and style), each with two scope selectors and an optional default:

- `region_scope` — the scope the host grammar keeps on the stack for the
  whole region (Markdown: `markup.raw.code-fence`; Vue:
  `meta.embedded.block.script` / `meta.embedded.block.style`);
- `language_scope` — the scope the host grammar puts on the language
  token of the opening line (`constant.other.language-name` for both);
- `default_language` — used when the opening line names no language *or*
  names one that doesn't resolve to a syntax in the set. Vue uses js/css;
  Markdown uses `None`, meaning such regions keep the host's own raw-code
  styling.

A compact TextMate TypeScript grammar is bundled *solely* for embedded
contexts (`lang="ts"` in Vue, ```ts fences): the grammar catalog skips it
by name — mirroring the JavaScript skip — so `.ts` buffers keep the richer
tree-sitter highlighting, while `find_syntax_by_token("ts")` still
resolves it for regions. (Vendoring Sublime's official TS grammar is not
an option: it needs `branch_point`, which no released syntect supports.)

Per line, the host parser runs first (inside a region it is in a cheap
"raw" context — it must run regardless, because only the host knows where
the region ends), and the region-scope presence at line start vs end
classifies the line:

| region scope before → after | meaning | styled by |
|---|---|---|
| absent → absent | ordinary host line | host |
| absent → present | region opened; language token resolved via `find_syntax_by_token` | host |
| present → present | region content | child (host, if language unrecognized) |
| present → absent | closing delimiter; child state dropped | host |

Driving detection off the host grammar's scopes has a correctness property
worth preserving: the highlighted region is *exactly* what the grammar
recognizes as a region (marker-length rules, indentation quirks and all),
so region styling can never disagree with the fence rendering itself. It
also means zero extra scanning: detection rides along the line parse the
engine was doing anyway.

Unrecognized languages (and fences with no info string, and anything
resolving to plain text) keep the host's own styling (`markup.raw` →
string color), so nothing regresses.

### Costs and limits

- Content lines are parsed twice (host raw-context + child), but the host
  side is a near-no-op; measured behavior is dominated by the child parse
  the feature exists to perform.
- Snapshots inside regions are roughly twice the size; checkpoint spacing
  is unchanged.
- One nesting level: the child parser is never itself region-scanned.
  (A fence inside a fence is host-terminated at the first closing marker
  anyway, so deeper nesting cannot arise for line-delimited regions.)
- **Convergence granularity**: syntect states that carry regex captures
  compare unequal even when logically identical (`onig::Region` equality
  is allocation identity — a clone is already unequal to its original;
  pre-existing, also true on the host-only tuple this replaced). The fence
  context holds captures for its close-marker backreference, so an edit
  inside a region re-parses to the *region's end* and converges at the
  first checkpoint after it, still bounded per pass by the convergence
  budget. Fixing that requires value-equality regions upstream (syntect's
  `regex-fancy` backend has them; `regex-onig` does not).
- Cold-starting a viewport in the middle of a huge region without nearby
  checkpoints shows host-default styling until checkpoints exist — the
  same documented trade-off the engine already makes for all multi-line
  constructs (strings, block comments, HTML `<style>`).

### Region membership as an exported fact

The per-line classification in the table above is not only used to pick a
parser — it is the editor's *only* authoritative answer to "is this line
inside a fence", and `TextMateEngine::region_lines_in` exports it (a thin
projection of `structure_lines_in`, which also classifies tables — see
"Generalizing the pattern" below). The
`lines_changed` plugin hook carries it per line as
`region: "open" | "body" | "close"`, and compose mode's fenced-code framing
is built on it.

Why it has to come from here. Region membership is the one property of a
Markdown line that cannot be derived from the line's own text: a bare
` ``` ` opens or closes depending on every fence above it (only a fence
*with* an info string is unambiguously an opener, per CommonMark). A
decoration plugin sees one `lines_changed` batch — the lines an edit or a
scroll touched — and cannot read the buffer above it synchronously, so any
plugin-side rule frames a block whose opening fence happens to be visible
and gives up on one that isn't: a frame that appears and disappears with
scroll position. Nor can it be memoised plugin-side; a memo of fence
extents carries *structure*, and stale structure means framing the wrong
lines (contrast the compose table-width memo, which carries only numbers).

Driving it off the same classification that picks the parser also means the
frame can never disagree with the colouring inside it. A fence the grammar
does not recognize as a region — a ` ```js ` abutting a table row with no
blank line between, which the Markdown grammar does not open a region for —
is left unhighlighted *and* unframed, rather than framing a block the
highlighter does not believe in.

### Generalizing the pattern: tables

Tables are the first case that borrowed the *pattern* without belonging to
the mechanism. A Markdown table embeds no second language, so it has no
`EMBEDDING_SPECS` entry and `region_lines_in` correctly reports nothing for
its lines. What transfers is the shape of the answer: a per-line structural
classification the host grammar already makes while parsing, delivered on
the `lines_changed` payload, so the consumer stores no block model.

`structure_lines_in` is the single walk that produces both. Tables get
their own tiny spec table (`TABLE_SPECS`: host syntax → table *body*
scope, `meta.table` for Markdown) and their own vocabulary, because
`open`/`body`/`close` does not describe a table:

| line | `meta.table` before → after | reported as |
|---|---|---|
| header row | absent → absent | `header` (the line *above* a delimiter) |
| delimiter row | absent → present | `delimiter` |
| data row | present → present | `row` (`first_row` on the one below the delimiter) |
| line below the table | present → absent | nothing; the line above it is marked `last` |

Two things fall out of that table and are worth stating plainly:

- **The header is not covered by the body scope.** The grammar opens
  `meta.table` on the *delimiter* row, so a pure before/after test cannot
  see the header at all. It is identified positionally, as the line above
  the delimiter — which also means a walk that resumes from a checkpoint
  *inside* a table never claims to know where that table began. That is
  the honest answer, and it costs nothing: a header above the resume
  anchor is off-screen anyway.
- **`last` needs the line below.** It is the one fact here that a forward
  walk cannot settle in place, so the walk runs one line past the range it
  was asked about (`TABLE_LOOKAHEAD_BYTES`). Where it cannot — the range
  ends at the lookahead bound, or the walk stopped early — `last` stays
  false, which draws no closing edge rather than one in the wrong place.

The consequence for consumers is different from regions, and the
difference is the point. Region membership has no fallback: absence must
be read as *unknown*, because a bare ` ``` ` tells you nothing on its own.
"Is this line a table row" **is** line-local, so a consumer may fall back
to its own text rule — and compose mode does, deliberately. The grammar
scopes fewer things as tables than the plugin renders as tables (a table
inside a blockquote is not scoped; a table interrupting a paragraph is
correctly not scoped, since GFM tables cannot interrupt one), and a buffer
too large to resolve would otherwise lose every frame it has today. So the
editor's answer is authoritative *when present*, and the batch-local rules
stay as the degradation path.

What this does **not** fix is column widths. Knowing a table's extent is
not the same as being able to read its off-screen rows, which a plugin
still cannot do synchronously, so compose mode's grow-only width memo
stays. That memo is safe for the reason it always was: it carries only
numbers, where staleness costs a column width for one frame.

`region_lines_in` is a probe, not a cache: it resumes from the nearest
checkpoint and discards what it computes, so it stays out of the three
incremental cache paths and cannot perturb highlighting. Two rules keep it
from answering confidently-wrong, both stricter than the highlight paths'
own resume logic, because a wrong region is a wrong *frame* rather than a
wrong colour:

- it never resumes from a fresh state mid-file (a fresh state reads as
  "outside a region"), only from a real checkpoint or byte 0;
- it never resumes from a checkpoint at or after `dirty_from`.
  `notify_insert`/`notify_delete` shift checkpoint *positions* but leave
  their stored states for `try_partial_update` to repair lazily, so a
  checkpoint past an unrepaired edit can still hold pre-edit region state.

When neither is available — a buffer past `MAX_PARSE_BYTES` whose viewport
has no checkpoint before it yet — it reports nothing, and consumers must
read that as *unknown*, not as "outside a region". That is the same
cold-start trade-off this engine already makes for styling inside a huge
region.

### Adding a new host

Add one `EmbeddingSpecDef` entry per region kind (host syntax name, the
two scope selectors, and the optional default language) in the engine,
and unit tests mirroring the Markdown/Vue ones:
recognized region, unrecognized language fallback, an edit to the
language token restyling the whole region, and a region past
`MAX_PARSE_BYTES`. If the host grammar doesn't scope a language token or
region, fix the grammar first (tool 1) — the engine mechanism assumes the
host grammar tells the truth about regions. That is exactly what the Vue
grammar needed: its hand-rolled pseudo-JS/CSS contexts were replaced with
honestly-scoped raw regions (`meta.embedded.block.*`) plus a scoped `lang`
attribute value, and the engine does the rest.

### Non-goals / future

- The tree-sitter backend (JS/TS/JSON/Templ/Go fallback) has injections
  explicitly disabled and none of those languages currently host embedded
  regions; if one ever does, tree-sitter injection queries are the natural
  analogue there.
- The WASM-reserved `textmate_engine.rs` mirror does not implement the
  mechanism yet.
