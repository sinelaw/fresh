# Embedded-Language Highlighting (Mixed-Language Files)

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status: IMPLEMENTED** for TextMate-engine hosts, with Markdown fenced code
blocks as the first (and motivating) case — issue #2689. Companion to
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
   Block-delimited, line-granular regions only.

3. **`highlight_string`**: one-shot highlighting of a detached string
   (hover popups, the markdown *preview* renderer). Never use it for
   buffer content — it has no incrementality and no cache.

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
second lexer. A small per-host spec table declares two scope selectors:

- `region_scope` — the scope the host grammar keeps on the stack for the
  whole region (Markdown: `markup.raw.code-fence`);
- `language_scope` — the scope the host grammar puts on the language
  token of the opening line (Markdown: `constant.other.language-name`).

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

### Adding a new host

Add one `EmbeddingSpecDef` entry (host syntax name + the two scope
selectors) in the engine, and unit tests mirroring the Markdown ones:
recognized region, unrecognized language fallback, an edit to the
language token restyling the whole region, and a region past
`MAX_PARSE_BYTES`. If the host grammar doesn't scope a language token or
region, fix the grammar first (tool 1) — the engine mechanism assumes the
host grammar tells the truth about regions.

### Non-goals / future

- The tree-sitter backend (JS/TS/JSON/Templ/Go fallback) has injections
  explicitly disabled and none of those languages currently host embedded
  regions; if one ever does, tree-sitter injection queries are the natural
  analogue there.
- The WASM-reserved `textmate_engine.rs` mirror does not implement the
  mechanism yet.
