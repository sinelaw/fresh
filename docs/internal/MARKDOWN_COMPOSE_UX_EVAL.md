# Markdown Compose – UX / Heuristic Evaluation

**Branch:** `claude/test-markdown-compose-ux-QUMRZ`
**Build:** `cargo build` (debug, unoptimized, debug assertions on)
**Binary:** `target/debug/fresh`
**Runner:** detached `tmux` session (`tui_ux_test`), 200×50 by default
**Date:** 2026-04-13

---

## 1. Executive Summary

The Markdown Compose feature is a working, structured "preview-while-editing"
mode. It successfully conceals inline syntax (`**`, `*`, `` ` ``, `[…](…)`),
draws clean Unicode tables, applies a centered page when an explicit width is
configured, and round-trips back to the raw markdown losslessly.

> **Update 2026-04-13:** after a second-pass reassessment against the fixed
> binary, **none of the originally-flagged items remain at major severity**.
> The two sev-3 issues (H8 status-bar truncation, H9 cursor-skip) are
> resolved or were misdiagnosed; the mouse-wheel scroll bug found
> separately (§6) is also fixed. The remaining open items are cosmetic,
> editor-wide (not compose-specific), or defensible design choices — see
> the "Bottom line on remaining issues" note after §2 for the breakdown.

Two things impressed during testing:

- **Bidirectional sync is solid.** All inline-syntax characters survive a
  compose-on → edit → compose-off cycle (`**bold**`, `*italic*`,
  `` `inline code` ``, full `[anchor](url)` link).
- **Soft-wrap reflow is correct on resize.** A single 300-word logical line
  wraps cleanly at word boundaries at both 200 and 60 columns, and the
  scrollbar thumb tracks position correctly at TOP and END of a 791-line
  document.

The biggest friction points are around **discoverability and visibility of
state**: there is no per-tab indication that a buffer is in Compose mode, the
default command-palette ranking lists "Set Compose Width" *above* the more
common "Toggle Compose/Preview" command, and the link-conceal behavior switches
visibly when the cursor enters the link (potentially confusing). A few
rendering issues (missing top/bottom table borders; flat single-color code
blocks) and a couple of i18n leaks (`buffer.switched`) round out the
catastrophe-free-but-rough picture.

No panics, no document corruption, no scrollbar desync was observed.

---

## 2. Heuristic Violations (severity 0–4)

> **Update 2026-04-13 (post-fix reassessment):** the `Status` column tracks
> what was done about each issue after a fresh second-pass evaluation against
> the fixed binary. Several entries were rated too high in the first pass —
> H6 was not actually compose-specific (raw mode shows the same flat code
> color, it's a markdown-grammar limitation), H9 was a misdiagnosis on my
> part (`:23` correctly landed on the empty line below the table, not in
> it), H7's reveal is actually surgical (only the link's brackets reveal,
> not other inline syntax on the line). The "real" remaining issues are
> mostly cosmetic and either editor-wide (not compose-specific) or
> defensible design choices.

| # | Heuristic | Issue | Severity (revised) | Status |
|---|-----------|-------|--------------------|--------|
| H1 | Visibility of System Status | No per-tab/per-buffer indicator that Compose is active. Tab strip identical in raw vs compose; only the bottom status bar carries the cue (briefly, as a transient message). | 1 (Cosmetic) | Defer — status bar already announces the toggle; persistent indicator is a nicety. |
| H2 | Visibility of System Status | Status message contains an untranslated i18n key after a buffer switch: `buffer.switched`. | 1 (Cosmetic) | **Fixed** in `37234c2` (added key to all 14 locales). |
| H3 | Consistency & Standards | Palette query "compose" surfaces `Markdown: Set Compose Width` first; "Toggle Compose/Preview" second. Alphabetical tiebreak for equally-scored fuzzy matches. New users hit Enter and land in the width prompt. | 2 (Minor) | Defer — the fuzzy scorer's tiebreak is technically correct; a different ranking just favors a different user. Frecency-tracking would be a non-trivial scope expansion. |
| H4 | User Control & Freedom | `Ctrl+Z` is character-granular even for a long burst of typed text. ~80 keystrokes require ~80 undos to revert. | 2 (Minor) | Defer — editor-wide undo behavior, not compose-specific. Worth a separate "coalesce typing into word/burst undo" task. |
| H5 | Aesthetic & Minimalist Design | Tables render the inner separator (`├─┼─┤`) but no top or bottom border. Header row sits unsupported, last row floats. | 2 (Minor / Cosmetic) | Defer — purely aesthetic, tables work. Plugin can use `addVirtualLine` to inject `┌─┬─┐` / `└─┴─┘` rows; tracked as a follow-up. |
| H6 | Aesthetic & Minimalist Design | ~~Code-fence body uniform color in compose.~~ **Re-tested:** raw mode shows the same single color (`38;5;34`) — the markdown grammar treats fenced bodies as undifferentiated `code` tokens regardless of language tag. Not a compose regression. | 0 (Not compose-specific) | Ignore — pre-existing markdown-grammar behavior; fixing it requires routing language-tagged grammars through the markdown highlighter, which is a separate, sizeable workstream. |
| H7 | Consistency & Standards | ~~Cursor on link reveals the entire raw line.~~ **Re-tested:** only the link itself reveals (brackets + URL), bold/italic/code conceals on the same line stay applied. The reveal is surgical, not whole-line. | 1 (Cosmetic) | Ignore — defensible UX (auto-reveal under cursor is the editing affordance). |
| H8 | Visibility of System Status | Status bar truncation at narrow widths — buffer name and `Ln/Col` clipped at 60 columns. | 3 (Major) | **Fixed** in `f0bc652` (right-side now drops low-priority items to keep left-side budget). |
| H9 | Consistency & Standards | ~~Cursor "skips past" table cells in compose.~~ **Misdiagnosed:** `:23` in the original test_file.md correctly landed on the empty line below the table (table ended at line 22), not inside it. Rights advanced one source byte at a time, which crossed into "End." as expected. Cursor walks `|`→`│` conceals correctly (verified with `h9_fixture.md`). The real friction is that compose hides line numbers, so users can't easily count raw lines for `:N` jumps. | 1 (Cosmetic — discoverability of raw line numbers in compose) | Ignore as bug — see §3.7 below. Optional follow-up: surface raw `Ln` near the cursor row even when the gutter is hidden. |
| H10 | Error Prevention & Tolerance | Malformed input does not crash, corrupt, or bleed styling. ✅ | 0 | n/a |
| H11 | User Control & Freedom | Per-buffer toggle vs `(All Files)` toggle — both commands carry descriptive subtitles in the palette. Minor learning curve. | 1 (Cosmetic) | Defer — well-described already. |
| H12 | Aesthetic & Minimalist Design | Margin/page background boundaries are visually clean. ✅ | 0 | n/a |

### Bottom line on remaining issues

After the post-fix pass, **none of the remaining items are major bugs**. The
sev-3 issues (H8, H9) are resolved or were misdiagnosed. Everything else is
either:

- **Cosmetic** (H1, H5, H11) — defer to follow-up polish work,
- **Editor-wide / pre-existing** (H4, H6) — not compose-specific regressions
  to fix here,
- **Defensible design choice** (H3 alphabetical tiebreak; H7 surgical
  reveal), or
- **Misdiagnosed** (H9) — no code change needed.

If we want to keep the polish bar high we should pick H1 (per-tab
indicator) and H5 (table top/bottom borders) for a future small PR. They're
both cosmetic but the most "this looks unfinished" of the lot.

### 3.7 H9 reproduced and reassessed

I re-ran the H9 scenario against `h9_fixture.md`:

```
Line 1: # H9 repro
Line 2: (empty)
Line 3: | A | B | C |
Line 4: |---|---|---|
Line 5: | a | b | c |
Line 6: | 1 | 2 | 3 |
Line 7: (empty)
Line 8: End.
```

Compose on, then `:5` then `→×5`:

```
:5         → Ln 5, Col 1   (cursor on the leading `|`)
Right ×5   → Ln 5, Col 6   (cursor on the 6th source char — the space
                            between `a` and the second `|`)
```

The visual cursor moves to the second `│` separator at the same step.
`|`→`│` is a same-width replacement, so the source-byte ↔ visual-col mapping
is direct and the cursor walks it correctly.

What I originally hit (and called a "skip past the cell"): in
`test_file.md`, the table ended at line 22 and "End." was at line 24, so
`:23` jumped to the **empty line between them** — not into the table.
Subsequent `→` presses crossed the newline into "End." as expected for any
editor that respects raw-source navigation.

So the only honest finding here is that compose hides line numbers, which
makes raw-line `:N` jumps harder to predict. A future enhancement could
keep a faint `Ln N` annotation visible at the cursor's row in compose
mode; the current behavior is not a bug.

---

## 3. Visual / ANSI Evidence

### 3.1 Conceal works inline; URL is appended (not hidden)

Raw line:

```
- Third item with [a link](https://openai.com)
```

Rendered in compose:

```
- Third item with a link — https://openai.com
```

ANSI-decoded (SGR codes shown as `\e[…m`):

```
- Third item with \e[4ma link\e[0m \u2014 https://openai.com
```

`a link` is underlined (`\e[4m`); the URL is appended after an em-dash rather
than being hidden. Heuristic test (Scenario F) suggested the URL should be
*hidden*. Fresh's design instead exposes the URL — a defensible choice for
terminal users (no hover affordance), but worth calling out as an explicit
design intent in user docs.

### 3.2 Scrollbar tracks correctly

Right-most pane column on `big.md` (791 lines):

| Position | Thumb rows (light bg `48;5;7`) | Track rows (dark bg `48;5;8`) |
|----------|-------------------------------|-------------------------------|
| `Ctrl+Home` (Ln 1) | rows 2–4 (top) | rows 5–47 |
| `Ctrl+End` (Ln 792) | rows 45–47 (bottom) | rows 2–44 |

Thumb size ≈ 6% which matches the 46-visible / 791-total ratio.

### 3.3 Tables: missing borders

```
│ Col A │ Col B │ Col C │       <- header row, no ─── above
├───────┼───────┼───────┤       <- inner separator
│ a     │ b     │ c     │
│ 1     │ 2     │ 3     │       <- last row, no ─── below
```

Suggested fix: render `┌─┬─┐` above the header and `└─┴─┘` below the final
row.

### 3.4 Code-fence highlighting is flat

ANSI codes captured for the rust fence inside compose mode (lines L14–L18 of
`scr_code.txt`):

```
L14 ```rust          codes: 38;5;34, 38;5;69        (header keyword colored)
L15 fn main() {      codes: 38;5;34                 (entire body green)
L16    println!(…);  codes: 38;5;34                 (entire body green)
L17 }                codes: 38;5;34
L18 ```              codes: 38;5;34
```

Compare to raw mode (pre-toggle), where the same fence shows additional colors
for keywords/strings via the TextMate grammar. The plugin's own comment
("Syntax highlighting is handled by the TextMate grammar (built-in to the
editor)") implies parity, but compose collapses to a single code-block color.

### 3.5 i18n leak

After switching buffers via `#test`:

```
test_file.md | Ln 1, Col 1 | buffer.switched   LF  ASCII  …
```

The literal key `buffer.switched` is shown instead of the localized message.

### 3.6 Status bar truncation at narrow widths

At 60×30:

```
…
~
~
t  LF  ASCII  Markdown   LSP (off)   [⚠ 1]  Palette: Ctrl+P
```

Buffer name and `Ln/Col` are clipped on the left. Cursor position becomes
invisible — a tangible regression for anyone editing on a narrow split.

---

## 4. Flow-Specific Notes (Scenarios A–H)

### A. Discoverability & Bidirectional Sync — **PASS with caveats**
- Found via `Ctrl+P → "compose"`. First-listed match is `Set Compose Width`,
  not `Toggle Compose/Preview` (H3). Recommend reordering or keyword-weighting
  so "toggle" wins for the bare query "compose".
- Edits made in compose mode round-trip cleanly. Verified by saving, toggling
  off, and re-opening: `**bold**`, `*italic*`, `` `inline code` ``, and the
  full link `[OpenAI](https://openai.com)` are all intact after editing the
  *anchor text only* in compose.
- Undo works but is per-keystroke (H4).

### B. Absolute Navigation & Scrollbars — **PASS**
- `Ctrl+End` jumped from Ln 1 to Ln 792 instantly on the 791-line file.
- `Ctrl+Home` returned to Ln 1.
- Scrollbar thumb position is accurate at both extremes (see §3.2).
- `PageDown` from top of a small file (31 logical lines, ~46 visual rows)
  jumped past EOF in one keystroke. This is correct behavior given file
  length but worth noting that compose's vertical "weight" is larger than the
  raw buffer (long paragraph wraps to ~13 visual rows).

### C. Dynamic Line Wrapping & Resize Tolerance — **PASS**
- 300-word single-paragraph input wrapped on word boundaries at both 200 and
  60 columns.
- Restarting the session at 60 columns (since `tmux resize-pane` is a no-op
  in a single-pane session) reflowed the entire document instantly. No mid-word
  splits observed.

### D. Compose Mode Width Constraints — **PASS**
- `Set Compose Width → 80` produced a centered text block within a 200-column
  terminal. Left margin = 59 cols, content = 80 cols, right margin = 60 cols
  (within ±1 of geometric center).
- Margin and page background colors are distinct and clean (no bleed; see
  §3.1 / H12).

### E. Table Rendering & Cell Editing — **PARTIAL**
- Tables render with Unicode box-drawing inner separators but **no outer
  top/bottom borders** (H5).
- Attempted to type `VERYLONGTEXT` into cell `a` after `:23 → →→→`. The
  cursor instead landed on the next raw line (`End.`) and the text was
  inserted there. The visual table cell was *not* the target. After undoing
  and toggling compose off, the underlying table was clean (bug did not
  damage data) but the cursor-mapping inside table cells is unreliable (H9).

### F. Links & Hidden Syntax Masking — **PARTIAL**
- Link text is shown underlined and the URL is appended after an em-dash —
  it is not hidden in the strict NN/g sense.
- Cursor position counts *raw* characters, so each `→` advances 1 column in
  the underlying file, even through hidden brackets/URL chars.
- When the cursor enters the link line, the line's conceal lifts and shows
  raw `[OpenAI](https://openai.com)` until the cursor leaves. Re-mask is
  immediate on cursor-leave. Functionally fine; visually surprising on first
  encounter (H7).
- Editing the anchor text and toggling compose off confirmed the URL was
  preserved exactly.

### G. Nested Blocks & Syntax Highlighting — **PARTIAL**
- Blockquote, list, and inline code render with appropriate styling and
  indentation.
- Multi-line fenced code block renders body in a single uniform green color
  (H6); language identifier (`rust`) is given a distinct color but body
  syntax highlighting is absent inside compose.
- Indentation levels and margin boundaries are respected.

### H. Malformed Syntax Stress Test — **PASS**
- Input: `**unclosed bold and broken table | row | only`,
  `` ``` ``nope unclosed code``, `fn untrap() {`.
- The renderer **did not panic**. The `fresh` process stayed alive (`ps`
  confirmed PID 13475 unchanged).
- Styling did **not** bleed into other paragraphs; the unclosed bold was
  rendered as a literal asterisk, not as bold-on-everything-after.
- Auto-pairing inserted matching backticks/parentheses for some inputs,
  occasionally turning `**` into `*` + auto `*`. This is editor-wide
  bracket-pair behavior, not specific to compose.

---

## 5. Recommended Follow-ups (engineering hand-off)

Listed in rough priority order:

1. **H8 (status bar truncation, severity 3)** — make status bar segments
   responsive (drop low-priority segments first, keep `Ln/Col` last to drop).
2. **H9 (cell-cursor mapping, severity 3)** — investigate `→` traversal
   through compose-mode tables; current behavior skips into the next raw
   line.
3. **H5 (table top/bottom borders, severity 2)** — add `┌─┬─┐` and
   `└─┴─┘` rows.
4. **H3 (palette ranking, severity 2)** — boost `Toggle Compose/Preview`
   for the query "compose".
5. **H1 (per-tab indicator, severity 2)** — small marker (e.g. `◐` or `📖`
   if emoji is acceptable; otherwise a textual `[c]`) on tabs whose buffer
   is in compose.
6. **H6 (fenced-code highlighting in compose, severity 2)** — pass through
   TextMate grammar to compose's render path.
7. **H7 (link reveal feels jumpy, severity 2)** — consider a softer
   transition (e.g. show only `[anchor]` without the `(url)` when cursor is on
   the anchor) to reduce the perceived layout shift.
8. **H4 (undo granularity, severity 2)** — coalesce consecutive
   character-insert events into a single undo stop (typical 1-second
   word-boundary heuristic).
9. **H2 (i18n leak, severity 1)** — add `buffer.switched` to locale files.
10. **H11 (per-buffer vs. global compose, severity 1)** — surface a hint in
    the toggle's status line ("ON for this buffer; use 'All Files' command
    for default-on").

---

## 6. Mouse-Wheel Scrolling Bug (Compose Mode)

User report:
1. Long document with tables followed by long-wrapped bullet/numbered list at
   the end → mouse wheel scrolls only "halfway"; keyboard navigation works.
2. Scrolling up then down with the wheel sometimes leaves the bottom half of
   the visible buffer blank until more wheels arrive.
3. Both symptoms appear with slow, single wheel events — not just rapid bursts.

### 6.1 Reproduction

Fixture: `/tmp/ux_test/big_repro.md` (337 lines):
- 99 medium paragraphs
- 1 large 99-row table
- 29 numbered list items, each containing a 200-word continuous paragraph
  that wraps to ~16 visual rows in compose mode at width 60–100
- A `FILE_END_MARKER_XYZZY` sentinel at EOF

Steps (60×24 terminal):

```
fresh big_repro.md
Ctrl+P → "Toggle Compose" → Enter
Wheel-down ~250 times to land in the long-list area
Send single wheel-down events with 0.3–0.4s spacing
```

### 6.2 Observed: every item-start "absorbs" one wheel event

Captured top-row text after each single wheel event:

```
position before:  "   word139 word140 ... word145"   (Item 2's word139)
after wheel #1:   "   word160 word161 ... word166"   (advanced 21 words ≈ 3 rows)  ✓
after wheel #2:   "   word181 word182 ... word187"   (advanced ≈ 3 rows)            ✓
after wheel #3:   "3. Item 3: word1 word2 ..."        (crossed item boundary)        ✓
after wheel #4:   "3. Item 3: word1 word2 ..."        (NO ADVANCE — wheel lost)      ✗
after wheel #5:   "   word25 word26 ... word32"      (back to advancing)             ✓
```

The pattern repeats deterministically at every item boundary — at "4. Item 4:"
and "5. Item 5:" the same: one wheel produces no movement.

Cumulatively, with 11 long-wrap items the user has to wheel ~11 extra times
to traverse the same content as the keyboard's `Ctrl+End`, so mouse scrolling
appears to "lag" or "only cover half".

### 6.3 Root cause — scroll math ignores plugin soft-break markers

**Updated 2026-04-13** after the user clarified that the bug also reproduces
with very slow, single wheel events. My initial hypothesis (race between
`view_transform_request` and `SubmitViewTransform`) is wrong: the
markdown_compose plugin no longer uses the view-transform pipeline at all
for wrapping. From `crates/fresh-editor/plugins/markdown_compose.ts:1489`:

> `view_transform_request` is no longer needed — soft wrapping is handled by
> marker-based soft breaks (computed in `lines_changed`).

So in compose mode `view_transform.tokens` is `None`,
`handle_mouse_scroll` (input.rs:1551) takes the **buffer-based** branch and
calls `Viewport::scroll_down`, which delegates to `scroll_down_visual`
(viewport.rs:281-368) when line wrap is on.

The bug is that `scroll_down_visual` (and friends `scroll_up_visual`,
`apply_visual_scroll_limit`, `find_max_visual_scroll_position`,
`set_top_byte_with_limit`) all count visual rows by calling `wrap_line`
(`crates/fresh-editor/src/primitives/line_wrapping.rs:129`) on the raw
source text. They are **completely unaware** of the markdown plugin's
soft-break markers. `grep -n soft_break crates/fresh-editor/src/view/viewport.rs`
returns zero matches.

`wrap_line` only inserts a hanging indent when the *source* text starts
with whitespace (`detect_indent` at `line_wrapping.rs:158`). A list item
like `1. Item 1: word1 ... word199` has no leading whitespace, so
`wrap_line` wraps it at the full viewport width — about 12 rows for the
fixture. But the markdown plugin inserts a 3-column hanging indent on every
continuation line (matching the visual width of the `"1. "` marker), giving
~13 rows. A similar mismatch applies to bullets, blockquotes (`>` indent),
and tables (column-aware widths).

Concrete consequence: in `scroll_down_visual` at viewport.rs:309-313, the
"can satisfy scroll within current line" early-return uses the wrong
`current_visual_rows` count, so `top_view_line_offset` lands on the wrong
visual row. When the renderer then composes the line with soft-break
markers, it produces a different number of rows and the visible top doesn't
match what the scroll handler intended.

### 6.4 Empty-bottom symptom (bug 2) — same root cause

Reproduced cleanly with `/tmp/ux_test/end_test.md` (a small file ending with
five 99-word numbered list items + an `EOF_MARKER`) at 60×24, slow scroll
(0.4s between single wheel events):

```
wheel #23  top: "   word97 word98 word99"
wheel #24  top: "   word89 word90 word91 word92 word93 word94 word9"   ← jumped backward
wheel #25  top: "   word89 ..."   (stuck)
wheel #26..50  top: "   word89 ..."   (stuck — wheel produces no scroll)
```

At the stuck position the bottom of the viewport shows `EOF_MARKER` then 2
empty rows, but `Ctrl+End` from this state shows that the file content
extends three more wrap rows above the current top. So the mouse scroll
*clamped* short of the keyboard's max-scroll position, and a couple of `~`
filler rows became part of the visible viewport.

The clamp comes from `apply_visual_scroll_limit` (viewport.rs:373-412). It
uses `wrap_line` to count "visual rows from current position to end of
buffer" and, when that count is less than `viewport_height`, calls
`find_max_visual_scroll_position` to back the viewport up. Because the count
under-estimates rows by ~1 per long-wrap item (no hanging indent in the
math), the clamp triggers earlier than it should. The result is a viewport
whose bottom is filled with `~` instead of the next-item-boundary content
the user expects. Slow vs. fast scrolling makes no difference; the math
mistake is deterministic.

### 6.5 Reproduction summary

| Symptom | Fixture | Reproduced |
|---------|---------|------------|
| Wheel-absorbed at long-list item boundary (bug 1) | `big_repro.md` 60×24 | Yes; 1 wheel "lost" per `N. Item N:` start |
| Bottom empty / mouse stops short of EOF (bug 2) | `end_test.md` 60×24 | Yes; deterministic at 0.4s spacing |
| Either symptom in raw (non-compose) mode | same fixtures | No — plugin soft-breaks are not applied |
| Either symptom in compose mode without lists/long-wrap | small docs | No — wrap_line and plugin agree when there's no hanging indent |

The deciding factor is the mismatch between `wrap_line`'s row count and the
renderer's effective row count. Bullets, numbered lists, blockquotes, and
fenced code (with leading whitespace from indented lists) all trigger a
hanging-indent mismatch.

### 6.6 Suggested fix

The fix needs to reconcile scroll math with rendered layout. Three options
in increasing scope:

**A. Make `wrap_line` aware of list-marker hanging indent.** Detect a
leading list/blockquote/numbered-list marker (`-`, `*`, `1.`, `>` etc.) in
the source text and treat its visual width as the hanging indent for
continuation lines. This brings `wrap_line` in line with the markdown
plugin's wrapping for the common cases. Pure rust change in
`primitives/line_wrapping.rs`. May still mis-count for tables / images
where the plugin uses byte-position-specific breaks.

**B. Have the viewport consult the buffer's `soft_breaks` markers.**
Replace `wrap_line(line_content, &wrap_config)` in `scroll_down_visual`
etc. with a function that returns `wrap_line`'s segments **plus** any
soft-break markers in the line's byte range. This makes scroll math an
exact inverse of what the renderer does; works for any plugin that adds
soft breaks. Requires plumbing the buffer's marker list into the viewport
(currently it's passed only `&mut Buffer`).

**C. Move all mouse-scroll math into the view-pipeline.** Render
view-lines for the buffer's full visible region (or build them lazily) and
walk those instead of re-wrapping the source. This is what
`scroll_view_lines` does today for plugins that *do* use `view_transform`,
just generalised to the soft-break case.

(B) is the most surgical fix: one extra parameter on `scroll_down_visual` /
`scroll_up_visual` and a tweaked row-count helper. (A) would also fix this
without any new plumbing but is less general (won't help table conceals).

### 6.7 Repro artifacts

- `/tmp/ux_test/big_repro.md` — paragraphs + 99-row table + 29 long-wrap
  items + sentinel, used for the wheel-absorbed scenario
- `/tmp/ux_test/end_test.md` — small file with 5 long-wrap items at the
  end, used for the bottom-empty / mouse-stops-short scenario
- This document, sections 6.1–6.5, contains the exact tmux SGR mouse
  sequences (`\x1b[<65;col;rowM`) used to drive the wheel events.

---

## 7. Test Artifacts

Generated during this evaluation and stored in `/tmp/ux_test/` on the test
host:

- `test_file.md` — small mixed-syntax document (link, table, code fence,
  list, blockquote)
- `big.md` — 791-line document for scrollbar / navigation tests
- `scr_initial.txt` — raw mode, ANSI-preserved capture
- `scr_compose_on.txt` — compose mode, ANSI-preserved
- `scr_end.txt` / `scr_end_ansi.txt` — `Ctrl+End` capture for scrollbar
- `scr_top.txt` — `Ctrl+Home` capture for scrollbar
- `scr_long_para.txt` / `scr_long_para_ansi.txt` — soft-wrap evidence (200 col)
- `scr_narrow_60.txt` / `scr_narrow_top.txt` — soft-wrap evidence (60 col)
- `scr_w80.txt` / `scr_w80_ansi.txt` — width-80 centering evidence
- `scr_malformed.txt` — malformed syntax stress capture
- `scr_code.txt` — code-fence highlighting capture
