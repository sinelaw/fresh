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

| # | Heuristic | Issue | Severity |
|---|-----------|-------|----------|
| H1 | Visibility of System Status | No per-tab/per-buffer indicator that Compose is active. The tab strip shows `test_file.md* ×` identically in raw and compose; only the bottom status bar carries the cue. | 2 (Minor) |
| H2 | Visibility of System Status | Status message contains an untranslated i18n key after a buffer switch: `buffer.switched` (should resolve to a localized string). | 1 (Cosmetic) |
| H3 | Consistency & Standards | In the command palette, prefix-search "compose" surfaces `Markdown: Set Compose Width` *first* and the more frequently used `Markdown: Toggle Compose/Preview` second. New users hit Enter and land in the width prompt. | 2 (Minor) |
| H4 | User Control & Freedom | Undo (`Ctrl+Z`) is character-granular even for a long burst of typed text. ~80 keystrokes required ~80 undos to revert a sentence. | 2 (Minor) |
| H5 | Aesthetic & Minimalist Design | Tables render with inner row separator (`├─┼─┤`) but **no top or bottom border**. Visually the first row sits unsupported above the separator and the last row floats. | 2 (Minor) |
| H6 | Aesthetic & Minimalist Design | Code-fence blocks render every line in a single `38;5;34` (green) color. No language-aware highlighting bleeds through compose mode despite TextMate grammar being declared as the highlighting source for raw. | 2 (Minor) |
| H7 | Consistency & Standards | Cursor enters a link in compose: the conceal disappears and the line **stays raw** until the cursor moves away. Functionally correct, but the abrupt re-render with the cursor on the line makes it look like a rendering glitch the first time. | 2 (Minor) |
| H8 | Visibility of System Status | The status bar text is not responsive: at 60 columns the buffer name and `Ln/Col` indicator are silently truncated to `t  LF  ASCII  Markdown …`, hiding cursor position entirely. | 3 (Major) |
| H9 | Consistency & Standards | Compose mode width-jump test: `:23` (jump to raw line 23 == `\| 1 \| 2 \| 3 \|`) followed by 3× `→` lands the cursor at `Ln 24, Col 3` (i.e., on `End.`), skipping past the table cell entirely. The `→` arrow does not consistently traverse intra-cell positions. | 3 (Major) |
| H10 | Error Prevention & Tolerance | Malformed input (`**unclosed`, `` ``` ``unclosed-fence``, broken `\| row \|`) does **not** crash, does **not** corrupt the buffer, and styling does not bleed into surrounding text. ✅ Pass — listed for completeness. | 0 (No problem) |
| H11 | User Control & Freedom | Compose-mode toggle is per-buffer; it is not preserved when opening a new markdown file in the same session unless `Toggle Compose/Preview (All Files)` is used. The two commands are discoverable but the relationship is not explained. | 1 (Cosmetic) |
| H12 | Aesthetic & Minimalist Design | Margin/page boundaries are visually clean: page background `48;5;16`, gutter background `48;5;232`, scrollbar `48;5;7`. No bleed observed at width 80 / terminal 200. ✅ | 0 (No problem) |

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

## 6. Test Artifacts

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
