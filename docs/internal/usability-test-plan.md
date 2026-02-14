# Usability Test Plan: Markdown Compose Seamless Canvas

## Test Environment
- Terminal: tmux session
- Test file: `test_usability.md`
- Editor: `./target/release/fresh test_usability.md`

## Test Cases

### T1: Compose Mode Toggle
**Steps:**
1. Open test_usability.md
2. Verify file opens in normal mode (raw markdown visible, line numbers shown)
3. Open command palette (Ctrl+P or equivalent)
4. Search for "Markdown: Toggle Compose"
5. Execute the command

**Expected:**
- Line numbers disappear
- Text reflows to 80-char compose width
- Emphasis markers get styled (bold, italic, etc.)
- Status bar shows compose mode enabled

**Reverse:**
6. Toggle again → back to normal mode with line numbers

---

### T2: Emphasis Concealment (Cursor Away)
**Steps:**
1. Enable compose mode
2. Move cursor to a line WITHOUT emphasis (e.g., the "## Emphasis Tests" heading)
3. Look at lines with `**bold text**`, `*italic text*`, etc.

**Expected:**
- `**bold text**` → shows "bold text" in bold (no asterisks visible)
- `*italic text*` → shows "italic text" in italic (no asterisk visible)
- `` `inline code` `` → shows "inline code" with background highlight (no backticks visible)
- `~~strikethrough text~~` → shows "strikethrough text" struck through (no tildes visible)
- `***bold italic text***` → shows "bold italic text" bold+italic (no asterisks visible)

---

### T3: Emphasis Cursor-Aware Reveal
**Steps:**
1. In compose mode, navigate cursor INTO the word "bold" on the `**bold text**` line
2. Observe what happens to the asterisks

**Expected:**
- When cursor enters the `**bold text**` span, the `**` markers appear
- When cursor leaves (move to different line), markers disappear again
- Same behavior for all emphasis types (italic, code, strikethrough, bold-italic)

---

### T4: Link Concealment (Cursor Away)
**Steps:**
1. In compose mode, move cursor away from link lines
2. Look at `[Fresh Editor](https://github.com/user/fresh)`

**Expected:**
- Shows just "Fresh Editor" (no brackets, no URL)
- Text styled with blue color and underline
- Image link `![image alt](...)` is NOT concealed (stays raw)

---

### T5: Link Cursor-Aware Reveal
**Steps:**
1. Move cursor to the "Fresh Editor" link text
2. Move cursor away

**Expected:**
- When cursor enters link span → full `[Fresh Editor](https://github.com/user/fresh)` syntax revealed
- When cursor leaves → concealed back to just "Fresh Editor"

---

### T6: Table Grid Rendering (Cursor Away)
**Steps:**
1. In compose mode, move cursor away from both tables
2. Observe table rendering

**Expected:**
- Pipe characters `|` → box-drawing `│`
- Separator row `|---------|--------|-------|` → `├─────────┼────────┼───────┤`
- Header and data rows use `│` dividers
- Table structure looks like a proper grid

---

### T7: Table Cursor-Aware Reveal
**Steps:**
1. Move cursor onto a table data row (e.g., the "Alice | 30 | NYC" row)
2. Observe that row

**Expected:**
- The row where cursor sits shows raw `|` characters
- Other table rows still show box-drawing characters
- Moving cursor to different row: previous row conceals, new row reveals

---

### T8: Visual Line Cursor Movement
**Steps:**
1. In compose mode, navigate to the long paragraph in "## Wrapping Test"
2. Position cursor at start of the wrapped paragraph
3. Press Down arrow repeatedly

**Expected:**
- Cursor moves to the next VISUAL line (not jumping to the next logical line)
- Each Down press moves through the visual wrapped lines
- Up arrow works symmetrically
- Cursor column position is maintained (or adjusted to line length)

---

### T9: Multi-Cursor Conceal
**Steps:**
1. In compose mode, place cursor on a bold span
2. Add a second cursor (if multi-cursor is supported, e.g., Ctrl+D or similar)
3. Place second cursor on a link span

**Expected:**
- Both the bold span (where cursor 1 is) and the link span (where cursor 2 is) show raw syntax
- Other spans remain concealed
- (If multi-cursor isn't easily accessible, skip this test)

---

### T10: Soft Wrapping and Hanging Indents
**Steps:**
1. In compose mode, look at the long bullet items in "## List with Long Text"
2. Observe wrapping behavior

**Expected:**
- Long list items wrap at ~80 chars
- Continuation lines are indented to align with the text after the bullet marker (hanging indent)
- Block quote text wraps with proper continuation indent
- Code blocks do NOT wrap (displayed as-is)
- Table rows do NOT wrap

---

## Results (2026-02-12)

| Test | Result | Notes |
|------|--------|-------|
| T1: Compose toggle | PASS | Line numbers hide, text reflows, status bar shows "ON" |
| T2: Emphasis concealment | PASS | All 5 types work (bold, italic, code, strikethrough, bold-italic) |
| T3: Emphasis cursor reveal | PASS | `**` markers appear on cursor entry, hide on exit |
| T4: Link concealment | PASS | `[text](url)` → styled "text"; `![img]()` unaffected |
| T5: Link cursor reveal | PASS | Full `[text](url)` syntax shown when cursor in link |
| T6: Table grid rendering | PASS | `│`, `├──┼──┤` box-drawing, proper corners |
| T7: Table cursor reveal | PASS | Raw pipes on cursor row, box-drawing on others |
| T8: Visual line movement | PASS | Down: Col 1→79→160→236→next line. Up: reverse |
| T9: Multi-cursor conceal | SKIP | Not manually tested (API supports it) |
| T10: Wrapping/hanging indent | PASS | Lists: 2-space hanging indent, code: no wrap |

### Editing Tests

| Test | Result | Notes |
|------|--------|-------|
| T11: Edit emphasis text | PASS | Typed "VERY " inside `**bold text**` → `**VERY bold text**` |
| T12: Edit link text | PASS | Typed " v2" inside link → `[Fresh v2 Editor](url)` preserved |
| T13: Edit table cell | PASS | Typed "X" after "Alice" → `| AliceX | 30 | NYC |` correct |
| T14: Edit wrapped paragraph | PASS | Typed at start → text reflows, wrapping adjusts correctly |

### Bugs Found During Testing

1. **Render glitch on scroll**: Briefly shows raw markdown before plugin transform renders
2. **Cursor/overlay flicker while typing**: Highlights momentarily jump (off-by-one frame lag)
3. **Mouse wheel scroll**: Does not work in compose mode
4. **Table columns not aligned**: No auto-padding to equal column widths
