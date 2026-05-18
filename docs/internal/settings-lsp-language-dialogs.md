# UX Review: Language & LSP Settings Dialogs

**Scope.** End-user evaluation of the dialogs reached by:
*Open Settings → General → Languages → \<lang\>* and
*Open Settings → General → Lsp → \<lang\> → \<server\>*.

The base settings page (`Open Settings`, top level) is reasonably usable.
The nested dialogs are not. This review treats those nested dialogs as the
unit under test, from the perspective of a user who has never seen the
config schema and expects something close to "a web form".

---

## Method

Two independent walkthroughs, merged here.

**Session A** — launched the editor in tmux, ran `Open Settings`,
navigated keyboard‑only to:

1.  General → Languages → `hyprlang` *(language entry editor)*
2.  General → Lsp → `python` → `pylsp` *(LSP map entry → server editor)*
3.  General → Lsp → `astro` → \[+\] Add new *(brand‑new server)*

For each, tried focus cycling (Tab, ↑↓), entering and leaving edit
mode on text/number/boolean/JSON/list fields, adding & removing list
items, saving, cancelling, and using the on-screen help row.

**Session B** — separate user adding a *second* server
(`pyright-langserver`) under the existing `python` key:
Settings → search "python" → open `python` LSP value → **Add new** →
fill in Command/Name/Args/Root Markers → Save. Focus was on
keyboard model (Esc, Tab, Enter), focus traps in list rows, and
whether the dirty-state indicator and the collapsed LSP table row
reflect the saved state.

The two sessions agree on most findings; where they diverge the
divergence is preserved (see F4 / F21 on commit semantics).

---

## Summary

The dialogs work as data dumps of the underlying struct, not as forms a
human is meant to fill in. Three issues compound to make multi-list
editing actively hostile:

1.  **No visible "I am editing this field now" state** (F1) — at any
    moment the user has no way to tell whether keystrokes will
    navigate, toggle, type into a field, or be silently swallowed.
2.  **Esc inside a list row dismisses the entire enclosing dialog and
    discards every field** (F6), with no confirmation.
3.  **The trailing `[+] Add new` slot of a list traps the keyboard**
    (F19) — ↓ / Tab / → all do nothing from there; the only escape
    paths are Ctrl+S (which saves and *closes the whole dialog*) or
    Esc (which destroys all work).

Together, (2) and (3) force a save → re-open → save → re-open workflow
just to fill multiple list fields in one server config. Almost every
other complaint below stacks on top of these three.

Severity tags below: 🟥 blocks the task · 🟧 forces guessing · 🟨 polish.

---

## Top‑level finding: the LSP "Edit Value" wrapper is mostly empty

What the user sees after pressing Enter on the `python` row in the Lsp
list (matches the screenshot in the task):

```
╭ Edit Value ─────────────────────────────────────────╮
│   Key:python                                        │
│ ─────────────────────────────────────────────────── │
│ ● Value:                                            │
│>                           → pylsp [x]              │
│      [+] Add new                                    │
│                                                     │
│   ... ~30 blank lines ...                           │
│                                                     │
│            [ Save ]  [ Delete ]  [ Cancel ]         │
╰─────────────────────────────────────────────────────╯
```

Problems a first-time user hits in the first 5 seconds:

- 🟥 **The dialog looks empty.** A huge modal with one cryptic row.
- 🟧 **`→ pylsp` is centred in the row.** There is no column header,
  no label "Server" / "Command". The `→` glyph is unexplained.
- 🟧 **"Value:" is meaningless.** The user already knows this dialog is
  for the value of `python`; the label adds zero info.
- 🟧 **The wrapper exists only because the schema is `Map<String,
  Vec<LspServer>>`.** Three nesting levels (map row → list → server) are
  exposed even though 95% of languages have exactly one server. The user
  has to drill down twice for what should be one form.
- 🟥 **Delete sits between Save and Cancel.** A misclick destroys the
  entire LSP config for the language with no undo prompt.

---

## Findings — interaction model

### F1. 🟥 No edit‑mode indicator (text, number, JSON)

Steps: navigate to **Command : \[pylsp\]**, press Enter, type
`test`.

Observed: the text appears inside the field — but nothing about the field
changed when Enter was pressed. No caret, no border colour change, no
"editing" badge. The user can:

- type before pressing Enter and the keystrokes are still accepted
  (because focus already implies edit on some controls), or
- press Enter and not realise the field is now hot, so the next ↑/↓
  navigates the textbox instead of the form.

A field that is being *focused for navigation* and a field that is
*accepting characters* are visually identical.

Reinforced by session B: typing `pyright-langserver` directly on the
Command field's first focus, with no prior Enter, immediately wrote
the characters into the field. The footer says `Enter:Edit`, implying
a modal text input — but the input is in fact modeless. Either the
footer is wrong or the controls are. A new user reading the legend
will press Enter expecting to enter edit mode and will instead trigger
"commit / add another row" on list controls (see F21).

### F2. 🟧 Checkboxes look like text inputs

```
Enabled              : [ ✓ ACTIVE ]
Auto Start           : [          ]
Name                 : [                            ]
```

`[          ]` (unchecked) and `[                            ]` (empty
text) differ only in width. `[ ✓ ACTIVE ]` is shouty and asymmetric
with the unchecked state. A first-time user does not know whether
`[          ]` is a checkbox they can toggle or a string they should type
into.

Suggestion: render as `[ ]` / `[x]`, or `( ) Off` / `(•) On`, or
`Enabled: ☑ on`. The label "ACTIVE" reads like the *system* state of the
field, not the *value*.

### F3. 🟥 ↑/↓ behaviour around lists is inconsistent

In the python‑pylsp dialog, with the cursor on `Args:` (empty list
header), pressing ↓ once jumps past the `[+] Add new` line into the
next top-level field. With the cursor on `Root Markers:` (populated
list), the same ↓ stops on each item. So the same key has different
"skip" semantics depending on whether the list is empty.

Result: after adding the first item to a list, the user's keyboard map
silently changes.

### F4. 🟧 `[+] Add new` is a hidden state machine

Steps: cursor onto an `[+] Add new` line, press Enter (for primitive
lists like `Args:` / `Root Markers:`).

Observed: the line transforms in-place into
`[                  ] [+]` — a draft input plus a separate confirm
button. There is no "Adding new item..." caption, no helper text, no
visible focus on the new input. The list of existing items above is
unchanged so the eye doesn't catch the shift. Esc collapses the row
back, but again silently.

For struct lists (`[+] Add new` under `Lsp.python.Value`), Enter instead
pops a brand‑new "Add Item" dialog *on top of* the open dialog. Same
verb, two different mechanisms, no preview.

### F5. 🟧 Tab navigates invisibly

The footer says `Tab:Fields/Buttons`. In practice, Tab moves focus to
the Save/Delete/Cancel row but those buttons never paint a focused
state. The `>` cursor disappears from the field list and nothing else
lights up. The user cannot tell if Tab "did" anything until they press
Enter and something happens.

### F6. 🟥 Esc discards without confirmation — and *one level too deep*

Esc closes the current dialog level immediately. If the user typed into
Command, toggled Enabled, added a Root Marker, then pressed Esc by
reflex (e.g. to dismiss a popup that wasn't there), all of it is gone
with no "Discard changes?" prompt.

Worse, session B showed that **Esc on an in-progress list row jumps
straight past the row to dismiss the enclosing "Edit Item" dialog and
silently discards every change to every field** — Command, Name,
Args, Root Markers, all of it. A user's natural mental model is "Esc
cancels the smallest current thing": the row I'm editing, not the
whole form. Here Esc cancels the largest enclosing thing instead.

Expected: Esc on an in-progress text row commits-or-reverts only the
row; Esc on a clean dialog closes the dialog; Esc on a dirty dialog
prompts.

### F7. 🟧 Two visual focus systems on the same dialog

The selected field shows `>` at column 0. Modified fields show `●`
at column 1. Both glyphs are the same width and similar weight, and
they sit next to each other unlabelled:

```
>● Command         : [pylsp]      ← focused AND modified
 ● Name            : [     ]      ← unfocused, modified
   Enabled         : [ ✓ ACTIVE ] ← unfocused, unmodified
>  Enabled         : [ ✓ ACTIVE ] ← focused, unmodified
```

There is no legend anywhere. A user cannot guess that `●` means
"differs from default" — they will assume it's a bullet for an enabled
field, or a focus dot, or noise.

### F8. 🟨 The full main settings page sits underneath the dialog

The "Edit Value" / "Edit Item" / "Add Item" dialogs render as smaller
floating panels with the rest of the settings UI fully visible behind
them — including the unchanged language list on the left. There is no
dim/overlay, so the eye cannot tell at a glance which surface is active,
and the panel borders fight the borders of the page underneath.

---

## Findings — fields that aren't user‑editable in any practical sense

### F9. 🟥 JSON fields show literal `null`

```
Initialization Options:
  │null
Only Features:
  │null
Except Features:
  │null
Process Limits:
  │{
  │  "max_memory_percent": 50,
  │  "max_cpu_percent": 90,
  │  "enabled": true
  │}
```

What the user has to do to set, say, a `pylsp` initialization option:

1.  Guess that `null` means "no value set" (not "the JSON literal
    null").
2.  Press Enter — no visible mode change.
3.  Type valid JSON, in one inline visual line, with no syntax
    highlighting, no validation message, no completion of `{}`, no way
    to expand the area.
4.  Hope Save accepts it.

In testing, typing `{` then `}` produced the broken display
`│{` / `│{null` — the placeholder text "null" was treated as content
and the new characters were inserted alongside it. The user has no
indication this is a malformed state.

### F10. 🟧 `Process Limits` should be three fields

It already has a known shape: `max_memory_percent` (int %),
`max_cpu_percent` (int %), `enabled` (bool). Exposing it as raw JSON
makes the user re-type JSON syntax to change a number.

### F11. 🟨 Number controls waste space

```
Tab Size           : [  0 ] [-] [+]
```

The `[-]` and `[+]` buttons are small, not obviously clickable, not
documented in the footer, and redundant given the user can type the
number. Click targets in a TUI are also unreliable.

### F12. 🟧 The "Name" field on a server is unlabelled in purpose

```
● Command              : [pylsp]
● Name                 : [     ]
```

"Name" vs "Command" is ambiguous — both look like identifiers. There is
no helper text describing that Name is a display label (or whatever it
is). The user will either fill it in by guessing or leave it blank
forever.

### F13. 🟨 "── Advanced ──" is a divider, not a fold

Half the fields in the LSP server editor (Env, Language Id Overrides,
Initialization Options, Only Features, Except Features, Process Limits)
are below an `── Advanced ──` line. The line is a static separator —
the user cannot collapse it. So the dialog stays long and intimidating
even when nothing in Advanced is being changed.

---

## Findings — information architecture

### F14. 🟥 `→` glyph and unaligned columns inside list rows

In the LSP map edit dialog the only data row is rendered as:

```
>                           → pylsp [x]
```

The leading whitespace is the width of an empty `Name` column that was
never drawn. The `→` is a separator between the (missing) name and the
command. The `[x]` to remove the row sits flush against the command.
None of this is labelled. The 30+ blank rows below it make the dialog
feel broken.

### F15. 🟧 The language editor dialog is too narrow

In the `hyprlang` language editor (matches what users will see for any
language), the panel renders inside the centre column and labels
collide with values at common widths:

```
Show Whitespace Tabs: [ ✓ ACTIVE ]
Tab Size           : [  0 ] [-] [+]
Textmate Grammar   : [                            ]
```

The dialog should expand to the available width like the main settings
page does, or wrap labels onto two lines, instead of clipping.

### F16. 🟨 Search exists outside but not inside dialogs

The main settings page supports `/` to search across all settings —
this is great. Inside the language / LSP editor dialog there is no `/`
search, so a user looking for "tab size" has to scan ~25 fields by eye
even though half of them are unused defaults.

---

## Findings — destructive actions

### F17. 🟥 Save / Delete / Cancel arrangement is unsafe

```
            [ Save ]  [ Delete ]  [ Cancel ]
```

- Delete is between the two non-destructive actions.
- Delete is styled with red brackets, but so is `[ Cancel ]` in the
  outer footer — the red signal is overloaded.
- There is no confirmation step for Delete. Pressing Enter on it
  vapourises the entire server config (or language entry).

### F18. 🟧 Resetting back to default is not discoverable

Modified fields are marked with `●` but there's no per-field "reset to
default" action. The only Reset button lives in the outer settings
footer and resets *the whole page*.

---

## Findings — list rows (session B, multi-server flow)

### F19. 🟥 Focus trap in the trailing "\[+\] Add new" slot

Steps: in the "Add Item" / "Edit Item" dialog, open **Args**, type one
arg, press Enter to commit. Focus lands on the freshly-spawned empty
`[ ] [+]` row.

Observed: from that empty row,

- ↓ (any number of times) does nothing visible.
- Tab does nothing visible.
- → does nothing visible.
- Only ↑, back into the committed list, works.

So the empty add-new sentinel **traps the keyboard**. The user cannot
move forward to **Auto Start** or any later field without losing data.
The only ways out are:

- Press Ctrl+S, which saves *and closes the whole dialog* — wrong if
  the user still has other fields to fill.
- Press Esc — which discards everything per F6.

This forces a save → re-open → save → re-open workflow to fill a
multi-list form. It is the single most damaging finding alongside F6.

Suggestion: treat the trailing `[+] Add new` as a sentinel — ↓/Tab
from it must escape to the next form control, not absorb the key.

### F20. 🟥 Committed list rows appear to vanish when focus leaves the list

Observed in session B for both **Args** and **Root Markers**: after
committing one or more rows and moving focus away from the list,
previously-entered values rendered as if the list were empty (just
`[+] Add new`). Moving focus back into the list, or saving and
re-opening, showed the rows still there.

User impact: combined with F6, this turns *"did my data actually
save?"* into a real question at every step. The user starts hitting
Ctrl+S defensively (which then bites them via F22 below).

Suggestion: always render committed list rows. Use a row highlight to
mark focus inside the list rather than hiding non-focused rows.

### F21. 🟧 Commit semantics differ between text fields and list rows

- On simple text fields (Command, Name): the typed value is just
  there. Navigating away with ↑/↓ keeps it. There is no separate
  commit step.
- On list rows (Args, Root Markers): typing alone is **not** enough —
  the row is only persisted if Enter (or ↓ that crosses the list
  boundary) is pressed. If the dialog is saved while the empty
  trailing slot is focused, that slot is silently dropped (which is
  correct, but indistinguishable from data loss).

The user has no way to see whether a list row is "committed" or
"still being typed". This is a different shape of the same problem as
F1 (no edit-mode indicator), specialised to lists.

Suggestion: a clear visual cue for committed vs. pending rows (border
colour, inline ✓), or auto-commit on focus change so the model is
uniform across input types.

### F22. 🟧 "Settings \[User\] • (modified)" survives nested-dialog saves

Session B: Ctrl+S in the inner **Edit Item** dialog, Ctrl+S in the
outer **Edit Value** dialog, and the top-level
*Settings \[User\] • (modified)* title still showed the dirty
marker. A third Ctrl+S at the top level was required to flush to
disk. Nothing in the UI told the user the outer save was needed, or
which dialog level was currently dirty.

Suggestion: either flush each nested-dialog save to disk
immediately, or surface a per-level "N unsaved changes" indicator so
the save hierarchy is visible.

### F23. 🟧 Collapsed Lsp row only shows the first server

After adding `pyright-langserver` as a second server under `python`
and saving all the way out, the collapsed row in the Lsp table still
rendered just `pylsp`. The user reasonably concluded that the save
had failed and re-entered the dialog to check.

The schema supports multiple servers per language (Multi config);
the table should too.

Suggestion: render `name1, name2` (with `+N more` truncation when
long) so multi-server config is visible at a glance.

---

## Comparison: what a user expects (web form mental model)

| Web form convention                                  | Current dialog                              |
|------------------------------------------------------|---------------------------------------------|
| Focused field has a coloured border / caret          | No visible change                           |
| Checkbox looks like a checkbox                       | `[ ✓ ACTIVE ]` / `[          ]`             |
| Save disabled until something changed                | Always enabled                              |
| "Unsaved changes?" prompt on close                   | Esc silently discards                       |
| Field has a help tooltip / description               | Almost no inline help                       |
| Modified-vs-default shown with "Reset" link per row  | `●` glyph with no legend, no reset         |
| Sections collapse                                    | "── Advanced ──" is a text divider          |
| Delete is separated and confirms                     | Adjacent to Save, no confirm                |
| List add shows the new row in place with a label     | Hidden state, sometimes opens a sub‑dialog  |
| Complex value (JSON) opens in a code editor          | Single inline line, no validation           |

---

## Prioritised suggestions

### Must-fix (blocks task)

1.  **Edit-mode indicator on every control.** When a field is accepting
    keystrokes, paint a coloured border or invert the input background.
    Distinguish from "focused for navigation" (the `>` indicator).
2.  **Real checkbox glyph.** `[ ]` / `[x]` (or `☐` / `☑`), with the
    label not changing case. Drop "ACTIVE".
3.  **Esc on a list row commits-or-reverts only the row**, never the
    parent dialog. Esc on a clean dialog closes it; Esc on a dirty
    dialog prompts "Discard changes?".
4.  **Down/Tab from the trailing `[+] Add new` slot moves focus to the
    next form control.** No focus traps.
5.  **Always render committed list rows** regardless of focus — never
    collapse them to `[+] Add new` when focus is elsewhere.
6.  **Delete moved away from Save** (right edge of dialog), red and
    requiring a confirmation step ("Delete the `pylsp` server for
    Python? \[Yes\] \[No\]").
7.  **Skip the LSP map "Edit Value" wrapper for single-server
    languages.** Pressing Enter on `python` should drop the user
    straight into the server form. Show a "+ Add another server"
    affordance underneath for the rare multi-server case.
8.  **Replace `null` placeholder for JSON fields** with
    `(not set — press Enter to add)` and pop a full-size JSON editor
    (the same editor users already know from `.json` buffers) when they
    enter it. Validate on save.
9.  **Reconcile the footer legend with actual behaviour.** Either make
    text inputs truly modal (Enter to begin editing, Enter/Esc to
    commit/cancel, with a visible mode indicator) or drop `Enter:Edit`
    from the legend and document what Enter actually does on each
    control type.

### High value (forces guessing → makes obvious)

10. **Legend at top of dialog** explaining `●` = "set, differs from
    default; press Ctrl+R to reset".
11. **Per-field reset.** A small `[reset]` button or `Ctrl+R` shortcut
    on focused row, with the description in the footer.
12. **Tab focus must be visible.** Highlight whichever of
    \[Save\]/\[Delete\]/\[Cancel\] holds focus.
13. **Consistent navigation in lists.** ↓ should always step through
    list items including `[+] Add new`. Never skip from a list header
    to the next top-level field unless the list is collapsed.
14. **`[+] Add new` consistency.** For primitive lists, open the same
    "edit one row" sub-form modal that struct lists use; or for both,
    add the new row inline with a clear "Editing new item — Enter to
    save, Esc to cancel" caption.
15. **Uniform commit semantics.** Either auto-commit list rows on
    focus change (so they behave like Command/Name) or require an
    explicit commit gesture on *all* inputs. Don't mix.
16. **Show committed vs. pending list rows distinctly** — border
    colour, inline ✓, or a "(unsaved)" tag.
17. **Per-level dirty indicator.** Each open dialog shows its own
    "unsaved changes in this dialog" badge, and saves at any level
    flush all the way to disk (or surface the remaining unsaved levels
    clearly).
18. **Multi-server summary in the Lsp table row.** Render
    `pylsp, pyright-langserver` (truncate to `+N more` when long) so
    the user can see at a glance that a second server saved.
19. **Decompose `Process Limits`** into three labelled controls.
20. **Helper text** under field labels (e.g. "Name (optional): a
    friendly label used in the LSP menu"), so Name vs Command is
    obvious.
21. **Collapsible "Advanced" section** that starts collapsed.

### Polish

22. Number control: drop `[-]/[+]`. Keep `[ 0 ]` as a typed input;
    spec-allowed range can be shown in helper text.
23. Dim the page behind any active dialog (or render the dialog
    full-width like the main page).
24. Add `/` search inside the dialog to filter by field name.
25. Column headers inside list rows; drop the `→` glyph and the
    empty padding column.
26. Show "Modified" / "Inherited" / "Default" as small textual badges
    on the right edge of each row instead of relying on `●` /
    `(Inherited)` mixed signals.
27. Save button should be disabled (greyed) until the dialog has a
    real change.

---

## Appendix: literal screen captures used

### A. Python LSP map entry — the screenshot in the task

```
╭ Edit Value ───────────────────────────────╮
│   Key:python                              │
│ ───────────────────────────────────────── │
│ ● Value:                                  │
│>                           → pylsp [x]    │
│      [+] Add new                          │
│                                           │
│       [ Save ]  [ Delete ]  [ Cancel ]    │
╰───────────────────────────────────────────╯
```

### B. The pylsp server editor

```
╭ Edit Item ─────────────────────────────────╮
│  ● Command              : [pylsp        ]  │
│    Enabled              : [ ✓ ACTIVE ]     │
│  ● Name                 : [            ]   │
│    Args:                                   │
│      [+] Add new                           │
│    Auto Start           : [           ]    │
│  ● Root Markers:                           │
│      [pyproject.toml         ] [x]         │
│      [setup.py               ] [x]         │
│      [setup.cfg              ] [x]         │
│      [pyrightconfig.json     ] [x]         │
│      [.git                   ] [x]         │
│      [+] Add new                           │
│  ── Advanced ──                            │
│    Env:                                    │
│      [+] Add new                           │
│    Language Id Overrides:                  │
│      [+] Add new                           │
│  ● Initialization Options:                 │
│      │null                                 │
│  ● Only Features:                          │
│      │null                                 │
│  ● Except Features:                        │
│      │null                                 │
│    Process Limits:                         │
│      │{                                    │
│      │  "max_memory_percent": 50,          │
│      │  "max_cpu_percent": 90,             │
│      │  "enabled": true                    │
│      │}                                    │
│        [ Save ]  [ Delete ]  [ Cancel ]    │
╰────────────────────────────────────────────╯
```

### C. Footer help line (same for both)

```
↑↓:Navigate  Tab:Fields/Buttons  Enter:Edit  Ctrl+S:Save  Esc:Cancel
```

The footer is the *only* place edit mode is mentioned, and it does not
distinguish "Enter to start editing a text field" from "Enter to toggle
a checkbox" from "Enter to open a sub-dialog". All three happen on the
same key with no UI feedback.
