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

Launched the editor in tmux, ran `Open Settings`, navigated keyboard‑only
to:

1.  General → Languages → `hyprlang` *(language entry editor)*
2.  General → Lsp → `python` → `pylsp` *(LSP map entry → server editor)*
3.  General → Lsp → `astro` → \[+\] Add new *(brand‑new server)*

For each I tried: focus cycling (Tab, ↑↓), entering and leaving edit mode
on text/number/boolean/JSON/list fields, adding & removing list items,
saving, cancelling, and using the on-screen help row.

---

## Summary

The dialogs work as data dumps of the underlying struct, not as forms a
human is meant to fill in. The single most damaging issue is **there is
no visible "I am editing this field now" state** — at any moment the
user has no way to tell whether keystrokes will navigate, toggle, type
into a field, or be silently swallowed. Almost every other complaint
below stacks on top of that.

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

### F6. 🟥 Esc discards without confirmation

Esc closes the current dialog level immediately. If the user typed into
Command, toggled Enabled, added a Root Marker, then pressed Esc by
reflex (e.g. to dismiss a popup that wasn't there), all of it is gone
with no "Discard changes?" prompt.

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
3.  **Esc confirmation when there are unsaved changes.**
4.  **Delete moved away from Save** (right edge of dialog), red and
    requiring a confirmation step ("Delete the `pylsp` server for
    Python? \[Yes\] \[No\]").
5.  **Skip the LSP map "Edit Value" wrapper for single-server
    languages.** Pressing Enter on `python` should drop the user
    straight into the server form. Show a "+ Add another server"
    affordance underneath for the rare multi-server case.
6.  **Replace `null` placeholder for JSON fields** with
    `(not set — press Enter to add)` and pop a full-size JSON editor
    (the same editor users already know from `.json` buffers) when they
    enter it. Validate on save.

### High value (forces guessing → makes obvious)

7.  **Legend at top of dialog** explaining `●` = "set, differs from
    default; press Ctrl+R to reset".
8.  **Per-field reset.** A small `[reset]` button or `Ctrl+R` shortcut
    on focused row, with the description in the footer.
9.  **Tab focus must be visible.** Highlight whichever of
    \[Save\]/\[Delete\]/\[Cancel\] holds focus.
10. **Consistent navigation in lists.** ↓ should always step through
    list items including `[+] Add new`. Never skip from a list header
    to the next top-level field unless the list is collapsed.
11. **`[+] Add new` consistency.** For primitive lists, open the same
    "edit one row" sub-form modal that struct lists use; or for both,
    add the new row inline with a clear "Editing new item — Enter to
    save, Esc to cancel" caption.
12. **Decompose `Process Limits`** into three labelled controls.
13. **Helper text** under field labels (e.g. "Name (optional): a
    friendly label used in the LSP menu"), so Name vs Command is
    obvious.
14. **Collapsible "Advanced" section** that starts collapsed.

### Polish

15. Number control: drop `[-]/[+]`. Keep `[ 0 ]` as a typed input;
    spec-allowed range can be shown in helper text.
16. Dim the page behind any active dialog (or render the dialog
    full-width like the main page).
17. Add `/` search inside the dialog to filter by field name.
18. Column headers inside list rows; drop the `→` glyph and the
    empty padding column.
19. Show "Modified" / "Inherited" / "Default" as small textual badges
    on the right edge of each row instead of relying on `●` /
    `(Inherited)` mixed signals.
20. Save button should be disabled (greyed) until the dialog has a
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
