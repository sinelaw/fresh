# Widget controls own their interaction

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

**Status:** IMPLEMENTED in steps (see the checklist at the end); each step
names what shipped and what the plugins stopped doing because of it.

## The problem

Plugin dialogs built from the shared widget kinds (`Text`, `Dropdown`,
`List`, `Tree`, `Button`, …) misbehaved in ways that looked unrelated — Enter
after closing a menu with Esc, Enter on a field with suggestions saving the
dialog, Tab stuck on a suggestion list, a dropdown changing value on ↑/↓ while
closed, a pasted prompt not scrolling to its end, paths cut at both ends — and
each plugin carried its own workaround. They share one cause: **the controls
did not own their interaction.** Keys reached the plugin before the focused
control, the plugin kept copies of state the host already owned, and sizes were
guessed by plugins from the screen width. The fix is made once, in the host's
shared controls, and the plugin workarounds are deleted as each rule lands.

## The rules

### R1 — the focused control handles a key first

A panel's keys used to be captured by its plugin mode (`defineMode`) on the
tree's capture leg, *ahead* of the widget holding focus. A plugin that bound
Enter, Esc, Tab or an arrow therefore had to guess what the focused control
would have done and forward the key back by hand (`panel.command(key(...))`);
the New Workspace form bound Tab, Enter, Esc, Home, End and all four arrows,
over its multi-line prompt too, and forwarded about twenty of them.

The order is now, for every described panel (dock, floating, pane-mounted):

1. **The focused control**, with every key exactly as typed: the kind's
   `on_text` for a character, `on_key` for anything else — Esc (an open pop-up
   closes before the dialog does) and chords (a read-only document copies on
   Ctrl+C) included. A key is never handed over with a modifier masked off:
   Ctrl+Enter is not Enter to a text area.
   `Consumed` ends the key. `Pass` and `PassAfter` (the control acted, e.g.
   closed a suggestion list it had not been stepped into, and the key should
   still act beneath) go on.
2. **The panel's mode** — the plugin's commands, chords included.
3. **The panel's defaults** — Tab walks the tree's ring, Esc cancels a modal /
   blurs the dock, the single-line field's Enter, an unbound chord's fate.

The exception is declared, not guessed: a binding whose third element is
`"shortcut"` (`["C-Enter", "submit", "shortcut"]`) is a **dialog-wide
shortcut**. The resolver keeps a per-mode set of them, and those — plus the next
key of a chord the mode has already started — are the only keys the panel's
interior still takes on the capture leg. Tab reaches the runtime like every
other key; the ring moves only after the control passes it, which is what lets
a stepped-into suggestion list accept on Tab.

The kinds were tightened so their answers mean something now that they are
asked first: a read-only or markdown `Text` passes Space, Backspace, Delete,
undo/redo and (plain read-only) Enter instead of swallowing them as no-ops; a
non-checkable `Tree`'s Space still activates, but its `activate` carries
`via: "space"` so a plugin whose Enter "opens" a row can leave Space inert.

Plugins migrated: the New Workspace form (only Ctrl+Enter / Alt+Enter as
shortcuts; Enter on a single-line field is the host's — it moves on; Esc and
the history arrows stay bound), the dock (Enter on the tree is the tree's `activate`; Space no longer
bound), Add Machine and New Folder (Enter is just "save" / "create" — buttons,
lists and a highlighted suggestion answer their own), Repositories, Machines,
code-tour, search/replace and the review filter (forwarders deleted). Where a
plugin's stepping is genuinely its own — pkg's ↑/↓ skip section headers, the
review comments panel steps a whole wrapped comment, the picker's Space bulk
select and `/` focus the filter from anywhere, the tour's Space/Backspace step
the tour — the binding is declared a shortcut, keeping its old precedence
explicitly rather than by accident.

### R2 — plugins hold no interaction state

Focus, open/closed, highlight, scroll and caret are the host's. Plugins had
been keeping copies — the form's focus ring mirror, the dock's
`pickerFocusKey` and `dockFocus`, the Add Machine, New Folder, Repositories
and Machines dialogs' focus keys, the form's open-dropdown mirror — each
maintained from `focus` events and patched by hand wherever the plugin moved
focus itself, and each able to drift.

- **Every `widget_event` carries `focus_key`**: the panel's focused widget
  after the event, read from the registry at the moment the event fires.
- **`editor.getPanelFocusKey(panelId)` / `panel.focusKey()`** read the same
  fact from the plugins' state snapshot. It is published ahead of every
  `widget_event` and every plugin action (`Editor::publish_panel_focus`), so a
  mode binding's handler reads the focus as of its key; a plugin's own
  `setFocusKey` writes through, so a read right after it agrees.
- **Focus returns to what opened a panel.** When a centred or anchored panel
  mounts over a focused dock, the host records the dock widget that had the
  keyboard (`Editor::floating_opener`); when the floating slot empties — Esc,
  a press outside, the plugin's own unmount — the dock takes the keyboard back
  on that widget (`Editor::floating_slot_closed`). The plugin's
  `closeMainMenu` / `closeCreateFolderDialog` / `restoreDockAfterDialog` /
  `restoreDockAfterForm` refocus code and the `yieldDock` / `restoreDock`
  discovery hooks are gone. A dialog opened while the editor had the keyboard
  closes back to the editor, not to a dock the user was not in.
- "`change` means a value was accepted" is a dropdown contract, and lands
  with R3.

### R3 — every control behaves the same everywhere

Each control's keyboard behaviour is defined once, in its kind file, with its
contract tests beside it (`kinds::dropdown::contract_tests`,
`kinds::text::key_contract_tests`).

**The shared pop-up list** (`kinds::popup_list`). A dropdown's option list and
a field's suggestion list were two implementations of one thing — one wrapped
its highlight, one clamped; one followed its window both ways at render time,
the other forward at render time and backward only on a key. There is now one
copy of each rule: which keys move a highlight (↑/↓, PgUp/PgDn a window with a
row of overlap, Home/End), the clamp, type-to-jump, the scrollbar, and the
window that keeps the highlight in view — computed from the highlight on every
layout, so nothing that moves it (a key, a list that shrank) can leave it off
screen. Both lists drop below their control and flip above it when the frame
has no room, and both close on a press outside them as well as on Esc.

**Dropdown.** Closed, ↑/↓ are not its keys — they pass, and move focus on;
←/→ step the value in place; Enter, Space and Alt+↓ open. Open, the keys move
a *highlight* (`WidgetInstanceState::Dropdown::highlight`) and fire nothing;
typing jumps to a match. Enter, Space, Alt+↑ or a click commit — one `change`,
and only if the value differs; Tab commits and moves on; Esc, a press outside
or focus leaving closes with no event. `change` means a value was accepted.
The Menu's project dropdown no longer needs `mainMenuProjectOpen` /
`mainMenuProjectPick` to hold back the filter until the list closed, and the
form's `Manage repositories…` no longer needs to be "armed".

**Text with suggestions (combo box).** ↓ enters the list; Enter or Tab on the
highlighted row accepts it — the host writes the value (caret at its end),
closes the list and fires `change` then `completion_accept`; with nothing
highlighted, Enter and Tab close the list and go on. Esc closes the list
first. Plugins stopped copying the accepted value back into the field
(`applyAcceptedCompletion`, the Add Machine Host field's accept) and stopped
mirroring whether a list is up (`machineHostSuggesting`).

**What stays two renderers.** The two lists still draw their own chrome: the
option list is a bordered box, and the suggestion list paints over its
section's bottom border so field and list read as one frame. Their model,
window, scrollbar, placement and dismissal are shared; the anchored context
menus and the Menu panel are panels, not lists, and get their placement from
the anchored layer and their focus return from R2.

### R3 — the text area keeps its caret in view

A multi-line `Text` is a `fresh_ui::List` of its wrapped rows whose selection
is the caret's row. `List` revealed a selection through a memo that fired only
when the selected row *changed*, once, on the build that carried it. Anything
that left the row index alone but changed the geometry — content re-wrapping
under an unmoved caret row, the box changing height a round trip later (the
prompt grew itself from the plugin's `promptTextRows`), focus arriving by
mouse with the row already "revealed" — left that one-shot answer stale, and
the plugin's dialog-level ↑/↓ bindings (gone with R1) meant the arrow keys that
would have re-triggered it never reached the box.

- `Anchor::follow` (fresh-ui) is a **standing** reveal: it is re-applied on
  every layout against the window's real height, and a wheel over the window
  clears it — the reader chose where to look.
- `List::follow_selection(token)` arms it whenever the selection or the
  owner's token changes. The text area passes the caret's byte and the
  document's length, so a key, typing or a paste re-arms it and a wheel wins
  until then.
- The text area is built in a `layout_reader` and wraps at the width layout
  actually gives it, not the width the description arithmetic handed down.
- `minRows` / `maxRows`: a box that grows with its text is as tall as its value
  wraps to at that width, between the two, and scrolls past `maxRows`
  (`kinds::text::text_area_height`). The New Workspace prompt uses it; the
  plugin's `promptRows` / `promptTextRows` width guess is gone.

## Checklist

- [x] R1 — controls get keys first; declared dialog-wide shortcuts.
- [x] R2 — the focus key in every `widget_event`, a getter, focus returns to
      the opener; plugin state copies deleted.
- [x] R3 — one shared pop-up; the dropdown and combo-box contracts.
- [x] R3 — the text area keeps its caret in view on every layout; `minRows` /
      `maxRows`.
- [ ] R4 — unconsumed arrows move focus by screen position.
- [ ] R5 — sizes from layout: fill-the-row `Text`, the table widget.
- [ ] R6 — shared composites (path picker, Machine picker) in `plugins/lib`.
