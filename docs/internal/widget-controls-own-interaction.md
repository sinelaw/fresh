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

1. **The focused control**, in its own vocabulary: the kind's `on_key` for a
   key, `on_text` for a character. A key the router would have to *mask* to put
   it in the vocabulary (Ctrl+Enter is not Enter to a text area) is not offered.
   Esc is offered too — an open pop-up closes before the dialog does.
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
shortcuts, Enter kept inert on single-line fields, Esc and the history
arrows), the dock (Enter on the tree is the tree's `activate`; Space no longer
bound), Add Machine and New Folder (Enter is just "save" / "create" — buttons,
lists and a highlighted suggestion answer their own), Repositories, Machines,
code-tour, search/replace and the review filter (forwarders deleted). Where a
plugin's stepping is genuinely its own — pkg's ↑/↓ skip section headers, the
review comments panel steps a whole wrapped comment, the picker's Space bulk
select and `/` focus the filter from anywhere, the tour's Space/Backspace step
the tour — the binding is declared a shortcut, keeping its old precedence
explicitly rather than by accident.

## Checklist

- [x] R1 — controls get keys first; declared dialog-wide shortcuts.
- [ ] R2 — the focus key in every `widget_event`, a getter, focus returns to
      the opener; plugin state copies deleted.
- [ ] R3 — one shared pop-up; the dropdown and combo-box contracts.
- [ ] R3 — the text area keeps its caret in view on every layout; `minRows` /
      `maxRows`.
- [ ] R4 — unconsumed arrows move focus by screen position.
- [ ] R5 — sizes from layout: fill-the-row `Text`, the table widget.
- [ ] R6 — shared composites (path picker, Machine picker) in `plugins/lib`.
