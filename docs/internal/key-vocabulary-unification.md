# One Key Vocabulary: Parser, Syntax, and Types

Purpose: collapse the several independent parsers, name tables and spellings
that turn a *string* into a key into a single parser with a single syntax, so a
name that binds and a key that arrives cannot drift apart. Status: PLANNED, and
being implemented in the sequence at the end of this document. Each numbered
step says what it changes; the ones marked "no behaviour change" widen what
parses and change nothing else.

This is the same class of defect as issue #1128 ("binding a key by name only
worked for the names someone had remembered to list") one layer out. That fix
unified the *config* names with the *decoder* names. Several vocabularies were
left outside it, and this document is about those.

---

## 1. The problem: five parsers, three tables, three spellings

A key reaches Fresh as a string from five different directions, and until this
work each direction parsed it differently.

| Where the string comes from | Name table it used | Modifier syntax | Chords |
|---|---|---|---|
| Config `key` + `modifiers` fields | the shared decoder table (`key_name_to_code`) | JSON array of names | via the `keys` array |
| Config `keys` array (chords) | same | JSON array | JSON array |
| Plugin `defineMode` binding tables | **its own** uppercase match | `C-` `M-` `S-` prefixes | space-separated |
| Widget text fields | **its own** eight-key match | `C-` `S-` `A-` prefixes | none |
| Widget kind dispatch | **unwritten** — literal match arms | literal strings | none |

Three consequences, all of which have shipped as bugs:

- **The same key has three names.** Escape is `esc`/`escape` in config,
  `ESC`/`ESCAPE` to a plugin mode, and `Escape` to a widget. Shift+Tab is
  `S-Tab`, `BackTab`, and `Shift+Tab`. Which one works depends on which parser
  happens to see it, and nothing says so.
- **The formatting and the matching can disagree with no compile error.** The
  widget router *formats* a name; each widget kind *re-matches* that text. A
  typo on either side is a silently dead key.
- **The vocabulary is written down nowhere**, so nobody can enumerate it, and a
  name that no producer emits (or that no consumer matches) is invisible.

Two live examples found while surveying, both of the "registers fine, never
fires" shape:

- A panel's plugin sends the widget key `S-Tab`, which no widget code has ever
  matched — the kinds match `Shift+Tab`. Shift+Tab in that panel does nothing.
- The review mode binds the chord `z a` (collapse all files). Its FILES and
  COMMENTS sidebars are widget panels, and a panel's keymap resolves *single
  keys only* — it has no chord state — so the `z` prefix is never accumulated
  and the chord is dead whenever a sidebar holds the keyboard. It works with
  the diff focused, through the window's chord state.

Note the second is not a *parsing* bug: unifying the parsers does not fix it.
It needs chord resolution added to the panel keymap, which is why it is a
separate step below.

---

## 2. The target: one parser, one syntax, several printers

**One type.** `Key` is one press (a code and its modifiers); `KeySeq` is a
non-empty sequence of them — an emacs-style chord. These live in the data-layer
crate, because the widget kinds are the most constrained consumer and the crate
dependency edge runs one way.

The code is the decoder's own key-code type rather than a small enum closed over
the widget vocabulary. The config path has to express function keys, keypad,
media and punctuation keys, which such an enum could not hold; the widget
vocabulary becomes a *validated subset* of the one type rather than a parallel
type of its own.

**One name table.** Each entry carries a canonical name and its aliases:

```
{ canonical: "escape", aliases: ["esc"], code: Esc }
```

Every spelling that any of the five parsers accepted today enters the table as
an alias. The table is private to the parser module; only the parse and print
entry points are public, so a sixth parser cannot be written without visibly
duplicating it.

**Where the keypad and media names stay.** Those two lookups live in the
terminal input-parser crate, which the data-layer crate does not (and should
not) depend on — the data layer has no business knowing about escape sequences.
So name resolution splits rather than the table moving wholesale: the shared
table is consulted first, and the editor layer appends the keypad and media
lookups behind it. Both are still reached only through the one parser.

**One syntax** for the compact string form, which is what `defineMode` and the
widget wire already use and what the emacs keymap's own comments describe:

- modifier prefixes `C-` (control), `S-` (shift), `M-` / `A-` (alt), applied in
  any order;
- a key name from the shared table, or a single character;
- presses separated by a space for a chord: `C-x C-s`.

The space is unambiguous because the space *key* is spelled `Space`, never a
literal space character.

**Several printers, deliberately.** Three output forms survive because they
serve different audiences: the human-facing display (`Ctrl+X`, or the macOS
symbol form), the canonical wire and config spelling, and the config
write-back's names. That is correct — but it is also where drift would return,
so a round-trip test binds them: every printer's output must parse back through
the one parser to the value it came from.

---

## 3. Backwards compatibility: parsing widens, printing narrows

The single rule that makes this safe: **the one parser accepts the union of
every spelling all five parsers accepted, and each key has exactly one canonical
spelling for output.** Nothing that parses today stops parsing. The only thing
that changes is what the editor *writes*, and that is a small controlled set of
places.

Surface by surface:

- **Existing config files.** Untouched. `key` + `modifiers` and the `keys` array
  keep working exactly as before.
- **The compact form in config** arrives as a *new optional field*, not by
  overloading `key` — overloading would be ambiguous against a literal `-` key.
  It desugars at parse time to the same sequence, so nothing downstream branches
  on which spelling was used.
- **Built-in keymaps** (default, emacs, macos, …) are deliberately *not*
  migrated, so the diff stays reviewable. The guarantee is a test that parses
  every built-in keymap and asserts the resulting binding maps are identical
  before and after.
- **Plugin `defineMode` strings** are a public API and their parser is deleted,
  so the replacement must accept a strict superset — the `RET`/`SPC`/`PGUP`/`BS`
  family, function keys, the `M-` prefix, and the Shift+Tab-to-BackTab
  normalisation. Every one is pinned by a test table taken from bindings that
  actually ship, *before* the old parser is removed.
- **The widget wire** is a public API: `Shift+Tab` keeps parsing, as an alias,
  and keeps being *emitted*, so plugins that read it back see no change.
- **The keybinding editor's write-back** is the one place the editor writes key
  names into a user's config. It moves to the canonical names, guarded by the
  existing round-trip test extended to cover every alias.
- **The generated config schema** gains an optional property, which is a
  non-breaking schema change.
- **The generated keyboard reference** is already produced from these tables by
  a test that fails when they drift; it moves with them and starts documenting
  aliases as well as canonical names.

---

## 4. What stays outside the chokepoint

Stated explicitly, because "one parser for key strings" is true and "one
representation of a key everywhere" is not:

- **The terminal decoder.** It turns byte sequences into key events, not strings
  into keys, so it is outside by nature. Its keysym lookups are reached only
  through the shared parser, as described above.
- **The calibration table's on-disk form.** It serialises a key code as a tagged
  enum, which is a further on-disk spelling. It parses no binding strings, so
  this work does not touch it. Foldable later.
- **Hint-bar labels.** A plugin writes these as free-form display text and the
  host only renders them; the shipped values include several spellings and some
  are glyphs. Making them consistent is a different change — plugins would pass
  a key sequence and let the host format it — and is not attempted here.

---

## 5. Sequence

Each step compiles and is committed on its own. Steps 1–4 change no behaviour
beyond widening what parses.

1. **The shared type, table and parser** in the data-layer crate. Nothing uses
   it yet. Tests: canonical round-trip, and every alias resolving to its
   canonical value.
2. **Point the keybinding resolver at it** — the config key, modifier and chord
   parsers, with the keypad and media lookups appended by the editor layer.
   Test: every built-in keymap produces identical bindings before and after.
3. **Delete the plugin mode parser** and route `defineMode` through the shared
   one, with the superset test table written first.
4. **The widget kinds take the type.** The router builds a value instead of
   formatting a string; the per-kind literal match arms and the widget text
   field's private parser are deleted. This is also what fixes the dead `S-Tab`
   described above — it becomes an alias rather than an unmatched string.
5. **The compact form in config**, plus regenerating the schema and the keyboard
   reference.
6. **Chord resolution in the panel keymap**, which fixes the dead `z a` / `z r`
   described above. This one *is* a behaviour change and carries its own
   end-to-end coverage.
