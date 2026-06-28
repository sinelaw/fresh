# Configuration, Settings UI & Themes

> _AI-generated: describes Fresh's architecture and design rationale, not implementation details; where it disagrees with the source, the source is authoritative._

Purpose: explain how Fresh resolves layered configuration, generates and consumes
its JSON Schema, renders the schema-driven Settings UI, preserves user comments
through JSONC read/write, and manages themes, the keybinding editor, and the
`init.ts` programmable-config surface — distinguishing what is implemented from
what is still planned.

---

## 1. The Layered Configuration Overlay

### 1.1 Layers and resolution order

The overlay applies **five rungs**. Earlier design notes describe a "4-layer"
model, but the resolver inserts a platform-specific User variant between Project
and User, so the real count is five.

The writable layer enum exposes four targets — System, User, Project, Session —
with precedence rising in that order. There is **no** writable enum variant for
the platform rung; it is a read-only variant of the User layer rather than a save
target.

A configuration resolver carries directory context and a working directory, and
drives resolution. Effective precedence, highest wins:

**Session > Project > User-Platform > User > System(defaults)**

Resolution starts from Session and merges Project, then the User-platform file
(for example a Linux-specific user config), then User, then fills remaining gaps
from the hardcoded default config (the System layer).

Layer file locations:

| Layer | Location |
|---|---|
| User | the user config file in the OS config directory |
| User-platform | a platform-suffixed user config file (`config_linux.json` / `config_macos.json` / `config_windows.json`) |
| Project | a `.fresh/config.json` under the working directory (legacy fallback to a bare `config.json`); writes always target the `.fresh/` form |
| Session | a volatile `.fresh/session.json` under the working directory |
| System | none; the hardcoded default config |

The config directory is OS-specific: macOS forces `~/.config/fresh`;
Linux/Windows use the platform config directory under a `fresh` subfolder. A
separate legacy search-path API also exists.

> **Discrepancy.** The doc comments on the layer enum and on the older
> layered-load entry point describe a clean 4-layer "Session > Project > User >
> System" and omit the UserPlatform rung that resolution actually inserts. The
> layer-write path handles only User/Project/Session and errors on System —
> platform is read-only.

### 1.2 Merge strategy

Two distinct mechanisms.

**(A) Layer merge — the `Merge` trait.** For an optional value, if the receiver
is unset, it takes the other layer's value. Because the higher-precedence layer
is merged first, a value set there wins → **scalars: highest layer wins.**

- Scalars: per-field merge.
- Maps, shallow: per-key union, higher wins on collision, value not deep-merged
  (used for keybinding maps, LSP, universal LSP).
- Maps, recursive: on key collision the existing entry is merged field-by-field,
  **deep-merging** (used for `languages` and `plugins`). This is what makes
  per-language overrides such as `languages.python.tab_size` compose correctly,
  rather than replacing the whole language entry.
- Lists: **replaced wholesale.** `keybindings` and environment detectors merge as
  whole optionals, so the higher layer's list replaces rather than
  element-merges.
- Plugin free-form `settings` (untyped JSON): objects deep-merge key-by-key,
  non-objects and null replace.

**(B) Resolve-with-defaults.** HashMaps start from a clone of the defaults' map
and overlay user entries, so defaults for un-overridden keys survive; LSP
single-server vs multi-server forms get special handling; resolution ends by
normalizing zero sentinels, treating `0` as "unset" for fields such as the wrap
column, page width, and tab size.

This matches the intended overlay contract: scalars highest-wins, maps
deep-merge, lists replace.

### 1.3 Delta serialization and pruning

Layers store only their difference from the resolved parent, so setting a value
back to an inherited or default value **prunes the key**.

Three save paths exist on the resolver:

- A base save path resolves the parent up to the target layer, diffs the current
  config against it, and merges the delta over the existing file.
- A baseline-aware save path (preferred) computes the exact touched JSON pointers
  between a baseline and the current config; for each, **if the current value
  equals the parent it removes the pointer (prune)**, otherwise it sets the
  pointer. This is the "equal-to-inherited ⇒ key removed" behavior.
- A changes-based save path (the Settings-UI path) takes explicit pointer→value
  changes plus deletions, applies them onto the read-existing file value,
  round-trip-validates through the partial-config shape, and writes.

The diff core performs a recursive object diff emitting only differing leaves.
Empty objects and nulls are stripped before writing. Lifting a full config to its
partial form prunes default plugins and never serializes a plugin's auto-discovered
`path`.

> **Known limitation (implemented, ignored test).** The base save path alone
> cannot prune a value reset to the parent value (covered by an `#[ignore]`d
> test); the baseline and changes paths solve it. Other tests document that the
> base save path is not a true read-modify-write under external concurrent edits;
> the pointer-based changes path is what preserves external edits and comments.

### 1.4 The partial-config substrate

A parallel set of `Option`-wrapped mirrors of every config struct exists, where
`None` means "inherit". This is the substrate for delta serialization:
serializing a partial naturally omits `None` keys, and the diff/prune logic
operates on these shapes. Lifting from a full config produces all-`Some`;
resolving collapses back. The session config is a focused volatile subset (theme,
editor, per-buffer overrides).

### 1.5 The Config struct

The top-level config struct has all fields default-able. Top level: version,
theme, locale, update-check. Nested sections cover the editor, file explorer,
file browser, clipboard, terminal, warnings, packages, and environment.
Keybindings are held as a list plus a map of named keymaps and an active-keymap
selector (default/emacs/vscode/macos/macos-gui keymaps are embedded into the
binary). Languages are a map plus a default-language selector. LSP has an enable
flag plus single- and universal-server maps. Plugins are an auto-discovered map.

The plugin config type lives in **fresh-core** so the plugin runtime can read it
from the JS bindings; the editor crate re-exports it. Each plugin's free-form
`settings` is stored as untyped JSON so a malformed plugin schema cannot poison
the rest of the config.

Migration: a current-config-version constant gates a migration chain that runs
before deserialization — v0→v1 converts camelCase to snake_case, v1→v2 prepends a
remote element to the status bar's left section.

---

## 2. JSONC Support & Comment Preservation

A sequence of changes, landed in order, fixed a class of silent-data-loss bug.
The chronology is the design rationale.

1. **Accept JSONC on read.** Config files were parsed with a strict JSON parser,
   so a single `//` comment made a layer invalid; in the layered flow the error
   was swallowed and the editor silently fell back to defaults. All reads now
   route through a JSONC parser that tolerates comments and trailing commas;
   empty or comment-only input yields an empty object. This covers the user,
   project, and session layers, the read-modify-write save paths, the raw
   user-config read, and the explicit `--config` load. Genuinely malformed input
   still errors.

2. **Preserve comments on rewrite.** Writes still serialized from a value, which
   reconstructs the file and drops all comments. Writes now go through the JSONC
   parser's concrete syntax tree: existing text is parsed, then edited minimally —
   unchanged properties are left byte-for-byte (writes are **skipped when the
   value is already equal**, to keep inline comments), nested objects recurse,
   removed keys are deleted, and new keys appended. New, unparseable, or
   non-object-root files fall back to pretty-printing. All save paths funnel
   through the same clean-write routine, so this covers every layer write.

3. **Never overwrite an unparseable config.** A save is a read-modify-write; the
   read side previously swallowed parse failures and returned an empty object, so
   one comment plus any save destroyed the whole file. The existing-file read now
   **errors** with a parse error when a non-empty file cannot parse, and the base
   save path repeats the guard inline. Regression tests assert the file is left
   byte-for-byte intact.

**Surfacing save errors.** When the no-clobber guard aborts a save, a plain
status-bar line was easy to miss. The error now propagates out and the settings
save flow raises a save-error popup — a centered, red-bordered modal showing the
parse error, the file path and line/column, and a note that the file was left
unchanged. Acknowledging the modal **opens the offending config file** so the
user can fix the syntax.

The config-error type distinguishes IO, parse, serialize, and validation errors.

---

## 3. Schema Generation (schemars) and How It Drives the UI

### 3.1 Generation

The config schema is generated offline, not at build time: a generator binary
runs schemars over the config struct, drops the large `menu` default, and the
result is committed alongside the editor crate's bundled plugins. The same binary
emits the theme schema and the package schema. The build script notes that the
schema is no longer build-generated.

The schema is **compiled into the binary** via an include at the single load
point — opening the settings UI — and reused in settings tests.

Custom `x-` extensions that schemars emits or the struct attributes inject
(present in the committed JSON) include: `x-display-field`,
`x-standalone-category`, `x-no-add`, `x-section`, `x-order`, `x-enum-from`,
`x-dual-list-options`, `x-dual-list-sibling`, and a status-bar
dynamically-extendable marker. For example, the plugin config carries a
display-field pointing at its `enabled` flag and marks `path` and `settings`
read-only.

### 3.2 Schema → UI pipeline

The schema is parsed into a list of setting categories, which are built into
settings pages of setting items (each carrying a setting control), rendered and
hit-tested by the settings layout. The schema-parse module is the only
WASM-compatible settings module; everything else is gated behind the runtime
feature.

Schema parsing walks top-level properties. A property becomes a **standalone
category** when the standalone-category extension is set (maps such as
`languages`), a **category with nested settings** when the resolved schema has
properties, else a **top-level setting** folded into a synthesized "General"
category. Plugin schemas are appended as `Plugin: <name>` categories rooted at
each plugin's settings subtree.

Control-type selection maps the schema to a setting type:

| Schema | SettingType | Control |
|---|---|---|
| `boolean` | `Boolean` | Toggle |
| `integer` (+min/max) | `Integer` | Number |
| `number` | `Number` | Number (floats stored as scaled integers) |
| `string` | `String` | Text (Dropdown if enum-from set) |
| inline / enum-values enum | `Enum` | Dropdown |
| `array<string\|int>` | `StringArray`/`IntegerArray` | TextList |
| `array` + dual-list-options | `DualList` | DualList |
| `array<$ref object>` | `ObjectArray` | ObjectArray (keybindings) |
| `object` + `additionalProperties` | `Map` | Map (entry dialogs) |
| `object` + fixed `properties` | `Object` | flattened controls or JSON |
| fallback | `Complex` | uneditable / JSON editor |

It handles the schemars `Option<T>` shape (an `anyOf` of a ref and a null type)
and turns a `null` enum value into an "Auto-detect" option with an empty-string
value.

> **Discrepancy.** The category and page types carry subcategory/subpage fields,
> but schema parsing never populates subcategories — the left panel is a flat
> categories-plus-sections tree, not true nested subpages. An inner `Object` is
> flattened into native child controls only when every child is editable;
> otherwise the whole object collapses to one JSON editor. So "nested object →
> subcategory" holds only for top-level properties.

---

## 4. Settings UI Architecture

### 4.1 State and modules

The central state object holds pending changes (pointer→value), pending
deletions, per-value layer sources, a target layer (default User), a nested
entry-dialog stack, and the original config. Module responsibilities:

- Schema parse — pure parse of schema into categories.
- Items — schema into renderable setting items plus live controls.
- State — all UI state, pending changes, dialogs, layers, input mutators.
- Render / layout / mouse / input — draw, hit-test, mouse dispatch, and key
  routing (Dialog → Panel → Control).
- Search — fuzzy search.
- Entry dialog — a modal for Map and ObjectArray entries.

### 4.2 Control widgets

The control enum covers: Toggle, Number (integer or float-as-percent), Dropdown
(enum/select), Text, TextList (string/int arrays), DualList (an ordered subset
with sibling cross-exclusion), Map (key→value dict), an ObjectArray for
keybinding lists, a multiline JSON editor for Object/Complex values, and an
uneditable Complex control. There is **no dedicated color or file-path control** —
colors fall through to Text/JSON.

### 4.3 Modified indicator

"Modified" means **the value is defined in the target layer being edited**, not
"differs from default" (the newer semantics): a value is modified when its layer
source equals the target layer. Layer sources come from the resolver and default
to System when a value is purely a schema default.

Rendering uses a three-column indicator gutter: a focus marker, a modified dot,
and a spacer. Category labels get a dot prefix when the page has pending changes;
the title bar shows a modified marker with the layer name; the per-value layer
source (user/project/session, none for System) is shown in the description band.

The layer-definition signal and the unsaved-this-session signal are deliberately
separate.

> The dialog context uses the *old* definition (modified = differs from schema
> default) because dialogs have no layer context.

### 4.4 Entry dialogs

The entry dialog is **schema-driven, not per-type hardcoded** — there is no
special "add language" versus "add LSP" dialog. The language, LSP, and
universal-LSP dialogs are the same generic dialog opened over the respective
maps. Building from schema injects a synthetic read-only key field plus one item
per object property; an array-item variant handles ObjectArray items by index.
Nested Map/ObjectArray values open by pushing onto the entry-dialog stack.

Per-field buttons `[Reset]`/`[Inherit]`/`[Clear]`: Reset restores the built-in
default; Inherit/Clear set null. Built-in per-entry defaults come from the bundled
config. Inherited (null) nullable fields are omitted on save so they keep
inheriting.

### 4.5 Edit → save flow

Control mutators report a value change, which compares against the effective
original and records a pending change — toggling a value back to its original
**clears** the pending entry. The target layer cycles User→Project→Session;
System is read-only; switching layers clears pending changes and rebuilds pages.

Saving builds the merged config (deletions first, then changes), applies runtime
side-effects (theme, locale, plugins, keybindings, LSP, bars, file explorer),
persists the **delta** via the changes-based save path, re-resolves from disk,
and clears the settings state so a reopen is fresh. On error the change is not
applied and the save-error modal (§2) is raised.

Reset/revert: discarding restores; resetting the current value to default removes
it from the target layer (via pending deletions) so it falls back to inherited —
it does **not** write the schema default; separate operations set a nullable item
to null or clear a nullable category.

> **Separate prompt system.** A command-palette prompt set (theme, language,
> encoding, keybinding-map, cursor style, locale, line-ending, ruler) is
> independent of the modal; it calls the save paths directly against the User
> layer. Its module doc flags the repetitive start/apply/save triples as awaiting
> refactor.

### 4.6 Search

Settings search fuzzy-matches name, description, and path (best of three, with a
custom scorer) and recurses into composite controls and Map/TextList values.
Jumping to a result navigates to the page and item, dives into the deep-match
target, auto-expands the category, and cancels search.

> **Discrepancy.** Footer button indices enumerate four (Layer/Reset/Save/Cancel)
> but comments reference a 5th "Edit" button — worth verifying against the live
> footer renderer.

---

## 5. Theme System

### 5.1 Types

Two parallel representations exist:

- **The theme file form** — the serializable file form, nested into optional
  default-able sections: editor colors, UI colors, search colors, diagnostic
  colors, and syntax colors. It supports an optional `extends`.
- **The runtime theme** — the flat runtime form, around 140 fields, every color a
  ratatui color, plus two modifier fields for SGR attributes.

Conversion runs both directions between the file and runtime forms. Colors are an
untagged enum: an RGB triple serialized as `[r,g,b]`, or a named string
(`"Default"`/`"Reset"` map to terminal transparency). The theme name is a config
newtype, not part of the theme module, defaulting to a high-contrast theme.

### 5.2 Loading and built-ins (consolidation — IMPLEMENTED)

> Earlier design notes list theme consolidation as "not yet shipped". **It is
> shipped, and went further than the plan.** There are no Rust `Theme::dark()` /
> `Theme::light()` constructors; all built-ins are JSON.

Built-in themes are embedded JSON. A build script scans the themes directory and
generates a built-in-themes table that includes each file's JSON, pulled in at
runtime. Eight files ship: dark, light, high-contrast, nostalgia, dracula, nord,
solarized-dark, and terminal.

The theme loader deserializes each built-in JSON to the file form and then to the
runtime form, then scans user themes (recursively under the config directory's
`themes/`) and theme packages, producing a registry. Override-by-name applies:
built-ins are keyed by bare name, user themes by a `file://` path; resolution
allows a `builtin://NAME` form to force the built-in, while a relative name
resolves to a user theme overriding the matching built-in. Inheritance via
`extends` or auto-base selection (luminance-based light/dark auto-pick) is applied
on top of overrides. The planned plugin API for listing built-in themes exists.

### 5.3 Live preview (IMPLEMENTED) and the theme editor

Two preview mechanisms:

1. **"Select Theme" palette live preview** (Rust). Starting the prompt stores the
   original theme; navigating suggestions swaps the in-memory theme **without
   persisting**, runs a crossfade, and updates the cursor color. Cancelling
   restores the original.
2. **The `theme_editor.ts` plugin.** A command opens the editor, exposing a set
   of scoped commands under a theme-editor context. It loads the registry, edits
   fields, accepts hex / `[r,g,b]` / named colors, renders an in-buffer preview
   panel, and saves or deletes themes.

Known usability gaps: no full live-apply while editing colors (only the preview
panel), no discard confirmation, no way to directly edit an existing theme, and
inconsistent navigation. Note that some theme docs reference an obsolete
single-file theme module; the code is the split theme module.

---

## 6. Indentation Rules & `init.ts` Config

### 6.1 User-configurable indentation rules (IMPLEMENTED)

A third indentation tier provides per-language regex rules in the style of VS
Code indentation rules, run via the standard regex crate (RE2-style, no
lookarounds) with **scope-masking** (comment and string bytes blanked before
matching) as the anti-glitch mechanism. The authoritative primitive lives in the
indent-rules module. The config surface is an optional indent-rules config on the
language config, with patterns for increase-indent, decrease-indent,
indent-next-line, dedent-next-line, and self-close. A user-rules registry layers
overrides over built-in families; unset patterns inherit from the family; config
load and reload re-register the overrides. The rollout also reduced the bundled
tree-sitter grammars substantially, with a parity CI guard.

### 6.2 `init.ts` — programmable user config (IMPLEMENTED)

The init-script loader loads `~/.config/fresh/init.ts` (if present) through the
plugin pipeline as a plugin named `init.ts` — the same code path as "Load Plugin
from Buffer", so reload and unload are free. Design intent: for decisions that
depend on the runtime environment (host, `$TERM`, SSH, project path) and for
imperative plugin configuration — **not** for static preferences (Settings UI),
keybindings (editor), permanent themes (selector), or reusable features (plugin
packages). The starter template is fully commented and spells out this
non-overlap.

- **Crash fuse**: a crashes log counts consecutive failures within a rolling time
  window; after the threshold is reached the next launch auto-skips init.ts, and
  a successful run resets the counter.
- **Disable flags**: `--no-init` / `--safe`.
- **Type scaffolding**: type-declaration scaffolding always overwrites the bundled
  type-definitions file from the embedded copy (it must track the binary) and
  writes a tsconfig only on first run; per-plugin declarations are aggregated so a
  plugin-API lookup is typed.
- **Check mode**: a check command parses init.ts and reports syntax errors;
  deeper type-checking and scope-discipline lints are deliberately not
  implemented.

---

## 7. Keybinding Editor

### 7.1 Architecture (view/app split)

State and logic live in the app-side keybinding-editor module (the editor struct,
its data model, and conversion helpers); rendering **and input** live on the
view side; persistence glue is a separate actions module. The modal shows a
collapsible, plugin-grouped table: **Key | Action | Description | Context |
Source**.

Binding resolution layers sources by precedence into a `(key_display, context)`
dedup map: the active keymap, then user keybindings (which override the keymap),
then plugin defaults, then `Unbound` placeholders for every action so all stay
rebindable.

### 7.2 Editing, conflicts, persistence

Enter opens the edit dialog. Key capture is a deliberate two-step "special
capture": Enter arms capture mode, then the next raw keypress is normalized
(uppercase mapped to lowercase plus a SHIFT modifier) and stored. The action field
has substring autocomplete. Conflict detection is **advisory only — it does not
block saving**.

Applying the dialog validates the action name and pushes a binding to a pending-adds
list. The final save removes pending removals and adds pending additions to the
keybindings list, reloads the resolver, and writes the keybindings JSON pointer to
the **User** layer. Deleting a keymap or plugin binding shadows it with a custom
`noop` override.

> **Discrepancies.** (1) Editing an existing binding pushes a fresh pending-add
> entry without removing the superseded one — repeated edits accumulate redundant
> entries. (2) Chords are displayed and resolved but **cannot be authored** in the
> editor — applying the dialog always writes an empty keys list and a false chord
> flag. (3) Conflicts are non-blocking.

### 7.3 Keymaps

The four built-in maps plus user maps live in the keymap map; the active one is
the active-keymap selector (resolution with inheritance). The editor reads only
the active map and shows the map names read-only — it cannot switch the active
map. Switching is a `switch_keybinding_map:<name>` action, expanded per map, that
persists the choice to the User layer.

---

## 8. Toggle Actions

A set of editor methods handle boolean/config toggles. Two patterns: **global
preference toggles** mutate runtime config and persist to User via a
single-pointer save to the User layer, versus **per-split/per-buffer view-state
toggles** stored only in per-file workspace state.

| Toggle | Setting | Persists |
|---|---|---|
| line numbers | `editor.line_numbers` | yes |
| line numbers, current buffer | per-buffer override | workspace only |
| line wrap, current buffer | per-buffer wrap | workspace only |
| menu bar | `editor.show_menu_bar` | yes |
| file-explorer side | `file_explorer.side` | yes |
| vertical scrollbar | `editor.show_vertical_scrollbar` | yes |
| horizontal scrollbar | `editor.show_horizontal_scrollbar` | yes |
| mouse capture | window mouse-enabled | runtime only |
| mouse hover | `editor.mouse_hover_enabled` | **runtime only — bug** |
| inlay hints | `editor.enable_inlay_hints` | **runtime only — bug** |

Related config operations live here too: dumping the whole config and opening it,
reloading config (re-resolving layers and re-applying theme/keybindings/bars/LSP),
and reloading themes.

> **Discrepancy.** The mouse-hover and inlay-hints toggles mutate runtime config
> but never persist it, unlike the sibling `editor.*` toggles — so they reset on
> restart. The tab-bar and status-bar toggles are not here; they live on the
> window type.

---

## 9. Cross-Cutting Principles

- **Single source of truth via deltas.** Each layer stores only its diff from the
  resolved parent; pruning prevents config drift; partial mirrors make this
  natural.
- **Schema-driven UI.** One schemars-generated JSON Schema drives both validation
  and the entire Settings UI; `x-` extensions carry UI hints.
- **Never lose user data.** JSONC reads, comment-preserving CST writes, and the
  no-clobber-on-unparseable guard form a layered defense; failures surface as a
  modal that opens the file.
- **Right tool per surface.** Settings UI for static prefs, keybinding editor for
  keys, theme selector for themes, `init.ts` only for env-dependent or imperative
  config — enforced by the starter template and the `init check` tool.

---

## Discrepancies & Open Items (summary)

- The 4-layer docs omit the UserPlatform rung that resolution applies (five rungs
  in total).
- The base save path can't prune a value reset to the parent and isn't
  concurrency-safe (covered by an ignored test and concurrent-edit tests); use the
  baseline or changes paths.
- The category subcategory and page subpage fields are defined but never
  populated.
- Settings footer button count: code has four, comments reference a 5th "Edit".
- Float settings are stored as scaled integers throughout the UI.
- Keybinding editor: accumulating pending-adds on re-edit; no chord authoring;
  non-blocking conflicts.
- The mouse-hover and inlay-hints toggles don't persist.
- Theme consolidation is shipped despite earlier notes marking it planned; several
  theme docs reference an obsolete single-file theme module.
