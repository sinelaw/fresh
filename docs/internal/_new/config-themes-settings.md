# Configuration, Settings UI & Themes

Purpose: explain how Fresh resolves layered configuration, generates and consumes
its JSON Schema, renders the schema-driven Settings UI, preserves user comments
through JSONC read/write, and manages themes, the keybinding editor, and the
`init.ts` programmable-config surface — at code level, distinguishing what is
implemented from what is still planned.

Scope crates/files:
`crates/fresh-core/src/config.rs` (shared `PluginConfig`),
`crates/fresh-editor/src/config.rs`, `config_io.rs`, `partial_config.rs`,
`plugin_schemas.rs`, `init_script.rs`, `bin/generate_schema.rs`,
`view/settings/`, `view/theme/`, `view/keybinding_editor.rs`,
`app/settings_actions.rs`, `settings_prompts.rs`, `toggle_actions.rs`,
`app/keybinding_editor/`, and the generated `plugins/config-schema.json`.

---

## 1. The Layered Configuration Overlay (design-decisions #12)

### 1.1 Layers and resolution order

The model is documented as "4-layer" but the resolver actually applies **five
rungs**: a platform-specific User variant sits between Project and User.

- `ConfigLayer` enum — `System | User | Project | Session`
  (`config_io.rs:393`), precedence `System=0 … Session=3`
  (`config_io.rs:406`). There is **no** `ConfigLayer` variant for the platform
  rung; it is a variant of the User layer, not a writable target.
- `ConfigResolver { dir_context, working_dir }` (`config_io.rs:420`) drives
  resolution in `ConfigResolver::resolve` (`config_io.rs:440`). Effective
  precedence, highest wins:

  **Session > Project > User-Platform > User > System(defaults)**

  Build order (`config_io.rs:442-463`): start from Session, `merge_from`
  Project, then User-platform (e.g. `config_linux.json`), then User, then
  `merged.resolve()` fills remaining gaps from `Config::default()` (System).

Layer file paths:

| Layer | Path | Resolver fn |
|---|---|---|
| User | `<config_dir>/config.json` | `user_config_path` `config_io.rs:467`; `config.rs:1130` |
| User-platform | `<config_dir>/config_{linux,macos,windows}.json` | `user_platform_config_path` `config_io.rs:498` |
| Project | `<wd>/.fresh/config.json` (legacy fallback `<wd>/config.json`) | `project_config_path` `config_io.rs:473`; write always `.fresh/config.json` `:488` |
| Session | `<wd>/.fresh/session.json` (volatile) | `session_config_path` `config_io.rs:493` |
| System | none (hardcoded `Config::default()`) | — |

`config_dir` is OS-specific (`DirectoryContext`, `config.rs:990`): macOS forces
`~/.config/fresh` (`default_config_dir`, `config.rs:1155`); Linux/Windows use
`dirs::config_dir()/fresh`. A separate legacy search-path API also exists
(`system_config_paths` `config.rs:838`, `find_config_path` `config.rs:883`).

> **Discrepancy.** The doc comment on `ConfigLayer` (`config_io.rs:418`) and
> `Config::load_with_layers` (`config.rs:889`) describe a clean 4-layer
> "Session > Project > User > System" and omit the UserPlatform rung that
> `resolve()` actually inserts. `layer_write_path` (`config_io.rs:564`) handles
> only User/Project/Session and errors on System — platform is read-only.

### 1.2 Merge strategy

Two distinct mechanisms.

**(A) Layer merge — the `Merge` trait** (`partial_config.rs:17`).
`impl Merge for Option<T>` (`partial_config.rs:23`): if `self.is_none()`, take
the other layer's value. Since the higher-precedence layer is merged first, a
set value there wins → **scalars: highest layer wins.**

- Scalars: per-field `merge_from` (e.g. `PartialEditorConfig::merge_from`,
  `partial_config.rs:232`).
- Maps, shallow: `merge_hashmap` (`partial_config.rs:33`) — per-key union,
  higher wins on collision, value not deep-merged (used for `keybinding_maps`,
  `lsp`, `universal_lsp`).
- Maps, recursive: `merge_hashmap_recursive` (`partial_config.rs:51`) — on key
  collision calls `existing.merge_from(value)` to **deep-merge field-by-field**
  (used for `languages` and `plugins`). This is what makes per-language
  overrides like `languages.python.tab_size` compose correctly (test
  `merge_languages_same_key_higher_wins`, `partial_config.rs:1534`).
- Lists: **replaced wholesale.** `keybindings` and env detectors use
  `Option::merge_from` (`partial_config.rs:116-120`), so the higher layer's
  list replaces rather than element-merges.
- Plugin free-form `settings` (untyped JSON): `merge_json_values`
  (`partial_config.rs:498`) — objects deep-merge key-by-key, non-objects/null
  replace.

**(B) Resolve-with-defaults** — `PartialConfig::resolve_with_defaults`
(`partial_config.rs:1197`). HashMaps start from `defaults.<map>.clone()` and
overlay user entries so defaults for un-overridden keys survive; LSP single
vs multi-server has special handling (`:1213`); ends with
`normalize_zero_sentinels` (`config.rs:3757`) treating `0` as "unset" for
`wrap_column`/`page_width`/`tab_size` (commit `26fd01421`).

This matches design-decisions #12: scalars highest-wins, maps deep-merge, lists
replace.

### 1.3 Delta serialization and pruning

Layers store only their difference from the resolved parent, so setting a value
back to an inherited/default value **prunes the key**.

Three save paths on `ConfigResolver`:

- `save_to_layer` (`config_io.rs:576`): `parent = resolve_up_to_layer(layer)`,
  `delta = diff_partial_config(current, parent)`, merge over existing file.
- `save_to_layer_with_baseline` (`config_io.rs:618`): preferred. Uses
  `find_changed_paths(baseline, current)` (`:136`) to get exact touched JSON
  pointers; for each, **if `current == parent` → `remove_json_pointer` (prune)**,
  else `set_json_pointer`. This is the "equal-to-inherited ⇒ key removed"
  behavior (test `save_to_layer_changing_to_default_value_should_persist`,
  `config_io.rs:2841`).
- `save_changes_to_layer` (`config_io.rs:658`): the Settings-UI path; takes
  explicit `changes` (pointer→value) and `deletions`, applies onto the
  read-existing file value, round-trip-validates through `PartialConfig`, writes.

Diff core: `diff_partial_config` (`config_io.rs:820`) → `json_diff`
(`:929`), recursive object diff emitting only differing leaves. Empties are
stripped by `strip_nulls`/`strip_empty_defaults` (`:19`, `:43`) inside
`write_clean_value_to_path` (`:182`). `PartialConfig::from(&Config)`
(`partial_config.rs:1096`) prunes default plugins and never serializes plugin
`path` (auto-discovered).

> **Known limitation (implemented, ignored test).** `save_to_layer` alone
> cannot prune a value reset to the parent value (`#[ignore]` test
> `config_io.rs:2085`); the baseline / changes paths solve it. Tests
> `issue_806_*` (`config_io.rs:2679+`) document that `save_to_layer` is not a
> true read-modify-write under external concurrent edits; the pointer-based
> `save_changes_to_layer` is what preserves external edits and comments.

### 1.4 partial_config.rs

`partial_config.rs` defines `Option`-wrapped mirrors of every config struct
(`PartialConfig` `:73`, `PartialEditorConfig` `:146`, `PartialLanguageConfig`
`:524`, `PartialPluginConfig` `:483`, …) where `None` means "inherit". This is
the substrate for delta serialization: serializing a Partial naturally omits
`None` keys, and the diff/prune logic operates on these shapes. `From<&Config>`
lifts a full config to all-`Some`; `resolve()/resolve_with_defaults()` collapse
back. `SessionConfig` (`partial_config.rs:1385`) is a focused volatile subset
(theme, editor, per-buffer `buffer_overrides`).

### 1.5 The Config struct

`pub struct Config` (`config.rs:357`), all fields `#[serde(default)]`. Top
level: `version`, `theme: ThemeName`, `locale`, `check_for_updates`; nested
`editor`, `file_explorer`, `file_browser`, `clipboard`, `terminal`, `warnings`,
`packages`, `env`. Keybindings: `keybindings: Vec<Keybinding>`,
`keybinding_maps: HashMap<String, KeymapConfig>`, `active_keybinding_map`
(default/emacs/vscode/macos/macos-gui embedded via `include_str!`,
`config.rs:3817`). `languages: HashMap<String, LanguageConfig>` (+
`default_language`). LSP: `lsp_enabled`, `lsp`, `universal_lsp`. `plugins:
HashMap<String, PluginConfig>` (auto-discovered).

`PluginConfig` lives in **fresh-core** (`crates/fresh-core/src/config.rs:20`) so
the plugin runtime can read it from JS bindings; `plugin_schemas.rs` is a
6-line re-export for the editor crate. Each plugin's free-form `settings` is
stored as untyped `serde_json::Value` so a malformed plugin schema cannot
poison the rest of the config (`fresh-core/src/config.rs:34-41`).

Migration: `CURRENT_CONFIG_VERSION = 2` (`config_io.rs:322`); `migrate_config`
chains v0→v1 (camelCase→snake_case) and v1→v2 (prepend `{remote}` to
`status_bar.left`), run in `load_layer_from_path` before deserialization.

---

## 2. JSONC Support & Comment Preservation

Three commits, landed in order, fixed a silent-data-loss class of bug. The
chronology is the design rationale.

1. **Accept JSONC on read** (`21c5fd801`). Config files were parsed with strict
   `serde_json::from_str`, so a single `//` comment made a layer invalid; in the
   layered flow the error was swallowed and the editor silently fell back to
   defaults. All reads now route through `parse_config_jsonc` (`config.rs:7457`)
   wrapping **`jsonc_parser::parse_to_serde_value`** — tolerates comments and
   trailing commas; empty/comment-only input → empty object. Covers user,
   project, session layers, the read-modify-write save paths,
   `read_user_config_raw`, and `Config::load_from_file` (`--config`,
   `config.rs:3803`). Genuinely malformed input still errors.

2. **Preserve comments on rewrite** (`d5b9693c8`). Writes still went through
   `serde_json::to_string_pretty`, which reconstructs from a value and drops all
   comments. Writes now go through the jsonc-parser **CST**:
   `render_config_text` (`config_io.rs:198`) → `reconcile_preserving_comments`
   (`:211`) parses existing text with `CstRootNode::parse`, and
   `reconcile_cst_object` (`:230`) edits minimally — unchanged props left
   byte-for-byte (it even **skips writes when the value is already equal**,
   `:252`, to keep inline comments), nested objects recurse, removed keys
   deleted, new keys appended. New/unparseable/non-object-root files fall back
   to pretty-print. All three save paths funnel through
   `write_clean_value_to_path`, so this covers every layer write.

3. **Never overwrite an unparseable config** (`ba4ba54d1`). A save is a
   read-modify-write; the read side previously swallowed parse failures and
   returned `{}`, so one comment plus any save destroyed the whole file.
   `read_existing_json` (`config_io.rs:303`) now **errors** (`ParseError`) when
   an existing non-empty file can't parse; `save_to_layer` repeats the guard
   inline (`:587`). Regression tests assert the file is left byte-for-byte
   intact (`config_io.rs:1507-1582`).

**Surfacing save errors** (`7ac820734`, follow-up `1811e7b4f`). When the
no-clobber guard aborts a save, a plain status-bar line was easy to miss. The
`ConfigError` propagates out and `app/settings_actions.rs:268` raises
`show_settings_save_error_popup(target_layer, &err)`
(`app/popup_dialogs.rs:1540`) — a centered red-bordered modal with the parse
error, file path and line/column, and a note that the file was left unchanged.
Acknowledging the modal (Esc/Enter) **opens the offending config file** via
`PopupResolver::SettingsSaveError { layer }` so the user can fix the syntax.

`ConfigError` variants: `IoError | ParseError | SerializeError |
ValidationError` (`config.rs:7428`).

---

## 3. Schema Generation (schemars) and How It Drives the UI

### 3.1 Generation

`config-schema.json` is generated offline, not at build time:
`bin/generate_schema.rs:21` calls `schema_for!(Config)` (schemars), drops the
huge `menu` default, and the result is committed to
`crates/fresh-editor/plugins/config-schema.json` (2070 lines). The same binary
emits the theme schema (`schema_for!(ThemeFile)`) and the package schema.
`build.rs:4` notes the schema is no longer build-generated.

The schema is **compiled into the binary** via
`include_str!("../../plugins/config-schema.json")` at the single load point
`Editor::open_settings` (`app/settings_actions.rs:22`) and reused in settings
tests.

Custom `x-` extensions schemars emits / the struct attributes inject (verified
present in the JSON): `x-display-field`, `x-standalone-category`, `x-no-add`,
`x-section`, `x-order`, `x-enum-from`, `x-dual-list-options`,
`x-dual-list-sibling`, `x-dynamically-extendable-status-bar-elements`. Example:
`PluginConfig` carries `x-display-field = "/enabled"`
(`fresh-core/src/config.rs:21`) and marks `path`/`settings` `readOnly`.

### 3.2 Schema → UI pipeline

`config-schema.json` → `parse_schema` (`view/settings/schema.rs:285`) →
`Vec<SettingCategory>` → `build_pages` (`items.rs:874`) → `Vec<SettingsPage>`
of `SettingItem` (each carrying a `SettingControl`) → rendered by `render.rs`,
hit-tested by `SettingsLayout` (`layout.rs`). `schema.rs` is the only
WASM-compatible settings module; everything else is `#[cfg(feature =
"runtime")]` (`view/settings/mod.rs:16-34`).

`parse_schema` walks top-level `properties`. A property becomes a **standalone
category** when `x-standalone-category` is set (Maps like `languages`,
`schema.rs:318`), a **category with nested settings** when the resolved schema
has `properties` (`:329`), else a **top-level setting** folded into a
synthesized "General" category (`:357`). Plugin schemas are appended as
`Plugin: <name>` categories rooted at `/plugins/<name>/settings` (`:390`).

Control-type selection: `determine_type` (`schema.rs:577`) maps the schema to a
`SettingType` (`schema.rs:92`):

| Schema | SettingType | Control |
|---|---|---|
| `boolean` | `Boolean` | Toggle |
| `integer` (+min/max) | `Integer` | Number |
| `number` | `Number` | Number (floats stored ×100, `items.rs:1031`) |
| `string` | `String` | Text (Dropdown if `x-enum-from`) |
| inline/`x-enum-values` enum | `Enum` | Dropdown |
| `array<string|int>` | `StringArray`/`IntegerArray` | TextList |
| `array` + `x-dual-list-options` | `DualList` | DualList |
| `array<$ref object>` | `ObjectArray` | ObjectArray (keybindings) |
| `object` + `additionalProperties` | `Map` | Map (entry dialogs) |
| `object` + fixed `properties` | `Object` | flattened controls or JSON |
| fallback | `Complex` | uneditable / JSON editor |

It handles the schemars `Option<T>` shape `anyOf:[{$ref},{type:"null"}]`
(`schema.rs:739`) and turns a `null` enum value into an "Auto-detect" option
with empty-string value (`:606`).

> **Discrepancy.** `SettingCategory.subcategories` (`schema.rs:163`) and
> `SettingsPage.subpages` (`items.rs:846`) exist but `parse_schema` never
> populates subcategories — the left panel is a flat categories+sections tree
> (`TreeRow`, `state.rs:710`), not true nested subpages. `expand_or_build`
> (`items.rs:951`) flattens an inner `Object` into native child controls only
> when every child is editable; otherwise the whole object collapses to one JSON
> editor. So "nested object → subcategory" holds only for top-level properties.

---

## 4. Settings UI Architecture

### 4.1 State and modules

Module map at `view/settings/mod.rs:6`. The central state is `SettingsState`
(`state.rs:80`, ~3800 lines) holding `pending_changes: HashMap<String, Value>`
(`:94`), `pending_deletions: HashSet<String>` (`:174`), `layer_sources:
HashMap<String, ConfigLayer>` (`:170`), `target_layer: ConfigLayer` (`:163`,
default User), `entry_dialog_stack` (`:159`, nested dialog editing), and
`original_config` (`:96`). Module responsibilities:

- `schema.rs` — pure parse of schema → categories.
- `items.rs` — schema → renderable `SettingItem` + live `SettingControl`.
- `state.rs` — all UI state, pending changes, dialogs, layers, input mutators.
- `render.rs` / `layout.rs` / `mouse.rs` / `input.rs` — draw, hit-test
  (`SettingsHit`, `layout.rs:377`), mouse dispatch, key routing
  (Dialog→Panel→Control).
- `search.rs` — fuzzy search.
- `entry_dialog.rs` — `EntryDialogState` modal for Map/ObjectArray entries.

### 4.2 Control widgets

`SettingControl` enum (`items.rs:343`): `Toggle`, `Number` (int/float-as-%),
`Dropdown` (enum/select), `Text`, `TextList` (string/int arrays), `DualList`
(ordered subset with sibling cross-exclusion), `Map` (key→value dict), `Map`'s
`ObjectArray` (`KeybindingListState`), `Json` (multiline JSON editor for
Object/Complex), and `Complex` (uneditable). There is **no dedicated color or
file-path control** — colors fall through to Text/JSON. Heights via
`control_height` (`items.rs:365`); render dispatch `render_control`
(`render.rs:3772`) with aligned widget renderers.

### 4.3 Modified indicator

"Modified" means **the value is defined in the target layer being edited**, not
"differs from default" (new semantics, `items.rs:1221`): `modified = layer_source
== ctx.target_layer`. `layer_source` comes from `ConfigResolver::get_layer_sources`
(`config_io.rs:758`) → `state.set_layer_sources` (`state.rs:1113`), defaulting to
`System` when a value is purely a schema default.

Rendering: a 3-column indicator gutter (`focus_indicator_cols: 3`,
`items.rs:506`) — col 0 the focus `>`, col 1 the modified `●`
(`render.rs:3758`), col 2 spacer. Category labels get a `"● "` prefix when the
page has pending changes (`render.rs:444`); the title bar shows `•
(modified)` with the layer name (`render.rs:194`). The per-value layer source
(`user`/`project`/`session`, None for System) is shown in the description band
(`render.rs:1147`).

`item.modified` (layer-definition) and `path_has_pending_change`
(unsaved-this-session, `state.rs:382`) are deliberately separate signals.

> The dialog context uses the *old* definition (modified = differs from schema
> default, `items.rs:1459`) because dialogs have no layer context.

### 4.4 Entry dialogs

`EntryDialogState` (`entry_dialog.rs:80`) is **schema-driven, not per-type
hardcoded** — there is no special "add language" vs "add LSP" dialog. The
language/LSP/universal-LSP dialogs are the same generic dialog opened over the
`/languages`, `/lsp`, `/universal_lsp` Maps. `from_schema` (`:152`) injects a
synthetic read-only `__key__` field plus one item per object property;
`for_array_item` (`:259`) handles ObjectArray items by index. Nested
Map/ObjectArray values open via `open_nested_entry_dialog`
(`state.rs:1743`) pushing onto `entry_dialog_stack`.

Per-field buttons `[Reset]`/`[Inherit]`/`[Clear]` (`FieldAction`,
`entry_dialog.rs:31`): Reset → built-in default; Inherit/Clear → null. Built-in
per-entry defaults come from the bundled config (`apply_builtin_defaults`,
`state.rs:3011`). Inherited (null) nullable fields are omitted on save so they
keep inheriting (`to_value`, `entry_dialog.rs:589`; issue #2345,
`9e02fd631`).

### 4.5 Edit → save flow

Control mutators call `on_value_changed` (`state.rs:1296`), which compares
against the effective original and calls `set_pending_change`
(`state.rs:1016`) — toggling a value back to its original **clears** the
pending entry. `target_layer` cycles User→Project→Session via
`cycle_target_layer` (`state.rs:1087`); System is read-only; switching layers
clears pending changes and rebuilds pages.

`close_settings(true)` → `save_settings` (`app/settings_actions.rs:75`): builds
the merged config via `apply_changes` (`state.rs:1031`, deletions first then
changes), applies runtime side-effects (theme, locale, plugins, keybindings,
LSP, bars, file-explorer, `:114-231`), persists the **delta** via
`resolver.save_changes_to_layer(&pending_changes, &pending_deletions,
target_layer)` (`:244`), re-resolves from disk (`:250`), and clears
`settings_state` so a reopen is fresh (#474). On `Err` the change is not applied
and the save-error modal (§2) is raised.

Reset/revert: `discard_changes` (`state.rs:1066`); `reset_current_to_default`
(`state.rs:1169`) removes the value from the target layer (via
`pending_deletions`) so it falls back to inherited — it does **not** write the
schema default; `set_current_to_null` / `clear_current_category` for nullable
items.

> **Separate prompt system.** `app/settings_prompts.rs` (~980 lines) is a
> command-palette prompt set (theme, language, encoding, keybinding-map, cursor
> style, locale, line-ending, ruler) independent of the modal; it calls
> `save_to_layer`/`save_changes_to_layer` directly against `ConfigLayer::User`.
> Its module doc flags the repetitive `start_*`/`apply_*`/`save_*` triples as
> awaiting refactor.

### 4.6 Search

`search_settings` (`search.rs:60`) fuzzy-matches name/description/path (best of
three, custom scorer `fuzzy_match` `:275`) and recurses into composite controls
and Map/TextList values (`DeepMatch`, `search.rs:10`). `jump_to_search_result`
(`state.rs:1495`) navigates to the page/item, dives into the deep-match target,
auto-expands the category, and cancels search.

> **Discrepancy.** Footer button indices in `app/settings_actions.rs:451`
> enumerate four (Layer/Reset/Save/Cancel) but comments in `state.rs:290`,
> `state.rs:966` reference a 5th "Edit" button — worth verifying against the
> live footer renderer.

---

## 5. Theme System (design-decisions #19)

### 5.1 Types

Two parallel representations (`view/theme/types.rs`):

- **`ThemeFile`** (`:408`) — the serializable file form, nested into optional
  sections each with `#[serde(default)]`: `EditorColors` (`:471`), `UiColors`
  (`:654`), `SearchColors` (`:1128`), `DiagnosticColors` (`:1167`),
  `SyntaxColors` (`:1222`). Supports `extends: Option<String>` (`:417`).
- **`Theme`** (`:1294`) — the flat runtime form, ~140 fields, every color a
  `ratatui::style::Color`, plus two `Modifier` fields for SGR attributes.

Conversion is `From<ThemeFile> for Theme` (`:1501`) and back (`:1748`). Colors:
`ColorDef` enum (`:155`), `#[serde(untagged)]` — `Rgb(u8,u8,u8)` serialized as
`[r,g,b]` or `Named(String)` (`"Default"`/`"Reset"` → terminal transparency).
`ThemeName` is a config newtype, not in the theme module
(`config.rs:14`, default `"high-contrast"`).

### 5.2 Loading and built-ins (consolidation — IMPLEMENTED)

> design-decisions #19 lists theme consolidation as "not yet shipped". **It is
> shipped, and went further than the plan.** There are no `Theme::dark()` /
> `Theme::light()` Rust constructors; all built-ins are JSON.

Built-in themes are embedded JSON generated by `build.rs:155`
(`generate_builtin_themes`), which scans `themes/` and emits
`$OUT_DIR/builtin_themes.rs` defining `BUILTIN_THEMES: &[BuiltinTheme]` using
`include_str!` per file (`build.rs:174`), pulled in at `types.rs:31`. Eight
shipped files: `themes/{dark,light,high-contrast,nostalgia,dracula,nord,
solarized-dark,terminal}.json`.

`ThemeLoader` (`loader.rs:303`) `load_all` (`:332`) deserializes each builtin
JSON to `ThemeFile` → `Theme`, then scans user themes (`<config_dir>/themes/`
recursively) and `themes/packages/*/`, producing a `ThemeRegistry` (`:97`).
Override-by-name: builtins keyed by bare name, user themes keyed by
`file://<path>`; resolution precedence in `ThemeRegistry::resolve_key`
(`loader.rs:138`) — `builtin://NAME` forces the built-in, a relative
`dark.json` resolves to the **user** theme overriding builtin `dark` (test
`loader.rs:887`). `extends`/auto-base inheritance via `resolve_base_theme`
(`types.rs:1914`, luminance-based light/dark auto-pick) + `apply_theme_overrides`
(`:1965`). The planned `getBuiltinThemes()` plugin API exists
(`get_builtin_themes`, `types.rs:2312`; JS `editor.getBuiltinThemes()`).

### 5.3 Live preview (IMPLEMENTED) and the theme editor

Two preview mechanisms:

1. **"Select Theme" palette live preview** (Rust). `start_select_theme_prompt`
   (`settings_prompts.rs:321`) stores `original_theme`; navigating suggestions
   dispatches `DeferredAction::PreviewThemeFromPrompt` → `preview_theme`
   (`settings_prompts.rs:540`) which swaps `self.theme` in-memory **without
   persisting**, runs a crossfade, and updates the cursor color. Cancel restores
   the original (`prompt_lifecycle.rs:913`).
2. **`theme_editor.ts` plugin** (`plugins/theme_editor.ts`, ~3000 lines): command
   `open_theme_editor`, ~17 scoped commands under context `"theme-editor"`. Loads
   the registry, edits fields, accepts hex/`[r,g,b]`/named colors
   (`applyPickerColor`), renders an in-buffer "Preview:" panel, saves via
   `saveTheme()` / `deleteTheme()`.

Usability issues identified (`theme-usability-improvements.md`): no full
live-apply while editing colors (only the preview panel), no discard
confirmation, can't directly edit existing themes, inconsistent navigation. Note
the consolidation/user-flow docs reference the obsolete `src/view/theme.rs`
single file — the code is the split `view/theme/{mod,types,loader}.rs`.

---

## 6. Indentation Rules & `init.ts` Config

### 6.1 User-configurable indentation rules (IMPLEMENTED)

A third indentation tier (`indentation-rules-design.md`, status implemented):
per-language regex rules in the style of VS Code `indentationRules`, run via the
`regex` crate (no `fancy-regex` — RE2, no lookarounds) with **scope-masking**
(comment/string bytes blanked before matching) as the anti-glitch mechanism.
The authoritative shape is `primitives/indent_rules.rs`. The config surface is
`IndentRulesConfig` on `LanguageConfig.indent: Option<IndentRulesConfig>`
(`config.rs:2539`, struct `:2557`): `increase_indent_pattern`,
`decrease_indent_pattern`, `indent_next_line_pattern`,
`dedent_next_line_pattern`, `self_close_pattern` (commit `85371c52c`). A
`USER_RULES` registry layers overrides over built-in families; unset patterns
inherit from the family; `config::reload_indent_overrides` re-registers on every
config load/reload. The rollout also reduced bundled tree-sitter grammars
(43.9 MB → 25.8 MB), with a parity CI guard.

### 6.2 `init.ts` — programmable user config (IMPLEMENTED)

`init_script.rs` loads `~/.config/fresh/init.ts` (if present) through the plugin
pipeline as a plugin named `init.ts` (`INIT_PLUGIN_NAME`, `:21`) — same code
path as "Load Plugin from Buffer", so reload/unload are free. Design intent
(`init-config-design.md`): for decisions that depend on the runtime environment
(host, `$TERM`, SSH, project path) and imperative plugin configuration — **not**
for static preferences (Settings UI), keybindings (editor), permanent themes
(selector), or reusable features (plugin packages). The starter template
(`STARTER_TEMPLATE`, `:30`) is all-commented and spells out this non-overlap.

- **Crash fuse**: `~/.config/fresh/logs/init.crashes` counts consecutive
  failures in a 300s rolling window (`CRASH_FUSE_THRESHOLD = 3`,
  `CRASH_FUSE_WINDOW_SECS = 300`); after 3 the next launch auto-skips init.ts
  (`check_and_increment_fuse`, `:471`; reset by `record_success`, `:497`).
- **Disable flags**: `--no-init` / `--safe` (`decide_load`, `:538`).
- **Type scaffolding**: `refresh_types_scaffolding` (`:280`) always overwrites
  `types/fresh.d.ts` from the embedded copy (must track the binary) and writes
  `tsconfig.json` only on first run; `write_plugin_declarations` (`:319`)
  aggregates each loaded plugin's `.d.ts` so `getPluginApi("foo")` is typed.
- **Check mode**: `fresh --cmd init check` parses init.ts via `oxc` and reports
  syntax errors (`check`, `:600`); deeper `tsc` type-check and scope-discipline
  lints are deliberately not implemented (`:586`).

---

## 7. Keybinding Editor

### 7.1 Architecture (view/app split)

State+logic in `app/keybinding_editor/` (`editor.rs` `KeybindingEditor` struct
`:16`, `types.rs` data model, `helpers.rs` conversions); rendering **and input**
in `view/keybinding_editor.rs`; persistence glue in
`app/keybinding_editor_actions.rs`. Invoked by
`Editor::open_keybinding_editor` (`keybinding_editor_actions.rs:14`). The modal
shows a collapsible, plugin-grouped table: **Key | Action | Description |
Context | Source** (`view/keybinding_editor.rs:288`).

`resolve_all_bindings` (`editor.rs:204`) layers sources by precedence into a
`(key_display, context)` dedup map: active keymap (`BindingSource::Keymap`) →
user `config.keybindings` (`Custom`, overrides keymap) → plugin defaults
(`Plugin`) → `Unbound` placeholders for every action so all stay rebindable.

### 7.2 Editing, conflicts, persistence

Enter opens the edit dialog (`open_edit_dialog`, `editor.rs:801`). Key capture
is a deliberate two-step "special capture": Enter arms
`capturing_special`, the next raw keypress is normalized (`normalize_key`,
uppercase→lowercase+SHIFT, issue #1899) and stored. The action field has
substring autocomplete. Conflict detection `find_conflicts` (`editor.rs:1070`)
is **advisory only — it does not block saving**.

`apply_edit_dialog` (`editor.rs:979`) validates the action name and pushes a
`Keybinding` to `pending_adds`. Final save `save_keybinding_editor_changes`
(`keybinding_editor_actions.rs:72`) removes `pending_removes` and adds
`pending_adds` to `config.keybindings`, reloads the resolver, and writes the
`/keybindings` JSON pointer to **`ConfigLayer::User`** via
`save_changes_to_layer`. Deleting a keymap/plugin binding shadows it with a
custom `noop` (`override_binding_with_noop`, `:880`).

> **Discrepancies.** (1) Editing an existing binding pushes a fresh
> `pending_adds` entry without removing the superseded one
> (`editor.rs:1026`) — repeated edits accumulate redundant entries. (2) Chords
> are displayed and resolved but **cannot be authored** in the editor —
> `apply_edit_dialog` always writes `keys: []`, `is_chord: false`
> (`editor.rs:1019`). (3) Conflicts are non-blocking.

### 7.3 Keymaps

The four built-in maps + user maps live in `config.keybinding_maps`; the active
one is `config.active_keybinding_map` (resolution with inheritance,
`config.rs:3838`). The editor reads only the active map and shows the map names
read-only — it cannot switch the active map. Switching is the
`switch_keybinding_map:<name>` action, expanded per map by
`expand_variant_actions` (`editor.rs:438`); `Action::SwitchKeybindingMap`
persists the choice to the User layer (issue #474).

---

## 8. Toggle Actions (`app/toggle_actions.rs`)

`impl Editor` block for boolean/config toggles. Two patterns: **global
preference toggles** mutate runtime config and persist to User via
`persist_config_change` (`:651`, a single-pointer `save_changes_to_layer` to
`ConfigLayer::User`), vs **per-split/per-buffer view-state toggles** stored only
in per-file workspace state.

| Method | Setting | Persists |
|---|---|---|
| `toggle_line_numbers` `:28` | `editor.line_numbers` | yes (`/editor/line_numbers`) |
| `toggle_line_numbers_current_buffer` `:76` | per-buffer override | workspace only |
| `toggle_line_wrap_current_buffer` `:116` | per-buffer wrap | workspace only |
| `toggle_menu_bar` `:233` | `editor.show_menu_bar` | yes (#1156) |
| `toggle_file_explorer_side` `:262` | `file_explorer.side` | yes |
| `toggle_vertical_scrollbar` `:284` | `editor.show_vertical_scrollbar` | yes (#474) |
| `toggle_horizontal_scrollbar` `:302` | `editor.show_horizontal_scrollbar` | yes (#474) |
| `toggle_mouse_capture` `:378` | window mouse_enabled | runtime only |
| `toggle_mouse_hover` `:405` | `editor.mouse_hover_enabled` | **runtime only — bug** |
| `toggle_inlay_hints` `:447` | `editor.enable_inlay_hints` | **runtime only — bug** |

Related config ops live here too: `dump_config` (`:474`, saves whole config and
opens it), `reload_config` (`:546`, re-resolves layers and re-applies
theme/keybindings/bars/LSP), `reload_themes` (`:615`).

> **Discrepancy.** `toggle_mouse_hover` and `toggle_inlay_hints` mutate
> `config_mut()` but never call `persist_config_change`, unlike the sibling
> `editor.*` toggles — so they reset on restart. `toggle_tab_bar` /
> `toggle_status_bar` are not here (they live on `impl Window`, comment `:252`).

---

## 9. Cross-Cutting Principles

- **Single source of truth via deltas.** Each layer stores only its diff from
  the resolved parent; pruning prevents config drift; Partial mirrors make this
  natural.
- **Schema-driven UI.** One schemars-generated JSON Schema drives both
  validation and the entire Settings UI; `x-` extensions carry UI hints.
- **Never lose user data.** JSONC reads, CST comment-preserving writes, and the
  no-clobber-on-unparseable guard form a layered defense; failures surface as a
  modal that opens the file.
- **Right tool per surface.** Settings UI for static prefs, keybinding editor
  for keys, theme selector for themes, `init.ts` only for env-dependent or
  imperative config — enforced by the starter template and the `init check`
  tool.

---

## Discrepancies & Open Items (summary)

- 4-layer docs omit the UserPlatform rung `resolve()` applies
  (`config_io.rs:437` vs `:418`).
- `save_to_layer` can't prune a value reset to parent and isn't concurrency-safe
  (`#[ignore]` test `:2085`; `issue_806_*` tests); use the baseline / changes
  paths.
- `SettingCategory.subcategories` / `subpages` are defined but never populated.
- Settings footer button count: code has 4, comments reference a 5th "Edit".
- Float settings stored as integer ×100 throughout the UI (`items.rs:1031`).
- Keybinding editor: accumulating `pending_adds` on re-edit; no chord authoring;
  non-blocking conflicts.
- `toggle_mouse_hover` / `toggle_inlay_hints` don't persist.
- Theme consolidation is shipped despite design-decisions #19 marking it
  planned; several theme docs reference the obsolete `src/view/theme.rs`.
