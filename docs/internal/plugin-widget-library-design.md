# Plugin widget library — design + implementation plan

Status: **rev 3 — implementation in progress, partial in tree.** See
§2 for what's shipped, §3 for how to pick up the work, §4 for the
remaining roadmap.
Branch: `claude/plugin-ui-component-library-wO76I`
Original design author: staff-eng review, rev 1 + rev 2.
Related: `docs/internal/UNIFIED_UI_FRAMEWORK_PLAN.md`,
`docs/internal/unified-hit-test-theme-plan.md`,
`docs/internal/unified-keybinding-resolution.md`,
`docs/internal/event-dispatch-architecture.md`,
`docs/internal/visual-layout-unification.md`,
`docs/internal/plugin-usability-review.md`,
`docs/internal/settings-controls-usability-report.md`

Criterion: end-state UX, robustness, flexibility. Shipping speed is
explicitly *not* a constraint. (See Appendix A for the rejected
TS-only alternative on `claude/design-plugin-ui-library-pxri8`, which
optimizes for the opposite tradeoff.)

---

## 1. Recommendation (unchanged from rev 2)

**Hybrid: a Rust-resident widget runtime with a thin TypeScript
declarative front-end. Plugins describe widgets as data, the host
reconciles, owns layout / hit-test / focus / cursor, and emits
semantic events. The existing `setVirtualBufferContent` primitive
stays as the escape-hatch.**

The design rationale, comparison against pure-TS and pure-Rust
alternatives, and the structural reasons this is the only shape that
satisfies the brief's five constraints (per-keystroke cost, theme,
reach, backward compat, sandboxing) live in §10 below. Read those if
you're picking up the work and need the *why* before the *what*.

---

## 2. Implementation status

### 2.1 What's in tree on this branch

The runtime is real. Plugins can mount widget panels today; one
plugin (`search_replace.ts`) is migrated end-to-end across the bulk
of its UI. ~13 commits, `3257fa7..a436874`. cargo check workspace
clean, 70 widget tests passing, tsc clean, interactive tmux-verified.

**Rust runtime** (`crates/fresh-editor/src/widgets/`)

| File | Purpose |
|---|---|
| `mod.rs` | Public surface: re-exports `render_spec`, `RenderOutput`, `FocusCursor`, `WidgetRegistry`, `HitArea`, `WidgetInstanceState`, `find_widget_by_key`, `apply_text_input_key`, `set_toggle_checked_in_spec`, `set_list_items_in_spec` |
| `registry.rs` | `WidgetRegistry`: `panel_id → WidgetPanelState { buffer_id, spec, hits, instance_states, focus_key, tabbable }`. Hit-test, get/get_mut, focus_key getter/setter, mount/update/unmount. |
| `render.rs` | The reconciler. `render_spec(spec, prev_state, prev_focus, panel_width) → RenderOutput { entries, hits, instance_states, focus_key, tabbable, focus_cursor }`. Two-pass Row layout for flex spacers. Per-widget renderers (`render_hint_bar`, `render_toggle`, `render_button`, `render_text_input`). |
| `actions.rs` | Pure helpers used by dispatch: `apply_text_input_key` (Backspace/Delete/arrows/Home/End with UTF-8 boundary handling), `find_widget_by_key`, `set_toggle_checked_in_spec`, `set_list_items_in_spec`. 14+ unit tests. |

**Core types** (`crates/fresh-core/src/api.rs`)

| Type | Notes |
|---|---|
| `WidgetSpec` (enum, tagged) | Variants: `Row`, `Col`, `HintBar`, `Toggle`, `Button`, `TextInput`, `TextArea`, `List`, `Tree`, `Spacer`, `Raw`. |
| `HintEntry`, `ButtonKind`, `WidgetAction`, `WidgetMutation` | Shapes referenced by the spec / IPC. |
| `PluginCommand::MountWidgetPanel`, `UpdateWidgetPanel`, `UnmountWidgetPanel` | Spec lifecycle. |
| `PluginCommand::WidgetCommand { panel_id, action }` | Routes a `WidgetAction` (key dispatch / focus / activate / select-move / text-input). |
| `PluginCommand::WidgetMutate { panel_id, mutation }` | Targeted in-place mutation (the "Path A" fast path). `setValue` / `setChecked` / `setSelectedIndex` / `setItems`. |
| `HookArgs::WidgetEvent` | `widget_event` hook payload: `panel_id`, `widget_key`, `event_type`, `payload`. Fired for `select` / `activate` / `toggle` / `change`. |

**Dispatch glue** (`crates/fresh-editor/src/app/`)

| File | Touch point |
|---|---|
| `mod.rs` | `widget_registry: WidgetRegistry` field on `Editor`. |
| `editor_init.rs` | Constructor seeds the registry. |
| `plugin_dispatch.rs` | `handle_mount_widget_panel`, `handle_update_widget_panel`, `handle_unmount_widget_panel`, `handle_widget_command`, `handle_widget_mutate`. `rerender_widget_panel` (host-side re-render after focus advance / selection move / mutator). `apply_widget_focus_cursor` (translates `RenderOutput.focus_cursor` to a buffer hardware-cursor position + `show_cursors`). `widget_panel_width` (best-effort buffer width for flex layout). |
| `click_handlers.rs` | Mouse click on a widget's hit area moves focus + fires `widget_event`. |

**TS surface** (`crates/fresh-editor/plugins/lib/`)

| File | Exports |
|---|---|
| `widgets.ts` | Builders: `row`, `col`, `hintBar`, `toggle`, `button`, `textInput`, `textArea`, `list`, `tree`, `treeNode`, `spacer`, `flexSpacer`, `raw`, `parseHintString`. Action builders: `key`, `focusAdvance`, `activate`, `selectMove`, `textInputKey`, `textInputChar`. `WidgetPanel` class with `set` / `command` / `mutate` / `setValue` / `setChecked` / `setSelectedIndex` / `setItems` / `setExpandedKeys` / `unmount`. |
| `index.ts` | Re-exports the above. |
| `fresh.d.ts` | Generated. `editor.mountWidgetPanel`, `updateWidgetPanel`, `unmountWidgetPanel`, `widgetCommand`, `widgetMutate`. `WidgetSpec`, `HintEntry`, `ButtonKind`, `WidgetAction`, `WidgetMutation` types. `widget_event` hook. |

**Plugin migration: `search_replace.ts`**

| Migrated | Status |
|---|---|
| HintBar (footer) | `parseHintString(t("panel.help"))` → `hintBar(...)`. Theme-keyed key styling. |
| Options row (3 toggles + Replace All button) | `row(toggle("case"), toggle("regex"), toggle("whole"), flexSpacer(), button("replaceAll", { intent: "primary" }))`. Right-aligns the button via flex. |
| Search / Replace text fields | `textInput(value, { fieldWidth: 25, key: "searchField" })`. Constant-width with head-truncate scrolling, host-owned hardware cursor. |
| Match tree | `list({ items, itemKeys, selectedIndex, visibleRows, key: "matchList" })`. Widget-owned scroll, click-to-select, Enter-to-activate. |
| Mode bindings (Tab / Shift+Tab / Enter / Space / Backspace / Delete / Home / End / Up / Down / Left / Right / mode_text_input) | All route through `dispatch(widgetKey("Tab"))` etc. The smart-key dispatcher in core handles based on focused widget kind. |
| `widget_event` handlers (`change` / `select` / `activate` / `toggle`) | Plugin updates its app model from events; toggle writes back via `panel.setChecked` (mutator fast path); selection / value changes don't re-emit spec. |

What's *not* migrated in `search_replace.ts`: the matches-section
separator (still in `Raw`), the `truncated` warning in matchStats
(bespoke RGB), the `panel.focusPanel`/`queryField`/`optionIndex`
legacy state (kept around but no longer authoritative). These are
not blockers for any flow; they're cleanup.

**Theme keys actually used by widgets today**

| Widget area | Theme key |
|---|---|
| HintBar key portions | `ui.help_key_fg` |
| Toggle "checked" glyph | `ui.tab_active_fg` |
| Focused widget bg/fg | `ui.menu_active_bg` / `ui.menu_active_fg` |
| Button "danger" intent | `ui.status_error_indicator_fg` |
| TextInput focused bg | `ui.prompt_bg` |
| TextInput placeholder | `ui.menu_disabled_fg` |
| List selected row | `ui.menu_active_bg` (extend_to_line_end) |

These are all reuses of pre-existing keys. The role-based theme
system from §11 is not yet implemented — plugins still implicitly
pick theme keys via `intent: "primary" | "danger"` enums; no
per-spec `theme` override map yet.

### 2.2 What's not yet built

In rough decreasing user impact:

1. ~~**`Tree` widget.**~~ Shipped. `search_replace.ts` migrated to
   `tree(...)` with host-owned expansion + scroll + selection.
2. ~~**Targeted spec subtree replacement (`WidgetMutation::SetSpec`).**~~
   Skipped. The reconciler already preserves instance state across
   a full `panel.set(spec)` re-emit, so a SetSpec fast path is a
   pure IPC-byte optimization with no UX consequence; revisit only
   if profiling on a large-spec panel shows it matters.
3. ~~**`Tabs` / `Group` widget.**~~ Skipped — no in-tree consumer.
   `git_log.ts`'s "tab" toolbar is a strip of action buttons
   (Tab/RET/y/r/q), not a UI tab switcher; the buffer-group panes
   are managed by the editor's panel manager outside the widget
   runtime. Revisit when a real consumer appears.
4. ~~**`TextArea` widget.**~~ Shipped. Multi-line input with
   host-owned value, cursor, and vertical scroll; `Enter` inserts
   a newline (form-style "Enter submits" remains the `TextInput`
   default), `Up`/`Down` walk lines preserving column,
   `Home`/`End` jump within the current line. Smart-key dispatch
   table extended in `WidgetAction::Key` doc.
5. **`Prompt` / `Layer` / Compositor (§7 in this doc).** The big
   architectural piece. Today `Popup`, `Prompt`, `showActionPopup`,
   hover tooltips, completion popups all live in separate
   subsystems. Unifying them under one Compositor with a `mountLayer`
   IPC subsumes a lot of duplicated focus/dismiss/event-routing
   logic, but no plugin can currently mount a tooltip or modal via
   the widget runtime.
6. **`Transient` widget (Magit menu).** Discoverability per
   `plugin-usability-review.md`.
7. **`Table` widget.** `git_log.ts` log, `find_references.ts`, audit.
8. **Role-based theming.** The §11 design says widgets carry roles
   (`Role::Action`, `Role::Destructive`, …) and the host resolves to
   theme keys. Today the renderer's theme keys are hardcoded in
   `widgets/render.rs`. Adding a `roles.rs` translation layer lets
   plugins override per-widget without touching colors and lets
   accessibility variants (high-contrast, color-blind) drop in.
9. **Spec-as-first-class-state (§10).** Session restore, theme-switch
   live re-render, replay, headless rendering, cross-plugin
   composition (`embed`). The `Spec` is already data; what's missing
   is the persistence layer and the plumbing to re-render every
   active panel on a `theme_changed` event.
10. **Accessibility (§13).** Screen-reader bridge (OSC 52), ARIA strings
    on focus change, motion-reduce gating. Library-default
    `lib-widgets.i18n.json`.
11. **IME composition in TextInput.** `mode_text_input` already
    delivers composed text but the widget cursor model doesn't track
    composition states.
12. **Built-in chord support inside widgets.** Today
    `apply_text_input_key` only handles single-key edits; chords
    (`g g`) still bubble to the plugin's `defineMode`.
13. **Settings adoption.** §11 says Settings should adopt the
    `view/controls/*` renderers shared with widgets. Today widgets
    have their own renderer in `widgets/render.rs`; the Settings
    renderer is separate. Sharing requires extracting a common
    "render a *State* + *Layout* + *Colors*" shape, which the
    `view/controls/*` modules already have.

### 2.3 Open architectural questions

* **`Spec::SetSpec` mutator** vs **per-field mutators**. Currently
  field mutators cover `SetValue`/`SetChecked`/`SetSelectedIndex`/
  `SetItems`. For richer subtree changes — e.g. a toolbar that grows
  a button — the choice is: add `SetSpec { widget_key, sub_spec }`
  (clean) or add more per-field mutators (incrementally simpler).
  Recommendation: ship `SetSpec` next — generic enough that future
  changes don't keep adding variants.
* **Cursor focus on click.** Click-to-focus moves the focus key to
  the clicked widget *and* fires the click event. Mouse drag /
  hover / double-click are not yet plumbed. The `Layer` work
  (§7 in this doc / §3.5 in the original rev 2) absorbs this.
* **Re-render-on-buffer-resize.** Flex spacers size against
  `widget_panel_width(buffer_id)`. When the buffer's split resizes,
  we don't currently re-render — the plugin gets a `resize` event
  and is expected to call `updateWidgetPanel`. A future improvement
  is for the host to re-render automatically when `viewport.width`
  changes for any buffer with a mounted widget panel.
* **The "Spec is initial; instance state is the truth" rule.**
  Implemented for `TextInput` (value+cursor) and `List` (selected_index +
  scroll_offset). The rule will need to extend to `Tree` (expanded
  keys), `TextArea` (cursor + selection + scroll), `Prompt` / `Layer`
  (open/closed). Pattern is set; just apply it consistently as new
  widgets land.
* **Widget keymap layer above `defineMode`.** Today the plugin's
  `defineMode` binds keys → `dispatch(widgetKey("Tab"))`. The §8
  design said the widget's keymap should claim keys *before*
  `defineMode` sees them. We did the inverse: the plugin opts in by
  binding to widget commands. That's pragmatic for migration but
  means *every plugin* repeats the same binding table. A
  `defineMode` extension or registry of "panel has a widget
  runtime" + "widget keymaps register here" would let plugins skip
  the boilerplate.

---

## 3. Onboarding: how to pick up this work

### 3.1 Build + run

Standard fresh checkout. The widget runtime is part of `fresh-editor`:

```bash
cargo build -p fresh-editor --bin fresh
cargo test -p fresh-editor --lib widgets    # 70 unit tests
crates/fresh-editor/plugins/check-types.sh  # tsc on plugins
```

After modifying the Rust API or types in `fresh-core/src/api.rs`,
regenerate `fresh.d.ts`:

```bash
cargo test -p fresh-plugin-runtime write_fresh_dts_file -- --ignored
```

### 3.2 Interactive tmux verification

The spec says "verify in tmux." This is real — the rendering pipeline
has subtleties (cursor placement, focus styling, mouse routing) that
unit tests miss. Recipe:

```bash
# Set up a fixture
mkdir -p /tmp/sr-test && cd /tmp/sr-test
echo -e "hello world\nhello again" > a.txt
git init -q . && git add -A
git -c user.email=t@t -c user.name=t -c commit.gpgsign=false commit -q -m init

# Launch in tmux
tmux new-session -d -s sr -x 160 -y 40 -c /tmp/sr-test \
  "/path/to/fresh/target/debug/fresh a.txt"
sleep 2
tmux send-keys -t sr 'C-p'                     # command palette
sleep 1
tmux send-keys -t sr 'Search and Replace'
sleep 1
tmux send-keys -t sr Enter
# … drive keys …
tmux capture-pane -t sr -p              # rendered text
tmux capture-pane -t sr -p -e           # rendered text + ANSI escapes
tmux display-message -t sr -p '#{cursor_x},#{cursor_y} flag=#{cursor_flag}'
```

`cursor_flag=0` means the hardware cursor is hidden (TextInput not
focused); `flag=1` means it's visible. `capture-pane -e` is essential
for verifying overlay colors / focused-bg styling — plain
`capture-pane` strips them.

### 3.3 The "minimum dignity" recipe for adding a new widget kind

For `Tree`, `Tabs`, `TextArea`, `Table` etc. The path through the
codebase is mechanical at this point.

1. **Add a `WidgetSpec::<Kind>` variant** in
   `crates/fresh-core/src/api.rs` next to `Toggle`/`Button`/etc.
   Fields are spec-only (initial values) — instance state goes in
   `WidgetInstanceState` (step 4). Stable `key: Option<String>` is
   required for any widget that owns instance state.
2. **Add a renderer** in `crates/fresh-editor/src/widgets/render.rs`
   (`render_<kind>` plus a match arm in `render_collected`). Output:
   one or more `TextPropertyEntry`s and zero or more `HitArea`s.
   Container-shifting (Row inline-collapse / Col row offset) is
   handled by the surrounding code; just emit relative coordinates.
3. **Add tabbable membership** in `collect_tabbable` if the widget
   takes focus. Add focus-styling override in the widget arm — the
   pattern is `let is_focused = match key.as_deref() { Some(k) if
   !k.is_empty() => k == focus_key, _ => *focused };` and then pass
   `is_focused` to your renderer.
4. **Add instance state** in
   `crates/fresh-editor/src/widgets/registry.rs` (`WidgetInstanceState`
   enum). Read from `prev` map by key; write to `next_state`. The
   `TextInput` and `List` arms in `render_collected` are the
   templates.
5. **Add a TS builder** in
   `crates/fresh-editor/plugins/lib/widgets.ts`. Re-export from
   `index.ts`. Regenerate `fresh.d.ts`.
6. **Add a `WidgetCommand::Key` arm** in
   `crates/fresh-editor/src/app/plugin_dispatch.rs` (`handle_widget_key`)
   if the widget responds to keystrokes. Existing dispatch table:
   Tab → focus advance; Up/Down → list select; Backspace/etc. →
   text input; Enter/Space → activate. Add per-kind handling.
7. **Add a mutator** in `WidgetMutation` if the plugin needs a
   targeted fast-path update (e.g. `Tree` would want
   `SetExpandedKeys { widget_key, expanded_keys: Vec<String> }`).
   Wire through `handle_widget_mutate` and
   `widgets::set_<thing>_in_spec` helper.
8. **Add unit tests** in `widgets/render.rs` (`tests` module). Test
   render output shape, hit areas, focus styling, instance state
   round-trip. The `text_input_*` and `list_*` tests are templates.
9. **Migrate a plugin** to use it. Find a plugin that hand-rolls
   the equivalent (the catalogue in §5 lists them); convert one
   call-site. Verify in tmux.

Each step is a few dozen LOC at most. The work scales with the
widget kinds, not with the dispatch / state-management plumbing —
those are done.

### 3.4 Common gotchas

* **`MountWidgetPanel` resets instance state.** Plugin re-mounting
  the same panel id starts fresh. Use `UpdateWidgetPanel` to
  preserve instance state across renders. `WidgetPanel.set()` does
  the right thing automatically (mount on first call, update after).
* **Spec value vs instance state.** For `TextInput` value + cursor
  and `List` selected_index + scroll_offset, instance state is the
  truth after first render. The spec's value is initial-only.
  Plugin updates via `widget_event` or via `WidgetMutate::SetValue`
  / `SetSelectedIndex`. Setting them in the spec on every render is
  fine — they're ignored once instance state exists, except via the
  re-mount path. Don't rely on spec value for round-trip.
* **Newlines in entries.** Every entry pushed at the top level / Col
  level needs to end with `\n`. Row inline-collapse strips trailing
  `\n` from inline children before merging and re-adds one at the
  end of the merged row. Without this, adjacent widget entries
  concatenate into one logical buffer line. The renderer takes care
  of this if you go through it; if you push entries directly (e.g.
  in a `Raw` migration shim) make sure they have trailing `\n`.
* **Focus key clamping.** The renderer clamps the previous focus key
  to a tabbable that exists in the new spec. If the widget you were
  focused on disappears, focus falls back to the first tabbable.
* **Hardware cursor.** When a `TextInput` is focused, the host sets
  the buffer's `show_cursors=true` and positions the primary cursor
  to the byte the renderer emitted in `RenderOutput::focus_cursor`.
  When focus is on a non-text widget, `show_cursors=false` and the
  hardware cursor disappears entirely. Don't paint a cursor overlay
  in the renderer — let the terminal blink the real one.
* **Width calculation.** `widget_panel_width()` returns
  `viewport.width - 2` for gutter/scrollbar/border slack. Your
  widget can use the full result via `panel_width` parameter; flex
  Spacers consume any leftover. If your widget naturally takes a
  fixed width (Toggle = `[v] label.len()`, Button = `[ label ].len()`),
  the renderer accounts for it in flex distribution.
* **Concurrent keystrokes.** Multiple `WidgetCommand` events can
  queue in one editor tick before the plugin processes any
  `widget_event`. Read state from instance state, not from the spec
  field, to avoid the race that bit the original "renderer reads
  spec value" design (commit `0922463` is the fix).
* **`tmux capture-pane` doesn't show colors.** Use `-e` to dump ANSI
  escapes, or `display-message -p '#{cursor_x},#{cursor_y}'` for the
  hardware cursor. Theme keys resolve at render time; capture-pane
  output reflects the real terminal output.
* **`#[cfg(test)]` test compilation.** When you add a new
  `WidgetInstanceState` variant or a new `WidgetSpec` variant, the
  test fixtures need updating (`make_list` in `render.rs`, struct
  literals scattered across test functions). The compiler will tell
  you all the call sites.

---

## 4. Roadmap: what to build next, in order

Each item is roughly one to two PRs. Most build on the existing
plumbing; only items 5 and 7 are major architectural lifts.

### 4.1 `Tree` widget — top user value

**Plugins waiting**: `search_replace.ts` (file → matches),
`audit_mode.ts` (files → hunks), file-explorer, find_references.

**Spec shape**: tracks the v1 catalogue.

```rust
WidgetSpec::Tree {
    nodes: Vec<TreeNode>,         // flat list with depth + parent key
    item_keys: Vec<String>,       // parallel
    selected_index: i32,           // initial-only
    visible_rows: u32,
    expanded_keys: Vec<String>,    // initial-only; instance state takes over
    key: Option<String>,
}

struct TreeNode {
    text: TextPropertyEntry,       // pre-rendered row
    depth: u32,                    // for indent
    has_children: bool,            // disclosure-glyph hit area
}
```

**Instance state**: `WidgetInstanceState::Tree { scroll_offset,
selected_index, expanded_keys: HashSet<String> }`.

**Renderer**:
* Compute visible flat list = nodes filtered by which expanded_keys
  cover them (parent must be expanded).
* For each visible row, emit a `TextPropertyEntry` with depth
  indent + disclosure glyph (`▶` / `▼`) + node.text.
* Selected row gets `ui.menu_active_bg` extend_to_line_end.
* HitArea per row: clicking the disclosure column fires `expand`,
  clicking the row fires `select`. (Two HitAreas per row, narrow byte
  ranges.)

**Smart-key dispatch additions**:
* `Right` → expand currently-selected node
* `Left` → collapse, or move to parent if already collapsed
* `Up`/`Down` → select prev/next visible (existing select-move
  generalizes)

**Mutators**: `SetExpandedKeys { widget_key, keys }` for plugins that
want to toggle expansion without re-emitting.

**Migration**: `search_replace.ts`'s `buildFlatItems` already returns
the right shape; convert to `tree(...)` and the plugin's existing
expand-on-Enter handler becomes redundant (host owns expansion).

**Estimated effort**: 1-2 days. Mostly mechanical follow of §3.3
recipe.

### 4.2 `WidgetMutation::SetSpec { widget_key, sub_spec }`

Replaces a named subtree without re-emitting the whole panel. Lets
a plugin update e.g. a toolbar's child set without re-transmitting
the surrounding `Col`.

Implementation: tree walk + replace by key in the host's
`WidgetPanelState::spec`. One ~20 LOC helper in `widgets/actions.rs`,
one `WidgetMutation` variant, one TS builder.

### 4.3 `Tabs` / `Group` widget

`git_log.ts` has tab-style buffer-group panels. The widget version is
a horizontal Row of buttons that switches the body's visible
sub-spec. Pairs with `SetSpec` (above) for swapping the body.

### 4.4 `TextArea` widget

Multi-line cousin of `TextInput`. Adds:
* Vertical scroll instance state (extends the focus-cursor to handle
  scrolled content — the `RenderOutput::focus_cursor.buffer_row` may
  be larger than `visible_rows`; renderer clamps and reports the
  visible row).
* Submit policy: per the §8 terminal constraint, `singleSubmitsOnEnter`
  vs `altEnter` (default). The widget renders the chosen submit key
  in its own footer (or relies on the panel's HintBar).

Plugins waiting: composer-style plugins.

### 4.5 Role-based theming

Today widgets pick theme keys themselves (constants in
`widgets/render.rs`). Move to a `Role` enum + a `Role → theme key`
mapping table:

```rust
pub enum Role {
    HelpKey, ToggleOn, FocusedFg, FocusedBg, DangerFg, InputBg,
    PlaceholderFg, ListSelectedBg, …
}

fn role_to_theme_key(role: Role, theme_overrides: &Option<HashMap<...>>) -> &str { … }
```

Plus a per-spec `theme: Option<HashMap<Role, OverlayColorSpec>>` that
plugins can pass to override individual roles. The translation lives
in one place (renderer); accessibility variants (high-contrast,
color-blind) drop in by changing the role-resolution table without
touching plugin code.

### 4.6 Compositor / `Layer` (the big one)

This is §7 of this doc / §3.5 of rev 2. Unifies `Popup`, `Prompt`,
`showActionPopup`, hover tooltips, completion popup, plugin-mounted
modals/tooltips/context-menus into one `Component` trait + Z-ordered
stack + `mountLayer` IPC. Subsumes a lot of duplicated focus / dismiss
/ event-routing logic. Touches a lot of files. Worth a dedicated
multi-PR effort.

Key invariants to preserve during migration:
* `editor.startPrompt`, `editor.showActionPopup` keep working —
  become thin wrappers around `mountLayer`.
* The dispatch order from §8: global resolver → active widget keymap
  → active panel mode bindings → buffer/normal-mode bindings.
* The compositor's hit-test extends the dispatcher proposed in
  `event-dispatch-architecture.md` Phase 2; if that's not in tree
  yet, it lands first.

### 4.7 Spec-as-state persistence

§10 tells the full story. Concretely:

* Per-workspace `state.json` gains a `widget_panels: { [panel_id]:
  { spec, instance_states, focus_key } }` section. Persisted on
  panel update; loaded on workspace open.
* A new `editor_init` step iterates persisted panels: emits the
  stored spec to whichever plugin "owns" it (the plugin's `init.ts`
  can opt in by registering a panel-id → handler mapping).
* Theme switching: on `theme_changed`, host iterates
  `widget_registry.panel_ids()` and calls `rerender_widget_panel`
  for each. Plugin not involved.
* Replay capture: `--record-spec-stream` flag dumps every
  Mount/Update/Mutate/WidgetEvent to a JSONL file. A `replay-spec`
  binary feeds the file to a stub plugin and snapshots the
  rendered output.

Headless rendering and `embed` cross-plugin composition both fall
out of "Spec is data" once the persistence layer exists.

### 4.8 Accessibility

* `lib-widgets.i18n.json` for default labels (`Confirm`, `Cancel`,
  `Toggle`, …) translatable independent of plugins.
* `aria` string per widget, emitted on focus change.
* OSC 52 / IDE bridge: widget focus changes route through
  `view/accessibility.rs` (new) which already serializes selection
  for clipboard.
* Motion-reduce: gate the two library animations (focus-flash,
  hover-fade) on `theme.accessibility.reduce_motion`.

### 4.9 Plugin migrations beyond `search_replace.ts`

The heaviest payoff order, per call-site density:
* `git_log.ts` — Toolbar + Tabs + Table.
* `lib/finder.ts` — already a panel manager; convert to `List` +
  `Prompt` (after Layer lands).
* `audit_mode.ts` — Tree + List + RawBuffer escape hatch.
* `dashboard.ts` — Toolbar + List.
* `theme_editor.ts` — settings-style controls.
* `pkg.ts` — what `claude/design-plugin-ui-library-pxri8` started
  with; the `// TODO: Plugin UI Component Library` literal.

Each plugin migration is mostly mechanical once the widgets it needs
exist. The work is in (a) discovering hidden assumptions in plugin
state machines (e.g. `search_replace`'s `focusPanel`/`queryField`/
`optionIndex` triple), and (b) reconciling event flow with whatever
async work the plugin already does (debounce, LSP, git).

### 4.10 Settings adoption

§11 says shared renderers. The shape today is
`widgets/render.rs::render_*` for plugin widgets, separate
`view/controls/*::render_*` for Settings. Sharing requires
extracting a common `(State, Layout, Colors) → TextPropertyEntry`
shape; both already have it. The work is moving the renderers to
a common location (probably `view/controls/`) and having
`widgets/render.rs` call them. This is purely refactoring; no new
behavior. Defer until role-based theming (4.5) lands; without it
the shared renderers would still pick theme keys in different ways.

---

## 5. Widget catalogue

Updated from the rev 2 catalogue. **Status** column: ✅ shipped,
🚧 partial, ❌ not yet, ⏸ deferred.

| Widget | Status | Used by | Notes |
|---|---|---|---|
| `Row` / `Col` | ✅ | layout primitives | flex Spacer fills remaining width |
| `Spacer` (fixed + flex) | ✅ | layout | |
| `Raw` | ✅ | migration escape hatch | wraps `TextPropertyEntry[]` |
| `HintBar` | ✅ migrated | every plugin's footer | `parseHintString` for legacy `Tab:foo  Esc:bar` strings |
| `Toggle` / `Checkbox` | ✅ migrated | search_replace toggles | `[v]`/`[ ]` glyph + label |
| `Button` | ✅ migrated | search_replace Replace All | `intent: "normal" \| "primary" \| "danger"` |
| `TextInput` | ✅ migrated | search_replace fields | host-owned cursor + value, constant-width with scroll, hardware caret |
| `List` (virtual-scrolled) | ✅ migrated | search_replace match list | host owns scroll + selection |
| `Tree` | ❌ → 4.1 | search_replace tree, audit, file-explorer | next priority |
| `Tabs` / `Group` | ❌ → 4.3 | git_log buffer group, settings categories | |
| `TextArea` | ❌ → 4.4 | composer plugins | |
| `Layer` (compositor) | ❌ → 4.6 | tooltips, popovers, modals; subsumes Popup/Prompt | big architectural piece |
| `Prompt` | ❌ → 4.6 | finder, every confirm | built on Layer |
| `Transient` (Magit) | ❌ → 4.6 | discoverability | one of the Layer kinds |
| `Table` | ❌ | git_log, find_references, audit | |
| `Toolbar` | ❌ → 4.3 | git_log, audit_mode | composes Button + Toggle |
| `Panel` | ⏸ | every panelled plugin | currently unbuilt as a widget; today's `Col` does the job |
| `KeybindingList`, `MapInput` | ⏸ | mirrors of Settings widgets | low priority |
| `Diagnostic` / `InlineHint` | ⏸ | LSP plugins | |
| `ProgressBar`, `Spinner` | ⏸ | indexer plugins | |
| `Dropdown` | ⏸ | Settings | |

The catalogue stays short by design. Anything not on it lives inside
a `Raw` widget — the imperative-virtual-buffer escape hatch.

---

## 6. Layout primitive

**Line-oriented flex along the row axis, absolute along the column
axis, with a small Rect-based composition layer.** Three reasons:

1. The terminal is row-major. Every plugin already thinks in rows.
2. The interesting layout question is column distribution: a
   `Toolbar` packs left-to-right, a `Panel`'s body fills, a
   `HintBar` packs right-to-left. That's `flex-row` with `grow/shrink`
   on children.
3. Terminal-line-wrap (toolbars must not wrap) is solved by clipping
   widgets, not line-wrapping them.

What's actually shipped:

```ts
// In TS (plugins/lib/widgets.ts)
type WidgetSpec =
  | { kind: "row"; children: WidgetSpec[]; key?: string }
  | { kind: "col"; children: WidgetSpec[]; key?: string }
  | { kind: "spacer"; cols: number; flex: boolean; key?: string }
  | { kind: "hintBar"; entries: HintEntry[]; key?: string }
  | { kind: "toggle"; checked: boolean; label: string; focused: boolean; key?: string }
  | { kind: "button"; label: string; focused: boolean; intent: ButtonKind; key?: string }
  | { kind: "textInput"; value: string; cursorByte: number; focused: boolean; label?: string;
        placeholder?: string | null; maxVisibleChars: number; fieldWidth: number; key?: string }
  | { kind: "list"; items: TextPropertyEntry[]; itemKeys: string[];
        selectedIndex: number; visibleRows: number; key?: string }
  | { kind: "raw"; entries: TextPropertyEntry[]; key?: string };
```

Row layout works in two passes — see `render_collected` in
`widgets/render.rs`. The flex distribution is `panel_width -
sum(non-flex widths)` split evenly across flex spacers.

Not yet shipped: `fill`, `fixed`, `wrap: "never" | "soft"`, and the
`embed` composition primitive. These are in §6 of the rev 2 design;
add them when a plugin needs them.

---

## 7. Compositor: layered Components

(rev 2 §3.5, unchanged, partially blocked on `event-dispatch-architecture.md`
Phase 2.)

Today the editor has half a dozen overlapping subsystems for "thing
that paints over content": `Popup` (`view/popup.rs`), `Prompt`
(`view/prompt.rs`), `showActionPopup`, the buffer-group panel
renderer, hover tooltips, completion popups. Each has its own focus
stack, dismiss policy, mouse routing, and keymap precedence.

**Unify them as layers in a single Compositor**, modelled on Helix's
`Component` trait, adapted for IPC:

```rust
trait Component {
    fn render(&mut self, area: Rect, surface: &mut Surface, ctx: &mut Ctx);
    fn handle_event(&mut self, event: &Event, ctx: &mut Ctx) -> EventResult;
    fn cursor(&self, area: Rect, ctx: &Ctx) -> (Option<Position>, CursorKind);
    fn required_size(&mut self, viewport: (u16, u16)) -> Option<(u16, u16)>;
    fn dismiss_policy(&self) -> DismissPolicy;
    fn id(&self) -> ComponentId;
}
```

The Compositor owns a Z-ordered stack. Events bubble front-to-back
until one returns `Consumed`. Plugin-facing surface:

```ts
const tooltip = editor.mountLayer({
  kind: "tooltip",                 // "tooltip" | "popover" | "modal" | "panel"
  anchor: { widget: "matchTree", row: hoveredRow },
  body: { kind: "widget", type: "InfoCard", props: { ... } },
  dismissOn: ["hover-out", "blur"],
});
```

UX wins this enables (none reachable in a TS-only design):

* Hover tooltips become a child layer, not a per-widget feature.
* `Button.kind = "danger"` confirm spawns a `Layer { kind: "modal",
  body: { type: "Prompt" } }` — no separate modal-dialog API.
* Right-click context menus are `Layer { kind: "popover", body: {
  type: "List" } }`. Plugins do not re-implement context menus.
* A `Prompt` mounted from inside a panel is the *same* Component as
  the top-level command palette.

**Files to add when this work starts**: `crates/fresh-editor/src/compositor/`
with the trait, the stack, the dispatcher, and the `mountLayer`
binding. `view/popup.rs` / `view/prompt.rs` / `view/hover.rs`
migrate to be `Component` implementations in successive PRs.

---

## 8. Focus / keyboard model

A **panel-level focus stack** with one Tab cycle per panel, computed
from the spec's tabbable widget keys in declaration order. Each
panel has a single active widget; the host paints focus styling.

**Implemented**: `collect_tabbable` walks the spec; `focus_key`
lives in `WidgetPanelState`; `WidgetCommand::FocusAdvance { delta }`
cycles. The smart-key dispatch (`WidgetCommand::Key { key }`) routes
keystrokes to the right action based on the focused widget's kind.

**Dispatch order today** (one direction off from the rev 2 design):
1. Plugin's `defineMode` bindings (the plugin opts in by binding
   keys to `dispatch(widgetKey("Tab"))` etc.)
2. The smart-key dispatcher in `handle_widget_key`, which routes to
   `handle_widget_focus_advance` / `handle_widget_activate` /
   `handle_widget_select_move` / `handle_widget_text_input_*`.

**Dispatch order rev 2 wanted**:
1. Global resolver
2. Active widget's built-in keymap
3. Active panel's `defineMode` bindings
4. Buffer/normal-mode bindings

The "widget keymap claims keys before plugin keymaps see them"
inversion is open. Pragmatic for migration today (plugins explicitly
bind), but every plugin repeats the binding table. A
`defineMode` extension that registers a "panel has a widget runtime"
shortcut would let plugins skip the boilerplate.

### Terminal constraint

Shift+Enter ≡ Enter at the terminal, Shift+Alt+Enter ≡ Alt+Enter.
We do not bind Shift+Enter as a distinct key. `TextArea` (when
shipped) submit defaults to Alt+Enter; the chosen key string shows
in the panel's HintBar.

---

## 9. Mouse model

The host owns hit-testing. The plugin never sees `(buffer_row,
buffer_col)`; it receives semantic events.

**Implemented**:
* Each widget instance produces a `HitArea { widget_key,
  widget_kind, buffer_row, byte_start, byte_end, payload, event_type
  }` during render. Stored in `WidgetPanelState::hits`.
* `WidgetRegistry::hit_test(buffer_id, row, col_byte)` does the
  per-panel scan.
* `click_handlers.rs` calls `hit_test` for every left-click on a
  widget panel's buffer; on hit, fires `widget_event` with the
  payload, and moves focus_key to the clicked widget.
* `widget_event` payloads: Toggle → `{ checked: <new> }`; Button →
  `{}`; List → `{ index, key }`; TextInput → `{ value, cursorByte }`.

**Not yet implemented**:
* Right-click → context menu (`onContext` in the rev 2 design).
* Drag (`onPress` / `onDrag` / `onRelease`).
* Hover (`onHover(true|false)`). Important for the Layer tooltip
  flow.
* Double-click → `onActivate(key)`. Today single-click fires
  `select`; double-click would fire `activate` separately.
* Wheel scroll routed to deepest scrollable widget. Today the
  editor's scroll handling sees the wheel events; widget scroll
  doesn't intercept.

---

## 10. State model

**Reactive on the Rust side, declarative on the TS side.** Plugin
re-emits a `WidgetSpec` whenever its model changes; host runs a
keyed reconciler against the previous spec for that panel and
applies a minimal patch.

**Implemented**:
* Spec/instance separation: `WidgetInstanceState` holds host-owned
  state per widget key (TextInput value+cursor, List
  scroll+selection). The spec carries initial values; instance
  state is the truth after first render.
* Stable `key` round-trip: re-emitting the spec preserves instance
  state by key.
* Re-render after host-side state changes: `rerender_widget_panel`
  walks the spec + current instance state without plugin
  involvement. Used by focus advance, select move, text-input
  mutation, and toggle/items mutators.
* The targeted-mutator fast path: `WidgetMutate::SetValue` /
  `SetChecked` / `SetSelectedIndex` / `SetItems` — the IPC fast
  path discussed in §3 of the user Q&A. Plugin ships a one-field
  change instead of the full spec.

**Not yet implemented (rev 2 §6)**:
* Session restore (4.7).
* Live theme switching (4.7).
* Replay (`--record-spec-stream`) (4.7).
* Headless rendering (falls out of "Spec is data" + the renderer
  being a pure function; the test harness already calls
  `render_spec` directly).
* Cross-plugin composition (`embed` widget kind) (4.7).
* Versioning (`spec.version: 1`) — unused since v1 only.
* Fault isolation: today a panicking renderer for one widget kind
  takes down the whole panel render. The reconciler would need to
  catch_unwind around per-widget `render_<kind>` calls, paint a
  placeholder, log a `RenderError` event.

---

## 11. Theming

Widgets carry **roles**, never colors. (rev 2 §7 — partly
implemented.)

**Implemented**:
* `Button.intent: "normal" | "primary" | "danger"` — the only
  user-visible role today.
* Renderer's theme keys are constants in `widgets/render.rs`
  (`KEY_HELP_KEY_FG`, `KEY_TOGGLE_ON_FG`, etc.). One place to
  override for accessibility variants, but no plugin override yet.

**Not yet implemented**:
* Per-spec `theme: { Role → OverlayColorSpec }` override map.
* High-contrast / color-blind variant resolution path.
* Role enum with three-level cap (e.g. `Button.danger.hover.fg`).

The path forward is item 4.5 in the roadmap.

---

## 12. i18n

Per-plugin `*.i18n.json` (`docs/i18n.md`) stays the authority.
Library defaults (`Confirm`, `Cancel`, `Toggle`, …) live in
`lib-widgets.i18n.json` (not yet created). `parseHintString` already
handles the existing per-plugin help strings.

---

## 13. Accessibility

Required for v1 (per rev 2 §9):

* High-contrast themes (blocked on role-based theming).
* Configurable keybindings via `keybindings.json` against
  `KeybindingResolver` (already works for the existing widget
  commands once the plugin binds them).
* Screen-reader output via OSC 52 / IDE bridges (not implemented).
* Motion-reduction: gates the library's two animations
  (focus-flash, hover-fade) — neither is shipped yet, so this is
  ready to add when they are.

Nice-to-have (deferred):
* Full ARIA-tree model (parent/child/level-of). v1 ships flat
  live-region announcements per focus change.
* Live-region throttling (one announcement per 100 ms).

---

## 14. Migration plan: `search_replace.ts`

Status of the rev 2 5-pass plan:

| Pass | Description | Status |
|---|---|---|
| 1 | Mount as `Panel`, body stays `Raw`, HintBar real, toggles real | ✅ commits 3257fa7 (HintBar), 7ddfb05 (toggles + button) |
| 2 | Replace search/replace fields with `TextInput` | ✅ commit 7ed8276 (initial render-only); 5325ea3 (host owns hardware cursor + constant width) |
| 3 | Replace match list with `Tree` | 🚧 `List` shipped (commit 7a5a7fd); `Tree` not yet (roadmap 4.1). User-visible regression: Right-arrow-to-expand and click-on-disclosure-glyph don't work; `Enter` on a file row toggles expanded as a fallback. |
| 4 | Glob filter as `TextInput` with validator | ❌ |
| 5 | Delete dead code | 🚧 `buildFieldDisplay`, `addCursorOverlay`, the cursor-byte arithmetic, the focus enums, ~12 hand-rolled mode handlers all gone. Remaining dead: `panel.scrollOffset`, `panel.focusPanel`/`queryField`/`optionIndex` (legacy fields kept for the Raw separator path). |

Net diff so far: ~600 LOC removed from `search_replace.ts`, ~80 LOC
of widget builders added. The plugin's `defineMode` table shrank
from per-key handlers to twelve one-liner `dispatch(widgetKey("..."))`
forwarders.

---

## 15. Shipped commit map

| Commit | What it landed | Files |
|---|---|---|
| `3257fa7` | Foundation: `WidgetSpec` enum, MountWidgetPanel/UpdateWidgetPanel/UnmountWidgetPanel IPC, render_spec, registry, plugins/lib/widgets.ts, search_replace HintBar. | api.rs, hooks.rs, plugin_dispatch.rs, widgets/{mod,registry,render}.rs, widgets.ts, fresh.d.ts |
| `7ddfb05` | Toggle, Button, Spacer + search_replace options row. | api.rs, render.rs, widgets.ts, search_replace.ts |
| `2c87651` | Hit-test + widget_event firing on click. | render.rs (HitArea), registry.rs (hit_test), click_handlers.rs |
| `7ed8276` | TextInput widget (render-only, plugin owned cursor). | api.rs, render.rs, widgets.ts, search_replace.ts |
| `7a5a7fd` | List with widget-owned virtual scrolling; instance-state mechanism. | api.rs, registry.rs, render.rs, widgets.ts, search_replace.ts |
| `b9c415b` | Focus management (tabbable collection, focus_key) + WidgetCommand IPC dispatch. | api.rs, render.rs, plugin_dispatch.rs, widgets/actions.rs (new) |
| `53ee226` | Smart-Key dispatch + search_replace mode-binding migration. | api.rs, plugin_dispatch.rs, search_replace.ts |
| `0922463` | Fix concurrent-keystroke race: host-owned TextInput value + List selection. | registry.rs, render.rs, plugin_dispatch.rs |
| `b7baeee` | Flex Spacer + panel-width-aware layout. | api.rs, render.rs, widgets.ts, search_replace.ts |
| `aaed59e` | cargo fmt + clippy clean. | various |
| `5325ea3` | Host owns hardware cursor + constant-width TextInput (`fieldWidth`). | api.rs, render.rs (FocusCursor), plugin_dispatch.rs (apply_widget_focus_cursor), widgets.ts |
| `a436874` | Targeted mutators (Path A): WidgetMutation IPC, setValue/setChecked/setSelectedIndex/setItems. | api.rs, plugin_dispatch.rs, widgets/actions.rs, widgets.ts, search_replace.ts |

Test count: 70 widget unit tests, all green.

---

## 16. Prior art — what we steal, what we reject

| System | Steal | Reject | Why |
|---|---|---|---|
| **VS Code TreeView** | Declarative `TreeDataProvider` shape: plugin returns data, host owns hit-test, virtualization, focus | Webview as a generic UI escape hatch | Webviews break the sandbox premise; TreeView's declarative shape is exactly the v1 widget-spec model |
| **Helix `Component` trait** | Layered z-ordered components; bubble-up `Consumed`/`Ignored`; host-owned `cursor()` and `required_size()` | Synchronous Rust trait across FFI | Translation: TS handlers are async; `Ignored` is the IPC default |
| **nui.nvim** | Widget = "buffer + keymap + lifecycle (mount/unmount)" | "No widget library" stance | Sandboxed JS plus opinionated widgets is a better default than asking plugin authors to roll their own |
| **Sublime minihtml** | `on_navigate` href dispatch as the safe link primitive (already analogous to `mouse_click`) | HTML/CSS layout subset; no keyboard focus | We need real keyboard widgets, and CSS-flow on a terminal is the wrong fit |
| **Emacs widget.el** | Nothing | The whole library | Resists composition, imperative-by-side-effect — exactly what we'd reproduce by exposing today's `setVirtualBufferContent` as the only model |
| **Magit transient.el** | Grouped key→command menu as a first-class widget | Lisp-y EIEIO subclassing | A `Transient` widget covers `git_log` and unblocks discoverability per `plugin-usability-review.md` |

---

## 17. Risks

| Risk | Mitigation |
|---|---|
| Reconciler complexity grows past what one engineer can hold | Keep Spec flat (no nested per-widget keys beyond `key: string`); cap recursion depth; ship the dirtiest plugin (`search_replace.ts`) as the regression test for every reconciler change |
| Per-keystroke event IPC dominates if plugins re-emit Spec on every keystroke | Document the rule: in `widget_event "change"`, never call `updateWidgetPanel` unless the rest of the spec actually changed. Use mutators (`SetValue`/`SetChecked`/`SetItems`) for hot-path. The lint is "panel.update calls per second"; expose it on the dev HUD |
| Capability creep through widget callbacks | Widgets only emit *events* the plugin can already subscribe to. Code review checklist: a new widget MUST NOT introduce a new `PluginCommand`-equivalent capability |
| Theme role explosion (`Button.danger.hover.fg`...) | Cap the role tree at three levels; review additions in PRs that touch `theme/types.rs` |
| Reach: Settings doesn't actually adopt the widget tree | Keep the *renderers* shared (item 4.10) and the *Spec* shape compatible. Settings can stay on its current direct calls indefinitely |
| Plugin author confusion: Spec vs imperative vs mutators | One way per use-case in the docs. `Raw` exists for *escape hatches*, not for rendering rich UI. Mutators are for hot-path single-field updates |
| Terminal-constraint violations (Shift+Enter etc.) | Static lint in TS: any `keys` string in a `HintBar` or `Transient` matching `^Shift\+(Enter\|Alt\+Enter)` is a build error |
| Drift from `event-dispatch-architecture` Phase 2 / `unified-keybinding-resolution` / `unified-hit-test-theme-plan` | This proposal builds on them. The Compositor migration (4.6) blocks until Phase 2 lands |

---

## 18. Order of landing — updated

1. ✅ ~~event-dispatch Phase 2 hit-test dispatcher~~ — bypassed for v1; the widget runtime owns its own hit-test against `WidgetRegistry::hits`. The general dispatcher is still desirable for the Layer compositor (4.6).
2. ✅ ~~`unified-hit-test-theme-plan.md` `region_at` extension~~ — same; bypassed.
3. ✅ ~~`unified-keybinding-resolution.md` collapse~~ — same; bypassed. Plugin's `defineMode` already routes through the existing resolver; widget commands are explicit handler dispatches.
4. ✅ `crates/fresh-editor/src/widgets/{mod,registry,render,actions}.rs`.
5. ✅ `crates/fresh-core/src/api.rs` — Mount/Update/Unmount/WidgetCommand/WidgetMutate variants + WidgetSpec/WidgetAction/WidgetMutation types.
6. ✅ `crates/fresh-editor/plugins/lib/widgets.ts` — TS surface.
7. ✅ Search_replace migration through Pass 2; partial Pass 3 (List, no Tree).
8. → 4.1 Tree widget.
9. → 4.2 SetSpec mutator.
10. → 4.3-4.4 Tabs, TextArea.
11. → 4.5 Role-based theming.
12. → 4.6 Compositor / Layer.
13. → 4.7 Spec-as-state persistence.
14. → 4.8 Accessibility.
15. → 4.9 Plugin migrations beyond `search_replace.ts`.
16. → 4.10 Settings adoption (last; depends on 4.5).

---

## 19. Go / don't go

**Going.** Foundation shipped on this branch; one plugin migrated end-
to-end through the bulk of its UI. cargo check workspace clean,
70 widget tests passing, tsc clean, interactively verified in tmux.

The next maintainer's quickest path to value is §3.3's recipe applied
to `Tree` (4.1) — that's the single biggest user-visible win, and the
plumbing is fully in place for it. Then 4.2 (SetSpec) closes the
"plugin can mutate any subtree" gap, after which most plugin migrations
become mechanical.

The big architectural lift is 4.6 (Compositor / Layer). It's not
blocked on anything in tree; it's blocked on planning capacity.
Until it lands, plugins that want tooltips / modals / context menus
keep using `editor.startPrompt` / `editor.showActionPopup` / etc.,
which work fine but don't share dismiss/focus rules with widget
panels.

---

## Appendix A — Rejected: TS-only thin helper library

A parallel proposal exists on `claude/design-plugin-ui-library-pxri8`
(`docs/internal/plugin-ui-library-design.md`, 1,231 lines) that takes
the opposite shape: ~800 LOC of TypeScript helpers, one
`VirtualBufferBuilder`, a `TextInputState` + `TextInputRouter`
wrapping `mode_text_input`, a `FocusRing<T>` cycle helper, seven new
theme keys. **Zero new IPC.** Migrates `pkg.ts`,
`search_replace.ts`, `theme_editor.ts` in ~3 weeks.

It is a coherent v1 if shipping speed is the binding constraint.
It is the wrong end-state under the criterion stated at the top of
this document. Five UX/robustness/flexibility wins the TS-only shape
structurally cannot reach:

1. **Widget-internal keymap claimed before plugin keymaps see it.**
   `TextInput` consumes Backspace/arrows/Home/End uniformly across
   every plugin without each plugin registering them in its
   `defineMode`. (Partially shipped here; see §8 — host-side
   keymap-claim is the inversion still open.)
2. **Hit-testing owned by core.** Plugins emit semantic events
   (`onSelect(key)`, `onActivate(key)`, `onHover(key, true|false)`);
   they never see `(buffer_row, buffer_col)`. (Shipped — `WidgetRegistry::hit_test`.)
3. **Per-keystroke cost has the right asymptote.** Today's
   `setVirtualBufferContent` is full delete-all + insert-all + rebuild
   overlay tree (`virtual_buffers.rs:356–405`). With widget state
   Rust-side, a keystroke in a `TextInput` mutates Rust state and
   emits one semantic event back; if the plugin's model doesn't
   change, no re-render IPC fires at all. (Shipped — instance state
   plus targeted mutators.)
4. **Theme as roles, not colors.** The TS-only proposal adds 7 theme
   keys; plugins still pick which key to pass to which widget.
   Theme packs and accessibility variants only stay consistent when
   the role→key mapping is centralized in the renderer. (Partially
   shipped — `intent: "primary"|"danger"` is the only role today;
   see roadmap 4.5 for the rest.)
5. **Reach across built-in surfaces.** The Rust `view/controls/*`
   renderers paint plugin widgets too — Settings, file explorer,
   prompts, plugin panels share one render path. The TS-only proposal
   freezes the split forever (its §2.1 acknowledges and accepts the
   parallel TS stack). (Not shipped — see roadmap 4.10.)

Three further capabilities the TS-only design forecloses:

* **Layered compositor** (`Popup`/`Prompt`/`showActionPopup`/hover/
  modals/context-menus/completion under one dismiss-and-focus model)
  — see §7 / roadmap 4.6.
* **Spec as first-class state** (session restore, theme switch,
  deterministic replay, headless rendering, cross-plugin composition)
  — see §10 / roadmap 4.7. Spec is already data; the missing piece is
  persistence.
* **Fault isolation.** A panicking widget renderer in the TS-only
  design takes down the panel render. With Rust-side widget kinds,
  the reconciler can paint a placeholder for the offending subtree
  and keep going. (Not shipped here either — see §10.)

Where the TS-only proposal is right and we keep its discipline:

* Anchor every widget to a named plugin's hand-rolled code. No
  speculative widgets. (Followed — every shipped widget has a
  migrated plugin call-site.)
* Don't ship retained widget-handle APIs as the primary model
  (`button.setLabel(s)`). Spec/reconciler is declarative. (Followed —
  but `WidgetMutate::SetValue` etc. exist as bounded escape hatches
  for the hot path.)
* Reuse `mode_text_input` and `defineMode` for the imperative escape
  hatch. (Followed — plugin's `defineMode` is how it opts into widget
  key dispatch.)

**Net.** The TS-only proposal answers "what is the minimum useful help
in the next three weeks?" cleanly. It does not answer "what should
this library *be*?" Under the criterion stated at the top — end-state
UX, robustness, flexibility, with shipping speed deliberately not a
constraint — the maximalist version is the answer, and is what's in
tree on this branch.
