# Macro System Improvements: Persistable, Editable, Promotable Macros

Status: Design proposal
Audience: Fresh maintainers
Related code: `crates/fresh-editor/src/app/macros.rs`,
`crates/fresh-editor/src/app/macro_actions.rs`,
`crates/fresh-editor/src/init_script.rs`,
`crates/fresh-editor/src/input/keybindings.rs`,
`crates/fresh-editor/src/app/plugin_dispatch.rs`,
`crates/fresh-plugin-runtime/src/backend/quickjs_backend.rs`

## 1. Problem statement

Today a macro is a `Vec<Action>` recorded into a single-character register and
held only in memory (`MacroState` in `app/macros.rs`). When the editor closes,
every macro is gone. The user's request is three connected capabilities:

1. **Store macros in `init.ts`** so they survive restarts and live alongside
   the rest of the user's configuration.
2. **Edit the stored macros** — open them as text, tweak the steps, re-run.
3. **Improve a macro into arbitrary code logic** — start from a recorded
   sequence and graduate it into a real function with loops, conditionals, and
   the full plugin API, without rewriting from scratch.

The current system has none of these. `ShowMacro` already serialises a macro to
JSON in a scratch buffer whose header even claims "this buffer can be saved as a
.json file for persistence" — but there is **no load path**, so that promise is
empty. Macros are also completely invisible to the plugin/TS layer: there is no
`getMacro`, no `defineMacro`, no way for `init.ts` to read or register one.

This document proposes closing that loop by making the **recorded-macro world**
(register chars, `Action` enum, ephemeral) and the **`init.ts` world**
(persistent TS, editable, arbitrarily programmable) two ends of one pipeline.

## 2. Current architecture (what we build on)

The good news: every primitive we need already exists in isolation. The work is
wiring, not invention.

- **`Action` already round-trips to/from strings.** `define_action_str_mapping!`
  in `input/keybindings.rs` generates `Action::from_str(name, args)`,
  `Action::to_action_str()`, and `Action::all_action_names()` from one source of
  truth. A recorded `Vec<Action>` can therefore be rendered as a list of action
  names and parsed back losslessly — *as long as payloads are carried* (see the
  gap in §3).
- **`Action` derives `Serialize`/`Deserialize`.** `ShowMacro` already uses
  `serde_json::to_string_pretty` on the action slice.
- **`init.ts` is loaded as an ordinary plugin** named `init.ts`
  (`init_script.rs`), through the same pipeline as any other plugin. It has the
  full `EditorAPI`, hot-reload, a crash fuse, and `--safe` / `--no-init`
  escape hatches.
- **Plugins can already drive the editor by action name.**
  `editor.executeAction(name)` and `editor.executeActions(ActionSpec[])` exist;
  `ActionSpec` is `{ action: string, count: number }` and is dispatched in
  `plugin_dispatch.rs::handle_execute_actions`.
- **Plugins can register commands and key handlers.** `registerHandler`,
  `editor.registerCommand`, `editor.exportPluginApi` / `getPluginApi`, and
  `defineMode` keybindings are all in `fresh.d.ts`.

So the macro register layer and the scripting layer are two rooms with no door
between them. This proposal adds the door.

## 3. The one real gap: `ActionSpec` cannot carry payloads

`handle_execute_actions` (`plugin_dispatch.rs:2122`) calls:

```rust
Action::from_str(&action_spec.action, &HashMap::new())  // <-- always empty args
```

Because the args map is always empty, payload-carrying actions can't be
expressed via `executeActions`:

- `InsertChar(char)` needs a `char` arg,
- `PromptConfirmWithText(String)` needs the text,
- any future `with_char` / `custom` action in the mapping macro.

Recorded macros are *full* of these (every typed character is an `InsertChar`).
So today's `executeActions` can replay motions and commands but **not typed
text** — which makes it unsuitable as the export target for a real macro. This
is the single blocking issue, and fixing it is step one of the plan.

### Fix: optional `args` on `ActionSpec`

```rust
// fresh-core/src/api.rs
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[serde(deny_unknown_fields)]
pub struct ActionSpec {
    pub action: String,
    #[serde(default = "default_action_count")]
    pub count: u32,
    /// Action payload args (e.g. { "char": "x" }, { "text": "hello" }).
    /// Empty/absent for the common no-arg actions.
    #[serde(default)]
    pub args: HashMap<String, serde_json::Value>,
}
```

```rust
// plugin_dispatch.rs::handle_execute_actions
if let Some(action) = Action::from_str(&action_spec.action, &action_spec.args) {
    ...
}
```

This is backward compatible (`args` defaults to empty; existing callers and the
vi-mode plugin are unaffected) and makes `executeActions` a complete replay
target. The TS type updates automatically via `ts-rs`.

With this in place, `{ action: "insert_char", count: 1, args: { char: "x" } }`
replays correctly, and a recorded macro is fully expressible as TS.

## 4. The macro bridge API (Rust → TS surface)

Add a small, focused macro surface to `EditorAPI` (implemented in
`quickjs_backend.rs`, dispatched through new `PluginCommand` variants, served by
`MacroState`). Everything here is a thin wrapper over existing `MacroState`
methods plus the `to_action_str` / `from_str` conversions.

```ts
interface EditorAPI {
  /** Register keys of all in-memory macros, sorted. */
  listMacros(): string[];               // wraps MacroState::keys_sorted

  /**
   * The recorded steps of macro `register` as ActionSpecs, or null if none.
   * This is the editable, serialisable form — the same shape executeActions
   * consumes, so a macro is its own replay script.
   */
  getMacro(register: string): ActionSpec[] | null;

  /**
   * Define (or replace) an in-memory macro from a step list. Lets init.ts
   * seed registers at startup so `@q` works in a fresh session exactly as a
   * hand-recorded one would.
   */
  defineMacro(register: string, steps: ActionSpec[]): boolean;

  /** Play a macro by register (same as the PlayMacro action). */
  playMacro(register: string): boolean;
}
```

Note `getMacro` returns `ActionSpec[]`, *not* opaque JSON. Combined with §3,
that array is directly re-runnable: `editor.executeActions(editor.getMacro("q"))`.
That equivalence is the conceptual core — **a macro is a script and a script is
a macro.**

### Conversion helpers (Rust side)

```rust
impl Action {
    /// Render an action as an ActionSpec (name + payload args).
    pub fn to_action_spec(&self) -> ActionSpec { /* to_action_str + args map */ }
}
```

`MacroState::get(key)` already returns `&[Action]`; mapping through
`to_action_spec` yields the `Vec<ActionSpec>` the bridge returns. `defineMacro`
runs each `ActionSpec` through `Action::from_str` and inserts the resulting
`Vec<Action>` into the macros map under the register key.

## 5. The three user-facing capabilities

With §3 (lossless replay) and §4 (the bridge) in place, the three requested
features become small, mostly-TS features.

### 5.1 Store a macro in `init.ts` — "Persist Macro"

A new command, **`Macro: Save to init.ts`** (register prompt → target), takes
the recorded macro and appends a generated, *human-readable* block to
`init.ts`. The generated code is a registration that re-seeds the register at
startup:

```ts
// --- fresh:macro q (generated, editable) ---
editor.defineMacro("q", [
  { action: "move_line_start" },
  { action: "insert_char", args: { char: "-" } },
  { action: "insert_char", args: { char: " " } },
  { action: "move_line_down" },
]);
```

Mechanism: a new `PluginCommand::AppendToInitTs { text }` (or a focused
`init.ts` writer in `init_script.rs`) appends the block between sentinel
markers, then triggers the existing `init: Reload`. On next launch the
`defineMacro` call runs and `@q` is immediately available. Because the block is
plain TS in a file the user owns, it is also the edit surface (§5.2) and the
promotion surface (§5.3).

Generation niceties:

- Emit `count` only when `> 1`; emit `args` only when non-empty — so the common
  case reads cleanly.
- Coalesce runs of `insert_char` into a comment showing the literal typed text
  (`// types: "- "`) for readability, while keeping the precise steps.
- Wrap each macro in `// fresh:macro <key>` … `// fresh:end` sentinels so a
  future "update this macro" can rewrite in place instead of appending a
  duplicate.

This finally fulfils the promise the `ShowMacro` header already makes, but
points it at `init.ts` (a real load path) instead of a dead-end `.json`.

### 5.2 Edit a stored macro

Two editing routes, both falling out of §5.1 for free:

1. **Edit the code directly.** The macro is literal TS in `init.ts`. Open it
   (`init: Edit init.ts`), change a step, hot-reload. This is the primary,
   no-new-UI path.
2. **Round-trip through a scratch buffer (enhanced `ShowMacro`).** Keep the
   existing "show macro in a buffer" affordance but make it *loadable*: the
   buffer renders the `ActionSpec[]` (not raw serde), and a companion
   **`Macro: Load from buffer`** command parses the buffer back via
   `defineMacro`. This restores the symmetry the current read-only JSON dump
   lacks, and gives a lightweight "tweak and re-run without touching init.ts"
   loop.

### 5.3 Promote a macro into arbitrary code logic

This is the headline capability and the reason the export target is
`executeActions`-shaped rather than an opaque blob. "Promotion" is just
*choosing not to use `defineMacro`* and instead pasting the steps into a
handler you then edit freely.

A **`Macro: Promote to command`** command generates a registered command stub
seeded with the recorded steps:

```ts
// --- fresh:macro q (promoted — now ordinary code) ---
registerHandler("macro_q", async function () {
  // Originally recorded; edit freely from here.
  await editor.executeActions([
    { action: "move_line_start" },
    { action: "insert_char", args: { char: "-" } },
    { action: "insert_char", args: { char: " " } },
  ]);

  // ↓ Arbitrary logic the recording could never express:
  // for (const cursor of editor.getAllCursors()) { ... }
  // if (editor.getBufferPath(editor.getActiveBufferId()).endsWith(".md")) { ... }
});

editor.registerCommand("Bulletize line", "Prefix line with '- '", "macro_q");
```

The recorded sequence is now an ordinary `executeActions` call inside a real
function. Because the handler has the entire `EditorAPI`, the user can add
loops, conditionals, cursor iteration (`getAllCursors`), buffer/path checks,
process calls (`spawnProcess`), prompts, async/await — anything a plugin can do.
The macro was the *starting scaffold*; the function is the destination.

This gives a smooth capability ramp with no cliff:

```
record (@q)  →  defineMacro in init.ts  →  promote to registerHandler  →  arbitrary plugin code
   ephemeral        persistent, replayable        editable function          full programmability
```

Each step is strictly more powerful and each is reachable from the previous one
by a single command — exactly the "edit them or improve them into arbitrary
code logic" the request asks for.

## 6. Optional: bind a stored/promoted macro to a key

Promotion produces a registered command, which the existing keybindings editor
can already bind. For `defineMacro`-style macros, `init.ts` can also bind a key
to `playMacro` via `defineMode` / handler. No new keybinding machinery is
required — this is called out only so the design is understood to compose with
the existing input layer rather than fork it.

## 7. Implementation plan (incremental, each step shippable)

1. **`ActionSpec.args`** (§3). Core enabler; unblocks lossless replay. Tiny,
   backward-compatible. Add a test that `executeActions` with `insert_char` +
   `char` arg types the character. *(Rust only.)*
2. **`Action::to_action_spec` + macro bridge** `listMacros` / `getMacro` /
   `defineMacro` / `playMacro` (§4): new `PluginCommand` variants, `MacroState`
   already has the storage. Regenerate `fresh.d.ts`. *(Rust + generated TS.)*
3. **Enhanced `ShowMacro` + `Macro: Load from buffer`** (§5.2 route 2): render
   `ActionSpec[]`, add the parse-back command. Makes the round-trip real.
4. **`Macro: Save to init.ts`** (§5.1): the `init.ts` append-with-sentinels
   writer + reload. Delivers persistence.
5. **`Macro: Promote to command`** (§5.3): the handler-stub generator.
   Delivers the "arbitrary code" graduation.
6. **Docs + starter template**: add a commented macro example to
   `init_script.rs::STARTER_TEMPLATE` and a short section to the macros docs.

Steps 1–2 are the foundation; 3–5 are independent leaves that can land in any
order once the bridge exists.

## 8. Risks and mitigations

- **Forward/backward compatibility of action names.** Action names are a public
  contract once they live in `init.ts`. `to_action_str`/`from_str` already share
  one source of truth; we should treat renames as breaking and keep `alias`
  entries (the mapping macro already supports aliases) when an action is
  renamed, so old `init.ts` macros keep working.
- **Unknown action on load.** `Action::from_str` falls back to
  `PluginAction(name)` for unknown strings. `defineMacro` should validate
  against `all_action_names()` and warn (status message) on unknown steps rather
  than silently producing a no-op `PluginAction`, so a typo in hand-edited
  `init.ts` is visible.
- **`init.ts` corruption from generated writes.** Use sentinel-delimited blocks
  and never rewrite outside them; if the markers aren't found, append rather
  than edit. The existing crash fuse already protects against a broken `init.ts`
  bricking startup, and `--safe` disables it entirely.
- **Replay fidelity.** Macros recorded against prompt flows already need the
  `PromptConfirm → PromptConfirmWithText` transform (handled in
  `record_macro_action`). With `ActionSpec.args` that transformed variant
  serialises faithfully too; add a round-trip test
  (`record → to_action_spec → from_str` equals original) over a representative
  action set including `InsertChar` and `PromptConfirmWithText`.
- **Security / trust.** Promoted macros are arbitrary code, but they live in
  `init.ts`, which is already trusted, user-owned, and gated by the same
  fuse/`--safe` controls as any other startup script. No new trust surface is
  introduced — promotion just writes to a file the user already controls.

## 9. Why this shape (design rationale)

- **One representation, two uses.** Making `getMacro` return the exact shape
  `executeActions` consumes means a macro and its replay script are the same
  object. That equivalence is what makes promotion feel like "reveal the code
  behind this macro" rather than "rewrite this macro as code."
- **Reuse the `init.ts` pipeline, don't invent a macro store.** `init.ts`
  already gives persistence, editing, hot-reload, type-checking, crash recovery,
  and version-controllability for free. A bespoke `macros.json` would
  re-implement all of that worse and wouldn't enable the "arbitrary logic"
  endpoint at all.
- **Smallest core change.** The only non-additive Rust change is one optional
  struct field (`ActionSpec.args`); everything else is new commands and a TS
  surface over storage that already exists. The capability ramp is mostly
  generated TypeScript, which is the right place for user-facing, user-editable
  behaviour to live.
