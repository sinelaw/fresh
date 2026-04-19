# init.ts — Implementation Plan

> Plan for the design in [init-config-design.md](init-config-design.md).
> Plugin-side primitives (PTY, raw-ANSI virtual buffers, floating windows,
> treesitter, animation timers) are tracked in #1529 and do not block
> the MVP here.

The work splits into six milestones. **M0 → M1 → M5 → M6** is the MVP
path that delivers a useful init.ts. **M2 → M3** unlocks the
plugin-configuration plane. **M4** is reload (tightens the iteration
loop). Each milestone is a shippable unit.

## M0 — Startup auto-load & safety plumbing

**Goal.** Fresh auto-loads `~/.config/fresh/init.ts` at startup using
the existing plugin pipeline; users can opt out.

- At the phase-1 step in design §3.3, read the init.ts file and call
  the existing `PluginManager::load_plugin_from_source(source,
  "init.ts", true)`. The runtime's hot-reload semantics already do
  the right thing: a prior plugin named `init.ts` (none, on first
  load) is unloaded, the new one is loaded.
- CLI flags `--safe` (skip init.ts and plugins) and `--no-init` (skip
  init.ts only).
- Crash-fuse: failed-launch counter at
  `~/.config/fresh/logs/init.crashes`; auto-engages safe mode after
  three crashes in a short window; resets after one good launch.
- Errors during evaluation are caught — status indicator + log entry,
  editor continues with whatever was applied so far.

No new source-tagging work: the plugin runtime already tags
registrations by plugin name (`CommandSource::Plugin(plugin_name)`,
`unregister_commands_by_plugin`). The plugin name `init.ts` is the
tag.

**Verifies.** Empty init.ts loads silently. A `throw` produces a
status indicator; editor still opens. `--safe` skips evaluation.
Crash fuse engages and resets correctly. `editor.unloadPlugin("init.ts")`
from the palette tears down init.ts's registrations.

**Depends on.** Nothing.

## M1 — Runtime config writes (§6.1)

**Goal.** Init.ts can change a setting for the lifetime of the
session. This is the single P0 blocker — everything else degrades
gracefully without it; this does not.

- Insert a runtime config layer between User and Project per §3.3.
- Add to `EditorAPI`: `setSetting(path, value)`,
  `applyConfigPatch(partial)`, `getSetting(path)`. Validate paths
  against `config-schema.json`.
- Record source file:line per write for attribution.
- Build step: generate `types/fresh-config.d.ts` from
  `config-schema.json` so setting paths and value types are checked
  at compile time.
- First-run setup: copy `crates/fresh-editor/plugins/lib/fresh.d.ts`
  to `~/.config/fresh/types/fresh.d.ts`; write
  `~/.config/fresh/tsconfig.json`.
- Settings UI: render `(init)` source badge with file:line.

**Verifies.** `setSetting("editor.tab_size", 2)` makes the session use
2; Settings UI shows `(init.ts:NN)`. Deleting init.ts restores the
prior value on next launch (nothing persisted).

**Depends on.** M0.

## M2 — Lifecycle events (§6.3)

**Goal.** Init.ts code can opt into running after plugins load or
after session restore, without a dedicated API surface.

- Add closure overload to existing `editor.on`: alongside the current
  `on(event, handlerName: string)`, accept `on(event, fn: Function)`.
  String form continues to work; closure form removes the
  `registerHandler` dance for one-shot callbacks.
- Add two new event names emitted by the runtime: `plugins_loaded`
  (fires between §3.3 steps 4 and 6) and `ready` (after step 8).
- Plugins may also subscribe to these events.

**Verifies.** A closure registered via `editor.on("plugins_loaded",
fn)` sees plugins loaded; one for `ready` sees the active buffer.
Existing string-handler form still works on the new events.

**Depends on.** M0.

## M3 — Plugin-API plane (§6.2)

**Goal.** Init.ts can configure plugins that expose imperative-config
APIs. Plugin gating reuses §6.1 — no separate API.

- Add to `EditorAPI`: `exportPluginApi(name, api)` (called from a
  plugin at load time) and `getPluginApi<T>(name)` (called from
  init.ts in the `plugins_loaded` callback).
- Plugin loader reads `setSetting("plugins.<id>.enabled", false)` from
  the runtime layer before starting plugins, so init.ts can gate
  loading via the existing `setSetting` mechanism.
- Per-plugin types: when a plugin ships `types.d.ts`, copy to
  `~/.config/fresh/types/plugins/<name>.d.ts` on install/update;
  auto-add to tsconfig `include`.

**Verifies.** `setSetting("plugins.vi_mode.enabled", false)` from
init.ts top level prevents `vi_mode` loading. A plugin calling
`exportPluginApi("foo", {…})` is reachable via
`getPluginApi<FooApi>("foo")`; type-check succeeds against the
shipped `.d.ts`.

**Depends on.** M0, M1, M2.

## M4 — Reload palette commands

**Goal.** Discoverable `init: Reload` and `init: Revert` palette
commands. The actual reload mechanism is "Load Plugin from Buffer"
which already exists.

- Add `init: Reload` palette command: read `~/.config/fresh/init.ts`,
  call `load_plugin_from_source(content, "init.ts", true)`. Existing
  hot-reload semantics drop the prior init.ts plugin (and with it
  init.ts's commands, handlers, event subs, LSP registrations,
  per-source runtime config layer from M1) and load the new source.
- Add `init: Revert`: call `unloadPlugin("init.ts")` and stop —
  leaves the editor as if init.ts had never run.
- Before re-loading, also call `reloadPlugin(name)` for every plugin
  init.ts touched via `getPluginApi` so their `configure` state
  resets. Track touched plugins in a small in-memory set indexed by
  the init.ts plugin instance.
- Invoking the existing "Load Plugin from Buffer" command on the open
  init.ts buffer must produce the same result. They share the same
  code path with the same plugin name.

**Verifies.** Edit init.ts → `init: Reload` → state matches the new
file. Edit to introduce a syntax error → reload → status indicator
surfaces the diagnostic; another reload after the fix lands clean.
"Load Plugin from Buffer" on init.ts produces the same effect as
`init: Reload`.

**Depends on.** M0 (auto-load uses the same pipeline), M1 (runtime
layer is per-plugin-source so it gets dropped on unload), M2 (typical
user code uses these events), M3 (plugin reload list).

## M5 — Check command & scope-discipline lints

**Goal.** Users can validate init.ts before launch; CI can lint a
dotfiles repo.

- `fresh --cmd init check`: oxc parse + scope-discipline lints.
- Lints walk the AST: a `setSetting` / `loadPlugin` / `applyTheme`
  call must have a data-flow dependency on `getEnv` / `getCwd` /
  `getPluginApi`. If not, emit `unconditional-preference` /
  `unconditional-plugin-load` warnings.
- Per-line escape: `// fresh-init: allow-unconditional`.
- Optional `--strict` runs `tsc --noEmit` when `tsc` is on PATH; warn
  once and fall back to parse mode otherwise.
- At startup, if check fails, init.ts is skipped and a status
  indicator surfaces the diagnostic.
- Fresh sets `FRESH_INTERACTIVE=1` on its own process env when
  starting in normal interactive mode, so init.ts can branch on
  `editor.getEnv("FRESH_INTERACTIVE")` without a dedicated API.

**Verifies.** A typo (`editor.tab_siz`) and an unconditional
`setSetting` both surface as diagnostics. Strict mode catches
schema-typed errors when `tsc` is available. `FRESH_INTERACTIVE` is
present under normal launch and absent under `$GIT_EDITOR`-style
invocations.

**Depends on.** M1 (so `setSetting` exists to lint).

## M6 — Discoverability & scaffolding

**Goal.** Users find init.ts and have a starting point.

- Palette: `init: Edit init.ts` (creates from template if missing),
  `init: Reload`, `init: Check`, `init: Status`.
- Starter template: minimal, all examples commented out. Comments
  name the existing surfaces (Settings UI, keybindings editor, theme
  selector, plugins) so users learn when init.ts is the *wrong*
  tool.
- One-time first-launch hint in the welcome screen, dismissible.

**Verifies.** A fresh install with no init.ts has palette commands
available; `init: Edit` creates a working file.

**Depends on.** M1.

## Cross-cutting

- **Tests.** Each milestone adds e2e fixtures in `crates/fresh-editor`
  alongside existing suites: a fixture init.ts plus assertions on the
  resulting editor state. M4 adds e2e coverage for the
  drop-tagged-then-re-eval flow, including the failed-reload case.
- **User docs.** Land `docs/configuration/init.md` after M1.
  Plugin-author docs for the `exportPluginApi` convention land in
  `docs/plugins/development/code-config.md` after M3.
- **#1529 coordination.** No milestone here blocks on #1529.
  Conversely, the first plugin that wants code-config — likely a
  snacks-style welcome dashboard — needs M3 to land first. Sequence
  the two designs so M3 lands before that plugin merges.
