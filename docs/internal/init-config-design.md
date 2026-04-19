# `init.ts` — Programmable User Config

> Design / RFC for `~/.config/fresh/init.ts`, an optional user file.
> Companion to #1529, which tracks the plugin-side primitives (PTY,
> raw-ANSI virtual buffers, floating windows, treesitter, animation
> timers) that rich UIs need.

This doc states objectives, principles, and architecture. It is
deliberately light on UI flow, command names, and output formats so the
implementer has room to choose well.

## 1. Purpose

Fresh already has a Settings UI, a keybindings editor, a theme
selector, project `.fresh/config.json`, and a plugin registry. `init.ts`
exists for the two things none of those handles well:

1. **Decisions that depend on the runtime environment** — host, `$TERM`,
   SSH, project path, time, env vars. Things that would be wrong in a
   shared file because the right value differs across machines or
   launches.
2. **Imperative configuration of plugins** that benefit from being
   expressed in code (closures for actions, branches on env, dynamic
   values) rather than a JSON form. The `init.lua` role.

Both modes require user code; neither is reusable enough to belong in a
plugin.

## 2. Non-overlap with existing surfaces

If the user's intent matches an existing surface, `init.ts` is the
wrong tool. The design must reinforce this — overlap with the Settings
UI, keybindings editor, theme selector, or plugin registry is a smell.

| Intent | Belongs in |
|---|---|
| Static preference (tab size, line numbers) | Settings UI → `config.json` |
| Permanent key binding | Keybindings editor |
| Permanent theme | Theme selector |
| Reusable feature | Plugin package |
| Plugin's basic toggles | Plugin's Settings UI panel |

`init.ts` enters when the *value* depends on environment, or when the
configuration shape is naturally code (closures, conditionals,
computed values). The `check` tool surfaces warnings when a write
isn't gated on an environment read or a plugin-API call.

## 3. Architecture

### 3.1 File layout

```
~/.config/fresh/
├── init.ts                 # entry point
├── init/                   # private modules; not auto-loaded as plugins
├── types/
│   ├── fresh.d.ts          # mirror of crates/.../plugins/lib/fresh.d.ts
│   ├── fresh-config.d.ts   # generated from config-schema.json
│   └── plugins/<name>.d.ts # shipped by plugins that expose code-config
└── tsconfig.json           # standard, makes init.ts portable to any TS LSP
```

### 3.2 API

`init.ts` uses the **same plugin API** plugins use — same
`getEditor()`, same `registerHandler`, same `editor.*`. Not a new
dialect. Five `EditorAPI` additions (§6) cover what's missing.

### 3.3 Lifecycle — three phases

`init.ts` is loaded once, but executes in three phases. Phase 1 is
top-level code; phases 2 and 3 are `editor.on("plugins_loaded", fn)`
and `editor.on("ready", fn)` callbacks. The user mostly doesn't think
about the split — gating sits at the top, plugin configuration in the
`plugins_loaded` callback, buffer/UI work in `ready`.

```
1. Built-in defaults
2. User config.json
3. init.ts top-level              ← phase 1: env-gated decisions
4. Plugins load
5. on("plugins_loaded") fires     ← phase 2: configure plugins
6. Project config.json            (still wins; collaboration guarantee)
7. Session overrides
8. UI opens, session restored
9. on("ready") fires              ← phase 3: touch buffers / dashboards
```

Project `config.json` always wins over `init.ts` writes. A teammate's
shared repo config is authoritative, regardless of the user's
`init.ts`.

### 3.4 Config layering

Init.ts writes go to a runtime layer between User and Project
`config.json`. Settings UI gains an `(init)` source badge that names
the originating file:line. **No init.ts write persists to disk** —
removing the file is a complete undo.

### 3.5 Plugin configuration plane

Plugins that want imperative config expose an API at load time:

```ts
// in welcome_dashboard.ts
editor.exportPluginApi("welcome-dashboard", {
  configure(opts: DashboardOptions) { /* idempotent */ }
});
```

User code:

```ts
editor.on("plugins_loaded", () => {
  const dash = editor.getPluginApi<DashboardApi>("welcome-dashboard");
  dash.configure({ /* … */ });
});
```

Conventions for plugin authors:

- `configure` is **idempotent** — re-applying replaces prior state, so
  reload works.
- Plugins must work with **no** `configure` call — sensible defaults,
  driven by Settings UI / `config.json` for simple toggles.
- A plugin that wants code-config ships `types.d.ts`; Fresh copies it
  into `types/plugins/<name>.d.ts` and adds it to the user's tsconfig.

## 4. Recovery & safety

The editor must always reach a usable state. Required, in order:

- **Type-check failure** — `init.ts` is skipped, an error indicator
  links to the diagnostic, editor starts with defaults.
- **Runtime throw** — effects from the failed run roll back; user can
  reload, edit, or enter safe mode.
- **Crash inside `init.ts`** three times within a short window — next
  launch enters safe mode automatically. Resets after one good launch.

Reload reverts the previous run's effects then re-runs; if the new run
fails, the previous effects re-apply (session keeps working).

The user can always start with `--safe` (skip init.ts and plugins) or
`--no-init` (skip init.ts only). Safe-mode startup must not require
`init.ts` to be syntactically valid.

## 5. Type safety

- `types/fresh.d.ts` mirrors the in-tree plugin API.
- `types/fresh-config.d.ts` is generated from `config-schema.json`
  so `editor.setSetting(path, value)` is type-checked against the
  real schema.
- Plugin-shipped types (§3.5) make `getPluginApi("name").configure({…})`
  type-checked.
- `tsconfig.json` is standard so `init.ts` works in any TS-aware
  editor, not just Fresh.

The `check` command runs in two modes:

- Default (always-on, low latency): syntax + scope-discipline lints
  (`unconditional-preference`, `unconditional-plugin-load`).
- Strict (opt-in): full type check via `tsc --noEmit` if available.

Specific output format and CLI shape are implementer's choice.

## 6. Required API additions

#1529 covers the *plugin-side* primitives needed for rich UIs (PTY
mode, raw-ANSI virtual buffers, floating windows, treesitter,
animation loops). Those land in the plugin API and benefit every
plugin.

`init.ts` needs the following on top, all on `EditorAPI`:

| # | Addition | Purpose | Priority |
|---|---|---|---|
| 6.1 | `setSetting(path, value)` | Runtime per-setting writes — the only true blocker | **P0** |
| 6.2 | `exportPluginApi(name, api)` / `getPluginApi<T>(name)` | Plugin-configuration plane (§3.5) | P1 |
| 6.3 | Closure overload for `editor.on(event, fn)`; new event names `plugins_loaded` and `ready` | Lifecycle phases (§3.3) without dedicated APIs | P1 |
| 6.4 | Effect-tracking proxy on the `editor.*` handle init.ts uses (internal; no public API change) | Reliable reload/revert; plugins continue to use the raw API | P2 |

§6.1 is the only blocker; the rest degrade gracefully or unlock
specific scenarios (plugin code-config, clean reload).

**Deliberately not added** — the alternative in each case is good
enough that a new method would just inflate the surface:

- `applyConfigPatch(partial)` — multiple `setSetting` calls cover it.
- `getSetting(path)` — the existing `getConfig()` returns the merged
  config; users cast.
- `getStartupContext()` — `getEnv("SSH_TTY")` etc. cover most of it.
  Fresh sets `FRESH_INTERACTIVE=1` on its own process env for the
  one case (TTY/normal-mode detection) that needs editor-internal
  knowledge.
- `setPluginEnabled(id, enabled)` — `setSetting("plugins.<id>.enabled",
  false)` does the same thing through the same mechanism. The plugin
  loader reads the runtime layer before plugins start.
- `onceConfigured(fn)` / `onceReady(fn)` — the closure overload on
  `editor.on` plus the two new event names cover this without a
  second API surface.
- `getProjectRoot()` — a short loop using existing `editor.fileExists`
  / `pathDirname` / `pathJoin` covers it.

## 7. Open questions

These genuinely change the design if answered differently. Other
implementation choices (CLI command names, dry-run output format,
scaffolder UX, file watcher behaviour) are deliberately omitted —
implementer chooses.

- **Project-config override.** Current proposal: project always wins
  over init.ts. Should there be an explicit `forceSetting` for the
  "I personally hate trailing whitespace, regardless of the project"
  case? If yes, surface a distinct badge in Settings UI.
- **Secrets in effect logs.** `getEnv("GITHUB_TOKEN")` flows into
  `setSetting` / `registerLspServer` writes that get serialised into
  reload effect logs and dry-run output. Either taint-track env reads
  or add an opaque `getSecret(name)` handle that never serialises.
- **Plugin-API resolution.** Lookup-style (`getPluginApi("name")`)
  ships immediately with no module loader work. ESM imports
  (`import { … } from "@fresh/plugin-name"`) is nicer but needs a real
  resolver. Lean toward the former for v1.
- **Settings UI interaction with `(init)` writes.** Toggling an
  init-written value either silently reverts on next launch
  (confusing) or pops a "controlled by init.ts:42 — open?" dialog.
  Lean toward the dialog.
