# `init.ts` — Programmable User Config

> Design for a user-authored TypeScript entry point at `~/.config/fresh/init.ts`
> that complements the existing layered `config.json` and plugin packages.

Status: **Design / RFC** — not yet implemented.

---

## Table of Contents

1. [Motivation & Positioning](#1-motivation--positioning)
2. [Authoring Experience](#2-authoring-experience)
3. [Type Checking](#3-type-checking)
4. [Evaluation Model](#4-evaluation-model)
5. [Testing & Dry-Run](#5-testing--dry-run)
6. [Undo, Safe Mode & Versioning](#6-undo-safe-mode--versioning)
7. [What Users Will Put There](#7-what-users-will-put-there)
8. [Open Questions](#8-open-questions)
9. [Required API Additions](#9-required-api-additions)

---

## 1. Motivation & Positioning

### 1.1 Why another config surface?

Fresh today exposes three user-modifiable surfaces:

| Surface | Location | Form | Good for |
|---------|----------|------|----------|
| **User config** | `~/.config/fresh/config.json` | Static JSON | Simple preferences |
| **Project config** | `.fresh/config.json` | Static JSON | Per-project overrides |
| **Plugins** | `~/.config/fresh/plugins/packages/<pkg>/` | TypeScript package | Reusable, distributable features |

There is no surface for the middle ground: **user-specific, imperative,
one-off customisation** that doesn't justify packaging. Neovim solved this with
`init.lua`, Emacs with `init.el`, VS Code with `keybindings.json` + settings
sync, Zed with `settings.json` + `keymap.json`. Fresh's equivalent today is
"write a throwaway plugin package" — too much ceremony.

The proposed `~/.config/fresh/init.ts` fills that gap:

- **Single file.** No `package.json`, no manifest, no scaffolding.
- **Imperative.** Can compute values (host-dependent theme, time-of-day,
  `$SSH_TTY` detection, project-path heuristics).
- **First-class access** to the same `editor.*` API plugins use, plus a new
  `config.*` surface for setting/overriding layered settings.
- **Runs once at startup**, before user plugins, after built-in defaults.

### 1.2 Guiding principles

1. **Zero config stays zero config.** A missing `init.ts` must be
   indistinguishable from today's behaviour. No upgrade prompts, no nags.
2. **Failures are recoverable.** A broken `init.ts` must never brick the
   editor. The fallback is "start with defaults + a visible diagnostic".
3. **Everything discoverable via the command palette.** Every action
   `init.ts` can do must also be reachable by a human clicking around —
   `init.ts` is a scriptable shortcut, not a gate.
4. **Type-safe by default.** `editor`, `config`, and `ctx` globals come with
   ambient declarations; users get completion and errors without manual setup.
5. **Observable.** `fresh --cmd init status` shows what ran, what failed,
   what settings it changed and from which line.

### 1.3 Relationship to `config.json`

`init.ts` is a **new layer in the layer stack**, inserted between
`User config.json` and `Project config.json`:

```
System defaults        (lowest)
  ↓
User config.json
  ↓
init.ts  (new)   — imperative, can branch on env/project/host
  ↓
Project config.json
  ↓
Session overrides      (highest)
```

Rationale:

- Sits **above** `User config.json` so users can override their own JSON
  defaults conditionally (e.g. "dark theme at night").
- Sits **below** `Project config.json` so per-project JSON still wins —
  a checked-in `.fresh/config.json` must be authoritative for the project
  regardless of what the user's `init.ts` tried to set. This preserves the
  collaboration guarantee.
- Session layer still wins, so `:set` / Settings UI overrides are unaffected.

Writes from `init.ts` go through `editor.setSetting(path, value)`
(§9.1) which records the originating file + line so the Settings UI
can display `(from ~/.config/fresh/init.ts:42)` alongside `(user)` /
`(project)` badges.

### 1.4 Relationship to plugins

| | Plugin package | `init.ts` |
|---|---|---|
| Distribution | Shareable, installable via `pkg:` | Personal, not shared |
| Boilerplate | `package.json`, entry, manifest | None |
| Discovery | Registry, palette | Invisible — it's yours |
| Lifecycle | Auto-loaded each session | Runs once at startup, before plugins |
| API | `getEditor()` + `editor.*` | Same `getEditor()` + `editor.*` |
| Typical contents | A reusable feature (TODO highlighter, git grep) | Environment-conditional wiring (see §7) |

`init.ts` **is** a plugin in terms of code shape — it uses the same
`getEditor()` / `registerHandler` idiom. Its difference is *where*
it's loaded from, *when* (before registry plugins), and *what extra
API additions* it motivates (§9). A plugin that outgrows init.ts gets
promoted via `fresh --init plugin --from init.ts:myFunc`; the code
mostly copies over.

### 1.5 What `init.ts` is *not* for

Fresh already has dedicated surfaces for the common customisation tasks.
`init.ts` overlaps with none of them — overlap is a design smell.

| If the user wants to… | …they should use |
|---|---|
| Change tab size, line numbers, format-on-save, etc. | Settings UI → `config.json` |
| Rebind a key | Keybindings editor |
| Pick a theme | Theme selector / theme editor |
| Install a feature permanently | Plugin from the registry (`pkg:`) |
| Write a reusable feature | A plugin package |

`init.ts` is specifically for decisions that satisfy **all three** of
these tests:

1. The right answer depends on the **runtime environment** at startup
   (host, `$TERM`, SSH, project path, available binaries, `$PROFILE`…).
2. The decision would be **wrong to bake into a shared config file**,
   because it differs across machines or launches.
3. It is **not a reusable feature** another user would install from a
   registry.

If any of those fails, the answer is config.json, a UI editor, or a
plugin — not `init.ts`.

### 1.6 Non-goals

- **Not** a replacement for `config.json`. Static preferences belong in JSON;
  the Settings UI still writes there.
- **Not** a general scripting host. `init.ts` is startup-only; long-lived
  background work belongs in a plugin.
- **Not** unsandboxed. Runs in the same QuickJS sandbox as plugins —
  no arbitrary `fs` writes outside `~/.config/fresh/`, no raw process
  spawn without going through `editor.spawnProcess`.
- **Not** auto-generated. The Settings UI will never write to `init.ts`;
  it remains fully user-owned.

---

## 2. Authoring Experience

Target user: someone who installed Fresh, hit `Ctrl+P`, and wants to
"make a few things stick". They are comfortable with JSON; TypeScript is
a stretch. `init.ts` must feel closer to "a config file that can branch"
than to "write a program".

### 2.1 Zero-friction first file

From the command palette:

```
init: Edit init.ts
```

If `~/.config/fresh/init.ts` does not exist, this command:

1. Creates it with a commented-out starter template (see below).
2. Opens it in the current split.
3. Activates the embedded LSP (types auto-wired — §3).

The starter file mirrors the idioms every Fresh plugin uses — the same
`getEditor()` + `registerHandler` pattern, the same `editor.*` API
(`crates/fresh-editor/plugins/lib/fresh.d.ts`). `init.ts` is *not* a
new dialect; it's a plugin that Fresh loads first, from a fixed path.

```typescript
/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

// Fresh init.ts — decisions that depend on the environment at startup.
//
// init.ts is NOT for:
//   • Static preferences (tab size, line numbers, …) → Settings UI
//   • Key bindings                                    → Keybindings editor
//   • Themes you always want                          → Theme selector
//   • Reusable features                               → A plugin package
//
// init.ts IS for things that:
//   • Depend on where/how Fresh is starting (host, SSH, $TERM, project…)
//   • Would differ across machines or launches
//   • Can't live in a shared config.json without lying to teammates
//
// The API `editor.*` here is the same one plugins use.
// Full reference: ~/.config/fresh/types/fresh.d.ts
//
// Type-check:   fresh --cmd init check
// Dry-run:      fresh --cmd init dry-run
// Safe start:   fresh --safe    (skip this file for one launch)

// Example: pick a theme from terminal background detection.
// const isDark = (editor.getEnv("COLORFGBG") ?? "").endsWith(";0");
// editor.applyTheme(isDark ? "one-dark" : "one-light");

// Example: opt into vi-mode only for interactive launches —
// not when Fresh is invoked as $GIT_EDITOR.
// if (!editor.getEnv("GIT_EDITOR")) {
//   editor.loadPlugin(editor.pathJoin(editor.getPluginDir(), "vi_mode.ts"));
// }

// Example: host-specific rust-analyzer path.
// if (editor.getEnv("HOSTNAME") === "my-mac") {
//   editor.registerLspServer("rust", {
//     command: "/opt/homebrew/bin/rust-analyzer",
//     args: [],
//     autoStart: true,
//     initializationOptions: null,
//     processLimits: null,
//   });
// }

// Example: calmer UI over SSH. Requires a new API — see §9.
// if (editor.getEnv("SSH_TTY")) {
//   editor.setSetting("editor.diagnostics_inline_text", false);  /* PROPOSED */
//   editor.setSetting("terminal.mouse", false);                   /* PROPOSED */
// }
```

Design choices:

- **Top-level code, same as every plugin.** No `export default`, no
  custom entry shape. `const editor = getEditor();` is line 1 of every
  plugin in the repo; we don't invent a different idiom for init.ts.
- **`editor.*` only.** The full editor API is the contract. Users learn
  one API, not a second "init dialect".
- **Every example commented out.** An empty file is valid; users
  un-comment what they want.
- **`/* PROPOSED */` markers** flag any call that depends on an API
  addition. Every such marker has a corresponding entry in §9.
- **Triple-slash reference** to the shipped `types/fresh.d.ts` gives
  the TypeScript LSP full completion with no user setup (§3).

### 2.2 No `InitContext` — use the plugin API

An earlier draft of this design invented an `InitContext` parameter
containing `editor`, `config`, `plugins`, `ctx`. That invention is
dropped: it duplicated the plugin API, added a second way to do
everything, and pretended `config.set` / `plugins.enable` existed.

Instead, init.ts uses the real API:

| Need | Real API |
|---|---|
| Editor handle | `const editor = getEditor();` |
| Register a named callback | `registerHandler("fn_name", fn);` |
| Register a palette command | `editor.registerCommand(name, desc, "fn_name", null);` |
| Subscribe to an event | `editor.on("buffer_activated", "fn_name");` |
| Read env vars | `editor.getEnv("SSH_TTY")` |
| Read cwd / config dir | `editor.getCwd()`, `editor.getConfigDir()` |
| Apply a theme | `editor.applyTheme("one-dark")` |
| Configure an LSP | `editor.registerLspServer("rust", { … })` |
| Disable an LSP for this session | `editor.disableLspForLanguage("rust")` |
| Load/unload a plugin | `editor.loadPlugin(path)` / `editor.unloadPlugin(name)` |
| Persistent key/value store | `editor.setGlobalState(k, v)` / `getGlobalState(k)` |
| Run a built-in action | `editor.executeAction("save")` |
| Filesystem (under config dir) | `editor.fileExists`, `readFile`, `writeFile` |

Things `init.ts` wants that the API doesn't yet expose (a richer
environment snapshot, per-setting writes, pre-plugin-load gating,
rollback-tracked config changes) are enumerated in §9 as concrete
additions to the existing `EditorAPI`. Every starter-template comment
marked `/* PROPOSED */` maps to an entry there.

### 2.3 Imports, modules, and splitting `init.ts`

Users who outgrow a single file:

```typescript
/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

import { applyLaptopSettings }      from "./init/laptop";
import { applyWorkstationSettings } from "./init/workstation";

const host = editor.getEnv("HOSTNAME") ?? "";
if (host.startsWith("work-")) applyWorkstationSettings(editor);
else                          applyLaptopSettings(editor);
```

- Imports resolve relative to `~/.config/fresh/`.
- Bare specifiers (`"lodash"`) are **not** supported — no `node_modules`,
  no network. This keeps `init.ts` reviewable at a glance.
- A single `init/` subdirectory is recognised as private-to-user; files there
  are **not** auto-loaded as plugins.

### 2.4 Hot reload

```
init: Reload init.ts
```

The existing plugin runtime already supports `editor.reloadPlugin(name)`;
init.ts gets the same mechanism. Semantics:

- Handlers registered via `registerHandler` / `editor.registerCommand` /
  `editor.on` during the previous run are unregistered first.
- Imperative settings changes made via the proposed `editor.setSetting`
  (§9) are reverted from the init source-layer before the new run.
- Then the file is re-transpiled and re-executed.
- If the new run throws, the previous effects are re-applied; the
  session keeps working. A red status-bar badge links to the error log.

This makes `init.ts` safe to iterate on — edit, `Ctrl+P → init: Reload`,
see the result. Note that a full rollback depends on the setting-write
API in §9; until those additions land, reload is best-effort for
settings and only reliably clean for handlers/commands/events.

### 2.5 Scaffolding, not magic

`fresh --init` already scaffolds plugin/theme/language packages. We add:

```bash
fresh --init user       # create ~/.config/fresh/init.ts from template
fresh --init user --with vim,format-on-save,dark-theme-at-night
```

`--with` drops ready-to-edit snippets into the file (still commented by
default). The recipes are plain strings in the Fresh binary; no network
fetch at scaffold time. This keeps the "my editor starts broken because
a CDN is down" class of failure off the table.

---

## 3. Type Checking

The value of a `.ts` file over `.js` is the compiler catching typos before
the user hits a runtime crash at startup. That requires shipping types
the editor and external tooling can both find.

### 3.1 Shipped ambient types

Fresh already ships `fresh.d.ts` for plugin authors at
`crates/fresh-editor/plugins/lib/fresh.d.ts`. For init.ts we write the
same file to the user's config dir on first run, plus a generated
typed-path file for config lookups:

```
~/.config/fresh/types/
├── fresh.d.ts         # exact copy of the plugin API reference
└── fresh-config.d.ts  # generated from config-schema.json for §9.1 path typing
```

Generation:

- `fresh.d.ts` mirrors the in-tree plugin types; Fresh copies it on
  first run if absent or stale.
- `fresh-config.d.ts` is produced at build time from `config-schema.json`
  (already the source of truth for the Settings UI). It turns the
  proposed `setSetting` into a typed path lookup, so
  `editor.setSetting("editor.tab_size", "4")` fails at compile time —
  `tab_size` is `number`, not `string`.
- Both files are regenerated on every Fresh start **only if their
  content hash differs from what's on disk**, so editing them is
  futile but the disk isn't churned.
- Users can pin a specific version by adding the files to their dotfiles
  repo; Fresh respects an `init-types: frozen` marker line to skip
  regeneration.

### 3.2 Auto-wiring the LSP

The starter template uses the same `/// <reference path>` idiom every
plugin does:

```typescript
/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();
```

`~/.config/fresh/tsconfig.json` is written at the same time as the types
so stand-alone editors (VS Code, Neovim with `tsserver`) also resolve
the types when opening `init.ts` directly:

```json
{
  "compilerOptions": {
    "target": "es2022",
    "module": "esnext",
    "strict": true,
    "noEmit": true,
    "allowJs": false
  },
  "include": ["init.ts", "init/**/*.ts", "types/**/*.d.ts"]
}
```

Two consequences:

1. Opening `init.ts` in Fresh (with the TypeScript LSP installed) gives
   full completion, hover, and diagnostics — no user setup.
2. Opening `init.ts` in VS Code, Neovim, etc. **also** works, because the
   `tsconfig.json` is standard. Fresh's `init.ts` is not a dialect;
   it's just a TS file with the same types plugins use.

### 3.3 `fresh --cmd init check`

A non-interactive command for users who want a hard pre-flight, or want
to wire `init.ts` into CI for their dotfiles repo:

```bash
$ fresh --cmd init check
~/.config/fresh/init.ts:42:17  error  Argument of type 'string' is not
  assignable to parameter of type 'number'.
    editor.setSetting("editor.tab_size", "2");
                                         ~~~
1 error. init.ts will not be evaluated until fixed.
```

Implementation: Fresh embeds the same TypeScript-to-QuickJS pipeline used
for plugins (oxc_transformer), plus an optional deeper check via the
shipped types. Two modes:

| Mode | Engine | Catches | Latency |
|------|--------|---------|---------|
| **Parse** (default, always on) | oxc | Syntax, undeclared identifiers | <10ms |
| **Strict** (`--strict` / CI) | `tsc --noEmit` if available on PATH | Full type errors | seconds |

The default mode is fast enough to run on every startup; strict mode is
opt-in to avoid making `tsc` a hard dependency.

Alongside type checks, `check` runs **scope-discipline lints** that
nudge users away from using `init.ts` for things that belong in the
Settings UI, the keybindings editor, or a plugin. These are warnings,
not errors, and are defined in §7.11. Two examples:

- `init/unconditional-preference` — an `editor.setSetting` /
  `editor.applyConfigPatch` call (§9.1) whose value cannot be shown
  to depend on an environment read (`getEnv`, `getCwd`,
  `getStartupContext`).
- `init/unconditional-plugin-load` — an `editor.loadPlugin` or
  `editor.setPluginEnabled(id, true)` (§9.3) with no environment gate.

Users can silence a specific line with a `// fresh-init: allow-unconditional`
comment when they genuinely want the programmability despite the lint.

### 3.4 Diagnostics surfacing

If parse-mode finds errors at startup, the editor starts normally with:

- A red badge in the status bar: `init.ts: 1 error`.
- Clicking it opens `init.ts` at the error line.
- The file is **not executed** — we start from defaults for that session.

If strict mode is configured in `config.json`:

```json
{ "init": { "type_check": "strict" } }
```

…and `tsc` is unavailable, Fresh falls back to parse mode and warns once.
No silent degradation.

### 3.5 Why not just `.js`?

We briefly considered `.js` for simplicity. Rejected because:

- The type safety for `editor.setSetting(path, value)` (§9.1) is the
  single biggest usability win — the JSON schema is huge and
  typo-prone.
- Users editing `init.ts` inside Fresh get inline errors "for free"
  without needing `jsdoc` comments.
- Plugins are already `.ts`; keeping the same extension avoids teaching
  two languages for "the TypeScript-shaped thing Fresh runs".

---

## 4. Evaluation Model

### 4.1 When does it run?

Exactly one call to `init()` per Fresh process, at a fixed point in
startup:

```
1. Load built-in defaults
2. Merge User config.json
3. Resolve ambient types / tsconfig (write if missing)
4. Parse-check init.ts          ← abort to (8) on error
5. Transpile init.ts via oxc
6. Evaluate init.ts              ← run init() inside QuickJS
7. Merge Project config.json     (overrides init.ts writes)
8. Apply Session overrides
9. Load installed plugins
10. Open UI, restore session
```

Key ordering choices:

- **init.ts runs before plugins load.** That way `editor.setPluginEnabled`
  (§9.3) actually influences which plugins the loader picks up, and users
  can set plugin-visible config keys (`plugins.<id>.*`) before the plugin
  reads them.
- **init.ts does not see the restored session.** No buffer is open, no
  split exists yet. `init.ts` is configuration, not a first-frame script.
  Anything that needs live buffers goes through `editor.on("ready", …)`.
- **Project config wins over init.ts.** A user opening a teammate's
  repo gets the repo's conventions, not their own `init.ts` writes —
  this preserves the collaboration guarantee from §1.3.

### 4.2 Sandbox

`init.ts` runs in the same QuickJS sandbox as plugins, with the same
capability model:

| Capability | Available? | Notes |
|------------|------------|-------|
| `editor.*` API | Yes | Full plugin API |
| `config.*` API | Yes | New — writes attributed to init layer |
| `plugins.*` API | Yes | New — enable/disable only |
| Read files under `~/.config/fresh/` | Yes | Via `editor.readFile` |
| Read any other file | No | Must route through `editor.spawnProcess` |
| Write files | No | Only the runtime mutates config; `init.ts` is pure |
| Network | No | No `fetch`, no sockets |
| Subprocess | Yes, via `editor.spawnProcess` | Audit log entry emitted |
| `setTimeout` / `setInterval` | **No** | `init.ts` must be synchronous-ish |
| `Promise` / `async` | Yes | Awaited with a hard timeout |

The prohibition on timers is deliberate: a timer registered from `init.ts`
would outlive the init phase, making the "undo" story in §6 much harder.
Long-lived work should be in a plugin.

### 4.3 Async and the startup budget

`init()` may be `async`. The runtime awaits it with a **2-second hard
deadline** by default. Timing out:

- Cancels any in-flight `spawnProcess` children.
- Rolls back side effects registered so far (§6).
- Surfaces a status-bar error: `init.ts timed out at line 17`.
- Continues startup with defaults.

The budget is tunable in `config.json`:

```json
{ "init": { "timeout_ms": 2000 } }
```

Rationale: the user's bar for "how fast does my editor start" is tight.
If `init.ts` needs to wait on an LSP, a git probe, or a shell RC, that
should be a *plugin* reacting to an event — not a blocking step.

### 4.4 Determinism and caching

`init.ts` is expected to produce the same effects given the same inputs
(env vars, `ctx`, config layers below it). To reinforce this:

- The runtime records a **fingerprint** after each successful run:
  SHA-256 of `(init.ts bytes, ctx snapshot, lower-layer config bytes)`.
- On the next start, if the fingerprint matches, Fresh may **replay the
  cached effects** instead of re-executing `init.ts`. This keeps a warm
  start cheap even for users with substantial init files.
- Replay is invalidated by: file change, config.json change, `$TERM`
  change, `--no-init-cache`, hostname change, or a version bump.
- Users can disable the cache entirely: `{ "init": { "cache": false } }`.

This is purely an optimisation; correctness never depends on it.

### 4.5 Error handling

Any throw from `init()` is caught. The runtime:

1. Rolls back effects applied during the failed run (§6).
2. Writes the stack trace, with the user's original line numbers restored
   via oxc's source map, to `~/.config/fresh/logs/init.log`.
3. Shows a one-line banner in the status bar:
   `init.ts failed — [View log] [Reload] [Safe mode]`.
4. Continues startup as if `init.ts` were absent.

**The editor always reaches a usable state.** This is the most important
property of the entire design; everything else is quality-of-life.

### 4.6 Observability

`fresh --cmd init status` reports, for the current session:

- Whether `init.ts` was evaluated, skipped, cached, or failed.
- Each `editor.setSetting` / `applyConfigPatch` call (§9.1) with path,
  value, and source location.
- Each `editor.setPluginEnabled` / `loadPlugin` / `unloadPlugin` call.
- Each `editor.registerLspServer` / `applyTheme` / `registerGrammar`
  call.
- Each `registerHandler` / `registerCommand` / `on` registered.
- Cumulative wall-clock cost.

The same data is browsable from the Settings UI: a new "Init" tab lists
settings with an `(init)` source badge and the exact line that wrote them.
Hovering that badge shows a peek of the source line. Clicking jumps to the
file at that line. Users can see, in one place, *what their init file did*.

---

## 5. Testing & Dry-Run

Users need to answer three questions before letting `init.ts` run on their
next real launch:

1. "Does it parse and type-check?" — answered in §3 by
   `fresh --cmd init check`.
2. "What would it actually do?" — dry-run.
3. "Does my `if (isSsh)` branch work?" — scenario testing.

### 5.1 Dry-run

```bash
$ fresh --cmd init dry-run
Evaluating ~/.config/fresh/init.ts in dry-run mode…

settings (2):                                          (via setSetting — §9.1)
  editor.diagnostics_inline_text  true → false         init.ts:14 (gated on SSH_TTY)
  terminal.mouse                  true → false         init.ts:15

plugins (1):
  setPluginEnabled  vi_mode = false                    init.ts:19  (§9.3)

lsp (1):
  registerLspServer  rust                              init.ts:24
    command = "/opt/homebrew/bin/rust-analyzer"

themes (1):
  applyTheme  one-dark                                 init.ts:8

handlers / commands (0)

Total time: 11ms. No errors.
```

Mechanics:

- Runs init.ts against a **shadow** `EditorAPI` — a proxy that records
  every mutating call (`setSetting`, `registerLspServer`, `applyTheme`,
  `setPluginEnabled`, `loadPlugin`, `registerCommand`, `on`, …) without
  applying it. Non-mutating reads (`getEnv`, `getCwd`, `getConfig`,
  `getStartupContext`) are served from a scenario-controlled snapshot.
- `editor.spawnProcess` returns `{stdout:"", stderr:"", exit_code:0}`
  with a warning; real subprocesses never run.
- Exits non-zero if any call fails (unknown plugin id, unknown setting
  path), so dry-run is safe to use in CI.

A `--json` flag produces a structured report:

```json
{
  "ok": true,
  "settings": [
    { "path": "editor.diagnostics_inline_text", "from": true, "to": false, "at": "init.ts:14" }
  ],
  "plugins":  [ { "op": "setPluginEnabled", "id": "vi_mode", "enabled": false, "at": "init.ts:19" } ],
  "lsp":      [ { "language": "rust", "command": "/opt/homebrew/bin/rust-analyzer", "at": "init.ts:24" } ],
  "themes":   [ { "name": "one-dark", "at": "init.ts:8" } ],
  "duration_ms": 11
}
```

### 5.2 Scenario overrides

Branching on the environment is the whole point of init.ts. To test the
branches:

```bash
$ fresh --cmd init dry-run --env SSH_TTY=/dev/pts/3 --env HOSTNAME=my-mac
$ fresh --cmd init dry-run --ctx isDark=true --ctx platform=macos
```

- `--env KEY=value` overrides what `editor.getEnv(...)` returns.
- `--ctx field=value` overrides a field of `editor.getStartupContext()`
  (§9.2).
- Anything not overridden uses the real value from the host.

The runtime asserts that overridden values are **used** — if the user
passes `--env HOSTNAME=laptop` and the script never reads `HOSTNAME`,
the flag is flagged as unused. Catches "I thought this branch triggered
on hostname" mistakes.

### 5.3 User-authored assertions

For users who want their `init.ts` to double as self-test:

```typescript
/// <reference path="./types/fresh.d.ts" />
import { assert } from "fresh/testing";
const editor = getEditor();

// … normal init.ts code …

if (import.meta.test) {
  assert.equal(editor.getSetting("editor.diagnostics_inline_text"), false);
  assert.equal(editor.getStartupContext().isSsh, true);
}
```

- `import.meta.test` is `true` only under `fresh --cmd init dry-run --test`.
- `fresh/testing` is a tiny built-in module; no package install.
- Failed assertions are reported like type errors: non-zero exit, line
  number, message. Suitable for a GitHub Action that lints a dotfiles
  repo on every push.

### 5.4 Snapshot tests

Power users can lock in the exact set of effects:

```bash
$ fresh --cmd init snapshot > init.snapshot.json
$ git add init.ts init.snapshot.json

# later, in CI:
$ fresh --cmd init dry-run --json | diff - init.snapshot.json
```

The snapshot format is the same JSON produced by `dry-run --json`, with
timestamps and durations stripped. Fresh does not manage the snapshot
file itself — it's the user's responsibility to commit and diff it.

### 5.5 Fresh's own tests

Inside the Fresh codebase we add two test layers:

- **Runtime unit tests** over the shadow-state engine: every mutating
  `EditorAPI` method tracked by init.ts (§9.5) has a test that asserts
  it records an effect and that rollback restores the prior state.
- **End-to-end** tests that launch Fresh with a fixture `init.ts`, assert
  the resulting config, then reload with an edited file and assert the
  deltas. These run in `crates/fresh-editor` alongside existing e2e
  suites.

Neither depends on `tsc`; strict-mode checks are gated on an optional
dev-env feature flag.

---

## 6. Undo, Safe Mode & Versioning

A programmable config that can brick the editor is a non-starter. The
recovery story needs to work for three distinct situations:

1. **The file is wrong.** Syntax error, type error, runtime throw.
2. **The file is right, but the user hates the result.** "My theme
   changed and I don't know why."
3. **The editor won't start at all.** Fresh crashes inside `init()`.

### 6.1 Effect tracking as a first-class record

Every mutating call the init runtime intercepts (§9.5) produces an
**effect record**:

```typescript
type Effect =
  | { kind: "setting"; path: string; before: unknown; after: unknown; at: SourceLoc }
  | { kind: "plugin";  op: "load" | "unload" | "setEnabled"; id: string; prev?: unknown; at: SourceLoc }
  | { kind: "lsp";     language: string; before: LspServerPackConfig | null; after: LspServerPackConfig | null; at: SourceLoc }
  | { kind: "theme";   before: string | null; after: string; at: SourceLoc }
  | { kind: "handler"; name: string; at: SourceLoc }
  | { kind: "command"; name: string; handler: string; at: SourceLoc }
  | { kind: "hook";    event: string; handler: string; at: SourceLoc };
```

These are:

- Appended to an in-memory list during `init()`.
- Snapshotted to `~/.config/fresh/logs/init.last.json` after a
  successful run, for use by `--cmd init status`, `init: Reload`, and
  `fresh init revert`.
- The `before` field on `config` effects is what makes precise rollback
  possible — it's the value the layer below init.ts had at the time the
  write happened.

### 6.2 Reload = revert + re-run

`init: Reload init.ts`:

1. Takes the current effect list.
2. Iterates it in **reverse**, calling the inverse of each effect
   (`config` restores `before`, `keybind` restores `prev`, `hook`/`command`
   unregisters, `plugin.enable` unloads).
3. Clears the list.
4. Runs the new `init()`.
5. If the new run throws: reapply the saved effect list from the previous
   run. The user ends up back where they were.

This mirrors the editor's own undo model and is the primary reason for
the "no timers" rule in §4.2 — an unowned timer has no inverse.

### 6.3 `fresh init revert`

For step-by-step backing out:

```bash
$ fresh --cmd init revert
Reverting the effects of your last init.ts run.
  config    editor.tab_size   2 → 4
  config    theme             "dark" → "default"
  plugin    disable           vim-mode
  keybind   Ctrl+Shift+D      (removed)

Session is now running as if init.ts were empty.
To make this permanent, delete or rename ~/.config/fresh/init.ts.
```

Revert uses `init.last.json` so it works even if `init.ts` has been
edited since. It operates **on the running session only**; the next
startup will re-run the file as usual unless the user also disables it.

### 6.4 Safe mode

```bash
$ fresh --safe
```

Starts Fresh with:

- `init.ts` skipped entirely.
- All plugins disabled.
- Session overrides ignored.
- A persistent banner: `Safe mode — init.ts & plugins disabled.`

Safe mode is the "it won't even start" escape hatch. It must not require
the user's `init.ts` to be syntactically valid (since they may have
pushed broken code to their dotfiles and synced to a new machine).

A softer `fresh --no-init` skips only `init.ts`, keeps plugins. Useful
for bisecting "is this init.ts or a plugin?".

### 6.5 Automatic crash fuse

If Fresh crashes three times within 60 seconds while evaluating
`init.ts` (tracked in `~/.config/fresh/logs/init.crashes`):

- The next start enters safe mode automatically.
- A dialog on first paint: *"init.ts appears to crash Fresh. Safe mode
  is on. [Edit init.ts] [View log] [Disable this fuse]"*.
- The fuse resets once any launch succeeds normally.

This is the analogue of shell rc files getting `-f`-protected after a
failed login: the user keeps control of their tooling even when the
tooling is misconfigured.

### 6.6 Per-run history

Every evaluation — success or failure — writes a timestamped entry to
`~/.config/fresh/logs/init.history/`:

```
2026-04-15T08:21:04Z-ok.json
2026-04-16T09:03:11Z-fail.json
2026-04-16T09:04:02Z-ok.json
```

Each file is a JSON object containing:

- The SHA-256 of the `init.ts` that ran.
- The effect list (same shape as `dry-run --json`).
- Duration, exit status, any thrown error.

The directory is capped at 50 entries, rotated oldest-first. Users can
look at the history to answer "what did my config do last Tuesday?"
without needing a git repo.

### 6.7 Git-friendliness

Nothing in the design assumes or requires `git`, but everything cooperates
with it:

- `init.ts`, `tsconfig.json`, and the `init/` subdirectory are plain
  files — trivial to dotfile-manage.
- `types/`, `logs/`, and the `cache` are all in paths documented as
  `.gitignore`-friendly; the scaffolder writes a suggested `.gitignore`.
- The snapshot file from §5.4 is designed to be diffed in PR review.

The combined effect: losing your `init.ts` is no worse than losing a
single file from your dotfiles repo, and recovering is `git checkout`.

---

## 7. What Users Will Put There

Every example here passes all three tests from §1.5:

1. It depends on the runtime environment.
2. It would be wrong in a shared config file.
3. It isn't a reusable feature.

Examples that fail any of those tests — static preferences, unconditional
keybindings, "I always want theme X", reusable features — are **not**
`init.ts` examples and are listed in §7.10 with the surface that *is* right
for them.

Every snippet below uses the real plugin API
(`crates/fresh-editor/plugins/lib/fresh.d.ts`) except where marked
`/* PROPOSED */`, which points to an addition described in §9.

### 7.1 Conditional plugin enable (the seed example, sharpened)

A user who wants vi-mode **always on** just installs the plugin —
installation persists; nothing to do here. `init.ts`'s niche is the
*conditional* version:

```typescript
// init.ts — top-level, same idiom as every plugin.
const editor = getEditor();

// vi-mode only for interactive launches — not when Fresh is invoked
// as $GIT_EDITOR for a commit message or from a quick-diff tool.
if (!editor.getEnv("GIT_EDITOR")) {
  editor.loadPlugin(editor.pathJoin(editor.getPluginDir(), "vi_mode.ts"));
}
```

The decision recomputes on every launch. `loadPlugin` is a real API
already used in plugin development. The cleaner form — gating *before*
the plugin loader runs, instead of loading-then-maybe-unloading —
requires a small addition (§9.3).

### 7.2 Theme by terminal / OS appearance

```typescript
const colorfgbg = editor.getEnv("COLORFGBG") ?? "";
// COLORFGBG="15;0" means light fg on dark bg → dark terminal.
const isDark = colorfgbg.endsWith(";0") || colorfgbg.endsWith(";default");

editor.applyTheme(isDark ? "one-dark" : "one-light");
```

`applyTheme` is real. The "stickiness" is a question for §9.1: this
writes the theme to the user's config permanently, whereas init.ts
wants "for this launch only."

### 7.3 Calmer UI over slow links

```typescript
if (editor.getEnv("SSH_TTY")) {
  editor.setSetting("editor.diagnostics_inline_text", false);  /* PROPOSED §9.1 */
  editor.setSetting("terminal.mouse", false);                   /* PROPOSED §9.1 */
}
```

No equivalent exists in the current API — `getConfig()` reads, but
there's no `setConfig(path, value)`. See §9.1.

### 7.4 Host-dependent LSP binary

```typescript
if (editor.getEnv("HOSTNAME") === "my-mac") {
  editor.registerLspServer("rust", {
    command: "/opt/homebrew/bin/rust-analyzer",
    args: [],
    autoStart: true,
    initializationOptions: null,
    processLimits: null,
  });
}

if (editor.getEnv("CI") === "true") {
  editor.disableLspForLanguage("rust");
}
```

Entirely real API. LSP config is one place where the plugin API is
actually ahead of the JSON config — `registerLspServer` takes the full
`LspServerPackConfig` shape.

### 7.5 Project-path branching without touching teammates' repos

```typescript
const cwd = editor.getCwd();
const home = editor.getEnv("HOME") ?? "";

if (cwd.startsWith(`${home}/work/`)) {
  editor.setSetting("editor.format_on_save", true);  /* PROPOSED §9.1 */
}
```

`editor.getCwd()` is real. The per-path config write is §9.1 again.
Note: `getCwd` gives the working dir Fresh was launched from, not a
discovered project root — see §9.4.

### 7.6 Terminal-capability adaptation

```typescript
const term = editor.getEnv("TERM") ?? "";
if (term === "linux" || term === "dumb") {
  editor.setSetting("editor.rulers", []);               /* PROPOSED §9.1 */
  editor.setSetting("editor.show_whitespace", "off");   /* PROPOSED §9.1 */
}
if (editor.getEnv("TMUX")) {
  editor.setSetting("editor.terminal_background", true); /* PROPOSED §9.1 */
}
```

Environment reads are real, the writes are §9.1.

### 7.7 Resource-aware tuning

```typescript
const host = editor.getEnv("HOSTNAME") ?? "";
if (host.startsWith("laptop-")) {
  editor.registerLspServer("rust", {
    command: "rust-analyzer",
    args: [],
    autoStart: true,
    initializationOptions: { cachePriming: { enable: false } },
    processLimits: {
      maxMemoryPercent: 25,
      maxCpuPercent: 50,
      enabled: true,
    },
  });
}
```

Shows the pattern `rust-lsp.ts` already uses — `registerLspServer` with
`processLimits` to cap memory/CPU. Entirely real.

### 7.8 Env-driven profiles

```typescript
switch (editor.getEnv("FRESH_PROFILE")) {
  case "writing":
    editor.loadPlugin(
      editor.pathJoin(editor.getPluginDir(), "prose_tools.ts"),
    );
    editor.setSetting("editor.line_wrap", true);        /* PROPOSED §9.1 */
    editor.setSetting("editor.wrap_column", 80);        /* PROPOSED §9.1 */
    break;
  case "review":
    editor.loadPlugin(
      editor.pathJoin(editor.getPluginDir(), "pr_tools.ts"),
    );
    break;
}
```

`FRESH_PROFILE=writing fresh essay.md` activates a prose setup.
`loadPlugin` is real; the `editor.line_wrap` write needs §9.1.

### 7.9 Secrets wiring

```typescript
const token = editor.getEnv("GITHUB_TOKEN");
if (token) {
  editor.registerLspServer("copilot", {
    command: "copilot-language-server",
    args: [],
    autoStart: true,
    initializationOptions: { token },
    processLimits: null,
  });
}
```

Every call here is real. This is arguably the single cleanest
init.ts use case today — the API already supports exactly what's
needed.

### 7.10 Dotfile composition across hosts

```typescript
/// <reference path="./types/fresh.d.ts" />
const editor = getEditor();

import { applyLaptop }      from "./init/laptop";
import { applyWorkstation } from "./init/workstation";
import { applySshGuest }    from "./init/ssh-guest";

const host = editor.getEnv("HOSTNAME") ?? "";
if (editor.getEnv("SSH_TTY"))         applySshGuest(editor);
else if (host.startsWith("work-"))    applyWorkstation(editor);
else                                  applyLaptop(editor);
```

One dotfiles repo, every machine — without conditionals scattered
through a JSON file that can't express them.

### 7.11 Things that look init.ts-shaped but aren't

Every row here fails at least one test from §1.5.

| Instinct | Better home | Why it fails |
|---|---|---|
| Set `editor.tab_size = 2` unconditionally | Settings UI → config.json | Not environment-dependent |
| Bind `Ctrl+Shift+D` to duplicate-line unconditionally | Keybindings editor | Not environment-dependent |
| "Always use tokyonight" | Theme selector | Not environment-dependent |
| "Install a TODO highlighter for me" | Plugin package (installed = persistent) | Reusable feature |
| "Register a `:scratch` command" | Plugin package | Reusable feature |
| "Format on save in every project" | Settings UI → config.json | Not environment-dependent |
| `editor.on("buffer_save", …)` always running | Plugin with an event hook | Reusable feature, long-lived |
| "Fetch tags from a registry on startup" | Plugin | Long-lived; init is sync-ish |
| "Watch files and rebuild" | Plugin | Long-lived |
| "Install a missing plugin automatically" | `pkg:` commands | init.ts can't write disk |
| "Put my API token as a literal in init.ts" | Shell env + `editor.getEnv` | init.ts may be git-committed |

`fresh --cmd init check` emits two scope-discipline lints aimed at the
top of that table:

- **`init/unconditional-preference`** — a setting write whose value
  has no data-flow dependency on `editor.getEnv(...)` / `getCwd()` /
  similar environment reads. Suggests the Settings UI.
- **`init/unconditional-plugin-load`** — a `loadPlugin` call not
  gated on any environment read. Suggests just installing the plugin
  via `pkg:`.

Both are warnings, not errors, and can be silenced on a specific line
with `// fresh-init: allow-unconditional`.

---

## 8. Open Questions

These are the points that deserve a second pair of eyes before
implementation starts. They are roughly ordered by how much they would
change the design if answered differently.

### 8.1 File name: `init.ts` vs alternatives

Candidates considered:

- `init.ts` — matches `init.lua`, `init.el`; immediately familiar.
- `user.ts` — explicit about the layer.
- `config.ts` — pairs with `config.json`, but confusing alongside it.
- `fresh.config.ts` — noisy.

Proposal: **`init.ts`**. But if we later introduce a separate
`keymap.ts`, we should revisit whether `init.ts` is reserved or
user-extensible (Zed has `settings.json` + `keymap.json`, Neovim stuffs
everything into `init.lua` plus optional modules — we default to the
latter).

### 8.2 Should `init.ts` be allowed to **unconditionally** override project config?

The design in §1.3 puts project config above init.ts so teams are
protected from a user accidentally changing tab width on a shared repo.
But there are legitimate opposite cases: "I personally hate trailing
whitespace, strip it regardless of what the project says."

Options:

- **A — Current proposal.** Project wins. Users who want the opposite
  use a plugin (plugins load after project config).
- **B — Split the layer.** §9.1 exposes two setters:
  `editor.setSetting(path, value)` → writes below project,
  `editor.forceSetting(path, value)` → writes above project. Force
  surfaces a distinct badge in the Settings UI.
- **C — Session-layer escape.** A separate
  `editor.setSessionOverride(path, value)` for the "nothing can
  override me" case.

Leaning toward **B**, because it makes the override explicit in user
code and the Settings UI badges remain honest.

### 8.3 What happens when a plugin *also* wants `setSetting`?

Because §9.1 adds `setSetting` to the plugin `EditorAPI` (not to a
separate init-only surface), plugins get it too. That creates a layer
question: are plugin writes above or below project config? Leaning
toward a **plugin layer between init and project** — plugins like
`rust-lsp.ts` already call `registerLspServer`, which effectively
writes config; `setSetting` is the general case of the same thing.
Worth writing up separately.

### 8.4 Async rollback

The rollback in §6.2 is synchronous. If an effect's inverse is async
(unloading a plugin can be), rollback becomes a Promise chain. We need
to decide whether reload is blocking (block the UI until rollback
finishes) or non-blocking (freeze a "reloading…" indicator). Neovim's
Lua blocks; VS Code's extension reload is non-blocking with a toast.
Probably: **blocking with a 500ms-grace spinner**, because init runs
are typically short.

### 8.5 Credentials in `editor.getEnv`

`editor.getEnv("GITHUB_TOKEN")` is the obvious way for users to pipe
secrets into LSP config (see §7.9). But then dry-run captures that
value into effect logs and snapshot files. We should either:

- Mark specific env-var names (configurable, defaults include `*TOKEN*`,
  `*SECRET*`, `*KEY*`) as tainted and redact them from logs/snapshots, or
- Add `editor.getSecret("NAME")` that returns an opaque handle only
  `setSetting` / `registerLspServer` accept — never serialised.

The taint approach is simpler; the secret-handle approach is safer.
No strong preference yet.

### 8.6 Interaction with the Settings UI

Every setting already has a source badge: `(user)`, `(project)`,
`(session)`. Adding `(init)` is straightforward, but: what happens when
the user toggles a setting in the UI that was written by `init.ts`?

Options:

- **Persist to session layer.** Non-durable; next start, init.ts wins
  again. Confusing ("I changed it but it reverted").
- **Persist to user `config.json`.** Then the session has *two* writers
  for the same key; init.ts still wins on the next start.
- **Block the toggle with a "This is controlled by init.ts:42. Open
  init.ts?" dialog.** Matches IDE behaviour for policy-locked settings.

Leaning toward the third option — it keeps users aware that their
imperative config is in charge and sends them to the right place.

### 8.7 Dry-run fidelity for `editor.*` calls

Dry-run (§5.1) mocks `editor.spawnProcess`. But `init.ts` is allowed to
read real files via `editor.readFile`, and branches can depend on that
content. Dry-run must document clearly what is and isn't faithful to
a real run. A "strict" dry-run that errors on any non-deterministic
call may be worth having.

### 8.8 Discoverability for brand-new users

If `init.ts` is an invisible file the user has to know exists, we've
failed. Proposed discovery surfaces:

- First-launch tip: *"Tip: You can customise Fresh programmatically
  with `init.ts`. [Learn more] [Create it]"* — shown once, dismissible.
- Settings UI footer: *"Customise with init.ts…"* button next to
  "Edit config.json".
- Command palette: `init:` commands are prefix-searchable from day one
  even if the file doesn't exist.

Need UX input on whether the first-launch tip is worth the noise for
users who'd never touch it anyway.

---

## 9. Required API Additions

The plugin API in `crates/fresh-editor/plugins/lib/fresh.d.ts` already
covers much of what init.ts needs — `getEnv`, `getCwd`, `applyTheme`,
`registerLspServer`, `disableLspForLanguage`, `loadPlugin`,
`registerCommand`, `on`, `registerHandler`. The §7 examples use that
real API verbatim.

What the API does **not** yet cover — and every `/* PROPOSED */`
marker in §7 points here — is five additions to the existing
`EditorAPI` interface. They are not init.ts-specific; any plugin
benefits from them.

### 9.1 Per-setting runtime writes (P0 — blocking)

**Problem.** `editor.getConfig()` / `editor.getUserConfig()` return the
merged config as `unknown`; `editor.reloadConfig()` reloads from disk.
There is no API to *write* a single setting or a partial patch at
runtime. Seven of the ten §7 examples depend on this.

The existing API has narrow setters for specific domains —
`registerLspServer`, `registerLanguageConfig`, `registerGrammar`,
`applyTheme`. What's missing is the general case: "set
`editor.diagnostics_inline_text` to `false` for this session."

**Proposal.** Three methods on `EditorAPI`:

```typescript
/**
 * Set a single config setting for the lifetime of this session.
 * Path is dot-separated, e.g. "editor.tab_size" or "lsp.rust.enabled".
 * The write lands in an in-memory "runtime" layer (between User and
 * Project; see §1.3). It does not modify any file on disk.
 * Returns false if the path is unknown or the value fails schema check.
 */
setSetting(path: string, value: unknown): boolean;

/**
 * Apply a partial config patch (same shape as config.json) to the
 * runtime layer. Deep-merges; lists replace wholesale (matches
 * docs/configuration/ layer semantics).
 */
applyConfigPatch(partial: unknown): boolean;

/**
 * Read a single merged setting by path. Shallow convenience over
 * getConfig(); returns `unknown` so the caller types it locally.
 */
getSetting(path: string): unknown;
```

**Open questions.**
- Writes are session-only by design — removing `init.ts` is a full
  undo. An alternative (persist to `user` layer) is more permanent
  but harder to reason about, and duplicates the Settings UI.
- Attribution: every `setSetting` call needs to record source file +
  line so the Settings UI can show `(init.ts:27)`; the existing
  `(user)` / `(project)` / `(session)` badges (`docs/configuration/`)
  gain an `(init)` source.

### 9.2 Startup-context snapshot (P1)

**Problem.** Every §7 example reconstructs an environment snapshot
from a pile of `editor.getEnv(...)` calls. Hostname, SSH detection,
dark-mode heuristic, platform — all derivable but error-prone. Fresh
already has most of this information during startup.

**Proposal.**

```typescript
interface StartupContext {
  /** "linux" | "macos" | "windows" | "freebsd" | "other" */
  platform: string;
  hostname: string;
  /** SSH_TTY or SSH_CONNECTION is set */
  isSsh: boolean;
  /** stdin is a TTY and Fresh is in normal interactive mode
   *  (not $GIT_EDITOR, not a pager, not piped) */
  isInteractive: boolean;
  /** Best-effort terminal-background inference; null when unknown */
  isDark: boolean | null;
  /** Raw values where available */
  term: string;
  colorterm: string | null;
  termProgram: string | null;
}

getStartupContext(): StartupContext;
```

Lets every §7 example replace `editor.getEnv("SSH_TTY")` with the more
reliable `editor.getStartupContext().isSsh`.

**Open questions.**
- What `isDark` heuristic? Candidates: `COLORFGBG`, OSC 11 query, macOS
  appearance API, Windows registry. Fine to start with env-var only and
  document the gaps.
- Resist adding `onBattery` / `memoryMb` / `cpuCount` here — that
  pushes the editor into "system monitor" territory. Plugins that need
  those can `spawnProcess`.

### 9.3 Pre-plugin-load gating (P1)

**Problem.** §7.1 (conditional vi-mode) currently loads the plugin
*after* startup via `editor.loadPlugin(path)`. Backwards: Fresh has
already initialised, mode tables are empty for an instant, plugin load
races other startup work. The natural shape is "decide before the
plugin loader runs."

**Proposal.**

```typescript
/**
 * Gate a plugin's inclusion in this session's plugin load list.
 * Must be called during the init phase (before plugins load); a no-op
 * afterwards. Affects auto-load only — explicit loadPlugin() calls
 * remain as-is.
 */
setPluginEnabled(pluginId: string, enabled: boolean): boolean;
```

`§7.1` becomes:

```typescript
if (editor.getEnv("GIT_EDITOR")) {
  editor.setPluginEnabled("vi_mode", false);
}
```

Race-free and symmetric with "the plugin was never loaded" rather
than "the plugin loaded then unloaded itself".

**Open questions.**
- Does a `setPluginEnabled(id, true)` re-enable an installed plugin
  that the user previously disabled via a `pkg:` command? Probably no
  — "installed, user-disabled" is a persistent decision that init.ts
  shouldn't silently override. Surface a warning in that case.

### 9.4 Project-root resolution (P2)

**Problem.** §7.5 wants "am I in my work monorepo?" and has to
reconstruct project root from `getCwd()` plus walking up for a marker.
Fresh already does this internally to find `.fresh/config.json`.

**Proposal.**

```typescript
/**
 * The resolved project root for this launch (directory containing
 * .fresh/config.json or a recognised project marker), or null if none.
 */
getProjectRoot(): string | null;
```

Removes a duplication/bug source; small addition.

### 9.5 Effect-tracked rollback (P2)

**Problem.** §6's reload story ("revert + re-run") needs per-call
knowledge of what the previous value was. `registerCommand` / `on` /
`registerHandler` are already revert-able by name. `registerLspServer`
overwrites the prior registration; the runtime doesn't keep what it
replaced.

**Proposal.** No public API change. The init runtime wraps the
`editor.*` object it hands to init.ts in a proxy that records, per
mutating call, `(path, prior-value, new-value, source-line)`. Reload
replays the list in reverse before running the new file. Methods to
track:

- `registerCommand` / `unregisterCommand`
- `on` / `off`
- `registerHandler`
- `registerLspServer` / `disableLspForLanguage` / `restartLspForLanguage`
- `registerGrammar` / `registerLanguageConfig`
- `applyTheme`
- `loadPlugin` / `unloadPlugin`
- `setSetting` / `applyConfigPatch` (after §9.1)
- `setPluginEnabled` (after §9.3)

Plugins continue to use the raw `editor.*` with no change.

### 9.6 Priority summary

| Addition | Required for | Priority |
|---|---|---|
| §9.1 `setSetting` / `applyConfigPatch` / `getSetting` | Most §7 examples; without it init.ts is limited to theme + LSP + plugin gates | **P0 (blocking)** |
| §9.2 `getStartupContext` | Quality-of-life; otherwise 5–10 `getEnv` calls per file | **P1** |
| §9.3 `setPluginEnabled` | Clean pre-load plugin gating | **P1** |
| §9.4 `getProjectRoot` | Project-path branching without walking dirs | **P2** |
| §9.5 Effect tracking | Clean reload/revert (§6) | **P2** |

The design *works* without §9.4 and §9.5 (those are polish), and
without §9.2 and §9.3 (users get the same outcome with more
boilerplate and one extra frame of startup ordering). **§9.1 is the
real blocker**: without runtime setting writes, half of §7 cannot be
implemented regardless of how init.ts is wrapped.

---

## Appendix: Summary of new surfaces

| Surface | Introduced for |
|---|---|
| `~/.config/fresh/init.ts` | The user file itself |
| `~/.config/fresh/init/` | Private module directory (not auto-loaded) |
| `~/.config/fresh/types/` | Ambient types written by Fresh on first run |
| `~/.config/fresh/tsconfig.json` | LSP/IDE wiring, also user-owned |
| `~/.config/fresh/logs/init.log` | Current-session diagnostics |
| `~/.config/fresh/logs/init.last.json` | Effects of the last successful run |
| `~/.config/fresh/logs/init.history/` | Rolling per-run history |
| `~/.config/fresh/logs/init.crashes` | Crash-fuse counter |
| API: `setSetting` / `applyConfigPatch` / `getSetting` (§9.1) | Runtime config writes |
| API: `getStartupContext` (§9.2) | One-call env snapshot |
| API: `setPluginEnabled` (§9.3) | Pre-load plugin gating |
| API: `getProjectRoot` (§9.4) | Project detection |
| `fresh/testing` ambient module | Optional assertion helpers |
| CLI: `fresh --init user [--with …]` | Scaffold |
| CLI: `fresh --cmd init {check,dry-run,status,revert,snapshot}` | Tooling |
| CLI: `fresh --safe`, `fresh --no-init` | Recovery |
| Command palette: `init: Edit / Reload / Status / Revert` | In-editor access |
| Config: `init.{timeout_ms,cache,type_check}` | Knobs |

