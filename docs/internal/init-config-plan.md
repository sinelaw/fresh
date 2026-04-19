# init.ts — Implementation Status

> Tracks the design in [init-config-design.md](init-config-design.md).
> Plugin-side primitives (PTY, raw-ANSI virtual buffers, floating windows,
> treesitter, animation timers) are tracked in #1529.

## Implemented (all verified via e2e + unit tests, tested in tmux)

### M0 — Startup auto-load & safety

- `~/.config/fresh/init.ts` auto-loaded at startup via existing
  `load_plugin_from_source("init.ts", ...)` pipeline.
- CLI flags: `--safe` (skip init.ts + plugins), `--no-init` (skip
  init.ts only).
- Crash fuse at `~/.config/fresh/logs/init.crashes` (3 failures /
  300s rolling window → auto-skip on next launch).
- Errors surface in status bar; editor continues.

### M1 — `setSetting(path, value)`

Fire-and-forget into shared Config, matching Neovim/VS Code/Emacs/
Sublime (no editor tracks per-plugin setting writes). Patches Config
via JSON round-trip with schema validation. Any plugin can call it;
last write wins; writes persist until overwritten or editor restart.

### M2 — Lifecycle events

- `editor.on(event, fn)` closure overload via JS shim (alongside
  the existing string-handler form).
- New events: `plugins_loaded` (after all plugins + init.ts load)
  and `ready` (after session restore, before event loop).

### M3 — Plugin-API plane

- `exportPluginApi(name, api)` / `getPluginApi(name)` backed by
  `Persistent<Object>` shared across QuickJS contexts.
- `Drop` impl on `QuickJsBackend` clears persistents before runtime
  teardown.
- `editor.pluginName()` accessor.

### M4 — Palette commands

- `init: Reload` — re-runs init.ts via hot-reload; fires
  `plugins_loaded` afterwards.
- `init: Revert` — unloads init.ts plugin (commands, handlers,
  events, APIs gone; setSetting writes persist per fire-and-forget
  model).

### M5 — Check command & FRESH_INTERACTIVE

- `fresh --cmd init check` — oxc parse, line:col diagnostics,
  non-zero exit on errors.
- `FRESH_INTERACTIVE=1` set on process env for interactive launches.

### M6 — Discoverability

- `init: Edit init.ts` — creates starter template if missing, opens
  in buffer.
- `init: Check init.ts` — in-editor parse check via status bar.
- Starter template embedded in binary with commented examples.

## Not yet implemented

| Item | Notes |
|---|---|
| Type scaffolding (copy `fresh.d.ts` + write `tsconfig.json` to `~/.config/fresh/types/`) | Needed for IntelliSense in init.ts. High impact. |
| `types/fresh-config.d.ts` generated from `config-schema.json` | Would type-check `setSetting` paths at compile time. |
| Scope-discipline lints (`unconditional-preference`, `unconditional-plugin-load`) | Needs AST walker tracing data-flow from `getEnv`→`setSetting`. |
| `--strict` mode for `fresh --cmd init check` (`tsc --noEmit`) | Small addition, gated on tsc availability. |
| Per-plugin types (`types/plugins/<name>.d.ts`) auto-copied on install | Enables typed `getPluginApi` calls. |
| `init: Status` palette command | Show what init.ts did (settings, hooks, commands). |
| User docs (`docs/configuration/init.md`) | |
| Plugin-author docs for `exportPluginApi` | |

## Test coverage

- 14 e2e tests in `tests/e2e/plugins/init_script.rs`
- 10 unit tests in `src/init_script.rs`
- Verified end-to-end in tmux (init.ts → setSetting → line numbers
  toggled by `$TMUX` detection; `--no-init` restores defaults)
