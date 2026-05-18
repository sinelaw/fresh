# Plugin Configuration

Plugins can expose user-configurable settings that appear in the editor's
Settings UI under their own "Plugin: \<name\>" category. The values are
saved alongside the rest of the user config and respect the same
User/Project/Session layering as built-in settings.

## How it works

Each setting is declared from TypeScript by calling one of the
strongly-typed `editor.defineConfigX(...)` methods. The host validates
the call synchronously — a typo or wrong shape throws an exception
right there, at the call site, instead of failing silently later. The
return type is inferred from the call, so plugin code is type-safe
without any cast.

```ts
const autoEnable = editor.defineConfigBoolean("autoEnable", {
  default: false,
  description: "Auto-enable the plugin on startup",
});

const maxItems = editor.defineConfigInteger("maxItems", {
  default: 5,
  minimum: 1,
  maximum: 50,
});

const mode = editor.defineConfigEnum("mode", {
  values: ["normal", "insert"] as const,
  default: "normal",
});

const patterns = editor.defineConfigStringArray("patterns", {
  default: ["TODO", "FIXME"],
});

// `mode` is typed as `"normal" | "insert"` thanks to the `as const`.
// `autoEnable: boolean`, `maxItems: number`, `patterns: string[]`.
```

Each call returns the **current** merged value (user override if any,
otherwise the declared `default`). For values that may change while the
plugin is running, re-read on demand:

```ts
const cfg = editor.getPluginConfig() as { autoEnable?: boolean };
if (cfg.autoEnable) { /* ... */ }
```

## Available methods

| Method                          | TS return type     | Settings UI widget |
| ------------------------------- | ------------------ | ------------------ |
| `defineConfigBoolean`           | `boolean`          | Toggle             |
| `defineConfigInteger`           | `number`           | Number input       |
| `defineConfigNumber`            | `number`           | Number input       |
| `defineConfigString`            | `string`           | Text input         |
| `defineConfigEnum`              | one of `values`    | Dropdown           |
| `defineConfigStringArray`       | `string[]`         | List editor        |

All methods accept `description` (string). Numeric methods accept
`minimum` / `maximum`. Each method's `options` parameter is strictly
typed — unknown keys (`defualt` → "Did you mean 'default'?"), wrong
default types, and out-of-range defaults are all compile-time errors.

The Settings UI renders one **Plugin: \<name\>** top-level category per
enabled plugin that has registered at least one field, sorted alongside
the built-in categories.

## What's not supported

By design:

- **Colors** are not exposed as plugin settings. Plugins should pass
  theme keys (e.g. `"ui.status_bar_fg"`, `"editor.diff_add_bg"`) to
  `addOverlay` / `addVirtualLine` instead, so the colors track the
  user's active theme.
- **`x-enum-from`** pointing at host config paths like `/languages`.
  Plugins that need to react to host config should read
  `editor.getConfig()` directly at use-time.
- **Per-language settings.** Plugin config is flat, namespaced by
  plugin name.

## Layering

Plugin settings follow the same layered-config rules as the rest of the
editor:

- Higher layers (Session > Project > User) win for any individual key.
- Object-typed values are deep-merged so a project can add one key
  without restating the user's defaults.
- Primitive values and arrays are replaced wholesale.

## Lifecycle

- **Disabled plugins** are hidden from the Settings UI. Stored values
  are preserved in the config file and reappear when re-enabled.
- **Uninstalling a plugin** leaves its `plugins.<name>.settings` entry
  in the user's config (cheap; harmless). Reinstalling restores the
  values.
- **Reloading a plugin** re-runs its `defineConfigX(...)` calls, so
  plugin authors iterating with `Reload Plugin` see updated schemas
  immediately.
- The Settings UI rebuilds its category list every time it opens, so
  newly-registered schemas show up without an editor restart.
