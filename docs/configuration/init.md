# Startup Script (`init.ts`)

Fresh auto-loads `~/.config/fresh/init.ts` on startup. It's a TypeScript file that runs once with full access to the plugin API, and it complements the declarative config — it doesn't replace it.

## When to use which

The declarative config (`config.json`, the Settings UI, keybindings editor, theme selector) and `init.ts` cover different needs. Reach for the declarative side first:

- **Static preferences** (tab size, line numbers, default theme) — `config.json` / Settings UI.
- **Key bindings** — Keybinding editor.
- **A reusable feature** you'd want on more than one machine, or want to share — publish a plugin package.

Reach for `init.ts` when the decision depends on *where or how Fresh is starting*, and can't be baked into a shared config file without lying to the next person who opens it:

- Different settings when launched over SSH vs. locally.
- Host-specific tool paths (e.g. `rust-analyzer` lives in a different prefix on your work laptop).
- Environment-driven profiles (`FRESH_PROFILE=writing fresh` → wrap at 80, turn off diagnostics).
- Extending a bundled plugin with your own data — e.g. adding a custom section to the [Dashboard](/features/dashboard).
- One-off startup effects — e.g. fade in to your theme on launch.

## Complementary with plugins

If you're building something reusable, prefer a plugin — it's installable, shareable, and gets the plugin lifecycle for free.

Sometimes a plugin is the right unit, but part of its behavior only makes sense at startup or depends on the environment. In that case expose the knobs as a plugin API and *call them from `init.ts`*. Plugin APIs are typed automatically — `editor.getPluginApi("dashboard")` returns the right interface or `null`, no `as`-cast needed:

```ts
// In init.ts — plug the parts together for this machine.
editor.on("plugins_loaded", () => {
  // Add a custom section to the Dashboard plugin.
  const dash = editor.getPluginApi("dashboard");
  if (dash) {
    dash.registerSection("todo", async (ctx) => {
      ctx.kv("open", "3", "warn");
      ctx.newline();
    });
  }

  // Configure another plugin for this environment.
  const todo = editor.getPluginApi("todo-highlighter");
  if (todo) todo.configure({ tags: ["TODO", "FIXME", "HACK"] });
});
```

That way the plugin stays declarative and shareable, and the environment-specific glue lives in the one file that isn't meant to be shared.

## Small examples

```ts
// Calmer UI over SSH. setSetting writes to a runtime layer — nothing
// is persisted, so removing this file is a complete undo.
if (editor.getEnv("SSH_TTY")) {
  editor.setSetting("editor.diagnostics_inline_text", false);
  editor.setSetting("terminal.mouse", false);
}

// Host-specific LSP path.
if (editor.getEnv("HOSTNAME") === "work-mac") {
  editor.registerLspServer("rust", {
    command: "/opt/homebrew/bin/rust-analyzer",
    args: [],
    autoStart: true,
  });
}

// Env-driven profile: FRESH_PROFILE=writing fresh
if (editor.getEnv("FRESH_PROFILE") === "writing") {
  editor.setSetting("editor.line_wrap", true);
  editor.setSetting("editor.wrap_column", 80);
}
```

## Editing and reloading

- **`init: Edit`** from the command palette opens (or creates) `~/.config/fresh/init.ts` with a starter template. The same command also writes `types/fresh.d.ts` and a `tsconfig.json` so LSP gives you completions against the real plugin API.
- **`init: Reload`** re-runs the file without restarting Fresh.
- **`init: Check`** type-checks without running.
- **`fresh --no-init`** (alias `--safe`) skips loading for a single launch — useful if the file errors out.

The full API surface is the same as plugins — see the [Plugin API reference](/plugins/api/).

See it in action: [What's New in 0.3.0 → init.ts](/blog/fresh-0.3.0/#init-ts-a-startup-script).
