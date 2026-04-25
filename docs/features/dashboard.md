# Dashboard

> **Activation:** `plugins.dashboard.enabled` in `config.json`, or in the Settings UI under **Plugins → dashboard**. No palette command.

Fresh includes a built-in TUI dashboard plugin that replaces the default `[No Name]` buffer you see after `fresh` with no arguments. It shows weather info, git status and repo URL, a "vs master" row (commits ahead/behind), recent GitHub PRs for the current repo, and disk usage for common mounts.

## Enabling

The dashboard is off by default. Turn it on from the Settings UI (**Open Settings** → **Plugins** → **dashboard** → **enabled**), or directly in `config.json`:

```json
{
  "plugins": {
    "dashboard": { "enabled": true }
  }
}
```

## Tips

- The dashboard only renders in buffers that have no file attached, so opening any file replaces it — you don't need to close it manually.
- Weather and GitHub widgets need network access; if either is unreachable, the section is quietly hidden rather than blocking the rest of the dashboard.
- `git` must be on `PATH` for the git and "vs master" rows to populate.
- The GitHub section shows open PRs for the *current repo* (detected from the `origin` remote). Outside a GitHub clone, it renders a short explanatory message instead.
- **Keyboard navigation** — `Tab` / `Down` / `j` step to the next clickable row, `Shift+Tab` / `Up` / `k` step back, `Enter` activates. Mouse clicks still work.

## Adding Your Own Sections

Third-party plugins and your [`init.ts`](../configuration/init.md) can contribute their own rows through the dashboard's plugin API:

```ts
editor.on("plugins_loaded", () => {
  const dash = editor.getPluginApi("dashboard");
  if (!dash) return;
  dash.registerSection("todo", async (ctx) => {
    const count = 3;
    ctx.kv("open", String(count), count > 5 ? "warn" : "value");
    ctx.text("open inbox", {
      color: "accent",
      onClick: () => editor.executeAction("open_inbox"),
    });
    ctx.newline();
  });
});
```

The `ctx` parameter exposes `kv`, `text`, `newline`, and `error` primitives. Colors are symbolic (`"muted"`, `"accent"`, `"ok"`, `"warn"`, `"err"`, `"value"`), so sections pick up theme changes automatically. `onClick` is routed through the editor's mouse-click dispatcher and works even in terminals that strip OSC-8 hyperlinks.

`registerSection` returns a function you can call to remove that one section later; `dash.clearAllSections()` drops every section a plugin has registered. Call these when your plugin unloads so hot-reload doesn't leave stale rows.

See it in action: [What's New in 0.3.0 → Dashboard](/blog/fresh-0.3.0/#dashboard).
