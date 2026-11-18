# Dashboard

> **Palette:** `Show Dashboard`.
>
> **Settings:** **Plugins → dashboard** in the Settings UI.

Fresh includes a built-in TUI dashboard plugin that can take the place of the empty `[No Name]` buffer when nothing else is open. By default it shows git status and repo URL, a "vs master" row (commits ahead/behind), and disk usage for common mounts. Weather and recent GitHub PRs are bundled but opt-in — see [Built-in opt-in widgets](#built-in-opt-in-widgets) below.

## Opening the Dashboard

The dashboard plugin is loaded by default, but the dashboard doesn't open by itself: run `Show Dashboard` from the command palette whenever you want it.

To have it open by itself when Fresh starts with no files open and after the last buffer is closed, turn on **AutoOpen** in the Settings UI (**Open Settings** → **Plugins** → **dashboard**), or set it in `config.json`:

```json
{
  "plugins": {
    "dashboard": { "settings": { "autoOpen": true } }
  }
}
```

To unload the plugin entirely, turn it off in the **Plugins** list of the Settings UI (`plugins.dashboard.enabled`).

## Tips

- The dashboard only renders in buffers that have no file attached, so opening any file replaces it — you don't need to close it manually.
- `git` must be on `PATH` for the git and "vs master" rows to populate.
- **Keyboard navigation** — `Tab` / `Down` / `j` step to the next clickable row, `Shift+Tab` / `Up` / `k` step back, `Enter` activates. Mouse clicks still work.

## Built-in opt-in widgets

`weather` and `github` ship with the plugin but aren't registered by default — both hit the network on every refresh, so opting in is explicit. Their refresh handlers live on the plugin API as `builtinHandlers`; pass either to `registerSection` from your [`init.ts`](../configuration/init.md):

```ts
editor.on("plugins_loaded", () => {
  const dash = editor.getPluginApi("dashboard");
  if (!dash) return;
  dash.registerSection("weather", dash.builtinHandlers.weather);
  dash.registerSection("github", dash.builtinHandlers.github);
});
```

The weather section uses your IP-based location in metric units by default; set **WeatherLocation** and **WeatherUnits** (Metric or Imperial) on the dashboard's Settings page to change either.

The GitHub section shows open PRs for the *current repo* (detected from the `origin` remote). Outside a GitHub clone, it renders a short explanatory message instead. If either widget can't reach its endpoint, the section surfaces a one-line error rather than blocking the rest of the dashboard.

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

The `ctx` parameter exposes `kv`, `text`, `newline`, and `error` primitives. Colors are symbolic (`"muted"`, `"accent"`, `"value"`, `"number"`, `"ok"`, `"warn"`, `"err"`, `"branch"`), so sections pick up theme changes automatically. `onClick` is routed through the editor's mouse-click dispatcher and works even in terminals that strip OSC-8 hyperlinks.

`registerSection` returns a function you can call to remove that one section later; `dash.clearAllSections()` drops every section a plugin has registered. Call these when your plugin unloads so hot-reload doesn't leave stale rows.

See it in action: [What's New in 0.3.0 → Dashboard](/blog/fresh-0.3.0/#dashboard).
