# Plugin Utilities Library

Fresh's bundled plugins share a small TypeScript library in the `plugins/lib/` folder of the [Fresh repository](https://github.com/sinelaw/fresh/tree/master/crates/fresh-editor/plugins/lib). Bundled plugins import it by relative path:

```typescript
import { Finder } from "./lib/finder.ts";
import { PanelManager, NavigationController } from "./lib/index.ts";
```

The library isn't installed with Fresh, so a plugin of your own can't import it from there. To use a helper, copy its file (and the files it imports, such as `types.ts`) into your plugin's `lib/` folder, delete the `/// <reference path="./fresh.d.ts" />` line at the top of each copy, and import it the same way:

```typescript
import { PanelManager } from "./lib/panel-manager.ts";
```

## What's in it

| Export | File | Use it for |
|--------|------|------------|
| `Finder` | `finder.ts` | "Find something and go to it": a search prompt or results panel with fuzzy filtering and preview |
| `PanelManager` | `panel-manager.ts` | Opening, updating and closing a results panel in a split |
| `NavigationController` | `navigation-controller.ts` | Tracking the selected item in a list |
| `createVirtualBufferFactory` | `virtual-buffer-factory.ts` | Creating virtual buffers with sensible defaults |
| Widgets (`row`, `col`, `button`, `textInput`, `WidgetPanel`, …) | `widgets.ts` | Declarative panel and dialog UI |
| Types (`RGB`, `Location`, `PanelOptions`, `NavigationOptions`, …) | `types.ts` | Shared types |

## Example

A results panel with a selectable list:

```typescript
import { PanelManager } from "./lib/panel-manager.ts";
import { NavigationController } from "./lib/navigation-controller.ts";
const editor = getEditor();

type Hit = { file: string; line: number; text: string };
const panel = new PanelManager(editor, "*Results*", "my-results");
const nav = new NavigationController<Hit>(editor, { itemLabel: "Result" });

registerHandler("my_show_results", async () => {
  const hits: Hit[] = [{ file: "src/main.rs", line: 42, text: "found match" }];
  nav.setItems(hits);
  await panel.open({
    entries: hits.map((h) => ({
      text: `${h.file}:${h.line}: ${h.text}\n`,
      properties: { file: h.file, line: h.line },
    })),
    ratio: 0.7, // the panel gets the bottom 30%
  });
});
registerHandler("my_next_result", () => nav.next());
registerHandler("my_close_results", () => panel.close());

editor.defineMode("my-results", [["n", "my_next_result"], ["q", "my_close_results"]], true);
editor.registerCommand("My Plugin: Show Results", "Show results", "my_show_results");
```

- `PanelManager`: `open({ entries, ratio })` opens the panel (or updates it if it's open), `updateContent(entries)` replaces its text, `close()` closes it, and `isOpen` tells you whether it's showing.
- `NavigationController`: `setItems(items)`, then `next()`, `prev()`, `first()`, `last()`; read the choice with `selected` and `selectedIndex`.

Each file documents its API at the top. See [Common Patterns](./patterns.md) for doing the same with the plain `editor` API.
