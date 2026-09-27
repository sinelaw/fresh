# Setting Up a Plugin Project

You need Fresh and Node.js.

## 1. Create the plugin

```bash
fresh --cmd init plugin
```

Enter a name, such as `my-plugin`. Press Enter to skip the description and
author. The command then:

- creates the `my-plugin/` folder with `my-plugin.ts`, `package.json`, and `tsconfig.json`
- links Fresh's API types into `my-plugin/types/`
- installs TypeScript with `npm install`
- trusts the folder, so the TypeScript language server can run

It does not install the plugin. The plugin runs only when you load it.

It prints one line per step. A line starting with `✗` or `!` tells you what
to do.

## 2. Load it

```bash
cd my-plugin
fresh my-plugin.ts
```

1. Press `Ctrl+P` and run **Load Plugin from Buffer**. The status bar shows
   `Plugin 'my-plugin' loaded from my-plugin.ts`.
2. Press `Ctrl+P` and run **my-plugin: Say Hello**. The status bar shows
   `Hello from my-plugin!`.

Fresh forgets the plugin when it quits. Load it again after each restart.

## 3. Edit and reload

1. Edit `my-plugin.ts`.
2. Save with `Ctrl+S`.
3. Press `Ctrl+P` and run **Load Plugin from Buffer**.

The status bar shows `Plugin 'my-plugin' reloaded from my-plugin.ts`, or the
error if loading failed.

Quit Fresh with `Ctrl+Q`.

## 4. Check types

Inside Fresh, errors show as you type: a `●` next to the line and `E:1` in
the status bar. Run **Show Diagnostics Panel** to read them. Press
`Ctrl+Space` for completion.
This needs the TypeScript language server:

```bash
npm install -g typescript-language-server
```

From the shell:

```bash
npx tsc -p .
```

No output means no errors.

## 5. Use another plugin's API

Plugins can share an API with `exportPluginApi`. Get one with
`editor.getPluginApi`. It's fully typed. This example adds a section to the
built-in dashboard:

```ts
registerHandler("my_plugin_dashboard", () => {
  const dash = editor.getPluginApi("dashboard"); // type: DashboardApi | null
  if (!dash) {
    editor.setStatus("dashboard is not loaded");
    return;
  }
  dash.removeSection("my-plugin"); // don't add it twice
  dash.registerSection("my-plugin", async (ctx) => {
    ctx.kv("buffers", String(editor.listBuffers().length), "number");
  });
  editor.setStatus("Added a section. Run Show Dashboard to see it.");
});
editor.registerCommand("my-plugin: Add Dashboard Section", "Add a section to the dashboard", "my_plugin_dashboard");
```

Load the plugin, run **my-plugin: Add Dashboard Section**, then run
**Show Dashboard**. Your section shows the number of open buffers.

- Call `getPluginApi` inside your handler, not at the top of the file. The
  other plugin may not be loaded yet when your file runs.
- It returns `null` if that plugin isn't loaded. Check before using it.
- The types come from `types/plugins.d.ts`. Fresh rewrites it on every start
  with the APIs of all installed plugins. Search it for `FreshPluginRegistry`
  to see which APIs exist.
- After you install a new plugin, restart Fresh to get its types.

## 6. Share your plugin's API

Define the API type in your entry file, register it in
`FreshPluginRegistry`, and export it:

```ts
export type MyPluginApi = {
  countWords(text: string): number;
};
declare global {
  interface FreshPluginRegistry {
    "my-plugin": MyPluginApi;
  }
}
editor.exportPluginApi("my-plugin", {
  countWords: (text) => text.split(/\s+/).filter(Boolean).length,
} satisfies MyPluginApi);
```

Other plugins get the typed API once your plugin is installed (see below)
and Fresh has restarted.

Define the types in the entry file itself. Types imported from another file
become `any` for other plugins.

## What the command changes outside the folder

- The folder is marked as trusted. Change this with **Workspace Trust…**.
- Fresh's type files are written to `~/.config/fresh/types/`. Fresh also does
  this every time it starts.

## Load it every time Fresh starts

When you're ready to use the plugin every day, link it into your plugins
folder:

```bash
mkdir -p ~/.config/fresh/plugins/packages
ln -s "$PWD" ~/.config/fresh/plugins/packages/my-plugin
```

Delete the link to stop loading it. Run `fresh --cmd config paths` if your
config folder isn't `~/.config/fresh`.

## Common problems

- **Keep helper files in `lib/`.** Once the plugin is installed, Fresh loads
  every `.ts` and `.js` file at the top of its folder as a separate plugin.
  Import helpers from `lib/` instead:

  ```ts
  import { countWords } from "./lib/count.ts";
  ```

- **You can't find your log output.** `editor.setStatus` messages go to
  `status-<pid>.log`, and `editor.debug` output goes to `fresh-<pid>.log`.
  Both are in the Logs folder that `fresh --cmd config paths` prints.

## More help

```bash
fresh --cmd script api <query>   # search the API, e.g. getBufferText
fresh --cmd help plugin          # rules of the plugin runtime
```

See also: [Plugin Development](./index.md) · [Common Patterns](./patterns.md) ·
[API Reference](../api/)
