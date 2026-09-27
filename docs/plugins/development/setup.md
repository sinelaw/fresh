# Setting Up a Plugin Project

This page sets up a plugin project with full TypeScript support: autocomplete,
hover docs and type errors for the whole `editor` API, plus a way to reload
your changes without restarting Fresh. Setup takes about five minutes. You need
Fresh and Node.js (for `npm`).

## 1. Scaffold the package

```bash
fresh --cmd init plugin          # asks for a name, description and author
cd my-plugin
```

Fresh names a plugin after its entry file, so `plugin.ts` is loaded as a plugin
called `plugin`. Give yours a distinctive name, and update `"entry"` in
`package.json` to match:

```bash
mv plugin.ts my_plugin.ts
sed -i 's/"plugin.ts"/"my_plugin.ts"/' package.json   # macOS: sed -i ''
```

## 2. Link the type definitions

Fresh writes its API declarations to `~/.config/fresh/types/` every time it
starts, so they always match the Fresh version you have installed:

- `fresh.d.ts` is the full plugin API.
- `plugins.d.ts` holds the APIs other plugins publish, so
  `editor.getPluginApi("dashboard")` is typed.

Start Fresh once so these files exist, then link the folder into your project:

```bash
ln -s "$(dirname "$(fresh --cmd script types | head -n1)")" types
```

## 3. Add `tsconfig.json`

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "moduleResolution": "bundler",
    "moduleDetection": "force",
    "allowImportingTsExtensions": true,
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true,
    "lib": ["ES2020"],
    "types": []
  },
  "include": ["*.ts", "lib/**/*.ts", "types/fresh.d.ts", "types/plugins.d.ts"]
}
```

Plugins run in QuickJS, not Node or a browser, so there is no DOM and there
are no Node types. `allowImportingTsExtensions` lets you write
`import { x } from "./lib/x.ts"`, which is the style the bundled plugins use.

## 4. Install TypeScript and check the plugin

```bash
npm install --save-dev typescript@6
printf 'node_modules/\ntypes\n' > .gitignore
npx tsc -p .
```

Use TypeScript 6. TypeScript 7 doesn't ship `tsserver`, which the
TypeScript language server needs for in-editor checking (step 7). Adding
`devDependencies` to `package.json` is fine, because the Fresh manifest schema
allows it.

`npx tsc -p .` should print nothing. Try a typo to see it working:

```
my_plugin.ts(18,8): error TS2551: Property 'setStatuss' does not exist on type 'EditorAPI'. Did you mean 'setStatus'?
```

## 5. Load it into Fresh

Fresh loads every folder under `~/.config/fresh/plugins/packages/`, so symlink
your project there:

```bash
mkdir -p ~/.config/fresh/plugins/packages
ln -s "$PWD" ~/.config/fresh/plugins/packages/my-plugin
```

Restart Fresh, and your commands appear in the command palette (`Ctrl+P`).
The scaffold registers one called `hello`. Run `fresh --cmd config paths` if
your config directory isn't `~/.config/fresh`.

## 6. Reload without restarting

Add a reload command to your `init.ts`. To open the file, run
**init: Edit init.ts** from the command palette. Then run
**init: Reload init.ts** to apply it.

```ts
const editor = getEditor();

const DEV_PLUGIN = "my_plugin"; // entry file name without .ts
const DEV_ENTRY = editor.pathJoin(
  editor.getConfigDir(), "plugins", "packages", "my-plugin", "my_plugin.ts",
);

registerHandler("dev_reload_plugin", async () => {
  try {
    await editor.reloadPlugin(DEV_PLUGIN);
  } catch {
    // The last load failed, so the plugin is no longer registered.
    try {
      await editor.loadPlugin(DEV_ENTRY);
    } catch (e) {
      editor.setStatus(`Reload failed: ${e}`);
      return;
    }
  }
  editor.setStatus(`Reloaded ${DEV_PLUGIN}`);
});
editor.registerCommand("Dev: Reload Plugin", "Reload the plugin under development", "dev_reload_plugin");
```

Your loop is now: edit, save, **Dev: Reload Plugin**, try it. The reload
removes the old copy's commands, handlers and event subscriptions first, picks
up changes in imported files too, and shows load errors in the status bar.

## 7. Type checking inside Fresh (optional)

Any editor that understands `tsconfig.json` works, VS Code included. To get
the same checks inside Fresh:

```bash
npm install -g typescript-language-server
```

Then, in Fresh:

1. **Trust the folder.** Because the project has a `package.json`, Fresh opens
   it as *Restricted*, which blocks language servers. Run **Workspace Trust…**
   from the palette, press `T`, then OK.
2. **Start the server.** Open your entry file and run
   **Start/Restart LSP Server**. The TypeScript server doesn't start on its
   own. To start it every time, add this to `config.json`:

   ```json
   { "lsp": { "typescript": { "auto_start": true } } }
   ```

You then get diagnostics, hover docs, and completion with `Ctrl+Space`.

## Things that will bite you

- **Every top-level `.ts` and `.js` file in the package folder is loaded as its own plugin.**
  Put helper modules in a subfolder such as `lib/`, and keep tool config files
  like `eslint.config.js` out of the root.
- **Load Plugin from Buffer** is only for single-file experiments. It doesn't
  resolve `import`s, and it fails with "already registered" if the same plugin
  is also installed.
- **Keep exported API types self-contained.** If you publish an API with
  `exportPluginApi`, define the types it uses in the entry file.
  `plugins.d.ts` copies your declarations, but relative imports inside it
  don't resolve, so an imported type turns into `any` for anyone consuming
  your API:

  ```ts
  export type MyPluginApi = { count(text: string): { words: number } };
  declare global {
    interface FreshPluginRegistry { "my-plugin": MyPluginApi }
  }
  editor.exportPluginApi("my-plugin", { count } satisfies MyPluginApi);
  ```

- **Finding output:** `editor.setStatus()` messages are also appended to
  `status-<pid>.log`, and `editor.debug()` output goes to `fresh-<pid>.log`.
  Both are in the Logs directory that `fresh --cmd config paths` prints.

## More help from the CLI

```bash
fresh --cmd script api <query>   # search the API, e.g. "getBufferText"
fresh --cmd help plugin          # runtime rules: async, handlers, timers, panels
```

Next: [Plugin Development](./index.md) · [Common Patterns](./patterns.md) ·
[API Reference](../api/)
