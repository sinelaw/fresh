# Setting Up a Plugin Project

This page sets up a plugin project with TypeScript types, autocomplete, and
reload without restarting Fresh.

You need Fresh and Node.js.

## 1. Create the plugin

```bash
fresh --cmd init plugin
cd my-plugin
```

Rename the entry file. The plugin's name is its file name, so every
`plugin.ts` would be called `plugin`.

```bash
mv plugin.ts my_plugin.ts
sed -i 's/"plugin.ts"/"my_plugin.ts"/' package.json   # macOS: sed -i ''
```

## 2. Link the types

Start Fresh once. It writes the API types to `~/.config/fresh/types/`.

Then link that folder into your project:

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

## 4. Install TypeScript

```bash
npm install --save-dev typescript@6
printf 'node_modules/\ntypes\n' > .gitignore
```

Use version 6. Version 7 doesn't work with the language server in step 7.

Check your code:

```bash
npx tsc -p .
```

No output means no errors. A mistake looks like this:

```
my_plugin.ts(18,8): error TS2551: Property 'setStatuss' does not exist on type 'EditorAPI'. Did you mean 'setStatus'?
```

## 5. Load the plugin in Fresh

```bash
mkdir -p ~/.config/fresh/plugins/packages
ln -s "$PWD" ~/.config/fresh/plugins/packages/my-plugin
```

Restart Fresh. Press `Ctrl+P` and run `hello`, the command the template adds.

If your config folder isn't `~/.config/fresh`, run `fresh --cmd config paths`
to find it.

## 6. Reload without restarting

1. Run **init: Edit init.ts** from the command palette.
2. Paste this:

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
       // A failed load removes the plugin, so load it again from the file.
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

3. Save, then run **init: Reload init.ts**.

Now your loop is: edit, save, run **Dev: Reload Plugin**, test. Errors show in
the status bar.

## 7. Type checking inside Fresh (optional)

VS Code and other editors pick up `tsconfig.json` on their own. For Fresh:

1. Install the language server:

   ```bash
   npm install -g typescript-language-server
   ```

2. Run **Workspace Trust…**, press `T`, then OK. Folders with a
   `package.json` start as Restricted, and Restricted blocks language servers.
3. Open your plugin file and run **Start/Restart LSP Server**.

To start the server automatically, add this to `config.json`:

```json
{ "lsp": { "typescript": { "auto_start": true } } }
```

Press `Ctrl+Space` for completion.

## Common problems

- **A file gets loaded as a plugin by mistake.** Fresh loads every `.ts` and
  `.js` file at the top of the plugin folder as a plugin. Put helper files in
  `lib/`.
- **Imports don't work.** **Load Plugin from Buffer** doesn't support
  imports. Use step 6 instead.
- **Your API type is `any` in other plugins.** If you share an API with
  `exportPluginApi`, define its types in the entry file, not in an imported
  file:

  ```ts
  export type MyPluginApi = { count(text: string): { words: number } };
  declare global {
    interface FreshPluginRegistry { "my-plugin": MyPluginApi }
  }
  editor.exportPluginApi("my-plugin", { count } satisfies MyPluginApi);
  ```

- **You can't find your log output.** `setStatus` messages go to
  `status-<pid>.log` and `editor.debug` output goes to `fresh-<pid>.log`.
  Both are in the Logs folder that `fresh --cmd config paths` prints.

## More help

```bash
fresh --cmd script api <query>   # search the API, e.g. getBufferText
fresh --cmd help plugin          # rules of the plugin runtime
```

See also: [Plugin Development](./index.md) · [Common Patterns](./patterns.md) ·
[API Reference](../api/)
