# Setting Up a Plugin Project

You need Fresh and Node.js.

## 1. Create the plugin

```bash
fresh --cmd init plugin
```

Enter a name, such as `my-plugin`. The command then:

- creates the `my-plugin/` folder with `my-plugin.ts`, `package.json`, and `tsconfig.json`
- links Fresh's API types into `my-plugin/types/`
- installs TypeScript with `npm install`
- loads the plugin in Fresh
- trusts the folder, so the TypeScript language server can run

It prints one line per step. A line starting with `✗` or `!` tells you what
to do.

## 2. Open it

```bash
cd my-plugin
fresh my-plugin.ts
```

Press `Ctrl+P` and run **my-plugin: Say Hello**. The status bar shows
`Hello from my-plugin!`.

## 3. Edit and reload

1. Edit `my-plugin.ts`.
2. Save with `Ctrl+S`.
3. Press `Ctrl+P` and run **Load Plugin from Buffer**.

The status bar shows `Plugin 'my-plugin' reloaded from my-plugin.ts`, or the
error if loading failed. You don't need to restart Fresh.

## 4. Check types

Inside Fresh, errors show as you type. Press `Ctrl+Space` for completion.
This needs the TypeScript language server:

```bash
npm install -g typescript-language-server
```

From the shell:

```bash
npx tsc -p .
```

No output means no errors.

## What the command changes outside the folder

- `~/.config/fresh/plugins/packages/my-plugin` is a link to your folder.
  Delete it to stop loading the plugin.
- The folder is marked as trusted. Change this with **Workspace Trust…**.

Run `fresh --cmd config paths` to see where your config folder is.

## Common problems

- **A file gets loaded as a plugin by mistake.** Fresh loads every `.ts` and
  `.js` file at the top of the plugin folder as a plugin. Put helper files in
  `lib/` and import them:

  ```ts
  import { countWords } from "./lib/count.ts";
  ```

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
