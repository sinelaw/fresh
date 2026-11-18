# Plugins

Fresh's functionality can be extended with packages:

*   **Plugins:** TypeScript code that extends editor functionality
*   **Themes:** Color schemes for syntax highlighting and UI
*   **Language Packs:** Syntax highlighting, language configuration, and LSP support for new languages
*   **Bundles:** Combine multiple languages and plugins into a single package

Bundled plugins:

*   **TODO Highlighter:** Highlights `TODO`, `FIXME`, and other keywords in your comments.
*   **Git Grep:** Interactively search through your Git repository.
*   **Git Find File:** Quickly find and open files in your Git repository.
*   **Diff Chunk Navigation:** Navigate between diff chunks in the current buffer.

## Load Plugin from Buffer

Run "Load Plugin from Buffer" from the command palette to load or reload the plugin in the current buffer:

- **Saved file:** loads it from disk, including its imports. If the plugin is already running, it reloads it.
- **Unsaved buffer:** loads the buffer text. Imports are not supported.

## Plugin API: `registerHandler()`

Plugins should use `registerHandler()` to register command handlers instead of the older `globalThis` pattern. This provides better type safety and is the recommended approach for all new plugins.

> On macOS, plugins folder needs to live either in the same directory as the binary OR in the directory that fresh is run from. If installed via homebrew, the binary lives in ```/opt/homebrew/bin/fresh```. The simplest, cleanest way to to create a symbolic link in that folder pointing to your plugins. i.e. ``` ln -s /Users/username/freshplugins /opt/homebrew/bin/plugins```

## Package Manager

Fresh includes a built-in package manager for plugins, themes, language packs and bundles. Two commands in the command palette (`Ctrl+P`) cover everything:

| Command | Description |
|---------|-------------|
| `Package: Packages` | Browse, install, update and remove packages |
| `Package: Install from URL` | Install from a git URL, a theme file URL or a local directory |

Installed and updated packages load immediately, without restarting Fresh.

### Browsing and Installing

**Package: Packages** opens the package browser. It lists your installed packages first, then what the registry offers. Packages with a newer version in the registry are marked `↑`.

![The package browser: filter buttons across the top, the registry's packages on the left, and the selected theme's details with an Install action on the right](/images/packages-browser.png)

- Press `/` to search, or use `Tab` to reach the filter buttons (**All**, **Installed**, **Plugins**, **Themes**, **Languages**, **Bundles**).
- Press `Enter` on a package to see its details and actions: **Install**, **Update** or **Uninstall**.
- The registry syncs in the background when the browser opens. The **Sync** button fetches the latest package lists on demand.

### Installing from a URL or Local Directory

**Package: Install from URL** installs a package that isn't in the registry. It accepts:

- **A git repository URL**, e.g. `https://github.com/user/fresh-plugin`.
- **A subdirectory of a repository** (for repositories that hold several packages), using a URL fragment: `https://github.com/user/fresh-plugins#packages/rainbow-brackets`.
- **A single theme file**: a URL ending in `.json`, which is downloaded and installed as a theme.
- **A local directory** containing a `package.json`, e.g. `~/src/my-plugin` (a `#subdir` fragment works here too). Fresh installs a copy, so run the command again after you change the source.

Running it again for a package that is already installed upgrades it in place.

### Updating and Removing

Select an installed package in **Package: Packages** and choose **Update** to re-install it from the source it came from (a registry, git URL or local directory), or **Uninstall** to remove it. Uninstalling unloads a plugin right away and moves the package to the system trash.

### Package Locations

Installed packages are stored in:
- **Plugins:** `~/.config/fresh/plugins/packages/`
- **Themes:** `~/.config/fresh/themes/packages/`
- **Language Packs:** `~/.config/fresh/languages/packages/`
- **Bundles:** `~/.config/fresh/bundles/packages/`

### Registry Sources

By default, Fresh uses the official package registry. You can add additional registries in your config:

```json
{
  "packages": {
    "sources": [
      "https://github.com/sinelaw/fresh-plugins-registry",
      "https://github.com/my-org/private-plugins"
    ]
  }
}
```

Use the **Sync** button in **Package: Packages** to fetch the latest package lists.

## Creating Packages

Use the CLI to scaffold new packages:

```bash
fresh --cmd init           # Interactive mode
fresh --cmd init plugin    # Create a plugin
fresh --cmd init theme     # Create a theme
fresh --cmd init language  # Create a language pack
```

For detailed guides, see:
- [Plugin Development](./development/) - Getting started with plugin development
- [Project Setup](./development/setup.md) - TypeScript types, autocomplete and live reload for a plugin project
- [Language Packs](./development/language-packs.md) - Creating syntax highlighting and LSP support
- [Common Patterns](./development/patterns.md) - Reusable plugin patterns and recipes
- [Utilities Library](./development/utilities.md) - Built-in utility functions for plugins
- [Internationalization](./development/i18n.md) - Adding translations to plugins

### API Reference

- [Core Concepts & Types](./api/) - Buffers, splits, virtual buffers, overlays, modes
- [Status & Logging](./api/status-logging.md) - Status bar and debug logging
- [Buffer Operations](./api/buffer.md) - Reading and modifying buffer content
- [Overlays & Virtual Text](./api/overlays.md) - Visual decorations on buffer text
- [Filesystem & Paths](./api/filesystem.md) - File and directory operations
- [Events & Hooks](./api/events.md) - Subscribing to editor events
- [Virtual Buffers](./api/virtual-buffers.md) - Creating plugin-managed buffers

## Bundles

Bundles combine multiple languages and plugins into a single installable package. Useful for language ecosystems with multiple file types (e.g., Elixir + HEEx templates).

```json
{
  "name": "elixir-bundle",
  "type": "bundle",
  "fresh": {
    "languages": [
      {
        "id": "elixir",
        "grammar": { "file": "grammars/elixir.sublime-syntax", "extensions": ["ex", "exs"] },
        "language": { "commentPrefix": "#" },
        "lsp": { "command": "elixir-ls" }
      },
      {
        "id": "heex",
        "grammar": { "file": "grammars/heex.sublime-syntax", "extensions": ["heex"] }
      }
    ],
    "plugins": [
      { "id": "elixir-tools", "entry": "plugins/tools.ts" }
    ]
  }
}
```

Bundles are shown with a "B" tag in the package manager. `fresh --cmd init` doesn't scaffold bundles; write the `package.json` by hand.

## Clangd helper plugin

Fresh ships `plugins/clangd_support.ts` with the source tree; see `plugins/clangd_support.md` for an overview of the plugin commands and how it surfaces clangd-specific notifications in the status bar.
