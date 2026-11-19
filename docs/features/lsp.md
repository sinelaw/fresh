# LSP Integration

Fresh has native support for the Language Server Protocol (LSP), providing features like:

*   **Real-time diagnostics:** See errors and warnings in your code as you type.
*   **Code completion:** Auto-imports are applied when you accept a completion. Fresh also provides [basic buffer-word completions](./editing.md#basic-completions) without an LSP.
*   **Code actions:** Quick fixes and refactorings, including ones that create or rename files, all through a single popup that merges actions from every configured server. A server is never allowed to delete a file or overwrite an existing one; such a request is refused and reported in the status bar.
*   **Navigation:** Go to Definition (`F12`), Find References (`Shift+F12`), and Go to Implementation (`Ctrl+F12`).
*   **Hover, rename, and signature help.**
*   **Formatting:** "Format Buffer" from the command palette formats the whole file with the configured external formatter, falling back to LSP formatting when none is set. With an active selection it formats only that range via the language server's range formatting (`textDocument/rangeFormatting`) when the server supports it, matching VS Code's "Format Selection".

Every LSP operation is a palette command under its own name (**Go to Definition**, **Find References**, **Rename Symbol**, **Code Actions**, **Show Hover Info**, …); the server-management commands (**Show LSP Status**, **Start/Restart LSP Server**, **Stop LSP Server**) come up when you search for "LSP". Use the [Keybinding Editor](./keybinding-editor.md) to see or change the keys bound to each one.

## Status Bar

The status bar shows a single `LSP` indicator — colour-coded, with a spinner during startup and indexing. Activate it (click, or run **Show LSP Status** from the command palette) to open a popup with per-server status, live progress, per-server actions (start, restart, stop), and a **View Log** entry. Servers that are configured but whose binary isn't on `PATH` are flagged so Fresh doesn't quietly spawn failing processes, and the "not installed" copy is container-aware when you're attached to a devcontainer (it points at the container's PATH, not the host's). You can also turn a language off from the popup (for example **Disable LSP for python**), which is saved to your config.

The indicator reads `LSP (off)` when a server is configured for the file but not running, and `LSP (n/a)` when servers are running but the current file is skipped (for example, it is over the large-file threshold).

![The LSP status popup for Python: pyright-langserver is ready, with Restart, Stop, Disable LSP for python and View Log actions](/images/lsp-status-popup.png)

Requests to a server give up after 30 seconds and say they timed out rather than showing an empty result. If a running server keeps timing out, the indicator reads `LSP (stuck)` in a warning colour; restart the server from the popup.

## Remote-Aware LSP

Language servers spawn through the editor's current [Authority](../plugins/api/), so attaching to an SSH remote or a devcontainer runs the servers over there. The "is it installed" check and the memory and CPU limits (`process_limits`: `max_memory_percent`, `max_cpu_percent`) go through the same authority, so quotas apply whether the server is local or in a container.

## Hover and Diagnostics

Hover popups fuse any overlapping diagnostic with the hover body — severity-coloured and source-tagged (`rustc`, `clippy`, `clangd`, etc.), so you see the error message and the type information together.

![A hover popup showing a Pyright type error above the hovered variable's type](/images/lsp-hover-diagnostic.png)

## Diagnostics Panel

Open the diagnostics panel with "Show Diagnostics Panel" or "Toggle Diagnostics Panel" from the command palette. In the panel, Up/Down scrolls the editor to preview each diagnostic's location; Enter jumps to the diagnostic and focuses the editor. `F8` and `Shift+F8` jump to next/previous diagnostic without the panel.

Diagnostics can also be shown inline at the end of each line — see [Editing — Inline Diagnostics](./editing.md#inline-diagnostics).

## Signature Help

Signature help popups render markdown with proper formatting, hanging indent, and paragraph spacing.

## Code Folding

When the LSP server provides `foldingRange`, fold indicators appear in the gutter. See [Editing — Code Folding](./editing.md#code-folding).

## Multi-Server Support

You can configure multiple LSP servers for the same language (e.g., pylsp + pyright for Python). Configure this in the Settings UI (run **Open Settings** from the palette) under **Syntax & Languages → Language Servers**.

Each server can opt into or out of specific features using `only_features` / `except_features` — for example, route completions to one server and diagnostics to another. Fresh merges completions and hover results from every eligible server and tracks diagnostics per-server. Servers configured for all languages are spawned once per project rather than once per language.

## Disabling LSP

To disable a single server, set `"enabled": false` on its entry in the `lsp` map (Settings UI: **Syntax & Languages → Language Servers**), or pick **Disable LSP for …** in the status-bar popup.

To disable LSP for **all** languages at once, turn off the **Lsp Enabled** master switch on the same Settings page:

![Settings at Syntax & Languages, with the Lsp Enabled master switch selected under Language Servers](/images/settings-lsp-enabled.png)

::: details Show JSON
```json
{
  "lsp_enabled": false
}
```
:::

No language server will auto-start for any language (universal servers included), and the status bar shows a dimmed `LSP (off)` pill when servers are configured for the current language. You can still start a server explicitly with **Start/Restart LSP Server** from the command palette — a manual start overrides the global switch for that language.

## C/C++ Header Routing

When you open a `.h` file, Fresh routes to the C++ LSP if there's a clear signal in the project (a sibling `.cpp`, `.hpp`, or `.hxx`), and to the C LSP otherwise.

## Workspace Root Detection

Fresh walks upward from the file's directory to the nearest folder containing one of the server's `root_markers` (e.g., `Cargo.toml`, `package.json`, `.git`) and uses it as the workspace root. Built-in servers come with suitable markers; with no markers, or no match, the file's own folder is used. Configure this in the Settings UI (run **Open Settings** from the palette) under **Syntax & Languages → Language Servers**.

## Built-in LSP Support

Fresh includes built-in LSP configurations for many languages. Install the server, then start it from the LSP status popup: **Start … (always)** (for example **Start rust-analyzer (always)**) starts it now and whenever you open a file of that language; **Start … once** starts it for this session only.

| Language | LSP Server | Install Command |
|----------|-----------|-----------------|
| Rust | rust-analyzer | `rustup component add rust-analyzer` |
| Go | gopls | `go install golang.org/x/tools/gopls@latest` |
| TypeScript/JavaScript | typescript-language-server | `npm install -g typescript-language-server typescript` |
| Python | pylsp | `pip install python-lsp-server` |
| Java | jdtls | `brew install jdtls` |
| Zig | zls | `brew install zls` |
| LaTeX | texlab | `brew install texlab` |
| Markdown | marksman | `brew install marksman` |
| C/C++ | clangd | `brew install llvm` |
| Kotlin | kotlin-lsp | `brew install JetBrains/utils/kotlin-lsp`, AUR `kotlin-lsp-bin`, or the [standalone archive](https://github.com/Kotlin/kotlin-lsp/releases) |
| Odin | ols | build from source: <https://github.com/DanielGavin/ols> |
| Slang | slangd | ships with the [Slang compiler](https://github.com/shader-slang/slang) |

`kotlin-lsp` is JetBrains' official Kotlin server (Alpha). It imports the
Gradle or Maven project on first start, which can take a while on large
builds, and it does not analyse `.kts` files yet. To keep using fwcd's
`kotlin-language-server`, set `lsp.kotlin.command` to `kotlin-language-server`
and `args` to `[]`.

In a JavaScript/TypeScript project with a `deno.json` or `deno.jsonc` at its root, Fresh uses `deno lsp` instead of `typescript-language-server`, as long as `deno` is on your `PATH`.

## Python LSP Configuration

The default Python server is `pylsp`. To use another one, open **Settings → Syntax & Languages → Language Servers → python**, open the server, and change its **Command** and **Args**:

![The Language Servers entry for python opened in Settings: the server's Command, Enabled, Name, Args, Auto Start and Root Markers, with Env and Language Id Overrides under Advanced](/images/settings-lsp-entry.png)

**Using pyright (recommended for type checking):** Command `pyright-langserver`, Args `--stdio`.

::: details Show JSON
```json
{
  "lsp": {
    "python": {
      "command": "pyright-langserver",
      "args": ["--stdio"],
      "enabled": true
    }
  }
}
```
:::

Install pyright with: `npm install -g pyright` or `pip install pyright`

**Using basedpyright (enhanced pyright fork):** Command `basedpyright-langserver`, Args `--stdio`.

::: details Show JSON
```json
{
  "lsp": {
    "python": {
      "command": "basedpyright-langserver",
      "args": ["--stdio"],
      "enabled": true
    }
  }
}
```
:::

Install basedpyright with: `pip install basedpyright` or `uv pip install basedpyright`

**Using pylsp with plugins:**

pylsp supports various plugins for enhanced functionality. Turn them on through the server's **Initialization Options** (under **Advanced** in the same dialog):

::: details Show JSON
```json
{
  "lsp": {
    "python": {
      "command": "pylsp",
      "args": [],
      "enabled": true,
      "initialization_options": {
        "pylsp": {
          "plugins": {
            "pycodestyle": { "enabled": true },
            "pylint": { "enabled": true }
          }
        }
      }
    }
  }
}
```
:::

## Configuring LSP for a New Language

Adding a language server for a language takes two entries in **Settings → Syntax & Languages**, under the same key (for example `csharp`):

1. **Languages → [+] Add new** — the language itself: its file **Extensions**, **Grammar** and **Comment Prefix**.
2. **Language Servers → [+] Add new** — the server: its **Command** and **Args**.

For example, to add C# support:

::: details Show JSON
```json
{
  "languages": {
    "csharp": {
      "extensions": ["cs"],
      "grammar": "csharp",
      "comment_prefix": "//"
    }
  },
  "lsp": {
    "csharp": {
      "command": "/path/to/csharp-language-server",
      "args": [],
      "enabled": true
    }
  }
}
```
:::

The language name (e.g., `"csharp"`) must match in both sections. The `grammar` field must be a valid grammar name — run `fresh --cmd grammar list` to see all available grammars. Fresh includes built-in language definitions for many languages, visible in the Settings UI (run **Open Settings** from the palette) under **Syntax & Languages → Languages**.

### Environment Variables

Pass environment variables to LSP server binaries with the server's **Env** list (under **Advanced** in its Settings dialog):

::: details Show JSON
```json
{
  "lsp": {
    "rust": {
      "command": "rust-analyzer",
      "env": { "RUST_LOG": "info" },
      "enabled": true
    }
  }
}
```
:::

### Language ID Overrides

Some LSP servers expect a different `languageId` than Fresh's internal language name for certain files. Use the server's **Language Id Overrides** (under **Advanced** in its Settings dialog) to map a **file extension** (without the dot) to the `languageId` to send:

::: details Show JSON
```json
{
  "lsp": {
    "typescript": {
      "command": "typescript-language-server",
      "args": ["--stdio"],
      "language_id_overrides": {
        "tsx": "typescriptreact"
      }
    }
  }
}
```
:::

### Rust LSP Mode Switching

Use "Rust LSP: Configure Mode" from the command palette to toggle between Full and Reduced Memory modes for rust-analyzer.

## Configuring Language Detection via Settings UI

You can also configure language detection using the Settings UI instead of editing `config.json` directly:

1. **Open Settings**: Run **Open Settings** from the palette (or **Edit → Settings...** from the menu bar).
2. **Navigate to Languages**: Go to **Syntax & Languages → Languages**
3. **Add or Edit a Language**: Click on an existing language to edit it, or add a new one
4. **Configure Detection**: Set the following fields:
   - **Extensions**: File extensions that should use this language (e.g., `cs` for C#, `rs` for Rust)
   - **Filenames**: Specific filenames without extensions (e.g., `Makefile`, `.bashrc`, `.zshrc`)
   - **Grammar**: The syntax highlighting grammar to use (must match a grammar name from syntect)

### Example: Adding Shell Script Detection for Dotfiles

To make Fresh recognize `.bashrc`, `.zshrc`, and similar files as shell scripts:

1. Open Settings (**Edit → Settings...**)
2. Go to **Languages** → **bash** (or create a new `bash` entry)
3. Add filenames: `.bashrc`, `.zshrc`, `.bash_profile`, `.profile`
4. The grammar should be set to `Bourne Again Shell (bash)` or similar

The `filenames` field supports glob patterns like `*.conf`, `*rc`, or `/etc/**/rc.*` for matching files without standard extensions.

Fresh checks filenames first, then extensions, allowing dotfiles without traditional extensions to get proper syntax highlighting.