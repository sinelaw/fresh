# LSP Integration

Fresh has native support for the Language Server Protocol (LSP), providing features like:

*   **Real-time diagnostics:** See errors and warnings in your code as you type.
*   **Code completion:** Auto-imports are applied when you accept a completion. Fresh also provides [basic buffer-word completions](./editing.md#basic-completions) without an LSP.
*   **Code actions:** Quick fixes, refactorings, and server-initiated file create/rename/delete, all through a single popup that merges actions from every configured server.
*   **Go-to-definition, hover, rename, find references**, and **signature help**.
*   **Formatting:** "Format Buffer" from the command palette uses the configured external formatter, falling back to LSP formatting (including range formatting) when none is set.

All LSP operations are available as palette commands (search for "LSP"). Use the [Keybinding Editor](./keybinding-editor.md) to see or change the keys bound to each one.

## Status Bar

The status bar shows a single `LSP` indicator — colour-coded, with a spinner during startup and indexing. Activate it (click, or run **LSP: Server Status** from the command palette) to open a popup with per-server status, live progress, and per-server actions (restart, stop, view log). Servers that are configured but whose binary isn't on `PATH` are flagged so Fresh doesn't quietly spawn failing processes. The popup also shows buffer-skip state when a file is too large for LSP, and the "not installed" copy is container-aware when you're attached to a devcontainer (it points at the container's PATH, not the host's). You can also mute a language from the popup.

## Remote-Aware LSP

Language servers spawn through the editor's current [Authority](../plugins/api/), so attaching to an SSH remote or a devcontainer runs the servers over there. `command_exists` probes and `ProcessLimits` (`max_memory_mb`, `max_cpu_percent`) are threaded through the same authority, so quotas apply whether the server is local or in a container.

## Hover and Diagnostics

Hover popups fuse any overlapping diagnostic with the hover body — severity-coloured and source-tagged (`rustc`, `clippy`, `clangd`, etc.), so you see the error message and the type information together.

## Diagnostics Panel

Open the diagnostics panel with "Show Diagnostics Panel" or "Toggle Diagnostics Panel" from the command palette. In the panel, Up/Down scrolls the editor to preview each diagnostic's location; Enter jumps to the diagnostic and focuses the editor. `F8` and `Shift+F8` jump to next/previous diagnostic without the panel.

Diagnostics can also be shown inline at the end of each line — see [Editing — Inline Diagnostics](./editing.md#inline-diagnostics).

## Signature Help

Signature help popups render markdown with proper formatting, hanging indent, and paragraph spacing.

## Code Folding

When the LSP server provides `foldingRange`, fold indicators appear in the gutter. See [Editing — Code Folding](./editing.md#code-folding).

## Multi-Server Support

You can configure multiple LSP servers for the same language (e.g., pylsp + pyright for Python). Configure this in the Settings UI (run **Open Settings** from the palette) under the **LSP** section.

Each server can opt into or out of specific features using `only_features` / `except_features` — for example, route completions to one server and diagnostics to another. Fresh merges completions from every eligible server and tracks diagnostics per-server. Servers configured for all languages are spawned once per project rather than once per language.

## Disabling LSP

To disable a single server, set `"enabled": false` on its entry in the `lsp` map (Settings UI: **LSP** section), or mute the language from the status-bar popup.

To disable LSP for **all** languages at once, set the top-level `lsp_enabled` master switch to `false` (Settings UI: **General** section):

```json
{
  "lsp_enabled": false
}
```

No language server will auto-start for any language (universal servers included), and the status bar shows a dimmed `LSP (off)` pill when servers are configured for the current language. You can still start a server explicitly with **Start/Restart LSP Server** from the command palette — a manual start overrides the global switch for that language.

## C/C++ Header Routing

When you open a `.h` file, Fresh routes to the C++ LSP if there's a clear signal in the project (a sibling `.cpp`, `.hpp`, or `.hxx`), and to the C LSP otherwise.

## Workspace Root Detection

By default, Fresh uses the working directory as the LSP workspace root. You can configure `root_markers` on an LSP server entry (e.g., `Cargo.toml`, `package.json`) so the editor walks upward from the file's directory to find the project root. Configure this in the Settings UI (run **Open Settings** from the palette) under the **LSP** section.

## Built-in LSP Support

Fresh includes built-in LSP configurations for many languages. Install the server and Fresh will use it automatically:

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

## Python LSP Configuration

The default Python server is `pylsp`. Alternatives:

**Using pyright (recommended for type checking):**

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

Install pyright with: `npm install -g pyright` or `pip install pyright`

**Using basedpyright (enhanced pyright fork):**

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

Install basedpyright with: `pip install basedpyright` or `uv pip install basedpyright`

**Using pylsp with plugins:**

pylsp supports various plugins for enhanced functionality:

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

## Configuring LSP for a New Language

To add LSP support for a language, you need to configure two sections in your `~/.config/fresh/config.json`:

1. **`languages`**: Define the file extensions for the language
2. **`lsp`**: Configure the language server command

For example, to add C# support:

```json
{
  "languages": {
    "csharp": {
      "extensions": ["cs"],
      "grammar": "c_sharp",
      "comment_prefix": "//",
      "auto_indent": true
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

The language name (e.g., `"csharp"`) must match in both sections. The `grammar` field must be a valid grammar name — run `fresh --cmd grammar list` to see all available grammars. Fresh includes built-in language definitions for many languages, visible in the Settings UI (run **Open Settings** from the palette) under the **Languages** section.

### Environment Variables

Pass environment variables to LSP server binaries:

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

### Language ID Overrides

Some LSP servers expect a different `languageId` than Fresh's internal language name. Use `language_id_overrides` to map them:

```json
{
  "lsp": {
    "typescript": {
      "command": "typescript-language-server",
      "args": ["--stdio"],
      "language_id_overrides": {
        "typescriptreact": "typescriptreact",
        "javascriptreact": "javascriptreact"
      }
    }
  }
}
```

### Rust LSP Mode Switching

Use "Switch Rust Analyzer Mode" from the command palette to toggle between Full and Reduced Memory modes for rust-analyzer.

## Configuring Language Detection via Settings UI

You can also configure language detection using the Settings UI instead of editing `config.json` directly:

1. **Open Settings**: Run **Open Settings** from the palette (or **Edit → Settings...** from the menu bar).
2. **Navigate to Languages**: Go to the **Languages** section
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