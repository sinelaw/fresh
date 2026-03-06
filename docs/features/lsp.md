# LSP Integration

Fresh has native support for the Language Server Protocol (LSP), providing features like:

*   **Real-time diagnostics:** See errors and warnings in your code as you type.
*   **Code completion:** Get intelligent code completion suggestions.
*   **Go-to-definition:** Quickly jump to the definition of a symbol.

## Diagnostics Panel

Open the diagnostics panel with "Show Diagnostics Panel" or "Toggle Diagnostics Panel" from the command palette. In the panel, Up/Down scrolls the editor to preview each diagnostic's location; Enter jumps to the diagnostic and focuses the editor. `F8` and `Shift+F8` jump to next/previous diagnostic without the panel.

Diagnostics can also be shown inline at the end of each line — see [Editing — Inline Diagnostics](./editing.md#inline-diagnostics).

## Signature Help

Signature help popups render markdown with proper formatting, hanging indent, and paragraph spacing.

## Code Folding

When the LSP server provides `foldingRange`, fold indicators appear in the gutter. See [Editing — Code Folding](./editing.md#code-folding).

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

The language name (e.g., `"csharp"`) must match in both sections. Fresh includes built-in language definitions for Rust, JavaScript, TypeScript, and Python.

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

1. **Open Settings**: Use **Edit → Settings...** or the command palette (`Ctrl+P`) and search for "Settings"
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