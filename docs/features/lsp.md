# LSP Integration

Fresh has native support for the Language Server Protocol (LSP), providing features like:

*   **Real-time diagnostics:** See errors and warnings in your code as you type.
*   **Code completion:** Get intelligent code completion suggestions.
*   **Go-to-definition:** Quickly jump to the definition of a symbol.

## Built-in LSP Support

Fresh includes built-in LSP configurations for many popular languages. Simply install the language server and Fresh will use it automatically:

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

Fresh includes built-in support for Python with `pylsp` (Python Language Server). However, you can also use alternative Python language servers:

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

The language name (e.g., `"csharp"`) must match in both sections. Fresh includes built-in language definitions for Rust, JavaScript, TypeScript, and Python, but you can add any language by configuring it in your config file.

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

Fresh checks filenames first, then extensions, allowing dotfiles without traditional extensions to get proper syntax highlighting.