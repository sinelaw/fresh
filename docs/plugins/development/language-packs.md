# Creating Language Packs

Language packs add syntax highlighting, language configuration, and LSP support for new languages in Fresh.

> Want to add a language *inside* Fresh's source tree (a built-in family, an
> embedded grammar, or a tree-sitter parser) rather than ship a pack? See the
> contributor guide [Adding a Built-in Language](/development/adding-languages),
> which explains how auto-indent and the language families work. This page covers
> the user-facing pack path, which needs no recompile.

## Quick Start

Use the CLI to scaffold a new language pack:

```bash
fresh --init language
```

This creates a directory with the basic structure:
```
my-language/
├── package.json          # Package manifest
├── grammars/
│   └── syntax.sublime-syntax  # Sublime syntax grammar (YAML)
├── validate.sh           # Validation script
└── README.md
```

## Package Structure

### package.json

The manifest configures the language pack:

```json
{
  "$schema": "https://raw.githubusercontent.com/sinelaw/fresh/main/crates/fresh-editor/plugins/schemas/package.schema.json",
  "name": "my-language",
  "version": "0.1.0",
  "description": "Language support for MyLang",
  "type": "language",
  "author": "Your Name",
  "license": "MIT",
  "fresh": {
    "grammar": {
      "file": "grammars/syntax.sublime-syntax",
      "extensions": ["mylang", "ml"]
    },
    "language": {
      "commentPrefix": "//",
      "blockCommentStart": "/*",
      "blockCommentEnd": "*/",
      "tabSize": 4,
      "autoIndent": true
    },
    "lsp": {
      "command": "my-language-server",
      "args": ["--stdio"],
      "autoStart": true
    }
  }
}
```

### Grammar Configuration

| Field | Description |
|-------|-------------|
| `file` | Path to the grammar file (relative to package root) |
| `extensions` | File extensions this grammar handles (without dots) |
| `firstLine` | Optional regex for shebang detection |

### Language Configuration

| Field | Description |
|-------|-------------|
| `commentPrefix` | Line comment prefix (e.g., `//`, `#`, `--`) |
| `blockCommentStart` | Block comment opening (e.g., `/*`, `<!--`) |
| `blockCommentEnd` | Block comment closing (e.g., `*/`, `-->`) |
| `tabSize` | Default indentation width |
| `useTabs` | Use tabs instead of spaces |
| `autoIndent` | Enable automatic indentation |
| `formatter.command` | Formatter command (e.g., `prettier`, `rustfmt`) |
| `formatter.args` | Arguments for the formatter (file path is passed automatically) |

**Formatter Examples:**

```json
// Prettier (JavaScript/TypeScript/etc.)
"formatter": {
  "command": "prettier",
  "args": ["--write"]
}

// Prettier with plugin (Svelte, Vue, etc.)
"formatter": {
  "command": "prettier",
  "args": ["--write", "--plugin", "prettier-plugin-svelte"]
}

// Black (Python)
"formatter": {
  "command": "black",
  "args": ["-"]
}

// rustfmt (Rust)
"formatter": {
  "command": "rustfmt",
  "args": []
}

// gofmt (Go)
"formatter": {
  "command": "gofmt",
  "args": ["-w"]
}
```

**Note:** The file path is automatically appended to the args by Fresh. Some formatters expect stdin (use `"-"` as arg), others expect file path.

### LSP Configuration

| Field | Description |
|-------|-------------|
| `command` | LSP server executable (e.g., `rust-analyzer`, `typescript-language-server`) |
| `args` | Arguments to pass to the server (e.g., `["--stdio"]`) |
| `autoStart` | Start server when opening matching files |
| `initializationOptions` | Custom LSP initialization options (language-specific JSON) |

**Finding LSP Servers:**
- [Language Server Protocol Implementations](https://microsoft.github.io/language-server-protocol/implementors/servers/) - Official registry
- [langserver.org](https://langserver.org/) - Community directory

**Common LSP Servers:**

| Language | Server | Command | Installation |
|----------|--------|---------|--------------|
| Rust | rust-analyzer | `rust-analyzer` | `rustup component add rust-analyzer` |
| TypeScript/JavaScript | typescript-language-server | `typescript-language-server` | `npm install -g typescript-language-server` |
| Python | pyright | `pyright-langserver` | `npm install -g pyright` |
| Go | gopls | `gopls` | `go install golang.org/x/tools/gopls@latest` |
| C/C++ | clangd | `clangd` | System package manager |

**Example with initialization options:**
```json
"lsp": {
  "command": "rust-analyzer",
  "args": [],
  "autoStart": true,
  "initializationOptions": {
    "cargo": {
      "buildScripts": {
        "enable": true
      }
    }
  }
}
```

## Finding Existing Grammars

Before writing a grammar from scratch, search online for existing Sublime Text or TextMate grammars:

1. **Search GitHub** for `<language> sublime-syntax` or `<language> tmLanguage`
2. **Check VS Code extensions** - many use TextMate/Sublime grammars
3. **Browse [Package Control](https://packagecontrol.io/)** - Sublime Text's package repository

### ⚠️ Grammar Compatibility

**Important:** Fresh supports a subset of sublime-syntax features. Before using a grammar, check that it:

**Will NOT work:**
- Uses `extends: Packages/...` directive (grammar inheritance)
- References external grammars or packages
- Has dependencies on other grammar files

**Will work:**
- Standalone, self-contained grammars
- Grammars using only `include` for internal contexts
- No external dependencies

**Examples of compatible grammars:**
- See [fresh-plugins/languages](https://github.com/sinelaw/fresh-plugins/tree/main/languages) for working examples (templ, hare, solidity)
- Standalone grammars from Package Control that don't use `extends`

**To test compatibility:**

Try installing your language pack locally (see Testing section below) and check the logs for parse errors.

If you find a grammar that uses `extends`, you'll need to either:
1. Find an alternative standalone grammar
2. Manually merge the base grammar into your grammar file
3. Create a new standalone grammar from scratch

### Attribution

When using an existing grammar:

1. **Check the license** - ensure it allows redistribution (MIT, Apache, BSD are common)
2. **Include a copy of the license** in your `grammars/` directory (e.g., `grammars/LICENSE`)
3. **Credit the original author** in your README and package description

Example attribution in README:
```markdown
## Grammar Attribution

The syntax grammar is derived from [original-package](https://github.com/user/repo)
by Original Author, licensed under MIT. See `grammars/LICENSE` for details.
```

## Writing Sublime Syntax Grammars

Fresh uses Sublime Text's `.sublime-syntax` format (YAML-based).

**Recommendation**: Start with an existing grammar from [fresh-plugins/languages](https://github.com/sinelaw/fresh-plugins/tree/main/languages) and adapt it for your language, rather than writing from scratch.

### Minimal Example

```yaml
%YAML 1.2
---
name: My Language
scope: source.mylang
file_extensions: [mylang, ml]

contexts:
  main:
    # Line comments
    - match: //.*$
      scope: comment.line

    # Strings
    - match: '"'
      scope: string.quoted.double
      push:
        - match: '"'
          pop: true
        - match: \\.
          scope: constant.character.escape

    # Keywords
    - match: \b(if|else|while|for|return)\b
      scope: keyword.control
```

### Documentation Resources

Official documentation:

- **[Sublime Text Syntax Reference](https://www.sublimetext.com/docs/syntax.html)** - Complete format specification
- **[Scope Naming Guide](https://www.sublimetext.com/docs/scope_naming.html)** - Standard scope names for syntax elements
- **[TextMate Language Grammars](https://macromates.com/manual/en/language_grammars)** - Additional background

### Working Examples

Browse complete, tested grammars in the [fresh-plugins repository](https://github.com/sinelaw/fresh-plugins/tree/main/languages):
- **Templ** - Simple, self-contained
- **Hare** - Systems language
- **Solidity** - Smart contracts

## Examples

### Minimal Example

See the [Solidity language pack](https://github.com/sinelaw/fresh-plugins/tree/main/languages/solidity):

```
languages/solidity/
├── package.json
├── grammars/
│   ├── solidity.sublime-syntax
│   └── LICENSE
├── validate.sh
└── README.md
```

### Complete Working Example

See the [Templ language pack](https://github.com/sinelaw/fresh-plugins/tree/main/languages/templ) for a complete, self-contained grammar example:

```yaml
%YAML 1.2
---
name: Templ
scope: source.templ
version: 2

file_extensions:
  - templ

variables:
  ident: '[a-zA-Z_][a-zA-Z0-9_]*'

contexts:
  main:
    # All grammar rules defined inline
    # No external dependencies
```

## Testing and Local Development

### Testing with Local Path (Recommended)

The fastest way to test your language pack during development:

1. **Open Fresh** with a test file
2. **Open command palette**: Press `Ctrl+P` then type `>`
3. **Install from local path**:
   - Type `package` and select "Package: Install from URL"
   - Enter the full path to your language pack directory: `/path/to/your-language-pack`
4. **Check for errors**:
   - Open command palette and run "Show Warnings"
   - Check for grammar parse errors or missing files
5. **Iterate**: Edit your grammar, then reinstall from the same local path to reload

### Alternative: Manual Installation

1. **Copy** your language pack to `~/.config/fresh/grammars/<package-name>/`
2. **Validate manifest**: Run `./validate.sh` in your package directory
3. **Restart Fresh** to load the new grammar

### Validation

Always validate your package before publishing:

```bash
# Validate package.json schema
./validate.sh

# Test by installing locally and checking logs
# (see Troubleshooting section for log commands)
```

## Troubleshooting

### Debugging Commands

```bash
# Show log locations
fresh --show-paths

# View Fresh logs (check for grammar parse errors)
tail -f ~/.local/state/fresh/logs/fresh-*.log

# Check LSP logs
tail -f ~/.local/state/fresh/logs/lsp/<language>-*.log

# Validate package.json
./validate.sh
```

### Common Issues

**Syntax highlighting not working:**
- Check logs for `Failed to parse grammar` - most often caused by `extends` directive (see compatibility warning)
- Verify file extension in package.json: use `["py"]` not `[".py"]`
- Confirm grammar file path is correct

**LSP server not starting:**
- Verify server is installed: `which <server-command>`
- Check LSP logs for error messages
- See [LSP server registry](https://microsoft.github.io/language-server-protocol/implementors/servers/) for correct invocation

**Formatter not working:**
- Verify formatter is installed: `which <formatter>`
- Test manually: `<formatter> <args> <file>`
- Check formatter documentation for correct arguments

## Publishing

1. Push your package to a public Git repository
2. Submit a PR to [fresh-plugins-registry](https://github.com/sinelaw/fresh-plugins-registry)
3. Add your package to `languages.json`

After approval, users can install via the command palette:
1. Press `Ctrl+P` then type `>`
2. Type `package` and select "Package: Install from URL"
3. Enter your package name or git URL

Users can also install directly from your git repository:
```bash
# In Fresh command palette
Package: Install from URL
# Then enter: https://github.com/username/your-language-pack
```
