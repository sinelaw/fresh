# Language Support

Fresh ships syntax highlighting and indentation for a large set of languages out of the box, and connects to language servers for richer features.

## How a language is detected

Fresh picks a language for a buffer in this order:

1. **Filename** — well-known names map to their real format, including lock and config files (`yarn.lock` → YAML; `Cargo.lock`, `poetry.lock`, `uv.lock` → TOML; `composer.lock`, `Pipfile.lock`, `flake.lock`, `deno.lock` → JSON).
2. **Extension** — e.g. `.rs`, `.py`, `.fish`, `.smali`, `.gd`.
3. **Shebang** — for extensionless scripts, the interpreter on the first line (`#!/usr/bin/fish`, `python3.11`, `env -S …`) selects the language. An existing extension match wins over the shebang.

To see every built-in language, open **Open Settings** from the command palette and look under **Languages**, or run `fresh --cmd grammar list`.

## Recently added

- **Assembly** (GAS / AT&T and NASM/Intel, across x86, x86_64, ARM, RISC-V) — see [Assembly (asm-lsp)](#assembly-asm-lsp) below.
- **Fish** — highlighting and auto-indentation.
- **Smali** — highlighting.
- **GDScript** — highlighting; LSP available via Godot (see below).

## Language servers

Each language can be wired to an LSP server under `lsp.<language-id>` in config:

```jsonc
{
  "lsp": {
    "python": {
      "command": "pyright-langserver",
      "args": ["--stdio"],
      "enabled": true,
      "auto_start": true,
      "root_markers": ["pyproject.toml", ".git"]
    }
  }
}
```

`enabled` defaults to `true`; `auto_start` defaults to `false`, so some servers must be started manually (run **LSP: Server Status** from the palette). Language servers only run in a [trusted workspace](./workspace-trust.md). See [LSP Integration](./lsp.md) for the editor-side features.

### Assembly (asm-lsp)

Assembly support uses [asm-lsp](https://github.com/bergercookie/asm-lsp) and is opt-in. When you open an assembly file with no `.asm-lsp.toml` in the project, Fresh offers to generate one from the detected assembler and architecture. asm-lsp defaults to GAS / x86_64 without a config; the offer is scoped to the buffer that triggered it.

### GDScript (Godot)

GDScript highlighting is built in. For LSP, enable `lsp.gdscript` — it connects over TCP to Godot's built-in language server (default `127.0.0.1:6005`), so the Godot editor must be running. It is disabled by default.

## Adding your own

To add or tune a language beyond the built-ins — grammars, indentation rules, comment tokens — see [Adding a Language](../development/adding-languages.md) and [Language Packs](../plugins/development/language-packs.md).
