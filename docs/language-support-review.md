# Language & Syntax Highlighting Support Review

## How Syntax Highlighting Works in Fresh

Fresh uses a **dual-engine architecture** for language support:

### Syntect (TextMate/Sublime grammars) — Primary highlighting engine
- **Syntect is the PRIMARY system for syntax highlighting** (colorizing code).
- Uses TextMate / Sublime `.sublime-syntax` grammars to tokenize and color code.
- Provides broad coverage: **100+ languages** via built-in defaults + 9 custom embedded grammars.
- Selection priority: TextMate grammar is tried first; tree-sitter is only used as a
  fallback for highlighting when no TextMate grammar exists for a file.

### Tree-sitter — Structural/semantic features (NOT primarily for highlighting)
- Tree-sitter is used for **non-highlighting features**: auto-indentation (via `.scm`
  indent queries), bracket matching, reference/semantic highlighting, and language detection.
- Tree-sitter CAN highlight as a **fallback** when no TextMate grammar is available,
  but this is not the primary path.
- Currently supports **18 languages** with tree-sitter parsers.

### How they interact
1. `HighlightEngine::for_file()` first looks for a TextMate grammar in the registry.
2. If none found, falls back to tree-sitter for highlighting.
3. Even when using TextMate highlighting, tree-sitter `Language` is still detected and
   stored for indentation/bracket/semantic features.

**Key implication for adding language support:** Adding a new language primarily means
adding a **TextMate/Sublime grammar** (for highlighting) and optionally a **tree-sitter
parser** (for indentation, bracket matching). The recommendations below reflect this.

---

## Summary

This document catalogs all language support requests from GitHub issues, combined with
common DevOps/infrastructure languages and file types that developers expect modern
editors to handle. It compares against Fresh's current support levels.

### Support Level Key

| Level | Meaning |
|-------|---------|
| **Syntect (built-in)** | Highlighting via syntect's built-in TextMate grammar defaults (~100 languages) |
| **Embedded grammar** | Custom .sublime-syntax file bundled with Fresh (better/specialized highlighting) |
| **Tree-sitter** | Structural features: auto-indent, bracket matching, semantic highlighting; fallback syntax highlighting |
| **LSP config** | Built-in LSP server configuration in config.example.json |
| **None** | No current support |

---

## Languages Requested in GitHub Issues

| Language | Issue(s) | Status | Current Support | What's Missing |
|----------|----------|--------|-----------------|----------------|
| Dart | [#1252](https://github.com/sinelaw/fresh/issues/1252) | Open | Syntect (basic) | LSP config (dart analyze --lsp) |
| KDL | [#1266](https://github.com/sinelaw/fresh/issues/1266) | Open | None | TextMate grammar (.sublime-syntax) |
| Hyprlang | [#1266](https://github.com/sinelaw/fresh/issues/1266) | Open | None | TextMate grammar (.sublime-syntax) |
| Nushell | [#1031](https://github.com/sinelaw/fresh/issues/1031) | Open | None | TextMate grammar, LSP config (nu --lsp) |
| Solidity | [#857](https://github.com/sinelaw/fresh/issues/857) | Closed | None | TextMate grammar, LSP config (solc --lsp) |
| Deno (TS variant) | [#1191](https://github.com/sinelaw/fresh/issues/1191) | Open | TypeScript supported | Deno LSP auto-detection (deno.json presence) |
| LaTeX | [#807](https://github.com/sinelaw/fresh/issues/807) | Closed/Added | Syntect + LSP config | Done (highlighting via syntect) |
| Zig | [#737](https://github.com/sinelaw/fresh/issues/737) | Closed/Added | Embedded grammar + LSP | Done |
| Bash/Zsh configs | [#383](https://github.com/sinelaw/fresh/issues/383) | Closed/Added | Tree-sitter + Syntect | Done |
| Config file fallback | [#1219](https://github.com/sinelaw/fresh/issues/1219) | Added (v0.2.18) | `fallback` config field | Set `"fallback": {"grammar": "bash", "comment_prefix": "#"}` in config. See [Configuration docs](../configuration/index.md#set-a-fallback-language-for-unrecognized-files). |

---

## Common DevOps / Infrastructure Languages & Files

| Language / File Type | File Extensions / Names | Current Support | Recommendation |
|---------------------|------------------------|-----------------|----------------|
| **Dockerfile** | `Dockerfile`, `Dockerfile.*`, `*.dockerfile` | Syntect + LSP config | Adequate (highlighting works) |
| **Docker Compose** | `docker-compose.yml`, `compose.yml` | YAML (Syntect) | Adequate (YAML grammar) |
| **Makefile** | `Makefile`, `*.mk`, `GNUmakefile` | Syntect + LSP config | Adequate (highlighting works) |
| **CMake** | `CMakeLists.txt`, `*.cmake` | Syntect | Add LSP config (cmake-language-server) |
| **Terraform / HCL** | `*.tf`, `*.tfvars`, `*.hcl` | None | Add TextMate grammar + LSP config (terraform-ls) |
| **Helm templates** | `*.tpl` (in chart dirs) | None | Add Go template grammar |
| **Nix** | `*.nix`, `flake.nix` | Syntect | Add LSP config (nil/nixd) |
| **Shell (POSIX)** | `*.sh`, `*.bash`, `*.zsh` | Syntect + Tree-sitter + LSP | Done |
| **PowerShell** | `*.ps1`, `*.psm1`, `*.psd1` | Syntect | Add LSP config (powershell-editor-services) |
| **YAML** | `*.yml`, `*.yaml` | Syntect + LSP config | Adequate (highlighting works) |
| **TOML** | `*.toml` | Embedded grammar + LSP | Adequate |
| **JSON / JSONC** | `*.json`, `*.jsonc` | Syntect + Tree-sitter + LSP | Done |
| **XML** | `*.xml`, `*.xsl`, `*.xsd`, `*.svg` | Syntect | Adequate; optionally add LSP config |
| **INI / CONF** | `*.ini`, `*.conf`, `*.cfg`, `*.env` | Syntect (INI) | Add .env grammar |
| **Protobuf** | `*.proto` | None | Add TextMate grammar + LSP config (buf-language-server) |
| **GraphQL** | `*.graphql`, `*.gql` | None | Add TextMate grammar + LSP config (graphql-lsp) |
| **SQL** | `*.sql` | Syntect | Add LSP config (sqls) |

---

## Common Programming Languages (Not Yet Fully Supported)

| Language | File Extensions | Current Support | Recommendation |
|----------|----------------|-----------------|----------------|
| **Kotlin** | `*.kt`, `*.kts` | Syntect | Add LSP config (kotlin-language-server) |
| **Swift** | `*.swift` | Syntect | Add LSP config (sourcekit-lsp) |
| **Scala** | `*.scala`, `*.sc` | Syntect | Add LSP config (metals) |
| **Elixir** | `*.ex`, `*.exs` | Syntect | Add LSP config (elixir-ls) |
| **Erlang** | `*.erl`, `*.hrl` | Syntect | Add LSP config (erlang_ls) |
| **Haskell** | `*.hs`, `*.lhs` | Syntect | Add LSP config (haskell-language-server) |
| **OCaml** | `*.ml`, `*.mli` | Syntect | Add LSP config (ocamllsp) |
| **Clojure** | `*.clj`, `*.cljs`, `*.cljc` | Syntect | Add LSP config (clojure-lsp) |
| **R** | `*.r`, `*.R`, `*.Rmd` | Syntect | Add LSP config (languageserver) |
| **Julia** | `*.jl` | None | Add TextMate grammar + LSP config (LanguageServer.jl) |
| **Perl** | `*.pl`, `*.pm` | Syntect | Add LSP config (Perl::LanguageServer) |
| **Dart** | `*.dart` | Syntect (basic) | Add LSP config (dart analyze --lsp) |
| **V** | `*.v` | None | Add TextMate grammar |
| **Nim** | `*.nim`, `*.nims` | None | Add TextMate grammar, LSP config (nimlangserver) |
| **Gleam** | `*.gleam` | None | Add TextMate grammar, LSP config (gleam lsp) |

---

## Web Frontend Languages

| Language | File Extensions | Current Support | Recommendation |
|----------|----------------|-----------------|----------------|
| **Vue** | `*.vue` | None | Add TextMate grammar + LSP config (vue-language-server) |
| **Svelte** | `*.svelte` | None | Add TextMate grammar + LSP config (svelte-language-server) |
| **Astro** | `*.astro` | None | Add TextMate grammar + LSP config (@astrojs/language-server) |
| **SCSS** | `*.scss` | Syntect (Sass) | Add LSP config (vscode-css-languageserver) |
| **LESS** | `*.less` | Syntect | Add LSP config (vscode-css-languageserver) |
| **Tailwind CSS** | (within HTML/JSX) | None | Add LSP config (tailwindcss-language-server) |

---

## Package Manager & Build Tool Config Files

| File Type | File Names / Extensions | Current Support | Recommendation |
|-----------|------------------------|-----------------|----------------|
| **npm** | `package.json`, `package-lock.json`, `.npmrc` | JSON grammar | Adequate |
| **pip / PyPI** | `requirements.txt`, `pyproject.toml`, `setup.cfg`, `Pipfile` | TOML/INI grammars | Adequate |
| **uv** | `uv.toml`, `uv.lock` | TOML grammar | Adequate |
| **Cargo (Rust)** | `Cargo.toml`, `Cargo.lock` | TOML grammar | Adequate |
| **Go modules** | `go.mod`, `go.sum` | None (specific) | Add go.mod grammar |
| **Gradle** | `build.gradle`, `build.gradle.kts`, `settings.gradle` | Syntect (Groovy) / None (kts) | Add Kotlin script support |
| **Maven** | `pom.xml` | XML grammar | Adequate |
| **Bazel / Buck** | `BUILD`, `BUILD.bazel`, `WORKSPACE`, `*.bzl` | None | Add Starlark grammar |
| **Meson** | `meson.build`, `meson_options.txt` | None | Add grammar |
| **Justfile** | `justfile`, `Justfile`, `.justfile` | None | Add grammar (similar to Makefile) |
| **Taskfile** | `Taskfile.yml`, `Taskfile.yaml` | YAML grammar | Adequate |
| **Tiltfile** | `Tiltfile` | None | Add Starlark grammar |
| **Earthfile** | `Earthfile` | None | Add Dockerfile-like grammar |
| **Podfile** | `Podfile` | Syntect (Ruby) | Adequate |
| **Gemfile** | `Gemfile`, `*.gemspec` | Syntect (Ruby) | Adequate |
| **Brewfile** | `Brewfile` | None | Map to Ruby grammar |

---

## CI/CD & Orchestration Config Files

| File Type | File Names / Extensions | Current Support | Recommendation |
|-----------|------------------------|-----------------|----------------|
| **GitHub Actions** | `.github/workflows/*.yml` | YAML grammar | Adequate |
| **GitLab CI** | `.gitlab-ci.yml` | YAML grammar | Adequate |
| **Jenkinsfile** | `Jenkinsfile` | None | Map to Groovy grammar |
| **CircleCI** | `.circleci/config.yml` | YAML grammar | Adequate |
| **Kubernetes manifests** | `*.yaml` (k8s) | YAML grammar | Adequate |
| **Ansible** | `*.yml` (playbooks) | YAML grammar | Adequate |
| **Vagrant** | `Vagrantfile` | None | Map to Ruby grammar |

---

## Data & Markup Formats

| Format | File Extensions | Current Support | Recommendation |
|--------|----------------|-----------------|----------------|
| **Markdown** | `*.md`, `*.mdx` | Syntect + LSP config | Adequate (highlighting works) |
| **reStructuredText** | `*.rst` | Syntect | Adequate |
| **AsciiDoc** | `*.adoc`, `*.asciidoc` | None | Add grammar |
| **CSV / TSV** | `*.csv`, `*.tsv` | None | Add basic grammar (rainbow CSV) |
| **Diff / Patch** | `*.diff`, `*.patch` | Syntect | Adequate |
| **Dotenv** | `.env`, `.env.*` | None | Add grammar |
| **EditorConfig** | `.editorconfig` | None | Map to INI grammar |
| **Ignore files** | `.gitignore`, `.dockerignore` | Embedded grammar | Done |

---

## Priority Recommendations

### High Priority (Frequently Requested + Common DevOps)

| # | Language/File | What to Add | Reason |
|---|--------------|-------------|--------|
| 1 | **Terraform / HCL** | TextMate grammar + LSP config | Very common in DevOps, zero support |
| 2 | **Dart** | LSP config | Requested in #1252, already has syntect highlighting |
| 3 | **Nix** | LSP config (nil/nixd) | Already has syntect highlighting, project uses Nix |
| 4 | **Vue / Svelte** | TextMate grammar + LSP config | Very popular web frameworks, no support at all |
| 5 | **Protobuf** | TextMate grammar + LSP config | Common in microservices, no support |
| 6 | **GraphQL** | TextMate grammar + LSP config | Common in web APIs, no support |
| 7 | **Kotlin** | LSP config | Already has syntect highlighting, major language |
| 8 | **SQL** | LSP config (sqls) | Already has syntect highlighting, very common |
| 9 | **CMake** | LSP config | Already has syntect highlighting, common build system |
| 10 | **PowerShell** | LSP config | Already has syntect highlighting, common on Windows |

### Medium Priority (Requested in Issues)

| # | Language/File | Reason |
|---|--------------|--------|
| 11 | **KDL** | Requested in #1266 |
| 12 | **Hyprlang** | Requested in #1266, niche but passionate users |
| 13 | **Nushell** | Requested in #1031 |
| 14 | **Solidity** | Requested in #857 |
| 15 | **Deno LSP detection** | Requested in #1191 |
| 16 | **Config file fallback** | Requested in #1219, broad impact |

### Lower Priority (Nice to Have)

| # | Language/File | Reason |
|---|--------------|--------|
| 17 | **Starlark** (Bazel/Tiltfile/Buck) | Niche but useful |
| 18 | **Justfile** | Growing popularity |
| 19 | **Gleam** | New but growing |
| 20 | **Nim** | Small community |
| 21 | **Astro** | Web framework |
| 22 | **Dotenv** | Simple but useful |
| 23 | **go.mod** | Small grammar |
| 24 | **Meson** | Build system |

---

## Validated Syntax Highlighting Results (tmux capture-pane -e)

Tested by opening each file in Fresh via tmux, capturing the terminal output with
ANSI escape codes (`tmux capture-pane -e -p`), and checking for syntax-colored tokens
(colors beyond default text white/gray).

### WORKING - Full syntax highlighting (55 files)

| File | Detected Syntax | Engine | Colors |
|------|----------------|--------|--------|
| hello.rs | Rust | tree-sitter | 4 (keywords, strings, functions, types) |
| hello.py | Python | tree-sitter | 5 |
| hello.js | JavaScript | tree-sitter | 4 |
| hello.ts | typescript | tree-sitter | 6 |
| hello.tsx | typescript | tree-sitter | 6 |
| hello.jsx | JavaScript | syntect | 6 |
| hello.mjs | JavaScript | syntect | 6 |
| hello.go | Go | syntect | 5 |
| hello.c | C | syntect | 5 |
| hello.h | C | syntect | 6 |
| hello.cpp | C++ | syntect | 5 |
| hello.cc | C++ | syntect | 6 |
| hello.hpp | C++ | syntect | 6 |
| hello.cs | C# | syntect | 4 |
| hello.java | Java | syntect | 4 |
| hello.php | PHP | syntect | 5 |
| hello.rb | Ruby | syntect | 4 |
| hello.lua | Lua | syntect | 5 |
| hello.sh | Bourne Again Shell | syntect | 4 |
| hello.bash | Bourne Again Shell | syntect | 4 |
| hello.zsh | Bourne Again Shell | syntect | 3 |
| dot_bashrc | Bourne Again Shell | syntect | 4 |
| dot_profile | Bourne Again Shell | syntect | 4 |
| hello.json | JSON | tree-sitter | 3 |
| hello.html | HTML | syntect | 3 |
| hello.css | CSS | syntect | 3 |
| hello.py | Python | syntect | 5 |
| hello.pyi | Python | syntect | 5 |
| hello.toml | TOML | syntect (embedded) | 4 |
| hello.zig | Zig | syntect (embedded) | 6 |
| hello.odin | Odin | syntect (embedded) | 6 |
| hello.typ | Typst | syntect (embedded) | 5 |
| hello.gitconfig | Git Config | syntect (embedded) | 3 |
| hello.yaml | YAML | syntect | 3 |
| docker-compose.yml | YAML | syntect | 3 |
| hello.xml | XML | syntect | 4 |
| hello.md | Markdown | syntect | 5 |
| hello.sql | SQL | syntect | 6 |
| hello.tex | LaTeX | syntect | 5 |
| hello.r | R | syntect | 5 |
| hello.scala | Scala | syntect | 4 |
| hello.hs | Haskell | syntect | 6 |
| hello.clj | Clojure | syntect | 6 |
| hello.erl | Erlang | syntect | 6 |
| hello.ml | OCaml | syntect | 6 |
| hello.pl | Perl | syntect | 6 |
| hello.lisp | Lisp | syntect | 6 |
| hello.d | D | syntect | 6 |
| hello.bat | Batch File | syntect | 4 |
| Makefile | Makefile | syntect | 5 |
| GNUmakefile | Makefile | syntect | 5 |
| hello.mk | Makefile | syntect | 5 |
| Gemfile | Ruby | syntect | 4 |
| Rakefile | Ruby | syntect | 6 |
| Vagrantfile | Ruby | syntect | 6 |

### PARTIAL - Detected but minimal/no highlighting colors (4 files)

| File | Detected Syntax | Issue |
|------|----------------|-------|
| hello.gitignore | Gitignore | Only 2 highlight colors (comment green + path gray) |
| hello.diff | Diff | Syntax detected but 0 highlight colors (scope-to-category mapping gap) |
| hello.mts | typescript | Tree-sitter detected but no highlighting rendered |
| hello.vue | text | Only tag bracket color (220/gold) |

### NOT WORKING - No syntax highlighting (35 files)

| File | Detected As | Reason | Fix Needed |
|------|------------|--------|------------|
| Dockerfile | dockerfile | Tree-sitter detects language but no TextMate grammar | Add Dockerfile.sublime-syntax |
| Dockerfile.dev | text | Not detected at all (variant filename) | Add filename pattern |
| Containerfile | dockerfile | Same as Dockerfile | Add Dockerfile.sublime-syntax |
| CMakeLists.txt | Plain Text | No CMake grammar in syntect defaults | Add CMake.sublime-syntax |
| hello.kt | text | No Kotlin grammar in syntect | Add Kotlin.sublime-syntax |
| hello.swift | text | No Swift grammar in syntect | Add Swift.sublime-syntax |
| hello.dart | text | No Dart grammar in syntect | Add Dart.sublime-syntax |
| hello.ex | text | No Elixir grammar in syntect | Add Elixir.sublime-syntax |
| hello.fs | text | No F# grammar in syntect | Add FSharp.sublime-syntax |
| hello.nix | text | No Nix grammar in syntect | Add Nix.sublime-syntax |
| hello.ps1 | text | No PowerShell grammar in syntect | Add PowerShell.sublime-syntax |
| hello.scss | text | No SCSS grammar in syntect | Add SCSS.sublime-syntax |
| hello.less | text | No LESS grammar in syntect | Add LESS.sublime-syntax |
| hello.ini | text | No INI grammar in syntect | Add INI.sublime-syntax |
| hello.cjs | text | .cjs not mapped to JavaScript | Add extension mapping |
| hello.jl | text | No Julia grammar in syntect | Add Julia.sublime-syntax |
| hello.nim | text | No Nim grammar in syntect | Add Nim.sublime-syntax |
| hello.gleam | text | No Gleam grammar in syntect | Add Gleam.sublime-syntax |
| hello.v | text | No V grammar in syntect | Add V.sublime-syntax |
| hello.sol | text | No Solidity grammar in syntect | Add Solidity.sublime-syntax |
| hello.kdl | text | No KDL grammar in syntect | Add KDL.sublime-syntax |
| hello.nu | text | No Nushell grammar in syntect | Add Nushell.sublime-syntax |
| hello.tf | text | No Terraform/HCL grammar | Add HCL.sublime-syntax |
| hello.proto | text | No Protobuf grammar in syntect | Add Protobuf.sublime-syntax |
| hello.graphql | text | No GraphQL grammar in syntect | Add GraphQL.sublime-syntax |
| hello.astro | text | No Astro grammar in syntect | Add Astro.sublime-syntax |
| hello.svelte | text | No Svelte grammar (partial tag color) | Add Svelte.sublime-syntax |
| hello.env | text | No dotenv grammar | Add env.sublime-syntax |
| hello.editorconfig | text | No editorconfig grammar | Map to INI grammar |
| BUILD.bazel | text | No Starlark grammar | Add Starlark.sublime-syntax |
| Jenkinsfile | text | Not mapped to Groovy | Add filename mapping |
| Tiltfile | text | No Starlark grammar | Add Starlark.sublime-syntax |
| Earthfile | text | No Earthfile grammar | Add Earthfile.sublime-syntax |
| justfile | text | No Justfile grammar | Add Justfile.sublime-syntax |
| go.mod | text | No go.mod grammar | Add go.mod.sublime-syntax |

### Bugs Found

1. **hello.diff**: Diff grammar IS in syntect and IS detected, but produces zero highlight
   colors. The scope-to-category mapping in `textmate_engine.rs` doesn't handle Diff scopes
   (`markup.inserted`, `markup.deleted`, `meta.diff.header`, `punctuation.definition.from-file`).

2. **hello.mts**: Tree-sitter detects TypeScript but produces no colors. The `.mts` extension
   may not be triggering the highlight engine properly despite status bar showing "typescript".

3. **Dockerfile / Containerfile**: Tree-sitter detects the language (status bar shows
   "dockerfile") but there's no TextMate grammar and tree-sitter fallback isn't producing colors.

4. **Jenkinsfile**: Syntect has Groovy grammar (with `.groovy`, `.gvy`, `.gradle` extensions)
   but `Jenkinsfile` isn't mapped to it.

5. **hello.cjs**: JavaScript grammar exists but `.cjs` extension isn't in its extension list
   (`["js", "htc"]`). Same issue for `.mjs` detection (though it works - may go through
   tree-sitter JS).

---

## Current Support Summary

- **Syntect defaults (57 syntaxes):** Primary highlighting engine. The actual built-in set is
  MUCH smaller than previously claimed (~57 syntaxes, NOT 100+). Notable absences: Kotlin,
  Swift, Dart, Elixir, Nix, PowerShell, SCSS, LESS, CMake, Dockerfile, F#, Julia, Nim, INI.
- **Embedded grammars (9):** TOML, Odin, Zig, Typst, Git Rebase, Git Commit, Gitignore,
  Git Config, Git Attributes
- **Tree-sitter (18):** Rust, Python, JavaScript, TypeScript, HTML, CSS, C, C++, Go, JSON,
  Java, C#, PHP, Ruby, Bash, Lua, Pascal, Odin. Used for auto-indent/brackets, and as
  highlighting fallback (works for TS/TSX, but NOT for Dockerfile).
- **LSP configs (23):** Bash, C, C++, C#, CSS, Go, HTML, Java, JavaScript, JSON, LaTeX, Lua,
  Markdown, PHP, Python, Ruby, Rust, Templ, TOML, TypeScript, Typst, YAML, Zig

### What "adding a language" means in practice

| Need | Action | Effort |
|------|--------|--------|
| **Syntax highlighting** for a language not in syntect defaults | Add a `.sublime-syntax` file to `crates/fresh-editor/src/grammars/` | Medium |
| **LSP support** for a language already highlighted | Add server config to `config.example.json` | Low |
| **Better indentation/brackets** for an existing language | Add tree-sitter parser + indent queries | High |
| **Extension mapping** for an existing grammar | Add to `file_extensions` list or `filename_scopes` | Low |

### Priority fixes (bugs in existing support)

1. Fix Diff scope-to-category mapping (markup.inserted/deleted not handled)
2. Fix .mts tree-sitter highlighting (detected but not rendered)
3. Add Dockerfile TextMate grammar (tree-sitter detects but can't highlight)
4. Map Jenkinsfile -> Groovy grammar
5. Map .cjs -> JavaScript grammar
