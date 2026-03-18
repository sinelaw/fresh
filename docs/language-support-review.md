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
| Config file fallback | [#1219](https://github.com/sinelaw/fresh/issues/1219) | Open | N/A | Fallback grammar for .conf, .rc, .rules, etc. |

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

## Current Support Summary

- **Syntect defaults (~100+):** Primary highlighting engine. Broad coverage via built-in TextMate grammars (the full Sublime Text default grammar set)
- **Embedded grammars (9):** Custom .sublime-syntax files bundled with Fresh for specialized/improved highlighting: TOML, Odin, Zig, Typst, Git Rebase, Git Commit, Gitignore, Git Config, Git Attributes
- **Tree-sitter (18):** Used for auto-indentation, bracket matching, and semantic features (NOT primary highlighting): Rust, Python, JavaScript, TypeScript, HTML, CSS, C, C++, Go, JSON, Java, C#, PHP, Ruby, Bash, Lua, Pascal, Odin
- **LSP configs (23):** Bash, C, C++, C#, CSS, Go, HTML, Java, JavaScript, JSON, LaTeX, Lua, Markdown, PHP, Python, Ruby, Rust, Templ, TOML, TypeScript, Typst, YAML, Zig

### What "adding a language" means in practice

| Need | Action | Effort |
|------|--------|--------|
| **Syntax highlighting** for a language not in syntect defaults | Add a `.sublime-syntax` file to `crates/fresh-editor/src/grammars/` | Medium |
| **LSP support** for a language already highlighted | Add server config to `config.example.json` | Low |
| **Better indentation/brackets** for an existing language | Add tree-sitter parser + indent queries | High |
| **Highlighting already works** (via syntect defaults) | Nothing needed for highlighting; just add LSP config | Low |

Most languages in the tables above that show "Syntect" already have working syntax
highlighting. The main gap is **LSP configurations** (for autocomplete, diagnostics,
go-to-definition) and **TextMate grammars for niche languages** not in syntect's defaults.
