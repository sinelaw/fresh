# Language & Syntax Highlighting Support Review

## Summary

This document catalogs all language support requests from GitHub issues, combined with
common DevOps/infrastructure languages and file types that developers expect modern
editors to handle. It compares against Fresh's current support levels.

### Support Level Key

| Level | Meaning |
|-------|---------|
| **Tree-sitter** | Full semantic highlighting, indentation, folding |
| **Syntect** | TextMate grammar-based highlighting (built-in via syntect defaults) |
| **Embedded grammar** | Custom .sublime-syntax bundled with Fresh |
| **LSP config** | Built-in LSP server configuration in config.example.json |
| **None** | No current support |

---

## Languages Requested in GitHub Issues

| Language | Issue(s) | Status | Current Support | What's Missing |
|----------|----------|--------|-----------------|----------------|
| Dart | [#1252](https://github.com/sinelaw/fresh/issues/1252) | Open | Syntect (basic) | Tree-sitter, LSP config (dart analyze) |
| KDL | [#1266](https://github.com/sinelaw/fresh/issues/1266) | Open | None | Grammar, tree-sitter |
| Hyprlang | [#1266](https://github.com/sinelaw/fresh/issues/1266) | Open | None | Grammar |
| Nushell | [#1031](https://github.com/sinelaw/fresh/issues/1031) | Open | None | Grammar, tree-sitter |
| Solidity | [#857](https://github.com/sinelaw/fresh/issues/857) | Closed | None | Grammar |
| Deno (TS variant) | [#1191](https://github.com/sinelaw/fresh/issues/1191) | Open | TypeScript supported | Deno LSP auto-detection |
| LaTeX | [#807](https://github.com/sinelaw/fresh/issues/807) | Closed/Added | Syntect + LSP config | Tree-sitter |
| Zig | [#737](https://github.com/sinelaw/fresh/issues/737) | Closed/Added | Embedded grammar + LSP | Tree-sitter |
| Bash/Zsh configs | [#383](https://github.com/sinelaw/fresh/issues/383) | Closed/Added | Tree-sitter | Done |
| Config file fallback | [#1219](https://github.com/sinelaw/fresh/issues/1219) | Open | N/A | Fallback grammar for .conf, .rc, .rules, etc. |

---

## Common DevOps / Infrastructure Languages & Files

| Language / File Type | File Extensions / Names | Current Support | Recommendation |
|---------------------|------------------------|-----------------|----------------|
| **Dockerfile** | `Dockerfile`, `Dockerfile.*`, `*.dockerfile` | Syntect + LSP config | Add tree-sitter |
| **Docker Compose** | `docker-compose.yml`, `compose.yml` | YAML (Syntect) | Adequate (YAML grammar) |
| **Makefile** | `Makefile`, `*.mk`, `GNUmakefile` | Syntect + LSP config | Add tree-sitter |
| **CMake** | `CMakeLists.txt`, `*.cmake` | Syntect | Add LSP config (cmake-language-server) |
| **Terraform / HCL** | `*.tf`, `*.tfvars`, `*.hcl` | None | Add grammar + LSP config (terraform-ls) |
| **Helm templates** | `*.tpl` (in chart dirs) | None | Add Go template grammar |
| **Nix** | `*.nix`, `flake.nix` | Syntect | Add tree-sitter, LSP config (nil/nixd) |
| **Shell (POSIX)** | `*.sh`, `*.bash`, `*.zsh` | Tree-sitter + LSP | Done |
| **PowerShell** | `*.ps1`, `*.psm1`, `*.psd1` | Syntect | Add LSP config (powershell-editor-services) |
| **YAML** | `*.yml`, `*.yaml` | Syntect + LSP config | Add tree-sitter |
| **TOML** | `*.toml` | Embedded grammar + LSP | Add tree-sitter |
| **JSON / JSONC** | `*.json`, `*.jsonc` | Tree-sitter + LSP | Done |
| **XML** | `*.xml`, `*.xsl`, `*.xsd`, `*.svg` | Syntect | Add tree-sitter |
| **INI / CONF** | `*.ini`, `*.conf`, `*.cfg`, `*.env` | Syntect (INI) | Add .env grammar |
| **Protobuf** | `*.proto` | None | Add grammar + LSP config (buf-language-server) |
| **GraphQL** | `*.graphql`, `*.gql` | None | Add grammar + LSP config (graphql-lsp) |
| **SQL** | `*.sql` | Syntect | Add tree-sitter, LSP config (sqls) |

---

## Common Programming Languages (Not Yet Fully Supported)

| Language | File Extensions | Current Support | Recommendation |
|----------|----------------|-----------------|----------------|
| **Kotlin** | `*.kt`, `*.kts` | Syntect | Add tree-sitter, LSP config (kotlin-language-server) |
| **Swift** | `*.swift` | Syntect | Add tree-sitter, LSP config (sourcekit-lsp) |
| **Scala** | `*.scala`, `*.sc` | Syntect | Add tree-sitter, LSP config (metals) |
| **Elixir** | `*.ex`, `*.exs` | Syntect | Add tree-sitter, LSP config (elixir-ls) |
| **Erlang** | `*.erl`, `*.hrl` | Syntect | Add tree-sitter, LSP config (erlang_ls) |
| **Haskell** | `*.hs`, `*.lhs` | Syntect | Add tree-sitter, LSP config (haskell-language-server) |
| **OCaml** | `*.ml`, `*.mli` | Syntect | Add tree-sitter, LSP config (ocamllsp) |
| **Clojure** | `*.clj`, `*.cljs`, `*.cljc` | Syntect | Add tree-sitter, LSP config (clojure-lsp) |
| **R** | `*.r`, `*.R`, `*.Rmd` | Syntect | Add LSP config (languageserver) |
| **Julia** | `*.jl` | None | Add grammar + tree-sitter, LSP config (LanguageServer.jl) |
| **Perl** | `*.pl`, `*.pm` | Syntect | Add LSP config (Perl::LanguageServer) |
| **Dart** | `*.dart` | Syntect (basic) | Add tree-sitter, LSP config (dart analyze) |
| **V** | `*.v` | None | Add grammar |
| **Nim** | `*.nim`, `*.nims` | None | Add grammar, LSP config (nimlangserver) |
| **Gleam** | `*.gleam` | None | Add grammar, LSP config (gleam lsp) |

---

## Web Frontend Languages

| Language | File Extensions | Current Support | Recommendation |
|----------|----------------|-----------------|----------------|
| **Vue** | `*.vue` | None | Add grammar + LSP config (vue-language-server) |
| **Svelte** | `*.svelte` | None | Add grammar + LSP config (svelte-language-server) |
| **Astro** | `*.astro` | None | Add grammar + LSP config (@astrojs/language-server) |
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
| **Markdown** | `*.md`, `*.mdx` | Syntect + LSP config | Add tree-sitter |
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

| # | Language/File | Reason |
|---|--------------|--------|
| 1 | **Terraform / HCL** | Very common in DevOps, no support at all |
| 2 | **Dockerfile** (tree-sitter) | Already has syntect, upgrade to tree-sitter |
| 3 | **Nix** (tree-sitter + LSP) | Already has syntect, project uses Nix, user community overlap |
| 4 | **Dart** | Requested in #1252, growing language |
| 5 | **Vue / Svelte** | Very popular web frameworks, no support |
| 6 | **Protobuf** | Common in microservices, no support |
| 7 | **GraphQL** | Common in web APIs, no support |
| 8 | **Kotlin** | Major Android/JVM language, only syntect |
| 9 | **YAML** (tree-sitter) | Already has syntect, ubiquitous in DevOps |
| 10 | **SQL** (tree-sitter + LSP) | Already has syntect, very common |

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

- **Tree-sitter (18):** Rust, Python, JavaScript, TypeScript, HTML, CSS, C, C++, Go, JSON, Java, C#, PHP, Ruby, Bash, Lua, Pascal, Odin
- **Embedded grammars (9):** TOML, Odin, Zig, Typst, Git Rebase, Git Commit, Gitignore, Git Config, Git Attributes
- **LSP configs (23):** Bash, C, C++, C#, CSS, Go, HTML, Java, JavaScript, JSON, LaTeX, Lua, Markdown, PHP, Python, Ruby, Rust, Templ, TOML, TypeScript, Typst, YAML, Zig
- **Syntect defaults (~50+):** Broad coverage via TextMate grammars including many languages listed above
