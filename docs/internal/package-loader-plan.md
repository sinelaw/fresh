# Move Package Loading from pkg Plugin to Rust

## Problem

The `pkg` plugin's startup `loadInstalledPackages()` function loads language
packs and bundles via async JS callbacks. This serializes grammar rebuilds
behind `await reloadGrammars()` calls, causing cascading background builds
that take ~90s under constrained CPU (3 builds instead of 1).

The root cause is an architectural split: Rust loads only grammars from
`languages/packages/`, while the pkg plugin loads everything else (language
config, LSP config, bundle grammars, bundle plugins). The pkg plugin
exists because Rust was never taught to read those manifest fields.

## Goal

Move all "load installed packages at startup" logic into Rust. The pkg
plugin becomes install/uninstall/browse only — no startup loading.

This eliminates all async grammar rebuilds from plugin callbacks. The
background grammar build happens once, including all grammar files from
both `languages/packages/` and `bundles/packages/`.

## Current State

### What Rust handles at startup
- Grammars from `~/.config/fresh/languages/packages/` (via `load_language_pack_grammars` in `loader.rs`)
- Plugin discovery from `~/.config/fresh/plugins/packages/*/`
- Parses a minimal `FreshPackageManifest` (only `name` + `fresh.grammar`)

### What the pkg plugin handles at startup (`loadInstalledPackages`)
- Language packs (`languages/packages/`): registers grammar, language config, LSP config via JS API, then `await reloadGrammars()`
- Bundles (`bundles/packages/`): registers grammars + language config + LSP config for each language entry, loads bundle plugins via `await loadPlugin()`, reloads themes

### Rust types that already exist
- `LanguageConfig` in `config.rs` — has `comment_prefix`, `tab_size`, `use_tabs`, `auto_indent`, `formatter`, etc.
- `LspServerConfig` in `types.rs` — has `command`, `args`, `auto_start`, `initialization_options`
- `FreshPackageManifest` in `loader.rs` — only parses `name` + `fresh.grammar` (needs expansion)
- `LanguagePackConfig` in `fresh_core::api` — already has `comment_prefix`, `block_comment_start`, etc. (used by plugin API)
- `LspServerPackConfig` in `fresh_core::api` — already has `command`, `args`, `auto_start`, etc.

### Schema
- `plugins/schemas/package.schema.json` — hand-maintained JSON schema covering all package types
- CONTRIBUTING.md line 50: "Package schema: Manually maintained"

### Theme loader
- `view/theme/loader.rs` already scans `~/.config/fresh/themes/packages/*/` with `package.json`
  manifest support. The gap is only loading themes from `bundles/packages/*/` — which means
  adding one more scan directory to the existing `load_all()` method.

### Recent changes on master (parallel plugin loading)
- `7a63ee07` — Plugin loading is now two-phase: Phase 1 reads files and
  transpiles TS→JS in parallel using `std::thread::scope`; Phase 2 executes
  prepared JS serially in QuickJS in topologically-sorted order.
- `faff0a47` — Plugins can declare dependencies via `import type { T } from
  "fresh:plugin/name"`. Dependencies are extracted during Phase 1 and used
  for topological sorting in Phase 2.
- Bundle plugin dirs added to the `plugin_dirs` list will automatically get
  the parallel prepare → serial execute treatment. No special handling needed.
- The `LoadPlugin` request (used by JS `editor.loadPlugin()` for dynamic
  single-plugin loads) still uses the old serial path. Moving bundle plugins
  to Rust plugin dirs means they use the faster parallel path instead.

## Design

### 1. Define `PackageManifest` Rust struct

A single serde struct matching the full `package.schema.json` schema. Lives in
a new module `crates/fresh-editor/src/services/packages.rs`.

Fields use `#[serde(default)]` liberally so that unknown or missing fields
are silently ignored — this ensures forward compatibility with manifests
written for newer versions of Fresh.

```rust
#[derive(Debug, Deserialize, JsonSchema)]
pub struct PackageManifest {
    pub name: String,
    #[serde(default)]
    pub version: Option<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(rename = "type", default)]
    pub package_type: Option<PackageType>,
    #[serde(default)]
    pub fresh: Option<FreshManifestConfig>,
    // author, license, repository, keywords — not needed at load time
}

#[derive(Debug, Deserialize, JsonSchema)]
#[serde(rename_all = "kebab-case")]
pub enum PackageType {
    Plugin,
    Theme,
    ThemePack,
    Language,
    Bundle,
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct FreshManifestConfig {
    pub grammar: Option<GrammarManifestConfig>,
    pub language: Option<LanguageManifestConfig>,
    pub lsp: Option<LspManifestConfig>,
    pub languages: Option<Vec<BundleLanguage>>,  // bundles
    pub plugins: Option<Vec<BundlePlugin>>,      // bundles
    pub themes: Option<Vec<BundleTheme>>,         // bundles
    pub entry: Option<String>,                    // plugins
    pub main: Option<String>,                     // alias for entry
    // ...
}

#[derive(Debug, Deserialize, JsonSchema)]
pub struct BundleLanguage {
    pub id: String,
    pub grammar: Option<GrammarManifestConfig>,
    pub language: Option<LanguageManifestConfig>,
    pub lsp: Option<LspManifestConfig>,
}
```

The `LanguageManifestConfig` and `LspManifestConfig` use camelCase serde
rename to match the JSON schema (`commentPrefix`, `autoStart`, etc.), then
convert to the existing Rust `LanguageConfig` / `LspServerConfig` types.

### 2. Generate `package.schema.json` from Rust

Add a `"package"` arm to `src/bin/generate_schema.rs`:

```rust
"package" => {
    let schema = schema_for!(PackageManifest);
    serde_json::to_value(&schema).expect("...")
}
```

Update `scripts/gen_schema.sh` to also generate the package schema. Update
CONTRIBUTING.md to remove the "manually maintained" note.

### 3. Package scanner: `scan_installed_packages()`

A function (in the new module) that runs during `Editor::new()`, **before**
plugin loading:

```rust
pub struct PackageScanResult {
    /// Language configs to insert into Config.languages (package defaults)
    pub language_configs: Vec<(String, LanguageConfig)>,
    /// LSP configs to apply
    pub lsp_configs: Vec<(String, LspServerConfig)>,
    /// Additional grammar files for the background build
    /// (bundle grammars not already in languages/packages/)
    pub additional_grammars: Vec<GrammarSpec>,
    /// Bundle plugin directories to add to the plugin loading list
    pub bundle_plugin_dirs: Vec<PathBuf>,
    /// Bundle theme directories for theme reloading
    pub bundle_theme_dirs: Vec<PathBuf>,
}
```

Config precedence: **user config wins over package defaults**. The scanner
uses `or_insert` (not `merge_from`) — package configs are inserted first,
then user config overlays them during the normal config merge. This avoids
the need for a `merge_from` method on `LanguageConfig`.

### 4. Integrate into `Editor::new()` (in `with_options`)

Insert the scan between config creation and plugin loading (~line 1210):

```rust
// Scan installed packages (language packs + bundles)
let scan_result = packages::scan_installed_packages(&dir_context.config_dir);

// Apply language configs (package defaults, user config takes priority)
for (lang_id, lang_config) in scan_result.language_configs {
    config.languages.entry(lang_id).or_insert(lang_config);
}

// Apply LSP configs (package defaults)
for (lang_id, lsp_config) in scan_result.lsp_configs {
    config.lsp.entry(lang_id).or_insert(lsp_config);
}

// Add bundle plugin dirs to the plugin loading list
for dir in scan_result.bundle_plugin_dirs {
    plugin_dirs.push(dir);
}

// Store additional grammars for the deferred background build
editor.pending_grammars.extend(scan_result.additional_grammars);
```

Bundle plugin dirs are added to `plugin_dirs` before the plugin loading loop.
This means bundle plugins go through the same parallel prepare → serial
execute pipeline (from `7a63ee07`) as all other plugins. They benefit from
parallel I/O and transpilation, participate in dependency-based topological
sorting (from `faff0a47`), and have first-writer-wins collision detection
(from `26a03625`).

This is strictly better than the current `editor.loadPlugin()` path, which
loads each bundle plugin serially via a one-off `LoadPlugin` request during
JS callback resolution — bypassing parallel preparation and dependency
ordering entirely.

### 5. Extend grammar loader to scan `bundles/packages/`

Extend the grammar loader to also scan `bundles/packages/*/` for grammar
files. The `LocalGrammarLoader` grows a `bundles_packages_dir()` method,
and `load_language_pack_grammars` is generalized to also handle bundle
manifests (which have `fresh.languages[].grammar` instead of `fresh.grammar`).

This means all grammars are built in a single `builder.build()` pass —
zero grammar rebuilds from plugin callbacks.

### 6. Extend theme loader for bundle themes

The theme loader's `load_all()` already scans `themes/packages/*/`. Add a
second scan for `bundles/packages/*/` using the same `load_package_themes()`
function. This is a small addition (~10 lines) since the theme loading
infrastructure already handles manifest-based theme discovery.

### 7. Remove `loadInstalledPackages()` from pkg plugin

Delete the startup IIFE at the bottom of `pkg.ts` (lines 3042-3066). The
`loadLanguagePack()` and `loadBundle()` functions stay — they're still
needed for dynamic install (when the user installs a package at runtime via
the package manager UI).

### 8. Update CONTRIBUTING.md

Change line 50 from:
```
- **Package schema** (`plugins/schemas/package.schema.json`): Manually maintained
```
to:
```
- **Package schema** (`plugins/schemas/package.schema.json`): Auto-generated from Rust types. Run: `./scripts/gen_schema.sh`
```

## Ordering / Commits

1. **Add `PackageManifest` struct + schema generation** — new Rust types,
   regenerate `package.schema.json`, update CONTRIBUTING.md. No behavior change.

2. **Add `scan_installed_packages()`** — new function, not yet called. Unit
   tests with mock package directories.

3. **Integrate scan into `Editor::new()` + extend grammar loader for bundles
   + extend theme loader for bundle themes** — apply language/LSP configs,
   add bundle plugin dirs, pass bundle grammars to background build, load
   bundle themes.

4. **Remove `loadInstalledPackages()` from pkg plugin** — the startup
   loader is now dead code.

## What Stays in the pkg Plugin

- `pkg_list` / `pkg_install_url` commands (install, uninstall, browse)
- Registry sync and search
- Lockfile management
- `loadLanguagePack()` / `loadBundle()` for **runtime** install (not startup)
- Package validation

## Risks

- **Manifest compatibility**: The Rust struct must parse all existing
  `package.json` files without error. Use `#[serde(default)]` liberally and
  test against real installed packages. Non-required fields should be
  `Option<T>` or have defaults.

- **Ordering**: Language/LSP configs must be applied before plugins load, so
  plugins that query language config during init see the right values. The
  scan runs before the plugin loading loop, so this is satisfied.

- **Config precedence**: Package configs provide defaults. User-defined
  language/LSP configs in `config.toml` must always take priority. Using
  `entry().or_insert()` ensures user config is never overwritten.

- **Bundle plugin loading**: Bundle plugins currently load via
  `editor.loadPlugin()` in JS, which uses the serial `LoadPlugin` request —
  bypassing parallel preparation, dependency sorting, and collision detection.
  Moving them to the Rust `plugin_dirs` list means they go through the same
  two-phase parallel pipeline as all other plugins. This is strictly better:
  faster (parallel I/O and transpilation), correct ordering (topological sort
  respects their dependencies), and safer (first-writer-wins collision
  detection applies).

- **Themes from bundles**: The theme loader already handles `themes/packages/`
  with manifest support. Adding `bundles/packages/` is a small addition
  (~10 lines) using the existing `load_package_themes()` function.

- **Bundle plugins with dependencies on embedded plugins**: If a bundle
  plugin imports from an embedded plugin (e.g., `import type { T } from
  "fresh:plugin/some-embedded"`), the dependency system already handles this —
  topological sort works across all plugin directories. The only requirement
  is that the embedded plugin is in the same `PreparedPlugin` set, which it
  will be since all dirs are prepared together in Phase 1.
