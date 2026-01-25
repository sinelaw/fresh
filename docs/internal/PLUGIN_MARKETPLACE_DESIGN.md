# Plugin & Theme Marketplace Design

## Executive Summary

This document proposes a **decentralized, git-based package system** for Fresh plugins and themes. The design prioritizes simplicity, user control, and minimal editor complexity by leveraging git as the underlying distribution mechanism—similar to how Emacs package managers (straight.el, elpaca) and Neovim (lazy.nvim, packer) approach the problem.

The core innovation: **the package manager itself is a plugin**, keeping the editor lean while providing full package management capabilities.

---

## Design Principles

### 1. Git as the Distribution Layer
- Every plugin/theme is a git repository
- Users can install from any git URL (GitHub, GitLab, self-hosted, local)
- No centralized package server required
- Versioning uses git tags and commits
- Updates are `git pull` operations

### 2. Decentralization First
- No mandatory registry—users can install any repo directly
- Optional curated lists are just git repos containing metadata
- Community can fork and maintain their own lists
- Multiple sources can coexist

### 3. Minimal Editor Footprint
- Editor core only knows how to load plugins from disk
- Package management logic lives in a "package manager" plugin
- Users can choose different package managers or write their own
- Zero new Rust code for marketplace features

### 4. User Control
- All packages stored in user-readable directories
- No binary blobs or opaque package formats
- Users can manually edit, fork, or patch any package
- Configuration is plain JSON

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                           Fresh Editor                               │
│  ┌─────────────────┐                                                │
│  │   Plugin Loader │ ← Loads .ts files from ~/.config/fresh/plugins │
│  └────────┬────────┘                                                │
│           │                                                          │
│           ▼                                                          │
│  ┌─────────────────────────────────────────────────────────────┐    │
│  │              Package Manager Plugin (pkg.ts)                 │    │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌─────────────┐  │    │
│  │  │ Install  │  │  Update  │  │  Remove  │  │ List/Search │  │    │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘  └──────┬──────┘  │    │
│  │       │             │             │               │          │    │
│  │       └─────────────┴─────────────┴───────────────┘          │    │
│  │                           │                                   │    │
│  │                    editor.spawnProcess()                      │    │
│  │                           │                                   │    │
│  │                           ▼                                   │    │
│  │                  ┌─────────────────┐                          │    │
│  │                  │   Git Commands  │                          │    │
│  │                  │  clone/pull/tag │                          │    │
│  │                  └─────────────────┘                          │    │
│  └─────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────┘

                               │
                               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                     Package Sources (Git Repos)                      │
│                                                                      │
│  ┌─────────────────────────┐  ┌──────────────────────────────────┐  │
│  │  Official Registry Repo │  │     User's Private Repo          │  │
│  │  (fresh-plugins/index)  │  │  github.com/user/my-plugin       │  │
│  │  ┌─────────────────┐    │  └──────────────────────────────────┘  │
│  │  │ plugins.json    │    │                                        │
│  │  │ themes.json     │    │  ┌──────────────────────────────────┐  │
│  │  └─────────────────┘    │  │   Community Index Repo           │  │
│  └─────────────────────────┘  │   (awesome-fresh-plugins/index)  │  │
│                               └──────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Directory Structure

### User's System

```
~/.config/fresh/
├── config.json              # Editor config (includes package list)
├── plugins/
│   ├── welcome.ts           # User's direct plugin files
│   └── packages/            # Git-managed packages
│       ├── vim-mode/        # git clone of vim-mode plugin
│       │   ├── .git/
│       │   ├── package.json # Package manifest
│       │   ├── main.ts      # Entry point
│       │   └── lib/
│       ├── rainbow-brackets/
│       │   ├── .git/
│       │   ├── package.json
│       │   └── main.ts
│       └── .index/          # Cached registry data (git repo)
│           ├── .git/
│           ├── plugins.json
│           └── themes.json
└── themes/
    ├── my-custom.json       # User's direct theme files
    └── packages/            # Git-managed themes
        ├── catppuccin/
        │   ├── .git/
        │   ├── package.json
        │   ├── mocha.json
        │   ├── latte.json
        │   └── frappe.json
        └── tokyo-night/
            ├── .git/
            └── tokyo-night.json
```

---

## Package Manifest Format

Every package has a `package.json` (or `fresh.json`) at its root:

```json
{
  "name": "rainbow-brackets",
  "version": "1.2.0",
  "description": "Colorize matching brackets for easier visual parsing",
  "type": "plugin",
  "author": "Jane Developer <jane@example.com>",
  "license": "MIT",
  "repository": "https://github.com/jane/fresh-rainbow-brackets",

  "fresh": {
    "min_version": "0.1.80",
    "entry": "main.ts",
    "config_schema": {
      "colors": {
        "type": "array",
        "default": ["#ff0000", "#00ff00", "#0000ff"],
        "description": "Colors to cycle through for bracket pairs"
      }
    }
  },

  "keywords": ["brackets", "colors", "syntax"],

  "dependencies": {}
}
```

### Theme Package Manifest

```json
{
  "name": "catppuccin",
  "version": "2.0.0",
  "description": "Soothing pastel theme collection",
  "type": "theme-pack",
  "author": "Catppuccin Team",
  "license": "MIT",
  "repository": "https://github.com/catppuccin/fresh",

  "fresh": {
    "min_version": "0.1.75",
    "themes": [
      { "file": "mocha.json", "name": "Catppuccin Mocha", "variant": "dark" },
      { "file": "latte.json", "name": "Catppuccin Latte", "variant": "light" },
      { "file": "frappe.json", "name": "Catppuccin Frappé", "variant": "dark" },
      { "file": "macchiato.json", "name": "Catppuccin Macchiato", "variant": "dark" }
    ]
  },

  "keywords": ["pastel", "dark", "light", "colorful"]
}
```

---

## Registry Format

The registry is a git repository containing JSON indices:

### `plugins.json`

```json
{
  "schema_version": 1,
  "updated": "2025-01-15T10:30:00Z",
  "packages": {
    "rainbow-brackets": {
      "description": "Colorize matching brackets",
      "repository": "https://github.com/jane/fresh-rainbow-brackets",
      "author": "Jane Developer",
      "license": "MIT",
      "keywords": ["brackets", "colors"],
      "stars": 142,
      "downloads": 5230,
      "latest_version": "1.2.0",
      "fresh_min_version": "0.1.80"
    },
    "vim-mode": {
      "description": "Vim keybindings for Fresh",
      "repository": "https://github.com/bob/fresh-vim-mode",
      "author": "Bob Vimmer",
      "license": "MIT",
      "keywords": ["vim", "modal", "keybindings"],
      "stars": 890,
      "downloads": 12500,
      "latest_version": "3.1.0",
      "fresh_min_version": "0.1.85"
    }
  }
}
```

### `themes.json`

```json
{
  "schema_version": 1,
  "updated": "2025-01-15T10:30:00Z",
  "packages": {
    "catppuccin": {
      "description": "Soothing pastel theme collection",
      "repository": "https://github.com/catppuccin/fresh",
      "author": "Catppuccin Team",
      "license": "MIT",
      "variants": ["mocha", "latte", "frappe", "macchiato"],
      "keywords": ["pastel", "dark", "light"],
      "stars": 2100,
      "downloads": 45000
    },
    "tokyo-night": {
      "description": "Clean dark theme with Tokyo city lights colors",
      "repository": "https://github.com/tokyo-night/fresh",
      "author": "Tokyo Night Team",
      "license": "MIT",
      "variants": ["night", "storm", "day"],
      "keywords": ["dark", "blue", "purple"],
      "stars": 1800
    }
  }
}
```

---

## User Configuration

### `config.json` Package Section

```json
{
  "theme": "catppuccin-mocha",

  "packages": {
    "sources": [
      "https://github.com/sinelaw/fresh-plugins-registry",
      "https://github.com/awesome-fresh/community-index"
    ],

    "plugins": {
      "rainbow-brackets": {
        "enabled": true,
        "source": "https://github.com/jane/fresh-rainbow-brackets",
        "version": "^1.2.0",
        "config": {
          "colors": ["#e06c75", "#98c379", "#61afef", "#c678dd"]
        }
      },
      "vim-mode": {
        "enabled": true,
        "version": "3.1.0",
        "config": {
          "leader": " "
        }
      },
      "my-experimental": {
        "enabled": true,
        "source": "~/code/my-fresh-plugin",
        "version": "local"
      }
    },

    "themes": {
      "catppuccin": {
        "source": "https://github.com/catppuccin/fresh",
        "version": "latest"
      }
    }
  },

  "plugins": {
    "welcome": { "enabled": true },
    "git_grep": { "enabled": true }
  }
}
```

---

## Version Specification

Packages support multiple version specification formats:

| Format | Meaning | Example |
|--------|---------|---------|
| `"1.2.0"` | Exact version (git tag `v1.2.0`) | Pin to specific release |
| `"^1.2.0"` | Compatible (>= 1.2.0, < 2.0.0) | Semver compatible |
| `"~1.2.0"` | Patch updates only (>= 1.2.0, < 1.3.0) | Conservative updates |
| `"latest"` | Latest tag or HEAD | Always newest |
| `"main"` | Specific branch | Track development |
| `"abc1234"` | Specific commit | Exact reproducibility |
| `"local"` | Local directory, no git | Development mode |

---

## Monorepo Support

A single git repository can contain multiple packages. Use URL fragments to specify a subdirectory:

```
https://github.com/user/fresh-plugins#packages/rainbow-brackets
https://github.com/user/fresh-plugins#packages/vim-mode
https://github.com/user/fresh-plugins#themes/catppuccin
```

### URL Format

```
<repo-url>#<path/to/package>
```

- **repo-url**: Standard git clone URL
- **path/to/package**: Directory path within the repository

### Installation Process

For monorepo packages:
1. Clone the full repository to a temporary directory
2. Copy the specified subdirectory to the packages folder
3. Create a `.fresh-source.json` file to track the original source
4. Clean up the temporary clone

### Registry Support

Registries can list monorepo packages:

```json
{
  "rainbow-brackets": {
    "description": "Colorize matching brackets",
    "repository": "https://github.com/user/fresh-plugins#packages/rainbow-brackets",
    "author": "User"
  }
}
```

### Benefits

- **Reduced maintenance**: One repo for multiple related plugins
- **Shared dependencies**: Common code in parent directories
- **Easier discovery**: Browse all plugins in one place
- **Atomic releases**: Tag the monorepo to release multiple plugins

---

## Command Palette Commands

The package manager plugin registers these commands:

| Command | Description |
|---------|-------------|
| `pkg: Install Plugin` | Browse registry and install a plugin |
| `pkg: Install Theme` | Browse registry and install a theme |
| `pkg: Install from URL` | Install directly from git URL |
| `pkg: Update All` | Update all installed packages |
| `pkg: Update Plugin` | Select and update a specific plugin |
| `pkg: Remove Plugin` | Remove an installed plugin |
| `pkg: Remove Theme` | Remove an installed theme |
| `pkg: List Installed` | Show all installed packages |
| `pkg: Search` | Search registry for packages |
| `pkg: Sync Registry` | Pull latest registry data |
| `pkg: Show Outdated` | List packages with updates available |
| `pkg: Lock Versions` | Generate lockfile for reproducibility |

---

## Implementation: Package Manager Plugin

### Core Structure (`plugins/pkg.ts`)

```typescript
/// <reference path="../types/fresh.d.ts" />

const PACKAGES_DIR = editor.getConfigDir() + "/plugins/packages";
const THEMES_PACKAGES_DIR = editor.getConfigDir() + "/themes/packages";
const INDEX_DIR = PACKAGES_DIR + "/.index";

interface PackageInfo {
  name: string;
  description: string;
  repository: string;
  version: string;
  installed_version?: string;
  type: "plugin" | "theme" | "theme-pack";
}

// ─────────────────────────────────────────────────────────────────
// Installation
// ─────────────────────────────────────────────────────────────────

globalThis.pkg_install = async function(): Promise<void> {
  // 1. Load registry
  const plugins = await loadRegistry("plugins");

  // 2. Show picker
  const items = Object.entries(plugins.packages).map(([name, info]) => ({
    label: name,
    description: info.description,
    detail: `★ ${info.stars} | v${info.latest_version}`,
    data: { name, ...info }
  }));

  editor.startPrompt("Install plugin:", "pkg-install");
  editor.setPromptSuggestions(items);
};

globalThis.pkg_install_confirm = async function(): Promise<void> {
  const selection = editor.getPromptSelection();
  if (!selection) return;

  const { name, repository } = selection.data;
  editor.setStatus(`Installing ${name}...`);

  const targetDir = `${PACKAGES_DIR}/${name}`;
  const result = await editor.spawnProcess("git", [
    "clone", "--depth", "1", repository, targetDir
  ]);

  if (result.exit_code === 0) {
    // Add to config
    await addPackageToConfig(name, repository);
    editor.setStatus(`Installed ${name} successfully. Restart to activate.`);
  } else {
    editor.setStatus(`Failed to install ${name}: ${result.stderr}`);
  }
};

// ─────────────────────────────────────────────────────────────────
// Updates
// ─────────────────────────────────────────────────────────────────

globalThis.pkg_update_all = async function(): Promise<void> {
  const packages = await getInstalledPackages();
  let updated = 0;
  let failed = 0;

  for (const pkg of packages) {
    editor.setStatus(`Updating ${pkg.name}...`);

    const result = await editor.spawnProcess("git", [
      "-C", pkg.path, "pull", "--ff-only"
    ]);

    if (result.exit_code === 0) {
      if (!result.stdout.includes("Already up to date")) {
        updated++;
      }
    } else {
      failed++;
    }
  }

  editor.setStatus(`Update complete: ${updated} updated, ${failed} failed`);
};

// ─────────────────────────────────────────────────────────────────
// Version Management
// ─────────────────────────────────────────────────────────────────

async function checkoutVersion(pkgPath: string, version: string): Promise<boolean> {
  let target: string;

  if (version === "latest") {
    // Get latest tag
    const tags = await editor.spawnProcess("git", [
      "-C", pkgPath, "tag", "--sort=-v:refname"
    ]);
    target = tags.stdout.split("\n")[0] || "HEAD";
  } else if (version.startsWith("^") || version.startsWith("~")) {
    // Semver matching - find best matching tag
    target = await findMatchingVersion(pkgPath, version);
  } else {
    target = version.startsWith("v") ? version : `v${version}`;
  }

  const result = await editor.spawnProcess("git", [
    "-C", pkgPath, "checkout", target
  ]);

  return result.exit_code === 0;
}

// ─────────────────────────────────────────────────────────────────
// Registry Management
// ─────────────────────────────────────────────────────────────────

async function syncRegistry(): Promise<void> {
  const sources = await getRegistrySources();

  for (const source of sources) {
    const indexPath = `${INDEX_DIR}/${hashSource(source)}`;

    if (editor.fileExists(indexPath)) {
      await editor.spawnProcess("git", ["-C", indexPath, "pull"]);
    } else {
      await editor.spawnProcess("git", [
        "clone", "--depth", "1", source, indexPath
      ]);
    }
  }
}

async function loadRegistry(type: "plugins" | "themes"): Promise<RegistryData> {
  const sources = await getRegistrySources();
  const merged: RegistryData = { packages: {} };

  for (const source of sources) {
    const indexPath = `${INDEX_DIR}/${hashSource(source)}/${type}.json`;
    if (editor.fileExists(indexPath)) {
      const content = await editor.readFile(indexPath);
      const data = JSON.parse(content);
      Object.assign(merged.packages, data.packages);
    }
  }

  return merged;
}

// ─────────────────────────────────────────────────────────────────
// Install from URL (unlisted packages)
// ─────────────────────────────────────────────────────────────────

globalThis.pkg_install_url = async function(): Promise<void> {
  editor.startPrompt("Git URL:", "pkg-install-url");
};

globalThis.pkg_install_url_confirm = async function(): Promise<void> {
  const url = editor.getPromptText();
  if (!url) return;

  // Extract name from URL
  const name = url.split("/").pop()?.replace(/\.git$/, "") || "unknown";

  editor.setStatus(`Installing from ${url}...`);

  const targetDir = `${PACKAGES_DIR}/${name}`;
  const result = await editor.spawnProcess("git", [
    "clone", "--depth", "1", url, targetDir
  ]);

  if (result.exit_code === 0) {
    await addPackageToConfig(name, url);
    editor.setStatus(`Installed ${name}. Restart to activate.`);
  } else {
    editor.setStatus(`Failed: ${result.stderr}`);
  }
};

// Register commands
editor.registerCommand("pkg_install", "pkg: Install Plugin", "pkg_install", "normal");
editor.registerCommand("pkg_install_url", "pkg: Install from URL", "pkg_install_url", "normal");
editor.registerCommand("pkg_update_all", "pkg: Update All", "pkg_update_all", "normal");
// ... more commands
```

---

## Alternative Designs Considered

### Alternative 1: Centralized Package Server

**Approach**: Host packages on a dedicated server with REST API.

**Pros**:
- Faster metadata queries
- Download statistics
- Verification/signing possible

**Cons**:
- Requires server infrastructure
- Single point of failure
- Maintenance burden
- Against decentralization principle

**Verdict**: Rejected. Git-based approach provides same functionality without centralized dependency.

### Alternative 2: NPM-style Package Format

**Approach**: Use tar.gz archives with checksums.

**Pros**:
- Reproducible builds
- Smaller download size (no .git)
- Familiar to JS developers

**Cons**:
- Requires build/publish step
- Loses git history and easy forking
- Need infrastructure for hosting
- Updates require re-downloading entire package

**Verdict**: Rejected. Git provides better update mechanism and easier contribution workflow.

### Alternative 3: Built-in Package Manager (Rust)

**Approach**: Implement package management in Rust within the editor core.

**Pros**:
- Faster execution
- Tighter integration
- No plugin dependency

**Cons**:
- Increases editor complexity
- Harder to customize
- Requires editor updates for pkg changes
- Duplicates work (git is already a package manager)

**Verdict**: Rejected. Plugin-based approach keeps editor simple and allows customization.

### Alternative 4: Lua-based Configuration (Neovim-style)

**Approach**: Use Lua for package configuration like lazy.nvim.

**Pros**:
- Powerful programmatic configuration
- Conditional logic built-in
- Popular pattern in Neovim

**Cons**:
- Another language for users to learn
- Fresh already uses TypeScript
- JSON is simpler for most use cases

**Verdict**: Rejected. TypeScript plugins can provide same power; JSON config is sufficient.

---

## Registry Hosting Alternatives

### Option A: GitHub Repository (Recommended)

```
github.com/sinelaw/fresh-plugins-registry/
├── README.md
├── CONTRIBUTING.md
├── plugins.json
├── themes.json
└── schemas/
    ├── plugin.schema.json
    └── theme.schema.json
```

**Contribution Flow**:
1. Fork the repository
2. Add package entry to plugins.json or themes.json
3. Submit pull request
4. Maintainers review and merge
5. Users run `pkg: Sync Registry` to get updates

**Validation**: CI/CD validates JSON schema and checks that repos exist.

### Option B: GitHub Releases + API

Use GitHub's REST API to discover packages tagged with `fresh-plugin` or `fresh-theme` topics.

**Pros**: Zero maintenance, automatic discovery
**Cons**: Rate limits, GitHub-only, no curation

### Option C: Distributed Index (IPFS/DNS)

Store index on IPFS with ENS/DNS pointer.

**Pros**: Truly decentralized, censorship-resistant
**Cons**: Complex, slow, overkill for this use case

### Option D: No Central Index

Users share URLs directly. Discovery via:
- GitHub search with topic `fresh-plugin`
- Community lists on Reddit/Discord
- Blog posts and READMEs

**Pros**: Maximum decentralization
**Cons**: Poor discoverability for new users

---

## Lockfile for Reproducibility

### `fresh.lock`

```json
{
  "lockfile_version": 1,
  "generated": "2025-01-15T10:30:00Z",
  "packages": {
    "rainbow-brackets": {
      "source": "https://github.com/jane/fresh-rainbow-brackets",
      "commit": "abc123def456789",
      "version": "1.2.0",
      "integrity": "sha256-xxxxx"
    },
    "vim-mode": {
      "source": "https://github.com/bob/fresh-vim-mode",
      "commit": "def789abc123456",
      "version": "3.1.0",
      "integrity": "sha256-yyyyy"
    }
  }
}
```

**Commands**:
- `pkg: Lock Versions` - Generate lockfile from current state
- `pkg: Install from Lockfile` - Reproduce exact package versions

---

## Security Considerations

### 1. Code Review Before Install

The package manager shows a confirmation dialog:

```
┌─────────────────────────────────────────────────────────────┐
│ Install rainbow-brackets?                                    │
│                                                             │
│ Source: github.com/jane/fresh-rainbow-brackets              │
│ Author: Jane Developer                                       │
│ License: MIT                                                 │
│ Stars: 142 | Downloads: 5230                                │
│                                                             │
│ ⚠ Plugins can execute arbitrary code. Only install from     │
│   sources you trust.                                         │
│                                                             │
│ [Enter] Install   [v] View Source   [Esc] Cancel            │
└─────────────────────────────────────────────────────────────┘
```

### 2. Plugin Sandboxing

Plugins run in QuickJS sandbox with limited capabilities:
- No direct filesystem access (use `editor.readFile()`)
- No network access (except via `editor.spawnProcess()`)
- Process spawning is auditable

### 3. Registry Signing (Future)

Registry maintainers can sign `plugins.json`:

```json
{
  "packages": { ... },
  "signatures": [
    {
      "keyid": "maintainer-1",
      "sig": "base64-signature"
    }
  ]
}
```

### 4. Known-Malicious Package List

Registry includes a `blocklist.json` that the package manager checks:

```json
{
  "blocked": [
    {
      "repository": "https://github.com/bad/malware-plugin",
      "reason": "Contained cryptocurrency miner",
      "blocked_at": "2025-01-10"
    }
  ]
}
```

---

## Theme-Specific Considerations

### Theme Discovery

Themes from packages are discovered by scanning:
1. `~/.config/fresh/themes/*.json` (direct files)
2. `~/.config/fresh/themes/packages/*/package.json` (read theme list)
3. Load each declared theme file

### Theme Activation

```typescript
// In theme loader (Rust side or plugin)
function loadPackageThemes(): Theme[] {
  const packagesDir = `${THEMES_DIR}/packages`;
  const themes: Theme[] = [];

  for (const pkgDir of listDirs(packagesDir)) {
    const manifest = JSON.parse(readFile(`${pkgDir}/package.json`));

    if (manifest.fresh?.themes) {
      for (const themeEntry of manifest.fresh.themes) {
        const themeData = JSON.parse(readFile(`${pkgDir}/${themeEntry.file}`));
        themes.push({
          ...themeData,
          name: themeEntry.name || themeData.name,
          source: manifest.repository
        });
      }
    }
  }

  return themes;
}
```

### Theme Preview

Before installing, show live preview:

```typescript
globalThis.pkg_preview_theme = async function(): Promise<void> {
  const selection = editor.getPromptSelection();
  if (!selection) return;

  // Clone to temp directory
  const tempDir = `/tmp/fresh-theme-preview`;
  await editor.spawnProcess("git", [
    "clone", "--depth", "1", selection.data.repository, tempDir
  ]);

  // Load and apply first theme
  const manifest = JSON.parse(await editor.readFile(`${tempDir}/package.json`));
  const themeFile = manifest.fresh.themes[0].file;
  const themeData = await editor.readFile(`${tempDir}/${themeFile}`);

  // Apply temporarily (requires editor API extension)
  editor.previewTheme(JSON.parse(themeData));

  // Clean up on cancel
  editor.onPromptClose(() => {
    editor.revertTheme();
    editor.spawnProcess("rm", ["-rf", tempDir]);
  });
};
```

---

## Implementation Phases

### Phase 1: Core Package Manager Plugin
- [ ] Basic install/remove from git URL
- [ ] Update single package / all packages
- [ ] List installed packages
- [ ] Simple version pinning (tag/commit)

### Phase 2: Registry Support
- [ ] Create initial registry repo structure
- [ ] Registry sync command
- [ ] Package search/browse UI
- [ ] Merge multiple registry sources

### Phase 3: Enhanced Features
- [ ] Semver version matching
- [ ] Lockfile generation/restore
- [ ] Update notifications
- [ ] Dependency resolution (if needed)

### Phase 4: Polish
- [ ] Theme preview before install
- [ ] Package health indicators (stars, updates)
- [ ] Security warnings for unverified sources
- [ ] Configuration wizard for new installs

---

## Editor Core Changes Required

### Minimal Required Changes

1. **Package directory scanning**: Update plugin loader to scan `~/.config/fresh/plugins/packages/*/main.ts`

2. **Theme package scanning**: Update theme loader to read package manifests

3. **No other changes**: All package management logic lives in the plugin

### Optional Enhancements

1. **`editor.previewTheme(data)`**: Apply theme temporarily without saving
2. **`editor.reloadPlugins()`**: Hot-reload plugins without restart
3. **`editor.getPackageConfig(name)`**: Access package-specific config section

---

## Comparison with Other Editors

| Feature | Fresh (Proposed) | Emacs (straight.el) | Neovim (lazy.nvim) | VS Code |
|---------|-----------------|---------------------|-------------------|---------|
| Distribution | Git repos | Git repos | Git repos | VSIX packages |
| Registry | Optional git repo | MELPA git | None | Centralized |
| Version control | Git tags/commits | Git commits | Git tags/commits | Semver |
| Install location | ~/.config/fresh/plugins/packages | ~/.emacs.d/straight | ~/.local/share/nvim/lazy | ~/.vscode/extensions |
| Update mechanism | git pull | git pull | git pull | VS Code API |
| Config format | JSON | Elisp | Lua | JSON |
| Manager location | Plugin | Package | Plugin | Built-in |

---

## Success Metrics

1. **Simplicity**: Users can install a plugin with one command
2. **Reliability**: Updates work consistently via git
3. **Flexibility**: Any git repo can be installed, not just registered ones
4. **Performance**: Registry sync < 2 seconds, install < 10 seconds
5. **Discoverability**: New users can find popular plugins easily

---

## Open Questions

1. **Dependencies between plugins**: Do we need inter-plugin dependencies? Most editors avoid this complexity.

2. **Automatic updates**: Should packages auto-update on editor start? Probably opt-in only.

3. **Plugin enable/disable**: Currently via config. Should we add quick toggle command?

4. **Conflicting packages**: What if two packages define the same command?

5. **Rollback**: Should we keep previous version for quick rollback on breakage?

---

## Future Work

### TODO: Plugin UI Component Library

Currently, plugins that need rich UI (like the package manager) must manually construct their interface using raw text property entries. This is verbose, error-prone, and leads to code duplication.

We need a **UI component library** that plugins can use to build interfaces in virtual buffers:

- **Buttons**: Focusable, clickable elements with keyboard navigation
- **Lists**: Scrollable, selectable item lists with highlighting
- **Scroll bars**: Visual scroll indicators for long content
- **Text inputs**: In-buffer text entry fields
- **Tabs/Tab bars**: Switchable content panels
- **Split views**: Side-by-side or stacked layouts
- **Progress indicators**: Loading spinners, progress bars
- **Dialogs/Modals**: Overlay prompts for confirmations

This would allow plugins to declaratively define UI:

```typescript
// Hypothetical API
const ui = editor.createUI(bufferId);
ui.header("Package Manager");
ui.tabBar(["All", "Installed", "Plugins", "Themes"], activeTab, onTabChange);
ui.splitView({
  left: ui.list(items, { onSelect, onActivate }),
  right: ui.panel([
    ui.title(selected.name),
    ui.text(selected.description),
    ui.button("Install", onInstall),
  ]),
});
```

Benefits:
- Consistent look and feel across plugins
- Automatic keyboard navigation and focus management
- Theme-aware styling
- Reduced boilerplate in plugin code
- Accessibility support built-in

Note: The editor's settings UI already implements many of these UI elements (dropdowns, toggles, input fields, sections, etc.) but they are not organized into a reusable library. A shared component framework could unify the settings UI implementation with plugin UI needs, reducing duplication and ensuring consistency.

---

## Conclusion

This design provides a **simple, decentralized, git-based package system** that:

- Leverages existing tools (git) instead of reinventing wheels
- Keeps the editor core minimal by implementing management as a plugin
- Gives users full control over their packages
- Supports both curated registries and direct URL installation
- Follows patterns proven successful in Emacs and Neovim ecosystems

The key insight is that **git is already a package manager**—we just need a thin UI layer on top.
