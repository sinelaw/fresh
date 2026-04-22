# VS Code Dev Containers Plugin Design

## Overview

This document describes the design for a Fresh plugin that detects VS Code Dev Container configurations (`.devcontainer/devcontainer.json`) and provides in-editor support for working with containerized development environments. The plugin surfaces devcontainer metadata, lifecycle commands, port forwarding info, and feature listings — all within Fresh's existing plugin UI patterns.

## Goals

1. **Configuration Awareness**: Parse and display `devcontainer.json` settings so developers can understand their container environment without leaving the editor
2. **Lifecycle Command Access**: Expose devcontainer lifecycle scripts (onCreateCommand, postCreateCommand, etc.) as runnable commands from the command palette
3. **Feature Browsing**: List installed Dev Container Features with their options and documentation links
4. **Port Forwarding Visibility**: Show configured port forwards and their attributes in a discoverable panel
5. **Zero Dependencies**: Pure TypeScript plugin using Fresh's existing `spawnProcess` API — no external tooling required beyond what's already in the container

## Non-Goals

- **Container orchestration**: This plugin does not build, start, or stop containers. That is the job of the `devcontainer` CLI or VS Code. Fresh runs *inside* an already-running container.
- **Feature installation**: Adding/removing Dev Container Features requires rebuilding the container image, which is outside Fresh's scope.
- **Docker/Compose management**: No direct Docker socket interaction.
- **Replacing the devcontainer CLI**: The plugin complements, not replaces, existing tooling.

## Background: Dev Container Specification

The [Dev Container specification](https://containers.dev/) defines a standard for enriching containers with development-specific metadata. Key concepts:

- **`devcontainer.json`**: Configuration file placed in `.devcontainer/devcontainer.json` (or `.devcontainer.json`, or `.devcontainer/<folder>/devcontainer.json`) that defines image, features, lifecycle scripts, ports, environment variables, and tool customizations.
- **Features**: Self-contained, shareable units of installation code (e.g., `ghcr.io/devcontainers/features/rust:1`). Each feature has a `devcontainer-feature.json` manifest with options, install scripts, and metadata.
- **Lifecycle Scripts**: Ordered hooks that run at container creation and startup:
  1. `initializeCommand` — runs on the host before container creation
  2. `onCreateCommand` — runs once when container is first created
  3. `updateContentCommand` — runs when new content is available
  4. `postCreateCommand` — runs after container creation completes
  5. `postStartCommand` — runs each time the container starts
  6. `postAttachCommand` — runs each time a tool attaches
- **Customizations**: Tool-specific settings under `customizations.<tool>` (e.g., `customizations.vscode.extensions`).

---

## Architecture

```
┌──────────────────────────────────────────────────────────────────────┐
│ Fresh Editor (running inside dev container)                          │
│                                                                      │
│  ┌────────────────────────────────────────────────────────────────┐  │
│  │ devcontainer.ts Plugin                                         │  │
│  │                                                                │  │
│  │  ┌──────────────┐  ┌──────────────┐  ┌─────────────────────┐  │  │
│  │  │ Config Parser │  │ Lifecycle    │  │ Panel Renderer      │  │  │
│  │  │ (JSON + JSONC)│  │ Runner       │  │ (virtual buffer)    │  │  │
│  │  └──────┬───────┘  └──────┬───────┘  └──────────┬──────────┘  │  │
│  │         │                 │                      │             │  │
│  │         └────────┬────────┴──────────────────────┘             │  │
│  │                  │                                              │  │
│  │           editor.spawnProcess()                                 │  │
│  │           editor.readFile()                                     │  │
│  │           editor.createVirtualBufferInSplit()                   │  │
│  └────────────────────────────────────────────────────────────────┘  │
│                                                                      │
│  Filesystem:                                                         │
│  ├── .devcontainer/devcontainer.json                                │
│  ├── .devcontainer/docker-compose.yml (optional)                    │
│  └── .devcontainer/Dockerfile (optional)                            │
└──────────────────────────────────────────────────────────────────────┘
```

The plugin operates entirely within Fresh's TypeScript plugin runtime (QuickJS). It reads configuration files from disk using `editor.readFile()`, runs lifecycle commands via `editor.spawnProcess()`, and displays information using virtual buffers and status bar messages.

---

## User Flows

### Flow 1: Automatic Detection on Startup

When Fresh opens a workspace containing a `.devcontainer/` directory:

1. Plugin's `on_loaded` hook fires
2. Plugin searches for `devcontainer.json` in priority order:
   - `.devcontainer/devcontainer.json`
   - `.devcontainer.json`
   - `.devcontainer/<subfolder>/devcontainer.json` (first match)
3. If found, parse the config and display a brief status message:
   ```
   Dev Container: rust-dev (mcr.microsoft.com/devcontainers/rust:1) • 3 features • 2 ports
   ```
4. Register all command palette commands

If no devcontainer config is found, the plugin remains dormant — no commands registered, no status messages.

### Flow 2: View Container Info Panel

User invokes command palette → "Dev Container: Show Info":

```
┌─ Dev Container: rust-dev ────────────────────────────────────────────┐
│                                                                       │
│ Image                                                                 │
│   mcr.microsoft.com/devcontainers/rust:1-bookworm                    │
│                                                                       │
│ Features                                                              │
│   ✓ ghcr.io/devcontainers/features/rust:1                            │
│       version = "1.91.0"                                              │
│   ✓ ghcr.io/devcontainers/features/node:1                            │
│       version = "lts"                                                 │
│   ✓ ghcr.io/devcontainers-contrib/features/apt-packages:1           │
│       packages = "pkg-config,libssl-dev"                              │
│                                                                       │
│ Ports                                                                 │
│   8080 → http  (label: "Web App", onAutoForward: notify)             │
│   5432 → tcp   (label: "PostgreSQL", onAutoForward: silent)          │
│                                                                       │
│ Environment                                                           │
│   CARGO_HOME = /usr/local/cargo                                      │
│   RUST_LOG   = debug                                                  │
│                                                                       │
│ Mounts                                                                │
│   cargo-cache → /usr/local/cargo (volume)                            │
│                                                                       │
│ Users                                                                 │
│   containerUser: vscode                                               │
│   remoteUser:    vscode                                               │
│                                                                       │
│ Lifecycle Commands                                                    │
│   onCreateCommand:     cargo build                                   │
│   postCreateCommand:   cargo test --no-run                           │
│   postStartCommand:    cargo watch -x check                          │
│                                                                       │
│ [r] Run lifecycle command  [o] Open devcontainer.json  [q] Close     │
└───────────────────────────────────────────────────────────────────────┘
```

This is rendered in a virtual buffer via `editor.createVirtualBufferInSplit()`, following the same pattern as `diagnostics_panel.ts` and `git_log.ts`.

### Flow 3: Run Lifecycle Command

User invokes command palette → "Dev Container: Run Lifecycle Command":

```
┌─ Run Lifecycle Command ──────────────────────────────────────────────┐
│ Select a lifecycle command to run:                                    │
│                                                                       │
│ > onCreateCommand:     cargo build                                   │
│   postCreateCommand:   cargo test --no-run                           │
│   postStartCommand:    cargo watch -x check                          │
└───────────────────────────────────────────────────────────────────────┘
```

On selection, the command runs via `editor.spawnProcess()` in a terminal split, showing live output. This mirrors how `git_log.ts` spawns git processes.

### Flow 4: Open Configuration File

User invokes command palette → "Dev Container: Open Config":

Opens `.devcontainer/devcontainer.json` in a new buffer. If multiple configs exist (subfolders), show a picker first.

---

## Configuration Parsing

### JSONC Support

`devcontainer.json` uses JSON with Comments (JSONC). The plugin includes a minimal JSONC stripper that removes:
- Single-line comments (`//`)
- Multi-line comments (`/* */`)
- Trailing commas

This is sufficient for parsing without adding a full JSONC parser dependency.

```typescript
function stripJsonc(text: string): string {
  let result = "";
  let i = 0;
  let inString = false;
  while (i < text.length) {
    if (inString) {
      if (text[i] === "\\" && i + 1 < text.length) {
        result += text[i] + text[i + 1];
        i += 2;
        continue;
      }
      if (text[i] === '"') inString = false;
      result += text[i];
    } else if (text[i] === '"') {
      inString = true;
      result += text[i];
    } else if (text[i] === "/" && text[i + 1] === "/") {
      while (i < text.length && text[i] !== "\n") i++;
      continue;
    } else if (text[i] === "/" && text[i + 1] === "*") {
      i += 2;
      while (i < text.length - 1 && !(text[i] === "*" && text[i + 1] === "/")) i++;
      i += 2;
      continue;
    } else {
      result += text[i];
    }
    i++;
  }
  // Remove trailing commas before } or ]
  return result.replace(/,\s*([}\]])/g, "$1");
}
```

### Parsed Configuration Type

```typescript
interface DevContainerConfig {
  name?: string;

  // Image / Dockerfile / Compose
  image?: string;
  build?: {
    dockerfile?: string;
    context?: string;
    args?: Record<string, string>;
    target?: string;
    cacheFrom?: string | string[];
  };
  dockerComposeFile?: string | string[];
  service?: string;

  // Features
  features?: Record<string, string | boolean | Record<string, unknown>>;

  // Ports
  forwardPorts?: (number | string)[];
  portsAttributes?: Record<string, PortAttributes>;
  appPort?: number | string | (number | string)[];

  // Environment
  containerEnv?: Record<string, string>;
  remoteEnv?: Record<string, string>;

  // Users
  containerUser?: string;
  remoteUser?: string;

  // Mounts
  mounts?: (string | MountConfig)[];

  // Lifecycle
  initializeCommand?: LifecycleCommand;
  onCreateCommand?: LifecycleCommand;
  updateContentCommand?: LifecycleCommand;
  postCreateCommand?: LifecycleCommand;
  postStartCommand?: LifecycleCommand;
  postAttachCommand?: LifecycleCommand;

  // Customizations
  customizations?: Record<string, unknown>;

  // Runtime
  runArgs?: string[];
  workspaceFolder?: string;
  workspaceMount?: string;
  shutdownAction?: "none" | "stopContainer" | "stopCompose";
  overrideCommand?: boolean;
  init?: boolean;
  privileged?: boolean;
  capAdd?: string[];
  securityOpt?: string[];

  // Host requirements
  hostRequirements?: {
    cpus?: number;
    memory?: string;
    storage?: string;
    gpu?: boolean | string | { cores?: number; memory?: string };
  };
}

type LifecycleCommand = string | string[] | Record<string, string | string[]>;

interface PortAttributes {
  label?: string;
  protocol?: "http" | "https";
  onAutoForward?: "notify" | "openBrowser" | "openBrowserOnce" | "openPreview" | "silent" | "ignore";
  requireLocalPort?: boolean;
  elevateIfNeeded?: boolean;
}

interface MountConfig {
  type: "bind" | "volume" | "tmpfs";
  source: string;
  target: string;
}
```

---

## Command Palette Commands

| Command | Description |
|---------|-------------|
| `Dev Container: Show Info` | Open info panel in virtual buffer split |
| `Dev Container: Run Lifecycle Command` | Pick and run a lifecycle script |
| `Dev Container: Open Config` | Open devcontainer.json in editor |
| `Dev Container: Show Features` | List installed features with options |
| `Dev Container: Show Ports` | Display port forwarding configuration |
| `Dev Container: Show Environment` | Display container/remote env vars |
| `Dev Container: Rebuild` | Run `devcontainer rebuild` if CLI available |

Commands are only registered when a `devcontainer.json` is detected in the workspace.

---

## Implementation Details

### Plugin Entry Point

**New file**: `crates/fresh-editor/plugins/devcontainer.ts`

```typescript
/// <reference path="../types/fresh.d.ts" />

// ─── Config Discovery ────────────────────────────────────────────────

const CONFIG_PATHS = [
  ".devcontainer/devcontainer.json",
  ".devcontainer.json",
];

let config: DevContainerConfig | null = null;
let configPath: string | null = null;

async function findConfig(): Promise<void> {
  const cwd = editor.getCwd();

  for (const rel of CONFIG_PATHS) {
    const full = `${cwd}/${rel}`;
    try {
      const text = await editor.readFile(full);
      config = JSON.parse(stripJsonc(text));
      configPath = full;
      return;
    } catch {
      // not found, try next
    }
  }

  // Check for subdirectory configs: .devcontainer/<name>/devcontainer.json
  try {
    const result = await editor.spawnProcess("ls", [
      "-d", `${cwd}/.devcontainer/*/devcontainer.json`
    ]);
    if (result.exit_code === 0) {
      const first = result.stdout.trim().split("\n")[0];
      if (first) {
        const text = await editor.readFile(first);
        config = JSON.parse(stripJsonc(text));
        configPath = first;
      }
    }
  } catch {
    // no subdirectory configs
  }
}

// ─── Startup ─────────────────────────────────────────────────────────

editor.on("on_loaded", async () => {
  await findConfig();
  if (!config) return;

  registerCommands();

  const featureCount = config.features ? Object.keys(config.features).length : 0;
  const portCount = config.forwardPorts?.length ?? 0;
  const name = config.name ?? "unnamed";
  const image = config.image ?? config.build?.dockerfile ?? "compose";

  editor.setStatus(
    `Dev Container: ${name} (${image}) • ${featureCount} features • ${portCount} ports`
  );
});
```

### Info Panel Rendering

Uses the virtual buffer pattern from `diagnostics_panel.ts`:

```typescript
async function showInfoPanel(): Promise<void> {
  if (!config) return;

  const lines: string[] = [];
  const overlays: Overlay[] = [];
  let line = 0;

  function heading(text: string) {
    overlays.push({ line, style: "bold", text });
    lines.push(text);
    line++;
  }

  function entry(key: string, value: string) {
    lines.push(`  ${key}: ${value}`);
    line++;
  }

  function blank() {
    lines.push("");
    line++;
  }

  // Header
  heading(`Dev Container: ${config.name ?? "unnamed"}`);
  blank();

  // Image / Build
  if (config.image) {
    heading("Image");
    entry("image", config.image);
    blank();
  } else if (config.build?.dockerfile) {
    heading("Build");
    entry("dockerfile", config.build.dockerfile);
    if (config.build.context) entry("context", config.build.context);
    if (config.build.target) entry("target", config.build.target);
    blank();
  } else if (config.dockerComposeFile) {
    heading("Docker Compose");
    const files = Array.isArray(config.dockerComposeFile)
      ? config.dockerComposeFile.join(", ")
      : config.dockerComposeFile;
    entry("files", files);
    if (config.service) entry("service", config.service);
    blank();
  }

  // Features
  if (config.features && Object.keys(config.features).length > 0) {
    heading("Features");
    for (const [id, opts] of Object.entries(config.features)) {
      if (typeof opts === "object" && opts !== null) {
        const optStr = Object.entries(opts)
          .map(([k, v]) => `${k} = ${JSON.stringify(v)}`)
          .join(", ");
        lines.push(`  ✓ ${id}`);
        line++;
        if (optStr) {
          lines.push(`      ${optStr}`);
          line++;
        }
      } else {
        lines.push(`  ✓ ${id}`);
        line++;
      }
    }
    blank();
  }

  // Ports
  if (config.forwardPorts && config.forwardPorts.length > 0) {
    heading("Ports");
    for (const port of config.forwardPorts) {
      const attrs = config.portsAttributes?.[String(port)];
      const label = attrs?.label ? ` (label: "${attrs.label}")` : "";
      const proto = attrs?.protocol ?? "tcp";
      lines.push(`  ${port} → ${proto}${label}`);
      line++;
    }
    blank();
  }

  // Environment
  const allEnv = { ...config.containerEnv, ...config.remoteEnv };
  if (Object.keys(allEnv).length > 0) {
    heading("Environment");
    for (const [k, v] of Object.entries(allEnv)) {
      entry(k, v);
    }
    blank();
  }

  // Lifecycle Commands
  const lifecycle: [string, LifecycleCommand | undefined][] = [
    ["initializeCommand", config.initializeCommand],
    ["onCreateCommand", config.onCreateCommand],
    ["updateContentCommand", config.updateContentCommand],
    ["postCreateCommand", config.postCreateCommand],
    ["postStartCommand", config.postStartCommand],
    ["postAttachCommand", config.postAttachCommand],
  ];
  const defined = lifecycle.filter(([, v]) => v !== undefined);
  if (defined.length > 0) {
    heading("Lifecycle Commands");
    for (const [name, cmd] of defined) {
      entry(name, formatLifecycleCommand(cmd!));
    }
    blank();
  }

  // Users
  if (config.containerUser || config.remoteUser) {
    heading("Users");
    if (config.containerUser) entry("containerUser", config.containerUser);
    if (config.remoteUser) entry("remoteUser", config.remoteUser);
    blank();
  }

  const content = lines.join("\n");
  editor.createVirtualBufferInSplit(
    "devcontainer-info",
    content,
    "Dev Container Info",
    { overlays, readOnly: true }
  );
}

function formatLifecycleCommand(cmd: LifecycleCommand): string {
  if (typeof cmd === "string") return cmd;
  if (Array.isArray(cmd)) return cmd.join(" ");
  return Object.entries(cmd)
    .map(([k, v]) => `${k}: ${Array.isArray(v) ? v.join(" ") : v}`)
    .join("; ");
}
```

### Lifecycle Command Runner

```typescript
async function runLifecycleCommand(): Promise<void> {
  if (!config) return;

  const lifecycle: [string, LifecycleCommand | undefined][] = [
    ["onCreateCommand", config.onCreateCommand],
    ["updateContentCommand", config.updateContentCommand],
    ["postCreateCommand", config.postCreateCommand],
    ["postStartCommand", config.postStartCommand],
    ["postAttachCommand", config.postAttachCommand],
  ];

  const defined = lifecycle.filter(([, v]) => v !== undefined);
  if (defined.length === 0) {
    editor.setStatus("No lifecycle commands defined");
    return;
  }

  const items = defined.map(([name, cmd]) => ({
    label: name,
    description: formatLifecycleCommand(cmd!),
  }));

  editor.startPrompt("Run lifecycle command:", "devcontainer-lifecycle");
  editor.setPromptSuggestions(items);
}

// Handle selection
editor.on("prompt_selection_changed", (ctx) => {
  if (ctx.promptId !== "devcontainer-lifecycle") return;
  // Preview: show full command in status bar
  if (ctx.selection) {
    editor.setStatus(`Will run: ${ctx.selection.description}`);
  }
});

async function executeLifecycleCommand(name: string): Promise<void> {
  const cmd = (config as any)?.[name];
  if (!cmd) return;

  if (typeof cmd === "string") {
    editor.setStatus(`Running ${name}...`);
    const result = await editor.spawnProcess("sh", ["-c", cmd]);
    if (result.exit_code === 0) {
      editor.setStatus(`${name} completed successfully`);
    } else {
      editor.setStatus(`${name} failed (exit ${result.exit_code})`);
    }
  } else if (Array.isArray(cmd)) {
    const [bin, ...args] = cmd;
    editor.setStatus(`Running ${name}...`);
    const result = await editor.spawnProcess(bin, args);
    if (result.exit_code === 0) {
      editor.setStatus(`${name} completed successfully`);
    } else {
      editor.setStatus(`${name} failed (exit ${result.exit_code})`);
    }
  } else {
    // Object form: run each named command sequentially
    for (const [label, subcmd] of Object.entries(cmd)) {
      editor.setStatus(`Running ${name} (${label})...`);
      const c = Array.isArray(subcmd) ? subcmd : ["sh", "-c", subcmd];
      const [bin, ...args] = c;
      const result = await editor.spawnProcess(bin, args);
      if (result.exit_code !== 0) {
        editor.setStatus(`${name} (${label}) failed (exit ${result.exit_code})`);
        return;
      }
    }
    editor.setStatus(`${name} completed successfully`);
  }
}
```

### Command Registration

```typescript
function registerCommands(): void {
  editor.registerCommand(
    "devcontainer_show_info",
    "Dev Container: Show Info",
    "devcontainer_show_info",
    "normal"
  );
  editor.registerCommand(
    "devcontainer_run_lifecycle",
    "Dev Container: Run Lifecycle Command",
    "devcontainer_run_lifecycle",
    "normal"
  );
  editor.registerCommand(
    "devcontainer_open_config",
    "Dev Container: Open Config",
    "devcontainer_open_config",
    "normal"
  );
  editor.registerCommand(
    "devcontainer_show_features",
    "Dev Container: Show Features",
    "devcontainer_show_features",
    "normal"
  );
  editor.registerCommand(
    "devcontainer_show_ports",
    "Dev Container: Show Ports",
    "devcontainer_show_ports",
    "normal"
  );
  editor.registerCommand(
    "devcontainer_rebuild",
    "Dev Container: Rebuild",
    "devcontainer_rebuild",
    "normal"
  );
}

// Command handlers
globalThis.devcontainer_show_info = showInfoPanel;
globalThis.devcontainer_run_lifecycle = runLifecycleCommand;
globalThis.devcontainer_open_config = () => {
  if (configPath) editor.openFile(configPath);
};
globalThis.devcontainer_rebuild = async () => {
  const result = await editor.spawnProcess("which", ["devcontainer"]);
  if (result.exit_code !== 0) {
    editor.setStatus("devcontainer CLI not found. Install with: npm i -g @devcontainers/cli");
    return;
  }
  editor.setStatus("Rebuilding dev container...");
  await editor.spawnProcess("devcontainer", ["rebuild", "--workspace-folder", editor.getCwd()]);
};
```

---

## Internationalization

Following Fresh's i18n convention, the plugin includes a companion `devcontainer.i18n.json`:

```json
{
  "en": {
    "status_detected": "Dev Container: {name} ({image}) • {features} features • {ports} ports",
    "no_config": "No devcontainer.json found",
    "running": "Running {name}...",
    "completed": "{name} completed successfully",
    "failed": "{name} failed (exit {code})",
    "cli_not_found": "devcontainer CLI not found. Install with: npm i -g @devcontainers/cli"
  }
}
```

---

## Files to Create

| File | Purpose |
|------|---------|
| `crates/fresh-editor/plugins/devcontainer.ts` | Main plugin implementation |
| `crates/fresh-editor/plugins/devcontainer.i18n.json` | Internationalization strings |

No Rust code changes required. The plugin uses only existing plugin APIs.

---

## Alternative Designs Considered

### Alternative 1: Rust-native Config Parser

**Approach**: Parse `devcontainer.json` in Rust and expose it via a new plugin API.

**Pros**: Faster parsing, type-safe, could integrate with editor core features.

**Cons**: Adds Rust code for a niche feature, couples devcontainer awareness to the editor core, requires editor updates for devcontainer spec changes.

**Verdict**: Rejected. A TypeScript plugin is the right granularity — it can evolve independently of editor releases and follows Fresh's extension philosophy.

### Alternative 2: Full devcontainer CLI Wrapper

**Approach**: Shell out to `devcontainer read-configuration` for parsed config instead of parsing JSON ourselves.

**Pros**: Handles all edge cases (variable substitution, feature merging, image label metadata).

**Cons**: Requires `devcontainer` CLI to be installed (it often isn't inside the container itself), adds ~2s startup latency for the CLI invocation, and makes the plugin useless in environments without the CLI.

**Verdict**: Rejected. Direct JSON parsing covers the common case. A future enhancement could optionally use the CLI when available for full config resolution.

### Alternative 3: LSP-based Approach

**Approach**: Use a devcontainer JSON Schema LSP server for validation and completion.

**Pros**: Get validation, completion, and hover docs for free.

**Cons**: Orthogonal to the plugin's purpose (which is displaying info, not editing the config). JSON schema validation can be added independently via Fresh's existing JSON LSP support.

**Verdict**: Out of scope, but complementary. Users can already get JSON schema validation by configuring the JSON LSP with the devcontainer schema URL.

---

## Testing Strategy

### Unit Tests

- JSONC stripping: comments, trailing commas, edge cases
- Config parsing: all property types (image, Dockerfile, Compose)
- Lifecycle command formatting: string, array, and object forms
- Port attribute rendering

### E2E Tests

Using `EditorTestHarness` with a temp directory containing `.devcontainer/devcontainer.json`:

```rust
#[test]
fn test_devcontainer_plugin_detects_config() {
    let mut harness = EditorTestHarness::new(120, 40).unwrap();
    harness.copy_plugin("devcontainer");

    // Create devcontainer.json fixture
    let dc_dir = harness.files.path().join(".devcontainer");
    std::fs::create_dir_all(&dc_dir).unwrap();
    std::fs::write(
        dc_dir.join("devcontainer.json"),
        r#"{ "name": "test", "image": "ubuntu:22.04" }"#,
    ).unwrap();

    harness.open_directory(harness.files.path()).unwrap();
    harness.wait_for_plugins().unwrap();
    harness.render().unwrap();

    harness.assert_screen_contains("Dev Container: test");
}
```

### Manual Testing

1. Open a project with `.devcontainer/devcontainer.json`
2. Verify status bar shows container info
3. Run "Dev Container: Show Info" from command palette
4. Run a lifecycle command and verify output
5. Test with various config shapes (image-only, Dockerfile, Compose)
6. Test with JSONC comments and trailing commas

---

## Implementation Phases

### Phase 1: Core Detection & Info Panel
- [ ] JSONC parser
- [ ] Config file discovery
- [ ] Config type definitions and parsing
- [ ] Info panel virtual buffer
- [ ] Status bar message on detection
- [ ] "Open Config" command

### Phase 2: Lifecycle Commands
- [ ] Lifecycle command picker prompt
- [ ] Command execution (string, array, object forms)
- [ ] Output display in terminal split

### Phase 3: Polish
- [ ] i18n support
- [ ] Rebuild command (optional devcontainer CLI integration)
- [ ] E2E tests
- [ ] Handle workspace reloads / config file changes

---

## Open Questions

1. **Config file watching**: Should the plugin re-parse `devcontainer.json` when it changes on disk? Fresh has file-watching infrastructure, but the added complexity may not be worth it for a config file that rarely changes during a session.

2. **Variable substitution**: `devcontainer.json` supports `${localEnv:VAR}` and `${containerEnv:VAR}` template variables. Should the plugin resolve these? Initial implementation can show them as-is and add resolution later.

3. **Multiple configurations**: When `.devcontainer/` contains multiple subdirectories (each with its own `devcontainer.json`), should the plugin show a picker or auto-detect which one is active? The spec doesn't define "active" — that's determined by the tool that created the container.
