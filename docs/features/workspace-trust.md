# Workspace Trust

Opening a folder can expose Fresh to repo-controlled code execution — language servers, build scripts, task runners, and environment activation all run programs the project chooses. Workspace Trust gates that execution per folder.

## Trust Levels

A workspace is always in one of three levels, shown by the `{trust}` element at the left of the status bar:

| Level | What runs |
|-------|-----------|
| **Restricted** (default) | System tools found on `$PATH` (`git`, `ripgrep`, the system `python`). Blocks executables and scripts *inside* the project (`./gradlew`, `.venv/bin/python`, `node_modules/.bin/*`), environment activation, and language servers. |
| **Trusted** | Everything: language servers, build scripts, tasks, environment activation. |
| **Blocked** | Nothing — no system tools, language servers, scripts, or tasks. |

A folder with no executable markers (empty or document-only) is trusted automatically. A folder that *can* execute code opens **Restricted** until you decide.

## Granting Trust

When you open a folder containing executable markers (e.g. `.envrc`, `Cargo.toml`, `package.json`), Fresh shows a one-time prompt listing the detected markers, with three choices: **Trust folder & Allow Tooling** (`T`), **Keep Restricted (Default)** (`K`), **Block All Execution** (`B`).

You can change the level any time:

- Click the `{trust}` element in the status bar.
- Run **Workspace Trust…** from the command palette (`Ctrl+P`) to reopen the dialog.

The decision is remembered per folder across restarts. Changing the level resets only the active workspace, not the whole editor.

::: tip
Plugins can *request* the trust prompt but can never grant trust themselves — only you can.
:::

## Environments

When a workspace is trusted, Fresh detects and activates a project environment so terminals, formatters, and language servers see the right `PATH` and variables. Detection is marker-based:

| Detector | Markers | Kind |
|----------|---------|------|
| `.venv` / `venv` | `.venv/` or `venv/` with an interpreter inside | path-only |
| direnv | `.envrc` | shell |
| mise | `mise.toml`, `.mise.toml`, `.tool-versions` | shell |
| pipenv | `Pipfile` | shell |
| poetry | `poetry.lock` | shell |

The first matching detector wins. **Path-only** environments (a virtualenv) activate silently once the workspace is trusted — no extra prompt. **Shell** environments (direnv/mise/pipenv/poetry) run a snippet and are gated on trust. A bare `.venv` directory with no interpreter does not auto-activate.

The active environment is shown as a clickable pill in the status bar and applies uniformly across every backend — the integrated terminal, Docker / devcontainers, Kubernetes, and SSH.

### Customizing detectors

Detectors are defined under `env.detectors` and are user-extensible. Each entry has a `name` (the pill label), a list of `markers`, a `kind` (`"path-only"` or `"shell"`), an activation `snippet`, and an optional `require` list of evidence paths:

```jsonc
{
  "env": {
    "detectors": [
      {
        "name": "conda",
        "markers": ["environment.yml"],
        "kind": "shell",
        "snippet": "eval \"$(conda shell.bash hook)\" && conda activate ."
      }
    ]
  }
}
```

Listing `detectors` replaces the built-in set, so re-add the defaults you still want. Most users never need to touch this — the built-ins cover the common Python and shell tools.

See also [Remote Editing (SSH)](./ssh.md) and [Devcontainers](./devcontainer.md).
