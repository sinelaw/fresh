# Devcontainers

> **Palette:** `Dev Container: Attach`, `Dev Container: Detach`, `Dev Container: Rebuild`, `Dev Container: Cancel Startup`, `Dev Container: Create Config`, `Dev Container: Show Info`, `Dev Container: Show Ports`, `Dev Container: Show Forwarded Ports`, `Dev Container: Show Logs`, `Dev Container: Show Build Logs`, `Dev Container: Show Features`, `Dev Container: Open Config`, `Dev Container: Run Lifecycle Command`. A proactive popup also appears on launch for projects with a `.devcontainer/devcontainer.json`.

Fresh detects projects that ship a `.devcontainer/devcontainer.json` and prompts to **Attach** or **Rebuild** the container. When attached, the embedded terminal runs *inside* the container, and filesystem and process operations target the container instead of your host — including LSP servers, which Fresh spawns through the container so you don't need a host toolchain.

## Requirements

Install the [devcontainer CLI](https://github.com/devcontainers/cli):

```bash
npm install -g @devcontainers/cli
```

Fresh shells out to `devcontainer` for build/up/exec — if it's not on `PATH`, the Attach and Rebuild commands show an install hint instead.

If a project doesn't have a `.devcontainer/devcontainer.json` yet, run **Dev Container: Create Config** to scaffold one.

## Using it

Open a project that contains `.devcontainer/devcontainer.json`. Run **Dev Container: Attach** from the command palette (`Ctrl+P`). The first attach runs the devcontainer `initializeCommand` (if any) on the host, then builds and starts the container; subsequent attaches reuse it. **Dev Container: Rebuild** forces a full rebuild — reach for it after changing the Dockerfile or `devcontainer.json`.

During build or attach, the **build log** streams into a workspace split. If an attach fails, a recovery popup offers **Retry**, **Show Build Logs**, or **Reopen Locally** (Esc dismisses without changing authority). The launch-time attach prompt itself is one-shot per workspace: once you choose Attach or Dismiss, reopening the same project doesn't re-prompt. The status-bar **{remote}** indicator tracks the lifecycle — `Connecting`, `Connected`, or `FailedAttach` — and clicking it opens a context-aware menu.

While attached:

- The embedded terminal drops you into a shell inside the container.
- Opening files through the file explorer or `Ctrl+P` pulls them from the container's filesystem.
- LSP servers that Fresh spawns run in the container (install them there, not on your host). The LSP indicator marks servers whose binary isn't on the *container's* `PATH`.
- **Dev Container: Show Ports** opens a picker that merges configured `forwardPorts` with live `docker port` output.
- **Dev Container: Show Logs** shows a one-shot `docker logs` snapshot of the container's recent output.

Use **Dev Container: Detach** to return to host filesystem and process authority without quitting Fresh.

## Related

- [Remote Editing (SSH)](./ssh.md) — same Authority mechanism pointed at a remote host instead of a container.
- [Session Persistence](./session-persistence.md) — detach/reattach model that works well alongside containerized development.
- [Authority](../plugins/api/) — the underlying plugin-API slot that makes all three remote-editing modes interchangeable.
