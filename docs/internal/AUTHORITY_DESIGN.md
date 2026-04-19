# Authority — where does the editor act?

Supersedes `DEVCONTAINER_INTEGRATION_PLAN.md`. That plan predicted the
Authority Provider pattern; this document describes the architecture
as actually shipped.

## What Authority is

The editor has exactly one `Authority` at any moment. It is the single
answer to "where does this primitive run?":

- file I/O (open, save, list, stat, rename, create_dir, …)
- integrated terminal (`:term`, plugin `createTerminal`)
- plugin `spawnProcess`
- LSP server spawn
- find-in-files, save, auto-recovery, file watcher
- any future primitive that opens a handle to "out there"

The struct carries four fields and nothing else:

```rust
pub struct Authority {
    pub filesystem: Arc<dyn FileSystem + Send + Sync>,
    pub process_spawner: Arc<dyn ProcessSpawner>,
    pub terminal_wrapper: TerminalWrapper,
    pub display_label: String,
}
```

Empty `display_label` means the status bar renders nothing — the SSH
constructor leaves it empty so the existing
`filesystem.remote_connection_info()` path (which knows about
disconnect) stays the source of truth for user\@host labels.

## Principles (load-bearing)

1. **One authority on `Editor`.** No `Option`. Local is an authority;
   SSH is an authority; devcontainer is an authority; anything a plugin
   invents is an authority. Local's terminal wrapper is
   `detect_shell()` with no args and `manages_cwd: false`.

2. **Authority is the sole router for "where".** Every primitive routes
   through `editor.authority()`. Nothing reads a backend-specific
   field. Nothing branches on "is this SSH / a container".

3. **Authority is opaque to core.** No string `"docker"` / `"ssh"` /
   `"container"` in core logic that *consumes* the authority. The only
   code that names a backend is the constructor for that backend's
   authority.

4. **Plugins own backend lifecycle; core owns the slot.** Plugins
   attach a container, parse a `devcontainer.json`, drive a rebuild.
   Core just holds the authority and re-routes through it.

5. **Modal per window, no composition.** One authority, one workspace.
   Opening a non-project file while attached still routes through the
   active authority — this is the contract, not a bug.

6. **Startup is local; plugins upgrade.** The editor always boots
   `Authority::local()` and renders immediately. The SSH CLI form
   (`fresh user@host:path`) substitutes `Authority::ssh(...)` at
   startup. Devcontainer attach is a plugin op and happens post-boot.

7. **Authority transitions are destructive.** See next section.

8. **The core shrinks.** This refactor net-deleted ~400 lines: the
   `services/devcontainer/` module, `DevcontainerConfig`, the
   `connect_devcontainer` block in `main.rs`, and every per-backend
   branch in the terminal manager, render code, and plugin dispatch.

9. **Identity lives in the authority.** Whoever constructs the
   authority fills in `display_label`. SSH intentionally leaves it
   empty so disconnect annotations flow through one place.

10. **Every authority is constructible in isolation.** `Authority::local()`,
    `Authority::ssh(...)`, `Authority::from_plugin_payload(...)` — all
    available for unit testing without a running editor.

## Transitions — why we restart the editor

Principle 7 says "atomic and destructive". The spec's original
phrasing — "installing a new authority closes all terminals spawned
under the previous one, restarts LSP servers, invalidates cached
spawner handles; pointer-equality against `Editor.authority` is the
'still attached to the same thing?' check" — describes an in-place
swap.

We chose the more conservative option: **transitions drop and rebuild
the whole `Editor`**, piggy-backing on the existing
`change_working_dir` / `request_restart` flow.

### Why not in-place

In-place swap means enumerating everything that holds an
`Arc<dyn FileSystem>` or an `Arc<dyn ProcessSpawner>` at the moment of
swap and invalidating each one. As of this refactor that set
includes, at minimum:

- every open buffer's `EditorState` (captured filesystem at load time)
- `FsManager` (file explorer)
- `FileProvider` in Quick Open
- `LspManager` (server handles spawned through the old spawner)
- `TerminalManager` (every PTY)
- the file watcher, recovery service, session restore paths
- every background tokio task that cloned an `Arc` to the old spawner

Any cache holding a closure over the old authority's filesystem would
silently keep using the old backend after a "successful" transition.
Enumerating them is doable, but easy to miss, and the miss manifests
as "files save to the wrong place" — a trust-destroying class of bug.

### Why restart is cheap enough

The `request_restart` path already drops the entire `Editor`, calls
`Drop` on every resource, rebuilds from scratch, and reloads plugins.
Session restore brings buffers back. LSPs cold-start, but they were
going to restart on authority change anyway. The visible cost is one
frame of "Restarting editor…" status and a ~1-second pause — a price
we pay once per attach/detach, not per keystroke.

### Implementation shape

- `Editor::install_authority(new)` stashes the replacement in
  `pending_authority` and calls `request_restart(self.working_dir)`.
- `Editor::clear_authority()` is sugar for `install_authority(Authority::local())`.
- The event loop returns; `main.rs` drains `take_pending_authority()`
  from the old editor before dropping it, threads the result into
  `current_authority`, then builds the next `Editor` with
  `set_boot_authority(current_authority)` immediately after
  construction so plugins load with the new backend from the first
  tick.

The in-place swap remains a future optimization. The single-line
escape hatch is at `install_authority` — replace the `pending + restart`
with a direct swap once every cache-holder is audited.

### Session mode (client/server daemon)

Session mode (`fresh --session` / `fresh server`) runs the editor in a
long-lived daemon with thin clients attaching over IPC. The daemon
must not exit on every authority transition or working-dir change —
that would disconnect every attached client.

`EditorServer` (`crates/fresh-editor/src/server/editor_server.rs`)
mirrors the standalone restart loop: when the editor sets
`should_quit` via `request_restart` (either from
`change_working_dir` or from `install_authority`), the server takes
the pending fields off the old editor, calls `rebuild_editor(...)`,
and clients stay attached. `rebuild_editor`:

1. Saves workspace + ends recovery session on the old editor.
2. Drops the old editor (terminals, LSPs, plugin threads unwind).
3. Updates `self.config.working_dir` and/or `self.current_authority`.
4. Builds a fresh editor via `build_editor_instance` with the new
   authority already installed (`set_boot_authority`).
5. Restores the workspace so open buffers come back under the new
   backend.
6. Flags every connected client for a full repaint on the next frame.

If neither a pending authority nor a restart dir is present,
`should_quit` is treated as a real shutdown request and the daemon
exits as before. Tests cover both the authority-swap and the
working-dir-swap paths (`test_session_rebuild_swaps_editor_and_authority`
and `test_session_rebuild_switches_working_dir`).

`EditorServerConfig` has two optional slots for callers that want
the daemon to boot into something other than local:

- `startup_authority: Option<Authority>` — installed as
  `current_authority` before the first editor is built. Defaults to
  `Authority::local()`.
- `session_keepalive: Option<Box<dyn Any + Send>>` — an opaque
  bundle held for the server's lifetime alongside `startup_authority`.
  SSH authorities back this with the Tokio runtime, the
  `SshConnection`, and the reconnect task; dropping any one of those
  would tear the remote session down, so the server just holds the
  bundle until shutdown. Local authorities leave it `None`.

### CLI → detached daemon plumbing

When a client command (`fresh -a <files>` or
`fresh --cmd session open-file <name> <files>`) sees any remote spec
in the file list, `extract_ssh_url_from_files`:

1. Parses every file through `parse_location`.
2. Validates that all remote entries agree on user/host/port
   (error otherwise) and that remote and local paths are not mixed.
3. Re-renders the shared authority as a canonical `ssh://` URL via
   `remote_location_to_ssh_url` (line/column are per-file and stay
   out of the authority URL).

That URL is forwarded to the detached child as
`--ssh-url <URL>` (a hidden internal flag) by
`spawn_server_detached(session_name, ssh_url)`.  The file list sent
to the daemon over the `OpenFiles` control message is stripped to
bare paths — the daemon's active authority already knows the host.

On the daemon side, `run_server_command` uses `parse_ssh_url_arg`
(URL-form only, hard error on anything else) to build a
`RemoteLocation`, calls the same `create_startup_authority` /
`connect_remote` used by standalone mode, and wraps the resulting
`RemoteSession` in the `session_keepalive` slot.  The remote path
becomes the daemon's `working_dir`; local cwd keeps its role as the
config-layering key.

Existing servers are not re-attached through a remote URL: a URL
passed to `fresh -a` is only consumed when the client starts a new
daemon. If a local-authority session is already running under the
target key, the URL is ignored. Callers wanting isolation should
pass `--session-name` (or equivalent) so the new SSH daemon gets a
distinct socket.

### Related: `change_working_dir`

`change_working_dir` uses the same restart machinery to switch project
roots. Authority transitions and project-root changes are the same
primitive at the main-loop level — drop the `Editor`, rebuild — with
different "what changes" semantics (working directory vs. the
`current_authority` slot). Keeping them separate entry points means
callers don't have to care about each other; each can evolve
independently.

## Plugin API

Three ops, intentionally small:

- `editor.setAuthority(payload)` — payload is a tagged
  `AuthorityPayload` (filesystem kind + spawner kind + terminal
  wrapper + display label). The concrete schema lives in
  `crates/fresh-editor/src/services/authority/mod.rs`; TS types are
  mirrored in `plugins/lib/fresh.d.ts`. Fire-and-forget — the editor
  restarts before any follow-up code on this call's return could run.
- `editor.clearAuthority()` — restore `Authority::local()` with the
  same restart semantics.
- `editor.spawnHostProcess(command, args, cwd?)` — run on the host
  regardless of the current authority. Reserved for plugin internals
  that must do host-side work (e.g. `devcontainer up`) before the
  authority they want even exists. Same calling shape as
  `spawnProcess`, a thenable returning a `SpawnResult`.

### Payload shape

```ts
type AuthorityPayload = {
  filesystem: { kind: "local" };
  spawner:
    | { kind: "local" }
    | {
        kind: "docker-exec";
        container_id: string;
        user?: string | null;
        workspace?: string | null;
      };
  terminal_wrapper:
    | { kind: "host-shell" }
    | { kind: "explicit"; command: string; args: string[]; manages_cwd?: boolean };
  display_label?: string;
};
```

New kinds go here and in `Authority::from_plugin_payload`.
`serde`'s tagged-enum representation means old payloads keep parsing
as new variants are added.

## Devcontainer, end-to-end

Example of the plugin-owned backend lifecycle, in full:

1. Editor boots with `Authority::local()`.
2. `plugins/devcontainer.ts` loads, calls `findConfig()`, sees
   `.devcontainer/devcontainer.json`.
3. If this workspace has no remembered decision, the plugin shows a
   one-shot "Attach?" action popup. User answer is stored in plugin
   global state keyed by `getCwd()` — reopening the project doesn't
   re-prompt.
4. On "Attach", the plugin calls
   `editor.spawnHostProcess("devcontainer", ["up", "--workspace-folder", cwd])`.
   This always runs on the host, even if the call originates from
   inside a container (important for rebuild).
5. Plugin parses the JSON result line, builds the docker-exec
   `AuthorityPayload`, calls `editor.setAuthority(payload)`.
6. Core stashes the payload, triggers restart, drops the editor,
   rebuilds. The plugin reloads with the container authority active
   and prints `status.detected` in the status bar.

Detach / rebuild follow the same path with different args:
`clearAuthority()` for detach, `up --remove-existing-container` for
rebuild.

## What this refactor is not

- Not a remote extension host.
- Not a port-forwarding UI.
- Not a path-translation layer — containers work because the workspace
  is mounted, not because we rewrite paths.
- Not multi-root workspaces.
- Not credential syncing.
- Not cross-authority composition (no `Vec<Authority>`, no path-prefix
  routing). Principle 5.
- Not backwards-compatible with the pre-refactor devcontainer config
  fields or core ops — `devcontainer.auto_detect` / `devcontainer.cli_path`
  in user config are ignored on load; the plugin now owns both.
