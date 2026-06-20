# Remote Editing

> **Activation:** command-line only — no palette command or settings toggle. Launch Fresh with a remote path as the first argument (see forms below).

Fresh supports editing files on remote machines via SSH. Two wire
forms are accepted and do the same thing — pick whichever is easier
to type:

- scp-style: `user@host:path[:line[:col]]`
- URL-style: `ssh://[user@]host[:port]/path[:line[:col]]`

The URL form is the only one that accepts a non-standard port and is
the only one where the user is optional (it defaults to `$USER` /
`$USERNAME`).

```bash
# scp-style: open a specific file
fresh deploy@server.example.com:/etc/nginx/nginx.conf

# scp-style: open home directory in file explorer
fresh user@host:~

# scp-style: open with line number
fresh user@host:/var/log/app.log:100

# URL-style: default user from the environment
fresh ssh://host.example/etc/hosts

# URL-style: non-standard SSH port
fresh ssh://deploy@server.example.com:2222/etc/nginx/nginx.conf

# URL-style: line and column
fresh ssh://alice@host/home/alice/src/main.rs:42:7
```

**Features:**
- Password and SSH key authentication
- File explorer shows remote directory
- Sudo save support for protected files
- Status bar shows `[SSH:user@host]` indicator
- Background auto-reconnect after a dropped connection, with a disconnected indicator in the status bar
- Integrated terminal opens a login shell **on the remote host** (`ssh -t … 'cd <workspace>; exec $SHELL -l'`), rooted at the workspace

Under the hood, attaching to an SSH host switches the workspace's backend to that host — file I/O, the embedded terminal, spawned LSP servers, and any process Fresh launches all run on the remote.

Because the integrated terminal re-parents itself onto the remote host, it runs the *remote* `$SHELL` as a login shell and the local [`terminal.shell`](./terminal.md#shell-override) override does not apply. Interactive auth prompts (key passphrase, password, 2FA) surface inside the terminal pane on first open.

**Requirements:**
- SSH access to the remote host
- Python 3 installed on the remote host (for the agent)

## Alternative: SSH + Daemon Mode

If you need an editor that survives connection drops, consider running Fresh directly on the remote host in [daemon mode](./session-persistence.md):

```bash
ssh user@host
fresh -a        # start a daemon on the remote host
# if SSH disconnects, just reconnect and reattach:
ssh user@host
fresh -a
```

You can also pair SSH with `tmux` for a similar effect—run `tmux` on the remote host and launch Fresh inside it. Daemon mode has the advantage of being built into Fresh, so editor state (open files, terminals, undo history) is preserved without an external multiplexer.

### Single-install "jump box" pattern

You don't need Fresh on every machine you edit. Pass a remote target directly to a daemon: the remote URL becomes the daemon's startup authority, so the daemon runs on the machine you launched it from (e.g. a jump/bastion host) and edits the remote over SSH. The target still only needs Python 3; Fresh is installed solely on the box you launch from.

```bash
# On the one box where Fresh is installed:
fresh -a webserver deploy@server.example.com:/etc/nginx/nginx.conf

# Detach (Command Palette → "Detach"), then reattach later from the same box:
fresh -a webserver
```

Notes:

- The remote target is only consumed when the daemon is *started*; reattaching to an existing daemon ignores it, so `fresh -a webserver` is enough to reconnect.
- Unsaved buffers survive detach/reattach via [Hot Exit](./session-persistence.md#hot-exit).
- A single workspace binds to one backend: you can't mix local and remote paths, or files from more than one remote host, *within the same workspace*. To work across backends, open more than one workspace. The **Orchestrator: New Workspace** command (`Ctrl+P` → "Orchestrator: New Workspace") spawns an additional workspace with its own authority, so a local workspace and a remote workspace (or workspaces on different hosts) can run side by side in one Fresh daemon.
