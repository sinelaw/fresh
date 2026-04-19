# Remote Editing (Experimental)

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

**Requirements:**
- SSH access to the remote host
- Python 3 installed on the remote host (for the agent)

## Alternative: SSH + Session Persistence

If you need a persistent editing session that survives connection drops, consider running Fresh directly on the remote host with [Session Persistence](./session-persistence.md):

```bash
ssh user@host
fresh -a        # start a persistent session on the remote host
# if SSH disconnects, just reconnect and reattach:
ssh user@host
fresh -a
```

You can also pair SSH with `tmux` for a similar effect—run `tmux` on the remote host and launch Fresh inside it. Session persistence has the advantage of being built into Fresh, so editor state (open files, terminals, undo history) is preserved without an external multiplexer.
