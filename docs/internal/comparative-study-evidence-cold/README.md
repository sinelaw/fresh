# Evidence — cold comparative usability study (fresh Orchestrator vs herdr 0.9.0)

Screens captured from `tmux capture-pane` at 200x50.

- `*.ansi` — full-screen capture **with colour** (`capture-pane -p -e`). View with `cat`.
- `*.txt`  — plain-text companion (SGR sequences stripped), for grep/diff.

Files named `herdr-01` … `herdr-22` are plain-text only: they were captured before the
study switched to full-screen colour capture, and several show only part of the screen.
The decisive herdr moment was re-captured in full colour as
`herdr-DECISIVE-hostkey-prompt-FULL.ansi` / `-accepted-FULL.ansi`.

## Decisive moments

| File | What it shows |
|---|---|
| `herdr-DECISIVE-hostkey-prompt-FULL` | herdr presents the OpenSSH fingerprint prompt in-band during `herdr machine add` |
| `herdr-DECISIVE-hostkey-accepted-FULL` | "Saved SSH machine … Remote server is ready." after typing `yes` |
| `herdr-11a-hostkey-prompt.txt` | the same moment in the original (non-re-enacted) run |
| `herdr-10-machine-add.txt` | `herdr machine add --label X X` → usage error, exit 2 (argument-order inconsistency) |
| `herdr-13-machine-in-sidebar.txt` | `testbox` appears live in the sidebar with no client restart |
| `herdr-18-new-worktree.txt` | New-worktree dialog pre-filling branch name and showing the checkout path |
| `herdr-22-delete-worktree.txt` | destructive confirm naming the path, and stating the branch is not deleted |
| `herdr-17-reattach.txt` | re-running `herdr` restores the live session with scrollback and a still-running probe |
| `herdr-25-after-server-kill-FULL` | after the server is killed: record restored, work gone |
| `fresh-01-launch-FULL` | Orchestrator dock visible and labelled at first launch |
| `fresh-05-machine-dropdown-FULL` | `testbox` already present, auto-discovered from `~/.ssh/config` |
| `fresh-09-testbox-selected-FULL` | selecting `testbox` shows the resolved target `root@127.0.0.1:2222` |
| `fresh-06-add-machine-FULL` | "Add machine…" selected — no dialog, no new fields (dead entry) |
| `fresh-10a-HOSTKEY-FAIL` | **"Host key verification failed."** on the default remote flow |
| `fresh-12-f2-menu-FULL` | the only in-product recovery offered: Retry / Dismiss |
| `fresh-13-retry-FULL` | Retry fails identically |
| `fresh-20-ssh-cmdline-resolved-address.txt` | the worktree-prep `ssh` invocation using `-p 2222 root@127.0.0.1` instead of the alias |
| `fresh-15-otherhost-accept-FULL` | remote workspace working via the "Other host…" escape hatch |
| `fresh-16-remote-verify-FULL` | `SSH_CONNECTION` set, `# REMOTE-DEMO-REPO` — genuinely remote |
| `fresh-23-relaunch-FULL` | after Ctrl+Q and relaunch: records restored |
| `fresh-24-restored-session-FULL` | restored session reconnects but is a new shell at `~`, no work |
| `fresh-26-delete-confirm-FULL` | delete confirmation enumerating exactly what it will do |
