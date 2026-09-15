# Usability Test: Orchestrator Workspace Sessions

**Method:** Moderated-style single-participant discount usability test (Nielsen Norman Group
protocol), think-aloud, cold start.
**Date:** 2026-09-15
**Build:** `cargo build` (debug) at commit `da558dd`, `target/debug/fresh`
**Environment:** tmux 3.4, 200x50, Ubuntu 24.04, mouse reporting enabled.

## Participant profile

Deliberately constructed "first-time user" persona:

- Has never seen the product demoed and has watched no video.
- Has read no documentation, no README, no source code, no help text in advance.
- Knows git and ssh generally, but nothing about this editor's concepts
  ("workspace", "orchestrator", "agent").

The tester operated only through what was visible on screen.

## Test fixtures

| Fixture | Detail |
| --- | --- |
| SSH server | User-space `sshd` on `127.0.0.1:2222`, ed25519 host key, pubkey auth only |
| SSH client config | `~/.ssh/config` alias `testbox` with `HostName`, `Port`, `User`, `IdentityFile`, `UserKnownHostsFile`, `StrictHostKeyChecking no` |
| Local repo | `/root/fixtures/acme-widgets` — 3 commits, 2 branches, no remote |
| Remote repo | `/root/fixtures/acme-widgets-remote` — same repo, distinguishable README |

## Tasks

1. **Task 1 — Local:** "Start a workspace session on this machine."
2. **Task 2 — Remote:** "Start a workspace session on the machine `testbox`."

## Results summary

| Task | Outcome | Actions | Errors encountered |
| --- | --- | --- | --- |
| 1. Local session | **Success** | 2 clicks, ~5 s | 0 |
| 2. Remote session | **Failed in-app** | 3 failed attempts + 2 out-of-band shell fixes | 2 blocking |

Task 1 is close to a best-case first-run experience. Task 2 could not be completed
using the application alone; the tester had to leave the product, run `ssh-keyscan`
and copy a private key by hand, and then return. A real first-time user without
shell expertise would have abandoned the feature here.

---

## What works well

These are genuine strengths and should be protected in any redesign.

- **The dock is open on first launch** and `[ + New ]` is the top-left element.
  Discovering *where* to start took zero seconds — rare for a feature this deep.
- **Two clicks to a working local session.** New → Create Workspace, with no
  required typing, produced a live shell in a correctly-prepared git worktree.
- **SSH hosts are auto-discovered from `~/.ssh/config`.** Seeing `testbox`
  already in the Machine list, resolved to `root@127.0.0.1:2222`, was the single
  most delightful moment of the session. (See F2 — the promise is not kept.)
- **Defaults are well chosen.** Project path prefilled from cwd; workspace name
  auto-generated.
- **Hints are context-sensitive.** They change per machine type
  ("blank uses this default" → "/srv/project (blank = remote home)") and reflect
  real repo state ("no origin configured").
- **The destructive-action dialog is exemplary.** It enumerates consequences
  ("stop all workspace processes", "run `git worktree remove`", "drop the
  workspace record"), warns "Uncommitted changes will be lost.", and puts
  `[ Cancel ]` in the first, safe position. This is better than most commercial
  software.
- **Sessions persist across a restart and reconnect cleanly.** After quit and
  relaunch, the remote workspace reappeared and reattached to the right worktree.
- **The dock is resizable** by dragging the splitter.

---

## Findings

Severity uses the standard NN/g 0–4 scale (0 = not a problem, 4 = usability
catastrophe, must fix before release).

### F1 — Remote connection fails closed, with no trust-on-first-use prompt — **Severity 4**

Creating the SSH workspace failed with `Host key verification failed.` There is no
fingerprint shown, no "do you want to trust this host?" dialog, and no remediation
offered anywhere in the UI. Every standard SSH client resolves this with a yes/no
prompt on first connect.

Because the product advertises the host in its own dropdown, the user reasonably
expects it to be connectable. Instead the defining action of the feature — reaching
another machine — dead-ends. The tester could only proceed by exiting to a shell and
running `ssh-keyscan -p 2222 127.0.0.1 > ~/.ssh/known_hosts`.

> **Recommendation:** On an unknown host key, show a modal with the host, port and
> fingerprint and offer *Trust and connect* / *Cancel*, writing to `known_hosts` on
> accept. This single change converts a task failure into a two-second confirmation.

### F2 — `~/.ssh/config` is honored partially, creating a false affordance — **Severity 4**

The Machine list is populated from `~/.ssh/config`, and `HostName`, `Port` and
`User` are clearly applied (the hint correctly read `root@127.0.0.1:2222`).
But `IdentityFile`, `UserKnownHostsFile` and `StrictHostKeyChecking` were all
ignored. After fixing F1 the very next attempt failed with
`root@127.0.0.1: Permission denied (publickey).`, resolved only by copying the key
to the default `~/.ssh/id_ed25519`.

Partial support is worse than none: the host appearing in the list is an implicit
promise that the user's config works. The user has no way to know which half of
their config is live.

> **Recommendation:** Either honor the full per-host config (at minimum
> `IdentityFile`, `IdentitiesOnly`, `UserKnownHostsFile`, `StrictHostKeyChecking`,
> `ProxyJump`), or shell out to the system `ssh` so the user's config is
> authoritative by construction. If some directives stay unsupported, say so in the
> Machine hint line.

### F3 — Error text is truncated with no way to read it in place — **Severity 3**

At the default dock width the failure rendered as
`! ⇅ ssh:testbox  testbox  Host key v…`. Clicking the row did nothing. There is no
tooltip, detail pane, or log view. The tester had to drag the splitter out to ~110
columns to read a 28-character message.

> **Recommendation:** Make the row expandable on click, or surface failures as a
> toast/detail panel. Never truncate the only description of a blocking error.

### F4 — Recovery actions are hidden behind right-click — **Severity 3**

`Retry` and `Dismiss` exist, but only in a context menu reached by right-clicking the
failed row — discovered by the tester only through deliberate probing. The same menu
holds `Visit… / Rename… / Move to Folder… / Archive / Delete`. Nothing anywhere hints
that right-click is available.

A failed row is precisely the moment a user needs a visible, obvious next step.

> **Recommendation:** Render inline `[Retry]` / `[Dismiss]` affordances on error
> rows, and add a `⋯` per-row button mirroring the context menu.

### F5 — Checkbox glyph, help text and behavior all disagree — **Severity 3**

With Machine set to an SSH host, the control renders as:

```
[v] Create a git worktree
    ↳ Not checked — the host decides when you create
```

The glyph says checked, the caption says "Not checked", and the actual behavior was
to **create a worktree** remotely (`~/.fresh/worktrees/acme-widgets-remote/acme-widgets-2`,
branch `acme-widgets-2`). Three sources of truth, three different answers.

### F6 — "leave empty to use provided branch" does the opposite — **Severity 3**

With `Checkout branch: HEAD` and `New branch name:` empty, the hint reads
"leave empty to use provided branch". The session was created on a **new branch
named after the workspace** (`acme-widgets-1`), not on HEAD. A user relying on this
hint will silently get branches they did not ask for.

### F7 — Auto-generated workspace name collides with an existing session — **Severity 2**

After creating `acme-widgets-1` (local) and `acme-widgets-2` (remote), reopening the
dialog proposed `acme-widgets-2` again. The counter does not consider existing
sessions, so the default value is a duplicate.

### F8 — Undisclosed writes to the user's repository — **Severity 2**

Running `git worktree list` in the project afterwards showed an entry the user never
created:

```
/root/.local/share/fresh/orchestrator/.sync-workspace  b86033f [fresh/fresh-sessions]
```

A `fresh/fresh-sessions` branch and a `.sync-workspace` worktree are created silently.
Nothing in the UI mentions them. Users who inspect their own repo will find
unexplained refs and may delete them, or may report them as corruption.

> **Recommendation:** Disclose this once, on first use, with a one-line explanation
> and a link to docs.

### F9 — Deleting a workspace leaves its branch behind, undocumented — **Severity 2**

The otherwise-excellent confirm dialog lists three consequences but omits a fourth:
the branch survives. After deleting workspace `acme-widgets-1`, `git branch` still
listed `acme-widgets-1`. Keeping the branch is a defensible default — but the dialog
that carefully enumerates everything else should say so, ideally with a
"also delete branch" checkbox.

### F10 — Quitting with a live remote session gives no warning — **Severity 2**

`File ▸ Quit` terminated an active SSH workspace immediately, with no prompt. This is
a striking inconsistency with F9's thorough delete confirmation: the more destructive
path (kill everything) is the one without a guard.

### F11 — The one advertised global shortcut did not work — **Severity 2**

The status bar permanently reads `Palette: Ctrl+P`. Ctrl+P produced no palette with
the editor focused, with the dock focused, or when sent as a raw `0x10` byte. The
application was demonstrably receiving keys throughout (Escape, arrow keys and Enter
all worked). Additionally, when a workspace terminal holds focus it is ambiguous
whether a keystroke goes to the app or to the remote shell, and no indicator
distinguishes the two.

### F12 — No keyboard route to the Orchestrator dock — **Severity 2**

`View ▸ Orchestrator Dock` exists but carries **no accelerator**, and the View menu
shows no shortcuts at all — conspicuous next to `File`, which shows `Ctrl+O`,
`Ctrl+S`, `Ctrl+Q`. There is also no "New Workspace Session" command in any menu: the
`[ + New ]` button is the sole entry point. In a terminal-first editor, a core
feature reachable only by mouse is a significant accessibility and adoption gap.

### F13 — The `▼` disclosure arrow is not a click target — **Severity 2**

Clicking the `▼` on the Machine control did nothing; clicking the field text opened
the list. The arrow is the most obvious click target in the control and is inert.
Relatedly, the footer hint says `←→ change option`, but with the list open only
`↑↓` moved the selection, and `Esc` did not close the list.

### F14 — The New Workspace modal is mostly empty space — **Severity 2**

The dialog is 32 rows tall with roughly 12 rows of content, including two blank bands
of 5 and 6 rows that reserve space for machine-specific fields. On first sight it
reads as a rendering bug. Fields also jump position when Machine changes (the
branch controls vanish entirely for SSH), so the layout never feels stable.

### F15 — Session labels are inconsistent and over-long — **Severity 1**

Local rows read
`acme-widgets-1 · bash — root@vm: ~/.local/share/fresh/orchestrator/root_fixtures_acme-widgets/acme-widgets-1`,
remote rows read `⇅ ssh:testbox · ssh — root@vm: ~/.fresh/…  testbox` — the host is
repeated twice and the workspace name is absent. The same long string fills the whole
editor tab bar and truncates the Delete dialog's title mid-path.

### F16 — Status glyphs have no legend — **Severity 1**

`·` vs `*`, green vs grey, `!` for error, `⇅` for remote. All meaning must be inferred,
and the marker on a row changes as focus moves. A legend in the `⋯` menu, or
tooltips, would resolve this cheaply.

### F17 — Local and remote worktrees use different, unpreviewed path schemes — **Severity 1**

Local: `~/.local/share/fresh/orchestrator/<slug>/<name>`.
Remote: `~/.fresh/worktrees/<repo>/<name>`.
Neither is shown before creation, so the first-time user is surprised to land in a
deep opaque directory rather than their project.

### F18 — Assorted polish — **Severity 1**

- Dock width is not persisted across restart.
- Session ordering changed after restart (the SSH row moved above the project row)
  with no apparent rule.
- The dropdown popup lets underlying field text bleed through at its right edge
  (`│ Add machine… │acme-widgets`).
- The Confirm Delete dialog draws a box inside a box (`│╭─ … ─╮│`).

---

## Prioritized recommendations

1. **Add trust-on-first-use for host keys** (F1). Single highest-impact fix; converts
   a task failure into a confirmation click.
2. **Make `~/.ssh/config` support complete, or delegate to system `ssh`** (F2).
3. **Make errors readable and recoverable in place** (F3, F4) — full text plus inline
   Retry.
4. **Resolve the worktree checkbox contradiction and the branch-name hint** (F5, F6).
5. **Give the dock a keyboard shortcut and menu commands** (F11, F12).
6. **Tighten the modal layout and label scheme** (F14, F15).

## Reproduction

```bash
# SSH server
mkdir -p ~/sshtest/etc && cd ~/sshtest
ssh-keygen -q -t ed25519 -f etc/ssh_host_ed25519_key -N ''
ssh-keygen -q -t ed25519 -f etc/testkey -N ''
cat etc/testkey.pub > etc/authorized_keys
/usr/sbin/sshd -f etc/sshd_config       # Port 2222, ListenAddress 127.0.0.1

# Client alias
printf 'Host testbox\n  HostName 127.0.0.1\n  Port 2222\n  User root\n  IdentityFile ~/sshtest/etc/testkey\n' >> ~/.ssh/config

# Then: launch fresh in a git repo, click [ + New ], set Machine = testbox, Create.
```
